#include "Generics.inl"
#include "Parser.h"
#include "TypeInfo.h"
#include "Workspace.h"

#include <stdio.h>



static const bool s_fTypecheckPartialGenericStructs = true;

TypeInfo * PTinPromoteUntypedDefault(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtab,
	STNode * pStnodLit,
	TypeInfo * pTinExpected = nullptr,
	ERREP errep = ERREP_ReportErrors);

TypeInfo * PTinPromoteUntypedTightest(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtab,
	STNode * pStnodLit,	
	TypeInfo * pTinDst,
	ERREP errep = ERREP_ReportErrors);

TypeInfo * PTinPromoteUntypedArgument(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtab,
	STNode * pStnodLit,
	TypeInfo * pTinArgument,
	ERREP errep);

TypeInfo * PTinPromoteVarArg(TypeCheckWorkspace * pTcwork, SymbolTable * pSymtab, TypeInfo * pTinIn);

void OnTypeResolve(TypeCheckWorkspace * pTcwork, const Symbol * pSym);
void FinalizeLiteralType(TypeCheckWorkspace * pTcwork, SymbolTable * pSymtab, TypeInfo * pTinDst, STNode * pStnodLit);
static bool FCanImplicitCast(TypeInfo * pTinSrc, TypeInfo * pTinDst);
TypeInfo * PTinQualifyAfterAssignment(TypeInfo * pTin, SymbolTable * pSymtab, TypeInfo * pTinDst);
TypeInfo * PTinAfterRValueAssignment(TypeCheckWorkspace * pTcwork, const LexSpan & lexsp, TypeInfo * pTin, SymbolTable * pSymtab, TypeInfo * pTinDst);
SymbolTable * PSymtabFromType(TypeCheckWorkspace * pTcwork, TypeInfo * pTin, const LexSpan & lexsp);

using namespace Moe;

namespace BuiltIn
{
#define BLTIN(x) const char * g_pChz##x = 
#define STR(x) #x;
	BUILT_IN_TYPE_LIST	
#undef STR
#undef BLTIN

#define BLTIN(x) Moe::InString g_istr##x;
#define STR(x)
	BUILT_IN_TYPE_LIST	
#undef STR
#undef BLTIN
}

enum TCRET
{
	TCRET_Complete,
	TCRET_StoppingError,
	TCRET_WaitingForSymbolDefinition,
	TCRET_Continue,
};

enum TCCTX // tag = Type Check Context
{
	TCCTX_Normal,
	TCCTX_TypeSpecification, // type checking is inside type spec, walking the tree to do literal op evaluation
};

enum PROCMATCH
{
	PROCMATCH_None,
	PROCMATCH_Exact,
	PROCMATCH_ImplicitCast,

	MOE_MAX_MIN_NIL(PROCMATCH)
};

enum FARG
{
	FARG_DefaultArg			= 0x1,	// default argument needs to copy syntax tree
	FARG_NamedLabelChild	= 0x2,	// argument was specified with a label - pStnod points at label parent
	FARG_BakedValue			= 0x4,	// this stnod will be removed from the argument list (but not deleted until end of typecheck) 
	FARG_TypeArgument		= 0x8,  // this stnod will be removed from the argument list (but not deleted until end of typecheck) 
	FARG_ImplicitRef		= 0x10,	// this arg corresponds to an implicit reference

	GRFARG_Caller = FARG_DefaultArg | FARG_NamedLabelChild,								// flags related to how this proc was called
	GRFARG_DefinitionFlags = FARG_BakedValue | FARG_TypeArgument | FARG_ImplicitRef,	// flags pulled from the definition of this argument
	FARG_None = 0x0,
	FARG_All  = 0xF,
};

MOE_DEFINE_GRF(GRFARG, FARG, u8);

struct GenericMapScope // tag = genscope
{
							GenericMapScope(ErrorManager * pErrman, GenericMap * pGenmap)
							:m_pErrman(pErrman)
							,m_pGenmap(pGenmap)
								{
									if (pGenmap)
									{
										pErrman->PushGenmapContext(pGenmap);
									}
								}

							~GenericMapScope()
								{
									if (m_pGenmap)
									{
										m_pErrman->PopGenmapContext(m_pGenmap);
									}
								}

	ErrorManager *			m_pErrman;
	GenericMap * 			m_pGenmap;
};



// Unpacking procedures and generic struct arguments resolves the following:
//   value arguments are supplied in order separated by commas, or explicitly using an argument name. 
//   generic types can be supplied explicitly by name or inferred by the argument that features a '$' anchor  
//   struct/procedure definitions can supply a default to be used when an argument is omitted

struct ArgUnpack // tag=argunp
{
				ArgUnpack()
					:m_pStnodInit(nullptr)
					,m_grfarg(FARG_None)
					{ ; }

	STNode *	m_pStnodInit;		// value supplied for this argument (used for type inference if no explicit type)
	GRFARG		m_grfarg;
};

struct ProcMatchFit // tag pmfit
{
						ProcMatchFit(Alloc * pAlloc)
						:m_mpIArgArgunp(pAlloc, BK_TypeCheckProcmatch)
						,m_pGenmap(nullptr)
							{ ; }

	CDynAry<ArgUnpack>		m_mpIArgArgunp;
	GenericMap *			m_pGenmap;
};

struct ProcMatchParam // tag = pmparam
{
						ProcMatchParam(Alloc * pAlloc, const LexSpan & lexsp)
						:m_pAlloc(pAlloc)
						,m_lexsp(lexsp)
						,m_ppStnodCall(nullptr)
						,m_cpStnodCall(0)
						,m_pPmfit(nullptr)
						,m_fMustFindMatch(false)
							{ ; }

						~ProcMatchParam()
							{
								if (m_pPmfit)
								{
									m_pAlloc->MOE_DELETE(m_pPmfit);	
									m_pPmfit = nullptr;
								}
							}

	Alloc *				m_pAlloc;
	const LexSpan &		m_lexsp;
	STNode **			m_ppStnodCall;		// actual arguments passed to the call, no default/named args
	size_t				m_cpStnodCall;		// no default/named args

	ProcMatchFit *		m_pPmfit;

	bool				m_fMustFindMatch;
};


struct TypeCheckStackEntry // tag = tcsent
{
	int				m_nState;
	STNode *		m_pStnod;
	SymbolTable *	m_pSymtab;			// BB - Could omit this pointer with careful handling of stack pops?
										//  maybe swap out for fPushedStack?
	STNode *		m_pStnodProcedure;	// definition node for current procedure
	Symbol	*		m_pSymContext;		// Procedure or struct 
	GRFSYMLOOK		m_grfsymlook;
	PARK 			m_parkDeclContext;
	bool			m_fAllowForwardDecl;
	TCCTX			m_tcctx;
};

struct TypeCheckFrame // tag = tcfram
{
	size_t							m_ipTcframQueue;	// index in the pending/waiting queue
	WorkspaceEntry *				m_pEntry;
	CDynAry<TypeCheckStackEntry>	m_aryTcsent;
};

struct UnknownType // tag = untype
{
	CDynAry<TypeCheckFrame *>		m_arypTcframDependent;		// id for frames dependent on this type
};



class NameMangler // tag = mang
{
public:
					NameMangler(Moe::Alloc * pAlloc, size_t cBStartingMax=1024);
					~NameMangler();

	void			Resize(size_t cBStartingMax);
	void			AppendName(const char * pChz);
	void			AppendType(TypeInfo * pTin);

	Moe::InString	IstrMangleMethodName(TypeInfoProcedure * pTinproc);
	TypeInfoProcedure * 
					PTinprocDemangle(const Moe::InString & istrName, SymbolTable * pSymtab);

	Moe::Alloc *		m_pAlloc;
	Moe::StringBuffer	m_strbuf;
};



#define VALIDATE_NAME_MANGLING 1

// MOEB_TODO - why is this 'workspace' rather than TypeCheckContext tcctx?
struct TypeCheckWorkspace // tag = tcwork
{
					TypeCheckWorkspace(Alloc * pAlloc, ErrorManager * pErrman, BlockListEntry * pblistEntry)
					:m_pAlloc(pAlloc)
					,m_pErrman(pErrman)
					,m_mang(pAlloc)
					,m_pblistEntry(pblistEntry)
					,m_blistTcfram(pAlloc, Moe::BK_TypeCheck)
					,m_hashPSymUntype(pAlloc, Moe::BK_TypeCheck)
					,m_arypTcframPending(pAlloc, Moe::BK_TypeCheck, pblistEntry->C())
					,m_arypTcframWaiting(pAlloc, Moe::BK_TypeCheck, pblistEntry->C())
					,m_genreg(pAlloc)
						{ ; }

					~TypeCheckWorkspace()
						{ ; }

	Alloc *									m_pAlloc;
	ErrorManager *							m_pErrman;
	NameMangler								m_mang;
	BlockListEntry * 						m_pblistEntry;
	CBlockList<TypeCheckFrame, 128>			m_blistTcfram;
	CHash<const Symbol *, UnknownType>		m_hashPSymUntype;

	CDynAry<TypeCheckFrame *>				m_arypTcframPending;	// frames ready to be run again (may stop 
																	//  during check, not guaranteed to have all types)
	CDynAry<TypeCheckFrame *>				m_arypTcframWaiting;	// frames waiting for one specific symbol
	GenericRegistry							m_genreg;				// registry of instantiated generic types
};

void EmitWarning(TypeCheckWorkspace * pTcwork, const LexSpan & lexsp, ERRID errid, const char * pChz, ...)
{
	va_list ap;
	va_start(ap, pChz);
	EmitWarning(pTcwork->m_pErrman, lexsp, errid, pChz, ap);
}

void EmitError(TypeCheckWorkspace * pTcwork, const LexSpan & lexsp, ERRID errid, const char * pChz, ...)
{
	va_list ap;
	va_start(ap, pChz);
	EmitError(pTcwork->m_pErrman, lexsp, errid, pChz, ap);
}

Moe::InString IstrFromTypeInfo(TypeInfo * pTin)
{
	char aCh[1024];
	Moe::StringBuffer strbuf(aCh, MOE_DIM(aCh));

	WriteTypeInfoSExpression(&strbuf, pTin, PARK_Nil);
	return IstrIntern(aCh);
}

void AppendTypenameFromTypeSpecification(STNode * pStnodArg, Moe::StringBuffer * pStrbuf)
{
	// BB - This routine should reproduce a type specification, but it's not really up to the task
	//  (especially for generic types)
	STNode * pStnodIt = pStnodArg;
	while (pStnodIt)
	{
		switch (pStnodIt->m_park)
		{
			case PARK_Identifier:
			{
				auto pStval = PStnodRtiCast<STValue *>(pStnodIt);
				if (!MOE_FVERIFY(pStval && pStval->m_stvalk == STVALK_String, "identifier without identifier string detected"))
					break;
				AppendChz(pStrbuf, IstrFromIdentifier(pStnodIt).PChz()); 
				pStnodIt = nullptr;
			}break;
			case PARK_Literal:
			{
				PrintLiteral(pStrbuf, pStnodIt);
				pStnodIt = nullptr;
			}break;
			case PARK_GenericStructSpec:
			{
				auto pStnodIdent = pStnodIt->PStnodChildSafe(0);
				if (!MOE_FVERIFY(pStnodIdent, "GenericStructInst with no identifier"))
					break;

				AppendTypenameFromTypeSpecification(pStnodIdent, pStrbuf);
				AppendChz(pStrbuf, "(");
				auto cStnodChild = pStnodIt->CPStnodChild();
				if (cStnodChild > 1)
				{
					int iStnod = 1;
					while (1)
					{
						auto pStnodChild = pStnodIt->PStnodChildSafe(iStnod);
						AppendTypenameFromTypeSpecification(pStnodChild, pStrbuf);
						++iStnod;
						if (iStnod >= cStnodChild)
							break;
						AppendChz(pStrbuf, ",");
					}
				}
				AppendChz(pStrbuf, ")");
				pStnodIt=  nullptr;

			}break;
			case PARK_QualifierDecl:
			{
				auto pStval = PStnodRtiCast<STValue *>(pStnodIt);
				if (!MOE_FVERIFY(pStval, "qualifier without value string detected"))
					break;
				AppendChz(pStrbuf, pStval->m_istrRword.PChz()); 
				AppendChz(pStrbuf, " "); 
			}break;
			case PARK_ReferenceDecl:
				AppendChz(pStrbuf, "* "); 

				MOE_ASSERT(pStnodIt->CPStnodChild() == 1, "expected one child");
				pStnodIt = pStnodIt->PStnodChild(0);
				break;
			case PARK_ArrayDecl:
				// BB - should follow the [], [..], [c] convention
				MOE_ASSERT(false, "not type-checking asserts yet");
				pStnodIt=  nullptr;

				break;
			default:
				AppendChz(pStrbuf, "<unexpected PARK_"); 
				AppendChz(pStrbuf, PChzAbbrevFromPark(pStnodIt->m_park));
				AppendChz(pStrbuf, "> "); 

				pStnodIt = nullptr;
				break;
		}
	}
}

Moe::InString IstrTypenameFromTypeSpecification(STNode * pStnod)
{
	char aCh[2048];
	Moe::StringBuffer strbuf(aCh, MOE_DIM(aCh));

	AppendTypenameFromTypeSpecification(pStnod, &strbuf);
	return IstrInternCopy(aCh);
}

Moe::InString IstrFullyQualifiedSymbol(Symbol * pSym)
{
	char aCh[256];
	Moe::StringBuffer strbuf(aCh, MOE_DIM(aCh));
	FormatChz(&strbuf, "TBD::TBD::%s",pSym->m_istrName.PChz());
	return IstrInternCopy(aCh);
}

UnknownType * PUntypeEnsure(TypeCheckWorkspace * pTcwork, const Symbol * pSym)
{
	UnknownType * pUntype = pTcwork->m_hashPSymUntype.Lookup(pSym);
	if (!pUntype)
	{
		pTcwork->m_hashPSymUntype.InresEnsureKey(pSym, &pUntype);
		pUntype->m_arypTcframDependent.SetAlloc(pTcwork->m_pAlloc, Moe::BK_TypeCheck);
	}
	return pUntype;
}

NameMangler::NameMangler(Moe::Alloc * pAlloc, size_t cBStartingMax)
:m_pAlloc(pAlloc)
,m_strbuf()
{
	Resize(cBStartingMax);
}

NameMangler::~NameMangler()
{
	Resize(0);
}

void NameMangler::Resize(size_t cBNew)
{
	char * aBNew = nullptr;
	if (cBNew)
	{
		aBNew = (char *)m_pAlloc->MOE_ALLOC_TYPE_ARRAY(char, cBNew);

		if (m_strbuf.m_cBMax)
		{
			(void) CBCopyChz(m_strbuf.m_pChzBegin, aBNew, m_strbuf.m_cBMax);
		}
	}

	if (m_strbuf.m_cBMax)
	{
		m_pAlloc->MOE_DELETE(m_strbuf.m_pChzBegin);
	}

	m_strbuf = Moe::StringBuffer(aBNew, cBNew);
}

static inline s64 NReadNumber(const char ** ppChz)
{
	const char * pChz = *ppChz;
	if ((*pChz < '0') | (*pChz > '9'))
		return -1;
	
	s64 n = 0;
	while (1)
	{
		if ((*pChz>= '0') & (*pChz<= '9'))
			n = n*10 + (*pChz- '0');
		else
			break;
		++pChz;
    }

	*ppChz = pChz;
	return n;
}

static inline Moe::InString IstrReadName(const char ** ppChz)
{
	auto cCh = NReadNumber(ppChz);
	if (cCh < 0)
		return IstrIntern("");

	const char * pChzName = *ppChz;
	*ppChz = pChzName + cCh;
	return IstrInternCopy(pChzName, (size_t)cCh);
}

static inline bool FMatchString(const char * pChzRef, const char ** ppChz)
{
	//check for a string match and return the end of the string 

	auto pChzRefIt = pChzRef;
	const char * pChzIt = *ppChz;
	while (*pChzRefIt != '\0')
	{
		if (*pChzRefIt != *pChzIt)
			return false;
		++pChzRefIt;
		++pChzIt;
	}

	*ppChz = pChzIt;
	return true;
}

TypeInfo * PTinReadType(const char ** ppChz, SymbolTable * pSymtab)
{
	int chFirst = **ppChz;
	if (chFirst == 'B')	// built-in type
	{
		++(*ppChz);
		char chBuiltIn = *(*ppChz)++;
		switch(chBuiltIn)
		{
		case 'i':
		case 'u':
			{
				char aCh[4];
				Moe::StringBuffer strbufScratch(aCh, MOE_DIM(aCh));
				*strbufScratch.m_pChzAppend++ = (chBuiltIn == 'i') ? 's' : 'u';
				switch(*(*ppChz)++)
				{
					case 'c':	AppendChz(&strbufScratch, "8");		break;
					case 's':	AppendChz(&strbufScratch, "16");	break;
					case 'w':	AppendChz(&strbufScratch, "32");	break;
					case 'd':	AppendChz(&strbufScratch, "64");	break;
				}

				return pSymtab->PTinBuiltin(IstrInternCopy(aCh));
			} 
		case 'g':	return pSymtab->PTinBuiltin(BuiltIn::g_istrF32);
		case 'd':	return pSymtab->PTinBuiltin(BuiltIn::g_istrF64);
		case 'f':	return pSymtab->PTinBuiltin(BuiltIn::g_istrBool);
		case 's':	return pSymtab->PTinBuiltin(BuiltIn::g_istrString);
		case 'v':	return pSymtab->PTinBuiltin(BuiltIn::g_istrVoid);
		default: MOE_ASSERT(false, "unknown built-in type during de-mangling");
		}
	}
	else if (chFirst == 'Q') // Qualifier
	{
		++(*ppChz);
		GRFQUALK grfqualk = FQUALK_None;
		if (**ppChz == 'c')
		{
			grfqualk.AddFlags(FQUALK_Const);
			++(*ppChz);
		}
		if (**ppChz == 'i')
		{
			grfqualk.AddFlags(FQUALK_InArg);
			++(*ppChz);
		}

		auto pTinPointedTo = PTinReadType(ppChz, pSymtab);
		if (!pTinPointedTo)
			return nullptr;
		return pSymtab->PTinqualEnsure(pTinPointedTo, grfqualk);
	}
	else if (chFirst == 'P') // Pointer
	{
		++(*ppChz);
		bool fIsImplicitRef = false;
		if (**ppChz == 'i')
		{
			fIsImplicitRef = true;
			++(*ppChz);
		}

		auto pTinPointedTo = PTinReadType(ppChz, pSymtab);
		if (!pTinPointedTo)
			return nullptr;
		return pSymtab->PTinptrAllocate(pTinPointedTo, fIsImplicitRef);
	}
	else if (chFirst == 'A') // Array
	{
		++(*ppChz);
		TypeInfoArray * pTinary = MOE_NEW(pSymtab->m_pAlloc, TypeInfoArray) TypeInfoArray();

		if (**ppChz == 'R') // ARYK_Reference
		{
			++(*ppChz);
			pTinary->m_aryk = ARYK_Reference;
		}
		else
		{
			pTinary->m_aryk = ARYK_Fixed;
			pTinary->m_c = NReadNumber(ppChz);
		}

		pTinary->m_pTin = PTinReadType(ppChz, pSymtab);

		if (pTinary->m_pTin == nullptr || pTinary->m_c < 0)
		{
			pSymtab->m_pAlloc->MOE_DELETE(pTinary);
			return nullptr;
		}
		pSymtab->AddManagedTin(pTinary);
		return pTinary;
	}
	else if (chFirst == 'F') // procedure reference
	{
		++(*ppChz);
		auto cpTinReturn = NReadNumber(ppChz);
		if (!FMatchString("_", ppChz))
			return nullptr;

		bool fHasVarArgs = FMatchString("VA", ppChz);
		bool fIsForeign = FMatchString("FF", ppChz);
		Moe::CDynAry<TypeInfo *> arypTinParams(pSymtab->m_pAlloc, Moe::BK_Stack);
		Moe::CDynAry<TypeInfo *> arypTinReturns(pSymtab->m_pAlloc, Moe::BK_Stack);

		while (!FMatchString("_", ppChz) && **ppChz != '\0')
		{
			arypTinParams.Append(PTinReadType(ppChz, pSymtab));
			if (arypTinParams.Last() == nullptr)
				return nullptr;
		}

		for (int ipTinReturn = 0; ipTinReturn < cpTinReturn; ++ipTinReturn)
		{
			arypTinReturns.Append(PTinReadType(ppChz, pSymtab));
			if (arypTinReturns.Last() == nullptr)
				return nullptr;
		}

		auto pTinproc = pSymtab->PTinprocAllocate(InString(), arypTinParams.C(), arypTinReturns.C());
		pTinproc->m_grftinproc.AssignFlags(FTINPROC_HasVarArgs, fHasVarArgs);
		pTinproc->m_grftinproc.AssignFlags(FTINPROC_IsForeign, fIsForeign);

		size_t cpTin = arypTinParams.C();
		for (size_t ipTin = 0; ipTin < cpTin; ++ipTin)
		{
			pTinproc->m_arypTinParams.Append(arypTinParams[ipTin]);
		}

		cpTin = arypTinReturns.C();
		for (size_t ipTin = 0; ipTin < cpTin; ++ipTin)
		{
			pTinproc->m_arypTinReturns.Append(arypTinReturns[ipTin]);
		}

		return pTinproc;
	}
	else
	{
		// BB - need to handle namespacing and nesting.

		auto strName = IstrReadName(ppChz);
		LexSpan lexsp;

		auto pSym = pSymtab->PSymLookup(strName, lexsp);
		return (pSym) ? PTinFromSymbol(pSym) : nullptr;
	}

	return nullptr;
}

void NameMangler::AppendName(const char * pChz)
{
	size_t cCh = CCh(pChz);
	FormatChz(&m_strbuf, "%d%s", cCh, pChz);
}

void NameMangler::AppendType(TypeInfo * pTin)
{
	switch (pTin->m_tink)
	{
    case TINK_Numeric:
		{
			// BB - doesn't respect typedefs, including int - will mangle just based on integer size
			auto pTinn = (TypeInfoNumeric *)pTin;

			if (pTinn->m_grfnum.FIsSet(FNUM_IsFloat))
			{
				char aChz[4] = "Bx";
				switch (pTinn->m_cBit)
				{
				case 32:	aChz[1] = 'g';	break;	// float
				case 64:	aChz[1] = 'd';	break;	// double
				}
			}
			else
			{
				char aChz[4] = "Bxx";
				aChz[1] = (pTinn->m_grfnum.FIsSet(FNUM_IsSigned)) ? 'i' : 'u';
				switch (pTinn->m_cBit)
				{
				case 8:		aChz[2] = 'c';	break;	// char
				case 16:	aChz[2] = 's';	break;	// short
				case 32:	aChz[2] = 'w';	break;	// word
				case 64:	aChz[2] = 'd';	break;	// double
				}
				AppendChz(&m_strbuf, aChz);
			}
		} break;
	case TINK_Bool:		AppendChz(&m_strbuf, "Bf");	break;
	case TINK_Void:		AppendChz(&m_strbuf, "Bv");	break;
	case TINK_Qualifier:
		{
			auto pTinqual = (TypeInfoQualifier *)pTin;

			AppendChz(&m_strbuf, "Q");
			if (pTinqual->m_grfqualk.FIsSet(FQUALK_Const)) 
				AppendChz(&m_strbuf, "c");
			if (pTinqual->m_grfqualk.FIsSet(FQUALK_InArg)) 
				AppendChz(&m_strbuf, "i");

			AppendType(pTinqual->m_pTin);

		} break;
    case TINK_Pointer:
		{
			auto pTinptr = (TypeInfoPointer *)pTin;
			AppendChz(&m_strbuf, "P");
			if (pTinptr->m_fIsImplicitRef)
				AppendChz(&m_strbuf, "i");
			AppendType(pTinptr->m_pTin);
		} break;
    case TINK_Struct:
		{
			AppendName(pTin->m_istrName.m_pChz); 
		}break;
    case TINK_Enum:
		{
			AppendName(pTin->m_istrName.m_pChz); 
		}break;
    case TINK_Array:
		{
			auto pTinary = (TypeInfoArray *)pTin;
			switch (pTinary->m_aryk)
			{
			case ARYK_Fixed:
				{
					FormatChz(&m_strbuf, "A%d", pTinary->m_c);
					AppendType(pTinary->m_pTin);
				} break;
			case ARYK_Reference:
				{
					AppendChz(&m_strbuf, "AR");
					AppendType(pTinary->m_pTin);
				} break;
			default: MOE_ASSERT(false, "unhandled array type");
			}
		} break;
	case TINK_Procedure:
		{
			auto pTinproc = (TypeInfoProcedure *)pTin;
			FormatChz(&m_strbuf, "F%d_", pTinproc->m_arypTinReturns.C());

			if (pTinproc->FHasVarArgs())
			{
				AppendChz(&m_strbuf, "VA");
			}

			if (pTinproc->m_grftinproc.FIsSet(FTINPROC_IsForeign))
			{
				AppendChz(&m_strbuf, "FF");
			}

			size_t ipTinMax = pTinproc->m_arypTinParams.C();
			for (size_t ipTin = 0; ipTin < ipTinMax; ++ipTin)
			{
				auto pTin = pTinproc->m_arypTinParams[ipTin];
				AppendType(pTin);
			}

			AppendChz(&m_strbuf, "_"); // return types
			ipTinMax = pTinproc->m_arypTinReturns.C();
			for (size_t ipTin = 0; ipTin < ipTinMax; ++ipTin)
			{
				auto pTin = pTinproc->m_arypTinReturns[ipTin];
				AppendType(pTin);
			}
		} break;
	default: 
		InString istrTin = IstrFromTypeInfo(pTin);
		MOE_ASSERT(false, "unexpected type encountered while name mangling a procedure '%s'", istrTin.m_pChz);
	}
}

InString NameMangler::IstrMangleMethodName(TypeInfoProcedure * pTinproc)
{
	m_strbuf.m_pChzAppend = m_strbuf.m_pChzBegin;
	AppendChz(&m_strbuf, "__F"); // function

	auto strPunyName = IstrPunyEncode(pTinproc->m_istrName.m_pChz);
	AppendName(strPunyName.m_pChz);

	AppendChz(&m_strbuf, "_"); // arguments

	if (pTinproc->FHasVarArgs())
	{
		AppendChz(&m_strbuf, "VA");
	}

	size_t ipTinMax = pTinproc->m_arypTinParams.C();
	for (size_t ipTin = 0; ipTin < ipTinMax; ++ipTin)
	{
		auto pTin = pTinproc->m_arypTinParams[ipTin];
		AppendType(pTin);
	}

	AppendChz(&m_strbuf, "_"); // return types
	ipTinMax = pTinproc->m_arypTinReturns.C();
	for (size_t ipTin = 0; ipTin < ipTinMax; ++ipTin)
	{
		auto pTin = pTinproc->m_arypTinReturns[ipTin];
		AppendType(pTin);
	}
	return IstrInternCopy(m_strbuf.m_pChzBegin, m_strbuf.m_pChzAppend - m_strbuf.m_pChzBegin);
}

TypeInfoProcedure * NameMangler::PTinprocDemangle(const InString & istrName, SymbolTable * pSymtab)
{
	const char * pChz = istrName.m_pChz;
	if (!FMatchString("__F", &pChz))
		return nullptr;

	InString istrProcNamePuny = IstrReadName(&pChz);
	InString istrProcName = IstrPunyDecode(istrProcNamePuny.m_pChz);

	if (!FMatchString("_", &pChz))
		return nullptr;

	bool fHasVarArgs = FMatchString("VA", &pChz);
	bool fIsForeign = FMatchString("FF", &pChz);
	Moe::CDynAry<TypeInfo *> arypTinParams(pSymtab->m_pAlloc, Moe::BK_Stack);
	Moe::CDynAry<TypeInfo *> arypTinReturns(pSymtab->m_pAlloc, Moe::BK_Stack);

	while (!FMatchString("_", &pChz) && *pChz != '\0')
	{
		arypTinParams.Append(PTinReadType(&pChz, pSymtab));
		if (arypTinParams.Last() == nullptr)
			return nullptr;
	}

	while (*pChz != '\0')
	{
		arypTinReturns.Append(PTinReadType(&pChz, pSymtab));
		if (arypTinReturns.Last() == nullptr)
			return nullptr;
	}

	auto pTinproc = pSymtab->PTinprocAllocate(istrProcName, arypTinParams.C(), arypTinReturns.C());
	pTinproc->m_grftinproc.AssignFlags(FTINPROC_HasVarArgs, fHasVarArgs);
	pTinproc->m_grftinproc.AssignFlags(FTINPROC_IsForeign, fIsForeign);

	size_t cpTin = arypTinParams.C();
	for (size_t ipTin = 0; ipTin < cpTin; ++ipTin)
	{
		pTinproc->m_arypTinParams.Append(arypTinParams[ipTin]);
	}

	cpTin = arypTinReturns.C();
	for (size_t ipTin = 0; ipTin < cpTin; ++ipTin)
	{
		pTinproc->m_arypTinReturns.Append(arypTinReturns[ipTin]);
	}

	return pTinproc;
}

InString IstrComputeMangled(TypeCheckWorkspace * pTcwork, STNode * pStnod, SymbolTable * pSymtab)
{
	auto pStproc = PStnodRtiCast<STProc *>(pStnod);
	TypeInfoProcedure * pTinproc = nullptr;
	auto pSym = pStnod->PSym();
	if (pSym)
	{
		auto pTinSym = PTinFromSymbol(pSym);
		pTinproc = PTinDerivedCast<TypeInfoProcedure *>(pTinSym);
	}

	if (!MOE_FVERIFY(pStproc && pTinproc, "bad procedure definition in StrComputeMangled"))
		return InString();

	if (pStproc->m_grfstproc.FIsSet(FSTPROC_UseUnmangledName))
	{
		return pTinproc->m_istrName;
	}
	else
	{
		auto istrMangled = pTcwork->m_mang.IstrMangleMethodName(pTinproc);

#if VALIDATE_NAME_MANGLING
		char aCh[1024];
		Moe::StringBuffer strbuf(aCh, MOE_DIM(aCh));

		WriteTypeInfoSExpression(&strbuf, pTinproc, PARK_Nil, FSEW_UseSizedNumerics);

		TypeInfoProcedure * pTinprocDemangled = pTcwork->m_mang.PTinprocDemangle(istrMangled, pSymtab);
		if (MOE_FVERIFY(pTinprocDemangled, "Name demangling failed - null procedure type"))
		{
			char aChAfter[1024];
			Moe::StringBuffer strbufAfter(aChAfter, MOE_DIM(aChAfter));
			WriteTypeInfoSExpression(&strbufAfter, pTinprocDemangled, PARK_Nil, FSEW_UseSizedNumerics);

			//MOE_ASSERT(FAreChzEqual(aCh, aChAfter), "Unmangled type info doesn't match initial info\n '%s' != '%s'",
			//	aCh, aChAfter);
			if (FAreChzEqual(aCh, aChAfter) == false)
			{
				// BB - right now generic types do not mangle/demangle properly
				printf("Unmangled type info doesn't match initial info\n %s != %s (after)\n", aCh, aChAfter);
			}
		}
#endif
		return istrMangled;
	}
}

static inline bool FIsMutableType(TypeInfo * pTin)
{
	auto pTinqual = PTinRtiCast<TypeInfoQualifier *>(pTin);
	if (pTinqual)
	{
		return !pTinqual->m_grfqualk.FIsAnySet(FQUALK_Const | FQUALK_InArg);
	}
	return !pTin || pTin->m_tink != TINK_Literal;
}

void AddSymbolReference(Symbol * pSymContext, Symbol * pSymTarget)
{
	if (pSymContext == pSymTarget)
		return;

	Symbol ** ppSymMac = pSymTarget->m_aryPSymReferencedBy.PMac();
	for (Symbol ** ppSymIt = pSymTarget->m_aryPSymReferencedBy.A(); ppSymIt != ppSymMac; ++ppSymIt)
	{
		if (*ppSymIt == pSymContext)
			return;
	}

	if (MOE_FVERIFY(pSymContext, "missing symbol context for %s", pSymTarget->m_istrName.PChz()))
	{
		pSymTarget->m_aryPSymReferencedBy.Append(pSymContext);
		pSymContext->m_aryPSymHasRefTo.Append(pSymTarget);
	}
}



enum IVALK // Instance VALue Kind
{
	IVALK_Error,	
	IVALK_Type,		// not an expression value: either a type or Type.m_nonConstantMember
	IVALK_RValue,	// has a value, but does not correspond to a memory location
	IVALK_LValue,	// has an assignable value

	MOE_MAX_MIN_NIL(IVALK)
};

const char * PChzFromIvalk(IVALK ivalk)
{
	static const char * s_mpIvalkPChz[] =
	{
		"Error",
		"Type",
		"RValue",
		"LValue",
	};
	MOE_CASSERT(MOE_DIM(s_mpIvalkPChz) == IVALK_Max, "missing IVALK string");
	if (ivalk == IVALK_Nil)
		return "Nil";

	if ((ivalk < IVALK_Nil) | (ivalk >= IVALK_Max))
		return "Unknown IVALK";

	return s_mpIvalkPChz[ivalk];
}

IVALK IvalkFromSym(Symbol * pSym)
{
	if (pSym->m_grfsym.FIsSet(FSYM_IsType))
	{
		return IVALK_Type;
	}
	else if (pSym->m_grfsym.FIsSet(FSYM_VisibleWhenNested))
	{
		return IVALK_RValue;
	}

	auto pTinSym = PTinFromSymbol(pSym);
	return (FIsMutableType(pTinSym)) ? IVALK_LValue : IVALK_RValue;
}

IVALK IvalkCompute(STNode * pStnod)
{
	if (pStnod->m_park == PARK_MemberLookup)
	{
		// if the lhs is a type this is not an lvalue, check for constant rvalues
		STNode * pStnodLhs = pStnod->PStnodChildSafe(0);
		STNode * pStnodRhs = pStnod->PStnodChildSafe(1);
		if (!MOE_FVERIFY((pStnodLhs != nullptr) & (pStnodRhs != nullptr), "invalid member lookup"))
			return IVALK_Error;

		// BB - we should have symbol tables for arrays and this should work like any other symbol
		TypeInfo * pTinLhs = pStnodLhs->m_pTin;
		auto pTinenum = PTinRtiCast<TypeInfoEnum *>(pTinLhs);
		if (pTinLhs->m_tink == TINK_Pointer)
		{
			auto pTinptr = PTinRtiCast<TypeInfoPointer *>(pTinLhs);
			pTinenum = PTinRtiCast<TypeInfoEnum *>(pTinptr->m_pTin);
		}
		if (pTinenum)
		{
			// check for individual fflag setting (ie. fdir.m_left = true)
			if (pTinenum->m_enumk == ENUMK_FlagEnum)
			{
				IVALK ivalkLhs = IvalkCompute(pStnodLhs);
				if (ivalkLhs == IVALK_LValue)
					return IVALK_LValue;
			}

			return IVALK_RValue;
		}

		if (pTinLhs && (pTinLhs->m_tink == TINK_Array || pTinLhs->m_tink == TINK_Literal))
		{
			return IVALK_RValue;
		}

		// We currently allow using lvalues to specify an R-Value as it gets tricky to specify array R-Values otherwise
		// ie. SType.m_inst.kConstant is the same as SType.SInstType.kConstant, so we can say SType.m_aN.count
		auto ivalkLhs = IvalkCompute(pStnodLhs);
		auto ivalkRhs = IvalkCompute(pStnodRhs);
		if ((ivalkLhs == IVALK_Type && ivalkRhs == IVALK_LValue) ||
			(ivalkLhs == IVALK_Error && ivalkRhs != IVALK_RValue))
		{
			// (type, (inst, m_val)) -> IVALK_Error
			return IVALK_Error;
		}
		return ivalkRhs;
	}
	else if (pStnod->m_park == PARK_Cast)
	{
		if (!pStnod->m_pTin || pStnod->m_pTin->m_tink != TINK_Pointer)
		{
			return IVALK_RValue;
		}
	}
	else if (pStnod->m_park == PARK_ArrayElement)
	{
		auto pStnodArray = pStnod->PStnodChild(0);
		if (!FIsMutableType(pStnodArray->m_pTin))
		{
			return IVALK_RValue;
		}
		return (pStnodArray->m_pTin->m_tink == TINK_Literal) ? IVALK_RValue : IVALK_LValue;
	}
	else if (pStnod->m_park == PARK_UnaryOp && pStnod->m_tok == TOK_Dereference)
	{

		return (FIsMutableType(pStnod->m_pTin)) ? IVALK_LValue : IVALK_RValue;
	}

	auto pSymbase = pStnod->m_pSymbase;
	if (pSymbase && pSymbase->m_symk == SYMK_Path)
	{
		auto pSymp = (SymbolPath *)pSymbase;
		auto ppSymEnd = pSymp->m_arypSym.PMac();
		for (auto ppSymIt = pSymp->m_arypSym.A(); ppSymIt != ppSymEnd; ++ppSymIt)
		{
			IVALK ivalk = IvalkFromSym(*ppSymIt);
			if (ivalk != IVALK_LValue)
				return ivalk;

			auto pTin = PTinFromSymbol(*ppSymIt);
			if (!pTin)
				continue;

			auto pTinenum = PTinRtiCast<TypeInfoEnum *>(pTin);
			if (pTin->m_tink == TINK_Pointer)
			{
				auto pTinptr = PTinRtiCast<TypeInfoPointer *>(pTin);
				pTinenum = PTinRtiCast<TypeInfoEnum *>(pTinptr->m_pTin);
			}

			if (pTinenum)
			{
				// check for individual fflag setting (ie. fdir.m_left = true)
				if (pTinenum->m_enumk == ENUMK_FlagEnum)
						return IVALK_LValue;
				return IVALK_RValue;
			}
		}
	}

	if (pStnod->m_pTin && pStnod->m_pTin->m_tink == TINK_Literal)
		return IVALK_RValue;

	auto pSym = pStnod->PSym();
	if (!pSym)
	{
		MOE_ASSERT(pStnod->m_park != PARK_Identifier, "Expected identifiers to have symbol");
		return IVALK_RValue;
	}
	return IvalkFromSym(pSym);
}

InString IstrFromIvalkStnod(Alloc * pAlloc, STNode * pStnod)
{
	switch (pStnod->m_park)
	{
	case PARK_MemberLookup:
		{
			Moe::StringEditBuffer seb(pAlloc);
			STNode * pStnodLhs = pStnod->PStnodChildSafe(0);
			STNode * pStnodRhs = pStnod->PStnodChildSafe(1);
			seb.AppendChz(IstrFromIvalkStnod(pAlloc, pStnodLhs).PChz());
			seb.AppendChz(".");
			seb.AppendChz(IstrFromIdentifier(pStnodRhs).PChz());
			return IstrIntern(seb.PChz());
		}
	case PARK_Identifier:
		return IstrFromIdentifier(pStnod);
	case PARK_ProcedureCall:
		{
			Moe::StringEditBuffer seb(pAlloc);
			seb.AppendChz("Procedure Call");
			STNode * pStnodName = pStnod->PStnodChildSafe(0);
			if (pStnodName && pStnodName->m_park == PARK_Identifier)
			{
				seb.AppendChz(" '");
				seb.AppendChz(IstrFromIdentifier(pStnodName).PChz());
				seb.AppendChz("'");
			}
			return IstrIntern(seb.PChz());
		}
	default:
		break;
	}

	return InString();
}

bool FVerifyIvalk(TypeCheckWorkspace * pTcwork, STNode * pStnod, IVALK ivalkExpected)
{
	auto ivalkActual = IvalkCompute(pStnod);
	if (ivalkActual < ivalkExpected)
	{
		const char * pChzIvalk = PChzFromIvalk(ivalkExpected);
		InString istrLhs = IstrFromIvalkStnod(pTcwork->m_pAlloc, pStnod);
		InString istrTin = IstrFromTypeInfo(pStnod->m_pTin);
		EmitError(pTcwork, pStnod->m_lexsp, ERRID_IncorrectIvalk, 
			"'%s%s%s' is not a valid %s", 
			istrLhs.PChz(), 
			(istrLhs.FIsEmpty()) ? "" : ": ",
			istrTin.PChz(), 
			pChzIvalk);

		return false;
	}

	return true;
}

bool FLiteralsAreSame(STNode * pStnodA, STNode * pStnodB)
{
	TypeInfoLiteral * pTinlitA = (TypeInfoLiteral *)pStnodA->m_pTin;
	TypeInfoLiteral * pTinlitB = (TypeInfoLiteral *)pStnodB->m_pTin;

	if (pTinlitA->m_litty.m_litk != pTinlitB->m_litty.m_litk)
		return false;

	if (pTinlitA->m_litty.m_litk == LITK_Compound)
	{
#if MOEB_LATER
		if (pTinlitA->m_pTinSource != pTinlitB->m_pTinSource)
			return false;

		auto pStdeclA = PStnodRtiCast<STDecl *>(pStnodA);
		auto pStdeclB = PStnodRtiCast<STDecl *>(pStnodB);
		if (!MOE_FVERIFY(pStdeclA && pStdeclA->m_pStnodInit, "compound literal with no values") ||
			!MOE_FVERIFY(pStdeclB && pStdeclB->m_pStnodInit, "compound literal with no values"))
		{
			return false;
		}

		auto pStnodListA = pStdeclA->m_pStnodInit;
		auto pStnodListB = pStdeclB->m_pStnodInit;

		if (pStnodListA->CPStnodChild() != pStnodListB->CPStnodChild())
			return false;

		for (int ipStnod = 0; ipStnod < pStnodListA->CPStnodChild(); ++ipStnod)
		{
			if (!FLiteralsAreSame(pStnodListA->PStnodChild(ipStnod), pStnodListB->PStnodChild(ipStnod)))
				return false;
		}

		return true;
#else
		return false;
#endif
	}

	STValue * pStvalA = PStnodRtiCast<STValue *>(pStnodA);
	STValue * pStvalB = PStnodRtiCast<STValue *>(pStnodB);

	switch (pTinlitA->m_litty.m_litk)
	{
	case LITK_Numeric:
		{
			if ((pTinlitA->m_litty.m_cBit != pTinlitB->m_litty.m_cBit) | 
				(pTinlitA->m_litty.m_grfnum != pTinlitB->m_litty.m_grfnum))
				return false;
		
			return pStvalA->m_nUnsigned == pStvalB->m_nUnsigned;
		}
	case LITK_Char:
			return pStvalA->m_nUnsigned == pStvalB->m_nUnsigned;
	case LITK_String:
			return pStvalA->m_istrValue == pStvalB->m_istrValue;
	case LITK_Bool:
		return pStvalA->m_nUnsigned == pStvalB->m_nUnsigned;
	case LITK_Null:
		return true;
	case LITK_Enum:
		{
			if (pTinlitA->m_pTinSource != pTinlitB->m_pTinSource)
				return false;

			return pStvalA->m_nUnsigned == pStvalB->m_nUnsigned;
		}
	default:
		MOE_ASSERT(false, "Unhandled LITK");
	}
	return false;
}


bool FAnchorsAreSame(Anchor * pAncA, Anchor * pAncB)
{
	pAncA->AssertIsValid();
	pAncB->AssertIsValid();
	if (pAncA->m_pTin != nullptr)
	{
		if (!FTypesAreSame(pAncA->m_pTin, pAncB->m_pTin))
			return false;
	}

	auto pStnodA = pAncA->m_pStnodBaked;
	if (pStnodA != nullptr)
	{
		auto pStnodB = pAncB->m_pStnodBaked;
		if (pStnodB == nullptr)
			return false;

		//check if the constant value of two syntax trees are equal
		bool fIsLiteralA = pStnodA->m_pTin && pStnodA->m_pTin->m_tink == TINK_Literal;
		bool fIsLiteralB = pStnodB->m_pTin && pStnodB->m_pTin->m_tink == TINK_Literal;
		
		if (fIsLiteralA != fIsLiteralB)
			return false;
		if (fIsLiteralA)
		{
			return FLiteralsAreSame(pStnodA, pStnodB);
		}
	}

	return true;
}

bool FTypesAreSame(TypeInfo * pTinLhs, TypeInfo * pTinRhs)
{
	if (pTinLhs == pTinRhs)
		return true;

	if (!pTinLhs || !pTinRhs || pTinLhs->m_tink != pTinRhs->m_tink)
		return false;
	
	switch(pTinLhs->m_tink)
	{
		// BB - We'll need to be a bit more explicit here if we're going to support some kind of explicit typedefs
	case TINK_Numeric:	return ((TypeInfoNumeric *)pTinLhs)->m_cBit == ((TypeInfoNumeric *)pTinRhs)->m_cBit;
		{
			auto pTinnLhs = (TypeInfoNumeric *)pTinLhs;
			auto pTinnRhs = (TypeInfoNumeric *)pTinRhs;
			return (pTinnLhs->m_cBit == pTinnRhs->m_cBit) & (pTinnLhs->m_grfnum == pTinnRhs->m_grfnum);
		}
	case TINK_Qualifier:
		{
			auto pTinqualLhs = (TypeInfoQualifier *)pTinLhs;
			auto pTinqualRhs = (TypeInfoQualifier *)pTinRhs;
			return pTinqualLhs->m_grfqualk == pTinqualRhs->m_grfqualk &&
				FTypesAreSame(pTinqualLhs->m_pTin, pTinqualRhs->m_pTin);
		}
	case TINK_Pointer:	
		{
			auto pTinptrLhs = (TypeInfoPointer *)pTinLhs;
			auto pTinptrRhs = (TypeInfoPointer *)pTinRhs;
			if (pTinptrLhs->m_fIsImplicitRef != pTinptrRhs->m_fIsImplicitRef)
				return false;

			return FTypesAreSame(pTinptrLhs->m_pTin, pTinptrRhs->m_pTin);
		}
	case TINK_Array:	
		{
			auto pTinaryLhs = (TypeInfoArray *)pTinLhs;
			auto pTinaryRhs = (TypeInfoArray *)pTinRhs;
			MOE_ASSERT(pTinaryLhs->m_pStnodBakedDim == nullptr && pTinaryRhs->m_pStnodBakedDim == nullptr, 
				"generic array should be instantiated before calling FTypesAreSame()");

			return (pTinaryLhs->m_aryk == pTinaryRhs->m_aryk) & (pTinaryLhs->m_c == pTinaryRhs->m_c) &&
				FTypesAreSame(pTinaryLhs->m_pTin, pTinaryRhs->m_pTin);
		}
	case TINK_Struct:
		{
			auto pTinstructLhs = (TypeInfoStruct *)pTinLhs;
			auto pTinstructRhs = (TypeInfoStruct *)pTinRhs;

			MOE_ASSERT(pTinstructLhs->m_pStnodStruct && pTinstructRhs->m_pStnodStruct,
				"struct AST should be instantiated before checking FTypesAreSame()");

			return pTinstructLhs->m_pStnodStruct == pTinstructRhs->m_pStnodStruct;
		}
	case TINK_Enum:
		{
			// if we're not the same enum, return false
			return false;
		}
	case TINK_Procedure:
		{
			auto pTinprocLhs = (TypeInfoProcedure *)pTinLhs;
			auto pTinprocRhs = (TypeInfoProcedure *)pTinRhs;
			if (pTinprocLhs->m_arypTinParams.C() != pTinprocRhs->m_arypTinParams.C() ||
				pTinprocLhs->m_arypTinReturns.C() != pTinprocRhs->m_arypTinReturns.C() ||
				pTinprocLhs->m_grftinproc != pTinprocRhs->m_grftinproc)
				return false;

			TypeInfo ** ppTinLhs = pTinprocLhs->m_arypTinParams.A();
			TypeInfo ** ppTinRhs = pTinprocRhs->m_arypTinParams.A();
			for (TypeInfo ** ppTinLhsMax = pTinprocLhs->m_arypTinParams.PMac() ; ppTinLhs != ppTinLhsMax; ++ppTinLhs, ++ppTinRhs)
			{
				if (!FTypesAreSame(*ppTinLhs, *ppTinRhs))
					return false;
			}
			ppTinLhs = pTinprocLhs->m_arypTinReturns.A();
			ppTinRhs = pTinprocRhs->m_arypTinReturns.A();
			for (TypeInfo ** ppTinLhsMax = pTinprocLhs->m_arypTinReturns.PMac() ; ppTinLhs != ppTinLhsMax; ++ppTinLhs, ++ppTinRhs)
			{
				if (!FTypesAreSame(*ppTinLhs, *ppTinRhs))
					return false;
			}
			return true;
		}
	case TINK_Anchor:
		{
			return pTinLhs->m_istrName == pTinRhs->m_istrName;
		}
	default:			
		MOE_ASSERT(false, "unhandled TINK in FTypesAreSame (TINK_%s, %d)", PChzFromTink(pTinLhs->m_tink), pTinLhs->m_tink);
		return false;
	}

}

bool FIsGenericType(TypeInfo * pTin)
{
	// This may happen with a type that's not done typechecking...
	if (!MOE_FVERIFY(pTin != nullptr, "null type in FIsGenericType"))
		return false;

	switch (pTin->m_tink)
	{
	case TINK_Anchor:
		return true;
    case TINK_Numeric:
    case TINK_Bool:
	case TINK_Literal:
    case TINK_Null:
    case TINK_Enum:
    case TINK_Any:
    case TINK_Void:
	case TINK_Type:
    	return false;
    case TINK_Pointer: 		return FIsGenericType(((TypeInfoPointer *)pTin)->m_pTin);
	case TINK_Qualifier:	return FIsGenericType(((TypeInfoQualifier *)pTin)->m_pTin);
    case TINK_Array:
		{
			auto pTinary = (TypeInfoArray *)pTin;		
			if (pTinary->m_pStnodBakedDim)
				return true;

			return FIsGenericType(pTinary->m_pTin);
		}
    case TINK_Procedure:
    	{
    		auto pTinproc = (TypeInfoProcedure *)pTin;
    		return pTinproc->FHasGenericArgs();
    	}
    case TINK_Struct:
    	{
    		auto pTinstruct = (TypeInfoStruct *)pTin;
			Moe::CAry<TypeStructMember>	m_aryTypemembField;

			return pTinstruct->FHasGenericParams();
    	}
	default:
		MOE_ASSERT(false, "unhandled TINK");
		return false;
	}
}

static inline Symbol * PSymRemapGeneric(
	TypeCheckWorkspace * pTcwork,
	Symbol * pSymSrc,
	Moe::CHash<Symbol *, Symbol *> * pmpPSymGenericPSymRemapped)
{
	if (!pSymSrc)
		return nullptr;

	Symbol ** ppSymRemapped = pmpPSymGenericPSymRemapped->Lookup(pSymSrc);
	if (ppSymRemapped)
	{
		return *ppSymRemapped;
	}
	return pSymSrc;
}

void CopySymbolsForGenericInstantiation(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtabSrc, 
	SymbolTable * pSymtabNew, 
	GenericMap * pGenmap, 
	Moe::CHash<Symbol *, STNode *> * pmpPSymSrcPStnodValue,
	Moe::CHash<Symbol *, Symbol *> * pmpPSymGenericPSymRemapped,
	Moe::CHash<STNode *, STNode *> * pmpPStnodGenPStnodNew)
{
	// create a duplicate copy of the symbols in this table 
	//  (Even if they aren't generic symbols they need a pStnodDefinition pointer that points into the instantiated 
	//  Syntax tree)

	Moe::CHash<InString, Symbol *>::CIterator iterSrc(&pSymtabSrc->m_hashIstrPSym);
	Symbol ** ppSymSrc;
	while ((ppSymSrc = iterSrc.Next()))
	{
		Symbol * pSymSrc = *ppSymSrc;
		MOE_ASSERT(pSymSrc->m_pSymPrev == nullptr, "not handing shadowed symbols"); // see PSymtabCopy

		if (MOE_FVERIFY(pSymSrc->m_pStnodDefinition, "symbol without defining syntax tree node"))
		{
			auto pAnc = pGenmap->PAncLookup(pSymSrc->m_istrName);
			if (pAnc && pAnc->m_genk == GENK_Value)
			{
				MOE_ASSERT(pAnc->m_pStnodBaked, "expected baked value (not type)");
				pmpPSymSrcPStnodValue->Insert(pSymSrc, pAnc->m_pStnodBaked);
					continue;

			}
		}

		auto pSymNew = pSymtabNew->PSymEnsure(
			pTcwork->m_pErrman,
			pSymSrc->m_istrName,
			pSymSrc->m_pStnodDefinition,
			pSymSrc->m_grfsym);

		auto ppStnodCopy = pmpPStnodGenPStnodNew->Lookup(pSymNew->m_pStnodDefinition);
		if (!ppStnodCopy)
		{
			EmitError(pTcwork, pSymNew->m_pStnodDefinition->m_lexsp, ERRID_MissingSymbolDef,
				"cannot look up definition stNode for symbol %s", 
				pSymNew->m_istrName.PChz());
		}
		else
		{
			pSymNew->m_pStnodDefinition = *ppStnodCopy;
		}

		auto pTinSymSrc = PTinFromSymbol(pSymSrc);
		if (pTinSymSrc)
		{
			auto pTinSymNew = PTinSubstituteGenerics(
								pTcwork,
								pSymtabNew,
								pSymSrc->m_pStnodDefinition->m_lexsp,
								pTinSymSrc,
								pGenmap,
								ERREP_ReportErrors);

			MOE_ASSERT(pTinSymNew = PTinFromSymbol(pSymNew), "bad sym type mapping");
		}
		pmpPSymGenericPSymRemapped->Insert(pSymSrc, pSymNew);
	}
}

void RemapUsingArray(
	TypeCheckWorkspace * pTcwork, 
	Moe::CHash<STNode *, STNode *> * pmpPStnodGenPStnodNew,
	Moe::CHash<SymbolTable *, SymbolTable *> * pmpPSymtabSrcPSymtabNew,
	SymbolTable * pSymtabSrc, 
	SymbolTable * pSymtabNew)
{
	MOE_ASSERT(pSymtabNew->m_aryUsing.FIsEmpty(), "expected new using array to be empty");
	for (int iUsing = 0; iUsing < pSymtabSrc->m_aryUsing.C(); ++iUsing)
	{
		auto pUsing = &pSymtabSrc->m_aryUsing[iUsing];
		SymbolTable * pSymtabAdj = pUsing->m_pSymtab;

		// This is tricky because the using might list a pSymtab inside a type that was instantiated while instantiateing this
		//  generic, but not the generic itself, ie. a procedure argument marked as 'using' with a generic type is not part of 
		//  the instantiation of this procedure but needs to remap the argument's using list

		STNode * pStnodUsing = pUsing->m_pStnod;
		SymbolTable * pSymtabSrcType = PSymtabFromType(pTcwork, pUsing->m_pStnod->m_pTin, pStnodUsing->m_lexsp);
		MOE_ASSERT(pSymtabSrcType == pUsing->m_pSymtab, "expected mapping failed");

		STNode * pStnodAdj = pUsing->m_pStnod;
		STNode ** ppStnodAdj = pmpPStnodGenPStnodNew->Lookup(pStnodUsing);
		if (ppStnodAdj)
		{
			pStnodAdj = *ppStnodAdj;
			pSymtabAdj = PSymtabFromType(pTcwork, pStnodAdj->m_pTin, pStnodUsing->m_lexsp);
		}
		else
		{
			SymbolTable ** ppSymtabLookup = pmpPSymtabSrcPSymtabNew->Lookup(pSymtabAdj);
			if (ppSymtabLookup)
			{
				pSymtabAdj = *ppSymtabLookup;
			}
		}

		pSymtabNew->AddUsingScope(pTcwork->m_pErrman, pSymtabAdj, pStnodAdj);
	}
}

void RemapGenericStnodCopy(
	TypeCheckWorkspace * pTcwork,
	STNode * pStnodGen,
	STNode * pStnodNew,
	int iInsreq,
	GenericMap * pGenmap,
	Moe::CHash<Symbol *, Symbol *> * pmpPSymGenericPSymRemapped,
	Moe::CHash<Symbol *, STNode *> * pmpPSymSrcPStnodConstant,
	Moe::CHash<STNode *, STNode *> * pmpPStnodGenPStnodNew,
	SymbolTable * pSymtabSrc,
	SymbolTable * pSymtabNew)
{
	CDynAry<STNode *> arypStnodStackGen(pTcwork->m_pAlloc, BK_TypeCheckGenerics);
	CDynAry<STNode *> arypStnodStackNew(pTcwork->m_pAlloc, BK_TypeCheckGenerics);
	arypStnodStackGen.Append(pStnodGen);
	arypStnodStackNew.Append(pStnodNew);

	CHash<SymbolTable *, SymbolTable *>	mpPSymtabSrcPSymtabNew(pTcwork->m_pAlloc, BK_TypeCheckGenerics);
	mpPSymtabSrcPSymtabNew.Insert(pSymtabSrc, pSymtabNew);

	struct SUsingRemap // tag = usrem
	{
		STNode * m_pStnodGen;
		STNode * m_pStnodNew;
	};
	CHash<SymbolTable *, SUsingRemap>	mpPSymtabUsrem(pTcwork->m_pAlloc, BK_TypeCheckGenerics);

	while (arypStnodStackGen.C())
	{
		MOE_ASSERT(arypStnodStackNew.C(),"remap stack mismatch!");
		auto pStnodGen	= arypStnodStackGen.TPopLast();
		auto pStnodNew	= arypStnodStackNew.TPopLast();

		auto pSymPrev = pStnodNew->PSym();
		if (pSymPrev)
		{
			bool fHasSymbolTin = pStnodNew->m_pTin == PTinFromSymbol(pSymPrev);
			auto pSymNew = PSymRemapGeneric(pTcwork, pSymPrev, pmpPSymGenericPSymRemapped);
			if (fHasSymbolTin)
			{
				pStnodNew->m_pTin = PTinFromSymbol(pSymNew);
			}

			pStnodNew->m_pSymbase = pSymNew;
		}
		else if (pStnodNew->m_pTin && FIsGenericType(pStnodNew->m_pTin))
		{
			 auto pTinRemapped = PTinSubstituteGenerics(pTcwork, pSymtabNew, pStnodNew->m_lexsp, pStnodNew->m_pTin, pGenmap, ERREP_ReportErrors);
			 if (pTinRemapped)
			 {
				 pStnodNew->m_pTin = pTinRemapped;
			 }
			 else
			{
				auto istrTin = IstrFromTypeInfo(pStnodNew->m_pTin);
				MOE_ASSERT(false, "pTin was not remapped. (no symbol, type = %s)", istrTin.PChz());
			}
		}

#if TRACK_IINSREQ
		MOE_ASSERT(pStnodNew->m_iInsreq < 0, "pStnod is already remapped?!?");
		pStnodNew->m_iInsreq = iInsreq;
#endif

		MOE_ASSERT(pStnodNew->CPStnodChild() == pStnodGen->CPStnodChild(), "copy child mismatch");
		for (int iStnod = 0; iStnod < pStnodNew->CPStnodChild(); ++iStnod)
		{
			auto pStnodChildGen = pStnodGen->PStnodChild(iStnod);
			auto pStnodChildNew = pStnodNew->PStnodChild(iStnod);

			Symbol * pSymGen = pStnodChildNew->PSym();
			if (!pSymGen && pStnodChildGen->m_park == PARK_Identifier)
			{
				InString istrIdent = IstrFromIdentifier(pStnodChildGen);
				pSymGen = pSymtabSrc->PSymLookup(istrIdent, pStnodChildGen->m_lexsp, FSYMLOOK_Local);
			}

			if (pSymGen)
			{
				STNode ** ppStnodBaked = pmpPSymSrcPStnodConstant->Lookup(pSymGen);
				if (ppStnodBaked)
				{
					auto pStnodCopy = PStnodCopy(pTcwork->m_pAlloc, *ppStnodBaked);

					pStnodNew->ReplaceChild(pStnodChildNew, pStnodCopy);

					// Don't delete this stnod - it's children may be referenced by another generic parameter.
					//   ie ($BAKE: $T)
					pGenmap->m_aryPStnodManaged.Append(pStnodChildNew);
					continue;
				}
			}

			arypStnodStackGen.Append(pStnodChildGen);
			arypStnodStackNew.Append(pStnodChildNew);
		}

		if (pStnodNew->m_pSymtab)
		{
			// replace any symbol tables that are (or are descended from) the generic symbol table
			SymbolTable * pSymtabParentNew = nullptr;
			SymbolTable * pSymtabIt = pStnodNew->m_pSymtab;
			int cCopy = 0;
			while (pSymtabIt)
			{
				auto ppSymtabNew = mpPSymtabSrcPSymtabNew.Lookup(pSymtabIt);
				if (ppSymtabNew)
				{
					pSymtabParentNew = *ppSymtabNew;
					break;
				}

				++cCopy;
				pSymtabIt = pSymtabIt->m_pSymtabParent;
			}

			if (pSymtabParentNew)
			{
				SymbolTable * pSymtabCopySrc = pStnodNew->m_pSymtab;
				SymbolTable ** ppSymtabNew = &pStnodNew->m_pSymtab;
				for (int iCopy = 0; iCopy < cCopy; ++iCopy)
				{
					auto pSymtabNew = PSymtabNew(pTcwork->m_pAlloc, pSymtabCopySrc, pSymtabCopySrc->m_istrNamespace);	
					MOE_ASSERT(pSymtabCopySrc->m_aryUsing.FIsEmpty(), "TBD - unmapped using");

					*ppSymtabNew = pSymtabNew;
					ppSymtabNew = &pSymtabNew->m_pSymtabParent;

					CopySymbolsForGenericInstantiation(
						pTcwork,
						pSymtabCopySrc,
						pSymtabNew,
						pGenmap,
						pmpPSymSrcPStnodConstant,
						pmpPSymGenericPSymRemapped,
						pmpPStnodGenPStnodNew);
				}

				*ppSymtabNew = pSymtabParentNew;
			}
			if (pStnodGen->m_pSymtab && !pStnodGen->m_pSymtab->m_aryUsing.FIsEmpty())
			{
				SUsingRemap * pUsrem = nullptr;
				INRES inres = mpPSymtabUsrem.InresEnsureKey(pStnodGen->m_pSymtab, &pUsrem);
				if (inres == INRES_Inserted)
				{
					pUsrem->m_pStnodGen = pStnodGen;
					pUsrem->m_pStnodNew = pStnodNew;
				}
			}
		}
	}
	
	CHash<SymbolTable *, SUsingRemap>::CIterator iter(&mpPSymtabUsrem);
	while (SUsingRemap * pUsremIt = iter.Next())
	{
		SymbolTable * pSymtabUsingSrc = pUsremIt->m_pStnodGen->m_pSymtab;
		if (!MOE_FVERIFY(pSymtabUsingSrc, "bad using remap entry"))
			continue;

		SymbolTable * pSymtabUsingNew = pUsremIt->m_pStnodNew->m_pSymtab;
		MOE_ASSERT(pSymtabUsingNew != nullptr, "new stnod has no symbol table");

		// BB - this is here' because we're remapping a using table several times! need a hash so aryUSrem are unique
		MOE_ASSERT(pSymtabUsingNew->m_aryUsing.FIsEmpty(), "remapping the using array multiple times");
		//if (pSymtabUsingNew->m_aryUsing.C())
		//	continue;

		RemapUsingArray(pTcwork, pmpPStnodGenPStnodNew, &mpPSymtabSrcPSymtabNew, pSymtabUsingSrc, pSymtabUsingNew);
	}
}

TypeInfoStruct * PTinstructEnsureUniqueInstance(
	TypeCheckWorkspace * pTcwork,
	TypeInfoStruct * pTinstruct)
{
	STNode * pStnodInstFrom = nullptr;
	if (pTinstruct->m_pTinstructInstFrom)
	{
		pStnodInstFrom = pTinstruct->m_pTinstructInstFrom->m_pStnodStruct;
	}
	MOE_ASSERT(pStnodInstFrom && pTinstruct->m_pGenmap, "expected generic structure with instanced AST");
	auto pEntry = pTcwork->m_genreg.PEntryEnsure(pStnodInstFrom, pTinstruct->m_pGenmap);
	if (pEntry->m_pTin == nullptr)
	{
		pEntry->m_pTin = pTinstruct;
		return pTinstruct;
	}

	return PTinDerivedCast<TypeInfoStruct *>(pEntry->m_pTin);
}

InstantiateRequest * PInsreqLookup(
	TypeCheckWorkspace * pTcwork,
	STNode * pStnodDefinition,
	GenericMap * pGenmap)
{
	auto pEntry = pTcwork->m_genreg.PEntryLookup(pStnodDefinition, pGenmap);
	if (pEntry)
	{
		return pEntry->m_pInsreq;
	}
	return nullptr;
}

void GenericRegistry::Cleanup()
{
	Moe::CHash<STNode *, EntryBlock *>::CIterator iter(&m_mpStnodInstFromBlock);

	EntryBlock ** ppBlock;
	while ((ppBlock = iter.Next()))
	{
		m_pAlloc->MOE_DELETE(*ppBlock);
	}

	m_aryInsreq.Clear();
}

GenericRegistry::Entry * GenericRegistry::PEntryLookup(STNode * pStnodInstFrom, GenericMap * pGenmap)
{
	EntryBlock ** ppBlock = m_mpStnodInstFromBlock.Lookup(pStnodInstFrom);
	if (ppBlock == nullptr)
		return nullptr;

	EntryBlock * pBlock = *ppBlock;
	auto pEntryMac = pBlock->m_aryEntry.PMac();
	for (auto pEntryIt = pBlock->m_aryEntry.A(); pEntryIt != pEntryMac; ++pEntryIt)
	{
		auto pGenmapKey = pEntryIt->m_pGenmapKey;
		if (pGenmapKey == pGenmap)
			return pEntryIt;

		if (pGenmap->m_mpIstrAnc.C() != pGenmapKey->m_mpIstrAnc.C())
			continue;

		Moe::CHash<InString, Anchor>::CIterator iterIt(&pGenmapKey->m_mpIstrAnc);
		Moe::CHash<InString, Anchor>::CIterator iterArg(&pGenmap->m_mpIstrAnc);

		// NOTE: This generic registry is used to avoid endless looping while instantiating generic types!
		//  Sidestepping it will cause the compiler to crash.

		bool fAreTheSame = true;
		InString * pIstrIt;
		InString * pIstrArg;
		Anchor * pAncIt;
		Anchor * pAncArg;
		while ((pAncIt = iterIt.Next(&pIstrIt)))
		{
			pAncArg = iterArg.Next(&pIstrArg);
			if (!MOE_FVERIFY(pAncArg, "hash value mismatch"))
				break;	

			fAreTheSame &= (*pIstrIt == *pIstrArg && FAnchorsAreSame(pAncIt, pAncArg));
			if (!fAreTheSame)
				break;
		}

		if (fAreTheSame)
		{
			return pEntryIt;
		}
	}

	return nullptr;
}

GenericRegistry::Entry * GenericRegistry::PEntryEnsure(STNode * pStnodInstFrom, GenericMap * pGenmap)
{
	// if this double lookup is a perf problem we could inline the lookup here, and just duplicate a bunch of code
	auto pEntry = PEntryLookup(pStnodInstFrom, pGenmap);
	if (pEntry)
		return pEntry;

	EntryBlock ** ppBlock;
	INRES inres = m_mpStnodInstFromBlock.InresEnsureKey(pStnodInstFrom, &ppBlock);
	if (inres == INRES_Inserted)
	{
		*ppBlock = MOE_NEW(m_pAlloc, EntryBlock) EntryBlock(m_pAlloc);
	}

	// we don't need to search for a matching entry, it would have been found by pEntryLookup
	auto pEntryNew = (*ppBlock)->m_aryEntry.AppendNew();
	pEntryNew->m_pGenmapKey = pGenmap;
	return pEntryNew;
}

InstantiateRequest * GenericRegistry::PInsreqNew(STNode * pStnodInstFrom, GenericMap * pGenmap)
{
	auto pEntry = PEntryEnsure(pStnodInstFrom, pGenmap);
	MOE_ASSERT(pEntry->m_pInsreq == nullptr, "insreq was already registered");

	int iInsreq = (int)m_aryInsreq.C();
	auto pInsreqNew = m_aryInsreq.AppendNew();
	pInsreqNew->m_iInsreq = iInsreq;
	pEntry->m_pInsreq = pInsreqNew;
	return pInsreqNew;
}

TypeRegistry::TypeRegistry(Moe::Alloc * pAlloc)
:m_pAlloc(pAlloc)
,m_hashHvPTinUnique(pAlloc, Moe::BK_TypeRegistry, 0)
{
}

void TypeRegistry::Clear()
{
	m_hashHvPTinUnique.Clear(0);
}

#ifdef MOEB_LATER
static inline u64 HvForPTin(TypeInfo * pTin, TINK tink, u8 other = 0)
{
	MOE_ASSERT(!pTin->m_strDesc.FIsEmpty(), "expected descriptor string");
	return u64(u64(pTin->m_strDesc.Hv()) | (u64(tink) << 32) | (u64(other) << 40));
}

static inline u64 HvForPTin(const CString & str, TINK tink)
{
	// really not needed, should switch back to just a string HV
	return u64(str.Hv() ^ (u64(tink) << 56));
}
#endif

TypeInfo * TypeRegistry::PTinMakeUnique(TypeInfo * pTin)
{
#ifndef MOEB_LATER
	//MOE_ASSERT(false, "type registry TBD");
	return pTin;
#else
	// NOTES ON UNIQUE TYPES (to avoid me getting it wrong again)
	// named types are unique'd based on name + definition namespace + parameters (if needed)
	//    not: based on type footprint ala LLVM type

	// unnamed types (pointer, array, qualifier) are named relative to contained types

	if (pTin->m_grftin.FIsSet(FTIN_IsUnique))
		return pTin;

	u64 hv;
	switch (pTin->m_tink)
	{
	case TINK_Literal:
	case TINK_Generic:
	case TINK_Type:
		// these types are not 'unique'd
		return pTin;
#if 0 // probably not worth the hassle here
	case TINK_Pointer:
		{
			auto pTinptr = (TypeInfoPointer *)pTin;
			auto pTinPointedTo = PTinMakeUnique(pTinptr->m_pTin);
			pTinptr->m_pTin = pTinPointedTo;

			hv = HvForPTin(pTinPointedTo, TINK_Pointer);
		} break;
	case TINK_Qualifier:
		{
			auto pTinqual = (TypeInfoQualifier *)pTin;
			auto pTinUnqual = PTinMakeUnique(pTinqual->m_pTin);
			pTinqual->m_pTin = pTinUnqual;

			hv = HvForPTin(pTinUnqual, TINK_Qualifier, pTinqual->m_grfqualk.m_raw);
		} break;
	case TINK_Array:
		{
			auto pTinary = (TypeInfoArray *)pTin;
			auto pTinElement = PTinMakeUnique(pTinary->m_pTin);
			pTinary->m_pTin = pTinElement;

			if (pTinary->m_aryk != ARYK_Fixed)
			{
				hv = HvForPTin(pTinElement, TINK_Array, u8(pTinary->m_aryk));
			}
		} // fallthrough
#else
	case TINK_Pointer:
	case TINK_Qualifier:
	case TINK_Array:
#endif
	case TINK_Bool:
	case TINK_Void:
	case TINK_Null:
	case TINK_Any:
	case TINK_Integer:
	case TINK_Float:
	case TINK_Enum:
		{
			Moe::StringEditBuffer seb(m_pAlloc);

			seb.Clear();
			AppendTypeDescriptor(pTin, &seb);
			hv = HvForPTin(pTin->m_strDesc, pTin->m_tink);

		} break;

	case TINK_Procedure:
	case TINK_Struct:
		{
			Moe::StringEditBuffer seb(m_pAlloc);

			seb.Clear();
			AppendTypeDescriptor(pTin, &seb);
			hv = HvForPTin(pTin->m_strDesc, pTin->m_tink);
		} break;
	default:
		MOE_ASSERT(false, "unhandled type kind");
		break;
	}

	TypeInfo ** ppTin;
	FINS fins = m_hashHvPTinUnique.FinsEnsureKey(hv, &ppTin);
	if (fins == FINS_AlreadyExisted)
	{
		// NOTE: doesn't delete non-unique type!
		return *ppTin;
	}

	if (ppTin)
	{
		pTin->m_grftin.AddFlags(FTIN_IsUnique);
		if (pTin->m_pTinNative)
		{
			pTin->m_pTinNative = PTinMakeUnique(pTin->m_pTinNative);
		}

		*ppTin = pTin;

		// make sure the types referred to by this type are unique
		// NOTE: cyclical references shouldn't be a problem now that we've added this type
		switch (pTin->m_tink)
		{
		case TINK_Pointer:
			{
				auto pTinptr = (TypeInfoPointer *)pTin;
				pTinptr->m_pTin = PTinMakeUnique(pTinptr->m_pTin);
			} break;
		case TINK_Qualifier:
			{
				auto pTinqual = (TypeInfoQualifier *)pTin;
				pTinqual->m_pTin = PTinMakeUnique(pTinqual->m_pTin);
			} break;
		case TINK_Array:
			{
				auto pTinary = (TypeInfoArray *)pTin;
				pTinary->m_pTin = PTinMakeUnique(pTinary->m_pTin);
			} break;
		case TINK_Procedure:
			{
				auto pTinproc = (TypeInfoProcedure *)pTin;
				for (size_t ipTin = 0; ipTin < pTinproc->m_arypTinParams.C(); ++ipTin)
				{
					pTinproc->m_arypTinParams[ipTin] =  PTinMakeUnique(pTinproc->m_arypTinParams[ipTin]);
				}

				for (size_t ipTin = 0; ipTin < pTinproc->m_arypTinReturns.C(); ++ipTin)
				{
					pTinproc->m_arypTinReturns[ipTin] =  PTinMakeUnique(pTinproc->m_arypTinReturns[ipTin]);
				}
			} break;
		case TINK_Struct:
			{
				auto pTinstruct = (TypeInfoStruct *)pTin;
			} break;

		default:
			break;
		}
	}
	return pTin;
#endif // MOEB_LATER
}

// Are we a baked constant? and if so what type?
TypeInfo * PTinBakedConstantType(STNode * pStnod)
{
	Symbol * pSym = pStnod->PSym();
	if (!pSym || !MOE_FVERIFY(pSym->m_pStnodDefinition, "no definition?"))
		return nullptr;

	auto pStnodDef = pSym->m_pStnodDefinition;
	if (pStnodDef->m_park == PARK_Decl)
	{
		auto pStdecl = PStnodRtiCast<STDecl *>(pStnodDef);
		if (pStdecl->m_fIsBakedConstant)
			return pStnod->m_pTin;
	}

	return nullptr;
}

bool FIsCompileTimeConstant(STNode * pStnod)
{
	// This just checks for a literal now, but will need something more elaborate once
	//  compile time code execution comes online.

	TypeInfo * pTin = PTinBakedConstantType(pStnod);
	if (pTin && pTin->m_tink == TINK_Literal)
		return true;

	return false;
}

TypeCheckStackEntry * PTcsentPush(TypeCheckFrame * pTcfram, TypeCheckStackEntry ** ppTcsentTop, STNode * pStnod)
{
	if (pStnod->m_strees >= STREES_TypeChecked)
	{
		// we can have type checked nodes here when we're dealing with baked constants
		return nullptr;
	}

	// update ppTcsentTop to handle times when the dynArray reallocs.
	size_t iTcsentTop = pTcfram->m_aryTcsent.IFromP(*ppTcsentTop);

	size_t cPrev = pTcfram->m_aryTcsent.C()-1;
	TypeCheckStackEntry * pTcsent = pTcfram->m_aryTcsent.AppendNew();
	*pTcsent = pTcfram->m_aryTcsent[cPrev];

	pTcsent->m_nState = 0;
	pTcsent->m_pStnod = pStnod;

	*ppTcsentTop = &pTcfram->m_aryTcsent[iTcsentTop];
	return pTcsent;
}

void PopTcsent(TypeCheckFrame * pTcfram, TypeCheckStackEntry ** ppTcsentTop, STNode * pStnodDebug)
{
	*ppTcsentTop = nullptr;
	MOE_ASSERT(pTcfram->m_aryTcsent.PLast()->m_pStnod == pStnodDebug || pStnodDebug == nullptr, "type check entry pop mismatch");

	pTcfram->m_aryTcsent.PopLast();
}

void RelocateTcfram(
	TypeCheckFrame * pTcfram,
	CAry<TypeCheckFrame *> * parypTcframOld,
	CAry<TypeCheckFrame *> * parypTcframNew)
{
	MOE_ASSERT((*parypTcframOld)[pTcfram->m_ipTcframQueue] == pTcfram, "bookkeeping error");

	s32 cOld = (s32)parypTcframOld->C() - 1; 
	size_t ipTcfram = pTcfram->m_ipTcframQueue;
	if (cOld != ipTcfram)
	{
		TypeCheckFrame * pTcframTop = (*parypTcframOld)[cOld];
		pTcframTop->m_ipTcframQueue = ipTcfram;
		(*parypTcframOld)[ipTcfram] = pTcframTop;
	}

	parypTcframOld->PopLast();

	if (parypTcframNew)
	{
		pTcfram->m_ipTcframQueue = (s32)parypTcframNew->C();
		parypTcframNew->Append(pTcfram);
	}
}

void ValidateTcframArray(CAry<TypeCheckFrame *> * parypTcfram)
{
	for (int ipTcfram = 0; ipTcfram < (int)parypTcfram->C(); ++ipTcfram)
	{
		MOE_ASSERT((*parypTcfram)[ipTcfram]->m_ipTcframQueue == ipTcfram, "invalid type check frame array");
	}
}

inline u64 NUnsignedLiteralCast(TypeCheckWorkspace * pTcwork, const STValue * pStval)
{
	switch (pStval->m_stvalk)
	{
	case STVALK_UnsignedInt:
		return pStval->m_nUnsigned;
	case STVALK_Float:
		return (s64)pStval->m_g;
	case STVALK_SignedInt:
		{
			if (pStval->m_nSigned < 0)
			{
				EmitError(pTcwork, pStval->m_lexsp, ERRID_LiteralOutsideBounds, "Implicit cast will discard negative value");
			}
			return (u64)pStval->m_nSigned;
		}
	default:
		MOE_ASSERT(false, "bad literal cast to unsigned int");
		return 0;
	}
}

inline s64 NSignedLiteralCast(TypeCheckWorkspace * pTcwork, const STValue * pStval)
{
	switch (pStval->m_stvalk)
	{
	case STVALK_UnsignedInt:
		{
			if (pStval->m_nUnsigned > LLONG_MAX)
			{
				EmitError(pTcwork, pStval->m_lexsp, ERRID_LiteralOutsideBounds, "Literal is too large for implicit signed int cast.");
			}
			return (s64)pStval->m_nUnsigned;
		}
	case STVALK_SignedInt:
		return pStval->m_nSigned;
	case STVALK_Float:
		return (s64)pStval->m_g;
	default:
		MOE_ASSERT(false, "bad literal cast to signed int");
		return 0;
	}
}

inline f64 GLiteralCast(const STValue * pStval)
{
	switch (pStval->m_stvalk)
	{
	case STVALK_UnsignedInt:	return (f64)pStval->m_nUnsigned;
	case STVALK_SignedInt:		return (f64)pStval->m_nSigned;
	case STVALK_Float:			return pStval->m_g;
	default:					MOE_ASSERT(false, "expected number");
	}

	return 0.0;
}

void OnTypeResolve(TypeCheckWorkspace * pTcwork, const Symbol * pSym)
{
	TypeInfo * pTinSym = PTinFromSymbol(pSym);
	MOE_ASSERT(pTinSym, "expected type for completed symbol");

	// BB - could replace this with a function that 'pops' a key/value pair from the hash and save a second lookup.
	UnknownType * pUntype = pTcwork->m_hashPSymUntype.Lookup(pSym);
	if (!pUntype)
		return;

	int cTcframDependent = (s32)pUntype->m_arypTcframDependent.C();
	MOE_ASSERT(cTcframDependent > 0, "unknown type not cleaned up (empty dependent array)");

	for (int iTcfram = 0; iTcfram < cTcframDependent; ++iTcfram)
	{
		TypeCheckFrame * pTcfram = pUntype->m_arypTcframDependent[iTcfram];

		if (MOE_FVERIFY(pTcwork->m_arypTcframWaiting[pTcfram->m_ipTcframQueue] == pTcfram, "bookkeeping error (OnTypeResolve)"))
		{
			RelocateTcfram(pTcfram, &pTcwork->m_arypTcframWaiting, &pTcwork->m_arypTcframPending);
		}
	}
	pTcwork->m_hashPSymUntype.Remove(pSym);
}

TypeInfo * PTinFromTypeArgument(STNode * pStnod)
{
	if (!MOE_FVERIFY(pStnod->m_park == PARK_TypeArgument, "expected type argument"))
		return nullptr;

	auto pStnodChild = pStnod->PStnodChildSafe(0);
	if (MOE_FVERIFY(pStnodChild && pStnodChild->m_pTin, "expected type argument child"))
	{
		return pStnodChild->m_pTin;
	}
	return nullptr;
}

GenericMap * PGenmapNew(TypeCheckWorkspace * pTcwork, const char * pChzPrefix, TypeInfo * pTinOwner)
{
	auto pGenmap = MOE_NEW(pTcwork->m_pAlloc, GenericMap) GenericMap(pTcwork->m_pAlloc, pChzPrefix, pTinOwner);
	pTcwork->m_pErrman->m_pWork->m_arypGenmapManaged.Append(pGenmap);
	return pGenmap;
}

Anchor * GenericMap::PAncMapValue(const InString & istrName, STNode * pStnodBaked)
{
	Anchor * pAnc = nullptr;
	if (m_mpIstrAnc.InresEnsureKey(istrName, &pAnc) == INRES_AlreadyExisted)
	{
		MOE_ASSERT(pAnc->m_genk == GENK_Value, "expeccted value");
		MOE_ASSERT(pAnc->m_pStnodBaked == nullptr || pAnc->m_pStnodBaked == pStnodBaked, "anchored value mismatch");
	}

	pAnc->m_genk = GENK_Value;
	pAnc->m_pStnodBaked = pStnodBaked;

	if (pStnodBaked)
	{
		auto pStdecl = PStnodRtiCast<STDecl *>(pStnodBaked);
		if (pStdecl && pStdecl->m_fIsBakedConstant)
		{
			m_grftingenResult |= FTINGEN_HasBakedValueArgs;
		}
	}
	return pAnc;
}

Anchor * GenericMap::PAncMapType(const InString & istrName, TypeInfo * pTin)
{
	// set a new type if one is not already mapped 

	Anchor * pAnc = nullptr;
	if (m_mpIstrAnc.InresEnsureKey(istrName, &pAnc) == INRES_AlreadyExisted)
	{
		MOE_ASSERT(pAnc->m_genk == GENK_Type, "expeccted type");
		if (pAnc->m_pTin != nullptr)
			return pAnc;
	}

	if (pAnc->m_pTin == nullptr && pTin && FIsGenericType(pTin))
	{
		m_grftingenResult |= FTINGEN_HasBakedTypeArgs;
	}
	pAnc->m_genk = GENK_Type;
	pAnc->m_pTin = pTin;

	return pAnc;
}

//After FUnpackArgumentList
//	we have a map from iArgDefine to pArgunp (pStnod for values, null for typeArg, includes varargs at the end)
//	we have anchored ALL generic types and baked values 

inline bool FUnpackArgumentList(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtab,
	size_t cArgDefinition,
	CAry<ArgUnpack> *pmpIArgArgunp,
	GenericMap * pGenmap,
	ERREP errep,
	STNode * pStnodDefParamDeclList,
	STNode ** ppStnodCall,
	size_t cpStnodCall, 
	const LexSpan & lexsp,
	const char * pChzOwner,
	const char * pChzStructOrProc)
{
	InString istrEmpty(IstrIntern("_"));
	CDynAry<InString> mpIArgIstrName(pTcwork->m_pAlloc, BK_TypeCheckProcmatch, cArgDefinition);
	mpIArgIstrName.AppendFill(cArgDefinition, istrEmpty);

	//	for cArgDefine build array of defaults for all arguments (both generic and concrete, typearg and named)
	//   also build up an array of argument names and find generic anchor names

	int cArgNoNamed = 0;
	if (pStnodDefParamDeclList)
	{
		int cpStnodParamList = pStnodDefParamDeclList->CPStnodChild();
		cArgNoNamed = cpStnodParamList;

		for (int iArg = 0; iArg < cpStnodParamList; ++iArg)
		{
			STNode * pStnodParamDecl = pStnodDefParamDeclList->PStnodChildSafe(iArg);
			if (!pStnodParamDecl)
				continue;

			auto pStdecl = PStnodRtiCast<STDecl *>(pStnodParamDecl);
			if (!pStdecl)
				continue;

			auto pStnodInit = pStdecl->m_pStnodInit;
			if (pStnodInit)
			{
				auto pArgunp = &(*pmpIArgArgunp)[iArg];
				pArgunp->m_pStnodInit = pStnodInit;
				pArgunp->m_grfarg.AddFlags(FARG_DefaultArg);
			}
			mpIArgIstrName[iArg] = IstrFromIdentifier(pStdecl->m_pStnodIdentifier);

			if (pStdecl->m_pStnodIdentifier == nullptr)
			{
				(*pmpIArgArgunp)[iArg].m_grfarg.AddFlags(FARG_TypeArgument);
			}
			
			FindGenericAnchorNames(pTcwork->m_pAlloc, pStnodParamDecl, pGenmap);
		}
	}

	// for cArgCallRaw
	//	- set ordered arg values in argument array, including generic values, (appending varargs)
	//	- set named arg values, (including baked values by name)
	//	- anchor named generic TYPES (not generic values supplied by name)

	STNode * pStnodName = nullptr;
	bool fHasShownOrderError = false;
	for (int iArg = 0; iArg < cpStnodCall; ++iArg)
	{
		int iArgDest = iArg;
		GRFARG grfarg;
		auto pStnodExp = ppStnodCall[iArg];

		if (pStnodExp->m_park == PARK_ArgumentLabel && 
			MOE_FVERIFY(pStnodExp->CPStnodChild() == 2, "argument label node children should be (name, arg)"))
		{
			STNode * pStnodIdentifier = pStnodExp->PStnodChild(0);
			pStnodName = pStnodIdentifier;

			STNode * pStnodLabelVal = pStnodExp->PStnodChild(1);
			InString istrIdentifier(IstrFromIdentifier(pStnodIdentifier));

			int iArgNamed = -1;
			for (int iArgIt = 0; iArgIt < mpIArgIstrName.C(); ++iArgIt)
			{
				if (mpIArgIstrName[iArgIt] == istrIdentifier)
				{
					iArgNamed = iArgIt;
					break;
				}
			}

			if (iArgNamed < 0)
			{
				Anchor * pAnc = pGenmap->PAncLookup(istrIdentifier);
				if (!pAnc)
				{
					if (errep == ERREP_ReportErrors)
					{
						EmitError(pTcwork->m_pErrman, lexsp, ERRID_NamedArgumentNotFound,
							"Cannot find argument named %s for %s %s",
							istrIdentifier.PChz(),
							pChzStructOrProc,
							pChzOwner);
					}
					return false;
				}

				if (pAnc->m_genk == GENK_Value)
				{
					pAnc->m_pStnodBaked = pStnodLabelVal;
				}
				else
				{
					if (!pStnodLabelVal->m_pTin || pStnodLabelVal->m_pTin->m_tink != TINK_Type)
					{
						if (errep == ERREP_ReportErrors)
						{
							EmitError(pTcwork->m_pErrman, lexsp, ERRID_NamedArgumentNotFound,
								"expected type value for named argument '%s' in generic %s %s",
								istrIdentifier.PChz(),
								pChzStructOrProc,
								pChzOwner);
						}
						return false;
					}

					pAnc->m_pTin = PTinFromTypeArgument(pStnodLabelVal);
				}
				continue;
			}
			else
			{
				iArgDest = iArgNamed;
				grfarg.AddFlags(FARG_NamedLabelChild);
			}
		}
		else if (pStnodName)
		{
			if (errep == ERREP_ReportErrors)
			{
				InString istrIdentifier(IstrFromIdentifier(pStnodName));
				EmitError(pTcwork->m_pErrman, lexsp, ERRID_OrderedAfterNamed,
					"ordered argument %d must come before named argument '%s' in %s %s",
					iArg + 1,
					istrIdentifier.PChz(),
					pChzStructOrProc,
					pChzOwner);
			}

			return false;
		}

		if (iArgDest >= pmpIArgArgunp->CMax())
		{
			if (errep == ERREP_ReportErrors)
			{
				EmitError(pTcwork->m_pErrman, lexsp, ERRID_TooManyArgs,
					"Too many arguments passed to %s %s",
					pChzStructOrProc,
					pChzOwner);
			}

			return false;
		}

		grfarg.AddFlags((*pmpIArgArgunp)[iArgDest].m_grfarg.m_raw & GRFARG_DefinitionFlags);

		cArgNoNamed = moeMax(cArgNoNamed, iArgDest+1);
		auto pArgunpDest = &(*pmpIArgArgunp)[iArgDest];
		if (pArgunpDest->m_pStnodInit != nullptr && !pArgunpDest->m_grfarg.FIsSet(FARG_DefaultArg))
		{
			if (errep == ERREP_ReportErrors)
			{
				EmitError(pTcwork->m_pErrman, lexsp, ERRID_ArgumentSuppliedTwice,
					"Argument %d '%s' to %s %s was supplied twice: as an ordered argument and specified by name.",
					iArgDest + 1,
					mpIArgIstrName[iArgDest].PChz(),
					pChzStructOrProc,
					pChzOwner);
			}
			return false;
		}

		MOE_ASSERT(pStnodExp, "argument specifies no stnode?")
		pArgunpDest->m_pStnodInit = pStnodExp; 
		pArgunpDest->m_grfarg = grfarg;
	}

	//for (cArgDefine)
	//	- anchor typeArgs supplied in order
	//	- infer generic types anchors ordered values in mpIArgArgunp
	if (pStnodDefParamDeclList)
	{
		int cpStnodParamList = pStnodDefParamDeclList->CPStnodChild();
		for (int iArg = 0; iArg < cpStnodParamList; ++iArg)
		{
			STNode * pStnodParamDef = pStnodDefParamDeclList->PStnodChildSafe(iArg);
			if (!pStnodParamDef)
				continue;

			auto pStdecl = PStnodRtiCast<STDecl *>(pStnodParamDef);
			if (!pStdecl)
				continue;

			auto pArgunp = &(*pmpIArgArgunp)[iArg];
			auto pStnodInit = pArgunp->m_pStnodInit;
			if (pStdecl->m_fIsBakedConstant)
			{
				if (pStnodInit->m_park == PARK_Decl)
				{
					// the passed argument is a 'decl' if we're aliasing a generic with another (more specified?) generic
					//  ie.  `CFixedIntArray typedef CFixedAry($C, :int)`

					auto pStdeclInit = PStnodRtiCast<STDecl *>(pStnodInit);
					MOE_ASSERT(pStdeclInit && pStdeclInit->m_fIsBakedConstant, "unexpected 'decl' arg");
				}
				else if (!FIsCompileTimeConstant(pStnodInit))
				{
					if (errep == ERREP_ReportErrors)
					{
						EmitError(pTcwork->m_pErrman, pStnodParamDef->m_lexsp, ERRID_BakingNonLiteralValue,
							"passing non-constant to argument %d of %s '%s'. '$%s' must be a compile-time constant",
							iArg + 1,
							pChzStructOrProc,
							pChzOwner,
							mpIArgIstrName[iArg].PChz());
					}
					return PROCMATCH_None;
				}

				auto pStnodBaked = pStnodInit;
				if (pArgunp->m_grfarg.FIsSet(FARG_NamedLabelChild))
				{
					MOE_ASSERT(pStnodBaked->m_park == PARK_ArgumentLabel, "expected argument label");
					pStnodBaked = pStnodBaked->PStnodChildSafe(1);
				}

				pArgunp->m_grfarg.AddFlags(FARG_BakedValue);
				pGenmap->PAncMapValue(mpIArgIstrName[iArg], pStnodBaked);
			}

			if (pStdecl->m_pStnodIdentifier == nullptr)
			{
				if (!pStnodInit)
					continue;

				auto pTinRaw = pStnodInit->m_pTin;
				if (pTinRaw->m_tink != TINK_Type)
				{
					if (errep == ERREP_ReportErrors)
					{
						// BB - should throw this error for default arguments (rather than only when the default is used)
						auto pStnodType = pStdecl->m_pStnodType;
						const char * pChzTypeArg = (pStnodType && pStnodType->PSym()) ? pStnodType->PSym()->m_istrName.PChz() : "unknown";
						InString istrType = IstrFromTypeInfo(pStnodInit->m_pTin);

						EmitError(pTcwork->m_pErrman, pStnodInit->m_lexsp, ERRID_BakingNonLiteralValue,
							"expected type for type argument $%s, encountered '%s'",
							pChzTypeArg,
							istrType.PChz());
					}
					return false;
				}

				auto pStnodChild = pStnodInit->PStnodChildSafe(0);
				if (MOE_FVERIFY(pStnodChild && pStnodChild->m_pTin, "expected type argument child"))
				{
					pStnodInit = pStnodChild;
				}
			}
			else if (pStnodInit && pStnodInit->m_pTin && pStnodInit->m_pTin->m_tink == TINK_Type)
			{
				// type passed into arg that is not a typearg
				if (errep == ERREP_ReportErrors)
				{
					// BB - should throw this error for default arguments (rather than only when the default is used)
					auto pTinArg =  PTinFromTypeArgument(pStnodInit);
					InString istrType = IstrFromTypeInfo(pTinArg);

					EmitError(pTcwork->m_pErrman, pStnodInit->m_lexsp, ERRID_BakingNonLiteralValue,
						"expected value for argument %s, but encountered type :%s",
						mpIArgIstrName[iArg].PChz(),
						istrType.PChz());
				}

				return false;
			}

			if (pArgunp->m_pStnodInit == nullptr)
			{
				// type arguments may have been supplied by name, missing anchors will be report errors later.
				if (pArgunp->m_grfarg.FIsSet(FARG_TypeArgument))
					continue;

				if (errep == ERREP_ReportErrors)
				{
					EmitError(pTcwork->m_pErrman, lexsp, ERRID_TooFewArgs,
						"Too few arguments to %s '%s'. cannot find value for parameter #%d: '%s'",
						pChzStructOrProc,
						pChzOwner,
						iArg + 1,
						mpIArgIstrName[iArg].PChz());
				}
				return false;
			}

			if (pStdecl->m_pStnodType)
			{
				auto pStnodType = pStdecl->m_pStnodType;
				auto pTinParam = pStnodType->m_pTin;

				if (pArgunp->m_grfarg.FIsSet(FARG_NamedLabelChild))
				{
					MOE_ASSERT(pStnodInit->m_park == PARK_ArgumentLabel, "expected argument label");
					pStnodInit = pStnodInit->PStnodChildSafe(1);
				}

				TypeInfo * pTinInitDefault = PTinPromoteUntypedArgument(pTcwork, pSymtab, pStnodInit, pTinParam, errep);
				pTinInitDefault = PTinAfterRValueAssignment(pTcwork, pStnodInit->m_lexsp, pTinInitDefault, pSymtab, pTinParam);

				GRFGENCOMP grfgencomp = pArgunp->m_grfarg.FIsSet(FARG_ImplicitRef) ? FGENCOMP_ImplicitRef : FGENCOMP_None;
				ERRID errid = ErridComputeDefinedGenerics(pTcwork, lexsp, errep, grfgencomp, pSymtab, pTinInitDefault, pStnodType, pGenmap);
				if (errid != ERRID_Nil)
					return false;

				pGenmap->m_aryLexspSrc.Append(pStnodType->m_lexsp);
			}
		}
	}

	// the size of the argunp array returned should include typeargs and var args, but not named args
	pmpIArgArgunp->PopToSize(cArgNoNamed);
	return true;
}

// find the subset of a generic map used by a given generic definition
GenericMap * PGenmapTrimUnusedAnchors(
	TypeCheckWorkspace * pTcwork,
	STNode * pStnodInstFrom,
	GenericMap * pGenmapSuperset, 
	const LexSpan & lexsp)
{
	GenericMap genmapTrim(pTcwork->m_pAlloc, "trim", nullptr);
	FindGenericAnchorNames(pTcwork->m_pAlloc, pStnodInstFrom, &genmapTrim);

	CHash<InString, Anchor>::CIterator iterTrim(&genmapTrim.m_mpIstrAnc);
	InString * pStrTrim;
	Anchor * pAncTrim;
	while ((pAncTrim = iterTrim.Next(&pStrTrim)))
	{
		auto pAncSuperset = pGenmapSuperset->PAncLookup(*pStrTrim);
		if (!pAncSuperset)
		{
			MOE_ASSERT(false, "all generic anchors should be shadowed");
		}
		else
		{
			MOE_ASSERT(pAncTrim->m_genk == pAncSuperset->m_genk, "anchor type mismatch");
			pAncTrim->m_pStnodBaked = pAncTrim->m_pStnodBaked;
			pAncTrim->m_pTin = pAncTrim->m_pTin;
		}
	}

	if (pGenmapSuperset->m_mpIstrAnc.C() == genmapTrim.m_mpIstrAnc.C())
		return pGenmapSuperset;

	auto pGenmapNew = PGenmapNew(pTcwork, "", nullptr);
	pGenmapNew->Swap(&genmapTrim);

	pGenmapNew->m_aryLexspSrc.Append(lexsp);
	return pGenmapNew;
}

// walk a proc|struct AST and find the names of all of the generic anchors
void FindGenericAnchorNames(
	Moe::Alloc * pAlloc,
	STNode * pStnodDef,
	GenericMap * pGenmap)
{
	CDynAry<STNode *> arypStnod(pAlloc, BK_TypeCheckGenerics, 16);
	arypStnod.Append(pStnodDef);

	// walk through 
	while (arypStnod.C())
	{
		auto pStnodIt = arypStnod.TPopLast();

		bool fIsStructHeader = pStnodIt->m_park == PARK_StructDefinition || pStnodIt->m_park == PARK_ParameterList;
		MOE_ASSERT(fIsStructHeader || pStnodIt->m_strees == STREES_TypeChecked, "Type specification should be type checked first, (for literal op eval)");

		while (pStnodIt)
		{
			STNode * pStnodCur = pStnodIt;
			pStnodIt = nullptr;	

			switch(pStnodCur->m_park)
			{
			case PARK_GenericDecl:
				{ 
					auto pTingen = PTinDerivedCast<TypeInfoAnchor *>(pStnodCur->m_pTin);

					auto pSym = pStnodCur->PSym();
					auto pTinSym = PTinFromSymbol(pSym);
					if (!MOE_FVERIFY(pTinSym && pTinSym->m_tink == TINK_Anchor, "expected generic anchor type"))
						break;

					pGenmap->PAncMapType(pSym->m_istrName, nullptr);
				} break;
			case PARK_MemberLookup:
			case PARK_Identifier:
			case PARK_Literal:
			case PARK_BakedValue:
				break;
				
			case PARK_Decl:
				{ 
					auto pStdecl = PStnodRtiCast<STDecl *>(pStnodCur);
					if (!MOE_FVERIFY(pStdecl, "expected declaration"))
						break;

					if (pStdecl->m_fIsBakedConstant && pStdecl->m_pStnodIdentifier)
					{
						auto pStnodIdent = pStdecl->m_pStnodIdentifier;
						pGenmap->PAncMapValue(IstrFromIdentifier(pStnodIdent), nullptr);
					}

					pStnodIt = pStdecl->m_pStnodType;
				} break;
			case PARK_ArgumentLabel:
				{
					// argument label's children are [identifier, value]
					pStnodIt = pStnodCur->PStnodChildSafe(1);
					MOE_ASSERT(pStnodIt, "bad argument label");
				} break;
			case PARK_TypeArgument:
				{
					pStnodIt = pStnodCur->PStnodChildSafe(0);
					MOE_ASSERT(pStnodIt, "bad type argument");
				} break;
			case PARK_ArrayDecl:
				{
					// array decl's children are [type] or [dim, type]

					if (pStnodCur->CPStnodChild() > 1)
					{
						// dim may be generic baked value
						auto pStnodDim = pStnodCur->PStnodChild(0);
						arypStnod.Append(pStnodDim);
					}
					pStnodIt = pStnodCur->PStnodChildSafe(pStnodCur->CPStnodChild()-1);
					MOE_ASSERT(pStnodIt, "bad array declaration");
				} break;
			case PARK_QualifierDecl:
				{
					MOE_ASSERT(pStnodCur->CPStnodChild() == 1, "expected one child");
					pStnodIt = pStnodCur->PStnodChildSafe(0);
				} break;
			case PARK_ReferenceDecl:
				{
					MOE_ASSERT(pStnodCur->CPStnodChild() == 1, "expected one child");
					pStnodIt = pStnodCur->PStnodChildSafe(0);
				} break;
			case PARK_GenericStructSpec:
				{
					auto pSymInst = pStnodCur->PSym();
					MOE_ASSERT(pSymInst && pStnodCur->m_strees >= STREES_TypeChecked, "expected to be type checked");
					auto pStnodDef = pSymInst->m_pStnodDefinition;

					auto pTinstructGen = PTinRtiCast<TypeInfoStruct *>(pStnodCur->m_pTin);
					auto pStstruct = PStnodRtiCast<STStruct *>(pStnodDef);
					if (MOE_FVERIFY(pStstruct && pTinstructGen, "bad PARK_GenericStructInst") &&
  						pStstruct->m_pStnodParameterList)
					{
						STNode * pStnodParameterList = pStstruct->m_pStnodParameterList;
						for (int ipStnod = 0; ipStnod < pStnodParameterList->CPStnodChild(); ++ipStnod)
						{
							STNode * pStnodParam = pStnodParameterList->PStnodChild(ipStnod);
							arypStnod.Append(pStnodParam);
						}
					}
				} break;
			case PARK_ProcedureReferenceDecl:
				{
					auto pTinprocGen = PTinRtiCast<TypeInfoProcedure *>(pStnodCur->m_pTin);

					auto pStproc = PStnodRtiCast<STProc *>(pStnodCur);
					if (MOE_FVERIFY(pStproc && pTinprocGen, "bad PARK_ProcedureReferenceDecl") &&
						pTinprocGen->FHasGenericArgs() &&
  						pStproc->m_pStnodParameterList)
					{
						STNode * pStnodParameterList = pStproc->m_pStnodParameterList;
						for (int ipStnod = 0; ipStnod < pStnodParameterList->CPStnodChild(); ++ipStnod)
						{
							STNode * pStnodParam = pStnodParameterList->PStnodChild(ipStnod);
							if (pStnodParam->m_park == PARK_VariadicArg)
								continue;

							arypStnod.Append(pStnodParam);
						}
					}
				} break;
			case PARK_StructDefinition:
				{
					auto pStstruct = PStnodRtiCast<STStruct *>(pStnodCur);
					if (MOE_FVERIFY(pStstruct, "bad struct node") && pStstruct->m_pStnodParameterList)
					{
						arypStnod.Append(pStstruct->m_pStnodParameterList);
					}

				} break;
			case PARK_ParameterList:
				{
					auto ppStnodMac = pStnodCur->PPStnodChildMax();
					for (auto ppStnodIt = pStnodCur->PPStnodChildMin(); ppStnodIt != ppStnodMac; ++ppStnodIt)
					{
						arypStnod.Append(*ppStnodIt);
					}
				} break;
			default: MOE_ASSERT(false, "Unexpected parse node PARK_%s in FindGenericAnchorName()", PChzAbbrevFromPark(pStnodCur->m_park));
				break;
			}
		}
	}
}

TypeInfo * PTinFindCanon(TypeCheckWorkspace * pTcwork, TypeInfo * pTin, SymbolTable * pSymtab, ERREP errep)
{
	switch (pTin->m_tink)
	{
	case TINK_Struct:
		{
			auto pTinstruct = (TypeInfoStruct *)pTin;

			auto pTinstructFrom = pTinstruct->m_pTinstructInstFrom;
			if (pTinstructFrom == nullptr)
			{
				// we're not instantiated, just make sure the flag is set and return
				pTin->m_grftin.AddFlags(FTIN_IsCanon);
				return pTin;
			}

			// iterate up the m_pTinstructInstantiatedFrom chain remapping anchors at each step until we have a canonical
			//   instantiation of this struct (no partial instantiations in the chain and any anchored type in our genmap is 
			//   also canonical

			// TODO:
			// [ ] cleanup the wasted interstitial genmaps that aren't used

			InString * pIstrAnchorFrom;
			Anchor * pAncFrom;
			auto pTinstructIt = pTinstruct;
			auto pGenmapIt = pTinstructIt->m_pGenmap;

			while (pTinstructFrom && pTinstructFrom->m_pGenmap)
			{
				auto pGenmapFrom = pTinstructFrom->m_pGenmap;
				auto pGenmapNew = PGenmapNew(pTcwork, "Canon", pTinstructFrom);

				Moe::CHash<Moe::InString, Anchor>::CIterator iter(&pGenmapFrom->m_mpIstrAnc);
				while ((pAncFrom = iter.Next(&pIstrAnchorFrom)))
				{
					pAncFrom->AssertIsValid();
					switch (pAncFrom->m_genk)
					{
					case GENK_Type:
						{
							TypeInfo * pTinNew = pAncFrom->m_pTin;
							if (FIsGenericType(pAncFrom->m_pTin))
							{
								LexSpan lexspIt(pGenmapIt->LexspReporting());
								pTinNew = PTinSubstituteGenerics(pTcwork, pSymtab, lexspIt, pAncFrom->m_pTin, pGenmapIt, errep);
							}
							pGenmapNew->PAncMapType(*pIstrAnchorFrom, pTinNew);
						} break;
					case GENK_Value:
						{
							STNode * pStnodValue = pAncFrom->m_pStnodBaked;
							if (pStnodValue->m_park == PARK_Decl)
							{
								auto istrAnchorIt = IstrIdentifierFromDecl(pAncFrom->m_pStnodBaked);
								auto pAncIt = pGenmapIt->PAncLookup(istrAnchorIt);
								MOE_ASSERT(pAncIt, "failed to lookup '$%s' while cannonicalizing generics", istrAnchorIt.PChz());
								MOE_ASSERT(pAncIt->m_genk == GENK_Value, "expected value");

								pStnodValue = pAncIt->m_pStnodBaked;
							}

							pGenmapNew->PAncMapValue(*pIstrAnchorFrom, pStnodValue);
						} break;
					default:
						MOE_ASSERT(false, "unexpected generic kind");
					}
				}
				
				pGenmapIt = pGenmapNew;
				pTinstructIt = pTinstructFrom;
				pTinstructFrom = pTinstructIt->m_pTinstructInstFrom;
			}

			pGenmapIt->m_aryLexspSrc.Append(pTinstruct->m_pGenmap->m_aryLexspSrc.A(), pTinstruct->m_pGenmap->m_aryLexspSrc.C());

			LexSpan lexspIt(pGenmapIt->LexspReporting());
			TypeInfo * pTinNew = pTinstructIt;
			if (pTinstructIt != pTinstruct || pGenmapIt != pTinstructIt->m_pGenmap)
			{
				pTinNew = PTinSubstituteGenerics(pTcwork, pSymtab, lexspIt, pTinstructFrom, pGenmapIt, errep);
			}

			auto pTinstructNew = PTinDerivedCast<TypeInfoStruct *>(pTinNew);

			pTinstructNew = PTinstructEnsureUniqueInstance(pTcwork, pTinstructNew);
			pTinstructNew->m_grftin.AddFlags(FTIN_IsCanon);

			return pTinstructNew;
		} break;
	case TINK_Procedure:
		{
			MOE_ASSERT(false, "TBD - write PTinFindCanon for generic procs");
		} break;
	case TINK_Anchor:
		{
			MOE_ASSERT(false, "Generic type is not canon... not sure what we should do here");
		} break;
	case TINK_Qualifier:
		{
			auto pTinqual = (TypeInfoQualifier *)pTin;
			auto pTinCanon = PTinFindCanon(pTcwork, pTinqual->m_pTin, pSymtab, errep);
			if (pTinqual->m_pTin != pTinCanon)
			{
				pTinqual = pSymtab->PTinqualEnsure(pTinCanon, pTinqual->m_grfqualk);
			}
			return pTinqual;
		} break;
	case TINK_Array:
		{
			auto pTinary = (TypeInfoArray *)pTin;
			auto pTinCanon = PTinFindCanon(pTcwork, pTinary->m_pTin, pSymtab, errep);
			if (pTinary->m_pTin != pTinCanon)
			{
				pTinary = pSymtab->PTinaryCopyWithNewElementType(pTinary, pTinCanon);
			}
			return pTinary;
		} break;
	case TINK_Pointer:
		{
			auto pTinptr = (TypeInfoPointer *)pTin;
			auto pTinCanon = PTinFindCanon(pTcwork, pTinptr->m_pTin, pSymtab, errep);
			if (pTinptr->m_pTin != pTinCanon)
			{
				pTinptr = pSymtab->PTinptrAllocate(pTinCanon);
			}
			return pTinptr;
		} break;
	default:
		return pTin;
	}
	return pTin;
}

TypeInfo * PTinSubstituteGenerics(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtab,
	const LexSpan & lexsp,
	TypeInfo * pTinUnsub,
	GenericMap * pGenmap,
	ERREP errep)
{
	// given known generics and un-substituted type, generate an instantiated type
	switch (pTinUnsub->m_tink)
	{
		case TINK_Anchor:
			{
				auto pTinanc = (TypeInfoAnchor *)pTinUnsub;
				Anchor * pAnc = pGenmap->PAncLookup(pTinanc->m_pStnodDefinition->PSym()->m_istrName);
				if (!pAnc || pAnc->FIsNull())
				{
					if (errep == ERREP_ReportErrors)
					{
						EmitError(pTcwork->m_pErrman, lexsp, ERRID_GenericLookupFail,
							"Unable to pattern match instance type for generic value '$%s'", pTinanc->m_istrName.PChz());
					}
					return nullptr;
				}

				MOE_ASSERT(pAnc->m_pTin, "expected baked type (not value)");
				return pAnc->m_pTin;
			}
    	case TINK_Numeric:
    	case TINK_Bool:
    	case TINK_Void:
    	case TINK_Null:
    	case TINK_Enum:
	    		return pTinUnsub;
	    case TINK_Pointer:
		    {
		    	auto pTinptr = (TypeInfoPointer *)pTinUnsub;
		    	auto pTinTarget = PTinSubstituteGenerics(pTcwork, pSymtab, lexsp, pTinptr->m_pTin, pGenmap, errep);
				return pSymtab->PTinptrAllocate(pTinTarget);
		    }
	    case TINK_Procedure:
		    {
		    	auto pTinprocUnsub = (TypeInfoProcedure *)pTinUnsub;
				auto pTinproc = pSymtab->PTinprocCopy(pTinprocUnsub);
				pTinproc->m_grftingen = pGenmap->m_grftingenResult;

		    	auto cpTinParams = pTinproc->m_arypTinParams.C();
				int ipTinDst = 0;
				for (int ipTinSrc = 0; ipTinSrc < cpTinParams; ++ipTinSrc)
				{
					// collapse compile-time baked values and types and...
					if (pTinprocUnsub->m_mpIptinGrfparmq[ipTinSrc].FIsAnySet(FPARMQ_BakedValue | FPARMQ_TypeArgument))
						continue;

					//...substitute types for the params that will remain
					pTinproc->m_arypTinParams[ipTinDst] = PTinSubstituteGenerics(pTcwork, pSymtab, lexsp, pTinproc->m_arypTinParams[ipTinSrc], pGenmap, errep);
					pTinproc->m_mpIptinGrfparmq[ipTinDst] = pTinprocUnsub->m_mpIptinGrfparmq[ipTinSrc];
					++ipTinDst;
				}

				while (pTinproc->m_arypTinParams.C() > ipTinDst)
				{
					pTinproc->m_arypTinParams.PopLast();
				}

		    	auto cpTinReturn = pTinproc->m_arypTinReturns.C();
				for (int ipTin = 0; ipTin < cpTinReturn; ++ipTin)
				{
					pTinproc->m_arypTinReturns[ipTin] = PTinSubstituteGenerics(pTcwork, pSymtab, lexsp, pTinproc->m_arypTinReturns[ipTin], pGenmap, errep);
				}

				return pTinproc;
			}
	    case TINK_Struct:
		    {
				auto pTinstructUnsub = (TypeInfoStruct *)pTinUnsub;
				if (!pTinstructUnsub->m_grftingen.FIsAnySet(FTINGEN_HasGenericArgs))
				{
					return pTinstructUnsub;
				}

				auto pGenmapTrim = PGenmapTrimUnusedAnchors(pTcwork, pTinstructUnsub->m_pStnodStruct, pGenmap, lexsp);

				auto pInsreq = PInsreqLookup(pTcwork, pTinstructUnsub->m_pStnodStruct, pGenmapTrim);
				if (pInsreq)
				{
					TypeInfo * pTinSym;
					if (MOE_FVERIFY(pInsreq->m_pSym && (pTinSym = PTinFromSymbol(pInsreq->m_pSym)), "expected symbol with type"))
					{
						return pTinSym;
					}
				}

				auto cTypememb = pTinstructUnsub->m_aryTypemembField.C();
				auto pTinstructNew = pSymtab->PTinstructAllocate(pTinUnsub->m_istrName, cTypememb, 0);
				
				pTinstructNew->m_pGenmap = pGenmapTrim;
				pTinstructNew->m_pTinstructInstFrom = pTinstructUnsub;

				for (int iTypememb = 0; iTypememb < cTypememb; ++iTypememb)
				{
					TypeStructMember * pTypemembUnsub = &pTinstructUnsub->m_aryTypemembField[iTypememb];
					pTinstructNew->m_aryTypemembField.Append(*pTypemembUnsub);
				}

				auto pTinCanon = PTinFindCanon(pTcwork, pTinstructNew, pSymtab, errep);
				pTinstructNew = PTinDerivedCast<TypeInfoStruct *>(pTinCanon);
				pTinstructNew =  PTinstructEnsureUniqueInstance(pTcwork, pTinstructNew);

				pTinstructNew->m_grftingen = pGenmapTrim->m_grftingenResult;
		    	return pTinstructNew;
		    }
	    case TINK_Array:
		    {
		    	auto pTinaryUnsub = (TypeInfoArray *)pTinUnsub;

		    	auto pTinaryNew = pSymtab->PTinaryCopy(pTinaryUnsub);
		    	pTinaryNew->m_pTin = PTinSubstituteGenerics(pTcwork, pSymtab, lexsp, pTinaryUnsub->m_pTin, pGenmap, errep);

				if (pTinaryUnsub->m_pStnodBakedDim)
				{
					auto pSymDim = pTinaryUnsub->m_pStnodBakedDim->PSym();
					if (pSymDim)
					{
						Anchor * pAnc = pGenmap->PAncLookup(pSymDim->m_istrName);
						if (pAnc && pAnc->m_genk == GENK_Value)
						{
							//auto pStvalDim = pAnc->m_pStnodBaked->m_pStval;
							auto pStvalDim = PStnodRtiCast<STValue *>(pAnc->m_pStnodBaked);
							if (pStvalDim)
							{
								pTinaryNew->m_c = NUnsignedLiteralCast(pTcwork, pStvalDim);
								pTinaryNew->m_pStnodBakedDim = nullptr;
							}
						}
					}
				}

		    	return pTinaryNew;
		    }
		case TINK_Qualifier:
		    {
		    	auto pTinqual = (TypeInfoQualifier *)pTinUnsub;
		    	auto pTinTarget = PTinSubstituteGenerics(pTcwork, pSymtab, lexsp, pTinqual->m_pTin, pGenmap, errep);
				return pSymtab->PTinqualWrap(pTinTarget, pTinqual->m_grfqualk);
		    }
		default:
			MOE_ASSERT(false, "unhandled type info.");
			break;
	}

	return nullptr;
}

InstantiateRequest * PInsreqInstantiateGenericStruct(
	TypeCheckWorkspace * pTcwork,
	STNode * pStnodGeneric,
	STNode * pStnodInstantiation,
	GenericMap * pGenmap)
{
	// BB - partially instantiated generics should list their definition as the PARK_GenericStruct node, not the typedef
	if (pStnodGeneric->m_park == PARK_Typedef)
	{
		// typedef children are [identifier, type] 
		auto pStnodStructInst = pStnodGeneric->PStnodChildSafe(1);
		if (!MOE_FVERIFY(pStnodStructInst != nullptr, "typedef missing type, TBD - change to error msg"))
			return nullptr;

		// BB - this only works if the typedef is directly the template type, is that legit?
		// BB - there should be error checking around this stuff - is the typedef generic? is it a struct?
		auto pTinstruct = PTinRtiCast<TypeInfoStruct *>(pStnodStructInst->m_pTin);
		if (!MOE_FVERIFY(pTinstruct, "typedef is not generic, TBD - change to error msg"))
			return nullptr;

		pStnodGeneric = pTinstruct->m_pStnodStruct;
	}

	auto pStstructSrc = PStnodRtiCast<STStruct *>(pStnodGeneric);

	if (!MOE_FVERIFY(pStstructSrc, "expected procedure def"))
		return nullptr;

	// remap the types for the argument list and build symbols for them
	Moe::CHash<STNode *, STNode *> mpPStnodGenPStnodCopy(pTcwork->m_pAlloc, BK_TypeCheckGenerics);

	auto pStnodStructCopy = PStnodCopy(pTcwork->m_pAlloc, pStnodGeneric, &mpPStnodGenPStnodCopy);
	pStnodStructCopy->m_grfstnod.Clear(FSTNOD_NoCodeGeneration);

	MOE_ASSERT(pStnodGeneric->m_strees = STREES_TypeChecked, "generic struct definition should be type checked prior to instantiation");
	pStnodStructCopy->m_strees = STREES_Parsed;

	// copy the symbol table, but replace generic types from the map

	// BB - is this really the right way to get the proc's symtab? might have a param list, might not.
	SymbolTable * pSymtabSrc = pStnodGeneric->m_pSymtab;
	if (!MOE_FVERIFY(pSymtabSrc, "generic structure source has no symbol table"))
		return nullptr;

	SymbolTable * pSymtabNew = PSymtabNew(pTcwork->m_pAlloc, pSymtabSrc, pSymtabSrc->m_istrNamespace);	
	pSymtabNew->m_pSymtabParent = pSymtabSrc->m_pSymtabParent;

	GenericMapScope genscope(pTcwork->m_pErrman, pGenmap);

	auto pInsreq = pTcwork->m_genreg.PInsreqNew(pStnodGeneric, pGenmap);
	pInsreq->m_pGenmap = pGenmap;
	pInsreq->m_pStnodGeneric = pStnodGeneric;

	// Figure out the generic args that will remain after this instantiation
	//   may not be fewer generic args ie. instantiating 'SPair(:$A, :$B)' to 'SPair( :CAry(:$U), :$V)'

	int cGenericValue = 0;
	int cGenericType = 0;

	STNode * pStnodNewParams = nullptr;
	int ipStnodFullyInstantated = -1;
	{
		GenericMap genmapNames(pTcwork->m_pAlloc, "findnames", nullptr);
		CDynAry<STNode *> aryStnodChild(pTcwork->m_pAlloc, BK_TypeCheckGenerics);

		int ipStnodMin = 1; // instantiation children are (name, arg0, arg1, arg2, ...)
		for (int ipStnodArg = ipStnodMin; ipStnodArg < pStnodInstantiation->CPStnodChild(); ++ipStnodArg)
		{
			auto pStnodInstArg = pStnodInstantiation->PStnodChild(ipStnodArg);
			size_t cAncPrev = genmapNames.m_mpIstrAnc.C();
			FindGenericAnchorNames(pTcwork->m_pAlloc, pStnodInstArg, &genmapNames);

			if (genmapNames.m_mpIstrAnc.C() <= cAncPrev)
			{
				if (ipStnodFullyInstantated < 0)
				{
					ipStnodFullyInstantated = ipStnodArg;
				}
			}
			else
			{
				if (ipStnodFullyInstantated >= 0)
				{
					EmitError(pTcwork, pStnodInstantiation->m_lexsp, ERRID_GenericArgsMustBeFirst,
						"Generic parameters (%d) are not allowed after fully instantiated parameter (%d)",
						ipStnodArg,
						ipStnodFullyInstantated);
				}

				// the code that will instantiate this generic expects the parameters to all be decl nodes
				if (pStnodInstArg->m_park == PARK_ArgumentLabel && 
					MOE_FVERIFY(pStnodInstArg->CPStnodChild() == 2, "expected argument label's children to be (identifier, arg)"))
				{
					pStnodInstArg = pStnodInstArg->PStnodChild(1);
				}

				STDecl * pStdecl = nullptr;

				switch (pStnodInstArg->m_park)
				{
				case PARK_Decl:
					{
						auto pStnodCopy = PStnodCopy(pTcwork->m_pAlloc, pStnodInstArg);
						pStdecl = PStnodRtiCast<STDecl * >(pStnodCopy);
					} break;
				case PARK_TypeArgument:
					{
						if (!MOE_FVERIFY(pStnodInstArg->CPStnodChild() >= 0, "type argument's first child should be type AST"))
							break;

						pStdecl = PStdeclAllocAfterParse(pTcwork->m_pAlloc, PARK_Decl, pStnodInstArg->m_lexsp);

						auto pStnodTypeOld = pStnodInstArg->PStnodChild(0);
						auto pStnodTypeNew = PStnodCopy(pTcwork->m_pAlloc, pStnodTypeOld);

						pStdecl->m_pStnodType = pStnodTypeNew;

					} break;
				default:
					MOE_ASSERT(false, "unexpected PARK in partially instantiated template arg");
				}

				if (pStdecl)
				{
					//pStnodNewParams->IAppendChild(pStdecl);
					aryStnodChild.Append(pStdecl);
				}
			}
		}

		if (!aryStnodChild.FIsEmpty())
		{
			pStnodNewParams = PStnodAllocAfterParse(pTcwork->m_pAlloc, PARK_ParameterList, pStnodInstantiation->m_lexsp);
			pStnodNewParams->CopyChildArray(pTcwork->m_pAlloc, aryStnodChild.A(), int(aryStnodChild.C()));
		}


		CHash<InString, Anchor>::CIterator iter(&genmapNames.m_mpIstrAnc);
		Anchor * pAnc;
		while ((pAnc = iter.Next()))
		{
			switch (pAnc->m_genk)
			{
			case GENK_Value:	++cGenericValue;	break;
			case GENK_Type:		++cGenericType;		break;
			default: MOE_ASSERT(false, "unexpected anchor type");
			}
		}
	}

	// Remap the top level symbol table
	Moe::CHash<Symbol *, STNode *> mpPSymSrcPStnodValue(pTcwork->m_pAlloc, BK_TypeCheckGenerics);
	Moe::CHash<Symbol *, Symbol *> mpPSymGenericPSymRemapped(pTcwork->m_pAlloc, BK_TypeCheckGenerics);
	Moe::CHash<InString, Symbol *>::CIterator iterSrc(&pSymtabSrc->m_hashIstrPSym);
	Symbol ** ppSymSrc;

	while ((ppSymSrc = iterSrc.Next()))
	{
		Symbol * pSymSrc = *ppSymSrc;
		MOE_ASSERT(pSymSrc->m_pSymPrev == nullptr, 
			"not handing shadowed symbols (%s shadows %s)", 
			pSymSrc->m_istrName.PChz(),
			pSymSrc->m_pSymPrev->m_istrName.PChz()); // see PSymtabCopy

		if (MOE_FVERIFY(pSymSrc->m_pStnodDefinition, "symbol without defining syntax tree node"))
		{
			auto pAnc = pGenmap->PAncLookup(pSymSrc->m_istrName);
			if (pAnc && pAnc->m_genk == GENK_Value)
			{
				MOE_ASSERT(pAnc->m_pStnodBaked, "expected baked value (not type)");
				mpPSymSrcPStnodValue.Insert(pSymSrc, pAnc->m_pStnodBaked);
					continue;

			}
		}

		auto pSymNew = pSymtabNew->PSymEnsure(
			pTcwork->m_pErrman,
			pSymSrc->m_istrName,
			pSymSrc->m_pStnodDefinition,
			pSymSrc->m_grfsym);

		auto ppStnodCopy = mpPStnodGenPStnodCopy.Lookup(pSymNew->m_pStnodDefinition);
		if (!ppStnodCopy)
		{
			EmitError(pTcwork, pSymNew->m_pStnodDefinition->m_lexsp, ERRID_MissingSymbolDef,
				"cannot look up definition stNode for symbol %s", 
				pSymNew->m_istrName.PChz());
		}
		else
		{
			pSymNew->m_pStnodDefinition = *ppStnodCopy;
		}

		TypeInfo * pTinSymSrc;
		if ((pTinSymSrc = PTinFromSymbol(pSymSrc)))
		{
			auto pTinSymNew = PTinSubstituteGenerics(
								pTcwork,
								pSymtabNew,
								pSymSrc->m_pStnodDefinition->m_lexsp,
								pTinSymSrc,
								pGenmap,
								ERREP_ReportErrors);

			MOE_ASSERT(pTinSymNew == PTinFromSymbol(pSymNew), "bad symbol type mapping");
		}
		mpPSymGenericPSymRemapped.Insert(pSymSrc, pSymNew);
	}

	// need to walk the generic map and make sure we have symbols for all the constants defined by anchors
	CHash<InString, Anchor>::CIterator iter(&pGenmap->m_mpIstrAnc);
	InString * pIstrAnc;
	Anchor * pAnc;
	while ((pAnc = iter.Next(&pIstrAnc)))
	{
		auto pStnodBaked = pAnc->m_pStnodBaked;
		if (pAnc->m_genk != GENK_Value || !MOE_FVERIFY(pStnodBaked, "missing baked value"))
			continue;

		auto pSymNew = pSymtabNew->PSymEnsure(
									pTcwork->m_pErrman,
									*pIstrAnc,
									pStnodBaked,
									FSYM_None);

		// add pSymGeneric pSym remapped entry for $CPrev to $CNew
		pStnodBaked->m_pSymbase = pSymNew;
	}

	// build pTinstruct for the instantiated struct

	auto pTinstructSrc = PTinDerivedCast<TypeInfoStruct *>(pStnodGeneric->m_pTin);

	auto cTypememb = pTinstructSrc->m_aryTypemembField.C();
	auto pTinstructNew = pSymtabNew->PTinstructAllocate(pTinstructSrc->m_istrName, cTypememb, cGenericValue + cGenericType);

	for (int iTypememb = 0; iTypememb < cTypememb; ++iTypememb)
	{
		TypeStructMember * pTypemembUnsub = &pTinstructSrc->m_aryTypemembField[iTypememb];
		pTinstructNew->m_aryTypemembField.Append(*pTypemembUnsub);

		MOE_ASSERT(pTypemembUnsub->m_pTin == nullptr, "expected pTin to be unresolved");
	}

	for (int iTypememb = 0; iTypememb < pTinstructNew->m_aryTypemembField.C(); ++iTypememb)
	{
		auto pTypememb = &pTinstructNew->m_aryTypemembField[iTypememb];

		auto ppStnodCopy = mpPStnodGenPStnodCopy.Lookup(pTypememb->m_pStdecl);
		if (!ppStnodCopy)
		{
			EmitError(pTcwork, pTypememb->m_pStdecl->m_lexsp, ERRID_MissingMemberDef,
				"cannot look up definition stNode for type member %s in %s", 
				pTypememb->m_istrName.PChz(),
				pTinstructSrc->m_istrName.PChz());
			continue;
		}
		pTypememb->m_pStdecl = PStnodRtiCast<STDecl*>(*ppStnodCopy);

	}

	pInsreq->m_pSym = pSymtabNew->PSymGenericInstantiate(pStnodGeneric->PSym(), pStnodStructCopy);
	MOE_ASSERT(pInsreq->m_pSym, "null symbol");

	pStnodStructCopy->m_pTin = pTinstructNew;
	pStnodStructCopy->m_pSymbase = pInsreq->m_pSym;
	pTinstructNew->m_pStnodStruct = pStnodStructCopy;

	MOE_ASSERT(pTinstructSrc->m_grftingen.FIsAnySet(FTINGEN_HasGenericArgs), "instantiating non-generic struct");
	pTinstructNew->m_pTinstructInstFrom = pTinstructSrc;
	pTinstructNew->m_pGenmap = pGenmap;

	pTinstructNew = PTinstructEnsureUniqueInstance(pTcwork, pTinstructNew);
	GRFTINGEN grftingen;
	grftingen.AssignFlags(FTINGEN_HasBakedValueArgs, cGenericValue > 0);
	grftingen.AssignFlags(FTINGEN_HasBakedTypeArgs, cGenericType > 0);
	MOE_ASSERT(grftingen == pGenmap->m_grftingenResult, "bad generic type");
	pTinstructNew->m_grftingen = pGenmap->m_grftingenResult;

	auto pStstructCopy = PStnodRtiCast<STStruct *>(pStnodStructCopy);
	pStstructCopy->m_pStnodBakedParameterList = pStstructCopy->m_pStnodParameterList;
	pStstructCopy->m_pStnodParameterList = nullptr;

	// if we still have parameters we need to set them up here.

	for (int ipStnod = 0; ipStnod < pStnodStructCopy->CPStnodChild(); ++ipStnod)
	{
		STNode * pStnodChild = pStnodStructCopy->PStnodChild(ipStnod);
		if (pStnodChild->m_pSymtab == pSymtabSrc)
		{
			pStnodChild->m_pSymtab = pSymtabNew;
		}
	}

	// BB - maybe error here instead of asserting
	if (!MOE_FVERIFY(pStstructSrc && pStstructSrc->m_pStnodDeclList, "empty structure definition"))
		return nullptr;	

	auto pStnodDeclSrc = pStstructSrc->m_pStnodDeclList;
	auto pStnodDeclCopy = pStstructCopy->m_pStnodDeclList;
	RemapGenericStnodCopy(
		pTcwork,
		pStnodGeneric,
		pStnodStructCopy,
		pInsreq->m_iInsreq,
		pGenmap,
		&mpPSymGenericPSymRemapped,
		&mpPSymSrcPStnodValue,
		&mpPStnodGenPStnodCopy,
		pSymtabSrc,
		pSymtabNew);

	// Add the stnodes for the remaining generic parameters (if any)
	//  This needs to happen after the remap so it doesn't confuse the remapper
	if (pStnodNewParams != nullptr)
	{
		pStstructCopy->m_pStnodParameterList = pStnodNewParams;
	}

	TypeCheckFrame * pTcfram = pTcwork->m_blistTcfram.AppendNew();
	pTcfram->m_ipTcframQueue = pTcwork->m_arypTcframPending.C();
	pTcwork->m_arypTcframPending.Append(pTcfram);

	WorkspaceEntry * pEntry = pTcwork->m_pblistEntry->AppendNew();
	pEntry->m_pStnod = pStnodStructCopy;
	pEntry->m_pSymtab = pSymtabNew;
	pTcfram->m_pEntry = pEntry;

#if KEEP_TYPEINFO_DEBUG_STRING
	pTinstructNew->m_istrDebug = StrFromTypeInfo(pTinstructNew);
#endif

	if (!pTinstructNew->FHasGenericParams() || s_fTypecheckPartialGenericStructs)
	{
		pTcfram->m_aryTcsent.SetAlloc(pTcwork->m_pAlloc, Moe::BK_TypeCheckStack);
		TypeCheckStackEntry * pTcsent = pTcfram->m_aryTcsent.AppendNew();
		pTcsent->m_nState = 0;
		pTcsent->m_pStnod = pStnodStructCopy;
		pTcsent->m_pSymtab = pSymtabNew;
		pTcsent->m_pStnodProcedure = nullptr;	// BB - how to find pStnodProcedure for pStnodGen
		pTcsent->m_pSymContext = pInsreq->m_pSym;
		pTcsent->m_grfsymlook = FSYMLOOK_Default;
		pTcsent->m_parkDeclContext = PARK_Nil;
		pTcsent->m_fAllowForwardDecl = false;
		pTcsent->m_tcctx = TCCTX_Normal;
	}

	return pInsreq;
}



enum ARGORD // ARGument ORDer
{
	ARGORD_Normal,
	ARGORD_Reversed,	// argument order reversed (used for checking comutative procedures)

	MOE_MAX_MIN_NIL(ARGORD)
};

struct MatchTypeInfo // tag = mtin
{
					MatchTypeInfo()
					:m_pTinCall(nullptr)
					,m_pTinCallDefault(nullptr)
					,m_pTinParam(nullptr)
					,m_pStnodArg(nullptr)
					,m_pStnodRawArg(nullptr)
						{ ; }

	TypeInfo *		m_pTinCall;				// argument type, if literal promoted tightest
	TypeInfo *		m_pTinCallDefault;		// argument type, if literal promoted to fit argument
	TypeInfo *		m_pTinParam;
	STNode *		m_pStnodArg;
	STNode *		m_pStnodRawArg;			// argument stnod, before adjusting for named label and/or type argument
};

 bool FTryComputeMatchTypeInfo(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtab,
	MatchTypeInfo * pMtin, 
	STNode * pStnodArg,
	TypeInfo * pTinParam,
	int	iStnodArg,
	GRFARG grfarg,
	GRFPARMQ grfparmq,
	ERREP errep)
{
	STNode * pStnodRawArg = pStnodArg;
	if (grfarg.FIsSet(FARG_NamedLabelChild))
	{
		MOE_ASSERT(pStnodArg->m_park == PARK_ArgumentLabel, "expected argument label");
		pStnodArg = pStnodArg->PStnodChildSafe(1);
	}

	TypeInfo * pTinCall = pStnodArg->m_pTin;
	
	// Find the default literal promotion, as we need this to check for exact matches (which have precedence for matching)
	//  Things that can't default (void *) are problematic.

	if (pStnodArg && pStnodArg->m_park == PARK_TypeArgument)
	{
		auto pStnodChild = pStnodArg->PStnodChildSafe(0);
		pStnodArg = pStnodChild;
		if (MOE_FVERIFY(pStnodChild && pStnodChild->m_pTin, "expected type argument child"))
		{
			pTinCall = pStnodChild->m_pTin;
		}
	}
	else if (pTinParam)
	{
		if (grfparmq.FIsSet(FPARMQ_ImplicitRef))
		{
			if (pTinParam->m_tink != TINK_Pointer)
				return false;

			if (!FVerifyIvalk(pTcwork, pStnodArg, IVALK_LValue))
			{
				EmitError(pTcwork->m_pErrman, pStnodArg->m_lexsp, ERRID_NotLvalue,
					"Argument %d, must be an LValue for implicit conversion to pointer.",
					iStnodArg+1);
			}
			pTinParam = ((TypeInfoPointer*)pTinParam)->m_pTin;
		}

		pTinCall = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodArg, pTinParam, errep);
		pTinCall = PTinAfterRValueAssignment(pTcwork, pStnodArg->m_lexsp, pTinCall, pSymtab, pTinParam);
	}

	TypeInfo * pTinCallDefault = PTinPromoteUntypedArgument(pTcwork, pSymtab, pStnodArg, pTinParam, errep);
	pTinCallDefault = PTinAfterRValueAssignment(pTcwork, pStnodArg->m_lexsp, pTinCallDefault, pSymtab, pTinParam);

	pMtin->m_pTinCall = pTinCall;
	pMtin->m_pTinCallDefault = pTinCallDefault;
	pMtin->m_pStnodArg = pStnodArg;
	pMtin->m_pStnodRawArg = pStnodRawArg;

	// we'll need to rebuild pTinParam once we know what all the generic types are
	pMtin->m_pTinParam = pTinParam;
	return true;
}

PROCMATCH ProcmatchCheckStructArguments(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtab,
	TypeInfoStruct * pTinstruct,
	ProcMatchParam * pPmparam,
	ERREP errep)
{
	auto pStnodStruct = pTinstruct->m_pStnodStruct;
	auto pStstruct = PStnodRtiCast<STStruct *>(pStnodStruct);

	if (!MOE_FVERIFY(pStstruct, "expected ststruct") || pStstruct->m_pStnodParameterList == nullptr)
		return PROCMATCH_None;

	auto pStnodDefParamList = pStstruct->m_pStnodParameterList;
	int cParam = pStnodDefParamList->CPStnodChild();

	// unpack  the 'packed' args supplied in the code to the unpacked list resolving named, ordered and default args.
	CDynAry<ArgUnpack> mpIArgArgunp(pTcwork->m_pAlloc, BK_TypeCheckGenerics, cParam);
	mpIArgArgunp.AppendNew(cParam);

	GenericMap genmap(pTcwork->m_pAlloc, "CheckStructArg", pTinstruct);

	if (!FUnpackArgumentList(
		pTcwork,
		pSymtab,
		cParam,
		&mpIArgArgunp,
		&genmap,
		errep,
		pStnodDefParamList,
		pPmparam->m_ppStnodCall,
		pPmparam->m_cpStnodCall,
		pStnodStruct->m_lexsp,
		pTinstruct->m_istrName.PChz(),
		"struct"))
	{
		return PROCMATCH_None;
	}

	// set cParam to size of trimmed argument list
	cParam = (int)mpIArgArgunp.C();
		
	CDynAry<MatchTypeInfo> aryMtin(pTcwork->m_pAlloc, BK_TypeCheckProcmatch, pPmparam->m_cpStnodCall);


	for (int iArg = 0; iArg < cParam; ++iArg)
	{
		STNode * pStnodArg = mpIArgArgunp[iArg].m_pStnodInit;
		auto pMtin = aryMtin.AppendNew();

		auto pStnodDefParam = pStnodDefParamList->PStnodChild(iArg);
		TypeInfo * pTinParam = pStnodDefParam->m_pTin;

		if (!MOE_FVERIFY(pTinParam, "unknown parameter type"))
			return PROCMATCH_None;

		if (mpIArgArgunp[iArg].m_grfarg.FIsSet(FARG_TypeArgument) && pStnodArg == nullptr)
		{
			//Type arguments don't need to be explicitly named if all named type anchors are supplied
			pMtin->m_pTinParam = pTinParam;
			continue;
		}

		if (!FTryComputeMatchTypeInfo(
			pTcwork,
			pSymtab,
			pMtin,
			pStnodArg,
			pTinParam,
			iArg,
			mpIArgArgunp[iArg].m_grfarg,
			FPARMQ_None,
			errep))
		{
			return PROCMATCH_None;
		}
	}

	if (pTinstruct->FHasGenericParams())
	{
		int cMtin = (int)aryMtin.C();
		for (int iMtin = 0; iMtin < cMtin; ++iMtin)
		{
			auto pMtin = &aryMtin[iMtin];
			pMtin->m_pTinParam = PTinSubstituteGenerics(pTcwork, pSymtab, pPmparam->m_lexsp, pMtin->m_pTinParam, &genmap, errep);
			if (!pMtin->m_pTinParam)
			{
				// errors should have been reported in PTinSubstituteGenerics
				return PROCMATCH_None;
			}

			if (pMtin->m_pStnodArg)
			{
				pMtin->m_pTinCall = PTinPromoteUntypedTightest(pTcwork, pSymtab, pMtin->m_pStnodArg, pMtin->m_pTinParam);
				pMtin->m_pTinCallDefault = PTinPromoteUntypedArgument(pTcwork, pSymtab, pMtin->m_pStnodArg, pMtin->m_pTinParam, errep);

				pMtin->m_pTinCall = PTinAfterRValueAssignment(pTcwork, pMtin->m_pStnodArg->m_lexsp, pMtin->m_pTinCall, pSymtab, pMtin->m_pTinParam);
				pMtin->m_pTinCallDefault = PTinAfterRValueAssignment(pTcwork, pMtin->m_pStnodArg->m_lexsp, pMtin->m_pTinCallDefault, pSymtab, pMtin->m_pTinParam);
			}
			else
			{
				pMtin->m_pTinCall = pMtin->m_pTinParam;
				pMtin->m_pTinCallDefault = pMtin->m_pTinParam;
			}
		}
	}

	PROCMATCH procmatch = PROCMATCH_Exact;
	for (int iStnodArg = 0; iStnodArg < pPmparam->m_cpStnodCall; ++iStnodArg)
	{
		// This behavior can be a bit confusing when we're calling an overloaded function with a numeric literal
		//  we consider the overload an exact match when the default promotion matches exactly, we can't use the tightest
		//  promotion because that would exact match all implicit numeric conversions (ie. 2 tightest matches to both int and float)

		auto pMtin = &aryMtin[iStnodArg];
		if (FTypesAreSame(pMtin->m_pTinCallDefault, pMtin->m_pTinParam))
			continue;

		procmatch = PROCMATCH_ImplicitCast;

		if (!FCanImplicitCast(pMtin->m_pTinCall, pMtin->m_pTinParam))
		{
			GenericMapScope genscope(pTcwork->m_pErrman, (genmap.FIsEmpty()) ? nullptr : &genmap);

			InString istrTinCall = IstrFromTypeInfo(pMtin->m_pTinCall);
			InString istrTinParam = IstrFromTypeInfo(pMtin->m_pTinParam);
			EmitError(pTcwork->m_pErrman, pStnodStruct->m_lexsp, ERRID_BadImplicitConversion,
				"generic structure '%s' cannot convert argument %d from type %s to %s",
				pTinstruct->m_istrName.m_pChz,
				iStnodArg + 1,
				istrTinCall.PChz(),
				istrTinParam.PChz());

			return PROCMATCH_None;
		}
	}

	if (genmap.FIsEmpty())
		return PROCMATCH_None;

	auto pGenmap = PGenmapNew(pTcwork, "CheckStructArgs", pTinstruct);
	pGenmap->m_aryLexspSrc.Append(pPmparam->m_lexsp);
	pGenmap->Swap(&genmap);

	auto pPmfit = MOE_NEW(pPmparam->m_pAlloc, ProcMatchFit) ProcMatchFit(pPmparam->m_pAlloc);
	pPmfit->m_pGenmap = pGenmap;
	pPmparam->m_pPmfit = pPmfit;

	return procmatch;
}

inline const char * PChzProcName(TypeInfo * pTin, Symbol * pSym)
{
	if (!pTin->m_istrName.FIsEmpty())
		return pTin->m_istrName.PChz();

	if (pSym)
		return pSym->m_istrName.PChz();
	return "unnamed";
}

ERRID ErridComputeDefinedGenerics(
	TypeCheckWorkspace * pTcwork,
	const LexSpan & lexspRef,
	ERREP errep,
	GRFGENCOMP grfgencomp, 
	SymbolTable * pSymtab,
	TypeInfo * pTinRefEntry,
	STNode * pStnodDockEntry, // pStnod of the parameter's type
	GenericMap * pGenmapOut)
{
	// given a reference type and a generic type specification compute the anchored types/values
	//   ie. given a decl: '[2] $T' and a call arg: '[2] &int' compute that $T == &int

	struct GenericFrame // tag = genfram
	{
		STNode * 		m_pStnodDock;
		TypeInfo *		m_pTinRef;
	};

	CDynAry<GenericFrame> aryGenfram(pTcwork->m_pAlloc, BK_TypeCheckGenerics, 16);

	{
		auto pGenfram = aryGenfram.AppendNew();
		pGenfram->m_pStnodDock = pStnodDockEntry;
		pGenfram->m_pTinRef = pTinRefEntry;
	}

	ERRID erridReturn = ERRID_Nil;
	while (aryGenfram.C())
	{
		auto genfram = aryGenfram.TPopLast();
		auto pStnodDockIt = genfram.m_pStnodDock;

		MOE_ASSERT(pStnodDockIt->m_strees == STREES_TypeChecked, "Type specification should be type checked first, (for literal op eval)");
		while (pStnodDockIt)
		{

			STNode * pStnodDockCur = pStnodDockIt;
			pStnodDockIt = nullptr;	

			switch(pStnodDockCur->m_park)
			{
			case PARK_GenericDecl:
				{ 
					auto pTinanc = PTinDerivedCast<TypeInfoAnchor *>(pStnodDockCur->m_pTin);

					auto pSym = pStnodDockCur->PSym();
					auto pTinSym = PTinFromSymbol(pSym);
					if (!MOE_FVERIFY(pTinSym && pTinSym->m_tink == TINK_Anchor, "expected generic anchor type"))
						break;

					auto pAnc = pGenmapOut->PAncMapType(pSym->m_istrName, genfram.m_pTinRef);
					if (!pAnc->m_pTin && errep == ERREP_ReportErrors)
					{
						EmitError(pTcwork, pStnodDockCur->m_lexsp, ERRID_CannotInferGeneric,
							"cannot infer type for generic argument '%s'",
							pSym->m_istrName.PChz());
					}
				} break;
			case PARK_MemberLookup:
				{
					// I *think* there aren't any member lookup cases that can contain another type decl
					/*
					MOE_ASSERT(pStnodCur->CPStnodChild() == 2, "Expected Lhs.Rhs in PARK_MemberLookup");
					STNode * pStnodIdent = pStnodCur->PStnodChild(0);
					auto istrIdent = IstrFromIdentifier(pStnodIdent);
					auto pSym = pSymtab->PSymLookup(strIdent, pStnodIdent->m_lexsp, grfsymlook);
					MOE_ASSERT(pSym && pSym->m_pStnodDefinition, "bad outer type in type specification");

					STNode * pStnodDefinition = pSym->m_pStnodDefinition;
					if (MOE_FVERIFY(pStnodDefinition->m_pSymtab, "Struct without symbol table"))
					{
						pSymtab = pStnodDefinition->m_pSymtab;
					}

					pStnodCur = pStnodIt->PStnodChild(1);
					*/
				} break;
			case PARK_Identifier:
				{
				} break;
			case PARK_Decl:
				{ 
					auto pStdeclDock = PStnodRtiCast<STDecl *>(pStnodDockCur);
					if (!MOE_FVERIFY(pStdeclDock, "expected declaration"))
						break;

					// baked constants should already be baked in the struct decl, so we know about the parent baked genmap

					pStnodDockIt = pStdeclDock->m_pStnodType;
				} break;
			case PARK_ArrayDecl:
				{
					auto pTinaryRef = PTinRtiCast<TypeInfoArray *>(genfram.m_pTinRef);
					bool fArykMatches = false;
					auto pTinaryDock = PTinRtiCast<TypeInfoArray *>(pStnodDockCur->m_pTin);
					if (pTinaryRef && MOE_FVERIFY(pTinaryDock, "expected array dock type"))
					{
						fArykMatches |= pTinaryDock->m_aryk == pTinaryRef->m_aryk;
						fArykMatches |= pTinaryDock->m_aryk == ARYK_Reference;	// other aryk types can implicit convert to a reference
					}

					if (!pTinaryRef || !fArykMatches)
					{
						if (errep == ERREP_ReportErrors)
						{
							const char * pChzTink = (genfram.m_pTinRef) ? PChzFromTink(genfram.m_pTinRef->m_tink) : "null"; 
							auto istrTinRef = IstrFromTypeInfo(genfram.m_pTinRef);
							auto istrTinDock = IstrFromTypeInfo(pTinaryDock);
							EmitError(pTcwork, pStnodDockCur->m_lexsp, ERRID_CannotInferGeneric,
								"Failed matching generic array : matching %s, encountered %s type '%s'",
								istrTinDock.PChz(),
								pChzTink,
								istrTinRef.PChz());
						}

						return ERRID_CannotInferGeneric;
					}

					genfram.m_pTinRef = pTinaryRef->m_pTin;

					if (pStnodDockCur->CPStnodChild() == 2)
					{
						auto pStnodDim = pStnodDockCur->PStnodChild(0);
						auto pStdecl = PStnodRtiCast<STDecl *>(pStnodDim);

						if (pStdecl && pStdecl->m_fIsBakedConstant)
						{
							auto istrIdent = IstrFromIdentifier(pStdecl->m_pStnodIdentifier);

							auto pStvalLiteral = PStvalAllocAfterParse(pTcwork->m_pAlloc, PARK_Literal, pStnodDim->m_lexsp);

							//pStvalLiteral->m_litkLex = LITK_Numeric;
							pStvalLiteral->SetU64(pTinaryRef->m_c);

							pGenmapOut->m_aryPStnodManaged.Append(pStvalLiteral);
							pGenmapOut->PAncMapValue(istrIdent, pStvalLiteral);
						}
					}
					// array decl's children are [type] or [m_c, type]
					pStnodDockIt = pStnodDockCur->PStnodChildSafe(pStnodDockCur->CPStnodChild()-1);
					MOE_ASSERT(pStnodDockIt, "bad array declaration");
				} break;
			case PARK_QualifierDecl:
				{
					auto pTinqualRef = PTinRtiCast<TypeInfoQualifier *>(genfram.m_pTinRef);

					bool fGrfqualkMatches = false;
					auto pTinqualDock = PTinRtiCast<TypeInfoQualifier *>(pStnodDockCur->m_pTin);
					if (pTinqualRef && MOE_FVERIFY(pTinqualDock, "expected qualifier type"))
					{
						fGrfqualkMatches |= pTinqualDock->m_grfqualk == pTinqualRef->m_grfqualk;
					}

					if (!pTinqualRef || !fGrfqualkMatches)
					{
						// I think this is fine because we'd be adding const to an argument, do we actually need to calculate
						// const after assignment?

#if 0
						if (errep == ERREP_ReportErrors)
						{
							const char * pChzTink = (genfram.m_pTinRef) ? PChzFromTink(genfram.m_pTinRef->m_tink) : "null"; 
							//auto strTinRef = StrFromTypeInfo(genfram.m_pTinRef);
							auto istrTinRef = IstrFromTypeInfo(pTinRefEntry);
							auto istrTinDock = IstrFromTypeInfo(pStnodDockEntry->m_pTin);

							CLexerLookup lexlook(pTcwork->m_pErrman->m_pWork, pLexlocRef);

							EmitError(pTcwork, pStnodDockCur->m_lexsp, ERRID_CannotInferGeneric,
								"generic match missing qualifier : matching %s, encountered %s type '%s', matching %s (%d, %d)",
								istrTinDock.PChz(),
								pChzTink,
								istrTinRef.PChz(),
								lexlook.m_istrFilename.PChz(),
								lexlook.m_iLine,
								lexlook.m_iCodepoint);
						}

						return ERRID_CannotInferGeneric;
#endif
						
					}
					else
					{
						genfram.m_pTinRef = pTinqualRef->m_pTin;
					}

					MOE_ASSERT(pStnodDockCur->CPStnodChild() == 1, "expected one child");
					pStnodDockIt = pStnodDockCur->PStnodChildSafe(0);

				} break;
			case PARK_ReferenceDecl:
				{
					if (!grfgencomp.FIsSet(FGENCOMP_ImplicitRef))
					{
						if (auto pTinptrRef = PTinRtiCast<TypeInfoPointer *>(genfram.m_pTinRef))
						{
							genfram.m_pTinRef = pTinptrRef->m_pTin;
						}
						else if (auto pTinaryRef = PTinRtiCast<TypeInfoArray *>(genfram.m_pTinRef))
						{
							genfram.m_pTinRef = pTinaryRef->m_pTin;
						}
						else
						{
							if (errep == ERREP_ReportErrors)
							{
								const char * pChzTink = (genfram.m_pTinRef) ? PChzFromTink(genfram.m_pTinRef->m_tink) : "null"; 
								auto istrTinRef = IstrFromTypeInfo(genfram.m_pTinRef);
								auto istrTinDock = IstrFromTypeInfo(pStnodDockCur->m_pTin);
								EmitError(pTcwork, pStnodDockCur->m_lexsp, ERRID_CannotInferGeneric,
									"generic matching failed, matching %s, encountered %s type '%s'",
									istrTinDock.PChz(),
									pChzTink,
									istrTinRef.PChz());
							}
							return ERRID_CannotInferGeneric;
						}
					}
					MOE_ASSERT(pStnodDockCur->CPStnodChild() == 1, "expected one child");
					pStnodDockIt = pStnodDockCur->PStnodChildSafe(0);
				} break;
			case PARK_GenericStructSpec:
				{
					auto pTinstructBaked = PTinRtiCast<TypeInfoStruct *>(genfram.m_pTinRef);

					if (!pTinstructBaked)
					{
						if (errep == ERREP_ReportErrors)
						{
							const char * pChzTink = (genfram.m_pTinRef) ? PChzFromTink(genfram.m_pTinRef->m_tink) : "null"; 
							auto istrTin = IstrFromTypeInfo(genfram.m_pTinRef);
							EmitError(pTcwork, pStnodDockCur->m_lexsp, ERRID_CannotInferGeneric,
								"matching expected generic struct, encountered %s type '%s'",
								pChzTink,
								istrTin.PChz());
						}
						return ERRID_CannotInferGeneric;
					}

					auto pSymDock = pStnodDockCur->PSym();
					MOE_ASSERT(pSymDock && pStnodDockCur->m_strees >= STREES_TypeChecked, "expected to be type checked");
					auto pStnodDock = pSymDock->m_pStnodDefinition;
					if (!MOE_FVERIFY(pStnodDock, "expected definition"))
						break;

					LexLookup lexlookDoc(pTcwork->m_pErrman->m_pWork, pStnodDock);
					auto pStstructDock = PStnodRtiCast<STStruct *>(pStnodDock);
					auto pTinstructDock = PTinRtiCast<TypeInfoStruct *>(pStnodDock->m_pTin);

					auto pStnodRefDef = pTinstructBaked->m_pStnodStruct;
					auto pStstructRefDef = PStnodDerivedCast<STStruct *>(pStnodRefDef);

					auto pStnodListDock = pStstructDock->m_pStnodParameterList;
					auto pStnodListBakedRef = pStstructRefDef->m_pStnodBakedParameterList;

					if (pStnodListDock && pStnodListBakedRef)
					{
						MOE_ASSERT(pStnodListDock->CPStnodChild() == pStnodListBakedRef->CPStnodChild(), "child count mismatch");

						for (int ipStnodParam = 0; ipStnodParam < pStnodListDock->CPStnodChild(); ++ipStnodParam)
						{
							auto pStnodParamDock = pStnodListDock->PStnodChild(ipStnodParam);
							auto pStnodParamRef = pStnodListBakedRef->PStnodChild(ipStnodParam);

							auto pStdeclDock = PStnodRtiCast<STDecl *>(pStnodParamDock);
							if (!MOE_FVERIFY(pStdeclDock, "expected decl"))
								continue;

							if (pStdeclDock->m_fIsBakedConstant)
							{
								auto pSymRef = pStnodParamRef->PSym();
								if (MOE_FVERIFY(pSymRef && pSymRef->m_pStnodDefinition, "expected baked symbol"))
								{
									auto pStnodIdentifier = pStdeclDock->m_pStnodIdentifier;
									pGenmapOut->PAncMapValue(IstrFromIdentifier(pStnodIdentifier), pStnodParamRef);
								}
							}

							if (pStdeclDock->m_pStnodType)
							{
								auto pStnodDockType = pStdeclDock->m_pStnodType;
								auto pTinDock = pStnodDockType->m_pTin;
								if (pTinDock && FIsGenericType(pTinDock))
								{
									ERRID errid = ErridComputeDefinedGenerics(pTcwork, lexspRef, errep, FGENCOMP_None,
										pSymtab, 
										pStnodParamRef->m_pTin,
										pStnodDockType,
										pGenmapOut);
									if (errid != ERRID_Nil)
										return errid;
								}
							}
						}
					}
				} break;
			case PARK_ProcedureReferenceDecl:
				{
					auto pTinprocRef = PTinRtiCast<TypeInfoProcedure *>(genfram.m_pTinRef);
					auto pTinprocGen = PTinRtiCast<TypeInfoProcedure *>(pStnodDockCur->m_pTin);

					if (pTinprocRef && pTinprocRef->FHasGenericArgs())
					{
						if (errep == ERREP_ReportErrors)
						{
							EmitError(pTcwork, pStnodDockCur->m_lexsp, ERRID_NoGenericRValue,
								"cannot make a reference to a generic procedure definition '%s'",
								pTinprocRef->m_istrName.PChz());
						}

						return ERRID_NoGenericRValue;
					}

					auto pStproc = PStnodRtiCast<STProc *>(pStnodDockCur);
					if (MOE_FVERIFY(pStproc && pTinprocGen, "bad PARK_ProcedureReferenceDecl") &&
						pTinprocGen->FHasGenericArgs() &&
  						pStproc->m_pStnodParameterList)
					{
						bool fReferenceProcMatches = genfram.m_pTinRef == nullptr;
						if (pTinprocRef)
						{
							fReferenceProcMatches = 
								pTinprocRef->m_arypTinParams.C() == pTinprocGen->m_arypTinParams.C() &&
								pTinprocRef->m_arypTinReturns.C() == pTinprocGen->m_arypTinReturns.C() &&
								pTinprocRef->FHasVarArgs() == pTinprocGen->FHasVarArgs();
						}

						if (fReferenceProcMatches)
						{
							STNode * pStnodParameterList = pStproc->m_pStnodParameterList;
							for (int ipStnod = 0; ipStnod < pStnodParameterList->CPStnodChild(); ++ipStnod)
							{
								STNode * pStnodParam = pStnodParameterList->PStnodChild(ipStnod);
								if (pStnodParam->m_park == PARK_VariadicArg)
									continue;

								auto pGenfram = aryGenfram.AppendNew();
								pGenfram->m_pStnodDock = pStnodParam;
								pGenfram->m_pTinRef = (pTinprocRef) ? pTinprocRef->m_arypTinParams[ipStnod] : nullptr; 
							}
						}
							
					}
				} break;
			default: MOE_ASSERT(false, "Unexpected parse node PARK_%s in FComputeDefinedGenerics", PChzAbbrevFromPark(pStnodDockCur->m_park));
				break;
			}
		}
	}

	return erridReturn;
}


PROCMATCH ProcmatchCheckProcArguments(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtab,
	TypeInfoProcedure * pTinproc,
	ProcMatchParam * pPmparam,
	ERREP errep,
	ARGORD argord,
	Symbol * pSymProc)
{
	size_t cArgCall = pPmparam->m_cpStnodCall;

	auto pStnodDefinition = pTinproc->m_pStnodDefinition;
	GenericMap genmap(pTcwork->m_pAlloc, "CheckProcArgs", pTinproc);

	STNode * pStnodDefParamList = nullptr;
	STProc * pStproc = nullptr;
	if (MOE_FVERIFY(pTinproc->m_pStnodDefinition, "expected procedure definition node"))
	{
		pStproc = PStnodRtiCast<STProc *>(pTinproc->m_pStnodDefinition);

		if (pStproc)
		{
			pStnodDefParamList = pStproc->m_pStnodParameterList;
		}
	}

	// unpack  the 'packed' args supplied in the code to the unpacked list resolving named, ordered and default args.
	size_t cArgTinproc = pTinproc->m_arypTinParams.C();
	size_t cArgMax = moeMax(cArgCall, cArgTinproc); 
	CDynAry<ArgUnpack> mpIArgArgunp(pTcwork->m_pAlloc, BK_TypeCheckGenerics, cArgMax);
	mpIArgArgunp.AppendNew(cArgMax);

	if (!pTinproc->m_mpIptinGrfparmq.FIsEmpty() && pTinproc->m_mpIptinGrfparmq[0].FIsSet(FPARMQ_ImplicitRef)) 
	{
		mpIArgArgunp[0].m_grfarg.AddFlags(FARG_ImplicitRef);
	}

	if (!FUnpackArgumentList(
		pTcwork,
		pSymtab,
		cArgTinproc,
		&mpIArgArgunp,
		&genmap,
		errep,
		pStnodDefParamList,
		pPmparam->m_ppStnodCall,
		pPmparam->m_cpStnodCall,
		pPmparam->m_lexsp,
		PChzProcName(pTinproc, pSymProc),
		"procedure"))
	{
		return PROCMATCH_None;
	}

	// set cArgMax to size of trimmed argument list (omitting type arguments)
	// output argument array is sized to include type arguments and varArgs (but not named arguments)

	cArgMax = (int)mpIArgArgunp.C();

	if 	(!pTinproc->FHasVarArgs() && cArgMax > pTinproc->m_arypTinParams.C())
	{
		if (errep == ERREP_ReportErrors)
		{
			EmitError(pTcwork->m_pErrman, pPmparam->m_lexsp, ERRID_TooManyArgs,
				"Too many arguments to procedure '%s'. Expected %d but encountered %d",
				PChzProcName(pTinproc, pSymProc),
				pTinproc->m_arypTinParams.C(),
				cArgCall);
		}
		return PROCMATCH_None;
	}

	CDynAry<MatchTypeInfo> aryMtin(pTcwork->m_pAlloc, BK_TypeCheckProcmatch, cArgMax);

	//computes arypTinCallDefault and arypTinParam

	for (int iStnodArg = 0; iStnodArg < cArgMax; ++iStnodArg)
	{
		int iStnodArgAdj = (argord == ARGORD_Reversed) ? ((int)cArgMax - 1 - iStnodArg) : iStnodArg;
		STNode * pStnodArg = mpIArgArgunp[iStnodArgAdj].m_pStnodInit;

		auto pMtin = aryMtin.AppendNew();
		TypeInfo * pTinParam = nullptr;
		GRFPARMQ grfparmq = FPARMQ_None;

		bool fIsArgVariadic = iStnodArg >= (int)pTinproc->m_arypTinParams.C();
		if (!fIsArgVariadic)
		{
			pTinParam = pTinproc->m_arypTinParams[iStnodArg];
			if (!MOE_FVERIFY(pTinParam, "unknown parameter type"))
				return PROCMATCH_None;
			grfparmq = pTinproc->m_mpIptinGrfparmq[iStnodArg];
		}

		if (mpIArgArgunp[iStnodArgAdj].m_grfarg.FIsSet(FARG_TypeArgument) && pStnodArg == nullptr)
		{
			//Type arguments don't need to be explicitly named if all named type anchors are supplied

			pMtin->m_pTinParam = pTinParam;
			continue;
		}

		if (!FTryComputeMatchTypeInfo(
			pTcwork,
			pSymtab,
			pMtin,
			pStnodArg,
			pTinParam,
			iStnodArg,
			mpIArgArgunp[iStnodArgAdj].m_grfarg,
			grfparmq,
			errep))
		{
			return PROCMATCH_None;
		}

		if (fIsArgVariadic)
		{
			if (!pTinproc->FHasVarArgs())
			{
				if (errep == ERREP_ReportErrors)
				{
					EmitError(pTcwork->m_pErrman, pPmparam->m_lexsp, ERRID_TooManyArgs,
						"procedure '%s' expected %d arguments but encountered %d",
						PChzProcName(pTinproc, pSymProc),
						pTinproc->m_arypTinParams.C(),
						cArgMax);
				}
				return PROCMATCH_None;
			}

			pMtin->m_pTinCall = pMtin->m_pTinCallDefault;
			pMtin->m_pTinParam = PTinPromoteVarArg(pTcwork, pSymtab, pMtin->m_pTinCall);
		}
	}

	if (pTinproc->FHasGenericArgs())
	{
		// given known generic type anchors and unsubstituted types, generate instantiated types

		int cMtin = (int)aryMtin.C();
		for (int iMtin = 0; iMtin < cMtin; ++iMtin)
		{
			auto pMtin = &aryMtin[iMtin];
			pMtin->m_pTinParam = PTinSubstituteGenerics(pTcwork, pSymtab, pPmparam->m_lexsp, pMtin->m_pTinParam, &genmap, errep);
			if (!pMtin->m_pTinParam)
			{
				// errors should have already been reported inside pTinSubstituteGenerics
				return PROCMATCH_None;
			}

			if (pMtin->m_pStnodArg)
			{
				pMtin->m_pTinCall = PTinPromoteUntypedTightest(pTcwork, pSymtab, pMtin->m_pStnodArg, pMtin->m_pTinParam);
				pMtin->m_pTinCallDefault = PTinPromoteUntypedArgument(pTcwork, pSymtab, pMtin->m_pStnodArg, pMtin->m_pTinParam, errep);

				pMtin->m_pTinCall = PTinAfterRValueAssignment(pTcwork, pMtin->m_pStnodArg->m_lexsp, pMtin->m_pTinCall, pSymtab, pMtin->m_pTinParam);
				pMtin->m_pTinCallDefault = PTinAfterRValueAssignment(pTcwork, pMtin->m_pStnodArg->m_lexsp, pMtin->m_pTinCallDefault, pSymtab, pMtin->m_pTinParam);
			}
			else
			{
				pMtin->m_pTinCall = pMtin->m_pTinParam;
				pMtin->m_pTinCallDefault = pMtin->m_pTinParam;
			}
		}
	}

	PROCMATCH procmatch = PROCMATCH_Exact;
	for (int iStnodArg = 0; iStnodArg < cArgMax; ++iStnodArg)
	{
		// This behavior can be a bit confusing when we're calling an overloaded function with a numeric literal
		//  we consider the overload an exact match when the default promotion matches exactly, we can't use the tightest
		//  promotion because that would exact match all implicit numeric conversions (ie. 2 tightest matches to both int and float)

		auto pMtin = &aryMtin[iStnodArg];

		if (FTypesAreSame(pMtin->m_pTinCallDefault, pMtin->m_pTinParam))
			continue;

		if (FCanImplicitCast(pMtin->m_pTinCall, pMtin->m_pTinParam))
		{
			procmatch = PROCMATCH_ImplicitCast;
		}
		else
		{
			if (errep == ERREP_ReportErrors)
			{
				GenericMapScope genscope(pTcwork->m_pErrman, (genmap.FIsEmpty()) ? nullptr : &genmap);

				InString istrTinCall = IstrFromTypeInfo(pMtin->m_pTinCall);
				InString istrTinParam = IstrFromTypeInfo(pMtin->m_pTinParam);

				EmitError(pTcwork->m_pErrman, pPmparam->m_lexsp, ERRID_BadImplicitConversion,
					"procedure call '%s' cannot convert argument %d from type %s to %s",
					PChzProcName(pTinproc, pSymProc),
					iStnodArg+1,
					istrTinCall.PChz(),
					istrTinParam.PChz());
			}
			procmatch = PROCMATCH_None;
			break;
		}
	}

	if (procmatch == PROCMATCH_Exact || procmatch == PROCMATCH_ImplicitCast)
	{
		MOE_ASSERT(pPmparam->m_pPmfit == nullptr, "leaking proc match fit struct");
		auto pPmfit = MOE_NEW(pPmparam->m_pAlloc, ProcMatchFit) ProcMatchFit(pPmparam->m_pAlloc);

		pPmparam->m_pPmfit = pPmfit;
		pPmfit->m_mpIArgArgunp.Swap(&mpIArgArgunp);

		if (!genmap.FIsEmpty())
		{
			auto pGenmap = PGenmapNew(pTcwork, "", nullptr);
			pGenmap->m_aryLexspSrc.Append(pPmparam->m_lexsp);
			pGenmap->Swap(&genmap);
			pPmfit->m_pGenmap = pGenmap;
		}
	}

	return procmatch;
}

TCRET TcretTryFindMatchingProcedureCall(
	TypeCheckWorkspace * pTcwork, 
	TypeCheckFrame * pTcfram,
	InString istrProcName,
	SymbolTable * pSymtab,
	ProcMatchParam * pPmparam,
	Symbol ** ppSym,
	ARGORD * pArgord,
	GRFSYMLOOK grfsymlook)
{
	SymbolTable::SymbolIterator symiter;
	if (!istrProcName.FIsEmpty())
	{
		symiter = SymbolTable::SymbolIterator(pSymtab, istrProcName, pPmparam->m_lexsp, grfsymlook);
	}

	if (symiter.FIsDone())
	{
		if (pPmparam->m_fMustFindMatch)
		{
			EmitError(pTcwork->m_pErrman, pPmparam->m_lexsp, ERRID_CantFindProc,
				"unknown procedure in type check: %s", istrProcName.m_pChz);
		}
		return TCRET_StoppingError;
	}

	// the first argument is index 1, (the procedure's identifier is element zero)

	int cSymOptions = 0;

	struct SymMatch
	{
		Symbol *	m_pSym;
		PROCMATCH 	m_procmatch;
		ARGORD		m_argord;
	};
	CFixAry<SymMatch, 32> arySymmatch;
	int	mpProcmatchCSymmatch[PROCMATCH_Max];
	ProcMatchFit * mpProcmatchPPmfit[PROCMATCH_Max];
	ZeroAB(mpProcmatchCSymmatch, sizeof(mpProcmatchCSymmatch));
	ZeroAB(mpProcmatchPPmfit, sizeof(mpProcmatchPPmfit));

	Symbol * pSymIt;
	while ((pSymIt = symiter.PSymNext()))
	{
		STNode * pStnodDefinition = pSymIt->m_pStnodDefinition;
		if (!pStnodDefinition)
			continue;

		if (pStnodDefinition->m_strees < STREES_SignatureTypeChecked)
		{
			for (auto ppPmfit = mpProcmatchPPmfit; ppPmfit != MOE_PMAC(mpProcmatchPPmfit); ++ppPmfit)
			{
				if (*ppPmfit)
				{
					pPmparam->m_pAlloc->MOE_DELETE(*ppPmfit);
				}
			}

			// wait for this procedure's signature to be type checked.
			*ppSym = pSymIt;
			return TCRET_WaitingForSymbolDefinition;
		}

		TypeInfo * pTinSymIt = PTinFromSymbol(pSymIt);
		if (!MOE_FVERIFY(pTinSymIt, "bad symbol in proc call lookup"))
			continue;

		if (pTinSymIt->m_tink == TINK_Struct)
		{
			++cSymOptions;

			auto pTinSymIt = PTinFromSymbol(pSymIt);
			auto pTinstructSym = PTinRtiCast<TypeInfoStruct  *>(pTinSymIt);
			if (!MOE_FVERIFY(pTinstructSym, "expected type info procedure"))
				continue;

			ProcMatchParam pmparamTry(*pPmparam);
			auto procmatch = ProcmatchCheckStructArguments(
								pTcwork,
								pSymtab,
								pTinstructSym,
								&pmparamTry,
								ERREP_HideErrors);
			GenericMap * pGenmap = (pmparamTry.m_pPmfit) ? pmparamTry.m_pPmfit->m_pGenmap : nullptr;
			if (procmatch == PROCMATCH_None || !MOE_FVERIFY(pGenmap, "Expected generic mapping"))
				continue;

			++mpProcmatchCSymmatch[procmatch];
			auto pSymmatch = arySymmatch.AppendNew();
			pSymmatch->m_pSym = pSymIt;
			pSymmatch->m_procmatch = procmatch;

			if (mpProcmatchPPmfit[procmatch] == nullptr)
			{
				mpProcmatchPPmfit[procmatch] = pmparamTry.m_pPmfit;
				pmparamTry.m_pPmfit = nullptr;
			}
		}
		else if (pTinSymIt->m_tink == TINK_Procedure)
		{
			++cSymOptions;
			auto pTinprocSym = PTinRtiCast<TypeInfoProcedure  *>(pTinSymIt);
			if (!MOE_FVERIFY(pTinprocSym, "expected type info procedure"))
				continue;

			int argordMax = ARGORD_Normal+1;
			if (pTinprocSym->m_grftinproc.FIsSet(FTINPROC_IsCommutative))
			{
				argordMax = ARGORD_Max;
				++cSymOptions;
			}

			for (int argord = ARGORD_Min; argord < argordMax; ++argord)
			{
				ProcMatchParam pmparamTry(*pPmparam);
				auto procmatch = ProcmatchCheckProcArguments(pTcwork, pSymtab, pTinprocSym, &pmparamTry, ERREP_HideErrors, (ARGORD)argord, pSymIt);
				if (procmatch != PROCMATCH_None)
				{
					++mpProcmatchCSymmatch[procmatch];
					auto pSymmatch = arySymmatch.AppendNew();
					pSymmatch->m_pSym = pSymIt;
					pSymmatch->m_procmatch = procmatch;
					pSymmatch->m_argord = (ARGORD)argord;

					if (mpProcmatchPPmfit[procmatch] == nullptr)
					{
						mpProcmatchPPmfit[procmatch] = pmparamTry.m_pPmfit;
						pmparamTry.m_pPmfit = nullptr;
					}
				}
			}
		}
	}

	PROCMATCH procmatchFinal = (mpProcmatchCSymmatch[PROCMATCH_Exact] > 0) ? PROCMATCH_Exact : PROCMATCH_ImplicitCast;

	// set pPmparam to point to the final SProcMatchFit struct and clean up the rest
	for (int procmatch = PROCMATCH_Min; procmatch < PROCMATCH_Max; ++procmatch)
	{
		if (procmatch == (int)procmatchFinal)
		{
			pPmparam->m_pPmfit = mpProcmatchPPmfit[procmatch];	
		}
		else if (mpProcmatchPPmfit[procmatch])	
		{
			pPmparam->m_pAlloc->MOE_DELETE(mpProcmatchPPmfit[procmatch]);
		}
		mpProcmatchPPmfit[procmatch] = nullptr;
	}

	int cSysmatch = mpProcmatchCSymmatch[procmatchFinal];
	if (cSysmatch == 0)
	{
		if (cSymOptions == 0)
		{
			if (pPmparam->m_fMustFindMatch)
			{
				EmitError(pTcwork->m_pErrman, pPmparam->m_lexsp, ERRID_CantFindProc, "'%s' does not evaluate to a procedure.", istrProcName.PChz());
			}
		}
		else if (cSymOptions == 1)
		{
			// print out non overloaded mismatch errors.
			Symbol * pSymProc = pSymtab->PSymLookup(istrProcName, pPmparam->m_lexsp, grfsymlook);
			if (pPmparam->m_fMustFindMatch)
			{
				TypeInfo * pTinSym = PTinFromSymbol(pSymProc);
				if (pTinSym)
				{
					switch (pTinSym->m_tink)
					{
					case TINK_Procedure:
						{
							auto pTinproc = (TypeInfoProcedure *)pTinSym;
							(void)ProcmatchCheckProcArguments(pTcwork, pSymtab, pTinproc, pPmparam, ERREP_ReportErrors, ARGORD_Normal, pSymProc);
						}break;
					case TINK_Struct:
						{
							auto pTinstruct = (TypeInfoStruct *)pTinSym;

							(void)ProcmatchCheckStructArguments(
								pTcwork,
								pSymtab,
								pTinstruct,
								pPmparam,
								ERREP_ReportErrors);
						}break;
					default:
						MOE_ASSERT(false, "expected type info kind %s", PChzFromTink(pTinSym->m_tink));
					}
				}

				if (!pTcwork->m_pErrman->FHasErrors() && !pTcwork->m_pErrman->FHasHiddenErrors())
				{
					EmitError(pTcwork->m_pErrman, pPmparam->m_lexsp, ERRID_UnknownError,
						"error type matching symbol '%s' (one option, unknown match error)", istrProcName.PChz());
				}
			}
		}
		else if (pPmparam->m_fMustFindMatch)
		{
			Error error(pTcwork->m_pErrman);
			PrintErrorLine(&error, "Error:", pPmparam->m_lexsp, "No overload matches procedure call. Options are:");

			symiter = SymbolTable::SymbolIterator(pSymtab, istrProcName, pPmparam->m_lexsp, grfsymlook);
			while (Symbol * pSym = symiter.PSymNext())
			{
				auto pTinproc = PTinDerivedCast<TypeInfoProcedure *>(PTinFromSymbol(pSym));
				InString strProc = IstrFromTypeInfo(pTinproc);

				PrintErrorLine(&error, "   ", pSym->m_pStnodDefinition->m_lexsp, "%s", strProc.PChz());
			}
		}
		return TCRET_StoppingError;
	}
	else if (cSysmatch == 1)
	{
		SymMatch * pSymmatch = nullptr;
		for (size_t iSymmatch = 0; iSymmatch < arySymmatch.C(); ++iSymmatch)
		{
			if (arySymmatch[iSymmatch].m_procmatch == procmatchFinal)
			{
				pSymmatch = &arySymmatch[iSymmatch];
				break;
			}
		}
		if (!MOE_FVERIFY(pSymmatch, "matching procedure lookup failed"))
			return TCRET_StoppingError;

		Symbol * pSymProc = pSymmatch->m_pSym;
		TypeCheckStackEntry * pTcsentTop = pTcfram->m_aryTcsent.PLast();

		// mark the symbol used if not a generic (generics will be marked used during instantiation)
		if (!pPmparam->m_pPmfit || !pPmparam->m_pPmfit->m_pGenmap)
		{
			AddSymbolReference(pTcsentTop->m_pSymContext, pSymProc);
		}

		*ppSym = pSymProc;
		*pArgord = pSymmatch->m_argord;
	}
	else // cSymmatch > 1 
	{	

		Error error(pTcwork->m_pErrman, ERRID_AmbiguousOverload);
		PrintErrorLine(&error, "Error:", pPmparam->m_lexsp, "Overloaded procedure is ambiguous. Options are:");

		SymMatch * pSymmatchMac = arySymmatch.PMac();
		for (SymMatch * pSymmatch = arySymmatch.A(); pSymmatch != pSymmatchMac; ++pSymmatch)
		{
			Symbol * pSym = pSymmatch->m_pSym;
			auto pTinproc = PTinDerivedCast<TypeInfoProcedure *>(PTinFromSymbol(pSym));
			InString istrProc = IstrFromTypeInfo(pTinproc);

			if (pSymmatch->m_argord == ARGORD_Reversed)
			{
				PrintErrorLine(&error, "   ", pSym->m_pStnodDefinition->m_lexsp, "%s (reversed)", istrProc.m_pChz);
			}
			else
			{
				PrintErrorLine(&error, "   ", pSym->m_pStnodDefinition->m_lexsp, "%s", istrProc.m_pChz);
			}
		}

		// just return any one of the ambiguous symbols so it doesn't error claiming "doesn't evaluate to a procedure"
		auto pSymmatch = &arySymmatch[0];
		*ppSym = pSymmatch->m_pSym;
		*pArgord = pSymmatch->m_argord;
	}

	return TCRET_Complete;
}



struct SOverloadCheck // tag ovcheck
{
			SOverloadCheck(TypeInfoProcedure * pTinproc, TCRET tcret = TCRET_StoppingError, ARGORD argord = ARGORD_Normal)
			:m_pTinproc(pTinproc)
			,m_tcret(tcret)
			,m_argord(argord)
				{ ; }

	TypeInfoProcedure *		m_pTinproc;
	TCRET					m_tcret;
	ARGORD					m_argord;
};


SOverloadCheck OvcheckTryCheckOverload(
	TypeCheckWorkspace * pTcwork,
	TypeCheckFrame * pTcfram,
	STNode * pStnod,
	ProcMatchParam * pPmparam)
{
	CDynAry<TypeCheckStackEntry> * paryTcsent = &pTcfram->m_aryTcsent;
	TypeCheckStackEntry * pTcsentTop = paryTcsent->PLast();

	SymbolTable * pSymtab = pTcsentTop->m_pSymtab;
	Moe::InString istrOverload = IstrOverloadNameFromTok(pStnod->m_tok);
	if (istrOverload.FIsEmpty())
		return SOverloadCheck(nullptr);

	InString strProcName(istrOverload);
	Symbol * pSymProc = nullptr;
	ARGORD argord = ARGORD_Normal;
	TCRET tcret = TcretTryFindMatchingProcedureCall(
					pTcwork, pTcfram, strProcName, pSymtab, pPmparam, &pSymProc, &argord, pTcsentTop->m_grfsymlook);

	auto pTinproc = (pSymProc) ? PTinRtiCast<TypeInfoProcedure*>(PTinFromSymbol(pSymProc)) : nullptr;

	if (pTinproc && tcret == TCRET_WaitingForSymbolDefinition)
	{
		// wait for this procedure's signature to be type checked.
		UnknownType * pUntype = PUntypeEnsure(pTcwork, pSymProc);
		pUntype->m_arypTcframDependent.Append(pTcfram);

		return SOverloadCheck(pTinproc, tcret, argord);
	}
	else if (tcret != TCRET_Complete)
	{
		return SOverloadCheck(nullptr);
	}

	if (pTinproc)
	{
		STNode * pStnodDefinition = pSymProc->m_pStnodDefinition;

		MOE_ASSERT(pStnodDefinition->m_strees >= STREES_SignatureTypeChecked, "expected definition to be type checked");
		MOE_ASSERT(pStnodDefinition->m_pTin == pTinproc, "tin mismatch");
	}

	MOE_ASSERT(pStnod->m_pTin == nullptr, "assignment op has no 'return' value");

	return SOverloadCheck(pTinproc, TCRET_Complete, argord);
}

static SymbolPath * PSympLookup(
	SymbolTable::SUsing * pUsingSource,
	InString istr,
	const LexSpan & lexsp,
	GRFSYMLOOK grfsymlook,
	int cpSymPath = 0)
{
	auto pSymtab = pUsingSource->m_pSymtab;
	auto pSym = pSymtab->PSymLookup(istr, lexsp, grfsymlook);
	if (pSym)
	{
		auto pSymp = MOE_NEW(pSymtab->m_pAlloc, SymbolPath) SymbolPath;
		pSymp->m_symk = SYMK_Path;
		pSymp->m_arypSym.SetAlloc(pSymtab->m_pAlloc, BK_Symbol);
		pSymp->m_arypSym.AppendFill(cpSymPath+1, nullptr);
		pSymp->m_arypSym[cpSymPath] = pSym;
		pSymp->m_arypSym[cpSymPath-1] = pUsingSource->m_pStnod->PSym();

		return pSymp;
	}

	for (auto pUsingIt = pSymtab->m_aryUsing.A(); pUsingIt != pSymtab->m_aryUsing.PMac(); ++pUsingIt)
	{
		auto pSymp = PSympLookup(pUsingIt, istr, lexsp, grfsymlook, cpSymPath + 1);
		if (pSymp)
		{
			pSymp->m_arypSym[cpSymPath-1] = pUsingSource->m_pStnod->PSym();
			return pSymp;
		}
	}
	return nullptr;
}

SymbolBase * PSymbaseLookup(
	SymbolTable * pSymtab,
	InString istr,
	const LexSpan & lexsp,
	GRFSYMLOOK grfsymlook)
{
	if (grfsymlook.FIsSet(FSYMLOOK_Local))
	{
		auto pSym = pSymtab->PSymLookup(istr, lexsp, grfsymlook);
		if (pSym)
			return pSym;

		for (auto pUsing = pSymtab->m_aryUsing.A(); pUsing != pSymtab->m_aryUsing.PMac(); ++pUsing)
		{
			auto ppSymp = pUsing->m_hashIstrPSymp.Lookup(istr);
			if (ppSymp)
			{
				return *ppSymp;
			}
		}

		for (auto pUsing = pSymtab->m_aryUsing.A(); pUsing != pSymtab->m_aryUsing.PMac(); ++pUsing)
		{
			auto pSymp = PSympLookup(pUsing, istr, lexsp, grfsymlook | FSYMLOOK_IgnoreOrder, 1);
			if (pSymp)
			{
				(void) pUsing->m_hashIstrPSymp.InresEnsureKeyAndValue(istr, pSymp);
				return pSymp;
			}
		}
	}

	SymbolTable * pSymtabIt = (grfsymlook.FIsSet(FSYMLOOK_Ancestors)) ? pSymtab->m_pSymtabParent : nullptr;
	LexSpan lexspChild = lexsp;
	while (pSymtabIt)
	{
		auto pSym = pSymtabIt->PSymLookup(istr, lexsp, grfsymlook);
		if (pSym)
			return pSym;

		for (auto pUsing = pSymtabIt->m_aryUsing.A(); pUsing != pSymtabIt->m_aryUsing.PMac(); ++pUsing)
		{
			auto ppSymp = pUsing->m_hashIstrPSymp.Lookup(istr);
			if (ppSymp)
			{
				return *ppSymp;
			}
		}

		for (auto pUsing = pSymtabIt->m_aryUsing.A(); pUsing != pSymtabIt->m_aryUsing.PMac(); ++pUsing)
		{
			auto pSymp = PSympLookup(pUsing, istr, lexsp, grfsymlook | FSYMLOOK_IgnoreOrder, 1);
			if (pSymp)
			{
				(void) pUsing->m_hashIstrPSymp.InresEnsureKeyAndValue(istr, pSymp);
				return pSymp;
			}
		}
		
		pSymtabIt = pSymtabIt->m_pSymtabParent;
	}

	return nullptr;
}

SymbolTable * PSymtabFromType(TypeCheckWorkspace * pTcwork, TypeInfo * pTin, const LexSpan & lexsp)
{
	switch (pTin->m_tink)
	{
	case TINK_Pointer:
		{
			auto pTinptr = (TypeInfoPointer *)pTin;
			return PSymtabFromType(pTcwork, pTinptr->m_pTin, lexsp);
		}
	case TINK_Qualifier:
		{
			auto pTinqual = (TypeInfoQualifier *)pTin;
			return PSymtabFromType(pTcwork, pTinqual->m_pTin, lexsp);
		}
	case TINK_Struct:
		{
			auto pTinstruct = (TypeInfoStruct *)pTin;
			MOE_ASSERT(pTinstruct->m_pStnodStruct->m_pSymtab, "missing symbol table");
			return pTinstruct->m_pStnodStruct->m_pSymtab;
		} break;
	case TINK_Enum:
		{
			auto pTinenum = (TypeInfoEnum *)pTin;
			auto pSymtab = pTinenum->m_tinstructProduced.m_pStnodStruct->m_pSymtab;
			MOE_ASSERT(pSymtab, "missing symbol table");
			return pSymtab;
		} break;
	default:
		auto istrTin = IstrFromTypeInfo(pTin);
		EmitError(pTcwork->m_pErrman, lexsp, ERRID_UsingStatementBadType,
			"cannot supply type '%s' in a 'using' statement", istrTin.m_pChz);
		return nullptr;
	}
}

TypeInfo * PTinStripQualifiers(TypeInfo * pTin, GRFQUALK * pGrfqualk)
{
	int cQualifiers = 0;
	while (pTin->m_tink == TINK_Qualifier)
	{
		auto pTinqual = (TypeInfoQualifier *)pTin;
		*pGrfqualk = pTinqual->m_grfqualk;
		pTin = pTinqual->m_pTin;
		MOE_ASSERT(++cQualifiers < 2, "TypeInfoQualifiers should not be directly nested");
	}
	return pTin;
}

TypeInfo * PTinStripQualifiers(TypeInfo * pTin)
{
	int cQualifiers = 0;
	while (pTin->m_tink == TINK_Qualifier)
	{
		auto pTinqual = (TypeInfoQualifier *)pTin;
		pTin = pTinqual->m_pTin;
		MOE_ASSERT(++cQualifiers < 2, "TypeInfoQualifiers should not be directly nested");
	}
	return pTin;
}

TypeInfo * PTinStripEnumToLoose(TypeInfo * pTin)
{
	if (pTin->m_tink != TINK_Enum)
		return pTin;

	return ((TypeInfoEnum *)pTin)->m_pTinLoose;
}

TypeInfo * PTinStripQualifiersAndPointers(TypeInfo * pTin)
{
	int cQualifiers = 0;
	while (pTin)
	{
		switch (pTin->m_tink)
		{
		case TINK_Qualifier:
			{
				auto pTinqual = (TypeInfoQualifier *)pTin;
				pTin = pTinqual->m_pTin;
				MOE_ASSERT(++cQualifiers < 2, "TypeInfoQualifiers should not be directly nested");
			} break;
		case TINK_Pointer:
			{
				auto pTinptr = (TypeInfoPointer *)pTin;
				pTin = pTinptr->m_pTin;
			} break;
		default:
			return pTin;
		}
	}
	return pTin;
}

BigInt BintFromStval(STValue * pStval)
{
	switch (pStval->m_stvalk)
	{
	case STVALK_SignedInt:		return BintFromInt(pStval->m_nSigned);
	case STVALK_UnsignedInt:	return BintFromUint(pStval->m_nUnsigned, false);
#if MOEB_LATER
	case STVALK_ReservedWord:
		{

			MOE_ASSERT(pStval->m_litkLex == LITK_Integer || pStval->m_litkLex == LITK_Bool, "Can't create Bint from non integer reserved word");
			return BintFromUint(pStval->m_nUnsigned, false);
		}
#endif
	default:
		MOE_ASSERT(false, "Can't create Bint from non integer value");
		return BigInt();
	}
}

TypeInfo * PTinFromBint(
	TypeCheckWorkspace *pTcwork,
	SymbolTable * pSymtab,
	BigInt bint)
{
	if (!bint.m_fIsNegative)
	{
		u64 nUnsigned = bint.U64Coerce();

		if (nUnsigned >  LLONG_MAX)
			return pSymtab->PTinBuiltin(BuiltIn::g_istrU64);
	}
	
	s64 nSigned = bint.S64Coerce();
	if ((nSigned <= SCHAR_MAX) & (nSigned > SCHAR_MIN))	return pSymtab->PTinBuiltin(BuiltIn::g_istrS8);
	if ((nSigned <= SHRT_MAX) & (nSigned > SHRT_MIN))	return pSymtab->PTinBuiltin(BuiltIn::g_istrS16);
	if ((nSigned <= INT_MAX) & (nSigned > INT_MIN))		return pSymtab->PTinBuiltin(BuiltIn::g_istrS32);
	return pSymtab->PTinBuiltin(BuiltIn::g_istrS64);
}

inline TypeInfo * PTinFromLiteralFinalized(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtab,
	const TypeInfoLiteral * pTinlit)
{
	MOE_ASSERT(pTinlit->m_fIsFinalized, "Expected finalized literal type");

	const LiteralType & litty = pTinlit->m_litty;
	switch (litty.m_litk)
	{
	case LITK_Numeric:
		{
			if (litty.m_grfnum.FIsSet(FNUM_IsFloat))
			{
				switch (litty.m_cBit)
				{
				case 32:	return pSymtab->PTinBuiltin(BuiltIn::g_istrF32);
				case 64:	return pSymtab->PTinBuiltin(BuiltIn::g_istrF64);
				}
			}
			else if (litty.m_grfnum.FIsSet(FNUM_IsSigned))
			{
				switch (litty.m_cBit)
				{
				case 8:		return pSymtab->PTinBuiltin(BuiltIn::g_istrS8);
				case 16:	return pSymtab->PTinBuiltin(BuiltIn::g_istrS16);
				case 32:	return pSymtab->PTinBuiltin(BuiltIn::g_istrS32);
				case 64:	return pSymtab->PTinBuiltin(BuiltIn::g_istrS64);
				}
			}
			else
			{
				switch (litty.m_cBit)
				{
				case 8:		return pSymtab->PTinBuiltin(BuiltIn::g_istrU8);
				case 16:	return pSymtab->PTinBuiltin(BuiltIn::g_istrU16);
				case 32:	return pSymtab->PTinBuiltin(BuiltIn::g_istrU32);
				case 64:	return pSymtab->PTinBuiltin(BuiltIn::g_istrU64);
				}
			}
		}break;
	case LITK_Char:		return pSymtab->PTinBuiltin(BuiltIn::g_istrChar);
	case LITK_Bool:		return pSymtab->PTinBuiltin(BuiltIn::g_istrBool);
	case LITK_String:
		{
			// right now string literals just promote to *u8, but will eventually promote to string
			auto pTinU8 = pSymtab->PTinBuiltin(BuiltIn::g_istrU8);
			return pSymtab->PTinptrAllocate(pTinU8);
		} break;
	case LITK_Enum:
		{
			MOE_ASSERT(false, "enum literals should not be finalized");
		} break;
	case LITK_Null:
		{
			return pTinlit->m_pTinSource;
		} break;
	default:
		break;
	}

	MOE_ASSERT(false, "Unknown literal kind %d", litty.m_litk);
	return nullptr;
}

int ITypemembLookup(TypeInfoStruct * pTinstruct, const InString & istrMemberName)
{
	// BB - could just store the members in a contiguous array... simplify this loop
	int iTypememb = 0;
	auto pTypemembMax = pTinstruct->m_aryTypemembField.PMac();
	for (auto pTypememb = pTinstruct->m_aryTypemembField.A(); pTypememb != pTypemembMax; ++pTypememb, ++iTypememb)
	{
		if (pTypememb->m_istrName == istrMemberName)
			return iTypememb;
	}
	return -1;
}

static inline TypeInfo * PTinPromoteUntypedCommon(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtab,
	bool * pFWasHandled,
	STNode * pStnodLit,
	TypeInfo * pTinExpected,
	ERREP errep)
{
	*pFWasHandled = true;
	TypeInfoLiteral * pTinlit = (TypeInfoLiteral *)pStnodLit->m_pTin;
	if (!pTinlit)
		return pStnodLit->m_pTin;

	if (pTinlit->m_tink != TINK_Literal)
		return pStnodLit->m_pTin;

	if (pTinlit->m_litty.m_litk == LITK_Compound)
	{
		// if this is a constant we need to look up the source STNode
		if (MOE_FVERIFY(pTinlit->m_pStnodDefinition, "bad array literal definition"))
		{
			pStnodLit = pTinlit->m_pStnodDefinition;
		}

		auto pStdecl = PStnodRtiCast<STDecl *>(pStnodLit);
		if (!MOE_FVERIFY(pStdecl, "bad array literal"))
			return nullptr;

		auto pStnodInit = pStdecl->m_pStnodInit;
		auto pTinSource = pTinlit->m_pTinSource;
		if (!pTinSource)
		{
			TypeInfo * pTinElement = nullptr;
			if (pTinExpected)
			{
				if (pTinExpected->m_tink == TINK_Pointer)
				{
					auto pTinptrExpected = (TypeInfoPointer *)pTinExpected;
					pTinElement = pTinptrExpected->m_pTin;
				}
				else
				{
					auto pTinUnqualExpected = PTinStripQualifiers(pTinExpected);
					if (MOE_FVERIFY(
						pTinUnqualExpected->m_tink == TINK_Array ||
						pTinUnqualExpected->m_tink == TINK_Struct,
						"Unexpected compound literal subtype (%s)", PChzFromTink(pTinUnqualExpected->m_tink)))
					{
						pTinSource = pTinExpected;
					}
				}
			}

			if (!pTinSource)
			{
				if (!pTinElement)
				{
					pTinElement = PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodInit->PStnodChild(0));
				}

				TypeInfoArray * pTinary = MOE_NEW(pSymtab->m_pAlloc, TypeInfoArray) TypeInfoArray();
				pTinary->m_pTin = pTinElement;
				pTinary->m_c = (pTinlit->m_c >= 0) ? pTinlit->m_c : pStnodInit->CPStnodChild();
				pTinary->m_aryk = ARYK_Fixed;
				pSymtab->AddManagedTin(pTinary);
				pTinary = pSymtab->PTinMakeUnique(pTinary);

				pTinSource = pTinary;
			}
		}

		if (pTinSource && pStnodInit && MOE_FVERIFY(pStnodInit->m_park == PARK_ExpressionList, "expression list expected"))
		{
			pTinSource = PTinStripQualifiers(pTinSource);

			bool fWasHandled;
			auto pStnodInit = pStdecl->m_pStnodInit;
			switch(pTinSource->m_tink)
			{
			case TINK_Array:
				{
					auto pTinary = (TypeInfoArray *)pTinSource;
					if (pTinary->m_grftin.FIsSet(FTIN_IsUnique))
					{
						pTinary = pSymtab->PTinaryCopy(pTinary);
					}

					MOE_ASSERT(!pTinary->m_grftin.FIsSet(FTIN_IsUnique), "modifying unique pTinAry");
					pTinary = pSymtab->PTinMakeUnique(pTinary);

					if (errep == ERREP_ReportErrors)
					{
						auto cElementLit = pTinlit->m_c;
						auto pStnodDef = pTinlit->m_pStnodDefinition;
						if (cElementLit < 0 && pStnodDef)
						{
							auto pStdecl = PStnodRtiCast<STDecl *>(pStnodDef);
							STNode * pStnodList = (pStdecl) ? pStdecl->m_pStnodInit : nullptr;

							if (pStnodList)
							{
								cElementLit = pStnodList->CPStnodChild();
							}
						}

						if (pTinary->m_c > cElementLit)
						{
							auto istrAry = IstrFromTypeInfo(pTinary);
							auto istrLit = IstrFromTypeInfo(pTinlit);

							// BB - We should generate a new literal that pads out the array values rather than error here
							EmitError(pTcwork, pStnodLit->m_lexsp, ERRID_InvalidCast,
								"cannot cast literal to different element count %s ->%s.  (should be fixed later)",
								istrLit.m_pChz,
								istrAry.m_pChz);
						}
					}

					for (int ipStnod = 0; ipStnod < pStnodInit->CPStnodChild(); ++ipStnod)
					{
						auto pStnodIt = pStnodInit->PStnodChild(ipStnod);
						if (!pStnodIt->m_pTin || pStnodIt->m_pTin->m_tink != TINK_Literal)
							continue;

						(void) PTinPromoteUntypedCommon(pTcwork, pSymtab, &fWasHandled, pStnodIt, pTinary->m_pTin, errep);
					}
				} break;
			case TINK_Struct:
				{
					auto pTinstruct = (TypeInfoStruct *)pTinSource;
					for (int ipStnod = 0; ipStnod < pStnodInit->CPStnodChild(); ++ipStnod)
					{
						auto pStnodIt = pStnodInit->PStnodChild(ipStnod);
						if (!pStnodIt->m_pTin || pStnodIt->m_pTin->m_tink != TINK_Literal)
							continue;

						int iTypememb = ipStnod;
						STNode * pStnodValue;
						if (pStnodIt->m_park == PARK_ArgumentLabel)
						{
							STNode * pStnodIdentifier = pStnodIt->PStnodChild(0);
							InString istrIdentifier(IstrFromIdentifier(pStnodIdentifier));

							iTypememb = ITypemembLookup(pTinstruct, istrIdentifier);
							pStnodValue = pStnodIt->PStnodChildSafe(1);
						}
						else
						{
							pStnodValue = pStnodIt;
						}

						if (iTypememb < 0 || iTypememb >= pTinstruct->m_aryTypemembField.C())
							continue;

						auto pTypememb  = &pTinstruct->m_aryTypemembField[iTypememb];

						auto pTinIt = PTinPromoteUntypedCommon(pTcwork, pSymtab, &fWasHandled, pStnodValue, pTypememb->m_pTin, errep);
					}
				} break;
			default:
				MOE_ASSERT(false, "unexpected literal type");
				break;
			}
		}
		return pSymtab->PTinqualWrap(pTinSource, FQUALK_Const);
	}

	if (pTinlit->m_fIsFinalized)
	{
		auto pTinFinalized = PTinFromLiteralFinalized(pTcwork, pSymtab, pTinlit);
		return pSymtab->PTinqualWrap(pTinFinalized, FQUALK_Const);
	}

	const STValue * pStval = PStnodRtiCast<STValue *>(pStnodLit);
	if (!MOE_FVERIFY(pStval, "literal without value"))
		return nullptr;

	*pFWasHandled = false;
	return nullptr;
}

TypeInfo * PTinPromoteUntypedDefault(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtab,
	STNode * pStnodLit,
	TypeInfo * pTinExpected,
	ERREP errep)
{
	bool fWasHandled;
	TypeInfo * pTinReturn = PTinPromoteUntypedCommon(pTcwork, pSymtab, &fWasHandled, pStnodLit, pTinExpected, errep);
	if (fWasHandled)
		return pTinReturn;

	TypeInfoLiteral * pTinlit = (TypeInfoLiteral *)pStnodLit->m_pTin;
	const LiteralType & litty = pTinlit->m_litty;

	switch (litty.m_litk)
	{
	case LITK_Numeric:
		{
			if (litty.m_grfnum.FIsSet(FNUM_IsFloat))
			{
				return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrFloat);
			}

			bool fIsSigned = litty.m_grfnum.FIsSet(FNUM_IsSigned);
			if (fIsSigned == false)
			{
				const STValue * pStval = PStnodRtiCast<STValue *>(pStnodLit);
				s64 nUnsigned = NUnsignedLiteralCast(pTcwork, pStval);
				fIsSigned = (nUnsigned < LLONG_MAX);
			}
			return pSymtab->PTinqualBuiltinConst((fIsSigned) ? BuiltIn::g_istrInt : BuiltIn::g_istrUint);
		}
	case LITK_Char:		return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrChar);
	case LITK_String:
	{
		// right now string literals just promote to *u8, but will eventually promote to string
		auto pTinU8 = pSymtab->PTinBuiltin(BuiltIn::g_istrU8);
		auto pTinqual = pSymtab->PTinqualEnsure(pTinU8, FQUALK_Const);
		return pSymtab->PTinqualWrap(pSymtab->PTinptrAllocate(pTinqual), FQUALK_Const);
	}
	case LITK_Bool:		return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrBool);
	case LITK_Null:
		{
			auto pTinU8 = pSymtab->PTinBuiltin(BuiltIn::g_istrVoid);
			return pSymtab->PTinptrAllocate(pTinU8);
		}break;
	case LITK_Enum:
		{
			auto pTinenum = PTinRtiCast<TypeInfoEnum *>(pTinlit->m_pTinSource);
			MOE_ASSERT(pTinenum, "Failed to infer type for enum literal");
			return pSymtab->PTinqualWrap(pTinenum, FQUALK_Const);
		}
	case LITK_Nil: 
		MOE_ASSERT(false, "Cannot infer type for LITK_Nil");
	default:
		MOE_ASSERT(false, "Cannot infer type for unexpected literal kind");
		break;
	}
	return nullptr;
}

TypeInfo * PTinPromoteUntypedArgument(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtab,
	STNode * pStnodLit,
	TypeInfo * pTinArgument,
	ERREP errep)
{
	TypeInfoLiteral * pTinlit = PTinRtiCast<TypeInfoLiteral *>(pStnodLit->m_pTin);
	if (pTinlit)
	{
		const LiteralType & litty = pTinlit->m_litty;
		if (litty.m_litk == LITK_Null)
		{
			if (pTinArgument && pTinArgument->m_tink == TINK_Pointer )
				return pTinArgument;

			return pSymtab->PTinptrAllocate(pSymtab->PTinBuiltin(BuiltIn::g_istrVoid));
		}
	}

	return PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodLit, pTinArgument, errep);
}


inline TypeInfo * PTinPromoteUntypedTightest(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtab,
	STNode * pStnodLit,	
	TypeInfo * pTinDst,
	ERREP errep)
{
	pTinDst = PTinStripQualifiers(pTinDst); 

	bool fWasHandled;
	TypeInfo * pTinReturn = PTinPromoteUntypedCommon(pTcwork, pSymtab, &fWasHandled, pStnodLit, pTinDst, errep);
	if (fWasHandled)
		return pTinReturn;

	TypeInfoLiteral * pTinlit = (TypeInfoLiteral *)pStnodLit->m_pTin;
	const LiteralType & litty = pTinlit->m_litty;
	switch (litty.m_litk)
	{
	case LITK_Enum:
		{
			auto pTinenum = PTinRtiCast<TypeInfoEnum *>(pTinlit->m_pTinSource);
			if (!MOE_FVERIFY(pTinenum, "bad enum literal"))
				return nullptr;

			if (pTinDst->m_tink == TINK_Enum)
			{
				return pSymtab->PTinqualWrap(pTinenum, FQUALK_Const);
			}

			if (MOE_FVERIFY(pTinenum->m_pTinLoose, "expected loose type"))
			{
				return pSymtab->PTinqualWrap(pTinenum->m_pTinLoose, FQUALK_Const);
			}
			return nullptr;
		}
	case LITK_Numeric:
		{
			// NOTE: We're casting the value to fit the type info here, not letting the value determine the type.

			auto pTinnDst = PTinRtiCast<TypeInfoNumeric *>(pTinDst);
			if (pTinnDst && pTinnDst->m_grfnum.FIsSet(FNUM_IsFloat))
			{
				// integer literals can be used to initialize floating point numbers
				return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrF32);
			}

			const STValue * pStval = PStnodRtiCast<STValue *>(pStnodLit);
			bool fDestIsSigned = pTinnDst && pTinnDst->m_grfnum.FIsSet(FNUM_IsSigned);
			bool fIsValNegative = pStval->m_stvalk == STVALK_SignedInt && pStval->m_nSigned < 0;

			if (fDestIsSigned == false && fIsValNegative == false)
			{
				s64 nUnsigned = NUnsignedLiteralCast(pTcwork, pStval);
				if (nUnsigned <= UCHAR_MAX)	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrU8);
				if (nUnsigned <= USHRT_MAX)	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrU16);
				if (nUnsigned <= UINT_MAX)	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrU32);
				return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrU64);
			}

			s64 nSigned = NSignedLiteralCast(pTcwork, pStval);
			if (fIsValNegative)
			{
				if (nSigned >= SCHAR_MIN)	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrS8);
				if (nSigned >= SHRT_MIN)	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrS16);
				if (nSigned >= INT_MIN)	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrS32);
				return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrS64);
			}

			// NOTE - if this value isn't explicitly negative, allow code to initialize it with 
			//  values large enough to cause it to be negative. ie. n:s32=0xFFFFFFFF;

			if (nSigned <= UCHAR_MAX)	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrS8);
			if (nSigned <= USHRT_MAX)	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrS16);
			if (nSigned <= UINT_MAX)	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrS32);
			return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrS64);
		}
	case LITK_Char:
		{
			const STValue * pStval = PStnodRtiCast<STValue *>(pStnodLit);
			bool fDestIsSigned = pTinDst->m_tink == TINK_Numeric && ((TypeInfoNumeric*)pTinDst)->m_grfnum.FIsSet(FNUM_IsSigned);
			if (fDestIsSigned)
			{
				s64 nSigned = NSignedLiteralCast(pTcwork, pStval);
				if ((nSigned <= SCHAR_MAX) & (nSigned > SCHAR_MIN))	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrS8);
				if ((nSigned <= SHRT_MAX) & (nSigned > SHRT_MIN))	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrS16);
				return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrS32);
			}

			s64 nUnsigned = NUnsignedLiteralCast(pTcwork, pStval);
			if (nUnsigned <= UCHAR_MAX)	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrU8);
			if (nUnsigned <= USHRT_MAX)	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrU16);
			return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrU32);
		}
	case LITK_String:
	{
		// right now string literals just promote to *u8, but will eventually promote to string
		auto pTinU8 = pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrU8);
		auto pTinqual = pSymtab->PTinqualEnsure(pTinU8, FQUALK_Const);
		return pSymtab->PTinptrAllocate(pTinqual);
	}
	case LITK_Bool:		return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrBool);
	case LITK_Null:		
		{
			if (pTinDst && (pTinDst->m_tink == TINK_Pointer || pTinDst->m_tink == TINK_Procedure))
				return pTinDst;
			if (errep == ERREP_ReportErrors)
			{
				EmitError(pTcwork, pStnodLit->m_lexsp, ERRID_CannotConvertToNull, 
					"Trying to initialize non pointer type with null value");
			}
		} break;
	case LITK_Nil: 
		MOE_ASSERT(false, "Cannot infer type for LITK_Nil");
	default:
		MOE_ASSERT(false, "Cannot infer type for unknown literal kind");
	}
	return nullptr;
}

struct TinSpecEntry // tag = tinse
{
	int				m_nState;
	STNode *		m_pStnod;
	SymbolTable *	m_pSymtab;
	TypeInfo *		m_pTin;			// should this be pushed into the stnod's m_pTin pointer?
};

void PushTinSpecStack(CDynAry<TinSpecEntry> * paryTinse, STNode * pStnod, SymbolTable * pSymtab)
{
	auto pTinse = paryTinse->AppendNew();
	pTinse->m_nState = 0;
	pTinse->m_pStnod = pStnod;
	pTinse->m_pSymtab = pSymtab;
	pTinse->m_pTin = nullptr;
}

void PopTinSpecStack(CDynAry<TinSpecEntry> * paryTinse, TypeInfo * pTin, TypeInfo ** ppTinRoot)
{
	paryTinse->PopLast();
	if (paryTinse->FIsEmpty())
	{
		*ppTinRoot = pTin;
	}
	else
	{
		paryTinse->PLast()->m_pTin = pTin;
	}
}

// --- Notes on const and literals in Moe ---

// Const is used to prevent modifying read-only data, it exists solely for guarding literal data
//   We cannot reserve it only for data that won't be modified elsewhere or we're unable to write a
//   routine that can take read-only data that is either const or non-const. It IS legal to cast non-const
//   data to const.
//
//   Const is transitive - any data pointed to by a const pointer is also const. Assignment operations remove the 
//   top layer of const as that is being copied by value. (ie. it's legal to copy a const int to a regular int, or 
//   set a non-const reference to const ints because the pointer is copied by value (not the elements pointed to))

// Literal data is const. TypeInfoLiteral should behave as if there is an implicit TypeInfoQualifier(FQUALK_Const)
//   above pTinlit->m_pTinSource. If it is the RHS of an assignment the child type needs to be wrapped in a const qualifier.


TypeInfo * PTinQualifyAfterAssignment(TypeInfo * pTin, SymbolTable * pSymtab, TypeInfo * pTinDst)
{
	bool fIsConst = false;
	TypeInfo * pTinUnqual = nullptr;
	if (pTin->m_tink == TINK_Qualifier)
	{
		auto pTinqual = (TypeInfoQualifier *)pTin;
		fIsConst = pTinqual->m_grfqualk.FIsSet(FQUALK_Const);
		if (!fIsConst)
		{
			// just strip inArg

			return pTinqual->m_pTin;
		}

		pTinUnqual = pTinqual->m_pTin;
	}
	else if(pTin->m_tink == TINK_Literal)
	{
		auto pTinlit = (TypeInfoLiteral *)pTin;
		pTinUnqual = pTinlit->m_pTinSource;
		fIsConst = true;
	}

	if (!fIsConst)
		return pTin;

	// strip off the top level const, but make sure there's a const qualifier one level below

	switch (pTinUnqual->m_tink)
	{
	case TINK_Pointer:
		{
			auto pTinptr = (TypeInfoPointer *)pTinUnqual;
			TypeInfoQualifier * pTinqualChild = (TypeInfoQualifier*)pTinptr->m_pTin;
			if (pTinqualChild->m_tink != TINK_Qualifier || pTinqualChild->m_grftin.FIsSet(FTIN_IsUnique))
			{
				pTinqualChild = pSymtab->PTinqualEnsure(pTinptr->m_pTin, FQUALK_Const);
			}

			pTinqualChild->m_grfqualk.AddFlags(FQUALK_Const);
			if (pTinptr->m_grftin.FIsSet(FTIN_IsUnique))
			{
				pTinptr = pSymtab->PTinptrAllocate(pTinqualChild);
				pTinUnqual = pTinptr;
			}
			else
			{
				pTinptr->m_pTin = pTinqualChild;
			}
		}break;
	case TINK_Array:
		{
			auto pTinary = (TypeInfoArray *)pTinUnqual;
			auto pTinaryDst = PTinRtiCast<TypeInfoArray *>(PTinStripQualifiers(pTinDst));
			if (pTinaryDst && pTinaryDst->m_aryk == ARYK_Fixed)
			{
				// fixed arrays copy members by value, treat this like a member rvalue assignment
				auto pTinElement = PTinQualifyAfterAssignment(pTinary->m_pTin, pSymtab, pTinaryDst->m_pTin);
				if (FTypesAreSame(pTinElement, pTinary->m_pTin))
					return pTinary;

				pTinary = pSymtab->PTinaryCopyWithNewElementType(pTinary, pTinElement);
				return pTinary;
			}

			TypeInfoQualifier * pTinqualChild = (TypeInfoQualifier*)pTinary->m_pTin;
			if (pTinqualChild->m_tink != TINK_Qualifier || pTinqualChild->m_grftin.FIsSet(FTIN_IsUnique))
			{
				pTinqualChild = pSymtab->PTinqualEnsure(pTinary->m_pTin, FQUALK_Const);
			}

			pTinqualChild->m_grfqualk.AddFlags(FQUALK_Const);
			if (pTinary->m_grftin.FIsSet(FTIN_IsUnique))
			{

				pTinary = pSymtab->PTinaryCopy(pTinary);
				pTinUnqual = pTinary;
			}

			pTinary->m_pTin = pTinqualChild;
		}break;
	default: break;
	}

	return pTinUnqual;
}

TypeInfo * PTinAfterRValueAssignment(TypeCheckWorkspace * pTcwork, const LexSpan & lexsp, TypeInfo * pTin, SymbolTable * pSymtab, TypeInfo * pTinDst)
{
	if (!pTin)
		return nullptr;

	if (pTin->m_tink == TINK_Flag)
	{
		return pSymtab->PTinBuiltin(BuiltIn::g_istrBool);
	}
	if (pTin->m_tink == TINK_Procedure)
	{
		auto pTinproc = (TypeInfoProcedure *)pTin;
		if (pTinproc->FHasGenericArgs())	
		{
			EmitError(pTcwork->m_pErrman, lexsp, ERRID_NoGenericRValue,
				"cannot make a reference to a generic procedure definition '%s'", pTinproc->m_istrName.m_pChz);
		}
	}

	return PTinQualifyAfterAssignment(pTin, pSymtab, pTinDst);
}

static bool FCanImplicitCast(TypeInfo * pTinSrc, TypeInfo * pTinDst)
{
	if (!pTinSrc)
		return false;	 // NOTE: this can happen after an error has occurred, don't assert - just return.

	MOE_ASSERT(pTinSrc->m_tink != TINK_Literal, "literals should be promoted before calling FCanImplicitCast()");

	if (pTinDst->m_tink == TINK_Qualifier)
	{
		TypeInfo * pTinSrcAdj = (pTinSrc->m_tink == TINK_Qualifier) ? ((TypeInfoQualifier *)pTinSrc)->m_pTin : pTinSrc;
		auto pTinqualDst = (TypeInfoQualifier *)pTinDst;

		return FCanImplicitCast(pTinSrcAdj, pTinqualDst->m_pTin);
	}

	if (pTinSrc->m_tink == pTinDst->m_tink)
	{
		// Note - can't just compare pointers directly as tins are not unique. (but they should be)
		switch (pTinSrc->m_tink)
		{
		case TINK_Numeric:
			{
				auto pTinnSrc = (TypeInfoNumeric *)pTinSrc;
				auto pTinnDst = (TypeInfoNumeric *)pTinDst;

				bool fIsSrcFloat = pTinnSrc->m_grfnum.FIsSet(FNUM_IsFloat);
				bool fIsDstFloat = pTinnDst->m_grfnum.FIsSet(FNUM_IsFloat);
				if (fIsSrcFloat != fIsDstFloat)
					return false;

				if (fIsSrcFloat)
				{
					return pTinnDst->m_cBit >= pTinnSrc->m_cBit;
				}

				bool fIsSrcSigned = pTinnSrc->m_grfnum.FIsSet(FNUM_IsSigned);
				bool fIsDstSigned = pTinnDst->m_grfnum.FIsSet(FNUM_IsSigned);
				if ((pTinnDst->m_cBit >= pTinnSrc->m_cBit) & (fIsSrcSigned == fIsDstSigned))
					return true;

				// Allow unsigned->signed conversions if a higher cBit
				return ((fIsDstSigned == true) & (pTinnDst->m_cBit > pTinnSrc->m_cBit));
			}
		case TINK_Bool: return true;
		case TINK_Pointer:
			{
				auto pTinptrSrc = (TypeInfoPointer *)pTinSrc;
				auto pTinptrDst = (TypeInfoPointer *)pTinDst;
				if (pTinptrDst->m_pTin->m_tink == TINK_Void)
					return true;

				TypeInfo * pTinChildSrc = pTinptrSrc->m_pTin;
				TypeInfo * pTinChildDst = pTinptrDst->m_pTin;
				GRFQUALK grfqualkSrc;
				GRFQUALK grfqualkDst;
				if (pTinChildSrc->m_tink == TINK_Qualifier)
				{
					auto pTinqualSrc = (TypeInfoQualifier *)pTinChildSrc;
					grfqualkSrc = pTinqualSrc->m_grfqualk;
					pTinChildSrc = pTinqualSrc->m_pTin;
				}
				if (pTinChildDst->m_tink == TINK_Qualifier)
				{
					auto pTinqualDst = (TypeInfoQualifier *)pTinChildDst;
					grfqualkDst = pTinqualDst->m_grfqualk;
					pTinChildDst = pTinqualDst->m_pTin;
				}

				// can upcast to const/inarg, but not downcast
				grfqualkSrc.AddFlags(grfqualkDst);
				if (grfqualkDst != grfqualkSrc)
					return false;

				return FTypesAreSame(pTinChildSrc, pTinChildDst);	
			}
		case TINK_Array:
			{
				auto pTinaryDst = (TypeInfoArray *)pTinDst;
				auto pTinarySrc = (TypeInfoArray *)pTinSrc;

				TypeInfo * pTinChildSrc = pTinarySrc->m_pTin;
				TypeInfo * pTinChildDst = pTinaryDst->m_pTin;
				GRFQUALK grfqualkSrc;
				GRFQUALK grfqualkDst;
				if (pTinChildSrc->m_tink == TINK_Qualifier)
				{
					auto pTinqualSrc = (TypeInfoQualifier *)pTinChildSrc;
					grfqualkSrc = pTinqualSrc->m_grfqualk;
					pTinChildSrc = pTinqualSrc->m_pTin;
				}
				if (pTinChildDst->m_tink == TINK_Qualifier)
				{
					auto pTinqualDst = (TypeInfoQualifier *)pTinChildDst;
					grfqualkDst = pTinqualDst->m_grfqualk;
					pTinChildDst = pTinqualDst->m_pTin;
				}

				// can upcast to const/inarg, but not downcast
				grfqualkSrc.AddFlags(grfqualkDst);
				if (grfqualkDst != grfqualkSrc)
					return false;

				if (pTinaryDst->m_aryk == ARYK_Reference)
				{
					return FTypesAreSame(pTinChildSrc, pTinChildDst);
				}

				return FTypesAreSame(pTinSrc, pTinDst);
			} 
		case TINK_Enum: 
		case TINK_Struct:
		case TINK_Procedure:
			{
				return FTypesAreSame(pTinSrc, pTinDst);
			}
		default: return false;
		}
	}

	if ((pTinSrc->m_tink == TINK_Array) & (pTinDst->m_tink == TINK_Pointer))
	{
		auto pTinarySrc = (TypeInfoArray *)pTinSrc;
		auto pTinptrDst = (TypeInfoPointer *)pTinDst;
		auto pTinPointedTo = pTinptrDst->m_pTin;
		if (pTinPointedTo->m_tink == TINK_Void)
			return true;
		return FCanImplicitCast(pTinarySrc->m_pTin, pTinPointedTo);	
	}

	if (pTinSrc->m_tink == TINK_Enum && pTinDst->m_tink == TINK_Numeric)
	{
		auto pTinenum = (TypeInfoEnum *)pTinSrc;
		return FCanImplicitCast(pTinenum->m_pTinLoose, pTinDst);
	}

	if (pTinSrc->m_tink == TINK_Bool && pTinDst->m_tink == TINK_Numeric)
	{
		return ((TypeInfoNumeric*)pTinDst)->m_grfnum.FIsSet(FNUM_IsFloat) == false;
	}

	if (pTinDst->m_tink == TINK_Bool || pTinDst->m_tink == TINK_Flag)
	{
		switch (pTinSrc->m_tink)
		{
		case TINK_Numeric:	return true;
		case TINK_Pointer:	return true;
		case TINK_Bool:	return true;
		case TINK_Flag: return true;
		default : return false;
		}
	}
	return false;
}

inline bool FCanCastForInit(TypeCheckWorkspace * pTcwork, const LexSpan & lexsp, SymbolTable * pSymtab, TypeInfo * pTinSrc, TypeInfo * pTinDst)
{
	// Only require a const destination if the source is const, otherwise we'll strip it because it's ok to
	//  initialize a const value (just not to assign to it)

	auto pTinSrcAdj = PTinAfterRValueAssignment(pTcwork, lexsp, pTinSrc, pSymtab, pTinDst);
	auto pTinDstAdj = PTinAfterRValueAssignment(pTcwork, lexsp, pTinDst, pSymtab, pTinDst);

	return FCanImplicitCast(pTinSrcAdj, pTinDstAdj);
}

inline bool FIsNumericTink(TINK tink)
{
	switch (tink)
	{
	case TINK_Numeric:	return true;
	case TINK_Bool:		return true;
	case TINK_Enum:		return true;
	default: return false;
	}
}

inline bool FCanExplicitCast(TypeInfo * pTinSrc, TypeInfo * pTinDst, SymbolTable * pSymtab)
{
	if (pTinSrc->m_tink == TINK_Pointer && pTinDst->m_tink == TINK_Pointer)
	{
		auto pTinptrSrc = (TypeInfoPointer *)pTinSrc;
		auto pTinptrDst = (TypeInfoPointer *)pTinDst;

		return FIsMutableType(pTinptrSrc->m_pTin) == true || FIsMutableType(pTinptrDst->m_pTin) == false;
	}
	if (pTinSrc->m_tink == TINK_Procedure && pTinDst->m_tink == TINK_Procedure)
		return true;

#if 0 // not supported yet - need to write LLVM codegen
	// allow for ptr->int and int->ptr casts 
	if (pTinSrc->m_tink == TINK_Pointer || pTinDst->m_tink == TINK_Pointer)
	{
		auto pTinOther = pTinDst;
		if (pTinOther->m_tink == TINK_Pointer)
		{
			pTinOther = pTinSrc;
		}

		// BB - We don't have a formal way to get this during typecheck
		size_t cBPointer = sizeof(void*);
		TypeInfoInteger * pTinint = PTinRtiCast<TypeInfoInteger *>(pTinOther);
		if (pTinint && pTinint->m_cBit == cBPointer * 8 && pTinint->m_fIsSigned == false)
		{
			return true;
		}
	}
#endif

	// Result of this cast is an RValue, we can step the const down a level
	pTinSrc = PTinQualifyAfterAssignment(pTinSrc, pSymtab, pTinDst);

	if (FIsNumericTink(pTinSrc->m_tink))
	{
		return FIsNumericTink(pTinDst->m_tink);
	}

	return FCanImplicitCast(pTinSrc, pTinDst);
}

bool FIsValidLhs(const STNode * pStnod)
{
	// BB - this is just returning the easy failures... needs a more thorough check.
	TypeInfo * pTin = pStnod->m_pTin;
	if (!pTin)
		return false;

	TINK tink = pTin->m_tink;
	if (tink == TINK_Array)
	{
		auto pTinary = (TypeInfoArray *)pTin;
		return pTinary->m_aryk == ARYK_Reference;
	}

	return (tink != TINK_Null) & (tink != TINK_Void) & (tink != TINK_Literal);
}

void FinalizeCompoundLiteralType(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtab,
	TypeInfoLiteral * pTinlit,
	TypeInfo * pTinDst,
	STNode * pStnodLit)
{
	MOE_ASSERT(pStnodLit->m_pTin == pTinlit, "expected literal to be set");
	if (!MOE_FVERIFY(pTinlit->m_litty.m_litk == LITK_Compound, "finalizing array with non-array literal") ||
		!MOE_FVERIFY(pTinlit->m_fIsFinalized == false, "expected non-finalized literal"))
		return;

	STNode * pStnodDef = pStnodLit;
	if (MOE_FVERIFY(pTinlit->m_pStnodDefinition, "bad literal definition"))
	{
		pStnodDef = pTinlit->m_pStnodDefinition;
		MOE_ASSERT(pStnodDef->m_strees == STREES_TypeChecked, "expected type checked by now");
	}

	if (pStnodDef != pStnodLit)
	{
		// if this literal is being referred to via an immutable definition we should
		//  make a copy before we finalize it so it won't affect other references 

		// BB - We should be checking to see if this pTinLit has been finalized to this pTin elsewhere
		//  to avoid a bunch of duplicate array copies (could check the stnods tacked onto the origional pStnodDef?)
		auto pTinlitNew = pSymtab->PTinlitCopy(pTinlit);

		pStnodLit->m_pTin = pTinlitNew;
		pTinlit = pTinlitNew;

		// setup pTinlit->pStnodSource
		auto pStnodDefCopy = PStnodCopy(pTcwork->m_pAlloc, pStnodDef);
		pTinlitNew->m_pStnodDefinition = pStnodDefCopy;

		// just tack the new value copy on the end of the literal so it gets cleaned up (yuck)
		pStnodDef->AppendChildToArray(pTcwork->m_pAlloc, pStnodDefCopy);
		pStnodDef = pStnodDefCopy;
	}

	auto pStdecl = PStnodRtiCast<STDecl *>(pStnodDef);
	if (!pStdecl)
		return;

	auto pStnodList = pStdecl->m_pStnodInit;
	if (!pStnodList)
		return;

	MOE_ASSERT(pTinDst->m_tink != TINK_Qualifier, "literal const qualifier should be implicit");
	switch (pTinDst->m_tink)
	{
	case TINK_Array:
		{
			auto pTinary = (TypeInfoArray *)pTinDst;
			pTinlit->m_pTinSource = pTinary;

			pTinlit->m_c = pStnodList->CPStnodChild();
			pTinlit->m_fIsFinalized = true;

			if (pTinlit->m_c < pTinary->m_c)
			{
				EmitError(pTcwork, pStnodDef->m_lexsp, ERRID_InitTypeMismatch,
					"too few elements in array literal definition '%s'",
					IstrFromTypeInfo(pTinlit).m_pChz);
				break;
			}

			for (int iStnod = 0; iStnod < pStnodList->CPStnodChild(); ++iStnod)
			{
				auto pStnodIt = pStnodList->PStnodChild(iStnod);
				if (pStnodIt && pStnodIt->m_park == PARK_ArgumentLabel)
				{
					EmitError(pTcwork, pStnodLit->m_lexsp, ERRID_InvalidLabel,
						"labeled member not allowed in array literal"); 
					pStnodIt = pStnodIt->PStnodChildSafe(1);
				}

				TypeInfo * pTinInit = pStnodIt->m_pTin;
				if (!pTinInit || pTinInit->m_tink != TINK_Literal)
				{
					EmitError(pTcwork, pStnodIt->m_lexsp, ERRID_NonConstantInLiteral,
						"array element %d cannot be initialized with a non-literal type '%s'",
						iStnod,
						IstrFromTypeInfo(pTinInit).m_pChz);
					continue;
				}

				auto pTinElement = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodIt, pTinary->m_pTin);

				if (FCanCastForInit(pTcwork, pStnodIt->m_lexsp, pSymtab, pTinElement, pTinary->m_pTin))
				{
					FinalizeLiteralType(pTcwork, pSymtab, pTinary->m_pTin, pStnodIt);
				}
				else
				{
					EmitError(pTcwork, pStnodIt->m_lexsp, ERRID_InitTypeMismatch,
						"array element %d is type '%s', cannot initialize with type '%s'",
						iStnod,
						IstrFromTypeInfo(pTinary->m_pTin).m_pChz,
						IstrFromTypeInfo(pStnodIt->m_pTin).m_pChz);
				}
			}
		} break;
	case TINK_Struct:
		{
			auto pTinstruct = (TypeInfoStruct *)pTinDst;
			pTinlit->m_pTinSource = pTinstruct;

			int cTypememb = int(pTinstruct->m_aryTypemembField.C());
	        CDynAry<STNode *> arypStnodInit(pTcwork->m_pAlloc, BK_CodeGen, cTypememb);
	        arypStnodInit.AppendFill(cTypememb, nullptr);

			int iStnodNamed = -1;
			for (int iStnod = 0; iStnod < pStnodList->CPStnodChild(); ++iStnod)
			{
				auto pStnodIt = pStnodList->PStnodChild(iStnod);
				if (pStnodIt->m_park == PARK_ArgumentLabel)
				{
					if (iStnodNamed < 0)
					{
						iStnodNamed = iStnod;
					}

					STNode * pStnodIdentifier = pStnodIt->PStnodChild(0);
					InString istrIdentifier(IstrFromIdentifier(pStnodIdentifier));
					int iTypememb = ITypemembLookup(pTinstruct, istrIdentifier);
					if (iTypememb < 0)
					{
						EmitError(pTcwork, pStnodLit->m_lexsp, ERRID_LiteralMemberNotFound,
							"struct '%s' has no member named '%s'", 
							pTinstruct->m_istrName.m_pChz, 
							istrIdentifier.m_pChz);
						return;
					}
					else
					{
						MOE_ASSERT(pStnodIt->CPStnodChild() == 2, "missing label value");
						arypStnodInit[iTypememb] = pStnodIt->PStnodChildSafe(1);
					}
				}
				else
				{
					if (iStnodNamed >= 0)
					{
						EmitError(pTcwork, pStnodLit->m_lexsp, ERRID_LiteralUnnamedMember, 
							"Unnamed expression encountered after named expression %d", iStnodNamed + 1);
						return;
					}

					if (iStnod > pTinstruct->m_aryTypemembField.C())
					{
						EmitError(pTcwork, pStnodLit->m_lexsp, ERRID_TooManyInitializers,
							"too many initializers for struct '%s'", pTinstruct->m_istrName.m_pChz);
						return;
					}
					else
					{
						arypStnodInit[iStnod] = pStnodIt;
					}
				}
			}

			for (int iTypememb = 0; iTypememb < cTypememb; ++iTypememb)
			{
				auto pTypememb = &pTinstruct->m_aryTypemembField[iTypememb];
				auto pStnodIt = arypStnodInit[iTypememb];
				if (pStnodIt)
				{
					TypeInfo * pTinInit = pStnodIt->m_pTin;
					if (!pTinInit || pTinInit->m_tink != TINK_Literal)
					{
						EmitError(pTcwork, pStnodIt->m_lexsp, ERRID_NonConstantInLiteral,
							"struct member '%s' cannot be initialize with a non-literal type '%s'",
							pTypememb->m_istrName.m_pChz,
							IstrFromTypeInfo(pTinInit).m_pChz);
						continue;
					}

					pTinInit = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodIt, pTypememb->m_pTin);

					// the top level literal is implicitly const so it's members are too

					auto pTinqualMember = pSymtab->PTinqualWrap(pTypememb->m_pTin, FQUALK_Const);
					if (FCanCastForInit(pTcwork, pStnodIt->m_lexsp, pSymtab, pTinInit, pTinqualMember))
					{
						FinalizeLiteralType(pTcwork, pSymtab, pTypememb->m_pTin, pStnodIt);
					}
					else
					{
						EmitError(pTcwork, pStnodIt->m_lexsp, ERRID_InitTypeMismatch,
							"struct member '%s' is type '%s', cannot initialize with type '%s'",
							pTypememb->m_istrName.m_pChz,
							IstrFromTypeInfo(pTinqualMember).m_pChz,
							IstrFromTypeInfo(pStnodIt->m_pTin).m_pChz);
					}
				}
			}
		} break;
	default:
		MOE_ASSERT(false, "compound literal with unexpected type '%s'", PChzFromTink(pTinlit->m_pTinSource->m_tink));
		return;
	}
}

void FinalizeLiteralType(TypeCheckWorkspace * pTcwork, SymbolTable * pSymtab, TypeInfo * pTinDst, STNode * pStnodLit)
{
	if (!pStnodLit->m_pTin || pStnodLit->m_pTin->m_tink != TINK_Literal)
		return;

	if (pTinDst->m_tink == TINK_Qualifier)
	{
		auto pTinqual = (TypeInfoQualifier *)pTinDst;
		pTinDst = pTinqual->m_pTin;
	}

	// we've found the place the literal will become 'typed' - flush that type back down into the literal
	// Note: we may re-finalize finalized literals here when using a typed constant (ie "SomeConst immutable : s8 = 2;" )

	MOE_ASSERT(pTinDst->m_tink != TINK_Literal, "cannot finalize literal with literal");
	switch (pTinDst->m_tink)
	{
	case TINK_Numeric:
		{
			auto pTinint = (TypeInfoNumeric *)pTinDst;
			pStnodLit->m_pTin = pSymtab->PTinlitFromLitk(LITK_Numeric, pTinint->m_cBit, pTinint->m_grfnum);
		}break;
	case TINK_Flag:		// fallthrough
	case TINK_Bool:		pStnodLit->m_pTin = pSymtab->PTinlitFromLitk(LITK_Bool);	break;
	case TINK_Procedure:
		{
			TypeInfoLiteral * pTinlitPrev = (TypeInfoLiteral *)pStnodLit->m_pTin;
			TypeInfoLiteral * pTinlit = nullptr;
			
			switch (pTinlitPrev->m_litty.m_litk)
			{
			case LITK_Null:		
				{
					pTinlit = MOE_NEW(pSymtab->m_pAlloc, TypeInfoLiteral) TypeInfoLiteral();
					pTinlit->m_litty.m_litk = LITK_Null;
					pTinlit->m_litty.m_cBit = -1;
					pTinlit->m_litty.m_grfnum = FNUM_None;
					pTinlit->m_fIsFinalized = true;
					pTinlit->m_pTinSource = (TypeInfoPointer*)pTinDst;
					pSymtab->AddManagedTin(pTinlit);

				} break;
			default: MOE_ASSERT(false, "unexpected literal type");
			}

			if (pTinlit)
			{
				pStnodLit->m_pTin = pTinlit;
			}
		} break;
    case TINK_Pointer:
		{
			TypeInfoLiteral * pTinlitPrev = PTinDerivedCast<TypeInfoLiteral *>(pStnodLit->m_pTin);
			TypeInfoLiteral * pTinlit = nullptr;
			
			switch (pTinlitPrev->m_litty.m_litk)
			{
			case LITK_String:	pTinlit = pSymtab->PTinlitFromLitk(LITK_String);	break;
			case LITK_Null:		
				{
					pTinlit = MOE_NEW(pSymtab->m_pAlloc, TypeInfoLiteral) TypeInfoLiteral();
					pSymtab->AddManagedTin(pTinlit);
					pTinlit->m_litty.m_litk = LITK_Null;
					pTinlit->m_litty.m_cBit = -1;
					pTinlit->m_litty.m_grfnum = FNUM_None;
					pTinlit->m_fIsFinalized = true;
					pTinlit->m_pTinSource = (TypeInfoPointer*)pTinDst;
				} break;
			case LITK_Numeric:	
				{
					const STValue * pStval = PStnodRtiCast<STValue *>(pStnodLit);
					GRFNUM grfnum = (pStval->m_stvalk == STVALK_SignedInt) ? FNUM_IsSigned : FNUM_None;
					pTinlit = pSymtab->PTinlitFromLitk(LITK_Numeric, 64, grfnum);
				} break;
			case LITK_Compound:
				{
					auto pTinPromoted = PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodLit, pTinDst);

					// strip const here, as it is under a literal, so it's implicitly const

					auto pTinDst = PTinStripQualifiers(pTinPromoted);
					FinalizeCompoundLiteralType(pTcwork, pSymtab, pTinlitPrev, pTinDst, pStnodLit);
				} break;
			default: MOE_ASSERT(false, "unexpected literal type");
			}

			if (pTinlit)
			{
				pStnodLit->m_pTin = pTinlit;
			}
		} break;
	case TINK_Enum:
		{
			auto pTinenum = (TypeInfoEnum *)pTinDst;

			auto pTinn = PTinRtiCast<TypeInfoNumeric *>(pTinenum->m_pTinLoose);
			if (MOE_FVERIFY(pTinn, "Expected integer 'loose' type for enum"))
			{
				pStnodLit->m_pTin = pSymtab->PTinlitFromLitk(LITK_Numeric, pTinn->m_cBit, pTinn->m_grfnum);
			}
		} break;
	case TINK_Array:
	case TINK_Struct:
		{
			TypeInfoLiteral * pTinlitPrev = PTinDerivedCast<TypeInfoLiteral *>(pStnodLit->m_pTin);
			if (pTinlitPrev && pTinlitPrev->m_litty.m_litk == LITK_Compound)
			{
				pTinlitPrev->m_pTinSource = pTinDst;
				FinalizeCompoundLiteralType(pTcwork, pSymtab, pTinlitPrev, pTinDst, pStnodLit);
			}
			else
			{
				const char * pChzExpect = (pTinDst->m_tink == TINK_Array) ? "array literal" : "struct literal";

				EmitError(pTcwork, pStnodLit->m_lexsp, ERRID_NonConstantInLiteral, 
					"Expected %s in literal but encountered %s",
					pChzExpect,
					IstrFromTypeInfo(pStnodLit->m_pTin).m_pChz);
			}
		} break;
	case TINK_Anchor:
		{
			(void) PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodLit);
		}break;
	case TINK_Null: // fall through
	case TINK_Void: // fall through
	default:
		MOE_ASSERT(false, "unexpected type");
	}
}

TypeInfo * PTinPromoteVarArg(TypeCheckWorkspace * pTcwork, SymbolTable * pSymtab, TypeInfo * pTinIn)
{
	// C99 requires that all floats are promoted to double and all integers < 32 bit are promoted to 32 bit.

	switch (pTinIn->m_tink)
	{
	case TINK_Numeric:
		{
			TypeInfoNumeric * pTinn = (TypeInfoNumeric*)pTinIn;
			if (pTinn->m_grfnum.FIsSet(FNUM_IsFloat))
			{
				if (pTinn->m_cBit < 64)
				{
					return pSymtab->PTinBuiltin(BuiltIn::g_istrF64);
				}
			}
			else if (pTinn->m_cBit < 32)
			{
				return pSymtab->PTinBuiltin((pTinn->m_grfnum.FIsSet(FNUM_IsSigned)) ? BuiltIn::g_istrS32 : BuiltIn::g_istrU32);
			}
			return pTinIn;
		}
	case TINK_Enum:
		{
			auto pTinenum = (TypeInfoEnum *)pTinIn;
			return PTinPromoteVarArg(pTcwork, pSymtab, pTinenum->m_pTinLoose);
		}
	case TINK_Array:
		{
			auto pTinary = (TypeInfoArray *)pTinIn;
			return pSymtab->PTinptrAllocate(pTinary->m_pTin);
		}
	default: return pTinIn;
	}
}

QUALK QualkFromRword(InString istr)
{
	if (istr == RWord::g_istrConst)
		return QUALK_Const;
	if (istr == RWord::g_istrInArg)
		return QUALK_InArg;

	MOE_ASSERT(false, "unexpected RWORD '%s' for qualk", istr.m_pChz);
	return QUALK_Nil;
}

TypeInfo * PTinFromTypeSpecification(
	TypeCheckWorkspace * pTcwork,
	SymbolTable * pSymtabRoot,
	STNode * pStnod,
	GRFSYMLOOK grfsymlook,
	Symbol **ppSymType,
	bool * pFIsValidTypeSpec)
{
	CDynAry<TinSpecEntry> aryTinse(pTcwork->m_pAlloc, BK_TypeCheck);

	// Returns null if this is an instance of an unknown type, if this is a reference to an unknown type we will return
	//  tinptr->tin(TINK_Unknown) because we need this to handle a struct with a pointer to an instance of itself.

	// Essentially, any non-null returned from this should be enough to determine the size of this type spec and 
	//  determine target type equivalence.

	bool fAllowForwardDecl = false;
	PushTinSpecStack(&aryTinse, pStnod, pSymtabRoot);
	TypeInfo * pTinReturn = nullptr;
	*pFIsValidTypeSpec = true;

	while (!aryTinse.FIsEmpty())
	{
		auto pTinse = aryTinse.PLast();
		auto pStnod = pTinse->m_pStnod;
		switch (pStnod->m_park)
		{
		case PARK_Identifier:
			{
				auto istrIdent = IstrFromIdentifier(pStnod);
				auto pSymbase = PSymbaseLookup(pTinse->m_pSymtab, istrIdent, pStnod->m_lexsp, grfsymlook);
				Symbol * pSym = nullptr;
				if (!pSymbase)
				{
					EmitError(pTcwork, pStnod->m_lexsp, ERRID_UnknownIdentifier, 
						"'%s' unknown identifier detected in type specificattion", istrIdent.m_pChz);
					*pFIsValidTypeSpec = false;
				}
				else
				{
					pSym = PSymLast(pSymbase);
				}

				if (!pSym || !pSym->m_grfsym.FIsSet(FSYM_IsType))
				{
					EmitError(pTcwork, pStnod->m_lexsp, ERRID_TypeSpecifierExpected,
						"Expected type specification but encountered '%s'", istrIdent.m_pChz);
					return nullptr;
				}

				auto pTinstruct = PTinRtiCast<TypeInfoStruct *>(PTinFromSymbol(pSym));
				if (pTinstruct && pTinstruct->FHasGenericParams())
				{
					EmitError(pTcwork, pStnod->m_lexsp, ERRID_GenericArgsExpected,
						"Generic struct '%s' needs argument list for instantiation", istrIdent.m_pChz);
					*pFIsValidTypeSpec = false;
				}

				if (ppSymType)
				{
					*ppSymType = pSym;
				}
	
				pStnod->m_pTin = PTinFromSymbol(pSym);
				PopTinSpecStack(&aryTinse, pStnod->m_pTin, &pTinReturn);
			} break;
		case PARK_GenericStructSpec:
			{
				MOE_ASSERT(pStnod->m_strees == STREES_TypeChecked, "generic inst needs to be type checked first");

				auto pSym = pStnod->PSym();
				if (TypeInfo * pTinSym = PTinFromSymbol(pSym))
				{
					pStnod->m_pTin = pTinSym;
				}
				else
				{
					*pFIsValidTypeSpec = false;
				}
				
				PopTinSpecStack(&aryTinse, pStnod->m_pTin, &pTinReturn);
			} break;
		case PARK_MemberLookup:
			{
				MOE_ASSERT(pStnod->CPStnodChild() == 2, "Expected Lhs.Rhs in PARK_MemberLookup");
				auto nState = pTinse->m_nState++;
				if (nState == 0)
				{
					// walk down the right side of the tree to find the type/symtab to search

					PushTinSpecStack(&aryTinse, pStnod->PStnodChild(0), pTinse->m_pSymtab);
				}
				else if (nState == 1)
				{
					auto pSymtabTin = PSymtabFromType(pTcwork, pTinse->m_pTin, pStnod->m_lexsp);

					STNode * pStnodIdent = pStnod->PStnodChild(1);
					auto strIdent = IstrFromIdentifier(pStnodIdent);
					auto pSym = pSymtabTin->PSymLookup(strIdent, pStnod->m_lexsp, grfsymlook);

					PushTinSpecStack(&aryTinse, pStnod->PStnodChild(1), pSymtabTin);
				}
				else
				{
					pStnod->m_pTin = pTinse->m_pTin;
					PopTinSpecStack(&aryTinse, pStnod->m_pTin, &pTinReturn);
				}
			} break;
		case PARK_ArrayDecl:
			{
				if (pTinse->m_nState++ == 0)
				{
					// array decl's children are [type] or [m_c, type]
					PushTinSpecStack(&aryTinse, pStnod->PStnodChild(pStnod->CPStnodChild() - 1), pTinse->m_pSymtab);
				}
				else
				{
					TypeInfoArray * pTinary = MOE_NEW(pSymtabRoot->m_pAlloc, TypeInfoArray) TypeInfoArray();
					pTinary->m_pTin = pTinse->m_pTin;

					if (pStnod->CPStnodChild() == 2)
					{
						STNode * pStnodDim = pStnod->PStnodChild(0);
						STValue * pStvalDim = nullptr;

						s64 cTinary = 0;
						if (!FIsCompileTimeConstant(pStnodDim))
						{
							EmitError(pTcwork, pStnod->m_lexsp, ERRID_FeatureNotImplemented,
								"Only static sized arrays are currently supported");
							*pFIsValidTypeSpec = false;
						}
						else
						{
							auto pTinnBaked = PTinRtiCast<TypeInfoNumeric*>(PTinBakedConstantType(pStnodDim));
							if (pTinnBaked && !pTinnBaked->m_grfnum.FIsSet(FNUM_IsFloat))
							{
								pTinary->m_pStnodBakedDim = pStnodDim;
							}
							else
							{
								Symbol * pSymDim = pStnodDim->PSym();
								TypeInfo * pTinCount = pTinse->m_pSymtab->PTinBuiltin(BuiltIn::g_istrInt);
								TypeInfo * pTinPromoted = PTinPromoteUntypedTightest(pTcwork, pTinse->m_pSymtab, pStnodDim, pTinCount);
								pTinPromoted = PTinAfterRValueAssignment(pTcwork, pStnodDim->m_lexsp, pTinPromoted, pTinse->m_pSymtab, pTinCount);

								if (!FCanImplicitCast(pTinPromoted, pTinCount))
								{

									EmitError(pTcwork, pStnod->m_lexsp, ERRID_BadArrayIndex, "static integer array size expected");
									*pFIsValidTypeSpec = false;
								}
								else
								{
									FinalizeLiteralType(pTcwork, pTinse->m_pSymtab, pTinCount, pStnodDim);
									pStvalDim = PStnodRtiCast<STValue *>(pStnodDim);
								}

								if (pStvalDim)
								{
									cTinary = NUnsignedLiteralCast(pTcwork, pStvalDim);
								}
							}
						}

						pTinary->m_c = cTinary;
						pTinary->m_aryk = ARYK_Fixed;
					}
					else
					{
						pTinary->m_aryk = (pStnod->m_tok == TOK_PeriodPeriod) ? ARYK_Dynamic : ARYK_Reference;
					}

					if (pTinary->m_aryk == ARYK_Dynamic)
					{
						EmitError(pTcwork, pStnod->m_lexsp, ERRID_NotYetSupported, "Dynamic arrays are not yet supported");
					}

					pStnod->m_pTin = pTinary;
					pTinse->m_pSymtab->AddManagedTin(pTinary);
					PopTinSpecStack(&aryTinse, pStnod->m_pTin, &pTinReturn);
				}
			} break;
		case PARK_QualifierDecl:
			{
				if (pTinse->m_nState++ == 0)
				{
					MOE_ASSERT(pStnod->CPStnodChild() == 1, "expected one child");
					PushTinSpecStack(&aryTinse, pStnod->PStnodChild(0), pTinse->m_pSymtab);
				}
				else
				{
					QUALK qualk = QUALK_Nil;

					if (auto pStval = PStnodDerivedCast<STValue *>(pStnod))
					{
						QualkFromRword(pStval->m_istrRword);
					}

					GRFQUALK grfqualk = 0x1 << qualk;
					
					if (auto pTinqualPrev = PTinRtiCast<TypeInfoQualifier *>(pTinse->m_pTin))
					{
						pTinqualPrev->m_grfqualk.AddFlags(grfqualk);
						pStnod->m_pTin = pTinqualPrev;
					}
					else
					{
						auto pTinqual = pTinse->m_pSymtab->PTinqualEnsure(pTinse->m_pTin, grfqualk);
						pStnod->m_pTin = pTinqual;
					}

					PopTinSpecStack(&aryTinse, pStnod->m_pTin, &pTinReturn);
				}
			} break;
		case PARK_ReferenceDecl:
			{
				if (pTinse->m_nState++ == 0)
				{
					fAllowForwardDecl |= true;
					MOE_ASSERT(pStnod->CPStnodChild() == 1, "expected one child");
					PushTinSpecStack(&aryTinse, pStnod->PStnodChild(0), pTinse->m_pSymtab);
				}
				else
				{
					auto pTinptr = pTinse->m_pSymtab->PTinptrAllocate(pTinse->m_pTin);
					pStnod->m_pTin = pTinptr;

					PopTinSpecStack(&aryTinse, pStnod->m_pTin, &pTinReturn);
				}
			} break;
		case PARK_ProcedureReferenceDecl:
		case PARK_GenericDecl:
			{
				PopTinSpecStack(&aryTinse, pStnod->m_pTin, &pTinReturn);
			} break;
		default: MOE_ASSERT(false, "Unexpected parse node %s in PTinFromTypeSpecification", PChzLongFromPark(pStnod->m_park));
			break;
		}
	}

	if (*pFIsValidTypeSpec)
	{
		pTinReturn = pSymtabRoot->PTinMakeUnique(pTinReturn);
	}
	return pTinReturn;
}

PARK ParkDefinition(TypeCheckWorkspace * pTcwork, const Symbol * pSym)
{
	if (MOE_FVERIFY(pSym->m_pStnodDefinition, "symbol without definition"))
	{
		PARK parkDef = pSym->m_pStnodDefinition->m_park;
		switch (parkDef)
		{
		case PARK_ProcedureDefinition:	return parkDef;
		case PARK_Decl:					return parkDef; 
		case PARK_StructDefinition:		return PARK_Decl; // instances and types shadow each other
		default: 
			EmitError(pTcwork, pSym->m_pStnodDefinition->m_lexsp, ERRID_UnexpectedSymbolDef,
				"Unexpected PARK %s for symbol definition", PChzLongFromPark(parkDef));
		}
	}
	return PARK_Nil;
}

void MarkAllSymbolsUsed(SymbolTable * pSymtab)
{
	Moe::CHash<InString, Symbol *>::CIterator iterSym(&pSymtab->m_hashIstrPSym);

	Symbol ** ppSym;
	while ((ppSym = iterSym.Next()))
	{
		Symbol * pSymIt = nullptr;
		if (ppSym)
			pSymIt = *ppSym;

		while (pSymIt)
		{
			pSymIt->m_symdep = SYMDEP_Used;
			pSymIt = pSymIt->m_pSymPrev;
		}
	}

	for (ppSym = pSymtab->m_arypSymGenerics.A(); ppSym != pSymtab->m_arypSymGenerics.PMac(); ++ppSym)
	{
		Symbol * pSymIt = *ppSym;
		while (pSymIt)
		{
			pSymIt->m_symdep = SYMDEP_Used;
			pSymIt = pSymIt->m_pSymPrev;
		}
	}
}

void MarkSymbolsUsed(Symbol * pSymEntry, Alloc * pAlloc)
{
	CDynAry<Symbol *> aryPSymStack(pAlloc, BK_Dependency, 128);;
	aryPSymStack.Append(pSymEntry);

	while (!aryPSymStack.FIsEmpty())
	{
		auto pSym = aryPSymStack.Last();
		aryPSymStack.PopLast();
		if (pSym->m_symdep == SYMDEP_Used)
			continue;

		pSym->m_symdep = SYMDEP_Used;

		auto ppSymMac = pSym->m_aryPSymHasRefTo.PMac();	
		for (auto ppSym = pSym->m_aryPSymHasRefTo.A(); ppSym != ppSymMac; ++ppSym)
		{
			aryPSymStack.Append(*ppSym);
		}
	}
}

void ComputeSymbolDependencies(Alloc * pAlloc, ErrorManager * pErrman, SymbolTable * pSymtabRoot)
{
	CDynAry<Symbol *> arypSym(pAlloc, BK_Dependency, 1024);

	LexSpan lexsp;
	auto pSymMain = pSymtabRoot->PSymLookup(RWord::g_istrMain, lexsp);
	if (!pSymMain)
	{
		EmitError(pErrman, lexsp, ERRID_CantFindMain, "Failed to find global 'main' procedure");
		return;
	}

	MarkSymbolsUsed(pSymMain, pAlloc);

	SymbolTable * pSymtabIt = pSymtabRoot;
	while (pSymtabIt)
	{
		Moe::CHash<InString, Symbol *>::CIterator iterSym(&pSymtabIt->m_hashIstrPSym);
		Symbol ** ppSym;
		while ((ppSym = iterSym.Next()))
		{
			Symbol * pSymIt = *ppSym;
			while (pSymIt)
			{
				if (pSymIt->m_symdep == SYMDEP_PublicLinkage)
				{
					MarkSymbolsUsed(pSymIt, pAlloc);
				}
				else if (pSymIt->m_symdep == SYMDEP_Nil)
				{
					pSymIt->m_symdep = SYMDEP_Unused;
				}
				pSymIt = pSymIt->m_pSymPrev;

			}
		}

		Symbol ** ppSymMac = pSymtabIt->m_arypSymGenerics.PMac();
		for (ppSym = pSymtabIt->m_arypSymGenerics.A(); ppSym != ppSymMac; ++ppSym)
		{
			MOE_ASSERT(*ppSym, "null symbol");
			Symbol * pSymIt = *ppSym;
			while (pSymIt)
			{
				if (pSymIt->m_symdep == SYMDEP_PublicLinkage)
				{
					MarkSymbolsUsed(pSymIt, pAlloc);
				}
				else if (pSymIt->m_symdep == SYMDEP_Nil)
				{
					pSymIt->m_symdep = SYMDEP_Unused;
				}
				pSymIt = pSymIt->m_pSymPrev;
			}
		}

		pSymtabIt = pSymtabIt->m_pSymtabNextManaged;
	}
}

TCRET TcretWaitForTypeSymbol(TypeCheckWorkspace * pTcwork, TypeCheckFrame * pTcfram, Symbol * pSymType, STNode * pStnodType)
{
	if (!pSymType)
	{
		InString istrTypename = IstrTypenameFromTypeSpecification(pStnodType);
		EmitError(pTcwork, pStnodType->m_lexsp, ERRID_UnknownSymbol, "'%s' unknown symbol detected", istrTypename.PChz());
		return TCRET_StoppingError;
	}

	if (!pSymType->m_grfsym.FIsSet(FSYM_IsType))
	{
		InString istrName = IstrFullyQualifiedSymbol(pSymType);
		EmitError(pTcwork, pStnodType->m_lexsp, ERRID_TypeExpected, "%s symbol refers to instance, but was expecting type", istrName.PChz());
		return TCRET_StoppingError;
	}
	else
	{
		// wait for this type to be resolved.
		UnknownType * pUntype = PUntypeEnsure(pTcwork, pSymType);
		pUntype->m_arypTcframDependent.Append(pTcfram);
		return TCRET_WaitingForSymbolDefinition;
	}
}

// wrapper struct to allow breaking on returning different TCRET values
struct TcretDebug
{
			TcretDebug(TCRET tcret)
			:m_tcret(tcret)
				{
					if (tcret == TCRET_StoppingError)
						DoNothing();
					if (tcret == TCRET_WaitingForSymbolDefinition)
						DoNothing();
				}

			operator TCRET()
				{ return m_tcret; }

	TCRET	m_tcret;
};

TCRET TcretCheckProcedureDef(STNode * pStnod, TypeCheckWorkspace * pTcwork, TypeCheckFrame * pTcfram, TypeCheckStackEntry * pTcsentTop)
{
	auto pStproc = PStnodRtiCast<STProc *>(pStnod);
	if (!MOE_FVERIFY(pStproc, "missing procedure parse data"))
		return TCRET_StoppingError;

	TypeInfoProcedure * pTinproc = (TypeInfoProcedure *)pStnod->m_pTin;
	if (!MOE_FVERIFY(pTinproc && pTinproc->m_tink == TINK_Procedure, "missing procedure type info"))
		return TCRET_StoppingError;

	switch(pTcsentTop->m_nState++)
	{
	case 0:
		{	
			auto pSym = pStnod->PSym();
			MOE_ASSERT(pSym, "expected procedure symbol");
			pTcsentTop->m_pSymContext = pSym;

			// push the parameter list
			if (pStproc->m_pStnodParameterList)
			{
				STNode * pStnodParamList = pStproc->m_pStnodParameterList;

				auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnodParamList);
				if (pTcsentPushed)
				{
					pTcsentPushed->m_pSymtab = pStnodParamList->m_pSymtab;
					MOE_ASSERT(pTcsentPushed->m_pSymtab, "null symbol table");
				}
			}
		}break;
	case 1:
		{	
			// push the return type
			if (STNode * pStnodReturn = pStproc->m_pStnodReturnType)
			{
				auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnodReturn);

				// if we have a parameter list, use it's symbol table so we can return generic types 
				//  defined in the arguments
				SymbolTable * pSymtab = pTcsentTop->m_pSymtab;
				STNode * pStnodParamList = pStproc->m_pStnodParameterList;
				if (pStnodParamList && pStnodParamList->m_pSymtab)
				{
					pSymtab = pStnodParamList->m_pSymtab;
				}

				pStnodReturn->m_pSymtab = pSymtab;
				if (pTcsentPushed)
				{
					pTcsentPushed->m_pSymtab = pSymtab;
					pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
				}
			}
		}break;
	case 2:
		{
			if (STNode * pStnodParamList = pStproc->m_pStnodParameterList)
			{
				int cParamsExpected = pStnodParamList->CPStnodChild() - pTinproc->FHasVarArgs();
				MOE_ASSERT(pTinproc->m_arypTinParams.C() == cParamsExpected, "parameter child mismatch");
				for (int iStnodArg = 0; iStnodArg < cParamsExpected; ++iStnodArg)
				{
					pTinproc->m_arypTinParams[iStnodArg] = pStnodParamList->PStnodChild(iStnodArg)->m_pTin;
				}
			}

			// type check the return list
			if (pStnod->m_strees < STREES_SignatureTypeChecked)
			{
				if (pStproc->m_pStnodReturnType)
				{
					STNode * pStnodReturn = pStproc->m_pStnodReturnType;
					bool fIsValidTypeSpec;
					TypeInfo * pTinReturn = PTinFromTypeSpecification(
												pTcwork,
												pStnodReturn->m_pSymtab,
												pStnodReturn,
												pTcsentTop->m_grfsymlook,
												nullptr, 
												&fIsValidTypeSpec);
					if (!fIsValidTypeSpec)
						return TCRET_StoppingError;

					if (!pTinReturn)
					{
						EmitError(pTcwork, pStnod->m_lexsp, ERRID_ReturnTypeExpected, "failed to parse return type");
					}
					pStnodReturn->m_pTin = pTinReturn;

					pTinproc->m_arypTinReturns[0] = pTinReturn;
				}
				pStnod->m_strees = STREES_SignatureTypeChecked;

				// find our symbol and resolve any pending unknown types

				STNode * pStnodIdent = nullptr;
				if (pStproc->m_pStnodName)
				{
					InString strProcName = IstrFromIdentifier(pStproc->m_pStnodName);
					pStnodIdent = pStproc->m_pStnodName;
				}

				if (pStnodIdent->m_tok != TOK_Identifier) // must be op overloaded procedure
				{
					auto errid = ErridCheckOverloadSignature(pStnodIdent->m_tok, pTinproc, pTcwork->m_pErrman, pStnod->m_lexsp);
					if (errid != ERRID_Nil)
						return TCRET_StoppingError;
				}
			}

			if (STNode * pStnodParams = pStproc->m_pStnodParameterList)
			{
				MOE_ASSERT(pStnodParams->m_park == PARK_ParameterList, "expected parameter list");

				for (int iStnod = 0; iStnod < pStnodParams->CPStnodChild(); ++iStnod)
				{
					auto pStnodParam = pStnodParams->PStnodChild(iStnod);
					if (pStnodParam->m_park == PARK_VariadicArg)
						continue;

					if (pTinproc->m_mpIptinGrfparmq[iStnod].FIsSet(FPARMQ_ImplicitRef))
					{
						auto pTinptr = PTinRtiCast<TypeInfoPointer*>(pStnodParam->m_pTin);
						if (pTinptr)
						{
							pTinptr->m_fIsImplicitRef = true;
						}
					}

					if (!FIsValidLhs(pStnodParam))
					{
						auto istrType = IstrFromTypeInfo(pStnodParam->m_pTin);
						EmitError(
							pTcwork, pStnod->m_lexsp, ERRID_InvalidArgument,
							"Argument %d is not a valid argument type. '%s' does not define an assignment operator", 
							iStnod + 1,
							istrType.m_pChz);
					}
				}

				if (pTcwork->m_pErrman->FHasErrors())
				{
					return TCRET_StoppingError;
				}
			}

			Symbol * pSymProc = pStnod->PSym();
			if (!MOE_FVERIFY(pSymProc, "failed to find procedure name symbol: %s", pTinproc->m_istrName.m_pChz))
				return TCRET_StoppingError;

			auto pTinSym = (PTinFromSymbol(pSymProc));
			if (!MOE_FVERIFY(pTinSym, "expected procedure type info to be created during parse"))
				return TCRET_StoppingError;

			MOE_ASSERT(pSymProc, "null symbol");

			if (pTinproc && FIsGenericType(pTinproc))
			{
				auto pTinproc = PTinDerivedCast<TypeInfoProcedure *>(pTinSym);

				OnTypeResolve(pTcwork, pSymProc);

				for (int ipTin = 0; ipTin < pTinproc->m_arypTinParams.C(); ++ipTin)
				{
					MOE_ASSERT(pTinproc->m_arypTinParams[ipTin], "null parameter type? arg %d", ipTin);
				}

				pStnod->m_grfstnod.AddFlags(FSTNOD_NoCodeGeneration);
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
				return TCRET_Complete;
			}

			// push the body subtree
			if (pStproc->m_pStnodBody)
			{
				STNode * pStnodBody = pStproc->m_pStnodBody;
				auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnodBody);

				if (pTcsentPushed)
				{
					pTcsentPushed->m_pStnodProcedure = pStnod;
					pTcsentPushed->m_pSymtab = pStnodBody->m_pSymtab;
				}
			}
		}break;
	case 3:
		{
			pTinproc->m_istrMangled = IstrComputeMangled(pTcwork, pStnod, pTcsentTop->m_pSymtab);
			PopTcsent(pTcfram, &pTcsentTop, pStnod);

			Symbol * pSymProc = pStnod->PSym();
			if (pStproc->m_grfstproc.FIsSet(FSTPROC_PublicLinkage))
			{
				pSymProc->m_symdep = SYMDEP_PublicLinkage;
			}

			pStnod->m_strees = STREES_TypeChecked;
			OnTypeResolve(pTcwork, pSymProc);
		}break;
	}
	return TCRET_Continue;
} 

TcretDebug TcretCheckDecl(STNode * pStnod, TypeCheckWorkspace * pTcwork, TypeCheckFrame * pTcfram, TypeCheckStackEntry * pTcsentTop)
{
	auto pStdecl = PStnodDerivedCast<STDecl *>(pStnod);

	auto pStnodIdent = pStdecl->m_pStnodIdentifier;
	if (pStnodIdent && pTcsentTop->m_pSymContext == nullptr)
	{
		auto pSym = pStnodIdent->PSym();
		MOE_ASSERT(pSym, "expected symbol for declaration");
		pTcsentTop->m_pSymContext = pSym;
	}

#define ENSURE_SYMBOL_LOOKUP 0
#if ENSURE_SYMBOL_LOOKUP
	if (pStnodIdent && pStnodIdent->PSym())
	{
		auto pSymThis = pStnodIdent->PSym();
		auto pSymLookup = pTcsentTop->m_pSymtab->PSymLookup(pSymThis->m_istrName, pStnod->m_lexsp);
		MOE_ASSERT(pSymThis == pSymLookup && pSymThis->m_pStnodDefinition == pSymLookup->m_pStnodDefinition, "Self lookup failed");

		auto pSymbase = PSymbaseLookup(pTcsentTop->m_pSymtab, pSymThis->m_istrName, pStnodIdent->m_lexsp, FSYMLOOK_Default);
		MOE_ASSERT(pSymbase == pSymLookup, "lookup mismatch");

	}
#endif

	if (pTcsentTop->m_nState < pStnod->CPStnodChild())
	{
		if (pStdecl->PStnodChild(pTcsentTop->m_nState) != pStdecl->m_pStnodIdentifier)
		{
			auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
			if (pTcsentPushed)
			{
				auto pStnodChild = pStdecl->PStnodChild(pTcsentTop->m_nState);
				if (pStnodChild == pStdecl->m_pStnodInit)
				{
					// Note: Allow forward declarations - we may be initializing to a pointer to the current procedure
					pTcsentPushed->m_fAllowForwardDecl = true;
				}
				else if (pStnodChild == pStdecl->m_pStnodType)
				{
					pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
				}
			}
		}

		++pTcsentTop->m_nState;
		return TCRET_Continue;
	}

	{
		SymbolTable * pSymtab = pTcsentTop->m_pSymtab;
		if (pStdecl->m_pStnodType)
		{
			STNode * pStnodType = pStdecl->m_pStnodType;

			Symbol * pSymType = nullptr;
			bool fIsValidTypeSpec;
			TypeInfo * pTinType = PTinFromTypeSpecification(
										pTcwork,
										pSymtab,
										pStnodType,
										pTcsentTop->m_grfsymlook,
										&pSymType,
										&fIsValidTypeSpec);

			if (!fIsValidTypeSpec)
				return TCRET_StoppingError;

			if (pTinType)
			{
				pStnod->m_pTin = pTinType;
			}
			else
			{
				TypeInfo * pTinTypeDebug = PTinFromTypeSpecification(
										pTcwork,
										pSymtab,
										pStnodType,
										pTcsentTop->m_grfsymlook,
										&pSymType,
										&fIsValidTypeSpec);

				return TcretWaitForTypeSymbol(pTcwork, pTcfram, pSymType, pStnodType);
			}
		}

		STNode * pStnodInit = pStdecl->m_pStnodInit;
		if (pStnodInit)
		{
			if (!FVerifyIvalk(pTcwork, pStnodInit, IVALK_RValue))
			{
				pStnodInit->m_park = PARK_Uninitializer;
			}

			TypeInfo * pTinInitDefault = pStnod->m_pTin;
			if (!pTinInitDefault && pStnodInit->m_park != PARK_Uninitializer)
			{
				InString istrIdent = IstrFromIdentifier(pStnodIdent);
				SymbolTable::SymbolIterator symiter(pSymtab, istrIdent, pStnodIdent->m_lexsp, pTcsentTop->m_grfsymlook);
				auto pSymPrior = symiter.PSymNext();

				auto pSymIdent = pStnodIdent->PSym();
				while (!symiter.FIsDone() && LexspFromSym(pSymIdent) <= LexspFromSym(pSymPrior))
				{
					pSymPrior = symiter.PSymNext();
				}

				if (pSymPrior && LexspFromSym(pSymPrior) < LexspFromSym(pSymIdent))
				{
					if (!pSymPrior->m_pStnodDefinition)
					{
						EmitError(pTcwork, pStnod->m_lexsp, ERRID_InitTypeMismatch,
							"'%s' is already declared. \n"
							"Type inference is not allowed on overloaded variables. Did you type ':=' but meant '='?",
							istrIdent.m_pChz);
					}
					else
					{
						LexLookup lexlook(pTcwork->m_pErrman->m_pWork, pSymPrior->m_pStnodDefinition);

						EmitError(pTcwork, pStnod->m_lexsp, ERRID_InitTypeMismatch,
							"'%s' is already declared at %s (%d, %d). \n"
							"Type inference is not allowed on overloaded variables. Did you type ':=' but meant '='?",
							istrIdent.PChz(),
							lexlook.m_istrFilename.m_pChz,
							lexlook.m_iLine,
							lexlook.m_iCodepoint);
					}
				}

				if (!pStnodInit->m_pTin)
				{
					EmitError(pTcwork, pStnod->m_lexsp, ERRID_InitTypeMismatch, "trying to initialize %s with a 'void' type", istrIdent.PChz());
					return TCRET_StoppingError;
				}

				switch (pStnodInit->m_pTin->m_tink)
				{
				case TINK_Void:
				case TINK_Type:
					EmitError(pTcwork, pStnod->m_lexsp, ERRID_UninstantiableType,
						"cannot initialize variable %s with type '%s'.", istrIdent.PChz(), PChzFromTink(pStnodInit->m_pTin->m_tink));
					return TCRET_StoppingError;
				default:
					break;
				}

				// BB - This won't allow an override of operator:= to return a different type
				// I'm planning on coming back to it when I handle return types values as regular LValues
				pTinInitDefault = PTinPromoteUntypedDefault(pTcwork, pTcsentTop->m_pSymtab, pStnodInit);

				// pass pTin as pTinDst - we aren't assigning it to a different type
				pTinInitDefault = PTinAfterRValueAssignment(pTcwork, pStnodInit->m_lexsp, pTinInitDefault, pTcsentTop->m_pSymtab, pTinInitDefault);

				MOE_ASSERT(pTinInitDefault, "failed to compute default init type");
			}

			SOverloadCheck ovcheck(nullptr);

			auto pStnodOpLhs = pStnodIdent;
#ifndef MOEB_LATER
			MOE_ASSERT(pStnodOpLhs, "compund decls are TBD");
#else
			if (!pStnodOpLhs)
			{
				pStnodOpLhs = pStdecl->m_pStnodChildMin;
			}
#endif

			if (pTinInitDefault && pStnodOpLhs && pStnodInit->m_park != PARK_Uninitializer)
			{
				MOE_ASSERT(pStnodOpLhs && pStnodOpLhs->m_pTin == nullptr, "expected null identifier type");
				pStnodOpLhs->m_pTin = pTinInitDefault;

				// Check for overloads on the ':=' operator
				//   What happens with constant init?
				//   What about globals?

				pStnod->m_tok = TOK_ColonEqual;

				STNode * apStnod[2];
				apStnod[0] = pStnodOpLhs;
				apStnod[1] = pStnodInit;

				ProcMatchParam pmparam(pTcwork->m_pAlloc, pStnod->m_lexsp);
				pmparam.m_cpStnodCall = 2;
				pmparam.m_ppStnodCall = apStnod;

				ovcheck = OvcheckTryCheckOverload(pTcwork, pTcfram, pStnod, &pmparam);
				pStnodOpLhs->m_pTin = nullptr;
			}

			if (ovcheck.m_pTinproc)
			{
				if (ovcheck.m_tcret != TCRET_Complete)
					return ovcheck.m_tcret;

				auto pTinproc = ovcheck.m_pTinproc;
				MOE_ASSERT(pTinproc->m_arypTinParams.C() == 2 && pTinproc->m_arypTinReturns.C() == 1, "bad operator overload signature");

				auto pStop = PStnodRtiCast<STOperator *>(pStnod);
				pStop->m_optype.m_pTinLhs = pTinproc->m_arypTinParams[0];
				pStop->m_optype.m_pTinRhs = pTinproc->m_arypTinParams[1];
				pStop->m_optype.m_pTinResult = pTinproc->m_arypTinReturns[0];
				pStop->m_optype.m_pTinprocOverload = pTinproc;
				MOE_ASSERT(ovcheck.m_argord == ARGORD_Normal, "Decl arguments cannot be commutative");

				pStnod->m_pTin = pTinInitDefault;
				FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pStop->m_optype.m_pTinLhs, pStnodInit);
			}
			else
			{
				// don't finalize the literal for init here if it's actually a default argument.
				//  enum values finalize to the loose int types and it causes the default argument type checking to fail

				bool fAllowFinalizing = pTcsentTop->m_parkDeclContext != PARK_ParameterList;

				if (pStnod->m_pTin && pStnodInit->m_park != PARK_Uninitializer)
				{
					// just make sure the init type fits the specified one
					TypeInfo * pTinInit = pStnodInit->m_pTin;
					pTinInit = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodInit, pStnod->m_pTin);

					// Strip the top level const, as we're declaring a new instance
					if (pStnod->m_pTin->m_tink == TINK_Anchor || FCanCastForInit(pTcwork, pStnod->m_lexsp, pSymtab, pTinInit, pStnod->m_pTin))
					{
						if (fAllowFinalizing)
						{
							FinalizeLiteralType(pTcwork, pSymtab, pStnod->m_pTin, pStnodInit);
						}
						pTinInit = pStnod->m_pTin;
					}
					else
					{
						const char * pChzFormat = (pTcsentTop->m_parkDeclContext == PARK_ParameterList) ?
							"parameter '%s' is type '%s', but default argument is '%s'" :
							"Cannot initialize variable '%s' of type '%s' with '%s'";

						InString istrIdent = IstrFromIdentifier(pStnodIdent);
						EmitError(pTcwork, pStnod->m_lexsp, ERRID_InitTypeMismatch,
							pChzFormat,
							istrIdent.PChz(),
							IstrFromTypeInfo(pStnod->m_pTin).PChz(),
							IstrFromTypeInfo(pTinInit).PChz());
					}
				}
				else if (pTinInitDefault)
				{
					pStnod->m_pTin = pTinInitDefault;
					if (fAllowFinalizing)
					{
						FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pStnod->m_pTin, pStnodInit);
					}
				}
			}
		}

#if MOEB_LATER
		// would this be better if there was a PARK_CompoundDecl?
		bool fIsCompoundDecl = pStdecl->m_iStnodChildMin != -1;
#else
		bool fIsCompoundDecl = false;
#endif
		if (!fIsCompoundDecl)
		{
			if (pStnod->m_pTin == nullptr)
			{
				const char * pChzIdent = (pStnodIdent) ? IstrFromIdentifier(pStnodIdent).m_pChz : "declaration";
				EmitError(pTcwork, pStnod->m_lexsp, ERRID_CannotInferType,
					"Unable to calculate type for $%s", pChzIdent);
				return TCRET_StoppingError;
			}

			if (pStnodIdent)
			{
				// find our symbol and resolve any pending unknown types
				InString strIdent = IstrFromIdentifier(pStnodIdent);

				// may not have symbols for a declaration if this is inside a procedure reference decl
				auto pSymIdent = pStnodIdent->PSym();
				if (pSymIdent)
				{
					pStnod->m_pSymbase = pSymIdent;
					OnTypeResolve(pTcwork, pSymIdent);
				}
			}
			else if (!FIsGenericType(pStnod->m_pTin))
			{
				EmitError(pTcwork, pStnod->m_lexsp, ERRID_UnnamedNotAllowed, 
					"Unnamed declaration must be used with a generic type specification");
			}

			if (pStdecl->m_fHasUsingPrefix)
			{
				auto pTinUsing = pStnod->m_pTin;
				auto pSymtabUsing = PSymtabFromType(pTcwork, pTinUsing, pStnod->m_lexsp);
				if (pSymtabUsing)
				{
					pSymtab->AddUsingScope(pTcwork->m_pErrman, pSymtabUsing, pStnod);
				}
			}
		}

		pStnod->m_strees = STREES_TypeChecked;
		PopTcsent(pTcfram, &pTcsentTop, pStnod);
	}
	return TCRET_Continue;
}

TcretDebug TcretTypeCheckSubtree(TypeCheckWorkspace * pTcwork, TypeCheckFrame * pTcfram)
{
	CDynAry<TypeCheckStackEntry> * paryTcsent = &pTcfram->m_aryTcsent;
	while (paryTcsent->C())
	{
		TypeCheckStackEntry * pTcsentTop = paryTcsent->PLast();
		STNode * pStnod = pTcsentTop->m_pStnod;

		switch (pStnod->m_park)
		{
		case PARK_ProcedureDefinition:
			{
				TCRET tcret = TcretCheckProcedureDef(pStnod, pTcwork, pTcfram, pTcsentTop);
				if (tcret != TCRET_Continue)
					return tcret;
			}

		case PARK_Decl:
		{
			TCRET tcret = TcretCheckDecl(pStnod, pTcwork, pTcfram, pTcsentTop);
				if (tcret != TCRET_Continue)
					return tcret;
		}

		default:
			MOE_ASSERT(false, "unknown parse kind (%s) encountered during type check", PChzLongFromPark(pStnod->m_park));
			break;
		}
	}
	return TCRET_Complete;
}

void PerformTypeCheck(
	Alloc * pAlloc,
	ErrorManager * pErrman,
	SymbolTable * pSymtabTop,
	BlockListEntry * pblistEntry,
	CDynAry<WorkspaceEntry *> * parypEntryChecked, 
	GRFUNT grfunt)
{
	auto pTcwork = MOE_NEW(pAlloc, TypeCheckWorkspace) TypeCheckWorkspace(pAlloc, pErrman, pblistEntry);

	Symbol * pSymRoot = nullptr;
	// if we're in a unit test we spoof a top level implicit function symbol
	if (grfunt.FIsSet(FUNT_ImplicitProc))
	{
		pSymRoot = pSymtabTop->PSymEnsure(pErrman, IstrIntern("__ImplicitMethod"), nullptr);
	}

	int ipTcfram = 0;
	BlockListEntry::CIterator iterEntry(pblistEntry);
	while (WorkspaceEntry * pEntry = iterEntry.Next())
	{
		MOE_ASSERT(pEntry->m_pSymtab, "entry point without symbol table");
		TypeCheckFrame * pTcfram = pTcwork->m_blistTcfram.AppendNew();
		pTcfram->m_ipTcframQueue = ipTcfram;
		pTcfram->m_pEntry = pEntry;

		pTcfram->m_aryTcsent.SetAlloc(pAlloc, Moe::BK_TypeCheckStack);
		TypeCheckStackEntry * pTcsent = pTcfram->m_aryTcsent.AppendNew();
		pTcsent->m_nState = 0;
		pTcsent->m_pStnod = pEntry->m_pStnod;
		pTcsent->m_pSymtab = pEntry->m_pSymtab;
		pTcsent->m_pStnodProcedure = nullptr;
		pTcsent->m_pSymContext = pSymRoot;
		pTcsent->m_grfsymlook = FSYMLOOK_Default;
		pTcsent->m_parkDeclContext = PARK_Nil;
		pTcsent->m_fAllowForwardDecl = false;
		pTcsent->m_tcctx = TCCTX_Normal;

		pTcwork->m_arypTcframPending.Append(pTcfram);
		++ipTcfram;
	}

	int cStoppingError = 0;
	while (pTcwork->m_arypTcframPending.C())
	{
		TypeCheckFrame * pTcfram = pTcwork->m_arypTcframPending[0];
		TCRET tcret = TcretTypeCheckSubtree(pTcwork, pTcfram);

		if (tcret == TCRET_StoppingError)
		{
			// make sure we've reported at least one error.
			if (!pErrman->FHasErrors() && !pErrman->FHasHiddenErrors())
			{
				
				printf("unknown error - typecheck stack:\n");
				CDynAry<TypeCheckStackEntry> * paryTcsent = &pTcfram->m_aryTcsent;
				for (size_t iTcsent = 0; iTcsent < paryTcsent->C(); ++iTcsent)
				{
					const TypeCheckStackEntry & tcsent = (*paryTcsent)[iTcsent];

					LexLookup lexlook(pErrman->m_pWork, tcsent.m_pStnod->m_lexsp);
					printf("%p: PARK_%s      %s (" MOE_S64FMT "," MOE_S64FMT ")\n", 
						tcsent.m_pStnod,
						PChzLongFromPark(tcsent.m_pStnod->m_park), 
						lexlook.m_istrFilename.m_pChz, lexlook.m_iLine, lexlook.m_iCodepoint);
				}

				LexSpan lexsp;
				EmitError(pErrman, lexsp, ERRID_UnknownError, "Unknown error in type checker, quitting.");
			}

			while (pTcfram->m_aryTcsent.C())
			{
				TypeCheckStackEntry * pTcsentTop = pTcfram->m_aryTcsent.PLast();
				PopTcsent(pTcfram, &pTcsentTop, nullptr);
			}

			RelocateTcfram(pTcfram, &pTcwork->m_arypTcframPending, nullptr);
			++cStoppingError;
		}
		else if (tcret == TCRET_Complete)
		{
			RelocateTcfram(pTcfram, &pTcwork->m_arypTcframPending, nullptr);
			if (MOE_FVERIFY(pTcfram->m_pEntry, "type check frame missing workspace entry"))
			{
				parypEntryChecked->Append(pTcfram->m_pEntry);
			}
		}
		else if (tcret == TCRET_WaitingForSymbolDefinition)
		{
			RelocateTcfram(pTcfram, &pTcwork->m_arypTcframPending, &pTcwork->m_arypTcframWaiting);
		}
		else
		{
			MOE_ASSERT(false, "Unhandled type check return value.")	
		}

		ValidateTcframArray(&pTcwork->m_arypTcframPending);
		ValidateTcframArray(&pTcwork->m_arypTcframWaiting);
	}

	CHash<const Symbol *, UnknownType>::CIterator iter(&pTcwork->m_hashPSymUntype);

	// NOTE: Don't report unresolved type errors if we stopped typechecking any entry points early, odds
	//  are that our missing type is in there and it makes for very confusing error messages...
	//  The *right* thing to do is have some kind of graph check to see if the type was actually skipped.
	const Symbol ** ppSym;
	while (UnknownType * pUntype = iter.Next(&ppSym))
	{
		MOE_ASSERT(pUntype->m_arypTcframDependent.C() > 0, "unknown type not cleaned up (empty dependent array)");

		int cTcframDependent = (s32)pUntype->m_arypTcframDependent.C();
		for (size_t iTcfram = 0; iTcfram < cTcframDependent; ++iTcfram)
		{
			auto pTcfram = pUntype->m_arypTcframDependent[iTcfram];
			if (cStoppingError == 0)
			{
				// Note: we're assuming the top thing on the stack is the thing we're waiting for.
				const TypeCheckStackEntry * pTcsent = pTcfram->m_aryTcsent.PLast();

				EmitError(pTcwork, pTcsent->m_pStnod->m_lexsp, ERRID_UnresolvedTypeRef, 
					"Unresolved type '%s' reference found here", (*ppSym)->m_istrName.m_pChz);
			}

			// clean up the tcsent array for frames that are waiting on a symbol hidden by an error
			while (pTcfram->m_aryTcsent.C())
			{
				TypeCheckStackEntry * pTcsentTop = pTcfram->m_aryTcsent.PLast();
				PopTcsent(pTcfram, &pTcsentTop, nullptr);
			}
		}
	}

	//PerformFlushResolvedLiteralsPass(pTcwork, paryEntry);

	//check for top level collisions
	{
		Moe::CHash<Moe::InString, Symbol *>::CIterator iterSym(&pSymtabTop->m_hashIstrPSym);
		Symbol ** ppSym;
		while ((ppSym = iterSym.Next()))
		{
			auto pSym = *ppSym;
			for (auto pSymSrc = pSym; pSymSrc; pSymSrc = pSymSrc->m_pSymPrev)
			{
				for (auto pSymDst = pSymSrc->m_pSymPrev; pSymDst; pSymDst = pSymDst->m_pSymPrev)
				{
					PARK parkSrc = ParkDefinition(pTcwork, pSymSrc);
					PARK parkDst = ParkDefinition(pTcwork, pSymDst);

					if (parkSrc != parkDst)
						continue;

					if (parkSrc == PARK_ProcedureDefinition)
					{
						auto pTinSrc = PTinFromSymbol(pSymSrc);
						auto pTinDst = PTinFromSymbol(pSymDst);
						if (!FTypesAreSame(pTinSrc, pTinDst))
							continue;
					}

					if (pSymDst->m_pStnodDefinition)
					{
						LexLookup lexlook(pErrman->m_pWork, pSymDst->m_pStnodDefinition->m_lexsp);

						EmitError(
							pTcwork,
							pSym->m_pStnodDefinition->m_lexsp, 
							ERRID_ShadowedDefine,
							"Top level symbol '%s' is also defined here %s(%d,%d)",
							pSym->m_istrName.m_pChz,
							lexlook.m_istrFilename.m_pChz,
							lexlook.m_iLine,
							lexlook.m_iCodepoint);
					}
				}
			}
		}
	}
	
	if (grfunt.FIsSet(FUNT_ResolveAllSymbols))
	{
		SymbolTable * pSymtabIt = pSymtabTop;
		while(pSymtabIt)
		{
			MarkAllSymbolsUsed(pSymtabIt);
			pSymtabIt = pSymtabIt->m_pSymtabNextManaged;
		}
	}
	else
	{
		ComputeSymbolDependencies(pAlloc, pErrman, pSymtabTop);
	}

	pAlloc->MOE_DELETE(pTcwork);
}
