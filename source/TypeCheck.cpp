#include "CodeGen.h"
#include "Generics.inl"
#include "Parser.h"
#include "TypeInfo.h"
#include "Workspace.h"

#include <stdio.h>



static const bool s_fTypecheckPartialGenericStructs = true;

TypeInfo * PTinPromoteUntypedDefault(
	TypeCheckContext * pTcwork,
	SymbolTable * pSymtab,
	STNode * pStnodLit,
	TypeInfo * pTinExpected = nullptr,
	ERREP errep = ERREP_ReportErrors);

TypeInfo * PTinPromoteUntypedTightest(
	TypeCheckContext * pTcwork,
	SymbolTable * pSymtab,
	STNode * pStnodLit,	
	TypeInfo * pTinDst,
	ERREP errep = ERREP_ReportErrors);

TypeInfo * PTinPromoteUntypedArgument(
	TypeCheckContext * pTcwork,
	SymbolTable * pSymtab,
	STNode * pStnodLit,
	TypeInfo * pTinArgument,
	ERREP errep);

TypeInfo * PTinPromoteVarArg(TypeCheckContext * pTcwork, SymbolTable * pSymtab, TypeInfo * pTinIn);

void OnTypeResolve(TypeCheckContext * pTcwork, const Symbol * pSym);
void FinalizeLiteralType(TypeCheckContext * pTcwork, SymbolTable * pSymtab, TypeInfo * pTinDst, STNode * pStnodLit);
static bool FCanImplicitCast(TypeInfo * pTinSrc, TypeInfo * pTinDst);
TypeInfo * PTinQualifyAfterAssignment(TypeInfo * pTin, SymbolTable * pSymtab, TypeInfo * pTinDst);
TypeInfo * PTinAfterRValueAssignment(TypeCheckContext * pTcwork, const LexSpan & lexsp, TypeInfo * pTin, SymbolTable * pSymtab, TypeInfo * pTinDst);
SymbolTable * PSymtabFromType(TypeCheckContext * pTcwork, TypeInfo * pTin, const LexSpan & lexsp);

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

#define MOEB_TCJOB 0
#if MOEB_TCJOB
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
#endif

struct TypeCheckContext // tag = tcctx
{
									TypeCheckContext(Workspace * pWork, Alloc * pAlloc, WorkspaceEntry * pEntry)
									:m_pWork(pWork)
									,m_pAlloc(pAlloc)
									,m_pJobTc(nullptr)
									,m_mang(pAlloc)
									,m_pEntry(pEntry)
										{ ; }

	ErrorManager *					PErrman() 
										{ return m_pWork->m_pErrman; }

	Workspace *								m_pWork;
	Alloc *									m_pAlloc;
	Job *									m_pJobTc;
	NameMangler								m_mang;

	WorkspaceEntry *						m_pEntry;
	CDynAry<TypeCheckStackEntry>			m_aryTcsent;

};

void EmitWarning(TypeCheckContext * pTcctx, const LexSpan & lexsp, ERRID errid, const char * pChz, ...)
{
	va_list ap;
	va_start(ap, pChz);
	EmitWarning(pTcctx->PErrman(), lexsp, errid, pChz, ap);
}

void EmitError(TypeCheckContext * pTcctx, const LexSpan & lexsp, ERRID errid, const char * pChz, ...)
{
	va_list ap;
	va_start(ap, pChz);
	EmitError(pTcctx->PErrman(), lexsp, errid, pChz, ap);
}

Moe::InString IstrFromTypeInfo(TypeInfo * pTin)
{
	char aCh[1024];
	Moe::StringBuffer strbuf(aCh, MOE_DIM(aCh));

	WriteTypeInfoSExpression(&strbuf, pTin, PARK_Nil);
	return IstrInternCopy(aCh);
}

static inline void SetupSymbolWait(TypeCheckContext * pTcctx, Symbol * pSym)
{
	//@NotThreadSafe
	Workspace * pWork = pTcctx->m_pWork;
	JobPrereqSet * pJps;
	if (pWork->m_hashPSymJps.InresEnsureKey(pSym, &pJps) == INRES_Inserted)
	{
		pJps->m_arypJob.SetAlloc(pTcctx->m_pAlloc, BK_Job, 16);
	}

	pJps->m_arypJob.Append(pTcctx->m_pJobTc);
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

#if MOEB_TCJOB
UnknownType * PUntypeEnsure(TypeCheckContext * pTcwork, const Symbol * pSym)
{
	UnknownType * pUntype = pTcwork->m_hashPSymUntype.Lookup(pSym);
	if (!pUntype)
	{
		pTcwork->m_hashPSymUntype.InresEnsureKey(pSym, &pUntype);
		pUntype->m_arypTcframDependent.SetAlloc(pTcwork->m_pAlloc, Moe::BK_TypeCheck);
	}
	return pUntype;
}
#endif

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

			if (pTinn->m_numk == NUMK_Float)
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
				aChz[1] = (pTinn->m_numk == NUMK_SignedInt) ? 'i' : 'u';
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
	return IstrInternCopy(m_strbuf.m_pChzBegin, m_strbuf.m_pChzAppend - m_strbuf.m_pChzBegin + 1);
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

InString IstrComputeMangled(TypeCheckContext * pTcctx, STNode * pStnod, SymbolTable * pSymtab)
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
		auto istrMangled = pTcctx->m_mang.IstrMangleMethodName(pTinproc);

#if VALIDATE_NAME_MANGLING
		char aCh[1024];
		Moe::StringBuffer strbuf(aCh, MOE_DIM(aCh));

		WriteTypeInfoSExpression(&strbuf, pTinproc, PARK_Nil, FSEW_UseSizedNumerics);

		TypeInfoProcedure * pTinprocDemangled = pTcctx->m_mang.PTinprocDemangle(istrMangled, pSymtab);
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

bool FVerifyIvalk(TypeCheckContext * pTcctx, STNode * pStnod, IVALK ivalkExpected)
{
	auto ivalkActual = IvalkCompute(pStnod);
	if (ivalkActual < ivalkExpected)
	{
		const char * pChzIvalk = PChzFromIvalk(ivalkExpected);
		InString istrLhs = IstrFromIvalkStnod(pTcctx->m_pAlloc, pStnod);
		InString istrTin = IstrFromTypeInfo(pStnod->m_pTin);
		EmitError(pTcctx, pStnod->m_lexsp, ERRID_IncorrectIvalk, 
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
				(pTinlitA->m_litty.m_numk != pTinlitB->m_litty.m_numk))
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
			return (pTinnLhs->m_cBit == pTinnRhs->m_cBit) & (pTinnLhs->m_numk == pTinnRhs->m_numk);
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
	TypeCheckContext * pTcctx,
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
	TypeCheckContext * pTcctx,
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
			pTcctx->PErrman(),
			pSymSrc->m_istrName,
			pSymSrc->m_pStnodDefinition,
			pSymSrc->m_grfsym);

		auto ppStnodCopy = pmpPStnodGenPStnodNew->Lookup(pSymNew->m_pStnodDefinition);
		if (!ppStnodCopy)
		{
			EmitError(pTcctx, pSymNew->m_pStnodDefinition->m_lexsp, ERRID_MissingSymbolDef,
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
								pTcctx,
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
	TypeCheckContext * pTcctx, 
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
		SymbolTable * pSymtabSrcType = PSymtabFromType(pTcctx, pUsing->m_pStnod->m_pTin, pStnodUsing->m_lexsp);
		MOE_ASSERT(pSymtabSrcType == pUsing->m_pSymtab, "expected mapping failed");

		STNode * pStnodAdj = pUsing->m_pStnod;
		STNode ** ppStnodAdj = pmpPStnodGenPStnodNew->Lookup(pStnodUsing);
		if (ppStnodAdj)
		{
			pStnodAdj = *ppStnodAdj;
			pSymtabAdj = PSymtabFromType(pTcctx, pStnodAdj->m_pTin, pStnodUsing->m_lexsp);
		}
		else
		{
			SymbolTable ** ppSymtabLookup = pmpPSymtabSrcPSymtabNew->Lookup(pSymtabAdj);
			if (ppSymtabLookup)
			{
				pSymtabAdj = *ppSymtabLookup;
			}
		}

		pSymtabNew->AddUsingScope(pTcctx->PErrman(), pSymtabAdj, pStnodAdj);
	}
}

void RemapGenericStnodCopy(
	TypeCheckContext * pTcctx,
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
	CDynAry<STNode *> arypStnodStackGen(pTcctx->m_pAlloc, BK_TypeCheckGenerics);
	CDynAry<STNode *> arypStnodStackNew(pTcctx->m_pAlloc, BK_TypeCheckGenerics);
	arypStnodStackGen.Append(pStnodGen);
	arypStnodStackNew.Append(pStnodNew);

	CHash<SymbolTable *, SymbolTable *>	mpPSymtabSrcPSymtabNew(pTcctx->m_pAlloc, BK_TypeCheckGenerics);
	mpPSymtabSrcPSymtabNew.Insert(pSymtabSrc, pSymtabNew);

	struct SUsingRemap // tag = usrem
	{
		STNode * m_pStnodGen;
		STNode * m_pStnodNew;
	};
	CHash<SymbolTable *, SUsingRemap>	mpPSymtabUsrem(pTcctx->m_pAlloc, BK_TypeCheckGenerics);

	while (arypStnodStackGen.C())
	{
		MOE_ASSERT(arypStnodStackNew.C(),"remap stack mismatch!");
		auto pStnodGen	= arypStnodStackGen.TPopLast();
		auto pStnodNew	= arypStnodStackNew.TPopLast();

		auto pSymPrev = pStnodNew->PSym();
		if (pSymPrev)
		{
			bool fHasSymbolTin = pStnodNew->m_pTin == PTinFromSymbol(pSymPrev);
			auto pSymNew = PSymRemapGeneric(pTcctx, pSymPrev, pmpPSymGenericPSymRemapped);
			if (fHasSymbolTin)
			{
				pStnodNew->m_pTin = PTinFromSymbol(pSymNew);
			}

			pStnodNew->m_pSymbase = pSymNew;
		}
		else if (pStnodNew->m_pTin && FIsGenericType(pStnodNew->m_pTin))
		{
			 auto pTinRemapped = PTinSubstituteGenerics(pTcctx, pSymtabNew, pStnodNew->m_lexsp, pStnodNew->m_pTin, pGenmap, ERREP_ReportErrors);
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
					auto pStnodCopy = PStnodCopy(pTcctx->m_pAlloc, *ppStnodBaked);

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
					auto pSymtabNew = PSymtabNew(pTcctx->m_pAlloc, pSymtabCopySrc, pSymtabCopySrc->m_istrNamespace);	
					MOE_ASSERT(pSymtabCopySrc->m_aryUsing.FIsEmpty(), "TBD - unmapped using");

					*ppSymtabNew = pSymtabNew;
					ppSymtabNew = &pSymtabNew->m_pSymtabParent;

					CopySymbolsForGenericInstantiation(
						pTcctx,
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

		RemapUsingArray(pTcctx, pmpPStnodGenPStnodNew, &mpPSymtabSrcPSymtabNew, pSymtabUsingSrc, pSymtabUsingNew);
	}
}

TypeInfoStruct * PTinstructEnsureUniqueInstance(
	TypeCheckContext * pTcctx,
	TypeInfoStruct * pTinstruct)
{
	STNode * pStnodInstFrom = nullptr;
	if (pTinstruct->m_pTinstructInstFrom)
	{
		pStnodInstFrom = pTinstruct->m_pTinstructInstFrom->m_pStnodStruct;
	}
	MOE_ASSERT(pStnodInstFrom && pTinstruct->m_pGenmap, "expected generic structure with instanced AST");
	auto pEntry = pTcctx->m_pWork->m_pGenreg->PEntryEnsure(pStnodInstFrom, pTinstruct->m_pGenmap);
	if (pEntry->m_pTin == nullptr)
	{
		pEntry->m_pTin = pTinstruct;
		return pTinstruct;
	}

	return PTinDerivedCast<TypeInfoStruct *>(pEntry->m_pTin);
}

InstantiateRequest * PInsreqLookup(
	TypeCheckContext * pTcctx,
	STNode * pStnodDefinition,
	GenericMap * pGenmap)
{
	auto pEntry = pTcctx->m_pWork->m_pGenreg->PEntryLookup(pStnodDefinition, pGenmap);
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

TypeCheckStackEntry * PTcsentPush(TypeCheckContext * pTcctx, TypeCheckStackEntry ** ppTcsentTop, STNode * pStnod)
{
	if (!pStnod)
		return nullptr;

	if (pStnod->m_strees >= STREES_TypeChecked)
	{
		// we can have type checked nodes here when we're dealing with baked constants
		return nullptr;
	}

	// update ppTcsentTop to handle times when the dynArray reallocs.
	size_t iTcsentTop = pTcctx->m_aryTcsent.IFromP(*ppTcsentTop);

	size_t cPrev = pTcctx->m_aryTcsent.C()-1;
	TypeCheckStackEntry * pTcsent = pTcctx->m_aryTcsent.AppendNew();
	*pTcsent = pTcctx->m_aryTcsent[cPrev];

	pTcsent->m_nState = 0;
	pTcsent->m_pStnod = pStnod;

	*ppTcsentTop = &pTcctx->m_aryTcsent[iTcsentTop];
	return pTcsent;
}

void PopTcsent(TypeCheckContext * pTcctx, TypeCheckStackEntry ** ppTcsentTop, STNode * pStnodDebug)
{
	*ppTcsentTop = nullptr;
	MOE_ASSERT(pTcctx->m_aryTcsent.PLast()->m_pStnod == pStnodDebug || pStnodDebug == nullptr, "type check entry pop mismatch");

	pTcctx->m_aryTcsent.PopLast();
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

inline u64 NUnsignedLiteralCast(TypeCheckContext * pTcctx, const STValue * pStval)
{
	switch (pStval->m_stvalk)
	{
	case STVALK_Bool:
	case STVALK_Null:
	case STVALK_UnsignedInt:
		return pStval->m_nUnsigned;
	case STVALK_Float:
		return (s64)pStval->m_g;
	case STVALK_SignedInt:
		{
			if (pStval->m_nSigned < 0)
			{
				EmitError(pTcctx, pStval->m_lexsp, ERRID_LiteralOutsideBounds, "Implicit cast will discard negative value");
			}
			return (u64)pStval->m_nSigned;
		}
	default:
		MOE_ASSERT(false, "bad literal cast to unsigned int");
		return 0;
	}
}

inline s64 NSignedLiteralCast(TypeCheckContext * pTcctx, const STValue * pStval)
{
	switch (pStval->m_stvalk)
	{
	case STVALK_UnsignedInt:
		{
			if (pStval->m_nUnsigned > LLONG_MAX)
			{
				EmitError(pTcctx, pStval->m_lexsp, ERRID_LiteralOutsideBounds, "Literal is too large for implicit signed int cast.");
			}
			return (s64)pStval->m_nUnsigned;
		}
	case STVALK_Bool:
	case STVALK_Null:
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
	case STVALK_Bool:
	case STVALK_Null:
	case STVALK_UnsignedInt:	return (f64)pStval->m_nUnsigned;
	case STVALK_SignedInt:		return (f64)pStval->m_nSigned;
	case STVALK_Float:			return pStval->m_g;
	default:					MOE_ASSERT(false, "expected number");
	}

	return 0.0;
}

bool FDoesOperatorReturnBool(PARK park)
{
	// return if operator returns a bool (rather than the operand type)
	return  (park == PARK_RelationalOp) | (park == PARK_LogicalAndOrOp);
}

inline void SetIntegerValue(TypeCheckContext * pTcctx, STValue * pStval, const BigInt bint)
{
	if (bint.m_fIsNegative)
	{
		if (bint.m_nAbs > LLONG_MAX)
		{
			EmitError(pTcctx, pStval->m_lexsp, ERRID_LitOverflow, "Literal value overflow. Value is too large for signed int.");
		}
		pStval->SetS64(bint.S64Coerce());
	}
	else
	{
		pStval->SetU64(bint.U64Coerce());
	}
}

BigInt BintFromStval(STValue * pStval)
{
	switch (pStval->m_stvalk)
	{
	case STVALK_Bool:
	case STVALK_Null:
	case STVALK_UnsignedInt:	return BintFromUint(pStval->m_nUnsigned, false);
	case STVALK_SignedInt:		return BintFromInt(pStval->m_nSigned);
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

inline TypeInfo * PTinResult(PARK park, SymbolTable * pSymtab, TypeInfo * pTinOp)
{
	if (FDoesOperatorReturnBool(park))
		return pSymtab->PTinBuiltin(BuiltIn::g_istrBool);
	return pTinOp;
}

OpTypes OptypeFromPark(
	TypeCheckContext * pTcctx,
	SymbolTable * pSymtab,
	TOK tok,
	PARK parkOperator,
	TypeInfo * pTinLhs,
	TypeInfo * pTinRhs)
{
	if (parkOperator == PARK_LogicalAndOrOp)
	{
		auto pTinBool = pSymtab->PTinBuiltin(BuiltIn::g_istrBool);
		return OpTypes(pTinBool, pTinBool, pTinBool);
	}

	GRFQUALK grfqualkLhs = FQUALK_None;
	GRFQUALK grfqualkRhs = FQUALK_None;
	if (auto pTinqualLhs = PTinRtiCast<TypeInfoQualifier *>(pTinLhs))
		{ grfqualkLhs = pTinqualLhs->m_grfqualk; }
	if (auto pTinqualRhs = PTinRtiCast<TypeInfoQualifier *>(pTinRhs))
		{ grfqualkRhs = pTinqualRhs->m_grfqualk; }
	auto pTinUnqualLhs = PTinStripQualifiers(pTinLhs);
	auto pTinUnqualRhs = PTinStripQualifiers(pTinRhs);

	bool fLhsIsReference = (pTinUnqualLhs->m_tink == TINK_Pointer) | (pTinUnqualLhs->m_tink == TINK_Array);
	bool fRhsIsReference = (pTinUnqualRhs->m_tink == TINK_Pointer) | (pTinUnqualRhs->m_tink == TINK_Array);

	// BB - Could this be cleaner with a table?
	if (fLhsIsReference | fRhsIsReference)
	{
		TypeInfo * pTinMin = pTinUnqualLhs;
		TypeInfo * pTinMax = pTinUnqualRhs;
		if (pTinMin->m_tink > pTinMax->m_tink)
		{
			moeSwap(pTinMin, pTinMax);
		}
		TINK tinkMin = pTinMin->m_tink;
		TINK tinkMax = pTinMax->m_tink;

		if (fLhsIsReference & fRhsIsReference)
		{
			bool fLhsIsArrayRef = pTinUnqualLhs->m_tink == TINK_Array && 
									((TypeInfoArray*)pTinLhs)->m_aryk == ARYK_Reference;
			TypeInfo * pTinRefMax = nullptr;
			if (tinkMax == TINK_Array)
			{
				if (tinkMin != TINK_Pointer && !fLhsIsArrayRef) // no operand for array & array
					return OpTypes();

				pTinRefMax = PTinStripQualifiers(((TypeInfoArray *)pTinMax)->m_pTin);
			}
			else if (MOE_FVERIFY(tinkMax == TINK_Pointer, "unexpected reference type info"))
			{
				pTinRefMax = ((TypeInfoPointer *)pTinMax)->m_pTin;
				pTinRefMax = PTinStripQualifiers(pTinRefMax);
			}

			auto pTinRefMin = ((TypeInfoPointer*)pTinMin)->m_pTin;
			pTinRefMin = PTinStripQualifiers(pTinRefMin);

			if (parkOperator == PARK_AssignmentOp)
			{
				if (tok == TOK('='))
				{
					bool fAreRefTypesSame = FTypesAreSame(pTinRefMin, pTinRefMax);

					if (pTinLhs->m_tink == TINK_Array && !fLhsIsArrayRef)
						return OpTypes();

					if (!fAreRefTypesSame && 
						(pTinLhs->m_tink != TINK_Pointer || ((TypeInfoPointer *)pTinLhs)->m_pTin->m_tink != TINK_Void))
						return OpTypes();

					return OpTypes(pTinLhs, pTinRhs, pSymtab->PTinBuiltin(BuiltIn::g_istrBool));
				}
			}

			bool fAreRefTypesSame = FTypesAreSame(pTinRefMin, pTinRefMax);
			bool fIsOneTypeVoid = (pTinRefMin->m_tink == TINK_Void) | (pTinRefMax->m_tink == TINK_Void);
			if (parkOperator == PARK_RelationalOp && (fAreRefTypesSame | fIsOneTypeVoid))
			{
				if ((tok == TOK_EqualEqual) | (tok == TOK_NotEqual))
				{
					// cast the array to a pointer before comparing
					return OpTypes(pTinMin, pTinMin, pSymtab->PTinBuiltin(BuiltIn::g_istrBool));
				}
			}

			if (parkOperator == PARK_AdditiveOp)
			{
				if (tok == TOK('-') && fAreRefTypesSame)
				{
					return OpTypes(pTinLhs, pTinRhs, pSymtab->PTinBuiltin(BuiltIn::g_istrSSize));
				}
			}

			return OpTypes();
		}

		TypeInfo * pTinRef = pTinLhs;
		TypeInfo * pTinOther = pTinRhs;
		if (pTinOther->m_tink == TINK_Pointer || pTinOther->m_tink == TINK_Array)
		{
			pTinRef = pTinRhs;
			pTinOther = pTinLhs;
		}

		if (pTinRef->m_tink == TINK_Pointer && ((TypeInfoPointer *)pTinRef)->m_pTin->m_tink == TINK_Void)
		{
			return OpTypes();
		}
		
		PARK parkOperatorAdj = parkOperator;
		if (parkOperator == PARK_AssignmentOp && ((tok == TOK_PlusEqual) | (tok == TOK_MinusEqual)))
		{
			parkOperatorAdj = PARK_AdditiveOp;
		}

		if (parkOperatorAdj == PARK_AdditiveOp)
		{
			if (pTinOther->m_tink == TINK_Numeric && FIsInteger(((TypeInfoNumeric*)pTinOther)->m_numk))
			{
				return OpTypes(pTinLhs, pTinRhs, pTinRef);
			}
			else
			{
				MOE_ASSERT(false, "unexpected Additive operator");
			}
		}
	}

	if (pTinLhs->m_tink == pTinRhs->m_tink)
	{
		switch(pTinLhs->m_tink)
		{
		case TINK_Numeric:
			{
				TypeInfoNumeric * pTinnLhs = (TypeInfoNumeric*)pTinLhs;
				TypeInfoNumeric * pTinnRhs = (TypeInfoNumeric*)pTinRhs;

				if (pTinnRhs->m_numk != pTinnRhs->m_numk)
					return OpTypes();

				auto pTinOp = pTinLhs;
				if (pTinnLhs->m_cBit < pTinnRhs->m_cBit)
				{
					if (parkOperator == PARK_AssignmentOp)
					{
						return OpTypes();
					}
					pTinOp = pTinRhs;
				}

				return OpTypes(pTinOp, pTinOp, PTinResult(parkOperator, pSymtab, pTinOp));
			}
		case TINK_Array:
			return OpTypes();
		default:
			break;
		}

		if (FTypesAreSame(pTinLhs, pTinRhs))
		{
			return OpTypes(pTinLhs, pTinLhs, PTinResult(parkOperator, pSymtab, pTinLhs));
		}
	}

	if (pTinLhs->m_tink == TINK_Enum || pTinRhs->m_tink == TINK_Enum)
	{
		TypeInfo * pTinEnum = pTinLhs;
		TypeInfo * pTinOther = pTinRhs;
		if (pTinOther->m_tink == TINK_Enum)
		{
			pTinEnum = pTinRhs;
			pTinOther = pTinLhs;
		}

		bool fIsInteger = (pTinOther->m_tink == TINK_Numeric && FIsInteger(((TypeInfoNumeric*)pTinOther)->m_numk));
		if (parkOperator == PARK_AdditiveOp && fIsInteger)
		{
			return OpTypes(pTinEnum, pTinEnum, pTinEnum);
		}
		else if (parkOperator == PARK_ShiftOp && fIsInteger)
		{
			MOE_ASSERT(pTinEnum->m_tink == TINK_Enum && ((TypeInfoEnum*)pTinEnum)->m_pTinLoose, "expected loose type");
			return OpTypes(pTinLhs, pTinRhs, ((TypeInfoEnum*)pTinEnum)->m_pTinLoose);
		}
	}

	bool fIsRhsInteger = (pTinRhs->m_tink == TINK_Numeric && FIsInteger(((TypeInfoNumeric*)pTinRhs)->m_numk));
	if (pTinLhs->m_tink == TINK_Bool || fIsRhsInteger)
	{
		if (parkOperator == PARK_AssignmentOp)
		{
			return OpTypes(pTinLhs, pTinLhs, pTinLhs);
		}
	}
	if (pTinLhs->m_tink == TINK_Flag || pTinRhs->m_tink == TINK_Bool)
	{
		if (parkOperator == PARK_AssignmentOp)
		{
			return OpTypes(pTinLhs, pTinLhs, pTinLhs);
		}
	}
	return OpTypes();
}


void OnTypeResolve(TypeCheckContext * pTcctx, const Symbol * pSym)
{
	TypeInfo * pTinSym = PTinFromSymbol(pSym);
	MOE_ASSERT(pTinSym, "expected type for completed symbol");

#if MOEB_TCJOB
	// BB - could replace this with a function that 'pops' a key/value pair from the hash and save a second lookup.
	UnknownType * pUntype = pTcctx->m_hashPSymUntype.Lookup(pSym);
	if (!pUntype)
		return;

	int cTcframDependent = (s32)pUntype->m_arypTcframDependent.C();
	MOE_ASSERT(cTcframDependent > 0, "unknown type not cleaned up (empty dependent array)");

	for (int iTcfram = 0; iTcfram < cTcframDependent; ++iTcfram)
	{
		TypeCheckFrame * pTcfram = pUntype->m_arypTcframDependent[iTcfram];

		if (MOE_FVERIFY(pTcctx->m_arypTcframWaiting[pTcfram->m_ipTcframQueue] == pTcfram, "bookkeeping error (OnTypeResolve)"))
		{
			RelocateTcfram(pTcfram, &pTcctx->m_arypTcframWaiting, &pTcctx->m_arypTcframPending);
		}
	}
	pTcctx->m_hashPSymUntype.Remove(pSym);
#else

	auto pWork = pTcctx->m_pWork;
	JobPrereqSet * pJps = pWork->m_hashPSymJps.Lookup(pSym);
	if (!pJps)
		return;

	Job ** ppJobMax = pJps->m_arypJob.PMac();
	for (Job ** ppJobIt = pJps->m_arypJob.A(); ppJobIt != ppJobMax; ++ppJobIt)
	{
		EnqueueJob(pWork->m_pComp, *ppJobIt);
	}

	pWork->m_hashPSymJps.Remove(pSym);
#endif
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

GenericMap * PGenmapNew(TypeCheckContext * pTcctx, const char * pChzPrefix, TypeInfo * pTinOwner)
{
	auto pGenmap = MOE_NEW(pTcctx->m_pAlloc, GenericMap) GenericMap(pTcctx->m_pAlloc, pChzPrefix, pTinOwner);
	pTcctx->m_pWork->m_arypGenmapManaged.Append(pGenmap);
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
	TypeCheckContext * pTcctx,
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
	CDynAry<InString> mpIArgIstrName(pTcctx->m_pAlloc, BK_TypeCheckProcmatch, cArgDefinition);
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
			
			FindGenericAnchorNames(pTcctx->m_pAlloc, pStnodParamDecl, pGenmap);
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
						EmitError(pTcctx, lexsp, ERRID_NamedArgumentNotFound,
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
							EmitError(pTcctx, lexsp, ERRID_NamedArgumentNotFound,
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
				EmitError(pTcctx, lexsp, ERRID_OrderedAfterNamed,
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
				EmitError(pTcctx, lexsp, ERRID_TooManyArgs,
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
				EmitError(pTcctx, lexsp, ERRID_ArgumentSuppliedTwice,
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
						EmitError(pTcctx, pStnodParamDef->m_lexsp, ERRID_BakingNonLiteralValue,
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

						EmitError(pTcctx, pStnodInit->m_lexsp, ERRID_BakingNonLiteralValue,
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

					EmitError(pTcctx, pStnodInit->m_lexsp, ERRID_BakingNonLiteralValue,
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
					EmitError(pTcctx, lexsp, ERRID_TooFewArgs,
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

				TypeInfo * pTinInitDefault = PTinPromoteUntypedArgument(pTcctx, pSymtab, pStnodInit, pTinParam, errep);
				pTinInitDefault = PTinAfterRValueAssignment(pTcctx, pStnodInit->m_lexsp, pTinInitDefault, pSymtab, pTinParam);

				GRFGENCOMP grfgencomp = pArgunp->m_grfarg.FIsSet(FARG_ImplicitRef) ? FGENCOMP_ImplicitRef : FGENCOMP_None;
				ERRID errid = ErridComputeDefinedGenerics(pTcctx, lexsp, errep, grfgencomp, pSymtab, pTinInitDefault, pStnodType, pGenmap);
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
	TypeCheckContext * pTcctx,
	STNode * pStnodInstFrom,
	GenericMap * pGenmapSuperset, 
	const LexSpan & lexsp)
{
	GenericMap genmapTrim(pTcctx->m_pAlloc, "trim", nullptr);
	FindGenericAnchorNames(pTcctx->m_pAlloc, pStnodInstFrom, &genmapTrim);

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

	auto pGenmapNew = PGenmapNew(pTcctx, "", nullptr);
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

TypeInfo * PTinFindCanon(TypeCheckContext * pTcctx, TypeInfo * pTin, SymbolTable * pSymtab, ERREP errep)
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
				auto pGenmapNew = PGenmapNew(pTcctx, "Canon", pTinstructFrom);

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
								pTinNew = PTinSubstituteGenerics(pTcctx, pSymtab, lexspIt, pAncFrom->m_pTin, pGenmapIt, errep);
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
				pTinNew = PTinSubstituteGenerics(pTcctx, pSymtab, lexspIt, pTinstructFrom, pGenmapIt, errep);
			}

			auto pTinstructNew = PTinDerivedCast<TypeInfoStruct *>(pTinNew);

			pTinstructNew = PTinstructEnsureUniqueInstance(pTcctx, pTinstructNew);
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
			auto pTinCanon = PTinFindCanon(pTcctx, pTinqual->m_pTin, pSymtab, errep);
			if (pTinqual->m_pTin != pTinCanon)
			{
				pTinqual = pSymtab->PTinqualEnsure(pTinCanon, pTinqual->m_grfqualk);
			}
			return pTinqual;
		} break;
	case TINK_Array:
		{
			auto pTinary = (TypeInfoArray *)pTin;
			auto pTinCanon = PTinFindCanon(pTcctx, pTinary->m_pTin, pSymtab, errep);
			if (pTinary->m_pTin != pTinCanon)
			{
				pTinary = pSymtab->PTinaryCopyWithNewElementType(pTinary, pTinCanon);
			}
			return pTinary;
		} break;
	case TINK_Pointer:
		{
			auto pTinptr = (TypeInfoPointer *)pTin;
			auto pTinCanon = PTinFindCanon(pTcctx, pTinptr->m_pTin, pSymtab, errep);
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
	TypeCheckContext * pTcctx,
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
						EmitError(pTcctx, lexsp, ERRID_GenericLookupFail,
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
		    	auto pTinTarget = PTinSubstituteGenerics(pTcctx, pSymtab, lexsp, pTinptr->m_pTin, pGenmap, errep);
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
					pTinproc->m_arypTinParams[ipTinDst] = PTinSubstituteGenerics(pTcctx, pSymtab, lexsp, pTinproc->m_arypTinParams[ipTinSrc], pGenmap, errep);
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
					pTinproc->m_arypTinReturns[ipTin] = PTinSubstituteGenerics(pTcctx, pSymtab, lexsp, pTinproc->m_arypTinReturns[ipTin], pGenmap, errep);
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

				auto pGenmapTrim = PGenmapTrimUnusedAnchors(pTcctx, pTinstructUnsub->m_pStnodStruct, pGenmap, lexsp);

				auto pInsreq = PInsreqLookup(pTcctx, pTinstructUnsub->m_pStnodStruct, pGenmapTrim);
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

				auto pTinCanon = PTinFindCanon(pTcctx, pTinstructNew, pSymtab, errep);
				pTinstructNew = PTinDerivedCast<TypeInfoStruct *>(pTinCanon);
				pTinstructNew =  PTinstructEnsureUniqueInstance(pTcctx, pTinstructNew);

				pTinstructNew->m_grftingen = pGenmapTrim->m_grftingenResult;
		    	return pTinstructNew;
		    }
	    case TINK_Array:
		    {
		    	auto pTinaryUnsub = (TypeInfoArray *)pTinUnsub;

		    	auto pTinaryNew = pSymtab->PTinaryCopy(pTinaryUnsub);
		    	pTinaryNew->m_pTin = PTinSubstituteGenerics(pTcctx, pSymtab, lexsp, pTinaryUnsub->m_pTin, pGenmap, errep);

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
								pTinaryNew->m_c = NUnsignedLiteralCast(pTcctx, pStvalDim);
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
		    	auto pTinTarget = PTinSubstituteGenerics(pTcctx, pSymtab, lexsp, pTinqual->m_pTin, pGenmap, errep);
				return pSymtab->PTinqualWrap(pTinTarget, pTinqual->m_grfqualk);
		    }
		default:
			MOE_ASSERT(false, "unhandled type info.");
			break;
	}

	return nullptr;
}

InstantiateRequest * PInsreqInstantiateGenericStruct(
	TypeCheckContext * pTcctx,
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
	Moe::CHash<STNode *, STNode *> mpPStnodGenPStnodCopy(pTcctx->m_pAlloc, BK_TypeCheckGenerics);

	auto pStnodStructCopy = PStnodCopy(pTcctx->m_pAlloc, pStnodGeneric, &mpPStnodGenPStnodCopy);
	pStnodStructCopy->m_grfstnod.Clear(FSTNOD_NoCodeGeneration);

	MOE_ASSERT(pStnodGeneric->m_strees = STREES_TypeChecked, "generic struct definition should be type checked prior to instantiation");
	pStnodStructCopy->m_strees = STREES_Parsed;

	// copy the symbol table, but replace generic types from the map

	// BB - is this really the right way to get the proc's symtab? might have a param list, might not.
	SymbolTable * pSymtabSrc = pStnodGeneric->m_pSymtab;
	if (!MOE_FVERIFY(pSymtabSrc, "generic structure source has no symbol table"))
		return nullptr;

	SymbolTable * pSymtabNew = PSymtabNew(pTcctx->m_pAlloc, pSymtabSrc, pSymtabSrc->m_istrNamespace);	
	pSymtabNew->m_pSymtabParent = pSymtabSrc->m_pSymtabParent;

	GenericMapScope genscope(pTcctx->PErrman(), pGenmap);

	auto pInsreq = pTcctx->m_pWork->m_pGenreg->PInsreqNew(pStnodGeneric, pGenmap);
	pInsreq->m_pGenmap = pGenmap;
	pInsreq->m_pStnodGeneric = pStnodGeneric;

	// Figure out the generic args that will remain after this instantiation
	//   may not be fewer generic args ie. instantiating 'SPair(:$A, :$B)' to 'SPair( :CAry(:$U), :$V)'

	int cGenericValue = 0;
	int cGenericType = 0;

	STNode * pStnodNewParams = nullptr;
	int ipStnodFullyInstantated = -1;
	{
		GenericMap genmapNames(pTcctx->m_pAlloc, "findnames", nullptr);
		CDynAry<STNode *> aryStnodChild(pTcctx->m_pAlloc, BK_TypeCheckGenerics);

		int ipStnodMin = 1; // instantiation children are (name, arg0, arg1, arg2, ...)
		for (int ipStnodArg = ipStnodMin; ipStnodArg < pStnodInstantiation->CPStnodChild(); ++ipStnodArg)
		{
			auto pStnodInstArg = pStnodInstantiation->PStnodChild(ipStnodArg);
			size_t cAncPrev = genmapNames.m_mpIstrAnc.C();
			FindGenericAnchorNames(pTcctx->m_pAlloc, pStnodInstArg, &genmapNames);

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
					EmitError(pTcctx, pStnodInstantiation->m_lexsp, ERRID_GenericArgsMustBeFirst,
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
						auto pStnodCopy = PStnodCopy(pTcctx->m_pAlloc, pStnodInstArg);
						pStdecl = PStnodRtiCast<STDecl * >(pStnodCopy);
					} break;
				case PARK_TypeArgument:
					{
						if (!MOE_FVERIFY(pStnodInstArg->CPStnodChild() >= 0, "type argument's first child should be type AST"))
							break;

						pStdecl = PStdeclAllocAfterParse(pTcctx->m_pAlloc, PARK_Decl, pStnodInstArg->m_lexsp);

						auto pStnodTypeOld = pStnodInstArg->PStnodChild(0);
						auto pStnodTypeNew = PStnodCopy(pTcctx->m_pAlloc, pStnodTypeOld);

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
			pStnodNewParams = PStnodAllocAfterParse(pTcctx->m_pAlloc, PARK_ParameterList, pStnodInstantiation->m_lexsp);
			pStnodNewParams->CopyChildArray(pTcctx->m_pAlloc, aryStnodChild.A(), int(aryStnodChild.C()));
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
	Moe::CHash<Symbol *, STNode *> mpPSymSrcPStnodValue(pTcctx->m_pAlloc, BK_TypeCheckGenerics);
	Moe::CHash<Symbol *, Symbol *> mpPSymGenericPSymRemapped(pTcctx->m_pAlloc, BK_TypeCheckGenerics);
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
			pTcctx->PErrman(),
			pSymSrc->m_istrName,
			pSymSrc->m_pStnodDefinition,
			pSymSrc->m_grfsym);

		auto ppStnodCopy = mpPStnodGenPStnodCopy.Lookup(pSymNew->m_pStnodDefinition);
		if (!ppStnodCopy)
		{
			EmitError(pTcctx, pSymNew->m_pStnodDefinition->m_lexsp, ERRID_MissingSymbolDef,
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
								pTcctx,
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
									pTcctx->PErrman(),
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

		MOE_ASSERT(pTypemembUnsub->PTin() == nullptr, "expected pTin to be unresolved");
	}

	for (int iTypememb = 0; iTypememb < pTinstructNew->m_aryTypemembField.C(); ++iTypememb)
	{
		auto pTypememb = &pTinstructNew->m_aryTypemembField[iTypememb];

		auto ppStnodCopy = mpPStnodGenPStnodCopy.Lookup(pTypememb->m_pStdecl);
		if (!ppStnodCopy)
		{
			EmitError(pTcctx, pTypememb->m_pStdecl->m_lexsp, ERRID_MissingMemberDef,
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

	pTinstructNew = PTinstructEnsureUniqueInstance(pTcctx, pTinstructNew);
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
		pTcctx,
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

#if MOEB_TCJOB
	TypeCheckFrame * pTcfram = pTcctx->m_blistTcfram.AppendNew();
	pTcfram->m_ipTcframQueue = pTcctx->m_arypTcframPending.C();
	pTcctx->m_arypTcframPending.Append(pTcfram);

	WorkspaceEntry * pEntry = pTcctx->m_pblistEntry->AppendNew();
	pEntry->m_pStnod = pStnodStructCopy;
	pEntry->m_pSymtab = pSymtabNew;
	pTcfram->m_pEntry = pEntry;
#else

	Workspace * pWork = pTcctx->m_pWork;
	WorkspaceEntry * pEntry = pWork->PEntryAppend(pStnodStructCopy, pSymtabNew);

	(void)PJobCreateTypeCheckRequest(pWork->m_pComp, pWork, pEntry, pTcctx->m_pJobTc);
#endif

#if KEEP_TYPEINFO_DEBUG_STRING
	pTinstructNew->m_istrDebug = StrFromTypeInfo(pTinstructNew);
#endif

	if (!pTinstructNew->FHasGenericParams() || s_fTypecheckPartialGenericStructs)
	{
		pTcctx->m_aryTcsent.SetAlloc(pTcctx->m_pAlloc, Moe::BK_TypeCheckStack);
		TypeCheckStackEntry * pTcsent = pTcctx->m_aryTcsent.AppendNew();
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
	TypeCheckContext * pTcctx,
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

			if (!FVerifyIvalk(pTcctx, pStnodArg, IVALK_LValue))
			{
				EmitError(pTcctx, pStnodArg->m_lexsp, ERRID_NotLvalue,
					"Argument %d, must be an LValue for implicit conversion to pointer.",
					iStnodArg+1);
			}
			pTinParam = ((TypeInfoPointer*)pTinParam)->m_pTin;
		}

		pTinCall = PTinPromoteUntypedTightest(pTcctx, pSymtab, pStnodArg, pTinParam, errep);
		pTinCall = PTinAfterRValueAssignment(pTcctx, pStnodArg->m_lexsp, pTinCall, pSymtab, pTinParam);
	}

	TypeInfo * pTinCallDefault = PTinPromoteUntypedArgument(pTcctx, pSymtab, pStnodArg, pTinParam, errep);
	pTinCallDefault = PTinAfterRValueAssignment(pTcctx, pStnodArg->m_lexsp, pTinCallDefault, pSymtab, pTinParam);

	pMtin->m_pTinCall = pTinCall;
	pMtin->m_pTinCallDefault = pTinCallDefault;
	pMtin->m_pStnodArg = pStnodArg;
	pMtin->m_pStnodRawArg = pStnodRawArg;

	// we'll need to rebuild pTinParam once we know what all the generic types are
	pMtin->m_pTinParam = pTinParam;
	return true;
}

PROCMATCH ProcmatchCheckStructArguments(
	TypeCheckContext * pTcctx,
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
	CDynAry<ArgUnpack> mpIArgArgunp(pTcctx->m_pAlloc, BK_TypeCheckGenerics, cParam);
	mpIArgArgunp.AppendNew(cParam);

	GenericMap genmap(pTcctx->m_pAlloc, "CheckStructArg", pTinstruct);

	if (!FUnpackArgumentList(
		pTcctx,
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
		
	CDynAry<MatchTypeInfo> aryMtin(pTcctx->m_pAlloc, BK_TypeCheckProcmatch, pPmparam->m_cpStnodCall);


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
			pTcctx,
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
			pMtin->m_pTinParam = PTinSubstituteGenerics(pTcctx, pSymtab, pPmparam->m_lexsp, pMtin->m_pTinParam, &genmap, errep);
			if (!pMtin->m_pTinParam)
			{
				// errors should have been reported in PTinSubstituteGenerics
				return PROCMATCH_None;
			}

			if (pMtin->m_pStnodArg)
			{
				pMtin->m_pTinCall = PTinPromoteUntypedTightest(pTcctx, pSymtab, pMtin->m_pStnodArg, pMtin->m_pTinParam);
				pMtin->m_pTinCallDefault = PTinPromoteUntypedArgument(pTcctx, pSymtab, pMtin->m_pStnodArg, pMtin->m_pTinParam, errep);

				pMtin->m_pTinCall = PTinAfterRValueAssignment(pTcctx, pMtin->m_pStnodArg->m_lexsp, pMtin->m_pTinCall, pSymtab, pMtin->m_pTinParam);
				pMtin->m_pTinCallDefault = PTinAfterRValueAssignment(pTcctx, pMtin->m_pStnodArg->m_lexsp, pMtin->m_pTinCallDefault, pSymtab, pMtin->m_pTinParam);
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
			GenericMapScope genscope(pTcctx->PErrman(), (genmap.FIsEmpty()) ? nullptr : &genmap);

			InString istrTinCall = IstrFromTypeInfo(pMtin->m_pTinCall);
			InString istrTinParam = IstrFromTypeInfo(pMtin->m_pTinParam);
			EmitError(pTcctx, pStnodStruct->m_lexsp, ERRID_BadImplicitConversion,
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

	auto pGenmap = PGenmapNew(pTcctx, "CheckStructArgs", pTinstruct);
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
	TypeCheckContext * pTcctx,
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

	CDynAry<GenericFrame> aryGenfram(pTcctx->m_pAlloc, BK_TypeCheckGenerics, 16);

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
						EmitError(pTcctx, pStnodDockCur->m_lexsp, ERRID_CannotInferGeneric,
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
							EmitError(pTcctx, pStnodDockCur->m_lexsp, ERRID_CannotInferGeneric,
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

							auto pStvalLiteral = PStvalAllocAfterParse(pTcctx->m_pAlloc, PARK_Literal, pStnodDim->m_lexsp);

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

							CLexerLookup lexlook(pTcctx->m_pWork, pLexlocRef);

							EmitError(pTcctx, pStnodDockCur->m_lexsp, ERRID_CannotInferGeneric,
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
								EmitError(pTcctx, pStnodDockCur->m_lexsp, ERRID_CannotInferGeneric,
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
							EmitError(pTcctx, pStnodDockCur->m_lexsp, ERRID_CannotInferGeneric,
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

					LexLookup lexlookDoc(pTcctx->m_pWork, pStnodDock);
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
									ERRID errid = ErridComputeDefinedGenerics(pTcctx, lexspRef, errep, FGENCOMP_None,
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
							EmitError(pTcctx, pStnodDockCur->m_lexsp, ERRID_NoGenericRValue,
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
	TypeCheckContext * pTcctx,
	SymbolTable * pSymtab,
	TypeInfoProcedure * pTinproc,
	ProcMatchParam * pPmparam,
	ERREP errep,
	ARGORD argord,
	Symbol * pSymProc)
{
	size_t cArgCall = pPmparam->m_cpStnodCall;

	auto pStnodDefinition = pTinproc->m_pStnodDefinition;
	GenericMap genmap(pTcctx->m_pAlloc, "CheckProcArgs", pTinproc);

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
	CDynAry<ArgUnpack> mpIArgArgunp(pTcctx->m_pAlloc, BK_TypeCheckGenerics, cArgMax);
	mpIArgArgunp.AppendNew(cArgMax);

	if (!pTinproc->m_mpIptinGrfparmq.FIsEmpty() && pTinproc->m_mpIptinGrfparmq[0].FIsSet(FPARMQ_ImplicitRef)) 
	{
		mpIArgArgunp[0].m_grfarg.AddFlags(FARG_ImplicitRef);
	}

	if (!FUnpackArgumentList(
		pTcctx,
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
			EmitError(pTcctx, pPmparam->m_lexsp, ERRID_TooManyArgs,
				"Too many arguments to procedure '%s'. Expected %d but encountered %d",
				PChzProcName(pTinproc, pSymProc),
				pTinproc->m_arypTinParams.C(),
				cArgCall);
		}
		return PROCMATCH_None;
	}

	CDynAry<MatchTypeInfo> aryMtin(pTcctx->m_pAlloc, BK_TypeCheckProcmatch, cArgMax);

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
			pTcctx,
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
					EmitError(pTcctx, pPmparam->m_lexsp, ERRID_TooManyArgs,
						"procedure '%s' expected %d arguments but encountered %d",
						PChzProcName(pTinproc, pSymProc),
						pTinproc->m_arypTinParams.C(),
						cArgMax);
				}
				return PROCMATCH_None;
			}

			pMtin->m_pTinCall = pMtin->m_pTinCallDefault;
			pMtin->m_pTinParam = PTinPromoteVarArg(pTcctx, pSymtab, pMtin->m_pTinCall);
		}
	}

	if (pTinproc->FHasGenericArgs())
	{
		// given known generic type anchors and unsubstituted types, generate instantiated types

		int cMtin = (int)aryMtin.C();
		for (int iMtin = 0; iMtin < cMtin; ++iMtin)
		{
			auto pMtin = &aryMtin[iMtin];
			pMtin->m_pTinParam = PTinSubstituteGenerics(pTcctx, pSymtab, pPmparam->m_lexsp, pMtin->m_pTinParam, &genmap, errep);
			if (!pMtin->m_pTinParam)
			{
				// errors should have already been reported inside pTinSubstituteGenerics
				return PROCMATCH_None;
			}

			if (pMtin->m_pStnodArg)
			{
				pMtin->m_pTinCall = PTinPromoteUntypedTightest(pTcctx, pSymtab, pMtin->m_pStnodArg, pMtin->m_pTinParam);
				pMtin->m_pTinCallDefault = PTinPromoteUntypedArgument(pTcctx, pSymtab, pMtin->m_pStnodArg, pMtin->m_pTinParam, errep);

				pMtin->m_pTinCall = PTinAfterRValueAssignment(pTcctx, pMtin->m_pStnodArg->m_lexsp, pMtin->m_pTinCall, pSymtab, pMtin->m_pTinParam);
				pMtin->m_pTinCallDefault = PTinAfterRValueAssignment(pTcctx, pMtin->m_pStnodArg->m_lexsp, pMtin->m_pTinCallDefault, pSymtab, pMtin->m_pTinParam);
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
				GenericMapScope genscope(pTcctx->PErrman(), (genmap.FIsEmpty()) ? nullptr : &genmap);

				InString istrTinCall = IstrFromTypeInfo(pMtin->m_pTinCall);
				InString istrTinParam = IstrFromTypeInfo(pMtin->m_pTinParam);

				EmitError(pTcctx, pPmparam->m_lexsp, ERRID_BadImplicitConversion,
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
			auto pGenmap = PGenmapNew(pTcctx, "", nullptr);
			pGenmap->m_aryLexspSrc.Append(pPmparam->m_lexsp);
			pGenmap->Swap(&genmap);
			pPmfit->m_pGenmap = pGenmap;
		}
	}

	return procmatch;
}

TCRET TcretTryFindMatchingProcedureCall(
	TypeCheckContext * pTcctx, 
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
			EmitError(pTcctx, pPmparam->m_lexsp, ERRID_CantFindProc,
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
								pTcctx,
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
				auto procmatch = ProcmatchCheckProcArguments(pTcctx, pSymtab, pTinprocSym, &pmparamTry, ERREP_HideErrors, (ARGORD)argord, pSymIt);
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
				EmitError(pTcctx, pPmparam->m_lexsp, ERRID_CantFindProc, "'%s' does not evaluate to a procedure.", istrProcName.PChz());
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
							(void)ProcmatchCheckProcArguments(pTcctx, pSymtab, pTinproc, pPmparam, ERREP_ReportErrors, ARGORD_Normal, pSymProc);
						}break;
					case TINK_Struct:
						{
							auto pTinstruct = (TypeInfoStruct *)pTinSym;

							(void)ProcmatchCheckStructArguments(
								pTcctx,
								pSymtab,
								pTinstruct,
								pPmparam,
								ERREP_ReportErrors);
						}break;
					default:
						MOE_ASSERT(false, "expected type info kind %s", PChzFromTink(pTinSym->m_tink));
					}
				}

				if (!pTcctx->PErrman()->FHasErrors() && !pTcctx->PErrman()->FHasHiddenErrors())
				{
					EmitError(pTcctx, pPmparam->m_lexsp, ERRID_UnknownError,
						"error type matching symbol '%s' (one option, unknown match error)", istrProcName.PChz());
				}
			}
		}
		else if (pPmparam->m_fMustFindMatch)
		{
			Error error(pTcctx->PErrman());
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
		TypeCheckStackEntry * pTcsentTop = pTcctx->m_aryTcsent.PLast();

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

		Error error(pTcctx->PErrman(), ERRID_AmbiguousOverload);
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



struct OverloadCheck // tag ovcheck
{
			OverloadCheck(TypeInfoProcedure * pTinproc, TCRET tcret = TCRET_StoppingError, ARGORD argord = ARGORD_Normal)
			:m_pTinproc(pTinproc)
			,m_tcret(tcret)
			,m_argord(argord)
				{ ; }

	TypeInfoProcedure *		m_pTinproc;
	TCRET					m_tcret;
	ARGORD					m_argord;
};


OverloadCheck OvcheckTryCheckOverload(
	TypeCheckContext * pTcctx,
	STNode * pStnod,
	ProcMatchParam * pPmparam)
{
	CDynAry<TypeCheckStackEntry> * paryTcsent = &pTcctx->m_aryTcsent;
	TypeCheckStackEntry * pTcsentTop = paryTcsent->PLast();

	SymbolTable * pSymtab = pTcsentTop->m_pSymtab;
	Moe::InString istrOverload = IstrOverloadNameFromTok(pStnod->m_tok);
	if (istrOverload.FIsEmpty())
		return OverloadCheck(nullptr);

	InString strProcName(istrOverload);
	Symbol * pSymProc = nullptr;
	ARGORD argord = ARGORD_Normal;
	TCRET tcret = TcretTryFindMatchingProcedureCall(
					pTcctx, strProcName, pSymtab, pPmparam, &pSymProc, &argord, pTcsentTop->m_grfsymlook);

	auto pTinproc = (pSymProc) ? PTinRtiCast<TypeInfoProcedure*>(PTinFromSymbol(pSymProc)) : nullptr;

	if (pTinproc && tcret == TCRET_WaitingForSymbolDefinition)
	{
		// wait for this procedure's signature to be type checked.

		SetupSymbolWait(pTcctx, pSymProc);
		return OverloadCheck(pTinproc, tcret, argord);
	}
	else if (tcret != TCRET_Complete)
	{
		return OverloadCheck(nullptr);
	}

	if (pTinproc)
	{
		STNode * pStnodDefinition = pSymProc->m_pStnodDefinition;

		MOE_ASSERT(pStnodDefinition->m_strees >= STREES_SignatureTypeChecked, "expected definition to be type checked");
		MOE_ASSERT(pStnodDefinition->m_pTin == pTinproc, "tin mismatch");
	}

	MOE_ASSERT(pStnod->m_pTin == nullptr, "assignment op has no 'return' value");

	return OverloadCheck(pTinproc, TCRET_Complete, argord);
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

SymbolTable * PSymtabFromType(TypeCheckContext * pTcctx, TypeInfo * pTin, const LexSpan & lexsp)
{
	switch (pTin->m_tink)
	{
	case TINK_Pointer:
		{
			auto pTinptr = (TypeInfoPointer *)pTin;
			return PSymtabFromType(pTcctx, pTinptr->m_pTin, lexsp);
		}
	case TINK_Qualifier:
		{
			auto pTinqual = (TypeInfoQualifier *)pTin;
			return PSymtabFromType(pTcctx, pTinqual->m_pTin, lexsp);
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
		EmitError(pTcctx, lexsp, ERRID_UsingStatementBadType,
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

TypeInfo * PTinFromBint(
	TypeCheckContext *pTcctx,
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
	TypeCheckContext * pTcctx,
	SymbolTable * pSymtab,
	const TypeInfoLiteral * pTinlit)
{
	MOE_ASSERT(pTinlit->m_fIsFinalized, "Expected finalized literal type");

	const LiteralType & litty = pTinlit->m_litty;
	switch (litty.m_litk)
	{
	case LITK_Numeric:
		{
			switch(litty.m_numk)
			{
			case NUMK_Float:
				{
					switch (litty.m_cBit)
					{
					case 32:	return pSymtab->PTinBuiltin(BuiltIn::g_istrF32);
					case 64:	return pSymtab->PTinBuiltin(BuiltIn::g_istrF64);
					}
				} break;
			case NUMK_UnsignedInt:
				{
					switch (litty.m_cBit)
					{
					case 8:		return pSymtab->PTinBuiltin(BuiltIn::g_istrU8);
					case 16:	return pSymtab->PTinBuiltin(BuiltIn::g_istrU16);
					case 32:	return pSymtab->PTinBuiltin(BuiltIn::g_istrU32);
					case 64:	return pSymtab->PTinBuiltin(BuiltIn::g_istrU64);
					}
				} break;
			case NUMK_SignedInt:
				{
					switch (litty.m_cBit)
					{
					case 8:		return pSymtab->PTinBuiltin(BuiltIn::g_istrS8);
					case 16:	return pSymtab->PTinBuiltin(BuiltIn::g_istrS16);
					case 32:	return pSymtab->PTinBuiltin(BuiltIn::g_istrS32);
					case 64:	return pSymtab->PTinBuiltin(BuiltIn::g_istrS64);
					}
				} break;
			default:
				MOE_ASSERT(false, "unknown numk");
				break;
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
	TypeCheckContext * pTcctx,
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
					pTinElement = PTinPromoteUntypedDefault(pTcctx, pSymtab, pStnodInit->PStnodChild(0));
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
							EmitError(pTcctx, pStnodLit->m_lexsp, ERRID_InvalidCast,
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

						(void) PTinPromoteUntypedCommon(pTcctx, pSymtab, &fWasHandled, pStnodIt, pTinary->m_pTin, errep);
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

						auto pTinIt = PTinPromoteUntypedCommon(pTcctx, pSymtab, &fWasHandled, pStnodValue, pTypememb->PTin(), errep);
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
		auto pTinFinalized = PTinFromLiteralFinalized(pTcctx, pSymtab, pTinlit);
		return pSymtab->PTinqualWrap(pTinFinalized, FQUALK_Const);
	}

	const STValue * pStval = PStnodRtiCast<STValue *>(pStnodLit);
	if (!MOE_FVERIFY(pStval, "literal without value"))
		return nullptr;

	*pFWasHandled = false;
	return nullptr;
}

TypeInfo * PTinPromoteUntypedDefault(
	TypeCheckContext * pTcctx,
	SymbolTable * pSymtab,
	STNode * pStnodLit,
	TypeInfo * pTinExpected,
	ERREP errep)
{
	bool fWasHandled;
	TypeInfo * pTinReturn = PTinPromoteUntypedCommon(pTcctx, pSymtab, &fWasHandled, pStnodLit, pTinExpected, errep);
	if (fWasHandled)
		return pTinReturn;

	TypeInfoLiteral * pTinlit = (TypeInfoLiteral *)pStnodLit->m_pTin;
	const LiteralType & litty = pTinlit->m_litty;

	switch (litty.m_litk)
	{
	case LITK_Numeric:
		{
			if (litty.m_numk == NUMK_Float)
			{
				return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrFloat);
			}

			bool fIsSigned = FIsSigned(litty.m_numk);
			if (fIsSigned == false)
			{
				const STValue * pStval = PStnodRtiCast<STValue *>(pStnodLit);
				s64 nUnsigned = NUnsignedLiteralCast(pTcctx, pStval);
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
	TypeCheckContext * pTcctx,
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

	return PTinPromoteUntypedDefault(pTcctx, pSymtab, pStnodLit, pTinArgument, errep);
}


inline TypeInfo * PTinPromoteUntypedTightest(
	TypeCheckContext * pTcctx,
	SymbolTable * pSymtab,
	STNode * pStnodLit,	
	TypeInfo * pTinDst,
	ERREP errep)
{
	pTinDst = PTinStripQualifiers(pTinDst); 

	bool fWasHandled;
	TypeInfo * pTinReturn = PTinPromoteUntypedCommon(pTcctx, pSymtab, &fWasHandled, pStnodLit, pTinDst, errep);
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
			if ((litty.m_numk == NUMK_Float) ||
				(pTinnDst && pTinnDst->m_numk == NUMK_Float))
			{
				// integer literals can be used to initialize floating point numbers
				return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrF32);
			}

			const STValue * pStval = PStnodRtiCast<STValue *>(pStnodLit);
			bool fDestIsSigned = pTinnDst && pTinnDst->m_numk == NUMK_SignedInt;
			bool fIsValNegative = pStval->m_stvalk == STVALK_SignedInt && pStval->m_nSigned < 0;

			if (fDestIsSigned == false && fIsValNegative == false)
			{
				s64 nUnsigned = NUnsignedLiteralCast(pTcctx, pStval);
				if (nUnsigned <= UCHAR_MAX)	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrU8);
				if (nUnsigned <= USHRT_MAX)	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrU16);
				if (nUnsigned <= UINT_MAX)	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrU32);
				return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrU64);
			}

			s64 nSigned = NSignedLiteralCast(pTcctx, pStval);
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
			bool fDestIsSigned = pTinDst->m_tink == TINK_Numeric && FIsSigned(((TypeInfoNumeric*)pTinDst)->m_numk);
			if (fDestIsSigned)
			{
				s64 nSigned = NSignedLiteralCast(pTcctx, pStval);
				if ((nSigned <= SCHAR_MAX) & (nSigned > SCHAR_MIN))	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrS8);
				if ((nSigned <= SHRT_MAX) & (nSigned > SHRT_MIN))	return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrS16);
				return pSymtab->PTinqualBuiltinConst(BuiltIn::g_istrS32);
			}

			s64 nUnsigned = NUnsignedLiteralCast(pTcctx, pStval);
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
				EmitError(pTcctx, pStnodLit->m_lexsp, ERRID_CannotConvertToNull, 
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

inline TypeInfo * PTinPromoteUntypedRvalueTightest(
	TypeCheckContext * pTcctx,
	SymbolTable * pSymtab,
	STNode * pStnodLit,	
	TypeInfo * pTinDst)
{
	// PromoteUntypedTightest may add a constant qualifier that  PTinAfterRValueAssignment will strip
	//  it would be nice to optimize that out 
	auto pTinPromoted = PTinPromoteUntypedTightest(pTcctx, pSymtab, pStnodLit, pTinDst);

	return PTinAfterRValueAssignment(pTcctx, pStnodLit->m_lexsp, pTinPromoted, pSymtab, pTinDst);
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

TypeInfo * PTinAfterRValueAssignment(TypeCheckContext * pTcctx, const LexSpan & lexsp, TypeInfo * pTin, SymbolTable * pSymtab, TypeInfo * pTinDst)
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
			EmitError(pTcctx, lexsp, ERRID_NoGenericRValue,
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

				bool fIsSrcFloat = pTinnSrc->m_numk == NUMK_Float;
				bool fIsDstFloat = pTinnDst->m_numk == NUMK_Float;
				if (fIsSrcFloat != fIsDstFloat)
					return false;

				if (fIsSrcFloat)
				{
					return pTinnDst->m_cBit >= pTinnSrc->m_cBit;
				}

				bool fIsSrcSigned = FIsSigned(pTinnSrc->m_numk);
				bool fIsDstSigned = FIsSigned(pTinnDst->m_numk);
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
		return FIsInteger(((TypeInfoNumeric*)pTinDst)->m_numk);
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

inline bool FCanCastForInit(TypeCheckContext * pTcctx, const LexSpan & lexsp, SymbolTable * pSymtab, TypeInfo * pTinSrc, TypeInfo * pTinDst)
{
	// Only require a const destination if the source is const, otherwise we'll strip it because it's ok to
	//  initialize a const value (just not to assign to it)

	auto pTinSrcAdj = PTinAfterRValueAssignment(pTcctx, lexsp, pTinSrc, pSymtab, pTinDst);
	auto pTinDstAdj = PTinAfterRValueAssignment(pTcctx, lexsp, pTinDst, pSymtab, pTinDst);

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

bool FIsType(STNode * pStnod)
{
	if (pStnod->m_pTin && pStnod->m_pTin->m_tink == TINK_Literal)
		return false;

	auto pSym = pStnod->PSym();
	if (!pSym)
	{
		// BB - The only current exception to his is spoofed array members
		MOE_ASSERT(pStnod->m_park != PARK_Identifier, "Expected identifiers to have symbol");
		return false;
	}

	return pSym->m_grfsym.FIsSet(FSYM_IsType);
}

void FinalizeCompoundLiteralType(
	TypeCheckContext * pTcctx,
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
		auto pStnodDefCopy = PStnodCopy(pTcctx->m_pAlloc, pStnodDef);
		pTinlitNew->m_pStnodDefinition = pStnodDefCopy;

		// just tack the new value copy on the end of the literal so it gets cleaned up (yuck)
		pStnodDef->AppendChildToArray(pTcctx->m_pAlloc, pStnodDefCopy);
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
				EmitError(pTcctx, pStnodDef->m_lexsp, ERRID_InitTypeMismatch,
					"too few elements in array literal definition '%s'",
					IstrFromTypeInfo(pTinlit).m_pChz);
				break;
			}

			for (int iStnod = 0; iStnod < pStnodList->CPStnodChild(); ++iStnod)
			{
				auto pStnodIt = pStnodList->PStnodChild(iStnod);
				if (pStnodIt && pStnodIt->m_park == PARK_ArgumentLabel)
				{
					EmitError(pTcctx, pStnodLit->m_lexsp, ERRID_InvalidLabel,
						"labeled member not allowed in array literal"); 
					pStnodIt = pStnodIt->PStnodChildSafe(1);
				}

				TypeInfo * pTinInit = pStnodIt->m_pTin;
				if (!pTinInit || pTinInit->m_tink != TINK_Literal)
				{
					EmitError(pTcctx, pStnodIt->m_lexsp, ERRID_NonConstantInLiteral,
						"array element %d cannot be initialized with a non-literal type '%s'",
						iStnod,
						IstrFromTypeInfo(pTinInit).m_pChz);
					continue;
				}

				auto pTinElement = PTinPromoteUntypedTightest(pTcctx, pSymtab, pStnodIt, pTinary->m_pTin);

				if (FCanCastForInit(pTcctx, pStnodIt->m_lexsp, pSymtab, pTinElement, pTinary->m_pTin))
				{
					FinalizeLiteralType(pTcctx, pSymtab, pTinary->m_pTin, pStnodIt);
				}
				else
				{
					EmitError(pTcctx, pStnodIt->m_lexsp, ERRID_InitTypeMismatch,
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
	        CDynAry<STNode *> arypStnodInit(pTcctx->m_pAlloc, BK_CodeGen, cTypememb);
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
						EmitError(pTcctx, pStnodLit->m_lexsp, ERRID_LiteralMemberNotFound,
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
						EmitError(pTcctx, pStnodLit->m_lexsp, ERRID_LiteralUnnamedMember, 
							"Unnamed expression encountered after named expression %d", iStnodNamed + 1);
						return;
					}

					if (iStnod > pTinstruct->m_aryTypemembField.C())
					{
						EmitError(pTcctx, pStnodLit->m_lexsp, ERRID_TooManyInitializers,
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
						EmitError(pTcctx, pStnodIt->m_lexsp, ERRID_NonConstantInLiteral,
							"struct member '%s' cannot be initialize with a non-literal type '%s'",
							pTypememb->m_istrName.m_pChz,
							IstrFromTypeInfo(pTinInit).m_pChz);
						continue;
					}

					pTinInit = PTinPromoteUntypedTightest(pTcctx, pSymtab, pStnodIt, pTypememb->PTin());

					// the top level literal is implicitly const so it's members are too

					auto pTinqualMember = pSymtab->PTinqualWrap(pTypememb->PTin(), FQUALK_Const);
					if (FCanCastForInit(pTcctx, pStnodIt->m_lexsp, pSymtab, pTinInit, pTinqualMember))
					{
						FinalizeLiteralType(pTcctx, pSymtab, pTypememb->PTin(), pStnodIt);
					}
					else
					{
						EmitError(pTcctx, pStnodIt->m_lexsp, ERRID_InitTypeMismatch,
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

void FinalizeLiteralType(TypeCheckContext * pTcctx, SymbolTable * pSymtab, TypeInfo * pTinDst, STNode * pStnodLit)
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
			auto pTinn = (TypeInfoNumeric *)pTinDst;
			pStnodLit->m_pTin = pSymtab->PTinlitFromLitk(LITK_Numeric, pTinn->m_cBit, pTinn->m_numk);
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
					pTinlit->m_litty.m_numk = NUMK_Nil;
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
					pTinlit->m_litty.m_numk = NUMK_Nil;
					pTinlit->m_fIsFinalized = true;
					pTinlit->m_pTinSource = (TypeInfoPointer*)pTinDst;
				} break;
			case LITK_Numeric:	
				{
					const STValue * pStval = PStnodRtiCast<STValue *>(pStnodLit);
					NUMK numk = (pStval->m_stvalk == STVALK_SignedInt) ? NUMK_SignedInt : NUMK_UnsignedInt;
					pTinlit = pSymtab->PTinlitFromLitk(LITK_Numeric, 64, numk);
				} break;
			case LITK_Compound:
				{
					auto pTinPromoted = PTinPromoteUntypedDefault(pTcctx, pSymtab, pStnodLit, pTinDst);

					// strip const here, as it is under a literal, so it's implicitly const

					auto pTinDst = PTinStripQualifiers(pTinPromoted);
					FinalizeCompoundLiteralType(pTcctx, pSymtab, pTinlitPrev, pTinDst, pStnodLit);
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
				pStnodLit->m_pTin = pSymtab->PTinlitFromLitk(LITK_Numeric, pTinn->m_cBit, pTinn->m_numk);
			}
		} break;
	case TINK_Array:
	case TINK_Struct:
		{
			TypeInfoLiteral * pTinlitPrev = PTinDerivedCast<TypeInfoLiteral *>(pStnodLit->m_pTin);
			if (pTinlitPrev && pTinlitPrev->m_litty.m_litk == LITK_Compound)
			{
				pTinlitPrev->m_pTinSource = pTinDst;
				FinalizeCompoundLiteralType(pTcctx, pSymtab, pTinlitPrev, pTinDst, pStnodLit);
			}
			else
			{
				const char * pChzExpect = (pTinDst->m_tink == TINK_Array) ? "array literal" : "struct literal";

				EmitError(pTcctx, pStnodLit->m_lexsp, ERRID_NonConstantInLiteral, 
					"Expected %s in literal but encountered %s",
					pChzExpect,
					IstrFromTypeInfo(pStnodLit->m_pTin).m_pChz);
			}
		} break;
	case TINK_Anchor:
		{
			(void) PTinPromoteUntypedDefault(pTcctx, pSymtab, pStnodLit);
		}break;
	case TINK_Null: // fall through
	case TINK_Void: // fall through
	default:
		MOE_ASSERT(false, "unexpected type");
	}
}

TypeInfo * PTinPromoteVarArg(TypeCheckContext * pTcctx, SymbolTable * pSymtab, TypeInfo * pTinIn)
{
	// C99 requires that all floats are promoted to double and all integers < 32 bit are promoted to 32 bit.

	switch (pTinIn->m_tink)
	{
	case TINK_Numeric:
		{
			TypeInfoNumeric * pTinn = (TypeInfoNumeric*)pTinIn;
			if (pTinn->m_numk == NUMK_Float)
			{
				if (pTinn->m_cBit < 64)
				{
					return pSymtab->PTinBuiltin(BuiltIn::g_istrF64);
				}
			}
			else if (pTinn->m_cBit < 32)
			{
				return pSymtab->PTinBuiltin((FIsSigned(pTinn->m_numk)) ? BuiltIn::g_istrS32 : BuiltIn::g_istrU32);
			}
			return pTinIn;
		}
	case TINK_Enum:
		{
			auto pTinenum = (TypeInfoEnum *)pTinIn;
			return PTinPromoteVarArg(pTcctx, pSymtab, pTinenum->m_pTinLoose);
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
	TypeCheckContext * pTcctx,
	SymbolTable * pSymtabRoot,
	STNode * pStnod,
	GRFSYMLOOK grfsymlook,
	Symbol **ppSymType,
	bool * pFIsValidTypeSpec)
{
	CDynAry<TinSpecEntry> aryTinse(pTcctx->m_pAlloc, BK_TypeCheck);

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
					EmitError(pTcctx, pStnod->m_lexsp, ERRID_UnknownIdentifier, 
						"'%s' unknown identifier detected in type specificattion", istrIdent.m_pChz);
					*pFIsValidTypeSpec = false;
				}
				else
				{
					pSym = PSymLast(pSymbase);
				}

				if (!pSym || !pSym->m_grfsym.FIsSet(FSYM_IsType))
				{
					EmitError(pTcctx, pStnod->m_lexsp, ERRID_TypeSpecifierExpected,
						"Expected type specification but encountered '%s'", istrIdent.m_pChz);
					return nullptr;
				}

				auto pTinstruct = PTinRtiCast<TypeInfoStruct *>(PTinFromSymbol(pSym));
				if (pTinstruct && pTinstruct->FHasGenericParams())
				{
					EmitError(pTcctx, pStnod->m_lexsp, ERRID_GenericArgsExpected,
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
					auto pSymtabTin = PSymtabFromType(pTcctx, pTinse->m_pTin, pStnod->m_lexsp);

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
							EmitError(pTcctx, pStnod->m_lexsp, ERRID_FeatureNotImplemented,
								"Only static sized arrays are currently supported");
							*pFIsValidTypeSpec = false;
						}
						else
						{
							auto pTinnBaked = PTinRtiCast<TypeInfoNumeric*>(PTinBakedConstantType(pStnodDim));
							if (pTinnBaked && FIsInteger(pTinnBaked->m_numk))
							{
								pTinary->m_pStnodBakedDim = pStnodDim;
							}
							else
							{
								Symbol * pSymDim = pStnodDim->PSym();
								TypeInfo * pTinCount = pTinse->m_pSymtab->PTinBuiltin(BuiltIn::g_istrInt);
								TypeInfo * pTinPromoted = PTinPromoteUntypedTightest(pTcctx, pTinse->m_pSymtab, pStnodDim, pTinCount);
								pTinPromoted = PTinAfterRValueAssignment(pTcctx, pStnodDim->m_lexsp, pTinPromoted, pTinse->m_pSymtab, pTinCount);

								if (!FCanImplicitCast(pTinPromoted, pTinCount))
								{

									EmitError(pTcctx, pStnod->m_lexsp, ERRID_BadArrayIndex, "static integer array size expected");
									*pFIsValidTypeSpec = false;
								}
								else
								{
									FinalizeLiteralType(pTcctx, pTinse->m_pSymtab, pTinCount, pStnodDim);
									pStvalDim = PStnodRtiCast<STValue *>(pStnodDim);
								}

								if (pStvalDim)
								{
									cTinary = NUnsignedLiteralCast(pTcctx, pStvalDim);
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
						EmitError(pTcctx, pStnod->m_lexsp, ERRID_NotYetSupported, "Dynamic arrays are not yet supported");
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
					GRFQUALK grfqualk;
					if (auto pStval = PStnodDerivedCast<STValue *>(pStnod))
					{
						QUALK qualk = QualkFromRword(pStval->m_istrRword);
						grfqualk = 0x1 << qualk;
					}
					
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

PARK ParkDefinition(TypeCheckContext * pTcctx, const Symbol * pSym)
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
			EmitError(pTcctx, pSym->m_pStnodDefinition->m_lexsp, ERRID_UnexpectedSymbolDef,
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

TCRET TcretWaitForTypeSymbol(TypeCheckContext * pTcctx, Symbol * pSymType, STNode * pStnodType)
{
	if (!pSymType)
	{
		InString istrTypename = IstrTypenameFromTypeSpecification(pStnodType);
		EmitError(pTcctx, pStnodType->m_lexsp, ERRID_UnknownSymbol, "'%s' unknown symbol detected", istrTypename.PChz());
		return TCRET_StoppingError;
	}

	if (!pSymType->m_grfsym.FIsSet(FSYM_IsType))
	{
		InString istrName = IstrFullyQualifiedSymbol(pSymType);
		EmitError(pTcctx, pStnodType->m_lexsp, ERRID_TypeExpected, "%s symbol refers to instance, but was expecting type", istrName.PChz());
		return TCRET_StoppingError;
	}
	else
	{
		// wait for this type to be resolved.
		SetupSymbolWait(pTcctx, pSymType);
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

TCRET TcretCheckProcedureDef(STNode * pStnod, TypeCheckContext * pTcctx, TypeCheckStackEntry * pTcsentTop)
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

				auto pTcsentPushed = PTcsentPush(pTcctx, &pTcsentTop, pStnodParamList);
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
				auto pTcsentPushed = PTcsentPush(pTcctx, &pTcsentTop, pStnodReturn);

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
												pTcctx,
												pStnodReturn->m_pSymtab,
												pStnodReturn,
												pTcsentTop->m_grfsymlook,
												nullptr, 
												&fIsValidTypeSpec);
					if (!fIsValidTypeSpec)
						return TCRET_StoppingError;

					if (!pTinReturn)
					{
						EmitError(pTcctx, pStnod->m_lexsp, ERRID_ReturnTypeExpected, "failed to parse return type");
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
					auto errid = ErridCheckOverloadSignature(pStnodIdent->m_tok, pTinproc, pTcctx->PErrman(), pStnod->m_lexsp);
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
							pTcctx, pStnod->m_lexsp, ERRID_InvalidArgument,
							"Argument %d is not a valid argument type. '%s' does not define an assignment operator", 
							iStnod + 1,
							istrType.m_pChz);
					}
				}

				if (pTcctx->PErrman()->FHasErrors())
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

				OnTypeResolve(pTcctx, pSymProc);

				for (int ipTin = 0; ipTin < pTinproc->m_arypTinParams.C(); ++ipTin)
				{
					MOE_ASSERT(pTinproc->m_arypTinParams[ipTin], "null parameter type? arg %d", ipTin);
				}

				pStnod->m_grfstnod.AddFlags(FSTNOD_NoCodeGeneration);
				PopTcsent(pTcctx, &pTcsentTop, pStnod);
				return TCRET_Complete;
			}

			// push the body subtree
			if (pStproc->m_pStnodBody)
			{
				STNode * pStnodBody = pStproc->m_pStnodBody;
				auto pTcsentPushed = PTcsentPush(pTcctx, &pTcsentTop, pStnodBody);

				if (pTcsentPushed)
				{
					pTcsentPushed->m_pStnodProcedure = pStnod;
					pTcsentPushed->m_pSymtab = pStnodBody->m_pSymtab;
				}
			}
		}break;
	case 3:
		{
			pTinproc->m_istrMangled = IstrComputeMangled(pTcctx, pStnod, pTcsentTop->m_pSymtab);
			PopTcsent(pTcctx, &pTcsentTop, pStnod);

			Symbol * pSymProc = pStnod->PSym();
			if (pStproc->m_grfstproc.FIsSet(FSTPROC_PublicLinkage))
			{
				pSymProc->m_symdep = SYMDEP_PublicLinkage;
			}

			pStnod->m_strees = STREES_TypeChecked;
			OnTypeResolve(pTcctx, pSymProc);
		}break;
	}
	return TCRET_Continue;
} 

TCRET TcretCheckStructDef(STNode * pStnod, TypeCheckContext * pTcctx, TypeCheckStackEntry * pTcsentTop)
{
	auto pTinstruct = PTinDerivedCast<TypeInfoStruct *>(pStnod->m_pTin);
	if (!MOE_FVERIFY(pTinstruct, "missing struct type info"))
		return TCRET_StoppingError;

	auto pStstruct = PStnodRtiCast<STStruct *>(pStnod);
	if (!MOE_FVERIFY(pStstruct, "expected STStruct"))
		return TCRET_StoppingError;

	if (pStnod->PStnodChild(pTcsentTop->m_nState) == pStstruct->m_pStnodIdentifier)
		++pTcsentTop->m_nState;

	// Don't try to typecheck the structure members if we haven't replaced our generic params yet.
	// NOTE: we type check the structure parameter decls, but we can't typecheck the members because we 
	//   haven't substituted all of our generic constants yet.
	if (pTinstruct->FHasGenericParams() && pStnod->PStnodChild(pTcsentTop->m_nState) == pStstruct->m_pStnodDeclList)
	{
		++pTcsentTop->m_nState;
	}

	if (pTcsentTop->m_nState < pStnod->CPStnodChild())
	{
		pStnod->m_strees = STREES_SignatureTypeChecked;

		auto pSym = pStnod->PSym();
		MOE_ASSERT(pSym, "expected structure symbol");
		pTcsentTop->m_pSymContext = pSym;

		auto pTcsentPushed = PTcsentPush(pTcctx, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
		if (pTcsentPushed)
		{
			pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;
		}

		++pTcsentTop->m_nState;
		return TCRET_Continue;
	}

	if (pStstruct->m_pStnodParameterList)
	{
		pStnod->m_grfstnod.AddFlags(FSTNOD_NoCodeGeneration);
		auto pStnodParamList = pStstruct->m_pStnodParameterList;
	}

	auto pTypemembMax = pTinstruct->m_aryTypemembField.PMac();
	for (auto pTypememb = pTinstruct->m_aryTypemembField.A(); pTypememb != pTypemembMax; ++pTypememb)
	{
		// BB - why is this cached here? shouldn't we just dig into the STNode's type?
		//pTypememb->m_pTin = pTypememb->m_pStdecl->m_pTin;
	}

	Symbol * pSymStruct = pStnod->PSym();
	if (!MOE_FVERIFY(pSymStruct, "struct symbol should be created during parse"))
		return TCRET_StoppingError;
	if (!MOE_FVERIFY(PTinFromSymbol(pSymStruct), "expected structure type info to be created during parse"))
		return TCRET_StoppingError;

	pStnod->m_strees = STREES_TypeChecked;

	PopTcsent(pTcctx, &pTcsentTop, pStnod);
	OnTypeResolve(pTcctx, pSymStruct);
	return TCRET_Continue;
}

TcretDebug TcretCheckDecl(STNode * pStnod, TypeCheckContext * pTcctx, TypeCheckStackEntry * pTcsentTop)
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
			auto pTcsentPushed = PTcsentPush(pTcctx, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
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
										pTcctx,
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
										pTcctx,
										pSymtab,
										pStnodType,
										pTcsentTop->m_grfsymlook,
										&pSymType,
										&fIsValidTypeSpec);

				return TcretWaitForTypeSymbol(pTcctx, pSymType, pStnodType);
			}
		}

		STNode * pStnodInit = pStdecl->m_pStnodInit;
		if (pStnodInit)
		{
			if (!FVerifyIvalk(pTcctx, pStnodInit, IVALK_RValue))
			{
				// it's unclear that this is the right thing to do, the AST should be immutable here, but we want to
				//  recover from this error, maybe mark this node as errored and handle it downstream?
				//pStnodInit->m_park = PARK_Uninitializer;
				pStnodInit->m_grfstnod.AddFlags(FSTNOD_HasTypeCheckError);
				pStnodInit->m_pTin = pStnod->m_pTin;
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
						EmitError(pTcctx, pStnod->m_lexsp, ERRID_InitTypeMismatch,
							"'%s' is already declared. \n"
							"Type inference is not allowed on overloaded variables. Did you type ':=' but meant '='?",
							istrIdent.m_pChz);
					}
					else
					{
						LexLookup lexlook(pTcctx->m_pWork, pSymPrior->m_pStnodDefinition);

						EmitError(pTcctx, pStnod->m_lexsp, ERRID_InitTypeMismatch,
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
					EmitError(pTcctx, pStnod->m_lexsp, ERRID_InitTypeMismatch, "trying to initialize %s with a 'void' type", istrIdent.PChz());
					return TCRET_StoppingError;
				}

				switch (pStnodInit->m_pTin->m_tink)
				{
				case TINK_Void:
				case TINK_Type:
					EmitError(pTcctx, pStnod->m_lexsp, ERRID_UninstantiableType,
						"cannot initialize variable %s with type '%s'.", istrIdent.PChz(), PChzFromTink(pStnodInit->m_pTin->m_tink));
					return TCRET_StoppingError;
				default:
					break;
				}

				// BB - This won't allow an override of operator:= to return a different type
				// I'm planning on coming back to it when I handle return types values as regular LValues
				pTinInitDefault = PTinPromoteUntypedDefault(pTcctx, pTcsentTop->m_pSymtab, pStnodInit);

				// pass pTin as pTinDst - we aren't assigning it to a different type
				pTinInitDefault = PTinAfterRValueAssignment(pTcctx, pStnodInit->m_lexsp, pTinInitDefault, pTcsentTop->m_pSymtab, pTinInitDefault);

				MOE_ASSERT(pTinInitDefault, "failed to compute default init type");
			}

			OverloadCheck ovcheck(nullptr);

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

				ProcMatchParam pmparam(pTcctx->m_pAlloc, pStnod->m_lexsp);
				pmparam.m_cpStnodCall = 2;
				pmparam.m_ppStnodCall = apStnod;

				ovcheck = OvcheckTryCheckOverload(pTcctx, pStnod, &pmparam);
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
				FinalizeLiteralType(pTcctx, pTcsentTop->m_pSymtab, pStop->m_optype.m_pTinLhs, pStnodInit);
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
					pTinInit = PTinPromoteUntypedTightest(pTcctx, pSymtab, pStnodInit, pStnod->m_pTin);

					// Strip the top level const, as we're declaring a new instance
					if (pStnod->m_pTin->m_tink == TINK_Anchor || FCanCastForInit(pTcctx, pStnod->m_lexsp, pSymtab, pTinInit, pStnod->m_pTin))
					{
						if (fAllowFinalizing)
						{
							FinalizeLiteralType(pTcctx, pSymtab, pStnod->m_pTin, pStnodInit);
						}
						pTinInit = pStnod->m_pTin;
					}
					else
					{
						const char * pChzFormat = (pTcsentTop->m_parkDeclContext == PARK_ParameterList) ?
							"parameter '%s' is type '%s', but default argument is '%s'" :
							"Cannot initialize variable '%s' of type '%s' with '%s'";

						InString istrIdent = IstrFromIdentifier(pStnodIdent);
						EmitError(pTcctx, pStnod->m_lexsp, ERRID_InitTypeMismatch,
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
						FinalizeLiteralType(pTcctx, pTcsentTop->m_pSymtab, pStnod->m_pTin, pStnodInit);
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
				EmitError(pTcctx, pStnod->m_lexsp, ERRID_CannotInferType,
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
					OnTypeResolve(pTcctx, pSymIdent);
				}
			}
			else if (!FIsGenericType(pStnod->m_pTin))
			{
				EmitError(pTcctx, pStnod->m_lexsp, ERRID_UnnamedNotAllowed, 
					"Unnamed declaration must be used with a generic type specification");
			}

			if (pStdecl->m_fHasUsingPrefix)
			{
				auto pTinUsing = pStnod->m_pTin;
				auto pSymtabUsing = PSymtabFromType(pTcctx, pTinUsing, pStnod->m_lexsp);
				if (pSymtabUsing)
				{
					pSymtab->AddUsingScope(pTcctx->PErrman(), pSymtabUsing, pStnod);
				}
			}
		}

		pStnod->m_strees = STREES_TypeChecked;
		PopTcsent(pTcctx, &pTcsentTop, pStnod);
	}
	return TCRET_Continue;
}

TcretDebug TcretCheckConstantDecl(STNode * pStnod, TypeCheckContext * pTcctx, TypeCheckStackEntry * pTcsentTop)
{
	auto pStdecl = PStnodDerivedCast<STDecl *>(pStnod);
	if (pTcsentTop->m_nState < 1)
	{
		pTcsentTop->m_nState = 1;	// skip the identifier

		auto pStnodIdent = pStdecl->m_pStnodIdentifier;
		if (MOE_FVERIFY(pStnodIdent, "constant missing identifier"))
		{
			auto pSym = pStnodIdent->PSym();
			MOE_ASSERT(pSym, "expected symbol for declaration");
			pTcsentTop->m_pSymContext = pSym;
		}
	}

	if (pTcsentTop->m_nState < pStnod->CPStnodChild())
	{
		auto pStnodCur = pStnod->PStnodChild(pTcsentTop->m_nState);
		auto pTcsentPushed = PTcsentPush(pTcctx, &pTcsentTop, pStnodCur);
		if (pTcsentPushed && pStnodCur == pStdecl->m_pStnodType)
		{
			pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
		}
		
		++pTcsentTop->m_nState;
		return TCRET_Continue;
	}

	auto pStnodInit = pStdecl->m_pStnodInit;
	auto pStnodIdent = pStdecl->m_pStnodIdentifier;
	InString istrIdent = IstrFromIdentifier(pStnodIdent);
	if (FIsType(pStnodInit))
	{
		EmitError(pTcctx, pStnod->m_lexsp, ERRID_InitTypeMismatch, 
			"Cannot initialize constant '%s' to non-instance value.",istrIdent.PChz());
		return TCRET_StoppingError;
	}

	auto pSymtab = pTcsentTop->m_pSymtab;
	auto pStnodType = pStdecl->m_pStnodType;
	if (pStnodType)
	{
		bool fIsValidTypeSpec;
		TypeInfo * pTinType = PTinFromTypeSpecification(
								pTcctx,
								pSymtab,
								pStnodType,
								pTcsentTop->m_grfsymlook,
								nullptr,
								&fIsValidTypeSpec);

		if (!fIsValidTypeSpec)
			return TCRET_StoppingError;

		// just make sure the init type fits the specified one
		TypeInfo * pTinInit = pStnodInit->m_pTin;
		pTinInit = PTinPromoteUntypedTightest(pTcctx, pSymtab, pStnodInit, pTinType);
		pTinInit = PTinAfterRValueAssignment(pTcctx, pStnodInit->m_lexsp, pTinInit, pSymtab, pTinType);
		if (FCanImplicitCast(pTinInit, pTinType))
		{
			FinalizeLiteralType(pTcctx, pSymtab, pTinType, pStnodInit);
		}
		else
		{
			EmitError(pTcctx, pStnod->m_lexsp, ERRID_InitTypeMismatch, "Cannot initialize constant of type %s with %s",
				IstrFromTypeInfo(pTinType).PChz(),
				IstrFromTypeInfo(pTinInit).PChz());
		}
	}
	else
	{
		// Promote as literal, just to error on untyped acasts. We don't actually force a type on
		//  the constant until it gets used
		(void) PTinPromoteUntypedDefault(pTcctx, pSymtab, pStnodInit);
	}

	auto pStval = PStnodRtiCast<STValue*>(pStnod);
	auto pStvalInit = PStnodRtiCast<STValue*>(pStnodInit);
	if (MOE_FVERIFY(pStval && pStvalInit, "expected value structs"))
	{
		pStval->CopyValues(pStvalInit);
	}
	pStnod->m_pTin = pStnodInit->m_pTin;

	// find our symbol and resolve any pending unknown types
	if (MOE_FVERIFY(pStnodIdent, "constant Declaration without identifier"))
	{
		// TODO: Using?
		Symbol * pSymIdent = pSymtab->PSymLookup(
										istrIdent,
										pStnodIdent->m_lexsp,
										pTcsentTop->m_grfsymlook);

		if (!pSymIdent || pSymIdent->m_pStnodDefinition != pStnod)
		{
			EmitError(pTcctx, pStnod->m_lexsp, ERRID_SymbolLookupFailed, "symbol lookup failed for '%s'", istrIdent.PChz());
			return TCRET_StoppingError;
		}
		else
		{
			pStnod->m_pSymbase = pSymIdent;
			auto pStnodIdent = pSymIdent->m_pStnodDefinition;
			if (pStnodIdent->m_pTin == nullptr)
			{
				pStnodIdent->m_pTin = pStnod->m_pTin;
			}
		}
		OnTypeResolve(pTcctx, pSymIdent);
	}

	pStnod->m_strees = STREES_TypeChecked;
	PopTcsent(pTcctx, &pTcsentTop, pStnod);
	return TCRET_Continue;
}

static bool FIsEnumFlagLValue(SymbolBase * pSymbase)
{
	if (pSymbase->m_symk != SYMK_Path)
		return false;

	auto pSympath = (SymbolPath *)pSymbase;pSymbase;pSymbase;pSymbase;
	if (pSympath->m_arypSym.C() < 2)
		return false;

	auto pSymEnum = pSympath->m_arypSym[pSympath->m_arypSym.C() - 2];
	if (pSymEnum->m_grfsym.FIsSet(FSYM_IsType))
		return false; 

	auto pTin = PTinStripQualifiersAndPointers(PTinFromSymbol(pSymEnum));
	auto pTinenum = PTinRtiCast<TypeInfoEnum *>(pTin);
	if (!pTinenum || pTinenum->m_enumk != ENUMK_FlagEnum)
		return false;

	// BB - need to make sure constant is not implicit constant
	return true;
}

TcretDebug TcretCheckIdentifier(STNode * pStnod, TypeCheckContext * pTcctx, TypeCheckStackEntry * pTcsentTop)
{
	// Note: we're only expecting to get here for identifiers within statements.
	//  Identifiers for function names, declaration names*, should do their own type checking.
	//      * declaration type identifiers will be type checked here

	InString istrIdent = IstrFromIdentifier(pStnod);
	if (MOE_FVERIFY(!istrIdent.FIsEmpty(), "identifier node with no value"))
	{
		SymbolTable * pSymtab = pTcsentTop->m_pSymtab;

		auto pSymbase = PSymbaseLookup(pSymtab, istrIdent, pStnod->m_lexsp, pTcsentTop->m_grfsymlook);
		if (!pSymbase)
		{
			EmitError(pTcctx, pStnod->m_lexsp, ERRID_UnknownIdentifier, "'%s' unknown identifier detected", istrIdent.PChz());
			return TCRET_StoppingError;
		}

		Symbol * pSymLast = PSymLast(pSymbase);
		if (pSymLast->m_grfsym.FIsSet(FSYM_IsBuiltIn))
		{
			pStnod->m_pTin = PTinFromSymbol(pSymLast);
			pStnod->m_pSymbase = pSymbase;
		}
		else if (MOE_FVERIFY(pSymLast->m_pStnodDefinition, "Non-built-in types must have a STNode"))
		{
			STNode * pStnodDefinition = pSymLast->m_pStnodDefinition;
			if (pStnodDefinition->m_park == PARK_GenericDecl)
			{
				// We're type checking the uninstantiated generic, don't wait for a symbol definition
				//  this symbol will be replaced later

				if (pStnodDefinition->m_pTin)
				{
					// we may encounter a PARK_GenericDecl node that has been copied during the instantiation
					//  if it has a concrete type, we're ok

					pStnod->m_pTin = pStnodDefinition->m_pTin;
					pStnod->m_pSymbase = pSymbase;
				}
				else
				{
					pStnod->m_pTin = pStnodDefinition->m_pTin;
					pStnod->m_pSymbase = pSymbase;
					AddSymbolReference(pTcsentTop->m_pSymContext, pSymLast);
				}
			}
			else if (pStnodDefinition->m_park == PARK_Decl ||
				pStnodDefinition->m_park == PARK_ConstantDecl ||
				pStnodDefinition->m_park == PARK_CompoundLiteral ||
				pStnodDefinition->m_park == PARK_Typedef ||
				pStnodDefinition->m_park == PARK_EnumDefinition ||
				pStnodDefinition->m_park == PARK_EnumConstant ||
				pStnodDefinition->m_park == PARK_StructDefinition ||
				pStnodDefinition->m_park == PARK_ProcedureDefinition)
			{
				if (pStnodDefinition->m_strees >= STREES_TypeChecked || 
				   ((pStnodDefinition->m_strees >= STREES_SignatureTypeChecked) && pTcsentTop->m_fAllowForwardDecl))
				{
					MOE_ASSERT(pStnodDefinition->m_pTin, "symbol definition was type checked, but has no type?");
					pStnod->m_pTin = pStnodDefinition->m_pTin;
					pStnod->m_pSymbase = pSymbase;

					AddSymbolReference(pTcsentTop->m_pSymContext, pSymLast);

					if (pStnod->m_pTin && 
						(pStnod->m_pTin->m_tink == TINK_Literal || pStnod->m_pTin->m_tink == TINK_Enum))
					{
						auto pStvalDefinition = PStnodRtiCast<STValue *>(pStnodDefinition);
						if (pStvalDefinition)
						{
							auto pStval = PStnodRtiCast<STValue *>(pStnod);
							if (MOE_FVERIFY(pStval, "expected nodes to be values"))
							{
								pStval->CopyValues(pStvalDefinition);
							}

							if (FIsEnumFlagLValue(pSymbase))
							{
								auto pTinFlag = pSymtab->PTinBuiltin(BuiltIn::g_istrEnumFlag);
								pStnod->m_pTin = pTinFlag;
							}
						}
					}
				}
				else
				{
					// set up dependency for either the definition or the type...
					
					SetupSymbolWait(pTcctx, pSymLast);
					return TCRET_WaitingForSymbolDefinition;
				}
			}
			else
			{
				LexLookup lexlook(pTcctx->m_pWork, pStnodDefinition->m_lexsp);
				MOE_ASSERT(false, "unexpected identifier source for '%s' in type check. %s %d:%d", 
					istrIdent.PChz(),
					lexlook.m_istrFilename.PChz(), lexlook.m_iLine, lexlook.m_iCodepoint);
			}
		}
	}

	PopTcsent(pTcctx, &pTcsentTop, pStnod);
	pStnod->m_strees = STREES_TypeChecked;
	return TCRET_Continue;
}

TcretDebug TcretCheckLiteral(STNode * pStnod, TypeCheckContext * pTcctx, TypeCheckStackEntry * pTcsentTop)
{
	if (MOE_FVERIFY(pStnod->m_pTin == nullptr, "TypeInfoLiteral should not be constructed before type checking"))
	{
		SymbolTable * pSymtab = pTcsentTop->m_pSymtab;

		auto pStval = PStnodRtiCast<STValue *>(pStnod);
		if (MOE_FVERIFY(pStval, "null value in literal"))
		{
			TypeInfoLiteral * pTinlit = pSymtab->PTinlitAllocUnfinal(pStval->m_stvalk);
			pStnod->m_pTin = pTinlit;
		}
	}
	
	pStnod->m_strees = STREES_TypeChecked;
	PopTcsent(pTcctx, &pTcsentTop, pStnod);
	return TCRET_Continue;
}

inline TypeInfo * PTinReturnFromStnodProcedure(STNode * pStnod)
{
	auto pStproc = PStnodRtiCast<STProc*>(pStnod);
	if (!MOE_FVERIFY(pStnod->m_park == PARK_ProcedureDefinition && pStproc, "Bad procedure node"))
		return nullptr;
	if (pStproc->m_pStnodReturnType)
		return pStproc->m_pStnodReturnType->m_pTin;
	return nullptr;
}

TcretDebug TcretCheckReservedWord(STNode * pStnod, TypeCheckContext * pTcctx, TypeCheckStackEntry * pTcsentTop)
{
	auto pStval = PStnodRtiCast<STValue*>(pStnod);
	if (!MOE_FVERIFY(pStval, "reserved word without value"))
		return TCRET_StoppingError;

	auto istrRword = pStval->m_istrRword;
	if (istrRword == RWord::g_istrSizeof || istrRword == RWord::g_istrAlignof || istrRword == RWord::g_istrTypeinfo)
	{
		if (pTcsentTop->m_nState < pStnod->CPStnodChild())
		{
			(void) PTcsentPush(pTcctx, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
			return TCRET_Continue;
		}

		auto pStnodChild = pStnod->PStnodChild(0);
		if (!MOE_FVERIFY(pStnodChild, "%s missing child", istrRword.PChz()))
			return TCRET_Continue;

		if (!pStnodChild->m_pTin)
		{
			EmitError(pTcctx, pStnod->m_lexsp, ERRID_CannotInferType, "%s unable to determine target type", istrRword.PChz());
		}

		auto pSymtab = pTcsentTop->m_pSymtab;

		auto pTinDefault = PTinPromoteUntypedDefault(pTcctx, pSymtab, pStnodChild);
		if (pStnodChild->m_pTin->m_tink == TINK_Literal)
		{
			pStnodChild->m_pTin = pTinDefault;
		}

		pStnodChild->m_pTin = pSymtab->PTinMakeUnique(pStnodChild->m_pTin);

		if (istrRword == RWord::g_istrTypeinfo)
		{
			// Make sure the type table is already resoved, this ensures that it will codegen before
			//  this typeInfo statement is generated
			auto pSymTinTable = pSymtab->PSymLookup(BuiltIn::g_istrGlobalTinTable, LexSpan());
			if (!pSymTinTable )
			{
				return TcretWaitForTypeSymbol(pTcctx, pSymTinTable, pStnod);
			}

			// lookup STypeInfo, or return waiting for type
			auto pSymTypeinfo = pSymtab->PSymLookup(BuiltIn::g_istrTypeInfo, LexSpan());

			if (!pSymTypeinfo)
			{
				return TcretWaitForTypeSymbol(pTcctx, pSymTypeinfo, pStnod);
			}
			pStnod->m_pTin = pSymtab->PTinptrAllocate(PTinFromSymbol(pSymTypeinfo));
		}
		else
		{
			pStnod->m_pTin = pSymtab->PTinBuiltin(BuiltIn::g_istrUSize);
		}

		pStnod->m_strees = STREES_TypeChecked;
		PopTcsent(pTcctx, &pTcsentTop, pStnod);
	}
	else if (istrRword == RWord::g_istrFor)
	{
		if (pTcsentTop->m_nState < pStnod->CPStnodChild())
		{
			auto pTcsentPushed = PTcsentPush(pTcctx, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
			if (pTcsentPushed)
			{
				pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;
				MOE_ASSERT(pTcsentPushed->m_pSymtab, "null symbol table");
			}

			return TCRET_Continue;
		}

		auto pStfor = PStnodRtiCast<STFor *>(pStnod);
		if (pStfor == nullptr)
		{
			EmitError(pTcctx, pStnod->m_lexsp, ERRID_MalformedAstInTypeCheck, "for loop was improperly parsed.");
			return TCRET_StoppingError;
		}

		auto pStnodPredicate = pStfor->m_pStnodPredicate;
		if (pStnodPredicate)
		{
			auto pSymtab = pTcsentTop->m_pSymtab;
			TypeInfo * pTinBool = pSymtab->PTinBuiltin(BuiltIn::g_istrBool);
			TypeInfo * pTinPredPromoted = PTinPromoteUntypedRvalueTightest(pTcctx, pTcsentTop->m_pSymtab, pStnodPredicate, pTinBool);

			if (!FCanImplicitCast(pTinPredPromoted, pTinBool))
			{
				auto istrTin = IstrFromTypeInfo(pTinPredPromoted);
				EmitError(pTcctx, pStnod->m_lexsp, ERRID_CannotConvertToBool, 
					"Cannot convert predicate from %s to bool", istrTin.PChz());
			}

			FinalizeLiteralType(pTcctx, pTcsentTop->m_pSymtab, pTinBool, pStnodPredicate);
		}

		pStnod->m_strees = STREES_TypeChecked;
		PopTcsent(pTcctx, &pTcsentTop, pStnod);
	}
	else if (istrRword == RWord::g_istrFallthrough)
	{
		MOE_ASSERT(pTcctx->m_aryTcsent.PLast()->m_pStnod == pStnod, "expected this node");
		STNode * pStnodChild = pStnod;
		for (int iTcsent = (int)pTcctx->m_aryTcsent.C()-2; iTcsent >= 0; --iTcsent)
		{
			auto pStnodIt = pTcctx->m_aryTcsent[iTcsent].m_pStnod;
			bool fIsValidPosition = false;
			switch (pStnodIt->m_park)
			{
				case PARK_List:
				{
					if (pStnodIt->PStnodChildSafe(pStnodIt->CPStnodChild()-1) == pStnodChild)
					{
						fIsValidPosition = true;
					}
				} break;
				case PARK_ReservedWord:
				{
					auto pStvalIt = PStnodRtiCast<STValue *>(pStnodIt);
					if (!MOE_FVERIFY(pStvalIt, "bad reserved word."))
						break;

					if (pStvalIt->m_istrRword == RWord::g_istrCase || pStvalIt->m_istrRword == RWord::g_istrElse)
					{
						pStnodIt->m_grfstnod.AddFlags(FSTNOD_Fallthrough);
						fIsValidPosition = true;
						iTcsent = -1;
					}
				} break;
				default:
					MOE_ASSERT(false, "unexpected parse kind");
					break;
			}

			if (!fIsValidPosition)
			{
				EmitError(pTcctx, pStnod->m_lexsp, ERRID_StatementAfterFallthrough,
					"fallthrough keyword should always be the last statement in a switch case");
			}
			pStnodChild = pStnodIt;
		}

		MOE_ASSERT(pStnod->CPStnodChild() == 0, "did not expect child nodes");
		pStnod->m_strees = STREES_TypeChecked;
		PopTcsent(pTcctx, &pTcsentTop, pStnod);

	}
	else if (istrRword == RWord::g_istrContinue || istrRword == RWord::g_istrBreak)
	{
		MOE_ASSERT(pStnod->CPStnodChild() == 0, "did not expect child nodes");
		pStnod->m_strees = STREES_TypeChecked;
		PopTcsent(pTcctx, &pTcsentTop, pStnod);
	}
	else if (istrRword == RWord::g_istrSwitch)
	{
		if (pTcsentTop->m_nState < pStnod->CPStnodChild())
		{
			(void) PTcsentPush(pTcctx, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
			return TCRET_Continue;
		}

		auto pStnodExp = pStnod->PStnodChildSafe(0);
		if (!pStnod)
		{
			EmitError(pTcctx, pStnod->m_lexsp, ERRID_SwitchExpressionExpected, "switch missing expression");
			return TCRET_StoppingError;
		}

		TypeInfo * pTinExpPromoted = PTinPromoteUntypedDefault(pTcctx, pTcsentTop->m_pSymtab, pStnodExp);
		FinalizeLiteralType(pTcctx, pTcsentTop->m_pSymtab, pTinExpPromoted, pStnodExp);

		// BB - should make pTinExpPromoted const?
		pTinExpPromoted = PTinStripQualifiers(pTinExpPromoted);
		if ((pTinExpPromoted->m_tink != TINK_Numeric && FIsInteger(((TypeInfoNumeric*)pTinExpPromoted)->m_numk)) && 
			pTinExpPromoted->m_tink != TINK_Bool && 
			pTinExpPromoted->m_tink != TINK_Enum)
		{
			EmitError(pTcctx, pStnod->m_lexsp, ERRID_SwitchCaseMustBeInteger, "switch expression must evaluate to an integer type");
			return TCRET_StoppingError;
		}

		Moe::CDynAry<STNode *> aryPStnod(pTcctx->m_pAlloc, BK_TypeCheck, pStnod->CPStnodChild());
		Moe::CDynAry<BigInt> aryBint(pTcctx->m_pAlloc, BK_TypeCheck, pStnod->CPStnodChild());

		int iStnodDefault = -1;
		for (int iStnodIt = 1; iStnodIt < pStnod->CPStnodChild(); ++iStnodIt)
		{
			auto pStnodIt = pStnod->PStnodChild(iStnodIt);

			auto pStvalIt = PStnodRtiCast<STValue *>(pStnodIt);
			if (!MOE_FVERIFY(pStnodIt->m_park == PARK_ReservedWord || !pStvalIt, "expected switch case"))
				continue;

			if (pStvalIt->m_istrRword == RWord::g_istrElse)
			{
				if (iStnodDefault >= 0)
				{
					auto pStnodPrev = pStnod->PStnodChild(iStnodDefault);
					LexLookup lexlook(pTcctx->m_pWork, pStnodPrev);

					EmitError(pTcctx, pStnodIt->m_lexsp, ERRID_SwitchElseAlreadyDefined,
						"switch statement else case already defined. %s (%d,%d). ",
						lexlook.m_istrFilename.PChz(),
						lexlook.m_iLine,
						lexlook.m_iCodepoint);
					continue;
				}

				iStnodDefault = iStnodIt;
			}
			else // "case"
			{
				int cStnodLit = pStnodIt->CPStnodChild()-1;
				for (int iStnodLit = 0; iStnodLit < cStnodLit; ++iStnodLit)
				{
					auto pStnodLit = pStnodIt->PStnodChildSafe(iStnodLit);
					if (!MOE_FVERIFY(pStnodLit, "missing case literal"))
						continue;

					TypeInfo * pTinCase = PTinPromoteUntypedTightest(
												pTcctx,
												pTcsentTop->m_pSymtab,
												pStnodLit,
												pTinExpPromoted);
					pTinCase = PTinAfterRValueAssignment(pTcctx, pStnodLit->m_lexsp, pTinCase, pTcsentTop->m_pSymtab, pTinExpPromoted);

					if (!FCanImplicitCast(pTinCase, pTinExpPromoted))
					{
						auto istrTinCase = IstrFromTypeInfo(pTinCase);
						auto istrTinExp = IstrFromTypeInfo(pTinExpPromoted);
						EmitError(pTcctx, pStnod->m_lexsp, ERRID_BadImplicitConversion,
							"No conversion between %s and %s", istrTinCase.PChz(), istrTinExp.PChz());
					}

					FinalizeLiteralType(pTcctx, pTcsentTop->m_pSymtab, pTinExpPromoted, pStnodLit);

					auto pTinLit = pStnodLit->m_pTin;
					if (!pTinLit || pTinLit->m_tink != TINK_Literal)
					{
						EmitError(pTcctx, pStnod->m_lexsp, ERRID_NonConstantInLiteral, "case literal does not evaluate to a constant");
						continue;
					}

					auto pStvalLit = PStnodRtiCast<STValue*>(pStnodLit); 
					if (!MOE_FVERIFY(pStvalLit, "case literal missing value"))
						continue;

					BigInt bint = BintFromStval(pStvalLit);
					aryPStnod.Append(pStnodLit);
					aryBint.Append(bint);
				}
			}
		}

		for (int iBintLhs = 0; iBintLhs < aryBint.C(); ++iBintLhs)
		{
			BigInt bintLhs = aryBint[iBintLhs];
			for (int iBintRhs = iBintLhs+1; iBintRhs < aryBint.C(); ++iBintRhs)
			{
				if (FAreEqual(bintLhs, aryBint[iBintRhs]))
				{
					auto pStnodLhs = aryPStnod[iBintLhs];
					auto pStnodRhs = aryPStnod[iBintLhs];

					LexLookup lexlook(pTcctx->m_pWork, pStnodLhs);

					EmitError(
						pTcctx, pStnodRhs->m_lexsp, ERRID_CaseAlreadyUsed,
						"case value %s%lld already used. %s(%d, %d):",
						(bintLhs.m_fIsNegative) ? "-" : "",
						bintLhs.m_nAbs,
						lexlook.m_istrFilename.PChz(),
						lexlook.m_iLine,
						lexlook.m_iCodepoint);
				}
			}
		}

		pStnod->m_strees = STREES_TypeChecked;
		PopTcsent(pTcctx, &pTcsentTop, pStnod);
	}
	else if (istrRword == RWord::g_istrElse || istrRword == RWord::g_istrCase)
	{
		if (pTcsentTop->m_nState < pStnod->CPStnodChild())
		{
			(void) PTcsentPush(pTcctx, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
			return TCRET_Continue;
		}

		pStnod->m_strees = STREES_TypeChecked;
		PopTcsent(pTcctx, &pTcsentTop, pStnod);
	} 
	else if (istrRword == RWord::g_istrWhile || istrRword == RWord::g_istrIf)
	{
		if (pTcsentTop->m_nState < pStnod->CPStnodChild())
		{
			(void) PTcsentPush(pTcctx, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
			return TCRET_Continue;
		}

		if (pStnod->CPStnodChild() < 2)
		{
			EmitError(pTcctx, pStnod->m_lexsp, ERRID_MissingPredicate,
				"encountered %s statement without expected predicate,child",
				istrRword.PChz());
			return TCRET_StoppingError;
		}

		// (if (predicate) (ifCase) (else (elseCase)))
		// (while (predicate) (body))
		STNode * pStnodPred = pStnod->PStnodChild(0);

		auto pSymtab = pTcsentTop->m_pSymtab;
		TypeInfo * pTinBool = pSymtab->PTinBuiltin(BuiltIn::g_istrBool);
		TypeInfo * pTinPredPromoted = PTinPromoteUntypedRvalueTightest(
										pTcctx,
										pTcsentTop->m_pSymtab,
										pStnodPred,
										pTinBool);

		if (!FCanImplicitCast(pTinPredPromoted, pTinBool))
		{
			auto istrTinPred = IstrFromTypeInfo(pTinPredPromoted);
			EmitError(pTcctx, pStnod->m_lexsp, ERRID_CannotConvertToBool, "No conversion between %s and bool", istrTinPred.PChz());
		}

		pStnod->m_pTin = pTinBool;
		FinalizeLiteralType(pTcctx, pTcsentTop->m_pSymtab, pTinBool, pStnodPred);

		pStnod->m_strees = STREES_TypeChecked;
		PopTcsent(pTcctx, &pTcsentTop, pStnod);
	}
	else if (istrRword == RWord::g_istrReturn)
	{
		if (pTcsentTop->m_nState < pStnod->CPStnodChild())
		{
			(void) PTcsentPush(pTcctx, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
			return TCRET_Continue;
		}

		STNode * pStnodProc = pTcsentTop->m_pStnodProcedure;
		if (!pStnodProc)
		{
			EmitError(pTcctx, pStnod->m_lexsp, ERRID_ReturnOutsideProcedure,
				"Return statement encountered outside of a procedure");
			return TCRET_StoppingError;
		}

		TypeInfo * pTinReturn = PTinReturnFromStnodProcedure(pStnodProc);
		if (!MOE_FVERIFY(pTinReturn, "expected return type (implicit void should be added by now"))
			return TCRET_StoppingError;

		if (pStnod->CPStnodChild() == 0)
		{
			if (pTinReturn->m_tink != TINK_Void)
			{
				EmitError(pTcctx, pStnod->m_lexsp, ERRID_NonVoidReturnExpected, "non void return type expected.");
				return TCRET_StoppingError;
			}
			pStnod->m_pTin = pTinReturn;
		}
		else if (pStnod->CPStnodChild() == 1)
		{
			STNode * pStnodRhs = pStnod->PStnodChild(0);
			if (!FVerifyIvalk(pTcctx, pStnodRhs, IVALK_RValue))
				return TCRET_StoppingError;

			TypeInfo * pTinRhs = pStnodRhs->m_pTin;
			TypeInfo * pTinRhsPromoted = PTinPromoteUntypedRvalueTightest(
											pTcctx,
											pTcsentTop->m_pSymtab,
											pStnodRhs,
											pTinReturn);

			// Strip the top level const, as we're declaring a new instance
			auto pTinInstance = PTinAfterRValueAssignment(pTcctx, pStnodRhs->m_lexsp, pTinReturn, pTcsentTop->m_pSymtab, pTinReturn);
			if (FCanImplicitCast(pTinRhsPromoted, pTinInstance))
			{
				pStnod->m_pTin = pTinReturn;
				FinalizeLiteralType(pTcctx, pTcsentTop->m_pSymtab, pTinReturn, pStnodRhs);
			}
			else
			{
				auto istrLhs = IstrFromTypeInfo(pTinReturn);
				auto istrRhs = IstrFromTypeInfo(pTinRhs);
				EmitError(pTcctx, pStnod->m_lexsp, ERRID_BadImplicitConversion,
					"implicit cast from %s to %s is not allowed by return statement",
					istrRhs.PChz(),
					istrLhs.PChz());
			}
		}
		else
		{
			MOE_ASSERT(false, "multiple return types not supported (yet).");
		}

		pStnod->m_strees = STREES_TypeChecked;
		PopTcsent(pTcctx, &pTcsentTop, pStnod);
	}
	else
	{
		EmitError(pTcctx, pStnod->m_lexsp, ERRID_UnhandledRWord, 
			"unhandled reserved word '%s' in type checker", istrRword.PChz());
		return TCRET_StoppingError;
	}

	return TCRET_Continue;
}

inline bool FComputeUnaryOpOnLiteral(
	TypeCheckContext * pTcctx,
	STOperator * pStop,
	SymbolTable * pSymtab,
	STNode * pStnodOperand,
	TypeInfoLiteral ** ppTinOperand,
	TypeInfoLiteral ** ppTinReturn)
{
	TypeInfo * pTinOperand = pStnodOperand->m_pTin;

	auto pStvalOperand = PStnodRtiCast<STValue *>(pStnodOperand);
	if ((pTinOperand->m_tink != TINK_Literal) | (pStvalOperand == nullptr))
		return false;

	TypeInfoLiteral * pTinlitOperand = (TypeInfoLiteral *)pTinOperand;
	const LiteralType & littyOperand = pTinlitOperand->m_litty;

	bool fOperandIsNumber = (littyOperand.m_litk == LITK_Numeric) | (littyOperand.m_litk == LITK_Enum);
	if (!fOperandIsNumber)
		return false;

	TOK tokOperator = pStop->m_tok;
	bool fIsBoolOp = FDoesOperatorReturnBool(pStop->m_park);

	if (littyOperand.m_numk == NUMK_Float)
	{
		bool f;
		f64 g = GLiteralCast(pStvalOperand);
		switch ((u32)tokOperator)
		{
		case '-':         g = -g; break;
		case '!':         f = !g; break;
		default: return false;
		}

		STValue * pStvalResult =  PStvalAllocAfterParse(pSymtab->m_pAlloc, PARK_Literal, pStop->m_lexsp);
		pStop->m_pStnodResult = pStvalResult;

		if (fIsBoolOp)
		{
			pStvalResult->SetBool(f);
	
			TypeInfoLiteral * pTinBool = pSymtab->PTinlitFromLitk(LITK_Bool);
			MOE_ASSERT(!pTinBool || pTinBool->m_tink == TINK_Literal, "expected literal type");

			*ppTinReturn = pTinBool;
			*ppTinOperand = pTinlitOperand;
		}
		else
		{
			pStvalResult->SetF64(g);

			*ppTinReturn = pTinlitOperand;
			*ppTinOperand = pTinlitOperand;
		}
		return true;
	} 
	else // LITK_Integer
	{
		MOE_ASSERT(littyOperand.m_cBit == -1, "expected unsized literal here");

		BigInt bintOperand(BintFromStval(pStvalOperand));

		auto pTinenum = PTinRtiCast<TypeInfoEnum *>(pTinlitOperand->m_pTinSource);
		bool fIsFlagEnum = (pTinenum && pTinenum->m_enumk == ENUMK_FlagEnum);

		bool f;
		switch ((u32)tokOperator)
		{
			case TOK('-'):
			{
				if (fIsFlagEnum)
					return false;

				bintOperand.m_fIsNegative = !bintOperand.m_fIsNegative; break;
			}
			// BB - We're not really handling unsized literals correctly here - we'll just promote to a 64 bit type
			case TOK('~'):       bintOperand = BintBitwiseNot(bintOperand); break;

			case TOK('!'):         
			{
				if (fIsFlagEnum)
					return false;
				f = bintOperand.m_nAbs == 0; break;
			}
			default: return false;
		}

		STValue * pStvalResult =  PStvalAllocAfterParse(pSymtab->m_pAlloc, PARK_Literal, pStop->m_lexsp);
		pStop->m_pStnodResult = pStvalResult;

		if (fIsBoolOp)
		{
			pStvalResult->SetBool(f);
	
			TypeInfoLiteral * pTinBool = pSymtab->PTinlitFromLitk(LITK_Bool);
			MOE_ASSERT(!pTinBool || pTinBool->m_tink == TINK_Literal, "expected literal type");

			*ppTinReturn = pTinBool;
			*ppTinOperand = pTinlitOperand;
		}
		else
		{
			SetIntegerValue(pTcctx, pStvalResult, bintOperand);

			if (pTinlitOperand->m_litty.m_litk == LITK_Enum)
			{
#if 1
				// BB - Is this supposed to end up being the loose type? if so we should find it from the enum rather than 
				//  making a non-unique integer literal type
				auto pTinlitInt = PTinRtiCast<TypeInfoLiteral *>(pStvalOperand->m_pTin);
				MOE_ASSERT(pTinlitInt, "expected enum literal");
#else // pre MOEB
				// We need to make an unfinalized integer literal
				auto pTinlitInt = MOE_NEW(pSymtab->m_pAlloc, TypeInfoLiteral) TypeInfoLiteral();
				pTinlitInt->m_litty.m_litk = LITK_Numeric;
				pSymtab->AddManagedTin(pTinlitInt);
#endif

				*ppTinReturn = pTinlitInt;
				*ppTinOperand = pTinlitOperand;
			}
			else
			{
				MOE_ASSERT(pTinlitOperand->m_litty.m_litk == LITK_Numeric, "expected integer literal");
				*ppTinReturn = pTinlitOperand;
				*ppTinOperand = pTinlitOperand;
			}
		}
		return true;
	}
}

TcretDebug TcretCheckUnaryOp(STNode * pStnod, TypeCheckContext * pTcctx, TypeCheckStackEntry * pTcsentTop)
{
	if (pTcsentTop->m_nState < pStnod->CPStnodChild())
	{
		(void) PTcsentPush(pTcctx, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
		return TCRET_Continue;
	}

	auto pStop = PStnodRtiCast<STOperator*>(pStnod);
	if (MOE_FVERIFY(pStop, "expected unary operator"))
		return TCRET_StoppingError;

	STNode * pStnodOperand = pStnod->PStnodChild(0);
	TypeInfo * pTinOperand = pStnodOperand->m_pTin;

	ProcMatchParam pmparam(pTcctx->m_pAlloc, pStnod->m_lexsp);
	pmparam.m_cpStnodCall = 1; //pStnod->m_arypStnodChild.C();
	pmparam.m_ppStnodCall = (pmparam.m_cpStnodCall) ? pStnod->m_apStnodChild : nullptr;

	OverloadCheck ovcheck = OvcheckTryCheckOverload(pTcctx, pStop, &pmparam);
	if (ovcheck.m_pTinproc)
	{
		MOE_ASSERT(ovcheck.m_argord == ARGORD_Normal, "unary arguments cannot be commutative");
		if (ovcheck.m_tcret != TCRET_Complete)
			return ovcheck.m_tcret;

		TypeInfoProcedure * pTinproc = ovcheck.m_pTinproc;
		if (MOE_FVERIFY(pTinproc->m_arypTinParams.C() == 1 && pTinproc->m_arypTinReturns.C() == 1,
					"bad operator overload signature"))
		{
			pStop->m_optype = OpTypes(pTinOperand, pTinOperand, pTinproc->m_arypTinReturns[0]);
		}

		pStop->m_optype.m_pTinprocOverload = ovcheck.m_pTinproc;
		pStop->m_pTin = pStop->m_optype.m_pTinResult;
	}
	else
	{
		pStop->m_optype = OpTypes(pTinOperand, pTinOperand, pTinOperand);

		IVALK ivalkExpected = (pStnod->m_tok == TOK_Reference) ? IVALK_LValue : IVALK_RValue;
		if (!FVerifyIvalk(pTcctx, pStnodOperand, ivalkExpected))
			return TCRET_StoppingError;

		if (MOE_FVERIFY(pTinOperand != nullptr, "unknown type in unary operation"))
		{
			if (pTinOperand->m_tink == TINK_Literal)
			{
				// this needs to be explicitly handled, create a new literal with the result
				if (MOE_FVERIFY(
						pStnod->m_pTin == nullptr, 
						"TypeInfoLiteral should be constructed during type checking"))
				{
					// NOTE: This computes the proper value and type, but will not collapse the AST
					//  The codegen pass will stop recursing when it gets to this finalized literal type

					SymbolTable * pSymtab = pTcsentTop->m_pSymtab;
					TypeInfoLiteral * pTinReturn;
					TypeInfoLiteral * pTinlitOperand = nullptr;
					if (FComputeUnaryOpOnLiteral(
							pTcctx,
							pStop,
							pSymtab,
							pStnodOperand,
							&pTinlitOperand,
							&pTinReturn))
					{
						pStnod->m_pTin = pTinReturn;
					}
					else
					{
						EmitError(pTcctx, pStnod->m_lexsp, ERRID_InvalidUnaryOp, 
							"invalid unary operand %s for %s literal", 
							PChzFromTok(pStnod->m_tok),
							PChzFromLitk(((TypeInfoLiteral *)pTinOperand)->m_litty.m_litk));
						return TCRET_StoppingError;
					}
				}
			}
			else
			{
				TOK tok = pStnod->m_tok;
				switch ((u32)tok)
				{
				case TOK_Dereference:
					{
						if (pTinOperand->m_tink != TINK_Pointer)
						{
							auto istrOp = IstrFromTypeInfo(pTinOperand);
							EmitError(pTcctx, pStnod->m_lexsp, ERRID_CannotDereference, "Cannot dereference type %s", istrOp.PChz());
							return TCRET_StoppingError;
						}
						else
						{
							TypeInfoPointer * pTinptr = (TypeInfoPointer *)pTinOperand;
							pStnod->m_pTin = pTinptr->m_pTin;
						}
					}break;
				case TOK_Reference:
					{
						// NOTE: TINK cannot be a literal - handled above...
						// NOTE: Can take a reference if we have a symbol that is not an enum or procedure
						//  definition, but need to walk past member lookups 
						
						// Need a better method for this - this fails in lots of different ways

						bool fCanTakeReference = false;
						auto pStnodMember = pStnodOperand;

						while (1)
						{
							if (!MOE_FVERIFY(pStnodMember, "bad member lookup child"))
								break;

							if (pStnodMember->m_park == PARK_MemberLookup)
							{
								TypeInfoEnum * pTinenum = nullptr;
								auto pStnodLhs = pStnodMember->PStnodChildSafe(0);
								if (pStnodLhs)
								{
									pTinenum = PTinRtiCast<TypeInfoEnum *>(pStnodLhs->m_pTin);
								}

								if (pTinenum && pTinenum->m_enumk == ENUMK_FlagEnum)
								{
									char aCh[2048];
									Moe::StringBuffer strbuf(aCh, MOE_DIM(aCh));
									WriteAstName(&strbuf, pStnodMember->PStnodChildSafe(0));
									AppendChz(&strbuf, ".");
									WriteAstName(&strbuf, pStnodMember->PStnodChildSafe(1));

									EmitError(pTcctx, pStnod->m_lexsp, ERRID_CannotTakeReference, 
										"Cannot take reference of enum_flag %s", (const char *)aCh);
									return TCRET_StoppingError;
								}
								pStnodMember = pStnodMember->PStnodChildSafe(1);
							}
							else if (pStnodMember->m_park == PARK_ArrayElement)
							{
								pStnodMember = pStnodMember->PStnodChildSafe(0);
							}
							else if (pStnodMember->m_park == PARK_Cast)
							{
								auto pStdecl = PStnodDerivedCast<STDecl*>(pStnodMember);
								pStnodMember = pStdecl->m_pStnodInit;
							}
							else
								break;
						}

						if (pStnodMember)
						{
							auto pSymMember = pStnodMember->PSym();
							if (pSymMember && pSymMember->m_pStnodDefinition)
							{
								PARK parkDefinition = pSymMember->m_pStnodDefinition->m_park;
								fCanTakeReference = (parkDefinition != PARK_ProcedureDefinition) | 
													(parkDefinition != PARK_EnumConstant);
							}
						}

						if (!fCanTakeReference)
						{
							auto istrOp = IstrFromTypeInfo(pTinOperand);
							EmitError(pTcctx, pStnod->m_lexsp, ERRID_CannotTakeReference, 
								"Cannot take reference of constant %s", istrOp.PChz());
							return TCRET_StoppingError;
						}

						SymbolTable * pSymtab = pTcsentTop->m_pSymtab;
						pStnod->m_pTin = pSymtab->PTinptrAllocate(pTinOperand);
					}break;

				case TOK('!'):
					{
						TypeInfo * pTinBool = pTcsentTop->m_pSymtab->PTinBuiltin(BuiltIn::g_istrBool);
						if (!MOE_FVERIFY(pTinBool, "missing bool type"))
							return TCRET_StoppingError;
						if (!FCanImplicitCast(pTinOperand, pTinBool))
						{
							auto istrOp = IstrFromTypeInfo(pTinOperand);
							EmitError(pTcctx, pStnod->m_lexsp, ERRID_CannotConvertToBool, "Cannot convert type %s to bool", istrOp.PChz());
						}

						pStop->m_pTin = pTinBool;
						pStop->m_optype.m_pTinResult = pTinBool;
					}break;

				case TOK('~'):
				case TOK_PlusPlus:
				case TOK_MinusMinus:
				case TOK('+'):
				case TOK('-'):
					{
						TINK tinkOperand = pTinOperand->m_tink;
						auto pTinn = PTinRtiCast<TypeInfoNumeric*>(pTinOperand);
						bool fIsInteger = (pTinn && FIsInteger(pTinn->m_numk));
						bool fIsFloat = (pTinn && pTinn->m_numk == NUMK_Float);

						bool fIsBasicEnum = false;
						bool fIsFlagEnum = false;
						auto pTinenum = PTinRtiCast<TypeInfoEnum *>(pTinOperand);
						if (pTinenum)
						{
							fIsBasicEnum = pTinenum->m_enumk == ENUMK_Basic;
							fIsFlagEnum = pTinenum->m_enumk == ENUMK_FlagEnum;
						}

						if (tinkOperand == TINK_Literal && pStop->m_pStnodResult)
						{
							auto pTinlit = (TypeInfoLiteral *)pTinOperand;
							LITK litk = ((TypeInfoLiteral *)pTinOperand)->m_litty.m_litk;
							fIsInteger |= FIsInteger(pTinlit->m_litty.m_numk);
							fIsFloat |= pTinlit->m_litty.m_numk == NUMK_Float;

							MOE_ASSERT(litk != LITK_Enum || pTinenum, "literal without enum type?");
						}

						bool fIsValidPtrOp = ((tok == TOK_PlusPlus) | (tok == TOK_MinusMinus)) &
							(tinkOperand == TINK_Pointer);
						bool fIsValidFloatOp = ((tok == TOK('+')) | (tok == TOK('-')) | (tok == TOK_PlusPlus) | (tok == TOK_MinusMinus)) & 
												fIsFloat;
						bool fIsValidBasicEnumOp = ((tok == TOK_PlusPlus) | (tok == TOK_MinusMinus)) & fIsBasicEnum;
						bool fIsValidFlagEnumOp = (tok == TOK('~')) & fIsFlagEnum;
						bool fIsSupported = fIsInteger | fIsValidPtrOp | fIsValidFloatOp | fIsValidBasicEnumOp | fIsValidFlagEnumOp;

						// BB - we should be checking for negating a signed literal here, but we can't really
						//  do operations on literals until we know the resolved type
						//  (Otherwise ~1 will always resolve to a u64)

						pStnod->m_pTin = pTinOperand;
						if (!fIsSupported)
						{
							auto istrOp = IstrFromTypeInfo(pTinOperand);
							EmitError(pTcctx, pStnod->m_lexsp, ERRID_InvalidUnaryOp, "invalid unary operator for type %s", istrOp.PChz());
						}
						else
						{
							if (tok == TOK('-') && FIsInteger(pTinn->m_numk))
							{
								TypeInfoNumeric * pTinn = (TypeInfoNumeric*)pTinOperand;
								if (pTinn->m_numk != NUMK_SignedInt)
								{
									auto istrOp = IstrFromTypeInfo(pTinOperand);
									EmitError(pTcctx, pStnod->m_lexsp, ERRID_LiteralOutsideBounds,
										"negate operand not valid for unsigned type %s",
										istrOp.PChz());
								}
							}
						}
					}break;
				}
			}
		}
	}

	pStnod->m_strees = STREES_TypeChecked;
	PopTcsent(pTcctx, &pTcsentTop, pStnod);
	return TCRET_Continue;
}

inline bool FComputeBinaryOpOnLiterals(
	TypeCheckContext * pTcctx,
	STOperator * pStop,
	SymbolTable * pSymtab,
	STNode * pStnodLhs,
	STNode * pStnodRhs, 
	TypeInfoLiteral ** ppTinOperand,
	TypeInfoLiteral ** ppTinReturn,
	STValue ** ppStval)
{
	TypeInfo * pTinLhs = pStnodLhs->m_pTin;
	TypeInfo * pTinRhs = pStnodRhs->m_pTin;

	STValue * pStvalLhs = PStnodRtiCast<STValue*>(pStnodLhs);
	STValue * pStvalRhs = PStnodRtiCast<STValue*>(pStnodRhs);
	if ((pTinLhs->m_tink != TINK_Literal) | (pTinRhs->m_tink != TINK_Literal) |
		(pStvalLhs == nullptr) | (pStvalRhs == nullptr))
		return false;

	TypeInfoLiteral * pTinlitLhs = (TypeInfoLiteral *)pTinLhs;
	TypeInfoLiteral * pTinlitRhs = (TypeInfoLiteral *)pTinRhs;
	const LiteralType & littyLhs = pTinlitLhs->m_litty;
	const LiteralType & littyRhs = pTinlitRhs->m_litty;

	bool fLhsIsNumber = (littyLhs.m_litk == LITK_Numeric) | (littyLhs.m_litk == LITK_Bool) | (littyLhs.m_litk == LITK_Enum);
	bool fRhsIsNumber = (littyRhs.m_litk == LITK_Numeric) | (littyRhs.m_litk == LITK_Bool) | (littyRhs.m_litk == LITK_Enum);
	if ((fLhsIsNumber == false) | (fRhsIsNumber == false))
		return false;

	TOK tokOperator = pStop->m_tok;
	bool fIsBoolOp = FDoesOperatorReturnBool(pStop->m_park);

	// NOTE: the *RIGHT* thing to do here is to use arbitrary precision floats, otherwise we'll lose some
	//  precision if the constants are ever turned into float before assignment

	// if lhs or rhs are float, upcast to float
	if ((littyLhs.m_numk == NUMK_Float) | (littyRhs.m_numk == NUMK_Float))
	{
		f64 g;
		bool f;
		f64 gLhs = GLiteralCast(pStvalLhs);
		f64 gRhs = GLiteralCast(pStvalRhs);
		switch ((u32)tokOperator)
		{
		case TOK('+'):			g = gLhs + gRhs; break;
		case TOK('-'):			g = gLhs - gRhs; break;
		case TOK('*'):			g = gLhs * gRhs; break;
		case TOK('/'):			g = gLhs / gRhs; break;
		case TOK('>'):			f = gLhs > gRhs; break;
		case TOK('<'):			f = gLhs < gRhs; break;
		case TOK_EqualEqual:	f = gLhs == gRhs; break;
		case TOK_NotEqual:		f = gLhs != gRhs; break;
		case TOK_LessEqual:		f = gLhs <= gRhs; break;
		case TOK_GreaterEqual:	f = gLhs >= gRhs; break;
		case TOK_AndAnd:		f = (gLhs != 0.0f) && (gRhs != 0.0f); break;
		case TOK_OrOr:			f = (gLhs != 0.0f) || (gRhs != 0.0f); break;
		default: return false;
		}

		STValue * pStvalResult =  PStvalAllocAfterParse(pSymtab->m_pAlloc, PARK_Literal, pStop->m_lexsp);
		pStop->m_pStnodResult = pStvalResult;

		if (fIsBoolOp)
		{
			pStvalResult->SetBool(f);
	
			TypeInfoLiteral * pTinBool = pSymtab->PTinlitFromLitk(LITK_Bool);
			MOE_ASSERT(!pTinBool || pTinBool->m_tink == TINK_Literal, "expected literal type");

			*ppTinReturn = pTinBool;
			*ppTinOperand = pTinlitLhs;
		}
		else
		{
			pStvalResult->SetF64(g);

			auto pTinlitFloat = (pTinlitLhs->m_litty.m_numk == NUMK_Float) ? pTinlitLhs : pTinlitRhs;
			*ppTinReturn = pTinlitFloat;
			*ppTinOperand = pTinlitFloat;
		}
		return true;
	} 
	else // both LITK_Integer
	{
		// may not be unsized literal in compound expressions, ie a == b == c
		// turns into (a == b) == c which ends up being bool == unsized comparison

		BigInt bintLhs(BintFromStval(pStvalLhs));
		BigInt bintRhs(BintFromStval(pStvalRhs));

		BigInt bintOut;
		bool f;
		switch ((u32)tokOperator)
		{
		case TOK('+'):			bintOut = BintAdd(bintLhs, bintRhs); break;
		case TOK('-'):			bintOut = BintSub(bintLhs, bintRhs); break;
		case TOK('*'):			bintOut = BintMul(bintLhs, bintRhs); break;
		case TOK('/'):			bintOut = BintDiv(bintLhs, bintRhs); break;
		case TOK('%'):			bintOut = BintRemainder(bintLhs, bintRhs); break;
		case TOK('|'):			bintOut = BintBitwiseOr(bintLhs, bintRhs); break;
		case TOK('&'):			bintOut = BintBitwiseAnd(bintLhs, bintRhs); break;
		case TOK('^'):			bintOut = BintBitwiseXor(bintLhs, bintRhs); break;
		case TOK_ShiftRight:	bintOut = BintShiftRight(bintLhs, bintRhs); break;
		case TOK_ShiftLeft:		bintOut = BintShiftLeft(bintLhs, bintRhs); break;
		case TOK('>'):			f = bintLhs > bintRhs; break;
		case TOK('<'):			f = bintLhs < bintRhs; break;
		case TOK_EqualEqual:	f = bintLhs == bintRhs; break;
		case TOK_NotEqual:		f = bintLhs != bintRhs; break;
		case TOK_LessEqual:		f = bintLhs <= bintRhs; break;
		case TOK_GreaterEqual:	f = bintLhs >= bintRhs; break;
		case TOK_AndAnd:	
			{
				f = bintLhs.m_nAbs && bintRhs.m_nAbs; 
			}	break;
		case TOK_OrOr:			f = bintLhs.m_nAbs || bintRhs.m_nAbs; break;
		default: return false;
		}

		STValue * pStvalResult =  PStvalAllocAfterParse(pSymtab->m_pAlloc, PARK_Literal, pStop->m_lexsp);
		pStop->m_pStnodResult = pStvalResult;

		if (fIsBoolOp)
		{
			pStvalResult->SetBool(f);
			TypeInfoLiteral * pTinlitBool = pSymtab->PTinlitFromLitk(LITK_Bool);
			MOE_ASSERT(!pTinlitBool || pTinlitBool->m_tink == TINK_Literal, "expected literal type");

			*ppTinReturn = pTinlitBool;
			*ppTinOperand = pTinlitLhs;
		}
		else
		{
			SetIntegerValue(pTcctx, pStvalResult, bintOut);

			if (pTinlitLhs->m_litty.m_litk == LITK_Enum)
			{
#if 1
				// BB - Is this supposed to end up being the loose type? if so we should find it from the enum rather than 
				//  making a non-unique integer literal type
				auto pTinlitInt = PTinRtiCast<TypeInfoLiteral *>(pStop->m_pTin);
				MOE_ASSERT(pTinlitInt, "expected enum literal");
#else // pre MOEB
				// We need to make an unfinalized integer literal
				auto pTinlitInt = MOE_NEW(pSymtab->m_pAlloc, TypeInfoLiteral) TypeInfoLiteral();
				pTinlitInt->m_litty.m_litk = LITK_Integer;
				pSymtab->AddManagedTin(pTinlitInt);
#endif

				*ppTinReturn = pTinlitInt;
				*ppTinOperand = pTinlitLhs;
			}
			else
			{
				MOE_ASSERT(pTinlitLhs->m_litty.m_litk == LITK_Numeric || pTinlitLhs->m_litty.m_litk == LITK_Bool, "unexpected literal kind");
				*ppTinReturn = pTinlitLhs;
				*ppTinOperand = pTinlitLhs;
			}
		}
		return true;
	}
}

TcretDebug TcretCheckBinaryOp(STNode * pStnod, TypeCheckContext * pTcctx, TypeCheckStackEntry * pTcsentTop)
{
	if (pTcsentTop->m_nState < pStnod->CPStnodChild())
	{
		(void) PTcsentPush(pTcctx, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
		return TCRET_Continue;
	}

	auto pStop = PStnodRtiCast<STOperator*>(pStnod);
	if (MOE_FVERIFY(pStop, "expected binary operator"))
		return TCRET_StoppingError;

	STNode * pStnodLhs = pStop->m_pStnodLhs;
	STNode * pStnodRhs = pStop->m_pStnodRhs;
	if (!MOE_FVERIFY(pStnodLhs && pStnodRhs, "expected two operands to binary ops"))
		return TCRET_StoppingError;

	TypeInfo * pTinLhs = pStnodLhs->m_pTin;
	TypeInfo * pTinRhs = pStnodRhs->m_pTin;

	if (!FVerifyIvalk(pTcctx, pStnodLhs, IVALK_RValue) || !FVerifyIvalk(pTcctx, pStnodRhs, IVALK_RValue))
		return TCRET_StoppingError;

	if (!MOE_FVERIFY((pTinLhs != nullptr) & (pTinRhs != nullptr), "unknown type in binary operation"))
		return TCRET_StoppingError;

	SymbolTable * pSymtab = pTcsentTop->m_pSymtab;
	if ((pTinLhs->m_tink == TINK_Literal) & (pTinRhs->m_tink == TINK_Literal))
	{
		// this needs to be explicitly handled, create a new literal with the result
		if (MOE_FVERIFY(
				pStnod->m_pTin == nullptr, 
				"TypeInfoLiteral should be constructed during type checking"))
		{
			// NOTE: this is only finding the type info for the result of our binary op
			//  it will be used for type inference from a literal, but this doesn't collapse
			//  the operator into a constant. (yet)

			TypeInfoLiteral * pTinReturn;
			TypeInfoLiteral * pTinOperand;
			STValue * pStval;
			if (FComputeBinaryOpOnLiterals(
					pTcctx,
					pStop,
					pSymtab,
					pStnodLhs,
					pStnodRhs,
					&pTinOperand,
					&pTinReturn,
					&pStval))
			{
				pStnod->m_pTin = pTinReturn;
				pStop->m_optype = OpTypes(pTinOperand, pTinOperand, pTinReturn);
			}
			else
			{
				EmitError(pTcctx, pStnod->m_lexsp, ERRID_OperatorNotDefined,
					"invalid operation %s for %s literal and %s literal", 
					PChzFromTok(pStnod->m_tok),
					PChzFromLitk(((TypeInfoLiteral *)pTinLhs)->m_litty.m_litk),
					PChzFromLitk(((TypeInfoLiteral *)pTinRhs)->m_litty.m_litk));
				return TCRET_StoppingError;
			}
		}
	}
	else
	{
		ProcMatchParam pmparam(pTcctx->m_pAlloc, pStnod->m_lexsp);
		pmparam.m_cpStnodCall = 2; //pStnod->CPStnodChild();
		pmparam.m_ppStnodCall = (pmparam.m_cpStnodCall) ? pStnod->m_apStnodChild : nullptr;

		OverloadCheck ovcheck = OvcheckTryCheckOverload(pTcctx, pStnod, &pmparam);
		if (ovcheck.m_pTinproc)
		{
			if (ovcheck.m_tcret != TCRET_Complete)
				return ovcheck.m_tcret;

			//AllocateOptype(pStnod);

			TypeInfoProcedure * pTinproc = ovcheck.m_pTinproc;
			if (MOE_FVERIFY(pTinproc->m_arypTinParams.C() == 2 && pTinproc->m_arypTinReturns.C() == 1, "bad operator overload signature"))
			{
				pStop->m_optype.m_pTinLhs = pTinproc->m_arypTinParams[0];
				pStop->m_optype.m_pTinRhs = pTinproc->m_arypTinParams[1];
				pStop->m_optype.m_pTinResult = pTinproc->m_arypTinReturns[0];
			}

			pStop->m_optype.m_pTinprocOverload = ovcheck.m_pTinproc;
			pStop->m_pTin = pStop->m_optype.m_pTinResult;

			if (ovcheck.m_argord == ARGORD_Reversed)
			{
				OpTypes * pOptype = &pStop->m_optype;
				auto pTin = pOptype->m_pTinLhs;

				pStop->m_grfstnod.AddFlags(FSTNOD_CommutativeCall);
				pOptype->m_pTinLhs = pOptype->m_pTinRhs;
				pOptype->m_pTinRhs = pTin;
			}
		}

		if (!pStop->m_optype.FIsValid())
		{
			PARK park = pStnod->m_park;
			TypeInfo * pTinUpcastLhs = PTinPromoteUntypedRvalueTightest(pTcctx, pSymtab, pStnodLhs, pTinRhs);
			TypeInfo * pTinUpcastRhs = PTinPromoteUntypedRvalueTightest(pTcctx, pSymtab, pStnodRhs, pTinLhs);

			OpTypes optype = OptypeFromPark(pTcctx, pSymtab, pStop->m_tok, park, pTinUpcastLhs, pTinUpcastRhs);

			if (!optype.FIsValid() || !FDoesOperatorExist(pStnod->m_tok, &optype))
			{
				InString istrLhs = IstrFromTypeInfo(pTinLhs);
				InString istrRhs = IstrFromTypeInfo(pTinRhs);
				EmitError(pTcctx, pStop->m_lexsp, ERRID_OperatorNotDefined,
					"%s operator not defined for %s and %s",
					PChzFromTok(pStnod->m_tok),
					istrLhs.PChz(),
					istrRhs.PChz());
				return TCRET_StoppingError;
			}
			pStop->m_optype = optype;
		}

		pStop->m_pTin = pStop->m_optype.m_pTinResult;

		FinalizeLiteralType(pTcctx, pTcsentTop->m_pSymtab, pStop->m_optype.m_pTinLhs, pStnodLhs);
		FinalizeLiteralType(pTcctx, pTcsentTop->m_pSymtab, pStop->m_optype.m_pTinRhs, pStnodRhs);
	}

	pStop->m_strees = STREES_TypeChecked;
	PopTcsent(pTcctx, &pTcsentTop, pStop);
	return TCRET_Continue;
}

TcretDebug TcretCheckList(STNode * pStnod, TypeCheckContext * pTcctx, TypeCheckStackEntry * pTcsentTop)
{
	if (pTcsentTop->m_nState >= pStnod->CPStnodChild())
	{
		if (pStnod->m_park == PARK_List)
		{
			int cStnodChild = pStnod->CPStnodChild();
			for (int iStnod = 0; iStnod < cStnodChild; ++iStnod)
			{
				auto pStnodChild = pStnod->PStnodChild(iStnod);
				switch (pStnodChild->m_park)
				{
				case PARK_Identifier:
				case PARK_Cast:
				case PARK_Literal:
					{
						EmitError(pTcctx, pStnod->m_lexsp, ERRID_LhsHasNoEffect, 
							"%s is left hand side, has no effect",
							PChzLongFromPark(pStnodChild->m_park));
					} break;
				default:
					break;
				}
			}
		}

		pStnod->m_strees = STREES_TypeChecked;
		PopTcsent(pTcctx, &pTcsentTop, pStnod);
		return TCRET_Continue;
	}

	auto pTcsentPushed = PTcsentPush(pTcctx, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
	if (pTcsentPushed)
	{
		if (pStnod->m_park == PARK_List || pStnod->m_park == PARK_ExpressionList)
		{
			if (pStnod->m_pSymtab)
			{
				pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;
			}
		}
		if (pStnod->m_park == PARK_ParameterList)
		{
			pTcsentPushed->m_parkDeclContext = pStnod->m_park;
			pTcsentPushed->m_fAllowForwardDecl = true;
		}
		if (pStnod->m_park == PARK_ReferenceDecl)
		{
			pTcsentPushed->m_fAllowForwardDecl = true;
		}
	}

	return TCRET_Continue;
}

TcretDebug TcretCheckTypeArgument(STNode * pStnod, TypeCheckContext * pTcctx, TypeCheckStackEntry * pTcsentTop)
{
	if (pTcsentTop->m_nState < pStnod->CPStnodChild())
	{
		(void) PTcsentPush(pTcctx, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
		return TCRET_Continue;
	}

	auto pStnodType = pStnod->PStnodChildSafe(0);
	if (pStnodType)
	{
		// BB - should this change to a STypeIntoType struct with the child type?
		auto pSymtab = pTcsentTop->m_pSymtab;
		bool fIsValidTypeSpec;
		TypeInfo * pTinType = PTinFromTypeSpecification(
								pTcctx,
								pSymtab,
								pStnodType,
								pTcsentTop->m_grfsymlook,
								nullptr,
								&fIsValidTypeSpec);
		if (fIsValidTypeSpec)
		{
			MOE_ASSERT(!pStnodType->m_pTin || FTypesAreSame(pStnodType->m_pTin, pTinType), "expected same types");
			if (!pStnodType->m_pTin)
			{
				pStnodType->m_pTin = pTinType;
			}
		}
	}

	pStnod->m_strees = STREES_TypeChecked;
	PopTcsent(pTcctx, &pTcsentTop, pStnod);
	return TCRET_Continue;
}

TcretDebug TcretTypeCheckSubtree(TypeCheckContext * pTcctx)
{
	#define CONTINUE_OR_BREAK(tcret) if (tcret != TCRET_Continue) return tcret; break;

	TCRET tcret;

	CDynAry<TypeCheckStackEntry> * paryTcsent = &pTcctx->m_aryTcsent;
	while (paryTcsent->C())
	{
		TypeCheckStackEntry * pTcsentTop = paryTcsent->PLast();
		STNode * pStnod = pTcsentTop->m_pStnod;

		switch (pStnod->m_park)
		{
		case PARK_ProcedureDefinition:
			tcret = TcretCheckProcedureDef(pStnod, pTcctx, pTcsentTop);
			CONTINUE_OR_BREAK(tcret);

		case PARK_StructDefinition:
			tcret = TcretCheckStructDef(pStnod, pTcctx, pTcsentTop);
			CONTINUE_OR_BREAK(tcret);

		case PARK_Decl:
			tcret = TcretCheckDecl(pStnod, pTcctx, pTcsentTop);
			CONTINUE_OR_BREAK(tcret);

		case PARK_ConstantDecl:
			tcret = TcretCheckConstantDecl(pStnod, pTcctx, pTcsentTop);
			CONTINUE_OR_BREAK(tcret);

		case PARK_Identifier:
			tcret = TcretCheckIdentifier(pStnod, pTcctx, pTcsentTop);
			CONTINUE_OR_BREAK(tcret);

		case PARK_Literal:
			tcret = TcretCheckLiteral(pStnod, pTcctx, pTcsentTop);
			CONTINUE_OR_BREAK(tcret);

		case PARK_ReservedWord:
			tcret = TcretCheckReservedWord(pStnod, pTcctx, pTcsentTop);
			CONTINUE_OR_BREAK(tcret);

		case PARK_PostfixUnaryOp:
		case PARK_UnaryOp:
			tcret = TcretCheckUnaryOp(pStnod, pTcctx, pTcsentTop);
			CONTINUE_OR_BREAK(tcret);

		case PARK_AdditiveOp:
		case PARK_MultiplicativeOp:
		case PARK_ShiftOp:
		case PARK_RelationalOp:
		case PARK_LogicalAndOrOp:
			tcret = TcretCheckBinaryOp(pStnod, pTcctx, pTcsentTop);
			CONTINUE_OR_BREAK(tcret);

		case PARK_ArrayDecl:
		case PARK_ReferenceDecl:
		case PARK_QualifierDecl:
		case PARK_ParameterList:
		case PARK_List:
		case PARK_ExpressionList:
			tcret = TcretCheckList(pStnod, pTcctx, pTcsentTop);
			CONTINUE_OR_BREAK(tcret);

		case PARK_TypeArgument:
			tcret = TcretCheckTypeArgument(pStnod, pTcctx, pTcsentTop);
			CONTINUE_OR_BREAK(tcret);

		case PARK_Uninitializer:
		case PARK_Nop:
		case PARK_VariadicArg:
			{
				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcctx, &pTcsentTop, pStnod);
			} break;

		default:
			MOE_ASSERT(false, "unknown parse kind (%s) encountered during type check", PChzLongFromPark(pStnod->m_park));
			break;
		}
	}
	return TCRET_Complete;

	#undef CONTINUE_OR_BREAK
}

#if MOEB_TCJOB
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
				PopTcsent(pTcctx, &pTcsentTop, nullptr);
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
				PopTcsent(pTcctx, &pTcsentTop, nullptr);
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
#endif

struct TypeCheckJobData // tag = tcjd
{
						TypeCheckJobData(Workspace * pWork, Alloc * pAlloc, WorkspaceEntry * pEntry)
						:m_tcctx(pWork, pAlloc, pEntry)
							{ ; }

	TypeCheckContext	m_tcctx;
};

JOBRET JobretExecuteTypeCheckJob(Compilation * pComp, Workspace * pWork, Job * pJob)
{
	auto pTcjd = (TypeCheckJobData *)pJob->m_pVData;

	TypeCheckContext * pTcctx = &pTcjd->m_tcctx;
	TCRET tcret = TcretTypeCheckSubtree(pTcctx);

	switch (tcret)
	{
	case TCRET_WaitingForSymbolDefinition:
		return JOBRET_Waiting;

	case TCRET_Complete:
		if (pJob->m_comphaseDesired > COMPHASE_TypeCheck)
		{
			// build codegen job
		}
		break;
	}

	return JOBRET_Complete;
}

void CleanupTypeCheckJob(Workspace * pWork, Job * pJob)
{
	if (pJob->m_pVData)
	{
		auto pTcjd = (TypeCheckJobData *)pJob->m_pVData;
		pWork->m_pAlloc->MOE_DELETE(pTcjd);
		pJob->m_pVData = nullptr;
	}
}

JobRef PJobCreateTypeCheckRequest(Compilation * pComp, Workspace * pWork, WorkspaceEntry * pEntry, Job * pJobSource)
{
	Alloc * pAlloc = pWork->m_pAlloc;
	TypeCheckJobData * pTcjd = MOE_NEW(pAlloc, TypeCheckJobData) TypeCheckJobData(pWork, pAlloc, pEntry);

	// job that waits for parsing and adds type check exec jobs to the master job when ready
	auto pJobTc = PJobAllocate(pComp, pTcjd, pJobSource);
	pJobTc->m_pFnUpdate = JobretExecuteTypeCheckJob;
	pJobTc->m_pFnCleanup = CleanupTypeCheckJob;

	TypeCheckContext * pTcctx = &pTcjd->m_tcctx;
	pTcctx->m_pJobTc = pJobTc;

	Symbol * pSymRoot = nullptr;
	// if we're in a unit test we spoof a top level implicit function symbol
	if (pWork->m_grfunt.FIsSet(FUNT_ImplicitProc))
	{
		pSymRoot = pWork->m_pSymtab->PSymEnsure(pWork->m_pErrman, IstrIntern("__ImplicitMethod"), nullptr);
	}

	MOE_ASSERT(pEntry->m_pSymtab, "entry point without symbol table");

	pTcctx->m_aryTcsent.SetAlloc(pAlloc, Moe::BK_TypeCheckStack);
	TypeCheckStackEntry * pTcsent = pTcctx->m_aryTcsent.AppendNew();
	pTcsent->m_nState = 0;
	pTcsent->m_pStnod = pEntry->m_pStnod;
	pTcsent->m_pSymtab = pEntry->m_pSymtab;
	pTcsent->m_pStnodProcedure = nullptr;
	pTcsent->m_pSymContext = pSymRoot;
	pTcsent->m_grfsymlook = FSYMLOOK_Default;
	pTcsent->m_parkDeclContext = PARK_Nil;
	pTcsent->m_fAllowForwardDecl = false;
	pTcsent->m_tcctx = TCCTX_Normal;

	pTcctx->m_pEntry = pEntry;

	JobRef pJobReturn = pJobTc;
	EnqueueJob(pComp, pJobTc);
	return pJobReturn;
}

