#include "Generics.inl"
#include "Parser.h"
#include "TypeInfo.h"
#include "Workspace.h"

#include <stdio.h>



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

void OnTypeResolve(TypeCheckWorkspace * pTcwork, const Symbol * pSym);
void FinalizeLiteralType(TypeCheckWorkspace * pTcwork, SymbolTable * pSymtab, TypeInfo * pTinDst, STNode * pStnodLit);

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
	const char * pCozIt = *ppChz;
	while (*pChzRefIt != '\0')
	{
		if (*pChzRefIt != *pCozIt)
			return false;
		++pChzRefIt;
		++pCozIt;
	}

	*ppChz = pCozIt;
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
			return pStvalA->m_istr == pStvalB->m_istr;
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

void GenericRegistry::Cleanup()
{
	Moe::CHash<STNode *, EntryBlock *>::CIterator iter(&m_mpStnodInstFromBlock);

	EntryBlock ** ppBlock;
	while (ppBlock = iter.Next())
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
				EmitError(pTcwork, pStval->m_lexsp, ERRID_InvalidCast, "Implicit cast will discard negative value");
			}
			return (u64)pStval->m_nSigned;
		}
	case STVALK_String:
		{
			if (pStval->m_istr == RWord::g_istrLineDirective)
			{
				MOE_ASSERT(false, "MOEB_LATER - this needs to be fixed, line directive has same no numeric value");
				//return pStval->m_nUnsigned;
				return 0;
			}
			else if (pStval->m_istr == RWord::g_istrTrue)
				return 1;
			else if (pStval->m_istr == RWord::g_istrFalse)
				return 0;

		} break;
	default:
		break;
	}

	MOE_ASSERT(false, "bad literal cast to unsigned int");
	return 0;
}

inline s64 NSignedLiteralCast(TypeCheckWorkspace * pTcwork, const STValue * pStval)
{
	switch (pStval->m_stvalk)
	{
	case STVALK_UnsignedInt:
		{
			if (pStval->m_nUnsigned > LLONG_MAX)
			{
				EmitError(pTcwork, pStval->m_lexsp, ERRID_LitOverflow, 
					"Literal '" MOE_U64FMT "' is too large for implicit signed int cast.", pStval->m_nUnsigned);
			}
			return (s64)pStval->m_nUnsigned;
		}
	case STVALK_SignedInt:
		return pStval->m_nSigned;
	case STVALK_Float:
		return (s64)pStval->m_g;
	case STVALK_String:
		{
			if (pStval->m_istr == RWord::g_istrLineDirective)
			{
				MOE_ASSERT(false, "MOEB_LATER - this needs to be fixed, line directive has same no numeric value");
				//return pStval->m_nUnsigned;
				return 0;
			}
			else if (pStval->m_istr == RWord::g_istrTrue)
				return 1;
			else if (pStval->m_istr == RWord::g_istrFalse)
				return 0;

		} break;
	default:
		break;
	}

	MOE_ASSERT(false, "bad literal cast to signed int");
	return 0;
}

inline f64 GLiteralCast(const STValue * pStval)
{
	switch (pStval->m_stvalk)
	{
	case STVALK_UnsignedInt:	return (f64)pStval->m_nUnsigned;
	case STVALK_SignedInt:		return (f64)pStval->m_nSigned;
	case STVALK_Float:			return pStval->m_g;
	default: MOE_ASSERT(false, "expected number");
	}
	return 0.0;
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

static inline bool FIsMutableType(TypeInfo * pTin)
{
	auto pTinqual = PTinRtiCast<TypeInfoQualifier *>(pTin);
	if (pTinqual)
	{
		return !pTinqual->m_grfqualk.FIsAnySet(FQUALK_Const | FQUALK_InArg);
	}
	return !pTin || pTin->m_tink != TINK_Literal;
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
						QualkFromRword(pStval->m_istr);
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
