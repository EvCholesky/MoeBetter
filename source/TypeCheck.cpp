#include "Generics.inl"
#include "Parser.h"
#include "TypeInfo.h"
#include "Workspace.h"

#include <stdio.h>



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
,m_scopidNext(SCOPID_Min)
{
}

void TypeRegistry::Clear()
{
	m_scopidNext = SCOPID_Min;
	m_hashHvPTinUnique.Clear(0);
}

#ifdef MOEB_LATER
static inline u64 HvForPTin(STypeInfo * pTin, TINK tink, u8 other = 0)
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
	MOE_ASSERT(false, "type registry TBD");
	return nullptr;
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
			auto pTinptr = (STypeInfoPointer *)pTin;
			auto pTinPointedTo = PTinMakeUnique(pTinptr->m_pTinPointedTo);
			pTinptr->m_pTinPointedTo = pTinPointedTo;

			hv = HvForPTin(pTinPointedTo, TINK_Pointer);
		} break;
	case TINK_Qualifier:
		{
			auto pTinqual = (STypeInfoQualifier *)pTin;
			auto pTinUnqual = PTinMakeUnique(pTinqual->m_pTin);
			pTinqual->m_pTin = pTinUnqual;

			hv = HvForPTin(pTinUnqual, TINK_Qualifier, pTinqual->m_grfqualk.m_raw);
		} break;
	case TINK_Array:
		{
			auto pTinary = (STypeInfoArray *)pTin;
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
			Moe::SStringEditBuffer seb(m_pAlloc);

			seb.Clear();
			AppendTypeDescriptor(pTin, &seb);
			hv = HvForPTin(pTin->m_strDesc, pTin->m_tink);

		} break;

	case TINK_Procedure:
	case TINK_Struct:
		{
			Moe::SStringEditBuffer seb(m_pAlloc);

			seb.Clear();
			AppendTypeDescriptor(pTin, &seb);
			hv = HvForPTin(pTin->m_strDesc, pTin->m_tink);
		} break;
	default:
		MOE_ASSERT(false, "unhandled type kind");
		break;
	}

	STypeInfo ** ppTin;
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
				auto pTinptr = (STypeInfoPointer *)pTin;
				pTinptr->m_pTinPointedTo = PTinMakeUnique(pTinptr->m_pTinPointedTo);
			} break;
		case TINK_Qualifier:
			{
				auto pTinqual = (STypeInfoQualifier *)pTin;
				pTinqual->m_pTin = PTinMakeUnique(pTinqual->m_pTin);
			} break;
		case TINK_Array:
			{
				auto pTinary = (STypeInfoArray *)pTin;
				pTinary->m_pTin = PTinMakeUnique(pTinary->m_pTin);
			} break;
		case TINK_Procedure:
			{
				auto pTinproc = (STypeInfoProcedure *)pTin;
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
				auto pTinstruct = (STypeInfoStruct *)pTin;
			} break;

		default:
			break;
		}
	}
	return pTin;
#endif // MOEB_LATER
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

TcretDebug TcretTypeCheckSubtree(TypeCheckWorkspace * pTcwork, TypeCheckFrame * pTcfram)
{
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
