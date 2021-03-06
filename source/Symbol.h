/* Copyrigh (C) 2019 Evan Christensen
|
| Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated 
| documentation files (the "Software"), to deal in the Software without restriction, including without limitation the 
| rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
| persons to whom the Software is furnished to do so, subject to the following conditions:
| 
| The above copyright notice and this permission notice shall be included in all copies or substantial portions of the 
| Software.
| 
| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE 
| WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
| COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR 
| OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. */
#pragma once

#include "Lexer.h"
#include "MoeArray.h"
#include "MoeTypes.h"

struct ErrorManager;
struct STEnum;
struct STNode;
struct STStruct;
struct TypeInfo;
struct TypeRegistry;
struct UniqueNameSet;
struct Workspace;

namespace Moe
{
	struct InString;
}

enum FSYM		// SYMbol flags
{
	FSYM_None				= 0x0,
	FSYM_IsBuiltIn			= 0x1,
	FSYM_IsType				= 0x2,	// this is a type declaration (if not set this is a named instance)
	FSYM_VisibleWhenNested	= 0x4,	// types, constants and procedures that are visible in a more deeply nested symbol table
									// - ie. not an instance. Nested procedure should be able to call peer procedure, but not
									//   access variable from parent procedure.
	FSYM_InternalUseOnly	= 0x8,	// type should not be available for user code declarations, just internal compiler use (ie _flag)

	FSYM_All				= 0xF,
};
MOE_DEFINE_GRF(GRFSYM, FSYM, u32);

enum FSYMLOOK	// SYMbol LOOKup flags
{
	FSYMLOOK_None			= 0x0,
	FSYMLOOK_Local			= 0x1,
	FSYMLOOK_Ancestors		= 0x2,
	FSYMLOOK_IgnoreOrder	= 0x4,

	FSYMLOOK_All			= 0x7,
	FSYMLOOK_Default		= FSYMLOOK_Local | FSYMLOOK_Ancestors,
};

MOE_DEFINE_GRF(GRFSYMLOOK, FSYMLOOK, u32);


enum SYMDEP		// SYMbol DEPendency 
{
							// NIL = Haven't determined if used
	SYMDEP_Unused,			// Symbol is not referenced by any live code - no need to codeGen
	SYMDEP_Used,			// Referenced by live code 
	SYMDEP_PublicLinkage,	// public linkage procedures will be considered used during symbol dependency determination

	MOE_MAX_MIN_NIL(SYMDEP)
};

enum SYMK
{
	SYMK_Symbol,
	SYMK_Path,

	MOE_MAX_MIN_NIL(SYMK)
};

enum FSHADOW
{
	FSHADOW_NoShadowing,
	FSHADOW_ShadowingAllowed,
};

struct SymbolBase // tag = symbase
{
	SYMK					m_symk;
};


struct Symbol : public SymbolBase	// tag = sym
{
	GRFSYM					m_grfsym;
	SYMDEP					m_symdep;
	Moe::InString			m_istrName;
	STNode *				m_pStnodDefinition;

//	TypeInfo *				m_pTin; - why is this here, rather than pStnodDefinition->m_pTin
	Symbol *				m_pSymPrev;		// list of shadowed symbols in reverse lexical order. 

	Moe::CDynAry<Symbol *>	m_aryPSymReferencedBy;
	Moe::CDynAry<Symbol *>	m_aryPSymHasRefTo;			// this symbol has a reference to all of these symbols

	void					AssertIsValid();
};

const TypeInfo * PTinFromSymbol(const Symbol * pSym);
LexSpan LexspFromSym(const Symbol * pSym);

// symbol path for 'using' aliases (used during codegen to reconstruct offsets)

struct SymbolPath : public SymbolBase // tag = symp
{
	Moe::CDynAry<Symbol *>	m_arypSym;					// implicit references followed by actual symbol  
														// foo.m_x is implicitly foo.m_mid.m_base.m_x  [m_mid, m_base, m_x]
};

struct SymbolTable		// tag = symtab
{
protected:
	friend SymbolTable * PSymtabNew(Moe::Alloc *, SymbolTable *, const Moe::InString &, TypeRegistry *, UniqueNameSet *);

							// protected constructor to force use of CWorkspace::PSymtabNew()
							SymbolTable(
								const Moe::InString & istrNamespace,
								Moe::Alloc * pAlloc,
								TypeRegistry * pTyper,
								UniqueNameSet * pUnset)
							:m_istrNamespace(istrNamespace)
							,m_pAlloc(pAlloc)
							,m_hashIstrPSym(pAlloc, Moe::BK_Symbol)
							,m_hashIstrPTinBuiltIn(pAlloc, Moe::BK_Symbol)
							,m_hashIstrPStnodBuiltIn(pAlloc, Moe::BK_Symbol)
							,m_arypTinManaged(pAlloc, Moe::BK_Symbol)
							,m_arypGenmapManaged(pAlloc, Moe::BK_Symbol)
							,m_arypSymGenerics(pAlloc, Moe::BK_Symbol)	
							,m_aryUsing(pAlloc, Moe::BK_Symbol)
							,m_arypSymtabUsedBy(pAlloc, Moe::BK_Symbol)
							,m_pTyper(pTyper)
							,m_pUnset(pUnset)
							,m_pSymtabParent(nullptr)
							,m_pSymtabNextManaged(nullptr)
							,m_iNestingDepth(0)
							,m_scopid(pTyper->ScopidAlloc())
							,m_nVisitId(0)
								{ ; }

public:
	struct SymbolIterator // tag = symiter
	{
	public:
							SymbolIterator()
							:m_pSymtab(nullptr)
							,m_pSym(nullptr)
							,m_grfsymlook(FSYMLOOK_Default)
								{ ; }

							SymbolIterator(
								SymbolTable * pSymtab,
								const Moe::InString & istr,
								const LexSpan & lexspan,
								GRFSYMLOOK grfsymlook);

		Symbol *			PSymNext();
		bool				FIsDone() const
								{ return (m_pSymtab == nullptr) | (m_pSym == nullptr); }

		SymbolTable *		m_pSymtab;
		Symbol *			m_pSym;
		LexSpan				m_lexsp;
		GRFSYMLOOK			m_grfsymlook;
	};

	struct SUsing // tag = using
	{
										SUsing()
										:m_pSymtab(nullptr)
										,m_pStnod(nullptr)
										,m_hashIstrPSymp()
											{ ; }

										~SUsing();

		SymbolTable *					m_pSymtab;
		STNode *						m_pStnod;

		Moe::CHash<Moe::InString, SymbolPath *>	
										m_hashIstrPSymp;			// a cache of the symbol paths found from this 
																// 'using' statement. Added lazily
	};

							~SymbolTable();

	void					AddBuiltInSymbols(Workspace * pWork);
	Symbol *				PSymEnsure(
								ErrorManager * pErrman,
								const Moe::InString & istrName,
								STNode * pStnodDefinition,
								GRFSYM grfsym = FSYM_None, 
	 							FSHADOW fshadow = FSHADOW_ShadowingAllowed);

	void					AddUsingScope(
								ErrorManager * pErrman,
								SymbolTable * pSymtabNew,
								STNode * pStnodSource
							);

	Symbol *				PSymNewUnmanaged(const Moe::InString & istrName, STNode * pStnodDefinition, GRFSYM grfsym);
	Symbol * 				PSymGenericInstantiate(Symbol * pSym, STNode * pStnodDefinition);

	Symbol *				PSymLookup(
								Moe::InString istr,
								const LexSpan & lexsp, 
								GRFSYMLOOK grfsymlook = FSYMLOOK_Default,
								SymbolTable ** ppSymtabOut = nullptr);

	const TypeInfo *		PTinBuiltin(const Moe::InString & istr);
	TypeInfoQualifier *		PTinqualBuiltinConst(const Moe::InString & istr)
								{
									return PTinqualEnsureIfNotNull(PTinBuiltin(istr), FQUALK_Const);
								}

	TypeInfoLiteral *		PTinlitFromLitk(LITK litk);
	TypeInfoLiteral *		PTinlitFromLitk(LITK litk, int cBit, NUMK numk);
	TypeInfoLiteral *		PTinlitAllocUnfinal(STVALK stvalk);
	TypeInfoLiteral *		PTinlitAllocate(const TypeInfo * pTinSource, LITK litk, s8 cBit, NUMK numk, bool fIsFinalized, s64 c = -1);
	TypeInfoLiteral *		PTinlitAllocCompound(const TypeInfo * pTinSource, s64 cElement);
	TypeInfoLiteral *		PTinlitNull(const TypeInfoPointer	 * pTinSource);

	TypeInfoPointer *		PTinptrAllocate(const TypeInfo * pTinPointedTo, bool fIsImplicitRef = false);

	TypeInfoQualifier *		PTinqualEnsure(const TypeInfo * pTinTarget, GRFQUALK grfqualk);
	TypeInfoQualifier *		PTinqualEnsureIfNotNull(const TypeInfo * pTinTarget, GRFQUALK grfqualk);

	TypeInfoEnum *			PTinenumAllocate(Moe::InString istrName, int cConstant, ENUMK enumk, STEnum * pStenumDef);
	TypeInfoProcedure *		PTinprocAllocate(Moe::InString istrName, 
								SCOPID scopid, 
								const ProcedureAttrib & procattrib, 
								const Moe::CAry<const TypeInfo *> & arypTinParam,
								const Moe::CAry<const TypeInfo *> & arypTinReturn,
								Moe::CAry<GRFPARMQ> * paryGrfparmq = nullptr);

	const TypeInfoStruct *	PTinstructAllocate(
								Moe::InString istrName, 
								SCOPID scopid,
								STNode * pStnodDefinition,
								const Moe::CAry<TypeStructMember> & aryTypemembField,
								const StructAttrib & structattrib,
								const TypeInfoStruct * pTinstructInstFrom = nullptr,
								GenericMap * pGenmap = nullptr);
	const TypeInfoStruct *  PTinstructEnsureCanon(const TypeInfoStruct * pTinstruct);

	TypeInfoArray *			PTinaryAllocate(const TypeInfo * pTinElement, ARYK aryk, s64 c, STNode * pStnodBakedDim = nullptr);
	TypeInfoArray *			PTinaryCopyWithNewElementType(const TypeInfoArray * pTinarySrc, const TypeInfo * pTinNew);

	template <typename T>
	T *						PTinMakeUnique(T * pTin)
								{ 
									return (T *)m_pTyper->PTinMakeUnique(pTin);
								}

	void					AddBuiltInType(ErrorManager * pErrman, Lexer * pLex, const TypeInfo * pTin, GRFSYM grfsym = FSYM_None);
	void					AddManagedTin(const TypeInfo * pTin);
	void					AddManagedSymtab(SymbolTable * pSymtab);

	void					PrintDump();

	Moe::InString						m_istrNamespace;		// unique name for this symbol table's scope
	Moe::Alloc *						m_pAlloc;
	Moe::CHash<Moe::InString, Symbol *>	m_hashIstrPSym;			// All the symbols defined within this scope, a full lookup requires
																//  walking up the parent list
	Moe::CHash<Moe::InString, const TypeInfo *>	
										m_hashIstrPTinBuiltIn;		// Builtin Types declared in this scope
	Moe::CHash<Moe::InString, STNode *>	
										m_hashIstrPStnodBuiltIn;	// stub AST nodes for Builtin Types declared in this scope
																	//  (these exist so that symbols can map to pTin via an AST node, and prevent 
																	//  having both pSym->m_pStnodDefinition and pSym->m_pTin that must agree on pTin

	Moe::CDynAry<const TypeInfo *>		m_arypTinManaged;		// all type info structs that need to be deleted.
	Moe::CDynAry<GenericMap *>			m_arypGenmapManaged;	// generic mappings used by instantiated generic structs
	Moe::CDynAry<Symbol *>				m_arypSymGenerics;		// symbol copies for generics, not mapped to an identifier
	Moe::CDynAry<SUsing>				m_aryUsing;				// symbol tables with members pushed into this table's scope
	Moe::CDynAry<SymbolTable *>			m_arypSymtabUsedBy;		// symbol tables that refer to this one via a using statement
	TypeRegistry *						m_pTyper;				// hashes to find unique type instances
	UniqueNameSet *						m_pUnset;				// set of unique names for implict names (created during parse)
	Moe::CDynAry<TypeInfoLiteral *>	
										m_mpLitkArypTinlit[LITK_Max];

	SymbolTable *						m_pSymtabParent;

	SymbolTable *						m_pSymtabNextManaged;	// next table in the global list
	s32									m_iNestingDepth;					
	SCOPID								m_scopid;				// unique scope id, for uniqueifying named types (unique = name+scopid)
	u64									m_nVisitId;				// id to check if this table has been visited during collision check
};

inline Symbol * PSymLast(SymbolBase * pSymbase)
{
	switch (pSymbase->m_symk)
	{
	case SYMK_Symbol:	return (Symbol *)pSymbase;
	case SYMK_Path:
		{
			auto pSymp = (SymbolPath *)pSymbase;
			if (pSymp->m_arypSym.FIsEmpty())
				return nullptr;

			return pSymp->m_arypSym.Last();
		}
	default: 
		MOE_ASSERT(false, "Unknown symbol kind");
		return nullptr;
	}
}


SymbolTable * PSymtabNew(Moe::Alloc * pAlloc, SymbolTable * pSymtabParent, const Moe::InString & istrNamespace, TypeRegistry * pTyper, UniqueNameSet * pUnsetTin);
SymbolTable * PSymtabNew(Moe::Alloc * pAlloc, SymbolTable * pSymtabParent, const Moe::InString & istrNamespace);
