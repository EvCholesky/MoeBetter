/* Copyright (C) 2019 Evan Christensen
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

// structures and methods dealing with generics

#include "Error.h"
#include "MoeTypes.h"
#include "MoeArray.h"
#include "MoeHash.h"
#include "MoeString.h"
#include "Lexer.h"

struct STNode;
struct SymbolTable;
struct Workspace;
struct Symbol;
struct TypeCheckContext;
struct TypeInfo;

enum GENK	// GENeric Kind
{
	GENK_Value,			// baked value (think integer array size)	($C: int)
	GENK_Type,			// generic type								(t: $T)
	MOE_MAX_MIN_NIL(GENK)
};

struct Anchor		// tag anc
{
					Anchor()
					:m_pStnodBaked(nullptr)
					,m_pTin(nullptr)
					,m_genk(GENK_Nil)
						{ ; }

	bool			FIsNull() const
						{ return m_pStnodBaked == nullptr && m_pTin == nullptr; }
	void			AssertIsValid() const
						{
							switch (m_genk)
							{
							case GENK_Nil:		MOE_ASSERT(FIsNull(), "should be null"); break;
							case GENK_Value:	MOE_ASSERT(m_pTin == nullptr, "unexpected type anchor"); break;
							case GENK_Type:		MOE_ASSERT(m_pStnodBaked == nullptr, "unexpected baked value"); break;
							default:			MOE_ASSERT(false, "unhandled generic kind"); break;
							}
						}


	STNode *		m_pStnodBaked;
	TypeInfo *		m_pTin;
	GENK			m_genk;
};

// A generic map is instructions for instantiating a generic procedure or struct. For a genmap to be complete it must anchor
//   all of the anchor points from the generic declaration, but they may be anchored to a different generic label

struct GenericMap // tag = genmap
{
							GenericMap(Moe::Alloc * pAlloc, const char * pChzPrefix, TypeInfo * pTinOwner)
							:m_istrName()	
							,m_mpIstrAnc(pAlloc, Moe::BK_TypeCheckGenerics)
							,m_aryPStnodManaged(pAlloc, Moe::BK_TypeCheckGenerics)
							,m_aryLexspSrc(pAlloc, Moe::BK_TypeCheckGenerics)
							,m_grftingenResult()
								{
#define BUILD_GENMAP_NAMES 1
#if BUILD_GENMAP_NAMES
									m_istrName = IstrInternCopy(pChzPrefix);
									if (pTinOwner)
									{
										char aCh[128];
										Moe::StringBuffer strbuf(aCh, MOE_DIM(aCh));
										FormatChz(&strbuf, "%s_%s", pTinOwner->m_istrName.m_pChz, pChzPrefix);

										m_istrName = IstrInternCopy(aCh);
									}
#endif 
								}

							~GenericMap()
								{
									MOE_ASSERT(m_aryPStnodManaged.C() == 0, "Generic map stnod list was not cleaned up");
								}

							void Cleanup(Moe::Alloc * pAlloc)
							{
								auto ppStnodPMac = m_aryPStnodManaged.PMac();
								for (auto ppStnod = m_aryPStnodManaged.A(); ppStnod != ppStnodPMac; ++ppStnod)
								{
									pAlloc->MOE_DELETE(*ppStnod);
								}
								m_aryPStnodManaged.Clear();
							}

	void 					Swap(GenericMap * pGenmapOther)
								{ 
									Moe::moeSwap(m_istrName, pGenmapOther->m_istrName);
									Moe::moeSwap(m_grftingenResult, pGenmapOther->m_grftingenResult);

									m_mpIstrAnc.Swap(&pGenmapOther->m_mpIstrAnc);
									m_aryPStnodManaged.Swap(&pGenmapOther->m_aryPStnodManaged);
									m_aryLexspSrc.Swap(&pGenmapOther->m_aryLexspSrc);
								}

	Anchor	*				PAncMapType(const Moe::InString & istrName, TypeInfo * pTin);
	Anchor *				PAncMapValue(const Moe::InString & istrName, STNode * pStnodBaked);
	Anchor *				PAncLookup(const Moe::InString & istr)
								{ return m_mpIstrAnc.Lookup(istr); }

	LexSpan 				LexspReporting()
								{
									if (MOE_FVERIFY(!m_aryLexspSrc.FIsEmpty(), "no source location in generic map '%s'.", m_istrName.m_pChz))
									{
										return m_aryLexspSrc[0];
									}
									return LexSpan();
								}

	bool					FIsEmpty() const
								{
									return m_mpIstrAnc.FIsEmpty();
								}


	Moe::InString						m_istrName;
	Moe::CHash<Moe::InString, Anchor>	m_mpIstrAnc;			// map from a string name to the anchor value or type mapped to it
	Moe::CDynAry<STNode *>				m_aryPStnodManaged;		// stnodes for baked constants
	Moe::CDynAry<LexSpan>				m_aryLexspSrc;			// lexer location where this was instantiated
	GRFTINGEN							m_grftingenResult;		// will the resultant type be generic?
};

void PrintGenmap(Workspace * pWork, GenericMap * pGenmap);
void PrintGenmapAnchors(Moe::StringBuffer * pStrbuf, GenericMap * pGenmap);
void PrintGenmapNoLocation(GenericMap * pGenmap, const char * pChzLineEnd = "\n");

struct InstantiateRequest // tag = insreq
{
								InstantiateRequest()
								:m_pStnodGeneric(nullptr)
								,m_pSym(nullptr)
								,m_pGenmap(nullptr)
								,m_iInsreq(-1)
									{ ; }

	STNode * 					m_pStnodGeneric;	// AST for unsubstituted generic 
	Symbol *					m_pSym;				// instantiated type, tin proc with resolved argument types
	GenericMap * 				m_pGenmap;
	int							m_iInsreq;
};

struct GenericRegistry // tag = genreg
{
public:
	struct Entry
	{
								Entry()
								:m_pGenmapKey(nullptr)
								,m_pTin(nullptr)
								,m_pInsreq(nullptr)
									{ ; }

		GenericMap *			m_pGenmapKey;	// genmap used as a key to lookup this entry 
		TypeInfo *				m_pTin;
		InstantiateRequest *	m_pInsreq;
	};

	struct EntryBlock	// tag = block
	{
								EntryBlock (Moe::Alloc * pAlloc)
								:m_aryEntry(pAlloc, Moe::BK_TypeCheckGenerics)
									{ ; }

		Moe::CDynAry<Entry>		m_aryEntry;
	};

								GenericRegistry(Moe::Alloc * pAlloc)
								:m_pAlloc(pAlloc)
								,m_mpStnodInstFromBlock(pAlloc, Moe::BK_TypeCheckGenerics, 512)
								,m_aryInsreq(pAlloc, Moe::BK_TypeCheckGenerics, 128)
									{ ; }

								~GenericRegistry()
								{
									Cleanup();
								}

	void						Cleanup();

	Entry *						PEntryLookup(STNode * pStnodInstFrom, GenericMap * pGenmap);
	Entry *						PEntryEnsure(STNode * pStnodInstFrom, GenericMap * pGenmap);
	InstantiateRequest *		PInsreqNew(STNode * pStnodInstFrom, GenericMap * pGenmap);

	Moe::Alloc *							m_pAlloc;
	Moe::CHash<STNode *, EntryBlock *>		m_mpStnodInstFromBlock;
	Moe::CDynAry<InstantiateRequest>		m_aryInsreq;
};





// Find the name of all the anchors
void FindGenericAnchorNames(Moe::Alloc * pAlloc, STNode * pStnodParamDecl, GenericMap * pGenmap);

// find the subset of a generic map used by a given generic definition
GenericMap * PGenmapTrimUnusedAnchors(
	TypeCheckContext * pTcwork,
	STNode * pStnodDef,
	GenericMap * pGenmapSuperset,
	const LexSpan & lexsp);

enum FGENCOMP
{
	FGENCOMP_None			= 0x0,
	FGENCOMP_ImplicitRef	= 0x1,

	FGENCOMP_All			= 0x1,
};

MOE_DEFINE_GRF(GRFGENCOMP, FGENCOMP, u32);

// Compute a generic map (values/types for all anchors)
ERRID ErridComputeDefinedGenerics( 
	TypeCheckContext * pTcwork,
	const LexSpan & lexsp, 
	ERREP errep,
	GRFGENCOMP grfgencomp, 
	SymbolTable * pSymtab,
	TypeInfo * pTinRef,
	STNode * pStnodDef,
	GenericMap * pGenmap);

// FindCanon
//   given SFoo($DIM=2) instantiated from SFoo($C=$DIM) compute SFoo($C=2)

TypeInfo * PTinFindCanon(TypeCheckContext * pTcwork, TypeInfo * pTin, SymbolTable * pSymtab, ERREP errep);
void AssertIsCanon(TypeInfo * pTin);

// Create a type by replacing the supplied type anchors, 
//  Note: resulting type can still contain generic anchors if not fully instantiated, or generic types are substituted.
//  Note: doesn't instantiate AST for generic type
//  NOTE: this should always return a canonical type with a trimmed generic map
TypeInfo * PTinSubstituteGenerics(
	TypeCheckContext * pTcwork,
	SymbolTable * pSymtab,
	const LexSpan & lexsp,
	TypeInfo * pTinUnsub,
	GenericMap * pGenmap,
	ERREP errep);

// lookup a instantiation request, via a generic map and pStnod that defined the base.
InstantiateRequest * PInsreqLookup(
	TypeCheckContext * pTcwork,
	STNode * pStnodDefinition,
	GenericMap * pGenmap);

TypeInfoStruct * PTinstructEnsureUniqueInstance(
	TypeCheckContext * pTcwork,
	TypeInfoStruct * pTinstruct);

// instantiate symbol & AST for generic procedure
InstantiateRequest * PInsreqInstantiateGenericProcedure(
	TypeCheckContext * pTcwork,
	STNode * pStnodGeneric,
	GenericMap * pGenmap);

// instantiate symbol & AST for generic struct
InstantiateRequest * PInsreqInstantiateGenericStruct(
	TypeCheckContext * pTcwork,
	STNode * pStnodGeneric,
	STNode * pStnodInstantiation,
	GenericMap * pGenmap);

// actually do the walking to remap a syntax tree copy
void RemapGenericStnodCopy(
	TypeCheckContext * pTcwork,
	STNode * pStnodGen,
	STNode * pStnodNew,
	int iInsreq,
	GenericMap * pGenmap,
	Moe::CHash<Symbol *, Symbol *> * pmpPSymGenericPSymRemapped,
	Moe::CHash<Symbol *, STNode *> * pmpPSymSrcPStnodConstant,
	Moe::CHash<STNode *, STNode *> * pmpPStnodGenPStnodNew,
	SymbolTable * pSymtabSrc,
	SymbolTable * pSymtabNew);
