
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

#include "Error.h"
#include "Lexer.h"
#include "MoeArray.h"
#include "MoeHash.h"
#include "MoeString.h"
#include "Symbol.h"
#include "TypeInfo.h"

struct Compilation;
struct Job;
struct MoeQuery;
struct SymbolTable;
struct SymbolBase;
struct TypeInfo;
struct TypeInfoEnum;
struct Workspace;

enum PARK : s8 // PARse Kind
{
	PARK_Error,
	PARK_Identifier,
	PARK_ReservedWord,
	PARK_Nop,
	PARK_Literal,
	PARK_AdditiveOp,
	PARK_MultiplicativeOp,
	PARK_ShiftOp,
	PARK_RelationalOp,
	PARK_LogicalAndOrOp,
	PARK_AssignmentOp,
	PARK_UnaryOp,
	PARK_PostfixUnaryOp,	// postfix increment, decrement
	PARK_Uninitializer,

	PARK_Cast,
	PARK_ArrayElement,		// [array, index]
	PARK_MemberLookup,		// [struct, child]
	PARK_ProcedureCall,		// [procedure, arg0, arg1, ...]
	PARK_SpecializedStruct,	// swapped in during typecheck for ProcedureCall nodes that turn out to be instantiated structs SArray(33)

	PARK_List,				// declarations used by structs
	PARK_ParameterList,		// comma separated declarations used by argument lists
	PARK_ExpressionList,	// list of expressions, used by compound literals - doesn't error on rhs only values.
	PARK_GenericTypeSpec,	// list of types to specify a generic procedure/struct instantiation
	PARK_If,
	PARK_Else,
	PARK_For,

	PARK_ArrayDecl,
	PARK_ReferenceDecl,		// used in type specification, not used for the unary address-of operator
	PARK_QualifierDecl,

	PARK_ProcedureReferenceDecl,
	PARK_Decl,
//	PARK_CompoundDecl,		// comma separated declarations - specialized AST node for future tuple return value support.
	PARK_ConstantDecl,
	PARK_Typedef,
	PARK_ProcedureDefinition,
	PARK_EnumDefinition,
	PARK_StructDefinition,
	PARK_EnumConstant,
	PARK_VariadicArg,
	PARK_CompoundLiteral,	// array/struct literal
	PARK_ArgumentLabel,
	PARK_GenericDecl,
	PARK_GenericStructSpec,		// 
	PARK_TypeArgument,			// raw type, specified to a generic instantiation SFoo(:int)
	PARK_BakedValue,
	
	MOE_MAX_MIN_NIL(PARK)
};

const char * PChzLongFromPark(PARK park);
const char * PChzAbbrevFromPark(PARK park);

enum STEXK // Syntax Tree EXtension Kind
{
	STEXK_Node,			// basic STNode
	STEXK_For,
	STEXK_Decl,
	STEXK_Enum,
	STEXK_Struct,
	STEXK_Proc,
	STEXK_Value,		// New, move pStval here?
	STEXK_Operator,     // New, move pOptype here?
	//STEXK_List,         // no subtype, variable list of children

	MOE_MAX_MIN_NIL(STEXK)
};

STEXK StexkFromPark(PARK park);
const char * PChzFromStexk(STEXK stexk);

enum STREES : s8
{
	STREES_Parsed,
									// Symbols should be known at this stage (BB - need to lock symbol tables to ensure this)

	STREES_SignatureTypeChecked,	// function's signature has been type checked, but not it's body. it's enough to type check
									//  proc calls, BB - Is this really necessary? should we just mark TypeChecked before we're done?
	STREES_TypeChecked,

									// all types should be defined by now
									// all Generics should be fully instantiated
	STREES_CodegenByteCode,
	STREES_CodegenNative,

	MOE_MAX_MIN_NIL(STREES)
};

enum FSTNOD
{
	FSTNOD_EntryPoint			= 0x1,	// this should be inserted as a top level entry point, not in place (local function)
	FSTNOD_ImplicitMember		= 0x2,	// this node was created as an implicit member, did not come directly from the source
	FSTNOD_Fallthrough			= 0x4,	// this node (should be a case/default statement) falls through - BB, need a better place to store this
	FSTNOD_CommutativeCall		= 0x8,	// this function is an overloaded operator with arguments reversed.
	FSTNOD_NoCodeGeneration		= 0x10, // skip this node for codegen - used by generic definitions
	FSTNOD_AssertOnDelete		= 0x20,	// debugging tool, assert when deleted
	FSTNOD_ChildArrayOnHeap		= 0x40, // child array is allocated on the heap, delete upon cleanup

	FSTNOD_HasParseError		= 0x80,
	FSTNOD_HasTypeCheckError	= 0x100,

	FSTNOD_None			= 0x0,
	FSTNOD_All			= 0xFF,
};
MOE_DEFINE_GRF(GRFSTNOD, FSTNOD, u16);

#define MOE_STNOD_NO_CHILDREN() \
	void SetDefaultChildArray() { ; } \

#define MOE_STNOD_CHILD2(N1, N2) \
	STNode *N1, *N2; \
	void SetDefaultChildArray() { SetChildArray(&N1, 2); } \

#define MOE_STNOD_CHILD3(N1, N2, N3) \
	STNode *N1, *N2, *N3; \
	void SetDefaultChildArray() { SetChildArray(&N1, 3); } \

#define MOE_STNOD_CHILD4(N1, N2, N3, N4) \
	STNode *N1, *N2, *N3, *N4; \
	void SetDefaultChildArray() { SetChildArray(&N1, 4); } \

#define MOE_STNOD_CHILD5(N1, N2, N3, N4, N5) \
	STNode *N1, *N2, *N3, *N4, *N5; \
	void SetDefaultChildArray() { SetChildArray(&N1, 5); } 

#define MOE_STNOD_CHILD6(N1, N2, N3, N4, N5, N6) \
	STNode *N1, *N2, *N3, *N4, *N5, *N6; \
	void SetDefaultChildArray() { SetChildArray(&N1, 6); } 

// Slimmed down AST
struct STNode // tag = stnod
{
public:
							STNode(STEXK stexk, PARK park, const LexSpan & lexsp)
							:m_tok(TOK_Nil)
							,m_park(park)
							,m_strees(STREES_Parsed)
							,m_grfstnod(FSTNOD_None)
							,m_lexsp(lexsp)
							,m_pTin(nullptr)
							,m_pSymtab(nullptr)
							,m_pSymbase(nullptr)
							,m_cpStnodChild(0)
							,m_apStnodChild(nullptr)
								{ 
									MOE_ASSERT(StexkFromPark(park) == stexk, "park/stexk mismatch PARK_%s != STEXK_%s",
										PChzAbbrevFromPark(park), PChzFromStexk(stexk));
								}
							~STNode();
								


	int 					CPStnodChild() const 
								{ return m_cpStnodChild; }
	STNode *				PStnodChild(int ipStnod)
								{ return m_apStnodChild[ipStnod]; }
	STNode *				PStnodChildSafe(int ipStnod);
	STNode **				PPStnodChildMin()
								{ return m_apStnodChild; }
	STNode **				PPStnodChildMax()
								{ return m_apStnodChild + m_cpStnodChild; }
	void					SetChildArray(STNode ** apStnodChild, int cpStnodChild);
	void					CopyChildArray(Moe::Alloc * pAlloc, STNode ** apStnodChild, int cpStnodChild);
	void					CopyChildArray(Moe::Alloc * pAlloc, STNode * pStnodChild);
	void					AppendChildToArray(Moe::Alloc * pAlloc, STNode * pStnodChild);
	void					ReplaceChild(STNode * pStnodOld, STNode * pStnodNew);
	bool					FHasChildArray() const
								{ return m_cpStnodChild > 0; }
	STEXK					Stexk() const
								{ return StexkFromPark(m_park); }

	bool					FCheckIsValid(ErrorManager * pErrman);

	Symbol *				PSym() const
								{ return (m_pSymbase) ? PSymLast(m_pSymbase) : nullptr; }

	SymbolPath *			PSymPath() const
								{
									if (m_pSymbase && m_pSymbase->m_symk == SYMK_Path)
										return (SymbolPath *)m_pSymbase;
									return nullptr;
								}
	TOK						m_tok;		
	PARK					m_park;		
	STREES					m_strees;	
	GRFSTNOD				m_grfstnod;

	LexSpan					m_lexsp;	// lexer handles for begin/end
											//CSTValue *			m_pStval;		// can this get moved into CSTExExpression - need to stop using it for RWORD
											//CSTIdentifier *		m_pStident;		// pull it from the source
											//SSyntaxTreeMap *		m_pStmap;       // moved into STEX classes
	TypeInfo *				m_pTin;
	SymbolTable *			m_pSymtab;
	SymbolBase *			m_pSymbase;

	// Child nodes are embedded in STNode derived with a fixed child layout 
	int						m_cpStnodChild;
	STNode **				m_apStnodChild;
};

void CleanupStnodeRecursive(Moe::Alloc * pAlloc, STNode * pStnod);
STNode * PStnodCopy(Moe::Alloc * pAlloc, STNode * pStnodSrc, Moe::CHash<STNode *, STNode *> * pmpPStnodSrcPStnodDst = nullptr);

template <typename T>
T PStnodRtiCast(STNode * pStnod)
{
	if (pStnod && pStnod->Stexk() == Moe::SStripPointer<T>::Type::s_stexk)
		return (T)pStnod;
	return nullptr;
}

template <typename T>
T PStnodDerivedCast(STNode * pStnod)
{
	MOE_ASSERT(pStnod && pStnod->Stexk() == Moe::SStripPointer<T>::Type::s_stexk, "illegal stnode derived cast");
	return (T)pStnod;
}

enum FSTPROC
{
	FSTPROC_IsForeign			= 0x1,	// This is a reference to a foreign procedure
	FSTPROC_UseUnmangledName	= 0x2,	// Don't mangle the procedure name (no overloading)
	FSTPROC_PublicLinkage		= 0x4,	// Expose to the linker, don't cull as unused symbol

	FSTPROC_None				= 0x0,
	FSTPROC_All					= 0x7,
};
MOE_DEFINE_GRF(GRFSTPROC, FSTPROC, u8);



struct STProc : public STNode
{
public:
	static const STEXK s_stexk = STEXK_Proc;

							STProc(PARK park, const LexSpan & lexsp)
							:STNode(s_stexk, park, lexsp)
							,m_pStnodName(nullptr)
							,m_pStnodParameterList(nullptr)
							,m_pStnodReturnType(nullptr)
							,m_pStnodBody(nullptr)
							,m_pStnodForeignAlias(nullptr)
							,m_pStnodParentScope(nullptr)
								{ ; }

	MOE_STNOD_CHILD6(m_pStnodName, m_pStnodParameterList, m_pStnodReturnType, m_pStnodBody, m_pStnodForeignAlias, m_pStnodParentScope );
	GRFSTPROC				m_grfstproc;

	bool					FCheckIsValid(ErrorManager * pErrman);
};



struct STFor : public STNode
{
public:
	static const STEXK s_stexk = STEXK_For;

							STFor(PARK park, const LexSpan & lexsp)
							:STNode(s_stexk, park, lexsp)
							,m_pStnodDecl(nullptr)
							,m_pStnodIterator(nullptr)
							,m_pStnodInit(nullptr)
							,m_pStnodBody(nullptr)
							,m_pStnodPredicate(nullptr)
							,m_pStnodIncrement(nullptr)
								{ ; }

	MOE_STNOD_CHILD6(m_pStnodDecl, m_pStnodIterator, m_pStnodInit, m_pStnodBody, m_pStnodPredicate, m_pStnodIncrement);

	bool					FCheckIsValid(ErrorManager * pErrman);
};



struct STDecl : public STNode
{
public:
	static const STEXK s_stexk = STEXK_Decl;

							STDecl(PARK park, const LexSpan & lexsp)
							:STNode(s_stexk, park, lexsp)
							,m_fIsBakedConstant(false)
							,m_fHasUsingPrefix(false)
							,m_pStnodIdentifier(nullptr)
							,m_pStnodType(nullptr)
							,m_pStnodInit(nullptr)
								{ ; }

	bool					m_fIsBakedConstant;
	bool					m_fHasUsingPrefix;
	MOE_STNOD_CHILD3(m_pStnodIdentifier, m_pStnodType, m_pStnodInit);

	// TODO: how to handle variable set of child decls, new decl list PARK?
	bool					FCheckIsValid(ErrorManager * pErrman);
};



enum ENUMIMP	// implicit enum members (added as STNodes during parse)
{
	ENUMIMP_NilConstant,	// (enum) -1 or max unsigned (not included in min)
	ENUMIMP_MinConstant,	// (enum) lowest user value
	ENUMIMP_LastConstant,	// (enum) highest user value 
	ENUMIMP_MaxConstant,	// (enum) one past the highest value
	ENUMIMP_None,			// (flagEnum) zero
	ENUMIMP_All,			// (flagEnum) all defined bits in bitmask
	ENUMIMP_Names,
	ENUMIMP_Values,

	ENUMIMP_Max,
	ENUMIMP_Min = 0,
	ENUMIMP_Nil = -1,
};

Moe::InString IstrFromEnumimp(ENUMIMP enumimp);
bool FNeedsImplicitMember(ENUMIMP enumimp, ENUMK enumk);

struct STEnum : public STNode
{
	static const STEXK s_stexk = STEXK_Enum;

							STEnum(PARK park, const LexSpan & lexsp)
							:STNode(s_stexk, park, lexsp)
							,m_pStnodIdentifier(nullptr)
							,m_pStnodType(nullptr)
							,m_pStnodConstantList(nullptr)
							,m_enumk(ENUMK_Basic)
							,m_cConstantExplicit(0)
							,m_cConstantImplicit(0)
							,m_pTinenum(nullptr)
								{ 
									for (int enumimp = 0; enumimp < MOE_DIM(m_mpEnumimpIstnod); ++enumimp)
										m_mpEnumimpIstnod[enumimp] = -1;
								}

	MOE_STNOD_CHILD3(m_pStnodIdentifier, m_pStnodType, m_pStnodConstantList);

	ENUMK					m_enumk;
	int						m_cConstantExplicit;
	int						m_cConstantImplicit;
	TypeInfoEnum *			m_pTinenum;     // why is this here? not just m_pTin?

	int						m_mpEnumimpIstnod[ENUMIMP_Max];		// pStnod index for immplicit constants in pStnodConstantList

	bool					FCheckIsValid(ErrorManager * pErrman);
};



struct STStruct : public STNode
{
	static const STEXK s_stexk = STEXK_Struct;

							STStruct(PARK park, const LexSpan & lexsp)
							:STNode(s_stexk, park, lexsp)
							,m_pStnodIdentifier(nullptr)
							,m_pStnodParameterList(nullptr)
							,m_pStnodBakedParameterList(nullptr)
							,m_pStnodDeclList(nullptr)
								{ ; }

	MOE_STNOD_CHILD4(m_pStnodIdentifier, m_pStnodParameterList, m_pStnodBakedParameterList, m_pStnodDeclList);

	bool					FCheckIsValid(ErrorManager * pErrman);
};

#define AST_ASSERT(PWORK, PSTNOD, PREDICATE, ... ) do { if (!(PREDICATE)) { \
		Moe::AssertHandler(__FILE__, __LINE__, #PREDICATE, __VA_ARGS__); \
		printf("compiling: %s:%llu\n", PSTNOD->m_lexsp.m_istrFilename.m_pChz, LexLookup(PWORK, PSTNOD->m_lexsp).m_iLine); \
		MOE_DEBUG_BREAK(); \
		 } } while(0)

#if defined( _MSC_VER )
#define AST_FVERIFY(PWORK, PSTNOD, PREDICATE, ... )\
(\
  ( ( PREDICATE ) ? \
	true :\
	(\
		Moe::AssertHandler( __FILE__, __LINE__, #PREDICATE, __VA_ARGS__ ),\
		printf("compiling: %s:" MOE_U64FMT "\n", PSTNOD->m_lexsp.m_istrFilename.m_pChz, LexLookup(PWORK, PSTNOD->m_lexsp).m_iLine), \
		MOE_DEBUG_BREAK(), \
		false\
	)\
  )\
)

#else
// use a goofy expression statement to play nice with clang
#define AST_FVERIFY(PWORK, PSTNOD, PREDICATE, ... )\
(\
  ( ( PREDICATE ) ? \
	true :\
	({\
		Moe::AssertHandler( __FILE__, __LINE__, #PREDICATE, __VA_ARGS__ );\
		printf("compiling: %s:" MOE_S64FMT "\n", PSTNOD->m_lexsp.m_istrFilename.m_pChz, LexLookup(PWORK, PSTNOD->m_lexsp).m_iLine); \
		MOE_DEBUG_BREAK(); \
		false;\
	})\
  )\
)
#endif



// indicates storage type only - actual type info should come from STypeInfoLiteral
enum STVALK // Syntax Tree VALue Kind
{
	STVALK_Nil = -1,
	STVALK_Float,
	STVALK_SignedInt,
	STVALK_UnsignedInt,
	STVALK_String,
};

struct STValue : public STNode // tag = stval
{
	static const STEXK s_stexk = STEXK_Value;

						STValue(PARK park, const LexSpan & lexsp)
						:STNode(s_stexk, park, lexsp)
						,m_stvalk(STVALK_Nil)
						,m_nUnsigned(0)
						,m_istrRword()
							{ ; }

	STVALK              m_stvalk;

	void				SetIdentifier(const Moe::InString& istr)
							{
								m_istrValue = istr;
								m_stvalk = STVALK_String;
							}

	void				SetReservedWord(const Moe::InString& istrRword)
							{
								m_istrRword = istrRword;
							}
	void				SetF64(f64 g)
							{
								m_g = g;
								m_stvalk = STVALK_Float;
							}
	void				SetU64(u64 n)
							{
								m_nUnsigned = n;
								m_stvalk = STVALK_UnsignedInt;
							}
	void				SetS64(s64 n)
							{
								m_nSigned = n;
								m_stvalk = STVALK_SignedInt;
							}
	union
	{
		f64					m_g;
		u64					m_nUnsigned;
		s64					m_nSigned;
		Moe::InString		m_istrValue;
	};

	Moe::InString			m_istrRword;		// reserved word name

	MOE_STNOD_NO_CHILDREN();

	bool					FCheckIsValid(ErrorManager * pErrman);
};



struct STOperator : public STNode // tag = stop
{
	static const STEXK s_stexk = STEXK_Operator;

							STOperator(PARK park, const LexSpan & lexsp)
							:STNode(s_stexk, park, lexsp)
							,m_optype()
							,m_pStnodLhs(nullptr)
							,m_pStnodRhs(nullptr)
								{ ; }

	OpTypes 				m_optype;
	MOE_STNOD_CHILD2(m_pStnodLhs, m_pStnodRhs);

	bool					FCheckIsValid(ErrorManager * pErrman);
};

struct ParseContext // tag = parctx
{
						ParseContext(Moe::Alloc * pAlloc, Workspace * pWork)
						:m_pAlloc(pAlloc)
						,m_pWork(pWork)
						,m_pSymtab(nullptr)
						,m_pSymtabGeneric(nullptr)
						,m_pStnodScope(nullptr)
						,m_grfsymlook(FSYMLOOK_Default)
							{ 
								m_pLrecst = PLrecstAlloc(pAlloc);
							}
						~ParseContext()
						{
							FreeLexRecoverStack(m_pAlloc, m_pLrecst);
						}

	ErrorManager *		PErrman() const;

	Moe::Alloc * 		m_pAlloc;
	Workspace *			m_pWork;
	SymbolTable *		m_pSymtab;
	SymbolTable *		m_pSymtabGeneric;	// symbol table for child generic types/values
											// ie. 'TakeFoo proc (foo : SFoo($T))' needs to add symbol 'D' to the symtable for TakeFoo
	STNode *			m_pStnodScope;		// current containg scope
	GRFSYMLOOK			m_grfsymlook;
	LexRecoverStack *	m_pLrecst;
};

struct ParseJobData // tag = parjd
{
					ParseJobData(Moe::Alloc * pAlloc, Workspace * pWork)
					:m_parctx(pAlloc, pWork)
					,m_lex()
					,m_pChzBody(nullptr)
						{ ; }

	ParseContext 	m_parctx;
	Lexer 			m_lex;
	const char *	m_pChzBody;

	static const int s_cBLexerStorage = 8 * 1024;
	char			m_aChStorage[s_cBLexerStorage];
};

// S-Expression writer notes 
enum SEWK // S-Expression Kind
{
	SEWK_Park,				// write an S-Expression with the parse kind
	SEWK_Parse,				// write an S-Expression with the parsed value at each node, 
							// 1. nodes that have a meaningful value (strings and literals)
							// 2. Operators show tokens
							// 3. types if they exist (they mostly shouldn't 
							// 4. park
	SEWK_Value,
	SEWK_TypeInfo,	
};

enum FSEW // DeBuG STRing Flags
{
	FSEW_LiteralSize			= 0x1,
	FSEW_UseSizedNumerics		= 0x2, // resolve type aliasing for simple integers - should this be all type aliasing?
	FSEW_NoWhitespace			= 0x4,
	FSEW_ShowStructArgs			= 0x8,

	FSEW_None					= 0x0,
	FSEW_All					= 0xF,
	GRFSEW_Default				= FSEW_None,
};
MOE_DEFINE_GRF(GRFSEW, FSEW, u32);

STNode * PStnodAllocAfterParse(Moe::Alloc * pAlloc, PARK park, const LexSpan & lexsp);
STDecl * PStdeclAllocAfterParse(Moe::Alloc * pAlloc, PARK park, const LexSpan & lexsp);
STValue * PStvalAllocAfterParse(Moe::Alloc * pAlloc, PARK park, const LexSpan & lexsp);

Moe::InString IstrSExpression(STNode * pTin, SEWK sewk, GRFSEW grfsew = GRFSEW_Default);
Moe::InString IstrSExpression(TypeInfo * pTin, GRFSEW grfsew = GRFSEW_Default);

void WriteTypeInfoSExpression(Moe::StringBuffer * pStrbuf, TypeInfo * pTin, PARK park, GRFSEW grfsew = GRFSEW_Default);
void WriteSExpression(Moe::StringBuffer * pStrbuf, STNode * pStnod, SEWK sewk, GRFSEW grfsew = GRFSEW_Default);
void WriteSExpressionForEntries(Workspace * pWork, char * pCo, char * pCoMax, SEWK sewk, GRFSEW grfsew);
void PrintLiteral(Moe::StringBuffer * pStrbuf, STNode * pStnodLit);

Moe::InString IstrFromIdentifier(STNode * pStnod);
Moe::InString IstrIdentifierFromDecl(STNode * pStnodDecl);
ERRID ErridCheckOverloadSignature(TOK tok, TypeInfoProcedure * pTinproc, ErrorManager * pErrman, const LexSpan & lexsp);

Job * PJobCreateParse(Compilation * pComp, Workspace * pWork, const char * pChzBody, Moe::InString istrFilename);

Moe::InString IstrOverloadNameFromTok(TOK tok);

