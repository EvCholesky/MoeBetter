
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

#include "Lexer.h"
#include "MoeArray.h"
#include "MoeHash.h"
#include "MoeString.h"
#include "Symbol.h"
#include "TypeInfo.h"

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

	PARK_ArrayDecl,
	PARK_ReferenceDecl,		// used in type specification, not used for the unary address-of operator
	PARK_QualifierDecl,

	PARK_ProcedureReferenceDecl,
	PARK_Decl,
//	PARK_CompoundDecl,		// comma separated declarations - specialized AST node for future tuple return value support.
	PARK_Typedef,
	PARK_ConstantDecl,
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

const char * PChzFromPark(PARK park);

enum STEXK // Syntax Tree EXtension Kind
{
	STEXK_None,			// basic STNode
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

enum FDBGSTR // DeBuG STRing Flags
{
	FDBGSTR_Name				= 0x1,
	FDBGSTR_Type				= 0x2,
	FDBGSTR_LiteralSize			= 0x4,
	FDBGSTR_UseSizedNumerics	= 0x8, // resolve type aliasing for simple integers - should this be all type aliasing?
	FDBGSTR_NoWhitespace		= 0x10,
	FDBGSTR_Values				= 0x20,
	FDBGSTR_ShowStructArgs		= 0x40,

	FDBGSTR_None				= 0x0,
	FDBGSTR_All					= 0x3F,
};
MOE_DEFINE_GRF(GRFDBGSTR, FDBGSTR, u32);

enum FSTNOD
{
	FSTNOD_EntryPoint			= 0x1,	// this should be inserted as a top level entry point, not in place (local function)
	FSTNOD_ImplicitMember		= 0x2,	// this node was created as an implicit member, did not come directly from the source
	FSTNOD_Fallthrough			= 0x4,	// this node (should be a case/default statement) falls through - BB, need a better place to store this
	FSTNOD_CommutativeCall		= 0x8,	// this function is an overloaded operator with arguments reversed.
	FSTNOD_NoCodeGeneration		= 0x10, // skip this node for codegen - used by generic definitions
	FSTNOD_AssertOnDelete		= 0x20,	// debugging tool, assert when deleted
	FSTNOD_DynamicChildArray	= 0x40, // child array is allocated on the heap, delete upon cleanup

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
	void SetDefaultChildArray() { SetChildArray(&N1, 5); } \
	static const int s_cpStnodChild = 6

#define MOE_STNOD_CHILD6(N1, N2, N3, N4, N5, N6) \
	STNode *N1, *N2, *N3, *N4, *N5, *N6; \
	void SetDefaultChildArray() { SetChildArray(&N1, 6); } \
	static const int s_cpStnodChild = 6

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
								{ MOE_ASSERT(StexkFromPark(park) == stexk, "park/stexk mismatch"); }

	STNode *				PStnodChild(int ipStnod)
								{ return m_apStnodChild[ipStnod]; }
	STNode *				PStnodChildSafe(int ipStnod);
	void					SetChildArray(STNode ** apStnodChild, size_t cpStnodChild);
	void					CopyChildArray(Moe::Alloc * pAlloc, STNode ** apStnodChild, size_t cpStnodChild);
	void					CopyChildArray(Moe::Alloc * pAlloc, STNode * apStnodChild);

	void					AssertValid();

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
	size_t                  m_cpStnodChild;
	STNode **				m_apStnodChild;
};

Moe::InString IstrFromIdentifier(STNode * pStnod);
Moe::InString IstrFromTypeInfo(TypeInfo * pTin);
Moe::InString IstrFromStnod(STNode * pStnod);

template <typename T>
T PStnodRtiCast(STNode * pStnod)
{
	if (pStnod && StexkFromPark(pStnod->m_park) == Moe::SStripPointer<T>::Type::s_stexk)
		return (T)pStnod;
	return nullptr;
}

template <typename T>
T PStnodDerivedCast(STNode * pStnod)
{
	MOE_ASSERT(pStnod && pStnod->m_stexk == Moe::SStripPointer<T>::Type::s_stexk, "illegal stnode derived cast");
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
				,m_pStnodParentScope(nullptr)
					{ ; }

	MOE_STNOD_CHILD4(m_pStnodName, m_pStnodParameterList, m_pStnodReturnType, m_pStnodParentScope);
	GRFSTPROC   m_grfstproc;

	void AssertValid();
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

	void AssertValid();
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

	bool			m_fIsBakedConstant;
	bool			m_fHasUsingPrefix;
	MOE_STNOD_CHILD3(m_pStnodIdentifier, m_pStnodType, m_pStnodInit);

	// TODO: how to handle variable set of child decls, new decl list PARK?
	void AssertValid();
};



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
							{ ; }

	MOE_STNOD_CHILD3(m_pStnodIdentifier, m_pStnodType, m_pStnodConstantList);

	ENUMK               m_enumk;
	size_t              m_cConstantExplicit;
	size_t              m_cConstantImplicit;
	TypeInfoEnum *		m_pTinenum;     // why is this here? not just m_pTin?

	void AssertValid();
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

	void AssertValid();
};

#define AST_ASSERT(PWORK, PSTNOD, PREDICATE, ... ) do { if (!(PREDICATE)) { \
		Moe::AssertHandler(__FILE__, __LINE__, #PREDICATE, __VA_ARGS__); \
		s32 iLine, iCol; \
		CalculateLinePosition(pWork, &PSTNOD->m_lexsp, &iLine, &iCol); \
		printf("compiling: %s:%u\n", PSTNOD->m_lexsp.m_istrFilename.m_pChz, iLine); \
		MOE_DEBUG_BREAK(); \
		 } } while(0)

#define AST_FVERIFY(PWORK, PSTNOD, PREDICATE, ... )\
(\
  ( ( PREDICATE ) ? \
	true :\
	(\
		Moe::AssertHandler( __FILE__, __LINE__, #PREDICATE, __VA_ARGS__ ),\
		s32 iLine, iCol; \
		CalculateLinePosition(pWork, &PSTNOD->m_lexsp, &iLine, &iCol); \
		printf("compiling: %s:%u\n", PSTNOD->m_lexsp.m_istrFilename.m_pChz, iLine); \
		MOE_DEBUG_BREAK(), \
		false\
	)\
  )\
)



// indicates storage type only - actual type info should come from STypeInfoLiteral
enum STVALK // Syntax Tree VALue Kind
{
	STVALK_Nil = -1,
	STVALK_Float,
	STVALK_SignedInt,
	STVALK_UnsignedInt,
	STVALK_String,
	STVALK_ReservedWord,
};

struct STValue : public STNode // tag = stval
{
	static const STEXK s_stexk = STEXK_Value;

						STValue(PARK park, const LexSpan & lexsp)
						:STNode(s_stexk, park, lexsp)
						,m_stvalk(STVALK_Nil)
						,m_nUnsigned(0)
							{ ; }

	STVALK              m_stvalk;

	void				SetIstr(Moe::InString& istr)
							{
								m_istr = istr;
								m_stvalk = STVALK_String;
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
		Moe::InString		m_istr;
	};

	MOE_STNOD_NO_CHILDREN();

	void AssertValid();
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

	OpTypes 			m_optype;
	MOE_STNOD_CHILD2(m_pStnodLhs, m_pStnodRhs);

	void AssertValid();
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
							{ ; }

	Moe::Alloc * 		m_pAlloc;
	Workspace *			m_pWork;
	SymbolTable *		m_pSymtab;
	SymbolTable *		m_pSymtabGeneric;	// symbol table for child generic types/values
											// ie. 'TakeFoo proc (foo : SFoo($T))' needs to add symbol 'D' to the symtable for TakeFoo
	STNode *			m_pStnodScope;		// current containg scope
	GRFSYMLOOK			m_grfsymlook;
};

void ParseTopLevel(Workspace * pWork, Lexer * pLex, MoeQuery * pMq);

