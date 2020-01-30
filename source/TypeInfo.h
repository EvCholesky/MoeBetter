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

#include "BigMath.h"
#include "MoeArray.h"
#include "MoeHash.h"
#include "MoeString.h"

// Uniqueness notes
// Types should be unique from creation, don't create types until we have enough info to ensure uniqueness.
//  - types aliased by typedef are their own typeinfo and refer to the native type 
//  - 

struct GenericMap;
struct STDecl;
struct STNode;
struct TypeInfoProcedure;

#define BUILT_IN_TYPE_LIST \
		BLTIN(Char) STR(char)	\
		BLTIN(Bool) STR(bool)	\
		BLTIN(Int) STR(int)		\
		BLTIN(Uint) STR(uint)	\
		BLTIN(SSize) STR(sSize)	\
		BLTIN(USize) STR(uSize)	\
		BLTIN(Float) STR(float)	\
		BLTIN(S8) STR(s8)	\
		BLTIN(S16) STR(s16)	\
		BLTIN(S32) STR(s32)	\
		BLTIN(S64) STR(s64)	\
		BLTIN(U8) STR(u8)	\
		BLTIN(U16) STR(u16)	\
		BLTIN(U32) STR(u32)	\
		BLTIN(U64) STR(u64)	\
		BLTIN(F32) STR(f32)	\
		BLTIN(F64) STR(f64)	\
		BLTIN(Void) STR(void) \
		BLTIN(String) STR(string) \
		BLTIN(EnumNil) STR(nil)	\
		BLTIN(EnumMin) STR(nil)	\
		BLTIN(EnumLast) STR(nil) \
		BLTIN(EnumMax) STR(nil)	\
		BLTIN(EnumNone) STR(nil) \
		BLTIN(EnumAll) STR(nil)	\
		BLTIN(EnumNames) STR(nil) \
		BLTIN(EnumValues) STR(nil) \
		BLTIN(EnumFlag) STR(_flag)	

#define BLTIN(x) \
	extern const char * g_pChz##x; \
	extern Moe::InString g_istr##x; 
#define STR(x)
namespace BuiltIn
{
	BUILT_IN_TYPE_LIST	
}
#undef STR
#undef BLTIN

inline void InternBuiltInTypeStrings()
{
#define BLTIN(x) BuiltIn::g_istr##x = IstrIntern(BuiltIn::g_pChz##x);
#define STR(x)
	BUILT_IN_TYPE_LIST
#undef STR
#undef BLTIN
}

inline void ClearBuiltInTypeStrings()
{
#define BLTIN(x) BuiltIn::g_istr##x = Moe::InString();
#define STR(x)
	BUILT_IN_TYPE_LIST
#undef STR
#undef BLTIN
}



enum TINK : s8
{
    TINK_Numeric	= 0,
    TINK_Bool		= 1,			// no specialized type info
    TINK_Pointer	= 2,
    TINK_Procedure	= 3,
    TINK_Void		= 4,			// no specialized type info
    TINK_Struct		= 5,
    TINK_Array		= 6,
    TINK_Null		= 7,			// no specialized type info
    TINK_Any		= 8,			// no specialized type info
    TINK_Enum		= 9, 
	TINK_Qualifier	= 10,
	TINK_Interface  = 11,
	TINK_Type		= 12,			// first class 'TYPE' - not really useful or supported until we get compile time code.
	TINK_ReflectedMax,

	TINK_ForwardDecl = TINK_ReflectedMax,	// Type info added for resolving pointers to self during the type-check phase.
	TINK_Literal,							// literal that hasn't been resolved to a specific type yet
	TINK_Anchor,							// type for generic anchors that haven't been substituted
	TINK_Flag,								// Type for enum flag assignments; no specialized type info

	MOE_MAX_MIN_NIL(TINK)
};

const char * PChzFromTink(TINK tink);

enum FTIN
{
	FTIN_IsUnique	= 0x1,
	FTIN_IsCanon	= 0x2,			// canonical type is unique, derived from the generic root type and is only parameterized by canonical types

	FTIN_None		= 0x0,
	FTIN_All		= 0x3
};

MOE_DEFINE_GRF(GRFTIN, FTIN, u8);

struct SDataLayout		// tag = dlay
{
	s32		m_cBBool;			// byte size of "bool"
	s32		m_cBInt;			// byte size of "int"
	s32		m_cBFloat;			// byte size of "float"
	s32		m_cBPointer;		// byte size of pointer
	s32		m_cBStackAlign;		// code gen stack alignment
};



struct TypeInfo	// tag = tin
{
						TypeInfo(const Moe::InString & istrName, TINK tink)
						:m_tink(tink)
						,m_grftin(FTIN_None)
						,m_istrName(istrName)
						,m_istrDesc()
#if KEEP_TYPEINFO_DEBUG_STRING
						,m_istrDebug()
#endif
						,m_pTinUnaliased(nullptr)
							{ ; }

    TINK				m_tink;
	GRFTIN				m_grftin;

	Moe::InString		m_istrName;				// user facing name 
	Moe::InString		m_istrDesc;				// unique descriptor used to unique this type 

#if KEEP_TYPEINFO_DEBUG_STRING
	Moe::InString		m_istrDebug;	
#endif

	// BB - This shouldn't be embedded in the typeinfo - it won't work for multiple codegen passes
	//CodeGenValueRef	m_pCgvalDIType;
	//CodeGenValueRef	m_pCgvalReflectGlobal;	// global variable pointing to the type info struct
												// const TypeInfo entry in the reflection type table

	TypeInfo *			m_pTinUnaliased;		// native non-aliased source type (ie sSize->s64)

	static const char * s_pChzGlobalTinTable;
};

Moe::InString IstrFromTypeInfo(TypeInfo * pTin);

template <typename T>
T PTinRtiCast(TypeInfo * pTin)
{
	if (pTin && pTin->m_tink == Moe::SStripPointer<T>::Type::s_tink)
		return (T)pTin;
	return nullptr;
}

template <typename T>
T PTinDerivedCast(TypeInfo * pTin)
{
	MOE_ASSERT(pTin && pTin->m_tink == Moe::SStripPointer<T>::Type::s_tink, "illegal type info derived cast");
	return (T)pTin;
}

enum FNUM
{
	FNUM_IsSigned	= 0x1,
	FNUM_IsFloat	= 0x2,			// canonical type is unique, derived from the generic root type and is only parameterized by canonical types

	FNUM_None		= 0x0,
	FNUM_All		= 0x3
};

MOE_DEFINE_GRF(GRFNUM, FNUM, u8);

struct TypeInfoNumeric : public TypeInfo // tag = tinn
{
	static const TINK s_tink = TINK_Numeric;

			TypeInfoNumeric(const Moe::InString & istrName, u32 cBit, GRFNUM grfnum)
			:TypeInfo(istrName, s_tink)
			,m_cBit(cBit)
			,m_grfnum(grfnum)
				{ ; }

	bool	FIsFloat() const
				{ return m_grfnum.FIsSet(FNUM_IsFloat); }
	bool	FIsInteger() const
				{ return m_grfnum.FIsSet(FNUM_IsFloat) == false; }
	bool	FIsSigned() const
				{ return m_grfnum.FIsSet(FNUM_IsSigned); }

	u32		m_cBit;
	GRFNUM	m_grfnum;
};

struct TypeInfoPointer : public TypeInfo	// tag = tinptr
{
	static const TINK s_tink = TINK_Pointer;

						TypeInfoPointer()
						:TypeInfo(IstrIntern(""), s_tink)
						,m_pTin(nullptr)
						,m_fIsImplicitRef(false)
							{ ; }

	TypeInfo *			m_pTin;
	bool				m_fIsImplicitRef;
};

enum QUALK : s8
{
	QUALK_Const,		// Read only value, transitive ie. members of an const struct or target of a const ref are const
						// - implies that the values will not change during program execution,
						// - unlike c it is NOT safe to upcast to const
						// - types infered from a const type are not const ( n := 5; '5' is const, n is not)

	QUALK_InArg,		// procedure arguments, variable can't be changed, but not transitive
						// - non arguments can currently be declared as inarg for testing, but I'm not sure I'll keep that.

	MOE_MAX_MIN_NIL(QUALK)
};

const char * PChzFromQualk(QUALK qualk);

enum FQUALK
{
	FQUALK_Const	= 0x1 << QUALK_Const,
	FQUALK_InArg	= 0x1 << QUALK_InArg,

	FQUALK_None		= 0x0,
	FQUALK_All		= FQUALK_Const | FQUALK_InArg
};


MOE_DEFINE_GRF(GRFQUALK, FQUALK, u8);

void AppendFlagNames(Moe::StringBuffer * pStrbuf, GRFQUALK grfqualk, const char * pChzSpacer);

struct TypeInfoQualifier : public TypeInfo // tag == tinqual
{
	static const TINK s_tink = TINK_Qualifier;

						TypeInfoQualifier(GRFQUALK grfqualk)
						:TypeInfo(IstrIntern(""), s_tink)
						,m_grfqualk(grfqualk)
							{ ; }

	TypeInfo *			m_pTin;
	GRFQUALK			m_grfqualk;
};

TypeInfo * PTinStripQualifiers(TypeInfo * pTin, GRFQUALK * pGrfqualk);
TypeInfo * PTinStripQualifiers(TypeInfo * pTin);



enum MCALLCON
{
	MCALLCON_CX86		= 0,
	MCALLCON_StdcallX86	= 1,
	MCALLCON_X64		= 2,

	MOE_MAX_MIN_NIL(MCALLCON)
};
const char * PChzFromMcallcon(MCALLCON callconv);

enum INLINEK
{
	INLINEK_AlwaysInline = 0,
	INLINEK_NoInline	 = 1,

	MOE_MAX_MIN_NIL(INLINEK)
};
const char * PChzFromInlinek(INLINEK inlinek);

enum FPARMQ	// Flags for ARGument Qualifiers
{
	FPARMQ_ImplicitRef		= 0x1,		// convert LValue argument and pass into procedure's pointer argument
	FPARMQ_BakedValue		= 0x2,		// baked value parameter, will be dropped during generic specialization.
	FPARMQ_TypeArgument		= 0x4,		// type only argument, no runtime value - dropped during generic specialization

	FPARMQ_None				= 0x0,
	FPARMQ_All				= 0x7,
};

MOE_DEFINE_GRF(GRFPARMQ, FPARMQ, u8);

enum FTINGEN // flags for generic procedures and structures
{

	FTINGEN_HasBakedTypeArgs		= 0x1,
	FTINGEN_HasBakedValueArgs		= 0x2,
	FTINGEN_HasUnsubArgs			= 0x4,	// some generic arguments have not been supplied 
											// (is NOT the same as saying the resultant type has generics!) 

	FTINGEN_None					= 0x0,
	FTINGEN_All						= 0x3,
	FTINGEN_HasGenericArgs			= FTINGEN_HasBakedTypeArgs | FTINGEN_HasBakedValueArgs,
};
MOE_DEFINE_GRF(GRFTINGEN, FTINGEN, u8);

enum FTINPROC
{
	FTINPROC_HasVarArgs			= 0x1,
	FTINPROC_IsCommutative		= 0x2,
	FTINPROC_Initializer		= 0x4,
	FTINPROC_IsForeign			= 0x8,		// foreign procedures are part of a function signature, so bytecode functions 
											//  can return a pointer to a native routine.

	FTINPROC_None				= 0x0,
	FTINPROC_All				= 0xF,
};
MOE_DEFINE_GRF(GRFTINPROC, FTINPROC, u8);

struct TypeInfoProcedure : public TypeInfo	// tag = 	tinproc
{
	static const TINK s_tink = TINK_Procedure;

						TypeInfoProcedure(const Moe::InString & istrName)
						:TypeInfo(istrName, s_tink)
						,m_istrMangled()
						,m_pStnodDefinition(nullptr)
						,m_arypTinParams()
						,m_arypTinReturns()
						,m_mpIptinGrfparmq()
						,m_grftingen(FTINGEN_None)
						,m_grftinproc(FTINPROC_None)
						,m_inlinek(INLINEK_Nil)
						,m_mcallcon(MCALLCON_Nil)
							{ ; }

	bool				FHasVarArgs() const
							{ return m_grftinproc.FIsSet(FTINPROC_HasVarArgs); }
	bool				FIsForeign() const
							{ return m_grftinproc.FIsSet(FTINPROC_IsForeign); }
	bool				FHasGenericArgs() const
							{ return m_grftingen.FIsAnySet(FTINGEN_HasGenericArgs); }

	Moe::InString				m_istrMangled;
	STNode *					m_pStnodDefinition;
	Moe::CAllocAry<TypeInfo *>	m_arypTinParams;
	Moe::CAllocAry<TypeInfo *>	m_arypTinReturns;
	Moe::CAllocAry<GRFPARMQ>	m_mpIptinGrfparmq;

	GRFTINGEN					m_grftingen;
	GRFTINPROC					m_grftinproc;
	INLINEK						m_inlinek;
	MCALLCON					m_mcallcon;
};

struct TypeInfoAnchor : public TypeInfo // tag = tinanc
{
	static const TINK s_tink = TINK_Anchor;
						TypeInfoAnchor(const Moe::InString & istrName)
						:TypeInfo(istrName, s_tink)
						,m_pStnodDefinition(nullptr)
							{ ; }

	STNode *			m_pStnodDefinition;
};


enum LITK
{
	LITK_Numeric,
	LITK_Char,
	LITK_String,
	LITK_Bool,
	LITK_Null,
	LITK_Enum,
	LITK_Compound,
	LITK_Pointer,	// pointer literal in bytecode

	MOE_MAX_MIN_NIL(LITK)
};

const char * PChzFromLitk(LITK litk);

// subset of litk for simple types, (complex types aren't represented by a single stvalue?)
enum STVALK // Syntax Tree VALue Kind
{
	STVALK_Float,
	STVALK_SignedInt,
	STVALK_UnsignedInt,
	STVALK_Null,
	STVALK_Bool,
	STVALK_String,

	MOE_MAX_MIN_NIL(STVALK)
};



struct LiteralType	// litty
{
			LiteralType()
			:m_litk(LITK_Nil)
			,m_cBit(-1)
			,m_grfnum(FNUM_IsSigned)
				{ ; }

			LiteralType(LITK litk, s8 cBit = -1, GRFNUM grfnum = FNUM_IsSigned)
			:m_litk(litk)
			,m_cBit(cBit)
			,m_grfnum(grfnum)
				{ ; }

	LITK	m_litk;
	s8		m_cBit;
	GRFNUM	m_grfnum;
};

// NOTE: just documenting a subtle relationship: The AST stores a literal value and is enough to determine 
// what the STypeInfoLiteral *could* be, (ie PTinlitDefault, PTinlitTightest) but the actual type is determined
// by the nodes type passed down after type checking.

struct TypeInfoLiteral : public TypeInfo // tag = tinlit
{
	static const TINK s_tink = TINK_Literal;

						TypeInfoLiteral()
						:TypeInfo(IstrIntern(""), s_tink)
						,m_c(-1)
						,m_pTinSource(nullptr)
						,m_fIsFinalized(false)
						,m_litty()
						,m_pStnodDefinition(nullptr)
							{ ; }
	
	s64					m_c;
	TypeInfo *			m_pTinSource;		// source type (for finalized null pointers or enum literals)
	bool				m_fIsFinalized;		// literals are finalized once they are assigned to a concrete (or default) type
	LiteralType			m_litty;
	STNode *			m_pStnodDefinition;	// (needed for array literal values)
};

struct TypeStructMember	// tag = typememb
{
					TypeStructMember()
					:m_istrName()
					,m_pTin(nullptr)
					,m_pStdecl(nullptr)
					,m_dBOffset(-1)
						{ ;}

	Moe::InString	m_istrName;
	TypeInfo *		m_pTin;
	STDecl *		m_pStdecl;		// syntax tree node for this member
	s32				m_dBOffset;		// for bytecode GEP
};

struct TypeInfoStruct : public TypeInfo	// tag = tinstruct
{
	static const TINK s_tink = TINK_Struct;

										TypeInfoStruct(const Moe::InString & istrName)
										:TypeInfo(istrName, s_tink)
										,m_pStnodStruct(nullptr)
										,m_pGenmap(nullptr)
										,m_pTinstructInstFrom(nullptr)
										,m_aryTypemembField()
										,m_pTinprocInit(nullptr)
										,m_grftingen(FTINGEN_None)
										,m_cB(-1)
										,m_cBAlign(-1)
											{ ; }
	
	bool								FHasGenericParams() const
											{ return m_grftingen.FIsAnySet(FTINGEN_HasGenericArgs); }

	STNode *							m_pStnodStruct;			// node that defined this struct (or struct instantiation)

	GenericMap *						m_pGenmap;				// generic mapping this was instantiated with	
	TypeInfoStruct *					m_pTinstructInstFrom;	// generic base that this struct was instantiated from
																//   null if not generic or root generic definition

	Moe::CAllocAry<TypeStructMember>	m_aryTypemembField;
	TypeInfoProcedure *					m_pTinprocInit;			// procedure used when cginitk == CGINITK_InitializerProc

	GRFTINGEN							m_grftingen;
	s64									m_cB;
	s64									m_cBAlign;
};

int ITypemembLookup(TypeInfoStruct * pTinstruct, const Moe::InString & istrMemberName);

struct TypeInfoEnumConstant // tag = tinecon
{
	Moe::InString		m_istrName;
	BigInt				m_bintValue;
};

enum ENUMK
{
	ENUMK_Basic,
	ENUMK_FlagEnum,
};

struct TypeInfoEnum : public TypeInfo	// tag = tinenum
{
	static const TINK s_tink = TINK_Enum;

						TypeInfoEnum(const Moe::InString & istrName)
						:TypeInfo(istrName, s_tink)
						,m_pTinLoose(nullptr)
						,m_enumk(ENUMK_Basic)
						,m_bintMin()
						,m_bintMax()
						,m_bintLatest()
						,m_tinstructProduced(istrName)
							{ ; }

	TypeInfo *			m_pTinLoose;
	ENUMK				m_enumk;
	BigInt				m_bintMin;
	BigInt				m_bintMax;
	BigInt				m_bintLatest;
	TypeInfoStruct 		m_tinstructProduced;

	Moe::CAllocAry<TypeInfoEnumConstant>	
						m_aryTinecon;
};

enum ARYK
{
    ARYK_Fixed		= 0,	// c-style fixed size array.			aN : [3] int;
    ARYK_Dynamic	= 1,	// dynamically resizing array.			aN : [..] int;
    ARYK_Reference	= 2,	// reference to array of either type.	aN : [] int;

	MOE_MAX_MIN_NIL(ARYK)
};
const char * PChzFromAryk(ARYK aryk);

enum ARYMEMB
{
	ARYMEMB_Count,
	ARYMEMB_Data,

	MOE_MAX_MIN_NIL(ARYMEMB)
};

const char * PChzFromArymemb(ARYMEMB arymemb);
ARYMEMB ArymembLookup(const char * pChzMember);

struct TypeInfoArray : public TypeInfo	// tag = tinary
{
	static const TINK s_tink = TINK_Array;

					TypeInfoArray()
					:TypeInfo(IstrIntern(""), s_tink)
					,m_pTin(nullptr)
					,m_pTinstructImplicit(nullptr)
					,m_c(0)
					,m_aryk(ARYK_Fixed)
					,m_pStnodBakedDim(nullptr)
					{ ; }

	TypeInfo *			m_pTin;
	TypeInfoStruct *	m_pTinstructImplicit;
	s64					m_c;
	STNode *			m_pStnodBakedDim; // workaround for arrays with unspecialized baked constant
	ARYK				m_aryk;
};

void DeleteTypeInfo(Moe::Alloc * pAlloc, TypeInfo * pTin);
bool FTypesAreSame(TypeInfo * pTinLhs, TypeInfo * pTinRhs);


struct TypeRegistry // tag treg
{
							TypeRegistry(Moe::Alloc * pAlloc);
							~TypeRegistry()
								{ Clear(); }

	void					Clear();
	TypeInfo *				PTinMakeUnique(TypeInfo * pTin);

	Moe::Alloc *						m_pAlloc;
	Moe::CHash<u64, TypeInfo *>			m_hashHvPTinUnique;
};

struct OpTypes // tag = optype
{
				OpTypes()
				:m_pTinLhs(nullptr)
				,m_pTinRhs(nullptr)
				,m_pTinResult(nullptr)
				,m_pTinprocOverload(nullptr)
					{ ; }

				OpTypes(TypeInfo * pTinLhs, TypeInfo * pTinRhs, TypeInfo * pTinResult)
				:m_pTinLhs(pTinLhs)
				,m_pTinRhs(pTinRhs)
				,m_pTinResult(pTinResult)
				,m_pTinprocOverload(nullptr)
					{ ; }

				bool FIsValid() const
					{ return m_pTinResult != nullptr; }

	TypeInfo *				m_pTinLhs;
	TypeInfo *				m_pTinRhs;
	TypeInfo *				m_pTinResult;
	TypeInfoProcedure *		m_pTinprocOverload;
};
