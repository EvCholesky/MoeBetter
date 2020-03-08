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

// changing the way we ensure types are unique, need to keep both versions around as I make sure I don't break anything
#define UNIQUE_TYPE_CLEANUP 1

// Uniqueness notes
// Types should be unique from creation, don't create types until we have enough info to ensure uniqueness.
//  - types aliased by typedef are their own typeinfo and refer to the native type 
//  - 

struct GenericMap;
struct ProcedureAttrib;
struct STDecl;
struct STNode;
struct STStruct;
struct StructAttrib;
struct TypeInfo;
struct TypeInfoProcedure;
struct TypeStructMember;

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
		BLTIN(EnumMin) STR(min)	\
		BLTIN(EnumLast) STR(last) \
		BLTIN(EnumMax) STR(max)	\
		BLTIN(EnumNone) STR(none) \
		BLTIN(EnumAll) STR(all)	\
		BLTIN(EnumNames) STR(names) \
		BLTIN(EnumValues) STR(values) \
		BLTIN(EnumFlag) STR(_flag)	\
		BLTIN(TypeInfo) STR(TypeInfo) \
		BLTIN(Loose) STR(loose) \
		BLTIN(Strict) STR(strict) \
		BLTIN(GlobalTinTable) STR(_tinTable)	

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

// unique scope id used uniquify-ing named types (unique = name+scopid)
enum SCOPID : s32
{
	SCOPID_Min = 0,
	SCOPID_Nil = -1,
};

enum LITK : s8
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

enum FTIN
{
	FTIN_IsCanon	= 0x1,			// canonical type is unique, derived from the generic root type and is only parameterized by canonical types

	FTIN_None		= 0x0,
	FTIN_All		= 0x3
};

MOE_DEFINE_GRF(GRFTIN, FTIN, u8);

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

enum FPARMQ	// Flags for ARGument Qualifiers
{
	FPARMQ_ImplicitRef		= 0x1,		// convert LValue argument and pass into procedure's pointer argument
	FPARMQ_BakedValue		= 0x2,		// baked value parameter, will be dropped during generic specialization.
	FPARMQ_TypeArgument		= 0x4,		// type only argument, no runtime value - dropped during generic specialization

	FPARMQ_None				= 0x0,
	FPARMQ_All				= 0x7,
};

MOE_DEFINE_GRF(GRFPARMQ, FPARMQ, u8);

enum NUMK : s8
{
	NUMK_Float,
	NUMK_UnsignedInt,
	NUMK_SignedInt,

	MOE_MAX_MIN_NIL(NUMK)
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

enum ARYK
{
    ARYK_Fixed		= 0,	// c-style fixed size array.			aN : [3] int;
    ARYK_Dynamic	= 1,	// dynamically resizing array.			aN : [..] int;
    ARYK_Reference	= 2,	// reference to array of either type.	aN : [] int;

	MOE_MAX_MIN_NIL(ARYK)
};
const char * PChzFromAryk(ARYK aryk);

struct SDataLayout		// tag = dlay
{
	s32		m_cBBool;			// byte size of "bool"
	s32		m_cBInt;			// byte size of "int"
	s32		m_cBFloat;			// byte size of "float"
	s32		m_cBPointer;		// byte size of pointer
	s32		m_cBStackAlign;		// code gen stack alignment
};

// hash value for JUST the decorator descriptor, not the child type 
struct Hvdec
{
				Hvdec()
				:m_hv(0)
					{ ; }

	explicit	Hvdec(u64 hv)
				:m_hv(hv)
					{ ; }

	bool		operator==(const Hvdec & hvdecOther)
					{ return m_hv == hvdecOther.m_hv; }

	// BB - this is gross and increases the chance of collisions. We need a hash map that works on hv64.
				operator HV () const
					{ return HvFromPBFVN(&m_hv, sizeof(m_hv)); }

	u64			m_hv;
};

Hvdec HvdecLiteral(LITK litk, s8 cBit, NUMK numk, bool fIsFinalized, s64 c);
Hvdec HvdecNumeric(u32 cBit, NUMK numk);
Hvdec HvdecPointer(bool fIsImplicitRef);
Hvdec HvdecQualifier(GRFQUALK grfqualk);
Hvdec HvdecArray(ARYK aryk, u64 c, STNode * pStnodBakedDim);
Hvdec HvdecNamed(TINK tink, const Moe::InString & istrName, SCOPID scopid);
Hvdec HvdecStruct(
	const Moe::InString & istrName,
	SCOPID scopid,
	size_t cTypememb, 
	const TypeStructMember * pTypememb,
	const StructAttrib & structattrib);

Hvdec HvdecProcedure(
	const Moe::InString & istrName,
	SCOPID scopid,
	size_t cParam, 
	size_t cReturn,
	const TypeInfo * const * ppTinParam,
	const TypeInfo * const * ppTinReturn,
	const GRFPARMQ * pGrfparmq,
	const ProcedureAttrib & procattrib);

// hash value for the entire unique type, the decorator descriptor and the type it decorates
struct Hvtype
{
				Hvtype()
				:m_hv(0)
					{ ; }

	explicit	Hvtype(u64 hv)
				:m_hv(hv)
					{ ; }

	bool		operator==(const Hvtype & hvtypeOther)
					{ return m_hv == hvtypeOther.m_hv; }

	// BB - this is gross and increases the chance of collisions. We need a hash map that works on hv64.
				operator HV () const
					{ return HvFromPBFVN(&m_hv, sizeof(m_hv)); }
	u64			m_hv;
};

inline Hvtype HvtypeFromPTinChild(Hvdec hvdec, const TypeInfo * pTinChild);



struct TypeInfo	// tag = tin
{
						TypeInfo(const Moe::InString & istrName, TINK tink, Hvdec hvdec)
						:m_tink(tink)
						,m_grftin(FTIN_None)
						,m_istrName(istrName)
#if KEEP_TYPEINFO_DEBUG_STRING
						,m_istrDebug()
#endif
						,m_hvdec(hvdec)
						,m_pTinUnaliased(nullptr)
							{ ; }

    TINK				m_tink;
	GRFTIN				m_grftin;

	Moe::InString		m_istrName;				// user facing name 

#if KEEP_TYPEINFO_DEBUG_STRING
	Moe::InString		m_istrDebug;	
#endif

	// BB - This shouldn't be embedded in the typeinfo - it won't work for multiple codegen passes
	//CodeGenValueRef	m_pCgvalDIType;
	//CodeGenValueRef	m_pCgvalReflectGlobal;	// global variable pointing to the type info struct
												// const TypeInfo entry in the reflection type table

	Hvdec				m_hvdec;
	const TypeInfo *	m_pTinUnaliased;		// native non-aliased source type (ie sSize->s64)
};

Moe::InString IstrFromTypeInfo(const TypeInfo * pTin);

template <typename T>
T PTinRtiCast(TypeInfo * pTin)
{
	if (pTin && pTin->m_tink == Moe::SStripPointer<T>::Type::s_tink)
		return (T)pTin;
	return nullptr;
}

template <typename T>
const T PTinRtiCast(const TypeInfo * pTin)
{
	if (pTin && pTin->m_tink == Moe::SStripPointer<T>::Type::s_tink)
		return (const T)pTin;
	return nullptr;
}

template <typename T>
T PTinDerivedCast(TypeInfo * pTin)
{
	MOE_ASSERT(pTin && pTin->m_tink == Moe::SStripPointer<T>::Type::s_tink, "illegal type info derived cast");
	return (T)pTin;
}

template <typename T>
const T PTinDerivedCast(const TypeInfo * pTin)
{
	MOE_ASSERT(pTin && pTin->m_tink == Moe::SStripPointer<T>::Type::s_tink, "illegal type info derived cast");
	return (const T)pTin;
}

const char * PChzTypeFromNumk(NUMK numk);
inline bool FIsSigned(NUMK numk)
	{ return numk == NUMK_Float || numk == NUMK_SignedInt; }
inline bool FIsInteger(NUMK numk)
	{ return numk != NUMK_Float; }

struct TypeInfoNumeric : public TypeInfo // tag = tinn
{
	static const TINK s_tink = TINK_Numeric;

			TypeInfoNumeric(const Moe::InString & istrName, u32 cBit, NUMK numk)
			:TypeInfo(istrName, s_tink, HvdecNumeric(cBit, numk))
			,m_cBit(cBit)
			,m_numk(numk)
				{ ; }

	u32		m_cBit;
	NUMK	m_numk;
};

struct TypeInfoPointer : public TypeInfo	// tag = tinptr
{
	static const TINK s_tink = TINK_Pointer;

						TypeInfoPointer(const TypeInfo * pTin, bool fIsImplicitRef)
						:TypeInfo(IstrIntern(""), s_tink, HvdecPointer(fIsImplicitRef))
						,m_pTin(pTin)
						,m_fIsImplicitRef(false)
							{ ; }

	const TypeInfo *	m_pTin;
	bool				m_fIsImplicitRef;
};

void AppendFlagNames(Moe::StringBuffer * pStrbuf, GRFQUALK grfqualk, const char * pChzSpacer);

struct TypeInfoQualifier : public TypeInfo // tag == tinqual
{
	static const TINK s_tink = TINK_Qualifier;

						TypeInfoQualifier(const TypeInfo * pTin, GRFQUALK grfqualk)
						:TypeInfo(IstrIntern(""), s_tink, HvdecQualifier(grfqualk))
						,m_grfqualk(grfqualk)
						,m_pTin(pTin)
							{ ; }

	GRFQUALK			Grfqualk() const 
							{ return m_grfqualk; }

	const TypeInfo *	m_pTin;

protected:
	GRFQUALK			m_grfqualk;
};

const TypeInfo * PTinStripQualifiers(const TypeInfo * pTin, GRFQUALK * pGrfqualk);
const TypeInfo * PTinStripQualifiers(const TypeInfo * pTin);



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

struct ProcedureAttrib
{
						ProcedureAttrib()
						:m_grftingen(FTINGEN_None)
						,m_grftinproc(FTINPROC_None)
						,m_inlinek(INLINEK_Nil)
						,m_mcallcon(MCALLCON_Nil)
							{ ; }

	GRFTINGEN			m_grftingen;
	GRFTINPROC			m_grftinproc;
	INLINEK				m_inlinek;
	MCALLCON			m_mcallcon;
};

struct TypeInfoProcedure : public TypeInfo	// tag = 	tinproc
{
	static const TINK s_tink = TINK_Procedure;

						TypeInfoProcedure(
							const Moe::InString & istrName, 
							SCOPID scopid, 
							size_t cParam,
							size_t cReturn,
							TypeInfo const ** ppTinParam,
							TypeInfo const ** ppTinReturn,
							GRFPARMQ * pGrfparmq,
							ProcedureAttrib procattrib)
						:TypeInfo(istrName, s_tink, HvdecProcedure(istrName, scopid, cParam, cReturn, ppTinParam, ppTinReturn, pGrfparmq, procattrib))
						,m_istrMangled()
						,m_pStnodDefinition(nullptr)
						,m_arypTinParams()
						,m_arypTinReturns()
						,m_mpIptinGrfparmq()
						,m_scopid(scopid)
						,m_procattrib()
							{
								m_arypTinParams.SetArray(ppTinParam, cParam, cParam);
								m_arypTinReturns.SetArray(ppTinReturn, cReturn, cReturn);
								m_mpIptinGrfparmq.SetArray(pGrfparmq, cParam, cParam);
							}

	bool				FHasVarArgs() const
							{ return m_procattrib.m_grftinproc.FIsSet(FTINPROC_HasVarArgs); }
	bool				FIsForeign() const
							{ return m_procattrib.m_grftinproc.FIsSet(FTINPROC_IsForeign); }
	bool				FIsCommutative() const
							{ return m_procattrib.m_grftinproc.FIsSet(FTINPROC_IsCommutative); }
	bool				FHasGenericArgs() const
							{ return m_procattrib.m_grftingen.FIsAnySet(FTINGEN_HasGenericArgs); }
	bool				FHasBakedValueArgs() const
							{ return m_procattrib.m_grftingen.FIsAnySet(FTINGEN_HasBakedValueArgs); }

	Moe::InString						m_istrMangled;
	STNode *							m_pStnodDefinition;
	Moe::CAllocAry<const TypeInfo *>	m_arypTinParams;
	Moe::CAllocAry<const TypeInfo *>	m_arypTinReturns;
	Moe::CAllocAry<GRFPARMQ>			m_mpIptinGrfparmq;

	SCOPID								m_scopid;
	ProcedureAttrib						m_procattrib;
};

struct TypeInfoAnchor : public TypeInfo // tag = tinanc
{
	static const TINK s_tink = TINK_Anchor;
						TypeInfoAnchor(const Moe::InString & istrName, SCOPID scopid)
						:TypeInfo(istrName, s_tink, HvdecNamed(s_tink, istrName, scopid))
						,m_pStnodDefinition(nullptr)
							{ ; }

	STNode *			m_pStnodDefinition;
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
			,m_numk(NUMK_SignedInt)
				{ ; }

			LiteralType(LITK litk, s8 cBit = -1, NUMK numk = NUMK_SignedInt)
			:m_litk(litk)
			,m_cBit(cBit)
			,m_numk(numk)
				{ ; }

	LITK	m_litk;
	s8		m_cBit;
	NUMK	m_numk;
};

// NOTE: just documenting a subtle relationship: The AST stores a literal value and is enough to determine 
// what the STypeInfoLiteral *could* be, (ie PTinlitDefault, PTinlitTightest) but the actual type is determined
// by the nodes type passed down after type checking.

struct TypeInfoLiteral : public TypeInfo // tag = tinlit
{
	static const TINK s_tink = TINK_Literal;

						TypeInfoLiteral(LITK litk, s8 cBit, NUMK numk, bool fIsFinalized, s64 c)
						:TypeInfo(IstrIntern(""), s_tink, HvdecLiteral(litk, cBit, numk, fIsFinalized, c))
						,m_c(c)
						,m_pTinSource(nullptr)
						,m_fIsFinalized(fIsFinalized)
						,m_litty(litk, cBit, numk)
							{ ; }
	
	s64					m_c;
	const TypeInfo *	m_pTinSource;		// source type (for finalized null pointers or enum literals)
	bool				m_fIsFinalized;		// literals are finalized once they are assigned to a concrete (or default) type
	LiteralType			m_litty;

#if !UNIQUE_TYPE_CLEANUP
	STNode *			m_pStnodDefinition;	// (needed for array literal values)
#endif
};

struct TypeStructMember	// tag = typememb
{
						TypeStructMember()
						:m_istrName()
						,m_pStdecl(nullptr)
						,m_dBOffset(-1)
							{ ;}

	const TypeInfo *	PTin() const;

	Moe::InString		m_istrName;
	STDecl *			m_pStdecl;		// syntax tree node for this member
	s32					m_dBOffset;		// for bytecode GEP
};

struct StructAttrib
{
						StructAttrib()
						:m_grftin(FTIN_None)
						,m_grftingen(FTINGEN_None)
							{ ; }

	GRFTIN				m_grftin;
	GRFTINGEN			m_grftingen;
};

struct TypeInfoStruct : public TypeInfo	// tag = tinstruct
{
	static const TINK s_tink = TINK_Struct;

										TypeInfoStruct(
											const Moe::InString & istrName, 
											SCOPID scopid, 
											size_t cTypememb,
											TypeStructMember const * pTypememb,
											const StructAttrib & structattrib)
										:TypeInfo(istrName, s_tink, HvdecStruct(istrName, scopid, cTypememb, pTypememb, structattrib))
										,m_pStnodStruct(nullptr)
										,m_pGenmap(nullptr)
										,m_pTinstructInstFrom(nullptr)
										,m_aryTypemembField()
										,m_pTinprocInit(nullptr)
										,m_grftingen(structattrib.m_grftingen)
										,m_scopid(scopid)
										,m_cB(-1)
										,m_cBAlign(-1)
											{ m_grftin = structattrib.m_grftin; }
	
	bool								FHasGenericParams() const
											{ return m_grftingen.FIsAnySet(FTINGEN_HasGenericArgs); }

	STNode *							m_pStnodStruct;		// node that defined this struct (or struct instantiation)

	GenericMap *						m_pGenmap;				// generic mapping this was instantiated with	
	const TypeInfoStruct *				m_pTinstructInstFrom;	// generic base that this struct was instantiated from
																//   null if not generic or root generic definition

	Moe::CAllocAry<TypeStructMember>	m_aryTypemembField;
	const TypeInfoProcedure *			m_pTinprocInit;			// procedure used when cginitk == CGINITK_InitializerProc

	GRFTINGEN							m_grftingen;
	SCOPID								m_scopid;
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

						TypeInfoEnum(const Moe::InString & istrName, SCOPID scopid)
						:TypeInfo(istrName, s_tink, HvdecNamed(s_tink, istrName, scopid))
						,m_pTinLoose(nullptr)
						,m_enumk(ENUMK_Basic)
						,m_bintMin()
						,m_bintMax()
						,m_bintLatest()
						,m_tinstructProduced(istrName, scopid, 0, nullptr, StructAttrib())
							{ ; }

	const TypeInfo *	m_pTinLoose;
	ENUMK				m_enumk;
	BigInt				m_bintMin;
	BigInt				m_bintMax;
	BigInt				m_bintLatest;
	TypeInfoStruct 		m_tinstructProduced;

	Moe::CAllocAry<TypeInfoEnumConstant>	
						m_aryTinecon;
};

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

					TypeInfoArray(const TypeInfo * pTinElement, ARYK aryk, s64 c, STNode * pStnodBakedDim)
					:TypeInfo(IstrIntern(""), s_tink, HvdecArray(aryk, c, pStnodBakedDim))
					,m_pTin(pTinElement)
					,m_pTinstructImplicit(nullptr)
					,m_c(0)
					,m_aryk(ARYK_Fixed)
					,m_pStnodBakedDim(nullptr)
					{ ; }

	const TypeInfo *	m_pTin;
	TypeInfoStruct *	m_pTinstructImplicit;
	s64					m_c;
	STNode *			m_pStnodBakedDim; // workaround for arrays with unspecialized baked constant
	ARYK				m_aryk;
};

void DeleteTypeInfo(Moe::Alloc * pAlloc, const TypeInfo * pTin);
bool FTypesAreSame(const TypeInfo * pTinLhs, const TypeInfo * pTinRhs);

// Type Decorator Set
//  one per decorated type, ie. one instance for Foo struct that maps to Foo*, const Foo, [3] Foo, etc
struct TypeDecoratorSet // tag = tdecset
{
							TypeDecoratorSet(Moe::Alloc * pAlloc)
							:m_hashHvdescPtin(pAlloc, Moe::BK_TypeRegistry, 16)
								{ ; }

	Moe::CHash<Hvdec, const TypeInfo *>		m_hashHvdescPtin;		// map from tdesc to decorating type
};

struct TypeRegistry // tag typer
{
							TypeRegistry(Moe::Alloc * pAlloc);
							~TypeRegistry()
								{ Clear(); }

	void					Clear();
	const TypeInfo *		PTinMakeUnique(const TypeInfo * pTin);

	TypeInfo const **		PPTinEnsureDecorator(const TypeInfo * pTin, Hvdec hvdesc);
	SCOPID					ScopidAlloc()
								{ m_scopidNext = SCOPID(m_scopidNext + 1); return m_scopidNext; }

	Moe::Alloc *							m_pAlloc;
	SCOPID									m_scopidNext;		// global id for symbol tables, used in for unique types strings
	Moe::CHash<Hvtype, const TypeInfo *>	m_hashHvtypePTinUnique;
	Moe::CHash<const TypeInfo *, TypeDecoratorSet *>	
											m_hashPTinPTdecset;
};

struct OpTypes // tag = optype
{
				OpTypes()
				:m_pTinLhs(nullptr)
				,m_pTinRhs(nullptr)
				,m_pTinResult(nullptr)
				,m_pTinprocOverload(nullptr)
					{ ; }

				OpTypes(const TypeInfo * pTinLhs, const TypeInfo * pTinRhs, const TypeInfo * pTinResult)
				:m_pTinLhs(pTinLhs)
				,m_pTinRhs(pTinRhs)
				,m_pTinResult(pTinResult)
				,m_pTinprocOverload(nullptr)
					{ ; }

				bool FIsValid() const
					{ return m_pTinResult != nullptr; }

	const TypeInfo *				m_pTinLhs;
	const TypeInfo *				m_pTinRhs;
	const TypeInfo *				m_pTinResult;
	const TypeInfoProcedure *		m_pTinprocOverload;
};
