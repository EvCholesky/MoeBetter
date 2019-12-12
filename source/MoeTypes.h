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

#include <xmmintrin.h>
#include <ctype.h>
#include <emmintrin.h>
#include <new>
#include <stdint.h>

#if _WIN64
#define MOE_X64 1
#elif MOE_OSX_X64
#define MOE_X64 1
#else
#define MOE_X64 0
#endif

#if MOE_X64
#define STBM_POINTER_SIZE 64
#define STBM_UINT32 uint32_t
#define STBM_UINTPTR uintptr_t
#endif

//#define STBM_DEBUGCHECK
#include "stb_malloc.h"



typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef wchar_t WChar; // tag = wch

typedef float f32;
typedef double f64;

typedef u32 HV; // Hash Value

enum TFN
{
	TFN_False,
	TFN_True,
	TFN_Nil = -1,
};

// BB - Messy dependency, defined in MoeString.cpp
u32 HvFromPBFVN(const void * pV, size_t cB);

namespace Moe
{

typedef __m128 Simd;
typedef __m128i SimdI;
typedef const Simd & SimdArg;

struct Simd3 { Simd x, y, z; };
struct Simd4 { Simd x, y, z, w; };

// standard macros

#if defined( __GNUC__ )
	#define		MOE_FORCE_INLINE	inline __attribute__((always_inline))
	#define		MOE_ALIGN(CB)		__attribute__((aligned(CB)))
	#define 	MOE_ALIGN_OF(T) 	__alignof__(T)
	#define		MOE_IS_ENUM(T)		__is_enum(T)
	#define		MOE_DEBUG_BREAK()	asm ("int $3")
#elif defined( _MSC_VER )
	#define		MOE_FORCE_INLINE	__forceinline
	#define		MOE_ALIGN(CB)		__declspec(align(CB))
	#define 	MOE_ALIGN_OF(T) 	__alignof(T)
	#define		MOE_IS_ENUM(T)		__is_enum(T)
	#define		MOE_DEBUG_BREAK()	__debugbreak()
#elif defined( __clang__)
	#define		MOE_FORCE_INLINE	inline __attribute__((always_inline))
	#define		MOE_ALIGN(CB)		__attribute__((aligned(CB)))
	#define 	MOE_ALIGN_OF(T) 	__alignof__(T)
	#define		MOE_IS_ENUM(T)		__is_enum(T)
	#define		MOE_DEBUG_BREAK()   asm("int $3")
#endif

#define MOE_OFFSET_OF(STRUCT_NAME, VAR_NAME)	offsetof(STRUCT_NAME, VAR_NAME)
#define MOE_DIM(arr) (sizeof(arr) / sizeof(*arr))
#define MOE_PMAC(arr) &arr[MOE_DIM(arr)]

#define MOE_RELEASE 0
#if MOE_RELEASE
#define MOE_TWEAK static const
#else
#define MOE_TWEAK static
#endif

#define MOE_ENUM_UTILS(ENUM_NAME)  \
	inline bool FIsValid(ENUM_NAME e) { return (e >= ENUM_NAME##_Min) & (e < ENUM_NAME##_Max); } \
	inline ENUM_NAME VerifyValidElement(ENUM_NAME e) { if (MOE_FVERIFY(FIsValid(e), "array access with bad " #ENUM_NAME)) return e; return (ENUM_NAME)0; }

#define MOE_MAX_MIN_NIL(ENUM_NAME) ENUM_NAME##_Max, ENUM_NAME##_Min = 0, ENUM_NAME##_Nil = -1 \
	}; MOE_ENUM_UTILS(ENUM_NAME) \
	enum ENUM_NAME##_Stub {

void AssertHandler( const char* pChzFile, u32 line, const char* pChzCondition, const char* pChzMessage = 0, ...);

#define MOE_VERIFY( PREDICATE, ... ) \
	do { if (!(PREDICATE)) { \
		Moe::AssertHandler(__FILE__, __LINE__, #PREDICATE, __VA_ARGS__); \
		MOE_DEBUG_BREAK(); \
	} } while (0)

#define MOE_ASSERT( PREDICATE, ... ) MOE_VERIFY(PREDICATE, __VA_ARGS__);

#define MOE_CASSERT( PREDICATE, ERROR ) static_assert(PREDICATE, "CASSERT: " ERROR)


#if defined( _MSC_VER )
#define MOE_FVERIFY_PROC( PREDICATE, ASSERTPROC, FILE, LINE, ... )\
(\
  ( ( PREDICATE ) ? \
	true :\
	(\
	  ASSERTPROC( FILE, LINE, #PREDICATE, __VA_ARGS__ ),\
	  MOE_DEBUG_BREAK(), \
	  false\
	)\
  )\
)
#else
// use a goofy expression statement to play nice with clang
#define MOE_FVERIFY( PREDICATE, ... )\
(\
  ( ( PREDICATE ) ? \
	true :\
	({\
	  ASSERTPROC( FILE, LINE, #PREDICATE, __VA_ARGS__ );\
	  MOE_DEBUG_BREAK(); \
	  false;\
	})\
  )\
)
#endif

#define MOE_FVERIFY(PREDICATE, ...) \
	MOE_FVERIFY_PROC (PREDICATE, Moe::AssertHandler, __FILE__, __LINE__, __VA_ARGS__ )

#define MOE_FASSERT( PREDICATE, ... ) MOE_FVERIFY(PREDICATE, __VA_ARGS__)

#define MOE_TRACE(PREDICATE, ...) \
do { if (PREDICATE) \
	{ printf(__VA_ARGS__); } \
} while (0)

void DoNothing();
static_assert(sizeof(s64) == 8, "wha");
static_assert(sizeof(s32) == 4, "wha");

// Memory functions

void	FillAB(u8 b, void * pDest, size_t cB);
void	ZeroAB(void * pDest, size_t cB);
void	CopyAB(const void * pSource, void * pDest, size_t cB);
bool	FAreSameAB(const void * aB0, void * aB1, size_t cB);

inline s32 S32Coerce(s64 n)		{ s32 nRet = (s32)n;	MOE_ASSERT((s64)nRet == n, "S32Coerce failure"); return nRet; }
inline s16 S16Coerce(s64 n)		{ s16 nRet = (s16)n;	MOE_ASSERT((s64)nRet == n, "S16Coerce failure"); return nRet; }
inline s8 S8Coerce(s64 n)		{ s8 nRet = (s8)n;		MOE_ASSERT((s64)nRet == n, "S8Coerce failure");  return nRet; }
inline u32 U32Coerce(u64 n)		{ u32 nRet = (u32)n;	MOE_ASSERT((u64)nRet == n, "u32Coerce failure"); return nRet; }
inline u16 U16Coerce(u64 n)		{ u16 nRet = (u16)n;	MOE_ASSERT((u64)nRet == n, "u16Coerce failure"); return nRet; }
inline u8 U8Coerce(u64 n)		{ u8 nRet = (u8)n;		MOE_ASSERT((u64)nRet == n, "u8Coerce failure");  return nRet; }
 
// type traits
template <typename T> struct SStripConst				{ typedef T Type;	enum { F_STRIPPED= false }; };
template <typename T> struct SStripConst<const T>		{ typedef T Type;	enum { F_STRIPPED= true }; };

template <typename T> struct SStripReference			{ typedef T Type; 	enum { F_STRIPPED= false }; };
template <typename T> struct SStripReference<T&>		{ typedef T Type; 	enum { F_STRIPPED= true }; };

template <typename T> struct SStripPointer				{ typedef T Type; 	enum { F_STRIPPED= false }; };
template <typename T> struct SStripPointer<T*>			{ typedef T Type; 	enum { F_STRIPPED= true }; };

template <typename T> struct SIsReference				{ enum { V = false }; };
template <typename T> struct SIsReference<T&>			{ enum { V = true }; };

template <typename T> struct SIsPointer					{ enum { V = false }; };
template <typename T> struct SIsPointer<T&>				{ enum { V = true }; };

template <typename T> struct SIsSignedInt				{ enum { V = false }; };
template <> struct SIsSignedInt<s8>						{ enum { V = true }; };
template <> struct SIsSignedInt<s16>					{ enum { V = true }; };
template <> struct SIsSignedInt<s32>					{ enum { V = true }; };
template <> struct SIsSignedInt<s64>					{ enum { V = true }; };

// templates to allocate a c-style block with proper alignment that doesn't run constructors

struct MOE_ALIGN(16)	SAlignedQword  					{ u8 m_a[16]; };
template <int ALIGNMENT> struct SAlignedBlock			{ SAlignedBlock() {MOE_CASSERT(sizeof(*this), "unknown alignment in SAlignedBlock");}  };
template <> struct SAlignedBlock<0>						{ u8 m_b; };
template <> struct SAlignedBlock<1>						{ u8 m_b1; };
template <> struct SAlignedBlock<2>						{ u16 m_b2; };
template <> struct SAlignedBlock<4>						{ u32 m_b4; };
template <> struct SAlignedBlock<8>						{ u64 m_b8; };
template <> struct SAlignedBlock<16>					{ SAlignedQword m_b16; };

template <int CB, int ALIGNMENT>
struct SAlignedBytes // tag=alby
{
	void *	A()		{ return m_aB; }
	size_t	CBMax()	{ return CB; }

	SAlignedBlock<ALIGNMENT> m_aB[(CB + (ALIGNMENT-1)) / ALIGNMENT];
};

//Alexanderscu's compile time conversion test - Modern C++ Design pg 34-36
template <typename SOURCE_TYPE, typename TARGET_TYPE>
struct SCanConvert
{
	typedef u8 FalseType;
	struct TrueType { FalseType a[2]; };
	static TrueType		TestFunction(TARGET_TYPE);
	static FalseType	TestFunction(...);
	static SOURCE_TYPE 	MakeSource();

	enum { V = sizeof(TestFunction(MakeSource())) == sizeof(TrueType) };
};

template <typename T> struct SIsUnsignedInt						{ enum { V = false }; };
template <> struct SIsUnsignedInt<u8>							{ enum { V = true }; };
template <> struct SIsUnsignedInt<u16>							{ enum { V = true }; };
template <> struct SIsUnsignedInt<u32>							{ enum { V = true }; };
template <> struct SIsUnsignedInt<u64>							{ enum { V = true }; };
 
template <typename T> struct SIsInt								{ enum { V  = SIsSignedInt<T>::V  || SIsUnsignedInt<T>::V  }; };

template <typename T> struct SIsFloat							{ enum { V = false }; };
template <> struct SIsFloat<f32>								{ enum { V = true }; };
template <> struct SIsFloat<f64>								{ enum { V = true }; };

template <typename T> struct SIsBool							{ enum { V = false }; };
template <> struct SIsBool<bool>								{ enum { V = true }; };

template <typename T> struct SIsVoid							{ enum { V = false }; };
template <> struct SIsVoid<void>								{ enum { V = true }; };

template <typename T> struct SVoidSafeSizeof					{ enum { V = sizeof(T) }; };
template <> struct SVoidSafeSizeof<void>						{ enum { V = 0 }; };

// NOTE: can't just check static_cast<T>(-1) because it doesn't work for custom types
template <typename T, bool IS_ENUM> struct SIsSignedSelector	{ enum { V = SIsFloat<T>::V || SIsSignedInt<T>::V }; };
template <typename T> struct SIsSignedSelector<T, true>			{ enum { V = static_cast<T>(-1) < 0 }; };
template <typename T> struct SIsSigned							{ enum { V = SIsSignedSelector<T, MOE_IS_ENUM(T)>::V }; };

template <typename T> struct SIsFundamentalType					{ enum { V  = 
																		SIsPointer<T>::V  || 
																		SIsReference<T>::V  || 
																		SIsInt<T>::V  || 
																		SIsFloat<T>::V  || 
																		SIsBool<T>::V  || 
																		MOE_IS_ENUM(T) 
																	}; 
															};
template <typename T>
struct SArrayTraits
{
	typedef T Element;
	enum { C_ELEMENTS = -1 };
	enum { F_IS_ARRAY = false };
};

template <typename A, int C>
struct SArrayTraits<A[C]>
{
	typedef A Element;
	enum { C_ELEMENTS = C };
	enum { F_IS_ARRAY = true };
};

template <typename T> struct SHasTrivialConstructor		{ enum { V  = SIsFundamentalType<T>::V  }; };
template <typename T> struct SHasTrivialCopy			{ enum { V  = SIsFundamentalType<T>::V  }; };
template <typename T> struct SHasTrivialDestructor		{ enum { V  = SIsFundamentalType<T>::V  }; };

template <typename T, bool TRIVIAL_CONSTRUCT>
struct SConstructSelector
{
	static void Construct(T * p)
	{
		MOE_ASSERT( ((uintptr_t)p & (MOE_ALIGN_OF(T)-1)) == 0, "trying to construct missaligned object" );
		new (p) T;
	}

	static void ConstructN(T * p, size_t c)
	{
		MOE_ASSERT( ((uintptr_t)p & (MOE_ALIGN_OF(T)-1)) == 0, "trying to construct missaligned object" );
		for (size_t i = 0; i < c; ++i)
			new (p + i) T;
	}
};

template <typename T>
struct SConstructSelector<T, true> // trivial constructor
{
	static void Construct(T * p)					{ }
	static void ConstructN(T * p, size_t c)			{ }
};

template <typename T, bool TRIVIAL_COPY>
struct SCopySelector
{
	static void CopyConstruct(T * p, const T & orig)
	{
		MOE_ASSERT( ((uintptr_t)p & (MOE_ALIGN_OF(T)-1)) == 0, "trying to copy construct missaligned object" );
		new (p) T(orig);
	}

	static void CopyConstructN(T * p, size_t c, const T & orig)
	{
		MOE_ASSERT( ((uintptr_t)p & (MOE_ALIGN_OF(T)-1)) == 0, "trying to copy construct missaligned object" );
		for (size_t i = 0; i < c; ++i)
			new (p + i) T(orig);
	}

	static void CopyConstructArray(T * pTDst, size_t cT, const T * pTSrc)
	{
		MOE_ASSERT( ((uintptr_t)pTDst & (MOE_ALIGN_OF(T)-1)) == 0, "trying to copy construct missaligned object" );
		auto pTDstMax = pTDst + cT;
		for (auto pTDstIt = pTDst; pTDstIt != pTDstMax; ++pTDstIt, ++pTSrc)
			new (pTDstIt) T(*pTSrc);
	}
};

template <typename T>
struct SCopySelector<T, true> // trivial copy constructor
{
	static void CopyConstruct(T * p, const T & orig)					{ *p = orig; }
	static void CopyConstructN(T * p, size_t c, const T & orig)		
	{ 
		for (T * pEnd = &p[c]; p != pEnd; ++p)
			*p = orig;
	}

	static void CopyConstructArray(T * pTDst, size_t cT, const T * pTSrc)		
	{ 
		CopyAB(pTSrc, pTDst, sizeof(T) * cT);
	}
};

template <typename T, bool TRIVIAL_DESTRUCT>
struct SDestructSelector
{
	static void Destruct(T * p)
	{
		p->~T();
	}

	static void DestructN(T * p, size_t c)
	{
		for (size_t i = 0; i < c; ++i)
			(p + i)->~T();
	}
};

template <typename T>
struct SDestructSelector<T, true> // trivial destructor 
{
	static void Destruct(T * p)					{ }
	static void DestructN(T * p, size_t c)		{ }
};

template <typename T> void Construct(T * p)									{ SConstructSelector<T, SHasTrivialConstructor<T>::V >::Construct(p); }
template <typename T> void ConstructN(T * p, size_t c)						{ SConstructSelector<T, SHasTrivialConstructor<T>::V >::ConstructN(p,c); }

template <typename T> void CopyConstruct(T * p, const T & orig)				{ SCopySelector<T, SHasTrivialCopy<T>::V >::CopyConstruct(p, orig); }
template <typename T> void CopyConstructN(T * p, size_t c, const T & orig)	{ SCopySelector<T, SHasTrivialCopy<T>::V >::CopyConstructN(p, c, orig); }
template <typename T> void CopyConstructArray(T * pTDst, size_t cT, const T * pTSrc)	
																			{ SCopySelector<T, SHasTrivialCopy<T>::V >::CopyConstructArray(pTDst, cT, pTSrc); }

template <typename T> void Destruct(T * p)									{ SDestructSelector<T, SHasTrivialDestructor<T>::V >::Destruct(p); }
template <typename T> void DestructN(T * p, size_t c)						{ SDestructSelector<T, SHasTrivialDestructor<T>::V >::DestructN(p,c); }


// String functions

struct StringBuffer // tag=strbuf
{
			StringBuffer()
			:m_pChzBegin(nullptr)
			,m_pChzAppend(nullptr)
			,m_cBMax(0)
				{ ; }

			StringBuffer(char * pChz, size_t cBMax)
			:m_pChzBegin(pChz)
			,m_pChzAppend(pChz)
			,m_cBMax(cBMax)
				{ ; }

	char *	m_pChzBegin;
	char *	m_pChzAppend;
	size_t	m_cBMax;
};

struct Alloc;

inline size_t CBCodepoint(const char * pChz)
{
	if ((*pChz & 0xF8) == 0xF0)	return 4;
	if ((*pChz & 0xF0) == 0xE0)	return 3;
	if ((*pChz & 0xE0) == 0xC0)	return 2;
	return 1;
}

// growing buffer for string edits
struct StringEditBuffer // tag = seb
{
				StringEditBuffer(Alloc * pAlloc)
				:m_pAlloc(pAlloc)
				,m_pChzMin(nullptr)
				,m_pChzBegin(nullptr)
				,m_pChzAppend(nullptr)
				,m_pChzMax(nullptr)
					{ ; }

				~StringEditBuffer();

	void		PrependCh(const char * pChz, size_t cB);
	void		PrependChz(const char * pChz);
	void		AppendCh(const char * pChz, size_t cB);
	void		AppendChz(const char * pChz);
	size_t		CBAppendCodepoint(const char * pChz)
					{
						auto cB = CBCodepoint(pChz);
						AppendCh(pChz, cB+1);	// +1 because AppendCo argument cB counts the null terminator
						return cB;
					}

	void		Clear();
	void		Resize(size_t cBPrefix, size_t cbUsed, size_t cBPostfix);
	char *		PChzAllocateCopy(Alloc * pAlloc);

	char *		PChz()
					{ return m_pChzBegin; }
	size_t		CB()
					{ 
						if (!m_pChzBegin)
							return 0;
						return m_pChzAppend - m_pChzBegin + 1;
					}

	static const int s_cChPrefixPad = 128;

	Alloc *	m_pAlloc;
	char *		m_pChzMin;
	char *		m_pChzBegin;
	char *		m_pChzAppend;
	char *		m_pChzMax;
};

inline bool FIsValid(const StringBuffer & strbuf)
{
	return strbuf.m_pChzBegin != nullptr && strbuf.m_pChzAppend != nullptr;
}

size_t  CBFree(const StringBuffer & strbuf);
size_t	CBCopyChz(const char * pChzSource, char * aCoDest, size_t cBDest);
void	AppendChz(StringBuffer * pStrbuf, const char * pChzSource);
void	AppendToCch(StringBuffer * pStrbuf, char ch, size_t cCh);
void	FormatChz(StringBuffer * pStrbuf, const char * pChzFormat, ...);
void	EnsureTerminated(StringBuffer * pStrbuf, char ch);

size_t	CBChz(const char * pChz);
size_t	CCh(const char * pChz);
size_t  CCodepoint(const char * pChz);
size_t  CBFromChz(const char * pChz, size_t cCodepoint);
void	ConcatPChz(const char* pChzA, const char * pChzB, char * pChOut, size_t cChOutMax);
bool	FAreChzEqual(const char * pChzA, const char * pChzB);
bool	FAreChzEqual(const char * pChzA, const char * pChzB, size_t cCh);
bool	FIsEmptyString(const char * pChzA);
void	ConvertChToWch(const char * pChz, size_t cWchMax, WChar * pWchz);
bool	FPChzContainsChar(const char * pChz, char ch);
void	ReplaceChars(const char * pChSrc, size_t cCh, const char * pChzRemove, char chFill, char * pChDst);

// file utils
size_t CChConstructFilename(const char * pChzFilenameIn, const char * pChzExtension, char * pChzFilenameOut, size_t cChOutMax);
const char * PChzSkipUnicodeBOM(const char * pChzFile);

inline const char * PChzVerifyAscii(const char * pChz)
{
	auto pChzIt = pChz;
	while (*pChzIt != '\0')
	{
		MOE_ASSERT(((*pChzIt) & 0x80) == 0, "unexpected upper ascii character");
		++pChzIt;
	}

	return pChz;
}

// Min/Max/Clamp

template <typename T> T moeMin(T a, T b)						{ return a < b ? a : b; }
template <typename T> T moeMax(T a, T b)						{ return a > b ? a : b; }
template <typename T> T moeClamp(T value, T min, T max)			{ return moeMin(moeMax(value, min), max); }
template <typename T> T moeLerp(T a, T b, f32 gLerp)			{ return a + (b - a) * gLerp; }
template <typename T> void moeSwap(T & a, T & b)				{ T temp = a; a = b; b = temp; }

 // system time

typedef f64 Syst;

inline Syst SystInvalid()										{ return -1.0f; }
inline bool FIsSystValid(Syst syst)								{ return syst >= 0.0f; }
inline Syst SystMax(Syst systA, Syst systB) 					{ return moeMax(systA, systB); }

// reflect types

class CClassType;
typedef u32 TID;
extern TID TID_Nil;

// Allocator
enum BK // block kind
{
	BK_Nil			= -1,
	BK_LexRecover	= 0,
	BK_Core,
	BK_Workspace,
	BK_WorkspaceVal,
	BK_WorkspaceFile,
	BK_TypeRegistry,
	BK_Parse,
	BK_TypeCheck,
	BK_TypeCheckProcmatch,
	BK_TypeCheckStack,
	BK_TypeCheckGenerics,
	BK_Dependency,
	BK_CodeGen,
	BK_CodeGenReflect,
	BK_SyntaxTree,
	BK_IR,
	BK_Symbol,
	BK_Stack,	// should be stack array allocated
	BK_StringTable,
	BK_ReflectTable,
	BK_UnitTest,
	BK_Linker,
	BK_FileSearch,
	BK_ByteCode,
	BK_ByteCodeCreator,
	BK_ByteCodeTest,
	BK_ForeignFunctions,
	BK_Request,
};

#define MOE_ALLOC(numBytes, alignment) 			AllocImpl(numBytes, alignment, __FILE__, __LINE__)
#define MOE_ALLOC_BK(numBytes, alignment, bk) 	AllocImpl(numBytes, alignment, __FILE__, __LINE__, bk)
#define MOE_ALLOC_TYPE(TYPE_NAME) 				AllocImpl(sizeof(TYPE_NAME), MOE_ALIGN_OF(TYPE_NAME), __FILE__, __LINE__)
#define MOE_ALLOC_TYPE_ARRAY(TYPE_NAME, C_MAX) 	AllocImpl(sizeof(TYPE_NAME) * C_MAX, MOE_ALIGN_OF(TYPE_NAME), __FILE__, __LINE__)
#define MOE_NEW(PALLOC, TYPE_NAME)				new ( (PALLOC)->AllocImpl(sizeof(TYPE_NAME), MOE_ALIGN_OF(TYPE_NAME), __FILE__, __LINE__))
#define MOE_FREE(P) 							FreeImpl(P, __FILE__, __LINE__)
#define MOE_DELETE(P) 							DeleteImpl(P, __FILE__, __LINE__)

extern void * STBM_CALLBACK EwcSystemAlloc(void * pUserContext, size_t cBRequested, size_t * pCbProvided);
extern void STBM_CALLBACK EwcSystemFree(void * pUserContext, void *p);

// BB - This allocation tracking code doesn't work yet, it doesn't clean up properly for deletions (it needs to
// add some kind of HV(file,line) into an allocation prefix

#define MOE_TRACK_ALLOCATION
#ifdef MOE_TRACK_ALLOCATION
	inline size_t CBAllocationPrefix() { return sizeof(HV); }
#else
	inline size_t CBAllocationPrefix() { return 0; }
#endif

//#define MOE_USE_SYS_HEAP

struct AllocTracker;
struct Alloc // tag=alloc
{
						Alloc()
						:m_pStbheap(nullptr)
						,m_pAltrac(nullptr)
						,m_cBFree(0)
							{ ; }

						Alloc(void * pBuffer, size_t cB)
						:m_pStbheap(nullptr)
						,m_pAltrac(nullptr)
							{
								Initialize(pBuffer, cB);
							}

						~Alloc()
							{
								Shutdown();
							}

	void				Initialize(void * pB, size_t cB)
							{
								MOE_ASSERT(cB > STBM_HEAP_SIZEOF, "heap is too small");                                                         

								stbm_heap_config config;
								config.system_alloc = EwcSystemAlloc;
								config.system_free = EwcSystemFree;
								config.user_context = nullptr;

								config.minimum_alignment = 8;
								config.align_all_blocks_to_minimum = false;
								config.allocation_mutex = nullptr;
								config.crossthread_free_mutex = nullptr;
								m_cBFree = cB;

								m_pStbheap = stbm_heap_init(pB, cB, &config);
							}

	void				Shutdown()
							{
								if(s_fProgramIsShutdown)
									return;

								if (m_pStbheap)
								{
									stbm_heap_free(m_pStbheap);
									m_pStbheap = nullptr;
								}

								m_cBFree = 0;
							}

	static void			StaticShutdown()
							{ s_fProgramIsShutdown = true; }

	AllocTracker *		PAltrac()
							{ return m_pAltrac; }

	void *				AllocImpl(size_t cB, size_t cBAlign, const char* pChzFile, int cLine, BK bk = BK_Nil)
							{
								size_t cBPrefix = CBAllocationPrefix();
								size_t alignOffset = ((cBAlign > cBPrefix) & (cBPrefix > 0)) ? cBAlign - cBPrefix : 0;

#ifdef MOE_USE_SYS_HEAP
								size_t cBActual = cB + cBPrefix;
								void *pV = malloc(cBActual);							

#else
								MOE_ASSERT(m_pStbheap, "Trying to allocate from a NULL heap");
								void * pV = stbm_alloc_align_fileline(
											nullptr, 
											m_pStbheap, 
											cB + cBPrefix, 
											0, 
											cBAlign, 
											alignOffset, 
											const_cast<char*>(pChzFile), 
											cLine);

								size_t cBActual = stbm_get_allocation_size(pV);
#endif
								
								// BB - We're letting the compiler overflow into system malloc, this is ok - we just
								//  need to change the CAlloc interface from CBFree to CBAllocated.
								//if (!MOE_FVERIFY(cBActual < m_cBFree, "CAlloc out of memory!"))
								//	return nullptr;

								m_cBFree -= cBActual;

								void * pVAdjust = ((char *)pV) + cBPrefix;

								uintptr_t nAlignMask = cBAlign - 1;
								MOE_ASSERT(((uintptr_t)pVAdjust & nAlignMask) == 0, "bad alignment calculation");

#ifdef MOE_TRACK_ALLOCATION
								MOE_ASSERT(cBActual > sizeof(HV), "overflowing allocation");
								HV * pHv = (HV *)pV;
								*pHv = 0;

								if (m_pAltrac)
									TrackAlloc(cBActual, pChzFile, cLine, bk, pHv);
#endif

								return pVAdjust;
							}

	void				FreeImpl(void * pV, const char * pChzFile, int cLine)
							{
								MOE_ASSERT(pV, "NULL pointer in CAlloc::FreeImpl");		
								MOE_ASSERT(m_pStbheap, "Trying to free to a NULL heap");

								pV = ((char *)pV) - CBAllocationPrefix();
#ifdef MOE_USE_SYS_HEAP
								size_t cB = 0;
#else
								size_t cB = stbm_get_allocation_size(pV);
#endif
								
#ifdef MOE_TRACK_ALLOCATION
								HV * pHv = (HV *)pV;
								if (m_pAltrac)
									TrackFree(cB, pHv);
#endif

#ifdef MOE_USE_SYS_HEAP
								free(pV);
#else
								stbm_free(nullptr, m_pStbheap, pV);
#endif
								m_cBFree += cB;
							}

	template <typename T> 
	void				DeleteImpl(T * p, const char * pChzFile, int cLine)
							{
								p->~T();
								FreeImpl(p, pChzFile, cLine);
							}

	void				TrackAlloc(size_t cB, const char * pChzFile, int cLine, BK bk, HV * pHv);
	void				TrackFree(size_t cBt, HV * pHv);
	void				PrintAllocations();

	void				VerifyHeap();

	void				SetAltrac(AllocTracker * pAltrac)
							{ m_pAltrac = pAltrac; }
	size_t				CB()	
							{ return m_cBFree; }

protected:
	stbm_heap *			m_pStbheap;
	AllocTracker * 		m_pAltrac;
	size_t				m_cBFree;
	static bool			s_fProgramIsShutdown;
};

AllocTracker * PAltracCreate(Alloc * pAllocWork);
void DeleteAltrac(Alloc * pAllocWork, AllocTracker * pAltrac);

// GRF definition macro - this used to be a template, but I'm working around clReflects poor template support

#define MOE_DEFINE_GRF(NAME, FENUM, STORAGE)                                                                            \
struct NAME                                                                                                             \
{                                                                                                                       \
	typedef STORAGE StorageType;	                                                                                    \
	NAME():m_raw(FENUM##_None)			{ MOE_CASSERT((STORAGE)FENUM##_All == FENUM##_All, "storage class overflow"); } \
	NAME(FENUM raw):m_raw(raw)			{ ; }                                                                           \
	NAME(u32 raw):m_raw(raw)			{ MOE_ASSERT(m_raw == raw, "GRF overflow"); }                                   \
                                                                                                                        \
	bool		operator==(const NAME & other) const			{ return m_raw == other.m_raw; }                        \
	bool		operator==(FENUM other) const					{ return m_raw == other; }                              \
                                                                                                                        \
	bool		operator!=(const NAME& other) const				{ return !(*this == other); }                           \
	bool		operator!=(STORAGE other) const					{ return !(*this == other); }                           \
	NAME		operator&(const NAME & other) const				{ return this->m_raw & other.m_raw; }                   \
	NAME		operator&(STORAGE other) const					{ return this->m_raw & other; }                         \
	NAME		operator|(const NAME & other) const				{ return this->m_raw | other.m_raw; }                   \
	NAME		operator|(STORAGE other) const					{ return this->m_raw | other; }                         \
                                                                                                                        \
	NAME		operator|=(const NAME & other)					{ m_raw |= other.m_raw; return *this;}                  \
	NAME		operator&=(const NAME & other)					{ m_raw &= other.m_raw; return *this; }                 \
	NAME		operator>>(s32 cBit) const						{ return this->m_raw >> cBit; }                         \
	NAME		operator<<(s32 cBit) const						{ return this->m_raw << cBit; }                         \
                                                                                                                        \
	bool		FIsSet(STORAGE flags) const						{ return (m_raw & flags) == flags; }                    \
	bool		FIsSet(NAME other) const						{ return (m_raw & other.m_raw) == other.m_raw; }        \
	bool		FIsAnySet(STORAGE flags) const					{ return (m_raw & flags) != 0; }                        \
	bool		FIsAnySet(NAME other) const						{ return (m_raw & other.m_raw) != 0; }                  \
	STORAGE		GetSetBits(STORAGE flags) const					{ return (m_raw & flags); }                             \
	void		AddFlags(STORAGE flags)							{ m_raw = STORAGE(m_raw | flags); }                     \
	void		AddFlags(NAME flags)							{ m_raw = STORAGE(m_raw | flags.m_raw); }               \
	void		Clear(STORAGE flags = FENUM##_All)				{ m_raw = m_raw & ~flags; }                             \
	void		Clear(NAME flags)								{ m_raw = m_raw & ~flags.m_raw; }                       \
	void		AssignFlags(STORAGE flags, bool fSet)			{ if (fSet) AddFlags(flags); else Clear(flags); }		\
	void		AssignFlags(NAME flags, bool fSet)				{ if (fSet) AddFlags(flags); else Clear(flags); }		\
	bool		FIsValid(STORAGE flags) const					{ return (flags & FENUM##_All) == flags; }              \
                                                                                                                        \
	STORAGE m_raw;                                                                                                      \
};



struct SSpan // tag = span
{
				SSpan()
				:m_gMin(0.0f)
				,m_gMax(0.0f)
					{
					}

	explicit	SSpan(f32 g)
				:m_gMin(g)
				,m_gMax(g)
					{
					}

				SSpan(f32 gMin, f32 gMax)
				:m_gMin(gMin)
				,m_gMax(gMax)
					{
					}

	void		SetValue(f32 g)
					{
						m_gMin = g;
						m_gMax = g;
					}

	bool		FIsSingleValue() const
					{ return m_gMax == m_gMin; }
	f32			DG() const
					{ return m_gMax - m_gMin; }

	SSpan		SpanExtrude(f32 sExtrude) const
					{
						if (sExtrude < 0.0f)	return SSpan(m_gMin + sExtrude, m_gMin);
						else					return SSpan(m_gMax, m_gMax + sExtrude);
					}
	SSpan		SpanFromPercent(f32 uMin, f32 uMax)
					{
						f32 dG = DG();
						return SSpan(m_gMin + uMin*dG, m_gMin + uMax*dG);
					}

	f32	m_gMin;
	f32	m_gMax;
};



inline void * PVAlign(void * pV, size_t cBAlign)
{
	size_t cBMasked = cBAlign - 1;
	uintptr_t upB = reinterpret_cast<uintptr_t>(pV);
	upB = ( (upB + cBMasked) & ~cBMasked);
	return reinterpret_cast<u8 *>(upB);
}

inline size_t CBAlign(size_t size, size_t cBAlign)
{
	size_t cBMasked = cBAlign - 1;
	size = ( (size + cBMasked) & ~cBMasked);
	return size;
}

inline u32		HvFromAB(const void * aB, size_t cB)
					{
						return HvFromPBFVN(aB, cB);
					}

inline u32		HvFromP(const void * pV)
					{
						return HvFromAB(&pV, sizeof(pV));
					}

template <typename T>
HV HvExtract(const T & t)
{
	return static_cast<HV>(t);
}

template <typename T>
HV HvExtract(const T * pT)
{
	return HvFromP((void *)pT);
}

template <typename T>
HV HvExtract(T * pT)
{
	return HvFromP((void *)pT);
}


// Thomas Wang's 32-bit hash mix function
template<typename T>
struct SHash
{
	HV operator()(const T& t) const
	{
		HV hv = HvExtract(t);
		hv = (hv+0x7ed55d16) + (hv<<12);
		hv = (hv^0xc761c23c) ^ (hv>>19);
		hv = (hv+0x165667b1) + (hv<<5);
		hv = (hv+0xd3a2646c) ^ (hv<<9);
		hv = (hv+0xfd7046c5) + (hv<<3);
		hv = (hv^0xb55a4f09) ^ (hv>>16);
		return hv;
	}
};

inline bool FIsPowerOfTwo(size_t value)
{
	return (value & (value-1)) == 0;
}

// container insertion
enum INRES
{
	INRES_Nil = -1,
	INRES_Error = INRES_Nil,
	INRES_AlreadyExisted,
	INRES_Inserted,
};

} // namespace Moe
