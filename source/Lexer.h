/* Copyright (C) 2015 Evan Christensen
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

#include "MoeTypes.h"
#include "MoeString.h"
#include "TypeInfo.h"

struct Lexer;
struct LexRecover;
struct LexRecoverStack;

enum TOK
{
	// don't define single char tokens (they are just ascii codepoints)
	TOK_Eof = 256,
	TOK_ParseError,
	TOK_Literal,
	TOK_Identifier,
//	TOK_ReservedWord,
	TOK_EqualEqual,
	TOK_NotEqual,
	TOK_LessEqual,
	TOK_GreaterEqual,
	TOK_AndAnd,
	TOK_OrOr,
	TOK_AndEqual,
	TOK_OrEqual,
	TOK_XorEqual,
	TOK_TildeEqual,
	TOK_ShiftLeft,
	TOK_ShiftRight,
	TOK_PlusPlus,
	TOK_MinusMinus,
	TOK_TripleMinus,
	TOK_PlusEqual,
	TOK_MinusEqual,
	TOK_MulEqual,
	TOK_DivEqual,
	TOK_ModEqual,
	TOK_Arrow,
	TOK_ColonColon,
	TOK_ColonEqual,
	TOK_PeriodPeriod,

	TOK_Max,
	TOK_Min = 0,
	TOK_Nil = -1,

	// token alias (for easy rebinding)
	TOK_Reference = '&',
	TOK_DoubleReference = TOK_AndAnd,
	TOK_Dereference = '@',
	TOK_Label = '`',
	TOK_Generic = '$',

	TOK_EndOfLine,		// never reported by the lexer, just used for error recovery

	TOK_SimpleMax = TOK_Eof,
};


#define RESERVED_WORD_LIST \
		RW(Null) STR(null) \
		RW(Proc) STR(proc) \
		RW(Struct) STR(struct) \
		RW(Enum) STR(enum) \
		RW(FlagEnum) STR(flag_enum) \
		RW(If) STR(if)	\
		RW(Else) STR(else) \
		RW(For) STR(for) \
		RW(While) STR(while) \
		RW(Switch) STR(switch) \
		RW(Case) STR(case) \
		RW(Fallthrough) STR(fallthrough) \
		RW(Break) STR(break) \
		RW(Continue) STR(continue) \
		RW(Return) STR(return) \
		RW(ImportDirective) STR(#import) \
		RW(Const) STR(const) \
		RW(Immutable) STR(immutable) \
		RW(Cast) STR(cast) \
		RW(Sizeof) STR(sizeof) \
		RW(Alignof) STR(alignof) \
		RW(Typeof) STR(typeof) \
		RW(Typeinfo) STR(typeinfo) \
		RW(InArg) STR(inarg) \
		RW(Typedef) STR(typedef) \
		RW(Inline) STR(inline) \
		RW(NoInline) STR(no_inline) \
		RW(Defer) STR(defer) \
		RW(True) STR(true) \
		RW(False) STR(false) \
		RW(New) STR(new) \
		RW(Main) STR(main) \
		RW(Delete) STR(delete) \
		RW(Using) STR(using) \
		RW(Operator) STR(operator) \
		RW(Commutative) STR(#commutative) \
		RW(ForeignDirective) STR(#foreign) \
		RW(FileDirective) STR(#file) \
		RW(LineDirective) STR(#line) \
		RW(StringDirective) STR(#string) \
		RW(ForeignLibraryDirective) STR(#foreign_library) \
		RW(StaticLibraryDirective) STR(#static_library) \
		RW(DynamicLibraryDirective) STR(#dynamic_library) \
		RW(CDecl) STR(#cdecl) \
		RW(StdCall) STR(#stdcall)

#define RW(x) \
	extern const char * g_pChz##x; \
	extern Moe::InString g_istr##x; 
#define STR(x)
namespace RWord
{
	RESERVED_WORD_LIST	
}
#undef STR
#undef RW

inline void InternReservedWordStrings()
{
#define RW(x) RWord::g_istr##x = IstrIntern(RWord::g_pChz##x);
#define STR(x)
	RESERVED_WORD_LIST
#undef STR
#undef RW
}

inline void ClearReservedWordStrings()
{
#define RW(x) RWord::g_istr##x = Moe::InString();
#define STR(x)
	RESERVED_WORD_LIST
#undef STR
#undef RW
}



enum FLEXER
{
	FLEXER_EndOfLine	= 0x1,	// lexing to this token passed a newline - really StartOfLine

	FLEXER_None			= 0x0,
	FLEXER_All			= 0x1,
};
MOE_DEFINE_GRF(GRFLEXER, FLEXER, u8);



LexRecoverStack * PLrecstAlloc(Moe::Alloc * pAlloc);
void FreeLexRecoverStack(Moe::Alloc * pAlloc, LexRecoverStack * pLrecst);
LexRecover * PLrecPush(LexRecoverStack * pLrecst, TOK tok);
LexRecover * PLrecPush(LexRecoverStack * pLrecst, TOK tokA, TOK tokB);
LexRecover * PLrecPush(LexRecoverStack * pLrecst, const TOK * aTok, int cTok);

template <typename T, size_t CELEM>
LexRecover * PLrecPush(LexRecoverStack * pLrecst, const T (&aTok) [CELEM])
{
	return PLrecPush(pLrecst, aTok, CELEM);
}

void PopLexRecover(LexRecoverStack * pLrecst, LexRecover * pLrec);
TOK TokSkipToRecovery(Lexer * pLex, LexRecoverStack * lrecst);

struct LexRecoverAmbit  // tag = lrecamb
{
				LexRecoverAmbit(LexRecoverStack * pLrecst, TOK tok)
					{
						m_pLrecst = pLrecst;
						m_pLrec = PLrecPush(m_pLrecst, tok);
					}
				LexRecoverAmbit(LexRecoverStack * pLrecst, TOK tokA, TOK tokB)
					{
						m_pLrecst = pLrecst;
						m_pLrec = PLrecPush(m_pLrecst, tokA, tokB);
					}
				template<int CELEM>
				LexRecoverAmbit(LexRecoverStack * pLrecst, const TOK (&aTok) [CELEM])
					{
						m_pLrecst = pLrecst;
						m_pLrec = PLrecPush(m_pLrecst, aTok, CELEM);
					}
				LexRecoverAmbit(LexRecoverStack * pLrecst, const TOK * aTok, int cTok)
					{
						m_pLrecst = pLrecst;
						m_pLrec = PLrecPush(m_pLrecst, aTok, cTok);
					}
				~LexRecoverAmbit()
					{ PopLexRecover(m_pLrecst, m_pLrec); }

	LexRecoverStack *	m_pLrecst;
	LexRecover *		m_pLrec;
};



struct Lexer // tag = lex
{
	// lexer variables
	const char *		m_pChInput;
	const char *		m_pChEof;
	const char *		m_pChParse;		// current parse location - points just after the current token
	char *				m_aChScratch;	// working character space - NOTE:
	u32					m_cChScratch;

	// lexer parse location for error messages
	Moe::InString		m_istrFilename;
	const char *		m_pChBegin;
	const char *		m_pChEnd;

	// current token info
	u32					m_tok;		// lexer->token is the token ID, which is unicode code point for a single-char token, 
									// < 0 for an error, > 256 for multichar or eof
	GRFLEXER			m_grflexer;
	//RWORD				m_rword;
	f64					m_g;
	u64					m_n;
	LITK				m_litk;
	NUMK				m_numk;
	Moe::InString		m_istr;
};


struct LexSpan // tag = lexsp
{
					LexSpan()
					:m_istrFilename()
					,m_iB(-1)
					,m_cB(0)
						{ ; }

					LexSpan(const Moe::InString & istrFilename, s32 dB = -1)
					:m_istrFilename(istrFilename)
					,m_iB(dB)
					,m_cB(0)
					{ ; }

	explicit		LexSpan(Lexer * pLex)
					:m_istrFilename(pLex->m_istrFilename)
					,m_iB((s32)(pLex->m_pChBegin - pLex->m_pChInput))
					,m_cB((uintptr_t)pLex->m_pChEnd - (uintptr_t)pLex->m_pChBegin)
						{ ; }

	bool			operator<(const LexSpan & lexspRhs) const
						{
							if (m_istrFilename == lexspRhs.m_istrFilename)
								return m_iB < lexspRhs.m_iB;

							return uintptr_t(m_istrFilename.m_pChz) < uintptr_t(lexspRhs.m_istrFilename.m_pChz);
						}

	bool			operator<=(const LexSpan & lexspRhs) const
						{
							if (m_istrFilename == lexspRhs.m_istrFilename)
								return m_iB <= lexspRhs.m_iB;

							return uintptr_t(m_istrFilename.m_pChz) <= uintptr_t(lexspRhs.m_istrFilename.m_pChz);
						}
		
	bool			FIsValid() const
						{ return m_iB >= 0;}

	Moe::InString	m_istrFilename;
	s64				m_iB;
	s64				m_cB;
};

void InitLexer(Lexer * pLex, const char * pChInput, const char * pChInputEnd, char * aChStorage, u32 cChStorage);
bool FTryConsumeToken(Lexer * pLex, TOK tok);
bool FConsumeIdentifier(Lexer * pLex, Moe::InString istr);
int TokNext(Lexer * pLex);

void SkipToToken(Lexer * pLex, TOK const * const aTok, int cTok, GRFLEXER grflexer);
inline void SkipRestOfLine(Lexer * pLex)
	{ SkipToToken(pLex, nullptr, 0, FLEXER_EndOfLine); }

void SplitToken(Lexer * pLex, TOK tokSplit);

//RWORD RwordLookup(Lexer * pLex);
bool FIsReservedWord(Moe::InString istr);

const char * PChzFromTok(TOK tok);
//const char * PCCozFromRword(RWORD rword);
const char * PChzCurrentToken(Lexer * pLex);
Moe::InString StrUnexpectedToken(Lexer * pLex);

