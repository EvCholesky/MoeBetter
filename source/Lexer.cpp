﻿/* Copyright (C) 2015 Evan Christensen
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

#include "Lexer.h"

namespace RWord
{
#define RW(x) const char * g_pChz##x = 
#define STR(x) #x;
	RESERVED_WORD_LIST	
#undef STR
#undef RW

#define RW(x) Moe::InString g_istr##x;
#define STR(x)
	RESERVED_WORD_LIST	
#undef STR
#undef RW
}

static int TokSetTokinf(Lexer * pLex, TOK tok, const char * pChStart, const char * pChEnd)
{
	pLex->m_tok = tok;
	pLex->m_pChBegin = pChStart;
	pLex->m_pChEnd = pChEnd;
	pLex->m_pChParse = pChEnd;

	pLex->m_litk = LITK_Nil;
	pLex->m_numk = NUMK_Nil;
	return 1;
}

/*
RWORD RwordFromHv(u32 hv)
{
	#define RW(x)
	#define STR(x) EWC::HvFromPChz(#x)
	u32 s_aHv[] =
	{
		RESERVED_WORD_LIST
	};
	#undef STR
	#undef RW

	for (int iHv = 0; iHv < EWC_DIM(s_aHv); ++iHv)
	{
		if (s_aHv[iHv] == hv)
		{
			return (RWORD)iHv;
		}
	}
	return RWORD_Nil;
}

const char * PChzFromRword(RWORD rword)
{
	if (!EWC_FVERIFY((rword >= RWORD_Nil) & (rword < RWORD_Max), "unknown reserved word"))
		return "(unknown)";

	if (rword <= RWORD_Nil)
		return "nil";

	#define RW(x) 
	#define STR(x) #x
	const char * s_mpRwordPChz[] =
	{
		RESERVED_WORD_LIST
	};
	#undef STR
	#undef RW

	EWC_CASSERT(EWC_DIM(s_mpRwordPChz) == RWORD_Max, "missing token string");
	return s_mpRwordPChz[rword];
}
*/

bool FIsReservedWord(Moe::InString istr)
{
	#define RW(x) RWord::g_pChz##x,
	#define STR(x)
	static const char * s_apChzRword[] =
	{
		RESERVED_WORD_LIST	
	};
	#undef STR
	#undef RW

	for (int ipChz = 0; ipChz < MOE_DIM(s_apChzRword); ++ipChz)
	{
		if (s_apChzRword[ipChz] == istr.m_pChz)
			return true;
	}
	return false;
}

static int FIsWhitespace(int ch)
{
   return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' || ch == '\f';
}

// copy suffixes at the end of a number into the working string
static int TokParseSuffixes(Lexer * pLex, TOK tok, LITK litk, NUMK numk, const char * pChzStart, const char * pChzCur)
{
	pLex->m_istr = Moe::InString();

	int tokReturn = TokSetTokinf(pLex, tok, pChzStart, pChzCur);
	pLex->m_litk = litk;
	pLex->m_numk = numk;
	return tokReturn;
}

static f64 GParse(const char * pChzIn, char const ** pChzOut)
{
	f64 gReturn = 0.0f;
	while ((*pChzIn >= '0') & (*pChzIn <= '9'))
	{
		gReturn = gReturn*10.0f + (*pChzIn++ - '0');
	}

	if (*pChzIn == '.') 
	{
		f64 gPow10 = 1.0f;
		f64 gAddend = 0;
		++pChzIn;
		while ((*pChzIn >= '0') & (*pChzIn <= '9'))
		{
			gAddend = gAddend * 10 + (*pChzIn++ - '0');
			gPow10 *= 10;
		}
		gReturn += gAddend / gPow10;
	}
	if (*pChzIn == 'e' || *pChzIn == 'E') 
	{
		int nSign = pChzIn[1] == '-';
		int fHasSign = (pChzIn[1] == '-') | (pChzIn[1] == '+');
		int nExponent = 0;
		double gPow10 = 10;
		pChzIn += 1+fHasSign;
		while ((*pChzIn >= '0') & (*pChzIn <= '9'))
		{
			nExponent = nExponent*10 + (*pChzIn++ - '0');
		}

		// can't use pow() from stdlib, so do it slow way
		while (--nExponent > 0)
			gPow10 *= 10;

		if (nSign)
			gReturn /= gPow10;
		else
			gReturn *= gPow10;
	}

	*pChzOut = pChzIn;
	return gReturn;
}

static int TokParseChar(const char * pChzIn, char const ** pChzOut)
{
	if (*pChzIn == '\\') 
	{
		*pChzOut = pChzIn+2; // tentatively parse two characters
		switch(pChzIn[1]) 
		{
			case '\\': return '\\';
			case '\'': return '\'';
			case '"': return '"';
			case 't': return '\t';
			case 'f': return '\f';
			case 'n': return '\n';
			case 'r': return '\r';
			case '0': return '\0';			// @TODO ocatal constants
			case 'x':  // fall through...
			case 'X': return -1;			// @TODO hex constants
			case 'u': return -1;			// @TODO unicode constants
		}
	}
	*pChzOut = pChzIn+1;
	return (unsigned char) *pChzIn;
}

static int TokParseString(Lexer * pLex, const char * pChz)
{
	const char * pChzStart = pChz;
	char chDelim = *pChz++; // grab the " or ' for later matching
	char * pChOut = pLex->m_aChScratch;
	char * pChOutEnd = pLex->m_aChScratch + pLex->m_cChScratch;
	while (*pChz != chDelim) 
	{
		int tok;
		if (*pChz == '\\') 
		{
			const char * pChzNext;
			tok = TokParseChar(pChz, &pChzNext);
			if (tok < 0)
			{
				return TokSetTokinf(pLex, TOK_ParseError, pChzStart, pChzNext);
			}
			pChz = pChzNext;
		} 
		else 
		{
			// @OPTIMIZE: could speed this up by looping-while-not-backslash
			tok = (unsigned char) *pChz++;
		}
		if (pChOut+1 > pChOutEnd)
		{
			return TokSetTokinf(pLex, TOK_ParseError, pChzStart, pChz+1);
		}

	      // @TODO expand unicode escapes to UTF8
		*pChOut++ = (char) tok;

		if (pChz == pLex->m_pChEof)
			break;
	}

	*pChOut = 0;
	pLex->m_istr = IstrInternCopy(pLex->m_aChScratch, pChOut);

	int tok = TokSetTokinf(pLex, TOK_Literal, pChzStart, pChz+1);
	pLex->m_litk = LITK_String;
	return tok;
}

static int TokLexHereString(Lexer * pLex, const char * pChz)
{
	while (pChz != pLex->m_pChEof && FIsWhitespace(*pChz))
	{
		++pChz;
	}

	// make sure this is a valid delimiter
	const char * pChzDelim = pChz;
	const char * pChzDelimEnd = pChz;

	while (!FIsWhitespace(*pChzDelimEnd))
	{
		if (pChzDelimEnd == pLex->m_pChEof)
			return TOK_ParseError;

		++pChzDelimEnd;
	}

	// make sure it's all whitespace from here to the EOL
	pChz = pChzDelimEnd;
	while (*pChz != '\n')
	{
		if (pChz == pLex->m_pChEof || !FIsWhitespace(*pChz))
			return TOK_ParseError;
		++pChz;
	}

	++pChz; // skip the newline
	const char * pChzStart = pChz;

	ptrdiff_t cChDelim = pChzDelimEnd - pChzDelim;
	int cChMatch = 0;
	const char * pChzLine = pChz;
	while (1)
	{
		if (pChz == pLex->m_pChEof)
			return TOK_ParseError;

		if (pChzLine)
		{
			if (pChzLine[cChMatch] == pChzDelim[cChMatch])
			{
				++cChMatch;
				if (cChMatch >= cChDelim)
					break;
			}
			else
			{
				cChMatch = 0;
				pChzLine = nullptr;
			}
		}

		if (*pChz == '\n')
		{
			pChzLine = pChz + 1;
		}
		++pChz;
	}

	// we've found a match!

	pLex->m_istr = IstrInternCopy(pChzStart, pChzLine - pChzStart);

	int tok = TokSetTokinf(pLex, TOK_Literal, pChzStart, pChz+1);
	pLex->m_litk = LITK_String;
	return tok;
}

// method used to split compound tokens (ie '&&' split into two '&' '&' tokens)
void SplitToken(Lexer * pLex, TOK tokSplit)
{
	pLex->m_tok = tokSplit;

	const char * pChzTok = PChzFromTok(tokSplit);
	const char * pChIt = pLex->m_pChBegin;
	const char * pChEnd = pLex->m_pChEnd;
	while (pChIt != pChEnd)
	{
		MOE_ASSERT(*pChIt == *pChzTok, "Split token mismatch");
		if (*pChzTok == '\0')
			break;

		++pChIt;
		++pChzTok;
	}

	pLex->m_pChEnd = pChIt;
	pLex->m_pChParse = pChIt;
}

bool FTryConsumeToken(Lexer * pLex, TOK tok)
{
	if (pLex->m_tok == tok)
	{
		TokNext(pLex);
		return true;
	}
	return false;
}

bool FConsumeIdentifier(Lexer * pLex, Moe::InString istr)
{
	if (pLex->m_tok == TOK_Identifier && pLex->m_istr == istr)
	{
		TokNext(pLex);
		return true;
	}
	return false;
}

int TokNext(Lexer * pLex)
{
	const char * pChz = pLex->m_pChParse;

	bool fContainsNewline = false;
	// skip whitespace and comments
	for (;;) 
	{
		while (pChz != pLex->m_pChEof && FIsWhitespace(*pChz))
		{
			fContainsNewline |= (*pChz == '\n');
			++pChz;
		}

		// C++ comments, aka. double slash comments
		if (pChz != pLex->m_pChEof && ((pChz[0] == '/') & (pChz[1] == '/')))
		{
			while (pChz != pLex->m_pChEof && ((*pChz != '\r') & (*pChz != '\n')))
			{
				fContainsNewline |= (*pChz == '\n');
				++pChz;
			}
		    continue;
		}

		// C comments /* like this */
		if (pChz != pLex->m_pChEof && ((pChz[0] == '/') & (pChz[1] == '*')))
		{
			const char * pChStart = pChz;
		    pChz += 2;
		    while (pChz != pLex->m_pChEof && ((pChz[0] != '*') | (pChz[1] != '/')))
			{
				fContainsNewline |= (*pChz == '\n');
				++pChz;
			}
		    if (pChz == pLex->m_pChEof)
		       return TokSetTokinf(pLex, TOK_ParseError, pChStart, pChz);
		    pChz += 2;
		    continue;
		}
		break;
	}
	pLex->m_grflexer.AssignFlags(FLEXER_EndOfLine, fContainsNewline);

	if (pChz == pLex->m_pChEof)
	{
	   pLex->m_tok = TOK_Eof;
	   return 0;
	}

	switch (*pChz) 
	{
		default:
			if (  ((*pChz >= 'a') & (*pChz <= 'z'))
				| ((*pChz >= 'A') & (*pChz <= 'Z'))
				| (*pChz == '_') 
				| (*pChz == '#')		// allowing identifiers containing '#' to catch directive rwords, parse will error.
				| (u8(*pChz) >= 128))   // >= 128 is UTF8 char
			{
				size_t iCh = 0;
				char * pChzScratch = pLex->m_aChScratch;
				do 
				{
					if (iCh+1 >= pLex->m_cChScratch)
						return TokSetTokinf(pLex, TOK_ParseError, pChz, pChz+iCh);

					pChzScratch[iCh] = pChz[iCh];
					++iCh;
				} while ( ((pChz[iCh] >= 'a') & (pChz[iCh] <= 'z'))
						| ((pChz[iCh] >= 'A') & (pChz[iCh] <= 'Z'))
						| ((pChz[iCh] >= '0') & (pChz[iCh] <= '9')) // allow digits in middle of identifier
						| (pChz[iCh] == '_')
						| (u8(pChz[iCh]) >= 128));
				pChzScratch[iCh] = '\0';
				pLex->m_istr = IstrInternCopy(pChzScratch);

#if 0 // not handling reserved word as a different token, just an identifier 
				bool fIsReservedWord = FIsReservedWord(pLex->m_istr);
				if (fIsReservedWord)
				{
					if (pLex->m_istr == RWord::g_pChzStringDirective)
					{
						return TokLexHereString(pLex, pChz+iCh);
					}
					return TokSetTokinf(pLex, TOK_ReservedWord, pChz, pChz+iCh-1);
				}
#endif

				return TokSetTokinf(pLex, TOK_Identifier, pChz, pChz+iCh);
			}

		single_char:         
			// not an identifier, return the character as itself
			return TokSetTokinf(pLex, TOK(*pChz), pChz, pChz + 1);

		case '+':
			if (pChz+1 != pLex->m_pChEof) 
			{
				if (pChz[1] == '+') 
					return TokSetTokinf(pLex, TOK_PlusPlus, pChz, pChz+2);
				if (pChz[1] == '=') 
					return TokSetTokinf(pLex, TOK_PlusEqual, pChz, pChz+2);
			} goto single_char;

		case '-':
			if (pChz+1 != pLex->m_pChEof) 
			{
				if (pChz[1] == '-') 
				{
					if ((pChz+2 != pLex->m_pChEof) && pChz[2] == '-')
						return TokSetTokinf(pLex, TOK_TripleMinus, pChz, pChz+3);
					return TokSetTokinf(pLex, TOK_MinusMinus, pChz, pChz+2);
				}
				if (pChz[1] == '=') 
					return TokSetTokinf(pLex, TOK_MinusEqual, pChz, pChz+2);
				if (pChz[1] == '>') 
					return TokSetTokinf(pLex, TOK_Arrow, pChz, pChz+2);
			} goto single_char;

		case '&':
			 if (pChz+1 != pLex->m_pChEof) 
			 {
				if (pChz[1] == '&') 
					return TokSetTokinf(pLex, TOK_AndAnd, pChz, pChz+2);
				if (pChz[1] == '=') 
					return TokSetTokinf(pLex, TOK_AndEqual, pChz, pChz+2);
			 } goto single_char;

		case '|':
			if (pChz+1 != pLex->m_pChEof) 
			{
				if (pChz[1] == '|') 
					return TokSetTokinf(pLex, TOK_OrOr, pChz, pChz+2);
				if (pChz[1] == '=') 
					return TokSetTokinf(pLex, TOK_OrEqual, pChz, pChz+2);
			} goto single_char;

		case '=':
		    if (pChz+1 != pLex->m_pChEof && pChz[1] == '=') 
				return TokSetTokinf(pLex, TOK_EqualEqual, pChz, pChz+2);
			goto single_char;

		case '!':
			if (pChz+1 != pLex->m_pChEof && pChz[1] == '=') 
				return TokSetTokinf(pLex, TOK_NotEqual, pChz, pChz+2);
			goto single_char;

		case '^':
			if (pChz+1 != pLex->m_pChEof && pChz[1] == '=') 
				return TokSetTokinf(pLex, TOK_XorEqual, pChz,pChz+2);
			goto single_char;

		case ':':
			if (pChz+1 != pLex->m_pChEof)
				{
					if (pChz[1] == ':') 
						return TokSetTokinf(pLex, TOK_ColonColon, pChz,pChz+2);
					if (pChz[1] == '=') 
						return TokSetTokinf(pLex, TOK_ColonEqual, pChz,pChz+2);
				}
			goto single_char;

		case '.':
			if (pChz+1 != pLex->m_pChEof && pChz[1] == '.') 
				return TokSetTokinf(pLex, TOK_PeriodPeriod, pChz,pChz+2);
			goto single_char;

		case '~':
			if (pChz+1 != pLex->m_pChEof && pChz[1] == '=') 
				return TokSetTokinf(pLex, TOK_TildeEqual, pChz,pChz+2);
			goto single_char;
			
		case '%':
			if (pChz+1 != pLex->m_pChEof && pChz[1] == '=')
				return TokSetTokinf(pLex, TOK_ModEqual, pChz, pChz+2);
			goto single_char;

		case '*':
			if (pChz+1 != pLex->m_pChEof && pChz[1] == '=')
				return TokSetTokinf(pLex, TOK_MulEqual, pChz, pChz+2);
			goto single_char;

		case '/':
			if (pChz+1 != pLex->m_pChEof && pChz[1] == '=')
				return TokSetTokinf(pLex, TOK_DivEqual, pChz, pChz+2);
			goto single_char;
			
		case '<':
			if (pChz+1 != pLex->m_pChEof) 
			{
			    if (pChz[1] == '=') 
					return TokSetTokinf(pLex, TOK_LessEqual, pChz, pChz+2);
			    if (pChz[1] == '<') 
                   return TokSetTokinf(pLex, TOK_ShiftLeft, pChz, pChz+2);
			} goto single_char;

		case '>':
			if (pChz+1 != pLex->m_pChEof) 
			{
			    if (pChz[1] == '=') 
					return TokSetTokinf(pLex, TOK_GreaterEqual, pChz, pChz+2);
			    if (pChz[1] == '>') 
					return TokSetTokinf(pLex, TOK_ShiftRight, pChz, pChz+2);
			 }
			 goto single_char;

		case '"':
			return TokParseString(pLex, pChz);

		case '\'':
		//STB_C_LEX_C_SQ_STRINGS(return stb__clex_parse_string(pLex, pChz, CLEX_sqstring);)
		// single quote chars
		{
		    const char * pChzStart = pChz;
		    int tok = TokParseChar(pChz+1, &pChz);
		    pLex->m_n = tok;

		    if (tok < 0)
		       return TokSetTokinf(pLex, TOK_ParseError, pChzStart,pChzStart+1);
		    if (pChz == pLex->m_pChEof || *pChz != '\'')
		       return TokSetTokinf(pLex, TOK_ParseError, pChzStart, pChz+1);

			tok = TokSetTokinf(pLex, TOK_Literal, pChzStart, pChz+1);
			pLex->m_litk = LITK_Char;
			return tok;
		}
		goto single_char;
		
		case '0':
			// hex ints
		    if (pChz+1 != pLex->m_pChEof) 
			{
				if (pChz[1] == 'x' || pChz[1] == 'X') 
				{
					const char * pChzNext = pChz+2;
					#ifdef STB__CLEX_use_stdlib
					pLex->m_n = strtol((char *) pChz, (char **) pChzNext, 16);
					#else
					u64 n = 0;
					while (pChzNext != pLex->m_pChEof) 
					{
						if		((*pChzNext >= '0') & (*pChzNext <= '9'))	n = n*16 + (*pChzNext - '0');
						else if ((*pChzNext >= 'a') & (*pChzNext <= 'f'))	n = n*16 + (*pChzNext - 'a') + 10;
						else if ((*pChzNext >= 'A') & (*pChzNext <= 'F'))	n = n*16 + (*pChzNext - 'A') + 10;
						else
						    break;
						++pChzNext;
					}
					pLex->m_n = n;
					#endif
					if (pChzNext == pChz+2)
						return TokSetTokinf(pLex, TOK_ParseError, pChz-2, pChz);
					return TokParseSuffixes(pLex, TOK_Literal, LITK_Numeric, NUMK_UnsignedInt, pChz, pChzNext);
				}
			}

		// fall through
		case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':

		{	// floats
		    const char * pChzNext = pChz;
			while (pChzNext != pLex->m_pChEof && ((*pChzNext >= '0') & (*pChzNext <= '9')))
		       ++pChzNext;
		    if (pChzNext != pLex->m_pChEof) 
			{
				if ((*pChzNext == '.') | (*pChzNext == 'e') | (*pChzNext == 'E'))
				{
					#ifdef STB__CLEX_use_stdlib
					pLex->m_g = strtod((char *) pChz, (char**) &pChzNext);
					#else
					pLex->m_g = GParse(pChz, &pChzNext);
					#endif

					int tok = TokParseSuffixes(pLex, TOK_Literal, LITK_Numeric, NUMK_Float, pChz, pChzNext);
					return tok;
				}
			}
		}

		{	// decimal ints
		    const char * pChzNext = pChz;
		    #ifdef STB__CLEX_use_stdlib
		    pLex->m_n = strtol((char *) pChz, (char **) &pChzNext, 10);
		    #else
		    u64 n = 0;
			while (pChzNext != pLex->m_pChEof) 
			{
				if ((*pChzNext >= '0') & (*pChzNext <= '9'))
					n = n*10 + (*pChzNext - '0');
				else
					break;
				++pChzNext;
		    }
		    pLex->m_n = n;
		    #endif

		    return TokParseSuffixes(pLex, TOK_Literal, LITK_Numeric, NUMK_UnsignedInt, pChz, pChzNext);
		 }
		 goto single_char;
	}
}

void InitLexer(Lexer * pLex, const char * pCoInput, const char * pCoInputEnd, char * aChStorage, u32 cChStorage)
{
	pLex->m_pChInput = pCoInput;
	pLex->m_pChParse = pCoInput;
	pLex->m_pChEof =  pCoInputEnd;
	pLex->m_aChScratch = aChStorage;
	pLex->m_cChScratch = cChStorage;

	pLex->m_istrFilename = Moe::InString();
	pLex->m_pChBegin = nullptr;
	pLex->m_pChEnd = nullptr;
	pLex->m_n = 0;
	pLex->m_g = 0;
	pLex->m_litk = LITK_Nil;
	pLex->m_numk = NUMK_Nil;
}

/*
RWORD RwordLookup(Lexer * pLex)
{
	if (pLex->m_tok != TOK_ReservedWord)
		return RWORD_Nil;
	return pLex->m_rword;
}*/

const char * PChzFromTok(TOK tok)
{
	if (!MOE_FVERIFY((tok >= TOK_Nil) & (tok < TOK_Max), "bad token value"))
		return "(err)";

	if (tok <= TOK_Nil)
		return "(nil)";
	if (tok < TOK_SimpleMax)
	{
		static char s_aB[TOK_SimpleMax * 2];
		s_aB[tok] = (char)tok;
		s_aB[tok + 1] = '\0';
		return &s_aB[tok];
	}

	static const char * s_mpTokPChz[] = 
	{
		"(Eof)",
		"(ParseError)",
		"(Literal)",
		"(Identifier)",
		//"(ReservedWord)",
		"==",
		"!=",
		"<=",
		">=",
		"&&",
		"||",
		"&=",
		"|=",
		"^=",
		"~=",
		"<<",
		">>",
		"++",
		"--",
		"---",
		"+=",
		"-=",
		"*=",
		"/=",
		"%=",
		"->",
		"::",
		":=",
		"..",
	};
	MOE_CASSERT(MOE_DIM(s_mpTokPChz) == TOK_Max - TOK_SimpleMax, "missing token string");
	return s_mpTokPChz[tok - TOK_SimpleMax];
}

const char * PChzCurrentToken(Lexer * pLex)
{
	TOK tok = (TOK)pLex->m_tok;
	if (tok == TOK_Identifier)
		return pLex->m_istr.m_pChz;

	return PChzFromTok(tok);
}


Moe::InString StrUnexpectedToken(Lexer * pLex)
{
	if (pLex->m_tok == TOK_Identifier)
	{
		return pLex->m_istr;
	}
	return IstrInternCopy(PChzCurrentToken(pLex));
}

void SkipToToken(Lexer * pLex, TOK const * const aTok, int cTok, GRFLEXER grflexer)
{
	while (1)
	{
		bool fFound = (grflexer != FLEXER_None) && pLex->m_grflexer.FIsSet(grflexer);
		TOK tok = (TOK)pLex->m_tok;
		if (tok == TOK_Eof)
			break;

		for (int iTok = 0; !fFound && iTok < cTok; ++iTok)
		{
			fFound |= (tok == aTok[iTok]);
		}

		if (fFound)
			break;
		TokNext(pLex);
	}
}

struct LexRecover // tag = lrec
{
	Moe::CFixAry<TOK, 6>	m_aryTok;	
//	GRFLEXER				m_grflexer;
};

struct LexRecoverStack // tag = lrecst
{
								LexRecoverStack(Moe::Alloc * pAlloc)
								:m_aryLrec(pAlloc, Moe::BK_LexRecover)
									{ ; }

	Moe::CDynAry<LexRecover>	m_aryLrec;
};

LexRecoverStack * PLrecstAlloc(Moe::Alloc * pAlloc)
{
	auto pLrecst = MOE_NEW(pAlloc, LexRecoverStack) LexRecoverStack(pAlloc);
	return pLrecst;
}

void FreeLexRecoverStack(Moe::Alloc * pAlloc, LexRecoverStack * pLrecst)
{
	pAlloc->MOE_DELETE(pLrecst);
}

LexRecover * PLrecPush(LexRecoverStack * pLrecst, TOK tok)
{
	auto pLrec = pLrecst->m_aryLrec.AppendNew();
	pLrec->m_aryTok.Append(tok);
	return pLrec;
}

LexRecover * PLrecPush(LexRecoverStack * pLrecst, TOK tokA, TOK tokB)
{
	auto pLrec = pLrecst->m_aryLrec.AppendNew();
	pLrec->m_aryTok.Append(tokA);
	pLrec->m_aryTok.Append(tokB);
	return pLrec;
}

LexRecover * PLrecPush(LexRecoverStack * pLrecst, const TOK * aTok, int cTok)
{
	auto pLrec = pLrecst->m_aryLrec.AppendNew();
	pLrec->m_aryTok.Append(aTok, cTok);
	return pLrec;
}

void PopLexRecover(LexRecoverStack * pLrecst, LexRecover * pLrec)
{
	MOE_ASSERT(pLrecst->m_aryLrec.PLast() == pLrec, "push/pop mismatch in the lex recovery stack");
	pLrecst->m_aryLrec.PopLast();
}

TOK TokSkipToRecovery(Lexer * pLex, LexRecoverStack * pLrecst)
{
	auto pLrec = pLrecst->m_aryLrec.PLast();

	TOK * pTokMin = pLrec->m_aryTok.A();
	TOK * pTokMax = pLrec->m_aryTok.PMac();

	while (1)
	{
		bool fEndOfLine (pLex->m_grflexer.FIsSet(FLEXER_EndOfLine));

		TOK tok = (TOK)pLex->m_tok;
		if (tok == TOK_Eof)
			return tok;
		if (tok == TOK_EndOfLine && fEndOfLine)
			return tok;

		for (TOK * pTok = pTokMin; pTok != pTokMax; ++pTok)
		{
			if (tok == *pTok)
				return tok;
		}

		TokNext(pLex);
	}
}

#define LEXER_TEST
#ifdef LEXER_TEST
void AssertMatches(
	const char * pChzInput, 
	const TOK * aTok, 
	const int * aN = nullptr, 
	const f64 * aG = nullptr,
	const char * apChz[] = nullptr,
	const LITK * aLitk = nullptr)
{
	Lexer lex;
	char aChStorage[1024 * 8];
	InitLexer(&lex, pChzInput, &pChzInput[Moe::CBChz(pChzInput)-1], aChStorage, MOE_DIM(aChStorage));
	
	int iTok = 0;
	const TOK * pTok = aTok;
	while (TokNext(&lex)) 
	{
		if (pTok && *pTok == TOK_Nil)
			pTok = nullptr;
		MOE_ASSERT(!pTok || lex.m_tok == *pTok, "lexed token doesn't match expected");
		++pTok;

		if (apChz && apChz[iTok] == nullptr)
			apChz = nullptr;

		bool fIsStringLiteral = lex.m_tok == TOK_Literal && lex.m_litk == LITK_String;
		if (apChz && (lex.m_tok == TOK_Identifier || fIsStringLiteral))
		{
			MOE_ASSERT(
				Moe::CCodepoint(apChz[iTok]) == lex.m_istr.CCodepoint(), 
				"lexed string length doesn't match expected value");
			MOE_ASSERT(
				Moe::FAreChzEqual(apChz[iTok], lex.m_istr.m_pChz, lex.m_istr.CCodepoint()), 
				"lexed string doesn't match expected value");
		}

		bool fIsIntLiteral = lex.m_tok == TOK_Literal && lex.m_litk == LITK_Numeric && lex.m_numk != NUMK_Float;
		if (aN && fIsIntLiteral)
		{
			MOE_ASSERT(lex.m_n == aN[iTok], "integer literal value doesn't match expected"); 
		}

		bool fIsFloatLiteral = lex.m_tok == TOK_Literal && lex.m_litk == LITK_Numeric && lex.m_numk == NUMK_Float;
		if (aG && fIsFloatLiteral)
		{
			MOE_ASSERT(lex.m_g == aG[iTok], "float literal value doesn't match expected"); 
		}

		if (aLitk)
		{
			MOE_ASSERT(aLitk[iTok] == lex.m_litk, "Literal type mismatch");
		}

		++iTok;
	}
}

bool FTestLexing()
{

	const char * s_pChz = u8"😁+✂";
	const TOK s_aTokEmoji[] = {	
										TOK_Identifier, TOK('+'),
										TOK_Identifier, TOK(';'),
										TOK_Nil};

	AssertMatches(s_pChz, s_aTokEmoji);

	const char * s_pChzLitString = " 'f'; \"foo\"; ";
	const TOK s_aTokLitString[] = {	
										TOK_Literal, TOK(';'),
										TOK_Literal, TOK(';'),
										TOK_Nil};
	AssertMatches(s_pChzLitString, s_aTokLitString);

	const char * s_pChzComparisons = "< > <= >= != ==";
	const TOK s_aTokCompfarisons[] = {	TOK('<'), TOK('>'), 
										TOK_LessEqual, TOK_GreaterEqual, 
										TOK_NotEqual, TOK_EqualEqual, 
										TOK_Nil};
	AssertMatches(s_pChzComparisons, s_aTokCompfarisons);

	const char * s_pChzBitwise = "& && &= | || |= ^ ^= ~ ~=";
	const TOK s_aTokBitwise[] = {	TOK('&'), TOK_AndAnd, TOK_AndEqual, 
									TOK('|'), TOK_OrOr, TOK_OrEqual, 
									TOK('^'), TOK_XorEqual, 
									TOK('~'), TOK_TildeEqual, 
									TOK_Nil};
	AssertMatches(s_pChzBitwise, s_aTokBitwise);

	const char * s_pChzOperator = "+ += - -= * *= / /= % %= << >>";
	const TOK s_aTofkOperator[] = {	TOK('+'), TOK_PlusEqual, TOK('-'), TOK_MinusEqual, 
										TOK('*'), TOK_MulEqual, TOK('/'), TOK_DivEqual,
										TOK('%'), TOK_ModEqual,
										TOK_ShiftLeft, TOK_ShiftRight,
										TOK_Nil };
	AssertMatches(s_pChzOperator, s_aTofkOperator);

	const char * s_pChzMisc = "foo:=bar->guh.wut;";
	const TOK s_aTokMisc[] = {TOK_Identifier, TOK_ColonEqual, 
								TOK_Identifier, TOK_Arrow,
								TOK_Identifier, TOK('.'),
								TOK_Identifier, TOK(';'),
								TOK_Nil };
	const char * s_apChzMiscStrings[] = {"foo", "", "bar", "", "guh", "", "wut", "", nullptr};
	AssertMatches(s_pChzMisc, s_aTokMisc, nullptr, nullptr, s_apChzMiscStrings);

	const char * s_pChzNum = "2.456,5,7.3,-1,0xFFFF, 12.34e12 12.34e-12";
	const TOK s_aTokNum[] = {TOK_Literal, TOK(','), 
								TOK_Literal, TOK(','), 
								TOK_Literal, TOK(','), 
								TOK('-'), TOK_Literal, TOK(','),
								TOK_Literal, TOK(','),
								TOK_Literal, TOK_Literal,
								TOK_Nil };
	const LITK s_aLitk[] = {	LITK_Numeric, LITK_Nil,
								LITK_Numeric, LITK_Nil,
										LITK_Numeric, LITK_Nil,
										LITK_Nil, LITK_Numeric, LITK_Nil,
										LITK_Numeric, LITK_Nil,
										LITK_Numeric,
										LITK_Numeric
									};
										
	const int s_aNNum[] = {0,	  0, 5, 0, 0,   0, 0, 1, 0, 65535, 0, 0,        0};	// negative integer literal comes through as two tokens
	const f64 s_aGNum[] = {2.456, 0, 0, 0, 7.3, 0, 0, 1, 0, 0,     0, 12.34e12, 12.34e-12};
	static_assert(MOE_DIM(s_aTokNum)-1 == MOE_DIM(s_aNNum), "s_aNNum size mismatch");
	static_assert(MOE_DIM(s_aTokNum)-1 == MOE_DIM(s_aGNum), "s_aGNum size mismatch");

	AssertMatches(s_pChzNum, s_aTokNum, s_aNNum, s_aGNum, nullptr, s_aLitk);

	const char * s_pChzRword = "for:=new.if\nelse while";
	const TOK s_aTokRword[] = {TOK_Identifier, TOK_ColonEqual, 
								TOK_Identifier, TOK('.'),
								TOK_Identifier, TOK_Identifier,
								TOK_Identifier, 
								TOK_Nil };
	const char * s_apChzRword[] = {	RWord::g_pChzFor, "",
									RWord::g_pChzNew, "",	
									RWord::g_pChzIf, RWord::g_pChzElse,
									RWord::g_pChzWhile};

	AssertMatches(s_pChzRword, s_aTokRword, nullptr, nullptr, s_apChzRword);

	const char * s_pChzTriple = ". .. ... - -- --- : ::";
	const TOK s_aTokTriple[] = {TOK('.'), TOK_PeriodPeriod, 
								TOK_PeriodPeriod, TOK('.'),
								TOK('-'), TOK_MinusMinus,
								TOK_TripleMinus, 
								TOK(':'), TOK_ColonColon,
								TOK_Nil };

	AssertMatches(s_pChzTriple, s_aTokTriple);

	return true;
}
#else // LEXER_TEST
void TestLexing() { ; }
#endif // LEXER_TEST