/* Copyright (C) 2017 Evan Christensen
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


#include "UnitTest.h"

#include "Error.h"
#include "MoeArray.h"
#include "MoeString.h"
#include "MoeTypes.h"
#include "Lexer.h"
#include "Parser.h"
#include "Request.h"
#include "Workspace.h"

#include <cstdarg>
#include <stdio.h>

using namespace Moe;

#define UNIT_TEST_KEYWORD_LIST \
		UTK(Errid) STR(errid) \
		UTK(Test) STR(test)	\
		UTK(Builtin) STR(builtin) \
		UTK(Prereq) STR(prereq)	\
		UTK(Input) STR(input) \
		UTK(Parse) STR(parse) \
		UTK(TypeCheck) STR(typecheck) \
		UTK(Values) STR(values) \
		UTK(ByteCode) STR(bytecode) \
		UTK(TestFlags) STR(testflags) \
		UTK(Global) STR(global) \
		UTK(Local) STR(local) \
		UTK(Lexer) STR(lexer) \
		UTK(Signed65) STR(signed65) \
		UTK(Unicode) STR(unicode) \
		UTK(UniqueNames) STR(uniqueNames) \
		UTK(BlockList) STR(blockList) 

namespace UTest
{
#define UTK(x) const char * g_pChz##x = 
#define STR(x) #x;
	UNIT_TEST_KEYWORD_LIST	
#undef STR
#undef UTK

#define UTK(x) Moe::InString g_istr##x;
#define STR(x)
	UNIT_TEST_KEYWORD_LIST	
#undef STR
#undef UTK
}


void InternUnitTestStrings()
{
#define UTK(x) UTest::g_istr##x = IstrIntern(UTest::g_pChz##x);
#define STR(x)
	UNIT_TEST_KEYWORD_LIST	
#undef STR
#undef UTK
}

void ClearUnitTestStrings()
{
#define UTK(x) UTest::g_istr##x = Moe::InString();
#define STR(x)
	UNIT_TEST_KEYWORD_LIST	
#undef STR
#undef UTK
}



extern bool FTestLexing();
extern bool FTestUnicode();

static const int s_cErridOptionMax = 4; //max error ids per option

struct Permutation;
struct Option // tag = opt
{
							Option(Alloc * pAlloc)
							:m_pChzOption(nullptr)
							,m_aryErridExpected()
							,m_fAllowSubstitution(false)
								{ ; }

							void FreeAll(Alloc * pAlloc)
								{ ; }

	const char *							m_pChzOption;
	Moe::CFixAry<ERRID, s_cErridOptionMax> 	m_aryErridExpected;
	bool									m_fAllowSubstitution;

};

struct Permutation // tag = perm
{
							Permutation(Alloc * pAlloc)
							:m_istrVar()
							,m_arypOpt(pAlloc, BK_UnitTest)
							,m_arypPermChild(pAlloc, BK_UnitTest)
							,m_lexsp()
								{ ; }

							void FreeAll(Alloc * pAlloc)
							{
								auto ppOptEnd = m_arypOpt.PMac();
								for (auto ppOpt = m_arypOpt.A(); ppOpt != ppOptEnd; ++ppOpt)
								{
									(*ppOpt)->FreeAll(pAlloc);
								}
								m_arypOpt.Clear();

								auto ppPermEnd = m_arypPermChild.PMac();
								for (auto ppPerm = m_arypPermChild.A(); ppPerm != ppPermEnd; ++ppPerm)
								{
									(*ppPerm)->FreeAll(pAlloc);
								}
								m_arypPermChild.Clear();
							}

	Moe::InString			m_istrVar;			// substitution name '?type' (excluding the 's')
	CDynAry<Option *>		m_arypOpt;
	CDynAry<Permutation *>	m_arypPermChild;	// other variable to permute for this option
	LexSpan					m_lexsp;
};

enum UTESTK
{
	UTESTK_Permute,
	UTESTK_Builtin,

	MOE_MAX_MIN_NIL(UTESTK)
};

struct UnitTest // tag = utest
{
							UnitTest(Alloc * pAlloc)
							:m_istrName()
							,m_pChzPrereq(nullptr)
							,m_pChzInput(nullptr)
							,m_pChzParse(nullptr)
							,m_pChzTypeCheck(nullptr)
							,m_pChzValues(nullptr)
							,m_pChzBytecode(nullptr)
							,m_utestk(UTESTK_Permute)
							,m_arypPerm(pAlloc, BK_UnitTest)
								{ ; }

							void FreeAll(Alloc * pAlloc)
							{
								pAlloc->MOE_FREE(m_pChzPrereq);
								pAlloc->MOE_FREE(m_pChzInput);
								pAlloc->MOE_FREE(m_pChzParse);
								pAlloc->MOE_FREE(m_pChzTypeCheck);
								pAlloc->MOE_FREE(m_pChzValues);
								pAlloc->MOE_FREE(m_pChzBytecode);
								m_pChzPrereq = nullptr;
								m_pChzInput = nullptr;
								m_pChzParse = nullptr;
								m_pChzTypeCheck = nullptr;
								m_pChzValues = nullptr;
								m_pChzBytecode = nullptr;

								auto ppPermEnd = m_arypPerm.PMac();
								for (auto ppPerm = m_arypPerm.A(); ppPerm != ppPermEnd; ++ppPerm)
								{
									(*ppPerm)->FreeAll(pAlloc);
								}
								m_arypPerm.Clear();
							}

	Moe::InString			m_istrName;
	char *					m_pChzPrereq;
	char *					m_pChzInput;
	char *					m_pChzParse;
	char *					m_pChzTypeCheck;
	char *					m_pChzValues;
	char *					m_pChzBytecode;
	UTESTK					m_utestk;
	CDynAry<Permutation *>	m_arypPerm;
};



struct Substitution // tag = sub
{
	Moe::InString	m_istrVar;
	Option	*		m_pOpt;
	const char *	m_pChzOption;	// option string with substitutions
};

enum TESTRES	// TEST RESults
{
	TESTRES_Success,
	TESTRES_UnitTestFailure,	// failed parsing the test
	TESTRES_SourceError,		// the unit test has an unexpected error in it's source
	TESTRES_MissingExpectedErr,
	TESTRES_ParseMismatch,		
	TESTRES_TypeCheckMismatch,
	TESTRES_CodeGenFailure,
	TESTRES_BuiltinFailure,
	
	MOE_MAX_MIN_NIL(TESTRES)
};

typedef CFixAry<Substitution, 64> SubStack;

struct TestContext // tag = tesctx
{
						TestContext(Moe::Alloc * pAlloc, ErrorManager * pErrman, Workspace * pWork, GRFCOMPILE grfcompile)
						:m_pAlloc(pAlloc)
						,m_pErrman(pErrman)
						,m_pWork(pWork)
						,m_grfcompile(grfcompile)
						,m_arySubStack()
							{ ZeroAB(m_mpTestresCResult, sizeof(m_mpTestresCResult)); }

	Moe::Alloc *		m_pAlloc;
	ErrorManager *		m_pErrman;
	Workspace *			m_pWork;

	GRFCOMPILE			m_grfcompile;
	int					m_mpTestresCResult[TESTRES_Max];
	SubStack			m_arySubStack;
};

static bool FConsumeIdentifier(TestContext * pTesctx, Lexer  * pLex, const Moe::InString & istrIdent)
{
	if (pLex->m_tok != TOK_Identifier)
		return false;

	if (pLex->m_istr != istrIdent)
		return false;

	TokNext(pLex);
	return true;
}


void EmitError(TestContext * pTesctx, const LexSpan & lexsp, ERRID errid, const char * pChzFormat, va_list ap)
{
	EmitError(pTesctx->m_pErrman, lexsp, errid, pChzFormat, ap);
}

void EmitError(TestContext * pTesctx, const LexSpan & lexsp, ERRID errid, const char * pChzFormat, ...)
{
	va_list ap;
	va_start(ap, pChzFormat);
	EmitError(pTesctx->m_pErrman, lexsp, errid, pChzFormat, ap);
}

void EmitWarning(TestContext * pTesctx, const LexSpan & lexsp, ERRID errid, const char * pChzFormat, ...)
{
	va_list ap;
	va_start(ap, pChzFormat);
	EmitWarning(pTesctx->m_pErrman, lexsp, errid, pChzFormat, ap);
}

char * PChzExpectString(TestContext * pTesctx, Lexer * pLex)
{
	if (pLex->m_tok != TOK_Literal || pLex->m_litk != LITK_String)
	{
		EmitError(pTesctx, LexSpan(pLex), ERRID_StringLiteralExpected, 
			"Expected string literal but encountered '%s'", PChzCurrentToken(pLex));
		return nullptr;
	}

	size_t cB = pLex->m_istr.CB();
	char * pChzReturn = (char*)pTesctx->m_pAlloc->MOE_ALLOC(cB, MOE_ALIGN_OF(char));
	Moe::CBCopyChz(pLex->m_istr.m_pChz, pChzReturn, cB);

	TokNext(pLex);
	return pChzReturn;
}

char * PChzParseInside(TestContext * pTesctx, Lexer * pLex, TOK tokBegin, TOK tokEnd)
{
	if (pLex->m_tok != tokBegin)
	{
		EmitError(pTesctx, LexSpan(pLex), ERRID_UnexpectedToken, 
			"Expected '%s' but encountered '%s'", PChzFromTok(tokBegin), PChzCurrentToken(pLex));
		return nullptr;
	}

	TokNext(pLex);

	LexSpan lexspBegin(pLex);
	auto pChzInput = pLex->m_pChBegin;

	int cMatches = 1;
	while (pLex->m_tok != TOK_Eof)
	{
		if (pLex->m_tok == tokBegin)
			++cMatches;
		if (pLex->m_tok == tokEnd)
		{
			--cMatches;
			if (cMatches <= 0)
				break;
		}
		TokNext(pLex);
	}
	LexSpan lexspEnd(pLex);
	TokNext(pLex);

	size_t cB = lexspEnd.m_iB - lexspBegin.m_iB + 1;
	char * pChzReturn = (char*)pTesctx->m_pAlloc->MOE_ALLOC(cB, MOE_ALIGN_OF(char));
	Moe::CBCopyChz(pChzInput, pChzReturn, cB);
	return pChzReturn;
}

bool FExpectToken(TestContext * pTesctx, Lexer * pLex, TOK tok)
{
	if (!FTryConsumeToken(pLex, tok))
	{
		EmitError(pTesctx, LexSpan(pLex), ERRID_UnexpectedToken, 
			"Expected '%s', but encountered '%s'", PChzFromTok(tok), PChzCurrentToken(pLex));
		return false;
	}

	return true;
}

void PromoteStringEscapes(TestContext * pTesctx, const LexSpan & lexsp, const char * pChzInput, StringEditBuffer * pSeb)
{
	const char * pChzIn = pChzInput;
	if (*pChzIn != '"')
	{
		EmitError(pTesctx, lexsp, ERRID_MissingQuote, "missing opening string quote for string literal (%s) ", pChzInput);
	}
	else
	{
		++pChzIn;
	}

	while (*pChzIn != '"')
	{
		if (*pChzIn == '\0')
		{
			EmitError(pTesctx, lexsp, ERRID_MissingQuote, "missing closing quote for string literal (%s) ", pChzInput);
			break;
		}

		if (*pChzIn == '\\')
		{
			++pChzIn;
			switch (*pChzIn)
			{
			case '\\':	pSeb->AppendChz("\\");	break;
			case 'n':	pSeb->AppendChz("\n");	break;
			case 'r':	pSeb->AppendChz("\r");	break;
			case 't':	pSeb->AppendChz("\t");	break;
			case '"':	pSeb->AppendChz("\"");	break;
			case '\'':	pSeb->AppendChz("\\");	break;

			default:	pSeb->AppendChz("?");		break;
			}
			++pChzIn;
		}
		else
		{
			pChzIn += pSeb->CBAppendCodepoint(pChzIn);
		}
	}
}

static inline bool FIsIdentifierChar(const char * pChz)
{
	return ((*pChz >= 'a') & (*pChz <= 'z')) | ((*pChz >= 'A') & (*pChz <= 'Z'))
		 || (*pChz == '_') 
		 || (u8(*pChz) >= 128);   // >= 128 is UTF8 char
}

char * PChzAllocateSubstitution(
	TestContext * pTesctx,
	const LexSpan & lexsp,
	const char * pChzInput,
	const SubStack & arySub,
	Substitution * pSubSelf = nullptr)
{
	if (!pChzInput)
	{
		pChzInput = "";
	}
	StringEditBuffer seb(pTesctx->m_pAlloc);

	const char * pChzIt = pChzInput;
	while (*pChzIt != '\0')
	{
		if (*pChzIt == '?')
		{
			++pChzIt;

			int cCodepoint = 0;
			auto pChzEnd = pChzIt;
			while (FIsIdentifierChar(pChzEnd))
			{
				++cCodepoint;
				pChzEnd += CBCodepoint(pChzEnd);
			}

			auto pSubMax = arySub.PMac();
			const Substitution * pSub = nullptr;
			for (auto pSubIt = arySub.A(); pSubIt != pSubMax; ++pSubIt)
			{
				if (FAreChzEqual(pSubIt->m_istrVar.m_pChz, pChzIt, cCodepoint) && CCodepoint(pSubIt->m_istrVar.m_pChz) == cCodepoint)
				{
					pSub = pSubIt;
					break;
				}
			}

			if (!pSub)
			{
				Moe::InString istrName = IstrInternCopy(pChzIt, pChzEnd - pChzIt +1);
				EmitError(pTesctx, lexsp, ERRID_CantFindSubstitution, 
					"Unable to find substitution for ?%s in string %s", istrName.m_pChz, pChzInput);
				return nullptr;
			}
			else if (pSub == pSubSelf)
			{
				EmitError(pTesctx, lexsp, ERRID_NoRecursiveSubstitute, 
					"Cannot substitute ?%s recursively in option %s", pSubSelf->m_istrVar.m_pChz, pChzInput);
				return nullptr;
			}

			seb.AppendChz(pSub->m_pChzOption);
			pChzIt = pChzEnd;
		}
		else
		{
			pChzIt += seb.CBAppendCodepoint(pChzIt);
		}
	}

	return seb.PChzAllocateCopy(pTesctx->m_pAlloc);
}

Option * POptParse(TestContext * pTesctx, Lexer * pLex)
{
	LexSpan lexsp(pLex);
	const char * pChzMin = pLex->m_pChBegin;
	const char * pChzMax = pChzMin;
	const char * pChzErridMin = nullptr;
	const char * pChzErridMax = nullptr;

	InString istrErrid = UTest::g_istrErrid;
	Moe::CFixAry<ERRID, s_cErridOptionMax> aryErrid;
	ERRID errid = ERRID_Nil;
	while (pLex->m_tok != TOK('|') && pLex->m_tok != TOK(')'))
	{
		if (pLex->m_tok == TOK('?'))
		{
			pChzErridMin = pLex->m_pChBegin;

			TokNext(pLex);

			if (!FConsumeIdentifier(pTesctx, pLex, istrErrid))
			{
				EmitError(pTesctx, LexSpan(pLex), ERRID_ErridExpected,
					"expected ?errid, encountered '%s'", PChzCurrentToken(pLex));
				break;
			}
			
			if (FExpectToken(pTesctx, pLex, TOK('(')))
			{
				while (1)
				{
					if (pLex->m_tok != TOK_Literal)
					{
						EmitError(pTesctx, LexSpan(pLex), ERRID_ErridExpected, 
							"expected ?errid number, encountered '%s'", PChzCurrentToken(pLex));
					}
					else
					{
						if (aryErrid.C() >= aryErrid.CMax())
						{
							EmitError(pTesctx, LexSpan(pLex), ERRID_TooManyErrids, 
								"too many error ids for this option, limit is %d", aryErrid.CMax());
						}
						else
						{
							aryErrid.Append((ERRID)pLex->m_n);
						}
						TokNext(pLex);
					}

					if (!FTryConsumeToken(pLex, TOK(',')))
						break;
				}
				FExpectToken(pTesctx, pLex, TOK(')'));
			}
			pChzErridMax = pLex->m_pChBegin;
			continue;
		}
		TokNext(pLex);
	}

	Option * pOpt = MOE_NEW(pTesctx->m_pAlloc, Option) Option(pTesctx->m_pAlloc);

	// copy the option string but omit the errid section

	pChzMax = pLex->m_pChBegin;
	size_t cBPrefix = pChzMax - pChzMin;
	if (pChzErridMin)
	{
		cBPrefix = pChzErridMin - pChzMin;
	}

	size_t cBPostfix = 0;
	if (pChzErridMin)
	{
		MOE_ASSERT(pChzErridMax, "expected min and max together");
		cBPostfix = (pChzMax - pChzErridMax);
	}
	if (cBPrefix + cBPostfix <= 0)
	{
		EmitError(pTesctx, LexSpan(pLex), ERRID_OptionStringExpected, "expected option string");
	}
	char * pChzOption = (char*)pTesctx->m_pAlloc->MOE_ALLOC(cBPrefix + cBPostfix+1, MOE_ALIGN_OF(char));

	if (cBPrefix)
	{
		Moe::CBCopyChz(pChzMin, pChzOption, cBPrefix+1);
	}
	if (cBPostfix)
	{
		Moe::CBCopyChz(pChzErridMin, pChzOption + cBPrefix, cBPostfix+1);
	}

	if (pChzOption[0] == '"')
	{
		StringEditBuffer seb(pTesctx->m_pAlloc);
		PromoteStringEscapes(pTesctx, lexsp, pChzOption, &seb);
		pTesctx->m_pAlloc->MOE_DELETE(pChzOption);
		pChzOption = seb.PChzAllocateCopy(pTesctx->m_pAlloc);
		pOpt->m_fAllowSubstitution = true;
	}

	pOpt->m_pChzOption = pChzOption;

	pOpt->m_aryErridExpected.Append(aryErrid.A(), aryErrid.C());
	return pOpt;
}

static Permutation * PPermParse(TestContext * pTesctx, Lexer * pLex)
{
	LexSpan lexsp(pLex);
	if (pLex->m_tok != TOK('?'))
	{
		return nullptr;
	}

	TokNext(pLex);

	if (pLex->m_tok != TOK_Identifier)
	{
		EmitError(pTesctx, LexSpan(pLex), ERRID_ExpectedTestVar, 
			"Expected variable name (following '?'), but encountered '%s'", PChzCurrentToken(pLex));
		return nullptr;
	}

	Permutation * pPerm = MOE_NEW(pTesctx->m_pAlloc, Permutation) Permutation(pTesctx->m_pAlloc);
	pPerm->m_istrVar = pLex->m_istr;
	pPerm->m_lexsp = lexsp;
	TokNext(pLex);

	if (FTryConsumeToken(pLex, TOK('(')))
	{
		while (1)
		{
			auto pOpt = POptParse(pTesctx, pLex);
			if (pOpt)
			{
				pPerm->m_arypOpt.Append(pOpt);
			}

			if (pLex->m_tok != TOK('|'))
				break;
			TokNext(pLex); // consume the '|'
		}
	}
	else
	{
		EmitError(pTesctx, LexSpan(pLex), ERRID_PermutationExpected, 
			"Expected permutation options, but encountered '%s'", PChzCurrentToken(pLex));
	}

	(void)FExpectToken(pTesctx, pLex, TOK(')'));

	// parse child permutations
	if (FTryConsumeToken(pLex, TOK('+')))
	{
		auto pPermChild = PPermParse(pTesctx, pLex);
		if (!pPermChild)
		{
			EmitError(pTesctx, LexSpan(pLex), ERRID_PermutationExpected,
				"Expected permutation, but encountered '%s'", PChzCurrentToken(pLex));
		}
		else
		{
			pPerm->m_arypPermChild.Append(pPermChild);
		}
	}
	else if (FTryConsumeToken(pLex, TOK('{')))
	{
		do
		{
			// allow a trailing comma on a list of permutations 
			if (pLex->m_tok == TOK('}'))
				break;

			auto pPermChild = PPermParse(pTesctx, pLex);
			if (!pPermChild)
			{
				EmitError(pTesctx, LexSpan(pLex), ERRID_PermutationExpected, 
					"Expected permutation, but encountered '%s'", PChzCurrentToken(pLex));
				break;
			}

			pPerm->m_arypPermChild.Append(pPermChild);
		}
		while (FTryConsumeToken(pLex, TOK(',')));

		(void) FExpectToken(pTesctx, pLex, TOK('}'));
	}

	return pPerm;
}
static void ParsePermuteString(TestContext * pTesctx, Lexer * pLex, UnitTest * pUtest)
{
	FExpectToken(pTesctx, pLex, TOK('{'));
	do
	{
		auto pPerm = PPermParse(pTesctx, pLex);
		if (!pPerm)
			break;

		pUtest->m_arypPerm.Append(pPerm);
	}
	while (FTryConsumeToken(pLex, TOK(',')));

	FExpectToken(pTesctx, pLex, TOK('}'));
}

static UnitTest * PUtestParse(TestContext * pTesctx, Lexer * pLex)
{
	if (!FConsumeIdentifier(pTesctx, pLex, UTest::g_istrTest))
	{
		EmitError(pTesctx, LexSpan(pLex), ERRID_TestExpected, "Expected 'test' directive, but encountered '%s'", PChzCurrentToken(pLex));
		return nullptr;
	}

	UnitTest * pUtest = MOE_NEW(pTesctx->m_pAlloc, UnitTest) UnitTest(pTesctx->m_pAlloc);

	auto pChzDirective = "test";
	if (pLex->m_istr == UTest::g_istrBuiltin)
	{
		TokNext(pLex);
		pUtest->m_utestk = UTESTK_Builtin;
		pChzDirective = "builtin";
	}
	if (pLex->m_tok != TOK_Identifier)
	{
		EmitError(pTesctx, LexSpan(pLex), ERRID_TestExpected,
			"Expected test name to follow %s directive, but encountered '%s'", pChzDirective, PChzCurrentToken(pLex));
		return nullptr;
	}


	pUtest->m_istrName = pLex->m_istr;
	TokNext(pLex);

	while (1)
	{
		if (FConsumeIdentifier(pTesctx, pLex, UTest::g_istrPrereq))
		{
			pUtest->m_pChzPrereq = PChzExpectString(pTesctx, pLex);
		}
		else if (FConsumeIdentifier(pTesctx, pLex, UTest::g_istrInput))
		{
			pUtest->m_pChzInput = PChzExpectString(pTesctx, pLex);
		}
		else if (FConsumeIdentifier(pTesctx, pLex, UTest::g_istrParse))
		{
			pUtest->m_pChzParse = PChzExpectString(pTesctx, pLex);
		}
		else if (FConsumeIdentifier(pTesctx, pLex, UTest::g_istrTypeCheck))
		{
			pUtest->m_pChzTypeCheck = PChzExpectString(pTesctx, pLex);
		}
		else if (FConsumeIdentifier(pTesctx, pLex, UTest::g_istrValues))
		{
			pUtest->m_pChzValues = PChzExpectString(pTesctx, pLex);
		}
		else if (FConsumeIdentifier(pTesctx, pLex, UTest::g_istrByteCode))
		{
			pUtest->m_pChzBytecode = PChzExpectString(pTesctx, pLex);
		}
		else if (pLex->m_tok == TOK('{'))
		{
			ParsePermuteString(pTesctx, pLex, pUtest);
		}
		else
		{
			if ((pLex->m_tok == TOK_Identifier && pLex->m_istr == IstrIntern("test")) ||
				pLex->m_tok == TOK_Eof)
			{
				break;
			}

			EmitError(pTesctx, LexSpan(pLex), ERRID_UnexpectedToken, 
				"unknown token encountered '%s' during test %s", PChzCurrentToken(pLex), pUtest->m_istrName.m_pChz);
			TokNext(pLex);
			SkipRestOfLine(pLex);
		}
	}

	if (pUtest->m_utestk != UTESTK_Permute)
	{
		if (pUtest->m_pChzPrereq || 
			pUtest->m_pChzInput || 
			pUtest->m_pChzParse || 
			pUtest->m_pChzTypeCheck || 
			pUtest->m_pChzValues || 
			pUtest->m_pChzBytecode || 
			pUtest->m_arypPerm.C())
		{
			EmitError(pTesctx, LexSpan(pLex), ERRID_NotPermuteTest,
				"permute string test is not allowed for test %s", pUtest->m_istrName.m_pChz);
		}
	}
	
	return pUtest;
}

void ParseMoetestFile(TestContext * pTesctx, Lexer * pLex, CDynAry<UnitTest *> * parypUtest)
{
	// load the first token
	TokNext(pLex);

	while (pLex->m_tok != TOK_Eof)
	{
		UnitTest * pUtest = PUtestParse(pTesctx, pLex);

		if (!pUtest)
		{
			EmitError(pTesctx, LexSpan(pLex), ERRID_UnexpectedToken, "Unexpected token '%s' in test definition", PChzCurrentToken(pLex));
			break;
		}

		parypUtest->Append(pUtest);
	}
}

void PrintHighlightMatch(const char * pChzPrint, const char * pChzMatch, GRFCCOL grfccolMatch, GRFCCOL grfccolMismatch)
{
	ConsoleColorAmbit ccolamb;

	bool fMatchPrev = true;
	SetConsoleTextColor(grfccolMatch);

	while (*pChzPrint != '\0')
	{
		bool fMatch = pChzMatch && *pChzMatch == *pChzPrint;
		if (fMatch != fMatchPrev)
		{
			fMatchPrev = fMatch;
			SetConsoleTextColor((fMatch) ? grfccolMatch : grfccolMismatch );
		}

		printf("%c", *pChzPrint);
		++pChzPrint;

		if (pChzMatch)
		{
			++pChzMatch;

			if (*pChzMatch == '\0')
			{
				pChzMatch = nullptr;
			}
		}
	}
}

void PrintTestError(const char * pChzIn, const char * pChzOut, const char * pChzExpected)
{
	ConsoleColorAmbit ccolamb;
	GRFCCOL grfccolWhite = GRFCCOL_FgIntenseWhite | (ccolamb.m_grfccol.m_raw & 0xF0);

	printf("in : %s\n", pChzIn);
	printf("out: ");
	PrintHighlightMatch(pChzOut, pChzExpected, ccolamb.m_grfccol, grfccolWhite);
	printf("\nout: ");
	PrintHighlightMatch(pChzExpected, pChzOut, ccolamb.m_grfccol, grfccolWhite);
	printf("\n");

	printf("   : ");
	auto pChOut = pChzOut;
	auto pChExp = pChzExpected;
	while (*pChOut != '\0' && *pChExp != '\0')
	{
		auto cBOut = CBCodepoint(pChOut);
		auto cBExp = CBCodepoint(pChExp);
		bool fAreSame = cBOut == cBExp;
		if (fAreSame)
		{
			for (int iB = 0; iB < cBOut; ++iB)
			{
				fAreSame &= pChOut[iB] == pChExp[iB];
			}
		}

		SetConsoleTextColor(GRFCCOL_FgIntenseRed | (ccolamb.m_grfccol.m_raw & 0xF0));
		printf("%c", (fAreSame) ? ' ' : '^');

		pChOut += cBOut;
		pChExp += cBExp;
	}

	while (*pChOut != '\0' || *pChExp != '\0')
	{
		printf("^");
		if (*pChOut != '\0')
		{
			pChOut += CBCodepoint(pChOut);
		}
		if (*pChExp != '\0')
		{
			pChExp += CBCodepoint(pChExp);
		}
	}

	printf("\n\n");
}

bool FCheckForExpectedErrors(ErrorManager * pErrman, ERRID erridMin, ERRID erridMax, TESTRES * pTestres)
{
	auto paryErrcExpected = pErrman->m_paryErrcExpected;
	if (!paryErrcExpected)
		return false;

	int cErrInRange = 0;
	auto pErrcMax = paryErrcExpected->PMac();
	for (auto pErrc = paryErrcExpected->A(); pErrc != pErrcMax; ++pErrc)
	{
		if (pErrc->m_errid < erridMin || pErrc->m_errid >= erridMax)
			continue;

		++cErrInRange;
		if (pErrc->m_c == 0)
		{
			printf("FAILURE: Missing expected Error(%d)\n", pErrc->m_errid);
			*pTestres = TESTRES_MissingExpectedErr;
		}
	}

	if (cErrInRange)
		return true;
	return false;
}

void HideDebugStringForEntries(Workspace * pWork, s64 cBHiddenMax)
{
	// hide entrys before a given point (for omitting the prereq from a unit test)

	BlockListEntry::CIterator iter(&pWork->m_blistEntry);
	while (WorkspaceEntry * pEntry = iter.Next())
	{
		auto pStnod = pEntry->m_pStnod;
		if (pStnod && pStnod->m_lexsp.m_iB < cBHiddenMax)
		{
			pEntry->m_fHideDebugString = true;
		}
	}
}

TESTRES TestresRunUnitTest(
	Workspace * pWorkParent,
	UnitTest * pUtest,
	GRFCOMPILE grfcompile,
	const char * pChzPrereq,
	const char * pChzIn,
	const char * pChzParseExpected,
	const char * pChzTypeCheckExpected,
	const char * pChzValuesExpected,
	const char * pChzBytecodeExpected,
	CDynAry<ErrorCount> * paryErrcExpected)
{
	//if (pChzPrereq && pChzPrereq[0] != '\0')
	//	printf("(%s): %s\n%s ", pUtest->m_strName.m_pChz, pChzPrereq, pChzIn);
	//else
	printf("(%s): %s ", pUtest->m_istrName.m_pChz, pChzIn);

	if (paryErrcExpected)
	{
		auto pErrcMax = paryErrcExpected->PMac();

		const char * pChzSpacer = "";
		for (auto pErrc = paryErrcExpected->A(); pErrc != pErrcMax; ++pErrc)
		{
			printf("%sErrid(%d)", pChzSpacer, pErrc->m_errid);
			pChzSpacer = ", ";
		}
	}
	printf("\n");

	ErrorManager errmanTest(pWorkParent->m_pErrman->m_aryErrid.m_pAlloc);
	Workspace work(pWorkParent->m_pAlloc, &errmanTest);
	work.m_grfunt = pWorkParent->m_grfunt;
	errmanTest.m_paryErrcExpected = paryErrcExpected;
	work.CopyUnitTestFiles(pWorkParent);

#ifdef MOE_TRACK_ALLOCATION
	u8 aBAltrac[1024 * 100];
	Alloc allocAltrac(aBAltrac, sizeof(aBAltrac));

	AllocTracker * pAltrac = PAltracCreate(&allocAltrac);
	work.m_pAlloc->SetAltrac(pAltrac);
#endif

	Compilation comp(work.m_pAlloc);

	BeginWorkspace(&work, &comp);

	StringEditBuffer sebFilename(work.m_pAlloc);
	StringEditBuffer sebInput(work.m_pAlloc);
	Workspace::File * pFile = nullptr;

	sebFilename.AppendChz(pUtest->m_istrName.m_pChz);
	pFile = work.PFileEnsure(IstrInternCopy(sebFilename.PChz()), Workspace::FILEK_Source);

	size_t cbPrereq = 0;
	if (pChzPrereq)
	{
		sebInput.AppendChz(pChzPrereq);
		sebInput.AppendChz("\n");
		cbPrereq = sebInput.CB() - 1; // don't count the null terminator
	}

	if (pChzIn)
	{
		sebInput.AppendChz(pChzIn);
	}

	pFile->m_pChzFileBody = sebInput.PChz();


	//Lexer lex;
	//BeginParse(&work, &lex, sebInput.PChz(), sebFilename.PChz());
	//work.m_pErrman->Clear();

	// Parse
	JobRef pJobParse = PJobCreateParse(&comp, &work, pFile->m_pChzFileBody, pFile->m_istrFilename, COMPHASE_TypeCheck);
	WaitForJob(&comp, &work, pJobParse.m_pJob);

	HideDebugStringForEntries(&work, cbPrereq);

	TESTRES testres = TESTRES_Success;
	if (work.m_pErrman->FHasErrors())
	{
		printf("Unexpected error parsing error during test %s\n", pUtest->m_istrName.m_pChz);
		printf("input = \"%s\"\n", sebInput.PChz());
		testres = TESTRES_SourceError;
	}

	
	bool fHasExpectedErr = FCheckForExpectedErrors(&errmanTest, ERRID_Min, ERRID_ParserMax, &testres);

	char aCh[1024];
	char * pCh = aCh;
	char * pChMax = &aCh[MOE_DIM(aCh)];
	if (!fHasExpectedErr && testres == TESTRES_Success)
	{
		if (!FIsEmptyString(pChzParseExpected))
		{
			WriteSExpressionForEntries(&work, pCh, pChMax, SEWK_Parse, FSEW_None);

			if (!FAreChzEqual(aCh, pChzParseExpected))
			{
				// print error location
				printf("PARSE ERROR during test for '%s'\n", pUtest->m_istrName.m_pChz);
				PrintTestError(pChzIn, aCh, pChzParseExpected);
				testres = TESTRES_ParseMismatch;
			}
		}
	}

	/*
	if (testres == TESTRES_Success && !work.m_pErrman->FHasHiddenErrors())
	{
		// Type Check
		PerformTypeCheck(work.m_pAlloc, work.m_pErrman, work.m_pSymtab, &work.m_blistEntry, &work.m_arypEntryChecked, work.m_grfunt);
		if (work.m_pErrman->FHasErrors())
		{
			printf("Unexpected error during type check test %s\n", pUtest->m_istrName.m_pChz);
			printf("input = \"%s\"\n", pChzIn);
			testres = TESTRES_SourceError;
		}

		fHasExpectedErr = FCheckForExpectedErrors(&errmanTest, ERRID_TypeCheckMin, ERRID_TypeCheckMax, &testres);

		if (!fHasExpectedErr && testres == TESTRES_Success && !FIsEmptyString(pChzTypeCheckExpected))
		{
			WriteSExpressionForEntries(&work, pCh, pChMax, FDBGSTR_Type | FSEW_LiteralSize | FSEW_NoWhitespace);

			size_t cB = CBChz(pChzTypeCheckExpected);
			char * aChExpected = (char *)work.m_pAlloc->MOE_ALLOC(cB, 1);
			SwapDoubleHashForPlatformBits(pChzTypeCheckExpected, aChExpected, cB);

			if (!FAreChzEqual(aCh, aChExpected))
			{
				// print error location
				printf("TYPE CHECK ERROR during test for '%s'\n", pUtest->m_istrName.m_pChz);
				PrintTestError(pChzIn, aCh, aChExpected);
				testres = TESTRES_TypeCheckMismatch;
			}
			work.m_pAlloc->MOE_DELETE(aChExpected);
		}

		if (!fHasExpectedErr && testres == TESTRES_Success && !FIsEmptyString(pChzValuesExpected))
		{
			WriteSExpressionForEntries(&work, pCh, pChMax, FDBGSTR_Values);

			if (!FAreChzEqual(aCh, pChzValuesExpected))
			{
				// print error location
				printf("VALUE CHECK ERROR during test for '%s'\n", pUtest->m_istrName.m_pChz);
				PrintTestError(pChzIn, aCh, pChzValuesExpected);
				testres = TESTRES_TypeCheckMismatch;
			}
		}
	}

	if (testres == TESTRES_Success && !work.m_pErrman->FHasHiddenErrors())
	{

		CBuilderIR buildir(&work, sebFilename.PChz(), FCOMPILE_None);

		SDataLayout dlay;
		buildir.ComputeDataLayout(&dlay);

		CodeGenEntryPointsLlvm(&work, &buildir, work.m_pSymtab, &work.m_blistEntry, &work.m_arypEntryChecked);

		if (work.m_pErrman->FHasErrors())
		{
			printf("Unexpected error during codegen for test %s\n", pUtest->m_istrName.m_pChz);
			testres = TESTRES_CodeGenFailure;
		}

		if (!fHasExpectedErr && testres == TESTRES_Success && !FIsEmptyString(pChzBytecodeExpected))
		{
			CHash<HV, void *> hashHvPFn(work.m_pAlloc, BK_ForeignFunctions);
			CDynAry<void *> arypDll(work.m_pAlloc, BK_ForeignFunctions);
			if (!BCode::LoadForeignLibraries(&work, &hashHvPFn, &arypDll))
			{
				printf("Failed loading foreign libraries.\n");
				testres = TESTRES_TypeCheckMismatch;
			}
			else
			{
				BCode::SProcedure * pProcUnitTest = nullptr;
				BCode::CBuilder buildBc(&work, &dlay, &hashHvPFn);
				CodeGenEntryPointsBytecode(&work, &buildBc, work.m_pSymtab, &work.m_blistEntry, &work.m_arypEntryChecked, &pProcUnitTest);
				if (grfcompile.FIsSet(FCOMPILE_PrintIR))
				{
					buildBc.PrintDump();
				}

				char aCh[2048];
				SStringBuffer strbufBytecode(aCh, MOE_DIM(aCh));

				if (MOE_FVERIFY(pProcUnitTest, "expected unit test procedure"))
				{
					static const u32 s_cBStackMax = 2048;
					u8 * pBStack = (u8 *)work.m_pAlloc->MOE_ALLOC(s_cBStackMax, 16);

					BCode::CVirtualMachine vm(pBStack, &pBStack[s_cBStackMax], &buildBc);
					buildBc.SwapToVm(&vm);

					vm.m_pStrbuf = &strbufBytecode;
#if DEBUG_PROC_CALL
					vm.m_aryDebCall.SetAlloc(work.m_pAlloc, BK_ByteCode, 32);
#endif

					BCode::ExecuteBytecode(&vm, pProcUnitTest);
					work.m_pAlloc->MOE_DELETE(pBStack);
				}

				if (!FAreChzEqual(aCh, pChzBytecodeExpected))
				{
					// print error location
					printf("BYTECODE ERROR during test for '%s'\n", pUtest->m_istrName.m_pChz);
					PrintTestError(pChzIn, aCh, pChzBytecodeExpected);
					testres = TESTRES_TypeCheckMismatch;
				}
			}

			BCode::UnloadForeignLibraries(&arypDll);
		}
	}
	*/

	(void) FCheckForExpectedErrors(&errmanTest, ERRID_CodeGenMin, ERRID_Max, &testres);

	sebFilename.Resize(0, 0, 0);
	sebInput.Resize(0, 0, 0);
	
	/*
	if (pFile && pFile->m_pDif)
	{
		work.m_pAlloc->MOE_DELETE(pFile->m_pDif);
		pFile->m_pDif = nullptr;
	}*/

	pWorkParent->m_pErrman->AddChildErrors(&errmanTest);
	errmanTest.m_aryErrid.Clear();
	MOE_ASSERT(pWorkParent->m_pErrman->m_pWork == pWorkParent, "whaa?");

	EndWorkspace(&work);

#ifdef MOE_TRACK_ALLOCATION
	DeleteAltrac(&allocAltrac, pAltrac);
	work.m_pAlloc->SetAltrac(nullptr);
#endif
	
	return testres;
}


void TestPermutation(TestContext * pTesctx, Permutation * pPerm, UnitTest * pUtest)
{
	auto pSub = pTesctx->m_arySubStack.AppendNew();
	pSub->m_istrVar = pPerm->m_istrVar;
	pSub->m_pChzOption = nullptr;

	bool fIsFlagPermutation = (pPerm->m_istrVar == UTest::g_istrTestFlags);

	char * pChzSub = nullptr;
	auto ppOptMax = pPerm->m_arypOpt.PMac();
	GRFUNT grfuntPrev = pTesctx->m_pWork->m_grfunt;
	for (auto ppOpt = pPerm->m_arypOpt.A(); ppOpt != ppOptMax; ++ppOpt)
	{
		pSub->m_pOpt = *ppOpt;
		if (fIsFlagPermutation)
		{
			// BB - need parsing for handling flag combinations
			GRFUNT grfunt(grfuntPrev);
			if (FAreChzEqual(pSub->m_pOpt->m_pChzOption, UTest::g_pChzGlobal))
			{
				grfunt.Clear(FUNT_ImplicitProc);
			}
			else if (FAreChzEqual(pSub->m_pOpt->m_pChzOption, UTest::g_pChzLocal))
			{
				grfunt.AddFlags(FUNT_ImplicitProc);
			}
			else
			{
				printf("unhandled permutation '%s' in '%s' \n", pSub->m_pOpt->m_pChzOption, UTest::g_pChzTestFlags);
				++pTesctx->m_mpTestresCResult[TESTRES_SourceError];
				continue;
			}
			pTesctx->m_pWork->m_grfunt = grfunt;
		}

		if (pSub->m_pOpt->m_fAllowSubstitution)
		{
			pChzSub = PChzAllocateSubstitution(pTesctx, pPerm->m_lexsp, pSub->m_pOpt->m_pChzOption, pTesctx->m_arySubStack, pSub);
			pSub->m_pChzOption = pChzSub;
		}
		else
		{
			pSub->m_pChzOption = pSub->m_pOpt->m_pChzOption;
		}


		if (pPerm->m_arypPermChild.C())
		{
			auto ppPermChildMax = pPerm->m_arypPermChild.PMac();
			for (auto ppPermChild = pPerm->m_arypPermChild.A(); ppPermChild != ppPermChildMax; ++ppPermChild)
			{
				TestPermutation(pTesctx, *ppPermChild, pUtest);
			}
		}
		else
		{
			CDynAry<ErrorCount> aryErrcExpected(pTesctx->m_pAlloc, BK_UnitTest);
			auto pSubMax = pTesctx->m_arySubStack.PMac();
			for (auto pSubIt = pTesctx->m_arySubStack.A(); pSubIt != pSubMax; ++pSubIt)
			{
				//printf("?%s:%s, ", pSubIt->m_strVar.PChz(), (pSubIt->m_pChzOptioh) ? pSubIt->m_pChzOption : "null");

				if (!pSubIt->m_pOpt->m_aryErridExpected.FIsEmpty())
				{
					auto pErridMac = pSubIt->m_pOpt->m_aryErridExpected.PMac();
					for (auto pErrid = pSubIt->m_pOpt->m_aryErridExpected.A(); pErrid != pErridMac; ++pErrid)
					{
						aryErrcExpected.Append(*pErrid);
					}
				}
			}

			//printf("\n");

			{
				auto pChzPrereq = PChzAllocateSubstitution(pTesctx, pPerm->m_lexsp, pUtest->m_pChzPrereq, pTesctx->m_arySubStack);
				auto pChzInput = PChzAllocateSubstitution(pTesctx, pPerm->m_lexsp, pUtest->m_pChzInput, pTesctx->m_arySubStack);
				auto pChzParse = PChzAllocateSubstitution(pTesctx, pPerm->m_lexsp, pUtest->m_pChzParse, pTesctx->m_arySubStack);
				auto pChzTypeCheck = PChzAllocateSubstitution(pTesctx, pPerm->m_lexsp, pUtest->m_pChzTypeCheck, pTesctx->m_arySubStack);
				auto pChzValues = PChzAllocateSubstitution(pTesctx, pPerm->m_lexsp, pUtest->m_pChzValues, pTesctx->m_arySubStack);
				auto pChzBytecode = PChzAllocateSubstitution(pTesctx, pPerm->m_lexsp, pUtest->m_pChzBytecode, pTesctx->m_arySubStack);

				if (!pChzInput || !pChzParse || !pChzTypeCheck || !pChzValues)
				{
					printf("... skipping test due to errors\n");
					++pTesctx->m_mpTestresCResult[TESTRES_UnitTestFailure];
				}
				else
				{
					TESTRES testres = TESTRES_UnitTestFailure;
					testres = TestresRunUnitTest(
								pTesctx->m_pWork,
								pUtest,
								pTesctx->m_grfcompile,
								pChzPrereq,
								pChzInput,
								pChzParse,
								pChzTypeCheck,
								pChzValues,
								pChzBytecode,
								&aryErrcExpected);
					++pTesctx->m_mpTestresCResult[testres];
				}

				if (pChzPrereq) pTesctx->m_pAlloc->MOE_DELETE((char*)pChzPrereq);
				if (pChzInput)	pTesctx->m_pAlloc->MOE_DELETE((char*)pChzInput);
				if (pChzParse)	pTesctx->m_pAlloc->MOE_DELETE((char*)pChzParse);
				if (pChzTypeCheck)	pTesctx->m_pAlloc->MOE_DELETE((char*)pChzTypeCheck);
				if (pChzValues)		pTesctx->m_pAlloc->MOE_DELETE((char*)pChzValues);
				if (pChzBytecode)	pTesctx->m_pAlloc->MOE_DELETE((char*)pChzBytecode);
			}
		}

		if (pChzSub)
		{
			pTesctx->m_pAlloc->MOE_DELETE(pChzSub);
			pChzSub = nullptr;
		}
		pSub->m_pChzOption = nullptr;
	}

	MOE_ASSERT(pTesctx->m_arySubStack.PLast() == pSub, "bad push/pop");
	pTesctx->m_pWork->m_grfunt = grfuntPrev;
	pTesctx->m_arySubStack.PopLast();
}

struct Counter
{
				Counter()
				{
					++s_cCtor;				
				}

				~Counter()
				{
					++s_cDtor;
				}

	static void	Reset()
				{
					s_cCtor = 0;
					s_cDtor = 0;
				}

	static int s_cCtor;
	static int s_cDtor;
};

int Counter::s_cCtor = 0;
int Counter::s_cDtor = 0;

bool FTestBlockList(Alloc * pAlloc)
{
	int aN[] = {1, 2, 3, 5, 6, 7, 8, 9, 10, 1111, 2222, 3333, 4444, -1, 0 };	

	CBlockList<int, 1> blistA(pAlloc, BK_UnitTest);
	CBlockList<int, 2> blistB(pAlloc, BK_UnitTest);
	CBlockList<int, 5> blistC(pAlloc, BK_UnitTest);
	CBlockList<int, 100> blistD(pAlloc, BK_UnitTest);

	int * pNMac = MOE_PMAC(aN);
	for (int * pN = aN; pN != pNMac; ++pN)
	{
		blistA.Append(*pN);	
		blistB.Append(*pN);	
		blistC.Append(*pN);	
		blistD.Append(*pN);	
	}

	if (blistA.C() != MOE_DIM(aN)) { printf("wrong count A\n"); return false; }
	if (blistB.C() != MOE_DIM(aN)) { printf("wrong count B\n"); return false; }
	if (blistC.C() != MOE_DIM(aN)) { printf("wrong count C\n"); return false; }
	if (blistD.C() != MOE_DIM(aN)) { printf("wrong count D\n"); return false; }

	CBlockList<int, 1>::CIterator iterA(&blistA);
	CBlockList<int, 2>::CIterator iterB(&blistB);
	CBlockList<int, 5>::CIterator iterC(&blistC);
	CBlockList<int, 100>::CIterator iterD(&blistD);

	for (int iN = 0; iN < MOE_DIM(aN); ++iN)
	{
		auto pNA = iterA.Next();
		auto pNB = iterB.Next();
		auto pNC = iterC.Next();
		auto pND = iterD.Next();

		if (!pNA || *pNA != aN[iN]) { printf ("mismatch A[%d] %d != %d\n", iN, *pNA, aN[iN]); return false; }
		if (!pNB || *pNB != aN[iN]) { printf ("mismatch B[%d] %d != %d\n", iN, *pNB, aN[iN]); return false; }
		if (!pNC || *pNC != aN[iN]) { printf ("mismatch C[%d] %d != %d\n", iN, *pNC, aN[iN]); return false; }
		if (!pND || *pND != aN[iN]) { printf ("mismatch D[%d] %d != %d\n", iN, *pND, aN[iN]); return false; }
	}
// TODO: Test Ctor, Dtors

	Counter::Reset();
	CBlockList<Counter, 2> blistCounter(pAlloc, BK_UnitTest);
	for (int i=0; i< 8; ++i)
	{
		Counter counter;
		blistCounter.Append(counter);
		blistCounter.AppendNew();
	}

	blistCounter.Clear();
	if (Counter::s_cCtor != 16)
	{
		printf("wrong ctor count %d\n", Counter::s_cCtor);
		return false;
	}
	if (Counter::s_cDtor != 24) // 8 during the loop, 16 from the blists
	{
		printf("wrong dtor count %d\n", Counter::s_cDtor);
		return false;
	}

	return true;
}

#define MOE_ASSERT_EQUALS(LHS, RHS) AssertEquals(LHS, RHS, __FILE__, __LINE__)

void AssertEquals(const BigInt & bintLhs, const BigInt & bintRhs, const char * pChzFile, u32 nLine)
{
	MOE_ASSERT(FAreEqual(bintLhs, bintRhs),
		"%s (%d): expected %s%llu but calculated %s%llu",
		pChzFile, nLine,
		(bintLhs.m_fIsNegative) ? "-" : "", bintLhs.m_nAbs,
		(bintRhs.m_fIsNegative) ? "-" : "", bintRhs.m_nAbs);
}

bool FTestSigned65()
{
	/* Failing on x64, shifting by 64 bits is undefined behavior
	s64 x = 0xFFFFFFFFFFFFFFFF << 64;

	s64 nL = -400000000;
	s64 nR = 0;
	s64 shift = nL >> nR;
	MOE_ASSERT_EQUALS(BintShiftRight(BintFromInt(nL), BintFromInt(nR)), BintFromInt(nL >> nR));
	*/

	s64 s_aNSigned[] = { 0, 1, -1, 400000000, -400000000 }; //, LLONG_MAX, LLONG_MIN + 1

	// NOTE: This does not replicate s64 overflow exactly, Signed65 structs overflow like a u64 but maintaining sign 
	// values. It's not clear that this is the wrong behavior for literals... it should probably throw an overflow
	// error (?)

	MOE_ASSERT_EQUALS(BintShiftRight(BintFromInt(-1), BintFromInt(1)), BintFromInt(-1 >> 1));
	MOE_ASSERT_EQUALS(BintSub(BintFromInt(0), BintFromInt(LLONG_MIN+1)), BintFromInt(LLONG_MAX));

	for (int iNLhs = 0; iNLhs < MOE_DIM(s_aNSigned); ++iNLhs)
	{
		for (int iNRhs = 0; iNRhs < MOE_DIM(s_aNSigned); ++iNRhs)
		{
			s64 nLhs = s_aNSigned[iNLhs];
			s64 nRhs = s_aNSigned[iNRhs];
			MOE_ASSERT_EQUALS(BintAdd(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs + nRhs));
			MOE_ASSERT_EQUALS(BintSub(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs - nRhs));
			MOE_ASSERT_EQUALS(BintMul(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs * nRhs));

			MOE_ASSERT_EQUALS(BintBitwiseOr(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs | nRhs));
			MOE_ASSERT_EQUALS(BintBitwiseAnd(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs & nRhs));

			if (nRhs >= 0)
			{
				// shifting by more than the number of bits in a type results in undefined behavior
				auto nRhsClamp = (nRhs > 31) ? 31 : nRhs;
			
				MOE_ASSERT_EQUALS(BintShiftRight(BintFromInt(nLhs), BintFromInt(nRhsClamp)), BintFromInt(nLhs >> nRhsClamp));
				MOE_ASSERT_EQUALS(BintShiftLeft(BintFromInt(nLhs), BintFromInt(nRhsClamp)), BintFromInt(nLhs << nRhsClamp));
			}

			if (nRhs != 0)
			{
				MOE_ASSERT_EQUALS(BintDiv(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs / nRhs));
				MOE_ASSERT_EQUALS(BintRemainder(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs % nRhs));
			}
		}
	}

	
	u64 s_aNUnsigned[] = { 0, 1, 400000000, ULLONG_MAX };

	for (int iNLhs = 0; iNLhs < MOE_DIM(s_aNUnsigned); ++iNLhs)
	{
		for (int iNRhs = 0; iNRhs < MOE_DIM(s_aNUnsigned); ++iNRhs)
		{
			u64 nLhs = s_aNUnsigned[iNLhs];
			u64 nRhs = s_aNUnsigned[iNRhs];
			MOE_ASSERT_EQUALS(BintAdd(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs + nRhs));

			MOE_ASSERT_EQUALS(BintBitwiseOr(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs | nRhs));
			MOE_ASSERT_EQUALS(BintBitwiseAnd(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs & nRhs));

			// shifting by more than the number of bits in a type results in undefined behavior
			auto nRhsClamp = (nRhs > 31) ? 31 : nRhs;
		
			MOE_ASSERT_EQUALS(BintShiftRight(BintFromUint(nLhs), BintFromUint(nRhsClamp)), BintFromUint(nLhs >> nRhsClamp));
			MOE_ASSERT_EQUALS(BintShiftLeft(BintFromUint(nLhs), BintFromUint(nRhsClamp)), BintFromUint(nLhs << nRhsClamp));

			// does not replicate unsigned underflow, because the sign bit is tracked seperately
			if (nLhs >= nRhs)
			{
				MOE_ASSERT_EQUALS(BintSub(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs - nRhs));
			}

			MOE_ASSERT_EQUALS(BintMul(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs * nRhs));
			if (nRhs != 0)
			{
				MOE_ASSERT_EQUALS(BintDiv(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs / nRhs));
				MOE_ASSERT_EQUALS(BintRemainder(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs % nRhs));
			}
		}
	}

	MOE_ASSERT_EQUALS(BintAdd(BintFromUint(ULLONG_MAX, true), BintFromUint(100)), BintFromUint(ULLONG_MAX - 100, true));
	MOE_ASSERT_EQUALS(BintAdd(BintFromUint(ULLONG_MAX, true), BintFromUint(ULLONG_MAX)), BintFromUint(0));
	MOE_ASSERT_EQUALS(BintSub(BintFromUint(100), BintFromUint(ULLONG_MAX)), BintFromUint(ULLONG_MAX - 100, true));

	return true;
}

bool FTestUniqueNames(Alloc * pAlloc)
{
#ifdef MOE_TRACK_ALLOCATION
	u8 aBAltrac[1024 * 100];
	Alloc allocAltrac(aBAltrac, sizeof(aBAltrac));

	AllocTracker * pAltrac = PAltracCreate(&allocAltrac);
	pAlloc->SetAltrac(pAltrac);
#endif

	size_t cbFreePrev = pAlloc->CB();
	{
		const char * pChzIn;
		char aCh[128];
		UniqueNameSet unset(pAlloc, Moe::BK_Workspace, 0);

		pChzIn = "funcName";
		GenerateUniqueName(&unset, pChzIn, aCh, MOE_DIM(aCh));
		MOE_ASSERT(FAreChzEqual(pChzIn, aCh), "bad unique name");

		GenerateUniqueName(&unset, pChzIn, aCh, MOE_DIM(aCh));
		MOE_ASSERT(FAreChzEqual("funcName1", aCh), "bad unique name");

		pChzIn = "funcName20";
		GenerateUniqueName(&unset, pChzIn, aCh, MOE_DIM(aCh));
		MOE_ASSERT(FAreChzEqual("funcName20", aCh), "bad unique name");

		pChzIn = "234";
		GenerateUniqueName(&unset, pChzIn, aCh, MOE_DIM(aCh));
		MOE_ASSERT(FAreChzEqual("234", aCh), "bad unique name");

		pChzIn = "test6000";
		GenerateUniqueName(&unset, pChzIn, aCh, MOE_DIM(aCh));
		MOE_ASSERT(FAreChzEqual("test6000", aCh), "bad unique name");

		pChzIn = "test6000";
		GenerateUniqueName(&unset, pChzIn, aCh, MOE_DIM(aCh));
		MOE_ASSERT(FAreChzEqual("test6001", aCh), "bad unique name");
	}

	size_t cbFreePost = pAlloc->CB();
#ifdef MOE_TRACK_ALLOCATION
	if (cbFreePrev != cbFreePost)
	{
		pAlloc->PrintAllocations();
	}

	DeleteAltrac(&allocAltrac, pAltrac);
	pAlloc->SetAltrac(nullptr);
#endif
	MOE_ASSERT(cbFreePrev == cbFreePost, "memory leak testing unique names");

	return true;
}

bool FRunBuiltinTest(Moe::InString istrName, Alloc * pAlloc)
{
	bool fReturn;
	if (istrName == UTest::g_istrLexer)
	{
		fReturn = FTestLexing();
	}
	else if (istrName == UTest::g_istrSigned65)
	{
		fReturn = FTestSigned65();
	}
	else if (istrName == UTest::g_istrUnicode)
	{
		fReturn = FTestUnicode();
	}
	else if (istrName == UTest::g_istrUniqueNames)
	{
		fReturn = FTestUniqueNames(pAlloc);
	}
	else if (istrName == UTest::g_istrBlockList)
	{
		fReturn = FTestBlockList(pAlloc);
	}
	else
	{
		printf("ERROR: Unknown built in test %s\n", istrName.m_pChz);
		fReturn = false;
	}
	
	return fReturn;
}

void ParseAndTestMoetestFile(Moe::Alloc * pAlloc, ErrorManager * pErrman, Lexer * pLex, GRFCOMPILE grfcompile)
{
	TestContext tesctx(pAlloc, pErrman, pErrman->m_pWork, grfcompile);
	pErrman->m_pWork->m_grfunt = GRFUNT_DefaultTest;
	CDynAry<UnitTest *> arypUtest(pAlloc, BK_UnitTest);

	ParseMoetestFile(&tesctx, pLex, &arypUtest);
	if (pErrman->FHasErrors())
	{
		int cError, cWarning;
		pErrman->ComputeErrorCounts(&cError, &cWarning);
		printf("Failed parsing unit test file: %d errors, %d warnings\n", cError, cWarning);
		return;
	}
	
	UnitTest ** ppUtestMax = arypUtest.PMac();
	for (auto ppUtest = arypUtest.A(); ppUtest != ppUtestMax; ++ppUtest)
	{
		UnitTest * pUtest = *ppUtest;

		if (pUtest->m_utestk == UTESTK_Builtin)
		{
			printf("Built-In %s\n", pUtest->m_istrName.m_pChz);
			if (!FRunBuiltinTest(pUtest->m_istrName, pAlloc))
			{
				printf("Built in test %s Failed\n", pUtest->m_istrName.m_pChz);
				++tesctx.m_mpTestresCResult[TESTRES_BuiltinFailure];
			}
			else
			{
				++tesctx.m_mpTestresCResult[TESTRES_Success];
			}
			continue;
		}

		if (pUtest->m_arypPerm.FIsEmpty())
		{
			// no permutations, just test it as is.
			TESTRES testres = TESTRES_UnitTestFailure;
			testres = TestresRunUnitTest(
						tesctx.m_pWork, 
						pUtest, 
						tesctx.m_grfcompile,
						pUtest->m_pChzPrereq,
						pUtest->m_pChzInput,
						pUtest->m_pChzParse,
						pUtest->m_pChzTypeCheck,
						pUtest->m_pChzValues,
						pUtest->m_pChzBytecode,
						nullptr);
			++tesctx.m_mpTestresCResult[testres];
			continue;
		}

		auto ppPermMax = pUtest->m_arypPerm.PMac();
		for (auto ppPerm = pUtest->m_arypPerm.A(); ppPerm != ppPermMax; ++ppPerm)
		{
			TestPermutation(&tesctx, *ppPerm, pUtest);
		}
	}

	int cTests = 0;
	for (int testres = TESTRES_Min; testres < TESTRES_Max; ++testres)
	{
		cTests += tesctx.m_mpTestresCResult[testres];
	}

	ConsoleColorAmbit ccolamb;
	if (tesctx.m_mpTestresCResult[TESTRES_Success] == cTests)
	{
		SetConsoleTextColor(GRFCCOL_FgIntenseYellow);
		printf("\nSUCCESS: ");
	}
	else
	{
		SetConsoleTextColor(GRFCCOL_FgIntenseRed);
		printf("\nFailure: ");
	}

	printf("%d / %d tests succeeded\n", tesctx.m_mpTestresCResult[TESTRES_Success], cTests);
}

bool FUnitTestFile(Workspace * pWork, const char * pChzFilenameIn, unsigned grfcompile)
{
	Alloc * pAlloc = pWork->m_pAlloc;
	if (!pChzFilenameIn)
		pChzFilenameIn = "";
	
	auto pFile = pWork->PFileEnsure(IstrIntern(pChzFilenameIn), Workspace::FILEK_UnitTest);
	pFile->m_pChzFileBody = nullptr;

	char aChFilenameOut[Workspace::s_cBFilenameMax];
	(void)CChConstructFilename(pFile->m_istrFilename.m_pChz, Workspace::s_pChzUnitTestExtension, aChFilenameOut, MOE_DIM(aChFilenameOut));

	pFile->m_pChzFileBody = pWork->PChzLoadFile(IstrIntern(aChFilenameOut), pWork->m_pAlloc);
	if (!pFile->m_pChzFileBody)
	{
		return false;
	}

	const char * pChzFileBody = PChzSkipUnicodeBOM(pFile->m_pChzFileBody);

	printf("Testing %s\n", pFile->m_istrFilename.m_pChz);

	static const size_t cChStorage = 1024 * 8;
	char * aChStorage = (char *)pAlloc->MOE_ALLOC(cChStorage, 4);
	Lexer lex;
	InitLexer(&lex, pChzFileBody, &pChzFileBody[CBChz(pChzFileBody)-1], aChStorage, cChStorage);
	lex.m_istrFilename = pFile->m_istrFilename;

	ParseAndTestMoetestFile(pAlloc, pWork->m_pErrman, &lex, grfcompile);
	return !pWork->m_pErrman->FHasErrors();
}
