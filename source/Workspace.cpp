#include "Generics.inl"
#include "MoeString.h"
#include "Parser.h"
#include "Symbol.h"
#include "TypeInfo.h"
#include "Workspace.h"
#include "stdio.h"
#include <cstdarg>

#ifdef _MSC_VER
	#include <windows.h>
	static HANDLE s_hConsole = 0;
#endif

using namespace Moe;

const char * Workspace::s_pChzSourceExtension = ".moe";
const char * Workspace::s_pChzUnitTestExtension = ".moetest";

void PrintErrorTextSpan(Workspace * pWork, const LexSpan & lexsp);

const char * PChzFromMcallcon(MCALLCON mcallcon)
{
	static const char * s_mpMcallconPChz[] =
	{
		"#x86",			//MCALLCON_CX86
		"#stdcall",		//MCALLCON_StdcallX86
		"#x64",			//MCALLCON_X64
	};
	MOE_CASSERT(MOE_DIM(s_mpMcallconPChz) == MCALLCON_Max, "missing CALLCONV string");
	if (mcallcon == MCALLCON_Nil)
		return "Nil";

	if ((mcallcon < MCALLCON_Nil) | (mcallcon >= MCALLCON_Max))
		return "Unknown MCALLCON";

	return s_mpMcallconPChz[mcallcon];
}

const char * PChzFromInlinek(INLINEK inlinek)
{
	static const char * s_mpInlinekPChz[] =
	{
		"inline",		// INLINEK_AlwaysInline
		"noinline",		// INLINEK_NoInline
	};
	MOE_CASSERT(MOE_DIM(s_mpInlinekPChz) == INLINEK_Max, "missing INLINEK string");
	if (inlinek == INLINEK_Nil)
		return "Nil";

	if ((inlinek < INLINEK_Nil) | (inlinek >= INLINEK_Max))
		return "Unknown INLINEK";

	return s_mpInlinekPChz[inlinek];
}


ErrorManager::ErrorManager(Alloc * pAlloc)
:m_pWork(nullptr)
,m_aryErrid(pAlloc, BK_Workspace, 0)
,m_paryErrcExpected(nullptr)
,m_arypGenmapContext(pAlloc, BK_Workspace, 64)
{ 

}

bool ErrorManager::FHasHiddenErrors()
{
	if (!m_paryErrcExpected)
		return false;

	auto pErrcMax = m_paryErrcExpected->PMac();
	for (auto pErrc = m_paryErrcExpected->A(); pErrc != pErrcMax; ++pErrc)
	{
		if (pErrc->m_c)
			return true;
	}
	return false;
}

bool ErrorManager::FTryHideError(ERRID errid)
{
	if (!m_paryErrcExpected)
		return false;

	auto pErrcMax = m_paryErrcExpected->PMac();
	for (auto pErrc = m_paryErrcExpected->A(); pErrc != pErrcMax; ++pErrc)
	{
		if (pErrc->m_errid == errid)
		{
			++pErrc->m_c;
			return true;
		}
	}
	return false;
}

void ErrorManager::PushGenmapContext(GenericMap * pGenmap)
{
	m_arypGenmapContext.Append(pGenmap);
}

void ErrorManager::PopGenmapContext(GenericMap * pGenmap)
{
	if (!MOE_FVERIFY(!m_arypGenmapContext.FIsEmpty(), "instantiate context underflow in error manager"))
		return;

	auto pGenmapTop = m_arypGenmapContext.TPopLast();
	if (!MOE_FVERIFY(pGenmapTop == pGenmap, "push/pop mismatch for instantate context"))
		return;
}

void ErrorManager::ComputeErrorCounts(int * pCError, int * pCWarning)
{
	int cError = 0;
	int cWarning = 0;
	auto pErridMax = m_aryErrid.PMac();
	for (auto pErrid = m_aryErrid.A(); pErrid != pErridMax; ++pErrid)
	{
		if (*pErrid < ERRID_ErrorMax)
			++cError;
		else
			++cWarning;

	}
	*pCError = cError;
	*pCWarning = cWarning;
}

Error::Error(ErrorManager * pErrman, ERRID errid)
:m_pErrman(pErrman)
,m_errid(errid)
,m_errs(ERRS_Unreported)
{
}

void PrintErrorLine(Error * pError, const char * pChzPrefix, const LexSpan & lexsp, const char * pChz, va_list ap)
{
	auto pErrman = pError->m_pErrman;
	if (pError->m_errs == ERRS_Unreported)
	{
		if (pErrman->FTryHideError(pError->m_errid))
		{
			pError->m_errs = ERRS_Hidden;
		}
		else
		{
			pErrman->m_aryErrid.Append(pError->m_errid);
			pError->m_errs = ERRS_Reported;
		}
	}

	if (pError->m_errs == ERRS_Hidden)
		return;

	if (lexsp.FIsValid())
	{
		LexLookup lexlook(pErrman->m_pWork, lexsp);
		printf("%s(" MOE_S64FMT "," MOE_S64FMT") %s ", lexlook.m_istrFilename.m_pChz, lexlook.m_iLine, lexlook.m_iCodepoint, pChzPrefix);
	}
	else
	{
		printf("Internal %s ", pChzPrefix);
	}

	ERRID errid = pError->m_errid;
	if (errid != ERRID_Nil && errid != ERRID_UnknownError && errid != ERRID_UnknownWarning)
	{
		printf("#%d: ", errid);
	}
	
	if (pChz)
	{
		ConsoleColorAmbit ccolamb;
		ccolamb.SetConsoleForegroundColor(GRFCCOL_FgIntenseWhite);

		vprintf(pChz, ap);
		printf("\n");
	}
}

void PrintGenmapAnchors(Moe::StringBuffer * pStrbuf, GenericMap * pGenmap)
{
	const char * pChzSeparate = "(";
	const char * pChzClose = "";
	InString * pIstrAnchor;
	Anchor * pAnc;
	Moe::CHash<Moe::InString, Anchor>::CIterator iter(&pGenmap->m_mpIstrAnc);
	while ((pAnc = iter.Next(&pIstrAnchor)))
	{
		pAnc->AssertIsValid();
		InString istrAnc;
		switch (pAnc->m_genk)
		{
		case GENK_Type:
			istrAnc = IstrSExpression(pAnc->m_pTin);
			FormatChz(pStrbuf, "%s$%s :%s", pChzSeparate, pIstrAnchor->m_pChz, istrAnc.m_pChz);
			break;
		case GENK_Value:
			{
				auto pStnodValue = pAnc->m_pStnodBaked;
				if (pStnodValue && pStnodValue->m_park == PARK_Decl)
				{
					InString strIdent;
					auto pStdecl = PStnodRtiCast<STDecl *>(pStnodValue);
					const char * pChzGeneric = "";
					if (pStdecl && pStdecl->m_pStnodIdentifier)
					{
						strIdent = IstrFromIdentifier(pStdecl->m_pStnodIdentifier);
						pChzGeneric = (pStdecl->m_fIsBakedConstant) ? "$" : "";
					}
					FormatChz(pStrbuf, "%s$%s decl(%s%s)", pChzSeparate, pIstrAnchor->m_pChz, pChzGeneric, strIdent.m_pChz);
				}
				else
				{
					istrAnc = IstrSExpression(pStnodValue, SEWK_Value);
					FormatChz(pStrbuf, "%s$%s %s", pChzSeparate, pIstrAnchor->m_pChz, istrAnc.m_pChz);
				}
			} break;
		default:
			MOE_ASSERT(false, "unhandled GENK");
		}

		pChzSeparate = ", ";
		pChzClose = ")";
	}
	AppendChz(pStrbuf, pChzClose);
}

void PrintGenmapNoLocation(GenericMap * pGenmap, const char * pChzLineEnd)
{
	char aCh[1024];
	Moe::StringBuffer strbuf(aCh, MOE_DIM(aCh));
	PrintGenmapAnchors(&strbuf, pGenmap);

	printf("%s%s%s", pGenmap->m_istrName.m_pChz, aCh, pChzLineEnd);
}

void PrintGenmap(Workspace * pWork, GenericMap * pGenmap)
{
	const char * pChzLineEnd = (pGenmap->m_aryLexspSrc.C() == 1) ? "" : "\n";
	PrintGenmapNoLocation(pGenmap, pChzLineEnd); 

	for (LexSpan * pLexsp = pGenmap->m_aryLexspSrc.A(); pLexsp != pGenmap->m_aryLexspSrc.PMac(); ++pLexsp)
	{
		LexLookup lexlook(pWork, *pLexsp);
		printf("    at %s(" MOE_S64FMT "," MOE_S64FMT ")\n", lexlook.m_istrFilename.m_pChz, lexlook.m_iLine, lexlook.m_iCodepoint);
	}
}

void PrintGenericInstantiateContext(ErrorManager * pErrman)
{
	for (GenericMap ** ppGenmap = pErrman->m_arypGenmapContext.A(); ppGenmap != pErrman->m_arypGenmapContext.PMac(); ++ppGenmap)
	{
		auto pGenmap = *ppGenmap;
		printf("  while instantiating generic ");	
		PrintGenmap(pErrman->m_pWork, *ppGenmap);
	}
}

void PrintErrorLine(Error * pError, const char * pChzPrefix, const LexSpan & lexsp, const char * pChz, ...)
{
	va_list ap;
	va_start(ap, pChz);
	PrintErrorLine(pError, pChzPrefix, lexsp, pChz, ap);
}

void EmitWarning(ErrorManager * pErrman, const LexSpan & lexsp, ERRID errid, const char * pChz, va_list ap)
{
	bool fHidden = pErrman->FTryHideError(errid);
	if (fHidden)
		return;

	if (lexsp.FIsValid())
	{
		LexLookup lexlook(pErrman->m_pWork, lexsp);
		printf("%s(" MOE_S64FMT "," MOE_S64FMT ") Warning: ", lexlook.m_istrFilename.m_pChz, lexlook.m_iLine, lexlook.m_iCodepoint);
	}
	else
	{
		printf("Internal Warning: ");
	}
	pErrman->m_aryErrid.Append(errid);
	
	if (pChz)
	{
		vprintf(pChz, ap);
		printf("\n");
	}

	PrintGenericInstantiateContext(pErrman);
}

void EmitWarning(ErrorManager * pErrman, const LexSpan & lexsp, ERRID errid, const char * pChz, ...)
{
	va_list ap;
	va_start(ap, pChz);
	EmitWarning(pErrman, lexsp, errid, pChz, ap);
}

void EmitWarning(Workspace * pWork, const LexSpan & lexsp, ERRID errid, const char * pChz, ...)
{
	va_list ap;
	va_start(ap, pChz);
	EmitWarning(pWork->m_pErrman, lexsp, errid, pChz, ap);
}

void EmitWarning(ParseContext * pParctx, const LexSpan & lexsp, ERRID errid, const char * pChz, ...)
{
	va_list ap;
	va_start(ap, pChz);
	EmitWarning(pParctx->m_pWork->m_pErrman, lexsp, errid, pChz, ap);
}


void EmitError(ErrorManager * pErrman, const LexSpan & lexsp, ERRID errid, const char * pChz, va_list ap)
{
	Error error(pErrman, errid);
	PrintErrorLine(&error, "Error:", lexsp, pChz, ap);

	PrintErrorTextSpan(pErrman->m_pWork, lexsp);

	if (error.m_errs != ERRS_Hidden)
	{
		PrintGenericInstantiateContext(pErrman);
	}
}

void EmitError(ErrorManager * pErrman, const LexSpan & lexsp, ERRID errid, const char * pChz, ...)
{
	va_list ap;
	va_start(ap, pChz);
	EmitError(pErrman, lexsp, errid, pChz, ap);
}

void EmitError(Workspace * pWork, const LexSpan & lexsp, ERRID errid, const char * pChz, ...)
{
	va_list ap;
	va_start(ap, pChz);
	EmitError(pWork->m_pErrman, lexsp, errid, pChz, ap);
}

void EmitError(ParseContext * pParctx, const LexSpan & lexsp, ERRID errid, const char * pChz, ...)
{
	va_list ap;
	va_start(ap, pChz);
	EmitError(pParctx->m_pWork->m_pErrman, lexsp, errid, pChz, ap);
}

void InitConsoleSettings()
{
#ifdef _MSC_VER
	SetConsoleOutputCP(CP_UTF8);
	HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	DWORD mode;
	auto result = GetConsoleMode(hStdout, &mode);
	if (result)
	{
		mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
		SetConsoleMode(hStdout, mode);
	}
	else
	{
		printf("Failed stting console settings.\n");
	}

	s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
#endif //defined _MSC_VER
}

GRFCCOL GrfccolCurrent()
{
	GRFCCOL grfccol;
#ifdef _MSC_VER
	MOE_ASSERT(s_hConsole, "No handle to output console");

	CONSOLE_SCREEN_BUFFER_INFO buffinfo;
	GetConsoleScreenBufferInfo(s_hConsole, &buffinfo);

	grfccol = GRFCCOL(buffinfo.wAttributes & FCCOL_All);
#endif //defined _MSC_VER
	return grfccol;
}

void ResetConsoleTextColor()
{
	printf("\033[39;49m");
}

#ifndef _MSC_VER
static const int s_nLinuxBaseFg = 30;
static const int s_nLinuxBaseBg = 40;
static inline int NLinuxTextColor(int iColor, bool fIntense, int nBase)
{
	static const int s_mpGrfcolN[] =
	{
		0,		// (000b Black)
		4,		// (001b Blue)
		2,		// (010b Green)
		6,		// (011b Cyan)
		1,		// (100b Red)
		5,		// (101b Purple)
		3,		// (110b Yellow)
		7,		// (111b White)
	};

	int n = s_mpGrfcolN[iColor] + nBase;
	if (fIntense)
	{
		n += 60; // linux color -> light color
	}

	return n;
}
#endif

void SetConsoleTextColor(GRFCCOL grfccol)
{
#ifdef  _MSC_VER
	SetConsoleTextAttribute(s_hConsole, WORD(grfccol.m_raw));
#else
	int iColorFg = grfccol.m_raw & GRFCCOL_FgAllColor;
	int nFg = NLinuxTextColor(iColorFg, grfccol.FIsSet(FCCOL_FgIntense), s_nLinuxBaseFg);

	int iColorBg = (grfccol.m_raw & GRFCCOL_BgAllColor) >> 4;
	int nBg = NLinuxTextColor(iColorBg, grfccol.FIsSet(FCCOL_BgIntense), s_nLinuxBaseBg);
	printf("\033[%d;%dm", nFg, nBg);
#endif
}

void ConsoleColorAmbit::SetConsoleForegroundColor(GRFCCOL grfccol)
{
#ifdef  _MSC_VER
	GRFCCOL grfccolAdj = (grfccol & GRFCCOL_FgAll) | (m_grfccol & GRFCCOL_BgAll);
	SetConsoleTextAttribute(s_hConsole, WORD(grfccolAdj.m_raw));
#else
	int iColorFg = grfccol.m_raw & GRFCCOL_FgAllColor;
	int nFg = NLinuxTextColor(iColorFg, grfccol.FIsSet(FCCOL_FgIntense), s_nLinuxBaseFg);
	printf("\033[%dm ", nFg);

#endif //defined _MSC_VER
}

void ConsoleColorAmbit::SetConsoleBackgroundColor(GRFCCOL grfccol)
{
#ifdef  _MSC_VER
	GRFCCOL grfccolAdj = (grfccol & GRFCCOL_BgAll) | (m_grfccol & GRFCCOL_FgAll);
	SetConsoleTextAttribute(s_hConsole, WORD(grfccolAdj.m_raw));
#else
	int iColorBg = (grfccol.m_raw & GRFCCOL_BgAllColor) >> 4;
	int nBg = NLinuxTextColor(iColorBg, grfccol.FIsSet(FCCOL_BgIntense), s_nLinuxBaseBg);
	printf("\033[%dm", nBg);
#endif //defined !_MSC_VER
}

void GenerateUniqueName(UniqueNameSet * pUnset, const char * pChzIn, char * pChzOut, size_t cBOutMax)
{
	size_t iCh = CBChz(pChzIn) - 2;

	// not handling whitespace...
	u32 nIn = 0;
	u32 nMultiple = 1;
	while ((pChzIn[iCh] >= '0') & (pChzIn[iCh] <= '9'))
	{
		nIn = (pChzIn[iCh] - '0') * nMultiple + nIn;
		nMultiple *= 10;
		--iCh;
	}

	HV hv = 0;
	hv = HvFromPChz(pChzIn, iCh+1);

	u32 * pN = nullptr;
	INRES inres = pUnset->m_hashHvNUnique.InresEnsureKey(hv, &pN);
	Moe::StringBuffer strbufOut(pChzOut, cBOutMax);
	if (inres == INRES_Inserted)
	{
		*pN = nIn;
		AppendChz(&strbufOut, pChzIn);
	}
	else
	{
		*pN = moeMax(nIn, *pN + 1);
		AppendChz(&strbufOut, pChzIn);

		strbufOut.m_pChzAppend = &strbufOut.m_pChzBegin[iCh+1];
		FormatChz(&strbufOut, "%d", *pN); 
	}
}

static inline void CalculateLinePositionRaw(const char * pChBegin, s64 dBLoc, s64 * piLine, s64 * piCol)
{
	s64 iLine = 0;
	s64 iCol = 0;

	// BB - Does this need to be changed for utf8?
	for (const char * pCh = pChBegin; *pCh != '\0'; ++pCh)
	{
		s64 dB = s64(pCh - pChBegin);
		if (dB >= dBLoc)
			break;

		if (*pCh == '\n')
		{
			++iLine;
			iCol = 0;
		}
		else if (*pCh == '\t')
		{
			iCol += 4;
		}
		else
		{
			++iCol;
		}
	}
	*piLine = iLine;
	*piCol = iCol;
}

static void CalculateLinePosition(Workspace * pWork, const LexSpan & lexsp, s64 * piLine, s64 * piCol)
{
	auto pFile = pWork->PFileLookup(lexsp.m_istrFilename.m_pChz, Workspace::FILEK_Nil);
	if (!pFile)
	{
		*piLine = -1;
		*piCol = -1;
		return;
	}

	const char * pChBegin = pFile->m_pChzFileBody;
	s64 iLine;
	s64 iCol;
	auto dBWarm = pFile->m_dBWarm;
	if (lexsp.m_iB < dBWarm)
	{
		CalculateLinePositionRaw(pChBegin, lexsp.m_iB, &iLine, &iCol);
	}
	else
	{
		CalculateLinePositionRaw(pChBegin + dBWarm, lexsp.m_iB - dBWarm, &iLine, &iCol);
		iCol += (iLine == 0) ? pFile->m_iColumnWarm : 0;	// columns reset on line increments
		iLine += pFile->m_iLineWarm;
	}

	/* // Debug Warm Start
	s32 iLineOld = 0;
	s32 iColumnOld = 0;
	CalculateLinePositionRaw(pChBegin, lexsp.m_iB, &iLineOld, &iColumnOld);
	MOE_ASSERT(iLine == iLineOld && iCol == iColumnOld, "bad warm start");
	*/

	pFile->m_dBWarm = lexsp.m_iB; 
	pFile->m_iLineWarm = iLine;
	pFile->m_iColumnWarm = iCol;

	*piLine = iLine + 1;	// 1 relative
	*piCol = iCol + 1;		// 1 relative
}

struct TextSpan // tag = textspan
{
	bool		FIsValid() const
					{ return m_pChzPre != nullptr && m_pChzPost != nullptr && m_pChzBegin != nullptr && m_pChzEnd != nullptr; }

	const char *		m_pChzPre;
	const char *		m_pChzBegin;
	const char *		m_pChzEnd;
	const char *		m_pChzPost;
};

static void ComputeTextSpan(Workspace * pWork, const LexSpan & lexsp, TextSpan * pTextspan)
{
	auto pFile = pWork->PFileLookup(lexsp.m_istrFilename.m_pChz, Workspace::FILEK_Nil);
	if (!pFile || !lexsp.FIsValid())
	{
		pTextspan->m_pChzPre = nullptr;
		pTextspan->m_pChzBegin = nullptr;
		pTextspan->m_pChzEnd = nullptr;
		pTextspan->m_pChzPost = nullptr;
		return;
	}

	static const int s_cChPrefixMax = 40;
	static const int s_cChPostfixMax = 40;

	const char * pChzBody = pFile->m_pChzFileBody;
	const char * pChzBegin = &pChzBody[lexsp.m_iB];
	const char * pChzEnd = &pChzBody[lexsp.m_iB + lexsp.m_cB];

	const char * pChzPre = pChzBegin;
	const char * pChzPreMin = &pChzPre[-s_cChPrefixMax];
	while(pChzPre != pChzBody && pChzPre != pChzPreMin)
	{
		const char * pChzNext = pChzPre - 1; 
		if (*pChzNext  == '\n')	
			break;

		pChzPre = pChzNext;
	}

	const char * pChzPost = pChzEnd;
	const char * pChzPostMax = &pChzPost[s_cChPostfixMax];
	for ( ; *pChzPost != '\0' && pChzPost != pChzPostMax; ++pChzPost)
	{
		if (*pChzPost  == '\n')	
			break;
	}

	pTextspan->m_pChzPre = pChzPre;
	pTextspan->m_pChzBegin = pChzBegin;
	pTextspan->m_pChzEnd = pChzEnd;
	pTextspan->m_pChzPost = pChzPost;
}

static inline int CDigit(s64 n)
{
	int cDigit = 0;
	while (n != 0)
	{
		n /= 10;
		++cDigit;
	}

	return cDigit;
}

void PrintErrorTextSpan(Workspace * pWork, const LexSpan & lexsp)
{
	TextSpan textspan;
	ComputeTextSpan(pWork, lexsp, &textspan);
	if (!textspan.FIsValid())
		return;

	s64 iLine;
	s64 iCodepoint;
	CalculateLinePosition(pWork, lexsp, &iLine, &iCodepoint);

	static const char * s_pChzMargin = " | ";
	int cDigit = CDigit(iLine);

	{
		ConsoleColorAmbit ccolamb;
		ccolamb.SetConsoleForegroundColor(GRFCCOL_FgIntenseWhite);

		printf(MOE_S64FMT "%s", iLine, s_pChzMargin);
	}

	size_t cB = textspan.m_pChzPost - textspan.m_pChzPre;
	char * aCh = (char *)pWork->m_pAlloc->MOE_ALLOC(cB, 1);

	size_t cBPre = textspan.m_pChzBegin - textspan.m_pChzPre;
	CBCopyChz(textspan.m_pChzPre, aCh, cBPre+1);
	printf("%s", aCh);

	size_t cBSpan = textspan.m_pChzEnd - textspan.m_pChzBegin;
	{
		ConsoleColorAmbit ccolamb;
		ccolamb.SetConsoleForegroundColor(GRFCCOL_FgIntenseWhite);

		CBCopyChz(textspan.m_pChzBegin, aCh, cBSpan+1);
		printf("%s", aCh);
	}

	size_t cBPost = textspan.m_pChzPost - textspan.m_pChzEnd;
	CBCopyChz(textspan.m_pChzEnd, aCh, cBPost+1);
	printf("%s\n", aCh);

	pWork->m_pAlloc->MOE_FREE(aCh);

	{
		ConsoleColorAmbit ccolamb;

		ccolamb.SetConsoleForegroundColor(GRFCCOL_FgIntenseWhite);

		printf("%*s%s%*s", cDigit, "", s_pChzMargin, (int)cBPre, "");

		ccolamb.SetConsoleForegroundColor(GRFCCOL_FgIntenseRed);

		for (int iCh = 0; iCh < moeMax<size_t>(1, cBSpan); ++iCh)
		{
			printf("^");
		}
	}
	printf("\n\n");
}

LexLookup::LexLookup(Workspace * pWork, const LexSpan & lexsp)
{
	m_istrFilename = lexsp.m_istrFilename;
	CalculateLinePosition(pWork, lexsp, &m_iLine, &m_iCodepoint);
}

LexLookup::LexLookup(Workspace * pWork, STNode * pStnod)
{
	m_istrFilename = pStnod->m_lexsp.m_istrFilename;
	CalculateLinePosition(pWork, pStnod->m_lexsp, &m_iLine, &m_iCodepoint);
}



Workspace::Workspace(Moe::Alloc * pAlloc, ErrorManager * pErrman)
:m_pAlloc(pAlloc)
,m_blistEntry(pAlloc, Moe::BK_Workspace)
,m_arypEntryChecked(pAlloc, Moe::BK_Workspace) 
//,m_arypValManaged(pAlloc, Moe::BK_WorkspaceVal, 0)
,m_arypGenmapManaged(pAlloc, Moe::BK_Workspace, 0)
,m_arypFile(pAlloc, Moe::BK_WorkspaceFile, 200)
,m_pChzObjectFilename(nullptr)
,m_pSymtab(nullptr)
,m_pTyper(nullptr)
,m_unset(pAlloc, Moe::BK_Workspace, 0)
,m_unsetTin(pAlloc, Moe::BK_Workspace, 0)
,m_pErrman(pErrman)
,m_cbFreePrev(-1)
,m_targetos(TARGETOS_Nil)
,m_optlevel(OPTLEVEL_Debug)
,m_grfunt(GRFUNT_Default)
{
	m_pErrman->SetWorkspace(this);

	for (int filek = FILEK_Min; filek < FILEK_Max; ++filek)
	{
		m_mpFilekPHashHvIPFile[filek] = MOE_NEW(m_pAlloc, HashHvIPFile) HashHvIPFile(pAlloc, Moe::BK_Workspace);
	}
}

void Workspace::AppendEntry(STNode * pStnod, SymbolTable * pSymtab)
{
	MOE_ASSERT(pStnod, "null entry point");
	WorkspaceEntry * pEntry = m_blistEntry.AppendNew();
	pEntry->m_pStnod = pStnod;
	pEntry->m_pSymtab = pSymtab;
}

Moe::CHash<HV, int> * Workspace::PHashHvIPFile(FILEK filek) 
{
	if (!MOE_FVERIFY(filek > FILEK_Nil && filek < FILEK_Max, "bad filek"))
		return nullptr;
	
	return m_mpFilekPHashHvIPFile[filek];
}

Workspace::File * Workspace::PFileEnsure(const Moe::InString istrFilename, FILEK filek)
{
	Moe::CHash<HV, int> * phashHvIPFile = PHashHvIPFile(filek);

	int * pipFile = nullptr;
	//BB - should check platform before using case-insensitive file lookup 
	INRES inres = phashHvIPFile->InresEnsureKey(Moe::HvFromPChzLowercase(istrFilename.m_pChz), &pipFile);
	if (inres == Moe::INRES_Inserted)
	{
		File * pFile = MOE_NEW(m_pAlloc, File) File(istrFilename, filek);
		*pipFile = (int)m_arypFile.C();
		m_arypFile.Append(pFile);

		pFile->m_istrFilename = istrFilename;
	}
	return m_arypFile[*pipFile];
}

Workspace::File * Workspace::PFileLookup(const char * pChzFile, FILEK filek)
{
	int filekMin = filek;
	int filekMax = filek + 1;
	if (filek == FILEK_Nil)
	{
		filekMin = FILEK_Min;
		filekMax = FILEK_Max;
	}

	//BB - should check platform before using case-insensitive file lookup 
	int hv = Moe::HvFromPChzLowercase(pChzFile);
	for (int filekIt = filekMin; filekIt < filekMax; ++filekIt)
	{
		Moe::CHash<HV, int> * phashHvIPFile = PHashHvIPFile((FILEK)filekIt);
		int * pipFile = phashHvIPFile->Lookup(hv);
		if (pipFile)
		{
			if (MOE_FVERIFY((*pipFile >= 0) & (*pipFile < (int)m_arypFile.C()), "bad file index"))
			{
				return m_arypFile[*pipFile];
			}
		}
	}

	return nullptr;
}

char * Workspace::PChzLoadFile(const Moe::InString & istrFilename, Moe::Alloc * pAlloc)
{
	LexSpan lexsp(istrFilename);
#if defined( _MSC_VER )
	FILE * pFile;
	fopen_s(&pFile, istrFilename.m_pChz, "rb");
#else
	FILE * pFile = fopen(istrFilename.m_pChz, "rb");
#endif
	if (!pFile)
	{
		EmitError(m_pErrman, lexsp, ERRID_FailedOpeningFile, "Failed opening file %s", istrFilename.m_pChz);
		return nullptr;
	}

	fseek(pFile, 0, SEEK_END);
	size_t cB = ftell(pFile);
	fseek(pFile, 0, SEEK_SET);

	char * pChzFile = (char *)pAlloc->MOE_ALLOC(cB + 1, 4);
	size_t cBRead = fread(pChzFile, 1, cB, pFile);
	fclose(pFile);

	if (cB != cBRead)
	{
		EmitError(m_pErrman, lexsp, ERRID_UnknownError, "Failed reading file %s", istrFilename.m_pChz);
		pAlloc->MOE_FREE(pChzFile);
		return nullptr;
	}

	pChzFile[cB] = '\0';
	return pChzFile;
}



void BeginWorkspace(Workspace * pWork)
{
	Alloc * pAlloc = pWork->m_pAlloc;

	pWork->m_arypEntryChecked.Clear();
	pWork->m_blistEntry.Clear();

#if MOEB_LATER
	MOE_ASSERT(pWork->m_arypValManaged.C() == 0, "Unexpected managed values in workspace");
#endif
	MOE_ASSERT(pWork->m_arypGenmapManaged.C() == 0, "Unexpected generic maps in workspace");

	pWork->m_arypFile.Clear();
	for (int filek = Workspace::FILEK_Min; filek < Workspace::FILEK_Max; ++filek)
	{
		pWork->m_mpFilekPHashHvIPFile[filek]->Clear(0);
	}
	pWork->m_cbFreePrev = pAlloc->CB();

	pWork->m_unset.Clear(0);
	pWork->m_unsetTin.Clear(0);
	
	pWork->m_pTyper = MOE_NEW(pAlloc, TypeRegistry) TypeRegistry(pAlloc);
	pWork->m_pSymtab = PSymtabNew(pAlloc, nullptr, IstrIntern("global"), pWork->m_pTyper, &pWork->m_unsetTin);
	pWork->m_pSymtab->AddBuiltInSymbols(pWork);
}



#ifdef MOEB_LATER
void BeginParse(Workspace * pWork, Lexer * pLex, const char * pChzIn, const char * pChzFilename)
{
	Alloc * pAlloc = pWork->m_pAlloc;
	ParseContext * pParctx = MOE_NEW(pAlloc, ParseContext) ParseContext(pAlloc, pWork);
	pWork->m_pParctx = pParctx;

	static const size_t cChStorage = 1024 * 8;
	char * aChStorage = (char *)pAlloc->MOE_ALLOC(cChStorage, 4);
	InitLexer(pLex, pChzIn, &pChzIn[CBChz(pChzIn)-1], aChStorage, cChStorage);

	if (pChzFilename)
	{
		pLex->m_pChzFilename = pChzFilename;
	}

	PushSymbolTable(pParctx, pWork->m_pSymtab);
}

void EndParse(Workspace * pWork, Lexer * pLex)
{
	Alloc * pAlloc = pWork->m_pAlloc;
	pAlloc->MOE_FREE(pLex->m_aChScratch);

	SymbolTable * pSymtabPop = PSymtabPop(pWork->m_pParctx);
	MOE_ASSERT(pSymtabPop == pWork->m_pSymtab, "symbol table push/pop mismatch");
	
	pAlloc->MOE_DELETE(pWork->m_pParctx);
	pWork->m_pParctx = nullptr;

	pWork->m_arypEntryChecked.EnsureSize(pWork->m_blistEntry.C());
}
#endif

void EndWorkspace(Workspace * pWork)
{
	Alloc * pAlloc = pWork->m_pAlloc;

	if (pWork->m_pSymtab)
	{
		SymbolTable * pSymtabIt = pWork->m_pSymtab;
		while (pSymtabIt)
		{
			SymbolTable * pSymtab = pSymtabIt;
			pSymtabIt = pSymtab->m_pSymtabNextManaged;
			pAlloc->MOE_DELETE(pSymtab);
		}

		pWork->m_pSymtab = nullptr;
	}

	BlockListEntry::CIterator iter(&pWork->m_blistEntry);
	while (WorkspaceEntry * pEntry = iter.Next())
	{
		CleanupStnodeRecursive(pAlloc, pEntry->m_pStnod);

		pAlloc->MOE_DELETE(pEntry->m_pStnod);
		pEntry->m_pStnod = nullptr;
		pEntry->m_pSymtab = nullptr;
	}

#ifdef MOEB_LATER
	auto ppValMac = pWork->m_arypValManaged.PMac();
	for (auto ppVal = pWork->m_arypValManaged.A(); ppVal != ppValMac; ++ppVal)
	{
		auto pVal = *ppVal;

		// I don't want vTable pointers in CIRValue, so faux-virtual destructor
		switch(pVal->m_valk)
		{
		case VALK_Procedure:
			{
				pAlloc->MOE_DELETE((CIRProcedure *)pVal);
				break;
			} break;
		default:
			pAlloc->MOE_DELETE(pVal);
			break;
		}
	}

	pWork->m_arypValManaged.Clear();

#endif
	for (auto ppGenmap = pWork->m_arypGenmapManaged.A(); ppGenmap != pWork->m_arypGenmapManaged.PMac(); ++ppGenmap)
	{
		(*ppGenmap)->Cleanup(pWork->m_pAlloc);
		pWork->m_pAlloc->MOE_DELETE(*ppGenmap);
	}
	pWork->m_arypGenmapManaged.Clear();

	pWork->m_blistEntry.Clear();
	pWork->m_arypEntryChecked.Clear();
	for (int filek = Workspace::FILEK_Min; filek < Workspace::FILEK_Max; ++filek)
	{
		pWork->m_mpFilekPHashHvIPFile[filek]->Clear(0);
	}

	pWork->m_pAlloc->MOE_DELETE(pWork->m_pTyper);
	pWork->m_pTyper = nullptr;

	pWork->m_unset.Clear(0);
	pWork->m_unsetTin.Clear(0);

	size_t cipFile = pWork->m_arypFile.C();
	for (size_t ipFile = 0; ipFile < cipFile; ++ipFile)
	{
		if (pWork->m_arypFile[ipFile])
		{
			pAlloc->MOE_DELETE(pWork->m_arypFile[ipFile]);
			pWork->m_arypFile[ipFile] = nullptr;
		}
	}
	pWork->m_arypFile.Clear();
	pWork->m_pErrman->Clear();

	if (pWork->m_pChzObjectFilename)
	{
		pWork->m_pAlloc->MOE_FREE((void*)pWork->m_pChzObjectFilename);
		pWork->m_pChzObjectFilename = nullptr;
	}

	size_t cbFreePost = pAlloc->CB();
	if (pWork->m_cbFreePrev != cbFreePost)
	{
		printf("\nWARNING: failed to free all bytes during compilation. (%zd -> %zd)\n", pWork->m_cbFreePrev, cbFreePost);
		printf("----------------------------------------------------------------------\n");
		pAlloc->PrintAllocations();
	}
}

void Workspace::CopyUnitTestFiles(Workspace * pWorkOther)
{
	File ** ppFileMax = pWorkOther->m_arypFile.PMac();
	for (auto ppFile = pWorkOther->m_arypFile.A(); ppFile != ppFileMax; ++ppFile)
	{
		auto pFileOld = *ppFile;
		if (pFileOld->m_filek != FILEK_UnitTest)
			continue; 

		auto pFileNew = PFileEnsure(pFileOld->m_istrFilename, pFileOld->m_filek);
		*pFileNew = *pFileOld;
	}
}

