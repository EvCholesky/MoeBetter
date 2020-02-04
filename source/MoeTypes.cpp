#include "MoeTypes.h" 

#include <algorithm>
#include <cstdarg>
#include <stdio.h>
#include <string.h>

#define STB_MALLOC_IMPLEMENTATION
#include "stb_malloc.h"

#ifdef MOE_TRACK_ALLOCATION
#include "MoeString.h"
#include "MoeArray.h"
#include "MoeHash.h"
#endif

namespace Moe
{

Alloc	g_allocCManaged;
bool	Alloc::s_fProgramIsShutdown = false;

const char * PChzFromBK(BK bk)
{
	static const char * s_mpBkPChz[] =
	{
#define BKDEF(x) #x,
		BK_DEF_LIST
#undef BKDEF
	};

	MOE_CASSERT(MOE_DIM(s_mpBkPChz) == BK_Max, "missing BK string");
	if (bk == BK_Nil)
		return "Nil";

	if ((bk < BK_Nil) | (bk >= BK_Max))
		return "Unknown BK";

	return s_mpBkPChz[bk];
}

struct AllocTracker //tag=altrac
{
public:
#ifdef MOE_TRACK_ALLOCATION
			AllocTracker()
			:m_aryEntry(BK_Core)
			,m_hashHvIentry()
				{ ; }

			AllocTracker(Alloc * pAlloc)
			:m_aryEntry(pAlloc, BK_Core)
			,m_hashHvIentry(pAlloc, BK_Nil)
				{ ; }
#endif 

	void	Clear()
			{
#ifdef MOE_TRACK_ALLOCATION
				m_aryEntry.Clear();
				m_hashHvIentry.Clear();
#endif
			}

	HV		HvFromFileLineBk(const char * pChzFile, int cLine, BK bk)
			{
#ifdef MOE_TRACK_ALLOCATION
				char aCh[2048];
				StringBuffer strbuf(aCh, MOE_DIM(aCh));
				FormatChz(&strbuf, "%s:%d%d", pChzFile, cLine, bk);
				return HvFromPChz(aCh);
#else
				return 0;
#endif
			}

	void	TrackAlloc(size_t cB, const char * pChzFile, int cLine, BK bk, int subKind, HV * pHv)
			{
#ifdef MOE_TRACK_ALLOCATION

				HV hv = HvFromFileLineBk(pChzFile, cLine, bk);
				*pHv = hv;

				int * piEntry;
				if (m_hashHvIentry.InresEnsureKey(hv, &piEntry) == INRES_AlreadyExisted)
				{
					SEntry * pEntry = &m_aryEntry[*piEntry];
					pEntry->m_cB += cB;
					pEntry->m_cBHighwater = moeMax(pEntry->m_cBHighwater, pEntry->m_cB);
					MOE_ASSERT(pEntry->m_cLine == cLine, "line mismatch in TrackAlloc");
					MOE_ASSERT(pEntry->m_bk == bk, "Block kind mismatch in TrackAlloc");
					++pEntry->m_cAllocations;
				}
				else
				{
					*piEntry = (int)m_aryEntry.C();
					SEntry * pEntry = m_aryEntry.AppendNew();
					pEntry->m_cB 	   	    = cB;
					pEntry->m_cBHighwater   = cB;
					pEntry->m_cAllocations  = 1;
					pEntry->m_pChzFile 	    = pChzFile;
					pEntry->m_cLine    	    = cLine;
					pEntry->m_bk			= bk;
					pEntry->m_subKind		= subKind;
				}
#endif // MOE_TRACK_ALLOCATION
			}

	void	TrackFree(size_t cB, HV * pHv)
			{
#ifdef MOE_TRACK_ALLOCATION
				if (*pHv)
				{
					int * piEntry = m_hashHvIentry.Lookup(*pHv);

					if (MOE_FASSERT(piEntry, "Failed to find allocation record during free"))
					{
						SEntry * pEntry = &m_aryEntry[*piEntry];
						pEntry->m_cB -= cB;
					}
				}
#endif // MOE_TRACK_ALLOCATION
			}

	void Print()
	{
#ifdef MOE_TRACK_ALLOCATION
		SEntry * pEntryMac = m_aryEntry.PMac();
		size_t cBTotal = 0;
		for (SEntry * pEntry = m_aryEntry.A(); pEntry != pEntryMac; ++pEntry)
		{
			if (pEntry->m_cB == 0)
				continue;
			if (pEntry->m_subKind >= 0)
			{
				printf("%zd / %zd\t\t %s BK(%s,%d) : %d\n", 
					pEntry->m_cB, pEntry->m_cBHighwater, pEntry->m_pChzFile, PChzFromBK(pEntry->m_bk), pEntry->m_subKind, pEntry->m_cLine);
			}
			else
			{
				printf("%zd / %zd\t\t %s BK(%s) : %d\n", 
					pEntry->m_cB, pEntry->m_cBHighwater, pEntry->m_pChzFile, PChzFromBK(pEntry->m_bk), pEntry->m_cLine);
			}
			cBTotal += pEntry->m_cB;
		}
		printf("%zd tracked\n", cBTotal);
#endif
	}

	struct SEntry // tag=entry
	{
		size_t			m_cB;
		size_t			m_cBHighwater;
		const char *	m_pChzFile;
		int				m_cAllocations;
		int				m_cLine;
		BK				m_bk;
		int				m_subKind;
	};

#ifdef MOE_TRACK_ALLOCATION
	// BB - replace with indexed set
	Moe::CDynAry<SEntry>	m_aryEntry;
	Moe::CHash<HV, int>		m_hashHvIentry;
#endif // MOE_TRACK_ALLOCATION
};


AllocTracker * PAltracCreate(Alloc * pAllocWork)
{
#ifdef MOE_TRACK_ALLOCATION
	AllocTracker * pAltrac = MOE_NEW(pAllocWork, AllocTracker) AllocTracker(pAllocWork);
	return pAltrac;
#else
	return nullptr;
#endif
}

void DeleteAltrac(Alloc * pAllocWork, AllocTracker * pAltrac)
{
#ifdef MOE_TRACK_ALLOCATION
	pAllocWork->MOE_DELETE(pAltrac);
#endif
}

void * STBM_CALLBACK EwcSystemAlloc(void * pUserContext, size_t cBRequested, size_t * pCbProvided)
{
	void * pReturn = malloc(cBRequested);
	*pCbProvided = pReturn ? cBRequested : 0;
	return pReturn;
}

void STBM_CALLBACK EwcSystemFree(void * pUserContext, void *p)
{
	free(p);
}

void Alloc::VerifyHeap()
{
	// doesn't do anything unless STBM_DEBUGCHECK is set
	STBM__CHECK_LOCKED(m_pStbheap);
}

void Alloc::TrackAlloc(size_t cB, const char * pChzFile, int cLine, BK bk, int subKind, HV * pHv)
{
	m_pAltrac->TrackAlloc(cB, pChzFile, cLine, bk, subKind, pHv);
}

void Alloc::TrackFree(size_t cB, HV * pHv)
{
	m_pAltrac->TrackFree(cB, pHv);
}

void Alloc::PrintAllocations()
{
	if (m_pAltrac)
		m_pAltrac->Print();
}

void AssertHandler(const char* pChzFile, u32 line, const char* pChzCondition, const char* pChzMessage, ... )
{
	printf("Assertion failed: \"%s\" at %s:%u\n", pChzCondition, pChzFile, line);

	if (pChzMessage)
	{
		va_list ap;
		va_start(ap, pChzMessage);
		vprintf(pChzMessage, ap);
		printf("\n");
	}
}

void FillAB(u8 b, void * pDest, size_t cB)
{
	memset(pDest, b, cB);
}

void ZeroAB(void * pDest, size_t cB)
{
	memset(pDest, 0, cB);
}

void CopyAB(const void * pSource, void * pDest, size_t cB)
{
	memcpy(pDest, pSource, cB);
}

bool FAreSameAB(const void * aB0, void * aB1, size_t cB)
{
	return memcmp(aB0, aB1, cB) == 0;
}

void AppendChz(StringBuffer * pStrbuf, const char *pChzSource)
{
	if (!MOE_FVERIFY(pChzSource && FIsValid(*pStrbuf), "Null pointer passed to CCoCopy"))
		return;

	char * pChzDest = pStrbuf->m_pChzAppend;
	char * pChzDestEnd = &pStrbuf->m_pChzBegin[pStrbuf->m_cBMax-1];
	for ( ; (*pChzSource != '\0') & (pChzDest != pChzDestEnd); ++pChzSource, ++pChzDest)
	{
		*pChzDest = *pChzSource;
	}

	pStrbuf->m_pChzAppend = pChzDest;
	EnsureTerminated(pStrbuf, '\0');
}

void AppendToCch(StringBuffer * pStrbuf, char ch, size_t cChDesired)
{
	auto cChCur = CCh(pStrbuf->m_pChzBegin);
	if (cChCur >= cChDesired)
		return;

	size_t dCh = cChDesired -  cChCur;

	char * pChzDest = pStrbuf->m_pChzAppend;
	char * pChzDestEnd = &pStrbuf->m_pChzBegin[pStrbuf->m_cBMax-1];
	char * pChzDestDesired = &pChzDest[dCh];
	for ( ; (pChzDest != pChzDestDesired) & (pChzDest != pChzDestEnd); ++pChzDest)
	{
		*pChzDest = ch;
	}

	pStrbuf->m_pChzAppend = pChzDest;
	EnsureTerminated(pStrbuf, '\0');
}

static inline bool FIsBasicChar(char ch)	{ return (ch & 0x80) == 0;}
static inline bool FIsStarterChar(char ch)	{ return (ch & 0xC0) == 0xC0;}

void EnsureTerminated(StringBuffer * pStrbuf, char ch)
{
	size_t iB = pStrbuf->m_pChzAppend - pStrbuf->m_pChzBegin;

	if (pStrbuf->m_cBMax <= 0)
		return;

	iB = moeMin(iB, pStrbuf->m_cBMax -1);

	auto pChzEnd = &pStrbuf->m_pChzBegin[iB];
	auto pChzBackup = pChzEnd; // - 1;	// skip the spot our terminator will go

	while (pChzBackup != pStrbuf->m_pChzBegin)
	{
		--pChzBackup;
		if (FIsBasicChar(*pChzBackup) || FIsStarterChar(*pChzBackup))
			break;
	}

	size_t cBBackup;
	if ((*pChzBackup & 0xF8) == 0xF0)		cBBackup = 4;
	else if ((*pChzBackup & 0xF0) == 0xE0)	cBBackup = 3;
	else if ((*pChzBackup & 0xE0) == 0xC0)	cBBackup = 2;
	else									cBBackup = 1;

	if (cBBackup > size_t(pChzEnd - pChzBackup))
	{
		*pChzBackup = ch;
	}
	else
	{
		*pChzEnd = ch;
	}
}

size_t CBFree(const StringBuffer & strbuf)
{
	return strbuf.m_cBMax - (strbuf.m_pChzAppend - strbuf.m_pChzBegin) - 1; // -1 because pChzAppend points at the null term, need to count it.
}

size_t	CBCopyChz(const char * pChzSource, char * aCoDest, size_t cBDest)
{
	StringBuffer strbuf(aCoDest, cBDest);
	AppendChz(&strbuf, pChzSource);
	return strbuf.m_pChzAppend - strbuf.m_pChzBegin + 1;
}

size_t CChConstructFilename(const char * pChzFilenameIn, const char * pChzExtension, char * pChzFilenameOut, size_t cChOutMax)
{
	// remove the last extension (if one exists) and replace it with the supplied one.

	char * pChzPeriod = nullptr;
	char * pChzOut = pChzFilenameOut;
	char * pChzOutMax = &pChzFilenameOut[cChOutMax];
	const char * pChIt = pChzFilenameIn;
	for ( ; *pChIt != '\0' && pChzOut != pChzOutMax; ++pChIt)
	{
		if (*pChIt == '.')
		{
			pChzPeriod = pChzOut;
		}

		*pChzOut++ = *pChIt;
	}

	if (pChzPeriod)
		pChzOut = pChzPeriod;

	pChIt = pChzExtension; 
	for ( ; *pChIt != '\0' && pChzOut != pChzOutMax; ++pChIt)
	{
		*pChzOut++ = *pChIt;
	}

	*pChzOut++ = '\0';
	return pChzOut - pChzFilenameOut;
}

const char * PChzSkipUnicodeBOM(const char * pChzFile)
{
	// utf8 BOM
	const u8 * pBFile = (const u8 *)pChzFile;
	if (pBFile[0] == 0xEF && pBFile[1] == 0xBB && pBFile[2] == 0xBF)
	{
		pChzFile += 3;
	}
	return pChzFile;
}

void FormatChz(StringBuffer * pStrbuf, const char * pChzFormat, ...)
{
	ptrdiff_t cBMax = &pStrbuf->m_pChzBegin[pStrbuf->m_cBMax] - pStrbuf->m_pChzAppend;
	if (cBMax > 1)
	{
		va_list ap;
		va_start(ap, pChzFormat);
#ifdef WIN32
		ptrdiff_t cCh = vsnprintf_s(pStrbuf->m_pChzAppend, cBMax, _TRUNCATE, pChzFormat, ap);
#else
		ptrdiff_t cCh = vsnprintf(pStrbuf->m_pChzAppend, cBMax, pChzFormat, ap);
		pStrbuf->m_pChzAppend[cBMax-1] = 0;
#endif
		va_end(ap);

		if (cCh == -1)
		{
			cCh = cBMax-1;
		}
		pStrbuf->m_pChzAppend += cCh;
	}

	// handle truncation within utf8 multibyte char
	EnsureTerminated(pStrbuf, '\0');
}

size_t CBChz(const char * pChz)
{
	// bytes needed for this string (including the null terminator)
	if (!pChz)
		return 0;

	auto pChzIt = pChz;
	while (*pChzIt != '\0')
		++pChzIt;

	++pChzIt;
	return pChzIt - pChz;
}

size_t CCh(const char * pChz)
{
	// characters included in this string (excluding the null terminator)

	if (!pChz)
		return 0;
	return strlen(pChz);
}

size_t CCodepoint(const char * pChz) 
{
	// utf-8 codepoints included in this string (excluding the null terminator)
	size_t cCodepoint = 0;
	while (*pChz != '\0')
	{
		u8 codepoint = *pChz;
		if ((0xf8 & codepoint) == 0xf0)			{ pChz += 4; }
		else if ((0xf0 & codepoint) == 0xe0)	{ pChz += 3; }
		else if ((0xc0 & codepoint) == 0xc0)	{ pChz += 2; }
		else									{ pChz += 1; }

		++cCodepoint;
	}

	return cCodepoint;
}

size_t CBFromChz(const char * pChz, size_t cCodepoint) 
{
	// bytes needed for this utf-8 string, up to a given codepoint (including the null terminator)
	size_t iCodepoint = 0;
	auto pChzIt = pChz;
	while ((*pChzIt != '\0') & (iCodepoint < cCodepoint))
	{
		u8 codepoint = *pChzIt;
		if ((0xf8 & codepoint) == 0xf0)			{ pChzIt += 4; }
		else if ((0xf0 & codepoint) == 0xe0)	{ pChzIt += 3; }
		else if ((0xc0 & codepoint) == 0xc0)	{ pChzIt += 2; }
		else									{ pChzIt += 1; }

		++iCodepoint;
	}
	++pChzIt;

	return pChzIt - pChz;
}

void ConcatPChz(const char* pChzA, const char * pChzB, char * pChOut, size_t cChOutMax)
{
	char* pChzOutIt = pChOut;
	char* pChzOutEnd = &pChOut[cChOutMax-1];

	const char* pChzInIt = pChzA;
	while((*pChzInIt != '\0') & (pChzOutIt != pChzOutEnd))
		*pChzOutIt++ = *pChzInIt++;

	pChzInIt = pChzB;
	while((*pChzInIt != '\0') & (pChzOutIt != pChzOutEnd))
		*pChzOutIt++ = *pChzInIt++;

	*pChzOutIt = '\0';
}

int NCmpChz(const char * pChzA, const char * pChzB)
{
	if ((pChzA == nullptr) | (pChzB == nullptr))
	{
		if (pChzA == pChzB)
			return 0;
		
		return (pChzA == nullptr) ? -1 : 1;
	}

	while ((*pChzA != '\0') | (*pChzB != '\0'))
	{
		auto chA = *pChzA;
		auto chB = *pChzB;
		if (chA < chB)
			return -1;
		else if (chA > chB)
			return 1;

		++pChzA;
		++pChzB;
	}

	return 0;
}

bool FIsEmptyString(const char * pChzA)
{
	if (!pChzA)
		return true;
	return pChzA[0] == '\0';
}

bool FAreChzEqual(const char * pChzA, const char * pChzB)
{
	return NCmpChz(pChzA, pChzB) == 0;
}

int NCmpChz(const char * pChzA, const char * pChzB, size_t cCodepoint)
{
	if ((pChzA == nullptr) | (pChzB == nullptr))
	{
		if (pChzA == pChzB)
			return 0;
		
		return (pChzA == nullptr) ? -1 : 1;
	}

	size_t iCodepoint = 0;
	while ((iCodepoint < cCodepoint) & ((*pChzA != '\0') | (*pChzB != '\0')))
	{
		auto chA = *pChzA;
		auto chB = *pChzB;
		if (chA < chB)
			return -1;
		else if (chA > chB)
			return 1;

		++pChzA;
		++pChzB;

		if ((chA & 0xC0) != 0x80)	// only count the first bytes of each codepoint
			++iCodepoint;
	}

	return 0;
}

bool FAreChzEqual(const char * pChzA, const char * pChzB, size_t cCodepoint)
{
	return NCmpChz(pChzA, pChzB, cCodepoint) == 0;
}

void ConvertChToWch(const char* pChz, size_t cWchMax, WChar * pWchz)
{
	WChar * pWchEnd = &pWchz[cWchMax-1];
	for ( ; (pWchz != pWchEnd) & (*pChz != '\0'); ++pWchz, ++pChz)
	{
		*pWchz = *pChz;
	}
	*pWchz = 0;
}

bool FPChzContainsChar(const char * pChz, char ch)
{
	while (*pChz != '\0')
	{
		if (*pChz == ch)
			return true;
		++pChz;
	}
	return false;
}

void ReplaceChars(const char * pChSrc, size_t cCh, const char * pChzRemove, char chFill, char * pChDst)
{
	const char * pChSrcEnd = &pChSrc[cCh];
	for (; pChSrc != pChSrcEnd; ++pChSrc, ++pChDst)
	{
		char chSrc = *pChSrc;
		*pChDst = FPChzContainsChar(pChzRemove, chSrc) ? chFill : chSrc;
	}
}

StringEditBuffer::~StringEditBuffer()
{
	Resize(0,0,0);
}

void StringEditBuffer::PrependCh(const char * pChz, size_t cB)
{
	auto cCh = cB - 1;
	size_t cBPrefix = m_pChzBegin - m_pChzMin;
	if (cBPrefix < cCh)
	{
		Resize(cCh + s_cChPrefixPad, moeMax<size_t>(s_cChPrefixPad, CB()), m_pChzMax - m_pChzAppend);
	}

	m_pChzBegin -= cCh;
	CopyAB(pChz, m_pChzBegin, cCh);
}

void StringEditBuffer::PrependChz(const char * pChz)
{
	size_t cBChz = CBChz(pChz);
	PrependCh(pChz, cBChz);
}

void StringEditBuffer::AppendCh(const char * pChz, size_t cB)
{
	size_t cBAvail = m_pChzMax - m_pChzAppend;
	if (cBAvail < cB) // +1 for null terminator
	{
		Resize(s_cChPrefixPad, moeMax<size_t>(s_cChPrefixPad, CB()), cB + s_cChPrefixPad);
	}

	CopyAB(pChz, m_pChzAppend, cB);

	m_pChzAppend += cB-1;
	MOE_ASSERT((uintptr_t(m_pChzAppend) <= uintptr_t(m_pChzMax)), "seb overflow"); 
}

void StringEditBuffer::AppendChz(const char * pChz)
{
	size_t cBChz = CBChz(pChz);
	AppendCh(pChz, cBChz);
}

void StringEditBuffer::Clear()
{
	if (!m_pChzMin)
		return;

	m_pChzBegin = m_pChzMin + s_cChPrefixPad;
	m_pChzAppend = m_pChzBegin;
	*m_pChzAppend = '\0';
}

void StringEditBuffer::Resize(size_t cBPrefix, size_t cBUsed, size_t cBPostfix)
{
	if (!MOE_FVERIFY(m_pAlloc, "missing allocator"))
		return;

	auto pChzMinOld = m_pChzMin;
	auto pChzBeginOld = m_pChzBegin;
	auto cBOld = CB();

	size_t cB = cBPrefix + cBUsed + cBPostfix;

	if (!cB)
	{
		m_pChzMin = nullptr;
		m_pChzBegin = nullptr;
		m_pChzAppend = nullptr;
		m_pChzMax = nullptr;

		if (pChzMinOld)
		{
			m_pAlloc->MOE_DELETE(pChzMinOld);
		}
		return;
	}

	m_pChzMin = (char*)m_pAlloc->MOE_ALLOC(sizeof(char) * cB, MOE_ALIGN_OF(char));
	m_pChzBegin = m_pChzMin + cBPrefix;
	m_pChzAppend = m_pChzBegin;
	m_pChzMax = m_pChzMin + cB;
	
	if (pChzBeginOld)
	{
		cBOld = moeMin<size_t>(cBOld, cBUsed);
		CopyAB(pChzBeginOld, m_pChzBegin, cBOld);

		m_pChzAppend = m_pChzBegin + (cBOld -1); // -1 to point at the null terminator.
		m_pAlloc->MOE_DELETE(pChzMinOld);
	}

	*m_pChzAppend = '\0';
}

char * StringEditBuffer::PChzAllocateCopy(Alloc * pAlloc)
{
	auto cBNew = CB();
	const char * pChzSource = PChz();

	if (!cBNew)
	{
		cBNew = 1;
		pChzSource = "";
	}
	
	char * pChzNew = (char*)pAlloc->MOE_ALLOC(cBNew, sizeof(char));

	CBCopyChz(pChzSource, pChzNew, cBNew);
	return pChzNew;
}

void DoNothing()
	{ ; }

} // namespace EWC

using namespace Moe;

namespace Puny
{
	static const u32 s_nBase = 36;
	static const int s_nTMin = 1;
	static const int s_nTMax = 26;
	static const int s_nSkew = 38;
	static const int s_nDamp = 700;
	static const u32 s_nBiasInitial = 72;
	static const char s_chDelimiter = '_';
	static const int s_cWrapInitial = 0x80;
	static const u32 s_nU32Max = 0xFFFFFFFF;

	ptrdiff_t NAdaptBias(ptrdiff_t nDelta, ptrdiff_t cPoint, bool fFirstTime)
	{
		if (fFirstTime) nDelta = nDelta / s_nDamp;
		else			nDelta = nDelta / 2;

		nDelta += nDelta / cPoint;
		int nK = 0;

		while (nDelta > ((s_nBase - s_nTMin) * s_nTMax) / 2)
		{
			nDelta = nDelta / (s_nBase - s_nTMin);
			nK += s_nBase;
		}

		return nK + (((s_nBase - s_nTMin + 1) * nDelta) / (nDelta + s_nSkew));
	}

u32 NReadCodepoint(const char ** ppChz, bool * pFIsValid)
{
	const char * pChz = *ppChz;
	u32 ch = (u32)*pChz;
	u32 ch1, ch2, ch3;
	if ((0xf8 & ch) == 0xf0)		
	{ 
		*pFIsValid = ((pChz[1] & 0xC0) == 0x80) & ((pChz[2] & 0xC0) == 0x80) & ((pChz[3] & 0xC0) == 0x80);
		*ppChz += 4; 
		ch = (ch & 0x07);		ch1 = u32(pChz[1]) & 0x3F;			ch2 = u32(pChz[2]) & 0x3F;		ch3 = u32(pChz[3]) & 0x3F;
		return (ch << 18) | (ch1 << 12) | (ch2 << 6) | ch3;
	}
	else if ((0xf0 & ch) == 0xe0)
	{ 
		*pFIsValid = ((pChz[1] & 0xC0) == 0x80) & ((pChz[2] & 0xC0) == 0x80);
		*ppChz += 3; 
		ch = (ch & 0x0F);		ch1 = u32(pChz[1]) & 0x3F;			ch2 = u32(pChz[2]) & 0x3F;
		return (ch << 12) | (ch1 << 6) | ch2;
	}
	else if ((0xE0 & ch) == 0xC0)
	{ 
		*pFIsValid = (pChz[1] & 0xC0) == 0x80;
		*ppChz += 2; 
		ch = (ch & 0x1F);		ch1 = u8(pChz[1]) & 0x3F;
		return (ch << 6) | ch1;
	}
	else
	{
		*pFIsValid = (pChz[0] & 0x80) == 0;
		*ppChz += 1;
		return ch;
	}
}

bool FIsValidUtf8(const char * pChz)
{
	bool fIsValid;
	while (*pChz != '\0')
	{
		(void) NReadCodepoint(&pChz, &fIsValid);
		if (!fIsValid)
			return false;
	}

	return true;
}

bool FTryConvertUtf8ToUcs4(const char * pChzIn, char32_t * pWchzOut, char32_t * pWchOutMax)
{
	// BB - not checking for nullptrs, or zero character destination string

	bool fIsValid;
	const char * pChz = pChzIn;

	while (*pChz != '\0' && pWchzOut != pWchOutMax)
	{
		*pWchzOut++ = NReadCodepoint(&pChz, &fIsValid);
		if (!fIsValid)
			return false;
	}

	if (pWchzOut == pWchOutMax)
	{
		*(pWchOutMax-1) = 0;
		return false;
	}
	*pWchzOut = 0;
	return true;
}

bool FTryConvertUcs4ToUtf8(const u32 * pWchzIn, u8 * pChzOut, u8 * pChzOutMax)
{
	// BB - not checking for nullptrs, or zero character destination string

	for (const u32 * pWchz = pWchzIn; *pWchz != 0; ++pWchz)
	{
		u32 wch = *pWchz;
		auto cBOutLeft = (pChzOutMax - pChzOut);
		if (wch < 0x80)
		{
			if (cBOutLeft < 1) break;
			*pChzOut++ = (u8)wch;
		}
		else if (wch < 0x7FF)
		{
			if (cBOutLeft < 2) break;
			*pChzOut++ = u8((wch >> 6) | 0xC0);
			*pChzOut++ = u8(wch & 0x3F) | 0x80;
		}
		else if (wch < 0xFFFF)
		{
			if (cBOutLeft < 3) break;
			*pChzOut++ = u8(wch >> 12) | 0xE0;
			*pChzOut++ = (u8(wch >> 6) & 0x3F) | 0x80;
			*pChzOut++ = u8(wch & 0x3F) | 0x80;
		}
		else if (wch < 0x10FFFF)
		{
			if (cBOutLeft < 4) break;
			*pChzOut++ = u8((wch >> 18) | 0xF0); 
			*pChzOut++ = (u8(wch >> 12) & 0x3F) | 0x80;
			*pChzOut++ = (u8(wch >> 6) & 0x3F) | 0x80;
			*pChzOut++ = u8(wch & 0x3F) | 0x80;
		}
		else
			return false;

	}

	if (pChzOut == pChzOutMax)
	{
		*(pChzOutMax-1) = '\0';
		return false;
	}
	*pChzOut = '\0';
	return true;
}

enum PUNYRET
{
	PUNYRET_Success,
	PUNYRET_BadInput,			// input is malformed utf8
	PUNYRET_OutputTooLong,
	PUNYRET_Overflow,			// Input needs wider integers to process
};

static inline char ChEncodeDigit(u32 d)
{
  //  0..25 map to ASCII a..z or A..Z,   26..35 map to ASCII 0..9 
  return d + 22 + 75 * (d < 26);
}

PUNYRET PunyretEncode(const char * pChzInput, char * pChzOut, size_t cBMaxOut)
{
	const char * pChz = pChzInput;
	size_t cBInput = Moe::CBChz(pChzInput);

	auto aNWork = (int *)alloca(cBInput * sizeof(int));		// UCS4 version of the input string
	auto aNExtSorted = (int*)alloca(cBInput * sizeof(int));	// sorted copy of all extended chars

	// copy and count the basic codepoints
	int * pN = aNWork;
	int * pNExt = aNExtSorted;

	char * pChzDest = pChzOut;
	char * pChzDestMax = &pChzOut[cBMaxOut];

	u32 nCextMin = 0xFFFFFFFF;
	bool fIsValid;
	bool fFoundDelimiter = false;
	while (*pChz != '\0')
	{
		fFoundDelimiter |= *pChz == s_chDelimiter;
		u32 nCodepoint = NReadCodepoint(&pChz, &fIsValid);
		*pN++ = nCodepoint;

		if (nCodepoint > 0x80)
		{
			if (nCextMin > nCodepoint)
				nCextMin = nCodepoint;
			*pNExt++ = nCodepoint;
		}
		else
		{
			*pChzDest++ = (u8)nCodepoint;
			if (pChzDest == pChzDestMax)
			{
				pChzOut[cBMaxOut-1] = '\0';
				return PUNYRET_OutputTooLong;
			}
		}
	}
	ptrdiff_t cBasic = pChzDest - pChzOut;
	ptrdiff_t cHandled = pChzDest - pChzOut;

	// BB - would like to change this to only add a delimiter when we have extended characters (or another delimiter)
	if ((pNExt - aNExtSorted) != 0 || fFoundDelimiter)
	{
		*pChzDest++ = s_chDelimiter;
	}

	if (pNExt == aNExtSorted)
	{
		*pChzDest++ = '\0';
		return PUNYRET_Success;
	}

	std::sort(aNExtSorted, pNExt);

	ptrdiff_t nDelta = 0;
	ptrdiff_t cWrap = s_cWrapInitial;
	ptrdiff_t nBias = s_nBiasInitial;

	int * pNWorkMax = pN;
	ptrdiff_t cCodepoint = pNWorkMax - aNWork;

	int iSorted = 0;
	while (cHandled < cCodepoint)
	{
		int nM = aNExtSorted[iSorted++];
		while (aNExtSorted[iSorted] == nM)
			++iSorted;
		nDelta += (nM - cWrap) * (cHandled + 1);

		cWrap = nM;
		for (int * pN = aNWork; pN != pNWorkMax; ++pN)
		{
			int nC = *pN;
			if (nC < cWrap)
			{
				++nDelta;
				if(nDelta == 0)
					return PUNYRET_Overflow;
			}
			else if (nC == cWrap)
			{
				ptrdiff_t nQ = nDelta;
				for (int nK = s_nBase; 1; nK += s_nBase)
				{
					int t = Moe::moeMax(Moe::moeMin(int(nK - nBias), s_nTMax), s_nTMin);
					if (nQ < t)
						break;

					*pChzDest++ = ChEncodeDigit(t + (nQ - t) % (s_nBase - t));
					nQ = (nQ - t) / (s_nBase - t);
				}
				
				*pChzDest++ = ChEncodeDigit(u32(nQ));
		        nBias = NAdaptBias(nDelta, cHandled + 1, cHandled == cBasic);
				nDelta = 0;
				++cHandled;
			}
		}
		++nDelta;
		++cWrap;
	}

	*pChzDest = '\0';
	return PUNYRET_Success;
}

// NDecodeDigit(cp) returns the numeric value of a basic code point (for use in representing integers) 
//  in the range 0 to base-1, or base if cp does not represent a value.

static u32 NDecodeDigit(u32 codepoint)
{
	if (codepoint - 48 < 10)	return codepoint - 22;
	if (codepoint - 65 < 26)	return codepoint - 65;
	if (codepoint - 97 < 26)	return codepoint - 97;
	return s_nBase;
}

PUNYRET PunyretDecode(const char * pChzInput, char * pChzOut, size_t cBOutMax)
{
	const char * pChzDelimiter = nullptr;
	const char * pChz = pChzInput;

	 // find the last delimiter
	while (*pChz != '\0')
	{
		if (*pChz == s_chDelimiter)
			pChzDelimiter = pChz;
		++pChz;
	}
	const char * pChzInputMax = pChz;

	size_t cBInput = (pChz - pChzInput) + 1;
	u32 * aNOutput = (u32 *)alloca(sizeof(u32) * cBInput);
	u32 * pNOutput = aNOutput;
	//if (cBOutMax <= cBInput)
	//	return PUNYRET_OutputTooLong;

	pChz = pChzInput;

	bool fAllBasic = pChzDelimiter == nullptr;
	if (fAllBasic)
	{
		pChzDelimiter = pChzInputMax;
	}

	while (pChz != pChzDelimiter)
	{
		unsigned char ch = *pChz;
		if (ch >= 0x80)
			return PUNYRET_BadInput;
		*pNOutput++ = *pChz++;
	}

	if (!fAllBasic)	// skip the delimiter
		++pChz;

	ptrdiff_t cWrap = s_cWrapInitial; // aka 'n'
	u32 iPrev;
	u32 iN = 0;
	size_t nBias = s_nBiasInitial;
	u32 nWeight;
	u32 nK;
	u32 t;

	size_t cNOut = pNOutput - aNOutput;
	for ( ; pChz != pChzInputMax; ++cNOut)
	{
	    // Decode a generalized variable-length integer into delta, which gets added to i.  The overflow checking is 
		//  easier if we increase i as we go, then subtract off its starting value at the end to obtain delta.
		iPrev = iN;
		nWeight = 1;
		for (nK = s_nBase; ; nK += s_nBase)
		{
			if (pChz == pChzInputMax)
				return PUNYRET_BadInput;
			u32 nDigit = NDecodeDigit(*pChz++);

			if (nDigit >= s_nBase)
				return PUNYRET_BadInput;
			if (nDigit > (s_nU32Max  -1) / nWeight) 
				return PUNYRET_Overflow;
			iN += nDigit * nWeight;

			if (nK <= nBias)				t = s_nTMin;
			else if (nK >= nBias + s_nTMax) t = s_nTMax;
			else							t = u32(nK - nBias);

			if (nDigit < t)
				break;

			if (nWeight > s_nU32Max  / (s_nBase - t)) 
				return PUNYRET_Overflow;
			nWeight *= (s_nBase - t);
		}

	    nBias = NAdaptBias(iN - iPrev, cNOut + 1, iPrev == 0);

	    // iN was supposed to wrap around from out+1 to 0, incrementing n each time, so we'll fix that now:

		ptrdiff_t dWrap = iN / (cNOut + 1);
	    if (dWrap > s_nU32Max  - cWrap) 
			return PUNYRET_Overflow;
	    cWrap += dWrap;
	    iN %= (cNOut + 1);

	    // Insert n at position i of the output:

	    if (cNOut >= cBOutMax) 
			return PUNYRET_OutputTooLong;
	    memmove(aNOutput + iN + 1, aNOutput + iN, (cNOut - iN) * sizeof(aNOutput[0]));
		aNOutput[iN++] = u32(cWrap);
	}

	aNOutput[cNOut] = 0;
	if (FTryConvertUcs4ToUtf8(aNOutput, (u8 *)pChzOut, (u8 *)&pChzOut[cBOutMax]))
		return PUNYRET_Success;
	return PUNYRET_BadInput;
}

} // namespace Puny

int NCmpWchz(const char32_t * pWchzA, const char32_t * pWchzB)
{
	if ((pWchzA == nullptr) | (pWchzB == nullptr))
	{
		if (pWchzA == pWchzB)
			return 0;
		
		return (pWchzA == nullptr) ? -1 : 1;
	}

	while ((*pWchzA != '\0') | (*pWchzB != '\0'))
	{
		auto chA = *pWchzA;
		auto chB = *pWchzB;
		if (chA < chB)
			return -1;
		else if (chA > chB)
			return 1;

		++pWchzA;
		++pWchzB;
	}

	return 0;
}

Moe::InString IstrPunyEncode(const char * pChz)
{
	// BB - there's a lot of stupid copying and counting going on here...

	// I don't have a great way to estimate the punycoded string length from the utf8 length - let's just say
	//  it won't be a lot longer than the original
	size_t cBMaxPuny = Moe::CBChz(pChz) * sizeof(char) + 512;
	char * pChzPuny = (char *)alloca(cBMaxPuny);

	auto punyret = Puny::PunyretEncode(pChz, pChzPuny, cBMaxPuny);
	MOE_ASSERT(punyret == Puny::PUNYRET_Success, "punycoding failed");
	return IstrIntern(pChzPuny);
}

Moe::InString IstrPunyDecode(const char * pChz)
{
	// BB - there's a lot of stupid copying and counting going on here...

	// I don't have a great way to estimate the utf8 string length from the punycoded length - let's just say
	//  it won't be a lot longer than the original
	size_t cBMaxPuny = Moe::CBChz(pChz) * sizeof(char) + 512;
	char * pChzPuny = (char *)alloca(cBMaxPuny);

	auto punyret = Puny::PunyretDecode(pChz, pChzPuny, cBMaxPuny);
	MOE_ASSERT(punyret == Puny::PUNYRET_Success, "punycode decoding failed");
	return IstrIntern(pChzPuny);
}

bool FAreWchzEqual(const char32_t * pWchzA, const char32_t * pWchzB)
{
	return NCmpWchz(pWchzA, pWchzB) == 0;
}

void TestUtf8()
{
	char aCh[256];

	const char * pChz;
	const char * pChzExpected;
	Moe::StringBuffer strbuf;

	pChz = u8"いろはに";
	pChzExpected = u8"いろは";
	MOE_ASSERT(Moe::CBChz(pChz) == 13, "bad byte count");
	MOE_ASSERT(Moe::CCodepoint(pChz) == 4, "bad codepoint count");

	strbuf = Moe::StringBuffer(aCh, 13);
	AppendChz(&strbuf, pChz);
	MOE_ASSERT(Moe::FAreChzEqual(strbuf.m_pChzBegin, pChz), "bad utf8 copy");

	strbuf = Moe::StringBuffer(aCh, 11);
	AppendChz(&strbuf, pChz);
	MOE_ASSERT(Moe::FAreChzEqual(strbuf.m_pChzBegin, pChzExpected), "bad utf8 copy");

	strbuf = Moe::StringBuffer(aCh, 11);
	FormatChz(&strbuf, "%s", pChz);
	MOE_ASSERT(Moe::FAreChzEqual(strbuf.m_pChzBegin, pChzExpected), "bad utf8 copy");

	pChz = u8"ößöß";
	pChzExpected = u8"ößö";
	MOE_ASSERT(Moe::CBChz(pChz) == 9, "bad byte count");
	MOE_ASSERT(Moe::CCodepoint(pChz) == 4, "bad codepoint count");

	strbuf = Moe::StringBuffer(aCh, 7);
	AppendChz(&strbuf, pChz);
	MOE_ASSERT(Moe::FAreChzEqual(strbuf.m_pChzBegin, pChzExpected), "bad utf8 copy");
}

bool FTestUnicode()
{
	TestUtf8();

	struct STestStrings // testr
	{
		const char *		m_pChzUtf8;
		const char32_t *	m_pWchzTest;
	};

	#define MAKE_TESTR(str) { u8##str, U##str }
	STestStrings s_aTestr[] = 
	{
		MAKE_TESTR("ascii"),
		MAKE_TESTR("mixedいろはにほ"),
		MAKE_TESTR("いろはにほmixed"),
		MAKE_TESTR("いmろiはxにeほd"),
		MAKE_TESTR("_delimiter"),
		MAKE_TESTR("delimiter_"),
		MAKE_TESTR("d_e_l_i_m_i_t_e_r"),
		MAKE_TESTR("Quizdeltagerne spiste jordbær med fløde, mens cirkusklovnen Wolther spillede på xylofon."), // danis
		MAKE_TESTR("Le cœur déçu mais l'âme plutôt naïve, Louÿs rêva de crapaüter en canoë au delà des îles, près du mälström où brûlent les novæ."), // French
		MAKE_TESTR("D'fhuascail Íosa, Úrmhac na hÓighe Beannaithe, pór Éava agus Ádhaimh"),	// Gaelic
		MAKE_TESTR("Falsches Üben von Xylophonmusik quält jeden größeren Zwerg"),	// German
		MAKE_TESTR("Γαζέες καὶ μυρτιὲς δὲν θὰ βρῶ πιὰ στὸ χρυσαφὶ ξέφωτο"),		// Greek
		MAKE_TESTR("Kæmi ný öxi hér ykist þjófum nú bæði víl og ádrepa"),		// Icelandic
		MAKE_TESTR("いろはにほへとちりぬるをわかよたれそつねならむうゐのおくやまけふこえてあさきゆめみしゑひもせす"),	// Japanese
		MAKE_TESTR("? דג סקרן שט בים מאוכזב ולפתע מצא לו חברה איך הקליטה"), // hebrew
		MAKE_TESTR("Árvíztűrő tükörfúrógép"), // Hungarian
		MAKE_TESTR("В чащах юга жил бы цитрус? Да, но фальшивый экземпляр!"),		// Russian
		MAKE_TESTR("El pingüino Wenceslao hizo kilómetros bajo exhaustiva lluvia y frío, añoraba a su querido cachorro."),		// Spanish
		MAKE_TESTR("Pijamalı hasta, yağız şoföre çabucak güvendi."),		// Turkish
		MAKE_TESTR("😁😄😔✂✋🚀⏰⏳"),
		MAKE_TESTR("😁MiXeD-CaSe⏳"),
	};
	#undef MAKE_TESTR

	u32 aNScratch[1024];
	u8 aChzScratch[1024];
	char aChzPuny[1024];

	for (int ipChz = 0; ipChz != MOE_DIM(s_aTestr); ++ipChz)
	{
		MOE_ASSERT(Puny::FIsValidUtf8(s_aTestr[ipChz].m_pChzUtf8), "bad input string");

		MOE_ASSERT(Puny::FTryConvertUtf8ToUcs4(s_aTestr[ipChz].m_pChzUtf8, (char32_t *)aNScratch, (char32_t*)MOE_PMAC(aNScratch)), "Failed converting to ucs4");
		MOE_ASSERT(FAreWchzEqual(s_aTestr[ipChz].m_pWchzTest, (const char32_t *)aNScratch), "conversion error");

		MOE_ASSERT(Puny::FTryConvertUcs4ToUtf8(aNScratch, aChzScratch, MOE_PMAC(aChzScratch)), "Failed converting to utf8");

		MOE_ASSERT(Moe::FAreChzEqual((char*)aChzScratch, s_aTestr[ipChz].m_pChzUtf8), "conversion error");

		auto punyret = Puny::PunyretEncode(s_aTestr[ipChz].m_pChzUtf8, (char *)aChzScratch, MOE_DIM(aChzScratch));
		MOE_ASSERT(punyret == Puny::PUNYRET_Success, "bad punycode encode");

		punyret = Puny::PunyretDecode((char *)aChzScratch, aChzPuny, MOE_DIM(aChzPuny));
		MOE_ASSERT(punyret == Puny::PUNYRET_Success, "bad punycode decode");
		MOE_ASSERT(Moe::FAreChzEqual((char*)aChzPuny, s_aTestr[ipChz].m_pChzUtf8), "punycode decode fail");

	}
	return true;
}

