#include "MoeTypes.h" 

#include <cstdarg>
#include <stdio.h>
#include <string.h>

#define STB_MALLOC_IMPLEMENTATION
#include "stb_malloc.h"

#ifdef MOE_TRACK_ALLOCATION
#include "MoeString.h"
#include "EwcArray.h"
#include "EwcHash.h"
#endif

namespace Moe
{

Alloc	g_allocCManaged;
bool	Alloc::s_fProgramIsShutdown = false;



class AllocTracker //tag=altrac
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

	void	TrackAlloc(size_t cB, const char * pChzFile, int cLine, BK bk, HV * pHv)
			{
#ifdef MOE_TRACK_ALLOCATION

				HV hv = HvFromFileLineBk(pChzFile, cLine, bk);
				*pHv = hv;

				int * piEntry;
				if (m_hashHvIentry.FinsEnsureKey(hv, &piEntry) == FINS_AlreadyExisted)
				{
					SEntry * pEntry = &m_aryEntry[*piEntry];
					pEntry->m_cB += cB;
					pEntry->m_cBHighwater = ewcMax(pEntry->m_cBHighwater, pEntry->m_cB);
					MOE_ASSERT(pEntry->m_cLine == cLine, "line mismatch in TrackAlloc");
					MOE_ASSERT(pEntry->m_bk == bk, "Block kind mismatch in TrackAlloc");
					++pEntry->m_cAllocations;
				}
				else
				{
					*piEntry = (int)m_aryEntry.C();
					SEntry * pEntry = m_aryEntry.AppendNew();
					pEntry->m_cB 	   	   = cB;
					pEntry->m_cBHighwater  = cB;
					pEntry->m_cAllocations = 1;
					pEntry->m_pChzFile 	   = pChzFile;
					pEntry->m_cLine    	   = cLine;
					pEntry->m_bk			= bk;
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
			printf("%zd / %zd\t\t %s BK(%d) : %d\n", pEntry->m_cB, pEntry->m_cBHighwater, pEntry->m_pChzFile, pEntry->m_bk, pEntry->m_cLine);
			cBTotal += pEntry->m_cB;
		}
		printf("%zd tracked\n", cBTotal);
#endif
	}

	struct SEntry // tag=entry
	{
		size_t			m_cB;
		size_t			m_cBHighwater;
		int				m_cAllocations;
		const char *	m_pChzFile;
		int				m_cLine;
		BK				m_bk;
	};

#ifdef MOE_TRACK_ALLOCATION
	// BB - replace with indexed set
	EWC::CDynAry<SEntry>	m_aryEntry;
	EWC::CHash<HV, int>		m_hashHvIentry;
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

void Alloc::TrackAlloc(size_t cB, const char * pChzFile, int cLine, BK bk, HV * pHv)
{
	m_pAltrac->TrackAlloc(cB, pChzFile, cLine, bk, pHv);
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

	iB = ewcMin(iB, pStrbuf->m_cBMax -1);

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
		Resize(cCh + s_cChPrefixPad, ewcMax<size_t>(s_cChPrefixPad, CB()), m_pChzMax - m_pChzAppend);
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
		Resize(s_cChPrefixPad, ewcMax<size_t>(s_cChPrefixPad, CB()), cB + s_cChPrefixPad);
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
		cBOld = ewcMin<size_t>(cBOld, cBUsed);
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

