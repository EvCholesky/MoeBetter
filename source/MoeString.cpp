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

#include "MoeString.h"
#include "MoeHash.h"

u32 HvFromPBFVN(const void * pV, size_t cB)
{
	auto pB = (u8*)pV;
    u32 hv = 2166136261;

	for (size_t iB=0; iB < cB; ++iB)
    {
        hv = (hv * 16777619) ^ pB[iB];
    }

    return hv;
}

u32 HvConcatPBFVN(u32 hv, const void * pV, size_t cB)
{
	auto pB = (u8*)pV;
	for (size_t iB=0; iB < cB; ++iB)
    {
        hv = (hv * 16777619) ^ pB[iB];
    }

    return hv;
}

u32 HvFromPChzLowercaseFVN(const char * pV, size_t cB)
{
	auto pB = (u8*)pV;
    u32 hv = 2166136261;

	for (size_t iB=0; iB < cB; ++iB)
    {
        hv = (hv * 16777619) ^ (u8)tolower(pB[iB]);
    }

    return hv;
}


namespace Moe
{

// string table for ref-counted string class
class StringTable // tag=strtab
{
public:
				StringTable(Moe::Alloc * pAlloc)
				:m_pAlloc(pAlloc)
				,m_mpHvEntry(pAlloc, Moe::BK_StringTable, 128)
					{ ; }

	const char * PChzIntern(const char * pChz, size_t cB, HV hv)
					{
						Entry * pEntry = nullptr;
						if (m_mpHvEntry.InresEnsureKey(hv, &pEntry) == Moe::INRES_Inserted)
						{
							pEntry->m_cRef = 1;
							pEntry->m_pChz = pChz;
							pEntry->m_fIsManaged = false;
						}
						else
						{
							++pEntry->m_cRef;
							MOE_ASSERT(!pEntry->m_fIsManaged && pEntry->m_pChz == pChz, "bad table lookup in CStringTable");
						}

						return pEntry->m_pChz;
					}

	const char * PChzInternCopy(const char * pChz, size_t cB, HV hv)
					{
						Entry * pEntry = nullptr;
						if (m_mpHvEntry.InresEnsureKey(hv, &pEntry) == Moe::INRES_Inserted)
						{
							pEntry->m_cRef = 1;
							char * aCh = (char*)m_pAlloc->MOE_ALLOC(sizeof(char) * cB, MOE_ALIGN_OF(char));
							pEntry->m_fIsManaged = true;
							pEntry->m_pChz = aCh;

							(void) Moe::CBCopyChz(pChz, aCh, cB);
						}
						else
						{
							++pEntry->m_cRef;
							MOE_ASSERT(pEntry->m_fIsManaged && Moe::FAreChzEqual(pEntry->m_pChz, pChz, cB-1), "bad table lookup in CStringTable");
						}

						return pEntry->m_pChz;
					}

	void		FreePChz(const char * pChz, HV hv)
					{
						Entry * pEntry = m_mpHvEntry.Lookup(hv);
						if (!pEntry)
						{
							MOE_ASSERT(false, "failed lookup in CStringTable::FreePChz");
							return;
						}
						--pEntry->m_cRef;

						if (pEntry->m_cRef <= 0)
						{
							if (pEntry->m_fIsManaged)
							{
								m_pAlloc->MOE_FREE((char*)pEntry->m_pChz);
							}
							m_mpHvEntry.Remove(hv);
						}
					}

	struct Entry
	{
		const char *	m_pChz;
		u32				m_cRef;
		bool			m_fIsManaged; // this string's allocateion is managed by the string table
	};

	Moe::Alloc * 			m_pAlloc;
	Moe::CHash<HV, Entry>	m_mpHvEntry;
};

StringTable * s_pStrtab = nullptr;



const char * PChzIntern(const char * pChz)
{
	// intern this string into the table, it's responsible for it's own lifetime

	return s_pStrtab->PChzIntern(pChz, Moe::CCh(pChz), Moe::HvFromPChz(pChz));
}

const char * PChzInternCopy(const char * pChz, size_t cB = 0)
{
	if (cB == 0)
	{
		return s_pStrtab->PChzInternCopy(pChz, Moe::CCh(pChz), Moe::HvFromPChz(pChz, cB));
	}

	return s_pStrtab->PChzInternCopy(pChz, cB, Moe::HvFromPChz(pChz, cB));
}

const char * PChzInternCopy(const char * pChz)
{
	return s_pStrtab->PChzInternCopy(pChz, Moe::CCh(pChz), Moe::HvFromPChz(pChz));
}

/*
InString IstrPchCopy(const char * pChz, size_t cB)
{
	// intern this string and manage it's memory allocation

	if (cB < 0)
	{
		return s_pStrtab->PChzInternCopy(pChz, Moe::CCh(pChz), Moe::HvFromPChz(pChz));
	}

	return s_pStrtab->PChzInternCopy(pChz, Moe::CCh(pChz), Moe::HvFromPChz(pChz));
}*/

void StaticInitStrings(Alloc * pAlloc)
{
	s_pStrtab = MOE_NEW(pAlloc, StringTable) StringTable(pAlloc);

	// BB - We wouldn't need this if we switched to an interned string table
	//SymbolTable::StaticStringInit();
}

void StaticShutdownStrings(Alloc * pAlloc)
{
	//SymbolTable::StaticStringShutdown();

	pAlloc->MOE_DELETE(s_pStrtab);
	s_pStrtab = nullptr;
}

} // namespace Moe

Moe::InString IstrIntern(const char * pChz)
{
	Moe::InString instr;
	instr.m_pChz = Moe::PChzIntern(pChz);
	return instr;
}

Moe::InString IstrInternCopy(const char * pChz, size_t cB)
{
	Moe::InString instr;
	instr.m_pChz = Moe::PChzInternCopy(pChz, cB);
	return instr;
}

