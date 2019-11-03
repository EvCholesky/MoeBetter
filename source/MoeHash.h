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
#include "MoeTypes.h"

namespace Moe
{


// hash based key -> value mapping
template <	typename K, 
			typename V, 
			int LOAD_FACTOR_PERCENT = 65> // hash grows when cUsed is greater that this percent of cCapacity
class CHash //tag=hash
{
protected:
	struct SEntry;
public:
	static const HV kHvUnused = 0xFFFFFFFF;
	static const HV kHvDeleted = 0xFFFFFFFE;
	typedef K KeyType;
	typedef V ValueType;

	class CIterator
	{
	public:
				CIterator(CHash<K, V, LOAD_FACTOR_PERCENT> * pHash)
				:m_pHash(pHash)
				,m_pEntry(pHash->m_aEntry)
					{ ; }

		V *		Next(K ** ppKey = nullptr) 
					{
						auto pEntry = m_pEntry;

						size_t cCapacity = m_pHash->m_cCapacity;
						size_t ipEntry = pEntry - m_pHash->m_aEntry;
						while (1)
						{
							if (ipEntry >= cCapacity)
							{
								if (ppKey)
									*ppKey = nullptr;
								return nullptr;
							}

							if ((pEntry->m_hv != kHvUnused) & (pEntry->m_hv != kHvDeleted))
								break;

							++pEntry;
							++ipEntry;
						}
						m_pEntry = pEntry+1;

						if (ppKey)
							*ppKey = &pEntry->m_key;
						return &pEntry->m_value;
					}

		CHash<K, V, LOAD_FACTOR_PERCENT> *	m_pHash;
		SEntry *							m_pEntry;
	};

	class CIteratorKey // tag = iterkey
	{
	public:
				CIteratorKey(CHash<K, V, LOAD_FACTOR_PERCENT> * pHash, K key)
				:m_pHash(pHash)
				,m_pEntry(pHash->m_aEntry)
				,m_key(key)
				,m_hv(HvExtract(key))
					{ ; }

		V *		Next() 
					{
						auto pEntry = m_pEntry;

						size_t cCapacity = m_pHash->m_cCapacity;
						size_t ipEntry = pEntry - m_pHash->m_aEntry;
						HV hvDesired = m_hv;
						while (1)
						{
							++ipEntry;
							if (ipEntry >= cCapacity)
							{
								return nullptr;
							}

							++pEntry;
							if (pEntry->m_hv == hvDesired)
							{
								if (pEntry->m_key == m_key)
									break;
							}
						}
						m_pEntry = pEntry;
						return &pEntry->m_value;
					}

		CHash<K, V, LOAD_FACTOR_PERCENT> *	m_pHash;
		SEntry *							m_pEntry;
		K									m_key;
		HV									m_hv;
	};

				CHash(Alloc * pAlloc, BK bk, u32 cCapacityStarting = 32)
				:m_pAlloc(pAlloc)
				,m_aEntry(nullptr)
				,m_cUsed(0)
				,m_cCapacity(0)
				,m_bk(bk)
					{ SetAlloc(pAlloc, bk, cCapacityStarting); }

				CHash()
				:m_pAlloc(nullptr)
				,m_aEntry(nullptr)
				,m_cUsed(0)
				,m_cCapacity(0)
				,m_bk(BK_Nil)
					{ ; }

				~CHash()
					{ Clear(0); }

	void		SetAlloc(Alloc * pAlloc, BK bk, u32 cCapacityStarting = 32)
					{
						m_pAlloc = pAlloc;
						m_bk = bk;
						Grow(cCapacityStarting);
					}

	V *			Lookup(K key)
					{
						HV hv = HvExtract(key); // BB - need to clamp less than kHvDeleted

						MOE_ASSERT(FIsPowerOfTwo(m_cCapacity), "cCapacity should be a power of two");
						u32 mask = m_cCapacity - 1;
						u32 iEntry = hv & mask;

						u32 cProbes = 0;
						while (cProbes < m_cCapacity)
						{
							SEntry & entry = m_aEntry[iEntry];
							if (entry.FIsUnused())
							{
								return nullptr;
							}
							else if (entry.m_hv == hv)
							{
								return &entry.m_value;
							}
							iEntry = (iEntry + 1) & mask;
							++cProbes;
						}

						return nullptr;
					}

	void		Clear(u32 cCapacityNew = 32)
					{
						SEntry * pEntryEnd = &m_aEntry[m_cCapacity];
						for (SEntry * pEntry = m_aEntry; pEntry != pEntryEnd; ++pEntry)
						{
							if ((pEntry->m_hv == kHvUnused)|(pEntry->m_hv == kHvDeleted))
								continue;

							Destruct(&pEntry->m_key);
							Destruct(&pEntry->m_value);
						}

						if (m_aEntry)
						{
							m_pAlloc->MOE_FREE(m_aEntry);
							m_aEntry = nullptr;
						}

						Grow(cCapacityNew);
					}

	void		Remove(K key)
					{
						HV hv = HvExtract(key); // BB - need to clamp less than kHvDeleted

						MOE_ASSERT(FIsPowerOfTwo(m_cCapacity), "cCapacity should be a power of two");
						u32 mask = m_cCapacity - 1;
						u32 iEntry = hv & mask;

						u32 cProbes = 0;
						while (cProbes < m_cCapacity)
						{
							SEntry & entry = m_aEntry[iEntry];
							if (entry.FIsUnused())
							{
								MOE_ASSERT(false, "Can't find key in hash (while trying to remove it)");
								return;
							}
							else if (entry.m_hv == hv)
							{
								Destruct(&entry.m_key);
								Destruct(&entry.m_value);
								entry.MarkAsDeleted();
								--m_cUsed;
								return;
							}
							iEntry = (iEntry + 1) & mask;
							++cProbes;
						}
					}

	void		Insert(K key, V value)
					{
						if (m_cUsed >= m_cCapacity)
						{
							int cCapacityNew = ewcMax<u32>(32, m_cCapacity * 2);
							Grow(cCapacityNew);
						}

						HV hv = HvExtract(key); // BB - need to clamp less than kHvDeleted
						u32 mask = m_cCapacity - 1;
						u32 iEntry = hv & mask;
						SEntry * pEntryAvailable = nullptr;

						u32 cProbes = 0;	
						for(; cProbes < m_cCapacity; ++cProbes) // prevent infinite looping
						{
							SEntry & entry = m_aEntry[iEntry];
							if (entry.FIsUnusedOrDeleted())
							{
								if (!pEntryAvailable)
									pEntryAvailable = &entry;

								if (entry.FIsUnused())
								{
									break;
								}
							}

							iEntry = (iEntry + 1) & mask;
						}

						if (pEntryAvailable)
						{
							pEntryAvailable->m_key = key;
							pEntryAvailable->m_value = value;
							pEntryAvailable->m_hv = hv;
							++m_cUsed;

							if (m_cUsed * 100 >= m_cCapacity * LOAD_FACTOR_PERCENT)
							{
								Grow(m_cCapacity * 2);
							}

							return;
						}

						MOE_ASSERT(false, "CHash overflow");
						return;
					}

	INRES		InresEnsureKey(K key, V ** ppValue = nullptr)
					{
						if (m_cUsed >= m_cCapacity)
						{
							int cCapacityNew = ewcMax<u32>(32, m_cCapacity * 2);
							Grow(cCapacityNew);
						}

						HV hv = HvExtract(key); // BB - need to clamp less than kHvDeleted
						u32 mask = m_cCapacity - 1;
						u32 iEntry = hv & mask;
						SEntry * pEntryAvailable = nullptr;

						u32 cProbes = 0;	
						for(; cProbes < m_cCapacity; ++cProbes) // prevent infinite looping
						{
							SEntry & entry = m_aEntry[iEntry];
							if (entry.m_hv == hv)
							{
								if (ppValue)
									*ppValue = &entry.m_value;
								return INRES_AlreadyExisted;
							}
							else if (entry.FIsUnusedOrDeleted())
							{
								if (!pEntryAvailable)
									pEntryAvailable = &entry;

								if (entry.FIsUnused())
								{
									break;
								}
							}

							iEntry = (iEntry + 1) & mask;
						}

						if (pEntryAvailable)
						{
							pEntryAvailable->m_key = key;
							Construct<V>(&pEntryAvailable->m_value);
							pEntryAvailable->m_hv = hv;
							++m_cUsed;

							if (m_cUsed * 100 >= m_cCapacity * LOAD_FACTOR_PERCENT)
							{
								Grow(m_cCapacity * 2);

								if (ppValue)
									*ppValue = Lookup(key);
								//MOE_ASSERT(*ppValue, "Hash ensure key failure");
							}
							else if (ppValue)
							{
								*ppValue = &pEntryAvailable->m_value;
								//MOE_ASSERT(*ppValue, "Hash ensure key failure");
							}

							return INRES_Inserted;
						}

						MOE_ASSERT(false, "CHash overflow");
						return INSRESj_Error;
					}

	INRES		InresEnsureKeyAndValue(K key, V value)
					{
						if (m_cUsed >= m_cCapacity)
						{
							int cCapacityNew = ewcMax<u32>(32, m_cCapacity * 2);
							Grow(cCapacityNew);
						}

						HV hv = HvExtract(key); // BB - need to clamp less than kHvDeleted
						u32 mask = m_cCapacity - 1;
						u32 iEntry = hv & mask;
						SEntry * pEntryAvailable = nullptr;

						for(u32 cProbes = 0; cProbes < m_cCapacity; ++cProbes) // prevent infinite looping
						{
							SEntry & entry = m_aEntry[iEntry];
							if (entry.m_hv == hv)
							{
								entry.m_value = value;

								return INRES_AlreadyExisted;
							}
							else if (entry.FIsUnusedOrDeleted())
							{
								if (!pEntryAvailable)
									pEntryAvailable = &entry;

								if (entry.FIsUnused())
								{
									break;
								}
							}

							iEntry = (iEntry + 1) & (m_cCapacity - 1);
						}

						if (pEntryAvailable)
						{
							pEntryAvailable->m_key = key;
							pEntryAvailable->m_value = value;
							pEntryAvailable->m_hv = hv;
							++m_cUsed;

							if (m_cUsed * 100 >= m_cCapacity * LOAD_FACTOR_PERCENT)
								Grow(m_cCapacity * 2);
							return INRES_Inserted;
						}

						MOE_ASSERT(false, "CHash overflow");
						return INRES_Error;
					}
	
	INRES		InresDebugEnsureKeyAndValueAfterDeleted(K key, V value, int cDeleted)
					{
						// simulate the effect of inserting multiple colliding entries then deleting all but the last one

						HV hv = HvExtract(key); // BB - need to clamp less than kHvDeleted
						HV hvDebug = hv + 1;
						u32 mask = m_cCapacity - 1;
						u32 iEntry = hv & mask;
						u32 iEntryStart = iEntry;;
								
						if (!MOE_FVERIFY((m_cUsed+1+cDeleted) * 100 < m_cCapacity * LOAD_FACTOR_PERCENT, "Debug insertion will cause rehash"))
							return INRES_Error;

						for (int iDeleted = 0; iDeleted < cDeleted; ++iDeleted)
						{
							for(u32 cProbes = 0; 1; ++cProbes) // prevent infinite looping
							{
								if (cProbes >= m_cCapacity)
								{
									MOE_ASSERT(false, "CHash overflow");
									return INRES_Error;
								}

								SEntry & entry = m_aEntry[iEntry];

								if (entry.FIsUnusedOrDeleted())
								{
									entry.m_hv = hvDebug;
									break;
								}

								iEntry = (iEntry + 1) & (m_cCapacity - 1);
							}
						}

						INRES insresjReturn = InresEnsureKeyAndValue(key, value);

						u32 iEntryEnd = iEntry;
						for (iEntry = iEntryStart; iEntry != iEntryEnd; ++iEntry)
						{
							SEntry & entry = m_aEntry[iEntry];
							if (entry.m_hv == hvDebug)
								entry.m_hv = kHvDeleted;
						}

						return InsresReturn;
					}

	void		Grow(u32 cCapacityNew)
					{
						// NOTE: can be called with the current capacity to force a realloc and rehash for testing.

						auto cCapacityOld = m_cCapacity;
						if (cCapacityNew == 0)
						{
							m_aEntry = nullptr;
							m_cCapacity = 0;
							return;
						}

						MOE_ASSERT(FIsPowerOfTwo(cCapacityNew), "invalid CHash capacity");

						SEntry * aEntryNew = (SEntry*)m_pAlloc->MOE_ALLOC_BK(sizeof(SEntry) * cCapacityNew, MOE_ALIGN_OF(SEntry), m_bk);
						ConstructN<SEntry>(aEntryNew, cCapacityNew);

						if (m_aEntry)
						{
							Rehash(m_aEntry, m_cCapacity, aEntryNew, cCapacityNew); 

							DestructN<SEntry>(m_aEntry, cCapacityOld);
							m_pAlloc->MOE_FREE(m_aEntry);
						}

						m_aEntry = aEntryNew;
						m_cCapacity = cCapacityNew;
					}

	void		Rehash(SEntry * aEntryOld, u32 cCapacityOld, SEntry * aEntryNew, u32 cCapacityNew)
					{
						u32 mask = cCapacityNew - 1;
						SEntry * pEntryEnd = &aEntryOld[cCapacityOld];
						for (SEntry * pEntryOld = aEntryOld; pEntryOld != pEntryEnd; ++pEntryOld)
						{
							// skip unused and deleted entries
							if ((pEntryOld->m_hv == kHvUnused)|(pEntryOld->m_hv == kHvDeleted))
								continue;

							u32 cProbes = 0;
							u32 iEntryNew = pEntryOld->m_hv & mask;
							for( ; cProbes < cCapacityNew; ++cProbes) // prevent infinite looping
							{
								SEntry & entryNew = aEntryNew[iEntryNew];
								if (entryNew.m_hv != pEntryOld->m_hv && entryNew.FIsUnusedOrDeleted())
								{
									entryNew.m_key = pEntryOld->m_key;
									entryNew.m_value = pEntryOld->m_value;
									entryNew.m_hv = pEntryOld->m_hv;
									break;
								}

								iEntryNew = (iEntryNew + 1) & (cCapacityNew - 1);
							}
							MOE_ASSERT(cProbes < cCapacityNew, "Rehash failure");
						}
					}

	void 		Swap(CHash<K, V, LOAD_FACTOR_PERCENT> * pHashOther)
					{
						auto pAllocTemp = m_pAlloc;
						auto aEntryTemp = m_aEntry;
						auto cUsedTemp = m_cUsed;
						auto cCapacityTemp = m_cCapacity;
						auto bkTemp = m_bk;

						m_pAlloc = pHashOther->m_pAlloc;
						m_aEntry = pHashOther->m_aEntry;
						m_cUsed = pHashOther->m_cUsed;
						m_cCapacity = pHashOther->m_cCapacity;
						m_bk = pHashOther->m_bk;

						pHashOther->m_pAlloc = pAllocTemp;
						pHashOther->m_aEntry = aEntryTemp;
						pHashOther->m_cUsed = cUsedTemp;
						pHashOther->m_cCapacity = cCapacityTemp;
						pHashOther->m_bk = bkTemp;
					}

	Alloc *	PAlloc()
					{ return m_pAlloc; } 
	u32			C() const
					{ return m_cUsed; }
	u32			CCapacity() const
					{ return m_cCapacity; }
	bool		FIsEmpty() const
					{ return m_cUsed == 0; }

protected:
	struct SEntry // tag=entry
	{
				SEntry()
				:m_hv(kHvUnused)
					{ ; }
		bool	FIsUnused()
					{ return m_hv == kHvUnused; }
		bool	FIsUnusedOrDeleted()
					{ return (m_hv == kHvUnused) | (m_hv == kHvDeleted); }
		void	MarkAsDeleted()
					{ m_hv = kHvDeleted; }

		HV	m_hv;
		K	m_key;
		V	m_value;
	};

	Alloc *	m_pAlloc;
	SEntry *	m_aEntry;
	u32			m_cUsed;
	u32			m_cCapacity;
	BK			m_bk;
};
} // namespace Moe
