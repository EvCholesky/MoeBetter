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

// Interned string class, equivalence can just check pointer.

#pragma once

#include "MoeTypes.h"

u32 HvFromPBFVN(const void * pV, size_t cB);
u32 HvConcatPBFVN(u32 hv, const void * pV, size_t cB);
u32 HvFromPChzLowercaseFVN(const char * pV, size_t cB);

namespace Moe
{
	class StringTable;

	const char * PChzIntern(const char * pChz);
	const char * PChzInternCopy(const char * pChz);

	inline u32		HvFromPChz(const char * pChz, size_t cB = 0)
						{
							if (!pChz)
								return 0;
							if (cB == 0)
								cB = CBChz(pChz)-1;

							return HvFromPBFVN(pChz, cB);
						}

	inline u32		HvConcatPChz(u32 hv, const char * pChz, size_t cB = 0)
						{
							if (!pChz)
								return hv;
							if (cB == 0)
								cB = CBChz(pChz)-1;

							return HvConcatPBFVN(hv, pChz, cB);
						}

	inline u32		HvFromPChzLowercase(const char * pChz, size_t cB = 0)
						{
							if (!pChz)
								return 0;
							if (cB == 0)
								cB = CBChz(pChz)-1;

							return HvFromPChzLowercaseFVN(pChz, cB);
						}

	// Interned string class 

	struct InString	// tag = istr
	{
		// force users to explicitly intern pointer or copy and intern
						InString()
						:m_pChz(nullptr)
							{ ; }

		bool			operator==(const char * pChzOther) const
							{ return m_pChz == pChzOther; }
		bool			operator!=(const char * pChzOther) const
							{ return !(*this == pChzOther); }

		bool			operator==(const InString & istrOther) const
							{ return m_pChz == istrOther.m_pChz; }
		bool			operator!=(const InString & istrOther) const
							{ return !(*this == istrOther); }

		InString &		operator=(const InString & istrOther)
							{ return *this = istrOther.m_pChz; }
		InString &		operator=(const char * pChz)
							{
								if (m_pChz != pChz)
								{
									m_pChz = PChzIntern(pChz);
								}

								return *this;
							}

		bool			FIsEmpty() const
							{ return m_pChz == nullptr || *m_pChz == '\0'; }
		size_t			CB() const
							{ return Moe::CBChz(m_pChz); }
		size_t			CCodepoint() const
							{ return Moe::CCodepoint(m_pChz); }

		const char *	m_pChz;

	};

	inline HV HvExtract(const Moe::InString & istr)
	{
		return HvFromP(istr.m_pChz);
	}

	void StaticInitStrings(Alloc * pAlloc);
	void StaticShutdownStrings(Alloc * pAlloc);

} // namespace Moe


Moe::InString IstrIntern(const char * pChz);
Moe::InString IstrInternCopy(const char * pChz, size_t cB = 0);
