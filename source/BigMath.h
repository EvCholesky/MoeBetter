/* Copyright (C) 2016 Evan Christensen
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
#include <limits.h>



struct BigInt // tag = bint
{
			BigInt()
			:m_nAbs(0)
			,m_fIsNegative(false)
				{ ; }

			s64 S64Coerce() const
			{
				MOE_ASSERT(m_nAbs <= LLONG_MAX, "int too large to be signed value");
				if (m_fIsNegative)
					return -(s64)m_nAbs;
				return (s64)m_nAbs;
			}

			u64 U64Coerce() const
			{
				MOE_ASSERT(!m_fIsNegative, "negative value being coerced into an unsigned value");
				return m_nAbs;
			}

	u64		m_nAbs;
	bool	m_fIsNegative;
};

inline BigInt BintFromInt(s64 nSigned)
{
	BigInt bint;
	bint.m_nAbs = llabs(nSigned);
	bint.m_fIsNegative = nSigned < 0;
	return bint;
}

inline BigInt BintFromUint(u64 nUnsigned, bool fIsNegative = false)
{
	BigInt bint;
	bint.m_nAbs = nUnsigned;
	bint.m_fIsNegative = fIsNegative;
	return bint;
}

inline bool FAreEqual(const BigInt & bintLhs, const BigInt & bintRhs)
{
	bool fIsAbsSame = bintLhs.m_nAbs == bintRhs.m_nAbs;
	bool fIsSignSame = bintLhs.m_fIsNegative == bintRhs.m_fIsNegative;
	fIsSignSame |= (fIsAbsSame & (bintLhs.m_nAbs == 0));
	return fIsAbsSame & fIsSignSame;
}



// NOTE: I'm opting away from operator overloading for Signed65 because I want it to be explicit
inline BigInt BintAdd(const BigInt & bintLhs, const BigInt & bintRhs)
{
	u64 nLhs = bintLhs.m_nAbs;
	u64 nRhs = bintRhs.m_nAbs;
	if (bintLhs.m_fIsNegative == bintRhs.m_fIsNegative)
	{
		return BintFromUint(nLhs + nRhs, bintLhs.m_fIsNegative);
	}

	// one of the operands is negative, if it's the larger one the result is negative
	if (nLhs > nRhs)
	{
		return BintFromUint(nLhs - nRhs, bintLhs.m_fIsNegative);
	}
	return BintFromUint(nRhs - nLhs, bintRhs.m_fIsNegative);
}

inline BigInt BintSub(const BigInt & bintLhs, const BigInt & bintRhs)
{
	u64 nLhs = bintLhs.m_nAbs;
	u64 nRhs = bintRhs.m_nAbs;
	bool fIsRhsNegative = !bintRhs.m_fIsNegative;
	if (bintLhs.m_fIsNegative == fIsRhsNegative)
	{
		return BintFromUint(nLhs + nRhs, bintLhs.m_fIsNegative);
	}

	// one of the operands is negative, if it's the larger one the result is negative
	if (nLhs > nRhs)
	{
		return BintFromUint(nLhs - nRhs, bintLhs.m_fIsNegative);
	}
	return BintFromUint(nRhs - nLhs, fIsRhsNegative);
}

inline BigInt BintMul(const BigInt & bintLhs, const BigInt & bintRhs)
{
	return BintFromUint(bintLhs.m_nAbs * bintRhs.m_nAbs, bintLhs.m_fIsNegative != bintRhs.m_fIsNegative);
}

inline BigInt BintDiv(const BigInt & bintLhs, const BigInt & bintRhs)
{
	return BintFromUint(bintLhs.m_nAbs / bintRhs.m_nAbs, bintLhs.m_fIsNegative != bintRhs.m_fIsNegative);
}

inline BigInt BintRemainder(const BigInt & bintLhs, const BigInt & bintRhs)
{
	// NOTE: The sign of c++'s % operator is determined by the sign of the numerator:
	//  % is defined as: a == (a/b) * b + a%b, where a/b truncates towards zero.
	//  so with a - (a/b)*b == a%b we know that (a/b)*b is smaller than a, thus a determines the sign.
	return BintFromUint(bintLhs.m_nAbs % bintRhs.m_nAbs, bintLhs.m_fIsNegative);
}

inline u64 NTwosCompliment(u64 n, bool fIsNegative)
{
	return (fIsNegative) ? (~n + 1) : n;
}

inline BigInt BintBitwiseOr(const BigInt & bintLhs, const BigInt & bintRhs)
{
	u64 nLhs = NTwosCompliment(bintLhs.m_nAbs, bintLhs.m_fIsNegative);
	u64 nRhs = NTwosCompliment(bintRhs.m_nAbs, bintRhs.m_fIsNegative);

	bool fIsOutNegative = bintLhs.m_fIsNegative | bintRhs.m_fIsNegative;
	u64 nOut = NTwosCompliment(nLhs | nRhs, fIsOutNegative);

	return BintFromUint(nOut, fIsOutNegative);
}

inline BigInt BintBitwiseAnd(const BigInt & bintLhs, const BigInt & bintRhs)
{
	u64 nLhs = NTwosCompliment(bintLhs.m_nAbs, bintLhs.m_fIsNegative);
	u64 nRhs = NTwosCompliment(bintRhs.m_nAbs, bintRhs.m_fIsNegative);

	bool fIsOutNegative = bintLhs.m_fIsNegative & bintRhs.m_fIsNegative;
	u64 nOut = NTwosCompliment(nLhs & nRhs, fIsOutNegative);

	return BintFromUint(nOut, fIsOutNegative);
}

inline BigInt BintBitwiseNot(const BigInt & bintOp)
{
	u64 nLhs = NTwosCompliment(bintOp.m_nAbs, bintOp.m_fIsNegative);

	bool fIsOutNegative = !bintOp.m_fIsNegative;
	u64 nOut = NTwosCompliment(~nLhs, fIsOutNegative);

	return BintFromUint(nOut, fIsOutNegative);
}

inline BigInt BintBitwiseXor(const BigInt & bintLhs, const BigInt & bintRhs)
{
	u64 nLhs = NTwosCompliment(bintLhs.m_nAbs, bintLhs.m_fIsNegative);
	u64 nRhs = NTwosCompliment(bintRhs.m_nAbs, bintRhs.m_fIsNegative);

	bool fIsOutNegative = bintLhs.m_fIsNegative ^ bintRhs.m_fIsNegative;
	u64 nOut = NTwosCompliment(nLhs ^ nRhs, fIsOutNegative);

	return BintFromUint(nOut, fIsOutNegative);
}
inline BigInt BintShiftRight(const BigInt & bintLhs, const BigInt & bintRhs)
{
	u64 nLhs = NTwosCompliment(bintLhs.m_nAbs, bintLhs.m_fIsNegative);
	if (bintRhs.m_fIsNegative)
	{
		return BintFromUint(NTwosCompliment(nLhs << bintRhs.m_nAbs, bintLhs.m_fIsNegative), bintLhs.m_fIsNegative);
	}
	else if (!bintLhs.m_fIsNegative)
	{
		return BintFromUint(nLhs >> bintRhs.m_nAbs, false);
	}
	else
	{
		u64 nSignExtend = (bintRhs.m_nAbs > 0) ? 0xFFFFFFFFFFFFFFFF << (64 - bintRhs.m_nAbs) : 0; // shift right by 64 bits is undefined behavior (doesn't work on x64)
		return BintFromUint(NTwosCompliment((nLhs >> bintRhs.m_nAbs) | nSignExtend, bintLhs.m_fIsNegative), bintLhs.m_fIsNegative);
	}
}

inline BigInt BintShiftLeft(const BigInt & bintLhs, const BigInt & bintRhs)
{
	u64 nLhs = NTwosCompliment(bintLhs.m_nAbs, bintLhs.m_fIsNegative);
	if (bintRhs.m_fIsNegative)
	{
		u64 nSignExtend = (bintLhs.m_fIsNegative) ? 0x8000000000000000 : 0;
		return BintFromUint(NTwosCompliment((nLhs >> bintRhs.m_nAbs) | nSignExtend, bintLhs.m_fIsNegative), bintLhs.m_fIsNegative);
	}
	else
	{
		return BintFromUint(NTwosCompliment(nLhs << bintRhs.m_nAbs, bintLhs.m_fIsNegative), bintLhs.m_fIsNegative);
	}
}

inline BigInt BintNextPowerOfTwo(const BigInt & bint)
{
	u64 n = bint.m_nAbs; // compute the next highest power of 2 of 32-bit v

	// smear the bits down and then add one
	n |= n >> 1;
	n |= n >> 2;
	n |= n >> 4;
	n |= n >> 8;
	n |= n >> 16;
	n |= n >> 32;
	n++;
	return BintFromUint(n);
}

inline bool operator ==(const BigInt & bintLhs, const BigInt & bintRhs)
{
	return (bintLhs.m_nAbs == bintRhs.m_nAbs) & (bintLhs.m_fIsNegative == bintRhs.m_fIsNegative);
}

inline bool operator !=(const BigInt & bintLhs, const BigInt & bintRhs)
{
	return !(bintLhs == bintRhs);
}

inline bool operator<(const BigInt & bintLhs, const BigInt & bintRhs)
{
	if (bintLhs.m_fIsNegative != bintRhs.m_fIsNegative)
		return bintLhs.m_fIsNegative;

	return (bintLhs.m_nAbs < bintRhs.m_nAbs) != bintLhs.m_fIsNegative;
}

inline bool operator<=(const BigInt & bintLhs, const BigInt & bintRhs)
{
	return (bintLhs == bintRhs) | (bintLhs < bintRhs);
}

inline bool operator >(const BigInt & bintLhs, const BigInt & bintRhs)
{
	return !(bintLhs <= bintRhs);
}

inline bool operator >=(const BigInt & bintLhs, const BigInt & bintRhs)
{
	return !(bintLhs < bintRhs);
}
