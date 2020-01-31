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

#include "CodeGen.h"
#include "MoeTypes.h"

using namespace Moe;



// helper routine for generating operators, used to make sure type checking errors are in sync with the code generator
struct OperatorInfo // tag = opinfo
{
					OperatorInfo()
					:m_irop(IROP_Nil)
					,m_npred(NPRED_Nil)
					,m_gpred(GPRED_Nil)
					,m_fNegateFirst(false)
					,m_pChzName(nullptr)
						{ ; }

	IROP			m_irop;
	NPRED			m_npred;
	GPRED			m_gpred;
	bool			m_fNegateFirst;
	const char *	m_pChzName;
};

void CreateOpinfo(IROP irop, const char * pChzName, OperatorInfo * pOpinfo)
{
	pOpinfo->m_irop = irop;
	pOpinfo->m_pChzName = pChzName;
}

void CreateOpinfo(NPRED npred, const char * pChzName, OperatorInfo * pOpinfo)
{
	pOpinfo->m_irop = IROP_NCmp;
	pOpinfo->m_npred = npred;
	pOpinfo->m_pChzName = pChzName;
}

void CreateOpinfo(GPRED gpred, const char * pChzName, OperatorInfo * pOpinfo)
{
	pOpinfo->m_irop = IROP_GCmp;
	pOpinfo->m_gpred = gpred;
	pOpinfo->m_pChzName = pChzName;
}

static void GenerateOperatorInfo(TOK tok, const OpTypes * pOptype, OperatorInfo * pOpinfo)
{
	TypeInfo * apTin[2] = {PTinStripQualifiers(pOptype->m_pTinLhs), PTinStripQualifiers(pOptype->m_pTinRhs)};
	TINK aTink[2];
	NUMK aNumk[2];

	for (int iOperand = 0; iOperand < 2; ++iOperand)
	{
		NUMK numk;
		TINK tink = apTin[iOperand]->m_tink;

		if (tink == TINK_Literal)
		{
			TypeInfoLiteral * pTinlit = (TypeInfoLiteral*)apTin[iOperand];
			numk = pTinlit->m_litty.m_numk;

			switch (pTinlit->m_litty.m_litk)
			{
			case LITK_Numeric:	tink = TINK_Numeric;	break;
			case LITK_Enum:		tink = TINK_Enum;		break;
			case LITK_Bool:		tink = TINK_Bool;		break;
			case LITK_Compound:
			{
				TINK tinkSource = (pTinlit->m_pTinSource) ? pTinlit->m_pTinSource->m_tink : TINK_Nil;
				MOE_ASSERT(tinkSource == TINK_Array || tinkSource == TINK_Struct, 
					"unexpected compound literal type kind '%s'", PChzFromTink(tinkSource));
				tink = tinkSource;
				break;
			} 
			default:
				tink = TINK_Nil;
			}
		}
		else if (tink == TINK_Enum)
		{
			auto pTinenum = (TypeInfoEnum*)apTin[iOperand];
			auto pTinnLoose = PTinRtiCast<TypeInfoNumeric*>(pTinenum->m_pTinLoose);
			if (MOE_FVERIFY(pTinnLoose && FIsInteger(pTinnLoose->m_numk), "expected integer loose type"))
			{
				numk = pTinnLoose->m_numk;
			}
		}
		else if (tink == TINK_Numeric)
		{
			numk = ((TypeInfoNumeric*)apTin[iOperand])->m_numk;
		}
		
		aNumk[iOperand] = numk;
		aTink[iOperand] = tink;
	}

	if (aTink[0] != aTink[1])
	{
		int nMin = (aTink[0] > aTink[1]) ? 1 : 0;

		TINK tinkMin = aTink[nMin];
		TINK tinkMax = aTink[!nMin];
		bool fIsIntegerMin = tinkMin == TINK_Numeric && FIsInteger(aNumk[nMin]);

		if (tinkMin == TINK_Pointer && tinkMax == TINK_Array)
		{
			// BB- check that it's a pointer to the array type.
			switch((u32)tok)
			{
			case '=':
				{
					CreateOpinfo(IROP_Store, "store", pOpinfo);
				} break;
			case '-': 				
				{
					CreateOpinfo(IROP_GEP, "ptrSub", pOpinfo);
					pOpinfo->m_fNegateFirst = true;
				} break;
			case TOK_EqualEqual:
				CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); 
				break;
			case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
			}
		}
		else if (fIsIntegerMin && tinkMax == TINK_Array)
		{
			switch((u32)tok)
			{
			case '+':				
				{
					CreateOpinfo(IROP_GEP, "ptrAdd", pOpinfo);
				} break;
			case '-': 				
				{
					CreateOpinfo(IROP_GEP, "ptrSub", pOpinfo);
					pOpinfo->m_fNegateFirst = true;
				} break;
			}
		}
		else if (fIsIntegerMin && tinkMax == TINK_Pointer)
		{
			switch((u32)tok)
			{
			case '+':				
				{
					CreateOpinfo(IROP_GEP, "ptrAdd", pOpinfo);
				} break;
			case '-': 				
				{
					CreateOpinfo(IROP_GEP, "ptrSub", pOpinfo);
					pOpinfo->m_fNegateFirst = true;
				} break;
			case TOK_PlusEqual:
				{
					CreateOpinfo(IROP_GEP, "ptrAdd", pOpinfo);
				} break;
			case TOK_MinusEqual:
				{
					CreateOpinfo(IROP_GEP, "ptrSub", pOpinfo);
					pOpinfo->m_fNegateFirst = true;
				} break;
			}
		}
		else if (fIsIntegerMin && tinkMax == TINK_Enum)
		{
			//bool fIsSigned = (aTink[0] == tinkMax) ? aFIsSigned[0] : aFIsSigned[1];
			bool fIsSigned = FIsSigned(aNumk[nMin]);
			switch (tok)
			{
				case TOK_ShiftRight:	// NOTE: AShr = arithmetic shift right (sign fill), LShr == zero fill
										CreateOpinfo((fIsSigned) ? IROP_AShr : IROP_LShr, "nShrTmp", pOpinfo); break;
				case TOK_ShiftLeft:		CreateOpinfo(IROP_Shl, "nShlTmp", pOpinfo); break;
				case TOK_AndEqual:
				case '&':				CreateOpinfo(IROP_And, "rAndTmp", pOpinfo); break;
				case TOK_OrEqual:
				case '|':				CreateOpinfo(IROP_Or, "nOrTmp", pOpinfo); break;
				case TOK_XorEqual:
				case '^':				CreateOpinfo(IROP_Xor, "nXorTmp", pOpinfo); break;
				default:
					MOE_ASSERT(false, "Unhandled TOK");
					break;
			}
		}

		return;
	}

	TINK tink = aTink[0];
	//bool fIsSigned = aFIsSigned[0];
	bool fIsSigned = FIsSigned(aNumk[0]);

	switch (tink)
	{
	case TINK_Flag:
		switch ((u32)tok)
		{
			case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
			case TOK_EqualEqual:	CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
			case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
			case TOK_AndAnd:		CreateOpinfo(IROP_Phi, "Phi", pOpinfo); break;	// only useful for FDoesOperatorExist, codegen is more complicated
			case TOK_OrOr:			CreateOpinfo(IROP_Phi, "Phi", pOpinfo); break;	// only useful for FDoesOperatorExist, codegen is more complicated
		} break;
	case TINK_Bool:
		switch ((u32)tok)
		{
			case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
			case TOK_EqualEqual:	CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
			case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
			case TOK_AndEqual:
			case '&':				CreateOpinfo(IROP_And, "nAndTmp", pOpinfo); break;
			case TOK_OrEqual:
			case '|':				CreateOpinfo(IROP_Or, "nOrTmp", pOpinfo); break;
			case TOK_AndAnd:		CreateOpinfo(IROP_Phi, "Phi", pOpinfo); break;	// only useful for FDoesOperatorExist, codegen is more complicated
			case TOK_OrOr:			CreateOpinfo(IROP_Phi, "Phi", pOpinfo); break;	// only useful for FDoesOperatorExist, codegen is more complicated
		} break;
	case TINK_Numeric:
		{
			if (aNumk[0] == NUMK_Float)
			{
				switch ((u32)tok)
				{
					case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
					case TOK_PlusEqual:
					case '+': 				CreateOpinfo(IROP_GAdd, "gAddTmp", pOpinfo); break;
					case TOK_MinusEqual:
					case '-': 				CreateOpinfo(IROP_GSub, "SubTmp", pOpinfo); break;
					case TOK_MulEqual:
					case '*': 				CreateOpinfo(IROP_GMul, "gMulTmp", pOpinfo); break;
					case TOK_DivEqual:
					case '/': 				CreateOpinfo(IROP_GDiv, "gDivTmp", pOpinfo); break;
					case TOK_ModEqual:
					case '%': 				CreateOpinfo(IROP_GRem, "gRemTmp", pOpinfo); break;
					case TOK_EqualEqual:	CreateOpinfo(GPRED_EQ, "GCmpEQ", pOpinfo); break;
					case TOK_NotEqual:		CreateOpinfo(GPRED_NE, "GCmpNE", pOpinfo); break;
					case TOK_LessEqual:		CreateOpinfo(GPRED_LE, "GCGpLE", pOpinfo); break;
					case TOK_GreaterEqual:	CreateOpinfo(GPRED_GE, "GCmpGE", pOpinfo); break;
					case '<': 				CreateOpinfo(GPRED_LT, "GCmpLT", pOpinfo); break;
					case '>': 				CreateOpinfo(GPRED_GT, "GCmpGT", pOpinfo); break;
				}
			}
			else // is integer
			{
				switch ((u32)tok)
				{
					case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
					case TOK_PlusEqual:
					case '+': 				CreateOpinfo(IROP_NAdd, "nAddTmp", pOpinfo); break;
					case TOK_MinusEqual:
					case '-': 				CreateOpinfo(IROP_NSub, "nSubTmp", pOpinfo); break;
					case TOK_MulEqual:
					case '*': 				CreateOpinfo(IROP_NMul, "nMulTmp", pOpinfo); break;
					case TOK_DivEqual:
					case '/':				CreateOpinfo((fIsSigned) ? IROP_SDiv : IROP_UDiv, "nDivTmp", pOpinfo); break;
					case TOK_ModEqual:
					case '%':				CreateOpinfo((fIsSigned) ? IROP_SRem : IROP_URem, "nRemTmp", pOpinfo); break;
					case TOK_AndEqual:
					case '&':				CreateOpinfo(IROP_And, "rAndTmp", pOpinfo); break;
					case TOK_OrEqual:
					case '|':				CreateOpinfo(IROP_Or, "nOrTmp", pOpinfo); break;
					case TOK_XorEqual:
					case '^':				CreateOpinfo(IROP_Xor, "nXorTmp", pOpinfo); break;
					case TOK_ShiftRight:	// NOTE: AShr = arithmetic shift right (sign fill), LShr == zero fill
											CreateOpinfo((fIsSigned) ? IROP_AShr : IROP_LShr, "nShrTmp", pOpinfo); break;
					case TOK_ShiftLeft:		CreateOpinfo(IROP_Shl, "nShlTmp", pOpinfo); break;
					case TOK_EqualEqual:	CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
					case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
					case TOK_LessEqual:
						if (fIsSigned)	CreateOpinfo(NPRED_SLE, "CmpSLE", pOpinfo);
						else			CreateOpinfo(NPRED_ULE, "NCmpULE", pOpinfo);
						break;
					case TOK_GreaterEqual:
						if (fIsSigned)	CreateOpinfo(NPRED_SGE, "NCmpSGE", pOpinfo);
						else			CreateOpinfo(NPRED_UGE, "NCmpUGE", pOpinfo);
						break;
					case '<':
						if (fIsSigned)	CreateOpinfo(NPRED_SLT, "NCmpSLT", pOpinfo);
						else			CreateOpinfo(NPRED_ULT, "NCmpULT", pOpinfo);
						break;
					case '>':
						if (fIsSigned)	CreateOpinfo(NPRED_SGT, "NCmpSGT", pOpinfo);
						else			CreateOpinfo(NPRED_UGT, "NCmpUGT", pOpinfo);
						break;
				}
			}

		} break;
	case TINK_Procedure:
		{
			switch ((u32)tok)
			{
				case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
				case TOK_EqualEqual:	CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
				case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
			}
		} break;
	case TINK_Struct:
		{
			switch ((u32)tok)
			{
				case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
			}
		} break;
	case TINK_Pointer:
	case TINK_Array:
		{
			switch ((u32)tok)
			{
				case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
				case '-': 			
					{
						CreateOpinfo(IROP_PtrDiff, "ptrDif", pOpinfo);
					} break;
				case TOK_EqualEqual:	CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
				case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
/*				case TOK_PlusEqual:		CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
				case TOK_MinusEqual:	CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
				case '+=':				CreateOpinfo(IROP_GEP, "ptrAdd", pOpinfo); break;
				case '-=': 				
					{
						CreateOpinfo(IROP_GEP, "ptrSub", pOpinfo);
						pOpinfo->m_fNegateFirst = true;
					} break;
					*/
			}
		} break;
	case TINK_Enum:
		{
			auto pTinenum = PTinRtiCast<TypeInfoEnum *>(apTin[0]);
			if (pTinenum->m_enumk == ENUMK_Basic)
			{
				// BB - why is the RHS still a literal here?
				//MOE_ASSERT(FTypesAreSame(pTinLhs, pTinRhs), "enum comparison type mismatch");

				// BB - Why no plus equals here?

				switch ((u32)tok)
				{
				case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
				case TOK_EqualEqual:	CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
				case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
				case '+': 				CreateOpinfo(IROP_NAdd, "nAddTmp", pOpinfo); break;
				case '-': 				CreateOpinfo(IROP_NSub, "nSubTmp", pOpinfo); break;
				case '%':				CreateOpinfo((fIsSigned) ? IROP_SRem : IROP_URem, "nRemTmp", pOpinfo); break;
				case TOK_ShiftRight:	// NOTE: AShr = arithmetic shift right (sign fill), LShr == zero fill
					CreateOpinfo((fIsSigned) ? IROP_AShr : IROP_LShr, "nShrTmp", pOpinfo); break;
				case TOK_ShiftLeft:		CreateOpinfo(IROP_Shl, "nShlTmp", pOpinfo); break;
				case TOK_LessEqual:
					if (fIsSigned)		CreateOpinfo(NPRED_SLE, "CmpSLE", pOpinfo);
					else				CreateOpinfo(NPRED_ULE, "NCmpULE", pOpinfo);
					break;
				case TOK_GreaterEqual:
					if (fIsSigned)		CreateOpinfo(NPRED_SGE, "NCmpSGE", pOpinfo);
					else				CreateOpinfo(NPRED_UGE, "NCmpUGE", pOpinfo);
					break;
				case '<':
					if (fIsSigned)		CreateOpinfo(NPRED_SLT, "NCmpSLT", pOpinfo);
					else				CreateOpinfo(NPRED_ULT, "NCmpULT", pOpinfo);
					break;
				case '>':
					if (fIsSigned)		CreateOpinfo(NPRED_SGT, "NCmpSGT", pOpinfo);
					else				CreateOpinfo(NPRED_UGT, "NCmpUGT", pOpinfo);
					break;
				}
			}
			else
			{
				MOE_ASSERT(pTinenum->m_enumk == ENUMK_FlagEnum, "Unhandled enumk");

				switch ((u32)tok)
				{
				case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
				case TOK_EqualEqual:	CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
				case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
				case TOK_ShiftRight:	// NOTE: AShr = arithmetic shift right (sign fill), LShr == zero fill
					CreateOpinfo((fIsSigned) ? IROP_AShr : IROP_LShr, "nShrTmp", pOpinfo); break;
				case TOK_ShiftLeft:		CreateOpinfo(IROP_Shl, "nShlTmp", pOpinfo); break;
				case TOK_AndEqual:
				case '&':				CreateOpinfo(IROP_And, "rAndTmp", pOpinfo); break;
				case TOK_OrEqual:
				case '|':				CreateOpinfo(IROP_Or, "nOrTmp", pOpinfo); break;
				case TOK_XorEqual:
				case '^':				CreateOpinfo(IROP_Xor, "nXorTmp", pOpinfo); break;
				}
			}
		} break;
	default: 
		break;
	}
}

bool FDoesOperatorExist(TOK tok, const OpTypes * pOptype)
{
	OperatorInfo opinfo;
	GenerateOperatorInfo(tok, pOptype, &opinfo);

	return opinfo.m_irop != IROP_Nil;
}
