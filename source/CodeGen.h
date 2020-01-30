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

#pragma once

#include "Lexer.h"
#include "MoeArray.h"


#define NPRED_LIST \
	MOE_PRED(EQ)  LLVM_PRED(LLVMIntEQ) \
	MOE_PRED(NE)  LLVM_PRED(LLVMIntNE) \
	MOE_PRED(UGT) LLVM_PRED(LLVMIntUGT) \
	MOE_PRED(UGE) LLVM_PRED(LLVMIntUGE) \
	MOE_PRED(ULT) LLVM_PRED(LLVMIntULT) \
	MOE_PRED(ULE) LLVM_PRED(LLVMIntULE) \
	MOE_PRED(SGT) LLVM_PRED(LLVMIntSGT) \
	MOE_PRED(SGE) LLVM_PRED(LLVMIntSGE) \
	MOE_PRED(SLT) LLVM_PRED(LLVMIntSLT) \
	MOE_PRED(SLE) LLVM_PRED(LLVMIntSLE)

#define GPRED_LIST \
	MOE_PRED(EQ) LLVM_PRED(LLVMRealOEQ) \
	MOE_PRED(GT) LLVM_PRED(LLVMRealOGT) \
	MOE_PRED(GE) LLVM_PRED(LLVMRealOGE) \
	MOE_PRED(LT) LLVM_PRED(LLVMRealOLT) \
	MOE_PRED(LE) LLVM_PRED(LLVMRealOLE) \
	MOE_PRED(NE) LLVM_PRED(LLVMRealONE) \

#define MOE_PRED(X) GPRED_##X,
#define LLVM_PRED(X)
enum GPRED
{
	GPRED_LIST

	GPRED_Max,
	GPRED_Min = 0,
	GPRED_Nil = -1,
};
#undef MOE_PRED
#undef LLVM_PRED

#define MOE_PRED(X) NPRED_##X,
#define LLVM_PRED(X)
enum NPRED
{
	NPRED_LIST

	NPRED_Max,
	NPRED_Min = 0,
	NPRED_Nil = -1,
};
#undef MOE_PRED
#undef LLVM_PRED

const char * PChzFromGpred(GPRED gpred);
const char * PChzFromNpred(NPRED npred);


enum OPSZ
{
	OPSZ_0,
	OPSZ_1,
	OPSZ_2,
	OPSZ_4,
	OPSZ_8,
	OPSZ_CB,
	OPSZ_PCB,	// stack index of pointer to a value cB in size
	OPSZ_Ptr,
	OPSZ_RegIdx,
};

enum CBSRC
{
	CBSRC_Lhs,
	CBSRC_Rhs,
	CBSRC_Nil = -1
};

struct OpSignature // tag = opsig
{
	OPSZ	m_opszLhs;
	OPSZ	m_opszRhs;
	OPSZ	m_opszRet;
	CBSRC	m_cbsrc;
};

#define OPCODE_LIST \
		OPMN(Terminal,	Error)		OPSIZE(0, 0, 0) \
						/* Ret(cBStack+cBArg) -> regRet */ \
		OPMX(Terminal,	Ret)		OPSIZE(4, 4, 0) \
		\
						/* Call(pProcNew|pFnForeign, pProcsig) -> regRet */ \
						/*  variadic: ExArgs(cArgVariadic, cBVariadic) */ \
		OPMN(JumpOp,	Call)		OPSIZE(Ptr, Ptr, CB) \
						/* CondBranch(fPred, {iInstT,iInstF}) */ \
		OP(				CondBranch)	OPSIZE(1, 8, 0) \
						/* Branch(0, iInst) */ \
		OP(				Branch)		OPSIZE(0, 0, 0) \
						/* Switch(Value, iInstElse) ExArgs(CmpValue, iInstBranch) */ \
		OP(				Switch)		OPSIZE(CB, 0, 0) \
						/* Phi(Value, iInstSrc,)->iBStack ExArgs(Value, iInstSrc) */ \
		OPMX(JumpOp,	Phi)		OPSIZE(CB, 0, 0) \
		\
		OPMN(BinaryOp,	NAdd)		OPSIZE(CB, CB, CB) \
		OP(				GAdd)		OPSIZE(CB, CB, CB) \
		OP(				NSub)		OPSIZE(CB, CB, CB) \
		OP(				GSub)		OPSIZE(CB, CB, CB) \
		OP(				NMul)		OPSIZE(CB, CB, CB) \
		OP(				GMul)		OPSIZE(CB, CB, CB) \
		OP(				SDiv)		OPSIZE(CB, CB, CB) \
		OP(				UDiv)		OPSIZE(CB, CB, CB) \
		OP(				GDiv)		OPSIZE(CB, CB, CB) \
		OP(				SRem)		OPSIZE(CB, CB, CB) \
		OP(				URem)		OPSIZE(CB, CB, CB) \
		OPMX(BinaryOp,	GRem)		OPSIZE(CB, CB, CB) \
		\
		OPMN(UnaryOp,	NNeg)		OPSIZE(CB, 0, CB) \
		OP(				GNeg)		OPSIZE(CB, 0, CB) \
		OP(				Not)		OPSIZE(CB, 0, CB) \
		OPMX(UnaryOp,	FNot)		OPSIZE(CB, 0, CB) \
		\
		OPMN(CmpOp,		NCmp)		OPSIZE(CB, CB, 1) \
		OPMX(CmpOp,		GCmp)		OPSIZE(CB, CB, 1) \
		\
		OPMN(LogicOp,	Shl)		OPSIZE(CB, CB, CB) \
		OP(				AShr)		OPSIZE(CB, CB, CB) \
		OP(				LShr)		OPSIZE(CB, CB, CB) \
		OP(				And)		OPSIZE(CB, CB, CB) \
		OP(				Or)			OPSIZE(CB, CB, CB) \
		OPMX(LogicOp,	Xor)		OPSIZE(CB, CB, CB) \
		\
						/* Alloca(iBStackResult, pTinDebug)->iBStack(ref) */ \
		OPMN(MemoryOp,	Alloca)		OPSIZE(RegIdx, Ptr, PCB) \
						/* Load(Reg(Pointer)) -> RegIdx */ \
		OP(				Load)		OPSIZE(PCB, 0, CB) \
						/* Store(Src, cBOperand)->iBStackDest */ \
		OP(				Store)		OPSIZE(CB, 4, 0) \
						/* GEP(Reg(Pointer), dBOffset)->iBStack  ExArgs(val, cBStride) ...*/ \
		OP(				GEP)		OPSIZE(Ptr, 8, Ptr) \
		OP(				PtrDiff)	OPSIZE(0, 0, 0) \
						/* Memset(pDst, valByte);  ExArgs(cB)*/ \
		OP(				Memset)		OPSIZE(RegIdx, 1, 0) \
						/* Memcpy(pDst, pSrc);  ExArgs(cB)*/ \
		OPMX(MemoryOp,	Memcpy)		OPSIZE(RegIdx, RegIdx, 0) \
		\
						/* CastOp(reg, cBOperandRhs); */ \
		OPMN(CastOp,	NTrunc)		OPSIZE(0, 8, CB) \
		OP(				SignExt)	OPSIZE(0, 8, CB) \
		OP(				ZeroExt)	OPSIZE(0, 8, CB) \
		OP(				GToS)		OPSIZE(0, 8, CB) \
		OP(				GToU)		OPSIZE(0, 8, CB) \
		OP(				SToG)		OPSIZE(0, 8, CB) \
		OP(				UToG)		OPSIZE(0, 8, CB) \
		OP(				GTrunc)		OPSIZE(0, 8, CB) \
		OP(				GExtend)	OPSIZE(0, 8, CB) \
		OP(				PtrToInt)	OPSIZE(0, 8, CB) \
		OP(				IntToPtr)	OPSIZE(0, 8, CB) \
		OPMX(CastOp,	Bitcast)	OPSIZE(0, 8, CB) \
		/* ---- bytecode only opcodes ----*/ \
						/* TraceStore(Reg) */ \
		OP(				TraceStore)	OPSIZE(8, Ptr, 0) \
						/* StoreToReg(Reg)->iBStackDest */ \
		OP(				StoreToReg)	OPSIZE(CB, 4, 0) \
						/* StoreToIdx(Reg)->iBStackDest */ \
		OP(				StoreToIdx)	OPSIZE(CB, 4, 0) \
						/* StoreAddress(RegIdx) ->iBStack */ \
		OP(				StoreAddress)	OPSIZE(RegIdx, 0, Ptr) \
						/* extra arguments for preceeding opcode */ \
		OPMX(BCodeOp,	ExArgs)	OPSIZE(0, 0, 0) \



#define OPSIZE(A, B, RET) 
	enum IROP : s8
	{
#define OP(X) IROP_##X,
#define OPMN(RANGE, X) IROP_##X,
#define OPMX(RANGE, X) IROP_##X,
		OPCODE_LIST
#undef OPMN
#undef OPMX
#undef OP

		IROP_Max,
		IROP_Min = 0,
		IROP_Nil = -1,

		// Add the range values in a second pass over OPCODE_LIST so that the debugger shows values rather than range endpoints
#define OP(X)
#define OPMN(RANGE, X) IROP_##RANGE##Min = IROP_##X,
#define OPMX(RANGE, X) IROP_##RANGE##Max = IROP_##X + 1,
		OPCODE_LIST
#undef OPMX
#undef OPMN
#undef OP
	};
#undef OPSIZE


bool FDoesOperatorExist(TOK tok, const OpTypes * pOptype);





