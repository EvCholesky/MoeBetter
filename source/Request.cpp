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

#include "Request.h"

namespace Moe
{

Compilation::Compilation(Moe::Alloc * pAlloc)
:m_aryRqsrc(pAlloc, BK_Request)
,m_aryRq(pAlloc, BK_Request)
,m_arypRqres(pAlloc, BK_Request)
{
}

void RequestSymbol(Request * pRq, RQK rqk, Moe::InString istrSymbolPath)
{
	pRq->m_rqk = rqk;
	pRq->m_istrPath = istrSymbolPath;
}

void RequestLocation(Request * pRq, RQK rqk, Moe::InString istrFilename, s32 iLine, s32 iCodepoint)
{
	pRq->m_rqk = rqk;
	pRq->m_istrPath = istrFilename;
	pRq->m_iLine = iLine;
	pRq->m_iCodepoint = iCodepoint;
}

void AddRequest(Compilation * pComp, Request * pRq)
{
	pComp->m_aryRq.Append(*pRq);
}

void AddSourceFile(Compilation * pComp, const char * pChzFilename)
{
	auto pRqsrc = pComp->m_aryRqsrc.AppendNew();
	pRqsrc->m_rqsrck = RQSRCK_Filename;
	pRqsrc->m_istr = pChzFilename;
}

int CRqresServiceRequest(Workspace * pWork, Compilation * pComp)
{
	return 0;
}

void PrintResult(Compilation * pComp, int iRqres, char * aCh, size_t cChMax)
{
}

}
