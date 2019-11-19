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

#include "Lexer.h"
#include "Parser.h"
#include "Request.h"
#include "Workspace.h"
#include <stdio.h>

using namespace Moe;

Compilation::Compilation(Moe::Alloc * pAlloc)
:m_aryRqsrc(pAlloc, BK_Request)
,m_aryRq(pAlloc, BK_Request)
,m_arypRqres(pAlloc, BK_Request)
,m_aryJob(pAlloc, BK_Request, 128)
,m_arypJobQueued(pAlloc, BK_Request, 128)
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
	pRqsrc->m_istr = IstrInternCopy(pChzFilename);
}

void AddSourceText(Compilation * pComp, const char * pChz)
{
	auto pRqsrc = pComp->m_aryRqsrc.AppendNew();
	pRqsrc->m_rqsrck = RQSRCK_SourceText;
	pRqsrc->m_istr = IstrInternCopy(pChz);
}

int CRqresServiceRequest(Compilation * pComp, Workspace * pWork)
{
	auto pRqsrcMax = pComp->m_aryRqsrc.PMac();
	for (auto pRqsrc = pComp->m_aryRqsrc.A(); pRqsrc != pRqsrcMax; ++pRqsrc)
	{
		Workspace::File * pFile = pWork->PFileEnsure(pRqsrc->m_istr, Workspace::FILEK_Source);

		switch(pRqsrc->m_rqsrck)
		{
		case RQSRCK_Filename:
			{
				char aChFilenameOut[Workspace::s_cBFilenameMax];
				(void)Moe::CChConstructFilename(pFile->m_istrFilename.m_pChz, Workspace::s_pChzSourceExtension, aChFilenameOut, MOE_DIM(aChFilenameOut));

				pFile->m_pChzFileBody = pWork->PChzLoadFile(IstrInternCopy(aChFilenameOut), pWork->m_pAlloc);
			} break;
		case RQSRCK_SourceText:
			{
				auto cB = pRqsrc->m_istr.CB();
				char * pChzCopy = (char *)pWork->m_pAlloc->MOE_ALLOC(cB, 1);
				CBCopyChz(pRqsrc->m_istr.m_pChz, pChzCopy, cB);

				pFile->m_pChzFileBody = pChzCopy;
			} break;
		default: 
			MOE_ASSERT(false, "unknown request source kind (%d)", pRqsrc->m_rqsrck);  
			break;
		}
	}

	Lexer lex;
	for (size_t ipFile = 0; ipFile < pWork->m_arypFile.C(); ++ipFile)
	{
		Workspace::File * pFile = pWork->m_arypFile[ipFile];
		if (pFile->m_filek != Workspace::FILEK_Source)
			continue;

		if (!pFile->m_pChzFileBody)
			continue;

		const char * pChzFileBody = Moe::PChzSkipUnicodeBOM(pFile->m_pChzFileBody);

		//printf("Parsing %s\n", pFile->m_qqistrFilename.m_pChz);

		auto pJobParse = PJobCreateParse(pComp, pWork, pChzFileBody, pFile->m_istrFilename);
		WaitForJob(pComp, pWork, pJobParse);

		BlockListEntry::CIterator iter(&pWork->m_blistEntry);
		while (WorkspaceEntry * pEntry = iter.Next())
		{
			pEntry->m_pStnod;
			InString istrParse = IstrSExpression(pEntry->m_pStnod, SEWK_Parse);
			printf("     : %s\n", istrParse.m_pChz);

			InString istrPark = IstrSExpression(pEntry->m_pStnod, SEWK_Park);
			printf("parse: %s\n", istrPark.m_pChz);
		}
	}

	return 0;
}

void PrintResult(Compilation * pComp, int iRqres, char * aCh, size_t cChMax)
{
}

Job * PJobAllocate(Compilation * pComp, void * pVData)
{
	Job * pJob = pComp->m_aryJob.AppendNew();
	pJob->m_pVData = pVData;
	return pJob;
}

void EnqueueJob(Compilation * pComp, Job * pJob)
{
	pJob->m_cJobUnfinished = 1;
	pComp->m_arypJobQueued.Append(pJob);
}

bool FHasJobCompleted(const Job * pJob)
{
	return pJob->m_cJobUnfinished <= 0;
}

Job * PJobGet(Compilation * pComp)
{
	if (pComp->m_arypJobQueued.FIsEmpty())
		return nullptr;

	return pComp->m_arypJobQueued.TPopLast();
}

void FinishJob(Compilation * pComp, Job * pJob)
{
    const s32 cJobUnfinished = --(pJob->m_cJobUnfinished);
    MOE_ASSERT(cJobUnfinished >= 0);
}

void WaitForJob(Compilation * pComp, Workspace * pWork, Job * pJob)
{
	// wait until the job has completed. in the meantime, work on any other job.

	while (!FHasJobCompleted(pJob))
	{
		Job * pJobjNext = PJobGet(pComp);
		if (pJobjNext)
		{
			pJob->m_pFnUpdate(pComp, pWork, pJob);
			FinishJob(pComp, pJob);
		}
	}
}

