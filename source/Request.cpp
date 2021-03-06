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
:m_pAlloc(pAlloc)
,m_aryRqsrc(pAlloc, BK_Compilation, 0)
,m_aryRq(pAlloc, BK_Compilation, 0)
,m_arypRqres(pAlloc, BK_Compilation, 0)
,m_arypJobQueued(pAlloc, BK_Compilation, 0)
{
}

void AddRequestSymbol(Request * pRq, RQK rqk, Moe::InString istrSymbolPath)
{
	pRq->m_rqk = rqk;
	pRq->m_istrPath = istrSymbolPath;
}

void AddRequestLocation(Request * pRq, RQK rqk, Moe::InString istrFilename, s32 iLine, s32 iCodepoint)
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
		switch(pRqsrc->m_rqsrck)
		{
		case RQSRCK_Filename:
			{
				Workspace::File * pFile = pWork->PFileEnsure(pRqsrc->m_istr, Workspace::FILEK_Source);

				char aChFilenameOut[Workspace::s_cBFilenameMax];
				(void)Moe::CChConstructFilename(pFile->m_istrFilename.m_pChz, Workspace::s_pChzSourceExtension, aChFilenameOut, MOE_DIM(aChFilenameOut));

				pFile->m_pChzFileBody = pWork->PChzLoadFile(IstrInternCopy(aChFilenameOut), pWork->m_pAlloc);
			} break;
		case RQSRCK_SourceText:
			{
				Workspace::File * pFile = pWork->PFileEnsure(IstrIntern("-src"), Workspace::FILEK_Source);

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

		COMPHASE comphase = COMPHASE_TypeCheck;
		JobRef pJobParse = PJobCreateParse(pComp, pWork, pChzFileBody, pFile->m_istrFilename, comphase);

		WaitForJob(pComp, pWork, pJobParse.m_pJob);

		BlockListEntry::CIterator iter(&pWork->m_blistEntry);
		while (WorkspaceEntry * pEntry = iter.Next())
		{
			InString istrParse = IstrSExpression(pEntry->m_pStnod, SEWK_Parse);
			printf("parse : %s\n", istrParse.m_pChz);
		
			InString istrTypeCheck = IstrSExpression(pEntry->m_pStnod, SEWK_TypeInfo);
			printf("tc    : %s\n", istrTypeCheck.m_pChz);

		//	InString istrPark = IstrSExpression(pEntry->m_pStnod, SEWK_Park);
		//	printf("parse: %s\n", istrPark.m_pChz);
		}

		int cError, cWarning;
		pWork->m_pErrman->ComputeErrorCounts(&cError, &cWarning);
		if (cError != 0)
		{
			ConsoleColorAmbit ccolamb;
			SetConsoleTextColor(GRFCCOL_FgIntenseRed);

			printf("--- Compile FAILED: %d errors, %d warnings ---\n", cError, cWarning);
		}
		else
		{
			ConsoleColorAmbit ccolamb;
			SetConsoleTextColor(GRFCCOL_FgIntenseYellow);

			printf("+++ Success: 0 errors, %d warnings +++\n", cWarning);
		}

	}

	for (size_t ipFile = 0; ipFile < pWork->m_arypFile.C(); ++ipFile)
	{
		Workspace::File * pFile = pWork->m_arypFile[ipFile];
		if (pFile->m_pChzFileBody)
		{
			pWork->m_pAlloc->MOE_FREE((u8 *)pFile->m_pChzFileBody);
			pFile->m_pChzFileBody = nullptr;
		}
	}

	return 0;
}

void PrintResult(Compilation * pComp, int iRqres, char * aCh, size_t cChMax)
{
}

Job * PJobAllocate(Compilation * pComp, void * pVData, Job * pJobParent)
{
	if (pJobParent)
	{
        (pJobParent->m_cJobWaiting)++;
	}

	Job * pJob = MOE_NEW_BK(pComp->m_pAlloc, BK_Job, Job) Job(pComp->m_pAlloc);
	pJob->m_pVData = pVData;
	pJob->m_cJobWaiting = 1;
	pJob->m_pJobParent = pJobParent;
	PRINT_REFC("allocateJob %p %d\n", pJob, pJob->m_cRef);

	return pJob;
}

void DeleteJob(Job * pJob)
{
	PRINT_REFC("deleteJob %p\n", pJob);
	MOE_ASSERT(pJob->m_cRef == 0, "deleting with nonzero ref count");
	Alloc * pAlloc = pJob->m_pAlloc;
	pAlloc->MOE_DELETE(pJob);
}

void EnqueueJob(Compilation * pComp, Job * pJob)
{
	pComp->m_arypJobQueued.Append(pJob);
}

void FinishJob(Workspace * pWork, Job * pJob)
{
    s32 cJobWaiting = --(pJob->m_cJobWaiting);
	MOE_ASSERT(cJobWaiting >= 0, "job count underflow");

	Job * pJobIt = pJob;
	while (cJobWaiting <= 0)
	{
		if (pJobIt->m_pFnCleanup)
		{
			(*pJobIt->m_pFnCleanup)(pWork, pJobIt);
		}

		{
			Job * pJobPrev  = pJobIt;
			pJobIt = pJobIt->m_pJobParent.m_pJob;
			pJobPrev->m_pJobParent = nullptr;
		}

		if (!pJobIt)
			break;

		cJobWaiting = --(pJobIt->m_cJobWaiting);
		MOE_ASSERT(cJobWaiting >= 0, "job count underflow");
	}
}

bool FIsJobADescendent(Job * pJobDescend, Job * pJobAncestor)
{
	Job * pJobIt = pJobDescend;
	while (pJobIt)
	{
		if (pJobIt == pJobAncestor)
			return true;
		pJobIt = pJobIt->m_pJobParent.m_pJob;
	}
	return true;
}

void WaitForJob(Compilation * pComp, Workspace * pWork, Job * pJobWait)
{
	// wait until the job has completed. in the meantime, work on any other job.

	while (!pJobWait->FHasJobCompleted())
	{
		if (pComp->m_arypJobQueued.FIsEmpty())
		{
			// brute force find the jobs or symbols we're waiting on. We need a better way to find this.
			bool fWasReported = false;
			const Symbol ** ppSym;
			JobPrereqSet * pJps;
			Moe::CHash<const Symbol *, JobPrereqSet>::CIterator iterJps(&pWork->m_hashPSymJps);
			while (pJps = iterJps.Next(&ppSym))
			{
				bool fIsDependent = false;
				JobRef * ppJobMax = pJps->m_arypJob.PMac();
				for (JobRef * ppJobIt = pJps->m_arypJob.A(); ppJobIt != ppJobMax; ++ppJobIt)
				{
					if (FIsJobADescendent(ppJobIt->m_pJob, pJobWait))
					{
						fIsDependent = true;
						break;
					}
				}

				if (fIsDependent)
				{
					for (JobRef * ppJobIt = pJps->m_arypJob.A(); ppJobIt != ppJobMax; ++ppJobIt)
					{
						FinishJob(pWork, ppJobIt->m_pJob);
					}
					pJps->m_arypJob.Clear();

					LexSpan lexsp = ((*ppSym)->m_pStnodDefinition) ? (*ppSym)->m_pStnodDefinition->m_lexsp : LexSpan();
					EmitError(pWork, lexsp, ERRID_CircularDepend, 
						"circular reference to unresolved symbol '%s'", (*ppSym)->m_istrName.PChz());
					fWasReported = true;	
				}
			}

			if (!fWasReported)
			{
				EmitError(pWork, pJobWait->m_jobInfo.m_lexsp, ERRID_CircularDepend, 
					"circular dependency with job '%s'", 
					((pJobWait->m_jobInfo.m_pChzName) ? pJobWait->m_jobInfo.m_pChzName : "unnamed"));
			}
			return;
		}

		JobRef pJob = pComp->m_arypJobQueued.TPopLast();

		if (!pJob->m_fUpdateComplete)
		{
			JOBRET jobret = pJob->m_pFnUpdate(pComp, pWork, pJob.m_pJob);

			if (jobret == JOBRET_Complete)
			{
				pJob->m_fUpdateComplete = true;
				FinishJob(pWork, pJob.m_pJob);
			}	
		}
	}
}

