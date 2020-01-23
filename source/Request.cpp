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
,m_aryRqsrc(pAlloc, BK_Request)
,m_aryRq(pAlloc, BK_Request)
,m_arypRqres(pAlloc, BK_Request)
,m_aryJob(pAlloc, BK_Job, 128)
,m_arypJobQueued(pAlloc, BK_Request, 128)
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
		auto pJobParse = PJobCreateParse(pComp, pWork, pChzFileBody, pFile->m_istrFilename, comphase);
		auto pJobMaster = PJobCreateMaster(pComp, pJobParse, comphase);

		WaitForJob(pComp, pWork, pJobMaster);

		BlockListEntry::CIterator iter(&pWork->m_blistEntry);
		while (WorkspaceEntry * pEntry = iter.Next())
		{
			InString istrParse = IstrSExpression(pEntry->m_pStnod, SEWK_Parse);
			printf("     : %s\n", istrParse.m_pChz);

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

Job * PJobAllocate(Compilation * pComp, void * pVData)
{
	Job * pJob = pComp->m_aryJob.AppendNew();
	pJob->m_pVData = pVData;
	pJob->m_arypJobDependents.SetAlloc(pComp->m_pAlloc, BK_Job, 8);

	pJob->m_cJobUnfinished = 1; // 1 == no dependency, job has not been completed
	return pJob;
}

void EnqueueJob(Compilation * pComp, Job * pJob)
{
//	pJob->m_cJobUnfinished += 1;

	if (pJob->m_cJobUnfinished == 1)
	{
		pComp->m_arypJobQueued.Append(pJob);
	}
	else if (pJob->m_cJobUnfinished > 1)
	{
	}
}

bool FHasJobCompleted(const Job * pJob)
{
	return pJob->m_cJobUnfinished <= 0;
}

Job * PJobGet(Compilation * pComp)
{
#if 1
	for (size_t ipJob = pComp->m_arypJobQueued.C(); --ipJob >= 0; )
	{
		Job * pJob = pComp->m_arypJobQueued.TPopLast();
		//Job * pJob = pComp->m_arypJobQueued[ipJob];
		if (pJob->m_cJobUnfinished > 1)
		{
			//pComp->m_arypJobWaiting.Append(pJob);
		}
		else
		{
			return pJob;
		}
	}
	return nullptr;

#else
	if (pComp->m_arypJobQueued.FIsEmpty())
		return nullptr;

	for (size_t ipJob = pComp->m_arypJobQueued.C(); --ipJob >= 0; )
	{
		Job * pJob = pComp->m_arypJobQueued[ipJob];
		if (pJob->m_cJobUnfinished <= 1)
		{
			pComp->m_arypJobQueued.RemoveByI(ipJob);
			return pJob;
		}
	}
//	return pComp->m_arypJobQueued.TPopLast();
	return nullptr;
#endif
}

void FinishJob(Compilation * pComp, Job * pJob)
{
	Job ** ppJobMac = pJob->m_arypJobDependents.PMac();
	for (Job ** ppJobIt = pJob->m_arypJobDependents.A(); ppJobIt != ppJobMac; ++ppJobIt)
	{
		Job * pJobIt = *ppJobIt;
		const s32 cJobUnfinished = --(pJobIt->m_cJobUnfinished);
		MOE_ASSERT(cJobUnfinished >= 0, "job count underflow");

		pJobIt->m_arypJobDependents.Remove(pJob);

		if (cJobUnfinished == 1)
		{
			pComp->m_arypJobQueued.Append(pJobIt);
		}
	}


    const s32 cJobUnfinished = --(pJob->m_cJobUnfinished);
	MOE_ASSERT(cJobUnfinished == 0, "job count underflow");
}

void Job::AddDependency(Compilation * pComp, Job * pJobDep)
{
	// Add a job that we depend on

	const s32 cJobUnfinished = ++(m_cJobUnfinished);

	// Adding a dependency to a job already in the active queue will not pull it from the queue, it will
	//  just fall off when we try to update it

	if (pJobDep)
	{
		pJobDep->m_arypJobDependents.Append(this);
	}
}

#if 0
void WaitForJob(Compilation * pComp, Workspace * pWork, Job * pJobWait)
{
	// wait until the job has completed. in the meantime, work on any other job.

	while (!FHasJobCompleted(pJobWait))
	{
		Job * pJob = PJobGet(pComp);
		if (pJob)
		{
			pJob->m_pFnUpdate(pComp, pWork, pJob);

			// This job is finished: assuming that updating the job once will complete the job, should jobs be the
			//  ones to mark themselves as complete?

			Job ** ppJobMac = pJob->m_arypJobDependents.PMac();
			for (Job ** ppJobIt = pJob->m_arypJobDependents.A(); ppJobIt != ppJobMac; ++ppJobIt)
			{
				FinishJob(pComp, pJob);
			}

			const s32 cJobUnfinished = --(pJob->m_cJobUnfinished);
			if (MOE_FVERIFY(cJobUnfinished == 0, "expected job (and it's dependencies to be finished)"))
			{
				if (pJob->m_pFnCleanup)
				{
					(*pJob->m_pFnCleanup)(pWork, pJob);
				}
			}
		}
	}
}
#endif

void WaitForJob(Compilation * pComp, Workspace * pWork, Job * pJobWait)
{
	// wait until the job has completed. in the meantime, work on any other job.

	while (!FHasJobCompleted(pJobWait))
	{
		Job * pJob = PJobGet(pComp);

		// BB - this is not the right way to deal with this 
		if (pJob->m_cJobUnfinished > 1)
		{
			printf("ERROR: circular job dependencies!\n");
			return;
		}

		if (pJob)
		{
			JOBRET jobret = pJob->m_pFnUpdate(pComp, pWork, pJob);
			switch (jobret)
			{
			case JOBRET_StoppingError:
				break;

			case JOBRET_Waiting:
				EnqueueJob(pComp, pJob);
				break;

			case JOBRET_Complete:
				{
					FinishJob(pComp, pJob);

					if (pJob->m_pFnCleanup)
					{
						(*pJob->m_pFnCleanup)(pWork, pJob);
					}
				} break;
			}
		}
	}
}

