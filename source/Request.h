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

#include "MoeArray.h"
#include "MoeString.h"
#include <atomic>

// The Query/Request system is organized as follows

// The API exposes the 'Request' interface which internally uses queries to fill out it's results
// Requests: RQK and source builds up one or more RequestResults
// Query: symbol search + m_treesRequired

struct Compilation;
struct Job;
struct STNode;
struct Symbol;
struct TypeInfo;
struct Workspace;

// Outward facing API

enum RQK // REQuest Kind
{
	RQK_FindSymbol,			// lookup a symbol by name
	RQK_FindAst,			// lookup an ast
	RQK_FindTypeInfo,		// find type info (from symbol)
	RQK_FindDefinition,		// where was this instance defined
	RQK_FindReferences,		// where was this symbol referenced
	RQK_ListMembers,		// what are the members for this struct
	RQK_BytecodeEntry,		// bytecode entry to run a given procedure
	RQK_CodegenResult,		// object file?
};

enum RQPARMK
{
	RQPARMK_SymbolPath,
	RQPARMK_SourceLocation,
};

struct RequestSymbolPath // tag = rqsympath
{
	Moe::InString		m_istrSymPath;
};

struct RequestSourceLocation // tag = rqsrcloc
{
	Moe::InString		m_istrFilename;
	s32					m_iLine;
	s32					m_iCodepoint;
};

struct Request // tag = rq
{
	RQK					m_rqk;
	RQPARMK				m_rqparmk;

	Moe::InString		m_istrPath;		// symbol path or filename, depending on rqparamk
	s32					m_iLine;
	s32					m_iCodepoint;
};

void AddRequestSymbol(Request * pRq, RQK rqk, Moe::InString istrSymbolPath);
void AddRequestLocation(Request * pRq, RQK rqk, Moe::InString istrFilename, s32 iLine, s32 iCodepoint);

enum RQRESK
{
	RQRESK_Symbols,	// result is an array of * Symbol
	RQRESK_Types,	// result is an array of * TypeInfo
	RQRESK_Ast,		// result is an array of * STNode
	RQRESK_Files,	// result is list of filenames
};

struct RequestResult // tag = rqres
{
	RQRESK				m_rqresk;
};

// BB - how do we manage the lifetime of these result objects??
struct RequestResultSymbol : public RequestResult // tag = rqressym
{
	Moe::CDynAry<Symbol *>		m_arypSym;
};

struct RequestResultType : public RequestResult // tag = rqrestin
{
	Moe::CDynAry<TypeInfo *>	m_arypTin;
};

struct RequestResultAst : public RequestResult // rqresst
{
	Moe::CDynAry<STNode *>		m_arypStnod;
};

// Internal Queries 

struct Query
{
	Moe::CDynAry <Request>		m_aryRq;
};

// indices

// lookup by symbol path string:		math.vec2.m_x
// lookup by source location			array.moe:12,2

/*struct Request // tag = rq
{
	REQK					m_reqk;
	RequestParm				m_rqparm;
};*/

enum RQSRCK
{
	RQSRCK_Filename,
	RQSRCK_SourceText,
};

struct RequestSource
{
	RQSRCK			m_rqsrck;
	Moe::InString	m_istr;
};

void AddRequest(Compilation * pComp, Request * pRq);
void AddSourceFile(Compilation * pComp, const char * pChzFilename);
void AddSourceText(Compilation * pComp, const char * pChz);
int CRqresServiceRequest(Compilation * pComp, Workspace * pWork);
void PrintResult(Compilation * pComp, int iRqres, char * aCh, size_t cChMax);

enum JOBRET 
{
	JOBRET_StoppingError,
	JOBRET_Waiting,			// symbol def or code execution. 
	JOBRET_Complete,
};

typedef JOBRET (*PFnJobUpdate)(Compilation * pComp, Workspace * pWork, Job *);
typedef void (*PFnJobCleanup)(Workspace * pWork, Job *);

// desired end phase for this job and it's outputs
enum COMPHASE
{
	COMPHASE_Parse,
	COMPHASE_TypeCheck,
	COMPHASE_Codegen,
	COMPHASE_Exec,

	MOE_MAX_MIN_NIL(COMPHASE)
};

// Here's my current thinking about the job queues, There are two queues: the active queue and the waiting queue. The waiting queue 
//  holds jobs that an outstanding dependency and can be removed from the wait queue at any time, in random order. The active queue 
//  pops jobs off of the end and cannot remove jobs internal to the queue. Jobs pulled from the active queue that have (new?) dependencies
//  will be pushed onto the waiting queue without executing the job.

// Strike that, I'm checking in and then I'll update to my new plan for jobs
// Jobs have a Parent/child structure where the parent cannot be completed until all child jobs are complete (but can execute it's task)
// In addition to that jobs have an atomic prerequisite count that must be decremented to zero before the job can execute
//		Parser->TypeCheck->Codegen dependencies are parent child dependencies, Waiting for symbol resolution is a prerequisite dependency

struct Job
{
								Job()
								:m_cJobUnfinished(0)
								,m_arypJobDependents()
								,m_pFnUpdate(nullptr)
								,m_pFnCleanup(nullptr)
								,m_pVData(nullptr)
								,m_comphaseDesired(COMPHASE_Nil)
									{ ; }

	// Job dependencies, must complete before this job can start (and complete) 
	std::atomic_int_fast32_t	m_cJobUnfinished;		// dependency count, 0 == complete, 1 == this job, >1 waiting for dependencies
	Moe::CDynAry<Job *>			m_arypJobDependents;	// Which jobs depend on me?

	void						AddDependency(Compilation * pComp, Job * pJobDe);
	
	PFnJobUpdate				m_pFnUpdate;
	PFnJobCleanup				m_pFnCleanup;
	void *						m_pVData;

	COMPHASE					m_comphaseDesired;	// What is the desired end phase for this job's output
};

Job * PJobAllocate(Compilation * pComp, void * pVData);
void EnqueueJob(Compilation * pComp, Job * pJob);
void WaitForJob(Compilation * pComp, Workspace * pWork, Job * pJob);



struct Compilation // tag = comp
{
								Compilation(Moe::Alloc * pAlloc);

	Moe::Alloc *						m_pAlloc;
	Moe::CDynAry<RequestSource>			m_aryRqsrc;
	Moe::CDynAry<Request>				m_aryRq;
	Moe::CDynAry<RequestResult *>		m_arypRqres;

	Moe::CDynAry<Job>					m_aryJob;
	Moe::CDynAry<Job *>					m_arypJobQueued;	// jobs ready to be executed
};


/*
struct SReqEntry    // tag = reqent
{
    REQENTK     m_reqentk;
    CString     m_strName;
};

struct SCompilation // tag = comp
{
    EWC::CDynAry<SReqEntry *>       m_arypReqent;
    EWC::CDynAry<SReqAST *>         m_aryReqast;
    SReqSymTable *                  m_pSymtabGlobal;
};

void CompileSource(const char * pChzFilename, SCompilation * pComp);
void RequestCompilation(SCompilation * pComp, REQK reqk);

SReqSymbol * PReqsymLookup(const char * pChzName);
SReqEntry * PReqentTypeOf(SReqSymbol * pReqsym);
*/