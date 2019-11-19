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

void RequestSymbol(Request * pRq, RQK rqk, Moe::InString istrSymbolPath);
void RequestLocation(Request * pRq, RQK rqk, Moe::InString istrFilename, s32 iLine, s32 iCodepoint);

enum RQRESK
{
	RQRESK_Symbols,	// result is an array of * Symbol
	RQRESK_Types,	// result is an array of * TypeInfo
	RQRESK_Ast,		// result is an array of * STNode
	RQEREK_Files,	// result is list of filenames
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
int CRqresServiceRequest(Compilation * pComp, Workspace * pWork);
void PrintResult(Compilation * pComp, int iRqres, char * aCh, size_t cChMax);

typedef void (*PFnJobUpdate)(Compilation * pComp, Workspace * pWork, Job *);
typedef void (*PFnJobDelete)(Job *);

struct Job
{
					Job()
					:m_cJobUnfinished(0)
					,m_pFnUpdate(nullptr)
					,m_pFnDelete(nullptr)
					,m_pVData(nullptr)
						{ ; }

	int				m_cJobUnfinished;
	PFnJobUpdate	m_pFnUpdate;
	PFnJobDelete	m_pFnDelete;
	void *			m_pVData;
};

Job * PJobAllocate(Compilation * pComp, void * pVData);
void EnqueueJob(Compilation * pComp, Job * pJob);
void WaitForJob(Compilation * pComp, Workspace * pWork, Job * pJob);



struct Compilation // tag = comp
{
								Compilation(Moe::Alloc * pAlloc);

	Moe::CDynAry<RequestSource>			m_aryRqsrc;
	Moe::CDynAry<Request>				m_aryRq;
	Moe::CDynAry<RequestResult *>		m_arypRqres;

	Moe::CDynAry<Job>					m_aryJob;
	Moe::CDynAry<Job *>					m_arypJobQueued;	// jobs waiting to be executed
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