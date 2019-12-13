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
#include "MoeArray.h"
#include "MoeString.h"
#include "MoeTypes.h"
#include "Request.h"
#include "UnitTest.h"
#include "Workspace.h"

#include "stdio.h"

using namespace Moe;

struct CommandLineManager // tag = cmdlineman
{
	struct Command // tag = com
	{
		HV				m_hvName;
		const char *	m_pChzValue;
	};

	enum OPTK
	{
		OPTK_Bool,
		OPTK_String,
	};

	struct Option
	{
						Option(const char * pChzOpt, const char * pChzHelp, OPTK optk = OPTK_Bool);

		HV				m_hvOpt;
		const char *	m_pChzOpt;
		const char *	m_pChzHelp;
		OPTK			m_optk;
	};

	CommandLineManager()
	:m_aryCom()
	,m_arypOpt()
	,m_pChzFilename(nullptr)
		{ ; }

	bool FTryParse(int cpChzArg, const char * apChzArg[])
	{
		HV hvLlvm = HvFromPChz("-llvm");
		bool fParsedOk = true;

		const char * pChzFilename = nullptr;
		for (int ipChz = 1; ipChz < cpChzArg; ++ipChz)
		{
			const char * pChzArg = apChzArg[ipChz];
			if (pChzArg[0] == '-')
			{
				if (m_aryCom.C() == m_aryCom.CMax())
				{
					printf("Too many arguments. ignoring %s\n", pChzArg);
				}
				else
				{
					Command * pCom = m_aryCom.AppendNew();
					pCom->m_hvName = HvFromPChz(pChzArg);

					Option * pOpt = POptLookup(pChzArg);

					if (!pOpt)
					{
						printf("Unexpected command-line argument '%s'\n", pChzArg);
						fParsedOk = false;
					}
					else if (pOpt->m_optk == OPTK_String)
					{
						if (ipChz + 1 >= cpChzArg)
						{
							printf("expected argument after %s\n", pChzArg);
						}
						else
						{

							++ipChz;
							pCom->m_pChzValue = apChzArg[ipChz];
						}
					}
				}
			}
			else
			{
				if (pChzFilename)
					printf("Expected one filename argument.");

				pChzFilename = pChzArg;
			}
		}
		m_pChzFilename = pChzFilename;
		return fParsedOk;
	}

	void AppendCommandValues(const char * pChzCommand, CAry<const char *> * paryPChzValues)
	{
		HV hvCommand = HvFromPChz(pChzCommand);

		for (size_t iCom = 0; iCom < m_aryCom.C(); ++iCom)
		{
			if (m_aryCom[iCom].m_hvName == hvCommand)
			{
				paryPChzValues->Append(m_aryCom[iCom].m_pChzValue);
			}
		}
	}

	Option * POptLookup(const char * pChz)
	{
		HV hvOpt = HvFromPChz(pChz);
		for (size_t ipOpt = 0; ipOpt < m_arypOpt.C(); ++ipOpt)
		{
			if (hvOpt == m_arypOpt[ipOpt]->m_hvOpt)
				return m_arypOpt[ipOpt];
		}
		return nullptr;
	}

	Command * PComLookup(const Option & opt)
	{
		HV hvCommand = opt.m_hvOpt;

		for (size_t iCom = 0; iCom < m_aryCom.C(); ++iCom)
		{
			if (m_aryCom[iCom].m_hvName == hvCommand)
				return &m_aryCom[iCom];
		}
		return nullptr;
	}

	bool FHasCommand(const Option & opt)
	{
		Command * pCom = PComLookup(opt);
		return pCom != nullptr;
		/*HV hvCommand = opt.m_hvOpt;

		for (size_t iCom = 0; iCom < m_aryCom.C(); ++iCom)
		{
			if (m_aryCom[iCom].m_hvName == hvCommand)
				return true;
		}
		return false;
		*/
	}

	void RegisterOption(Option * pOpt)
	{
		m_arypOpt.Append(pOpt);
	}

	void PrintHelp()
	{
		printf("moe [options] [filename]\n");
		printf("  options:\n");

		for (int ipOpt = 0; ipOpt < m_arypOpt.C(); ++ipOpt)
		{
			Option * pOpt = m_arypOpt[ipOpt];
			printf("    %-10s %s\n", pOpt->m_pChzOpt, pOpt->m_pChzHelp);
		}
	}

	static const int s_cComMax = 128;
	CFixAry<Command, s_cComMax>		m_aryCom;			// parsed commands strings
	CFixAry<Option *, 30>			m_arypOpt;			// registered command line options 
	const char *					m_pChzFilename;
};

CommandLineManager * PCmdlineman()
{
	static CommandLineManager s_cmdlineman;
	return &s_cmdlineman;
}

CommandLineManager::Option::Option(const char * pChzOpt, const char * pChzHelp, CommandLineManager::OPTK optk)
:m_hvOpt(HvFromPChz(pChzOpt))
,m_pChzOpt(pChzOpt)
,m_pChzHelp(pChzHelp)
,m_optk(optk)
{
	auto pCmdlineman = PCmdlineman();
	pCmdlineman->RegisterOption(this);
}

static CommandLineManager::Option s_optTest("-test", "Run compiler unit test file (*.moetest)", CommandLineManager::OPTK_String);
static CommandLineManager::Option s_optHelp("-help", "List the compiler options");
static CommandLineManager::Option s_optNoLink("-nolink", "Skip the linker step");
static CommandLineManager::Option s_optUseLLD("-useLLD", "Use the LLVM linker LLD");
static CommandLineManager::Option s_optRelease("-release", "Generate optimized code and link against optimized local libraries\n");
static CommandLineManager::Option s_optSrc("-src", "supply source snippet on the command-line", CommandLineManager::OPTK_String);
static CommandLineManager::Option s_optAst("-printAST", "print the post parse AST");

extern bool FTestLexing();

void InitInternStrings(Moe::Alloc * pAlloc)
{
	Moe::StaticInitStrings(pAlloc);
	InternReservedWordStrings();
	InternUnitTestStrings();
	InternBuiltInTypeStrings();
}

void ShutdownInternStrings(Moe::Alloc * pAlloc)
{
	ClearUnitTestStrings();
	ClearBuiltInTypeStrings();
	ClearReservedWordStrings();
	Moe::StaticShutdownStrings(pAlloc);
}

int main(int cpChzArg, const char * apChzArg[])
{
	static const int s_cBString = 100 * 1024;
	static const int s_cBHeap = 1000 * 1024;
	static const int s_cBError = 100 * 1024;

	InitConsoleSettings();

	{
		u8 aBString[s_cBString];
		Alloc allocString(aBString, s_cBString);

		InitInternStrings(&allocString);
		FTestLexing();
		ShutdownInternStrings(&allocString);
	}

#ifdef MOE_DEBUG
	printf("Warning: Compiler was built in debug mode and may be very slow.\n"); 
#endif

	if (cpChzArg < 2)
	{
		PCmdlineman()->PrintHelp();
		return 0;
	}

	auto pCmdlineman = PCmdlineman();
	bool fParsedOk = pCmdlineman->FTryParse(cpChzArg, apChzArg);

	if (!fParsedOk || pCmdlineman->FHasCommand(s_optHelp))
	{
		PCmdlineman()->PrintHelp();
	}

	u8 aBString[s_cBString];
	Alloc allocString(aBString, s_cBString);

	u8 * aBHeap = new u8[s_cBHeap];
	Alloc allocHeap(aBHeap, s_cBHeap);

	u8 * aBError = new u8[s_cBError];
	Alloc allocError(aBError, s_cBError);

	GRFCOMPILE grfcompile;
	if (CommandLineManager::Command * pCom = pCmdlineman->PComLookup(s_optTest))
	{
		InitInternStrings(&allocString);

		ErrorManager errman(&allocError);
		Workspace work(&allocHeap, &errman);

		FUnitTestFile(&work, pCom->m_pChzValue, grfcompile.m_raw);

		ShutdownInternStrings(&allocString);
	}
	else 
	{
		InitInternStrings(&allocString);

		Compilation comp(&allocHeap);

		if (pCmdlineman->m_pChzFilename)
		{
			AddSourceFile(&comp, pCmdlineman->m_pChzFilename);
		}
		else if (CommandLineManager::Command * pCom = pCmdlineman->PComLookup(s_optSrc))
		{
			AddSourceText(&comp, pCom->m_pChzValue);
		}

		if (CommandLineManager::Command * pCom = pCmdlineman->PComLookup(s_optAst))
		{
			Request rq;
			RequestSymbol(&rq, RQK_FindAst, IstrIntern(pCom->m_pChzValue));
			AddRequest(&comp, &rq);
		}

		ErrorManager errman(&allocError);
		Workspace work(&allocHeap, &errman);

		if (pCmdlineman->FHasCommand(s_optRelease))
		{
			work.m_optlevel = OPTLEVEL_Release;
		}

		BeginWorkspace(&work);

#ifdef MOE_TRACK_ALLOCATION
		u8 aBAltrac[1024 * 100];
		Alloc allocAltrac(aBAltrac, sizeof(aBAltrac));

		AllocTracker * pAltrac = PAltracCreate(&allocAltrac);
		work.m_pAlloc->SetAltrac(pAltrac);
#endif

		int cRqres = CRqresServiceRequest(&comp, &work);
		if (cRqres)
		{
			char aCh[1024];
			for (int iRqres = 0; iRqres < cRqres; ++iRqres)
			{
				PrintResult(&comp, iRqres, aCh, sizeof(aCh));	
				printf("%d) %s", iRqres, aCh);
			}
		}

		//bool fSuccess = FCompileModule(&work, grfcompile, pCmdlineman->m_pChzFilename);

		EndWorkspace(&work);

#ifdef MOE_TRACK_ALLOCATION
		DeleteAltrac(&allocAltrac, pAltrac);
		work.m_pAlloc->SetAltrac(nullptr);
#endif
		ShutdownInternStrings(&allocString);
	}
}
