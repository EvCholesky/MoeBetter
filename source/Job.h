#pragma once


enum JOBK
{
	JOBK_ParseFile,
	JOBK_TypeCheckEntry,
	JOBK_CodegenBytecode,
	JOBK_CodegenLlvm,
	JOBK_Execute,
};


typedef s32 SourceHandle; // tag = sohand

struct SSourceSpan // tag = sospan
{
	SourceHandle	m_sohandBegin;
	SourceHandle	m_sohandEnd;
};

struct SCompileJob
{
	JOBK					m_jobk;

	SSourceSpan				m_sospan;	// file span for parse jobs
	STNode *				m_pStnod;	// AST entry
	SymbolTable *		 	m_pSymtab;	// symbol table for this entry, local symbols for lambdas 
};





class CWorkspace
{
public:
	enum FILEK
	{
		FILEK_Source,
		FILEK_UnitTest,
		FILEK_Library,
		FILEK_StaticLibrary,
		FILEK_DynamicLibrary,

		FILEK_Max,
		FILEK_Min = 0,
		FILEK_Nil = -1,
	};

	enum FILES
	{
		FILES_Nil,
		FILES_Requested,
		FILES_Processing,
		FILES_Complete,
	};

	static const int	s_cBFilenameMax = 1024;
	static const char * s_pCozSourceExtension;
	static const char * s_pCozUnitTestExtension;

	struct SFile // tag = file
	{
						SFile(const EWC::CString & strFilename, FILEK filek)
						:m_strFilename(strFilename)
						,m_pChzFileBody(nullptr)
						,m_pDif(nullptr)
						,m_filek(filek)
						,m_files(FILES_Requested)
						,m_dBWarm(0)
						,m_iLineWarm(0)
						,m_iColumnWarm(0)
							{ ; }

		EWC::CString	m_strFilename;	// full filename;
		const char *	m_pChzFileBody;	// contents of the file
		SDIFile *		m_pDif;			
		FILEK			m_filek;
		FILES			m_files;

		s32				m_dBWarm;		// byte delta for warm start (previous lookup)
		s32				m_iLineWarm;
		s32				m_iColumnWarm;

	};

	void				SourceLookup(SSourceHandle * pSohand, SFile * pFileOut, int * piChz);
	void				SourceLookup(SSourceSpan * pSospan, SFile * pFileOut, int * piChzMin, int * piChzMac);

};

// helper struct for looking up source spans
class CSourceLookup // tag = solook
{
public:
					CLexerLookup(CWorkspace * pWork, const SSourceSpan * pSospan);

	EWC::CString	m_strFilename;
	s32				m_iLineMin;
	s32				m_iCodepointMin;
	s32				m_iLineMac;
	s32				m_iCodepointMac;
};

void CompileFile()