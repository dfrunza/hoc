/* Modified TimeMem - Windows port of Unix time utility */

#include <Windows.h>
#include <Psapi.h>
#include <stdio.h>
#include <tchar.h>

#define NO_MEM_INFO
#define NO_DEBUG

/* Displays usage help for this program. */
static void usage()
{
	_tprintf(_T("Usage: time.exe command [args...]\n"));
}

/* Converts FILETIME to ULONGLONG. */
static ULONGLONG ConvertFileTime(const FILETIME *t)
{
	ULARGE_INTEGER i;
	CopyMemory(&i, t, sizeof(ULARGE_INTEGER));
	return i.QuadPart;
}

/* Displays information about a process. */
static int info(HANDLE hProcess)
{
	DWORD dwExitCode;
	FILETIME ftCreation, ftExit, ftKernel, ftUser;
	double tElapsed, tKernel, tUser;
#ifndef NO_MEM_INFO
	PROCESS_MEMORY_COUNTERS pmc = { sizeof(PROCESS_MEMORY_COUNTERS) };
#endif

	/* Exit code */
	if (!GetExitCodeProcess(hProcess, &dwExitCode))
		return 1;

	/* CPU info */
	if (!GetProcessTimes(hProcess, &ftCreation, &ftExit, &ftKernel, &ftUser))
	{
		return 1;
	}
	tElapsed = 1.0e-7 * (ConvertFileTime(&ftExit) - ConvertFileTime(&ftCreation));
	tKernel = 1.0e-7 * ConvertFileTime(&ftKernel);
	tUser = 1.0e-7 * ConvertFileTime(&ftUser);

#ifndef NO_MEM_INFO
	/* Memory info */
	// Print information about the memory usage of the process.
	if (!GetProcessMemoryInfo(hProcess, &pmc, sizeof(pmc)))
		return 1;

	/* Display info. */
	_tprintf(_T("Exit code      : %u\n"), dwExitCode);

	_tprintf(_T("Elapsed time   : %.2lf\n"), tElapsed);
	_tprintf(_T("Kernel time    : %.2lf (%.1lf%%)\n"), tKernel, 100.0*tKernel/tElapsed);
	_tprintf(_T("User time      : %.2lf (%.1lf%%)\n"), tUser, 100.0*tUser/tElapsed);

	_tprintf(_T("page fault #   : %u\n"), pmc.PageFaultCount);
	_tprintf(_T("Working set    : %u KB\n"), pmc.PeakWorkingSetSize/1024);
	_tprintf(_T("Paged pool     : %u KB\n"), pmc.QuotaPeakPagedPoolUsage/1024);
	_tprintf(_T("Non-paged pool : %u KB\n"), pmc.QuotaPeakNonPagedPoolUsage/1024);
	_tprintf(_T("Page file size : %u KB\n"), pmc.PeakPagefileUsage/1024);
#else
	_tprintf(_T("Time : %.2lf sec\n"), tElapsed);
#endif
	return 0;
}

int _tmain(
#ifndef NO_DEBUG
	int argc, _TCHAR *argv[]
#endif
	)
{
	LPTSTR szCmdLine;
	LPTSTR szBegin;
	STARTUPINFO si = { sizeof(STARTUPINFO) };
	PROCESS_INFORMATION pi;
	int ret;

	/* Read the command line. */
	szCmdLine = GetCommandLine();

	/* Strip the first token from the command line. */
	if (szCmdLine[0] == '"')
	{
		/* The first token is double-quoted. Note that we don't need to 
		 * worry about escaped quote, because a quote is not a valid 
		 * path name under Windows.
		 */
		LPTSTR p = szCmdLine + 1;
		while (*p && *p != '"')
			++p;
		szBegin = (*p == '"')? p + 1 : p;
	}
	else
	{
		/* The first token is deliminated by a space or tab. 
		 * See "Parsing C++ Command Line Arguments" below:
		 * http://msdn.microsoft.com/en-us/library/windows/desktop/17w5ykft(v=vs.85).aspx
		 */
		LPTSTR p = szCmdLine;
		while (*p && *p != ' ' && *p != '\t')
			++p;
		szBegin = p;
	}

	/* Skip white spaces. */
	while (*szBegin == ' ' || *szBegin == '\t')
		++szBegin;

	/* If we have no more arguments, display usage info and exit. */
	if (*szBegin == 0)
	{
		usage();
		return 1;
	}

	/* Display argc,argv and command line for debugging purpose. */
#ifndef NO_DEBUG
	{
		int i;
		for (i = 0; i < argc; i++)
			_tprintf(_T("argv[%d]=%s\n"), i, argv[i]);
		_tprintf(_T("CmdLine=%s\n"), szCmdLine);
		_tprintf(_T("Invoked=%s\n"), szBegin);
	}
#endif

	/* Create the process. */
	if (!CreateProcess(NULL, szBegin, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi))
	{
		_tprintf(_T("Error: Cannot create process.\n"));
		return 1;
	}

	/* Wait for the process to finish. */
	if (WaitForSingleObject(pi.hProcess, INFINITE) != WAIT_OBJECT_0)
	{
		_tprintf(_T("Error: Cannot wait for process.\n"));
		return 1;
	}

	/* Display process statistics. */
	ret = info(pi.hProcess);

	/* Close process handles. */
	CloseHandle(pi.hThread);
	CloseHandle(pi.hProcess);

	return ret;
}
