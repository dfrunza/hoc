#include "translator.cpp"
#include "asm.cpp"

// User-defined PE resource:
//   nameId  typeId  fileName
#define OUT_RC "CODE  VM  \"%s\""

struct FileName
{
  char* name;
  int len;
};

struct OutFileNames
{
  char strings[4*80 + 4*10];
  FileName ir;
  FileName irc;
  FileName rc;
  FileName res;
};

char* GetFileStem(char* filePath)
{
  char* pChar = filePath;
  char* stem = pChar;

  // Get the file name part
  while(pChar && *pChar)
  {
    while(*pChar && *pChar != '\\')
      pChar++;
    if(*pChar == '\\')
      stem = ++pChar;
  }

  // Remove the extension
  if(stem)
  {
    char* pChar = stem;
    while(*pChar && *pChar != '.')
      pChar++;
    *pChar = '\0';
  }
  return stem;
}

bool32 MakeFileNames(OutFileNames* outFiles, char* stem)
{
  int stemLen = StrLen(stem);
  assert(stemLen > 0);
  bool32 success = (stemLen > 0 && stemLen < 80);

  if(success)
  {
    char* str = outFiles->strings;

    sprintf(str, "%s.ir", stem);
    outFiles->ir.name = str;
    outFiles->ir.len = StrLen(outFiles->ir.name);
    str = outFiles->ir.name + outFiles->ir.len + 1;

    sprintf(str, "%s.irc", stem);
    outFiles->irc.name = str;
    outFiles->irc.len = StrLen(outFiles->irc.name);
    str = outFiles->irc.name + outFiles->irc.len + 1;

    sprintf(str, "%s.rc", stem);
    outFiles->rc.name = str;
    outFiles->rc.len = StrLen(outFiles->rc.name);
    str = outFiles->rc.name + outFiles->rc.len + 1;

    sprintf(str, "%s.res", stem);
    outFiles->res.name = str;
    outFiles->res.len = StrLen(outFiles->res.name);
  } else
    Error("Length of file name out of range : '%s'", stem);
  return success;
}

bool32 WriteResFile(OutFileNames* outFiles)
{
  char buf[200];
  sprintf(buf, OUT_RC, outFiles->irc.name);
  int textLen = StrLen(buf);
  int bytesWritten = WriteBytesToFile(outFiles->rc.name, buf, textLen);
  bool32 success = (bytesWritten == textLen);
  if(success)
  {
    STARTUPINFO startInfo = {};
    startInfo.dwFlags = STARTF_USESTDHANDLES;
    startInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    startInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    startInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    PROCESS_INFORMATION procInfo = {};
    sprintf(buf, "rc.exe /nologo /fo%s %s", outFiles->res.name, outFiles->rc.name);
    DWORD exitCode = 0;
    success = CreateProcess(0, buf, 0, 0, true, 0, 0, 0, &startInfo, &procInfo);
    if(success)
    {
      WaitForSingleObject(procInfo.hProcess, INFINITE);
      GetExitCodeProcess(procInfo.hProcess, &exitCode);
      success = (exitCode == 0);

      CloseHandle(procInfo.hProcess);
      CloseHandle(procInfo.hThread);
    } else
      Error("Process could not be launched : %s", buf);
  } else
    Error("RC file '%s' incompletely written", outFiles->rc.name);
  return success;
}

bool32 WriteIrFile(OutFileNames* outFiles, ProgramText* irProgram)
{
  int bytesWritten = WriteBytesToFile(outFiles->ir.name, irProgram->text.start, irProgram->textLen);
  bool32 success = (bytesWritten == irProgram->textLen);
  if(!success)
    Error("IR file '%s' incompletely written", outFiles->ir.name);
  return success;
}

bool32 WriteIrcFile(OutFileNames* outFiles, IrCode* irCode)
{
  int bytesWritten = WriteBytesToFile(outFiles->irc.name, (char*)irCode->codeStart, irCode->codeSize);
  bool32 success = (bytesWritten == irCode->codeSize);
  if(!success)
    Error("IRC file '%s' incompletely written", outFiles->irc.name);
  return success;
}

int main(int argc, char* argv[])
{
  int ret = -1;

  if(argc >= 2)
  {
    MemoryArena arena = NewArena(10*MEGABYTE);

    Translator trans = {};
    InitTranslator(&trans, &arena);

    char* filePath = argv[1];
    char* hocProgram = ReadTextFromFile(&arena, filePath);
    if(hocProgram)
    {
      ProgramText irProgram = {};
      bool32 success = TranslateHocToIr(&trans, filePath, hocProgram, &irProgram);
      if(success)
      {
        OutFileNames outFiles = {};
        char* fileStem = GetFileStem(filePath);

        success = MakeFileNames(&outFiles, fileStem) &&
          WriteIrFile(&outFiles, &irProgram);
        if(success)
        {
          IrCode* irCode = 0;
          char* irText = irProgram.text.start;
          bool32 success = TranslateIrToCode(&arena, irText, &irCode);

          if(success)
          {
            success = WriteIrcFile(&outFiles, irCode) && WriteResFile(&outFiles);
            if(success)
              ret = 0;
          }
        }
      }
    } else
      Error("File '%s' could not be read", filePath);
  } else
    Error("Missing argument: input source file");

  return ret;
}
