#include "hasm.c"
#include "translate.c"

// User-defined PE resource:
//   nameId  typeId  fileName
#define OUT_RC "CODE  VM  \"%s\""

typedef struct
{
  char* name;
  int len;
}
FileName;

typedef struct
{
  char strings[4*80 + 4*10];
  FileName ir;
  FileName irc;
  FileName rc;
  FileName res;
}
OutFileNames;

char* get_file_stem(char* filePath)
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

bool32 make_file_names(OutFileNames* outFiles, char* stem)
{
  int stemLen = str_len(stem);
  assert(stemLen > 0);
  bool32 success = (stemLen > 0 && stemLen < 80);

  if(success)
  {
    char* str = outFiles->strings;

    sprintf(str, "%s.ir", stem);
    outFiles->ir.name = str;
    outFiles->ir.len = str_len(outFiles->ir.name);
    str = outFiles->ir.name + outFiles->ir.len + 1;

    sprintf(str, "%s.irc", stem);
    outFiles->irc.name = str;
    outFiles->irc.len = str_len(outFiles->irc.name);
    str = outFiles->irc.name + outFiles->irc.len + 1;

    sprintf(str, "%s.rc", stem);
    outFiles->rc.name = str;
    outFiles->rc.len = str_len(outFiles->rc.name);
    str = outFiles->rc.name + outFiles->rc.len + 1;

    sprintf(str, "%s.res", stem);
    outFiles->res.name = str;
    outFiles->res.len = str_len(outFiles->res.name);
  } else
    error("Length of file name out of range : '%s'", stem);
  return success;
}

bool32 write_res_file(OutFileNames* outFiles)
{
  char buf[200];
  sprintf(buf, OUT_RC, outFiles->irc.name);
  int text_len = str_len(buf);
  int bytesWritten = write_bytes_to_file(outFiles->rc.name, buf, text_len);
  bool32 success = (bytesWritten == text_len);
  if(success)
  {
    STARTUPINFO startInfo = {0};
    startInfo.dwFlags = STARTF_USESTDHANDLES;
    startInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    startInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    startInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    PROCESS_INFORMATION procInfo = {0};
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
      error("Process could not be launched : %s", buf);
  } else
    error("RC file '%s' incompletely written", outFiles->rc.name);
  return success;
}

bool32 write_ir_file(OutFileNames* outFiles, VmProgram* vm_program)
{
  int bytesWritten = write_bytes_to_file(outFiles->ir.name, vm_program->text.start, vm_program->text_len);
  bool32 success = (bytesWritten == vm_program->text_len);
  if(!success)
    error("IR file '%s' incompletely written", outFiles->ir.name);
  return success;
}

bool32 write_irc_file(OutFileNames* outFiles, IrCode* irCode)
{
  int bytesWritten = write_bytes_to_file(outFiles->irc.name, (char*)irCode->codeStart, irCode->codeSize);
  bool32 success = (bytesWritten == irCode->codeSize);
  if(!success)
    error("IRC file '%s' incompletely written", outFiles->irc.name);
  return success;
}

int main(int argc, char* argv[])
{
  int ret = -1;

  if(argc >= 2)
  {
    MemoryArena arena = new_arena(10*MEGABYTE);

    char* filePath = argv[1];
    char* hocProgram = read_text_from_file(&arena, filePath);
    if(hocProgram)
    {
      VmProgram vm_program = {0};
      bool32 success = translate_hoc(&arena, filePath, hocProgram, &vm_program);
      if(success)
      {
        OutFileNames outFiles = {0};
        char* fileStem = get_file_stem(filePath);

        success = make_file_names(&outFiles, fileStem) &&
          write_ir_file(&outFiles, &vm_program);
#if 1
        if(success)
        {
          IrCode* irCode = 0;
          char* irText = vm_program.text.start;
          bool32 success = translate_ir_to_code(&arena, irText, &irCode);

          if(success)
          {
            success = write_irc_file(&outFiles, irCode) && write_res_file(&outFiles);
            if(success)
              ret = 0;
          }
        }
#else
        ret = 0;
#endif
      }
    } else
      error("File '%s' could not be read", filePath);
  } else
    error("Missing argument: input source file");

  return ret;
}
