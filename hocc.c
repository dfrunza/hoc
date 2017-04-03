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

char* get_file_stem(char* file_path)
{
  char* p_char = file_path;
  char* stem = p_char;

  // Get the file name part
  while(p_char && *p_char)
  {
    while(*p_char && *p_char != '\\')
      p_char++;
    if(*p_char == '\\')
      stem = ++p_char;
  }

  // Remove the extension
  if(stem)
  {
    char* p_char = stem;
    while(*p_char && *p_char != '.')
      p_char++;
    *p_char = '\0';
  }
  return stem;
}

bool32 make_file_names(OutFileNames* out_files, char* stem)
{
  int stem_len = str_len(stem);
  assert(stem_len > 0);
  bool32 success = (stem_len > 0 && stem_len < 80);

  if(success)
  {
    char* str = out_files->strings;

    sprintf(str, "%s.ir", stem);
    out_files->ir.name = str;
    out_files->ir.len = str_len(out_files->ir.name);
    str = out_files->ir.name + out_files->ir.len + 1;

    sprintf(str, "%s.irc", stem);
    out_files->irc.name = str;
    out_files->irc.len = str_len(out_files->irc.name);
    str = out_files->irc.name + out_files->irc.len + 1;

    sprintf(str, "%s.rc", stem);
    out_files->rc.name = str;
    out_files->rc.len = str_len(out_files->rc.name);
    str = out_files->rc.name + out_files->rc.len + 1;

    sprintf(str, "%s.res", stem);
    out_files->res.name = str;
    out_files->res.len = str_len(out_files->res.name);
  } else
    error("Length of file name out of range : '%s'", stem);
  return success;
}

bool32 write_res_file(OutFileNames* out_files)
{
  char buf[200];
  sprintf(buf, OUT_RC, out_files->irc.name);
  int text_len = str_len(buf);
  int bytes_written = write_bytes_to_file(out_files->rc.name, buf, text_len);
  bool32 success = (bytes_written == text_len);
  if(success)
  {
    STARTUPINFO startInfo = {0};
    startInfo.dwFlags = STARTF_USESTDHANDLES;
    startInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    startInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    startInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    PROCESS_INFORMATION proc_info = {0};
    sprintf(buf, "rc.exe /nologo /fo%s %s", out_files->res.name, out_files->rc.name);
    DWORD exit_code = 0;
    success = CreateProcess(0, buf, 0, 0, true, 0, 0, 0, &startInfo, &proc_info);
    if(success)
    {
      WaitForSingleObject(proc_info.hProcess, INFINITE);
      GetExitCodeProcess(proc_info.hProcess, &exit_code);
      success = (exit_code == 0);

      CloseHandle(proc_info.hProcess);
      CloseHandle(proc_info.hThread);
    } else
      error("Process could not be launched : %s", buf);
  } else
    error("RC file '%s' incompletely written", out_files->rc.name);
  return success;
}

bool32 write_ir_file(OutFileNames* out_files, VmProgram* vm_program)
{
  int bytes_written = write_bytes_to_file(out_files->ir.name, vm_program->text.start, vm_program->text_len);
  bool32 success = (bytes_written == vm_program->text_len);
  if(!success)
    error("IR file '%s' incompletely written", out_files->ir.name);
  return success;
}

bool32 write_irc_file(OutFileNames* out_files, HasmCode* hasm_code)
{
  int bytes_written = write_bytes_to_file(out_files->irc.name, (char*)hasm_code->code_start, hasm_code->code_size);
  bool32 success = (bytes_written == hasm_code->code_size);
  if(!success)
    error("IRC file '%s' incompletely written", out_files->irc.name);
  return success;
}

int main(int argc, char* argv[])
{
  int ret = -1;

  if(argc >= 2)
  {
    MemoryArena arena = new_arena(10*MEGABYTE);

    char* file_path = argv[1];
    char* hoc_program = read_text_from_file(&arena, file_path);
    if(hoc_program)
    {
      VmProgram vm_program = {0};
      bool32 success = translate_hoc(&arena, file_path, hoc_program, &vm_program);
      if(success)
      {
        OutFileNames out_files = {0};
        char* fileStem = get_file_stem(file_path);

        success = make_file_names(&out_files, fileStem) &&
          write_ir_file(&out_files, &vm_program);
#if 1
        if(success)
        {
          HasmCode* hasm_code = 0;
          char* hasm_text = vm_program.text.start;
          bool32 success = translate_ir_to_code(&arena, hasm_text, &hasm_code);

          if(success)
          {
            success = write_irc_file(&out_files, hasm_code) && write_res_file(&out_files);
            if(success)
              ret = 0;
          }
        }
#else
        ret = 0;
#endif
      }
    } else
      error("File could not be read: %s", file_path);
  } else
    error("Missing argument: input source file");

  return ret;
}
