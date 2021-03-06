#include <stdarg.h>

#include "hocc.h"
#include "common.c"
#include "translate.c"

#include <stdio.h>
#undef UNICODE
#undef _UNICODE
#define VC_EXTRALEAN
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

typedef struct WindowsFile
{
  PlatformFile platform_file;

  HANDLE handle;
  int id_low;
  int id_high;
  int volume_serial_no;
}
WindowsFile;

void* platform_alloc_memory(int size)
{
  void *raw_mem = VirtualAlloc(0, size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
  return raw_mem;
}

int platform_stdin_read(char* buf, int buf_size)
{
  HANDLE h_std = GetStdHandle(STD_INPUT_HANDLE);
  DWORD bytes_read = 0;

  if(h_std && ReadFile(h_std, buf, (DWORD)buf_size, &bytes_read, 0))
  {
    if(bytes_read >= 0 && bytes_read < (uint)buf_size)
    {
      buf[bytes_read] = '\0';
    }
    else
      assert(!"bytes_read < 0 || bytes_read >= buf_size");
  }
  else
  {
    DWORD err = GetLastError();
    printf("Win32 error %d\n", err);
  }

  return (int)bytes_read;
}

int platform_file_read_bytes(MemoryArena* arena, uint8** bytes, char* file_path, int alloc_extra)
{
  *bytes = 0;
  int byte_count = -1;
  FILE* file = fopen(file_path, "rb");
  if(file)
  {
    if(fseek(file, 0, SEEK_END) == 0)
    {
      byte_count = ftell(file);
      if(byte_count >= 0)
      {
        if(fseek(file, 0, SEEK_SET) == 0)
        {
          *bytes = push_array(arena, uint8, byte_count + alloc_extra);
          fread(*bytes, (size_t)byte_count, 1, file);
          if(ferror(file))
            byte_count = -1;
        }
        else
          byte_count = -1;
      }
      fclose(file);
    }
    else
      byte_count = -1;
  }

  return byte_count;
}

char* platform_file_read_text(MemoryArena* arena, char* file_path)
{
  char* text = 0;
  int byte_count = 0;
  if((byte_count = platform_file_read_bytes(arena, (uint8**)&text, file_path, 1)) >= 0)
  {
    text[byte_count] = '\0'; // NULL terminator
  }

  return text;
}

int platform_file_write_bytes(char* file_path, uint8* bytes, int count)
{
  int bytes_written = 0;
  FILE* h_file = fopen(file_path, "wb");
  if(h_file)
  {
    bytes_written = (int)fwrite(bytes, 1, (size_t)count, h_file);
    fclose(h_file);
  }

  return bytes_written;
}

int platform_sscanf(char* buffer, char* format, ...)
{
  va_list args;
  va_start(args, format);
  int result = vsscanf(buffer, format, args);
  va_end(args);

  return result;
}

int platform_sprintf_va(char* buffer, char* format, va_list args)
{
  return vsprintf(buffer, format, args);
}

int platform_sprintf(char* buffer, char* format, ...)
{
  va_list args;
  va_start(args, format);
  int result = vsprintf(buffer, format, args);
  va_end(args);

  return result;
}

int platform_printf_va(char* format, va_list args)
{
  return vprintf(format, args);
}

int platform_printf(char* format, ...)
{
  va_list args;
  va_start(args, format);
  int result = vfprintf(stdout, format, args);
  va_end(args);

  fflush(stdout);
  return result;
}

PlatformFile* platform_file_open(MemoryArena* arena, char* file_path)
{
  WindowsFile* file = 0;

  HANDLE handle = CreateFile(file_path, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if(handle != INVALID_HANDLE_VALUE)
  {
    file = push_struct(arena, WindowsFile);
    file->handle = handle;
    file->platform_file.path = file_path;

    BY_HANDLE_FILE_INFORMATION* info = push_struct(arena, BY_HANDLE_FILE_INFORMATION);
    if(GetFileInformationByHandle(handle, info))
    {
      file->volume_serial_no = info->dwVolumeSerialNumber;
      file->id_low = info->nFileIndexLow;
      file->id_high = info->nFileIndexHigh;
    }
  }

  return (PlatformFile*)file;
}

bool platform_file_identity(PlatformFile* file_A_arg, PlatformFile* file_B_arg)
{
  WindowsFile* file_A = (WindowsFile*)file_A_arg;
  WindowsFile* file_B = (WindowsFile*)file_B_arg;

  bool is_identity = (file_A->volume_serial_no == file_B->volume_serial_no)
    && (file_A->id_low == file_B->id_low)
    && (file_A->id_high == file_B->id_high);
  return is_identity;
}

bool make_out_file_names(OutFileNames* out_files, MemoryArena* arena, char* src_file_path)
{
  bool success = true;

  const int buf_len = 200;
  out_files->working_dir = push_string(arena, buf_len);
  int working_dir_len = GetCurrentDirectoryA(buf_len, out_files->working_dir);
  if((working_dir_len > 0) && (working_dir_len < buf_len))
  {
    String str = {0};
    str_init(&str, arena);
    str_append(&str, src_file_path);
    out_files->title = str_cap(&str);
    out_files->title = platform_path_make_file_name(out_files->title, false);

    str_init(&str, arena);
    str_format(&str, "%s\\%s.asm", out_files->working_dir, out_files->title);
    out_files->asm_file = str_cap(&str);

    str_init(&str, arena);
    str_format(&str, "%s\\%s.obj", out_files->working_dir, out_files->title);
    out_files->obj_file = str_cap(&str);
  }
  else
    success = error("working directory could not be retrieved");

  return success;
}

bool run_process(char* process_cmd)
{
  bool success = true;
  STARTUPINFOA process_startup_info = {0};
  process_startup_info.cb = sizeof(STARTUPINFOA);
  PROCESS_INFORMATION process_info = {0};
  if(CreateProcessA(0, process_cmd, 0, 0, false, 0, 0, 0, &process_startup_info, &process_info))
  {
    DWORD wait_code = WaitForSingleObject(process_info.hProcess, 60*1000);

    if(wait_code == WAIT_OBJECT_0)
    {
      DWORD ml_exit_code = 0;
      if(GetExitCodeProcess(process_info.hProcess, &ml_exit_code))
      {
        success = (ml_exit_code == 0);
      }
      else
        success = error("could not retrieve process exit code");
    }
    else
      success = error("WaitForSingleObject() : unexpected return code");
  }
  else
    success = error("could not create process");
  return success;
}

bool assemble(MemoryArena* arena, OutFileNames* out_files)
{
  bool success = true;

  String str = {0};
  str_init(&str, arena);
  /*
      /Cx     - preserve case in publics, externs
      /Zi     - add symbolic debug info
      /Fl     - generate listing
      /c      - assemble without linking
  */
  str_format(&str, "ml.exe /c /Zi /Cx /nologo %s", out_files->asm_file);
  char* ml_cmd = str_cap(&str);
  success = run_process(ml_cmd);
  if (success) {
    String str = {0};
    str_init(&str, arena);
    str_format(&str, "link.exe /nologo /subsystem:console /incremental:no /entry:startup ..\\kernel32.lib %s", out_files->obj_file);
    char* link_cmd = str_cap(&str);
    success = run_process(link_cmd);
  }
  return success;
}

int make_exit_code(bool success)
{
  return success ? 0 : -1;
}

int main(int argc, char* argv[])
{
  bool success = true;

  if(argc < 2)
  {
    success = error("missing argument : source file");
    return make_exit_code(success);
  }

  char* src_file_path = argv[1];

  MemoryArena* arena = new_arena(32*MEGABYTE);

  char* hoc_text = platform_file_read_text(push_arena(&arena, 2*MEGABYTE), src_file_path);

  if(hoc_text == 0)
  {
    success = error("file could not be read : `%s`", src_file_path);
    return make_exit_code(success);
  }

  OutFileNames out_files = {0};
  make_out_file_names(&out_files, arena, src_file_path);

  String* x86_text = 0;
  if(!translate(arena, out_files.title, src_file_path, hoc_text, &x86_text))
  {
    success = error("program could not be translated");
    return make_exit_code(success);
  }

  int x86_text_len = str_len(x86_text);
  int bytes_written = platform_file_write_bytes(out_files.asm_file, (uint8*)x86_text->head, x86_text_len);
  if(bytes_written != x86_text_len)
  {
    success = error("number of bytes written not equal to text size : `%s`", out_files.asm_file);
    return make_exit_code(success);
  }

  if(!assemble(arena, &out_files))
  {
    success = error("program could not be assembled");
    return make_exit_code(success);
  }

#if 0
  getc(stdin);
#endif

  return make_exit_code(success);
}

