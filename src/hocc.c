#include <stdarg.h>
#include "hocc.h"

global_var bool DEBUG_enabled = true;
global_var bool DEBUG_zero_arena = true;
global_var bool DEBUG_check_arena_bounds = true;
global_var bool DEBUG_zero_struct = true;

#include "common.c"
#include "translate.c"

#include <stdio.h>
#undef UNICODE
#undef _UNICODE
#define VC_EXTRALEAN
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

void* platform_alloc_memory(int size)
{
  void *raw_mem = VirtualAlloc(0, size + sizeof(MemoryArena), MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
  return raw_mem;
}

int platform_stdin_read(char buf[], int buf_size)
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
          *bytes = mem_push_array_nz(arena, uint8, byte_count + alloc_extra);
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

struct HFile_platform
{
  HANDLE* handle;
  int id_low;
  int id_high;
  int volume_serial_no;
};

HFile* platform_open_file(MemoryArena* arena, char* file_path)
{
  HFile* file = 0;

  HANDLE* handle = CreateFile(file_path, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if(handle != INVALID_HANDLE_VALUE)
  {
    file = mem_push_struct(arena, HFile);
    file->platform = mem_push_struct(arena, HFile_platform);
    file->platform->handle = handle;
    file->path = file_path;

    BY_HANDLE_FILE_INFORMATION* info = mem_push_struct(arena, BY_HANDLE_FILE_INFORMATION);
    if(GetFileInformationByHandle(handle, info))
    {
      file->platform->volume_serial_no = info->dwVolumeSerialNumber;
      file->platform->id_low = info->nFileIndexLow;
      file->platform->id_high = info->nFileIndexHigh;
    }
  }

  return file;
}

bool platform_file_identity(HFile* file_A, HFile* file_B)
{
  bool is_identity = (file_A->platform->volume_serial_no == file_B->platform->volume_serial_no)
    && (file_A->platform->id_low == file_B->platform->id_low)
    && (file_A->platform->id_high == file_B->platform->id_high);
  return is_identity;
}

typedef struct
{
  char* name;
  int len;
}
FileName;

typedef struct
{
  char strings[4*80 + 4*10];
  FileName h_asm;
  FileName source;
}
OutFileNames;

bool make_out_file_names(MemoryArena* arena, OutFileNames* out_files, char* src_file_path)
{
  char* leaf = mem_push_array_nz(arena, char, cstr_len(src_file_path));
  cstr_copy(leaf, src_file_path);
  leaf = path_make_leaf(leaf, false);

  int leaf_len = cstr_len(leaf);
  assert(leaf_len > 0);
  bool success = true;

  if(leaf_len <= 0 || leaf_len >= 81)
  {
    return success = error("length of file name must be between 1..80 : '%s'", leaf);
  }
  char* str = out_files->strings;

  sprintf(str, "%s.asm", leaf);
  out_files->h_asm.name = str;
  out_files->h_asm.len = cstr_len(out_files->h_asm.name);
  str = out_files->h_asm.name + out_files->h_asm.len + 1;

  sprintf(str, "%s", leaf);
  out_files->source.name = str;
  out_files->source.len = cstr_len(out_files->source.name);
  str = out_files->source.name + out_files->source.len + 1;

  return success;
}

int main(int argc, char* argv[])
{
  bool success = true;

  if(argc < 2)
  {
    success = error("missing argument : input source file");
    goto end;
  }

  char* src_file_path = argv[1];

  MemoryArena* arena = new_arena(32*MEGABYTE);

  char* hoc_text = file_read_text(push_arena(&arena, 2*MEGABYTE), src_file_path);

  if(hoc_text == 0)
  {
    success = error("could not read source file `%s`", src_file_path);
    goto end;
  }

  OutFileNames out_files = {0};
  if(!make_out_file_names(arena, &out_files, src_file_path))
  {
    success = false;
    goto end;
  }

  String x86_text = {0};
  if(!translate(arena, out_files.source.name, src_file_path, hoc_text, &x86_text))
  {
    success = error("program could not be translated");
    goto end;
  }

  int x86_text_len = str_len(&x86_text);
  int bytes_written = platform_file_write_bytes(out_files.h_asm.name, (uint8*)x86_text.head, x86_text_len);
  if(bytes_written != x86_text_len)
  {
    success = error("not all bytes were written to file `%s`", out_files.h_asm.name);
    goto end;
  }

#if 0
  getc(stdin);
#endif

end:
  return success ? 0 : -1;
}

