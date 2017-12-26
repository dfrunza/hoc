#undef UNICODE
#undef _UNICODE
#include <stdio.h>
#include <stdarg.h>
#define VC_EXTRALEAN
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "hocc.h"

bool DEBUG_enabled = true;
bool DEBUG_zero_arena = true;
bool DEBUG_check_arena_bounds = true;

MemoryArena* arena = 0;

#include "platform.h"
#include "lib.c"
#include "translate.c"

void mem_zero_(void* mem, int len)
{
  memset(mem, 0, (size_t)len);
}

MemoryArena* new_arena(int size)
{
  void* raw_mem = VirtualAlloc(0, size + sizeof(MemoryArena), MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
  MemoryArena* arena = (MemoryArena*)raw_mem;
  mem_zero_struct(arena, MemoryArena);
  arena->base = (uint8*)arena + sizeof(MemoryArena);
  arena->free = arena->base;
  arena->cap = arena->free + size;
  return arena;
}

int stdin_read(char buf[], int buf_size)
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

char* path_find_leaf(char* file_path)
{
  char* p_char = file_path;
  char* leaf = p_char;

  /* get the file name part */
  while(p_char && *p_char)
  {
    while(*p_char && *p_char != '\\')
      p_char++;
    if(*p_char == '\\')
      leaf = ++p_char;
  }
  return leaf;
}

char* path_make_leaf(char* file_path, bool with_extension)
{
  char* leaf = path_find_leaf(file_path);

  /* remove the filename extension */
  if(leaf && !with_extension)
  {
    char* p_char = leaf;
    while(*p_char && *p_char != '.')
      p_char++;
    *p_char = '\0';
  }
  return leaf;
}

char* path_make_dir(char* file_path)
{
  char* leaf = path_find_leaf(file_path);
  if(leaf)
    *leaf = '\0';
  return file_path;
}

int file_write_bytes(char* file_path, uint8* bytes, int count)
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

bool str_dump_to_file(String* str, char* file_path)
{
  int char_count = str_len(str);
  int bytes_written = file_write_bytes(file_path, (uint8*)str->head, str_len(str));
  return (char_count == bytes_written);
}

int file_read_bytes(MemoryArena* arena, uint8** bytes, char* file_path, int alloc_extra)
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

char* file_read_text(MemoryArena* arena, char* file_path)
{
  char* text = 0;
  int byte_count = 0;
  if((byte_count = file_read_bytes(arena, (uint8**)&text, file_path, 1)) >= 0)
  {
    text[byte_count] = '\0'; // NULL terminator
  }
  return text;
}

int h_sscanf(char* buffer, char* format, ...)
{
  va_list args;
  va_start(args, format);
  int result = vsscanf(buffer, format, args);
  va_end(args);
  return result;
}

int h_vsprintf(char* buffer, char* format, va_list args)
{
  return vsprintf(buffer, format, args);
}

int h_sprintf(char* buffer, char* format, ...)
{
  va_list args;
  va_start(args, format);
  int result = vsprintf(buffer, format, args);
  va_end(args);
  return result;
}

int h_printf(char* format, ...)
{
  va_list args;
  va_start(args, format);
  int result = vfprintf(stdout, format, args);
  va_end(args);
  return result;
}

bool compile_error_(char* file, int line, SourceLoc* src_loc, char* message, ...)
{
  char* filename_buf = mem_push_array_nz(arena, char, cstr_len(file));
  cstr_copy(filename_buf, file);

  if(src_loc && src_loc->line_nr >= 0)
    fprintf(stderr, "%s:%d: (%s:%d) ", src_loc->file_path, src_loc->line_nr,
            path_make_leaf(filename_buf, false), line);
  else
    fprintf(stderr, "%s:%d: ", file, line);

  va_list args;
  va_start(args, message);
  vfprintf(stderr, message, args);
  va_end(args);

  fprintf(stderr, "\n");
  return false;
}

void assert_(char* message, char* file, int line)
{
  if(DEBUG_enabled)
  {
    fprintf(stderr, "%s:%d: ", file, line);
    if(!message || message[0] == '\0')
      message = "";
    fprintf(stderr, "assert(%s)\n", message);

    fflush(stderr);
    *(int*)0 = 0;
  }
}

void fail_(char* file, int line, char* message, ...)
{
  fprintf(stderr, "%s:%d: ", file, line);

  if(!message || message[0] == '\0')
    message = "fail";

  va_list args;
  va_start(args, message);
  vfprintf(stderr, message, args);
  va_end(args);

  fprintf(stderr, "\n");
  fflush(stderr);
  *(int*)0 = 0;
}

bool error_(char* file, int line, char* message, ...)
{
  fprintf(stderr, "%s:%d: ", file, line);

  if(!message || message[0] == '\0')
    message = "error";

  va_list args;
  va_start(args, message);
  vfprintf(stderr, message, args);
  va_end(args);

  fprintf(stderr, "\n");
  fflush(stderr);
  return false;
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

bool make_out_file_names(OutFileNames* out_files, char* src_file_path)
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
  arena = new_arena(ARENA_SIZE);

  char* hoc_text = file_read_text(arena, src_file_path);

  if(hoc_text == 0)
  {
    success = error("could not read source file `%s`", src_file_path);
    goto end;
  }
  OutFileNames out_files = {0};
  if(!make_out_file_names(&out_files, src_file_path))
  {
    success = false;
    goto end;
  }
  String x86_text = {0};
  if(!translate(out_files.source.name, src_file_path, hoc_text, &x86_text))
  {
    success = error("program could not be translated");
    goto end;
  }
  int x86_text_len = str_len(&x86_text);
  int bytes_written = file_write_bytes(out_files.h_asm.name, (uint8*)x86_text.head, x86_text_len);
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

