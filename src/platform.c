#undef UNICODE
#undef _UNICODE
#include <stdio.h>
#define VC_EXTRALEAN
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

void mem_zero_f(void* mem, int len)
{
  memset(mem, 0, (size_t)len);
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

int h_putc(int ch)
{
  return putc(ch, stdout);
}

bool compile_error_f(char* file, int line, SourceLoc* src_loc, char* message, ...)
{
  char* filename_buf = mem_push_array_nz(arena, char, cstr_len(file));
  cstr_copy(filename_buf, file);

  if(src_loc && src_loc->line_nr >= 0)
    fprintf(stderr, "%s(%d) : (%s:%d) ", src_loc->file_path, src_loc->line_nr,
            path_make_leaf(filename_buf, false), line);
  else
    fprintf(stderr, "%s(%d) : ", file, line);

  va_list args;
  va_start(args, message);
  vfprintf(stderr, message, args);
  va_end(args);

  fprintf(stderr, "\n");
  return false;
}

void assert_f(char* message, char* file, int line)
{
  if(DEBUG_enabled)
  {
    fprintf(stderr, "%s(%d) : ", file, line);
    if(!message || message[0] == '\0')
      message = "";
    fprintf(stderr, "assert(%s)\n", message);

    fflush(stderr);
    *(int*)0 = 0;
  }
}

void fail_f(char* file, int line, char* message, ...)
{
  fprintf(stderr, "%s(%d) : ", file, line);

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

bool error_f(char* file, int line, char* message, ...)
{
  fprintf(stderr, "%s(%d) : ", file, line);

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
    if(bytes_read)
    {
      if(bytes_read < (uint)buf_size)
        buf[bytes_read] = '\0';
      else
        assert(!"bytes_read = buf_size");
    }
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

int file_read_bytes(MemoryArena* arena, uint8** bytes, char* file_path)
{
  *bytes = 0;
  int byte_count = 0;
  FILE* file = fopen(file_path, "rb");
  if(file)
  {
    fseek(file, 0, SEEK_END);
    byte_count = ftell(file);
    if(byte_count > 0)
    {
      fseek(file, 0, SEEK_SET);
      *bytes = mem_push_array_nz(arena, uint8, byte_count);
      fread(*bytes, (size_t)byte_count, 1, file);
    }
    fclose(file);
  }
  return byte_count;
}

char* file_read_text(MemoryArena* arena, char* file_path)
{
  char* text = 0;
  file_read_bytes(arena, (uint8**)&text, file_path);
  *mem_push_array(arena, char, 1) = '\0'; // NULL terminator
  return text;
}


