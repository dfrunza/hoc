#include <stdarg.h>

void assert_(char* message, char* file, int line);

#define assert(EXPR)\
  do { if(!(EXPR)) assert_(#EXPR, __FILE__, __LINE__); } while(0);

void fail_(char* file, int line, char* message, ...);

#define fail(MESSAGE, ...)\
  fail_(__FILE__, __LINE__, (MESSAGE), __VA_ARGS__)

bool error_(char* file, int line, char* message, ...);

#define error(MESSAGE, ...)\
  error_(__FILE__, __LINE__, (MESSAGE), __VA_ARGS__)

bool compile_error_(char* file, int line, SourceLoc* src_loc, char* message, ...);

#define compile_error(SRC, MESSAGE, ...)\
  compile_error_(__FILE__, __LINE__, (SRC), (MESSAGE), __VA_ARGS__)

void mem_zero_(void* mem, int len);
int h_sscanf(char*, char*, ...);
int h_vsprintf(char *buffer, char *format, va_list args);
int h_printf(char *format, ...);
int h_sprintf(char* buffer, char* format, ...);
int h_putc(int ch);
char* path_find_leaf(char* file_path);
char* path_make_leaf(char* file_path, bool with_extension);
char* path_make_dir(char* file_path);
int file_write_bytes(char* file_path, uint8* bytes, int count);
int file_read_bytes(MemoryArena* arena, uint8** bytes, char* file_path, int alloc_extra);
char* file_read_text(MemoryArena* arena, char* file_path);

