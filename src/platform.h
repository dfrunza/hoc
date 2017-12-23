void assert_(char* message, char* file, int line);
#define assert(EXPR) do { if(!(EXPR)) assert_(#EXPR, __FILE__, __LINE__); } while(0)

void fail_(char* file, int line, char* message, ...);
#define fail(MESSAGE, ...) fail_(__FILE__, __LINE__, (MESSAGE), __VA_ARGS__)

bool error_(char* file, int line, char* message, ...);
#define error(MESSAGE, ...) error_(__FILE__, __LINE__, (MESSAGE), __VA_ARGS__)

bool compile_error_(char* file, int line, SourceLoc* src_loc, char* message, ...);
#define compile_error(SRC, MESSAGE, ...) compile_error_(__FILE__, __LINE__, (SRC), (MESSAGE), __VA_ARGS__)

void mem_zero_(void* mem, int len);
int h_sscanf(char*, char*, ...);
int h_vsprintf(char *buffer, char *format, va_list args);
int h_printf(char *format, ...);
int h_sprintf(char* buffer, char* format, ...);
bool str_dump_to_file(String* str, char* file_path);

