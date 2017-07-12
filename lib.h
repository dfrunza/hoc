#pragma once
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef int bool;

typedef char int8;
typedef short int16;
typedef int int32;
typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned int uint32;
typedef long long int64;
typedef unsigned long long uint64;
typedef float float32;
typedef double float64;

#define KILOBYTE (1024ll)
#define MEGABYTE (1024*KILOBYTE)
#define false 0
#define true  1
#define inline __inline
#define internal static

typedef struct MemoryArena
{
  uint8* alloc;
  uint8* free;
  uint8* cap;
  struct MemoryArena* host;
  int sub_arena_count;
}
MemoryArena;

typedef struct
{
  size_t total_avail;
  double in_use;
}
ArenaUsage;

typedef struct
{
  char* head;
  char* end;
  MemoryArena* arena;
}
String;

typedef struct ListItem
{
  void* elem;
  struct ListItem* next;
  struct ListItem* prev;
}
ListItem;

typedef struct
{
  ListItem* first;
  ListItem* last;
}
List;

#define assert(EXPR)\
  if(!(EXPR)) assert_f(#EXPR, __FILE__, __LINE__)
#define sizeof_array(ARRAY)\
  (sizeof(ARRAY)/sizeof(ARRAY[0]))
#define to_bool(EXPR)\
  ((EXPR) ? true : false)

void assert_f(char* expr, char* file, int line);
void error(char* message, ...);

MemoryArena* arena_new(int size);
MemoryArena* arena_pop(MemoryArena* arena);
MemoryArena* arena_push(MemoryArena* arena, size_t size);
void arena_free(MemoryArena* arena);
void arena_reset(MemoryArena* arena);
ArenaUsage arena_usage(MemoryArena* arena);
#define mem_push_struct(ARENA, TYPE)\
  ((TYPE*)mem_push_struct_f(ARENA, sizeof(TYPE), 1, true))
#define mem_push_count(ARENA, TYPE, COUNT)\
  ((TYPE*)mem_push_struct_f(ARENA, sizeof(TYPE), COUNT, true))
#define mem_push_count_nz(ARENA, TYPE, COUNT)\
  ((TYPE*)mem_push_struct_f(ARENA, sizeof(TYPE), COUNT, false))
#define mem_zero_struct(VAR, TYPE)\
  (mem_zero_f(VAR, sizeof(TYPE)))
void mem_zero_f(void* mem, size_t len);
void* mem_push_struct_f(MemoryArena* arena, size_t elem_size, size_t count, bool zero_mem);

bool char_is_letter(char ch);
bool char_is_numeric(char c);

/*
 * The function assumes that all characters in the input string are digits,
 * except for the first, which could be the negative sign '-'
 */
bool cstr_to_int(char* str, int* integer);

bool cstr_to_float(char* str, float* result);
bool cstr_start_with(char* str, char* prefix);
bool cstr_match(char* str_a, char* str_b);
int cstr_len(char* str);
char* cstr_copy(char* dest_str, char* src_str);
void cstr_copy_substr(char* dest_str, char* begin_char, char* end_char);

void str_init(String* str, MemoryArena* arena);

/* The 0-terminator is not counted. */
uint str_len(String* str);

void str_debug_output(String* str);
void str_stdout(String* str);
bool str_dump_to_file(String* str, char* file_path);
void str_append(String* str, char* cstr);
void str_printf(String* str, char* message, ...);
void str_printf_va(String* str, char* message, va_list varargs);
void str_tidyup(String* str);
void str_free(String* str);
char* str_cap(String* str);

char* path_find_leaf(char* file_path);

/* Writes over the 'file_path' string */
char* path_make_stem(char* file_path);

char* path_make_dir(char* file_path);
uint file_write_bytes(char* file_path, uint8* text, size_t count);
char* file_read_text(MemoryArena* arena, char* file_path);
int stdin_read(char buf[], int buf_size);

inline void list_init(List* list);
List* list_new(MemoryArena* arena);
void list_append_item(List* list, ListItem* item);
void list_append(MemoryArena* arena, List* list, void* elem);

/* Insert list `a` into `b` and delete the item `b` from `list_b`. */
void list_replace_at(List* list_a, List* list_b, ListItem* at_b_item);


