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

#define assert(EXPR)\
if(!(EXPR))\
{\
  fprintf(stderr, "Assertion failed: %s\n", #EXPR);\
  fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);\
  fflush(stderr);\
  *(int*)0 = 0;\
}\

#define sizeof_array(ARRAY) (sizeof(ARRAY)/sizeof(ARRAY[0]))
#define obj(STRUCT, FIELD) (&((STRUCT)->FIELD))

/* WARNING: Maximum length of 'message' is 128 chars. */
void DEBUG_output_short_cstr(char* message, ...);

void error(char* message, ...);

typedef struct
{
  uint8* base;
  uint8* free;
  uint8* limit;
}
MemoryArena;

MemoryArena mem_push_arena(MemoryArena* arena, size_t size);
#define mem_push_struct(ARENA, TYPE, COUNT) ((TYPE*)mem_push_struct_(ARENA, sizeof(TYPE), COUNT))
#define mem_push_size(ARENA, COUNT) (mem_push_struct_(ARENA, sizeof(uint8), COUNT))
#define mem_zero(VAR) mem_zero_(VAR, sizeof(VAR))
void mem_zero_(void* mem, size_t len);
void* mem_push_struct_(MemoryArena* arena, size_t elem_size, size_t count);
MemoryArena arena_new(int size);
void DEBUG_arena_print_occupancy(char* tag, MemoryArena* arena);

bool char_is_letter(char ch);
bool char_is_numeric(char c);

/*
 * The function assumes that all characters in the input string are digits,
 * except for the first, which could be the negative sign '-'
 */
bool cstr_to_int(char* string, int* integer);

bool cstr_to_float(char* string, float* result);
bool cstr_start_with(char* str, char* prefix);
bool cstr_match(char* str_a, char* str_b);
int cstr_len(char* str);
char* cstr_copy(char* dest_str, char* src_str);
void cstr_copy_substr(char* dest_str, char* begin_char, char* end_char);

typedef struct
{
  char* head;
  char* end;
  MemoryArena* arena;
}
String;

void str_init(String* string, MemoryArena* arena);

/* The 0-terminator is not counted. */
uint str_len(String* string);

void str_debug_output(String* string);
void str_stdout(String* string);
void str_append(String* string, char* cstr);
void str_printf(String* string, char* message, va_list varargs);
void str_tidyup(String* string);
char* path_find_leaf(char* file_path);

/* Writes over the 'file_path' string */
char* path_make_stem(char* file_path);

char* path_make_dir(char* file_path);
uint file_write_bytes(char* file_path, uint8* text, size_t count);
char* file_read_text(MemoryArena* arena, char* file_path);
int stdin_read(char buf[], int buf_size);

typedef struct ListItem_
{
  void* elem;
  struct ListItem_* next;
  struct ListItem_* prev;
}
ListItem;

typedef struct
{
  ListItem** last;
  ListItem** next_slot;
  int count;
  ListItem* first;
}
List;

inline void list_init(List* list);
List* list_new(MemoryArena* arena);
void list_append_item(List* list, ListItem* item);
void list_append(MemoryArena* arena, List* list, void* elem);

typedef struct
{
  char* file_path;
  int line_nr;
  char* src_line;
}
SourceLocation;

bool compile_error(SourceLocation* src_loc, char* message, ...);

