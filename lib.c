#pragma once
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <windows.h>

typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef int bool32;

typedef char int8;
typedef short int16;
typedef int int32;
typedef long long int64;
typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned int uint32;
typedef unsigned long long uint64;
typedef float float32;
typedef double float64;

#define KILOBYTE (1024ll)
#define MEGABYTE (1024*KILOBYTE)
#define false 0
#define true  1

typedef struct
{
  void* base;
  void* free;
  void* limit;
}
MemoryArena;

typedef struct
{
  char* head;
  char* end;
  MemoryArena* arena;
}
String;

#define sizeof_array(ARRAY) (sizeof(ARRAY)/sizeof(ARRAY[0]))

void
debug_print(char* message, ...)
{
  static char strbuf[128] = {0};
  va_list args;

  va_start(args, message);
  vsprintf(strbuf, message, args);
  va_end(args);

  OutputDebugString(strbuf);
}

void
error(char* message, ...)
{
  va_list args;
  fprintf(stdout, "error : ");

  va_start(args, message);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
}

int
maxi(int a, int b)
{
  int m = a;
  if(a < b)
    m = b;
  return m;
}

bool32
char_is_letter(char ch)
{
  return ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z');
}

bool32
char_is_numeric(char c)
{
  return '0' <= c && c <= '9';
}

#define struct_check_bounds(ARENA, TYPE, STRUCT) mem_check_bounds_(ARENA, sizeof(TYPE), STRUCT)
#define arena_check_bounds(ARENA) mem_check_bounds_((ARENA), 0, (ARENA)->free)

void
mem_check_bounds_(MemoryArena* arena, int elem_size, void* ptr)
{
  assert(arena->base <= ptr);
  assert((uint8*)arena->free + elem_size <= (uint8*)arena->limit);
}

#define mem_zero(VAR) mem_zero_(VAR, sizeof(VAR))

void
mem_zero_(void* mem, size_t len)
{
  memset(mem, 0, len);
}

void
mem_zero_range(void* start, void* one_past_end)
{
  size_t len = (uint8*)one_past_end - (uint8*)start;
  assert(len >= 0);
  mem_zero_(start, len);
}

void
arena_reset(MemoryArena* arena)
{
  arena->free = arena->base;
}

void
arena_set_watermark(MemoryArena* arena, void* ptr)
{
  mem_check_bounds_(arena, 0, ptr);
  arena->free = ptr;
}

MemoryArena
arena_push_(MemoryArena* arena, size_t elem_size, size_t count, bool32 clear_to_zero)
{
  assert(count > 0);

  MemoryArena sub_arena = {0};
  sub_arena.base = arena->free;
  sub_arena.free = sub_arena.base;
  arena->free = (uint8*)sub_arena.base + elem_size*count;
  arena_check_bounds(arena);
  sub_arena.limit = arena->free;
  if(clear_to_zero)
  {
    size_t size = (uint8*)sub_arena.limit - (uint8*)sub_arena.base;
    assert(size >= 0);
    mem_zero_(sub_arena.base, size);
  }
  return sub_arena;
}

#define mem_push_struct(ARENA, TYPE, COUNT) ((TYPE*)mem_push_struct_(ARENA, sizeof(TYPE), COUNT))
#define mem_push_size(ARENA, COUNT) ((uint8* )mem_push_struct_(ARENA, sizeof(uint8), COUNT))

void*
mem_push_struct_(MemoryArena* arena, size_t elem_size, size_t count)
{
  assert(count > 0);

  void* element = arena->free;
  arena->free = (uint8*)arena->free + elem_size*count;
  arena_check_bounds(arena);
  mem_zero_range(element, arena->free);
  return element;
}

MemoryArena
arena_new(int size)
{
  MemoryArena arena = {0};
  arena.free = malloc(size);
  arena.limit = (uint8*)arena.free + size;
  return arena;
}

// The function assumes that all characters in the input string are digits,
// except for the first, which could be the negative sign '-'
bool32
cstr_to_int(char* string, int* integer)
{
  bool32 negative = false;

  if(*string == '-')
  {
    negative = true;
    string++;
  }

  char c = *string++;
  if(char_is_numeric(c))
  {
    int result = (int)(c - '0');

    for(c = *string++; c != '\0'; c = *string++)
    {
      if(char_is_numeric(c))
      {
        int digit = (int)(c - '0');
        result = result*10 + digit;
      }
      else
        return false;
    }

    if(negative)
      result = -result;
    *integer = result;
  } else
    return false;

  return true;
}

bool32
cstr_start_with(char* str, char* prefix)
{
  while(*str == *prefix)
  {
    str++;
    prefix++;
    if(*prefix == '\0')
      break;
  }
  bool32 result = (*prefix == '\0');
  return result;
}

bool32
cstr_match(char* str_a, char* str_b)
{
  while(*str_a == *str_b)
  {
    str_a++;
    str_b++;
    if(*str_a == '\0')
      break;
  }
  bool32 result = (*str_a == *str_b);
  return result;
}

int
cstr_len(char* str)
{
  int len = 0;
  while(*str++ != 0)
    len++;
  return len;
}

char*
cstr_copy(char* dest_str, char* src_str)
{
  do
  {
    *dest_str++ = *src_str++;
  }
  while(*src_str);

  return dest_str;
}

void
cstr_copy_substr(char* dest_str, char* begin_char, char* end_char)
{
  char* src_str = begin_char;

  do
    *dest_str++ = *src_str++;
  while(src_str <= end_char);

  dest_str = 0;
}

void
str_init(String* string, MemoryArena* arena)
{
  string->arena = arena;
  string->head = mem_push_struct(arena, char, 1);
  *string->head = '\0';
  string->end = string->head;
}

//NOTE: The 0-terminator is not counted.
uint
str_len(String* string)
{
  assert(string->head <= string->end);
  uint len = (uint)(string->end - string->head);
  return len;
}

void
str_append(String* string, char* cstr)
{
  MemoryArena* arena = string->arena;
  assert(string->head <= string->end);
  assert(string->end == (char*)arena->free-1);
  int len = cstr_len(cstr);
  mem_push_struct(arena, char, len);
  cstr_copy(string->end, cstr);
  string->end = (char*)arena->free-1;
}

void
str_tidyup(String* string)
{
  assert(string->head <= string->end);
  if(string->end > string->head)
  {
    char* p_end = string->end - 1;
    while(p_end >= string->head && *p_end)
      p_end--;
    string->end = p_end;
    MemoryArena* arena = string->arena;
    arena->free = string->end+1;
  }
}

char*
path_find_leaf(char* file_path)
{
  char* p_char = file_path;
  char* leaf = p_char;

  // Get the file name part
  while(p_char && *p_char)
  {
    while(*p_char && *p_char != '\\')
      p_char++;
    if(*p_char == '\\')
      leaf = ++p_char;
  }
  return leaf;
}

/* Writes over the 'file_path' string */
char*
path_make_stem(char* file_path)
{
  char* leaf = path_find_leaf(file_path);

  // Remove the filename extension
  if(leaf)
  {
    char* p_char = leaf;
    while(*p_char && *p_char != '.')
      p_char++;
    *p_char = '\0';
  }
  return leaf;
}

char*
path_make_dir(char* file_path)
{
  char* leaf = path_find_leaf(file_path);
  if(leaf)
    *leaf = '\0';
  return file_path;
}

uint
file_write_bytes(char* file_path, char* text, size_t count)
{
  uint bytes_written = 0;
  FILE* h_file = fopen(file_path, "wb");
  if(h_file)
  {
    bytes_written = (uint)fwrite(text, 1, count, h_file);
    fclose(h_file);
  }
  return bytes_written;
}

char*
file_read_text(MemoryArena* arena, char* file_path)
{
  char* text = 0;
  FILE* file = fopen(file_path, "rb");
  if(file)
  {
    fseek(file, 0, SEEK_END);
    uint32 file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    text = (char*)mem_push_size(arena, file_size+1); // + NULL-terminator
    fread(text, file_size, 1, file);
    fclose(file);
    text[file_size] = '\0';
  }
  return text;
}

int
stdin_read(char buf[], int buf_size)
{
  HANDLE h_std = GetStdHandle(STD_INPUT_HANDLE);
  DWORD bytes_read = 0;

  if(h_std && ReadFile(h_std, buf, buf_size, &bytes_read, 0))
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

typedef void ListElem;

typedef struct ListItem_
{
  ListElem* elem;
  struct ListItem_* next;
  struct ListItem_* prev;
}
ListItem;

typedef struct
{
  ListItem* last;
  int       count;
  ListItem  sentinel;
}
List;

void
list_init(List* list)
{
  list->last = &list->sentinel;
}

void
list_append(MemoryArena* arena, List* list, ListElem* elem)
{
  ListItem* item = mem_push_struct(arena, ListItem, 1);
  item->elem = elem;
  list->last->next = item;
  item->prev = list->last;
  list->last = item;
  list->count++;
}

ListItem*
list_first_item(List* list)
{
  return list->sentinel.next;
}

