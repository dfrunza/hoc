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
  char* start;
  char* end;
  MemoryArena* arena;
}
String;

//FIXME: Obsolete
typedef MemoryArena StringArena;

#define sizeof_array(ARRAY) (sizeof(ARRAY)/sizeof(ARRAY[0]))

void debug_print(char* message, ...)
{
  static char strbuf[128] = {0};
  va_list args;

  va_start(args, message);
  vsprintf(strbuf, message, args);
  va_end(args);

  OutputDebugString(strbuf);
}

void error(char* message, ...)
{
  va_list args;
  fprintf(stdout, "error : ");

  va_start(args, message);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
}

int maxi(int a, int b)
{
  int m = a;
  if(a < b)
    m = b;
  return m;
}

bool32 is_letter_char(char ch)
{
  return ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z');
}

bool32 is_numeric_char(char c)
{
  return '0' <= c && c <= '9';
}

// The function assumes that all characters in the input string are digits,
// except for the first, which could be the negative sign '-'
bool32 str_to_int(char* string, int* integer)
{/*>>>*/
  bool32 negative = false;

  if(*string == '-')
  {
    negative = true;
    string++;
  }

  char c = *string++;
  if(is_numeric_char(c))
  {
    int result = (int)(c - '0');

    for(c = *string++; c != '\0'; c = *string++)
    {
      if(is_numeric_char(c))
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
}/*<<<*/

bool32 str_start_with(char* str_a, char* prefix)
{/*>>>*/
  while(*str_a == *prefix)
  {
    str_a++;
    prefix++;
    if(*prefix == '\0')
      break;
  }
  bool32 result = (*prefix == '\0');
  return result;
}/*<<<*/

#define str_match str_equals
bool32 str_equals(char* str_a, char* str_b)
{/*>>>*/
  while(*str_a == *str_b)
  {
    str_a++;
    str_b++;
    if(*str_a == '\0')
      break;
  }
  bool32 result = (*str_a == *str_b);
  return result;
}/*<<<*/

int str_len(char* str)
{
  int len = 0;
  while(*str++ != 0)
    len++;
  return len;
}

char* copy_cstr(char* dest_str, char* src_str)
{
  do
  {
    *dest_str++ = *src_str++;
  }
  while(*src_str);

  return dest_str;
}

char* copy_str(StringArena* dest, char* src_str)
{
  char* new_tail = copy_cstr((char* )dest->free, src_str);
  assert(new_tail < (char*)dest->limit);
  dest->free = new_tail;
  return new_tail;
}

void copy_substr(char* dest_str, char* begin_char, char* end_char)
{
  char* src_str = begin_char;

  do
    *dest_str++ = *src_str++;
  while(src_str <= end_char);

  dest_str = 0;
}

#if 0
#define AllocStack1(TYPE) (TYPE*)AllocStack_(sizeof(TYPE), 1)
#define AllocStack(TYPE, COUNT) (TYPE*)AllocStack_(sizeof(TYPE), COUNT)

void* AllocStack_(int elementSize, int count)
{
  return alloca(elementSize * count);
}
#endif

#define check_element_bounds(ARENA, TYPE, STRUCT) check_memory_bounds_(ARENA, sizeof(TYPE), STRUCT)
#define check_arena_bounds(ARENA) check_memory_bounds_((ARENA), 0, (ARENA)->free)

void check_memory_bounds_(MemoryArena* arena, int elementSize, void* ptr)
{
  assert(arena->base <= ptr);
  assert((uint8*)arena->free + elementSize <= (uint8*)arena->limit);
}

void clear_to_zero(void* first, void* onePastLast)
{
  uint8* byte = first;
  for(; byte < (uint8*)onePastLast; byte++)
  {
    *byte = 0;
  }
}

void reset_arena(MemoryArena* arena)
{
  arena->free = arena->base;
}

void set_watermark(MemoryArena* arena, void* ptr)
{
  check_memory_bounds_(arena, 0, ptr);
  arena->free = ptr;
}

#define first_element(ARENA, TYPE) ((TYPE* )(ARENA)->base)
#define last_element(ARENA, TYPE) ((TYPE* )(ARENA)->free - 1);
#define one_past_last_element(ARENA, TYPE) ((TYPE* )(ARENA)->free);
#define element_at(ARENA, TYPE, INDEX) ((TYPE* )(ARENA)->base + INDEX)
#define push_arena(ARENA, TYPE, COUNT) push_arena_(ARENA, sizeof(TYPE), COUNT, true)
#define push_arena_no_clear(ARENA, TYPE, COUNT) push_arena_(ARENA, sizeof(TYPE), COUNT, false)

MemoryArena push_arena_(MemoryArena* arena, int elementSize, int count, bool32 clearToZero)
{
  assert(count > 0);

  MemoryArena sub_arena = {0};
  sub_arena.base = arena->free;
  sub_arena.free = sub_arena.base;
  arena->free = (uint8*)sub_arena.base + elementSize*count;
  check_arena_bounds(arena);
  sub_arena.limit = arena->free;
  if(clearToZero)
  {
    clear_to_zero(sub_arena.base, sub_arena.limit);
  }
  return sub_arena;
}

#define push_one_element(ARENA, TYPE) ((TYPE* )push_element_(ARENA, sizeof(TYPE), 1))
#define push_element(ARENA, TYPE, COUNT) ((TYPE*)push_element_(ARENA, sizeof(TYPE), COUNT))
#define push_size(ARENA, COUNT) ((uint8* )push_element_(ARENA, sizeof(uint8), COUNT))

void* push_element_(MemoryArena* arena, int elementSize, int count)
{
  assert(count > 0);

  void* element = arena->free;
  arena->free = (uint8*)arena->free + elementSize*count;
  check_arena_bounds(arena);
  clear_to_zero(element, arena->free);
  return element;
}

MemoryArena new_arena(int size)
{
  MemoryArena arena = {0};
  arena.free = malloc(size);
  arena.limit = (uint8*)arena.free + size;
  return arena;
}

char* read_text_from_file(MemoryArena* arena, char* fileName)
{
  char* text = 0;
  FILE* file = fopen(fileName, "rb");
  if(file)
  {
    fseek(file, 0, SEEK_END);
    uint32 fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);
    text = (char*)push_size(arena, fileSize+1); // + NULL-terminator
    fread(text, fileSize, 1, file);
    fclose(file);
    text[fileSize] = '\0';
  }
  return text;
}

int read_stdin(char buf[], int bufSize)
{
  HANDLE h_std = GetStdHandle(STD_INPUT_HANDLE);
  DWORD bytesRead = 0;

  if(h_std && ReadFile(h_std, buf, bufSize, &bytesRead, 0))
  {
    if(bytesRead)
    {
      if(bytesRead < (uint)bufSize)
        buf[bytesRead] = '\0';
      else
        assert(!"bytesRead = bufSize");
    }
  }
  else
  {
    DWORD err = GetLastError();
    printf("Win32 error %d\n", err);
  }

  return (int)bytesRead;
}

void string_init(String* string, MemoryArena* arena)
{
  string->arena = arena;
  string->start = push_element(arena, char, 1);
  *string->start = '\0';
  string->end = string->start;
}

//NOTE: The 0-terminator is not counted.
uint string_len(String* string)
{
  assert(string->start <= string->end);
  uint len = (uint)(string->end - string->start);
  return len;
}

void append_string(String* string, char* cstr)
{
  MemoryArena* arena = string->arena;
  assert(string->start <= string->end);
  assert(string->end == (char*)arena->free-1);
  int len = str_len(cstr);
  push_element(arena, char, len);
  copy_cstr(string->end, cstr);
  string->end = (char*)arena->free-1;
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

void list_init(List* list)
{
  list->last = &list->sentinel;
}

void list_append(MemoryArena* arena, List* list, ListElem* elem)
{
  ListItem* item = push_element(arena, ListItem, 1);
  item->elem = elem;
  list->last->next = item;
  item->prev = list->last;
  list->last = item;
  list->count++;
}

ListItem* list_first_item(List* list)
{
  return list->sentinel.next;
}
