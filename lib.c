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

#define SizeofArray(ARRAY) (sizeof(ARRAY)/sizeof(ARRAY[0]))

void DebugPrint(char* message, ...)
{
  static char strbuf[128] = {0};
  va_list args;

  va_start(args, message);
  vsprintf(strbuf, message, args);
  va_end(args);

  OutputDebugString(strbuf);
}

void Error(char* message, ...)
{
  va_list args;
  fprintf(stdout, "P(0): Error : ");

  va_start(args, message);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
}

int Max(int a, int b)
{
  int max = a;
  if(a < b)
    max = b;
  return max;
}

bool32 IsLetterChar(char ch)
{
  return ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z');
}

bool32 IsNumericChar(char c)
{
  return '0' <= c && c <= '9';
}

// The function assumes that all characters in the input string are digits,
// except for the first, which could be the negative sign '-'
bool32 StrToInt(char* string, int* integer)
{/*>>>*/
  bool32 negative = false;

  if(*string == '-')
  {
    negative = true;
    string++;
  }

  char c = *string++;
  if(IsNumericChar(c))
  {
    int result = (int)(c - '0');

    for(c = *string++; c != '\0'; c = *string++)
    {
      if(IsNumericChar(c))
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

bool32 StrStartsWith(char* strA, char* prefix)
{/*>>>*/
  while(*strA == *prefix)
  {
    strA++;
    prefix++;
    if(*prefix == '\0')
      break;
  }
  bool32 result = (*prefix == '\0');
  return result;
}/*<<<*/

#define StrMatch StrEquals
bool32 StrEquals(char* strA, char* strB)
{/*>>>*/
  while(*strA == *strB)
  {
    strA++;
    strB++;
    if(*strA == '\0')
      break;
  }
  bool32 result = (*strA == *strB);
  return result;
}/*<<<*/

int StrLen(char* str)
{
  int len = 0;
  while(*str++ != 0)
    len++;
  return len;
}

char* CopyCStr(char* destStr, char* srcStr)
{
  do
  {
    *destStr++ = *srcStr++;
  }
  while(*srcStr);

  return destStr;
}

char* CopyStr(StringArena* dest, char* srcStr)
{
  char* newTail = CopyCStr((char* )dest->free, srcStr);
  assert(newTail < (char*)dest->limit);
  dest->free = newTail;
  return newTail;
}

void CopySubstr(char* destStr, char* beginChar, char* endChar)
{
  char* srcStr = beginChar;

  do
    *destStr++ = *srcStr++;
  while(srcStr <= endChar);

  destStr = 0;
}

#if 0
#define AllocStack1(TYPE) (TYPE*)AllocStack_(sizeof(TYPE), 1)
#define AllocStack(TYPE, COUNT) (TYPE*)AllocStack_(sizeof(TYPE), COUNT)

void* AllocStack_(int elementSize, int count)
{
  return alloca(elementSize * count);
}
#endif

#define CheckElementBounds(ARENA, TYPE, STRUCT) CheckMemoryBounds_(ARENA, sizeof(TYPE), STRUCT)
#define CheckArenaBounds(ARENA) CheckMemoryBounds_((ARENA), 0, (ARENA)->free)

void CheckMemoryBounds_(MemoryArena* arena, int elementSize, void* ptr)
{
  assert(arena->base <= ptr);
  assert((uint8*)arena->free + elementSize <= (uint8*)arena->limit);
}

void ClearToZero(void* first, void* onePastLast)
{
  uint8* byte = (uint8* )first;
  for(; byte < (uint8* )onePastLast; byte++)
  {
    *byte = 0;
  }
}

void ResetArena(MemoryArena* arena)
{
  arena->free = arena->base;
}

void SetWatermark(MemoryArena* arena, void* ptr)
{
  CheckMemoryBounds_(arena, 0, ptr);
  arena->free = ptr;
}

#define FirstElement(ARENA, TYPE) ((TYPE* )(ARENA)->base)
#define LastElement(ARENA, TYPE) ((TYPE* )(ARENA)->free - 1);
#define OnePastLastElement(ARENA, TYPE) ((TYPE* )(ARENA)->free);
#define ElementAt(ARENA, TYPE, INDEX) ((TYPE* )(ARENA)->base + INDEX)
#define PushArena(ARENA, TYPE, COUNT) PushArena_(ARENA, sizeof(TYPE), COUNT, true)
#define PushArenaDontClear(ARENA, TYPE, COUNT) PushArena_(ARENA, sizeof(TYPE), COUNT, false)

MemoryArena PushArena_(MemoryArena* arena, int elementSize, int count, bool32 clearToZero)
{
  assert(count > 0);

  MemoryArena subArena = {0};
  subArena.base = arena->free;
  subArena.free = subArena.base;
  arena->free = (uint8* )subArena.base + elementSize*count;
  CheckArenaBounds(arena);
  subArena.limit = arena->free;
  if(clearToZero)
  {
    ClearToZero(subArena.base, subArena.limit);
  }
  return subArena;
}

#define PushOneElement(ARENA, TYPE) ((TYPE* )PushElement_(ARENA, sizeof(TYPE), 1))
#define PushElement(ARENA, TYPE, COUNT) ((TYPE*)PushElement_(ARENA, sizeof(TYPE), COUNT))
#define PushSize(ARENA, COUNT) ((uint8* )PushElement_(ARENA, sizeof(uint8), COUNT))

void* PushElement_(MemoryArena* arena, int elementSize, int count)
{
  assert(count > 0);

  void* element = arena->free;
  arena->free = (uint8* )arena->free + elementSize*count;
  CheckArenaBounds(arena);
  ClearToZero(element, arena->free);
  return element;
}

MemoryArena NewArena(int size)
{
  MemoryArena arena = {0};
  arena.free = malloc(size);
  arena.limit = (uint8* )arena.free + size;
  return arena;
}

char* ReadTextFromFile(MemoryArena* arena, char* fileName)
{
  char* text = 0;
  FILE* file = fopen(fileName, "rb");
  if(file)
  {
    fseek(file, 0, SEEK_END);
    uint32 fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);
    text = (char*)PushSize(arena, fileSize+1); // + NULL-terminator
    fread(text, fileSize, 1, file);
    fclose(file);
    text[fileSize] = '\0';
  }
  return text;
}

int ReadStdin(char buf[], int bufSize)
{
  HANDLE hStd = GetStdHandle(STD_INPUT_HANDLE);
  DWORD bytesRead = 0;

  if(hStd && ReadFile(hStd, buf, bufSize, &bytesRead, 0))
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

void StringInit(String* string, MemoryArena* arena)
{
  string->arena = arena;
  string->start = PushElement(arena, char, 1);
  *string->start = '\0';
  string->end = string->start;
}

//NOTE: The 0-terminator is not counted.
uint StringLen(String* string)
{
  assert(string->start <= string->end);
  uint len = (uint)(string->end - string->start);
  return len;
}

void AppendString(String* string, char* cstr)
{
  MemoryArena* arena = string->arena;
  assert(string->start <= string->end);
  assert(string->end == (char*)arena->free-1);
  int len = StrLen(cstr);
  PushElement(arena, char, len);
  CopyCStr(string->end, cstr);
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

void ListInit(List* list)
{
  list->last = &list->sentinel;
}

void ListAppend(MemoryArena* arena, List* list, ListElem* elem)
{
  ListItem* item = PushElement(arena, ListItem, 1);
  item->elem = elem;
  list->last->next = item;
  item->prev = list->last;
  list->last = item;
  list->count++;
}

ListItem* ListFirstItem(List* list)
{
  return list->sentinel.next;
}
