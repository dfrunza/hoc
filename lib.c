#include "hocc.h"

local bool32 DEBUG_zero_arena = false;
local bool32 DEBUG_check_arena_bounds = true;
local bool32 DEBUG_enabled = true;

void
assert_f(char* message, char* file, int line)
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

void
fail_f(char* file, int line, char* message, ...)
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

bool32
error_f(char* file, int line, char* message, ...)
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

#define struct_check_bounds(ARENA, TYPE, STRUCT)\
  mem_check_bounds_f(ARENA, sizeof(TYPE), STRUCT)
#define arena_check_bounds(ARENA)\
  mem_check_bounds_f((ARENA), 0, (ARENA)->free)

void
mem_check_bounds_f(MemoryArena* arena, int elem_size, void* ptr)
{
  assert(arena->alloc <= (uint8*)ptr);
  assert((arena->free + elem_size) < arena->cap);
}

void
mem_zero_f(void* mem, size_t len)
{
  memset(mem, 0, len);
}

void
mem_zero_range(void* start, void* one_past_end)
{
  size_t len = (uint8*)one_past_end - (uint8*)start;
  assert(len >= 0);
  mem_zero_f(start, len);
}

void
arena_free(MemoryArena* arena)
{
  arena->free = arena->alloc;
}

void
arena_reset(MemoryArena* arena)
{
  arena->alloc = (uint8*)arena + sizeof(MemoryArena);
  arena->free = arena->alloc;
  assert(arena->free < arena->cap);
}

MemoryArena*
arena_pop(MemoryArena* arena)
{
  MemoryArena* host = arena->host;
  assert(host->free == arena->cap);
  assert(host->alloc == host->free);
  if(DEBUG_zero_arena)
    mem_zero_range((uint8*)arena, arena->cap);
  host->free = (uint8*)arena;

  if(--host->sub_arena_count > 0)
    host->alloc = host->free;
  else
    host->alloc = (uint8*)host + sizeof(MemoryArena);
  return host;
}

MemoryArena*
arena_push(MemoryArena* arena, size_t size)
{
  assert(size > 0);

  MemoryArena sub_arena = {0};
  sub_arena.alloc = arena->free + sizeof(MemoryArena);
  sub_arena.free = sub_arena.alloc;

  MemoryArena* sub_arena_p = (MemoryArena*)arena->free;
  arena->free = arena->free + size + sizeof(MemoryArena);
  if(DEBUG_check_arena_bounds)
    arena_check_bounds(arena);
  arena->alloc = arena->free;
  arena->sub_arena_count++;

  sub_arena.cap = arena->free;
  sub_arena.host = arena;
  *sub_arena_p = sub_arena;

  if(DEBUG_zero_arena)
    mem_zero_range(sub_arena.alloc, sub_arena.cap);
  return sub_arena_p;
}

void*
mem_push_struct_f(MemoryArena* arena, size_t elem_size, size_t count, bool32 zero_mem)
{
  assert(count > 0);

  void* element = arena->free;
  arena->free = arena->free + elem_size*count;

  if(DEBUG_check_arena_bounds)
    arena_check_bounds(arena);
  if(zero_mem)
    mem_zero_range(element, arena->free);
  return element;
}

MemoryArena*
arena_new(int size)
{
  void* raw_mem = VirtualAlloc(0, size + sizeof(MemoryArena), MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
  MemoryArena* arena = (MemoryArena*)raw_mem;
  arena->alloc = (uint8*)arena + sizeof(MemoryArena);
  arena->free = arena->alloc;
  arena->cap = arena->free + size;
  return arena;
}

ArenaUsage
arena_usage(MemoryArena* arena)
{
  ArenaUsage usage = {0};
#if 0
  uint8* base = (uint8*)arena + sizeof(MemoryArena);
#else
  uint8* base = arena->alloc;
#endif
  usage.total_avail = arena->cap - base;
  usage.in_use = (arena->free - base) / (double)usage.total_avail;
  return usage;
}

bool32
cstr_to_int(char* str, int* integer)
{
  bool32 negative = false;

  if(*str == '-')
  {
    negative = true;
    str++;
  }

  char c = *str++;
  if(char_is_numeric(c))
  {
    int result = (int)(c - '0');

    for(c = *str++; c != '\0'; c = *str++)
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
cstr_to_float(char* str, float* result)
{
#if 0
  bool32 negative = false;

  if(*str == '-')
  {
    negative = true;
    str++;
  }

  char c = *str++;
  if(char_is_numeric(c))
  {
    int int_part = (c - '0');
    for(c = *str++; c != '\0' && c != '.'; c = *str++)
    {
      if(char_is_numeric(c))
      {
        int_part = int_part*10 + (c - '0');
      }
      else if(c != '.')
        return false;
    }

    float float_val = (float)int_part;

    if(c == '.')
    {
      float fract_part = 0.0;

      c = *str++;
      for(c = *str++; c != '\0'; c = *str++)
      {
        if(char_is_numeric(c))
        {
          fract_part = fract_part + ((c - '0') / 10.0f);
        }
        else
          return false;
      }
      float_val += fract_part;
    }

    if(negative)
      float_val = -float_val;
    *result = float_val;
  }
  else
    return false;
#else
  if(sscanf(str, "%f", result) != 1)
    return false;
#endif

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
    *dest_str++ = *src_str++;
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
}

/* TODO: Check arena boundaries in the str_* functions */

void
str_init(String* str, MemoryArena* arena)
{
  str->arena = arena;
  str->head = mem_push_struct(arena, char);
  str->end = str->head;
}

uint
str_len(String* str)
{
  assert(str->head <= str->end);
  uint len = (uint)(str->end - str->head);
  return len;
}

void
str_stdout(String* str)
{
  fputs(str->head, stdout);
}

String*
str_new(MemoryArena* arena)
{
  String* str = mem_push_struct(arena, String);
  str_init(str, arena);
  return str;
}

void
str_append(String* str, char* cstr)
{
  assert(str->head && str->end && str->arena);
  MemoryArena* arena = str->arena;
  assert(str->head <= str->end);
  assert(str->end == (char*)arena->free-1);

  int len = cstr_len(cstr);
  if(len > 0)
  {
    mem_push_count_nz(arena, char, len); // will implicitly check bounds
    cstr_copy(str->end, cstr);
    str->end = (char*)arena->free-1;
  }
}

void
str_printf_va(String* str, char* fmessage, va_list varargs)
{
  assert(str->head && str->end && str->arena);
  MemoryArena* arena = str->arena;
  assert(str->head <= str->end);
  assert(str->end == (char*)arena->free-1);

  int len = vsprintf(str->end, fmessage, varargs);
  str->end += len;
  assert(str->end < (char*)arena->cap);
  arena->free = (uint8*)str->end+1;
}

void
str_printf(String* str, char* fmessage, ...)
{
  va_list varargs;
  va_start(varargs, fmessage);
  str_printf_va(str, fmessage, varargs);
  va_end(varargs);
}

void
str_tidyup(String* str)
{
  assert(str->head <= str->end);
  if(str->end > str->head)
  {
    char* p_end = str->end - 1;
    while(p_end >= str->head && *p_end)
      p_end--;
    str->end = p_end;
    MemoryArena* arena = str->arena;
    arena->free = (uint8*)str->end+1;
  }
}

void
str_free(String* str)
{
  assert(str->head <= str->end);
  MemoryArena* arena = str->arena;
  assert(str->head <= str->end);
  assert(str->end == (char*)arena->free-1);

  arena->free = (uint8*)str->head;
}

bool32
str_dump_to_file(String* str, char* file_path)
{
  int char_count = str_len(str);
  int bytes_written = file_write_bytes(file_path, (uint8*)str->head, str_len(str));
  return (char_count == bytes_written);
}

char*
str_cap(String* str)
{
  *(str->end++) = 0;
  assert(str->end < (char*)str->arena->cap);
  return str->head;
}

char*
path_find_leaf(char* file_path)
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

char*
path_make_stem(char* file_path)
{
  char* leaf = path_find_leaf(file_path);

  /* remove the filename extension */
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

int
file_write_bytes(char* file_path, uint8* bytes, int count)
{
  int bytes_written = 0;
  FILE* h_file = fopen(file_path, "wb");
  if(h_file)
  {
    bytes_written = (int)fwrite(bytes, 1, count, h_file);
    fclose(h_file);
  }
  return bytes_written;
}

int
file_read_bytes(MemoryArena* arena, uint8** bytes, char* file_path)
{
  *bytes = 0;
  int byte_count = 0;
  FILE* file = fopen(file_path, "rb");
  if(file)
  {
    fseek(file, 0, SEEK_END);
    byte_count = ftell(file);
    fseek(file, 0, SEEK_SET);
    *bytes = mem_push_count_nz(arena, uint8, byte_count);
    fread(*bytes, byte_count, 1, file);
    fclose(file);
  }
  return byte_count;
}

char*
file_read_text(MemoryArena* arena, char* file_path)
{
  char* text = 0;
  file_read_bytes(arena, &(uint8*)text, file_path);
  *mem_push_count(arena, char, 1) = '\0'; // NULL terminator
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

void
list_init(List* list)
{
  mem_zero_struct(list, List);
}

List*
list_new(MemoryArena* arena)
{
  List* list = mem_push_struct(arena, List);
  list_init(list);
  return list;
}

void
list_remove_item(List* list, ListItem* item)
{
  if(item->prev)
  {
    item->prev->next = item->next;
    if(item->next)
      item->next->prev = item->prev;
  }

  if(item == list->first)
    list->first = item->next;
  if(item == list->last)
    list->last = item->prev;

  if(DEBUG_enabled)/*>>>*/
  {
    item->next = 0;
    item->prev = 0;
  }/*<<<*/
}

void
list_append_item(List* list, ListItem* item)
{
  if(list->last)
  {
    item->prev = list->last;
    list->last->next = item;
    list->last = item;
  }
  else
    list->first = list->last = item;

  if(DEBUG_enabled)/*>>>*/
    item->next = 0;/*<<<*/
}

void
list_append(MemoryArena* arena, List* list, void* elem)
{
  ListItem* item = mem_push_struct(arena, ListItem);
  item->elem = elem;
  list_append_item(list, item);
}

void
list_replace_item_at(List* list_a, List* list_b, ListItem* at_b_item)
{
  ListItem* prev_b_item = at_b_item->prev;
  ListItem* next_b_item = at_b_item->next;

  if(list_a->first)
  {
    if(prev_b_item)
      prev_b_item->next = list_a->first;
    else
      list_b->first = list_a->first;
  }
  else
  {
    if(prev_b_item)
      prev_b_item->next = next_b_item;
    else
      list_b->first = next_b_item;
  }

  if(next_b_item)
  {
    next_b_item->prev = list_a->last;
    if(list_a->last)
      list_a->last->next = next_b_item;
  }
  else
    list_b->last = list_a->last;

  if(DEBUG_enabled)/*>>>*/
  {
    at_b_item->next = 0;
    at_b_item->prev = 0;
  }/*<<<*/
}

