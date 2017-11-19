bool char_is_letter(char ch)
{
  return ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z');
}

bool char_is_numeric(char c)
{
  return '0' <= c && c <= '9';
}

#define struct_check_bounds(ARENA, TYPE, STRUCT)\
  mem_check_bounds_f(ARENA, sizeof(TYPE), STRUCT)

#define arena_check_bounds(ARENA)\
  mem_check_bounds_f((ARENA), 0, (ARENA)->free)

#define mem_push_struct(ARENA, TYPE)\
  ((TYPE*)mem_push_struct_f(ARENA, sizeof(TYPE), 1, true))

#define mem_push_array(ARENA, TYPE, COUNT)\
  ((TYPE*)mem_push_struct_f(ARENA, sizeof(TYPE), COUNT, true))

#define mem_push_array_nz(ARENA, TYPE, COUNT)\
  ((TYPE*)mem_push_struct_f(ARENA, sizeof(TYPE), COUNT, false))

#define mem_zero_struct(VAR, TYPE)\
  (mem_zero_f(VAR, sizeof(TYPE)))

#define mem_zero_array(VAR, TYPE)\
  (mem_zero_f(VAR, sizeof_array(VAR) * sizeof(TYPE)))

void mem_check_bounds_f(MemoryArena* arena, int elem_size, void* ptr)
{
  assert(arena->base <= (uint8*)ptr);
  assert((arena->free + elem_size) < arena->cap);
}

void mem_zero_range(void* start, void* one_past_end)
{
  assert(one_past_end >= start);
  int len = (int)((uint8*)one_past_end - (uint8*)start);
  mem_zero_f(start, len);
}

void free_arena(MemoryArena* arena)
{
  arena->base = (uint8*)arena + sizeof(MemoryArena);
  arena->free = arena->base;
  assert(arena->free < arena->cap);

  if(DEBUG_zero_arena)
  {
    mem_zero_range(arena->free, arena->cap);
  }
}

void pop_arena(MemoryArena** arena)
{
  *arena = (*arena)->prev_arena;

  MemoryArena* curr_arena = *arena;
  if(DEBUG_zero_arena)
  {
    mem_zero_range(curr_arena->free, curr_arena->cap);
  }
}

MemoryArena* push_arena(MemoryArena** arena, int size)
{
  assert(size > 0);

  MemoryArena* prev_arena = *arena;

  MemoryArena* sub_arena = (MemoryArena*)prev_arena->free;
  mem_zero_struct(sub_arena, MemoryArena);
  sub_arena->base =(uint8*)sub_arena + sizeof(MemoryArena);
  sub_arena->free = sub_arena->base;
  sub_arena->cap = sub_arena->base + size;
  assert(sub_arena->cap <= prev_arena->cap);
  if(DEBUG_zero_arena)
  {
    mem_zero_range(sub_arena->base, sub_arena->cap);
  }

  MemoryArena* new_arena = (MemoryArena*)sub_arena->cap;
  mem_zero_struct(new_arena, MemoryArena);
  new_arena->base = (uint8*)new_arena + sizeof(MemoryArena);
  new_arena->free = new_arena->base;
  new_arena->cap = prev_arena->cap;
  assert(new_arena->free < new_arena->cap);

  new_arena->prev_arena = prev_arena;
  *arena = new_arena;

  return sub_arena;
}

void begin_temp_memory(MemoryArena** arena)
{
  MemoryArena* prev_arena = *arena;

  MemoryArena* new_arena = (MemoryArena*)prev_arena->free;
  mem_zero_struct(new_arena, MemoryArena);
  new_arena->free = (uint8*)new_arena + sizeof(MemoryArena);
  new_arena->cap = prev_arena->cap;
  assert(new_arena->free < new_arena->cap);

  new_arena->prev_arena = prev_arena;
  *arena = new_arena;
}

void end_temp_memory(MemoryArena** arena)
{
  pop_arena(arena);
}

void* mem_push_struct_f(MemoryArena* arena, int elem_size, int count, bool zero_mem)
{
  assert(count > 0);

  void* element = arena->free;
  arena->free = arena->free + elem_size*count;

  if(DEBUG_check_arena_bounds)
  {
    arena_check_bounds(arena);
  }
  if(zero_mem)
  {
    mem_zero_range(element, arena->free);
  }
  return element;
}

ArenaUsage arena_usage(MemoryArena* arena)
{
  ArenaUsage usage = {0};
#if 0
  uint8* base = (uint8*)arena + sizeof(MemoryArena);
#else
  uint8* base = arena->base;
#endif
  assert(arena->cap > base);
  usage.total_avail = (int)(arena->cap - base);
  usage.in_use = (arena->free - base) / (double)usage.total_avail;
  return usage;
}

bool cstr_to_int(char* str, int* integer)
{
  bool negative = false;

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

bool cstr_to_float(char* str, float* result)
{
#if 0
  bool negative = false;

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
  if(h_sscanf(str, "%f", result) != 1)
    return false;
#endif

  return true;
}

bool cstr_contains_char(char* str, char c)
{
  bool result = false;
  while(*str && !result)
  {
    result = (*str == c);
    str++;
  }
  return result;
}

bool cstr_start_with(char* str, char* prefix)
{
  while(*str == *prefix)
  {
    str++;
    prefix++;
    if(*prefix == '\0')
      break;
  }
  bool result = (*prefix == '\0');
  return result;
}

bool cstr_match(char* str_a, char* str_b)
{
  while(*str_a == *str_b)
  {
    str_a++;
    str_b++;
    if(*str_a == '\0')
      break;
  }
  bool result = (*str_a == *str_b);
  return result;
}

int cstr_len(char* str)
{
  int len = 0;
  while(*str++ != 0)
    len++;
  return len;
}

char* cstr_copy(char* dest_str, char* src_str)
{
  do
    *dest_str++ = *src_str++;
  while(*src_str);
  return dest_str;
}

void cstr_append(char* dest_str, char* src_str)
{
  while(*dest_str)
    dest_str++;

  do
    *dest_str++ = *src_str++;
  while(*src_str);
  *dest_str = '\0';
}

void cstr_copy_substr(char* dest_str, char* begin_char, char* end_char)
{
  char* src_str = begin_char;

  do
    *dest_str++ = *src_str++;
  while(src_str <= end_char);
}

/* TODO: Check arena boundaries in the str_* functions */

void str_init(String* str, MemoryArena* arena)
{
  // Two Strings cannot be both attached to an Arena at the same time
  assert(!arena->str);

  str->arena = arena;
  str->head = mem_push_struct(arena, char);
  str->end = str->head;
  arena->str = str;
}

int str_len(String* str)
{
  assert(str->head <= str->end);
  int len = (int)(str->end - str->head);
  return len;
}

#if 0
void str_stdout(String* str)
{
  fputs(str->head, stdout);
}
#endif

String* str_new(MemoryArena* arena)
{
  String* str = mem_push_struct(arena, String);
  str_init(str, arena);
  return str;
}

void str_append(String* str, char* cstr)
{
  assert(str->head && str->end && str->arena);
  MemoryArena* arena = str->arena;
  assert(str->head <= str->end);
  assert(str->end == (char*)arena->free-1);

  int len = cstr_len(cstr);
  if(len > 0)
  {
    mem_push_array_nz(arena, char, len); // will implicitly check arena bounds
    cstr_copy(str->end, cstr);
    str->end = (char*)arena->free-1;
  }
}

void str_printf_va(String* str, char* fmessage, va_list varargs)
{
  assert(str->head && str->end && str->arena);
  MemoryArena* arena = str->arena;
  assert(str->head <= str->end);
  assert(str->end == (char*)arena->free-1);

  int len = h_vsprintf(str->end, fmessage, varargs);
  str->end += len;
  assert(str->end < (char*)arena->cap);
  arena->free = (uint8*)str->end+1;
}

void str_printf(String* str, char* fmessage, ...)
{
  va_list varargs;
  va_start(varargs, fmessage);
  str_printf_va(str, fmessage, varargs);
  va_end(varargs);
}

void str_tidyup(String* str)
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

void str_free(String* str)
{
  assert(str->head <= str->end);
  MemoryArena* arena = str->arena;
  assert(str->head <= str->end);
  assert(str->end == (char*)arena->free-1);

  arena->free = (uint8*)str->head;
}

char* str_cap(String* str)
{
  *(str->end++) = 0;
  assert(str->end < (char*)str->arena->cap);
  str->arena->str = 0;
  return str->head;
}

bool str_dump_to_file(String* str, char* file_path)
{
  int char_count = str_len(str);
  int bytes_written = file_write_bytes(file_path, (uint8*)str->head, str_len(str));
  return (char_count == bytes_written);
}

void print_char(char buf[3], char raw_char)
{
  if(raw_char == '\0')
    cstr_copy(buf, "\\0");
  else if(raw_char == '\t')
    cstr_copy(buf, "\\t");
  else if(raw_char == '\n')
    cstr_copy(buf, "\\n");
  else if(raw_char == '\r')
    cstr_copy(buf, "\\r");
  else if(raw_char == '\'')
    cstr_copy(buf, "\\'");
  else
    *buf = raw_char;
}

List* new_list(MemoryArena* arena, eList kind)
{
  List* list = mem_push_struct(arena, List);
  list->kind = kind;
  list->arena = arena;
  return list;
}

void remove_list_item(List* list, ListItem* item)
{
  if(item->prev)
  {
    item->prev->next = item->next;
    if(item->next)
      item->next->prev = item->prev;
  }

  if(item == list->first && item == list->last)
    list->first = list->last = 0;
  else if(item == list->first)
  {
    list->first = item->next;
    if(list->first)
      list->first->prev = 0;
  }
  else if(item == list->last)
  {
    list->last = item->prev;
    if(list->last)
      list->last->next = 0;
  }

  item->next = item->prev = 0;
}

void append_list_item(List* list, ListItem* item)
{
  assert(list->kind == item->kind);

  if(list->last)
  {
    item->prev = list->last;
    item->next = 0;
    list->last->next = item;
    list->last = item;
  }
  else
  {
    list->first = list->last = item;
    item->next = item->prev = 0;
  }
}

void append_list_elem(List* list, void* elem, eList kind)
{
  ListItem* item = mem_push_struct(arena, ListItem);
  item->elem = elem;
  item->kind = kind;
  append_list_item(list, item);
}

void prepend_list_item(List* list, ListItem* item)
{
  assert(list->kind == item->kind);

  if(list->first)
  {
    item->next = list->first;
    item->prev = 0;
    list->first->prev = item;
    list->first = item;
  }
  else
  {
    list->first = list->last = item;
    item->next = item->prev = 0;
  }
}

void prepend_list_elem(List* list, void* elem, eList kind)
{
  ListItem* item = mem_push_struct(arena, ListItem);
  item->elem = elem;
  item->kind = kind;
  prepend_list_item(list, item);
}

void replace_list_item_at(List* list_a, List* list_b, ListItem* at_b_item)
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
}

void join_list_pair(List* list_a, List* list_b)
{
  ListItem* last_a_item = list_a->last;
  ListItem* first_b_item = list_b->first;

  if(last_a_item)
    last_a_item->next = list_b->first;

  if(first_b_item)
    first_b_item->prev = last_a_item;
}
