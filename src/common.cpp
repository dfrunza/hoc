global_var bool DEBUG_enabled = true;
global_var bool DEBUG_zero_arena = true;
global_var bool DEBUG_check_arena_bounds = true;
global_var bool DEBUG_zero_struct = true;

#define breakpoint() for(int x = 0; x != 0; )
#define sizeof_array(array) (sizeof(array)/sizeof(array[0]))
#define max_int() ~(1 << (sizeof(int)*8 - 1))

#define assert(EXPR) do { if(!(EXPR)) assert_(#EXPR, __FILE__, __LINE__); } while(0)
void assert_(char* message, char* file, int line)
{
  if(DEBUG_enabled)
  {
    platform_printf("%s:%d: ", file, line);
    if(!message || message[0] == '\0')
    {
      message = "";
    }
    platform_printf("assert(%s)\n", message);

    *(int*)0 = 0;
  }
}

void mem_zero_(void* mem, int len)
{
  /* slow, but CRT-independent func */
  uint8* p_byte = (uint8*)mem;
  for(int i = 0; i < len; i++)
  {
    *p_byte = 0;
  }
}

MemoryArenaUsage arena_get_usage(MemoryArena* arena)
{
  MemoryArenaUsage usage = {0};
#if 0
  uint8* base = (uint8*)this + sizeof(MemoryArena);
#endif
  assert(arena->cap > arena->base);
  usage.total_avail = (int)(arena->cap - arena->base);
  usage.in_use = (arena->free - arena->base) / (double)usage.total_avail;
  return usage;
}

void DEBUG_print_arena_usage(MemoryArena* arena, char* tag)
{
  MemoryArenaUsage usage = arena_get_usage(arena);
  platform_printf("in_use(`%s`) : %.2f%%\n", tag, usage.in_use*100);
}

#define check_struct_bounds(ARENA, TYPE, STRUCT) check_arena_bounds((ARENA), sizeof(TYPE), STRUCT)
#define check_arena_bounds(ARENA) check_memory_bounds((ARENA), 0, (ARENA)->free)
void check_memory_bounds(MemoryArena* arena, int elem_size, void* ptr)
{
  assert(arena->base <= (uint8*)ptr);
  assert((arena->free + elem_size) < arena->cap);
}

#define zero_struct(VAR, TYPE) (mem_zero_(VAR, sizeof(TYPE)))
#define zero_array(VAR, TYPE) (mem_zero_(VAR, sizeof_array(VAR) * sizeof(TYPE)))
void mem_zero_range(void* start, void* one_past_end)
{
  assert(one_past_end >= start);
  int len = (int)((uint8*)one_past_end - (uint8*)start);
  mem_zero_(start, len);
}

MemoryArena* new_arena(int size)
{
  void* raw_mem = platform_alloc_memory(size + sizeof(MemoryArena));
  MemoryArena* arena = (MemoryArena*)raw_mem;
  zero_struct(arena, MemoryArena);
  arena->base = (uint8*)arena + sizeof(MemoryArena);
  arena->free = arena->base;
  arena->cap = arena->free + size;

  return arena;
}

void dealloc_arena(MemoryArena* arena)
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
  zero_struct(sub_arena, MemoryArena);
  sub_arena->base =(uint8*)sub_arena + sizeof(MemoryArena);
  sub_arena->free = sub_arena->base;
  sub_arena->cap = sub_arena->base + size;
  assert(sub_arena->cap <= prev_arena->cap);
  if(DEBUG_zero_arena)
  {
    mem_zero_range(sub_arena->base, sub_arena->cap);
  }

  MemoryArena* new_arena = (MemoryArena*)sub_arena->cap;
  zero_struct(new_arena, MemoryArena);
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
  zero_struct(new_arena, MemoryArena);
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

#define push_struct(ARENA, TYPE) ((TYPE*)push_struct_((ARENA), sizeof(TYPE), 1))
#define push_array(ARENA, TYPE, COUNT) ((TYPE*)push_struct_((ARENA), sizeof(TYPE), COUNT))
#define push_string(ARENA, LEN) (char*)push_struct_((ARENA), sizeof(char), LEN+1)

void* push_struct_(MemoryArena* arena, int elem_size, int count)
{
  assert(count > 0);

  void* element = arena->free;
  arena->free += elem_size*count;

  if(DEBUG_check_arena_bounds)
  {
    check_arena_bounds(arena);
  }

  if(DEBUG_zero_struct)
  {
    mem_zero_range(element, arena->free);
  }
  return element;
}

int bitpos(int k)
{
  int pos = 1;
  for(; (k & 1) == 0 && pos < 32/*int*/;
      k = k >> 1)
  {
    pos++;
  }
  return (k & 1) ? pos : 0;
}

bool cstr_is_letter(char c)
{
  return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z');
}

bool cstr_is_dec_digit(char c)
{
  return '0' <= c && c <= '9';
}

bool cstr_is_hex_digit(char c)
{
  return ('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f');
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
  if(cstr_is_dec_digit(c))
  {
    int result = (int)(c - '0');

    for(c = *str++; c != '\0'; c = *str++)
    {
      if(cstr_is_dec_digit(c))
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

void cstr_print_char(char buf[3], char c)
{
  if(c == '\0')
    cstr_copy(buf, "\\0");
  else if(c == '\t')
    cstr_copy(buf, "\\t");
  else if(c == '\n')
    cstr_copy(buf, "\\n");
  else if(c == '\r')
    cstr_copy(buf, "\\r");
  else if(c == '\'')
    cstr_copy(buf, "\\'");
  else
    *buf = c;
}

/* TODO: Check arena boundaries in the str_* functions */

void str_init(String* str, MemoryArena* arena)
{
  // Two Strings cannot be both attached to the same Arena at the same time
  assert(!arena->str);

  str->arena = arena;
  str->head = push_struct(arena, char);
  str->end = str->head;
  arena->str = str;
}

String* str_new(MemoryArena* arena)
{
  String* str = push_struct(arena, String);
  str_init(str, arena);

  return str;
}

int str_len(String* str)
{
  assert(str->head <= str->end);
  int len = (int)(str->end - str->head);
  return len;
}

void str_append(String* str, char* cstr)
{
  assert(str->head && str->end && str->arena);
  assert(str->head <= str->end);
  assert(str->end == (char*)str->arena->free-1);

  int len = cstr_len(cstr);
  if(len > 0)
  {
    push_array(str->arena, char, len); // will implicitly check arena bounds
    cstr_copy(str->end, cstr);
    str->end = (char*)str->arena->free-1;
  }
}

void str_nl(String* str)
{
  str_append(str, "\n");
}

void str_append_nl(String* str, char* cstr)
{
  str_append(str, cstr);
  str_nl(str);
}

int str_format_va(String* str, char* fmessage, va_list args)
{
  assert(str->head && str->end && str->arena);
  assert(str->head <= str->end);
  assert(str->end == (char*)str->arena->free-1);

  int len = platform_sprintf_va(str->end, fmessage, args);
  str->end += len;
  assert(str->end < (char*)str->arena->cap);
  str->arena->free = (uint8*)str->end+1;

  return len;
}

int str_format(String* str, char* ftext, ...)
{
  va_list args;
  va_start(args, ftext);

  int text_len = str_format_va(str, ftext, args);

  va_end(args);
  return text_len;
}

int str_format_nl(String* str, char* fline, ...)
{
  va_list args;
  va_start(args, fline);

  int text_len = str_format_va(str, fline, args);

  va_end(args);

  str_append(str, "\n");
  text_len++;

  return text_len;
}

void str_tidyup(String* str)
{
  assert(str->head <= str->end);
  if(str->end > str->head)
  {
    char* p_end = str->end - 1;
    while(p_end >= str->head && *p_end)
    {
      p_end--;
    }
    str->end = p_end;
    str->arena->free = (uint8*)str->end+1;
  }
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
  int bytes_written = platform_file_write_bytes(file_path, (uint8*)str->head, char_count);
  return (char_count == bytes_written);
}

char* platform_path_find_file_name(char* file_path)
{
  char* p_char = file_path;
  char* name = p_char;

  /* get the file name part */
  while(p_char && *p_char)
  {
    while(*p_char && *p_char != '\\')
    {
      p_char++;
    }

    if(*p_char == '\\')
    {
      name = ++p_char;
    }
  }

  return name;
}

char* platform_path_make_file_name(char* file_path, bool with_extension)
{
  char* name = platform_path_find_file_name(file_path);

  /* remove the filename extension */
  if(name && !with_extension)
  {
    char* p_char = name;
    while(*p_char && *p_char != '.')
      p_char++;
    *p_char = '\0';
  }

  return name;
}

char* platform_path_make_dir(char* file_path)
{
  char* name = platform_path_find_file_name(file_path);
  if(name)
    *name = '\0';
  return file_path;
}

#define fail(MESSAGE, ...) fail_(__FILE__, __LINE__, (MESSAGE), ## __VA_ARGS__)
void fail_(char* file, int line, char* message, ...)
{
  platform_printf("%s:%d: fail : ", file, line);

  if(!message || message[0] == '\0')
  {
    message = "";
  }

  va_list args;
  va_start(args, message);

  platform_printf_va(message, args);

  va_end(args);

  platform_printf("\n");
  *(int*)0 = 0;
}

#define error(MESSAGE, ...) error_(__FILE__, __LINE__, (MESSAGE), ## __VA_ARGS__)
bool error_(char* file, int line, char* message, ...)
{
  platform_printf("%s:%d: error : ", file, line);

  if(!message || message[0] == '\0')
  {
    message = "";
  }

  va_list args;
  va_start(args, message);

  platform_printf_va(message, args);

  va_end(args);

  platform_printf("\n");

  return false;
}

bool compile_error_va(MemoryArena* arena, char* file, int line, SourceLoc* src_loc, char* message, va_list args)
{
  char* filename_buf = push_array(arena, char, cstr_len(file));
  cstr_copy(filename_buf, file);

  if(src_loc && src_loc->line_nr >= 0)
  {
    platform_printf("%s:%d: (%s:%d) error : ", src_loc->file_path, src_loc->line_nr,
                     platform_path_make_file_name(filename_buf, false), line);
  }
  else
  {
    platform_printf("%s:%d: error : ", file, line);
  }

  platform_printf_va(message, args);

  platform_printf("\n");

  return false;
}

#define compile_error(ARENA, SRC, MESSAGE, ...) compile_error_((ARENA), __FILE__, __LINE__, (SRC), (MESSAGE), ## __VA_ARGS__)
bool compile_error_(MemoryArena* arena, char* file, int line, SourceLoc* src_loc, char* message, ...)
{
  va_list args;
  va_start(args, message);

  bool result = compile_error_va(arena, file, line, src_loc, message, args);

  va_end(args);
  return result;
}

void list_init(List* list, MemoryArena* arena, eList kind)
{
  list->kind = kind;
  list->arena = arena;
  list->first = list->last = 0;
  list->count = 0;
}

List* list_new(MemoryArena* arena, eList kind)
{
  List* list = push_struct(arena, List);
  list_init(list, arena, kind);
  return list;
}

void list_remove_item(List* list, ListItem* item)
{
  if(item->prev)
  {
    item->prev->next = item->next;
    if(item->next)
      item->next->prev = item->prev;
  }

  if(item == list->first && item == list->last)
  {
    list->first = list->last = 0;
  }
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

  list->count--;
}

void list_append_item(List* list, ListItem* item)
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

  list->count++;
}

void list_append(List* list, void* elem, eList kind)
{
  assert(elem);
  ListItem* item = push_struct(list->arena, ListItem);
  item->elem = elem;
  item->kind = kind;
  list_append_item(list, item);
}

void list_prepend_item(List* list, ListItem* item)
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

  list->count++;
}

void list_prepend(List* list, void* elem, eList kind)
{
  ListItem* item = push_struct(list->arena, ListItem);
  item->elem = elem;
  item->kind = kind;
  list_prepend_item(list, item);
}

void list_replace_item_at(List* list_A, List* list_B, ListItem* at_b_item)
{
  ListItem* prev_b_item = at_b_item->prev;
  ListItem* next_b_item = at_b_item->next;

  if(list_A->first)
  {
    if(prev_b_item)
      prev_b_item->next = list_A->first;
    else
      list_B->first = list_A->first;
  }
  else
  {
    if(prev_b_item)
      prev_b_item->next = next_b_item;
    else
      list_B->first = next_b_item;
  }

  if(next_b_item)
  {
    next_b_item->prev = list_A->last;
    if(list_A->last)
      list_A->last->next = next_b_item;
  }
  else
    list_B->last = list_A->last;

  list_B->count += list_A->count - 1;
}

void list_join(List* list_A, List* list_B)
{
  ListItem* last_a_item = list_A->last;
  ListItem* first_b_item = list_B->first;

  if(last_a_item)
    last_a_item->next = first_b_item;

  if(first_b_item)
    first_b_item->prev = last_a_item;

  if(!list_A->first)
    list_A->first = list_B->first;

  list_A->last = list_B->last;

  list_A->count += list_B->count;
}

void list_insert_item_before(List* list, ListItem* at_li, ListItem* new_li)
{
  assert(at_li);
  if(at_li->prev)
  {
    at_li->prev->next = new_li;
  }

  if(at_li == list->first)
  {
    list->first = new_li;
  }

  new_li->next = at_li;
  new_li->prev = at_li->prev;
  at_li->prev = new_li;

  list->count++;
}

void list_insert_before(List* list, ListItem* at_li, void* elem, eList kind)
{
  assert(at_li);
  ListItem* new_li = push_struct(list->arena, ListItem);
  new_li->elem = elem;
  new_li->kind = kind;
  list_insert_item_before(list, at_li, new_li);
}

/* FIXME: Not tested!! */
void list_insert_item_after(List* list, ListItem* at_li, ListItem* new_li)
{
  assert(at_li);
  if(at_li->next)
  {
    at_li->next->prev = new_li;
  }

  if(at_li == list->last)
  {
    list->last = new_li;
  }

  new_li->prev = at_li;
  new_li->next = at_li->next;
  at_li->next = new_li;

  list->count++;
}

void list_insert_after(List* list, ListItem* at_li, void* elem, eList kind)
{
  assert(at_li);
  ListItem* new_li = push_struct(list->arena, ListItem);
  new_li->elem = elem;
  new_li->kind = kind;
  list_insert_item_after(list, at_li, new_li);
}

ListItem* list_remove_first_item(List* list)
{
  ListItem* result = 0;
  if(list->first)
  {
    result = list->first;
    list->first = list->first->next;
    list->count--;
  }
  return result;
}

void list_clear(List* list)
{
  list->first = list->last = 0;
  list->count = 0;
}
