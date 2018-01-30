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

    //fflush(stderr);
    *(int*)0 = 0;
  }
}

bool char_is_letter(char ch)
{
  return ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z');
}

bool char_is_dec_digit(char c)
{
  return '0' <= c && c <= '9';
}

bool char_is_hex_digit(char c)
{
  return ('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f');
}

void mem_zero_(void* mem, int len)
{
  /* slow but CRT-independent */
  uint8* p_byte = (uint8*)mem;
  for(int i = 0; i < len; i++)
  {
    *p_byte = 0;
  }
}

#define struct_check_bounds(ARENA, TYPE, STRUCT) mem_check_bounds_(ARENA, sizeof(TYPE), STRUCT)
#define arena_check_bounds(ARENA) mem_check_bounds_((ARENA), 0, (ARENA)->free)
void mem_check_bounds_(MemoryArena* arena, int elem_size, void* ptr)
{
  assert(arena->base <= (uint8*)ptr);
  assert((arena->free + elem_size) < arena->cap);
}

#define mem_zero_struct(VAR, TYPE) (mem_zero_(VAR, sizeof(TYPE)))
#define mem_zero_array(VAR, TYPE) (mem_zero_(VAR, sizeof_array(VAR) * sizeof(TYPE)))
void mem_zero_range(void* start, void* one_past_end)
{
  assert(one_past_end >= start);
  int len = (int)((uint8*)one_past_end - (uint8*)start);
  mem_zero_(start, len);
}

MemoryArena* new_arena(int size)
{
  void* raw_mem = platform_alloc_memory(size);
  MemoryArena* arena = (MemoryArena*)raw_mem;
  mem_zero_struct(arena, MemoryArena);
  arena->base = (uint8*)arena + sizeof(MemoryArena);
  arena->free = arena->base;
  arena->cap = arena->free + size;

  return arena;
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

#define mem_push_struct(ARENA, TYPE) ((TYPE*)mem_push_struct_(ARENA, sizeof(TYPE), 1))
#define mem_push_array(ARENA, TYPE, COUNT) ((TYPE*)mem_push_struct_(ARENA, sizeof(TYPE), COUNT))
#define mem_push_array_nz(ARENA, TYPE, COUNT) ((TYPE*)mem_push_struct_(ARENA, sizeof(TYPE), COUNT))

void* mem_push_struct_(MemoryArena* arena, int elem_size, int count)
{
  assert(count > 0);

  void* element = arena->free;
  arena->free = arena->free + elem_size*count;

  if(DEBUG_check_arena_bounds)
  {
    arena_check_bounds(arena);
  }
  if(DEBUG_zero_struct)
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
  if(char_is_dec_digit(c))
  {
    int result = (int)(c - '0');

    for(c = *str++; c != '\0'; c = *str++)
    {
      if(char_is_dec_digit(c))
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
  if(char_is_dec_digit(c))
  {
    int int_part = (c - '0');
    for(c = *str++; c != '\0' && c != '.'; c = *str++)
    {
      if(char_is_dec_digit(c))
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
        if(char_is_dec_digit(c))
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
  if(platform_sscanf(str, "%f", result) != 1)
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

void str_init(MemoryArena* arena, String* str)
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
String* str_new(MemoryArena* arena)
{
  String* str = mem_push_struct(arena, String);
  str_init(str, arena);
  return str;
}
#endif

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

int str_printf_va(String* str, char* fmessage, va_list args)
{
  assert(str->head && str->end && str->arena);
  MemoryArena* arena = str->arena;
  assert(str->head <= str->end);
  assert(str->end == (char*)arena->free-1);

  int len = platform_sprintf_va(str->end, fmessage, args);
  str->end += len;
  assert(str->end < (char*)arena->cap);
  arena->free = (uint8*)str->end+1;

  return len;
}

int str_printf(String* str, char* ftext, ...)
{
  va_list args;
  va_start(args, ftext);
  int text_len = str_printf_va(str, ftext, args);
  va_end(args);

  return text_len;
}

int str_printfln(String* str, char* fline, ...)
{
  va_list args;
  va_start(args, fline);
  int text_len = str_printf_va(str, fline, args);
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

char* path_find_leaf(char* file_path)
{
  char* p_char = file_path;
  char* leaf = p_char;

  /* get the file name part */
  while(p_char && *p_char)
  {
    while(*p_char && *p_char != '\\')
    {
      p_char++;
    }

    if(*p_char == '\\')
    {
      leaf = ++p_char;
    }
  }

  return leaf;
}

char* path_make_leaf(char* file_path, bool with_extension)
{
  char* leaf = path_find_leaf(file_path);

  /* remove the filename extension */
  if(leaf && !with_extension)
  {
    char* p_char = leaf;
    while(*p_char && *p_char != '.')
      p_char++;
    *p_char = '\0';
  }

  return leaf;
}

char* path_make_dir(char* file_path)
{
  char* leaf = path_find_leaf(file_path);
  if(leaf)
    *leaf = '\0';
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
  //vfprintf(stderr, message, args);
  platform_printf(message, args);
  va_end(args);

  platform_printf("\n");
  //fprintf(stderr, "\n");
  //fflush(stderr);
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
  platform_printf(message, args);
  va_end(args);

  platform_printf("\n");
  //fflush(stderr);

  return false;
}

#define compile_error(ARENA, SRC, MESSAGE, ...) compile_error_((ARENA), __FILE__, __LINE__, (SRC), (MESSAGE), ## __VA_ARGS__)
bool compile_error_(MemoryArena* arena, char* file, int line, SourceLoc* src_loc, char* message, ...)
{
  char* filename_buf = mem_push_array_nz(arena, char, cstr_len(file));
  cstr_copy(filename_buf, file);

  if(src_loc && src_loc->line_nr >= 0)
  {
    platform_printf("%s:%d: (%s:%d) error : ", src_loc->file_path, src_loc->line_nr,
            path_make_leaf(filename_buf, false), line);
  }
  else
  {
    platform_printf("%s:%d: error : ", file, line);
  }

  va_list args;
  va_start(args, message);
  //vfprintf(stderr, message, args);
  platform_printf(message, args);
  va_end(args);

  platform_printf("\n");

  return false;
}

char* file_read_text(MemoryArena* arena, char* file_path)
{
  char* text = 0;
  int byte_count = 0;
  if((byte_count = platform_file_read_bytes(arena, (uint8**)&text, file_path, 1)) >= 0)
  {
    text[byte_count] = '\0'; // NULL terminator
  }

  return text;
}

bool str_dump_to_file(String* str, char* file_path)
{
  int char_count = str_len(str);
  int bytes_written = platform_file_write_bytes(file_path, (uint8*)str->head, str_len(str));
  return (char_count == bytes_written);
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

void init_list(MemoryArena* arena, List* list, eList kind)
{
  list->kind = kind;
  list->arena = arena;
  list->first = list->last = 0;
  list->count = 0;
}

List* new_list(MemoryArena* arena, eList kind)
{
  List* list = mem_push_struct(arena, List);
  init_list(arena, list, kind);
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

  list->count--;
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

  list->count++;
}

void append_list_elem(List* list, void* elem, eList kind)
{
  assert(elem);
  ListItem* item = mem_push_struct(list->arena, ListItem);
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

  list->count++;
}

void prepend_list_elem(List* list, void* elem, eList kind)
{
  ListItem* item = mem_push_struct(list->arena, ListItem);
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

  list_b->count += list_a->count - 1;
}

void join_list_pair(List* list_a, List* list_b)
{
  ListItem* last_a_item = list_a->last;
  ListItem* first_b_item = list_b->first;

  if(last_a_item)
    last_a_item->next = list_b->first;

  if(first_b_item)
    first_b_item->prev = last_a_item;

  list_a->count += list_b->count;
}

void insert_item_before(List* list, ListItem* at_li, ListItem* new_li)
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

void insert_elem_before(List* list, ListItem* at_li, void* elem, eList kind)
{
  assert(at_li);
  ListItem* new_li = mem_push_struct(list->arena, ListItem);
  new_li->elem = elem;
  new_li->kind = kind;
  insert_item_before(list, at_li, new_li);
}

/* FIXME: Not tested!! */
void insert_item_after(List* list, ListItem* at_li, ListItem* new_li)
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

void insert_elem_after(List* list, ListItem* at_li, void* elem, eList kind)
{
  assert(at_li);
  ListItem* new_li = mem_push_struct(list->arena, ListItem);
  new_li->elem = elem;
  new_li->kind = kind;
  insert_item_after(list, at_li, new_li);
}

ListItem* remove_first_item(List* list)
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

void clear_list(List* list)
{
  list->first = list->last = 0;
  list->count = 0;
}
