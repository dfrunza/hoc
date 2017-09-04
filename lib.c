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

boole
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

boole
char_is_letter(char ch)
{
  return ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z');
}

boole
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
  assert(arena->base <= (uint8*)ptr);
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
free_arena(MemoryArena* arena)
{
  arena->base = (uint8*)arena + sizeof(MemoryArena);
  arena->free = arena->base;
  assert(arena->free < arena->cap);

  if(DEBUG_zero_arena)
  {
    mem_zero_range(arena->free, arena->cap);
  }
}

void
pop_arena(MemoryArena** arena)
{
  *arena = (*arena)->prev_arena;

  MemoryArena* curr_arena = *arena;
  if(DEBUG_zero_arena)
  {
    mem_zero_range(curr_arena->free, curr_arena->cap);
  }
}

MemoryArena*
push_arena(MemoryArena** arena, size_t size)
{
  assert(size > 0);

  MemoryArena* prev_arena = *arena;

  MemoryArena* sub_arena = (MemoryArena*)prev_arena->free;
  sub_arena->base =(uint8*)sub_arena + sizeof(MemoryArena);
  sub_arena->free = sub_arena->base;
  sub_arena->cap = sub_arena->base + size;
  assert(sub_arena->cap <= prev_arena->cap);
  if(DEBUG_zero_arena)
  {
    mem_zero_range(sub_arena->base, sub_arena->cap);
  }

  MemoryArena* new_arena = (MemoryArena*)sub_arena->cap;
  new_arena->base = (uint8*)new_arena + sizeof(MemoryArena);
  new_arena->free = new_arena->base;
  new_arena->cap = prev_arena->cap;
  assert(new_arena->free < new_arena->cap);

  new_arena->prev_arena = prev_arena;
  *arena = new_arena;

  return sub_arena;
}

void
begin_temp_memory(MemoryArena** arena)
{
  MemoryArena* prev_arena = *arena;

  MemoryArena* new_arena = (MemoryArena*)prev_arena->free;
  new_arena->free = (uint8*)new_arena + sizeof(MemoryArena);
  new_arena->cap = prev_arena->cap;
  assert(new_arena->free < new_arena->cap);

  new_arena->prev_arena = prev_arena;
  *arena = new_arena;
}

void
end_temp_memory(MemoryArena** arena)
{
  pop_arena(arena);
}

void*
mem_push_struct_f(MemoryArena* arena, size_t elem_size, size_t count, boole zero_mem)
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
new_arena(int size)
{
  void* raw_mem = VirtualAlloc(0, size + sizeof(MemoryArena), MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
  MemoryArena* arena = (MemoryArena*)raw_mem;
  arena->base = (uint8*)arena + sizeof(MemoryArena);
  arena->free = arena->base;
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
  uint8* base = arena->base;
#endif
  usage.total_avail = arena->cap - base;
  usage.in_use = (arena->free - base) / (double)usage.total_avail;
  return usage;
}

boole
cstr_to_int(char* str, int* integer)
{
  boole negative = false;

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

boole
cstr_to_float(char* str, float* result)
{
#if 0
  boole negative = false;

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

boole
cstr_start_with(char* str, char* prefix)
{
  while(*str == *prefix)
  {
    str++;
    prefix++;
    if(*prefix == '\0')
      break;
  }
  boole result = (*prefix == '\0');
  return result;
}

boole
cstr_match(char* str_a, char* str_b)
{
  while(*str_a == *str_b)
  {
    str_a++;
    str_b++;
    if(*str_a == '\0')
      break;
  }
  boole result = (*str_a == *str_b);
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
cstr_append(char* dest_str, char* src_str)
{
  while(*dest_str)
    dest_str++;

  do
    *dest_str++ = *src_str++;
  while(*src_str);
  *dest_str = '\0';
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
    if(byte_count > 0)
    {
      fseek(file, 0, SEEK_SET);
      *bytes = mem_push_count_nz(arena, uint8, byte_count);
      fread(*bytes, byte_count, 1, file);
    }
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

boole
str_dump_to_file(String* str, char* file_path)
{
  int char_count = str_len(str);
  int bytes_written = file_write_bytes(file_path, (uint8*)str->head, str_len(str));
  return (char_count == bytes_written);
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
init_list(List* list)
{
  mem_zero_struct(list, List);
}

List*
new_list(MemoryArena* arena, ListKind kind)
{
  List* list = mem_push_struct(arena, List);
  list->kind = kind;
  init_list(list);
  return list;
}

void
remove_list_item(List* list, ListItem* item)
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
    list->first = item->next;
  else if(item == list->last)
    list->last = item->prev;

  /* NOTE(to myself): Don't nullify the item->next and item->prev pointers;
   * they may be needed in an iteration loop */
}

void
append_list_item(List* list, ListItem* item)
{
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

void
append_list_elem(MemoryArena* arena, List* list, void* elem)
{
  ListItem* item = mem_push_struct(arena, ListItem);
  item->elem = elem;
  append_list_item(list, item);
}

void
replace_list_item_at(List* list_a, List* list_b, ListItem* at_b_item)
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

boole
compile_error_f(char* file, int line, SourceLocation* src_loc, char* message, ...)
{
  char* filename_buf = mem_push_count_nz(arena, char, cstr_len(file));
  cstr_copy(filename_buf, file);

  if(src_loc->line_nr >= 0)
    fprintf(stderr, "%s(%d) : (%s:%d) ", src_loc->file_path, src_loc->line_nr,
            path_make_stem(filename_buf), line);
  else
    fprintf(stderr, "%s(%d) : ", file, line);

  va_list args;
  va_start(args, message);
  vfprintf(stderr, message, args);
  va_end(args);

  fprintf(stderr, "\n");
  return false;
}
