#define breakpoint() for(int x = 0; x != 0; )
#define sizeof_array(array) (sizeof(array)/sizeof(array[0]))
#define max_int() ~(1 << (sizeof(int)*8 - 1))

#define assert(EXPR) do { if(!(EXPR)) assert_(#EXPR, __FILE__, __LINE__); } while(0)
void assert_(char* message, char* file, int line)
{
  if(DEBUG_enabled)
  {
    Platform::printf("%s:%d: ", file, line);
    if(!message || message[0] == '\0')
    {
      message = "";
    }
    Platform::printf("assert(%s)\n", message);

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

#define struct_check_bounds(ARENA, TYPE, STRUCT) (ARENA)->check_bounds(sizeof(TYPE), STRUCT)
#define arena_check_bounds(ARENA) (ARENA)->check_bounds(0, (ARENA)->free)
void MemoryArena::check_bounds(int elem_size, void* ptr)
{
  assert(base <= (uint8*)ptr);
  assert((free + elem_size) < cap);
}

#define mem_zero_struct(VAR, TYPE) (mem_zero_(VAR, sizeof(TYPE)))
#define mem_zero_array(VAR, TYPE) (mem_zero_(VAR, sizeof_array(VAR) * sizeof(TYPE)))
void mem_zero_range(void* start, void* one_past_end)
{
  assert(one_past_end >= start);
  int len = (int)((uint8*)one_past_end - (uint8*)start);
  mem_zero_(start, len);
}

MemoryArena* MemoryArena::create(int size)
{
  void* raw_mem = Platform::alloc_memory(size + sizeof(MemoryArena));
  MemoryArena* arena = (MemoryArena*)raw_mem;
  mem_zero_struct(arena, MemoryArena);
  arena->base = (uint8*)arena + sizeof(MemoryArena);
  arena->free = arena->base;
  arena->cap = arena->free + size;

  return arena;
}

void MemoryArena::dealloc()
{
  base = (uint8*)this + sizeof(MemoryArena);
  free = base;
  assert(free < cap);

  if(DEBUG_zero_arena)
  {
    mem_zero_range(free, cap);
  }
}

void MemoryArena::pop(MemoryArena** arena)
{
  *arena = (*arena)->prev_arena;

  MemoryArena* curr_arena = *arena;
  if(DEBUG_zero_arena)
  {
    mem_zero_range(curr_arena->free, curr_arena->cap);
  }
}

MemoryArena* MemoryArena::push(MemoryArena** arena, int size)
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

void MemoryArena::begin_temp_memory(MemoryArena** arena)
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

void MemoryArena::end_temp_memory(MemoryArena** arena)
{
  MemoryArena::pop(arena);
}

#define mem_push_struct(ARENA, TYPE) ((TYPE*)(ARENA)->push_struct(sizeof(TYPE), 1))
#define mem_push_array(ARENA, TYPE, COUNT) ((TYPE*)(ARENA)->push_struct(sizeof(TYPE), COUNT))

void* MemoryArena::push_struct(int elem_size, int count)
{
  assert(count > 0);

  void* element = free;
  free = free + elem_size*count;

  if(DEBUG_check_arena_bounds)
  {
    arena_check_bounds(this);
  }
  if(DEBUG_zero_struct)
  {
    mem_zero_range(element, free);
  }
  return element;
}

MemoryArena::Usage MemoryArena::usage()
{
  MemoryArena::Usage usage = {0};
#if 0
  uint8* base = (uint8*)this + sizeof(MemoryArena);
#endif
  assert(cap > base);
  usage.total_avail = (int)(cap - base);
  usage.in_use = (free - base) / (double)usage.total_avail;
  return usage;
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

bool Cstr::is_letter(char c)
{
  return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z');
}

bool Cstr::is_dec_digit(char c)
{
  return '0' <= c && c <= '9';
}

bool Cstr::is_hex_digit(char c)
{
  return ('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f');
}

bool Cstr::to_int(char* str, int* integer)
{
  bool negative = false;

  if(*str == '-')
  {
    negative = true;
    str++;
  }

  char c = *str++;
  if(Cstr::is_dec_digit(c))
  {
    int result = (int)(c - '0');

    for(c = *str++; c != '\0'; c = *str++)
    {
      if(Cstr::is_dec_digit(c))
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

bool Cstr::contains_char(char* str, char c)
{
  bool result = false;
  while(*str && !result)
  {
    result = (*str == c);
    str++;
  }
  return result;
}

bool Cstr::start_with(char* str, char* prefix)
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

bool Cstr::match(char* str_a, char* str_b)
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

int Cstr::len(char* str)
{
  int len = 0;
  while(*str++ != 0)
    len++;
  return len;
}

char* Cstr::copy(char* dest_str, char* src_str)
{
  do
    *dest_str++ = *src_str++;
  while(*src_str);
  return dest_str;
}

void Cstr::append(char* dest_str, char* src_str)
{
  while(*dest_str)
    dest_str++;

  do
    *dest_str++ = *src_str++;
  while(*src_str);
  *dest_str = '\0';
}

void Cstr::copy_substr(char* dest_str, char* begin_char, char* end_char)
{
  char* src_str = begin_char;

  do
    *dest_str++ = *src_str++;
  while(src_str <= end_char);
}

/* TODO: Check arena boundaries in the str_* functions */

void String::init(MemoryArena* arena)
{
  // Two Strings cannot be both attached to the same Arena at the same time
  assert(!arena->str);

  this->arena = arena;
  head = mem_push_struct(arena, char);
  end = head;
  arena->str = this;
}

String* String::create(MemoryArena* arena)
{
  String* str = mem_push_struct(arena, String);
  str->init(arena);

  return str;
}

int String::len()
{
  assert(head <= end);
  int len = (int)(end - head);
  return len;
}

void String::append(char* cstr)
{
  assert(head && end && arena);
  assert(head <= end);
  assert(end == (char*)arena->free-1);

  int len = Cstr::len(cstr);
  if(len > 0)
  {
    mem_push_array(arena, char, len); // will implicitly check arena bounds
    Cstr::copy(end, cstr);
    end = (char*)arena->free-1;
  }
}

int String::printf_va(char* fmessage, va_list args)
{
  assert(head && end && arena);
  assert(head <= end);
  assert(end == (char*)arena->free-1);

  int len = Platform::sprintf_va(end, fmessage, args);
  end += len;
  assert(end < (char*)arena->cap);
  arena->free = (uint8*)end+1;

  return len;
}

int String::printf(char* ftext, ...)
{
  va_list args;
  va_start(args, ftext);

  int text_len = printf_va(ftext, args);

  va_end(args);
  return text_len;
}

int String::printfln(char* fline, ...)
{
  va_list args;
  va_start(args, fline);

  int text_len = printf_va(fline, args);

  va_end(args);

  append("\n");
  text_len++;

  return text_len;
}

void String::println()
{
  append("\n");
}

void String::tidyup()
{
  assert(head <= end);
  if(end > head)
  {
    char* p_end = end - 1;
    while(p_end >= head && *p_end)
    {
      p_end--;
    }
    end = p_end;
    arena->free = (uint8*)end+1;
  }
}

void String::free()
{
  assert(head <= end);
  assert(head <= end);
  assert(end == (char*)arena->free-1);

  arena->free = (uint8*)head;
}

char* String::cap()
{
  *(end++) = 0;
  assert(end < (char*)arena->cap);
  arena->str = 0;
  return head;
}

void Cstr::print_char(char buf[3], char c)
{
  if(c == '\0')
    Cstr::copy(buf, "\\0");
  else if(c == '\t')
    Cstr::copy(buf, "\\t");
  else if(c == '\n')
    Cstr::copy(buf, "\\n");
  else if(c == '\r')
    Cstr::copy(buf, "\\r");
  else if(c == '\'')
    Cstr::copy(buf, "\\'");
  else
    *buf = c;
}

char* Platform::path_find_leaf(char* file_path)
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

char* Platform::path_make_leaf(char* file_path, bool with_extension)
{
  char* leaf = Platform::path_find_leaf(file_path);

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

char* Platform::path_make_dir(char* file_path)
{
  char* leaf = Platform::path_find_leaf(file_path);
  if(leaf)
    *leaf = '\0';
  return file_path;
}

#define fail(MESSAGE, ...) fail_(__FILE__, __LINE__, (MESSAGE), ## __VA_ARGS__)
void fail_(char* file, int line, char* message, ...)
{
  Platform::printf("%s:%d: fail : ", file, line);

  if(!message || message[0] == '\0')
  {
    message = "";
  }

  va_list args;
  va_start(args, message);

  Platform::printf_va(message, args);

  va_end(args);

  Platform::printf("\n");
  *(int*)0 = 0;
}

#define error(MESSAGE, ...) error_(__FILE__, __LINE__, (MESSAGE), ## __VA_ARGS__)
bool error_(char* file, int line, char* message, ...)
{
  Platform::printf("%s:%d: error : ", file, line);

  if(!message || message[0] == '\0')
  {
    message = "";
  }

  va_list args;
  va_start(args, message);

  Platform::printf_va(message, args);

  va_end(args);

  Platform::printf("\n");

  return false;
}

bool compile_error_va(MemoryArena* arena, char* file, int line, SourceLoc* src_loc, char* message, va_list args)
{
  char* filename_buf = mem_push_array(arena, char, Cstr::len(file));
  Cstr::copy(filename_buf, file);

  if(src_loc && src_loc->line_nr >= 0)
  {
    Platform::printf("%s:%d: (%s:%d) error : ", src_loc->file_path, src_loc->line_nr,
                     Platform::path_make_leaf(filename_buf, false), line);
  }
  else
  {
    Platform::printf("%s:%d: error : ", file, line);
  }

  Platform::printf_va(message, args);

  Platform::printf("\n");

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

bool String::dump_to_file(char* file_path)
{
  int char_count = len();
  int bytes_written = Platform::file_write_bytes(file_path, (uint8*)head, char_count);
  return (char_count == bytes_written);
}

void List::init(MemoryArena* arena, eList kind)
{
  this->kind = kind;
  this->arena = arena;
  first = last = 0;
  count = 0;
}

List* List::create(MemoryArena* arena, eList kind)
{
  List* list = mem_push_struct(arena, List);
  list->init(arena, kind);
  return list;
}

void List::remove_item(ListItem* item)
{
  if(item->prev)
  {
    item->prev->next = item->next;
    if(item->next)
      item->next->prev = item->prev;
  }

  if(item == first && item == last)
    first = last = 0;
  else if(item == first)
  {
    first = item->next;
    if(first)
      first->prev = 0;
  }
  else if(item == last)
  {
    last = item->prev;
    if(last)
      last->next = 0;
  }
  item->next = item->prev = 0;

  count--;
}

void List::append_item(ListItem* item)
{
  assert(kind == item->kind);

  if(last)
  {
    item->prev = last;
    item->next = 0;
    last->next = item;
    last = item;
  }
  else
  {
    first = last = item;
    item->next = item->prev = 0;
  }

  count++;
}

void List::append(void* elem, eList kind)
{
  assert(elem);
  ListItem* item = mem_push_struct(arena, ListItem);
  item->elem = elem;
  item->kind = kind;
  append_item(item);
}

void List::prepend_item(ListItem* item)
{
  assert(kind == item->kind);

  if(first)
  {
    item->next = first;
    item->prev = 0;
    first->prev = item;
    first = item;
  }
  else
  {
    first = last = item;
    item->next = item->prev = 0;
  }

  count++;
}

void List::prepend(void* elem, eList kind)
{
  ListItem* item = mem_push_struct(arena, ListItem);
  item->elem = elem;
  item->kind = kind;
  prepend_item(item);
}

void List::replace_item_at(List* list_b, ListItem* at_b_item)
{
  ListItem* prev_b_item = at_b_item->prev;
  ListItem* next_b_item = at_b_item->next;

  if(first)
  {
    if(prev_b_item)
      prev_b_item->next = first;
    else
      list_b->first = first;
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
    next_b_item->prev = last;
    if(last)
      last->next = next_b_item;
  }
  else
    list_b->last = last;

  list_b->count += count - 1;
}

void List::join(List* list_b)
{
  ListItem* last_a_item = last;
  ListItem* first_b_item = list_b->first;

  if(last_a_item)
    last_a_item->next = first_b_item;

  if(first_b_item)
    first_b_item->prev = last_a_item;

  if(!first)
    first = list_b->first;

  last = list_b->last;

  count += list_b->count;
}

void List::insert_item_before(ListItem* at_li, ListItem* new_li)
{
  assert(at_li);
  if(at_li->prev)
  {
    at_li->prev->next = new_li;
  }

  if(at_li == first)
  {
    first = new_li;
  }

  new_li->next = at_li;
  new_li->prev = at_li->prev;
  at_li->prev = new_li;

  count++;
}

void List::insert_before(ListItem* at_li, void* elem, eList kind)
{
  assert(at_li);
  ListItem* new_li = mem_push_struct(arena, ListItem);
  new_li->elem = elem;
  new_li->kind = kind;
  insert_item_before(at_li, new_li);
}

/* FIXME: Not tested!! */
void List::insert_item_after(ListItem* at_li, ListItem* new_li)
{
  assert(at_li);
  if(at_li->next)
  {
    at_li->next->prev = new_li;
  }

  if(at_li == last)
  {
    last = new_li;
  }

  new_li->prev = at_li;
  new_li->next = at_li->next;
  at_li->next = new_li;

  count++;
}

void List::insert_after(ListItem* at_li, void* elem, eList kind)
{
  assert(at_li);
  ListItem* new_li = mem_push_struct(arena, ListItem);
  new_li->elem = elem;
  new_li->kind = kind;
  insert_item_after(at_li, new_li);
}

ListItem* List::remove_first_item()
{
  ListItem* result = 0;
  if(first)
  {
    result = first;
    first = first->next;
    count--;
  }
  return result;
}

void List::clear()
{
  first = last = 0;
  count = 0;
}
