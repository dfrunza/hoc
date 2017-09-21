#include "hocc.h"

bool DEBUG_enabled = true;
bool DEBUG_zero_arena = true;
bool DEBUG_check_arena_bounds = true;

MemoryArena* arena = 0;
MemoryArena* symbol_table_arena = 0;

#include "lib.cpp"

//AstNode2* operator_table = 0;
SymbolTable* symbol_table = 0;
int last_scope_id = 0;
int tempvar_id = 0;

Type* basic_type_bool;
Type* basic_type_int;
Type* basic_type_char;
Type* basic_type_float;
Type* basic_type_void;
List* subst_list;
int typevar_id = 1;

#define ITEM(VAR, NAME)\
  (((VAR)->kind == List##_##NAME) ? (VAR)->NAME : 0)

List*
new_list(MemoryArena* arena, ListKind kind)
{
  List* list = mem_push_struct(arena, List);
  list->kind = kind;
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

void
append_list_elem(MemoryArena* arena, List* list, void* elem, ListKind kind)
{
  ListItem* item = mem_push_struct(arena, ListItem);
  item->elem = elem;
  item->kind = kind;
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

bool
compile_error_f(char* file, int line, SourceLoc* src_loc, char* message, ...)
{
  char* filename_buf = mem_push_array_nz(arena, char, cstr_len(file));
  cstr_copy(filename_buf, file);

  if(src_loc && src_loc->line_nr >= 0)
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

void
init_ast_meta_info(AstMetaInfo* ast, int gen_id)
{
  if(gen_id == 0)
  {
    ast->kind_count = 21;
    ast->kinds = mem_push_array(arena, AstKindMetaInfo, ast->kind_count);

    int kind_index = 0;
    AstKindMetaInfo* kind = 0;

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_array;
      kind->attrib_count = 2;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_type_expr;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_size_expr;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_call;
      kind->attrib_count = 2;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_id;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_list;
      attrib->name = AstAttributeName_args;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_hoc_new;
      kind->attrib_count = 2;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_type_expr;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_count_expr;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_hoc_putc;
      kind->attrib_count = 1;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_expr;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_return_stmt;
      kind->attrib_count = 1;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_expr;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_stmt;
      kind->attrib_count = 1;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_stmt;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_if_stmt;
      kind->attrib_count = 3;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_cond_expr;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_body;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_else_body;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_while_stmt;
      kind->attrib_count = 2;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_cond_expr;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_body;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_pointer;
      kind->attrib_count = 2;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_type_expr;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_expr;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_cast;
      kind->attrib_count = 2;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_type_expr;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_expr;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_proc;
      kind->attrib_count = 4;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_ret_type_expr;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_id;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_list;
      attrib->name = AstAttributeName_args;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_body;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_lit;
      kind->attrib_count = 6;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_lit_kind;
      attrib->name = AstAttributeName_lit_kind;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_int_val;
      attrib->name = AstAttributeName_int_val;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_float_val;
      attrib->name = AstAttributeName_float_val;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_bool_val;
      attrib->name = AstAttributeName_bool_val;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_char_val;
      attrib->name = AstAttributeName_char_val;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_str;
      attrib->name = AstAttributeName_str;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_continue_stmt;
      kind->attrib_count = 0;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_break_stmt;
      kind->attrib_count = 0;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_var;
      kind->attrib_count = 3;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_type_expr;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_id;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_init_expr;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_module;
      kind->attrib_count = 2;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_str;
      attrib->name = AstAttributeName_file_path;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_body;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_include;
      kind->attrib_count = 2;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_str;
      attrib->name = AstAttributeName_file_path;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_body;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_block;
      kind->attrib_count = 1;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_list;
      attrib->name = AstAttributeName_nodes;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_id;
      kind->attrib_count = 1;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);
      
      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_str;
      attrib->name = AstAttributeName_name;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_bin_expr;
      kind->attrib_count = 3;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_left_operand;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_right_operand;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_op_kind;
      attrib->name = AstAttributeName_op_kind;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_un_expr;
      kind->attrib_count = 2;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_operand;
      
      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_op_kind;
      attrib->name = AstAttributeName_op_kind;
    }
  }
  else if(gen_id == 1)
  {
    ast->kind_count = 4;
    ast->kinds = mem_push_array(arena, AstKindMetaInfo, ast->kind_count);

    int kind_index = 0;
    AstKindMetaInfo* kind = 0;

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_module;
      kind->attrib_count = 3;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_str;
      attrib->name = AstAttributeName_file_path;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_body;
      
      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_list;
      attrib->name = AstAttributeName_procs;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_block;
      kind->attrib_count = 2;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_list;
      attrib->name = AstAttributeName_stmts;
      
      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_scope;
      attrib->name = AstAttributeName_scope;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_var_decl;
      kind->attrib_count = 2;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_str;
      attrib->name = AstAttributeName_name;
      
      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_scope;
      attrib->name = AstAttributeName_decl_scope;
    }

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_var_occur;
      kind->attrib_count = 4;
      kind->attribs = mem_push_array(arena, AstAttributeMetaInfo, kind->attrib_count);

      int attrib_index = 0;
      AstAttributeMetaInfo* attrib = 0;

      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_str;
      attrib->name = AstAttributeName_name;
      
      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_ast_node;
      attrib->name = AstAttributeName_var_decl;
      
      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_scope;
      attrib->name = AstAttributeName_occur_scope;
      
      assert(attrib_index < kind->attrib_count);
      attrib = &kind->attribs[attrib_index++];
      attrib->kind = AstAttribute_int_val;
      attrib->name = AstAttributeName_decl_scope_offset;
    }
  }
  else
    assert(0);
}

void
init_ast_meta_infos()
{
  AstMetaInfo* ast = 0;
  int gen_id = 0;
  int generation_count = sizeof_array(ast_meta_infos);

  while(gen_id < generation_count)
  {
    ast = &ast_meta_infos[gen_id];
    init_ast_meta_info(ast, gen_id);
    gen_id++;
  }
}

AstAttribute*
get_ast_attribute(AstNode* node, AstAttributeName name)
{
  AstAttribute* result = 0;

  assert(node->gen_id >= 0 && node->gen_id < sizeof_array(ast_meta_infos));
  AstMetaInfo* ast_meta = &ast_meta_infos[node->gen_id];

  AstKindMetaInfo* kind_meta = 0;
  for(int kind_index = 0;
      kind_index < ast_meta->kind_count;
      kind_index++)
  {
    kind_meta = &ast_meta->kinds[kind_index];
    if(kind_meta->kind == node->kind)
    {
      break;
    }
    kind_meta = 0;
  }

  if(kind_meta)
  {
    AstAttributeMetaInfo* attrib_meta = 0;
    int attrib_index = 0;
    for(;
        attrib_index < kind_meta->attrib_count;
        attrib_index++)
    {
      attrib_meta = &kind_meta->attribs[attrib_index];
      if(attrib_meta->name == name)
      {
        break;
      }
      attrib_meta = 0;
    }

    if(attrib_meta)
    {
      result = &node->attribs[attrib_index];
      assert(result->kind == attrib_meta->kind);
      assert(result->name == attrib_meta->name);
    }
    else
      assert(0);
  }
  else
    assert(0);

  return result;
}

AstAttribute*
get_ast_attribute_safe(AstNode* node, AstAttributeKind kind, AstAttributeName name)
{
  AstAttribute* result = 0;
  if((result = get_ast_attribute(node, name))->kind != kind)
  {
    result = 0;
  }
  return result;
}

AstNode*
make_ast_node(int gen_id, AstNode* node, AstKind kind, SourceLoc* src_loc)
{
  assert(gen_id >= 0 && gen_id < sizeof_array(ast_meta_infos));
  node->gen_id = gen_id;
  node->kind = kind;
  node->src_loc = src_loc;

  AstMetaInfo* ast_meta = &ast_meta_infos[node->gen_id];

  AstKindMetaInfo* kind_meta = 0;
  for(int kind_index = 0;
      kind_index < ast_meta->kind_count;
      kind_index++)
  {
    kind_meta = &ast_meta->kinds[kind_index];
    if(kind_meta->kind == node->kind)
    {
      break;
    }
    kind_meta = 0;
  }

  if(kind_meta)
  {
    int attrib_index = 0;
    for(; 
       attrib_index < kind_meta->attrib_count;
       attrib_index++)
    {
      AstAttributeMetaInfo* attrib_meta = &kind_meta->attribs[attrib_index];
      AstAttribute* attrib = &node->attribs[attrib_index];
      mem_zero_struct(attrib, AstAttribute);
      attrib->kind = attrib_meta->kind;
      attrib->name = attrib_meta->name;
    }
  }
  else
    assert(0);

  return node;
}

AstNode*
new_ast_node(int gen_id, AstKind kind, SourceLoc* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode);
  make_ast_node(gen_id, node, kind, src_loc);
  return node;
}

#include "lex.cpp"
#include "syntax.cpp"
#include "typecheck.cpp"
#include "semantic.cpp"
/*
#include "runtime.cpp"
#include "codegen.cpp"
#include "hasm.cpp"
*/

void
DEBUG_print_arena_usage(char* tag)
{
  ArenaUsage usage = arena_usage(arena);
  ArenaUsage sym_usage = arena_usage(symbol_table_arena);
  printf("-----  %s  -----\n", tag);
  printf("in_use(arena) : %.2f%%\n", usage.in_use*100);
  printf("in_use(symbol_table_arena) : %.2f%%\n", sym_usage.in_use*100);
}

void
DEBUG_print_line(String* str, int indent_level, char* message, ...)
{
  for(int i = 0; i < indent_level; i++)
  {
    str_append(str, "  ");
  }

  va_list varargs;
  va_start(varargs, message);
  str_printf_va(str, message, varargs);
  va_end(varargs);

  str_append(str, "\n");
}

void
DEBUG_print_scope(String* str, int indent_level, Scope* scope, char* tag)
{
  if(scope)
  {
    ++indent_level;
    if(tag)
    {
      DEBUG_print_line(str, indent_level, tag);
      ++indent_level;
    }

    DEBUG_print_line(str, indent_level, "scope_id: %d", scope->scope_id);
    DEBUG_print_line(str, indent_level, "nesting_depth: %d", scope->nesting_depth);
    DEBUG_print_scope(str, indent_level, scope->encl_scope, "encl_scope");
    DEBUG_print_ast_node_list(str, indent_level, "local_decls", scope->local_decls);
    DEBUG_print_ast_node_list(str, indent_level, "nonlocal_occurs", scope->nonlocal_occurs);
  }
}

void
DEBUG_print_ast_node(String* str, int indent_level, char* tag, AstNode* node)
{
  if(node)
  {
    if(tag)
    {
      DEBUG_print_line(str, indent_level, tag);
      ++indent_level;
    }
#if 1
    if(node->src_loc)
    {
      DEBUG_print_line(str, indent_level, "%s src_line=\"%s:%d\"",
          get_ast_kind_printstr(node->kind), node->src_loc->file_path, node->src_loc->line_nr);
    }
    else
    {
      DEBUG_print_line(str, indent_level, "%s", get_ast_kind_printstr(node->kind));
    }
#else
    if(node->src_loc)
    {
      DEBUG_print_line(str, indent_level, "src_line=\"%s:%d\"", node->src_loc->file_path, node->src_loc->line_nr);
    }
#endif
    ++indent_level;

    if(node->gen_id == 0)
    {
      if(node->kind == AstNode_module
         || node->kind == AstNode_include)
      {
        DEBUG_print_line(str, indent_level, "file_path: \"%s\"", ATTR(node, str, file_path));
        DEBUG_print_ast_node(str, indent_level, "body", ATTR(node, ast_node, body));
      }
      else if(node->kind == AstNode_proc)
      {
        DEBUG_print_ast_node(str, indent_level, "ret_type", ATTR(node, ast_node, ret_type_expr));
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "args", ATTR(node, list, args));
        DEBUG_print_ast_node(str, indent_level, "body", ATTR(node, ast_node, body));
      }
      else if(node->kind == AstNode_var)
      {
        DEBUG_print_ast_node(str, indent_level, "type_expr", ATTR(node, ast_node, type_expr));
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node(str, indent_level, "init_expr", ATTR(node, ast_node, init_expr));
      }
      else if(node->kind == AstNode_id)
      {
        DEBUG_print_line(str, indent_level, "name: %s", ATTR(node, str, name));
      }
      else if(node->kind == AstNode_block)
      {
        DEBUG_print_ast_node_list(str, indent_level, "nodes", ATTR(node, list, nodes));
      }
      else if(node->kind == AstNode_bin_expr)
      {
        DEBUG_print_line(str, indent_level, "op: %s", get_op_kind_printstr(ATTR(node, op_kind, op_kind)));
        DEBUG_print_ast_node(str, indent_level, "left_operand", ATTR(node, ast_node, left_operand));
        DEBUG_print_ast_node(str, indent_level, "right_operand", ATTR(node, ast_node, right_operand));
      }
      else if(node->kind == AstNode_un_expr)
      {
        DEBUG_print_line(str, indent_level, "op: %s", get_op_kind_printstr(ATTR(node, op_kind, op_kind)));
        DEBUG_print_ast_node(str, indent_level, "operand", ATTR(node, ast_node, operand));
      }
      else if(node->kind == AstNode_lit)
      {
        LiteralKind lit_kind = ATTR(node, lit_kind, lit_kind);
        DEBUG_print_line(str, indent_level, get_literal_kind_printstr(lit_kind));
        if(lit_kind == Literal_int_val)
        {
          DEBUG_print_line(str, indent_level, "int_val: %d", ATTR(node, int_val, int_val));
        }
        else if(lit_kind == Literal_float_val)
        {
          DEBUG_print_line(str, indent_level, "float_val: %f", ATTR(node, float_val, float_val));
        }
        else if(lit_kind == Literal_bool_val)
        {
          DEBUG_print_line(str, indent_level, "bool_val: %d", ATTR(node, bool_val, bool_val));
        }
        else if(lit_kind == Literal_char_val)
        {
          char buf[3] = {0};
          print_char(buf, ATTR(node, char_val, char_val));
          DEBUG_print_line(str, indent_level, "char_val: '%s'", buf);
        }
        else if(lit_kind == Literal_str)
        {
          DEBUG_print_line(str, indent_level, "str: \"%s\"", ATTR(node, str, str));
        }
        else
          assert(0);
      }
      else if(node->kind == AstNode_stmt)
      {
        DEBUG_print_ast_node(str, indent_level, "stmt", ATTR(node, ast_node, stmt));
      }
      else if(node->kind == AstNode_return_stmt)
      {
        DEBUG_print_ast_node(str, indent_level, "expr", ATTR(node, ast_node, expr));
      }
      else if(node->kind == AstNode_if_stmt)
      {
        DEBUG_print_ast_node(str, indent_level, "cond_expr", ATTR(node, ast_node, cond_expr));
        DEBUG_print_ast_node(str, indent_level, "body", ATTR(node, ast_node, body));
        DEBUG_print_ast_node(str, indent_level, "else_body", ATTR(node, ast_node, else_body));
      }
      else if(node->kind == AstNode_while_stmt)
      {
        DEBUG_print_ast_node(str, indent_level, "cond_expr", ATTR(node, ast_node, cond_expr));
        DEBUG_print_ast_node(str, indent_level, "body", ATTR(node, ast_node, body));
      }
      else if(node->kind == AstNode_for_stmt)
      {
        DEBUG_print_ast_node(str, indent_level, "decl_expr", ATTR(node, ast_node, decl_expr));
        DEBUG_print_ast_node(str, indent_level, "cond_expr", ATTR(node, ast_node, cond_expr));
        DEBUG_print_ast_node(str, indent_level, "loop_expr", ATTR(node, ast_node, loop_expr));
        DEBUG_print_ast_node(str, indent_level, "body", ATTR(node, ast_node, body));
      }
      else if(node->kind == AstNode_cast)
      {
        DEBUG_print_ast_node(str, indent_level, "type_expr", ATTR(node, ast_node, type_expr));
        DEBUG_print_ast_node(str, indent_level, "expr", ATTR(node, ast_node, expr));
      }
      else if(node->kind == AstNode_array)
      {
        DEBUG_print_ast_node(str, indent_level, "type_expr", ATTR(node, ast_node, type_expr));
        DEBUG_print_ast_node(str, indent_level, "size_expr", ATTR(node, ast_node, size_expr));
      }
      else if(node->kind == AstNode_pointer)
      {
        DEBUG_print_ast_node(str, indent_level, "type_expr", ATTR(node, ast_node, type_expr));
      }
      else if(node->kind == AstNode_call)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "args", ATTR(node, list, args));
      }
      else if(node->kind == AstNode_break_stmt
              || node->kind == AstNode_continue_stmt)
      {
        /* no other info to print */
      }
      else if(node->kind == AstNode_struct_decl)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "members", ATTR(node, list, members));
      }
      else if(node->kind == AstNode_union_decl)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "members", ATTR(node, list, members));
      }
      else if(node->kind == AstNode_enum_decl)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "members", ATTR(node, list, members));
      }
      else if(node->kind == AstNode_init_list)
      {
        DEBUG_print_ast_node_list(str, indent_level, "members", ATTR(node, list, members));
      }
      else if(node->kind == AstNode_goto_stmt)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
      }
      else if(node->kind == AstNode_label)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
      }
      else if(node->kind == AstNode_hoc_new)
      {
        DEBUG_print_ast_node(str, indent_level, "type_expr", ATTR(node, ast_node, type_expr));
        DEBUG_print_ast_node(str, indent_level, "count_expr", ATTR(node, ast_node, count_expr));
      }
      else if(node->kind == AstNode_hoc_putc)
      {
        DEBUG_print_ast_node(str, indent_level, "expr", ATTR(node, ast_node, expr));
      }
      else
        assert(0);
    }
    else if(node->gen_id == 1)
    {
      printf("todo\n");
    }
    else
      assert(0);
  }
}

void
DEBUG_print_ast_node_list(String* str, int indent_level, char* tag, List* node_list)
{
  if(node_list->first)
  {
    if(tag)
    {
      DEBUG_print_line(str, indent_level, tag);
      ++indent_level;
    }

    for(ListItem* list_item = node_list->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* node = ITEM(list_item, ast_node);
      DEBUG_print_ast_node(str, indent_level, 0, node);
    }
  }
}

VmProgram*
translate(char* file_path, char* hoc_text)
{
  VmProgram* vm_program = mem_push_struct(arena, VmProgram);
  vm_program->instr_list = new_list(arena, List_VmInstr);
  vm_program->success = false;

  TokenStream token_stream = {0};
  init_token_stream(&token_stream, hoc_text, file_path);
  get_next_token(&token_stream);

  init_ast_meta_infos();

  AstNode* module = 0;
  if(vm_program->success = parse(&token_stream, &module))
  {
    if(DEBUG_enabled)/*>>>*/
    {
      DEBUG_print_arena_usage("Syntax");

      begin_temp_memory(&arena);
      String* str = str_new(arena);
      DEBUG_print_ast_node(str, 0, "module", module);
      str_dump_to_file(str, "debug_syntax.txt");
      end_temp_memory(&arena);
    }/*<<<*/

#if 1
    {
      // process the includes
      AstNode* module_body = ATTR(module, ast_node, body);
      for(ListItem* list_item = ATTR(module_body, list, nodes)->first;
          list_item;
          list_item = list_item->next)
      {
        AstNode* stmt = ITEM(list_item, ast_node);

        if(stmt->kind == AstNode_include)
        {
          AstNode* include_body = ATTR(stmt, ast_node, body);
          process_includes(ATTR(include_body, list, nodes), ATTR(module_body, list, nodes), list_item);
        }
      }
    }
#endif

    if(vm_program->success = semantic(module))
    {
      assert(symbol_table->scope_id == 0);
      assert(symbol_table->nesting_depth == 0);

      if(DEBUG_enabled)/*>>>*/
      {
        DEBUG_print_arena_usage("Semantic");

        begin_temp_memory(&arena);
        String* str = str_new(arena);
        DEBUG_print_ast_node(str, 0, "module", module);
        str_dump_to_file(str, "debug_semantic.txt");
        end_temp_memory(&arena);
      }/*<<<*/

#if 0
      build_runtime(module);
      if(DEBUG_enabled)/*>>>*/
      {
        DEBUG_print_arena_usage("Runtime");/*<<<*/
      }

      codegen(vm_program->instr_list, &vm_program->data, &vm_program->data_size, module);
      if(DEBUG_enabled)/*>>>*/
      {
        DEBUG_print_arena_usage("Codegen");/*<<<*/
      }

      str_init(&vm_program->text, arena);
      print_code(vm_program);
      if(DEBUG_enabled)/*>>>*/
      {
        DEBUG_print_arena_usage("Print code");/*<<<*/
      }
#endif
    }
  }
  return vm_program;
}

typedef struct
{
  char* name;
  int len;
}
FileName;

typedef struct
{
  char strings[4*80 + 4*10];
  FileName hasm;
  FileName bincode;
  FileName exe;
}
OutFileNames;

bool
make_out_file_names(OutFileNames* out_files, char* src_file_path)
{
  char* stem = mem_push_array_nz(arena, char, cstr_len(src_file_path));
  cstr_copy(stem, src_file_path);
  stem = path_make_stem(stem);

  int stem_len = cstr_len(stem);
  assert(stem_len > 0);
  bool success = true;

  if(success = (stem_len > 0 && stem_len < 81))
  {
    char* str = out_files->strings;

    sprintf(str, "%s.hasm", stem);
    out_files->hasm.name = str;
    out_files->hasm.len = cstr_len(out_files->hasm.name);
    str = out_files->hasm.name + out_files->hasm.len + 1;

    sprintf(str, "%s.bincode", stem);
    out_files->bincode.name = str;
    out_files->bincode.len = cstr_len(out_files->bincode.name);
    str = out_files->bincode.name + out_files->bincode.len + 1;

    sprintf(str, "%s.exe", stem);
    out_files->exe.name = str;
    out_files->exe.len = cstr_len(out_files->exe.name);
  }
  else
    error("length of file name must be between 1..80 : '%s'", stem);
  return success;
}

char*
make_vm_exe_path(char* hocc_exe_path)
{
  char* vm_exe_path = mem_push_array_nz(arena, char, cstr_len(hocc_exe_path) + cstr_len("vm.exe"));
  cstr_copy(vm_exe_path, hocc_exe_path);
  path_make_dir(vm_exe_path);
  cstr_append(vm_exe_path, "vm.exe");
  return vm_exe_path;
}

bool
write_hasm_file(OutFileNames* out_files, VmProgram* vm_program)
{
  int bytes_written = file_write_bytes(out_files->hasm.name, (uint8*)vm_program->text.head, vm_program->text_len);
  bool success = (bytes_written == vm_program->text_len);
  if(!success)
    error("HASM file '%s' incompletely written", out_files->hasm.name);
  return success;
}

int
main(int argc, char* argv[])
{
  bool success = true;

  if(success = (argc >= 2))
  {
    arena = new_arena(ARENA_SIZE);
    symbol_table_arena = push_arena(&arena, SYMBOL_TABLE_ARENA_SIZE);

    if(DEBUG_enabled)/*>>>*/
    {
#if 0
      begin_temp_memory(&arena);
      printf("----- CST struct sizes -----\n");
      DEBUG_print_sizeof_ast_structs(NodeKind_cst);
      printf("----- AST struct sizes -----\n");
      DEBUG_print_sizeof_ast_structs(NodeKind_ast);
      end_temp_memory(&arena);
#endif
    }/*<<<*/

    char* src_file_path = argv[1];

    char* hoc_text = file_read_text(arena, src_file_path);
    if(DEBUG_enabled)/*>>>*/
    {
      DEBUG_print_arena_usage("Read HoC text");/*<<<*/
    }

    if(success = to_bool(hoc_text))
    {
      VmProgram* vm_program = translate(src_file_path, hoc_text);
      if(success = vm_program->success)
      {
#if 0
        if(DEBUG_enabled)/*>>>*/
          printf("symbol count : %d\n", symbol_table->sym_count);/*<<<*/
#endif
        
        OutFileNames out_files = {0};
        if(success = make_out_file_names(&out_files, src_file_path))
        {
#if 0
          BinCode* bin_image = mem_push_struct(arena, BinCode);
          cstr_copy(bin_image->sig, BINCODE_SIGNATURE);

          char* hasm_text = str_cap(&vm_program->text);

          if(DEBUG_enabled)/*>>>*/
            write_hasm_file(&out_files, vm_program);/*<<<*/

          if(success = convert_hasm_to_instructions(hasm_text, vm_program))
          {
            char* hocc_exe_path = argv[0];
            char* vm_exe_path = make_vm_exe_path(hocc_exe_path);

            uint8* vm_bytes = 0;
            int vm_size = 0;
            if(vm_size = file_read_bytes(arena, &vm_bytes, vm_exe_path))
            {
              FILE* exe_file = fopen(out_files.exe.name, "wb");
              if(exe_file)
              {
                bin_image->code_offset = sizeof(BinCode);
                bin_image->code_size = sizeof(Instruction) * vm_program->instr_count;

                bin_image->data_offset = bin_image->code_offset + bin_image->code_size;
                bin_image->data_size = sizeof(uint8) * vm_program->data_size;

                if((int)fwrite(vm_bytes, 1, vm_size, exe_file) == vm_size
                  && (int)fwrite(bin_image, sizeof(BinCode), 1, exe_file) == 1
                  && (int)fwrite(vm_program->instructions, sizeof(Instruction), vm_program->instr_count, exe_file) == vm_program->instr_count
                  && (int)fwrite(vm_program->data, sizeof(uint8), vm_program->data_size, exe_file) == vm_program->data_size)
                {
                  ;/*OK*/
                }
                else
                  success = error("could not write to file `%s`", out_files.exe.name);
                fclose(exe_file);
              }
              else
                success = error("could not write to file `%s`", out_files.exe.name);
            }
            else
              success = error("could not read file `%s`", vm_exe_path);
          }
#endif
        }
      }
      else
        success = error("program could not be translated");
    }
    else
      success = error("could not read source file `%s`", src_file_path);
  }
  else
    success = error("missing argument : input source file");

#if 0
  getc(stdin);
#endif
  return success ? 0 : -1;
}

