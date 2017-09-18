#include "hocc.h"

bool DEBUG_enabled = true;
bool DEBUG_zero_arena = true;
bool DEBUG_check_arena_bounds = true;

MemoryArena* arena = 0;
MemoryArena* symbol_table_arena = 0;

#include "lib.cpp"

AstNode2* operator_table = 0;
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
    ast->kind_count = 20;
    ast->kinds = mem_push_array(arena, AstKindMetaInfo, ast->kind_count);

    int kind_index = 0;
    AstKindMetaInfo* kind = 0;

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_break_stmt;
      kind->attrib_count = 0;
    }

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
    printf("todo\n");
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
new_ast_node(int gen_id, AstKind kind, SourceLoc* src_loc)
{
  assert(gen_id >= 0 && gen_id < sizeof_array(ast_meta_infos));

  AstNode* node = mem_push_struct(arena, AstNode);
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
      attrib->kind = attrib_meta->kind;
      attrib->name = attrib_meta->name;
    }
  }
  else
    assert(0);

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

#if 0
struct NodeStructInfo
{
  NodeKind kind;

  int size;

  union
  {
    AstKind1 ast1_kind;
    AstKind2 ast2_kind;
  };
};

#define DEBUG_make_ast1_struct_info(KIND)\
{\
  struct AstNode1 node = {};\
  node.kind = AstKind1_##KIND;\
  struct_info[AstKind1_##KIND].ast1_kind = AstKind1_##KIND;\
  struct_info[AstKind1_##KIND].size = sizeof(node.##KIND);\
}\

#define DEBUG_make_ast2_struct_info(KIND)\
{\
  struct AstNode2 node = {};\
  node.kind = AstKind2_##KIND;\
  struct_info[AstKind2_##KIND].ast2_kind = AstKind2_##KIND;\
  struct_info[AstKind2_##KIND].size = sizeof(node.##KIND);\
}\

void
DEBUG_print_sizeof_ast_structs(NodeKind node_kind)
{
  NodeStructInfo* struct_info = {};
  int info_count = 0;

  if(node_kind == NodeKind_cst)
  {
    info_count = AstKind1__Count;
    struct_info = mem_push_array(arena, NodeStructInfo, info_count);

    assert(AstKind1__None == 0);
    DEBUG_make_ast1_struct_info(bin_expr);
    DEBUG_make_ast1_struct_info(un_expr);
    DEBUG_make_ast1_struct_info(lit);
    DEBUG_make_ast1_struct_info(var);
    DEBUG_make_ast1_struct_info(block);
    DEBUG_make_ast1_struct_info(proc);
    DEBUG_make_ast1_struct_info(id);
    DEBUG_make_ast1_struct_info(while_stmt);
    DEBUG_make_ast1_struct_info(for_stmt);
    DEBUG_make_ast1_struct_info(if_stmt);
    DEBUG_make_ast1_struct_info(return_stmt);
    DEBUG_make_ast1_struct_info(goto_stmt);
    DEBUG_make_ast1_struct_info(label);
    DEBUG_make_ast1_struct_info(include);
    DEBUG_make_ast1_struct_info(module);
    DEBUG_make_ast1_struct_info(cast);
    DEBUG_make_ast1_struct_info(call);
    DEBUG_make_ast1_struct_info(array);
    DEBUG_make_ast1_struct_info(pointer);
    DEBUG_make_ast1_struct_info(struct_decl);
    DEBUG_make_ast1_struct_info(union_decl);
    DEBUG_make_ast1_struct_info(enum_decl);
    DEBUG_make_ast1_struct_info(init_list);
  }
  else if(node_kind == NodeKind_ast)
  {
    info_count = AstKind2__Count;
    struct_info = mem_push_array(arena, NodeStructInfo, info_count);

    assert(AstKind2__None == 0);
    DEBUG_make_ast2_struct_info(block);
    DEBUG_make_ast2_struct_info(module);
    DEBUG_make_ast2_struct_info(stmt);
    DEBUG_make_ast2_struct_info(var_decl);
    DEBUG_make_ast2_struct_info(var_occur);
    DEBUG_make_ast2_struct_info(type_decl);
    DEBUG_make_ast2_struct_info(type_occur);
    DEBUG_make_ast2_struct_info(proc_decl);
    DEBUG_make_ast2_struct_info(proc_occur);
    DEBUG_make_ast2_struct_info(return_stmt);
    DEBUG_make_ast2_struct_info(if_stmt);
    DEBUG_make_ast2_struct_info(while_stmt);
    DEBUG_make_ast2_struct_info(do_while_stmt);
    DEBUG_make_ast2_struct_info(continue_stmt);
    DEBUG_make_ast2_struct_info(break_stmt);
  }
  else
    assert(0);

#if 1
  // insertion-sort the array
  for(int i = 1; i < info_count; i++)
  {
    for(int j = i;
        struct_info[j].size < struct_info[j-1].size;
        j--)
    {
      NodeStructInfo value_at_j = struct_info[j];
      struct_info[j] = struct_info[j-1];
      struct_info[j-1] = value_at_j;
    }
  }
#endif

  for(int i = info_count - 1; i >= 0; i--)
  {
    NodeStructInfo* info = &struct_info[i];
    if(info->size > 0)
    {
      const char* kind_str = 0;
      if(node_kind == NodeKind_cst)
      {
        kind_str = get_ast1_kind_printstr(info->ast1_kind);
      }
      else if(node_kind == NodeKind_ast)
      {
        kind_str = get_ast2_kind_printstr(info->ast2_kind);
      }
      else
        assert(0);

      printf("%s = %d bytes\n", kind_str, info->size);
    }
  }
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
      if(list_item->kind == ListKind_ast1_node)
      {
        AstNode1* node = AST1_ITEM(list_item);
        DEBUG_print_ast1_node(str, indent_level, 0, node);
      }
      else if(list_item->kind == ListKind_ast2_node)
      {
        AstNode2* node = AST2_ITEM(list_item);
        DEBUG_print_ast2_node(str, indent_level, 0, node);
      }
      else
        assert(0);
    }
  }
}
#endif

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

  AstNode* module1 = 0;
  if(vm_program->success = parse(&token_stream, &module1))
  {
    if(DEBUG_enabled)/*>>>*/
    {
      DEBUG_print_arena_usage("Syntactic");

      begin_temp_memory(&arena);
      String* str = str_new(arena);
      //DEBUG_print_ast1_node(str, 0, "module", module1);
      str_dump_to_file(str, "debug_syntax.txt");
      end_temp_memory(&arena);
    }/*<<<*/

#if 0
    // process the includes
    for(ListItem* list_item = AST1(AST1(module1, module)->body, block)->nodes->first;
        list_item;
        list_item = list_item->next)
    {
      assert(list_item->kind == ListKind_ast1_node);
      AstNode1* stmt = list_item->ast1_node;

      if(stmt->kind == AstKind1_include)
      {
        auto* include = AST1(stmt, include);
        process_includes(AST1(include->body, block)->nodes,
                         AST1(AST1(module1, module)->body, block)->nodes, list_item);
      }
    }

    AstNode2* module2 = 0;
    if(vm_program->success = build_ast2(module1, &module2))
    {
      assert(symbol_table->scope_id == 0);
      assert(symbol_table->nesting_depth == 0);

      if(DEBUG_enabled)/*>>>*/
      {
        DEBUG_print_arena_usage("Semantic");

        begin_temp_memory(&arena);
        String* str = str_new(arena);
        DEBUG_print_ast2_node(str, 0, "module", module2);
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
#endif
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

