#define SYMBOL_ARENA_SIZE (ARENA_SIZE / 8)

SymbolTable* symbol_table = 0;
int tempvar_id = 0;

Type* basic_type_bool;
Type* basic_type_int;
Type* basic_type_char;
Type* basic_type_float;
Type* basic_type_void;
Type* basic_type_type;
List* subst_list;
int typevar_id = 1;

int last_label_id;

char* make_unique_label()
{
  static char buf[10];
  h_sprintf(buf, "L%d", last_label_id++);
  return buf;
}

char* make_temp_name(char* label)
{
  String* str = str_new(arena);
  str_printf(str, "$%s%d", label, tempvar_id++);
  return str_cap(str);
}

void init_ast_meta_info(AstMetaInfo* ast, Ast_Gen gen)
{
  if(gen == Ast_gen0)
  {/*>>> gen0*/
    ast->kind_count = 23;
    ast->kinds = mem_push_array(arena, AstKindMetaInfo, ast->kind_count);

    int kind_index = 0;
    AstKindMetaInfo* kind = 0;

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_empty;
      kind->attr_count = 0;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_type_decl;
      kind->attr_count = 1;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_type_expr;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_array;
      kind->attr_count = 2;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_elem_expr;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_size_expr;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_pointer;
      kind->attr_count = 1;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_pointee_expr;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_proc_occur;
      kind->attr_count = 2;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_id;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_list;
      attr->name = AstAttributeName_actual_args;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_ret_stmt;
      kind->attr_count = 1;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_ret_expr;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_stmt;
      kind->attr_count = 1;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_stmt;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_if_stmt;
      kind->attr_count = 3;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_cond_expr;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_body;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_else_body;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_while_stmt;
      kind->attr_count = 2;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_cond_expr;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_body;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_proc_decl;
      kind->attr_count = 4;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_id;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_ret_var;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_list;
      attr->name = AstAttributeName_formal_args;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_body;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_lit;
      kind->attr_count = 6;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_lit_kind;
      attr->name = AstAttributeName_lit_kind;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_int_val;
      attr->name = AstAttributeName_int_val;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_float_val;
      attr->name = AstAttributeName_float_val;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_bool_val;
      attr->name = AstAttributeName_bool_val;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_char_val;
      attr->name = AstAttributeName_char_val;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_str_val;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_str;
      kind->attr_count = 1;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_str_lit;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_continue_stmt;
      kind->attr_count = 0;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_break_stmt;
      kind->attr_count = 0;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_var_decl;
      kind->attr_count = 3;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_id;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_init_expr;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_module;
      kind->attr_count = 2;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_file_path;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_body;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_include;
      kind->attr_count = 2;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_file_path;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_body;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_block;
      kind->attr_count = 3;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_list;
      attr->name = AstAttributeName_nodes;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_list;
      attr->name = AstAttributeName_procs;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_list;
      attr->name = AstAttributeName_stmts;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_id;
      kind->attr_count = 1;
      
      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_name;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_bin_expr;
      kind->attr_count = 3;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_left_operand;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_right_operand;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_op_kind;
      attr->name = AstAttributeName_op_kind;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_un_expr;
      kind->attr_count = 2;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_operand;
      
      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_op_kind;
      attr->name = AstAttributeName_op_kind;
    }
  }/*<<<*/
  else if(gen == Ast_gen1)
  {/*>>> gen1*/
    ast->kind_count = 23;
    ast->kinds = mem_push_array(arena, AstKindMetaInfo, ast->kind_count);

    int kind_index = 0;
    AstKindMetaInfo* kind = 0;

    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_empty;
      kind->attr_count = 2;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_array;
      kind->attr_count = 4;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_elem_expr;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_size_expr;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_pointer;
      kind->attr_count = 3;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_pointee_expr;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_type_decl;
      kind->attr_count = 5;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_name;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_symbol;
      attr->name = AstAttributeName_decl_sym;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_type_expr;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_type_occur;
      kind->attr_count = 5;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_name;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_symbol;
      attr->name = AstAttributeName_occur_sym;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_type_decl;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_stmt;
      kind->attr_count = 3;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_stmt;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_ret_stmt;
      kind->attr_count = 7;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_ret_expr;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_proc_decl;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_int_val;
      attr->name = AstAttributeName_nesting_depth;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_symbol;
      attr->name = AstAttributeName_decl_sym;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_symbol;
      attr->name = AstAttributeName_occur_sym;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_break_stmt;
      kind->attr_count = 4;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_loop;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_int_val;
      attr->name = AstAttributeName_nesting_depth;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_continue_stmt;
      kind->attr_count = 4;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_loop;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_int_val;
      attr->name = AstAttributeName_nesting_depth;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_while_stmt;
      kind->attr_count = 6;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_cond_expr;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_body;
      
      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;
      
      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_label_eval;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_label_break;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_un_expr;
      kind->attr_count = 4;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_operand;
      
      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_op_kind;
      attr->name = AstAttributeName_op_kind;
      
      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;
      
      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_if_stmt;
      kind->attr_count = 7;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_cond_expr;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_body;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_else_body;
      
      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;
      
      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_label_else;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_label_end;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_proc_occur; // procedure call
      kind->attr_count = 6;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_name;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_symbol;
      attr->name = AstAttributeName_occur_sym;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_proc_decl;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_list;
      attr->name = AstAttributeName_actual_args;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_proc_decl;
      kind->attr_count = 9;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_name;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_symbol;
      attr->name = AstAttributeName_decl_sym;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_ret_var;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_list;
      attr->name = AstAttributeName_formal_args;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_body;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_scope;
      attr->name = AstAttributeName_scope;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_label_end;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_lit;
      kind->attr_count = 10;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_lit_kind;
      attr->name = AstAttributeName_lit_kind;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_scope;
      attr->name = AstAttributeName_scope;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_int_val;
      attr->name = AstAttributeName_int_val;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_float_val;
      attr->name = AstAttributeName_float_val;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_bool_val;
      attr->name = AstAttributeName_bool_val;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_char_val;
      attr->name = AstAttributeName_char_val;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_str_val;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_str;
      kind->attr_count = 7;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_str_lit;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_str_val;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_symbol;
      attr->name = AstAttributeName_decl_sym;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_symbol;
      attr->name = AstAttributeName_occur_sym;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_module;
      kind->attr_count = 4;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_file_path;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_body;
      
      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;
      
      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_block;
      kind->attr_count = 6;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_list;
      attr->name = AstAttributeName_nodes;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_list;
      attr->name = AstAttributeName_stmts;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_list;
      attr->name = AstAttributeName_procs;
      
      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_scope;
      attr->name = AstAttributeName_scope;
      
      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;
      
      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_var_decl;
      kind->attr_count = 7;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_name;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_symbol;
      attr->name = AstAttributeName_decl_sym;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_type;
      
      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_init_expr;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_var_occur;
      kind->attr_count = 6;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_name;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_symbol;
      attr->name = AstAttributeName_occur_sym;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_symbol;
      attr->name = AstAttributeName_decl_sym;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_var_decl;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;
    }
    {
      assert(kind_index < ast->kind_count);
      kind = &ast->kinds[kind_index++];
      kind->kind = AstNode_bin_expr;
      kind->attr_count = 6;

      int attr_index = 0;
      AstAttributeMetaInfo* attr = 0;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_op_kind;
      attr->name = AstAttributeName_op_kind;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_left_operand;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_ast_node;
      attr->name = AstAttributeName_right_operand;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_type;
      attr->name = AstAttributeName_eval_type;

      assert(attr_index < kind->attr_count);
      attr = &kind->attrs[attr_index++];
      attr->kind = AstAttribute_str_val;
      attr->name = AstAttributeName_label_end;
    }
  }/*<<<*/
  else
    assert(0);
}

void init_ast_meta_infos()
{
  AstMetaInfo* ast = 0;
  int gen_count = Ast_gen_Count;

  for(int gen = 0; gen < gen_count; gen++)
  {
    ast = &ast_meta_infos[gen];
    init_ast_meta_info(ast, (Ast_Gen)gen);
  }
}

#define ATTR(NODE, KIND, NAME)\
  (get_ast_attribute_f((NODE), AstAttribute_##KIND, AstAttributeName_##NAME)->KIND)

AstAttribute* get_ast_attribute_f(AstNode* node, AstAttributeKind kind, AstAttributeName name)
{
  AstAttribute* result = 0;

  assert(node->gen >= 0 && node->gen < Ast_gen_Count);
  AstMetaInfo* ast_meta = &ast_meta_infos[node->gen];

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
    AstAttributeMetaInfo* attr_meta = 0;
    int attr_index = 0;
    for(;
        attr_index < kind_meta->attr_count;
        attr_index++)
    {
      attr_meta = &kind_meta->attrs[attr_index];
      if(attr_meta->kind == kind && attr_meta->name == name)
      {
        break;
      }
      attr_meta = 0;
    }

    if(attr_meta)
    {
      result = &node->attrs[attr_index];
      assert(result->kind == attr_meta->kind);
      assert(result->name == attr_meta->name);
    }
    else
      assert(0);
  }
  else
    assert(0);

  return result;
}

AstNode* make_ast_node(Ast_Gen gen, AstNode* node, AstKind kind)
{
  assert(gen >= 0 && gen < Ast_gen_Count);
  node->gen = gen;
  node->kind = kind;

  AstMetaInfo* ast_meta = &ast_meta_infos[node->gen];

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

  mem_zero_array(node->attrs, AstAttribute);

  if(kind_meta)
  {
    for(int attr_index = 0; 
       attr_index < kind_meta->attr_count;
       attr_index++)
    {
      AstAttributeMetaInfo* attr_meta = &kind_meta->attrs[attr_index];
      AstAttribute* attr = &node->attrs[attr_index];
      attr->kind = attr_meta->kind;
      attr->name = attr_meta->name;
    }
  }
  else
    assert(0);

  return node;
}

AstNode* new_ast_node(Ast_Gen gen, AstKind kind, SourceLoc* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode);
  node->src_loc = src_loc;
  make_ast_node(gen, node, kind);
  return node;
}

void DEBUG_print_arena_usage(MemoryArena* arena, char* tag)
{
  ArenaUsage usage = arena_usage(arena);
  h_printf("in_use(`%s`) : %.2f%%\n", tag, usage.in_use*100);
}

void make_type_printstr(String* str, Type* type)
{
  if(type->kind == Type_basic)
  {
    if(type->basic.kind == BasicType_bool)
      str_append(str, "bool");
    else if(type->basic.kind == BasicType_int)
      str_append(str, "int");
    else if(type->basic.kind == BasicType_float)
      str_append(str, "float");
    else if(type->basic.kind == BasicType_char)
      str_append(str, "char");
    else if(type->basic.kind == BasicType_void)
      str_append(str, "void");
  }
  else if(type->kind == Type_pointer)
  {
    make_type_printstr(str, type->pointer.pointee);
    str_append(str, "*");
  }
  else if(type->kind == Type_array)
  {
    str_append(str, "(");
    if(type->array.size >= 0)
      str_printf(str, "[%d]", type->array.size);
    else
      str_append(str, "[]");
    make_type_printstr(str, type->array.elem);
    str_append(str, ")");
  }
  else if(type->kind == Type_product)
  {
    make_type_printstr(str, type->product.left);
    str_append(str, ", ");
    make_type_printstr(str, type->product.right);
  }
  else if(type->kind == Type_proc)
  {
    make_type_printstr(str, type->proc.ret);
    str_append(str, " (");
    make_type_printstr(str, type->proc.args);
    str_append(str, ")");
  }
  else if(type->kind == Type_typevar)
  {
    str_printf(str, "$type%d", type->typevar.id);
  }
  else
    assert(0);
}

void DEBUG_print_line(String* str, int indent_level, char* message, ...)
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

void DEBUG_print_type(String* str, int indent_level, char* tag, Type* type)
{
  for(int i = 0; i < indent_level; i++)
  {
    str_append(str, "  ");
  }

  if(tag)
  {
    str_append(str, tag);
    str_append(str, ": ");
  }

  make_type_printstr(str, type);
  str_append(str, "\n");
}

void DEBUG_print_scope(String* str, int indent_level, char* tag, Scope* scope)
{
  if(scope)
  {
    if(tag)
    {
      DEBUG_print_line(str, indent_level, "%s @%lu", tag, scope);
      ++indent_level;
    }

    DEBUG_print_line(str, indent_level, "kind: %s", get_scope_kind_printstr(scope->kind));
    DEBUG_print_line(str, indent_level, "nesting_depth: %d", scope->nesting_depth);

    if(scope->encl_scope)
    {
      DEBUG_print_line(str, indent_level, "encl_scope @%lu %s",
                       scope->encl_scope, get_scope_kind_printstr(scope->encl_scope->kind));
    }

    if(scope->ast_node)
    {
      DEBUG_print_line(str, indent_level, "ast_node @%lu %s",
                       scope->ast_node, get_ast_kind_printstr(scope->ast_node->kind));
    }
  }
}

void DEBUG_print_ast_node(String* str, int indent_level, char* tag, AstNode* node)
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

    if(node->kind == AstNode_module || node->kind == AstNode_include)
    {
      DEBUG_print_line(str, indent_level, "file_path: \"%s\"", ATTR(node, str_val, file_path));
      DEBUG_print_ast_node(str, indent_level, "body", ATTR(node, ast_node, body));
    }
    else if(node->kind == AstNode_proc_decl)
    {
      if(node->gen == Ast_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "ret_var", ATTR(node, ast_node, ret_var));
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "formal_args", ATTR(node, list, formal_args));
        DEBUG_print_ast_node(str, indent_level, "body", ATTR(node, ast_node, body));
      }
      else if(node->gen == Ast_gen1)
      {
        //todo: print the symbol
        DEBUG_print_line(str, indent_level, "name: `%s`", ATTR(node, str_val, name));
        //DEBUG_print_scope(str, indent_level, "scope", ATTR(node, scope, scope));
        DEBUG_print_ast_node_list(str, indent_level, "formal_args", ATTR(node, list, formal_args));
        DEBUG_print_ast_node(str, indent_level, "body", ATTR(node, ast_node, body));
        //DEBUG_print_ast_node(str, indent_level, "ret_var", ATTR(node, ast_node, ret_var));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_var_decl)
    {
      if(node->gen == Ast_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "type", ATTR(node, ast_node, type));
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node(str, indent_level, "init_expr", ATTR(node, ast_node, init_expr));
      }
      else if(node->gen == Ast_gen1)
      {
        DEBUG_print_line(str, indent_level, "name: `%s`", ATTR(node, str_val, name));
        DEBUG_print_ast_node(str, indent_level, "init_expr", ATTR(node, ast_node, init_expr));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_var_occur)
    {
      if(node->gen == Ast_gen1)
      {
        DEBUG_print_line(str, indent_level, "name: `%s`", ATTR(node, str_val, name));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_str)
    {
      if(node->gen == Ast_gen0 || node->gen == Ast_gen1)
      {
        DEBUG_print_ast_node(str, indent_level, "str_lit", ATTR(node, ast_node, str_lit));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_id)
    {
      if(node->gen == Ast_gen0)
      {
        DEBUG_print_line(str, indent_level, "name: `%s`", ATTR(node, str_val, name));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_block)
    {
      if(node->gen == Ast_gen1)
      {
        DEBUG_print_scope(str, indent_level, "scope", ATTR(node, scope, scope));
      }
      DEBUG_print_ast_node_list(str, indent_level, "nodes", ATTR(node, list, nodes));
    }
    else if(node->kind == AstNode_bin_expr)
    {
      DEBUG_print_line(str, indent_level, "op: %s", get_operator_kind_printstr(ATTR(node, op_kind, op_kind)));
      DEBUG_print_ast_node(str, indent_level, "left_operand", ATTR(node, ast_node, left_operand));
      DEBUG_print_ast_node(str, indent_level, "right_operand", ATTR(node, ast_node, right_operand));
    }
    else if(node->kind == AstNode_un_expr)
    {
      DEBUG_print_line(str, indent_level, "op: %s", get_operator_kind_printstr(ATTR(node, op_kind, op_kind)));
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
      else if(lit_kind == Literal_str_val)
      {
        DEBUG_print_line(str, indent_level, "str: \"%s\"", ATTR(node, str_val, str_val));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_stmt)
    {
      DEBUG_print_ast_node(str, indent_level, "stmt", ATTR(node, ast_node, stmt));
    }
    else if(node->kind == AstNode_ret_stmt)
    {
      DEBUG_print_ast_node(str, indent_level, "ret_expr", ATTR(node, ast_node, ret_expr));
#if 0
      if(node->gen == Ast_gen1)
      {
        AstNode* proc = ATTR(node, ast_node, proc);
        DEBUG_print_line(str, indent_level, "proc @%lu `%s`", proc, ATTR(proc, str, name));
        DEBUG_print_line(str, indent_level, "nesting_depth: %d", ATTR(node, int_val, nesting_depth));
      }
#endif
    }
    else if(node->kind == AstNode_break_stmt || node->kind == AstNode_continue_stmt)
    {
#if 0
      if(node->gen == Ast_gen1)
      {
        DEBUG_print_line(str, indent_level, "nesting_depth: %d", ATTR(node, int_val, nesting_depth));
      }
#endif
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
    else if(node->kind == AstNode_array)
    {
      if(node->gen == Ast_gen0 || node->gen == Ast_gen1)
      {
        DEBUG_print_ast_node(str, indent_level, "elem_expr", ATTR(node, ast_node, elem_expr));
        DEBUG_print_ast_node(str, indent_level, "size_expr", ATTR(node, ast_node, size_expr));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_pointer)
    {
      if(node->gen == Ast_gen0 || node->gen == Ast_gen1)
      {
        DEBUG_print_ast_node(str, indent_level, "pointee_expr", ATTR(node, ast_node, pointee_expr));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_proc_occur)
    {
      if(node->gen == Ast_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "actual_args", ATTR(node, list, actual_args));
      }
      else if(node->gen == Ast_gen1)
      {
        DEBUG_print_line(str, indent_level, "name: `%s`", ATTR(node, str_val, name));
        DEBUG_print_ast_node_list(str, indent_level, "actual_args", ATTR(node, list, actual_args));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_struct_decl)
    {
      if(node->gen == Ast_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "members", ATTR(node, list, members));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_union_decl)
    {
      if(node->gen == Ast_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "members", ATTR(node, list, members));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_enum_decl)
    {
      if(node->gen == Ast_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "members", ATTR(node, list, members));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_init_list)
    {
      if(node->gen == Ast_gen0)
      {
        DEBUG_print_ast_node_list(str, indent_level, "members", ATTR(node, list, members));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_goto_stmt)
    {
      if(node->gen == Ast_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_label)
    {
      if(node->gen == Ast_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_type_decl)
    {
      if(node->gen == Ast_gen0 || node->gen == Ast_gen1)
      {
        DEBUG_print_ast_node(str, indent_level, "type_expr", ATTR(node, ast_node, type_expr));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_type_occur)
    {
      if(node->gen == Ast_gen1)
      {
        DEBUG_print_line(str, indent_level, "name: `%s`", ATTR(node, str_val, name));
      }
      else
        assert(0);
    }
    else if(node->kind == AstNode_empty)
    {
      ;//ok
    }
    else
      fail(get_ast_kind_printstr(node->kind));
  }
}

void DEBUG_print_ast_node_list(String* str, int indent_level, char* tag, List* node_list)
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

#include "lex.c"
#include "syntax.c"
#include "type.c"
#include "semantic.c"
#include "runtime.c"
#include "codegen.c"
#include "hasm.c"

bool translate(char* file_path, char* hoc_text, VmProgram** vm_program)
{
  *vm_program = mem_push_struct(arena, VmProgram);
  (*vm_program)->instr_list = new_list(arena, List_vm_instr);

  TokenStream token_stream = {0};
  init_token_stream(&token_stream, hoc_text, file_path);
  get_next_token(&token_stream);

  init_ast_meta_infos();

  AstNode* module = 0;
  bool success = false;
  if(success = parse(&token_stream, &module))
  {
    if(DEBUG_enabled)/*>>>*/
    {
      h_printf("--- Parse ---\n");
      DEBUG_print_arena_usage(arena, "arena");

      begin_temp_memory(&arena);
      String* str = str_new(arena);
      DEBUG_print_ast_node(str, 0, "module", module);
      str_dump_to_file(str, "debug_parse.txt");
      end_temp_memory(&arena);
    }/*<<<*/

    /*>>> process the includes*/
    {
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
    }/*<<<*/

    init_types();

    symbol_table = new_symbol_table(&arena, SYMBOL_ARENA_SIZE);
    symbol_table->global_scope = begin_scope(Scope_global, 0);
    add_builtin_types();
    add_builtin_procs();

    success = name_ident(module);
    end_scope();
    assert(symbol_table->active_scope == 0);
    assert(symbol_table->nesting_depth == -1);

    if(success)
    {
      if(DEBUG_enabled)/*>>>*/
      {
        h_printf("--- Name ID ---\n");
        DEBUG_print_arena_usage(arena, "arena");
        DEBUG_print_arena_usage(symbol_table->arena, "symbol_table");

        begin_temp_memory(&arena);
        String* str = str_new(arena);
        DEBUG_print_scope(str, 0, "global_scope", symbol_table->global_scope);
        DEBUG_print_ast_node(str, 0, "module", module);
        str_dump_to_file(str, "debug_name_ident.txt");
        end_temp_memory(&arena);
      }/*<<<*/

      build_types(module);
      if(success = eval_types(module) && resolve_types(module) && check_types(module))
      {
        if(DEBUG_enabled)/*>>>*/
        {
          h_printf("--- Semantic ---\n");
          DEBUG_print_arena_usage(arena, "arena");
        }/*<<<*/

        build_runtime();
        if(DEBUG_enabled)/*>>>*/
        {
          h_printf("--- Runtime ---\n");
          DEBUG_print_arena_usage(arena, "arena");
        }/*<<<*/

        gen_program(*vm_program, module);
        if(DEBUG_enabled)/*>>>*/
        {
          h_printf("--- Codegen ---\n");
          DEBUG_print_arena_usage(arena, "arena");
        }/*<<<*/

        print_code(*vm_program);
        if(DEBUG_enabled)/*>>>*/
        {
          h_printf("--- Print code ---\n");
          DEBUG_print_arena_usage(arena, "arena");
        }/*<<<*/
      }
    }
  }
  return success;
}


