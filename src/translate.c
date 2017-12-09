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

Label make_unique_label()
{
  Label label = {0};
  h_sprintf(label.name, "L%d", last_label_id++);
  return label;
}

char* make_temp_name(char* label)
{
  String str; str_init(&str, arena);
  str_printf(&str, "$%s%d", label, tempvar_id++);
  return str_cap(&str);
}

AstNode* new_ast_node(eAstNode kind, SourceLoc* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode);
  node->src_loc = src_loc;
  node->kind = kind;
  return node;
}

void DEBUG_print_arena_usage(MemoryArena* arena, char* tag)
{
  ArenaUsage usage = arena_usage(arena);
  h_printf("in_use(`%s`) : %.2f%%\n", tag, usage.in_use*100);
}

void make_type_printstr(String* str, Type* type)
{
  if(type->kind == eType_basic)
  {
    if(type->basic.kind == eBasicType_bool)
      str_append(str, "bool");
    else if(type->basic.kind == eBasicType_int)
      str_append(str, "int");
    else if(type->basic.kind == eBasicType_float)
      str_append(str, "float");
    else if(type->basic.kind == eBasicType_char)
      str_append(str, "char");
    else if(type->basic.kind == eBasicType_void)
      str_append(str, "void");
  }
  else if(type->kind == eType_pointer)
  {
    make_type_printstr(str, type->pointer.pointee);
    str_append(str, "*");
  }
  else if(type->kind == eType_array)
  {
    str_append(str, "(");
    if(type->array.size >= 0)
      str_printf(str, "[%d]", type->array.size);
    else
      str_append(str, "[]");
    make_type_printstr(str, type->array.elem);
    str_append(str, ")");
  }
  else if(type->kind == eType_product)
  {
    make_type_printstr(str, type->product.left);
    str_append(str, ", ");
    make_type_printstr(str, type->product.right);
  }
  else if(type->kind == eType_proc)
  {
    make_type_printstr(str, type->proc.ret);
    str_append(str, " (");
    make_type_printstr(str, type->proc.args);
    str_append(str, ")");
  }
  else if(type->kind == eType_typevar)
  {
    str_printf(str, "$type%d", type->typevar.id);
  }
  else
    assert(0);
}

#if 0
void DEBUG_print_line(String* str, int indent_level, char* message, ...)
{/*>>>*/
  for(int i = 0; i < indent_level; i++)
  {
    str_append(str, "  ");
  }

  va_list varargs;
  va_start(varargs, message);
  str_printf_va(str, message, varargs);
  va_end(varargs);

  str_append(str, "\n");
}/*<<<*/

void DEBUG_print_type(String* str, int indent_level, char* tag, Type* type)
{/*>>>*/
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
}/*<<<*/

void DEBUG_print_scope(String* str, int indent_level, char* tag, Scope* scope)
{/*>>>*/
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
}/*<<<*/

void DEBUG_print_ast_node(String* str, int indent_level, char* tag, AstNode* node)
{/*>>>*/
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

    if(node->kind == eAstNode_module || node->kind == eAstNode_include)
    {
      DEBUG_print_line(str, indent_level, "file_path: \"%s\"", ATTR(node, str_val, file_path));
      DEBUG_print_ast_node(str, indent_level, "body", ATTR(node, ast_node, body));
    }
    else if(node->kind == eAstNode_proc)
    {
      if(node->gen == eAstGen_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "ret_var", ATTR(node, ast_node, ret_var));
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "formal_args", ATTR(node, list, formal_args));
        DEBUG_print_ast_node(str, indent_level, "body", ATTR(node, ast_node, body));
      }
      else if(node->gen == eAstGen_gen1)
      {
        //todo: print the symbol
        DEBUG_print_line(str, indent_level, "name: `%s`", ATTR(node, str_val, name));
        DEBUG_print_ast_node_list(str, indent_level, "formal_args", ATTR(node, list, formal_args));
        DEBUG_print_ast_node(str, indent_level, "body", ATTR(node, ast_node, body));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_var_decl)
    {
      if(node->gen == eAstGen_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "type", ATTR(node, ast_node, type));
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node(str, indent_level, "init_expr", ATTR(node, ast_node, init_expr));
      }
      else if(node->gen == eAstGen_gen1)
      {
        DEBUG_print_line(str, indent_level, "name: `%s`", ATTR(node, str_val, name));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_var_occur)
    {
      if(node->gen == eAstGen_gen1)
      {
        DEBUG_print_line(str, indent_level, "name: `%s`", ATTR(node, str_val, name));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_id)
    {
      if(node->gen == eAstGen_gen0)
      {
        DEBUG_print_line(str, indent_level, "name: `%s`", ATTR(node, str_val, name));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_block)
    {
      if(node->gen == eAstGen_gen0)
      {
        DEBUG_print_ast_node_list(str, indent_level, "nodes", ATTR(node, list, nodes));
      }
      else if(node->gen == eAstGen_gen1)
      {
        DEBUG_print_scope(str, indent_level, "scope", ATTR(node, scope, scope));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_bin_expr)
    {
      DEBUG_print_line(str, indent_level, "op: %s", get_operator_kind_printstr(ATTR(node, op_kind, op_kind)));
      DEBUG_print_ast_node(str, indent_level, "left_operand", ATTR(node, ast_node, left_operand));
      DEBUG_print_ast_node(str, indent_level, "right_operand", ATTR(node, ast_node, right_operand));
    }
    else if(node->kind == eAstNode_un_expr)
    {
      DEBUG_print_line(str, indent_level, "op: %s", get_operator_kind_printstr(ATTR(node, op_kind, op_kind)));
      DEBUG_print_ast_node(str, indent_level, "operand", ATTR(node, ast_node, operand));
    }
    else if(node->kind == eAstNode_lit)
    {
      eLiteral lit_kind = ATTR(node, lit_kind, lit_kind);
      DEBUG_print_line(str, indent_level, get_literal_kind_printstr(lit_kind));
      if(lit_kind == eLiteral_int_val)
      {
        DEBUG_print_line(str, indent_level, "int_val: %d", ATTR(node, int_val, int_val));
      }
      else if(lit_kind == eLiteral_float_val)
      {
        DEBUG_print_line(str, indent_level, "float_val: %f", ATTR(node, float_val, float_val));
      }
      else if(lit_kind == eLiteral_bool_val)
      {
        DEBUG_print_line(str, indent_level, "bool_val: %d", ATTR(node, bool_val, bool_val));
      }
      else if(lit_kind == eLiteral_char_val)
      {
        char buf[3] = {0};
        print_char(buf, ATTR(node, char_val, char_val));
        DEBUG_print_line(str, indent_level, "char_val: '%s'", buf);
      }
      else if(lit_kind == eLiteral_str_val)
      {
        DEBUG_print_line(str, indent_level, "str: \"%s\"", ATTR(node, str_val, str_val));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_stmt)
    {
      DEBUG_print_ast_node(str, indent_level, "stmt", ATTR(node, ast_node, stmt));
    }
    else if(node->kind == eAstNode_ret_stmt)
    {
      DEBUG_print_ast_node(str, indent_level, "ret_expr", ATTR(node, ast_node, ret_expr));
#if 0
      if(node->gen == eAstGen_gen1)
      {
        AstNode* proc = ATTR(node, ast_node, proc);
        DEBUG_print_line(str, indent_level, "proc @%lu `%s`", proc, ATTR(proc, str, name));
        DEBUG_print_line(str, indent_level, "nesting_depth: %d", ATTR(node, int_val, nesting_depth));
      }
#endif
    }
    else if(node->kind == eAstNode_break_stmt || node->kind == eAstNode_continue_stmt)
    {
#if 0
      if(node->gen == eAstGen_gen1)
      {
        DEBUG_print_line(str, indent_level, "nesting_depth: %d", ATTR(node, int_val, nesting_depth));
      }
#endif
    }
    else if(node->kind == eAstNode_if_stmt)
    {
      DEBUG_print_ast_node(str, indent_level, "cond_expr", ATTR(node, ast_node, cond_expr));
      DEBUG_print_ast_node(str, indent_level, "body", ATTR(node, ast_node, body));
      DEBUG_print_ast_node(str, indent_level, "else_body", ATTR(node, ast_node, else_body));
    }
    else if(node->kind == eAstNode_while_stmt)
    {
      DEBUG_print_ast_node(str, indent_level, "cond_expr", ATTR(node, ast_node, cond_expr));
      DEBUG_print_ast_node(str, indent_level, "body", ATTR(node, ast_node, body));
    }
    else if(node->kind == eAstNode_for_stmt)
    {
      DEBUG_print_ast_node(str, indent_level, "decl_expr", ATTR(node, ast_node, decl_expr));
      DEBUG_print_ast_node(str, indent_level, "cond_expr", ATTR(node, ast_node, cond_expr));
      DEBUG_print_ast_node(str, indent_level, "loop_expr", ATTR(node, ast_node, loop_expr));
      DEBUG_print_ast_node(str, indent_level, "body", ATTR(node, ast_node, body));
    }
    else if(node->kind == eAstNode_array)
    {
      if(node->gen == eAstGen_gen0 || node->gen == eAstGen_gen1)
      {
        DEBUG_print_ast_node(str, indent_level, "elem_expr", ATTR(node, ast_node, elem_expr));
        DEBUG_print_ast_node(str, indent_level, "size_expr", ATTR(node, ast_node, size_expr));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_pointer)
    {
      if(node->gen == eAstGen_gen0 || node->gen == eAstGen_gen1)
      {
        DEBUG_print_ast_node(str, indent_level, "pointee_expr", ATTR(node, ast_node, pointee_expr));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_proc_occur)
    {
      if(node->gen == eAstGen_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "actual_args", ATTR(node, list, actual_args));
      }
      else if(node->gen == eAstGen_gen1)
      {
        DEBUG_print_line(str, indent_level, "name: `%s`", ATTR(node, str_val, name));
        DEBUG_print_ast_node_list(str, indent_level, "actual_args", ATTR(node, list, actual_args));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_struct_decl)
    {
      if(node->gen == eAstGen_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "members", ATTR(node, list, members));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_union_decl)
    {
      if(node->gen == eAstGen_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "members", ATTR(node, list, members));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_enum_decl)
    {
      if(node->gen == eAstGen_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
        DEBUG_print_ast_node_list(str, indent_level, "members", ATTR(node, list, members));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_init_list)
    {
      if(node->gen == eAstGen_gen0)
      {
        DEBUG_print_ast_node_list(str, indent_level, "members", ATTR(node, list, members));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_goto_stmt)
    {
      if(node->gen == eAstGen_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_label)
    {
      if(node->gen == eAstGen_gen0)
      {
        DEBUG_print_ast_node(str, indent_level, "id", ATTR(node, ast_node, id));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_type_decl)
    {
      if(node->gen == eAstGen_gen0 || node->gen == eAstGen_gen1)
      {
        DEBUG_print_ast_node(str, indent_level, "type_expr", ATTR(node, ast_node, type_expr));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_type_occur)
    {
      if(node->gen == eAstGen_gen1)
      {
        DEBUG_print_line(str, indent_level, "name: `%s`", ATTR(node, str_val, name));
      }
      else
        assert(0);
    }
    else if(node->kind == eAstNode_empty)
    {
      ;//ok
    }
    else if(node->kind == eAstNode_asm_block)
    {
      if(node->gen == eAstGen_gen0 || node->gen == eAstGen_gen1)
      {
        DEBUG_print_line(str, indent_level, "asm_text: `%s`", ATTR(node, str_val, asm_text));
      }
      else
        assert(0);
    }
    //else
    //  fail(get_ast_kind_printstr(node->kind));
  }
}/*<<<*/

void DEBUG_print_ast_node_list(String* str, int indent_level, char* tag, List* node_list)
{/*>>>*/
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
}/*<<<*/
#endif

#include "lex.c"
#include "syntax.c"
#include "type.c"
#include "semantic.c"
#include "x86.c"

bool translate(char* title, char* file_path, char* hoc_text, String* x86_text)
{
  TokenStream token_stream = {0};
  init_token_stream(&token_stream, hoc_text, file_path);
  get_next_token(&token_stream);

  //init_ast_meta_infos();

  AstNode* module = 0;
  if(!parse(&token_stream, &module))
  {
    return false;
  }
#if 0
  if(DEBUG_enabled)/*>>>*/
  {
    h_printf("--- Parse ---\n");
    DEBUG_print_arena_usage(arena, "arena");

    begin_temp_memory(&arena);
    String str; str_init(&str, arena);
    DEBUG_print_ast_node(&str, 0, "module", module);
    str_dump_to_file(&str, "debug_parse.txt");
    str_cap(&str);
    end_temp_memory(&arena);
  }/*<<<*/
#endif

  AstNode* module_body = module->module.body;
  List* module_nodes_list = module_body->block.nodes;
  for(ListItem* list_item = module_nodes_list->first;
      list_item;
      list_item = list_item->next)
  {
    AstNode* stmt = ITEM(list_item, ast_node);

    if(stmt->kind == eAstNode_include)
    {
      AstNode* incl_body = stmt->include.body;
      process_includes(incl_body->block.nodes, module_nodes_list, list_item);
    }
  }

  basic_type_bool = new_basic_type(eBasicType_bool);
  basic_type_int = new_basic_type(eBasicType_int);
  basic_type_char = new_basic_type(eBasicType_char);
  basic_type_float = new_basic_type(eBasicType_float);
  basic_type_void = new_basic_type(eBasicType_void);
  basic_type_type = new_basic_type(eBasicType_type);
  subst_list = new_list(arena, eList_type_pair);

  symbol_table = new_symbol_table(&arena, SYMBOL_ARENA_SIZE);
  begin_scope(eScope_global, 0);
  if(!name_ident(module))
  {
    return false;
  }
  end_scope();
  assert(symbol_table->active_scope == 0);
  assert(symbol_table->nesting_depth == -1);

#if 0
  if(DEBUG_enabled)/*>>>*/
  {
    h_printf("--- Name ID ---\n");
    DEBUG_print_arena_usage(arena, "arena");
    DEBUG_print_arena_usage(symbol_table->arena, "symbol_table");

    begin_temp_memory(&arena);
    String str; str_init(&str, arena);
    DEBUG_print_ast_node(&str, 0, "module", module);
    str_dump_to_file(&str, "debug_name_ident.txt");
    str_cap(&str);
    end_temp_memory(&arena);
  }/*<<<*/
#endif

  if(!(build_types(module) && eval_types(module)
      && resolve_types(module) && check_types(module)))
  {
    return false;
  }
  if(DEBUG_enabled)/*>>>*/
  {
    h_printf("--- Semantic ---\n");
    DEBUG_print_arena_usage(arena, "arena");
  }/*<<<*/

  for(ListItem* list_item = symbol_table->scopes->first;
      list_item;
      list_item = list_item->next)
  {
    Scope* scope = ITEM(list_item, scope);
    int offset = 0;
    for(ListItem* list_item = scope->decls[eSymbol_var]->first;
        list_item;
        list_item = list_item->next)
    {
      Symbol* symbol = ITEM(list_item, symbol);
      if(symbol->is_static_alloc)
      {
        symbol->data_loc = offset;
        offset += symbol->ty->width;
        scope->static_area_size += symbol->ty->width;
      }
      else
      {
        offset += symbol->ty->width;
        symbol->data_loc = -offset;
        scope->locals_area_size += symbol->ty->width;
      }
    }

    offset = 3*MACHINE_WORD_SIZE;
    for(ListItem* list_item = scope->decls[eSymbol_formal_arg]->first;
        list_item;
        list_item = list_item->next)
    {
      Symbol* symbol = ITEM(list_item, symbol);
      symbol->data_loc = offset;
      offset += symbol->ty->width;
      scope->args_area_size += symbol->ty->width;
    }

    for(ListItem* list_item = scope->decls[eSymbol_ret_var]->first;
        list_item;
        list_item = list_item->next)
    {
      Symbol* symbol = ITEM(list_item, symbol);
      symbol->data_loc = offset;
      offset += symbol->ty->width;
      scope->ret_area_size += symbol->ty->width;
    }

    //FIXME: This piece of code feels awfully out of place.
    for(ListItem* list_item = scope->decls[eSymbol_extern_proc]->first;
        list_item;
        list_item = list_item->next)
    {
      Symbol* decl_sym = ITEM(list_item, symbol);
      AstNode* proc = decl_sym->ast_node;
      assert(proc->kind == eAstNode_proc);
      assert(proc->proc.is_extern);

      int args_size = compute_type_width(decl_sym->ty->proc.args);
      char* label = proc->proc.label;
      String decorated_label; str_init(&decorated_label, arena);
      str_printf(&decorated_label, "%s@%d", label, args_size);
      proc->proc.label = str_cap(&decorated_label);
    }
  }

  str_init(x86_text, push_arena(&arena, X86_CODE_ARENA_SIZE));
  str_printfln(x86_text, ".686");
  str_printfln(x86_text, ".XMM");
  str_printfln(x86_text, ".MODEL flat, C");
  str_printfln(x86_text, ".STACK 4096");

  str_printfln(x86_text, ".DATA");
  str_printfln(x86_text, "static_area LABEL BYTE");
  Scope* module_scope = module->module.scope;
  for(ListItem* list_item = module_scope->decls[eSymbol_var]->first;
      list_item;
      list_item = list_item->next)
  {
    Symbol* symbol = ITEM(list_item, symbol);
    int data_size = symbol->ty->width;
    if(symbol->data)
    {
      str_printf(x86_text, "BYTE ");
      uint8* p_data = (uint8*)symbol->data;
      int i;
      for(i = 0; i < data_size - 1; i++)
      {
        str_printf(x86_text, "0%xh,", p_data[i]);
      }
      if(i < data_size)
      {
        str_printf(x86_text, "0%xh", p_data[i]);
      }
      str_printfln(x86_text, "");
    }
    else
    {
      str_printfln(x86_text, "BYTE %d DUP(?) ; %s", data_size, symbol->name);
    }
  }

  str_printfln(x86_text, ".CODE");

  Label label;
  str_printfln(x86_text, "_rt_module_prologue PROC");
  str_printfln(x86_text, "pop ebx ;return address");
  str_printfln(x86_text, "push esp ;access link");
  str_printfln(x86_text, "sub esp, %d ;dummy IP", MACHINE_WORD_SIZE);
  str_printfln(x86_text, "push ebp");
  str_printfln(x86_text, "mov ebp, esp");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_module_prologue ENDP");
  
  str_printfln(x86_text, "_rt_block_prologue PROC");
  str_printfln(x86_text, "pop ebx ;return address");
  str_printfln(x86_text, "push ebp");
  str_printfln(x86_text, "add dword ptr [esp], %d ;access link", 2*MACHINE_WORD_SIZE);
  str_printfln(x86_text, "sub esp, %d ;dummy IP", MACHINE_WORD_SIZE);
  str_printfln(x86_text, "push ebp");
  str_printfln(x86_text, "mov ebp, esp");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_block_prologue ENDP");

  str_printfln(x86_text, "_rt_load PROC");
  str_printfln(x86_text, "pop ebx ;return address");
  str_printfln(x86_text, "pop ecx ;byte count");
  str_printfln(x86_text, "mov esi, dword ptr [esp]");
  str_printfln(x86_text, "mov edi, esp");
  str_printfln(x86_text, "rep movs byte ptr [edi], byte ptr [esi]");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_load ENDP");

  str_printfln(x86_text, "_rt_store PROC");
  str_printfln(x86_text, "pop ebx ;return address;");
  str_printfln(x86_text, "pop ecx");
  str_printfln(x86_text, "pop edi");
  str_printfln(x86_text, "mov esi, esp");
  str_printfln(x86_text, "rep movs byte ptr [edi], byte ptr [esi]");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_store ENDP");

  str_printfln(x86_text, "_rt_leave_frame PROC");
  str_printfln(x86_text, "pop ebx ;return address;");
  label = make_unique_label();
  str_printfln(x86_text, "pop ecx ;depth");
  str_printfln(x86_text, "%s$loop:", label.name);
  str_printfln(x86_text, "mov esp, ebp");
  str_printfln(x86_text, "pop ebp");
  str_printfln(x86_text, "loop %s$loop", label.name);
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_leave_frame ENDP");

  str_printfln(x86_text, "_rt_load_access_link PROC");
  str_printfln(x86_text, "pop ebx ;return address;");
  str_printfln(x86_text, "pop ecx ;declaration scope offset");
  str_printfln(x86_text, "mov esi, dword ptr [esp]");
  label = make_unique_label();
  str_printfln(x86_text, "%s$loop:", label.name);
  str_printfln(x86_text, "mov esi, dword ptr[esi]");
  str_printfln(x86_text, "loop %s$loop", label.name);
  str_printfln(x86_text, "mov dword ptr [esp], esi");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_load_access_link ENDP");

  if(!gen_x86(x86_text, module))
  {
    return false;
  }
  str_printfln(x86_text, "END");

  return true;
}

