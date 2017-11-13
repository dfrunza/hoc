char* get_type_printstr(Type* type)
{
  String* str = str_new(arena);
  make_type_printstr(str, type);
  return str_cap(str);
}

bool is_relational_operator(OperatorKind op_kind)
{
  return op_kind == Operator_less || op_kind == Operator_less_eq
          || op_kind == Operator_greater || op_kind == Operator_greater_eq
          || op_kind == Operator_eq || op_kind == Operator_not_eq;
}

bool is_logical_operator(OperatorKind op_kind)
{
  return op_kind == Operator_logic_and || op_kind == Operator_logic_or || op_kind == Operator_logic_not;
}

bool is_arithmetic_operator(OperatorKind op_kind)
{
  return op_kind == Operator_add || op_kind == Operator_sub
    || op_kind == Operator_mul || op_kind == Operator_div;
}

SymbolTable* new_symbol_table(MemoryArena** arena, int size)
{
  MemoryArena* symbol_arena = push_arena(arena, size);
  symbol_table = mem_push_struct(symbol_arena, SymbolTable);
  symbol_table->arena = symbol_arena;
  symbol_table->nesting_depth = -1;
  symbol_table->scopes = new_list(symbol_arena, List_scope);
  return symbol_table;
}

typedef enum
{
  SymbolLookup__None,
  SymbolLookup_active,
  SymbolLookup_module,
  SymbolLookup_global,
}
SymbolLookup;

Scope* find_scope(ScopeKind kind)
{
  Scope* scope = symbol_table->active_scope;
  while(scope)
  {
    if(scope->kind == kind)
      break;
    scope = scope->encl_scope;
  }
  return scope;
}

Symbol* lookup_symbol(char* name, List* symbols, SymbolKind kind)
{
  Symbol* result = 0;
  ListItem* list_item = symbols->last;
  while(list_item)
  {
    Symbol* symbol = ITEM(list_item, symbol);
    if(symbol->kind == kind && cstr_match(symbol->name, name))
    {
      result = symbol;
      break;
    }
    list_item = list_item->prev;
  }
  return result;
}

Symbol* lookup_decl_symbol(char* name, Scope* scope, SymbolKind* kind_set)
{
  Symbol* result = 0;

  while(!result && scope)
  {
    for(SymbolKind* kind = kind_set;
        *kind != Symbol_None && !result;
        kind++)
    {
      result = lookup_symbol(name, scope->decls[*kind], *kind);
    }
    scope = scope->encl_scope;
  }

  return result;
}

Symbol* lookup_occur_symbol(char* name, Scope* scope, SymbolKind* kind_set)
{
  Symbol* result = 0;

  while(!result && scope)
  {
    for(SymbolKind* kind = kind_set;
        *kind != Symbol_None && !result;
        kind++)
    {
      result = lookup_symbol(name, scope->occurs[*kind], *kind);
    }
    scope = scope->encl_scope;
  }
  return result;
}

Symbol* add_decl_symbol(char* name, SourceLoc* src_loc, Scope* scope, SymbolKind kind)
{
  Symbol* sym = mem_push_struct(symbol_table->arena, Symbol);
  sym->kind = kind;
  sym->name = name;
  sym->src_loc = src_loc;
  sym->scope = scope;
  sym->nesting_depth = scope->nesting_depth;
  append_list_elem(scope->decls[kind], sym, List_symbol);
  return sym;
}

Symbol* add_occur_symbol(char* name, SourceLoc* src_loc, Scope* scope, SymbolKind kind)
{
  Symbol* sym = mem_push_struct(symbol_table->arena, Symbol);
  sym->kind = kind;
  sym->name = name;
  sym->src_loc = src_loc;
  sym->scope = scope;
  sym->nesting_depth = scope->nesting_depth;
  append_list_elem(scope->occurs[kind], sym, List_symbol);
  return sym;
}

void add_builtin_type(char* name, Type* type)
{
  assert(type->kind == Type_basic);
  AstNode* type_decl = new_ast_node(Ast_gen1, AstNode_type_decl, 0);
  ATTR(type_decl, str_val, name) = name;
  ATTR(type_decl, type, type) = type;
  Symbol* decl_sym = add_decl_symbol(name, 0, symbol_table->global_scope, Symbol_type);
  decl_sym->type = type;
  decl_sym->ast_node = type_decl;
  ATTR(type_decl, symbol, decl_sym) = decl_sym;
}

void add_builtin_proc(char* name, Type* type)
{
  assert(type->kind == Type_proc);
  AstNode* proc_decl = new_ast_node(Ast_gen1, AstNode_proc_decl, 0);
  ATTR(proc_decl, str_val, name) = name;
  ATTR(proc_decl, type, type) = type;
  Symbol* decl_sym = add_decl_symbol(name, 0, symbol_table->global_scope, Symbol_proc);
  decl_sym->type = type;
  decl_sym->ast_node = proc_decl;
  ATTR(proc_decl, symbol, decl_sym) = decl_sym;
}

void add_builtin_types()
{
  add_builtin_type("bool", basic_type_bool);
  add_builtin_type("int", basic_type_int);
  add_builtin_type("char", basic_type_char);
  add_builtin_type("float", basic_type_float);
  add_builtin_type("void", basic_type_void);
  /*add_builtin_type("type", basic_type_type);*/
}

void add_builtin_procs()
{
  /*add_builtin_proc("putc", new_proc_type(basic_type_char, basic_type_void));*/
}

Scope* begin_scope(ScopeKind kind, AstNode* ast_node)
{
  Scope* scope = mem_push_struct(symbol_table->arena, Scope);
  scope->kind = kind;
  scope->nesting_depth = ++symbol_table->nesting_depth;
  scope->encl_scope = symbol_table->active_scope;
  scope->ast_node = ast_node;
  for(int i = 0; i < Symbol_Count; i++)
  {
    scope->decls[i] = new_list(arena, List_symbol);
    scope->occurs[i] = new_list(arena, List_symbol);
  }
  scope->ret_area.kind = DataArea_data;
  scope->args_area.kind = DataArea_data;
  scope->link_area.kind = DataArea_data;
  scope->ctrl_area.kind = DataArea_data;
  scope->local_area.kind = DataArea_data;
  symbol_table->active_scope = scope;
  append_list_elem(symbol_table->scopes, scope, List_scope);
  return scope;
}

void end_scope()
{
  --symbol_table->nesting_depth;
  Scope* active_scope = symbol_table->active_scope;
  symbol_table->active_scope = active_scope->encl_scope;
}

void process_includes(List* include_list, List* module_list, ListItem* module_list_item)
{
  for(ListItem* list_item = include_list->first;
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = ITEM(list_item, ast_node);

    if(node->kind == AstNode_include)
    {
      AstNode* block = ATTR(node, ast_node, body);
      process_includes(ATTR(block, list, nodes), include_list, list_item);
    }
  }
  replace_list_item_at(include_list, module_list, module_list_item);

  mem_zero_struct(include_list, List);
}

bool name_ident(AstNode* node);

bool name_ident_type(AstNode* node)
{
  assert(node->gen == Ast_gen0);
  bool success = true;

  if(node->kind == AstNode_id)
  {
    AstNode id_gen0 = *node;
    AstNode* type_occur_gen1 = make_ast_node(Ast_gen1, node, AstNode_type_occur);
    char* name = ATTR(&id_gen0, str_val, name);
    ATTR(type_occur_gen1, str_val, name) = name;

    Symbol* decl_sym = lookup_decl_symbol(name, symbol_table->active_scope,
        (SymbolKind[]){Symbol_type, Symbol_None});
    if(decl_sym)
    {
      Symbol* occur_sym = add_occur_symbol(name, type_occur_gen1->src_loc, symbol_table->active_scope, Symbol_type);
      ATTR(type_occur_gen1, ast_node, type_decl) = decl_sym->ast_node;
      ATTR(type_occur_gen1, symbol, occur_sym) = occur_sym;
    }
    else
      success = compile_error(type_occur_gen1->src_loc, "unknown type `%s`", name);
  }
  else if(node->kind == AstNode_pointer)
  {
    AstNode ptr_gen0 = *node;
    AstNode* ptr_gen1 = make_ast_node(Ast_gen1, node, AstNode_pointer);
    ATTR(ptr_gen1, ast_node, pointee_expr) = ATTR(&ptr_gen0, ast_node, pointee_expr);

    success = name_ident_type(ATTR(&ptr_gen0, ast_node, pointee_expr));
  }
  else if(node->kind == AstNode_array)
  {
    AstNode array_gen0 = *node;
    AstNode* array_gen1 = make_ast_node(Ast_gen1, node, AstNode_array);

    ATTR(array_gen1, ast_node, elem_expr) = ATTR(&array_gen0, ast_node, elem_expr);
    ATTR(array_gen1, ast_node, size_expr) = ATTR(&array_gen0, ast_node, size_expr);

    if(success = name_ident_type(ATTR(&array_gen0, ast_node, elem_expr)))
    {
      AstNode* size_expr = ATTR(array_gen1, ast_node, size_expr);
      if(size_expr)
      {
        success = name_ident(size_expr);
      }
    }
  }
  else
    assert(0);

  return success;
}

bool name_ident_block(AstNode* node)
{
  assert(node->kind == AstNode_block);
  bool success = true;

  AstNode block_copy = *node;
  AstNode* block = make_ast_node(Ast_gen1, node, AstNode_block);
  ATTR(block, scope, scope) = symbol_table->active_scope;
  ATTR(block, list, nodes) = ATTR(&block_copy, list, nodes);

  for(ListItem* list_item = ATTR(&block_copy, list, nodes)->first;
      list_item && success;
      list_item = list_item->next)
  {
    success = name_ident(ITEM(list_item, ast_node));
  }

  return success;
}

//todo: remove this
bool name_ident_formal_arg(AstNode* node)
{
  assert(node->kind == AstNode_var_decl);
  bool success = true;

  AstNode var_decl_gen0 = *node;
  AstNode* var_decl_gen1 = make_ast_node(Ast_gen1, node, AstNode_var_decl);
  char* name = ATTR(ATTR(&var_decl_gen0, ast_node, id), str_val, name);
  ATTR(var_decl_gen1, str_val, name) = name;

  Symbol* decl_sym = lookup_decl_symbol(name, symbol_table->active_scope,
      (SymbolKind[]){Symbol_var, Symbol_formal_arg, Symbol_None});
  if(decl_sym && (decl_sym->scope == symbol_table->active_scope))
  {
    success = compile_error(var_decl_gen1->src_loc, "formal arg `%s` already declared", name);
    compile_error(decl_sym->src_loc, "see previous declaration of `%s`", name);
  }
  else
  {
    decl_sym = add_decl_symbol(name, var_decl_gen1->src_loc, symbol_table->active_scope, Symbol_formal_arg);
    ATTR(var_decl_gen1, symbol, decl_sym) = decl_sym;
    decl_sym->ast_node = var_decl_gen1;

    ATTR(var_decl_gen1, ast_node, type) = ATTR(&var_decl_gen0, ast_node, type);
    success = name_ident(ATTR(&var_decl_gen0, ast_node, type));
  }
  return success;
}

bool name_ident_var_decl(AstNode* node, Scope* scope, void* data)
{
  assert(node->kind == AstNode_var_decl);
  bool success = true;

  AstNode var_decl_gen0 = *node;
  AstNode* var_decl_gen1 = make_ast_node(Ast_gen1, node, AstNode_var_decl);
  char* name = ATTR(ATTR(&var_decl_gen0, ast_node, id), str_val, name);
  ATTR(var_decl_gen1, str_val, name) = name;

  Symbol* decl_sym = lookup_decl_symbol(name, symbol_table->active_scope,
      (SymbolKind[]){Symbol_var, Symbol_ret_var, Symbol_formal_arg, Symbol_None});
  if(decl_sym && (decl_sym->scope == symbol_table->active_scope))
  {
    success = compile_error(var_decl_gen1->src_loc, "variable `%s` already declared", name);
    compile_error(decl_sym->src_loc, "see previous declaration of `%s`", name);
  }
  else
  {
    decl_sym = ATTR(var_decl_gen1, symbol, decl_sym)
      = add_decl_symbol(name, var_decl_gen1->src_loc, scope, Symbol_var);
    decl_sym->ast_node = var_decl_gen1;
    decl_sym->data = data;

    AstNode* type = ATTR(var_decl_gen1, ast_node, type) = ATTR(&var_decl_gen0, ast_node, type);
    if(success = name_ident(type))
    {
      AstNode* init_expr = ATTR(var_decl_gen1, ast_node, init_expr) = ATTR(&var_decl_gen0, ast_node, init_expr);
      if(init_expr)
      {
        success = name_ident(init_expr);
      }
    }
  }
  return success;
}

bool name_ident(AstNode* node)
{
  assert(node->gen == Ast_gen0);
  bool success = true;

  if(node->kind == AstNode_module)
  {
    AstNode module_copy = *node;
    AstNode* module = make_ast_node(Ast_gen1, node, AstNode_module);
    ATTR(module, str_val, file_path) = ATTR(&module_copy, str_val, file_path);

    AstNode* body = ATTR(module, ast_node, body) = ATTR(&module_copy, ast_node, body);

    symbol_table->module_scope = begin_scope(Scope_module, module);
    success = name_ident_block(body);
    end_scope();
  }
  else if(node->kind == AstNode_type_decl)
  {
    AstNode type_gen0 = *node;
    AstNode* type_gen1 = make_ast_node(Ast_gen1, node, AstNode_type_decl);

    AstNode* type_expr = ATTR(type_gen1, ast_node, type_expr) = ATTR(&type_gen0, ast_node, type_expr);

    if(success = name_ident_type(type_expr))
    {
      Symbol* decl_sym = add_decl_symbol(make_temp_name("typ"), node->src_loc, symbol_table->active_scope, Symbol_type);
      ATTR(type_gen1, symbol, decl_sym) = decl_sym;
      decl_sym->ast_node = type_gen1;
    }
  }
  else if(node->kind == AstNode_var_decl)
  {
    success = name_ident_var_decl(node, symbol_table->active_scope, 0);
  }
  else if(node->kind == AstNode_id)
  {
    AstNode id_gen0 = *node;
    AstNode* var_occur_gen1 = make_ast_node(Ast_gen1, node, AstNode_var_occur);
    char* name = ATTR(&id_gen0, str_val, name);
    ATTR(var_occur_gen1, str_val, name) = name;

    Symbol* decl_sym = lookup_decl_symbol(name, symbol_table->active_scope,
        (SymbolKind[]){Symbol_var, Symbol_ret_var, Symbol_formal_arg, /*Symbol_str, */Symbol_None});
    if(decl_sym)
    {
      Symbol* occur_sym = add_occur_symbol(name, var_occur_gen1->src_loc, symbol_table->active_scope, Symbol_var);
      occur_sym->ast_node = var_occur_gen1;
      occur_sym->decl = decl_sym;
      assert(decl_sym->nesting_depth <= occur_sym->nesting_depth);
      occur_sym->decl_scope_offset = occur_sym->nesting_depth - decl_sym->nesting_depth;

      ATTR(var_occur_gen1, symbol, occur_sym) = occur_sym;
      ATTR(var_occur_gen1, symbol, decl_sym) = decl_sym;
      ATTR(var_occur_gen1, ast_node, var_decl) = decl_sym->ast_node;
    }
    else
      success = compile_error(var_occur_gen1->src_loc, "unknown var `%s`", name);
  }
  else if(node->kind == AstNode_proc_decl)
  {
    AstNode proc_decl_gen0 = *node;
    AstNode* proc_decl_gen1 = make_ast_node(Ast_gen1, node, AstNode_proc_decl);
    char* name = ATTR(ATTR(&proc_decl_gen0, ast_node, id), str_val, name);
    ATTR(proc_decl_gen1, str_val, name) = name;

    Symbol* decl_sym = lookup_decl_symbol(name, symbol_table->active_scope,
        (SymbolKind[]){Symbol_proc, Symbol_None});
    if(decl_sym && (decl_sym->scope == symbol_table->active_scope))
    {
      success = compile_error(proc_decl_gen1->src_loc, "proc `%s` already declared", name);
      compile_error(decl_sym->src_loc, "see previous declaration of `%s`", name);
    }
    else
    {
      decl_sym = ATTR(proc_decl_gen1, symbol, decl_sym)
        = add_decl_symbol(name, proc_decl_gen1->src_loc, symbol_table->active_scope, Symbol_proc);
      decl_sym->ast_node = proc_decl_gen1;

      List* formal_args = ATTR(proc_decl_gen1, list, formal_args) = ATTR(&proc_decl_gen0, list, formal_args);
      //todo
      AstNode* ret_var = ATTR(proc_decl_gen1, ast_node, ret_var) = ATTR(&proc_decl_gen0, ast_node, ret_var);
      AstNode* body = ATTR(proc_decl_gen1, ast_node, body) = ATTR(&proc_decl_gen0, ast_node, body);

      begin_scope(Scope_proc, proc_decl_gen1);
      ATTR(proc_decl_gen1, scope, scope) = symbol_table->active_scope;

      for(ListItem* list_item = formal_args->first;
          list_item && success;
          list_item = list_item->next)
      {
        success = name_ident_formal_arg(ITEM(list_item, ast_node));
      }

      if(success)
      {
        success = name_ident(ret_var) && name_ident_block(body);
      }
      end_scope();
    }
  }
  else if(node->kind == AstNode_proc_occur)
  {
    AstNode proc_occur_gen0 = *node;
    AstNode* proc_occur_gen1 = make_ast_node(Ast_gen1, node, AstNode_proc_occur);
    char* name = ATTR(ATTR(&proc_occur_gen0, ast_node, id), str_val, name);
    ATTR(proc_occur_gen1, str_val, name) = name;

    Symbol* occur_sym = add_occur_symbol(name, proc_occur_gen1->src_loc, symbol_table->active_scope, Symbol_proc);
    occur_sym->ast_node = proc_occur_gen1;
    ATTR(proc_occur_gen1, symbol, occur_sym) = occur_sym;

    Symbol* decl_sym = lookup_decl_symbol(name, symbol_table->active_scope,
        (SymbolKind[]){Symbol_proc, Symbol_None});
    if(decl_sym)
    {
      ATTR(proc_occur_gen1, symbol, occur_sym) = occur_sym;
      ATTR(proc_occur_gen1, ast_node, proc_decl) = decl_sym->ast_node;
    }

    ATTR(proc_occur_gen1, list, actual_args) = ATTR(&proc_occur_gen0, list, actual_args);
    for(ListItem* list_item = ATTR(&proc_occur_gen0, list, actual_args)->first;
        list_item && success;
        list_item = list_item->next)
    {
      success = name_ident(ITEM(list_item, ast_node));
    }
  }
  else if(node->kind == AstNode_bin_expr)
  {
    AstNode bin_expr_gen0 = *node;
    AstNode* bin_expr_gen1 = make_ast_node(Ast_gen1, node, AstNode_bin_expr);
    ATTR(bin_expr_gen1, op_kind, op_kind) = ATTR(&bin_expr_gen0 , op_kind, op_kind);

    AstNode* left_operand = ATTR(&bin_expr_gen0, ast_node, left_operand);
    AstNode* right_operand = ATTR(&bin_expr_gen0 , ast_node, right_operand);
    if(success = name_ident(left_operand) && name_ident(right_operand))
    {
      ATTR(bin_expr_gen1, ast_node, left_operand) = left_operand;
      ATTR(bin_expr_gen1, ast_node, right_operand) = right_operand;
    }
  }
  else if(node->kind == AstNode_un_expr)
  {
    AstNode un_expr_gen0 = *node;
    AstNode* un_expr_gen1 = make_ast_node(Ast_gen1, node, AstNode_un_expr);
    ATTR(un_expr_gen1, op_kind, op_kind) = ATTR(&un_expr_gen0, op_kind, op_kind);

    AstNode* operand = ATTR(&un_expr_gen0, ast_node, operand);
    if(success = name_ident(operand))
    {
      ATTR(un_expr_gen1, ast_node, operand) = operand;
    }
  }
  else if(node->kind == AstNode_lit)
  {
    AstNode lit_gen0 = *node;
    AstNode* lit_gen1 = make_ast_node(Ast_gen1, node, AstNode_lit);

    LiteralKind lit_kind = ATTR(lit_gen1, lit_kind, lit_kind) = ATTR(&lit_gen0, lit_kind, lit_kind);

    if(lit_kind == Literal_int_val)
    {
      ATTR(lit_gen1, int_val, int_val) = ATTR(&lit_gen0, int_val, int_val);
    }
    else if(lit_kind == Literal_float_val)
    {
      ATTR(lit_gen1, float_val, float_val) = ATTR(&lit_gen0, float_val, float_val);
    }
    else if(lit_kind == Literal_char_val)
    {
      ATTR(lit_gen1, char_val, char_val) = ATTR(&lit_gen0, char_val, char_val);
    }
    else if(lit_kind == Literal_bool_val)
    {
      ATTR(lit_gen1, bool_val, bool_val) = ATTR(&lit_gen0, bool_val, bool_val);
    }
    else if(lit_kind == Literal_str_val)
    {
      char* str_val = ATTR(lit_gen1, str_val, str_val) = ATTR(&lit_gen0, str_val, str_val);

      AstNode* var_decl = new_ast_node(Ast_gen0, AstNode_var_decl, node->src_loc);
      AstNode* decl_id = ATTR(var_decl, ast_node, id) = new_ast_node(Ast_gen0, AstNode_id, node->src_loc);
      char* var_name = ATTR(decl_id, str_val, name) = make_temp_name("str");

      AstNode* var_type = ATTR(var_decl, ast_node, type) = new_ast_node(Ast_gen0, AstNode_type_decl, node->src_loc);
      AstNode* type_expr = ATTR(var_type, ast_node, type_expr) = new_ast_node(Ast_gen0, AstNode_array, node->src_loc);
      AstNode* size_expr = ATTR(type_expr, ast_node, size_expr) = new_ast_node(Ast_gen0, AstNode_lit, node->src_loc);
      ATTR(size_expr, lit_kind, lit_kind) = Literal_int_val;
      ATTR(size_expr, int_val, int_val) = cstr_len(str_val) + 1; // +NULL
      AstNode* elem_expr = ATTR(type_expr, ast_node, elem_expr) = new_ast_node(Ast_gen0, AstNode_id, node->src_loc);
      ATTR(elem_expr, str_val, name) = "char";

      if(success = name_ident_var_decl(var_decl, symbol_table->module_scope, str_val))
      {
        AstNode* module_body = ATTR(symbol_table->module_scope->ast_node, ast_node, body);
        prepend_list_elem(ATTR(module_body, list, nodes), var_decl, List_ast_node);

        AstNode* occur_id = make_ast_node(Ast_gen0, node, AstNode_id);
        ATTR(occur_id, str_val, name) = var_name;

        success = name_ident(occur_id);
      }
    }
    else
      assert(0);
  }
  else if(node->kind == AstNode_block)
  {
    begin_scope(Scope_block, node);
    success = name_ident_block(node);
    end_scope();
  }
  else if(node->kind == AstNode_stmt)
  {
    AstNode stmt_gen0 = *node;
    AstNode* stmt_gen1 = make_ast_node(Ast_gen1, node, AstNode_stmt);

    AstNode* actual_stmt = ATTR(stmt_gen1, ast_node, stmt) = ATTR(&stmt_gen0, ast_node, stmt);
    if(actual_stmt)
    {
      success = name_ident(actual_stmt);
    }
  }
  else if(node->kind == AstNode_if_stmt)
  {
    AstNode if_stmt_gen0 = *node;
    AstNode* if_stmt_gen1 = make_ast_node(Ast_gen1, node, AstNode_if_stmt);

    AstNode* cond_expr = ATTR(&if_stmt_gen0, ast_node, cond_expr);
    if(success = name_ident(cond_expr))
    {
      ATTR(if_stmt_gen1, ast_node, cond_expr) = cond_expr;

      AstNode* body = ATTR(&if_stmt_gen0, ast_node, body);
      if(body->kind != AstNode_block)
      {
        List* nodes = new_list(arena, List_ast_node);
        append_list_elem(nodes, body, List_ast_node);
        body = new_ast_node(Ast_gen0, AstNode_block, body->src_loc);
        ATTR(body, list, nodes) = nodes;
      }

      begin_scope(Scope_block, if_stmt_gen1);
      success = name_ident_block(body);
      end_scope();

      if(success)
      {
        ATTR(if_stmt_gen1, ast_node, body) = body;

        AstNode* else_body = ATTR(&if_stmt_gen0, ast_node, else_body);
        if(else_body)
        {
          if(else_body->kind != AstNode_block)
          {
            List* nodes = new_list(arena, List_ast_node);
            append_list_elem(nodes, else_body, List_ast_node);
            else_body = new_ast_node(Ast_gen0, AstNode_block, else_body->src_loc);
            ATTR(else_body, list, nodes) = nodes;
          }

          begin_scope(Scope_block, if_stmt_gen1);
          success = name_ident_block(else_body);
          end_scope();

          if(success)
          {
            ATTR(if_stmt_gen1, ast_node, else_body) = else_body;
          }
        }
      }
    }
  }
  else if(node->kind == AstNode_while_stmt)
  {
    AstNode while_stmt_gen0 = *node;
    AstNode* while_stmt_gen1 = make_ast_node(Ast_gen1, node, AstNode_while_stmt);

    AstNode* cond_expr = ATTR(&while_stmt_gen0, ast_node, cond_expr);
    if(success = name_ident(cond_expr))
    {
      ATTR(while_stmt_gen1, ast_node, cond_expr) = cond_expr;

      AstNode* body = ATTR(&while_stmt_gen0, ast_node, body);
      if(body->kind != AstNode_block)
      {
        List* nodes = new_list(arena, List_ast_node);
        append_list_elem(nodes, body, List_ast_node);
        body = new_ast_node(Ast_gen0, AstNode_block, body->src_loc);
        ATTR(body, list, nodes) = nodes;
      }

      begin_scope(Scope_loop, while_stmt_gen1);
      success = name_ident_block(body);
      end_scope();

      if(success)
      {
        ATTR(while_stmt_gen1, ast_node, body) = body;
      }
    }
  }
  else if(node->kind == AstNode_break_stmt || node->kind == AstNode_continue_stmt)
  {
    make_ast_node(Ast_gen1, node, node->kind);

    Scope* loop_scope = find_scope(Scope_loop);
    if(loop_scope)
    {
      ATTR(node, ast_node, loop) = loop_scope->ast_node;
    }
    else
    {
      char* keyword = "???";
      if(node->kind == AstNode_break_stmt)
      {
        keyword = "break";
      }
      else if(node->kind == AstNode_continue_stmt)
      {
        keyword = "continue";
      }
      else
        assert(0);
      success = compile_error(node->src_loc, "unexpected `%s` at this location", keyword);
    }
  }
  else if(node->kind == AstNode_empty)
  {
    make_ast_node(Ast_gen1, node, node->kind);
  }
  else if(node->kind == AstNode_ret_stmt)
  {
    AstNode ret_stmt_gen0 = *node;
    AstNode* ret_stmt_gen1 = make_ast_node(Ast_gen1, node, AstNode_ret_stmt);

    Scope* scope = find_scope(Scope_proc);
    if(scope)
    {
      AstNode* proc = ATTR(ret_stmt_gen1, ast_node, proc_decl) = scope->ast_node;

      AstNode* ret_expr = ATTR(ret_stmt_gen1, ast_node, ret_expr) = ATTR(&ret_stmt_gen0, ast_node, ret_expr);
      if(ret_expr)
      {
        AstNode* ret_var = ATTR(proc, ast_node, ret_var);
        AstNode* occur_id = new_ast_node(Ast_gen0, AstNode_id, ret_var->src_loc);
        ATTR(occur_id, str_val, name) = ATTR(ret_var, str_val, name);

        AstNode* assign = new_ast_node(Ast_gen0, AstNode_bin_expr, ret_expr->src_loc);
        ATTR(assign, op_kind, op_kind) = Operator_assign;
        ATTR(assign, ast_node, left_operand) = occur_id;
        ATTR(assign, ast_node, right_operand) = ret_expr;

        ret_expr = new_ast_node(Ast_gen0, AstNode_stmt, assign->src_loc);
        ATTR(ret_expr, ast_node, stmt) = assign;
        ATTR(ret_stmt_gen1, ast_node, ret_expr) = ret_expr;

        success = name_ident(ret_expr);
      }
    }
    else
      success = compile_error(ret_stmt_gen1->src_loc, "unexpected `return` at this location");
  }
  else
    assert(0);

  return success;
}

bool build_types(AstNode* node)
{
  assert(node->gen == Ast_gen1);
  bool success = true;

  if(node->kind == AstNode_module)
  {
    AstNode* body = ATTR(node, ast_node, body);
    build_types(body);
    ATTR(node, type, type) = ATTR(body, type, type);
    ATTR(node, type, eval_type) = ATTR(body, type, eval_type);
  }
  else if(node->kind == AstNode_block)
  {
    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item;
        list_item = list_item->next)
    {
      build_types(ITEM(list_item, ast_node));
    }
    ATTR(node, type, type) = new_proc_type(basic_type_void, basic_type_void);
    ATTR(node, type, eval_type) = basic_type_void;
  }
  else if(node->kind == AstNode_stmt)
  {
    AstNode* actual_stmt = ATTR(node, ast_node, stmt);
    if(actual_stmt)
    {
      build_types(actual_stmt);
      ATTR(node, type, type) = ATTR(actual_stmt, type, eval_type);//new_proc_type(ATTR(actual_stmt, type, eval_type), basic_type_void);
      ATTR(node, type, eval_type) = basic_type_void;
    }
  }
  else if(node->kind == AstNode_var_decl)
  {
    AstNode* type = ATTR(node, ast_node, type);
    build_types(type);
    ATTR(node, type, type) = ATTR(type, type, type);
    ATTR(node, type, eval_type) = basic_type_void;

    AstNode* init_expr = ATTR(node, ast_node, init_expr);
    if(init_expr)
    {
      build_types(init_expr);
    }
  }
  else if(node->kind == AstNode_var_occur)
  {
    AstNode* var_decl = ATTR(node, ast_node, var_decl);
    ATTR(node, type, eval_type) = ATTR(node, type, type) = ATTR(var_decl, type, type);
  }
  else if(node->kind == AstNode_bin_expr)
  {
    AstNode* left_operand = ATTR(node, ast_node, left_operand);
    build_types(left_operand);
    AstNode* right_operand = ATTR(node, ast_node, right_operand);
    build_types(right_operand);

    Type* eval_type =  new_typevar();
    ATTR(node, type, type) = new_proc_type(new_product_type(ATTR(left_operand, type, eval_type),
          ATTR(right_operand, type, eval_type)), eval_type);
    ATTR(node, type, eval_type) = eval_type;
  }
  else if(node->kind == AstNode_un_expr)
  {
    AstNode* operand = ATTR(node, ast_node, operand);
    build_types(operand);

    Type* eval_type = new_typevar();
    ATTR(node, type, type) = new_proc_type(ATTR(operand, type, eval_type), eval_type);
    ATTR(node, type, eval_type) = eval_type;
  }
  else if(node->kind == AstNode_proc_decl)
  {
    List* args = ATTR(node, list, formal_args);
    for(ListItem* list_item = args->first;
        list_item;
        list_item = list_item->next)
    {
      build_types(ITEM(list_item, ast_node));
    }

    Type* args_type = basic_type_void;
    ListItem* list_item = args->first;
    if(list_item)
    {
      AstNode* arg = ITEM(list_item, ast_node);
      args_type = ATTR(arg, type, type);

      for(list_item = list_item->next;
          list_item;
          list_item = list_item->next)
      {
        AstNode* arg = ITEM(list_item, ast_node);
        args_type = new_product_type(args_type, ATTR(arg, type, type));
      }
    }

    AstNode* ret_var = ATTR(node, ast_node, ret_var);
    build_types(ret_var);
    Type* ret_type = ATTR(ret_var, type, type);

    ATTR(node, type, type) = new_proc_type(args_type, ret_type);
    ATTR(node, type, eval_type) = basic_type_void;

    build_types(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_ret_stmt)
  {
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    if(ret_expr)
    {
      build_types(ret_expr);
    }
    ATTR(node, type, type) = ATTR(node, type, eval_type) = basic_type_void;
  }
  else if(node->kind == AstNode_proc_occur)
  {
    List* args = ATTR(node, list, actual_args);
    for(ListItem* list_item = args->first;
        list_item;
        list_item = list_item->next)
    {
      build_types(ITEM(list_item, ast_node));
    }

    Type* args_type = basic_type_void;
    ListItem* list_item = ATTR(node, list, actual_args)->first;
    if(list_item)
    {
      AstNode* arg = ITEM(list_item, ast_node);
      args_type = ATTR(arg, type, eval_type);

      for(list_item = list_item->next;
          list_item;
          list_item = list_item->next)
      {
        AstNode* arg = ITEM(list_item, ast_node);
        args_type = new_product_type(args_type, ATTR(arg, type, eval_type));
      }
    }

    Type* ret_type = new_typevar();
    AstNode* proc_decl = ATTR(node, ast_node, proc_decl);
    if(proc_decl)
    {
      ret_type = ATTR(proc_decl, type, type)->proc.ret;
    }

    ATTR(node, type, type) = new_proc_type(args_type, ret_type);
    ATTR(node, type, eval_type) = ret_type;
  }
  else if(node->kind == AstNode_while_stmt)
  {
    build_types(ATTR(node, ast_node, cond_expr));

    AstNode* body = ATTR(node, ast_node, body);
    build_types(body);
    ATTR(node, type, type) = ATTR(body, type, type);
    ATTR(node, type, eval_type) = ATTR(body, type, eval_type);
  }
  else if(node->kind == AstNode_if_stmt)
  {
    build_types(ATTR(node, ast_node, cond_expr));

    AstNode* body = ATTR(node, ast_node, body);
    build_types(body);
    ATTR(node, type, type) = ATTR(body, type, type);
    ATTR(node, type, eval_type) = ATTR(body, type, eval_type);

    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      build_types(ATTR(node, ast_node, else_body));
    }
  }
  else if(node->kind == AstNode_lit)
  {
    LiteralKind lit_kind = ATTR(node, lit_kind, lit_kind);

    Type* type = 0;
    if(lit_kind == Literal_int_val)
    {
      type = basic_type_int;
    }
    else if(lit_kind == Literal_float_val)
    {
      type = basic_type_float;
    }
    else if(lit_kind == Literal_char_val)
    {
      type = basic_type_char;
    }
    else if(lit_kind == Literal_bool_val)
    {
      type = basic_type_bool;
    }
    else if(lit_kind == Literal_str_val)
    {
      int size_val = cstr_len(ATTR(node, str_val, str_val)) + 1; // +NULL
      type = new_array_type(size_val, basic_type_char);
    }
    else
      assert(0);

    ATTR(node, type, type) = ATTR(node, type, eval_type) = type;
  }
  else if(node->kind == AstNode_type_decl)
  {
    AstNode* type_expr = ATTR(node, ast_node, type_expr);
    build_types(type_expr);
    ATTR(node, type, type) = ATTR(node, type, eval_type) = ATTR(type_expr, type, type);
  }
  else if(node->kind == AstNode_type_occur)
  {
    AstNode* type_decl = ATTR(node, ast_node, type_decl);
    ATTR(node, type, type) = ATTR(type_decl, type, type);
    ATTR(node, type, eval_type) = ATTR(type_decl, type, eval_type);
  }
  else if(node->kind == AstNode_pointer)
  {
    AstNode* pointee_expr = ATTR(node, ast_node, pointee_expr);
    build_types(pointee_expr);
    ATTR(node, type, type) = new_pointer_type(ATTR(pointee_expr, type, type));
  }
  else if(node->kind == AstNode_array)
  {
    int size_val = 0;
    AstNode* size_expr = ATTR(node, ast_node, size_expr);
    if(size_expr)
    {
      if(size_expr->kind == AstNode_lit && ATTR(size_expr, lit_kind, lit_kind) == Literal_int_val)
      {
        size_val = ATTR(size_expr, int_val, int_val);
      }
      else
        success = compile_error(size_expr->src_loc, "array size must be an int literal");
    }
    AstNode* elem_expr = ATTR(node, ast_node, elem_expr);
    build_types(elem_expr);
    ATTR(node, type, type) = ATTR(node, type, eval_type) = new_array_type(size_val, ATTR(elem_expr, type, type));
  }
  else if(node->kind == AstNode_break_stmt || node->kind == AstNode_continue_stmt)
  {
    ATTR(node, type, type) = ATTR(node, type, eval_type) = basic_type_void;
  }
  else if(node->kind == AstNode_empty)
  {
    ATTR(node, type, type) = ATTR(node, type, eval_type) = basic_type_void;
  }
  else
    assert(0);

  return success;
}

bool eval_types(AstNode* node)
{
  assert(node->gen == Ast_gen1);
  bool success = true;

  if(node->kind == AstNode_module)
  {
    success = eval_types(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_block)
  {
    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item && success;
        list_item = list_item->next)
    {
      AstNode* stmt = ITEM(list_item, ast_node);
      if(success = eval_types(stmt))
      {
        Type* stmt_ty = ATTR(stmt, type, eval_type);
        Type* block_ty = ATTR(node, type, eval_type);
        success = type_unif(stmt_ty, block_ty);
        if(!success)
        {
          compile_error(stmt->src_loc, "type error (block stmt)");
        }
      }
    }
  }
  else if(node->kind == AstNode_stmt)
  {
    AstNode* actual_stmt = ATTR(node, ast_node, stmt);
    if(actual_stmt)
    {
      success = eval_types(actual_stmt);
    }
  }
  else if(node->kind == AstNode_var_decl)
  {
    AstNode* init_expr = ATTR(node, ast_node, init_expr);
    if(init_expr)
    {
      if(success = eval_types(init_expr))
      {
        Type* var_ty = ATTR(node, type, type);
        Type* init_expr_ty = ATTR(init_expr, type, type);
        success = type_unif(var_ty, init_expr_ty);
        if(!success)
        {
          compile_error(node->src_loc, "type error (var init expr)");
        }
      }
    }
  }
  else if(node->kind == AstNode_var_occur)
  {
    ;//skip
  }
  else if(node->kind == AstNode_bin_expr)
  {
    AstNode* right_operand = ATTR(node, ast_node, right_operand);
    AstNode* left_operand = ATTR(node, ast_node, left_operand);

    if(success = eval_types(right_operand) && eval_types(left_operand))
    {
      Type* expr_ty = ATTR(node, type, eval_type);
      Type* right_operand_ty = ATTR(right_operand, type, eval_type);
      Type* left_operand_ty = ATTR(left_operand, type, eval_type);

      OperatorKind op_kind = ATTR(node, op_kind, op_kind);
      if(op_kind == Operator_cast)
      {
        success = type_unif(expr_ty, left_operand_ty);
        if(!success)
        {
          compile_error(node->src_loc, "type error (cast)");
        }
      }
      else if(op_kind == Operator_index)
      {
        if(success = type_unif(right_operand_ty, basic_type_int))
        {
          if(left_operand_ty->kind == Type_array)
          {
            success = type_unif(left_operand_ty->array.elem, expr_ty);
          }
          else if(left_operand_ty->kind == Type_typevar)
          {
            success = type_unif(left_operand_ty, new_array_type(0, expr_ty));
          }
          else
            success = false;

          if(!success)
          {
            compile_error(node->src_loc, "type error (array index)");
          }
        }
        else
          compile_error(node->src_loc, "int type expected");
      }
      else
      {
        if(success = type_unif(left_operand_ty, right_operand_ty))
        {
          if(is_relational_operator(op_kind) || is_logical_operator(op_kind))
          {
            success = type_unif(expr_ty, basic_type_bool);
            if(!success)
            {
              compile_error(node->src_loc, "type error (bin expr)");
            }
          }
          else
          {
            success = type_unif(expr_ty, left_operand_ty);
            if(!success)
            {
              compile_error(node->src_loc, "type error (bin expr)");
            }
          }
        }
        else
          compile_error(node->src_loc, "type error (bin expr)");
      }

      if(success)
      {
        Type* type = ATTR(node, type, type);
        assert(type->kind == Type_proc);
        success = type_unif(type->proc.ret, expr_ty);
        if(!success)
        {
          compile_error(node->src_loc, "type error (bin expr)");
        }
      }
    }
  }
  else if(node->kind == AstNode_un_expr)
  {
    AstNode* operand = ATTR(node, ast_node, operand);

    Type* expr_ty = ATTR(node, type, eval_type) = new_typevar();
    if(success = eval_types(operand))
    {
      Type* operand_ty = ATTR(operand, type, eval_type);

      OperatorKind op_kind = ATTR(node, op_kind, op_kind);
      if(op_kind == Operator_neg || op_kind == Operator_logic_not)
      {
        success = type_unif(expr_ty, operand_ty);
        if(!success)
        {
          compile_error(node->src_loc, "type error (un expr)");
        }
      }
      else if(op_kind == Operator_deref)
      {
        Type* pointee_ty = new_typevar();
        if(success = type_unif(operand_ty, new_pointer_type(pointee_ty)))
        {
          success = type_unif(expr_ty, pointee_ty);
          if(!success)
          {
            compile_error(node->src_loc, "type error (un expr)");
          }
        }
        else
          success = compile_error(operand->src_loc, "pointer type expected");
      }
      else if(op_kind == Operator_address_of)
      {
        if(operand_ty->kind == Type_array)
        {
          // ptr(T) == ptr(array(T))
          success = type_unif(expr_ty, new_pointer_type(operand_ty->array.elem));
        }
        else
        {
          success = type_unif(expr_ty, new_pointer_type(operand_ty));
        }
        if(!success)
        {
          compile_error(node->src_loc, "type error (un expr)");
        }
      }
      else
        assert(0);
    }

    if(success)
    {
      Type* type = ATTR(node, type, type);
      assert(type->kind == Type_proc);
      success = type_unif(type->proc.ret, expr_ty);
      if(!success)
      {
        compile_error(node->src_loc, "type error (un expr)");
      }
    }
  }
  else if(node->kind == AstNode_proc_decl)
  {
    success = eval_types(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_proc_occur)
  {
    for(ListItem* list_item = ATTR(node, list, actual_args)->first;
        list_item && success;
        list_item = list_item->next)
    {
      success = eval_types(ITEM(list_item, ast_node));
    }

    if(success)
    {
      AstNode* proc_decl = ATTR(node, ast_node, proc_decl);
      if(!proc_decl)
      {
        Symbol* occur_sym = ATTR(node, symbol, occur_sym);
        Symbol* decl_sym = lookup_decl_symbol(occur_sym->name, occur_sym->scope,
            (SymbolKind[]){Symbol_proc, Symbol_None});
        if(decl_sym)
        {
          proc_decl = ATTR(node, ast_node, proc_decl) = decl_sym->ast_node;
        }
        else
          success = compile_error(occur_sym->src_loc, "unknown proc `%s`", occur_sym->name);
      }

      if(success)
      {
        Type* args_ty = basic_type_void;
        ListItem* list_item = ATTR(node, list, actual_args)->first;
        if(list_item)
        {
          AstNode* arg = ITEM(list_item, ast_node);
          args_ty = ATTR(arg, type, eval_type);

          for(list_item = list_item->next;
              list_item;
              list_item = list_item->next)
          {
            AstNode* arg = ITEM(list_item, ast_node);
            args_ty = new_product_type(args_ty, ATTR(arg, type, eval_type));
          }
        }

        Type* ret_ty = ATTR(proc_decl, type, type)->proc.ret;
        Type* occur_ty = ATTR(node, type, type); assert(occur_ty->kind == Type_proc);
        Type* decl_ty = ATTR(proc_decl, type, type); assert(decl_ty->kind == Type_proc);
        if(success = type_unif(occur_ty->proc.args, args_ty) && type_unif(occur_ty->proc.ret, ret_ty))
        {
          success = type_unif(decl_ty, occur_ty);
          if(!success)
          {
            compile_error(node->src_loc, "type error (proc occur)");
          }
        }
        else
          compile_error(node->src_loc, "type error (proc occur)");
      }
    }
  }
  else if(node->kind == AstNode_ret_stmt)
  {
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    Type* ret_expr_ty = basic_type_void;
    if(ret_expr)
    {
      if(success = eval_types(ret_expr))
      {
        ret_expr_ty = ATTR(ret_expr, type, type);
      }
    }

    if(success)
    {
      AstNode* proc = ATTR(node, ast_node, proc_decl);
      Type* proc_ret_ty = ATTR(proc, type, type)->proc.ret;
      success = type_unif(ret_expr_ty, proc_ret_ty);
      if(!success)
      {
        compile_error(node->src_loc, "type error (return stmt)");
      }
    }
  }
  else if(node->kind == AstNode_while_stmt)
  {
    AstNode* cond_expr = ATTR(node, ast_node, cond_expr);
    if(success = eval_types(cond_expr) && eval_types(ATTR(node, ast_node, body)))
    {
      Type* cond_ty = ATTR(cond_expr, type, eval_type);
      success = type_unif(cond_ty, basic_type_bool);
      if(!success)
      {
        compile_error(cond_expr->src_loc, "bool type expected");
      }
    }
  }
  else if(node->kind == AstNode_if_stmt)
  {
    AstNode* body = ATTR(node, ast_node, body);
    AstNode* else_body = ATTR(node, ast_node, else_body);

    if(success = eval_types(body))
    {
      if(else_body)
      {
        success = eval_types(else_body);
      }
    }

    AstNode* cond_expr = ATTR(node, ast_node, cond_expr);
    if(success && (success = eval_types(cond_expr)))
    {
      Type* cond_ty = ATTR(cond_expr, type, eval_type);
      success = type_unif(cond_ty, basic_type_bool);
      if(!success)
      {
        compile_error(cond_expr->src_loc, "bool type expected");
      }
    }
  }
  else if(node->kind == AstNode_lit)
  {
    ;//skip
  }
  else if(node->kind == AstNode_type_decl)
  {
    ;//skip
  }
  else if(node->kind == AstNode_break_stmt || node->kind == AstNode_continue_stmt)
  {
    ; // skip
  }
  else if(node->kind == AstNode_empty)
  {
    ; // skip
  }
  else
    assert(0);
  return success;
}

bool node_resolve_type(AstNode* node)
{
  bool success = true;

  Type* type = ATTR(node, type, type);
  if(success = resolve_type(type, &type))
  {
    compute_type_width(type);
    ATTR(node, type, type) = type;

    type = ATTR(node, type, eval_type);
    if(success = resolve_type(type, &type))
    {
      compute_type_width(type);
      ATTR(node, type, eval_type) = type;
    }
    else
      assert(0);
  }
  else
    assert(0);

  return success;
}

bool resolve_types(AstNode* node)
{
  bool success = true;

  if(node->kind == AstNode_module)
  {
    success = resolve_types(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_block)
  {
    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item && success;
        list_item = list_item->next)
    {
      success = resolve_types(ITEM(list_item, ast_node));
    }
  }
  else if(node->kind == AstNode_stmt)
  {
    AstNode* actual_stmt = ATTR(node, ast_node, stmt);
    if(actual_stmt)
    {
      success = resolve_types(actual_stmt);
    }
  }
  else if(node->kind == AstNode_var_decl)
  {
    if(success = node_resolve_type(node))
    {
      ATTR(node, symbol, decl_sym)->type = ATTR(node, type, type);

      AstNode* init_expr = ATTR(node, ast_node, init_expr);
      if(init_expr)
      {
        success = resolve_types(init_expr);
      }
    }
  }
  else if(node->kind == AstNode_var_occur)
  {
    if(success = node_resolve_type(node))
    {
      ATTR(node, symbol, occur_sym)->type = ATTR(node, type, type);
    }
  }
  else if(node->kind == AstNode_bin_expr)
  {
    success = resolve_types(ATTR(node, ast_node, left_operand))
      && resolve_types(ATTR(node, ast_node, right_operand))
      && node_resolve_type(node);
  }
  else if(node->kind == AstNode_un_expr)
  {
    success = resolve_types(ATTR(node, ast_node, operand))
      && node_resolve_type(node);
  }
  else if(node->kind == AstNode_proc_decl)
  {
    for(ListItem* list_item = ATTR(node, list, formal_args)->first;
        list_item && success;
        list_item = list_item->next)
    {
      success = resolve_types(ITEM(list_item, ast_node));
    }
    if(success && (success = node_resolve_type(node)))
    {
      ATTR(node, symbol, decl_sym)->type = ATTR(node, type, type);

      AstNode* ret_var = ATTR(node, ast_node, ret_var);
      if(success = node_resolve_type(ret_var))
      {
        ATTR(ret_var, symbol, decl_sym)->type = ATTR(ret_var, type, type);

        AstNode* body = ATTR(node, ast_node, body);
        success = resolve_types(body) && node_resolve_type(body);
      }
    }
  }
  else if(node->kind == AstNode_proc_occur)
  {
    for(ListItem* list_item = ATTR(node, list, actual_args)->first;
        list_item && success;
        list_item = list_item->next)
    {
      success = resolve_types(ITEM(list_item, ast_node));
    }
    if(success && (success = node_resolve_type(node)))
    {
      ATTR(node, symbol, occur_sym)->type = ATTR(node, type, type);
    }
  }
  else if(node->kind == AstNode_ret_stmt)
  {
    if(success = node_resolve_type(node))
    {
      AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
      if(ret_expr)
      {
        success = resolve_types(ret_expr);
      }
    }
  }
  else if(node->kind == AstNode_while_stmt)
  {
    success = resolve_types(ATTR(node, ast_node, cond_expr))
      && resolve_types(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_if_stmt)
  {
    success = resolve_types(ATTR(node, ast_node, cond_expr))
      && resolve_types(ATTR(node, ast_node, body));
    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      success = resolve_types(else_body);
    }
  }
  else if(node->kind == AstNode_lit)
  {
    success = node_resolve_type(node);
  }
  else if(node->kind == AstNode_type_decl)
  {
    if(success = node_resolve_type(node))
    {
      ATTR(node, symbol, decl_sym)->type = ATTR(node, type, type);
    }
  }
  else if(node->kind == AstNode_break_stmt || node->kind == AstNode_continue_stmt)
  {
    ; // skip
  }
  else if(node->kind == AstNode_empty)
  {
    ; // skip
  }
  else
    assert(0);
  return success;
}

bool check_types(AstNode* node)
{
  assert(node->gen == Ast_gen1);
  bool success = true;

  if(node->kind == AstNode_module)
  {
    success = check_types(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_block)
  {
    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item && success;
        list_item = list_item->next)
    {
      success = check_types(ITEM(list_item, ast_node));
    }
  }
  else if(node->kind == AstNode_stmt)
  {
    AstNode* actual_stmt = ATTR(node, ast_node, stmt);
    if(actual_stmt)
    {
      success = check_types(actual_stmt);
    }
  }
  else if(node->kind == AstNode_var_decl)
  {
    Type* var_ty = ATTR(node, type, type);
    if(var_ty == basic_type_void)
    {
      success = compile_error(node->src_loc, "variable type cannot be `void`");
    }
    else
    {
      AstNode* init_expr = ATTR(node, ast_node, init_expr);
      if(init_expr)
      {
        success = check_types(init_expr);
      }
    }
  }
  else if(node->kind == AstNode_var_occur)
  {
    ;//ok
  }
  else if(node->kind == AstNode_bin_expr)
  {
    AstNode* right_operand = ATTR(node, ast_node, right_operand);
    AstNode* left_operand = ATTR(node, ast_node, left_operand);

    if(success = check_types(right_operand) && check_types(left_operand))
    {
      Type* expr_ty = ATTR(node, type, type); assert(expr_ty->kind == Type_proc);
      Type* args_ty = expr_ty->proc.args; assert(args_ty->kind == Type_product);

      Type* ret_ty = expr_ty->proc.ret;
      Type* left_arg_ty = args_ty->product.left;
      Type* right_arg_ty = args_ty->product.right;

      OperatorKind op_kind = ATTR(node, op_kind, op_kind);
      if(is_arithmetic_operator(op_kind))
      {
        if(types_are_equal(ret_ty, basic_type_int)
            || types_are_equal(ret_ty, basic_type_float)
            || (types_are_equal(ret_ty, basic_type_char))
            || (ret_ty->kind == Type_pointer))
        {
          ;//ok
          assert(types_are_equal(ret_ty, left_arg_ty) && types_are_equal(left_arg_ty, right_arg_ty));
        }
        else
        {
          success = compile_error(node->src_loc, "type error: `%s` cannot be applied to `%s` operands",
              get_operator_kind_printstr(op_kind), get_type_printstr(args_ty));
        }
      }
      else if(op_kind == Operator_mod)
      {
        if(types_are_equal(ret_ty, basic_type_int))
        {
          ;//ok
          assert(types_are_equal(ret_ty, left_arg_ty) && types_are_equal(left_arg_ty, right_arg_ty));
        }
        else
        {
          success = compile_error(node->src_loc, "type error: `%s` cannot be applied to `%s` operands",
              get_operator_kind_printstr(op_kind), get_type_printstr(args_ty));
        }
      }
      else if(is_logical_operator(op_kind))
      {
        if(types_are_equal(left_arg_ty, basic_type_bool) && types_are_equal(left_arg_ty, right_arg_ty))
        {
          ;//ok
          assert(ret_ty == basic_type_bool);
        }
        else
          success = compile_error(node->src_loc, "type error: `%s` cannot be applied to `%s` operands",
              get_operator_kind_printstr(op_kind), get_type_printstr(args_ty));
      }
      else if(is_relational_operator(op_kind))
      {
        if(types_are_equal(left_arg_ty, basic_type_int)
            || types_are_equal(left_arg_ty, basic_type_char)
            || types_are_equal(left_arg_ty, basic_type_float)
            && types_are_equal(left_arg_ty, right_arg_ty))
        {
          ;//ok
          assert(types_are_equal(ret_ty, basic_type_bool));
        }
        else
        {
          success = compile_error(node->src_loc, "type error: `%s` cannot be applied to `%s` operands",
              get_operator_kind_printstr(op_kind), get_type_printstr(args_ty));
        }
      }
      else if(op_kind == Operator_assign)
      {
        ;//ok
        assert(types_are_equal(left_arg_ty, right_arg_ty) && types_are_equal(ret_ty, left_arg_ty));
      }
      else if(op_kind == Operator_index)
      {
        if(ret_ty->width > 0)
        {
          ;//ok
        }
        else
          success = compile_error(node->src_loc, "type error (array index): type size = 0");
      }
      else if(op_kind == Operator_cast)
      {
        if(!types_are_equal(left_arg_ty, right_arg_ty))
        {
          success = false;

          if(types_are_equal(left_arg_ty, basic_type_int))
          {
            // int <- float | bool | pointer | char
            success = types_are_equal(right_arg_ty, basic_type_float)
                || types_are_equal(right_arg_ty, basic_type_bool)
                || types_are_equal(right_arg_ty, basic_type_char)
                || (right_arg_ty->kind == Type_pointer);
          }
          else if(types_are_equal(left_arg_ty, basic_type_float))
          {
            // float <- int
            success = types_are_equal(right_arg_ty, basic_type_int);
          }
          else if(left_arg_ty->kind == Type_pointer)
          {
            // pointer <- pointer | array | int
            success = (right_arg_ty->kind == Type_pointer)
                || (right_arg_ty->kind == Type_array)
                || types_are_equal(right_arg_ty, basic_type_int);
          }
          else if(left_arg_ty->kind == Type_array)
          {
            // array <- pointer | array
            success = (right_arg_ty->kind == Type_pointer) || (right_arg_ty->kind == Type_array);
          }

          if(!success)
          {
            compile_error(node->src_loc, "type error: invalid cast `%s` -> `%s`",
                get_type_printstr(right_arg_ty), get_type_printstr(left_arg_ty));
          }
        }
      }
      else
        assert(0);
    }
  }
  else if(node->kind == AstNode_un_expr)
  {
    success = check_types(ATTR(node, ast_node, operand));
  }
  else if(node->kind == AstNode_proc_decl)
  {
    for(ListItem* list_item = ATTR(node, list, formal_args)->first;
        list_item && success;
        list_item = list_item->next)
    {
      success = check_types(ITEM(list_item, ast_node));
    }
    if(success)
    {
      success = check_types(ATTR(node, ast_node, body));
    }
  }
  else if(node->kind == AstNode_proc_occur)
  {
    for(ListItem* list_item = ATTR(node, list, actual_args)->first;
        list_item && success;
        list_item = list_item->next)
    {
      success = check_types(ITEM(list_item, ast_node));
    }
  }
  else if(node->kind == AstNode_ret_stmt)
  {
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    if(ret_expr)
    {
      success = check_types(ret_expr);
    }
  }
  else if(node->kind == AstNode_while_stmt)
  {
    success = check_types(ATTR(node, ast_node, cond_expr))
        && check_types(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_if_stmt)
  {
    success = check_types(ATTR(node, ast_node, cond_expr))
      && check_types(ATTR(node, ast_node, body));
    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      success = check_types(else_body);
    }
  }
  else if(node->kind == AstNode_lit)
  {
    ;//ok
  }
  else if(node->kind == AstNode_type_decl)
  {
    ;//ok
  }
  else if(node->kind == AstNode_break_stmt || node->kind == AstNode_continue_stmt)
  {
    ;//ok
  }
  else if(node->kind == AstNode_empty)
  {
    ;//ok
  }
  else
    assert(0);
  return success;
}

