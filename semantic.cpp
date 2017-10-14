char* get_type_printstr(Type* type)
{
  String* str = str_new(arena);
  make_type_printstr(str, type);
  return str_cap(str);
}

char* make_temp_name(char* label)
{
  String* str = str_new(arena);
  str_printf(str, "$%s%d", label, tempvar_id++);
  return str_cap(str);
}

bool is_relational_operator(OperatorKind op_kind)
{
  return op_kind == Operator_logic_and || op_kind == Operator_logic_or
          || op_kind == Operator_less || op_kind == Operator_less_eq
          || op_kind == Operator_greater || op_kind == Operator_greater_eq
          || op_kind == Operator_eq || op_kind == Operator_not_eq;
}

SymbolTable* new_symbol_table(MemoryArena** arena, int size)
{
  MemoryArena* symbol_arena = push_arena(arena, size);
  symbol_table = mem_push_struct(symbol_arena, SymbolTable);
  symbol_table->arena = symbol_arena;
  symbol_table->nesting_depth = -1;
  symbol_table->symbols = new_list(symbol_arena, List_symbol);
  return symbol_table;
}

typedef enum SymbolLookup
{
  SymbolLookup__None,
  SymbolLookup_active,
  SymbolLookup_module,
  SymbolLookup_global,
};

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

List* collect_symbols_in_scope(char* name, SymbolKind symbol_kind, Scope* scope)
{
  List* symbol_set = new_list(arena, List_symbol);
  Symbol* symbol = scope->last_symbol;
  while(symbol)
  {
    if(symbol->kind == symbol_kind && cstr_match(symbol->name, name))
    {
      append_list_elem(symbol_set, symbol, List_symbol);
    }
    symbol = symbol->prev_symbol;
  }
  return symbol_set;
}

Symbol* lookup_symbol_in_scope(char* name, SymbolKind symbol_kind, Scope* scope)
{
  Symbol* result = 0;
  while(!result && scope)
  {
    Symbol* symbol = scope->last_symbol;
    while(symbol)
    {
      if(symbol->kind == symbol_kind && cstr_match(symbol->name, name))
      {
        result = symbol;
        break;
      }
      symbol = symbol->prev_symbol;
    }
    scope = scope->encl_scope;
  }
  return result;
}

Symbol* lookup_symbol(char* name, SymbolKind symbol_kind, SymbolLookup lookup)
{
  Scope* scope = symbol_table->active_scope;
  if(lookup == SymbolLookup_module)
  {
    scope = find_scope(ScopeKind_module);
    if(!scope)
    {
      scope = find_scope(ScopeKind_global);
    }
  }
  else if(lookup == SymbolLookup_global)
  {
    scope = find_scope(ScopeKind_global);
  }
  else
    assert(lookup == SymbolLookup_active);

  Symbol* result = lookup_symbol_in_scope(name, symbol_kind, scope);
  return result;
}

Symbol* add_symbol(char* name, SourceLoc* src_loc, SymbolKind kind)
{
  Symbol* sym = mem_push_struct(symbol_table->arena, Symbol);
  sym->kind = kind;
  sym->name = name;
  sym->src_loc = src_loc;
  sym->scope = symbol_table->active_scope;
  sym->nesting_depth = symbol_table->nesting_depth;
  sym->prev_symbol = symbol_table->active_scope->last_symbol;
  symbol_table->active_scope->last_symbol = sym;
  append_list_elem(symbol_table->symbols, sym, List_symbol);
  return sym;
}

Type* make_type_of_type_expr(AstNode* node)
{
  assert(node->gen == Ast_gen1);
  Type* type = 0;

  if(node->kind == AstNode_type_occur)
  {
    Symbol* occur_sym = ATTR(node, symbol, occur_sym);
    Symbol* decl_sym = SYM(occur_sym, type_occur)->decl_sym;
    type = SYM(decl_sym, type_decl)->type;
  }
  else if(node->kind == AstNode_pointer)
  {
    Type* pointee = make_type_of_type_expr(ATTR(node, ast_node, type_expr));
    type = new_pointer_type(pointee);
  }
  else if(node->kind == AstNode_array)
  {
    int size = -1;
#if 0
    AstNode* size_node = ATTR(node, ast_node, size_expr);
    if(size_node)
    {
      if(size_node->kind == AstNode_lit && ATTR(size_node, lit_kind, lit_kind) == Literal_int_val)
      {
        size = ATTR(size_node, int_val, int_val);
      }
      else
        success = compile_error(size_node->src_loc, "array size must be an int literal");
    }
#endif

    Type* elem = make_type_of_type_expr(ATTR(node, ast_node, type_expr));
    type = new_array_type(size, elem);
  }
  else
    assert(0);

  return type;
}

void add_builtin_type(char* name, Type* type)
{
  assert(type->kind == Type_basic);
  Symbol* decl_sym = add_symbol(name, 0, Symbol_type_decl);
  SYM(decl_sym, type_decl)->type = type;
}

void add_builtin_types()
{
  add_builtin_type("bool", basic_type_bool);
  add_builtin_type("int", basic_type_int);
  add_builtin_type("char", basic_type_char);
  add_builtin_type("float", basic_type_float);
  add_builtin_type("void", basic_type_void);
  add_builtin_type("type", basic_type_type);
}

void add_builtin_proc(char* name, Type* type)
{
  assert(type->kind == Type_proc);
  AstNode* proc_decl = new_ast_node(Ast_gen1, AstNode_proc_decl, 0);
  ATTR(proc_decl, str, name) = name;
  ATTR(proc_decl, type, type) = type;
  ATTR(proc_decl, type, eval_type) = type->proc.ret;
  Symbol* decl_sym = add_symbol(name, 0, Symbol_proc_decl);
  decl_sym->type = type;
  decl_sym->ast_node = proc_decl;
}

void add_builtin_procs()
{
  add_builtin_proc("putc", new_proc_type(basic_type_char, basic_type_void));
}

void begin_scope(ScopeKind kind, AstNode* ast_node)
{
  Scope* scope = mem_push_struct(symbol_table->arena, Scope);
  scope->kind = kind;
  scope->nesting_depth = ++symbol_table->nesting_depth;
  scope->encl_scope = symbol_table->active_scope;
  scope->ast_node = ast_node;
  symbol_table->active_scope = scope;
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

bool name_resolve(AstNode* node);

bool name_resolve_type(AstNode* node)
{
  assert(node->gen == Ast_gen0);
  bool success = true;

  if(node->kind == AstNode_id)
  {
    AstNode id_gen0 = *node;
    AstNode* type_occur_gen1 = make_ast_node(Ast_gen1, node, AstNode_type_occur);
    char* name = ATTR(&id_gen0, str, name);
    ATTR(type_occur_gen1, str, name) = name;

    Symbol* decl_sym = lookup_symbol(name, Symbol_type_decl, SymbolLookup_active);
    if(decl_sym)
    {
      Symbol* occur_sym = add_symbol(name, type_occur_gen1->src_loc, Symbol_type_occur);
      SYM(occur_sym, type_occur)->decl_sym = decl_sym;

      ATTR(type_occur_gen1, symbol, occur_sym) = occur_sym;
    }
    else
      success = compile_error(type_occur_gen1->src_loc, "unknown type `%s`", name);
  }
  else if(node->kind == AstNode_pointer)
  {
    AstNode ptr_gen0 = *node;
    AstNode* ptr_gen1 = make_ast_node(Ast_gen1, node, AstNode_pointer);

    if(success = name_resolve_type(ATTR(&ptr_gen0, ast_node, type_expr)))
    {
      ATTR(ptr_gen1, ast_node, type_expr) = ATTR(&ptr_gen0, ast_node, type_expr);
    }
  }
  else if(node->kind == AstNode_array)
  {
    AstNode array_gen0 = *node;
    AstNode* array_gen1 = make_ast_node(Ast_gen1, node, AstNode_array);

    ATTR(array_gen1, ast_node, type_expr) = ATTR(&array_gen0, ast_node, type_expr);
    AstNode* size_expr = ATTR(array_gen1, ast_node, size_expr) = ATTR(&array_gen0, ast_node, size_expr);

    if(success = name_resolve_type(ATTR(&array_gen0, ast_node, type_expr)))
    {
      if(size_expr)
      {
        success = name_resolve(size_expr);
      }
    }
  }
  else
    assert(0);

  return success;
}

bool name_resolve_block(AstNode* node)
{
  assert(node->kind == AstNode_block);
  bool success = true;
  AstNode block_copy = *node;
  AstNode* block = make_ast_node(Ast_gen1, node, AstNode_block);
  ATTR(block, scope, scope) = symbol_table->active_scope;

  for(ListItem* list_item = ATTR(&block_copy, list, nodes)->first;
      list_item && success;
      list_item = list_item->next)
  {
    success = name_resolve(ITEM(list_item, ast_node));
  }

  if(success)
  {
    ATTR(block, list, nodes) = ATTR(&block_copy, list, nodes);
  }
  return success;
}

bool name_resolve(AstNode* node)
{
  assert(node->gen == Ast_gen0);
  bool success = true;

  if(node->kind == AstNode_module)
  {
    AstNode module_copy = *node;
    AstNode* module = make_ast_node(Ast_gen1, node, AstNode_module);
    ATTR(module, str, file_path) = ATTR(&module_copy, str, file_path);

    AstNode* body = ATTR(&module_copy, ast_node, body);

    begin_scope(ScopeKind_module, module);
    ATTR(module, ast_node, body) = body;
    success = name_resolve_block(body);
    end_scope();
  }
  else if(node->kind == AstNode_var_decl)
  {
    AstNode var_decl_gen0 = *node;
    AstNode* var_decl_gen1 = make_ast_node(Ast_gen1, node, AstNode_var_decl);
    char* name = ATTR(ATTR(&var_decl_gen0, ast_node, id), str, name);
    ATTR(var_decl_gen1, str, name) = name;

    Symbol* decl_sym = lookup_symbol(name, Symbol_var_decl, SymbolLookup_active);
    if(decl_sym && (decl_sym->scope == symbol_table->active_scope))
    {
      success = compile_error(var_decl_gen1->src_loc, "variable `%s` already declared", name);
      compile_error(decl_sym->src_loc, "see previous declaration of `%s`", name);
    }
    else
    {
      decl_sym = add_symbol(name, var_decl_gen1->src_loc, Symbol_var_decl);
      ATTR(var_decl_gen1, symbol, decl_sym) = decl_sym;
      decl_sym->ast_node = var_decl_gen1;

      ATTR(var_decl_gen1, ast_node, type) = ATTR(&var_decl_gen0, ast_node, type);
      success = name_resolve(ATTR(&var_decl_gen0, ast_node, type));

      AstNode* init_expr = ATTR(var_decl_gen1, ast_node, init_expr) = ATTR(&var_decl_gen0, ast_node, init_expr);
      if(success && init_expr)
      {
        success = name_resolve(init_expr);
      }
    }
  }
  else if(node->kind == AstNode_id)
  {
    AstNode id_gen0 = *node;
    AstNode* var_occur_gen1 = make_ast_node(Ast_gen1, node, AstNode_var_occur);
    char* name = ATTR(&id_gen0, str, name);
    ATTR(var_occur_gen1, str, name) = name;

    Symbol* occur_sym = add_symbol(name, var_occur_gen1->src_loc, Symbol_var_occur);
    occur_sym->ast_node = var_occur_gen1;

    Symbol* decl_sym = lookup_symbol(name, Symbol_var_decl, SymbolLookup_active);
    if(decl_sym)
    {
      SYM(occur_sym, var_occur)->decl_sym = decl_sym;
      ATTR(var_occur_gen1, symbol, occur_sym) = occur_sym;
      ATTR(var_occur_gen1, ast_node, var_decl) = decl_sym->ast_node;
    }
    else
      success = compile_error(var_occur_gen1->src_loc, "unknown var `%s`", name);
  }
  else if(node->kind == AstNode_proc_decl)
  {
    AstNode proc_decl_gen0 = *node;
    AstNode* proc_decl_gen1 = make_ast_node(Ast_gen1, node, AstNode_proc_decl);
    char* name = ATTR(ATTR(&proc_decl_gen0, ast_node, id), str, name);
    ATTR(proc_decl_gen1, str, name) = name;

    Symbol* decl_sym = lookup_symbol(name, Symbol_proc_decl, SymbolLookup_module);
    if(decl_sym && (decl_sym->scope == symbol_table->active_scope))
    {
      success = compile_error(proc_decl_gen1->src_loc, "proc `%s` already declared", name);
      compile_error(decl_sym->src_loc, "see previous declaration of `%s`", name);
    }
    else
    {
      decl_sym = add_symbol(name, proc_decl_gen1->src_loc, Symbol_proc_decl);
      decl_sym->ast_node = proc_decl_gen1;

      ATTR(proc_decl_gen1, symbol, decl_sym) = decl_sym;
      ATTR(proc_decl_gen1, list, formal_args) = ATTR(&proc_decl_gen0, list, formal_args);
      ATTR(proc_decl_gen1, ast_node, ret_type) = ATTR(&proc_decl_gen0, ast_node, ret_type);
      ATTR(proc_decl_gen1, ast_node, body) = ATTR(&proc_decl_gen0, ast_node, body);

      begin_scope(ScopeKind_proc, proc_decl_gen1);

      for(ListItem* list_item = ATTR(&proc_decl_gen0, list, formal_args)->first;
          list_item && success;
          list_item = list_item->next)
      {
        success = name_resolve(ITEM(list_item, ast_node));
      }

      if(success)
      {
        success = name_resolve(ATTR(&proc_decl_gen0, ast_node, ret_type)) &&
          name_resolve(ATTR(&proc_decl_gen0, ast_node, body));
      }
      end_scope();
    }
  }
  else if(node->kind == AstNode_proc_occur)
  {
    AstNode proc_occur_gen0 = *node;
    AstNode* proc_occur_gen1 = make_ast_node(Ast_gen1, node, AstNode_proc_occur);
    char* name = ATTR(ATTR(&proc_occur_gen0, ast_node, id), str, name);
    ATTR(proc_occur_gen1, str, name) = name;

    Symbol* occur_sym = add_symbol(name, proc_occur_gen1->src_loc, Symbol_proc_occur);
    occur_sym->ast_node = proc_occur_gen1;
    ATTR(proc_occur_gen1, symbol, occur_sym) = occur_sym;

    Symbol* decl_sym = lookup_symbol(name, Symbol_proc_decl, SymbolLookup_active);
    if(decl_sym)
    {
      SYM(occur_sym, proc_occur)->decl_sym = decl_sym;
      ATTR(proc_occur_gen1, symbol, occur_sym) = occur_sym;
      ATTR(proc_occur_gen1, ast_node, proc_decl) = decl_sym->ast_node;
    }

    ATTR(proc_occur_gen1, list, actual_args) = ATTR(&proc_occur_gen0, list, actual_args);
    for(ListItem* list_item = ATTR(&proc_occur_gen0, list, actual_args)->first;
        list_item && success;
        list_item = list_item->next)
    {
      success = name_resolve(ITEM(list_item, ast_node));
    }
  }
  else if(node->kind == AstNode_bin_expr)
  {
    AstNode bin_expr_gen0 = *node;
    AstNode* bin_expr_gen1 = make_ast_node(Ast_gen1, node, AstNode_bin_expr);
    ATTR(bin_expr_gen1, op_kind, op_kind) = ATTR(&bin_expr_gen0 , op_kind, op_kind);

    AstNode* left_operand = ATTR(&bin_expr_gen0, ast_node, left_operand);
    AstNode* right_operand = ATTR(&bin_expr_gen0 , ast_node, right_operand);
    if(success = name_resolve(left_operand) && name_resolve(right_operand))
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
    if(success = name_resolve(operand))
    {
      ATTR(un_expr_gen1, ast_node, operand) = operand;
    }
  }
  else if(node->kind == AstNode_lit)
  {
    AstNode lit_gen0 = *node;
    LiteralKind lit_kind = ATTR(node, lit_kind, lit_kind);

#if 0
    if(lit_kind == Literal_str)
    {
      char* name = make_temp_name("var");
      //todo: add the symbol to the global scope
      Symbol* decl_sym = add_symbol(name, node->src_loc, Symbol_var_decl);
      SYM(decl_sym, var_decl)->data = ATTR(&lit_gen0, str, str);

      Symbol* occur_sym = add_symbol(name, node->src_loc, Symbol_var_occur);
      SYM(occur_sym, var_occur)->var_decl = decl_sym;
      SYM(occur_sym, var_occur)->decl_scope_depth = decl_sym->nesting_depth - occur_sym->nesting_depth;

      AstNode* var_occur = make_ast_node(Ast_gen1, node, AstNode_var_occur);
      ATTR(var_occur, str, name) = name;
      ATTR(var_occur, symbol, occur_sym) = occur_sym;
    }
    else
#endif
    {
      AstNode* lit_gen1 = make_ast_node(Ast_gen1, node, AstNode_lit);
      ATTR(lit_gen1, lit_kind, lit_kind) = lit_kind;

      if(lit_kind == Literal_int_val)
        ATTR(lit_gen1, int_val, int_val) = ATTR(&lit_gen0, int_val, int_val);
      else if(lit_kind == Literal_float_val)
        ATTR(lit_gen1, float_val, float_val) = ATTR(&lit_gen0, float_val, float_val);
      else if(lit_kind == Literal_char_val)
        ATTR(lit_gen1, char_val, char_val) = ATTR(&lit_gen0, char_val, char_val);
      else if(lit_kind == Literal_bool_val)
        ATTR(lit_gen1, bool_val, bool_val) = ATTR(&lit_gen0, bool_val, bool_val);
      else if(lit_kind == Literal_str)
        ATTR(lit_gen1, str, str) = ATTR(&lit_gen0, str, str);
      else
        assert(0);
    }
  }
  else if(node->kind == AstNode_block)
  {
    begin_scope(ScopeKind_block, 0);
    success = name_resolve_block(node);
    end_scope();
  }
  else if(node->kind == AstNode_stmt)
  {
    AstNode stmt_gen0 = *node;
    AstNode* stmt_gen1 = make_ast_node(Ast_gen1, node, AstNode_stmt);

    if(success = name_resolve(ATTR(&stmt_gen0, ast_node, stmt)))
    {
      ATTR(stmt_gen1, ast_node, stmt) = ATTR(&stmt_gen0, ast_node, stmt);
    }
  }
  else if(node->kind == AstNode_if_stmt)
  {
    AstNode if_stmt_gen0 = *node;
    AstNode* if_stmt_gen1 = make_ast_node(Ast_gen1, node, AstNode_if_stmt);

    AstNode* cond_expr = ATTR(&if_stmt_gen0, ast_node, cond_expr);
    if(success = name_resolve(cond_expr))
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

      begin_scope(ScopeKind_block, if_stmt_gen1);
      success = name_resolve(body);
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

          begin_scope(ScopeKind_block, if_stmt_gen1);
          success = name_resolve_block(else_body);
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
    if(success = name_resolve(cond_expr))
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

      begin_scope(ScopeKind_loop, while_stmt_gen1);
      success = name_resolve_block(body);
      end_scope();

      if(success)
      {
        ATTR(while_stmt_gen1, ast_node, body) = body;
      }
    }
  }
  else if(node->kind == AstNode_break_stmt
      || node->kind == AstNode_continue_stmt)
  {
    make_ast_node(Ast_gen1, node, node->kind);

    Scope* loop_scope = find_scope(ScopeKind_loop);
    if(loop_scope)
    {
      ATTR(node, ast_node, loop) = loop_scope->ast_node;
    }
    else
    {
      char* keyword = "???";
      if(node->kind == AstNode_break_stmt)
        keyword = "break";
      else if(node->kind == AstNode_continue_stmt)
        keyword = "continue";
      else
        assert(0);
      success = compile_error(node->src_loc, "unexpected `%s` at this location", keyword);
    }
  }
  else if(node->kind == AstNode_return_stmt)
  {
    AstNode ret_stmt_gen0 = *node;
    AstNode* ret_stmt_gen1 = make_ast_node(Ast_gen1, node, AstNode_return_stmt);

    Scope* proc_scope = find_scope(ScopeKind_proc);
    if(proc_scope)
    {
      ATTR(ret_stmt_gen1, ast_node, proc) = proc_scope->ast_node;

      AstNode* ret_expr = ATTR(&ret_stmt_gen0, ast_node, ret_expr);
      ATTR(ret_stmt_gen1, ast_node, ret_expr) = ret_expr;
      if(ret_expr)
      {
        success = name_resolve(ret_expr);
      }
    }
    else
      success = compile_error(ret_stmt_gen1->src_loc, "unexpected `return` at this location");
  }
  else if(node->kind == AstNode_type)
  {
    AstNode type_gen0 = *node;
    AstNode* type_gen1 = make_ast_node(Ast_gen1, node, AstNode_type);

    if(success = name_resolve_type(ATTR(&type_gen0, ast_node, type_expr)))
    {
      ATTR(type_gen1, ast_node, type_expr) = ATTR(&type_gen0, ast_node, type_expr);
    }
  }
  else
    assert(0);

  return success;
}

void build_type_of_node(AstNode* node);

Type* make_proc_arguments_type(List* args)
{
  for(ListItem* list_item = args->first;
      list_item;
      list_item = list_item->next)
  {
    build_type_of_node(ITEM(list_item, ast_node));
  }

  Type* type = basic_type_void;
  ListItem* list_item = args->first;
  if(list_item)
  {
    AstNode* arg = ITEM(list_item, ast_node);
    type = ATTR(arg, type, eval_type);

    for(list_item = list_item->next;
        list_item;
        list_item = list_item->next)
    {
      AstNode* arg = ITEM(list_item, ast_node);
      type = new_product_type(type, ATTR(arg, type, eval_type));
    }
  }
  return type;
}

void build_type_of_node(AstNode* node)
{
  assert(node->gen == Ast_gen1);

  if(node->kind == AstNode_module)
  {
    build_type_of_node(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_block)
  {
    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item;
        list_item = list_item->next)
    {
      build_type_of_node(ITEM(list_item, ast_node));
    }
  }
  else if(node->kind == AstNode_stmt)
  {
    build_type_of_node(ATTR(node, ast_node, stmt));
  }
  else if(node->kind == AstNode_var_decl)
  {
    AstNode* type_node = ATTR(node, ast_node, type);
    build_type_of_node(type_node);
    ATTR(node, type, eval_type) = ATTR(node, type, type) = ATTR(type_node, type, type);

    AstNode* init_expr = ATTR(node, ast_node, init_expr);
    if(init_expr)
    {
      build_type_of_node(init_expr);
    }
  }
  else if(node->kind == AstNode_var_occur)
  {
    AstNode* var_decl = ATTR(node, ast_node, var_decl);
    ATTR(node, type, eval_type) = ATTR(node, type, type) = ATTR(var_decl, type, type);
  }
  else if(node->kind == AstNode_proc_decl)
  {
    AstNode* ret_type_node = ATTR(node, ast_node, ret_type);
    build_type_of_node(ret_type_node);
    Type* ret_type = ATTR(ret_type_node, type, type);
    Type* args_type = make_proc_arguments_type(ATTR(node, list, formal_args));
    ATTR(node, type, type) = new_proc_type(args_type, ret_type);
    ATTR(node, type, eval_type) = ret_type;

    AstNode* body = ATTR(node, ast_node, body);
    build_type_of_node(body);
  }
  else if(node->kind == AstNode_proc_occur)
  {
    Type* args_type = make_proc_arguments_type(ATTR(node, list, actual_args));
    Type* ret_type = 0;
    AstNode* proc_decl = ATTR(node, ast_node, proc_decl);
    if(proc_decl)
    {
      ret_type = ATTR(proc_decl, type, eval_type);
    }
    else
    {
      ret_type = new_typevar();
    }
    ATTR(node, type, type) = new_proc_type(args_type, ret_type);
    ATTR(node, type, eval_type) = ret_type;
  }
  else if(node->kind == AstNode_bin_expr)
  {
    AstNode* left_operand = ATTR(node, ast_node, left_operand);
    build_type_of_node(left_operand);
    AstNode* right_operand = ATTR(node, ast_node, right_operand);
    build_type_of_node(right_operand);

    ATTR(node, type, eval_type) = ATTR(node, type, type) = new_typevar();
  }
  else if(node->kind == AstNode_un_expr)
  {
    AstNode* operand = ATTR(node, ast_node, operand);
    build_type_of_node(operand);

    ATTR(node, type, type) = ATTR(node, type, eval_type) = new_typevar();
  }
  else if(node->kind == AstNode_lit)
  {
    LiteralKind lit_kind = ATTR(node, lit_kind, lit_kind);

    Type* type = 0;
    if(lit_kind == Literal_int_val)
      type = basic_type_int;
    else if(lit_kind == Literal_float_val)
      type = basic_type_float;
    else if(lit_kind == Literal_char_val)
      type = basic_type_char;
    else if(lit_kind == Literal_bool_val)
      type = basic_type_bool;
    else if(lit_kind == Literal_str)
      type = new_pointer_type(basic_type_char);
    else
      assert(0);

    ATTR(node, type, eval_type) = ATTR(node, type, type) = type;
  }
  else if(node->kind == AstNode_type)
  {
    ATTR(node, type, eval_type) = ATTR(node, type, type) = make_type_of_type_expr(ATTR(node, ast_node, type_expr));
  }
  else if(node->kind == AstNode_while_stmt)
  {
    build_type_of_node(ATTR(node, ast_node, cond_expr));
    build_type_of_node(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_if_stmt)
  {
    build_type_of_node(ATTR(node, ast_node, cond_expr));
    build_type_of_node(ATTR(node, ast_node, body));
    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      build_type_of_node(ATTR(node, ast_node, else_body));
    }
  }
  else if(node->kind == AstNode_return_stmt)
  {
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    if(ret_expr)
    {
      build_type_of_node(ret_expr);
      ATTR(node, type, type) = ATTR(node, type, eval_type) = ATTR(ret_expr, type, eval_type);
    }
    else
    {
      ATTR(node, type, type) = ATTR(node, type, eval_type) = basic_type_void;
    }
  }
  else if(node->kind == AstNode_break_stmt || node->kind == AstNode_continue_stmt)
  {
    ; // skip
  }
  else
    assert(0);
}

bool typecheck(AstNode* node)
{
  assert(node->gen == Ast_gen1);
  bool success = true;

  if(node->kind == AstNode_module)
  {
    success = typecheck(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_block)
  {
    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item && success;
        list_item = list_item->next)
    {
      success = typecheck(ITEM(list_item, ast_node));
    }
  }
  else if(node->kind == AstNode_stmt)
  {
    success = typecheck(ATTR(node, ast_node, stmt));
  }
  else if(node->kind == AstNode_var_decl)
  {
    AstNode* init_expr = ATTR(node, ast_node, init_expr);
    if(init_expr)
    {
      if(success = typecheck(init_expr))
      {
        Type* var_ty = ATTR(node, type, type);
        Type* init_expr_ty = ATTR(init_expr, type, eval_type);
        success = type_unif(var_ty, init_expr_ty);
        if(!success)
          compile_error(node->src_loc, "typecheck error (var init_expr)");
      }
    }
  }
  else if(node->kind == AstNode_bin_expr)
  {
    AstNode* right_operand = ATTR(node, ast_node, right_operand);
    AstNode* left_operand = ATTR(node, ast_node, left_operand);
    if(success = typecheck(right_operand) && typecheck(left_operand))
    {
      Type* expr_ty = ATTR(node, type, eval_type);
      Type* right_operand_ty = ATTR(right_operand, type, eval_type);
      Type* left_operand_ty = ATTR(left_operand, type, eval_type);

      OperatorKind op_kind = ATTR(node, op_kind, op_kind);
      if(op_kind == Operator_cast)
      {
        success = type_unif(left_operand_ty, expr_ty);
        if(!success)
          compile_error(node->src_loc, "typecheck error (cast)");
      }
      else if(op_kind == Operator_array_index)
      {
        if(success = type_unif(right_operand_ty, basic_type_int))
        {
          success = type_unif(left_operand_ty, new_array_type(-1, expr_ty));
          if(!success)
            compile_error(node->src_loc, "typecheck error (array index)");
        }
        else
          compile_error(node->src_loc, "int type expected");
      }
      else if(is_relational_operator(op_kind))
      {
        success = type_unif(expr_ty, basic_type_bool);
      }
      else
      {
        success = type_unif(left_operand_ty, right_operand_ty) && type_unif(left_operand_ty, expr_ty);
        if(!success)
          compile_error(node->src_loc, "typecheck error (bin_expr)");
      }
    }
  }
  else if(node->kind == AstNode_un_expr)
  {
    AstNode* operand = ATTR(node, ast_node, operand);
    if(success = typecheck(operand))
    {
      Type* expr_ty = ATTR(node, type, eval_type);
      Type* operand_ty = ATTR(operand, type, eval_type);

      OperatorKind op_kind = ATTR(node, op_kind, op_kind);
      if(op_kind == Operator_neg || op_kind == Operator_logic_not)
      {
        success = type_unif(operand_ty, expr_ty);
      }
      else if(op_kind == Operator_deref)
      {
        Type* pointee_ty = new_typevar();
        if(success = type_unif(operand_ty, new_pointer_type(pointee_ty)))
        {
          success = type_unif(pointee_ty, expr_ty);
        }
        else
          success = compile_error(operand->src_loc, "pointer type expected");
      }
      else if(op_kind == Operator_address_of)
      {
        success = type_unif(expr_ty, new_pointer_type(operand_ty));
      }
      else
        assert(0);
    }
  }
  else if(node->kind == AstNode_proc_decl)
  {
    AstNode* body = ATTR(node, ast_node, body);
    if(success = typecheck(body))
    {
      Type* ret_ty = ATTR(node, type, eval_type);
      Type* body_ty = ATTR(body, type, eval_type);

      if(!body_ty)
      {
        body_ty = ATTR(body, type, eval_type) = basic_type_void;
      }

      success = type_unif(body_ty, ret_ty);
      if(!success)
        success = compile_error(node->src_loc, "typecheck error (proc body)");
    }
  }
  else if(node->kind == AstNode_proc_occur)
  {
    List* args = ATTR(node, list, actual_args);
    for(ListItem* list_item = args->first;
        list_item && success;
        list_item = list_item->next)
    {
      success = typecheck(ITEM(list_item, ast_node));
    }

    if(success)
    {
      AstNode* proc_decl = ATTR(node, ast_node, proc_decl);
      if(!proc_decl)
      {
        Symbol* occur_sym = ATTR(node, symbol, occur_sym);
        Symbol* decl_sym = lookup_symbol_in_scope(occur_sym->name, Symbol_proc_decl, occur_sym->scope);
        if(decl_sym)
        {
          SYM(occur_sym, proc_occur)->decl_sym = decl_sym;
          proc_decl = ATTR(node, ast_node, proc_decl) = decl_sym->ast_node;
        }
        else
          success = compile_error(occur_sym->src_loc, "unknown proc `%s`", occur_sym->name);
      }

      if(success)
      {
        Type* decl_ty = ATTR(proc_decl, type, type);
        Type* occur_ty = ATTR(node, type, type);
        success = type_unif(decl_ty, occur_ty);
        if(!success)
          compile_error(node->src_loc, "typecheck error (proc occur)");
      }
    }
  }
  else if(node->kind == AstNode_return_stmt)
  {
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    Type* ret_ty = ATTR(node, type, eval_type);
    Type* ret_expr_ty = basic_type_void;
    if(ret_expr)
    {
      if(success = typecheck(ret_expr))
      {
        ret_expr_ty = ATTR(ret_expr, type, eval_type);
      }
    }

    if(success)
    {
      if(success = type_unif(ret_ty, ret_expr_ty))
      {
        AstNode* proc = ATTR(node, ast_node, proc);
        Type* proc_ret_ty = ATTR(proc, type, eval_type);
        if(success = type_unif(ret_expr_ty, proc_ret_ty))
        {
          AstNode* body = ATTR(proc, ast_node, body);
          Type* body_ty = ATTR(body, type, eval_type);
          if(body_ty)
          {
            success = type_unif(ret_expr_ty, body_ty);
            if(!success)
              compile_error(node->src_loc, "typecheck error (return stmt)");
          }
          else
          {
            ATTR(body, type, eval_type) = ret_expr_ty;
          }
        }
        else
          compile_error(node->src_loc, "typecheck error (return stmt)");
      }
      else
        compile_error(node->src_loc, "typecheck error (return stmt)");
    }
  }
  else if(node->kind == AstNode_while_stmt)
  {
    success = typecheck(ATTR(node, ast_node, cond_expr)) &&
      typecheck(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_if_stmt)
  {
    success = typecheck(ATTR(node, ast_node, cond_expr)) &&
      typecheck(ATTR(node, ast_node, body));
    AstNode*else_body = ATTR(node, ast_node, else_body);
    if(else_body && success)
    {
      success = typecheck(ATTR(node, ast_node, else_body));
    }
  }
  else if(node->kind == AstNode_var_occur || node->kind == AstNode_lit
      || node->kind == AstNode_break_stmt || node->kind == AstNode_continue_stmt
      || node->kind == AstNode_type)
  {
    ; // skip
  }
  else
    assert(0);
  return success;
}

bool semantic(AstNode* module)
{
  bool success = true;

  init_types();

  begin_scope(ScopeKind_global, 0);
  add_builtin_types();
  add_builtin_procs();

  if(success = name_resolve(module))
  {
    if(DEBUG_enabled)/*>>>*/
    {
      printf("--- Name ID ---\n");
      DEBUG_print_arena_usage(arena, "arena");
      DEBUG_print_arena_usage(symbol_table->arena, "symbol_table");

      begin_temp_memory(&arena);
      String* str = str_new(arena);
      DEBUG_print_scope(str, 0, "global_scope", symbol_table->active_scope);
      DEBUG_print_ast_node(str, 0, "module", module);
      str_dump_to_file(str, "debug_name_resolve.txt");
      end_temp_memory(&arena);
    }/*<<<*/

    build_type_of_node(module);
    success = typecheck(module);
    if(!success)
      compile_error(0, "typecheck error");
  }

  end_scope();

  return success;
}

