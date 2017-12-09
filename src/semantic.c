char* get_type_printstr(Type* type)
{
  String str; str_init(&str, arena);
  make_type_printstr(&str, type);
  return str_cap(&str);
}

SymbolTable* new_symbol_table(MemoryArena** arena, int size)
{
  MemoryArena* symbol_arena = push_arena(arena, size);
  symbol_table = mem_push_struct(symbol_arena, SymbolTable);
  symbol_table->arena = symbol_arena;
  symbol_table->nesting_depth = -1;
  symbol_table->scopes = new_list(symbol_arena, eList_scope);
  return symbol_table;
}

Scope* find_scope(Scope* active_scope, eScope kind)
{
  Scope* scope = active_scope;
  while(scope)
  {
    if(scope->kind == kind)
      break;
    scope = scope->encl_scope;
  }
  return scope;
}

Symbol* lookup_symbol(char* name, List* symbols, eSymbol kind)
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

Symbol* lookup_decl_by_kind(char* name, Scope* scope, eSymbol* kinds)
{
  Symbol* result = 0;
  while(!result && scope)
  {
    for(eSymbol* kind = kinds;
        *kind != eSymbol_None && !result;
        kind++)
    {
      result = lookup_symbol(name, scope->decls[*kind], *kind);
    }
    scope = scope->encl_scope;
  }
  return result;
}

Symbol* lookup_all_decls(char* name, Scope* scope)
{
  Symbol* result = 0;
  while(!result && scope)
  {
    for(int k = eSymbol_None+1;
        k < eSymbol_Count && !result;
        k++)
    {
      eSymbol sym_kind = (eSymbol)k;
      result = lookup_symbol(name, scope->decls[sym_kind], sym_kind);
    }
    scope = scope->encl_scope;
  }
  return result;
}

Symbol* add_decl_symbol(char* name, SourceLoc* src_loc, Scope* scope, eSymbol kind)
{
  Symbol* sym = mem_push_struct(symbol_table->arena, Symbol);
  sym->kind = kind;
  sym->name = name;
  sym->src_loc = src_loc;
  sym->scope = scope;
  append_list_elem(scope->decls[kind], sym, eList_symbol);
  return sym;
}

Symbol* add_occur_symbol(char* name, SourceLoc* src_loc, Scope* scope, eSymbol kind)
{
  Symbol* sym = mem_push_struct(symbol_table->arena, Symbol);
  sym->kind = kind;
  sym->name = name;
  sym->src_loc = src_loc;
  sym->scope = scope;
  append_list_elem(scope->occurs[kind], sym, eList_symbol);
  return sym;
}

void add_builtin_type(Scope* scope, char* name, Type* ty)
{
  assert(ty->kind == eType_basic);
  AstNode* type = new_ast_node(eAstNode_type, 0);
  type->type.name = name;
  type->ty = ty;
  Symbol* decl_sym = add_decl_symbol(name, 0, scope, eSymbol_type);
  decl_sym->ty = ty;
  decl_sym->ast_node = type;
  type->type.decl_sym = decl_sym;
}

void add_builtin_types(Scope* scope)
{
  add_builtin_type(scope, "bool", basic_type_bool);
  add_builtin_type(scope, "int", basic_type_int);
  add_builtin_type(scope, "char", basic_type_char);
  add_builtin_type(scope, "float", basic_type_float);
  add_builtin_type(scope, "void", basic_type_void);
}

Scope* begin_scope(eScope kind, AstNode* ast_node)
{
  Scope* scope = mem_push_struct(symbol_table->arena, Scope);
  scope->kind = kind;
  if(scope->kind == eScope_block)
  {
    symbol_table->nesting_depth++;
  }
  scope->nesting_depth = symbol_table->nesting_depth;
  scope->encl_scope = symbol_table->active_scope;
  scope->ast_node = ast_node;
  for(int i = 0; i < eSymbol_Count; i++)
  {
    scope->decls[i] = new_list(arena, eList_symbol);
    scope->occurs[i] = new_list(arena, eList_symbol);
  }
  symbol_table->active_scope = scope;
  append_list_elem(symbol_table->scopes, scope, eList_scope);
  return scope;
}

void end_scope()
{
  Scope* active_scope = symbol_table->active_scope;
  if(active_scope->kind == eScope_block)
  {
    symbol_table->nesting_depth--;
  }
  symbol_table->active_scope = active_scope->encl_scope;
}

void process_includes(List* include_list, List* module_list, ListItem* module_list_item)
{
  for(ListItem* list_item = include_list->first;
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = ITEM(list_item, ast_node);

    if(node->kind == eAstNode_include)
    {
      AstNode* block = node->include.body;
      process_includes(block->block.nodes, include_list, list_item);
    }
  }
  replace_list_item_at(include_list, module_list, module_list_item);

  mem_zero_struct(include_list, List);
}

bool name_ident(AstNode* node);

bool name_ident_type(AstNode* node)
{
  bool success = true;

  switch(node->kind)
  {
    case eAstNode_id:
      {
        char* name = node->id.name;

        Symbol* decl_sym = lookup_all_decls(name, symbol_table->active_scope);
        if(decl_sym)
        {
          Symbol* occur_sym = add_occur_symbol(name, node->src_loc, symbol_table->active_scope, eSymbol_type);
          node->id.decl_ast = decl_sym->ast_node;
          node->id.occur_sym = occur_sym;
        }
        else
          success = compile_error(node->src_loc, "unknown type `%s`", name);
      }
      break;

    case eAstNode_pointer:
      {
        success = name_ident_type(node->pointer.pointee_expr);
      }
      break;

    case eAstNode_array:
      {
        if(success = name_ident_type(node->array.elem_expr))
        {
          if(node->array.size_expr)
          {
            success = name_ident(node->array.size_expr);
          }
        }
      }
      break;

    default:
      assert(0);
  }
  return success;
}

bool name_ident_formal_arg(AstNode* node, eSymbol symkind)
{
  assert(node->kind == eAstNode_var);
  assert(symkind == eSymbol_ret_var || symkind == eSymbol_formal_arg);
  bool success = true;

  char* name = node->var.name;
  Symbol* decl_sym = lookup_all_decls(name, symbol_table->active_scope);
  if(decl_sym && (decl_sym->scope == symbol_table->active_scope))
  {
    success = compile_error(node->src_loc, "formal arg `%s` already declared", name);
    compile_error(decl_sym->src_loc, "see declaration of `%s`", name);
  }
  else
  {
    decl_sym = add_decl_symbol(name, node->src_loc, symbol_table->active_scope, symkind);
    node->var.decl_sym = decl_sym;
    decl_sym->ast_node = node;

    success = name_ident(node->var.type);
  }
  return success;
}

bool name_ident_var(AstNode* node, Scope* scope, void* data)
{
  assert(node->kind == eAstNode_var);
  bool success = true;

  char* name = node->var.name;
  Scope* proc_scope = find_scope(symbol_table->active_scope, eScope_proc);
  Symbol* decl_sym = lookup_all_decls(name, symbol_table->active_scope);
  if(decl_sym)
  {
    if((decl_sym->kind != eSymbol_var && decl_sym->kind != eSymbol_ret_var && decl_sym->kind != eSymbol_formal_arg)
        || decl_sym->scope == symbol_table->active_scope || decl_sym->scope == proc_scope)
    {
      success = compile_error(node->src_loc, "name `%s` already declared", name);
      compile_error(decl_sym->src_loc, "see declaration of `%s`", name);
    }
  }
  if(!success)
    return success;

  decl_sym = node->var.decl_sym = add_decl_symbol(name, node->src_loc, scope, eSymbol_var);
  decl_sym->ast_node = node;
  decl_sym->data = data;

  success = name_ident(node->var.type);
  return success;
}

bool name_ident(AstNode* node)
{
  bool success = true;

  switch(node->kind)
  {
    case eAstNode_module:
      {
        AstNode* module_body = node->module.body;
        Scope* module_scope = node->module.scope = begin_scope(eScope_module, node);
        add_builtin_types(module_scope);
        success = name_ident(module_body);
        end_scope();
        if(!success)
          break;

        Scope* body_scope = module_body->block.scope;
        List* body_vars = body_scope->decls[eSymbol_var];
        List* module_vars = module_scope->decls[eSymbol_var];
        for(ListItem* list_item = body_vars->first;
            list_item;)
        {
          ListItem* next_list_item = list_item->next;
          remove_list_item(body_vars, list_item);
          append_list_item(module_vars, list_item);
          list_item = next_list_item;
        }
        assert(!body_vars->first && !body_vars->last);

        for(ListItem* list_item = module_vars->first;
            list_item;
            list_item = list_item->next)
        {
          Symbol* decl_sym = ITEM(list_item, symbol);
          decl_sym->is_static_alloc = true;
        }
      }
      break;

    case eAstNode_block:
      {
        List* proc_list = node->block.procs = new_list(arena, eList_ast_node);
        List* stmt_list = node->block.stmts = new_list(arena, eList_ast_node);
        List* var_list = node->block.vars = new_list(arena, eList_ast_node);

        List* node_list = node->block.nodes;
        for(ListItem* list_item = node_list->first;
            list_item;)
        {
          AstNode* node = ITEM(list_item, ast_node);

          ListItem* next_list_item = list_item->next;
          remove_list_item(node_list, list_item);

          switch(node->kind)
          {
            case eAstNode_proc:
              append_list_elem(proc_list, node, eList_ast_node);
              break;

            case eAstNode_var:
              {
                append_list_elem(var_list, node, eList_ast_node);

                AstNode* init_expr = node->var.init_expr;
                if(init_expr)
                {
                  append_list_elem(stmt_list, init_expr, eList_ast_node);
                  node->var.init_expr = 0;
                }
              }
              break;

            case eAstNode_stmt:
            case eAstNode_while_stmt:
            case eAstNode_if_stmt:
            case eAstNode_ret_stmt:
            case eAstNode_loop_ctrl:
            case eAstNode_block:
            case eAstNode_asm_block:
              append_list_elem(stmt_list, node, eList_ast_node);
              break;

            default:
              assert(0);
          }
          list_item = next_list_item;
        }
        assert(node_list->first == node_list->last && node_list->last == 0); // list should be empty

        for(ListItem* list_item = var_list->first;
            list_item && success;
            list_item = list_item->next)
        {
          AstNode* node = ITEM(list_item, ast_node);
          append_list_elem(node_list, node, eList_ast_node);
          success = name_ident(node);
        }
        for(ListItem* list_item = proc_list->first;
            list_item && success;
            list_item = list_item->next)
        {
          AstNode* node = ITEM(list_item, ast_node);
          append_list_elem(node_list, node, eList_ast_node);
          success = name_ident(node);
        }
        for(ListItem* list_item = stmt_list->first;
            list_item && success;
            list_item = list_item->next)
        {
          AstNode* node = ITEM(list_item, ast_node);
          append_list_elem(node_list, node, eList_ast_node);
          success = name_ident(node);
        }
        end_scope();
      }
      break;

    case eAstNode_type:
      {
        if(success = name_ident_type(node->type.type_expr))
        {
          Symbol* decl_sym = add_decl_symbol(make_temp_name("typ"), node->src_loc, symbol_table->active_scope, eSymbol_type);
          node->type.decl_sym = decl_sym;
          decl_sym->ast_node = node;
        }
      }
      break;

    case eAstNode_var:
      {
        success = name_ident_var(node, symbol_table->active_scope, 0);
      }
      break;

    case eAstNode_id:
      {
        char* name = node->id.name;
        Symbol* decl_sym = lookup_all_decls(name, symbol_table->active_scope);
        if(decl_sym)
        {
          Symbol* occur_sym = add_occur_symbol(name, node->src_loc, symbol_table->active_scope, eSymbol_var);
          occur_sym->ast_node = node;
          occur_sym->decl = decl_sym;

          node->id.occur_sym = occur_sym;
          node->id.decl_sym = decl_sym;
          node->id.decl_ast = decl_sym->ast_node;
        }
        else
          success = compile_error(node->src_loc, "unknown var `%s`", name);
      }
      break;

    case eAstNode_proc:
      {
        char* name = node->proc.name = node->proc.id->id.name;

        char* label = node->proc.label = name;
        Scope* decl_scope = find_scope(symbol_table->active_scope, eScope_proc);
        if(!decl_scope)
        {
          decl_scope = find_scope(symbol_table->active_scope, eScope_module);
        }
        if(decl_scope->kind == eScope_proc)
        {
          AstNode* encl_proc = decl_scope->ast_node;
          assert(encl_proc->kind == eAstNode_proc);
          String qual_label; str_init(&qual_label, arena);
          str_append(&qual_label, encl_proc->proc.label);
          str_append(&qual_label, "$");
          str_append(&qual_label, label);
          label = node->proc.label = str_cap(&qual_label);
        }

        Symbol* decl_sym = lookup_all_decls(name, symbol_table->active_scope);
        if(decl_sym)
        {
          if((decl_sym->kind != eSymbol_proc && decl_sym->kind != eSymbol_extern_proc)
              || decl_sym->scope == symbol_table->active_scope)
          {
            success = compile_error(node->src_loc, "name `%s` already declared", name);
            compile_error(decl_sym->src_loc, "see declaration of `%s`", name);
          }
        }
        if(!success)
          return success;

        bool is_extern = node->proc.is_extern;
        decl_sym = node->proc.decl_sym
          = add_decl_symbol(name, node->src_loc, decl_scope, is_extern ? eSymbol_extern_proc : eSymbol_proc);
        decl_sym->ast_node = node;

        List* formal_args = node->proc.formal_args;
        AstNode* ret_var = node->proc.ret_var;
        AstNode* body = node->proc.body;

        Scope* proc_scope = node->proc.scope = begin_scope(eScope_proc, node);
        for(ListItem* list_item = formal_args->first;
            list_item && success;
            list_item = list_item->next)
        {
          success = name_ident_formal_arg(ITEM(list_item, ast_node), eSymbol_formal_arg);
        }
        if(success && (success = name_ident_formal_arg(ret_var, eSymbol_ret_var)))
        {
          if(!is_extern)
          {
            if(body)
            {
              if(success = name_ident(body))
              {
                Scope* body_scope = body->block.scope;
                proc_scope->nesting_depth = body_scope->nesting_depth;
              }
            }
            else
              success = compile_error(node->src_loc, "proc `%s` must have a body", name);
          }
          else if(body)
            success = compile_error(node->src_loc, "`extern` proc `%s` must not have a body", name);
        }
        end_scope();
      }
      break;

    case eAstNode_call:
      {
        char* name = node->call.name = node->call.id->id.name;

        Symbol* occur_sym = add_occur_symbol(name, node->src_loc, symbol_table->active_scope, eSymbol_proc);
        occur_sym->ast_node = node;
        node->call.occur_sym = occur_sym;

        Symbol* decl_sym = lookup_decl_by_kind(name, symbol_table->active_scope,
            (eSymbol[]){eSymbol_proc, eSymbol_extern_proc, eSymbol_None});
        if(decl_sym)
        {
          occur_sym->decl = decl_sym;
          node->call.decl_sym = decl_sym;
          node->call.proc = decl_sym->ast_node;
        }

        List* actual_args = node->call.actual_args;
        for(ListItem* list_item = actual_args->first;
            list_item && success;
            list_item = list_item->next)
        {
          success = name_ident(ITEM(list_item, ast_node));
        }
      }
      break;
      
    case eAstNode_bin_expr:
      {
        AstNode* left_operand = node->bin_expr.left_operand;
        AstNode* right_operand = node->bin_expr.right_operand;
        success = name_ident(left_operand) && name_ident(right_operand);
      }
      break;

    case eAstNode_un_expr:
      {
        AstNode* operand = node->un_expr.operand;
        success = name_ident(operand);
      }
      break;

    case eAstNode_lit:
      {
#if 0
        eLiteral lit_kind = node->lit.kind;
        if(lit_kind == eLiteral_str_val)
        {
          char* str_val = ATTR(lit_gen1, str_val, str_val) = ATTR(&lit_gen0, str_val, str_val);

          AstNode* var = new_ast_node(eAstGen_gen0, eAstNode_var, node->src_loc);
          AstNode* decl_id = ATTR(var, ast_node, id) = new_ast_node(eAstGen_gen0, eAstNode_id, node->src_loc);
          char* var_name = ATTR(decl_id, str_val, name) = make_temp_name("str");

          AstNode* var_type = ATTR(var, ast_node, type) = new_ast_node(eAstGen_gen0, eAstNode_type_decl, node->src_loc);
          AstNode* type_expr = ATTR(var_type, ast_node, type_expr) = new_ast_node(eAstGen_gen0, eAstNode_array, node->src_loc);
          AstNode* size_expr = ATTR(type_expr, ast_node, size_expr) = new_ast_node(eAstGen_gen0, eAstNode_lit, node->src_loc);
          ATTR(size_expr, lit_kind, lit_kind) = eLiteral_int_val;
          ATTR(size_expr, int_val, int_val) = cstr_len(str_val) + 1; // +NULL
          AstNode* elem_expr = ATTR(type_expr, ast_node, elem_expr) = new_ast_node(eAstGen_gen0, eAstNode_id, node->src_loc);
          ATTR(elem_expr, str_val, name) = "char";

          Scope* module_scope = find_scope(symbol_table->active_scope, eScope_module);
          if(success = name_ident_var(var, module_scope, str_val))
          {
            AstNode* module_body = ATTR(module_scope->ast_node, ast_node, body);
            prepend_list_elem(ATTR(module_body, list, vars), var, eList_ast_node);
            prepend_list_elem(ATTR(module_body, list, nodes), var, eList_ast_node);

            AstNode* occur_id = make_ast_node(eAstGen_gen0, node, eAstNode_id);
            ATTR(occur_id, str_val, name) = var_name;

            success = name_ident(occur_id);
          }
        }
#endif
      }
      break;

    case eAstNode_stmt:
      {
        if(node->stmt.stmt)
        {
          success = name_ident(node->stmt.stmt);
        }
      }
      break;

    case eAstNode_if_stmt:
      {
        AstNode* cond_expr = node->if_stmt.cond_expr;
        if(success = name_ident(cond_expr))
        {
          AstNode* body = node->if_stmt.body;
          if(body->kind != eAstNode_block)
          {
            List* single_stmt_block = new_list(arena, eList_ast_node);
            append_list_elem(single_stmt_block, body, eList_ast_node);
            body = new_ast_node(eAstNode_block, body->src_loc);
            body->block.nodes = single_stmt_block;
          }

          if(!(success = name_ident(body)))
            break;

          node->if_stmt.body = body;

          AstNode* else_body = node->if_stmt.else_body;
          if(else_body)
          {
            if(else_body->kind != eAstNode_block)
            {
              List* single_stmt_block = new_list(arena, eList_ast_node);
              append_list_elem(single_stmt_block, else_body, eList_ast_node);
              else_body = new_ast_node(eAstNode_block, else_body->src_loc);
              else_body->block.nodes = single_stmt_block;
            }

            success = name_ident(else_body);
          }
        }
      }
      break;

    case eAstNode_while_stmt:
      {
        Scope* enclosing_scope = symbol_table->active_scope;
        Scope* while_scope = node->while_stmt.scope = begin_scope(eScope_while, node);
        while_scope->nesting_depth = enclosing_scope->nesting_depth;

        AstNode* cond_expr = node->while_stmt.cond_expr;
        if(success = name_ident(cond_expr))
        {
          AstNode* body = node->while_stmt.body;
          if(body->kind != eAstNode_block)
          {
            List* single_stmt_block = new_list(arena, eList_ast_node);
            append_list_elem(single_stmt_block, body, eList_ast_node);
            body = new_ast_node(eAstNode_block, body->src_loc);
            body->block.nodes = single_stmt_block;
          }
          success = name_ident(body);
        }
        end_scope();
      }
      break;

    case eAstNode_loop_ctrl:
      {
        Scope* loop_scope = find_scope(symbol_table->active_scope, eScope_while);
        if(loop_scope)
        {
          node->loop_ctrl.loop = loop_scope->ast_node;
        }
        else
        {
          char* keyword = "???";
          if(node->loop_ctrl.kind == eLoopCtrl_break)
            keyword = "break";
          else if(node->loop_ctrl.kind == eLoopCtrl_continue)
            keyword = "continue";
          else
            assert(0);
          success = compile_error(node->src_loc, "unexpected `%s` at this location", keyword);
        }
      }
      break;

    case eAstNode_empty:
      {
        ;//skip
      }
      break;

    case eAstNode_ret_stmt:
      {
        Scope* scope = find_scope(symbol_table->active_scope, eScope_proc);
        if(scope)
        {
          AstNode* proc = node->ret_stmt.proc = scope->ast_node;

          AstNode* ret_expr = node->ret_stmt.ret_expr;
          if(ret_expr)
          {
            AstNode* ret_var = proc->proc.ret_var;
            AstNode* occur_id = new_ast_node(eAstNode_id, ret_var->src_loc);
            occur_id->id.name = ret_var->id.name;

            AstNode* assign = new_ast_node(eAstNode_bin_expr, ret_expr->src_loc);
            assign->bin_expr.op_kind = eOperator_assign;
            assign->bin_expr.left_operand = occur_id;
            assign->bin_expr.right_operand = ret_expr;

            ret_expr = new_ast_node(eAstNode_stmt, assign->src_loc);
            ret_expr->stmt.stmt = assign;
            node->ret_stmt.ret_expr = ret_expr;

            success = name_ident(ret_expr);
          }
        }
        else
          success = compile_error(node->src_loc, "unexpected `return` at this location");
      }
      break;

    case eAstNode_asm_block:
      {
        ;//skip
      }
      break;

    default:
      assert(0);
  }
  return success;
}

bool build_types(AstNode* node)
{
  bool success = true;

  switch(node->kind)
  {
    case eAstNode_module:
      {
        AstNode* body = node->module.body;
        if(success = build_types(body))
        {
          node->ty = body->ty;
          node->eval_ty = body->eval_ty;
        }
      }
      break;

    case eAstNode_block:
      {
        for(ListItem* list_item = node->block.nodes->first;
            list_item && success;
            list_item = list_item->next)
        {
          success = build_types(ITEM(list_item, ast_node));
        }

        node->ty = new_proc_type(basic_type_void, basic_type_void);
        node->eval_ty = basic_type_void;
      }
      break;

    case eAstNode_stmt:
      {
        AstNode* actual_stmt = node->stmt.stmt;
        if(actual_stmt)
        {
          success = build_types(actual_stmt);
          node->ty = actual_stmt->eval_ty;
          node->eval_ty = basic_type_void;
        }
      }
      break;

    case eAstNode_var:
      {
        AstNode* type = node->var.type;
        if(success = build_types(type))
        {
          node->ty = type->ty;
          node->eval_ty = basic_type_void;
        }
      }
      break;

#if 0
    case eAstNode_var_occur:
      {
        AstNode* var = ATTR(node, ast_node, var);
        ATTR(node, type, eval_ty) = ATTR(node, type, type) = ATTR(var, type, type);
      }
      break;
#endif

    case eAstNode_bin_expr:
      {
        AstNode* left_operand = node->bin_expr.left_operand;
        if(success = build_types(left_operand))
        {
          AstNode* right_operand = node->bin_expr.right_operand;
          if(success = build_types(right_operand))
          {
            Type* eval_ty =  new_typevar();
            node->ty = new_proc_type(new_product_type(left_operand->eval_ty, right_operand->eval_ty), eval_ty);
            node->eval_ty = eval_ty;
          }
        }
      }
      break;

    case eAstNode_un_expr:
      {
        AstNode* operand = node->un_expr.operand;
        if(success = build_types(operand))
        {
          Type* eval_ty = new_typevar();
          node->ty = new_proc_type(operand->eval_ty, eval_ty);
          node->eval_ty = eval_ty;
        }
      }
      break;

    case eAstNode_proc:
      {
        for(ListItem* list_item = node->proc.formal_args->first;
            list_item && success;
            list_item = list_item->next)
        {
          success = build_types(ITEM(list_item, ast_node));
        }

        if(success)
        {
          Type* args_type = basic_type_void;
          ListItem* list_item = node->proc.formal_args->first;
          if(list_item)
          {
            AstNode* arg = ITEM(list_item, ast_node);
            args_type = arg->ty;

            for(list_item = list_item->next;
                list_item;
                list_item = list_item->next)
            {
              AstNode* arg = ITEM(list_item, ast_node);
              args_type = new_product_type(args_type, arg->ty);
            }
          }

          AstNode* ret_var = node->proc.ret_var;
          if(success = build_types(ret_var))
          {
            node->ty = new_proc_type(args_type, ret_var->ty);
            node->eval_ty = basic_type_void;

            if(!node->proc.is_extern)
            {
              success = build_types(node->proc.body);
            }
          }
        }
      }
      break;

    case eAstNode_ret_stmt:
      {
        if(node->ret_stmt.ret_expr)
        {
          success = build_types(node->ret_stmt.ret_expr);
        }
        node->ty = node->eval_ty = basic_type_void;
      }
      break;

    case eAstNode_call:
      {
        for(ListItem* list_item = node->call.actual_args->first;
            list_item && success;
            list_item = list_item->next)
        {
          success = build_types(ITEM(list_item, ast_node));
        }

        Type* args_type = basic_type_void;
        ListItem* list_item = node->call.actual_args->first;
        if(list_item)
        {
          AstNode* arg = ITEM(list_item, ast_node);
          args_type = arg->eval_ty;

          for(list_item = list_item->next;
              list_item;
              list_item = list_item->next)
          {
            AstNode* arg = ITEM(list_item, ast_node);
            args_type = new_product_type(args_type, arg->eval_ty);
          }
        }

        Type* ret_type = new_typevar();
        AstNode* proc = node->call.proc;
        if(proc)
        {
          ret_type = proc->ty->proc.ret;
        }

        node->ty = new_proc_type(args_type, ret_type);
        node->eval_ty = ret_type;
      }
      break;

    case eAstNode_while_stmt:
      {
        if(success = build_types(node->while_stmt.cond_expr))
        {
          AstNode* body = node->while_stmt.body;
          if(success = build_types(body))
          {
            node->ty = body->ty;
            node->eval_ty = body->eval_ty;
          }
        }
      }
      break;

    case eAstNode_if_stmt:
      {
        if(success = build_types(node->if_stmt.cond_expr))
        {
          AstNode* body = node->if_stmt.body;
          if(success = build_types(body))
          {
            node->ty = body->ty;
            node->eval_ty = body->eval_ty;

            AstNode* else_body = node->if_stmt.else_body;
            if(else_body)
            {
              success = build_types(else_body);
            }
          }
        }
      }
      break;

    case eAstNode_lit:
      {
        Type* type = 0;
        switch(node->lit.kind)
        {
          case eLiteral_int_val:
            type = basic_type_int;
            break;
          case eLiteral_float_val:
            type = basic_type_float;
            break;
          case eLiteral_char_val:
            type = basic_type_char;
            break;
          case eLiteral_bool_val:
            type = basic_type_bool;
            break;
          case eLiteral_str_val:
            {
              int size_val = cstr_len(node->lit.str_val) + 1; // +NULL
              type = new_array_type(size_val, basic_type_char);
            }
            break;
          default:
            assert(0);
        }
        node->ty = node->eval_ty = type;
      }
      break;

    case eAstNode_type:
      {
        AstNode* type_expr = node->type.type_expr;
        if(success = build_types(type_expr))
        {
          node->ty = node->eval_ty = type_expr->ty;
        }
      }
      break;

#if 0
    case eAstNode_type_occur:
      {
        AstNode* type_decl = ATTR(node, ast_node, type_decl);
        ATTR(node, type, type) = ATTR(type_decl, type, type);
        ATTR(node, type, eval_ty) = ATTR(type_decl, type, eval_ty);
      }
      break;
#endif

    case eAstNode_pointer:
      {
        AstNode* pointee_expr = node->pointer.pointee_expr;
        if(success = build_types(pointee_expr))
        {
          node->ty = node->eval_ty = new_pointer_type(pointee_expr->ty);
        }
      }
      break;

    case eAstNode_array:
      {
        int size_val = 0;
        AstNode* size_expr = node->array.size_expr;
        if(size_expr)
        {
          if(size_expr->kind == eAstNode_lit && size_expr->lit.kind == eLiteral_int_val)
          {
            size_val = size_expr->lit.int_val;
          }
          else
            success = compile_error(size_expr->src_loc, "array size must be an int literal");
        }
        AstNode* elem_expr = node->array.elem_expr;
        if(success = build_types(elem_expr))
        {
          node->ty = node->eval_ty = new_array_type(size_val, elem_expr->ty);
        }
      }
      break;

    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    case eAstNode_asm_block:
      node->ty = node->eval_ty = basic_type_void;
      break;

    default:
      assert(0);
  }
  return success;
}

bool eval_types(AstNode* node)
{
  bool success = true;

  switch(node->kind)
  {
    case eAstNode_module:
      success = eval_types(node->module.body);
      break;

    case eAstNode_block:
      {
        for(ListItem* list_item = node->block.nodes->first;
            list_item && success;
            list_item = list_item->next)
        {
          AstNode* stmt = ITEM(list_item, ast_node);
          if(success = eval_types(stmt))
          {
            if(!type_unif(stmt->eval_ty, node->eval_ty))
            {
              success = compile_error(stmt->src_loc, "type error (block stmt)");
            }
          }
        }
      }
      break;
    case eAstNode_stmt:
      {
        if(node->stmt.stmt)
        {
          success = eval_types(node->stmt.stmt);
        }
      }
      break;

    case eAstNode_bin_expr:
      {
        AstNode* right_operand = node->bin_expr.right_operand;
        AstNode* left_operand = node->bin_expr.left_operand;

        if(success = eval_types(right_operand) && eval_types(left_operand))
        {
          switch(node->bin_expr.op_kind)
          {
            case eOperator_cast:
              if(!type_unif(node->eval_ty, left_operand->eval_ty))
              {
                success = compile_error(node->src_loc, "type error (cast)");
              }
              break;

            case eOperator_index:
              if(type_unif(right_operand->eval_ty, basic_type_int))
              {
                if(left_operand->eval_ty->kind == eType_array)
                {
                  success = type_unif(left_operand->eval_ty->array.elem, node->eval_ty);
                }
                else if(left_operand->eval_ty->kind == eType_typevar)
                {
                  success = type_unif(left_operand->eval_ty, new_array_type(0, node->eval_ty));
                }
                else
                  success = compile_error(node->src_loc, "type error (array index)");
              }
              else
                success = compile_error(node->src_loc, "int type expected");
              break;

            case eOperator_bit_and:
            case eOperator_bit_or:
            case eOperator_bit_xor:
              if(type_unif(left_operand->eval_ty, right_operand->eval_ty))
              {
                ;//ok
              }
              else
                success = compile_error(node->src_loc, "type error (bitwise op)");
              break;

            case eOperator_bit_shift_left:
            case eOperator_bit_shift_right:
              if(type_unif(left_operand->eval_ty, basic_type_int) && type_unif(right_operand->eval_ty, basic_type_char))
              {
                ;//ok
              }
              else
                success = compile_error(node->src_loc, "type error (bitwise op)");
              break;

            default:
              if(type_unif(left_operand->eval_ty, right_operand->eval_ty))
              {
                switch(node->bin_expr.op_kind)
                {
                  case eOperator_less:
                  case eOperator_less_eq:
                  case eOperator_greater:
                  case eOperator_greater_eq:
                  case eOperator_eq:
                  case eOperator_not_eq:
                  case eOperator_logic_and:
                  case eOperator_logic_or:
                  case eOperator_logic_not:
                    if(!type_unif(node->eval_ty, basic_type_bool))
                    {
                      success = compile_error(node->src_loc, "type error (bin expr)");
                    }
                    break;

                  default:
                    if(!type_unif(node->eval_ty, left_operand->eval_ty))
                    {
                      success = compile_error(node->src_loc, "type error (bin expr)");
                    }
                    break;
                }
              }
              else
                success = compile_error(node->src_loc, "type error (bin expr)");
              break;
          }
          if(!success)
            break;

          assert(node->ty->kind == eType_proc);
          if(!type_unif(node->ty->proc.ret, node->eval_ty))
          {
            success = compile_error(node->src_loc, "type error (bin expr)");
          }
        }
      }
      break;

    case eAstNode_un_expr:
      {
        AstNode* operand = node->un_expr.operand;

        node->eval_ty = new_typevar();
        if(success = eval_types(operand))
        {
          switch(node->un_expr.op_kind)
          {
            case eOperator_neg:
            case eOperator_logic_not:
              if(!type_unif(node->eval_ty, operand->eval_ty))
              {
                success = compile_error(node->src_loc, "type error (un expr)");
              }
              break;
            case eOperator_deref:
              {
                Type* pointee_ty = new_typevar();
                if(type_unif(operand->eval_ty, new_pointer_type(pointee_ty)))
                {
                  if(!type_unif(node->eval_ty, pointee_ty))
                  {
                    success = compile_error(node->src_loc, "type error (un expr)");
                  }
                }
                else
                  success = compile_error(operand->src_loc, "pointer type expected");
              }
              break;
            case eOperator_address_of:
              {
                if(operand->eval_ty->kind == eType_array)
                {
                  // ptr(T) == ptr(array(T))
                  success = type_unif(node->eval_ty, new_pointer_type(operand->eval_ty->array.elem));
                }
                else
                {
                  success = type_unif(node->eval_ty, new_pointer_type(operand->eval_ty));
                }
                if(!success)
                {
                  compile_error(node->src_loc, "type error (un expr)");
                }
              }
              break;
            default:
              assert(0);
          }

          assert(node->ty->kind == eType_proc);
          if(!type_unif(node->ty->proc.ret, node->eval_ty))
          {
            success = compile_error(node->src_loc, "type error (un expr)");
          }
        }
      }
      break;

    case eAstNode_proc:
      {
        if(!node->proc.is_extern)
        {
          success = eval_types(node->proc.body);
        }
      }
      break;

    case eAstNode_call:
      {
        for(ListItem* list_item = node->call.actual_args->first;
            list_item && success;
            list_item = list_item->next)
        {
          success = eval_types(ITEM(list_item, ast_node));
        }
        if(!success)
          break;

        AstNode* proc = node->call.proc;
        if(!proc)
        {
          Symbol* occur_sym = node->call.occur_sym;
          Symbol* decl_sym = lookup_decl_by_kind(occur_sym->name, occur_sym->scope,
              (eSymbol[]){eSymbol_proc, eSymbol_extern_proc, eSymbol_None});
          if(decl_sym)
          {
            proc = node->call.proc = decl_sym->ast_node;
          }
          else
            success = compile_error(occur_sym->src_loc, "unknown proc `%s`", occur_sym->name);
        }
        if(!success)
          break;

        Type* args_ty = basic_type_void;
        ListItem* list_item = node->call.actual_args->first;
        if(list_item)
        {
          AstNode* arg = ITEM(list_item, ast_node);
          args_ty = arg->eval_ty;

          for(list_item = list_item->next;
              list_item;
              list_item = list_item->next)
          {
            AstNode* arg = ITEM(list_item, ast_node);
            args_ty = new_product_type(args_ty, arg->eval_ty);
          }
        }

        Type* ret_ty = proc->ty->proc.ret;
        Type* occur_ty = node->ty; assert(occur_ty->kind == eType_proc);
        Type* decl_ty = proc->ty; assert(decl_ty->kind == eType_proc);
        if(type_unif(occur_ty->proc.args, args_ty) && type_unif(occur_ty->proc.ret, ret_ty))
        {
          if(!type_unif(decl_ty, occur_ty))
          {
            success = compile_error(node->src_loc, "type error (proc occur)");
          }
        }
        else
          success = compile_error(node->src_loc, "type error (proc occur)");
      }
      break;

    case eAstNode_ret_stmt:
      {
        AstNode* ret_expr = node->ret_stmt.ret_expr;
        Type* ret_expr_ty = basic_type_void;
        if(ret_expr)
        {
          if(success = eval_types(ret_expr))
          {
            ret_expr_ty = ret_expr->ty;
          }
        }

        if(success)
        {
          AstNode* proc = node->ret_stmt.proc;
          Type* proc_ret_ty = proc->ty->proc.ret;
          if(!type_unif(ret_expr_ty, proc_ret_ty))
          {
            success = compile_error(node->src_loc, "type error (return stmt)");
          }
        }
      }
      break;

    case eAstNode_while_stmt:
      {
        AstNode* cond_expr = node->while_stmt.cond_expr;
        if(success = eval_types(cond_expr) && eval_types(node->while_stmt.body))
        {
          if(!type_unif(cond_expr->ty, basic_type_bool))
          {
            success = compile_error(cond_expr->src_loc, "bool type expected");
          }
        }
      }
      break;

    case eAstNode_if_stmt:
      {
        AstNode* body = node->if_stmt.body;
        AstNode* else_body = node->if_stmt.else_body;

        if(success = eval_types(body))
        {
          if(else_body)
          {
            success = eval_types(else_body);
          }
        }

        AstNode* cond_expr = node->if_stmt.cond_expr;
        if(success && (success = eval_types(cond_expr)))
        {
          if(!type_unif(cond_expr->eval_ty, basic_type_bool))
          {
            success = compile_error(cond_expr->src_loc, "bool type expected");
          }
        }
      }
      break;

    case eAstNode_var:
    //case eAstNode_var_occur:
    case eAstNode_lit:
    case eAstNode_type:
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    case eAstNode_asm_block:
      break;

    default:
      assert(0);
  }
  return success;
}

bool node_resolve_type(AstNode* node)
{
  bool success = true;

  if(success = resolve_type(node->ty, &node->ty))
  {
    compute_type_width(node->ty);
    if(success = resolve_type(node->eval_ty, &node->eval_ty))
    {
      compute_type_width(node->eval_ty);
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

  switch(node->kind)
  {
    case eAstNode_module:
      success = resolve_types(node->module.body);
      break;

    case eAstNode_block:
      {
        for(ListItem* list_item = node->block.nodes->first;
            list_item && success;
            list_item = list_item->next)
        {
          success = resolve_types(ITEM(list_item, ast_node));
        }
      }
      break;

    case eAstNode_stmt:
      if(node->stmt.stmt)
      {
        success = resolve_types(node->stmt.stmt);
      }
      break;

    case eAstNode_var:
      if(success = node_resolve_type(node))
      {
        node->var.decl_sym->ty = node->ty;
      }
      break;

#if 0
    case eAstNode_var_occur:
      {
        if(success = node_resolve_type(node))
        {
          ATTR(node, symbol, occur_sym)->ty = ATTR(node, type, type);
        }
      }
      break;
#endif

    case eAstNode_bin_expr:
      success = resolve_types(node->bin_expr.left_operand)
        && resolve_types(node->bin_expr.right_operand)
        && node_resolve_type(node);
      break;

    case eAstNode_un_expr:
      success = resolve_types(node->un_expr.operand) && node_resolve_type(node);
      break;

    case eAstNode_proc:
      {
        for(ListItem* list_item = node->proc.formal_args->first;
            list_item && success;
            list_item = list_item->next)
        {
          success = resolve_types(ITEM(list_item, ast_node));
        }
        if(!success)
          break;

        if(success = node_resolve_type(node))
        {
          node->proc.decl_sym->ty = node->ty;

          AstNode* ret_var = node->proc.ret_var;
          if(success = node_resolve_type(ret_var))
          {
            node->id.decl_sym->ty = ret_var->ty;

            if(!node->proc.is_extern)
            {
              AstNode* body = node->proc.body;
              success = resolve_types(body) && node_resolve_type(body);
            }
          }
        }
      }
      break;

    case eAstNode_call:
      {
        for(ListItem* list_item = node->call.actual_args->first;
            list_item && success;
            list_item = list_item->next)
        {
          success = resolve_types(ITEM(list_item, ast_node));
        }
        if(success && (success = node_resolve_type(node)))
        {
          node->call.occur_sym->ty = node->ty;
        }
      }
      break;

    case eAstNode_ret_stmt:
      if(success = node_resolve_type(node))
      {
        if(node->ret_stmt.ret_expr)
        {
          success = resolve_types(node->ret_stmt.ret_expr);
        }
      }
      break;

    case eAstNode_while_stmt:
      success = resolve_types(node->while_stmt.cond_expr) && resolve_types(node->while_stmt.body);
      break;

    case eAstNode_if_stmt:
      {
        success = resolve_types(node->if_stmt.cond_expr) && resolve_types(node->if_stmt.body);
        if(node->if_stmt.else_body)
        {
          success = resolve_types(node->if_stmt.else_body);
        }
      }
      break;

    case eAstNode_lit:
      success = node_resolve_type(node);
      break;

    case eAstNode_type:
      if(success = node_resolve_type(node))
      {
        node->type.decl_sym->ty = node->ty;
      }
      break;

    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    case eAstNode_asm_block:
      break;

    default:
      assert(0);
  }
  return success;
}

bool check_types(AstNode* node)
{
  bool success = true;

  switch(node->kind)
  {
    case eAstNode_module:
      success = check_types(node->module.body);
      break;

    case eAstNode_block:
      for(ListItem* list_item = node->block.nodes->first;
          list_item && success;
          list_item = list_item->next)
      {
        success = check_types(ITEM(list_item, ast_node));
      }
      break;

    case eAstNode_stmt:
      if(node->stmt.stmt)
      {
        success = check_types(node->stmt.stmt);
      }
      break;

    case eAstNode_var:
      if(node->ty == basic_type_void)
      {
        success = compile_error(node->src_loc, "variable type cannot be `void`");
      }
      break;

    case eAstNode_bin_expr:
      {
        AstNode* right_operand = node->bin_expr.right_operand;
        AstNode* left_operand = node->bin_expr.left_operand;

        if(success = check_types(right_operand) && check_types(left_operand))
        {
          assert(node->ty->kind == eType_proc);
          Type* args_ty = node->ty->proc.args; assert(args_ty->kind == eType_product);
          Type* ret_ty = node->ty->proc.ret;

          switch(node->bin_expr.op_kind)
          {
            case eOperator_add:
            case eOperator_sub:
            case eOperator_mul:
            case eOperator_div:
              {
                if(types_are_equal(ret_ty, basic_type_int)
                    || types_are_equal(ret_ty, basic_type_float)
                    || (types_are_equal(ret_ty, basic_type_char))
                    || (ret_ty->kind == eType_pointer))
                {
                  ;//ok
                  assert(types_are_equal(ret_ty, args_ty->product.left) && types_are_equal(args_ty->product.left, args_ty->product.right));
                }
                else
                {
                  success = compile_error(node->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                      get_operator_kind_printstr(node->bin_expr.op_kind), get_type_printstr(args_ty));
                }
              }
              break;

            case eOperator_mod:
              {
                if(types_are_equal(ret_ty, basic_type_int))
                {
                  ;//ok
                  assert(types_are_equal(ret_ty, args_ty->product.left) && types_are_equal(args_ty->product.left, args_ty->product.right));
                }
                else
                {
                  success = compile_error(node->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                      get_operator_kind_printstr(node->bin_expr.op_kind), get_type_printstr(args_ty));
                }
              }
              break;

            case eOperator_logic_and:
            case eOperator_logic_or:
            case eOperator_logic_not:
              {
                if(types_are_equal(args_ty->product.left, basic_type_bool) && types_are_equal(args_ty->product.left, args_ty->product.right))
                {
                  ;//ok
                  assert(ret_ty == basic_type_bool);
                }
                else
                  success = compile_error(node->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                      get_operator_kind_printstr(node->bin_expr.op_kind), get_type_printstr(args_ty));
              }
              break;

            case eOperator_bit_and:
            case eOperator_bit_or:
            case eOperator_bit_xor:
              if(types_are_equal(args_ty->product.left, basic_type_int) && types_are_equal(args_ty->product.right, basic_type_int))
              {
                ;//ok
              }
              else
                success = compile_error(node->src_loc, "type error (bitwise op)");
              break;

            case eOperator_bit_shift_left:
            case eOperator_bit_shift_right:
              if(types_are_equal(args_ty->product.left, basic_type_int) && types_are_equal(args_ty->product.right, basic_type_char))
              {
                ;//ok
              }
              else
                success = compile_error(node->src_loc, "type error (bitwise op)");
              break;

            case eOperator_less:
            case eOperator_less_eq:
            case eOperator_greater:
            case eOperator_greater_eq:
            case eOperator_eq:
            case eOperator_not_eq:
              {
                if(types_are_equal(args_ty->product.left, basic_type_int)
                    || types_are_equal(args_ty->product.left, basic_type_char)
                    || types_are_equal(args_ty->product.left, basic_type_float)
                    && types_are_equal(args_ty->product.left, args_ty->product.right))
                {
                  ;//ok
                  assert(types_are_equal(ret_ty, basic_type_bool));
                }
                else
                {
                  success = compile_error(node->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                      get_operator_kind_printstr(node->bin_expr.op_kind), get_type_printstr(args_ty));
                }
              }
              break;

            case eOperator_assign:
              {
                ;//ok
                assert(types_are_equal(args_ty->product.left, args_ty->product.right) && types_are_equal(ret_ty, args_ty->product.left));
              }
              break;

            case eOperator_index:
              {
                if(ret_ty->width > 0)
                {
                  ;//ok
                }
                else
                  success = compile_error(node->src_loc, "type error (array index): type size = 0");
              }
              break;

            case eOperator_cast:
              {
                if(!types_are_equal(args_ty->product.left, args_ty->product.right))
                {
                  success = false;

                  if(types_are_equal(args_ty->product.left, basic_type_int))
                  {
                    // int <- float | bool | pointer | char
                    success = types_are_equal(args_ty->product.right, basic_type_float)
                      || types_are_equal(args_ty->product.right, basic_type_bool)
                      || types_are_equal(args_ty->product.right, basic_type_char)
                      || (args_ty->product.right->kind == eType_pointer);
                  }
                  else if(types_are_equal(args_ty->product.left, basic_type_char))
                  {
                    // char <- int
                    success = types_are_equal(args_ty->product.right, basic_type_int);
                  }
                  else if(types_are_equal(args_ty->product.left, basic_type_float))
                  {
                    // float <- int
                    success = types_are_equal(args_ty->product.right, basic_type_int);
                  }
                  else if(args_ty->product.left->kind == eType_pointer)
                  {
                    // pointer <- pointer | array | int
                    success = (args_ty->product.right->kind == eType_pointer)
                      || (args_ty->product.right->kind == eType_array)
                      || types_are_equal(args_ty->product.right, basic_type_int);
                  }
                  else if(args_ty->product.left->kind == eType_array)
                  {
                    // array <- pointer | array
                    success = (args_ty->product.right->kind == eType_pointer) || (args_ty->product.right->kind == eType_array);
                  }

                  if(!success)
                  {
                    compile_error(node->src_loc, "invalid cast `%s` <- `%s`",
                        get_type_printstr(args_ty->product.left), get_type_printstr(args_ty->product.right));
                  }
                }
              }
              break;

            default:
              assert(0);
          }
        }
      }
      break;

    case eAstNode_un_expr:
      success = check_types(node->un_expr.operand);
      break;

    case eAstNode_proc:
      {
        for(ListItem* list_item = node->proc.formal_args->first;
            list_item && success;
            list_item = list_item->next)
        {
          success = check_types(ITEM(list_item, ast_node));
        }
        if(!success)
          break;

        if(!node->proc.is_extern)
        {
          success = check_types(node->proc.body);
        }
      }
      break;

    case eAstNode_call:
      {
        for(ListItem* list_item = node->call.actual_args->first;
            list_item && success;
            list_item = list_item->next)
        {
          success = check_types(ITEM(list_item, ast_node));
        }
      }
      break;

    case eAstNode_ret_stmt:
      {
        if(node->ret_stmt.ret_expr)
        {
          success = check_types(node->ret_stmt.ret_expr);
        }
      }
      break;

    case eAstNode_while_stmt:
      {
        success = check_types(node->while_stmt.cond_expr) && check_types(node->while_stmt.body);
      }
      break;

    case eAstNode_if_stmt:
      {
        success = check_types(node->if_stmt.cond_expr) && check_types(node->if_stmt.body);
        if(node->if_stmt.else_body)
        {
          success = check_types(node->if_stmt.else_body);
        }
      }
      break;

    //case eAstNode_var_occur:
    case eAstNode_lit:
    case eAstNode_type:
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    case eAstNode_asm_block:
      break;

    default:
      assert(0);
  }
  return success;
}

