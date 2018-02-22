Scope* find_scope(Scope* scope, eScope kind)
{
  while(scope)
  {
    if(scope->kind == kind)
      break;
    scope = scope->encl_scope;
  }

  return scope;
}

Symbol* lookup_symbol(Scope* scope, char* name)
{
  Symbol* result = 0;

  ListItem* li = scope->decl_syms.last;
  while(li)
  {
    Symbol* symbol = KIND(li, eList_symbol)->symbol;
    if(cstr_match(symbol->name, name))
    {
      result = symbol;
      break;
    }
    li = li->prev;
  }

  return result;
}

Symbol* lookup_decl_symbol(Scope* scope, char* name)
{
  Symbol* result = 0;

  while(!result && scope)
  {
    result = lookup_symbol(scope, name);
    scope = scope->encl_scope;
  }

  return result;
}

void IrContext::alloc_data_object(Symbol* sym, Scope* scope)
{
  sym->data_loc = scope->allocd_size;
  sym->allocd_size = sym->ty->width;
  if((sym->allocd_size & (data_alignment-1)) != 0)
  {
    sym->allocd_size = (sym->allocd_size + data_alignment) & ~(data_alignment-1);
  }
  scope->allocd_size += sym->allocd_size;
}

void IrContext::alloc_data_object_incremental(Symbol* sym, Scope* scope)
{
  sym->data_loc = current_alloc_offset;

  sym->allocd_size = sym->ty->width;
  if((sym->allocd_size & (data_alignment-1)) != 0)
  {
    sym->allocd_size = (sym->allocd_size + data_alignment) & ~(data_alignment-1);
  }

  scope->allocd_size += sym->allocd_size;
  current_alloc_offset += sym->allocd_size;
}

void IrContext::alloc_scope_data_objects(Scope* scope)
{
  for(ListItem* li = scope->decl_syms.first;
      li;
      li = li->next)
  {
    Symbol* object = KIND(li, eList_symbol)->symbol;
    alloc_data_object(object, scope);
  }
}

void init_object_locations(Symbol* obj)
{
  for(int i = 0; i < sizeof_array(obj->locations._); i++)
  {
    obj->locations._[i] = 0;
  }
}

void init_symbol_pass(SymbolPass* pass, MemoryArena* gp_arena, MemoryArena* sym_arena, TypePass* type_pass)
{
  pass->basic_type_bool  = type_pass->basic_type_bool;
  pass->basic_type_int   = type_pass->basic_type_int;
  pass->basic_type_char  = type_pass->basic_type_char;
  pass->basic_type_float = type_pass->basic_type_float;
  pass->basic_type_void  = type_pass->basic_type_void;
  pass->basic_type_str   = type_pass->basic_type_str;

  pass->gp_arena = gp_arena;
  pass->sym_arena = sym_arena;
  pass->nesting_depth = -1;
  pass->data_alignment = 4;
  list_init(&pass->scopes, pass->sym_arena, eList_scope);
}

Symbol* new_const_object(SymbolPass* pass, Type* ty, SourceLoc* src_loc)
{
  Symbol* sym = push_struct(pass->sym_arena, Symbol);

  sym->kind = eSymbol_constant;
  sym->name = gen_tempvar_name(pass->gp_arena, "const_");
  sym->src_loc = src_loc;
  sym->ty = ty;
  sym->scope = 0;
  sym->order_nr = 0;
  sym->storage_space = eStorageSpace_static;
  sym->next_use = NextUse_None;
  sym->is_live_on_exit = true;
  sym->is_live = false;
  init_object_locations(sym);

  add_object_to_memory(pass->x86_context, sym);

  return sym;
}

Symbol* new_const_int_object(SymbolPass* pass, SourceLoc* src_loc, int int_val)
{
  Symbol* const_object = new_const_object(pass, pass->basic_type_int, src_loc);
  const_object->int_val = int_val;
  const_object->data = &const_object->int_val;

  return const_object;
}

Symbol* new_const_char_object(SymbolPass* pass, SourceLoc* src_loc, char char_val)
{
  Symbol* const_object = new_const_object(pass, pass->basic_type_char, src_loc);
  const_object->char_val = char_val;
  const_object->data = &const_object->char_val;

  return const_object;
}

Symbol* new_const_str_object(SymbolPass* pass, SourceLoc* src_loc, char* str_val)
{
  Symbol* const_object = new_const_object(pass, pass->basic_type_str, src_loc);
  const_object->str_val = str_val;
  const_object->data = const_object->str_val;

  const_object->scope = pass->module_scope;
  list_append(&pass->module_scope->decl_syms, const_object, eList_symbol);

  return const_object;
}

Symbol* new_const_float_object(SymbolPass* pass, SourceLoc* src_loc, float float_val)
{
  Symbol* const_object = new_const_object(pass, pass->basic_type_float, src_loc);
  const_object->float_val = float_val;
  const_object->data = &const_object->float_val;

  const_object->scope = pass->module_scope;
  list_append(&pass->module_scope->decl_syms, const_object, eList_symbol);

  return const_object;
}

Symbol* IrContext::create_temp_object(Scope* scope, Type* ty, SourceLoc* src_loc)
{
  Symbol* sym = push_struct(sym_pass->sym_arena, Symbol);

  sym->name = gen_tempvar_name(gp_arena, "temp_");
  sym->src_loc = src_loc;
  sym->ty = ty;
  sym->scope = scope;
  sym->order_nr = scope->sym_count++;
  sym->storage_space = eStorageSpace_local;
  sym->next_use = NextUse_None;
  sym->is_live_on_exit = false;
  sym->is_live = false;
  init_object_locations(sym);

  alloc_data_object_incremental(sym, scope);
  list_append(&scope->decl_syms, sym, eList_symbol);

  return sym;
}

Symbol* add_decl_symbol(SymbolPass* pass, char* name, eStorageSpace storage_space, Scope* scope, AstNode* ast_node)
{
  Symbol* sym = push_struct(pass->sym_arena, Symbol);

  sym->name = name;
  sym->src_loc = ast_node->src_loc;
  sym->scope = scope;
  sym->ast_node = ast_node;
  sym->order_nr = scope->sym_count++;
  sym->storage_space = storage_space;
  sym->next_use = NextUse_None;
  sym->is_live_on_exit = true;
  sym->is_live = true;
  init_object_locations(sym);

  list_append(&scope->decl_syms, sym, eList_symbol);

  return sym;
}

Scope* begin_scope(SymbolPass* pass, eScope kind, AstNode* ast_node)
{
  Scope* scope = push_struct(pass->sym_arena, Scope);

  scope->kind = kind;
  scope->nesting_depth = pass->nesting_depth;
  scope->sym_count = 0;
  scope->allocd_size = 0;
  scope->encl_scope = pass->active_scope;
  scope->ast_node = ast_node;
  list_init(&scope->decl_syms, pass->gp_arena, eList_symbol);
  pass->active_scope = scope;
  list_append(&pass->scopes, scope, eList_scope);

  return scope;
}

void end_scope(SymbolPass* pass)
{
  Scope* scope = pass->active_scope;
  pass->active_scope = scope->encl_scope;
}

Scope* begin_nested_scope(SymbolPass* pass, eScope kind, AstNode* ast_node)
{
  pass->nesting_depth++;
  return begin_scope(pass, kind, ast_node);
}

void end_nested_scope(SymbolPass* pass)
{
  end_scope(pass);
  pass->nesting_depth--;
}

bool SymbolPass_visit_expr(SymbolPass* pass, AstNode* expr);

bool SymbolPass_visit_formal_arg(SymbolPass* pass, Scope* proc_scope, AstNode* arg)
{
  assert(KIND(arg, eAstNode_var));
  bool success = true;
  
  Symbol* decl_sym = lookup_decl_symbol(proc_scope, arg->var.name);
  if(decl_sym && (decl_sym->scope == proc_scope))
  {
    success = compile_error(pass->gp_arena, arg->src_loc, "formal arg `%s` has already been declared", arg->var.name);
    compile_error(pass->gp_arena, decl_sym->src_loc, "see the declaration of `%s`", arg->var.name);
  }
  else
  {
    arg->var.decl_sym = add_decl_symbol(pass, arg->var.name, eStorageSpace_formal_param, proc_scope, arg);
    success = SymbolPass_visit_expr(pass, arg->var.type);
  }

  return success;
}

bool SymbolPass_visit_var(SymbolPass* pass, AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  Symbol* decl_sym = lookup_decl_symbol(pass->active_scope, var->var.name);
  Scope* preamble_scope = find_scope(pass->active_scope, eScope_args);
  if(decl_sym && (decl_sym->scope == pass->active_scope || decl_sym->scope == preamble_scope))
  {
    success = compile_error(pass->gp_arena, var->src_loc, "name `%s` already declared", var->var.name);
    compile_error(pass->gp_arena, decl_sym->src_loc, "see declaration of `%s`", var->var.name);
  }
  else
  {
    var->var.decl_sym = add_decl_symbol(pass, var->var.name, eStorageSpace_local, pass->active_scope, var);
    success = SymbolPass_visit_expr(pass, var->var.type)
      && (var->var.init_expr ? SymbolPass_visit_expr(pass, var->var.init_expr) : true);
  }

  return success;
}

bool SymbolPass_visit_lit(SymbolPass* pass, AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));
  bool success = true;

  switch(lit->lit.kind)
  {
    case eLiteral_int:
    {
      lit->lit.constant = new_const_int_object(pass, lit->src_loc, lit->lit.int_val);
    }
    break;

    case eLiteral_float:
    {
      lit->lit.constant = new_const_float_object(pass, lit->src_loc, lit->lit.float_val);
    }
    break;

    case eLiteral_bool:
    {
      lit->lit.constant = new_const_int_object(pass, lit->src_loc, (int)lit->lit.bool_val);
    }
    break;

    case eLiteral_char:
    {
      lit->lit.constant = new_const_char_object(pass, lit->src_loc, lit->lit.char_val);
    }
    break;
    
    case eLiteral_str:
    {
      lit->lit.constant = new_const_str_object(pass, lit->src_loc, lit->lit.str_val);
    }
    break;

    default: assert(0);
  }

  return success;
}

bool SymbolPass_visit_id(SymbolPass* pass, AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;

  Scope* scope = pass->active_scope;
  id->id.scope = scope;
  id->id.order_nr = scope->sym_count++;

  return success;
}

bool SymbolPass_visit_bin_expr(SymbolPass* pass, AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;
  
  success = SymbolPass_visit_expr(pass, bin_expr->bin_expr.left_operand) && SymbolPass_visit_expr(pass, bin_expr->bin_expr.right_operand);

  return success;
}

bool SymbolPass_visit_unr_expr(SymbolPass* pass, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));

  bool success = true;
  success = SymbolPass_visit_expr(pass, unr_expr->unr_expr.operand);

  return success;
}

bool SymbolPass_visit_actual_args(SymbolPass* pass, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = SymbolPass_visit_expr(pass, arg->call_arg.expr);
  }

  return success;
}

bool SymbolPass_visit_call(SymbolPass* pass, AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  bool success = true;
  
  AstNode* call_expr = call->call.expr;
  AstNode* args = call->call.args;

  if(call_expr->kind == eAstNode_id)
  {
    if(success = SymbolPass_visit_id(pass, call_expr) && SymbolPass_visit_actual_args(pass, args))
    {
      call->call.param_scope = begin_scope(pass, eScope_params, call);
      call->call.retvar = add_decl_symbol(pass, gen_tempvar_name(pass->gp_arena, "ret_"),
                                   eStorageSpace_actual_param, call->call.param_scope, call);

      for(ListItem* li = args->args.node_list.first;
          li;
          li = li->next)
      {
        AstNode* arg = KIND(li, eList_ast_node)->ast_node;
        arg->call_arg.param = add_decl_symbol(pass, gen_tempvar_name(pass->gp_arena, "param_"),
                                       eStorageSpace_actual_param, call->call.param_scope, arg);
      }

      end_scope(pass);
    }
  }
  else
  {
    success = compile_error(pass->gp_arena, call_expr->src_loc, "unsupported call expression");
  }

  return success;
}

bool SymbolPass_visit_index(SymbolPass* pass, AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;
  
  AstNode* array_expr = index->index.array_expr;
  if(array_expr->kind == eAstNode_id || array_expr->kind == eAstNode_index)
  {
    success = SymbolPass_visit_expr(pass, array_expr) && SymbolPass_visit_expr(pass, index->index.i_expr);
  }
  else
    success = compile_error(pass->gp_arena, array_expr->src_loc, "unsupported index expr");

  return success;
}

bool SymbolPass_visit_cast(SymbolPass* pass, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));

  bool success = true;
  success = SymbolPass_visit_expr(pass, cast->cast.to_type) && SymbolPass_visit_expr(pass, cast->cast.from_expr);
  return success;
}

bool SymbolPass_visit_array(SymbolPass* pass, AstNode* array)
{
  assert(KIND(array, eAstNode_array));
  bool success = true;

  AstNode* size_expr = array->array.size_expr;
  if(size_expr->kind == eAstNode_lit)
  {
    int size_val = size_expr->lit.int_val;
    if(size_val > 0)
    {
      size_expr->lit.constant = new_const_int_object(pass, size_expr->src_loc, size_val);
    }
    else if(size_val == 0)
      success = compile_error(pass->gp_arena, size_expr->src_loc, "array of 0 size");
  }
  else
    success = compile_error(pass->gp_arena, size_expr->src_loc, "unsupported size expr"); 

  return success;
}

bool SymbolPass_visit_pointer(SymbolPass* pass, AstNode* pointer)
{
  assert(KIND(pointer, eAstNode_pointer));

  bool success = true;
  success = SymbolPass_visit_expr(pass, pointer->pointer.pointee);
  return success;
}

bool SymbolPass_visit_assign(SymbolPass* pass, AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));

  bool success = true;
  success = SymbolPass_visit_expr(pass, assign->assign.dest_expr) && SymbolPass_visit_expr(pass, assign->assign.source_expr);
  
  return success;
}

bool SymbolPass_visit_expr(SymbolPass* pass, AstNode* expr)
{
  bool success = true;
  
  switch(expr->kind)
  {
    case eAstNode_cast:
    {
      success = SymbolPass_visit_cast(pass, expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = SymbolPass_visit_bin_expr(pass, expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = SymbolPass_visit_unr_expr(pass, expr);
    }
    break;
    
    case eAstNode_id:
    {
      success = SymbolPass_visit_id(pass, expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = SymbolPass_visit_call(pass, expr);
    }
    break;

    case eAstNode_array:
    {
      success = SymbolPass_visit_array(pass, expr);
    }
    break;

    case eAstNode_pointer:
    {
      success = SymbolPass_visit_pointer(pass, expr);
    }
    break;

    case eAstNode_basic_type:
    break;

    case eAstNode_lit:
    {
      success = SymbolPass_visit_lit(pass, expr);
    }
    break;
    
    case eAstNode_index:
    {
      success = SymbolPass_visit_index(pass, expr);
    }
    break;

    case eAstNode_assign:
    {
      success = SymbolPass_visit_assign(pass, expr);
    }
    break;

    default: assert(0);
  }
  return success;
}

bool SymbolPass_visit_block_stmt(SymbolPass* pass, AstNode* stmt);

bool SymbolPass_visit_if(SymbolPass* pass, AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  
  if(success = SymbolPass_visit_expr(pass, if_->if_.cond_expr) && SymbolPass_visit_block_stmt(pass, if_->if_.body))
  {
    if(success && if_->if_.else_body)
    {
      success = SymbolPass_visit_block_stmt(pass, if_->if_.else_body);
    }
  }

  return success;
}

bool SymbolPass_visit_do_while(SymbolPass* pass, AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  do_while->do_while.scope = begin_nested_scope(pass, eScope_while, do_while);
  success = SymbolPass_visit_block_stmt(pass, do_while->do_while.body) &&
    SymbolPass_visit_expr(pass, do_while->do_while.cond_expr);
  end_nested_scope(pass);

  return success;
}

bool SymbolPass_visit_while(SymbolPass* pass, AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  
  if(success = SymbolPass_visit_expr(pass, while_->while_.cond_expr))
  {
    while_->while_.scope = begin_nested_scope(pass, eScope_while, while_);
    success = SymbolPass_visit_block_stmt(pass, while_->while_.body);
    end_nested_scope(pass);
  }
  return success;
}

bool SymbolPass_visit_loop_ctrl(SymbolPass* pass, AstNode* stmt)
{
  bool success = true;
  
  Scope* loop_scope = find_scope(pass->active_scope, eScope_while);
  if(loop_scope)
  {
    stmt->loop_ctrl.loop = loop_scope->ast_node;
  }
  else
  {
    char* keyword = "???";
    if(stmt->loop_ctrl.kind == eLoopCtrl_break)
      keyword = "break";
    else if(stmt->loop_ctrl.kind == eLoopCtrl_continue)
      keyword = "continue";
    else
      assert(0);

    success = compile_error(pass->gp_arena, stmt->src_loc, "unexpected `%s`", keyword);
  }

  return success;
}

bool SymbolPass_visit_return(SymbolPass* pass, AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;
  
  Scope* proc_scope = find_scope(pass->active_scope, eScope_proc);
  if(proc_scope)
  {
    assert(KIND(proc_scope->ast_node, eAstNode_proc));

    ret->ret.proc = proc_scope->ast_node;
    if(ret->ret.expr)
    {
      success = SymbolPass_visit_expr(pass, ret->ret.expr);
    }
  }
  else
    success = compile_error(pass->gp_arena, ret->src_loc, "unexpected `return`");

  return success;
}

bool SymbolPass_visit_block(SymbolPass* pass, AstNode* block);

bool SymbolPass_visit_block_stmt(SymbolPass* pass, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_var:
    {
      success = SymbolPass_visit_var(pass, stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = SymbolPass_visit_if(pass, stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = SymbolPass_visit_do_while(pass, stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = SymbolPass_visit_while(pass, stmt);
    }
    break;
    
    case eAstNode_block:
    {
      stmt->block.scope = begin_nested_scope(pass, eScope_block, stmt);
      success = SymbolPass_visit_block(pass, stmt);
      end_nested_scope(pass);
    }
    break;
    
    case eAstNode_assign:
    {
      success = SymbolPass_visit_assign(pass, stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = SymbolPass_visit_cast(pass, stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    {
      success = SymbolPass_visit_expr(pass, stmt);
    }
    break;
    
    case eAstNode_loop_ctrl:
    {
      success = SymbolPass_visit_loop_ctrl(pass, stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = SymbolPass_visit_return(pass, stmt);
    }
    break;
    
    case eAstNode_basic_type:
    case eAstNode_empty:
    break;
    
    case eAstNode_index:
    {
      success = SymbolPass_visit_index(pass, stmt);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool SymbolPass_visit_block(SymbolPass* pass, AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = SymbolPass_visit_block_stmt(pass, stmt);
  }

  return success;
}

bool is_extern_proc(AstNode* proc)
{
  return (proc->proc.modifier & eModifier_extern) != 0;
}

bool SymbolPass_visit_proc_body(SymbolPass* pass, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  AstNode* body = proc->proc.body;

  if(is_extern_proc(proc))
  {
    if(body->kind != eAstNode_empty)
    {
      success = compile_error(pass->gp_arena, proc->src_loc, "`extern` proc `%s` must not define a body", proc->proc.name);
    }
  }
  else
  {
    if(body->kind == eAstNode_block)
    {
      body->block.scope = begin_scope(pass, eScope_block, body);
      success = SymbolPass_visit_block(pass, body);
      end_scope(pass);
    }
    else if(body->kind == eAstNode_empty)
    {
      success = compile_error(pass->gp_arena, proc->src_loc, "proc `%s` must define a body", proc->proc.name);
    }
    else assert(0);
  }

  return success;
}

bool SymbolPass_visit_formal_args(SymbolPass* pass, Scope* param_scope, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = SymbolPass_visit_formal_arg(pass, param_scope, arg);
  }

  return success;
}

bool SymbolPass_visit_module_proc(SymbolPass* pass, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  
  Symbol* decl_sym = lookup_decl_symbol(pass->active_scope, proc->proc.name);
  if(decl_sym && (decl_sym->scope == pass->active_scope))
  {
    success = compile_error(pass->gp_arena, proc->src_loc, "name `%s` has already been declared", proc->proc.name);
    compile_error(pass->gp_arena, decl_sym->src_loc, "see the declaration of `%s`", proc->proc.name);
  }
  else
  {
    proc->proc.decl_sym = add_decl_symbol(pass, proc->proc.name, eStorageSpace_None, pass->active_scope, proc);
    proc->proc.param_scope = begin_nested_scope(pass, eScope_args, proc);
    proc->proc.retvar = add_decl_symbol(pass, gen_tempvar_name(pass->gp_arena, "ret_"),
                                 eStorageSpace_formal_param, proc->proc.param_scope, proc->proc.ret_type);

    if(is_extern_proc(proc))
    {
      success = SymbolPass_visit_formal_args(pass, proc->proc.param_scope, proc->proc.args) && SymbolPass_visit_expr(pass, proc->proc.ret_type);
    }
    else
    {
      proc->proc.scope = begin_scope(pass, eScope_proc, proc);

      if(success = SymbolPass_visit_formal_args(pass, proc->proc.param_scope, proc->proc.args)
         && SymbolPass_visit_expr(pass, proc->proc.ret_type) && SymbolPass_visit_proc_body(pass, proc))
      {
        ;//ok
      }

      end_scope(pass);
    }

    end_nested_scope(pass);
  }

  return success;
}

bool SymbolPass_visit_module_var(SymbolPass* pass, AstNode* module, AstNode* var)
{
  assert(KIND(module, eAstNode_module));
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  Symbol* decl_sym = lookup_decl_symbol(pass->active_scope, var->var.name);
  if(decl_sym && (decl_sym->scope == pass->active_scope))
  {
    success = compile_error(pass->gp_arena, var->src_loc, "name `%s` already declared", var->var.name);
    compile_error(pass->gp_arena, decl_sym->src_loc, "see declaration of `%s`", var->var.name);
  }
  else
  {
    var->var.decl_sym = add_decl_symbol(pass, var->var.name, eStorageSpace_static, pass->active_scope, var);
  }

  return success;
}

bool SymbolPass_visit_module(SymbolPass* pass, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  module->module.scope = begin_nested_scope(pass, eScope_module, module);
  pass->module_scope = module->module.scope;
  
  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    switch(stmt->kind)
    {
      case eAstNode_var:
      {
        success = SymbolPass_visit_module_var(pass, module, stmt);
      }
      break;
      
      case eAstNode_proc:
      {
        success = SymbolPass_visit_module_proc(pass, stmt);
      }
      break;
      
      case eAstNode_include:
      break;
      
      default: assert(0);
    }
  }
  end_nested_scope(pass);

  assert(pass->active_scope == 0);
  assert(pass->nesting_depth == -1);

  return success;
}

