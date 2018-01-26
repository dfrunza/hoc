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

Symbol* lookup_sym(char* name, List* symbols)
{
  Symbol* result = 0;
  ListItem* li = symbols->last;
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

Symbol* lookup_decl_sym(char* name, Scope* scope)
{
  Symbol* result = 0;
  while(!result && scope)
  {
    result = lookup_sym(name, &scope->decl_syms);
    scope = scope->encl_scope;
  }

  return result;
}

void init_object_locations(Symbol* object)
{
  for(int i = 0; i < sizeof_array(object->locations._); i++)
  {
    object->locations._[i] = 0;
  }
}

void alloc_data_object(IrContext* ir_context, Symbol* sym, Scope* scope)
{
  sym->data_loc = scope->allocd_size;
  sym->allocd_size = sym->ty->width;
  if((sym->allocd_size & (ir_context->data_alignment-1)) != 0)
  {
    sym->allocd_size = (sym->allocd_size + ir_context->data_alignment) & ~(ir_context->data_alignment-1);
  }
  scope->allocd_size += sym->allocd_size;
}

void alloc_data_object_incremental(IrContext* ir_context, Symbol* sym, Scope* scope)
{
  sym->data_loc = ir_context->current_alloc_offset;

  sym->allocd_size = sym->ty->width;
  if((sym->allocd_size & (ir_context->data_alignment-1)) != 0)
  {
    sym->allocd_size = (sym->allocd_size + ir_context->data_alignment) & ~(ir_context->data_alignment-1);
  }

  scope->allocd_size += sym->allocd_size;
  ir_context->current_alloc_offset += sym->allocd_size;
}

void alloc_scope_data_objects(IrContext* ir_context, Scope* scope)
{
  for(ListItem* li = scope->decl_syms.first;
      li;
      li = li->next)
  {
    Symbol* object = KIND(li, eList_symbol)->symbol;
    alloc_data_object(ir_context, object, scope);
  }
}

Symbol* new_const_object(SymbolContext* sym_context, Type* ty, SourceLoc* src_loc)
{
  Symbol* sym = mem_push_struct(sym_context->sym_arena, Symbol);

  sym->kind = eSymbol_constant;
  sym->name = new_tempvar_name(sym_context->gp_arena, "const_");
  sym->src_loc = src_loc;
  sym->ty = ty;
  sym->scope = 0;
  sym->order_nr = 0;
  sym->storage_space = eStorageSpace_static;
  sym->next_use = NextUse_None;
  sym->is_temp = false;
  sym->is_live = false;
  init_object_locations(sym);

  return sym;
}

Symbol* new_const_object_int(SymbolContext* sym_context, SourceLoc* src_loc, int int_val)
{
  Symbol* const_object = new_const_object(sym_context, basic_type_int, src_loc);
  const_object->int_val = int_val;
  const_object->data = &const_object->int_val;

  return const_object;
}

Symbol* new_const_object_char(SymbolContext* sym_context, SourceLoc* src_loc, char char_val)
{
  Symbol* const_object = new_const_object(sym_context, basic_type_char, src_loc);
  const_object->char_val = char_val;
  const_object->data = &const_object->char_val;

  return const_object;
}

Symbol* new_const_object_str(SymbolContext* sym_context, SourceLoc* src_loc, char* str_val)
{
  Symbol* const_object = new_const_object(sym_context, basic_type_str, src_loc);
  const_object->str_val = str_val;
  const_object->data = const_object->str_val;

  Scope* module_scope = const_object->scope = sym_context->module_scope;
  append_list_elem(&module_scope->decl_syms, const_object, eList_symbol);

  return const_object;
}

Symbol* new_const_object_float(SymbolContext* sym_context, SourceLoc* src_loc, float float_val)
{
  Symbol* const_object = new_const_object(sym_context, basic_type_float, src_loc);
  const_object->float_val = float_val;
  const_object->data = &const_object->float_val;

  Scope* module_scope = const_object->scope = sym_context->module_scope;
  append_list_elem(&module_scope->decl_syms, const_object, eList_symbol);

  return const_object;
}

Symbol* new_temp_object(IrContext* ir_context, Scope* scope, Type* ty, SourceLoc* src_loc)
{
  SymbolContext* sym_context = ir_context->sym_context;
  Symbol* sym = mem_push_struct(sym_context->sym_arena, Symbol);

  sym->name = new_tempvar_name(ir_context->gp_arena, "temp_");
  sym->src_loc = src_loc;
  sym->ty = ty;
  sym->scope = scope;
  sym->order_nr = scope->sym_count++;
  sym->storage_space = eStorageSpace_local;
  sym->next_use = NextUse_None;
  sym->is_temp = true;
  sym->is_live = false;
  init_object_locations(sym);

  alloc_data_object_incremental(ir_context, sym, scope);
  append_list_elem(&scope->decl_syms, sym, eList_symbol);

  return sym;
}

Symbol* add_decl_sym(MemoryArena* arena, char* name, eStorageSpace storage_space,
                     Scope* scope, AstNode* ast_node)
{
  Symbol* sym = mem_push_struct(arena, Symbol);

  sym->name = name;
  sym->src_loc = ast_node->src_loc;
  sym->scope = scope;
  sym->ast_node = ast_node;
  sym->order_nr = scope->sym_count++;
  sym->storage_space = storage_space;
  sym->next_use = NextUse_None;
  sym->is_temp = false;
  sym->is_live = true;
  init_object_locations(sym);

  append_list_elem(&scope->decl_syms, sym, eList_symbol);

  return sym;
}

Scope* begin_scope(SymbolContext* context, eScope kind, AstNode* ast_node)
{
  Scope* scope = mem_push_struct(context->sym_arena, Scope);

  scope->kind = kind;
  scope->nesting_depth = context->nesting_depth;
  scope->sym_count = 0;
  scope->allocd_size = 0;
  scope->encl_scope = context->active_scope;
  scope->ast_node = ast_node;
  init_list(context->gp_arena, &scope->decl_syms, eList_symbol);
  context->active_scope = scope;
  append_list_elem(&context->scopes, scope, eList_scope);

  return scope;
}

void end_scope(SymbolContext* context)
{
  Scope* scope = context->active_scope;
  context->active_scope = scope->encl_scope;
}

Scope* begin_nested_scope(SymbolContext* context, eScope kind, AstNode* ast_node)
{
  context->nesting_depth++;
  return begin_scope(context, kind, ast_node);
}

void end_nested_scope(SymbolContext* context)
{
  end_scope(context);
  context->nesting_depth--;
}


#if 0
void process_includes(List* include_list, List* module_list, ListItem* module_li)
{
  for(ListItem* li = include_list->first;
      li;
      li = li->next)
  {
    AstNode* node = KIND(li, eList_ast_node)->ast_node;
    
    if(node->kind == eAstNode_include)
    {
      AstNode* block = node->include.body;
      process_includes(block->block.nodes, include_list, li);
    }
  }
  replace_li_at(include_list, module_list, module_li);
  
  mem_zero_struct(include_list, List);
}
#endif

bool sym_expr(SymbolContext* context, AstNode* expr);

bool sym_formal_arg(SymbolContext* context, Scope* proc_scope, AstNode* arg)
{
  assert(KIND(arg, eAstNode_var));
  bool success = true;
  
  Symbol* decl_sym = lookup_decl_sym(arg->var.name, proc_scope);
  if(decl_sym && (decl_sym->scope == proc_scope))
  {
    success = compile_error(context->gp_arena, arg->src_loc, "formal arg `%s` has already been declared", arg->var.name);
    compile_error(context->gp_arena, decl_sym->src_loc, "see the declaration of `%s`", arg->var.name);
  }
  else
  {
    arg->var.decl_sym = add_decl_sym(context->sym_arena, arg->var.name,
                                     eStorageSpace_arg, proc_scope, arg);
    success = sym_expr(context, arg->var.type);
  }

  return success;
}

bool sym_var(SymbolContext* context, AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  Symbol* decl_sym = lookup_decl_sym(var->var.name, context->active_scope);
  Scope* preamble_scope = find_scope(context->active_scope, eScope_args);
  if(decl_sym && (decl_sym->scope == context->active_scope || decl_sym->scope == preamble_scope))
  {
    success = compile_error(context->gp_arena, var->src_loc, "name `%s` already declared", var->var.name);
    compile_error(context->gp_arena, decl_sym->src_loc, "see declaration of `%s`", var->var.name);
  }
  else
  {
    var->var.decl_sym = add_decl_sym(context->sym_arena, var->var.name,
                                     eStorageSpace_local, context->active_scope, var);
    success = sym_expr(context, var->var.type);
  }

  return success;
}

bool sym_lit(SymbolContext* context, AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));
  bool success = true;

  switch(lit->lit.kind)
  {
    case eLiteral_int:
    {
      lit->lit.constant = new_const_object_int(context, lit->src_loc, lit->lit.int_val);
    }
    break;

    case eLiteral_float:
    {
      lit->lit.constant = new_const_object_float(context, lit->src_loc, lit->lit.float_val);
    }
    break;

    case eLiteral_bool:
    {
      lit->lit.constant = new_const_object_int(context, lit->src_loc, (int)lit->lit.bool_val);
    }
    break;

    case eLiteral_char:
    {
      lit->lit.constant = new_const_object_char(context, lit->src_loc, lit->lit.char_val);
    }
    break;
    
    case eLiteral_str:
    {
      lit->lit.constant = new_const_object_str(context, lit->src_loc, lit->lit.str_val);
    }
    break;

    default: assert(0);
  }

  return success;
}

bool sym_id(SymbolContext* context, AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;

  Scope* scope = context->active_scope;
  id->id.scope = scope;
  id->id.order_nr = scope->sym_count++;

  return success;
}

bool sym_bin_expr(SymbolContext* context, AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;
  
  success = sym_expr(context, bin_expr->bin_expr.left_operand)
    && sym_expr(context, bin_expr->bin_expr.right_operand);

  return success;
}

bool sym_unr_expr(SymbolContext* context, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));

  bool success = true;
  success = sym_expr(context, unr_expr->unr_expr.operand);

  return success;
}

bool sym_actual_args(SymbolContext* context, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = sym_expr(context, arg->actual_arg.expr);
  }

  return success;
}

bool sym_call(SymbolContext* context, AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  bool success = true;
  
  AstNode* call_expr = call->call.expr;
  AstNode* args = call->call.args;

  if(call_expr->kind == eAstNode_id)
  {
    if(success = sym_id(context, call_expr) && sym_actual_args(context, args))
    {
      call->call.param_scope = begin_scope(context, eScope_params, call);
      call->call.retvar = add_decl_sym(context->sym_arena, new_tempvar_name(context->gp_arena, "ret_"),
                                       eStorageSpace_param, call->call.param_scope, call);

      for(ListItem* li = args->node_list.first;
          li;
          li = li->next)
      {
        AstNode* arg = KIND(li, eList_ast_node)->ast_node;
        arg->actual_arg.param = add_decl_sym(context->sym_arena, new_tempvar_name(context->gp_arena, "param_"),
                                             eStorageSpace_param, call->call.param_scope, arg);
      }

      end_scope(context);
    }
  }
  else
  {
    success = compile_error(context->gp_arena, call_expr->src_loc, "unsupported call expression");
  }

  return success;
}

bool sym_index(SymbolContext* context, AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;
  
  AstNode* array_expr = index->index.array_expr;
  if(array_expr->kind == eAstNode_id || array_expr->kind == eAstNode_index)
  {
    success = sym_expr(context, array_expr) && sym_expr(context, index->index.i_expr);
  }
  else
    success = compile_error(context->gp_arena, array_expr->src_loc, "unsupported index expr");

  return success;
}

bool sym_cast(SymbolContext* context, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));

  bool success = true;
  success = sym_expr(context, cast->cast.to_type) && sym_expr(context, cast->cast.from_expr);
  return success;
}

bool sym_array(SymbolContext* context, AstNode* array)
{
  assert(KIND(array, eAstNode_array));
  bool success = true;

  AstNode* size_expr = array->array.size_expr;
  if(size_expr->kind == eAstNode_lit)
  {
#if 0
    Symbol* size_const = size_expr->lit.constant = new_const_object(context, size_expr->eval_ty, size_expr->src_loc);
    size_const->int_val = size_expr->lit.int_val;
#endif
    size_expr->lit.constant = new_const_object_int(context, size_expr->src_loc, size_expr->lit.int_val);
  }
  else assert(0);

  return success;
}

bool sym_expr(SymbolContext* context, AstNode* expr)
{
  bool success = true;
  
  switch(expr->kind)
  {
    case eAstNode_cast:
    {
      success = sym_cast(context, expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = sym_bin_expr(context, expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = sym_unr_expr(context, expr);
    }
    break;
    
    case eAstNode_id:
    {
      success = sym_id(context, expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = sym_call(context, expr);
    }
    break;

    case eAstNode_array:
    {
      success = sym_array(context, expr);
    }
    break;

    case eAstNode_pointer:
    case eAstNode_basic_type:
    break;

    case eAstNode_lit:
    {
      success = sym_lit(context, expr);
    }
    break;
    
    case eAstNode_index:
    {
      success = sym_index(context, expr);
    }
    break;

    default: assert(0);
  }
  return success;
}

bool sym_block(SymbolContext* context, AstNode* block);
bool sym_block_stmt(SymbolContext* context, AstNode* stmt);

bool sym_if(SymbolContext* context, AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  
  if(success = sym_expr(context, if_->if_.cond_expr) && sym_block_stmt(context, if_->if_.body))
  {
    if(success && if_->if_.else_body)
    {
      success = sym_block_stmt(context, if_->if_.else_body);
    }
  }

  return success;
}

bool sym_do_while(SymbolContext* context, AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  do_while->do_while.scope = begin_nested_scope(context, eScope_while, do_while);
  success = sym_block_stmt(context, do_while->do_while.body) &&
    sym_expr(context, do_while->do_while.cond_expr);
  end_nested_scope(context);

  return success;
}

bool sym_while(SymbolContext* context, AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  
  if(success = sym_expr(context, while_->while_.cond_expr))
  {
    while_->while_.scope = begin_nested_scope(context, eScope_while, while_);
    success = sym_block_stmt(context, while_->while_.body);
    end_nested_scope(context);
  }
  return success;
}

bool sym_loop_ctrl(SymbolContext* context, AstNode* stmt)
{
  bool success = true;
  
  Scope* loop_scope = find_scope(context->active_scope, eScope_while);
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

    success = compile_error(context->gp_arena, stmt->src_loc, "unexpected `%s`", keyword);
  }

  return success;
}

bool sym_return(SymbolContext* context, AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;
  
  Scope* proc_scope = find_scope(context->active_scope, eScope_proc);
  if(proc_scope)
  {
    assert(KIND(proc_scope->ast_node, eAstNode_proc));

    ret->ret.proc = proc_scope->ast_node;
    if(ret->ret.expr)
    {
      success = sym_expr(context, ret->ret.expr);
    }
  }
  else
    success = compile_error(context->gp_arena, ret->src_loc, "unexpected `return`");

  return success;
}

bool sym_assign(SymbolContext* context, AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));

  bool success = true;
  success = sym_expr(context, assign->assign.dest_expr) && sym_expr(context, assign->assign.source_expr);
  
  return success;
}

bool sym_block_stmt(SymbolContext* context, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_var:
    {
      success = sym_var(context, stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = sym_if(context, stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = sym_do_while(context, stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = sym_while(context, stmt);
    }
    break;
    
    case eAstNode_block:
    {
      stmt->block.scope = begin_nested_scope(context, eScope_block, stmt);
      success = sym_block(context, stmt);
      end_nested_scope(context);
    }
    break;
    
    case eAstNode_assign:
    {
      success = sym_assign(context, stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = sym_cast(context, stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    {
      success = sym_expr(context, stmt);
    }
    break;
    
    case eAstNode_loop_ctrl:
    {
      success = sym_loop_ctrl(context, stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = sym_return(context, stmt);
    }
    break;
    
    case eAstNode_basic_type:
    case eAstNode_empty:
    break;
    
    case eAstNode_index:
    {
      success = sym_index(context, stmt);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool sym_block(SymbolContext* context, AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = sym_block_stmt(context, stmt);
  }

  return success;
}

static inline
bool is_extern_proc(AstNode* proc)
{
  return (proc->modifier & eModifier_extern) != 0;
}

bool sym_proc_body(SymbolContext* context, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  AstNode* body = proc->proc.body;

  if(is_extern_proc(proc))
  {
    if(body->kind != eAstNode_empty)
    {
      success = compile_error(context->gp_arena, proc->src_loc, "`extern` proc `%s` must not define a body", proc->proc.name);
    }
  }
  else
  {
    if(body->kind == eAstNode_block)
    {
      body->block.scope = begin_scope(context, eScope_block, body);
      success = sym_block(context, body);
      end_scope(context);
    }
    else if(body->kind == eAstNode_empty)
    {
      success = compile_error(context->gp_arena, proc->src_loc, "proc `%s` must define a body", proc->proc.name);
    }
    else assert(0);
  }

  return success;
}

bool sym_formal_args(SymbolContext* context, Scope* param_scope, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = sym_formal_arg(context, param_scope, arg);
  }

  return success;
}

bool sym_module_proc(SymbolContext* context, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  
  Symbol* decl_sym = lookup_decl_sym(proc->proc.name, context->active_scope);
  if(decl_sym && (decl_sym->scope == context->active_scope))
  {
    success = compile_error(context->gp_arena, proc->src_loc, "name `%s` has already been declared", proc->proc.name);
    compile_error(context->gp_arena, decl_sym->src_loc, "see the declaration of `%s`", proc->proc.name);
  }
  else
  {
    proc->proc.decl_sym = add_decl_sym(context->sym_arena, proc->proc.name,
                                       eStorageSpace_None, context->active_scope, proc);
    proc->proc.param_scope = begin_nested_scope(context, eScope_args, proc);
    proc->proc.retvar = add_decl_sym(context->sym_arena, new_tempvar_name(context->gp_arena, "ret_"),
                                     eStorageSpace_arg, proc->proc.param_scope, proc->proc.ret_type);

    if(is_extern_proc(proc))
    {
      success = sym_formal_args(context, proc->proc.param_scope, proc->proc.args) && sym_expr(context, proc->proc.ret_type);
    }
    else
    {
      proc->proc.scope = begin_scope(context, eScope_proc, proc);

      if(success = sym_formal_args(context, proc->proc.param_scope, proc->proc.args)
         && sym_expr(context, proc->proc.ret_type) && sym_proc_body(context, proc))
      {
        ;//ok
      }

      end_scope(context);
    }

    end_nested_scope(context);
  }

  return success;
}

bool sym_module_var(SymbolContext* context, AstNode* module, AstNode* var)
{
  assert(KIND(module, eAstNode_module));
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  Symbol* decl_sym = lookup_decl_sym(var->var.name, context->active_scope);
  if(decl_sym && (decl_sym->scope == context->active_scope))
  {
    success = compile_error(context->gp_arena, var->src_loc, "name `%s` already declared", var->var.name);
    compile_error(context->gp_arena, decl_sym->src_loc, "see declaration of `%s`", var->var.name);
  }
  else
  {
    var->var.decl_sym = add_decl_sym(context->sym_arena, var->var.name,
                                     eStorageSpace_static, context->active_scope, var);
  }

  return success;
}

bool sym_module(SymbolContext* context, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  module->module.scope = begin_nested_scope(context, eScope_module, module);
  context->module_scope = module->module.scope;
  
  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    switch(stmt->kind)
    {
      case eAstNode_var:
      {
        success = sym_module_var(context, module, stmt);
      }
      break;
      
      case eAstNode_proc:
      {
        success = sym_module_proc(context, stmt);
      }
      break;
      
      case eAstNode_include:
      break;
      
      default: assert(0);
    }
  }
  end_nested_scope(context);

  assert(context->active_scope == 0);
  assert(context->nesting_depth == -1);

  return success;
}

