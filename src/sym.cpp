Scope* Scope::find(eScope kind)
{
  Scope* scope = this;
  while(scope)
  {
    if(scope->kind == kind)
      break;
    scope = scope->encl_scope;
  }

  return scope;
}

Symbol* Scope::lookup(char* name)
{
  Symbol* result = 0;

  ListItem* li = decl_syms.last;
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

Symbol* Scope::lookup_decl(char* name)
{
  Symbol* result = 0;
  Scope* scope = this;

  while(!result && scope)
  {
    result = scope->lookup(name);
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

void Symbol::init_locations()
{
  for(int i = 0; i < sizeof_array(locations._); i++)
  {
    locations._[i] = 0;
  }
}

void SymbolPass::init(MemoryArena* gp_arena, MemoryArena* sym_arena, TypePass* type_pass)
{
  basic_type_bool  = type_pass->basic_type_bool;
  basic_type_int   = type_pass->basic_type_int;
  basic_type_char  = type_pass->basic_type_char;
  basic_type_float = type_pass->basic_type_float;
  basic_type_void  = type_pass->basic_type_void;
  basic_type_str   = type_pass->basic_type_str;

  this->gp_arena = gp_arena;
  this->sym_arena = sym_arena;
  nesting_depth = -1;
  data_alignment = 4;
  scopes.init(sym_arena, eList_scope);
}

Symbol* SymbolPass::create_const(Type* ty, SourceLoc* src_loc)
{
  Symbol* sym = push_struct(sym_arena, Symbol);

  sym->kind = eSymbol_constant;
  sym->name = gen_tempvar_name(gp_arena, "const_");
  sym->src_loc = src_loc;
  sym->ty = ty;
  sym->scope = 0;
  sym->order_nr = 0;
  sym->storage_space = eStorageSpace_static_;
  sym->next_use = NextUse_None;
  sym->is_live_on_exit = true;
  sym->is_live = false;
  sym->init_locations();

  x86_context->add_object_to_memory(sym);

  return sym;
}

Symbol* SymbolPass::create_const_int(SourceLoc* src_loc, int int_val)
{
  Symbol* const_object = create_const(basic_type_int, src_loc);
  const_object->int_val = int_val;
  const_object->data = &const_object->int_val;

  return const_object;
}

Symbol* SymbolPass::create_const_char(SourceLoc* src_loc, char char_val)
{
  Symbol* const_object = create_const(basic_type_char, src_loc);
  const_object->char_val = char_val;
  const_object->data = &const_object->char_val;

  return const_object;
}

Symbol* SymbolPass::create_const_str(SourceLoc* src_loc, char* str_val)
{
  Symbol* const_object = create_const(basic_type_str, src_loc);
  const_object->str_val = str_val;
  const_object->data = const_object->str_val;

  const_object->scope = module_scope;
  module_scope->decl_syms.append(const_object, eList_symbol);

  return const_object;
}

Symbol* SymbolPass::create_const_float(SourceLoc* src_loc, float float_val)
{
  Symbol* const_object = create_const(basic_type_float, src_loc);
  const_object->float_val = float_val;
  const_object->data = &const_object->float_val;

  const_object->scope = module_scope;
  module_scope->decl_syms.append(const_object, eList_symbol);

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
  sym->init_locations();

  alloc_data_object_incremental(sym, scope);
  scope->decl_syms.append(sym, eList_symbol);

  return sym;
}

Symbol* SymbolPass::add_decl(char* name, eStorageSpace storage_space, Scope* scope, AstNode* ast_node)
{
  Symbol* sym = push_struct(sym_arena, Symbol);

  sym->name = name;
  sym->src_loc = ast_node->src_loc;
  sym->scope = scope;
  sym->ast_node = ast_node;
  sym->order_nr = scope->sym_count++;
  sym->storage_space = storage_space;
  sym->next_use = NextUse_None;
  sym->is_live_on_exit = true;
  sym->is_live = true;
  sym->init_locations();

  scope->decl_syms.append(sym, eList_symbol);

  return sym;
}

Scope* SymbolPass::begin_scope(eScope kind, AstNode* ast_node)
{
  Scope* scope = push_struct(sym_arena, Scope);

  scope->kind = kind;
  scope->nesting_depth = nesting_depth;
  scope->sym_count = 0;
  scope->allocd_size = 0;
  scope->encl_scope = active_scope;
  scope->ast_node = ast_node;
  scope->decl_syms.init(gp_arena, eList_symbol);
  active_scope = scope;
  scopes.append(scope, eList_scope);

  return scope;
}

void SymbolPass::end_scope()
{
  Scope* scope = active_scope;
  active_scope = scope->encl_scope;
}

Scope* SymbolPass::begin_nested_scope(eScope kind, AstNode* ast_node)
{
  nesting_depth++;
  return begin_scope(kind, ast_node);
}

void SymbolPass::end_nested_scope()
{
  end_scope();
  nesting_depth--;
}

bool SymbolPass::visit_formal_arg(Scope* proc_scope, AstNode* arg)
{
  assert(KIND(arg, eAstNode_var));
  bool success = true;
  
  Symbol* decl_sym = proc_scope->lookup_decl(arg->var.name);
  if(decl_sym && (decl_sym->scope == proc_scope))
  {
    success = compile_error(gp_arena, arg->src_loc, "formal arg `%s` has already been declared", arg->var.name);
    compile_error(gp_arena, decl_sym->src_loc, "see the declaration of `%s`", arg->var.name);
  }
  else
  {
    arg->var.decl_sym = add_decl(arg->var.name, eStorageSpace_formal_param, proc_scope, arg);
    success = visit_expr(arg->var.type);
  }

  return success;
}

bool SymbolPass::visit_var(AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  Symbol* decl_sym = active_scope->lookup_decl(var->var.name);
  Scope* preamble_scope = active_scope->find(eScope_args);
  if(decl_sym && (decl_sym->scope == active_scope || decl_sym->scope == preamble_scope))
  {
    success = compile_error(gp_arena, var->src_loc, "name `%s` already declared", var->var.name);
    compile_error(gp_arena, decl_sym->src_loc, "see declaration of `%s`", var->var.name);
  }
  else
  {
    var->var.decl_sym = add_decl(var->var.name, eStorageSpace_local, active_scope, var);
    success = visit_expr(var->var.type)
      && (var->var.init_expr ? visit_expr(var->var.init_expr) : true);
  }

  return success;
}

bool SymbolPass::visit_lit(AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));
  bool success = true;

  switch(lit->lit.kind)
  {
    case eLiteral_int:
    {
      lit->lit.constant = create_const_int(lit->src_loc, lit->lit.int_val);
    }
    break;

    case eLiteral_float:
    {
      lit->lit.constant = create_const_float(lit->src_loc, lit->lit.float_val);
    }
    break;

    case eLiteral_bool:
    {
      lit->lit.constant = create_const_int(lit->src_loc, (int)lit->lit.bool_val);
    }
    break;

    case eLiteral_char:
    {
      lit->lit.constant = create_const_char(lit->src_loc, lit->lit.char_val);
    }
    break;
    
    case eLiteral_str:
    {
      lit->lit.constant = create_const_str(lit->src_loc, lit->lit.str_val);
    }
    break;

    default: assert(0);
  }

  return success;
}

bool SymbolPass::visit_id(AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;

  Scope* scope = active_scope;
  id->id.scope = scope;
  id->id.order_nr = scope->sym_count++;

  return success;
}

bool SymbolPass::visit_bin_expr(AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;
  
  success = visit_expr(bin_expr->bin_expr.left_operand) && visit_expr(bin_expr->bin_expr.right_operand);

  return success;
}

bool SymbolPass::visit_unr_expr(AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));

  bool success = true;
  success = visit_expr(unr_expr->unr_expr.operand);

  return success;
}

bool SymbolPass::visit_actual_args(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = visit_expr(arg->call_arg.expr);
  }

  return success;
}

bool SymbolPass::visit_call(AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  bool success = true;
  
  AstNode* call_expr = call->call.expr;
  AstNode* args = call->call.args;

  if(call_expr->kind == eAstNode_id)
  {
    if(success = visit_id(call_expr) && visit_actual_args(args))
    {
      call->call.param_scope = begin_scope(eScope_params, call);
      call->call.retvar = add_decl(gen_tempvar_name(gp_arena, "ret_"),
                                   eStorageSpace_actual_param, call->call.param_scope, call);

      for(ListItem* li = args->args.node_list.first;
          li;
          li = li->next)
      {
        AstNode* arg = KIND(li, eList_ast_node)->ast_node;
        arg->call_arg.param = add_decl(gen_tempvar_name(gp_arena, "param_"),
                                       eStorageSpace_actual_param, call->call.param_scope, arg);
      }

      end_scope();
    }
  }
  else
  {
    success = compile_error(gp_arena, call_expr->src_loc, "unsupported call expression");
  }

  return success;
}

bool SymbolPass::visit_index(AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;
  
  AstNode* array_expr = index->index.array_expr;
  if(array_expr->kind == eAstNode_id || array_expr->kind == eAstNode_index)
  {
    success = visit_expr(array_expr) && visit_expr(index->index.i_expr);
  }
  else
    success = compile_error(gp_arena, array_expr->src_loc, "unsupported index expr");

  return success;
}

bool SymbolPass::visit_cast(AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));

  bool success = true;
  success = visit_expr(cast->cast.to_type) && visit_expr(cast->cast.from_expr);
  return success;
}

bool SymbolPass::visit_array(AstNode* array)
{
  assert(KIND(array, eAstNode_array));
  bool success = true;

  AstNode* size_expr = array->array.size_expr;
  if(size_expr->kind == eAstNode_lit)
  {
    int size_val = size_expr->lit.int_val;
    if(size_val > 0)
    {
      size_expr->lit.constant = create_const_int(size_expr->src_loc, size_val);
    }
    else if(size_val == 0)
      success = compile_error(gp_arena, size_expr->src_loc, "array of 0 size");
  }
  else
    success = compile_error(gp_arena, size_expr->src_loc, "unsupported size expr"); 

  return success;
}

bool SymbolPass::visit_pointer(AstNode* pointer)
{
  assert(KIND(pointer, eAstNode_pointer));

  bool success = true;
  success = visit_expr(pointer->pointer.pointee);
  return success;
}

bool SymbolPass::visit_assign(AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));

  bool success = true;
  success = visit_expr(assign->assign.dest_expr) && visit_expr(assign->assign.source_expr);
  
  return success;
}

bool SymbolPass::visit_expr(AstNode* expr)
{
  bool success = true;
  
  switch(expr->kind)
  {
    case eAstNode_cast:
    {
      success = visit_cast(expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = visit_bin_expr(expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = visit_unr_expr(expr);
    }
    break;
    
    case eAstNode_id:
    {
      success = visit_id(expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = visit_call(expr);
    }
    break;

    case eAstNode_array:
    {
      success = visit_array(expr);
    }
    break;

    case eAstNode_pointer:
    {
      success = visit_pointer(expr);
    }
    break;

    case eAstNode_basic_type:
    break;

    case eAstNode_lit:
    {
      success = visit_lit(expr);
    }
    break;
    
    case eAstNode_index:
    {
      success = visit_index(expr);
    }
    break;

    case eAstNode_assign:
    {
      success = visit_assign(expr);
    }
    break;

    default: assert(0);
  }
  return success;
}

bool SymbolPass::visit_if(AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  
  if(success = visit_expr(if_->if_.cond_expr) && visit_block_stmt(if_->if_.body))
  {
    if(success && if_->if_.else_body)
    {
      success = visit_block_stmt(if_->if_.else_body);
    }
  }

  return success;
}

bool SymbolPass::visit_do_while(AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  do_while->do_while.scope = begin_nested_scope(eScope_while, do_while);
  success = visit_block_stmt(do_while->do_while.body) &&
    visit_expr(do_while->do_while.cond_expr);
  end_nested_scope();

  return success;
}

bool SymbolPass::visit_while(AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  
  if(success = visit_expr(while_->while_.cond_expr))
  {
    while_->while_.scope = begin_nested_scope(eScope_while, while_);
    success = visit_block_stmt(while_->while_.body);
    end_nested_scope();
  }
  return success;
}

bool SymbolPass::visit_loop_ctrl(AstNode* stmt)
{
  bool success = true;
  
  Scope* loop_scope = active_scope->find(eScope_while);
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

    success = compile_error(gp_arena, stmt->src_loc, "unexpected `%s`", keyword);
  }

  return success;
}

bool SymbolPass::visit_return(AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;
  
  Scope* proc_scope = active_scope->find(eScope_proc);
  if(proc_scope)
  {
    assert(KIND(proc_scope->ast_node, eAstNode_proc));

    ret->ret.proc = proc_scope->ast_node;
    if(ret->ret.expr)
    {
      success = visit_expr(ret->ret.expr);
    }
  }
  else
    success = compile_error(gp_arena, ret->src_loc, "unexpected `return`");

  return success;
}

bool SymbolPass::visit_block_stmt(AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_var:
    {
      success = visit_var(stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = visit_if(stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = visit_do_while(stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = visit_while(stmt);
    }
    break;
    
    case eAstNode_block:
    {
      stmt->block.scope = begin_nested_scope(eScope_block, stmt);
      success = visit_block(stmt);
      end_nested_scope();
    }
    break;
    
    case eAstNode_assign:
    {
      success = visit_assign(stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = visit_cast(stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    {
      success = visit_expr(stmt);
    }
    break;
    
    case eAstNode_loop_ctrl:
    {
      success = visit_loop_ctrl(stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = visit_return(stmt);
    }
    break;
    
    case eAstNode_basic_type:
    case eAstNode_empty:
    break;
    
    case eAstNode_index:
    {
      success = visit_index(stmt);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool SymbolPass::visit_block(AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = visit_block_stmt(stmt);
  }

  return success;
}

bool AstNode_Proc::is_extern()
{
  return (modifier & eModifier_extern) != 0;
}

bool SymbolPass::visit_proc_body(AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  AstNode* body = proc->proc.body;

  if(proc->proc.is_extern())
  {
    if(body->kind != eAstNode_empty)
    {
      success = compile_error(gp_arena, proc->src_loc, "`extern` proc `%s` must not define a body", proc->proc.name);
    }
  }
  else
  {
    if(body->kind == eAstNode_block)
    {
      body->block.scope = begin_scope(eScope_block, body);
      success = visit_block(body);
      end_scope();
    }
    else if(body->kind == eAstNode_empty)
    {
      success = compile_error(gp_arena, proc->src_loc, "proc `%s` must define a body", proc->proc.name);
    }
    else assert(0);
  }

  return success;
}

bool SymbolPass::visit_formal_args(Scope* param_scope, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = visit_formal_arg(param_scope, arg);
  }

  return success;
}

bool SymbolPass::visit_module_proc(AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  
  Symbol* decl_sym = active_scope->lookup_decl(proc->proc.name);
  if(decl_sym && (decl_sym->scope == active_scope))
  {
    success = compile_error(gp_arena, proc->src_loc, "name `%s` has already been declared", proc->proc.name);
    compile_error(gp_arena, decl_sym->src_loc, "see the declaration of `%s`", proc->proc.name);
  }
  else
  {
    proc->proc.decl_sym = add_decl(proc->proc.name, eStorageSpace_None, active_scope, proc);
    proc->proc.param_scope = begin_nested_scope(eScope_args, proc);
    proc->proc.retvar = add_decl(gen_tempvar_name(gp_arena, "ret_"),
                                 eStorageSpace_formal_param, proc->proc.param_scope, proc->proc.ret_type);

    if(proc->proc.is_extern())
    {
      success = visit_formal_args(proc->proc.param_scope, proc->proc.args) && visit_expr(proc->proc.ret_type);
    }
    else
    {
      proc->proc.scope = begin_scope(eScope_proc, proc);

      if(success = visit_formal_args(proc->proc.param_scope, proc->proc.args)
         && visit_expr(proc->proc.ret_type) && visit_proc_body(proc))
      {
        ;//ok
      }

      end_scope();
    }

    end_nested_scope();
  }

  return success;
}

bool SymbolPass::visit_module_var(AstNode* module, AstNode* var)
{
  assert(KIND(module, eAstNode_module));
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  Symbol* decl_sym = active_scope->lookup_decl(var->var.name);
  if(decl_sym && (decl_sym->scope == active_scope))
  {
    success = compile_error(gp_arena, var->src_loc, "name `%s` already declared", var->var.name);
    compile_error(gp_arena, decl_sym->src_loc, "see declaration of `%s`", var->var.name);
  }
  else
  {
    var->var.decl_sym = add_decl(var->var.name, eStorageSpace_static_, active_scope, var);
  }

  return success;
}

bool SymbolPass::visit_module(AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  module->module.scope = begin_nested_scope(eScope_module, module);
  module_scope = module->module.scope;
  
  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    switch(stmt->kind)
    {
      case eAstNode_var:
      {
        success = visit_module_var(module, stmt);
      }
      break;
      
      case eAstNode_proc:
      {
        success = visit_module_proc(stmt);
      }
      break;
      
      case eAstNode_include:
      break;
      
      default: assert(0);
    }
  }
  end_nested_scope();

  assert(active_scope == 0);
  assert(nesting_depth == -1);

  return success;
}

bool SymbolPass::process(AstNode* module)
{
  return visit_module(module);
}

