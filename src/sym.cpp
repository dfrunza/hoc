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
    Symbol* symbol = KIND(li, eList::symbol)->symbol;
    if(Cstr::match(symbol->name, name))
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
    Symbol* object = KIND(li, eList::symbol)->symbol;
    alloc_data_object(ir_context, object, scope);
  }
}

void Symbol::init_locations()
{
  for(int i = 0; i < sizeof_array(locations._); i++)
  {
    locations._[i] = 0;
  }
}

Symbol* SymbolContext::create_const_object(Type* ty, SourceLoc* src_loc)
{
  Symbol* sym = mem_push_struct(sym_arena, Symbol);

  sym->kind = eSymbol::constant;
  sym->name = new_tempvar_name(gp_arena, "const_");
  sym->src_loc = src_loc;
  sym->ty = ty;
  sym->scope = 0;
  sym->order_nr = 0;
  sym->storage_space = eStorageSpace::static_;
  sym->next_use = NextUse_None;
  sym->is_live_on_exit = true;
  sym->is_live = false;
  sym->init_locations();

  add_object_to_memory(x86_context, sym);

  return sym;
}

Symbol* SymbolContext::create_const_object_int(SourceLoc* src_loc, int int_val)
{
  Symbol* const_object = create_const_object(basic_type_int, src_loc);
  const_object->int_val = int_val;
  const_object->data = &const_object->int_val;

  return const_object;
}

Symbol* SymbolContext::create_const_object_char(SourceLoc* src_loc, char char_val)
{
  Symbol* const_object = create_const_object(basic_type_char, src_loc);
  const_object->char_val = char_val;
  const_object->data = &const_object->char_val;

  return const_object;
}

Symbol* SymbolContext::create_const_object_str(SourceLoc* src_loc, char* str_val)
{
  Symbol* const_object = create_const_object(basic_type_str, src_loc);
  const_object->str_val = str_val;
  const_object->data = const_object->str_val;

  const_object->scope = module_scope;
  module_scope->decl_syms.append(const_object, eList::symbol);

  return const_object;
}

Symbol* SymbolContext::create_const_object_float(SourceLoc* src_loc, float float_val)
{
  Symbol* const_object = create_const_object(basic_type_float, src_loc);
  const_object->float_val = float_val;
  const_object->data = &const_object->float_val;

  const_object->scope = module_scope;
  module_scope->decl_syms.append(const_object, eList::symbol);

  return const_object;
}

Symbol* create_temp_object(IrContext* ir_context, Scope* scope, Type* ty, SourceLoc* src_loc)
{
  SymbolContext* sym_context = ir_context->sym_context;
  Symbol* sym = mem_push_struct(sym_context->sym_arena, Symbol);

  sym->name = new_tempvar_name(ir_context->gp_arena, "temp_");
  sym->src_loc = src_loc;
  sym->ty = ty;
  sym->scope = scope;
  sym->order_nr = scope->sym_count++;
  sym->storage_space = eStorageSpace::local;
  sym->next_use = NextUse_None;
  sym->is_live_on_exit = false;
  sym->is_live = false;
  sym->init_locations();

  alloc_data_object_incremental(ir_context, sym, scope);
  scope->decl_syms.append(sym, eList::symbol);

  return sym;
}

Symbol* SymbolContext::add_decl(char* name, eStorageSpace storage_space, Scope* scope, AstNode* ast_node)
{
  Symbol* sym = mem_push_struct(sym_arena, Symbol);

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

  scope->decl_syms.append(sym, eList::symbol);

  return sym;
}

Scope* SymbolContext::begin_scope(eScope kind, AstNode* ast_node)
{
  Scope* scope = mem_push_struct(sym_arena, Scope);

  scope->kind = kind;
  scope->nesting_depth = nesting_depth;
  scope->sym_count = 0;
  scope->allocd_size = 0;
  scope->encl_scope = active_scope;
  scope->ast_node = ast_node;
  scope->decl_syms.init(gp_arena, eList::symbol);
  active_scope = scope;
  scopes.append(scope, eList::scope);

  return scope;
}

void SymbolContext::end_scope()
{
  Scope* scope = active_scope;
  active_scope = scope->encl_scope;
}

Scope* SymbolContext::begin_nested_scope(eScope kind, AstNode* ast_node)
{
  nesting_depth++;
  return begin_scope(kind, ast_node);
}

void SymbolContext::end_nested_scope()
{
  end_scope();
  nesting_depth--;
}

bool SymbolContext::sym_formal_arg(Scope* proc_scope, AstNode* arg)
{
  assert(KIND(arg, eAstNode::var));
  bool success = true;
  
  Symbol* decl_sym = proc_scope->lookup_decl(arg->var.name);
  if(decl_sym && (decl_sym->scope == proc_scope))
  {
    success = compile_error(gp_arena, arg->src_loc, "formal arg `%s` has already been declared", arg->var.name);
    compile_error(gp_arena, decl_sym->src_loc, "see the declaration of `%s`", arg->var.name);
  }
  else
  {
    arg->var.decl_sym = add_decl(arg->var.name, eStorageSpace::formal_param, proc_scope, arg);
    success = sym_expr(arg->var.type);
  }

  return success;
}

bool SymbolContext::sym_var(AstNode* var)
{
  assert(KIND(var, eAstNode::var));
  bool success = true;
  
  Symbol* decl_sym = active_scope->lookup_decl(var->var.name);
  Scope* preamble_scope = active_scope->find(eScope::args);
  if(decl_sym && (decl_sym->scope == active_scope || decl_sym->scope == preamble_scope))
  {
    success = compile_error(gp_arena, var->src_loc, "name `%s` already declared", var->var.name);
    compile_error(gp_arena, decl_sym->src_loc, "see declaration of `%s`", var->var.name);
  }
  else
  {
    var->var.decl_sym = add_decl(var->var.name, eStorageSpace::local, active_scope, var);
    success = sym_expr(var->var.type)
      && (var->var.init_expr ? sym_expr(var->var.init_expr) : true);
  }

  return success;
}

bool SymbolContext::sym_lit(AstNode* lit)
{
  assert(KIND(lit, eAstNode::lit));
  bool success = true;

  switch(lit->lit.kind)
  {
    case eLiteral::int_:
    {
      lit->lit.constant = create_const_object_int(lit->src_loc, lit->lit.int_val);
    }
    break;

    case eLiteral::float_:
    {
      lit->lit.constant = create_const_object_float(lit->src_loc, lit->lit.float_val);
    }
    break;

    case eLiteral::bool_:
    {
      lit->lit.constant = create_const_object_int(lit->src_loc, (int)lit->lit.bool_val);
    }
    break;

    case eLiteral::char_:
    {
      lit->lit.constant = create_const_object_char(lit->src_loc, lit->lit.char_val);
    }
    break;
    
    case eLiteral::str:
    {
      lit->lit.constant = create_const_object_str(lit->src_loc, lit->lit.str_val);
    }
    break;

    default: assert(0);
  }

  return success;
}

bool SymbolContext::sym_id(AstNode* id)
{
  assert(KIND(id, eAstNode::id));
  bool success = true;

  Scope* scope = active_scope;
  id->id.scope = scope;
  id->id.order_nr = scope->sym_count++;

  return success;
}

bool SymbolContext::sym_bin_expr(AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode::bin_expr));
  bool success = true;
  
  success = sym_expr(bin_expr->bin_expr.left_operand)
    && sym_expr(bin_expr->bin_expr.right_operand);

  return success;
}

bool SymbolContext::sym_unr_expr(AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode::unr_expr));

  bool success = true;
  success = sym_expr(unr_expr->unr_expr.operand);

  return success;
}

bool SymbolContext::sym_actual_args(AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList::ast_node)->ast_node;
    success = sym_expr(arg->call_arg.expr);
  }

  return success;
}

bool SymbolContext::sym_call(AstNode* call)
{
  assert(KIND(call, eAstNode::call));
  bool success = true;
  
  AstNode* call_expr = call->call.expr;
  AstNode* args = call->call.args;

  if(call_expr->kind == eAstNode::id)
  {
    if(success = sym_id(call_expr) && sym_actual_args(args))
    {
      call->call.param_scope = begin_scope(eScope::params, call);
      call->call.retvar = add_decl(new_tempvar_name(gp_arena, "ret_"),
                                       eStorageSpace::actual_param, call->call.param_scope, call);

      for(ListItem* li = args->args.node_list.first;
          li;
          li = li->next)
      {
        AstNode* arg = KIND(li, eList::ast_node)->ast_node;
        arg->call_arg.param = add_decl(new_tempvar_name(gp_arena, "param_"),
                                           eStorageSpace::actual_param, call->call.param_scope, arg);
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

bool SymbolContext::sym_index(AstNode* index)
{
  assert(KIND(index, eAstNode::index));
  bool success = true;
  
  AstNode* array_expr = index->index.array_expr;
  if(array_expr->kind == eAstNode::id || array_expr->kind == eAstNode::index)
  {
    success = sym_expr(array_expr) && sym_expr(index->index.i_expr);
  }
  else
    success = compile_error(gp_arena, array_expr->src_loc, "unsupported index expr");

  return success;
}

bool SymbolContext::sym_cast(AstNode* cast)
{
  assert(KIND(cast, eAstNode::cast));

  bool success = true;
  success = sym_expr(cast->cast.to_type) && sym_expr(cast->cast.from_expr);
  return success;
}

bool SymbolContext::sym_array(AstNode* array)
{
  assert(KIND(array, eAstNode::array));
  bool success = true;

  AstNode* size_expr = array->array.size_expr;
  if(size_expr->kind == eAstNode::lit)
  {
    int size_val = size_expr->lit.int_val;
    if(size_val > 0)
    {
      size_expr->lit.constant = create_const_object_int(size_expr->src_loc, size_val);
    }
    else if(size_val == 0)
      success = compile_error(gp_arena, size_expr->src_loc, "array of 0 size");
  }
  else
    success = compile_error(gp_arena, size_expr->src_loc, "unsupported size expr"); 

  return success;
}

bool SymbolContext::sym_pointer(AstNode* pointer)
{
  assert(KIND(pointer, eAstNode::pointer));

  bool success = true;
  success = sym_expr(pointer->pointer.pointee);
  return success;
}

bool SymbolContext::sym_assign(AstNode* assign)
{
  assert(KIND(assign, eAstNode::assign));

  bool success = true;
  success = sym_expr(assign->assign.dest_expr) && sym_expr(assign->assign.source_expr);
  
  return success;
}

bool SymbolContext::sym_expr(AstNode* expr)
{
  bool success = true;
  
  switch(expr->kind)
  {
    case eAstNode::cast:
    {
      success = sym_cast(expr);
    }
    break;
    
    case eAstNode::bin_expr:
    {
      success = sym_bin_expr(expr);
    }
    break;
    
    case eAstNode::unr_expr:
    {
      success = sym_unr_expr(expr);
    }
    break;
    
    case eAstNode::id:
    {
      success = sym_id(expr);
    }
    break;
    
    case eAstNode::call:
    {
      success = sym_call(expr);
    }
    break;

    case eAstNode::array:
    {
      success = sym_array(expr);
    }
    break;

    case eAstNode::pointer:
    {
      success = sym_pointer(expr);
    }
    break;

    case eAstNode::basic_type:
    break;

    case eAstNode::lit:
    {
      success = sym_lit(expr);
    }
    break;
    
    case eAstNode::index:
    {
      success = sym_index(expr);
    }
    break;

    case eAstNode::assign:
    {
      success = sym_assign(expr);
    }
    break;

    default: assert(0);
  }
  return success;
}

bool SymbolContext::sym_if(AstNode* if_)
{
  assert(KIND(if_, eAstNode::if_));
  bool success = true;
  
  if(success = sym_expr(if_->if_.cond_expr) && sym_block_stmt(if_->if_.body))
  {
    if(success && if_->if_.else_body)
    {
      success = sym_block_stmt(if_->if_.else_body);
    }
  }

  return success;
}

bool SymbolContext::sym_do_while(AstNode* do_while)
{
  assert(KIND(do_while, eAstNode::do_while));
  bool success = true;

  do_while->do_while.scope = begin_nested_scope(eScope::while_, do_while);
  success = sym_block_stmt(do_while->do_while.body) &&
    sym_expr(do_while->do_while.cond_expr);
  end_nested_scope();

  return success;
}

bool SymbolContext::sym_while(AstNode* while_)
{
  assert(KIND(while_, eAstNode::while_));
  bool success = true;
  
  if(success = sym_expr(while_->while_.cond_expr))
  {
    while_->while_.scope = begin_nested_scope(eScope::while_, while_);
    success = sym_block_stmt(while_->while_.body);
    end_nested_scope();
  }
  return success;
}

bool SymbolContext::sym_loop_ctrl(AstNode* stmt)
{
  bool success = true;
  
  Scope* loop_scope = active_scope->find(eScope::while_);
  if(loop_scope)
  {
    stmt->loop_ctrl.loop = loop_scope->ast_node;
  }
  else
  {
    char* keyword = "???";
    if(stmt->loop_ctrl.kind == eLoopCtrl::break_)
      keyword = "break";
    else if(stmt->loop_ctrl.kind == eLoopCtrl::continue_)
      keyword = "continue";
    else
      assert(0);

    success = compile_error(gp_arena, stmt->src_loc, "unexpected `%s`", keyword);
  }

  return success;
}

bool SymbolContext::sym_return(AstNode* ret)
{
  assert(KIND(ret, eAstNode::return_));
  bool success = true;
  
  Scope* proc_scope = active_scope->find(eScope::proc);
  if(proc_scope)
  {
    assert(KIND(proc_scope->ast_node, eAstNode::proc));

    ret->ret.proc = proc_scope->ast_node;
    if(ret->ret.expr)
    {
      success = sym_expr(ret->ret.expr);
    }
  }
  else
    success = compile_error(gp_arena, ret->src_loc, "unexpected `return`");

  return success;
}

bool SymbolContext::sym_block_stmt(AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode::var:
    {
      success = sym_var(stmt);
    }
    break;
    
    case eAstNode::if_:
    {
      success = sym_if(stmt);
    }
    break;
    
    case eAstNode::do_while:
    {
      success = sym_do_while(stmt);
    }
    break;
    
    case eAstNode::while_:
    {
      success = sym_while(stmt);
    }
    break;
    
    case eAstNode::block:
    {
      stmt->block.scope = begin_nested_scope(eScope::block, stmt);
      success = sym_block(stmt);
      end_nested_scope();
    }
    break;
    
    case eAstNode::assign:
    {
      success = sym_assign(stmt);
    }
    break;
    
    case eAstNode::cast:
    {
      success = sym_cast(stmt);
    }
    break;
    
    case eAstNode::bin_expr:
    case eAstNode::unr_expr:
    case eAstNode::id:
    case eAstNode::call:
    case eAstNode::lit:
    {
      success = sym_expr(stmt);
    }
    break;
    
    case eAstNode::loop_ctrl:
    {
      success = sym_loop_ctrl(stmt);
    }
    break;
    
    case eAstNode::return_:
    {
      success = sym_return(stmt);
    }
    break;
    
    case eAstNode::basic_type:
    case eAstNode::empty:
    break;
    
    case eAstNode::index:
    {
      success = sym_index(stmt);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool SymbolContext::sym_block(AstNode* block)
{
  assert(KIND(block, eAstNode::block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList::ast_node)->ast_node;
    success = sym_block_stmt(stmt);
  }

  return success;
}

bool AstNode_Proc::is_extern()
{
  return ((int)modifier & (int)eModifier::extern_) != 0;
}

bool SymbolContext::sym_proc_body(AstNode* proc)
{
  assert(KIND(proc, eAstNode::proc));
  bool success = true;

  AstNode* body = proc->proc.body;

  if(proc->proc.is_extern())
  {
    if(body->kind != eAstNode::empty)
    {
      success = compile_error(gp_arena, proc->src_loc, "`extern` proc `%s` must not define a body", proc->proc.name);
    }
  }
  else
  {
    if(body->kind == eAstNode::block)
    {
      body->block.scope = begin_scope(eScope::block, body);
      success = sym_block(body);
      end_scope();
    }
    else if(body->kind == eAstNode::empty)
    {
      success = compile_error(gp_arena, proc->src_loc, "proc `%s` must define a body", proc->proc.name);
    }
    else assert(0);
  }

  return success;
}

bool SymbolContext::sym_formal_args(Scope* param_scope, AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList::ast_node)->ast_node;
    success = sym_formal_arg(param_scope, arg);
  }

  return success;
}

bool SymbolContext::sym_module_proc(AstNode* proc)
{
  assert(KIND(proc, eAstNode::proc));
  bool success = true;
  
  Symbol* decl_sym = active_scope->lookup_decl(proc->proc.name);
  if(decl_sym && (decl_sym->scope == active_scope))
  {
    success = compile_error(gp_arena, proc->src_loc, "name `%s` has already been declared", proc->proc.name);
    compile_error(gp_arena, decl_sym->src_loc, "see the declaration of `%s`", proc->proc.name);
  }
  else
  {
    proc->proc.decl_sym = add_decl(proc->proc.name, eStorageSpace::None, active_scope, proc);
    proc->proc.param_scope = begin_nested_scope(eScope::args, proc);
    proc->proc.retvar = add_decl(new_tempvar_name(gp_arena, "ret_"),
                                     eStorageSpace::formal_param, proc->proc.param_scope, proc->proc.ret_type);

    if(proc->proc.is_extern())
    {
      success = sym_formal_args(proc->proc.param_scope, proc->proc.args) && sym_expr(proc->proc.ret_type);
    }
    else
    {
      proc->proc.scope = begin_scope(eScope::proc, proc);

      if(success = sym_formal_args(proc->proc.param_scope, proc->proc.args)
         && sym_expr(proc->proc.ret_type) && sym_proc_body(proc))
      {
        ;//ok
      }

      end_scope();
    }

    end_nested_scope();
  }

  return success;
}

bool SymbolContext::sym_module_var(AstNode* module, AstNode* var)
{
  assert(KIND(module, eAstNode::module));
  assert(KIND(var, eAstNode::var));
  bool success = true;
  
  Symbol* decl_sym = active_scope->lookup_decl(var->var.name);
  if(decl_sym && (decl_sym->scope == active_scope))
  {
    success = compile_error(gp_arena, var->src_loc, "name `%s` already declared", var->var.name);
    compile_error(gp_arena, decl_sym->src_loc, "see declaration of `%s`", var->var.name);
  }
  else
  {
    var->var.decl_sym = add_decl(var->var.name, eStorageSpace::static_, active_scope, var);
  }

  return success;
}

bool SymbolContext::sym_module(AstNode* module)
{
  assert(KIND(module, eAstNode::module));
  bool success = true;
  
  module->module.scope = begin_nested_scope(eScope::module, module);
  module_scope = module->module.scope;
  
  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList::ast_node)->ast_node;
    switch(stmt->kind)
    {
      case eAstNode::var:
      {
        success = sym_module_var(module, stmt);
      }
      break;
      
      case eAstNode::proc:
      {
        success = sym_module_proc(stmt);
      }
      break;
      
      case eAstNode::include:
      break;
      
      default: assert(0);
    }
  }
  end_nested_scope();

  assert(active_scope == 0);
  assert(nesting_depth == -1);

  return success;
}

bool SymbolContext::process(AstNode* module)
{
  return sym_module(module);
}

