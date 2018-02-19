Label* Label::create(MemoryArena* arena)
{
  Label* label = push_struct(arena, Label);
  gen_label_name(arena, label);
  return label;
}

Label* Label::create_by_name(MemoryArena* arena, char* name)
{
  Label* label = push_struct(arena, Label);
  label->name = name;
  return label;
}

void IrContext::init(MemoryArena* gp_arena, MemoryArena* stmt_arena,
                     TypePass* type_pass, SymbolContext* sym_context)
{
  basic_type_bool  = type_pass->basic_type_bool;
  basic_type_int   = type_pass->basic_type_int;
  basic_type_char  = type_pass->basic_type_char;
  basic_type_float = type_pass->basic_type_float;
  basic_type_void  = type_pass->basic_type_void;
  basic_type_str   = type_pass->basic_type_str;

  this->gp_arena = gp_arena;
  this->stmt_arena = stmt_arena;
  stmt_array = (IrStmt*)stmt_arena->base;
  stmt_count = 0;
  this->sym_context = sym_context;
  label_list = List::create(gp_arena, eList::ir_label);
  data_alignment = 4;
}

bool IrContext::is_cast_op(eIrOp ir_op)
{
  bool is_conv = false;

  switch(ir_op)
  {
    case eIrOp::itof:
    case eIrOp::itoc:
    case eIrOp::itob:
    case eIrOp::ftoi:
    case eIrOp::ctoi:
    case eIrOp::btoi:
      is_conv = true;
    break;
  }

  return is_conv;
}

eIrOp IrContext::conv_operator_to_ir_op(eOperator op)
{
  eIrOp ir_op = eIrOp::None;
  switch(op)
  {
    case eOperator::add:
      ir_op = eIrOp::add;
    break;
    
    case eOperator::sub:
      ir_op = eIrOp::sub;
    break;
    
    case eOperator::mul:
      ir_op = eIrOp::mul;
    break;
    
    case eOperator::div:
      ir_op = eIrOp::div;
    break;

    case eOperator::mod:
      ir_op = eIrOp::mod;
    break;
    
    case eOperator::neg:
      ir_op = eIrOp::neg;
    break;
    
    case eOperator::bit_and:
      ir_op = eIrOp::bit_and;
    break;
    
    case eOperator::bit_or:
      ir_op = eIrOp::bit_or;
    break;
    
    case eOperator::bit_xor:
      ir_op = eIrOp::bit_xor;
    break;
    
    case eOperator::bit_shift_left:
      ir_op = eIrOp::bit_shift_left;
    break;
    
    case eOperator::bit_shift_right:
      ir_op = eIrOp::bit_shift_right;
    break;
    
    case eOperator::less:
      ir_op = eIrOp::less;
    break;
    
    case eOperator::less_eq:
      ir_op = eIrOp::less_eq;
    break;
    
    case eOperator::greater:
      ir_op = eIrOp::greater;
    break;
    
    case eOperator::greater_eq:
      ir_op = eIrOp::greater_eq;
    break;
    
    case eOperator::eq:
      ir_op = eIrOp::eq;
    break;
    
    case eOperator::not_eq_:
      ir_op = eIrOp::not_eq_;
    break;
    
    case eOperator::address_of:
      ir_op = eIrOp::address_of;
    break;

    case eOperator::deref:
      ir_op = eIrOp::deref_source;
    break;

    default: assert(0);
  }

  return ir_op;
}

Label* IrContext::get_label_at(int stmt_nr)
{
  Label* label = 0;
  for(ListItem* li = label_list->first;
      li;
      li = li->next)
  {
    label = KIND(li, eList::ir_label)->ir_label;
    if(label->stmt_nr == stmt_nr)
      break;
    label = 0;
  }

  return label;
}

void IrContext::emit_assign(eIrOp op, IrArg* arg1, IrArg* arg2, IrArg* result)
{
  IrStmt* stmt = push_struct(stmt_arena, IrStmt);
  stmt->kind = eIrStmt::assign;
  stmt->label = get_label_at(stmt_count);

  stmt->assign.op = op;
  stmt->assign.arg1 = push_struct(gp_arena, IrArg);
  *stmt->assign.arg1 = *arg1;
  if(arg2)
  {
    stmt->assign.arg2 = push_struct(gp_arena, IrArg);
    *stmt->assign.arg2 = *arg2;
  }
  stmt->assign.result = push_struct(gp_arena, IrArg);
  *stmt->assign.result = *result;

  stmt_count++;
}

void IrContext::emit_label(Label* label)
{
  label->stmt_nr = stmt_count;
  
  Label* prim_label = 0;
  for(ListItem* li = label_list->last;
      li;
      li = li->prev)
  {
    prim_label = KIND(li, eList::ir_label)->ir_label;
    if(prim_label->stmt_nr == label->stmt_nr)
      break;
    prim_label = 0;
  }

  if(prim_label)
  {
    label->primary = prim_label;
  }
  else
  {
    label_list->append(label, eList::ir_label);
  }
}

void IrContext::emit_nop()
{
  IrStmt* stmt = push_struct(stmt_arena, IrStmt);

  *stmt = {};
  stmt->kind = eIrStmt::nop;
  stmt->label = get_label_at(stmt_count);

  stmt_count++;
}

void IrContext::emit_cond_goto(eIrOp relop, IrArg* arg1, IrArg* arg2, Label* label)
{
  IrStmt* stmt = push_struct(stmt_arena, IrStmt);
  stmt->kind = eIrStmt::cond_goto;
  stmt->label = get_label_at(stmt_count);

  stmt->cond_goto.relop = relop;
  stmt->cond_goto.arg1 = arg1;
  stmt->cond_goto.arg2 = arg2;
  stmt->cond_goto.goto_label = label;

  stmt_count++;
}

void IrContext::emit_goto(Label* goto_label)
{
  IrStmt* stmt = push_struct(stmt_arena, IrStmt);
  stmt->kind = eIrStmt::goto_;
  stmt->label = get_label_at(stmt_count);
  stmt->goto_.goto_label = goto_label;

  stmt_count++;
}

void IrContext::emit_call(Label* name, Scope* param_scope, Symbol* retvar, bool is_extern)
{
  IrStmt* stmt = push_struct(stmt_arena, IrStmt);
  stmt->kind = eIrStmt::call;
  stmt->label = get_label_at(stmt_count);
  stmt->call.name = name;
  stmt->call.param_scope = param_scope;
  stmt->call.retvar = retvar;
  stmt->call.is_extern = is_extern;

  stmt_count++;
}

void IrContext::emit_return()
{
  IrStmt* stmt = push_struct(stmt_arena, IrStmt);
  stmt->kind = eIrStmt::return_;
  stmt->label = get_label_at(stmt_count);

  stmt_count++;
}

void IrContext::reset()
{
  stmt_array = &stmt_array[stmt_count];
  total_stmt_count += stmt_count;
  stmt_count = 0;
  current_alloc_offset = 0;

  /*XXX: 'label_list' storage is a good candidate for begin_temp_memory()/end_temp_memory() pattern of allocation. */
  label_list->clear();
}

IrArg* IrContext::create_arg_temp_object(Scope* scope, Type* ty, SourceLoc* src_loc)
{
  IrArg* arg = push_struct(gp_arena, IrArg);
  arg->object = create_temp_object(scope, ty, src_loc);

  return arg;
}

IrArg* IrContext::create_arg_existing_object(Symbol* object)
{
  IrArg* arg = push_struct(gp_arena, IrArg);
  arg->object = object;

  return arg;
}

bool IrContext::visit_bin_expr(Scope* scope, AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode::bin_expr));
  bool success = true;
  
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  eOperator op = bin_expr->bin_expr.op;
  
  switch(op)
  {
    case eOperator::add:
    case eOperator::sub:
    case eOperator::mul:
    case eOperator::div:
    case eOperator::mod:
    case eOperator::bit_and:
    case eOperator::bit_or:
    case eOperator::bit_xor:
    case eOperator::bit_shift_left:
    case eOperator::bit_shift_right:
    case eOperator::less:
    case eOperator::less_eq:
    case eOperator::greater:
    case eOperator::greater_eq:
    case eOperator::eq:
    case eOperator::not_eq_:
    {
      if(success = visit_expr(scope, left_operand) && visit_expr(scope, right_operand))
      {
        bin_expr->place = create_arg_temp_object(scope, bin_expr->eval_ty, bin_expr->src_loc);

        emit_assign(conv_operator_to_ir_op(op), left_operand->place, right_operand->place, bin_expr->place);
      }
    }
    break;
    
    default: assert(0);
  }

  return success;
}

void IrContext::visit_id(AstNode* id)
{
  assert(KIND(id, eAstNode::id));
  id->place = create_arg_existing_object(id->id.decl_sym);
}

bool IrContext::visit_unr_expr(Scope* scope, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode::unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;
  
  switch(op)
  {
    case eOperator::neg:
    {
      if(success = visit_expr(scope, operand))
      {
        unr_expr->place = create_arg_temp_object(scope, unr_expr->eval_ty, unr_expr->src_loc);
        emit_assign(conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
      }
    }
    break;
    
    case eOperator::logic_not:
    {
      fail("todo");
    }
    break;

    case eOperator::address_of:
    {
      if(success = visit_expr(scope, operand))
      {
        unr_expr->place = create_arg_temp_object(scope, unr_expr->eval_ty, unr_expr->src_loc);
        emit_assign(conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
      }
    }
    break;

    case eOperator::deref:
    {
      if(success = visit_expr(scope, operand))
      {
        unr_expr->place = create_arg_temp_object(scope, unr_expr->eval_ty, unr_expr->src_loc);
        emit_assign(conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
      }
    }
    break;
    
    default: assert(0);
  }

  return success;
}

void IrContext::visit_lit(Scope* scope, AstNode* lit)
{
  assert(KIND(lit, eAstNode::lit));
  
  lit->place = create_arg_existing_object(lit->lit.constant);
}

bool IrContext::visit_bool_unr_expr(Scope* scope, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode::unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;
  
  switch(op)
  {
    case eOperator::logic_not:
    {
      operand->label_true = unr_expr->label_false;
      operand->label_false = unr_expr->label_true;
      success = visit_bool_expr(scope, operand);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool IrContext::visit_actual_args(Scope* scope, AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  bool success = true;

  int arg_count = args->args.node_list.count;
  if(arg_count > 0)
  {
    IrArg** temp_places = push_array(gp_arena, IrArg*, arg_count);

    int i = 0;
    for(ListItem* li = args->args.node_list.first;
        li && success;
        li = li->next, i++)
    {
      AstNode* arg = KIND(li, eList::ast_node)->ast_node;
      AstNode* expr = arg->call_arg.expr;

      if(success = visit_expr(scope, expr))
      {
        temp_places[i] = create_arg_temp_object(scope, expr->eval_ty, expr->src_loc);

        emit_assign(eIrOp::None, expr->place, 0, temp_places[i]);
      }
    }

    i = 0;
    for(ListItem* li = args->args.node_list.first;
        li;
        li = li->next, i++)
    {
      AstNode* arg = KIND(li, eList::ast_node)->ast_node;

      arg->place = create_arg_existing_object(arg->call_arg.param);
      emit_assign(eIrOp::None, temp_places[i], 0, arg->place);
    }
  }

  return success;
}

void IrContext::visit_call(Scope* scope, AstNode* call)
{
  assert(KIND(call, eAstNode::call));
  AstNode* proc = call->call.proc;
  
  call->place = create_arg_existing_object(call->call.retvar);

  AstNode* args = call->call.args;
  assert(KIND(args, eAstNode::node_list));
  visit_actual_args(scope, args);

  if(proc->proc.is_extern())
  {
    // right-to-left (stdcall)
    alloc_data_object(call->call.retvar, call->call.param_scope);
    for(ListItem* li = args->args.node_list.last;
        li;
        li = li->prev)
    {
      AstNode* arg = KIND(li, eList::ast_node)->ast_node;
      alloc_data_object(arg->call_arg.param, call->call.param_scope);
    }

    emit_call(&proc->proc.label_name, call->call.param_scope, call->call.retvar, true);
  }
  else
  {
    // left-to-right
    for(ListItem* li = args->args.node_list.first;
        li;
        li = li->next)
    {
      AstNode* arg = KIND(li, eList::ast_node)->ast_node;
      alloc_data_object(arg->call_arg.param, call->call.param_scope);
    }
    alloc_data_object(call->call.retvar, call->call.param_scope);

    emit_call(&proc->proc.label_name, call->call.param_scope, call->call.retvar, false);
  }
}

bool IrContext::visit_index(Scope* scope, AstNode* index)
{
  assert(KIND(index, eAstNode::index));
  bool success = true;

  AstNode* array_expr = index->index.array_expr;
  AstNode* i_expr = index->index.i_expr;

  if(array_expr->kind == eAstNode::id)
  {
    visit_id(array_expr);
    index->index.place = array_expr->place;

    if(success = visit_expr(scope, i_expr))
    {
      index->index.i_place = i_expr->place;
    }
  }
  else if(array_expr->kind == eAstNode::index)
  {
    if(success = visit_index(scope, array_expr) && visit_expr(scope, i_expr))
    {
      index->index.place = array_expr->index.place;

      IrArg* offset = index->index.i_place = create_arg_temp_object(scope, basic_type_int, index->src_loc);

      Type* index_ty = index->ty;
      int size_val = KIND(index_ty, eType::array)->array.size;

      Symbol* size_constant = sym_context->create_const_int(index->src_loc, size_val);
      IrArg* dim_size = create_arg_existing_object(size_constant);

      if(size_val > 0)
      {
        emit_assign(eIrOp::mul, array_expr->index.i_place, dim_size, offset);
        emit_assign(eIrOp::add, offset, i_expr->place, offset);
      }
      else
        success = compile_error(gp_arena, i_expr->src_loc, "array dim size <= 0");
    }
  }
  else assert(0);

  return success;
}

bool IrContext::visit_index_with_offset(Scope* scope, AstNode* index)
{
  assert(KIND(index, eAstNode::index));
  bool success = true;

  if(success = visit_index(scope, index))
  {
    IrArg* offset = index->index.offset = create_arg_temp_object(scope, basic_type_int, index->src_loc);

    assert(index->index.ndim == 1);
    int width_val = index->eval_ty->width;

    Symbol* width_constant = sym_context->create_const_int(index->src_loc, width_val);
    IrArg* width = create_arg_existing_object(width_constant);

    emit_assign(eIrOp::mul, index->index.i_place, width, offset);
  }

  return success;
}

bool IrContext::visit_assign(Scope* scope, AstNode* assign)
{
  assert(KIND(assign, eAstNode::assign));
  bool success = true;
  
  AstNode* dest_expr = assign->assign.dest_expr;
  AstNode* source_expr = assign->assign.source_expr;
  
  if(dest_expr->kind == eAstNode::id)
  {
    if(success = visit_expr(scope, dest_expr) && visit_expr(scope, source_expr))
    {
      emit_assign(eIrOp::None, source_expr->place, 0, dest_expr->place);
    }
  }
  else if(dest_expr->kind == eAstNode::index)
  {
    if(success = visit_index_with_offset(scope, dest_expr) && visit_expr(scope, source_expr))
    {
      dest_expr->place = dest_expr->index.place;
      emit_assign(eIrOp::index_dest, source_expr->place, dest_expr->index.offset, dest_expr->index.place);
    }
  }
  else if(dest_expr->kind == eAstNode::unr_expr && dest_expr->unr_expr.op == eOperator::deref)
  {
    AstNode* operand = dest_expr->unr_expr.operand;
    if(success = visit_expr(scope, operand) && visit_expr(scope, source_expr))
    {
      dest_expr->place = operand->place;
      emit_assign(eIrOp::deref_dest, source_expr->place, 0, dest_expr->place);
    }
  }
  else
    success = compile_error(gp_arena, dest_expr->src_loc, "unsupported expression on the left-side of assignment");

  return success;
}

bool IrContext::visit_cast(Scope* scope, AstNode* cast)
{
  assert(KIND(cast, eAstNode::cast));
  bool success = true;
  AstNode* to_type = cast->cast.to_type;
  AstNode* from_expr = cast->cast.from_expr;
  
  if(success = visit_expr(scope, from_expr))
  {
    cast->place = from_expr->place;
    
    if(to_type->eval_ty->equal(from_expr->eval_ty) ||
       ((to_type->eval_ty->kind == from_expr->eval_ty->kind) && (to_type->eval_ty->kind == eType::pointer)))
    {
      return success;
    }
    bool require_conv = true;
    if(to_type->eval_ty->equal(basic_type_int))
    {
      // int <- pointer
      require_conv = from_expr->eval_ty->kind != eType::pointer;
    }
    else if(to_type->eval_ty->kind == eType::pointer)
    {
      // pointer <- int
      require_conv = !from_expr->eval_ty->equal(basic_type_int);
    }
    if(require_conv)
    {
      cast->place = create_arg_temp_object(scope, cast->eval_ty, cast->src_loc);

      eIrOp cast_op = eIrOp::None;

      if(to_type->eval_ty->equal(basic_type_int))
      {
        if(from_expr->eval_ty->equal(basic_type_float))
        {
          cast_op = eIrOp::ftoi; // int <- float
        }
        else if(from_expr->eval_ty->equal(basic_type_bool))
        {
          cast_op = eIrOp::btoi; // int <- bool
        }
        else if(from_expr->eval_ty->equal(basic_type_char))
        {
          cast_op = eIrOp::ctoi; // int <- char
        }
        else assert(0);
      }
      else if(to_type->eval_ty->equal(basic_type_float))
      {
        if(from_expr->eval_ty->equal(basic_type_int))
        {
          cast_op = eIrOp::itof; // float <- int
        }
        else assert(0);
      }
      else if(to_type->eval_ty->equal(basic_type_char))
      {
        if(from_expr->eval_ty->equal(basic_type_int))
        {
          cast_op = eIrOp::itoc; // char <- int
        }
        else assert(0);
      }
      else if(to_type->eval_ty->equal(basic_type_bool))
      {
        if(from_expr->eval_ty->equal(basic_type_int))
        {
          cast_op = eIrOp::itob; // bool <- int
        }
        else if(from_expr->eval_ty->kind == eType::pointer)
        {
          cast_op = eIrOp::itob; // bool <- pointer(T)
        }
        else assert(0);
      }
      emit_assign(cast_op, from_expr->place, 0, cast->place);
    }
  }

  return success;
}

bool IrContext::visit_expr(Scope* scope, AstNode* expr)
{
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode::bin_expr:
    {
      eOperator op = expr->bin_expr.op;
      if(is_operator_relation(op) || is_operator_logic(op))
      {
        expr->label_true = Label::create(gp_arena);
        expr->label_false = Label::create(gp_arena);
        expr->label_next = Label::create(gp_arena);

        Symbol* result_object = create_temp_object(scope, expr->eval_ty, expr->src_loc);
        result_object->is_live_on_exit = true;
        result_object->is_live = true;
        expr->place = create_arg_existing_object(result_object);

        visit_bool_expr(scope, expr);
        
        emit_label(expr->label_true);
        emit_assign(eIrOp::None, create_arg_existing_object(bool_true), 0, expr->place);
        emit_goto(expr->label_next);
        emit_label(expr->label_false);
        emit_assign(eIrOp::None, create_arg_existing_object(bool_false), 0, expr->place);
        emit_label(expr->label_next);
      }
      else
      {
        visit_bin_expr(scope, expr);
      }
    }
    break;
    
    case eAstNode::unr_expr:
    {
      eOperator op = expr->unr_expr.op;
      if(is_operator_relation(op) || is_operator_logic(op))
      {
        expr->label_true = Label::create(gp_arena);
        expr->label_false = Label::create(gp_arena);
        expr->label_next = Label::create(gp_arena);

        Symbol* result_object = create_temp_object(scope,  expr->eval_ty, expr->src_loc);
        result_object->is_live_on_exit = true;
        result_object->is_live = true;
        expr->place = create_arg_existing_object(result_object);

        visit_bool_unr_expr(scope, expr);

        emit_label(expr->label_true);
        emit_assign(eIrOp::None, create_arg_existing_object(bool_true), 0, expr->place);
        emit_goto(expr->label_next);
        emit_label(expr->label_false);
        emit_assign(eIrOp::None, create_arg_existing_object(bool_false), 0, expr->place);
        emit_label(expr->label_next);
      }
      else
      {
        visit_unr_expr(scope, expr);
      }
    }
    break;
    
    case eAstNode::id:
    {
      visit_id(expr);
    }
    break;
    
    case eAstNode::lit:
    {
      visit_lit(scope, expr);
    }
    break;
    
    case eAstNode::call:
    {
      visit_call(scope, expr);
    }
    break;
    
    case eAstNode::index:
    {
      if(success = visit_index_with_offset(scope, expr))
      {
        expr->place = create_arg_temp_object(scope, expr->eval_ty, expr->src_loc);

        emit_assign(eIrOp::index_source, expr->index.place, expr->index.offset, expr->place);
      }
    }
    break;
    
    case eAstNode::cast:
    {
      visit_cast(scope, expr);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool IrContext::visit_block(Scope* scope, AstNode* block)
{
  assert(KIND(block, eAstNode::block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList::ast_node)->ast_node;
    visit_block_stmt(scope, stmt);
  }

  return success;
}

bool IrContext::visit_bool_bin_expr(Scope* scope, AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode::bin_expr));
  bool success = true;
  
  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  eOperator op = bin_expr->bin_expr.op;
  
  switch(op)
  {
    case eOperator::eq:
    case eOperator::not_eq_:
    case eOperator::less:
    case eOperator::less_eq:
    case eOperator::greater:
    case eOperator::greater_eq:
    {
      if(success = visit_expr(scope, left_operand) && visit_expr(scope, right_operand))
      {
        emit_cond_goto(conv_operator_to_ir_op(op), left_operand->place, right_operand->place, bin_expr->label_true);
        emit_goto(bin_expr->label_false);
      }
    }
    break;
    
    case eOperator::logic_or:
    {
      assert(bin_expr->label_true);
      assert(bin_expr->label_false);

      left_operand->label_true = bin_expr->label_true;
      left_operand->label_false = Label::create(gp_arena);
      right_operand->label_true = bin_expr->label_true;
      right_operand->label_false = bin_expr->label_false;
      if(success = visit_bool_expr(scope, left_operand))
      {
        emit_label(left_operand->label_false);
        success = visit_bool_expr(scope, right_operand);
      }
    }
    break;
    
    case eOperator::logic_and:
    {
      assert(bin_expr->label_true);
      assert(bin_expr->label_false);

      left_operand->label_true = Label::create(gp_arena);
      left_operand->label_false = bin_expr->label_false;
      right_operand->label_true = bin_expr->label_true;
      right_operand->label_false = bin_expr->label_false;
      if(success = visit_bool_expr(scope, left_operand))
      {
        emit_label(left_operand->label_true);
        success = visit_bool_expr(scope, right_operand);
      }
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool IrContext::visit_bool_id(Scope* scope, AstNode* id)
{
  assert(KIND(id, eAstNode::id));
  bool success = true;

  if(success = visit_expr(scope, id))
  {
    emit_cond_goto(eIrOp::not_eq_, id->place,
                      create_arg_existing_object(bool_false), id->label_true);
    emit_goto(id->label_false);
  }

  return success;
}

bool IrContext::visit_bool_call(Scope* scope, AstNode* call)
{
  assert(KIND(call, eAstNode::call));
  bool success = true;

  if(success = visit_expr(scope, call))
  {
    emit_cond_goto(eIrOp::not_eq_, call->place, create_arg_existing_object(bool_false), call->label_true);
    emit_goto(call->label_false);
  }

  return success;
}

bool IrContext::visit_bool_cast(Scope* scope, AstNode* cast)
{
  assert(KIND(cast, eAstNode::cast));
  bool success = true;

  if(success = visit_cast(scope, cast))
  {
    emit_cond_goto(eIrOp::not_eq_, cast->place, create_arg_existing_object(bool_false), cast->label_true);
    emit_goto(cast->label_false);
  }

  return success;
}

void IrContext::visit_bool_lit(Scope* scope, AstNode* lit)
{
  assert(KIND(lit, eAstNode::lit));

  if(lit->lit.bool_val)
  {
    emit_goto(lit->label_true);
  }
  else
  {
    emit_goto(lit->label_false);
  }
}

bool IrContext::visit_bool_expr(Scope* scope, AstNode* expr)
{
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode::id:
    {
      success = visit_bool_id(scope, expr);
    }
    break;
    
    case eAstNode::lit:
    {
      visit_bool_lit(scope, expr);
    }
    break;
    
    case eAstNode::bin_expr:
    {
      success = visit_bool_bin_expr(scope, expr);
    }
    break;
    
    case eAstNode::unr_expr:
    {
      success = visit_bool_unr_expr(scope, expr);
    }
    break;
    
    case eAstNode::cast:
    {
      success = visit_bool_cast(scope, expr);
    }
    break;

    case eAstNode::call:
    {
      success = visit_bool_call(scope, expr);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool IrContext::visit_do_while(Scope* scope, AstNode* do_while)
{
  assert(KIND(do_while, eAstNode::do_while));
  bool success = true;

  AstNode* cond_expr = do_while->do_while.cond_expr;
  AstNode* body = do_while->do_while.body;
  
  do_while->label_begin = Label::create(gp_arena);
  do_while->label_next = Label::create(gp_arena);
  cond_expr->label_true = Label::create(gp_arena);
  cond_expr->label_false = do_while->label_next;
  
  emit_label(cond_expr->label_true);
  visit_block_stmt(scope, body);
  emit_label(do_while->label_begin);
  if(success = visit_bool_expr(scope, cond_expr))
  {
    emit_label(do_while->label_next);
  }

  return success;
}

bool IrContext::visit_while(Scope* scope, AstNode* while_)
{
  assert(KIND(while_, eAstNode::while_));
  bool success = true;
  
  AstNode* cond_expr = while_->while_.cond_expr;
  AstNode* body = while_->while_.body;
  
  while_->label_begin = Label::create(gp_arena);
  while_->label_next = Label::create(gp_arena);
  cond_expr->label_true = Label::create(gp_arena);
  cond_expr->label_false = while_->label_next;
  
  emit_label(while_->label_begin);
  if(success = visit_bool_expr(scope, cond_expr))
  {
    emit_label(cond_expr->label_true);
    visit_block_stmt(scope, body);
    emit_goto(while_->label_begin);
    emit_label(while_->label_next);
  }

  return success;
}

bool IrContext::visit_if(Scope* scope, AstNode* if_)
{
  assert(KIND(if_, eAstNode::if_));
  bool success = true;
  
  AstNode* cond_expr = if_->if_.cond_expr;
  AstNode* body = if_->if_.body;
  AstNode* else_body = if_->if_.else_body;
  
  if_->label_next = Label::create(gp_arena);
  if(else_body)
  {
    cond_expr->label_true = Label::create(gp_arena);
    cond_expr->label_false = Label::create(gp_arena);
    body->label_next = if_->label_next;
    else_body->label_next = if_->label_next;
    
    if(success = visit_bool_expr(scope, cond_expr))
    {
      emit_label(cond_expr->label_true);
      visit_block_stmt(scope, body);
      emit_goto(if_->label_next);
      emit_label(cond_expr->label_false);
      visit_block_stmt(scope, else_body);
    }
  }
  else
  {
    cond_expr->label_true = Label::create(gp_arena);
    cond_expr->label_false = if_->label_next;
    body->label_next = if_->label_next;
    
    if(success = visit_bool_expr(scope, cond_expr))
    {
      emit_label(cond_expr->label_true);
      visit_block_stmt(scope, body);
    }
  }
  if(success)
  {
    emit_label(if_->label_next);
  }

  return success;
}

bool IrContext::visit_return(Scope* scope, AstNode* ret)
{
  assert(KIND(ret, eAstNode::return_));
  bool success = true;

  AstNode* ret_expr = ret->ret.expr;
  AstNode* proc = ret->ret.proc;

  if(ret_expr)
  {
    if(success = visit_expr(scope, ret_expr))
    {
      IrArg* retvar = create_arg_existing_object(proc->proc.retvar);

      emit_assign(eIrOp::None, ret_expr->place, 0, retvar);
    }
  }

  emit_goto(proc->label_next);

  return success;
}

bool IrContext::visit_loop_ctrl(Scope* scope, AstNode* loop_ctrl)
{
  assert(KIND(loop_ctrl, eAstNode::loop_ctrl));
  bool success = true;

  AstNode* loop = loop_ctrl->loop_ctrl.loop;
  if(loop_ctrl->loop_ctrl.kind == eLoopCtrl::break_)
  {
    emit_goto(loop->label_next);
  }
  else if(loop_ctrl->loop_ctrl.kind == eLoopCtrl::continue_)
  {
    emit_goto(loop->label_begin);
  }
  else assert(0);

  return success;
}

bool IrContext::visit_var(Scope* scope, AstNode* var)
{
  bool success = true;
  assert(KIND(var, eAstNode::var));

  Symbol* object = var->var.decl_sym;
  alloc_data_object_incremental(object, scope);
  x86_context->add_object_to_memory(object);
  var->place = create_arg_existing_object(object);

  AstNode* init_expr = var->var.init_expr;
  if(init_expr)
  {
    if(success = visit_expr(scope, init_expr))
    {
      emit_assign(eIrOp::None, init_expr->place, 0, var->place);
    }
  }

  return success;
}

bool IrContext::visit_block_stmt(Scope* scope, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode::assign:
    {
      success = visit_assign(scope, stmt);
    }
    break;
    
    case eAstNode::cast:
    {
      success = visit_cast(scope, stmt);
    }
    break;
    
    case eAstNode::bin_expr:
    case eAstNode::unr_expr:
    case eAstNode::id:
    case eAstNode::call:
    case eAstNode::index:
    case eAstNode::lit:
    {
      success = visit_expr(scope, stmt);
    }
    break;
    
    case eAstNode::block:
    {
      success = visit_block(stmt->block.scope, stmt);
    }
    break;
    
    case eAstNode::if_:
    {
      success = visit_if(scope, stmt);
    }
    break;
    
    case eAstNode::do_while:
    {
      success = visit_do_while(scope, stmt);
    }
    break;
    
    case eAstNode::while_:
    {
      success = visit_while(scope, stmt);
    }
    break;
    
    case eAstNode::var:
    {
      success = visit_var(scope, stmt);
    }
    break;
    
    case eAstNode::return_:
    {
      success = visit_return(scope, stmt);
    }
    break;
    
    case eAstNode::empty:
    break;

    case eAstNode::loop_ctrl:
    {
      success = visit_loop_ctrl(scope, stmt);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

void IrContext::visit_formal_args(Scope* scope, AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  for(ListItem* li = args->args.node_list.first;
      li;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList::ast_node)->ast_node;
    Symbol* arg_object = KIND(arg, eAstNode::var)->var.decl_sym;
    alloc_data_object(arg_object, scope);
    x86_context->add_object_to_memory(arg_object);
  }
}

int IrContext::get_proc_arg_size(AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  int size = 0;

  for(ListItem* li = args->args.node_list.first;
      li;
      li = li->next)
  {
    AstNode* var = KIND(li, eList::ast_node)->ast_node;
    assert(KIND(var, eAstNode::var));
    Symbol* arg_object = var->var.decl_sym;
    size += arg_object->ty->set_width();
  }

  return size;
}

bool IrContext::visit_proc(Scope* scope, AstNode* proc)
{
  assert(KIND(proc, eAstNode::proc));
  bool success = true;
  
  proc->place = create_arg_existing_object(proc->proc.retvar);
  visit_formal_args(proc->proc.param_scope, proc->proc.args);
  alloc_data_object(proc->proc.retvar, proc->proc.param_scope);

  Label* label_name = &proc->proc.label_name;
  label_name->stmt_nr = 0;

  if(proc->proc.is_extern())
  {
    int arg_size = get_proc_arg_size(proc->proc.args);
    char* name = proc->proc.name;
    String decorated_label = {};
    decorated_label.init(gp_arena);
    decorated_label.format("%s@%d", name, arg_size);

    label_name->name = decorated_label.cap();
  }
  else
  {
    label_name->name = proc->proc.name;

    AstNode* body = proc->proc.body;
    assert(KIND(body, eAstNode::block));

    proc->label_begin = Label::create(gp_arena);
    proc->label_next = Label::create(gp_arena);

    emit_label(proc->label_begin);

    if(success = visit_block_stmt(body->block.scope, body))
    {
      emit_label(proc->label_next);
      emit_return();
    }

    Scope* proc_scope = proc->proc.scope;
    proc_scope->allocd_size = current_alloc_offset;
  }

  return success;
}

void IrContext::visit_module_var(Scope* scope, AstNode* var)
{
  assert(KIND(var, eAstNode::var));

  Symbol* object = var->var.decl_sym;
  x86_context->add_object_to_memory(object);
}

bool IrContext::visit_module_stmt(Scope* scope, AstNode* stmt)
{
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode::proc:
    {
      if(success = visit_proc(scope, stmt))
      {
        stmt->proc.ir_stmt_array = stmt_array;
        stmt->proc.ir_stmt_count = stmt_count;

        reset();
      }
    }
    break;
    
    case eAstNode::var:
      visit_module_var(scope, stmt);
    break;
    
    default: assert(0);
  }

  return success;
}

bool IrContext::visit_module(AstNode* module)
{
  assert(KIND(module, eAstNode::module));
  bool success = true;
  
  for(ListItem* li = module->module.nodes.first;
      li;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList::ast_node)->ast_node;
    success = visit_module_stmt(module->module.scope, stmt);
  }

  return success;
}

void IrContext::DEBUG_print_ir_op(String* text, eIrOp op)
{
  switch(op)
  {
    case eIrOp::add:
      text->append("+");
    break;
    
    case eIrOp::sub:
      text->append("-");
    break;
    
    case eIrOp::mul:
      text->append("*");
    break;
    
    case eIrOp::div:
      text->append("/");
    break;
    
    case eIrOp::mod:
      text->append("mod");
    break;
    
    case eIrOp::neg:
      text->append("-");
    break;
    
    case eIrOp::eq:
      text->append("==");
    break;
    
    case eIrOp::not_eq_:
      text->append("<>");
    break;
    
    case eIrOp::less:
      text->append("<");
    break;
    
    case eIrOp::less_eq:
      text->append("<=");
    break;
    
    case eIrOp::greater:
      text->append(">");
    break;
    
    case eIrOp::greater_eq:
      text->append(">=");
    break;
    
    case eIrOp::logic_and:
      text->append("and");
    break;
    
    case eIrOp::logic_or:
      text->append("or");
    break;
    
    case eIrOp::logic_not:
      text->append("not");
    break;
    
    case eIrOp::bit_and:
      text->append("&");
    break;
    
    case eIrOp::bit_or:
      text->append("|");
    break;
    
    case eIrOp::bit_xor:
      text->append("~");
    break;
    
    case eIrOp::bit_not:
      text->append("!");
    break;
    
    case eIrOp::bit_shift_left:
      text->append("<<");
    break;
    
    case eIrOp::bit_shift_right:
      text->append(">>");
    break;
    
    case eIrOp::itof:
      text->append("itof");
    break;
    
    case eIrOp::itoc:
      text->append("itoc");
    break;
    
    case eIrOp::itob:
      text->append("itob");
    break;
    
    case eIrOp::ftoi:
      text->append("ftoi");
    break;
    
    case eIrOp::ctoi:
      text->append("ctoi");
    break;
    
    case eIrOp::btoi:
      text->append("btoi");
    break;
    
    default: assert(0);
  }
}

void IrContext::DEBUG_print_ir_arg(String* text, IrArg* arg)
{
  Symbol* object = arg->object;

  switch(object->kind)
  {
    case eSymbol::None:
    {
      text->format("%s", arg->object->name);
    }
    break;
    
    case eSymbol::constant:
    {
      if(object->ty->equal(basic_type_int) || object->ty->equal(basic_type_bool))
      {
        text->format("%d", object->int_val);
      }
      else if(object->ty->equal(basic_type_float))
      {
        text->format("%f", object->float_val);
      }
      else if(object->ty->equal(basic_type_char))
      {
        char buf[3] = {0};
        Cstr::print_char(buf, object->char_val);
        text->format("'%s'", buf);
      }
      else if(object->ty->equal(basic_type_str))
      {
        text->format("\"%s\"", object->str_val);
      }
      else assert(0);
    }
    break;
    
    default: assert(0);
  }
}

void IrContext::DEBUG_print_ir_stmt(String* text, IrStmt* stmt)
{
  switch(stmt->kind)
  {
    case eIrStmt::assign:
    {
      IrStmt_Assign* assign = &stmt->assign;

      switch(assign->op)
      {
        case eIrOp::None:
        {
          DEBUG_print_ir_arg(text, assign->result);
          text->append(" = ");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;
        
        /* bin_ops */
        case eIrOp::add:
        case eIrOp::sub:
        case eIrOp::mul:
        case eIrOp::div:
        case eIrOp::mod:
        case eIrOp::eq:
        case eIrOp::not_eq_:
        case eIrOp::less:
        case eIrOp::less_eq:
        case eIrOp::greater:
        case eIrOp::greater_eq:
        case eIrOp::logic_and:
        case eIrOp::logic_or:
        case eIrOp::logic_not:
        case eIrOp::bit_and:
        case eIrOp::bit_or:
        case eIrOp::bit_xor:
        case eIrOp::bit_not:
        case eIrOp::bit_shift_left:
        case eIrOp::bit_shift_right:
        {
          DEBUG_print_ir_arg(text, assign->result);
          text->append(" = ");
          DEBUG_print_ir_arg(text, assign->arg1);
          text->append(" ");
          DEBUG_print_ir_op(text, assign->op);
          text->append(" ");
          DEBUG_print_ir_arg(text, assign->arg2);
        }
        break;
        
        /* unr_ops */
        case eIrOp::neg:
        case eIrOp::itof:
        case eIrOp::itoc:
        case eIrOp::itob:
        case eIrOp::ftoi:
        case eIrOp::ctoi:
        case eIrOp::btoi:
        {
          DEBUG_print_ir_arg(text, assign->result);
          text->append(" = ");
          DEBUG_print_ir_op(text, assign->op);
          text->append(" ");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;
        
        case eIrOp::index_dest:
        {
          DEBUG_print_ir_arg(text, assign->result);
          text->append("[");
          DEBUG_print_ir_arg(text, assign->arg2);
          text->append("] = ");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;
        
        case eIrOp::index_source:
        {
          DEBUG_print_ir_arg(text, assign->result);
          text->append(" = ");
          DEBUG_print_ir_arg(text, assign->arg1);
          text->append("[");
          DEBUG_print_ir_arg(text, assign->arg2);
          text->append("]");
        }
        break;

        case eIrOp::address_of:
        {
          DEBUG_print_ir_arg(text, assign->result);
          text->append(" = &");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;

        case eIrOp::deref_dest:
        {
          text->append("^");
          DEBUG_print_ir_arg(text, assign->result);
          text->append(" = ");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;

        case eIrOp::deref_source:
        {
          DEBUG_print_ir_arg(text, assign->result);
          text->append(" = ^");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;
        
        default: assert(0);
      }
    }
    break;
    
    case eIrStmt::cond_goto:
    {
      IrStmt_CondGoto* cond_goto = &stmt->cond_goto;
      text->append("if ");
      DEBUG_print_ir_arg(text, cond_goto->arg1);
      text->append(" ");
      DEBUG_print_ir_op(text, cond_goto->relop);
      text->append(" ");
      DEBUG_print_ir_arg(text, cond_goto->arg2);
      text->format(" goto %s", cond_goto->goto_label->name);
    }
    break;
    
    case eIrStmt::goto_:
    {
      IrStmt_Goto* goto_ = &stmt->goto_;
      text->format("goto %s", goto_->goto_label->name);
    }
    break;
    
    case eIrStmt::call:
    {
      IrStmt_Call* call = &stmt->call;
      text->format("call %s", call->name->name);
    }
    break;
    
    case eIrStmt::return_:
    {
      text->append("return");
    }
    break;
    
    case eIrStmt::nop:
    {
      text->append("nop");
    }
    break;
    
    default:
    {
      text->append("???");
    }
  }
}

void IrContext::DEBUG_print_basic_block(String* text, BasicBlock* bb)
{
  IrStmt** stmt_array = bb->stmt_array;
  for(int i = 0; i < bb->stmt_count; i++)
  {
    IrStmt* stmt = stmt_array[i];
    if(stmt->label)
    {
      text->format_nl("%10s:", stmt->label->name);
    }
    text->format("%10d: ", i);
    DEBUG_print_ir_stmt(text, stmt);
    text->nl();
  }
}

void IrContext::DEBUG_print_ir_code(List* procs, char* file_path)
{
  MemoryArena::begin_temp_memory(&gp_arena);
  String text = {};
  text.init(gp_arena);

  for(ListItem* li = procs->first;
      li;
      li = li->next)
  {
    AstNode* proc = KIND(li, eList::ast_node)->ast_node;

    if(proc->proc.is_extern())
    {
      ;//ok
    }
    else
    {
      text.format_nl("%10s:", proc->proc.name);

      List* basic_blocks = proc->proc.basic_blocks;
      for(ListItem* li = basic_blocks->first;
          li;
          li = li->next)
      {
        BasicBlock* bb = KIND(li, eList::basic_block)->basic_block;
        DEBUG_print_basic_block(&text, bb);
      }
    }
  }

  text.dump_to_file(file_path);
  MemoryArena::end_temp_memory(&gp_arena);
}

IrLeaderStmt* IrContext::get_leader_stmt(List* leaders, int stmt_nr)
{
  IrLeaderStmt* leader = 0;

  for(ListItem* li = leaders->first;
      li;
      li = li->next)
  {
    leader = KIND(li, eList::ir_leader_stmt)->ir_leader_stmt;
    if(stmt_nr == leader->stmt_nr)
      break;

    leader = 0;
  }

  return leader;
}

IrLeaderStmt* IrContext::create_leader_stmt(MemoryArena* arena, int stmt_nr, IrStmt* stmt)
{
  IrLeaderStmt* new_elem = push_struct(arena, IrLeaderStmt);
  new_elem->stmt_nr = stmt_nr;
  new_elem->stmt = stmt;
  Label* label = new_elem->label = stmt->label;
  if(label && label->primary)
  {
    new_elem->label = label->primary;
  }

  return new_elem;
}

void IrContext::insert_leader_stmt(List* leaders, int stmt_nr, IrStmt* stmt)
{
  assert(stmt);

  ListItem* li = leaders->first;
  assert(li);

  IrLeaderStmt* leader = KIND(li, eList::ir_leader_stmt)->ir_leader_stmt;
  assert(leader->stmt_nr == 0);

  for(; li; li = li->next)
  {
    leader = KIND(li, eList::ir_leader_stmt)->ir_leader_stmt;
    if(leader->stmt_nr >= stmt_nr)
    {
      break;
    }
    leader = 0;
  }

  if(leader)
  {
    if(leader->stmt_nr > stmt_nr)
    {
      leaders->insert_before(li, create_leader_stmt(leaders->arena, stmt_nr, stmt), eList::ir_leader_stmt);
    }
  }
  else
  {
    leaders->append(create_leader_stmt(leaders->arena, stmt_nr, stmt), eList::ir_leader_stmt);
  }
}

void IrContext::start_basic_block(List* leaders, int at_stmt_nr, IrStmt* stmt_array, int stmt_count)
{
  if(at_stmt_nr < stmt_count)
  {
    insert_leader_stmt(leaders, at_stmt_nr, &stmt_array[at_stmt_nr]);
  }
}

void IrStmt_Assign::update_object_live_info()
{
  result->object->is_live = result->is_live;
  result->object->next_use = result->next_use;

  arg1->object->is_live = arg1->is_live;
  arg1->object->next_use = arg1->next_use;

  if(arg2)
  {
    arg2->object->is_live = arg2->is_live;
    arg2->object->next_use = arg2->next_use;
  }
}

Label* IrContext::normalize_jump_target_labels(IrStmt* stmt)
{
  Label* target_label = 0;

  if(stmt->kind == eIrStmt::cond_goto)
  {
    IrStmt_CondGoto* cond_goto = &stmt->cond_goto;
    target_label = cond_goto->goto_label;
    if(target_label->primary)
    {
      cond_goto->goto_label = target_label->primary;
      target_label = cond_goto->goto_label;
    }
  }
  else if(stmt->kind == eIrStmt::goto_)
  {
    IrStmt_Goto* goto_ = &stmt->goto_;
    target_label = goto_->goto_label;
    if(target_label->primary)
    {
      goto_->goto_label = target_label->primary;
      target_label = goto_->goto_label;
    }
  }

  return target_label;
}

void IrContext::partition_basic_blocks_proc(AstNode* proc)
{
  if(proc->proc.ir_stmt_count > 0)
  {
    List* leaders = List::create(gp_arena, eList::ir_leader_stmt);

    IrStmt* stmt_array = proc->proc.ir_stmt_array;
    int stmt_count = proc->proc.ir_stmt_count;

    IrStmt* stmt = &stmt_array[0];
    leaders->append(create_leader_stmt(leaders->arena, 0, stmt), eList::ir_leader_stmt);
    
    for(int i = 0; i < proc->proc.ir_stmt_count; i++)
    {
      stmt = &stmt_array[i];
      if(stmt->kind == eIrStmt::cond_goto || stmt->kind == eIrStmt::goto_)
      {
        start_basic_block(leaders, i+1, stmt_array, stmt_count);
        Label* target_label = normalize_jump_target_labels(stmt);
        start_basic_block(leaders, target_label->stmt_nr, stmt_array, stmt_count);
      }
    }

    //------//
    
    List* basic_blocks = proc->proc.basic_blocks = List::create(stmt_arena, eList::basic_block);

    for(ListItem* li = leaders->first;
        li;
        li = li->next)
    {
      int next_stmt_nr = proc->proc.ir_stmt_count;
      if(li->next)
      {
        IrLeaderStmt* leader_next = KIND(li->next, eList::ir_leader_stmt)->ir_leader_stmt;
        next_stmt_nr = leader_next->stmt_nr;
      }

      IrLeaderStmt* leader = KIND(li, eList::ir_leader_stmt)->ir_leader_stmt;
      BasicBlock* block = push_struct(stmt_arena, BasicBlock);
      basic_blocks->append(block, eList::basic_block);
      block->pred_list.init(stmt_arena, eList::basic_block);
      block->succ_list.init(stmt_arena, eList::basic_block);
      leader->block = block;
      block->stmt_array = push_array(stmt_arena, IrStmt*, next_stmt_nr - leader->stmt_nr);
      block->stmt_count = 0;
      block->label = leader->label;

      for(int i = leader->stmt_nr;
          i < next_stmt_nr;
          i++)
      {
        block->stmt_array[block->stmt_count++] = &proc->proc.ir_stmt_array[i];
      }
      assert(block->stmt_count > 0);
    }

    for(ListItem* li = basic_blocks->first;
        li;
        li = li->next)
    {
      BasicBlock* bb_next = 0;
      if(li->next)
      {
        bb_next = KIND(li->next, eList::basic_block)->basic_block;
      }

      BasicBlock* bb = KIND(li, eList::basic_block)->basic_block;
      IrStmt* last_stmt = bb->stmt_array[bb->stmt_count - 1];

      if(last_stmt->kind == eIrStmt::goto_ || last_stmt->kind == eIrStmt::cond_goto)
      {
        Label* goto_label = 0;
        if(last_stmt->kind == eIrStmt::cond_goto)
        {
          goto_label = last_stmt->cond_goto.goto_label;
        }
        else if(last_stmt->kind == eIrStmt::goto_)
        {
          goto_label = last_stmt->goto_.goto_label;
        }
        else assert(0);
        
        int stmt_nr = goto_label->stmt_nr;
        IrLeaderStmt* leader = get_leader_stmt(leaders, stmt_nr);

        bb->succ_list.append(leader->block, eList::basic_block);
        leader->block->pred_list.append(bb, eList::basic_block);

        if(last_stmt->kind != eIrStmt::goto_)
        {
          bb->succ_list.append(bb_next, eList::basic_block);
          bb_next->pred_list.append(bb, eList::basic_block);
        }
      }
      else if(bb_next)
      {
        bb->succ_list.append(bb_next, eList::basic_block);
        bb_next->pred_list.append(bb, eList::basic_block);
      }
      
      // next-use information
      for(int i = bb->stmt_count - 1; i >=0 ; i--)
      {
        IrStmt* stmt = bb->stmt_array[i];

        if(stmt->kind == eIrStmt::assign)
        {
          IrArg* result = stmt->assign.result;
          IrArg* arg1 = stmt->assign.arg1;
          IrArg* arg2 = stmt->assign.arg2;

          result->is_live = result->object->is_live;
          result->next_use = result->object->next_use;

          arg1->is_live = arg1->object->is_live;
          arg1->next_use = arg1->object->next_use;

          if(arg2)
          {
            arg2->is_live = arg2->object->is_live;
            arg2->next_use = arg2->object->next_use;
          }

          //-----

          if(stmt->assign.op == eIrOp::index_dest || stmt->assign.op == eIrOp::deref_dest)
          {
            result->object->is_live = true;
            result->object->next_use = i;
          }
          else
          {
            result->object->is_live = result->object->is_live_on_exit ? true : false;
            result->object->next_use = NextUse_None;
          }

          arg1->object->is_live = true;
          arg1->object->next_use = i;

          if(arg2)
          {
            arg2->object->is_live = true;
            arg2->object->next_use = i;
          }
        }
      }
    }
  }
}

void IrContext::partition_basic_blocks_module(AstNode* module)
{
  List* procs = &module->module.procs;
  for(ListItem* li = procs->first;
      li;
      li = li->next)
  {
    AstNode* proc = KIND(li, eList::ast_node)->ast_node;

    partition_basic_blocks_proc(proc);
  }

  if(DEBUG_enabled)
    DEBUG_print_ir_code(&module->module.procs, "./module.ir");
}
