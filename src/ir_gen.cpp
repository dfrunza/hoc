Label* label_gen_new(MemoryArena* arena)
{
  Label* label = mem_push_struct(arena, Label);
  gen_label_name(arena, label);
  return label;
}

Label* label_new_by_name(MemoryArena* arena, char* name)
{
  Label* label = mem_push_struct(arena, Label);
  label->name = name;
  return label;
}

eIrOp conv_operator_to_ir_op(eOperator op)
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

eOperator negate_relop(eOperator op)
{
  eOperator result = eOperator::None;
  switch(op)
  {
    case eOperator::eq:
      result = eOperator::not_eq_;
    break;
    
    case eOperator::not_eq_:
      result = eOperator::eq;
    break;
    
    case eOperator::less:
      result = eOperator::greater_eq;
    break;
    
    case eOperator::less_eq:
      result = eOperator::greater;
    break;
    
    case eOperator::greater:
      result = eOperator::less_eq;
    break;
    
    case eOperator::greater_eq:
      result = eOperator::less;
    break;
    
    default: assert(0);
  }

  return result;
}

bool is_operator_relation(eOperator op)
{
  bool is_relop = false;

  switch(op)
  {
    case eOperator::eq:
    case eOperator::not_eq_:
    case eOperator::less:
    case eOperator::less_eq:
    case eOperator::greater:
    case eOperator::greater_eq:
      is_relop = true;
    break;
  }

  return is_relop;
}

bool is_operator_logic(eOperator op)
{
  bool is_logic = false;

  switch(op)
  {
    case eOperator::logic_and:
    case eOperator::logic_or:
    case eOperator::logic_not:
      is_logic = true;
    break;
  }

  return is_logic;
}

Label* get_label_at(List* label_list, int stmt_nr)
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

void ir_emit_assign(IrContext* ir_context, eIrOp op, IrArg* arg1, IrArg* arg2, IrArg* result)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt::assign;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);

  stmt->assign.op = op;
  stmt->assign.arg1 = mem_push_struct(ir_context->gp_arena, IrArg);
  *stmt->assign.arg1 = *arg1;
  if(arg2)
  {
    stmt->assign.arg2 = mem_push_struct(ir_context->gp_arena, IrArg);
    *stmt->assign.arg2 = *arg2;
  }
  stmt->assign.result = mem_push_struct(ir_context->gp_arena, IrArg);
  *stmt->assign.result = *result;

  ir_context->stmt_count++;
}

void ir_emit_label(IrContext* ir_context, Label* label)
{
  label->stmt_nr = ir_context->stmt_count;
  
  Label* prim_label = 0;
  for(ListItem* li = ir_context->label_list->last;
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
    ir_context->label_list->append(label, eList::ir_label);
  }
}

void ir_emit_nop(IrContext* ir_context)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);

  *stmt = {};
  stmt->kind = eIrStmt::nop;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);

  ir_context->stmt_count++;
}

void ir_emit_cond_goto(IrContext* ir_context, eIrOp relop, IrArg* arg1, IrArg* arg2, Label* label)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt::cond_goto;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);

  stmt->cond_goto.relop = relop;
  stmt->cond_goto.arg1 = arg1;
  stmt->cond_goto.arg2 = arg2;
  stmt->cond_goto.goto_label = label;

  ir_context->stmt_count++;
}

void ir_emit_goto(IrContext* ir_context, Label* goto_label)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt::goto_;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);
  stmt->goto_.goto_label = goto_label;

  ir_context->stmt_count++;
}

void ir_emit_call(IrContext* ir_context, Label* name, Scope* param_scope, Symbol* retvar, bool is_extern)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt::call;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);
  stmt->call.name = name;
  stmt->call.param_scope = param_scope;
  stmt->call.retvar = retvar;
  stmt->call.is_extern = is_extern;

  ir_context->stmt_count++;
}

void ir_emit_return(IrContext* ir_context)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt::return_;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);

  ir_context->stmt_count++;
}

void reset_ir_context(IrContext* ir_context)
{
  ir_context->stmt_array = &ir_context->stmt_array[ir_context->stmt_count];
  ir_context->total_stmt_count += ir_context->stmt_count;
  ir_context->stmt_count = 0;
  ir_context->current_alloc_offset = 0;

  /*XXX: 'label_list' storage is a good candidate for begin_temp_memory()/end_temp_memory() pattern of allocation. */
  ir_context->label_list->clear();
}

IrArg* ir_arg_new_temp_object(IrContext* ir_context, Scope* scope, Type* ty, SourceLoc* src_loc)
{
  IrArg* arg = mem_push_struct(ir_context->gp_arena, IrArg);
  arg->object = create_temp_object(ir_context, scope, ty, src_loc);

  return arg;
}

IrArg* ir_arg_new_existing_object(IrContext* ir_context, Symbol* object)
{
  IrArg* arg = mem_push_struct(ir_context->gp_arena, IrArg);
  arg->object = object;

  return arg;
}

bool ir_gen_expr(IrContext* ir_context, Scope* scope, AstNode* expr);
bool ir_gen_bool_expr(IrContext* ir_context, Scope* scope, AstNode* expr);
bool ir_gen_block_stmt(IrContext* ir_context, Scope* scope, AstNode* stmt);

bool ir_gen_bin_expr(IrContext* ir_context, Scope* scope, AstNode* bin_expr)
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
      if(success = ir_gen_expr(ir_context, scope, left_operand) && ir_gen_expr(ir_context, scope, right_operand))
      {
        bin_expr->place = ir_arg_new_temp_object(ir_context, scope, bin_expr->eval_ty, bin_expr->src_loc);

        ir_emit_assign(ir_context, conv_operator_to_ir_op(op), left_operand->place, right_operand->place, bin_expr->place);
      }
    }
    break;
    
    default: assert(0);
  }

  return success;
}

void ir_gen_id(IrContext* ir_context, AstNode* id)
{
  assert(KIND(id, eAstNode::id));
  id->place = ir_arg_new_existing_object(ir_context, id->id.decl_sym);
}

bool ir_gen_unr_expr(IrContext* ir_context, Scope* scope, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode::unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;
  
  switch(op)
  {
    case eOperator::neg:
    {
      if(success = ir_gen_expr(ir_context, scope, operand))
      {
        unr_expr->place = ir_arg_new_temp_object(ir_context, scope, unr_expr->eval_ty, unr_expr->src_loc);
        ir_emit_assign(ir_context, conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
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
      if(success = ir_gen_expr(ir_context, scope, operand))
      {
        unr_expr->place = ir_arg_new_temp_object(ir_context, scope, unr_expr->eval_ty, unr_expr->src_loc);
        ir_emit_assign(ir_context, conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
      }
    }
    break;

    case eOperator::deref:
    {
      if(success = ir_gen_expr(ir_context, scope, operand))
      {
        unr_expr->place = ir_arg_new_temp_object(ir_context, scope, unr_expr->eval_ty, unr_expr->src_loc);
        ir_emit_assign(ir_context, conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
      }
    }
    break;
    
    default: assert(0);
  }

  return success;
}

void ir_gen_lit(IrContext* ir_context, Scope* scope, AstNode* lit)
{
  assert(KIND(lit, eAstNode::lit));
  
  lit->place = ir_arg_new_existing_object(ir_context, lit->lit.constant);
}

bool ir_gen_bool_unr_expr(IrContext* ir_context, Scope* scope, AstNode* unr_expr)
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
      success = ir_gen_bool_expr(ir_context, scope, operand);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool ir_gen_actual_args(IrContext* ir_context, Scope* scope, AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  bool success = true;

  int arg_count = args->args.node_list.count;
  if(arg_count > 0)
  {
    IrArg** temp_places = mem_push_array(ir_context->gp_arena, IrArg*, arg_count);

    int i = 0;
    for(ListItem* li = args->args.node_list.first;
        li && success;
        li = li->next, i++)
    {
      AstNode* arg = KIND(li, eList::ast_node)->ast_node;
      AstNode* expr = arg->call_arg.expr;

      if(success = ir_gen_expr(ir_context, scope, expr))
      {
        temp_places[i] = ir_arg_new_temp_object(ir_context, scope, expr->eval_ty, expr->src_loc);

        ir_emit_assign(ir_context, eIrOp::None, expr->place, 0, temp_places[i]);
      }
    }

    i = 0;
    for(ListItem* li = args->args.node_list.first;
        li;
        li = li->next, i++)
    {
      AstNode* arg = KIND(li, eList::ast_node)->ast_node;

      arg->place = ir_arg_new_existing_object(ir_context, arg->call_arg.param);
      ir_emit_assign(ir_context, eIrOp::None, temp_places[i], 0, arg->place);
    }
  }

  return success;
}

void ir_gen_call(IrContext* ir_context, Scope* scope, AstNode* call)
{
  assert(KIND(call, eAstNode::call));
  AstNode* proc = call->call.proc;
  
  call->place = ir_arg_new_existing_object(ir_context, call->call.retvar);

  AstNode* args = call->call.args;
  assert(KIND(args, eAstNode::node_list));
  ir_gen_actual_args(ir_context, scope, args);

  if(proc->proc.is_extern())
  {
    // right-to-left (stdcall)
    alloc_data_object(ir_context, call->call.retvar, call->call.param_scope);
    for(ListItem* li = args->args.node_list.last;
        li;
        li = li->prev)
    {
      AstNode* arg = KIND(li, eList::ast_node)->ast_node;
      alloc_data_object(ir_context, arg->call_arg.param, call->call.param_scope);
    }

    ir_emit_call(ir_context, &proc->proc.label_name, call->call.param_scope, call->call.retvar, true);
  }
  else
  {
    // left-to-right
    for(ListItem* li = args->args.node_list.first;
        li;
        li = li->next)
    {
      AstNode* arg = KIND(li, eList::ast_node)->ast_node;
      alloc_data_object(ir_context, arg->call_arg.param, call->call.param_scope);
    }
    alloc_data_object(ir_context, call->call.retvar, call->call.param_scope);

    ir_emit_call(ir_context, &proc->proc.label_name, call->call.param_scope, call->call.retvar, false);
  }
}

bool ir_gen_index(IrContext* ir_context, Scope* scope, AstNode* index)
{
  assert(KIND(index, eAstNode::index));
  bool success = true;

  AstNode* array_expr = index->index.array_expr;
  AstNode* i_expr = index->index.i_expr;

  if(array_expr->kind == eAstNode::id)
  {
    ir_gen_id(ir_context, array_expr);
    index->index.place = array_expr->place;

    if(success = ir_gen_expr(ir_context, scope, i_expr))
    {
      index->index.i_place = i_expr->place;
    }
  }
  else if(array_expr->kind == eAstNode::index)
  {
    if(success = ir_gen_index(ir_context, scope, array_expr) && ir_gen_expr(ir_context, scope, i_expr))
    {
      index->index.place = array_expr->index.place;

      IrArg* offset = index->index.i_place = ir_arg_new_temp_object(ir_context, scope, ir_context->basic_type_int, index->src_loc);

      Type* index_ty = index->ty;
      int size_val = KIND(index_ty, eType::array)->array.size;

      Symbol* size_constant = ir_context->sym_context->create_const_object_int(index->src_loc, size_val);
      IrArg* dim_size = ir_arg_new_existing_object(ir_context, size_constant);

      if(size_val > 0)
      {
        ir_emit_assign(ir_context, eIrOp::mul, array_expr->index.i_place, dim_size, offset);
        ir_emit_assign(ir_context, eIrOp::add, offset, i_expr->place, offset);
      }
      else
        success = compile_error(ir_context->gp_arena, i_expr->src_loc, "array dim size <= 0");
    }
  }
  else assert(0);

  return success;
}

bool ir_gen_index_with_offset(IrContext* ir_context, Scope* scope, AstNode* index)
{
  assert(KIND(index, eAstNode::index));
  bool success = true;

  if(success = ir_gen_index(ir_context, scope, index))
  {
    IrArg* offset = index->index.offset = ir_arg_new_temp_object(ir_context, scope,
                                                                 ir_context->basic_type_int, index->src_loc);

    assert(index->index.ndim == 1);
    int width_val = index->eval_ty->width;

    Symbol* width_constant = ir_context->sym_context->create_const_object_int(index->src_loc, width_val);
    IrArg* width = ir_arg_new_existing_object(ir_context, width_constant);

    ir_emit_assign(ir_context, eIrOp::mul, index->index.i_place, width, offset);
  }

  return success;
}

bool ir_gen_assign(IrContext* ir_context, Scope* scope, AstNode* assign)
{
  assert(KIND(assign, eAstNode::assign));
  bool success = true;
  
  AstNode* dest_expr = assign->assign.dest_expr;
  AstNode* source_expr = assign->assign.source_expr;
  
  if(dest_expr->kind == eAstNode::id)
  {
    if(success = ir_gen_expr(ir_context, scope, dest_expr) && ir_gen_expr(ir_context, scope, source_expr))
    {
      ir_emit_assign(ir_context, eIrOp::None, source_expr->place, 0, dest_expr->place);
    }
  }
  else if(dest_expr->kind == eAstNode::index)
  {
    if(success = ir_gen_index_with_offset(ir_context, scope, dest_expr) && ir_gen_expr(ir_context, scope, source_expr))
    {
      dest_expr->place = dest_expr->index.place;
      ir_emit_assign(ir_context, eIrOp::index_dest, source_expr->place, dest_expr->index.offset, dest_expr->index.place);
    }
  }
  else if(dest_expr->kind == eAstNode::unr_expr && dest_expr->unr_expr.op == eOperator::deref)
  {
    AstNode* operand = dest_expr->unr_expr.operand;
    if(success = ir_gen_expr(ir_context, scope, operand) && ir_gen_expr(ir_context, scope, source_expr))
    {
      dest_expr->place = operand->place;
      ir_emit_assign(ir_context, eIrOp::deref_dest, source_expr->place, 0, dest_expr->place);
    }
  }
  else
    success = compile_error(ir_context->gp_arena, dest_expr->src_loc, "unsupported expression on the left-side of assignment");

  return success;
}

bool ir_gen_cast(IrContext* ir_context, Scope* scope, AstNode* cast)
{
  assert(KIND(cast, eAstNode::cast));
  bool success = true;
  AstNode* to_type = cast->cast.to_type;
  AstNode* from_expr = cast->cast.from_expr;
  
  if(success = ir_gen_expr(ir_context, scope, from_expr))
  {
    cast->place = from_expr->place;
    
    if(to_type->eval_ty->equal(from_expr->eval_ty) ||
       ((to_type->eval_ty->kind == from_expr->eval_ty->kind) && (to_type->eval_ty->kind == eType::pointer)))
    {
      return success;
    }
    bool require_conv = true;
    if(to_type->eval_ty->equal(ir_context->basic_type_int))
    {
      // int <- pointer
      require_conv = from_expr->eval_ty->kind != eType::pointer;
    }
    else if(to_type->eval_ty->kind == eType::pointer)
    {
      // pointer <- int
      require_conv = !from_expr->eval_ty->equal(ir_context->basic_type_int);
    }
    if(require_conv)
    {
      cast->place = ir_arg_new_temp_object(ir_context, scope, cast->eval_ty, cast->src_loc);

      eIrOp cast_op = eIrOp::None;

      if(to_type->eval_ty->equal(ir_context->basic_type_int))
      {
        if(from_expr->eval_ty->equal(ir_context->basic_type_float))
        {
          cast_op = eIrOp::ftoi; // int <- float
        }
        else if(from_expr->eval_ty->equal(ir_context->basic_type_bool))
        {
          cast_op = eIrOp::btoi; // int <- bool
        }
        else if(from_expr->eval_ty->equal(ir_context->basic_type_char))
        {
          cast_op = eIrOp::ctoi; // int <- char
        }
        else assert(0);
      }
      else if(to_type->eval_ty->equal(ir_context->basic_type_float))
      {
        if(from_expr->eval_ty->equal(ir_context->basic_type_int))
        {
          cast_op = eIrOp::itof; // float <- int
        }
        else assert(0);
      }
      else if(to_type->eval_ty->equal(ir_context->basic_type_char))
      {
        if(from_expr->eval_ty->equal(ir_context->basic_type_int))
        {
          cast_op = eIrOp::itoc; // char <- int
        }
        else assert(0);
      }
      else if(to_type->eval_ty->equal(ir_context->basic_type_bool))
      {
        if(from_expr->eval_ty->equal(ir_context->basic_type_int))
        {
          cast_op = eIrOp::itob; // bool <- int
        }
        else if(from_expr->eval_ty->kind == eType::pointer)
        {
          cast_op = eIrOp::itob; // bool <- pointer(T)
        }
        else assert(0);
      }
      ir_emit_assign(ir_context, cast_op, from_expr->place, 0, cast->place);
    }
  }

  return success;
}

bool ir_gen_expr(IrContext* ir_context, Scope* scope, AstNode* expr)
{
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode::bin_expr:
    {
      eOperator op = expr->bin_expr.op;
      if(is_operator_relation(op) || is_operator_logic(op))
      {
        expr->label_true = label_gen_new(ir_context->gp_arena);
        expr->label_false = label_gen_new(ir_context->gp_arena);
        expr->label_next = label_gen_new(ir_context->gp_arena);

        Symbol* result_object = create_temp_object(ir_context, scope, expr->eval_ty, expr->src_loc);
        result_object->is_live_on_exit = true;
        result_object->is_live = true;
        expr->place = ir_arg_new_existing_object(ir_context, result_object);

        ir_gen_bool_expr(ir_context, scope, expr);
        
        ir_emit_label(ir_context, expr->label_true);
        ir_emit_assign(ir_context, eIrOp::None,
                       ir_arg_new_existing_object(ir_context, ir_context->bool_true), 0, expr->place);
        ir_emit_goto(ir_context, expr->label_next);
        ir_emit_label(ir_context, expr->label_false);
        ir_emit_assign(ir_context, eIrOp::None,
                       ir_arg_new_existing_object(ir_context, ir_context->bool_false), 0, expr->place);
        ir_emit_label(ir_context, expr->label_next);
      }
      else
      {
        ir_gen_bin_expr(ir_context, scope, expr);
      }
    }
    break;
    
    case eAstNode::unr_expr:
    {
      eOperator op = expr->unr_expr.op;
      if(is_operator_relation(op) || is_operator_logic(op))
      {
        expr->label_true = label_gen_new(ir_context->gp_arena);
        expr->label_false = label_gen_new(ir_context->gp_arena);
        expr->label_next = label_gen_new(ir_context->gp_arena);

        Symbol* result_object = create_temp_object(ir_context, scope,  expr->eval_ty, expr->src_loc);
        result_object->is_live_on_exit = true;
        result_object->is_live = true;
        expr->place = ir_arg_new_existing_object(ir_context, result_object);

        ir_gen_bool_unr_expr(ir_context, scope, expr);

        ir_emit_label(ir_context, expr->label_true);
        ir_emit_assign(ir_context, eIrOp::None,
                       ir_arg_new_existing_object(ir_context, ir_context->bool_true), 0, expr->place);
        ir_emit_goto(ir_context, expr->label_next);
        ir_emit_label(ir_context, expr->label_false);
        ir_emit_assign(ir_context, eIrOp::None,
                       ir_arg_new_existing_object(ir_context, ir_context->bool_false), 0, expr->place);
        ir_emit_label(ir_context, expr->label_next);
      }
      else
      {
        ir_gen_unr_expr(ir_context, scope, expr);
      }
    }
    break;
    
    case eAstNode::id:
    {
      ir_gen_id(ir_context, expr);
    }
    break;
    
    case eAstNode::lit:
    {
      ir_gen_lit(ir_context, scope, expr);
    }
    break;
    
    case eAstNode::call:
    {
      ir_gen_call(ir_context, scope, expr);
    }
    break;
    
    case eAstNode::index:
    {
      if(success = ir_gen_index_with_offset(ir_context, scope, expr))
      {
        expr->place = ir_arg_new_temp_object(ir_context, scope, expr->eval_ty, expr->src_loc);

        ir_emit_assign(ir_context, eIrOp::index_source, expr->index.place, expr->index.offset, expr->place);
      }
    }
    break;
    
    case eAstNode::cast:
    {
      ir_gen_cast(ir_context, scope, expr);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool ir_gen_block(IrContext* ir_context, Scope* scope, AstNode* block)
{
  assert(KIND(block, eAstNode::block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList::ast_node)->ast_node;
    ir_gen_block_stmt(ir_context, scope, stmt);
  }

  return success;
}

bool ir_gen_bool_bin_expr(IrContext* ir_context, Scope* scope, AstNode* bin_expr)
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
      if(success = ir_gen_expr(ir_context, scope, left_operand) && ir_gen_expr(ir_context, scope, right_operand))
      {
        ir_emit_cond_goto(ir_context, conv_operator_to_ir_op(op), left_operand->place, right_operand->place, bin_expr->label_true);
        ir_emit_goto(ir_context, bin_expr->label_false);
      }
    }
    break;
    
    case eOperator::logic_or:
    {
      assert(bin_expr->label_true);
      assert(bin_expr->label_false);

      left_operand->label_true = bin_expr->label_true;
      left_operand->label_false = label_gen_new(ir_context->gp_arena);
      right_operand->label_true = bin_expr->label_true;
      right_operand->label_false = bin_expr->label_false;
      if(success = ir_gen_bool_expr(ir_context, scope, left_operand))
      {
        ir_emit_label(ir_context, left_operand->label_false);
        success = ir_gen_bool_expr(ir_context, scope, right_operand);
      }
    }
    break;
    
    case eOperator::logic_and:
    {
      assert(bin_expr->label_true);
      assert(bin_expr->label_false);

      left_operand->label_true = label_gen_new(ir_context->gp_arena);
      left_operand->label_false = bin_expr->label_false;
      right_operand->label_true = bin_expr->label_true;
      right_operand->label_false = bin_expr->label_false;
      if(success = ir_gen_bool_expr(ir_context, scope, left_operand))
      {
        ir_emit_label(ir_context, left_operand->label_true);
        success = ir_gen_bool_expr(ir_context, scope, right_operand);
      }
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool ir_gen_bool_id(IrContext* ir_context, Scope* scope, AstNode* id)
{
  assert(KIND(id, eAstNode::id));
  bool success = true;

  if(success = ir_gen_expr(ir_context, scope, id))
  {
    ir_emit_cond_goto(ir_context, eIrOp::not_eq_, id->place,
                      ir_arg_new_existing_object(ir_context, ir_context->bool_false), id->label_true);
    ir_emit_goto(ir_context, id->label_false);
  }

  return success;
}

bool ir_gen_bool_call(IrContext* ir_context, Scope* scope, AstNode* call)
{
  assert(KIND(call, eAstNode::call));
  bool success = true;

  if(success = ir_gen_expr(ir_context, scope, call))
  {
    ir_emit_cond_goto(ir_context, eIrOp::not_eq_, call->place,
                      ir_arg_new_existing_object(ir_context, ir_context->bool_false), call->label_true);
    ir_emit_goto(ir_context, call->label_false);
  }

  return success;
}

bool ir_gen_bool_cast(IrContext* ir_context, Scope* scope, AstNode* cast)
{
  assert(KIND(cast, eAstNode::cast));
  bool success = true;

  if(success = ir_gen_cast(ir_context, scope, cast))
  {
    ir_emit_cond_goto(ir_context, eIrOp::not_eq_, cast->place,
                      ir_arg_new_existing_object(ir_context, ir_context->bool_false), cast->label_true);
    ir_emit_goto(ir_context, cast->label_false);
  }

  return success;
}

void ir_gen_bool_lit(IrContext* ir_context, Scope* scope, AstNode* lit)
{
  assert(KIND(lit, eAstNode::lit));

  if(lit->lit.bool_val)
  {
    ir_emit_goto(ir_context, lit->label_true);
  }
  else
  {
    ir_emit_goto(ir_context, lit->label_false);
  }
}

bool ir_gen_bool_expr(IrContext* ir_context, Scope* scope, AstNode* expr)
{
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode::id:
    {
      success = ir_gen_bool_id(ir_context, scope, expr);
    }
    break;
    
    case eAstNode::lit:
    {
      ir_gen_bool_lit(ir_context, scope, expr);
    }
    break;
    
    case eAstNode::bin_expr:
    {
      success = ir_gen_bool_bin_expr(ir_context, scope, expr);
    }
    break;
    
    case eAstNode::unr_expr:
    {
      success = ir_gen_bool_unr_expr(ir_context, scope, expr);
    }
    break;
    
    case eAstNode::cast:
    {
      success = ir_gen_bool_cast(ir_context, scope, expr);
    }
    break;

    case eAstNode::call:
    {
      success = ir_gen_bool_call(ir_context, scope, expr);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool ir_gen_do_while(IrContext* ir_context, Scope* scope, AstNode* do_while)
{
  assert(KIND(do_while, eAstNode::do_while));
  bool success = true;

  AstNode* cond_expr = do_while->do_while.cond_expr;
  AstNode* body = do_while->do_while.body;
  
  do_while->label_begin = label_gen_new(ir_context->gp_arena);
  do_while->label_next = label_gen_new(ir_context->gp_arena);
  cond_expr->label_true = label_gen_new(ir_context->gp_arena);
  cond_expr->label_false = do_while->label_next;
  
  ir_emit_label(ir_context, cond_expr->label_true);
  ir_gen_block_stmt(ir_context, scope, body);
  ir_emit_label(ir_context, do_while->label_begin);
  if(success = ir_gen_bool_expr(ir_context, scope, cond_expr))
  {
    ir_emit_label(ir_context, do_while->label_next);
  }

  return success;
}

bool ir_gen_while(IrContext* ir_context, Scope* scope, AstNode* while_)
{
  assert(KIND(while_, eAstNode::while_));
  bool success = true;
  
  AstNode* cond_expr = while_->while_.cond_expr;
  AstNode* body = while_->while_.body;
  
  while_->label_begin = label_gen_new(ir_context->gp_arena);
  while_->label_next = label_gen_new(ir_context->gp_arena);
  cond_expr->label_true = label_gen_new(ir_context->gp_arena);
  cond_expr->label_false = while_->label_next;
  
  ir_emit_label(ir_context, while_->label_begin);
  if(success = ir_gen_bool_expr(ir_context, scope, cond_expr))
  {
    ir_emit_label(ir_context, cond_expr->label_true);
    ir_gen_block_stmt(ir_context, scope, body);
    ir_emit_goto(ir_context, while_->label_begin);
    ir_emit_label(ir_context, while_->label_next);
  }

  return success;
}

bool ir_gen_if(IrContext* ir_context, Scope* scope, AstNode* if_)
{
  assert(KIND(if_, eAstNode::if_));
  bool success = true;
  
  AstNode* cond_expr = if_->if_.cond_expr;
  AstNode* body = if_->if_.body;
  AstNode* else_body = if_->if_.else_body;
  
  if_->label_next = label_gen_new(ir_context->gp_arena);
  if(else_body)
  {
    cond_expr->label_true = label_gen_new(ir_context->gp_arena);
    cond_expr->label_false = label_gen_new(ir_context->gp_arena);
    body->label_next = if_->label_next;
    else_body->label_next = if_->label_next;
    
    if(success = ir_gen_bool_expr(ir_context, scope, cond_expr))
    {
      ir_emit_label(ir_context, cond_expr->label_true);
      ir_gen_block_stmt(ir_context, scope, body);
      ir_emit_goto(ir_context, if_->label_next);
      ir_emit_label(ir_context, cond_expr->label_false);
      ir_gen_block_stmt(ir_context, scope, else_body);
    }
  }
  else
  {
    cond_expr->label_true = label_gen_new(ir_context->gp_arena);
    cond_expr->label_false = if_->label_next;
    body->label_next = if_->label_next;
    
    if(success = ir_gen_bool_expr(ir_context, scope, cond_expr))
    {
      ir_emit_label(ir_context, cond_expr->label_true);
      ir_gen_block_stmt(ir_context, scope, body);
    }
  }
  if(success)
  {
    ir_emit_label(ir_context, if_->label_next);
  }

  return success;
}

bool ir_gen_return(IrContext* ir_context, Scope* scope, AstNode* ret)
{
  assert(KIND(ret, eAstNode::return_));
  bool success = true;

  AstNode* ret_expr = ret->ret.expr;
  AstNode* proc = ret->ret.proc;

  if(ret_expr)
  {
    if(success = ir_gen_expr(ir_context, scope, ret_expr))
    {
      IrArg* retvar = ir_arg_new_existing_object(ir_context, proc->proc.retvar);

      ir_emit_assign(ir_context, eIrOp::None, ret_expr->place, 0, retvar);
    }
  }

  ir_emit_goto(ir_context, proc->label_next);

  return success;
}

bool ir_gen_loop_ctrl(IrContext* ir_context, Scope* scope, AstNode* loop_ctrl)
{
  assert(KIND(loop_ctrl, eAstNode::loop_ctrl));
  bool success = true;

  AstNode* loop = loop_ctrl->loop_ctrl.loop;
  if(loop_ctrl->loop_ctrl.kind == eLoopCtrl::break_)
  {
    ir_emit_goto(ir_context, loop->label_next);
  }
  else if(loop_ctrl->loop_ctrl.kind == eLoopCtrl::continue_)
  {
    ir_emit_goto(ir_context, loop->label_begin);
  }
  else assert(0);

  return success;
}

bool ir_gen_var(IrContext* ir_context, Scope* scope, AstNode* var)
{
  bool success = true;
  assert(KIND(var, eAstNode::var));

  Symbol* object = var->var.decl_sym;
  alloc_data_object_incremental(ir_context, object, scope);
  add_object_to_memory(ir_context->x86_context, object);
  var->place = ir_arg_new_existing_object(ir_context, object);

  AstNode* init_expr = var->var.init_expr;
  if(init_expr)
  {
    if(success = ir_gen_expr(ir_context, scope, init_expr))
    {
      ir_emit_assign(ir_context, eIrOp::None, init_expr->place, 0, var->place);
    }
  }

  return success;
}

bool ir_gen_block_stmt(IrContext* ir_context, Scope* scope, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode::assign:
    {
      success = ir_gen_assign(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode::cast:
    {
      success = ir_gen_cast(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode::bin_expr:
    case eAstNode::unr_expr:
    case eAstNode::id:
    case eAstNode::call:
    case eAstNode::index:
    case eAstNode::lit:
    {
      success = ir_gen_expr(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode::block:
    {
      success = ir_gen_block(ir_context, stmt->block.scope, stmt);
    }
    break;
    
    case eAstNode::if_:
    {
      success = ir_gen_if(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode::do_while:
    {
      success = ir_gen_do_while(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode::while_:
    {
      success = ir_gen_while(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode::var:
    {
      success = ir_gen_var(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode::return_:
    {
      success = ir_gen_return(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode::empty:
    break;

    case eAstNode::loop_ctrl:
    {
      success = ir_gen_loop_ctrl(ir_context, scope, stmt);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

void ir_gen_formal_args(IrContext* ir_context, Scope* scope, AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  for(ListItem* li = args->args.node_list.first;
      li;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList::ast_node)->ast_node;
    Symbol* arg_object = KIND(arg, eAstNode::var)->var.decl_sym;
    alloc_data_object(ir_context, arg_object, scope);
    add_object_to_memory(ir_context->x86_context, arg_object);
  }
}

int get_proc_arg_size(AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  int size = 0;

  for(ListItem* li = args->args.node_list.first;
      li;
      li = li->next)
  {
    AstNode* var = KIND(li, eList::ast_node)->ast_node;
    Symbol* arg_object = var->var.decl_sym;
    size += arg_object->ty->set_width();
  }

  return size;
}

bool ir_gen_proc(IrContext* ir_context, Scope* scope, AstNode* proc)
{
  assert(KIND(proc, eAstNode::proc));
  bool success = true;
  
  proc->place = ir_arg_new_existing_object(ir_context, proc->proc.retvar);
  ir_gen_formal_args(ir_context, proc->proc.param_scope, proc->proc.args);
  alloc_data_object(ir_context, proc->proc.retvar, proc->proc.param_scope);

  Label* label_name = &proc->proc.label_name;
  label_name->stmt_nr = 0;

  if(proc->proc.is_extern())
  {
    int arg_size = get_proc_arg_size(proc->proc.args);
    char* name = proc->proc.name;
    String decorated_label = {};
    decorated_label.init(ir_context->gp_arena);
    decorated_label.printf("%s@%d", name, arg_size);

    label_name->name = decorated_label.cap();
  }
  else
  {
    label_name->name = proc->proc.name;

    AstNode* body = proc->proc.body;
    assert(KIND(body, eAstNode::block));

    proc->label_begin = label_gen_new(ir_context->gp_arena);
    proc->label_next = label_gen_new(ir_context->gp_arena);

    ir_emit_label(ir_context, proc->label_begin);

    if(success = ir_gen_block_stmt(ir_context, body->block.scope, body))
    {
      ir_emit_label(ir_context, proc->label_next);
      ir_emit_return(ir_context);
    }

    Scope* proc_scope = proc->proc.scope;
    proc_scope->allocd_size = ir_context->current_alloc_offset;
  }

  return success;
}

void ir_gen_module_var(IrContext* ir_context, Scope* scope, AstNode* var)
{
  assert(KIND(var, eAstNode::var));

  Symbol* object = var->var.decl_sym;
  add_object_to_memory(ir_context->x86_context, object);
}

bool ir_gen_module_stmt(IrContext* ir_context, Scope* scope, AstNode* stmt)
{
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode::proc:
    {
      if(success = ir_gen_proc(ir_context, scope, stmt))
      {
        stmt->proc.ir_stmt_array = ir_context->stmt_array;
        stmt->proc.ir_stmt_count = ir_context->stmt_count;

        reset_ir_context(ir_context);
      }
    }
    break;
    
    case eAstNode::var:
      ir_gen_module_var(ir_context, scope, stmt);
    break;
    
    default: assert(0);
  }

  return success;
}

bool ir_gen_module(IrContext* ir_context, AstNode* module)
{
  assert(KIND(module, eAstNode::module));
  bool success = true;
  
  for(ListItem* li = module->module.nodes.first;
      li;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList::ast_node)->ast_node;
    success = ir_gen_module_stmt(ir_context, module->module.scope, stmt);
  }

  return success;
}

void DEBUG_print_ir_op(String* text, eIrOp op)
{
  switch(op)
  {
    case eIrOp::add:
      text->printf("+");
    break;
    
    case eIrOp::sub:
      text->printf("-");
    break;
    
    case eIrOp::mul:
      text->printf("*");
    break;
    
    case eIrOp::div:
      text->printf("/");
    break;
    
    case eIrOp::mod:
      text->printf("mod");
    break;
    
    case eIrOp::neg:
      text->printf("-");
    break;
    
    case eIrOp::eq:
      text->printf("==");
    break;
    
    case eIrOp::not_eq_:
      text->printf("<>");
    break;
    
    case eIrOp::less:
      text->printf("<");
    break;
    
    case eIrOp::less_eq:
      text->printf("<=");
    break;
    
    case eIrOp::greater:
      text->printf(">");
    break;
    
    case eIrOp::greater_eq:
      text->printf(">=");
    break;
    
    case eIrOp::logic_and:
      text->printf("and");
    break;
    
    case eIrOp::logic_or:
      text->printf("or");
    break;
    
    case eIrOp::logic_not:
      text->printf("not");
    break;
    
    case eIrOp::bit_and:
      text->printf("&");
    break;
    
    case eIrOp::bit_or:
      text->printf("|");
    break;
    
    case eIrOp::bit_xor:
      text->printf("~");
    break;
    
    case eIrOp::bit_not:
      text->printf("!");
    break;
    
    case eIrOp::bit_shift_left:
      text->printf("<<");
    break;
    
    case eIrOp::bit_shift_right:
      text->printf(">>");
    break;
    
    case eIrOp::itof:
      text->printf("itof");
    break;
    
    case eIrOp::itoc:
      text->printf("itoc");
    break;
    
    case eIrOp::itob:
      text->printf("itob");
    break;
    
    case eIrOp::ftoi:
      text->printf("ftoi");
    break;
    
    case eIrOp::ctoi:
      text->printf("ctoi");
    break;
    
    case eIrOp::btoi:
      text->printf("btoi");
    break;
    
    default: assert(0);
  }
}

void DEBUG_print_ir_arg(IrContext* ir_context, String* text, IrArg* arg)
{
  Symbol* object = arg->object;

  switch(object->kind)
  {
    case eSymbol::None:
    {
      text->printf("%s", arg->object->name);
    }
    break;
    
    case eSymbol::constant:
    {
      if(object->ty->equal(ir_context->basic_type_int) || object->ty->equal(ir_context->basic_type_bool))
      {
        text->printf("%d", object->int_val);
      }
      else if(object->ty->equal(ir_context->basic_type_float))
      {
        text->printf("%f", object->float_val);
      }
      else if(object->ty->equal(ir_context->basic_type_char))
      {
        char buf[3] = {0};
        Cstr::print_char(buf, object->char_val);
        text->printf("'%s'", buf);
      }
      else if(object->ty->equal(ir_context->basic_type_str))
      {
        text->printf("\"%s\"", object->str_val);
      }
      else assert(0);
    }
    break;
    
    default: assert(0);
  }
}

void DEBUG_print_ir_stmt(IrContext* ir_context, String* text, IrStmt* stmt)
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
          DEBUG_print_ir_arg(ir_context, text, assign->result);
          text->printf(" = ");
          DEBUG_print_ir_arg(ir_context, text, assign->arg1);
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
          DEBUG_print_ir_arg(ir_context, text, assign->result);
          text->printf(" = ");
          DEBUG_print_ir_arg(ir_context, text, assign->arg1);
          text->printf(" ");
          DEBUG_print_ir_op(text, assign->op);
          text->printf(" ");
          DEBUG_print_ir_arg(ir_context, text, assign->arg2);
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
          DEBUG_print_ir_arg(ir_context, text, assign->result);
          text->printf(" = ");
          DEBUG_print_ir_op(text, assign->op);
          text->printf(" ");
          DEBUG_print_ir_arg(ir_context, text, assign->arg1);
        }
        break;
        
        case eIrOp::index_dest:
        {
          DEBUG_print_ir_arg(ir_context, text, assign->result);
          text->printf("[");
          DEBUG_print_ir_arg(ir_context, text, assign->arg2);
          text->printf("] = ");
          DEBUG_print_ir_arg(ir_context, text, assign->arg1);
        }
        break;
        
        case eIrOp::index_source:
        {
          DEBUG_print_ir_arg(ir_context, text, assign->result);
          text->printf(" = ");
          DEBUG_print_ir_arg(ir_context, text, assign->arg1);
          text->printf("[");
          DEBUG_print_ir_arg(ir_context, text, assign->arg2);
          text->printf("]");
        }
        break;

        case eIrOp::address_of:
        {
          DEBUG_print_ir_arg(ir_context, text, assign->result);
          text->printf(" = &");
          DEBUG_print_ir_arg(ir_context, text, assign->arg1);
        }
        break;

        case eIrOp::deref_dest:
        {
          text->printf("^");
          DEBUG_print_ir_arg(ir_context, text, assign->result);
          text->printf(" = ");
          DEBUG_print_ir_arg(ir_context, text, assign->arg1);
        }
        break;

        case eIrOp::deref_source:
        {
          DEBUG_print_ir_arg(ir_context, text, assign->result);
          text->printf(" = ^");
          DEBUG_print_ir_arg(ir_context, text, assign->arg1);
        }
        break;
        
        default: assert(0);
      }
    }
    break;
    
    case eIrStmt::cond_goto:
    {
      IrStmt_CondGoto* cond_goto = &stmt->cond_goto;
      text->printf("if ");
      DEBUG_print_ir_arg(ir_context, text, cond_goto->arg1);
      text->printf(" ");
      DEBUG_print_ir_op(text, cond_goto->relop);
      text->printf(" ");
      DEBUG_print_ir_arg(ir_context, text, cond_goto->arg2);
      text->printf(" goto %s", cond_goto->goto_label->name);
    }
    break;
    
    case eIrStmt::goto_:
    {
      IrStmt_Goto* goto_ = &stmt->goto_;
      text->printf("goto %s", goto_->goto_label->name);
    }
    break;
    
    case eIrStmt::call:
    {
      IrStmt_Call* call = &stmt->call;
      text->printf("call %s", call->name->name);
    }
    break;
    
    case eIrStmt::return_:
    {
      text->printf("return");
    }
    break;
    
    case eIrStmt::nop:
    {
      text->printf("nop");
    }
    break;
    
    default:
    {
      text->printf("???");
    }
  }
}

void DEBUG_print_basic_block(IrContext* ir_context, String* text, BasicBlock* bb)
{
  IrStmt** stmt_array = bb->stmt_array;
  for(int i = 0; i < bb->stmt_count; i++)
  {
    IrStmt* stmt = stmt_array[i];
    if(stmt->label)
    {
      text->printfln("%5s:", stmt->label->name);
    }
    text->printf("%5d: ", i);
    DEBUG_print_ir_stmt(ir_context, text, stmt);
    text->println();
  }
}

void DEBUG_print_ir_code(IrContext* ir_context, List* procs, char* file_path)
{
  MemoryArena::begin_temp_memory(&ir_context->gp_arena);
  String text = {};
  text.init(ir_context->gp_arena);

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
      text.printfln("%5s:", proc->proc.name);

      List* basic_blocks = proc->proc.basic_blocks;
      for(ListItem* li = basic_blocks->first;
          li;
          li = li->next)
      {
        BasicBlock* bb = KIND(li, eList::basic_block)->basic_block;
        DEBUG_print_basic_block(ir_context, &text, bb);
      }
    }
  }

  text.dump_to_file(file_path);
  MemoryArena::end_temp_memory(&ir_context->gp_arena);
}

IrLeaderStmt* get_leader_stmt(List* leaders, int stmt_nr)
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

IrLeaderStmt* new_leader_stmt(MemoryArena* arena, int stmt_nr, IrStmt* stmt)
{
  IrLeaderStmt* new_elem = mem_push_struct(arena, IrLeaderStmt);
  new_elem->stmt_nr = stmt_nr;
  new_elem->stmt = stmt;
  Label* label = new_elem->label = stmt->label;
  if(label && label->primary)
  {
    new_elem->label = label->primary;
  }

  return new_elem;
}

void insert_leader_stmt(List* leaders, int stmt_nr, IrStmt* stmt)
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
      leaders->insert_before(li, new_leader_stmt(leaders->arena, stmt_nr, stmt), eList::ir_leader_stmt);
    }
  }
  else
  {
    leaders->append(new_leader_stmt(leaders->arena, stmt_nr, stmt), eList::ir_leader_stmt);
  }
}

void start_new_basic_block(List* leaders, int at_stmt_nr, IrStmt* stmt_array, int stmt_count)
{
  if(at_stmt_nr < stmt_count)
  {
    insert_leader_stmt(leaders, at_stmt_nr, &stmt_array[at_stmt_nr]);
  }
}

void update_object_live_info(IrArg* result, IrArg* arg1, IrArg* arg2)
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

Label* normalize_jump_target_labels(IrStmt* stmt)
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

void ir_partition_basic_blocks_proc(IrContext* ir_context, AstNode* proc)
{
  if(proc->proc.ir_stmt_count > 0)
  {
    List* leaders = List::create(ir_context->gp_arena, eList::ir_leader_stmt);

    IrStmt* stmt_array = proc->proc.ir_stmt_array;
    int stmt_count = proc->proc.ir_stmt_count;

    IrStmt* stmt = &stmt_array[0];
    leaders->append(new_leader_stmt(leaders->arena, 0, stmt), eList::ir_leader_stmt);
    
    for(int i = 0; i < proc->proc.ir_stmt_count; i++)
    {
      stmt = &stmt_array[i];
      if(stmt->kind == eIrStmt::cond_goto || stmt->kind == eIrStmt::goto_)
      {
        start_new_basic_block(leaders, i+1, stmt_array, stmt_count);
        Label* target_label = normalize_jump_target_labels(stmt);
        start_new_basic_block(leaders, target_label->stmt_nr, stmt_array, stmt_count);
      }
    }

    //------//
    
    List* basic_blocks = proc->proc.basic_blocks = List::create(ir_context->stmt_arena, eList::basic_block);

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
      BasicBlock* block = mem_push_struct(ir_context->stmt_arena, BasicBlock);
      basic_blocks->append(block, eList::basic_block);
      block->pred_list.init(ir_context->stmt_arena, eList::basic_block);
      block->succ_list.init(ir_context->stmt_arena, eList::basic_block);
      leader->block = block;
      block->stmt_array = mem_push_array(ir_context->stmt_arena, IrStmt*, next_stmt_nr - leader->stmt_nr);
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

void ir_partition_basic_blocks_module(IrContext* ir_context, AstNode* module)
{
  List* procs = &module->module.procs;
  for(ListItem* li = procs->first;
      li;
      li = li->next)
  {
    AstNode* proc = KIND(li, eList::ast_node)->ast_node;

    ir_partition_basic_blocks_proc(ir_context, proc);
  }

  if(DEBUG_enabled)
    DEBUG_print_ir_code(ir_context, &module->module.procs, "./module.ir");
}
