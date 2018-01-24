IrLabel* new_gen_label(MemoryArena* arena)
{
  IrLabel* label = mem_push_struct(arena, IrLabel);
  gen_label_name(arena, label);
  return label;
}

IrLabel* new_label_by_name(MemoryArena* arena, char* name)
{
  IrLabel* label = mem_push_struct(arena, IrLabel);
  label->name = name;
  return label;
}

eIrOp conv_operator_to_ir_op(eOperator op)
{
  eIrOp ir_op = eIrOp_None;
  switch(op)
  {
    case eOperator_add:
      ir_op = eIrOp_add;
    break;
    
    case eOperator_sub:
      ir_op = eIrOp_sub;
    break;
    
    case eOperator_mul:
      ir_op = eIrOp_mul;
    break;
    
    case eOperator_div:
      ir_op = eIrOp_div;
    break;

    case eOperator_mod:
      ir_op = eIrOp_mod;
    break;
    
    case eOperator_neg:
      ir_op = eIrOp_neg;
    break;
    
    case eOperator_bit_and:
      ir_op = eIrOp_bit_and;
    break;
    
    case eOperator_bit_or:
      ir_op = eIrOp_bit_or;
    break;
    
    case eOperator_bit_xor:
      ir_op = eIrOp_bit_xor;
    break;
    
    case eOperator_bit_shift_left:
      ir_op = eIrOp_bit_shift_left;
    break;
    
    case eOperator_bit_shift_right:
      ir_op = eIrOp_bit_shift_right;
    break;
    
    case eOperator_less:
      ir_op = eIrOp_less;
    break;
    
    case eOperator_less_eq:
      ir_op = eIrOp_less_eq;
    break;
    
    case eOperator_greater:
      ir_op = eIrOp_greater;
    break;
    
    case eOperator_greater_eq:
      ir_op = eIrOp_greater_eq;
    break;
    
    case eOperator_eq:
      ir_op = eIrOp_eq;
    break;
    
    case eOperator_not_eq:
      ir_op = eIrOp_not_eq;
    break;
    
    case eOperator_address_of:
      ir_op = eIrOp_address_of;
    break;

    case eOperator_deref:
      ir_op = eIrOp_deref_source;
    break;

    default: assert(0);
  }

  return ir_op;
}

eOperator negate_relop(eOperator op)
{
  eOperator result = eOperator_None;
  switch(op)
  {
    case eOperator_eq:
      result = eOperator_not_eq;
    break;
    
    case eOperator_not_eq:
      result = eOperator_eq;
    break;
    
    case eOperator_less:
      result = eOperator_greater_eq;
    break;
    
    case eOperator_less_eq:
      result = eOperator_greater;
    break;
    
    case eOperator_greater:
      result = eOperator_less_eq;
    break;
    
    case eOperator_greater_eq:
      result = eOperator_less;
    break;
    
    default: assert(0);
  }

  return result;
}

IrLabel* get_label_at(List* label_list, int stmt_nr)
{
  IrLabel* label = 0;
  for(ListItem* li = label_list->first;
      li;
      li = li->next)
  {
    label = KIND(li, eList_ir_label)->ir_label;
    if(label->stmt_nr == stmt_nr)
      break;
    label = 0;
  }

  return label;
}

void ir_emit_assign(IrContext* ir_context, eIrOp op, IrArg* arg1, IrArg* arg2, IrArg* result)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_assign;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);

  stmt->assign.op = op;
  stmt->assign.arg1 = mem_push_struct(arena, IrArg);
  *stmt->assign.arg1 = *arg1;
  if(arg2)
  {
    stmt->assign.arg2 = mem_push_struct(arena, IrArg);
    *stmt->assign.arg2 = *arg2;
  }
  stmt->assign.result = mem_push_struct(arena, IrArg);
  *stmt->assign.result = *result;

  ir_context->stmt_count++;
}

void ir_emit_label(IrContext* ir_context, IrLabel* label)
{
  label->kind = eIrLabelTarget_stmt_nr;
  label->stmt_nr = ir_context->stmt_count;
  
  IrLabel* prim_label = 0;
  for(ListItem* li = ir_context->label_list->last;
      li;
      li = li->prev)
  {
    prim_label = KIND(li, eList_ir_label)->ir_label;
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
    append_list_elem(ir_context->label_list, label, eList_ir_label);
  }
}

void ir_emit_nop(IrContext* ir_context)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);

  *stmt = (IrStmt){0};
  stmt->kind = eIrStmt_nop;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);

  ir_context->stmt_count++;
}

void ir_emit_cond_goto(IrContext* ir_context, eIrOp relop, IrArg* arg1, IrArg* arg2, IrLabel* label)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_cond_goto;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);

  stmt->cond_goto.relop = relop;
  stmt->cond_goto.arg1 = arg1;
  stmt->cond_goto.arg2 = arg2;
  stmt->cond_goto.label = label;

  ir_context->stmt_count++;
}

void ir_emit_goto(IrContext* ir_context, IrLabel* goto_label)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_goto;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);
  stmt->goto_label = goto_label;

  ir_context->stmt_count++;
}

void ir_emit_call(IrContext* ir_context, AstNode* proc)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_call;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);
  stmt->call.proc = proc;

  ir_context->stmt_count++;
}

void ir_emit_return(IrContext* ir_context)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_return;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);

  ir_context->stmt_count++;
}

void reset_ir_context(IrContext* ir_context)
{
  ir_context->stmt_array = &ir_context->stmt_array[ir_context->stmt_count];
  ir_context->total_stmt_count += ir_context->stmt_count;
  ir_context->stmt_count = 0;

  /*XXX: 'label_list' storage is a good candidate for begin_temp_memory()/end_temp_memory() pattern of allocation. */
  clear_list(ir_context->label_list);
}

IrArg* ir_new_arg_temp_object(IrContext* context, Scope* scope, Type* ty, SourceLoc* src_loc)
{
  IrArg* arg = mem_push_struct(arena, IrArg);
  arg->object = new_temp_object(context->sym_arena, scope, ty, src_loc, context->data_alignment);

  return arg;
}

IrArg* ir_new_arg_existing_object(IrContext* context, Symbol* object)
{
  IrArg* arg = mem_push_struct(arena, IrArg);
  arg->object = object;

  return arg;
}

bool ir_gen_expr(IrContext* ir_context, Scope* scope, AstNode* expr);
bool ir_gen_bool_expr(IrContext* ir_context, Scope* scope, AstNode* expr);
bool ir_gen_block_stmt(IrContext* ir_context, Scope* scope, AstNode* stmt);

bool ir_gen_bin_expr(IrContext* ir_context, Scope* scope, AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;
  
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  eOperator op = bin_expr->bin_expr.op;
  
  switch(op)
  {
    case eOperator_add:
    case eOperator_sub:
    case eOperator_mul:
    case eOperator_div:
    case eOperator_mod:
    case eOperator_bit_and:
    case eOperator_bit_or:
    case eOperator_bit_xor:
    case eOperator_bit_shift_left:
    case eOperator_bit_shift_right:
    case eOperator_less:
    case eOperator_less_eq:
    case eOperator_greater:
    case eOperator_greater_eq:
    case eOperator_eq:
    case eOperator_not_eq:
    {
      if(success = ir_gen_expr(ir_context, scope, left_operand) && ir_gen_expr(ir_context, scope, right_operand))
      {
        bin_expr->place = ir_new_arg_temp_object(ir_context, scope, bin_expr->eval_ty, bin_expr->src_loc);

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
  assert(KIND(id, eAstNode_id));
  id->place = ir_new_arg_existing_object(ir_context, id->id.decl_sym);
}

bool ir_gen_unr_expr(IrContext* ir_context, Scope* scope, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;
  
  switch(op)
  {
    case eOperator_neg:
    {
      if(success = ir_gen_expr(ir_context, scope, operand))
      {
        unr_expr->place = ir_new_arg_temp_object(ir_context, scope, unr_expr->eval_ty, unr_expr->src_loc);
        ir_emit_assign(ir_context, conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
      }
    }
    break;
    
    case eOperator_logic_not:
    {
      fail("todo");
    }
    break;

    case eOperator_address_of:
    {
      if(success = ir_gen_expr(ir_context, scope, operand))
      {
        unr_expr->place = ir_new_arg_temp_object(ir_context, scope, unr_expr->eval_ty, unr_expr->src_loc);
        ir_emit_assign(ir_context, conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
      }
    }
    break;

    case eOperator_deref:
    {
      if(success = ir_gen_expr(ir_context, scope, operand))
      {
        unr_expr->place = ir_new_arg_temp_object(ir_context, scope, unr_expr->eval_ty, unr_expr->src_loc);
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
  assert(KIND(lit, eAstNode_lit));
  
  Symbol* object = lit->lit.constant;
  if(types_are_equal(object->ty, basic_type_str))
  {
    alloc_data_object(object, object->scope, ir_context->data_alignment);
  }

  lit->place = ir_new_arg_existing_object(ir_context, object);
}

bool ir_gen_bool_unr_expr(IrContext* ir_context, Scope* scope, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;
  
  switch(op)
  {
    case eOperator_logic_not:
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
  assert(KIND(args, eAstNode_node_list));
  bool success = true;

  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    AstNode* expr = arg->actual_arg.expr;
    if(success = ir_gen_expr(ir_context, scope, expr))
    {
      IrArg* place = ir_new_arg_existing_object(ir_context, arg->actual_arg.param);

      ir_emit_assign(ir_context, eIrOp_None, expr->place, 0, place);
    }
  }

  return success;
}

void ir_gen_call(IrContext* ir_context, Scope* scope, AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  
  call->place = ir_new_arg_existing_object(ir_context, call->call.retvar);

  AstNode* args = call->call.args;
  ir_gen_actual_args(ir_context, scope, args);

  for(ListItem* li = args->node_list.first;
      li;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    alloc_data_object(arg->actual_arg.param, call->call.param_scope, ir_context->data_alignment);
  }
  alloc_data_object(call->call.retvar, call->call.param_scope, ir_context->data_alignment);

  ir_emit_call(ir_context, call->call.proc);
}

bool ir_gen_index(IrContext* ir_context, Scope* scope, AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;

  AstNode* array_expr = index->index.array_expr;
  AstNode* i_expr = index->index.i_expr;
  
  Type* array_ty = array_expr->eval_ty;
  if(array_ty->kind == eType_array)
  {
    index->index.array_ty = array_ty;
  }
  else if(array_ty->kind == eType_pointer)
  {
    index->index.array_ty = new_array_type(0, 1, array_ty->pointer.pointee);
  }
  else assert(0);

  if(array_expr->kind == eAstNode_id)
  {
    ir_gen_id(ir_context, array_expr);
    index->index.place = array_expr->place;

    if(success = ir_gen_expr(ir_context, scope, i_expr))
    {
      index->index.i_place = i_expr->place;
    }
  }
  else if(array_expr->kind == eAstNode_index)
  {
    if(success = ir_gen_index(ir_context, scope, array_expr) && ir_gen_expr(ir_context, scope, i_expr))
    {
      index->index.place = array_expr->index.place;

      IrArg* offset = index->index.i_place = ir_new_arg_temp_object(ir_context, scope, basic_type_int, index->src_loc);

      Symbol* size_constant = new_const_object(ir_context->sym_arena, basic_type_int, index->src_loc);
      int size_val = size_constant->int_val = size_of_array_dim(index->index.array_ty, index->index.ndim);
      IrArg* dim_size = ir_new_arg_existing_object(ir_context, size_constant);

      if(size_val > 0)
      {
        ir_emit_assign(ir_context, eIrOp_mul, array_expr->index.i_place, dim_size, offset);
        ir_emit_assign(ir_context, eIrOp_add, offset, i_expr->place, offset);
      }
      else
        success = compile_error(i_expr->src_loc, "array dim size = 0");
    }
  }
  else assert(0);

  return success;
}

bool ir_gen_index_with_offset(IrContext* ir_context, Scope* scope, AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;

  if(success = ir_gen_index(ir_context, scope, index))
  {
    IrArg* offset = index->index.offset = ir_new_arg_temp_object(ir_context, scope, basic_type_int, index->src_loc);

    Symbol* width_constant = new_const_object(ir_context->sym_arena, basic_type_int, index->src_loc);
    width_constant->int_val = array_elem_width(index->index.array_ty);
    IrArg* width = ir_new_arg_existing_object(ir_context, width_constant);

    ir_emit_assign(ir_context, eIrOp_mul, index->index.i_place, width, offset);
  }

  return success;
}

bool ir_gen_assign(IrContext* ir_context, Scope* scope, AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;
  
  AstNode* dest_expr = assign->assign.dest_expr;
  AstNode* source_expr = assign->assign.source_expr;
  
  if(dest_expr->kind == eAstNode_id)
  {
    if(success = ir_gen_expr(ir_context, scope, dest_expr) && ir_gen_expr(ir_context, scope, source_expr))
    {
      ir_emit_assign(ir_context, eIrOp_None, source_expr->place, 0, dest_expr->place);
    }
  }
  else if(dest_expr->kind == eAstNode_index)
  {
    if(success = ir_gen_index_with_offset(ir_context, scope, dest_expr) && ir_gen_expr(ir_context, scope, source_expr))
    {
      dest_expr->place = dest_expr->index.place;
      ir_emit_assign(ir_context, eIrOp_index_dest, source_expr->place, dest_expr->index.offset, dest_expr->index.place);
    }
  }
  else if(dest_expr->kind == eAstNode_unr_expr && dest_expr->unr_expr.op == eOperator_deref)
  {
    AstNode* operand = dest_expr->unr_expr.operand;
    if(success = ir_gen_expr(ir_context, scope, operand) && ir_gen_expr(ir_context, scope, source_expr))
    {
      dest_expr->place = operand->place;
      ir_emit_assign(ir_context, eIrOp_deref_dest, source_expr->place, 0, dest_expr->place);
    }
  }
  else
    success = compile_error(dest_expr->src_loc, "unsupported expression on the left-side of assignment");

  return success;
}

bool ir_gen_cast(IrContext* ir_context, Scope* scope, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;
  AstNode* to_type = cast->cast.to_type;
  AstNode* from_expr = cast->cast.from_expr;
  
  if(success = ir_gen_expr(ir_context, scope, from_expr))
  {
    cast->place = from_expr->place;
    
    if(types_are_equal(to_type->eval_ty, from_expr->eval_ty) ||
       ((to_type->eval_ty->kind == from_expr->eval_ty->kind) && (to_type->eval_ty->kind == eType_pointer)))
    {
      return success;
    }
    bool require_conv = true;
    if(types_are_equal(to_type->eval_ty, basic_type_int))
    {
      // int <- pointer
      require_conv = from_expr->eval_ty->kind != eType_pointer;
    }
    else if(to_type->eval_ty->kind == eType_pointer)
    {
      // pointer <- int
      require_conv = !types_are_equal(from_expr->eval_ty, basic_type_int);
    }
    if(require_conv)
    {
      cast->place = ir_new_arg_temp_object(ir_context, scope, cast->eval_ty, cast->src_loc);

      eIrOp cast_op = eIrOp_None;

      if(types_are_equal(to_type->eval_ty, basic_type_int))
      {
        if(types_are_equal(from_expr->eval_ty, basic_type_float))
        {
          cast_op = eIrOp_ftoi; // int <- float
        }
        else if(types_are_equal(from_expr->eval_ty, basic_type_bool))
        {
          cast_op = eIrOp_btoi; // int <- bool
        }
        else if(types_are_equal(from_expr->eval_ty, basic_type_char))
        {
          cast_op = eIrOp_ctoi; // int <- char
        }
        else assert(0);
      }
      else if(types_are_equal(to_type->eval_ty, basic_type_float))
      {
        if(types_are_equal(from_expr->eval_ty, basic_type_int))
        {
          cast_op = eIrOp_itof; // float <- int
        }
        else assert(0);
      }
      else if(types_are_equal(to_type->eval_ty, basic_type_char))
      {
        if(types_are_equal(from_expr->eval_ty, basic_type_int))
        {
          cast_op = eIrOp_itoc; // char <- int
        }
        else assert(0);
      }
      else if(types_are_equal(to_type->eval_ty, basic_type_bool))
      {
        if(types_are_equal(from_expr->eval_ty, basic_type_int))
        {
          cast_op = eIrOp_itob; // bool <- int
        }
        else if(from_expr->eval_ty->kind == eType_pointer)
        {
          cast_op = eIrOp_itob; // bool <- pointer(T)
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
    case eAstNode_bin_expr:
    {
      eOperator op = expr->bin_expr.op;
      if(op == eOperator_logic_and || op == eOperator_logic_or)
      {
        expr->label_true = new_gen_label(arena);
        expr->label_false = new_gen_label(arena);
        expr->label_next = new_gen_label(arena);

        expr->place = ir_new_arg_temp_object(ir_context, scope, expr->eval_ty, expr->src_loc);

        ir_gen_bool_expr(ir_context, scope, expr);
        
        ir_emit_label(ir_context, expr->label_true);
        ir_emit_assign(ir_context, eIrOp_None, ir_new_arg_existing_object(ir_context, bool_true), 0, expr->place);
        ir_emit_goto(ir_context, expr->label_next);
        ir_emit_label(ir_context, expr->label_false);
        ir_emit_assign(ir_context, eIrOp_None, ir_new_arg_existing_object(ir_context, bool_false), 0, expr->place);
        ir_emit_label(ir_context, expr->label_next);
      }
      else
        ir_gen_bin_expr(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      eOperator op = expr->unr_expr.op;
      if(op == eOperator_logic_not)
      {
        expr->label_true = new_gen_label(arena);
        expr->label_false = new_gen_label(arena);
        expr->label_next = new_gen_label(arena);

        expr->place = ir_new_arg_temp_object(ir_context, scope, expr->eval_ty, expr->src_loc);

        ir_gen_bool_unr_expr(ir_context, scope, expr);

        ir_emit_label(ir_context, expr->label_true);
        ir_emit_assign(ir_context, eIrOp_None, ir_new_arg_existing_object(ir_context, bool_true), 0, expr->place);
        ir_emit_goto(ir_context, expr->label_next);
        ir_emit_label(ir_context, expr->label_false);
        ir_emit_assign(ir_context, eIrOp_None, ir_new_arg_existing_object(ir_context, bool_false), 0, expr->place);
        ir_emit_label(ir_context, expr->label_next);
      }
      else
        ir_gen_unr_expr(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_id:
    {
      ir_gen_id(ir_context, expr);
    }
    break;
    
    case eAstNode_lit:
    {
      ir_gen_lit(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_call:
    {
      ir_gen_call(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_index:
    {
      if(success = ir_gen_index_with_offset(ir_context, scope, expr))
      {
        expr->place = ir_new_arg_temp_object(ir_context, scope, expr->eval_ty, expr->src_loc);

        ir_emit_assign(ir_context, eIrOp_index_source, expr->index.place, expr->index.offset, expr->place);
      }
    }
    break;
    
    case eAstNode_cast:
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
  assert(KIND(block, eAstNode_block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    ir_gen_block_stmt(ir_context, scope, stmt);
  }

  return success;
}

bool ir_gen_bool_bin_expr(IrContext* ir_context, Scope* scope, AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;
  
  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  eOperator op = bin_expr->bin_expr.op;
  
  switch(op)
  {
    case eOperator_eq:
    case eOperator_not_eq:
    case eOperator_less:
    case eOperator_less_eq:
    case eOperator_greater:
    case eOperator_greater_eq:
    {
      if(success = ir_gen_expr(ir_context, scope, left_operand) && ir_gen_expr(ir_context, scope, right_operand))
      {
        ir_emit_cond_goto(ir_context, conv_operator_to_ir_op(op), left_operand->place, right_operand->place, bin_expr->label_true);
        ir_emit_goto(ir_context, bin_expr->label_false);
      }
    }
    break;
    
    case eOperator_logic_or:
    {
      assert(bin_expr->label_true);
      assert(bin_expr->label_false);

      left_operand->label_true = bin_expr->label_true;
      left_operand->label_false = new_gen_label(arena);
      right_operand->label_true = bin_expr->label_true;
      right_operand->label_false = bin_expr->label_false;
      if(success = ir_gen_bool_expr(ir_context, scope, left_operand))
      {
        ir_emit_label(ir_context, left_operand->label_false);
        success = ir_gen_bool_expr(ir_context, scope, right_operand);
      }
    }
    break;
    
    case eOperator_logic_and:
    {
      assert(bin_expr->label_true);
      assert(bin_expr->label_false);

      left_operand->label_true = new_gen_label(arena);
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
  assert(KIND(id, eAstNode_id));
  bool success = true;

  if(success = ir_gen_expr(ir_context, scope, id))
  {
    ir_emit_cond_goto(ir_context, eIrOp_not_eq, id->place, ir_new_arg_existing_object(ir_context, bool_false), id->label_true);
    ir_emit_goto(ir_context, id->label_false);
  }

  return success;
}

bool ir_gen_bool_cast(IrContext* ir_context, Scope* scope, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;

  if(success = ir_gen_cast(ir_context, scope, cast))
  {
    ir_emit_cond_goto(ir_context, eIrOp_not_eq, cast->place, ir_new_arg_existing_object(ir_context, bool_false), cast->label_true);
    ir_emit_goto(ir_context, cast->label_false);
  }

  return success;
}

void ir_gen_bool_lit(IrContext* ir_context, Scope* scope, AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));

  if(lit->lit.bool_val != bool_false->int_val)
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
    case eAstNode_id:
    {
      success = ir_gen_bool_id(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_lit:
    {
      ir_gen_bool_lit(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = ir_gen_bool_bin_expr(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = ir_gen_bool_unr_expr(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_cast:
    {
      success = ir_gen_bool_cast(ir_context, scope, expr);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool ir_gen_do_while(IrContext* ir_context, Scope* scope, AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  AstNode* cond_expr = do_while->do_while.cond_expr;
  AstNode* body = do_while->do_while.body;
  
  do_while->label_begin = new_gen_label(arena);
  do_while->label_next = new_gen_label(arena);
  do_while->label_true = cond_expr->label_true = do_while->label_begin;
  do_while->label_false = cond_expr->label_false = new_gen_label(arena);
  body->label_next = do_while->label_next;
  
  ir_emit_label(ir_context, do_while->label_begin);
  ir_gen_block_stmt(ir_context, scope, body);
  ir_emit_label(ir_context, do_while->label_next);
  if(success = ir_gen_bool_expr(ir_context, scope, cond_expr))
  {
    ir_emit_label(ir_context, cond_expr->label_false);
  }

  return success;
}

bool ir_gen_while(IrContext* ir_context, Scope* scope, AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  
  AstNode* cond_expr = while_->while_.cond_expr;
  AstNode* body = while_->while_.body;
  
  while_->label_begin = new_gen_label(arena);
  while_->label_next = new_gen_label(arena);
  cond_expr->label_true = new_gen_label(arena);
  cond_expr->label_false = while_->label_next;
  body->label_next = while_->label_begin;
  
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
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  
  AstNode* cond_expr = if_->if_.cond_expr;
  AstNode* body = if_->if_.body;
  AstNode* else_body = if_->if_.else_body;
  
  if_->label_next = new_gen_label(arena);
  if(else_body)
  {
    cond_expr->label_true = new_gen_label(arena);
    cond_expr->label_false = new_gen_label(arena);
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
    cond_expr->label_true = new_gen_label(arena);
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
  assert(KIND(ret, eAstNode_return));
  bool success = true;

  AstNode* ret_expr = ret->ret.expr;
  AstNode* proc = ret->ret.proc;

  if(ret_expr)
  {
    if(success = ir_gen_expr(ir_context, scope, ret_expr))
    {
      IrArg* retvar = ir_new_arg_existing_object(ir_context, proc->proc.retvar);

      ir_emit_assign(ir_context, eIrOp_None, ret_expr->place, 0, retvar);
    }
  }

  ir_emit_goto(ir_context, proc->label_next);

  return success;
}

bool ir_gen_loop_ctrl(IrContext* ir_context, Scope* scope, AstNode* loop_ctrl)
{
  assert(KIND(loop_ctrl, eAstNode_loop_ctrl));
  bool success = true;

  AstNode* loop = loop_ctrl->loop_ctrl.loop;
  if(loop_ctrl->loop_ctrl.kind == eLoopCtrl_break)
  {
    ir_emit_goto(ir_context, loop->label_false);
  }
  else if(loop_ctrl->loop_ctrl.kind == eLoopCtrl_continue)
  {
    ir_emit_goto(ir_context, loop->label_next);
  }
  else assert(0);

  return success;
}

void ir_gen_var(IrContext* ir_context, Scope* scope, AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  alloc_data_object(var->var.decl_sym, scope, ir_context->data_alignment);
}

bool ir_gen_block_stmt(IrContext* ir_context, Scope* scope, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_assign:
    {
      success = ir_gen_assign(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = ir_gen_cast(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_index:
    case eAstNode_lit:
    {
      success = ir_gen_expr(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = ir_gen_block(ir_context, stmt->block.scope, stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = ir_gen_if(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = ir_gen_do_while(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = ir_gen_while(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_var:
    {
      ir_gen_var(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = ir_gen_return(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_empty:
    break;

    case eAstNode_loop_ctrl:
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
  assert(KIND(args, eAstNode_node_list));
  for(ListItem* li = args->node_list.first;
      li;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    ir_gen_var(ir_context, scope, arg);
  }
}

int get_proc_arg_size(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  int size = 0;

  for(ListItem* li = args->node_list.first;
      li;
      li = li->next)
  {
    AstNode* var = KIND(li, eList_ast_node)->ast_node;
    Symbol* arg_object = var->var.decl_sym;
    size += get_type_width(arg_object->ty);
  }

  return size;
}

bool ir_gen_proc(IrContext* ir_context, Scope* scope, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  
  proc->place = ir_new_arg_existing_object(ir_context, proc->proc.retvar);

  if((proc->modifier & eModifier_extern) != 0)
  {
    int arg_size = get_proc_arg_size(proc->proc.args);
    char* name = proc->proc.name;
    String decorated_label; str_init(&decorated_label, arena);
    str_printf(&decorated_label, "%s@%d", name, arg_size);
    proc->proc.decorated_name = str_cap(&decorated_label);
  }
  else
  {
    AstNode* body = proc->proc.body;
    assert(KIND(body, eAstNode_block));

    ir_gen_formal_args(ir_context, proc->proc.preamble_scope, proc->proc.args);
    alloc_data_object(proc->proc.retvar, proc->proc.preamble_scope, ir_context->data_alignment);

    IrLabel* label_start = &proc->proc.label_start;
    label_start->name = proc->proc.name;
    proc->label_begin = label_start;

    IrLabel* label_return = &proc->proc.label_return;
    gen_label_name(arena, label_return);
    proc->label_next = label_return;

    ir_emit_label(ir_context, label_start);

    if(success = ir_gen_block_stmt(ir_context, body->block.scope, body))
    {
      ir_emit_label(ir_context, label_return);
      ir_emit_return(ir_context);
    }
  }

  return success;
}

bool ir_gen_module_stmt(IrContext* ir_context, Scope* scope, AstNode* stmt)
{
  bool success = true;
  switch(stmt->kind)
  {
    case eAstNode_proc:
    {
      if(success = ir_gen_proc(ir_context, scope, stmt))
      {
        stmt->proc.ir_stmt_array = ir_context->stmt_array;
        stmt->proc.ir_stmt_count = ir_context->stmt_count;

        reset_ir_context(ir_context);
      }
    }
    break;
    
    case eAstNode_var:
    {
      alloc_data_object(stmt->var.decl_sym, scope, ir_context->data_alignment);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool ir_gen_module(IrContext* ir_context, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  for(ListItem* li = module->module.nodes.first;
      li;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = ir_gen_module_stmt(ir_context, module->module.scope, stmt);
  }

  return success;
}

void DEBUG_print_ir_op(String* text, eIrOp op)
{
  switch(op)
  {
    case eIrOp_add:
      str_printf(text, "+");
    break;
    
    case eIrOp_sub:
      str_printf(text, "-");
    break;
    
    case eIrOp_mul:
      str_printf(text, "*");
    break;
    
    case eIrOp_div:
      str_printf(text, "/");
    break;
    
    case eIrOp_mod:
      str_printf(text, "mod");
    break;
    
    case eIrOp_neg:
      str_printf(text, "-");
    break;
    
    case eIrOp_eq:
      str_printf(text, "==");
    break;
    
    case eIrOp_not_eq:
      str_printf(text, "<>");
    break;
    
    case eIrOp_less:
      str_printf(text, "<");
    break;
    
    case eIrOp_less_eq:
      str_printf(text, "<=");
    break;
    
    case eIrOp_greater:
      str_printf(text, ">");
    break;
    
    case eIrOp_greater_eq:
      str_printf(text, ">=");
    break;
    
    case eIrOp_logic_and:
      str_printf(text, "and");
    break;
    
    case eIrOp_logic_or:
      str_printf(text, "or");
    break;
    
    case eIrOp_logic_not:
      str_printf(text, "not");
    break;
    
    case eIrOp_bit_and:
      str_printf(text, "&");
    break;
    
    case eIrOp_bit_or:
      str_printf(text, "|");
    break;
    
    case eIrOp_bit_xor:
      str_printf(text, "~");
    break;
    
    case eIrOp_bit_not:
      str_printf(text, "!");
    break;
    
    case eIrOp_bit_shift_left:
      str_printf(text, "<<");
    break;
    
    case eIrOp_bit_shift_right:
      str_printf(text, ">>");
    break;
    
    case eIrOp_itof:
      str_printf(text, "itof");
    break;
    
    case eIrOp_itoc:
      str_printf(text, "itoc");
    break;
    
    case eIrOp_itob:
      str_printf(text, "itob");
    break;
    
    case eIrOp_ftoi:
      str_printf(text, "ftoi");
    break;
    
    case eIrOp_ctoi:
      str_printf(text, "ctoi");
    break;
    
    case eIrOp_btoi:
      str_printf(text, "btoi");
    break;
    
    default: assert(0);
  }
}

void DEBUG_print_ir_arg(String* text, IrArg* arg)
{
  Symbol* object = arg->object;

  switch(object->kind)
  {
    case eSymbol_None:
    {
      str_printf(text, "%s", arg->object->name);
    }
    break;
    
    case eSymbol_constant:
    {
      if(types_are_equal(object->ty, basic_type_int) || types_are_equal(object->ty, basic_type_bool))
      {
        str_printf(text, "%d", object->int_val);
      }
      else if(types_are_equal(object->ty, basic_type_float))
      {
        str_printf(text, "%f", object->float_val);
      }
      else if(types_are_equal(object->ty, basic_type_char))
      {
        char buf[3] = {0};
        print_char(buf, object->char_val);
        str_printf(text, "'%s'", buf);
      }
      else if(types_are_equal(object->ty, basic_type_str))
      {
        str_printf(text, "\"%s\"", object->str_val);
      }
      else assert(0);
    }
    break;
    
    default: assert(0);
  }
}

void DEBUG_print_ir_stmt(String* text, IrStmt* stmt)
{
  switch(stmt->kind)
  {
    case eIrStmt_assign:
    {
      struct IrStmt_assign* assign = &stmt->assign;

      switch(assign->op)
      {
        case eIrOp_None:
        {
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, " = ");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;
        
        /* bin_ops */
        case eIrOp_add:
        case eIrOp_sub:
        case eIrOp_mul:
        case eIrOp_div:
        case eIrOp_mod:
        case eIrOp_eq:
        case eIrOp_not_eq:
        case eIrOp_less:
        case eIrOp_less_eq:
        case eIrOp_greater:
        case eIrOp_greater_eq:
        case eIrOp_logic_and:
        case eIrOp_logic_or:
        case eIrOp_logic_not:
        case eIrOp_bit_and:
        case eIrOp_bit_or:
        case eIrOp_bit_xor:
        case eIrOp_bit_not:
        case eIrOp_bit_shift_left:
        case eIrOp_bit_shift_right:
        {
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, " = ");
          DEBUG_print_ir_arg(text, assign->arg1);
          str_printf(text, " ");
          DEBUG_print_ir_op(text, assign->op);
          str_printf(text, " ");
          DEBUG_print_ir_arg(text, assign->arg2);
        }
        break;
        
        /* unr_ops */
        case eIrOp_neg:
        case eIrOp_itof:
        case eIrOp_itoc:
        case eIrOp_itob:
        case eIrOp_ftoi:
        case eIrOp_ctoi:
        case eIrOp_btoi:
        {
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, " = ");
          DEBUG_print_ir_op(text, assign->op);
          str_printf(text, " ");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;
        
        case eIrOp_index_dest:
        {
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, "[");
          DEBUG_print_ir_arg(text, assign->arg2);
          str_printf(text, "] = ");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;
        
        case eIrOp_index_source:
        {
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, " = ");
          DEBUG_print_ir_arg(text, assign->arg1);
          str_printf(text, "[");
          DEBUG_print_ir_arg(text, assign->arg2);
          str_printf(text, "]");
        }
        break;

        case eIrOp_address_of:
        {
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, " = &");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;

        case eIrOp_deref_dest:
        {
          str_printf(text, "^");
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, " = ");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;

        case eIrOp_deref_source:
        {
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, " = ^");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;
        
        default: assert(0);
      }
    }
    break;
    
    case eIrStmt_cond_goto:
    {
      struct IrStmt_cond_goto* cond_goto = &stmt->cond_goto;
      str_printf(text, "if ");
      DEBUG_print_ir_arg(text, cond_goto->arg1);
      str_printf(text, " ");
      DEBUG_print_ir_op(text, cond_goto->relop);
      str_printf(text, " ");
      DEBUG_print_ir_arg(text, cond_goto->arg2);
      str_printf(text, " goto %s", cond_goto->label->name);
    }
    break;
    
    case eIrStmt_goto:
    {
      str_printf(text, "goto %s", stmt->goto_label->name);
    }
    break;
    
    case eIrStmt_call:
    {
      struct IrStmt_call* call = &stmt->call;
      AstNode* proc = call->proc;

      str_printf(text, "call %s", proc->label_begin->name);
    }
    break;
    
    case eIrStmt_return:
    {
      str_printf(text, "return");
    }
    break;
    
    case eIrStmt_nop:
    {
      str_printf(text, "nop");
    }
    break;
    
    default:
    {
      str_printf(text, "???");
    }
  }
}

void DEBUG_print_basic_block(MemoryArena* arena, String* text, BasicBlock* bb)
{
  IrStmt** stmt_array = bb->stmt_array;
  for(int i = 0; i < bb->stmt_count; i++)
  {
    IrStmt* stmt = stmt_array[i];
    if(stmt->label)
    {
      str_printfln(text, "%5s:", stmt->label->name);
    }
    str_printf(text, "%5d: ", i);
    DEBUG_print_ir_stmt(text, stmt);
    str_printfln(text, "");
  }
}

void DEBUG_print_ir_code(MemoryArena* arena, List* procs, char* file_path)
{
  begin_temp_memory(&arena);
  String text = {0};
  str_init(&text, arena);

  for(ListItem* li = procs->first;
      li;
      li = li->next)
  {
    AstNode* proc = KIND(li, eList_ast_node)->ast_node;

    if((proc->modifier & eModifier_extern) != 0)
    {
      ;//ok
    }
    else
    {
      List* basic_blocks = proc->proc.basic_blocks;
      for(ListItem* li = basic_blocks->first;
          li;
          li = li->next)
      {
        BasicBlock* bb = KIND(li, eList_basic_block)->basic_block;
        DEBUG_print_basic_block(arena, &text, bb);
      }
    }
  }

  str_dump_to_file(&text, file_path);
  end_temp_memory(&arena);
}

IrLeaderStmt* get_leader_stmt(List* leaders, int stmt_nr)
{
  ListItem* li = leaders->first;
  assert(li);
  IrLeaderStmt* leader = KIND(li, eList_ir_leader_stmt)->ir_leader_stmt;
  assert(leader->stmt_nr == 0);
  for(;
      li && (stmt_nr != leader->stmt_nr);
      li = li->next, leader = KIND(li, eList_ir_leader_stmt)->ir_leader_stmt)
  { }
  return leader->stmt_nr == stmt_nr ? leader : 0;
}

IrLeaderStmt* new_leader_stmt(MemoryArena* arena, int stmt_nr, IrStmt* stmt)
{
  IrLeaderStmt* new_elem = mem_push_struct(arena, IrLeaderStmt);
  new_elem->stmt_nr = stmt_nr;
  new_elem->stmt = stmt;
  IrLabel* label = new_elem->label = stmt->label;
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

  IrLeaderStmt* leader = KIND(li, eList_ir_leader_stmt)->ir_leader_stmt;
  assert(leader->stmt_nr == 0);

  for(;
      li && (leader->stmt_nr < stmt_nr);
      li = li->next, leader = (li ? KIND(li, eList_ir_leader_stmt)->ir_leader_stmt : 0))
  { }

  if(leader)
  {
    if(leader->stmt_nr > stmt_nr)
    {
      insert_elem_before(leaders, li, new_leader_stmt(leaders->arena, stmt_nr, stmt), eList_ir_leader_stmt);
    }
  }
  else
  {
    append_list_elem(leaders, new_leader_stmt(leaders->arena, stmt_nr, stmt), eList_ir_leader_stmt);
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

IrLabel* normalize_jump_target_labels(IrStmt* stmt)
{
  IrLabel* target_label = 0;

  if(stmt->kind == eIrStmt_cond_goto)
  {
    target_label = stmt->cond_goto.label;
    if(target_label->primary)
    {
      stmt->cond_goto.label = target_label->primary;
      target_label = stmt->cond_goto.label;
    }
  }
  else if(stmt->kind == eIrStmt_goto)
  {
    target_label = stmt->goto_label;
    if(target_label->primary)
    {
      stmt->goto_label = target_label->primary;
      target_label = stmt->goto_label;
    }
  }

  return target_label;
}

void partition_to_basic_blocks(MemoryArena* stmt_arena, AstNode* proc)
{
  if(proc->proc.ir_stmt_count > 0)
  {
    List* leaders = new_list(arena, eList_ir_leader_stmt);

    IrStmt* stmt_array = proc->proc.ir_stmt_array;
    int stmt_count = proc->proc.ir_stmt_count;

    IrStmt* stmt = &stmt_array[0];
    normalize_jump_target_labels(stmt);
    append_list_elem(leaders, new_leader_stmt(leaders->arena, 0, stmt), eList_ir_leader_stmt);
    
    for(int i = 1; i < proc->proc.ir_stmt_count; i++)
    {
      stmt = &stmt_array[i];
      if(stmt->kind == eIrStmt_cond_goto || stmt->kind == eIrStmt_goto)
      {
        start_new_basic_block(leaders, i+1, stmt_array, stmt_count);
        IrLabel* target_label = normalize_jump_target_labels(stmt);
        start_new_basic_block(leaders, target_label->stmt_nr, stmt_array, stmt_count);
      }
      else if(stmt->kind == eIrStmt_call || stmt->kind == eIrStmt_return)
      {
        start_new_basic_block(leaders, i+1, stmt_array, stmt_count);
      }
    }

    //------
    
    List* basic_blocks = proc->proc.basic_blocks = new_list(stmt_arena, eList_basic_block);

    for(ListItem* li = leaders->first;
        li;
        li = li->next)
    {
      int next_stmt_nr = proc->proc.ir_stmt_count;
      if(li->next)
      {
        IrLeaderStmt* leader_next = KIND(li->next, eList_ir_leader_stmt)->ir_leader_stmt;
        next_stmt_nr = leader_next->stmt_nr;
      }

      IrLeaderStmt* leader = KIND(li, eList_ir_leader_stmt)->ir_leader_stmt;
      BasicBlock* block = mem_push_struct(stmt_arena, BasicBlock);
      append_list_elem(basic_blocks, block, eList_basic_block);
      init_list(&block->pred_list, stmt_arena, eList_basic_block);
      init_list(&block->succ_list, stmt_arena, eList_basic_block);
      leader->block = block;
      block->stmt_array = mem_push_array(stmt_arena, IrStmt*, next_stmt_nr - leader->stmt_nr);
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
        bb_next = KIND(li->next, eList_basic_block)->basic_block;
      }

      BasicBlock* bb = KIND(li, eList_basic_block)->basic_block;
      IrStmt* last_stmt = bb->stmt_array[bb->stmt_count - 1];

      if(last_stmt->kind == eIrStmt_goto || last_stmt->kind == eIrStmt_cond_goto)
      {
        IrLabel* goto_label = 0;
        if(last_stmt->kind == eIrStmt_cond_goto)
        {
          goto_label = last_stmt->cond_goto.label;
        }
        else if(last_stmt->kind == eIrStmt_goto)
        {
          goto_label = last_stmt->goto_label;
        }
        else assert(0);
        
        int stmt_nr = goto_label->stmt_nr;
        IrLeaderStmt* leader = get_leader_stmt(leaders, stmt_nr);
        append_list_elem(&bb->succ_list, leader->block, eList_basic_block);
        append_list_elem(&leader->block->pred_list, bb, eList_basic_block);
        
        if(last_stmt->kind != eIrStmt_goto)
        {
          append_list_elem(&bb->succ_list, bb_next, eList_basic_block);
          append_list_elem(&bb_next->pred_list, bb, eList_basic_block);
        }
      }
      else if(bb_next)
      {
        append_list_elem(&bb->succ_list, bb_next, eList_basic_block);
        append_list_elem(&bb_next->pred_list, bb, eList_basic_block);
      }
      
      // next-use information
      for(int i = bb->stmt_count - 1; i >=0 ; i--)
      {
        IrStmt* stmt = bb->stmt_array[i];

        if(stmt->kind == eIrStmt_assign)
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

          if(stmt->assign.op == eIrOp_index_dest || stmt->assign.op == eIrOp_deref_dest)
          {
            result->object->is_live = true;
            result->object->next_use = i;
          }
          else
          {
            result->object->is_live = result->object->is_temp ? false : true;
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
