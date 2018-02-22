Label* new_label(MemoryArena* arena)
{
  Label* label = push_struct(arena, Label);
  gen_label_name(arena, label);
  return label;
}

void init_ir_pass(IrPass* pass, MemoryArena* gp_arena, MemoryArena* stmt_arena,
                     TypePass* type_pass, SymbolPass* sym_pass)
{
  pass->basic_type_bool  = type_pass->basic_type_bool;
  pass->basic_type_int   = type_pass->basic_type_int;
  pass->basic_type_char  = type_pass->basic_type_char;
  pass->basic_type_float = type_pass->basic_type_float;
  pass->basic_type_void  = type_pass->basic_type_void;
  pass->basic_type_str   = type_pass->basic_type_str;

  pass->gp_arena = gp_arena;
  pass->stmt_arena = stmt_arena;
  pass->stmt_array = (IrStmt*)stmt_arena->base;
  pass->stmt_count = 0;
  pass->sym_pass = sym_pass;
  pass->label_list = list_new(gp_arena, eList_ir_label);
  pass->data_alignment = 4;
}

bool is_cast_ir_op(eIrOp ir_op)
{
  bool is_conv = false;

  switch(ir_op)
  {
    case eIrOp_itof:
    case eIrOp_itoc:
    case eIrOp_itob:
    case eIrOp_ftoi:
    case eIrOp_ctoi:
    case eIrOp_btoi:
      is_conv = true;
    break;
  }

  return is_conv;
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

Label* get_label_at(IrPass* pass, int stmt_nr)
{
  Label* label = 0;
  for(ListItem* li = pass->label_list->first;
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

void emit_assign(IrPass* pass, eIrOp op, IrArg* arg1, IrArg* arg2, IrArg* result)
{
  IrStmt* stmt = push_struct(pass->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_assign;
  stmt->label = get_label_at(pass, pass->stmt_count);

  stmt->assign.op = op;
  stmt->assign.arg1 = push_struct(pass->gp_arena, IrArg);
  *stmt->assign.arg1 = *arg1;
  if(arg2)
  {
    stmt->assign.arg2 = push_struct(pass->gp_arena, IrArg);
    *stmt->assign.arg2 = *arg2;
  }
  stmt->assign.result = push_struct(pass->gp_arena, IrArg);
  *stmt->assign.result = *result;

  pass->stmt_count++;
}

void emit_label(IrPass* pass, Label* label)
{
  label->stmt_nr = pass->stmt_count;
  
  Label* prim_label = 0;
  for(ListItem* li = pass->label_list->last;
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
    list_append(pass->label_list, label, eList_ir_label);
  }
}

void emit_nop(IrPass* pass)
{
  IrStmt* stmt = push_struct(pass->stmt_arena, IrStmt);

  *stmt = {};
  stmt->kind = eIrStmt_nop;
  stmt->label = get_label_at(pass, pass->stmt_count);

  pass->stmt_count++;
}

void emit_cond_goto(IrPass* pass, eIrOp relop, IrArg* arg1, IrArg* arg2, Label* label)
{
  IrStmt* stmt = push_struct(pass->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_cond_goto;
  stmt->label = get_label_at(pass, pass->stmt_count);

  stmt->cond_goto.relop = relop;
  stmt->cond_goto.arg1 = arg1;
  stmt->cond_goto.arg2 = arg2;
  stmt->cond_goto.goto_label = label;

  pass->stmt_count++;
}

void emit_goto(IrPass* pass, Label* goto_label)
{
  IrStmt* stmt = push_struct(pass->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_goto;
  stmt->label = get_label_at(pass, pass->stmt_count);
  stmt->goto_.goto_label = goto_label;

  pass->stmt_count++;
}

void emit_call(IrPass* pass, Label* name, Scope* param_scope, Symbol* retvar, bool is_extern)
{
  IrStmt* stmt = push_struct(pass->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_call;
  stmt->label = get_label_at(pass, pass->stmt_count);
  stmt->call.name = name;
  stmt->call.param_scope = param_scope;
  stmt->call.retvar = retvar;
  stmt->call.is_extern = is_extern;

  pass->stmt_count++;
}

void emit_return(IrPass* pass)
{
  IrStmt* stmt = push_struct(pass->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_return;
  stmt->label = get_label_at(pass, pass->stmt_count);

  pass->stmt_count++;
}

void reset_ir_pass(IrPass* pass)
{
  pass->stmt_array = &pass->stmt_array[pass->stmt_count];
  pass->total_stmt_count += pass->stmt_count;
  pass->stmt_count = 0;
  pass->current_alloc_offset = 0;

  /*XXX: 'label_list' storage is a good candidate for begin_temp_memory()/end_temp_memory() pattern of allocation. */
  list_clear(pass->label_list);
}

IrArg* new_arg_temp_object(IrPass* pass, Scope* scope, Type* ty, SourceLoc* src_loc)
{
  IrArg* arg = push_struct(pass->gp_arena, IrArg);
  arg->object = new_temp_object(pass, scope, ty, src_loc);

  return arg;
}

IrArg* new_arg_existing_object(IrPass* pass, Symbol* object)
{
  IrArg* arg = push_struct(pass->gp_arena, IrArg);
  arg->object = object;

  return arg;
}

bool IrPass_visit_expr(IrPass* pass, Scope* scope, AstNode* expr);

bool IrPass_visit_bin_expr(IrPass* pass, Scope* scope, AstNode* bin_expr)
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
      if(success = IrPass_visit_expr(pass, scope, left_operand) && IrPass_visit_expr(pass, scope, right_operand))
      {
        bin_expr->place = new_arg_temp_object(pass, scope, bin_expr->eval_ty, bin_expr->src_loc);

        emit_assign(pass, conv_operator_to_ir_op(op), left_operand->place, right_operand->place, bin_expr->place);
      }
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool IrPass_visit_unr_expr(IrPass* pass, Scope* scope, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;
  
  switch(op)
  {
    case eOperator_neg:
    {
      if(success = IrPass_visit_expr(pass, scope, operand))
      {
        unr_expr->place = new_arg_temp_object(pass, scope, unr_expr->eval_ty, unr_expr->src_loc);
        emit_assign(pass, conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
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
      if(success = IrPass_visit_expr(pass, scope, operand))
      {
        unr_expr->place = new_arg_temp_object(pass, scope, unr_expr->eval_ty, unr_expr->src_loc);
        emit_assign(pass, conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
      }
    }
    break;

    case eOperator_deref:
    {
      if(success = IrPass_visit_expr(pass, scope, operand))
      {
        unr_expr->place = new_arg_temp_object(pass, scope, unr_expr->eval_ty, unr_expr->src_loc);
        emit_assign(pass, conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
      }
    }
    break;
    
    default: assert(0);
  }

  return success;
}

void IrPass_visit_lit(IrPass* pass, Scope* scope, AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));
  
  lit->place = new_arg_existing_object(pass, lit->lit.constant);
}

bool IrPass_visit_bool_expr(IrPass* pass, Scope* scope, AstNode* expr);

bool IrPass_visit_bool_unr_expr(IrPass* pass, Scope* scope, AstNode* unr_expr)
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
      success = IrPass_visit_bool_expr(pass, scope, operand);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool IrPass_visit_actual_args(IrPass* pass, Scope* scope, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;

  int arg_count = args->args.node_list.count;
  if(arg_count > 0)
  {
    IrArg** temp_places = push_array(pass->gp_arena, IrArg*, arg_count);

    int i = 0;
    for(ListItem* li = args->args.node_list.first;
        li && success;
        li = li->next, i++)
    {
      AstNode* arg = KIND(li, eList_ast_node)->ast_node;
      AstNode* expr = arg->call_arg.expr;

      if(success = IrPass_visit_expr(pass, scope, expr))
      {
        temp_places[i] = new_arg_temp_object(pass, scope, expr->eval_ty, expr->src_loc);

        emit_assign(pass, eIrOp_None, expr->place, 0, temp_places[i]);
      }
    }

    i = 0;
    for(ListItem* li = args->args.node_list.first;
        li;
        li = li->next, i++)
    {
      AstNode* arg = KIND(li, eList_ast_node)->ast_node;

      arg->place = new_arg_existing_object(pass, arg->call_arg.param);
      emit_assign(pass, eIrOp_None, temp_places[i], 0, arg->place);
    }
  }

  return success;
}

void IrPass_visit_call(IrPass* pass, Scope* scope, AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  AstNode* proc = call->call.proc;
  
  call->place = new_arg_existing_object(pass, call->call.retvar);

  AstNode* args = call->call.args;
  assert(KIND(args, eAstNode_node_list));
  IrPass_visit_actual_args(pass, scope, args);

  if(is_extern_proc(proc))
  {
    // right-to-left (stdcall)
    alloc_data_object(pass, call->call.retvar, call->call.param_scope);
    for(ListItem* li = args->args.node_list.last;
        li;
        li = li->prev)
    {
      AstNode* arg = KIND(li, eList_ast_node)->ast_node;
      alloc_data_object(pass, arg->call_arg.param, call->call.param_scope);
    }

    emit_call(pass, &proc->proc.label_name, call->call.param_scope, call->call.retvar, true);
  }
  else
  {
    // left-to-right
    for(ListItem* li = args->args.node_list.first;
        li;
        li = li->next)
    {
      AstNode* arg = KIND(li, eList_ast_node)->ast_node;
      alloc_data_object(pass, arg->call_arg.param, call->call.param_scope);
    }
    alloc_data_object(pass, call->call.retvar, call->call.param_scope);

    emit_call(pass, &proc->proc.label_name, call->call.param_scope, call->call.retvar, false);
  }
}

void IrPass_visit_id(IrPass* pass, AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  id->place = new_arg_existing_object(pass, id->id.decl_sym);
}

bool IrPass_visit_index(IrPass* pass, Scope* scope, AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;

  AstNode* array_expr = index->index.array_expr;
  AstNode* i_expr = index->index.i_expr;

  if(array_expr->kind == eAstNode_id)
  {
    IrPass_visit_id(pass, array_expr);
    index->index.place = array_expr->place;

    if(success = IrPass_visit_expr(pass, scope, i_expr))
    {
      index->index.i_place = i_expr->place;
    }
  }
  else if(array_expr->kind == eAstNode_index)
  {
    if(success = IrPass_visit_index(pass, scope, array_expr) && IrPass_visit_expr(pass, scope, i_expr))
    {
      index->index.place = array_expr->index.place;

      IrArg* offset = index->index.i_place = new_arg_temp_object(pass, scope, pass->basic_type_int, index->src_loc);

      Type* index_ty = index->ty;
      int size_val = KIND(index_ty, eType_array)->array.size;

      Symbol* size_constant = new_const_int_object(pass->sym_pass, index->src_loc, size_val);
      IrArg* dim_size = new_arg_existing_object(pass, size_constant);

      if(size_val > 0)
      {
        emit_assign(pass, eIrOp_mul, array_expr->index.i_place, dim_size, offset);
        emit_assign(pass, eIrOp_add, offset, i_expr->place, offset);
      }
      else
        success = compile_error(pass->gp_arena, i_expr->src_loc, "array dim size <= 0");
    }
  }
  else assert(0);

  return success;
}

bool IrPass_visit_index_with_offset(IrPass* pass, Scope* scope, AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;

  if(success = IrPass_visit_index(pass, scope, index))
  {
    IrArg* offset = index->index.offset = new_arg_temp_object(pass, scope, pass->basic_type_int, index->src_loc);

    assert(index->index.ndim == 1);
    int width_val = index->eval_ty->width;

    Symbol* width_constant = new_const_int_object(pass->sym_pass, index->src_loc, width_val);
    IrArg* width = new_arg_existing_object(pass, width_constant);

    emit_assign(pass, eIrOp_mul, index->index.i_place, width, offset);
  }

  return success;
}

bool IrPass_visit_assign(IrPass* pass, Scope* scope, AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;
  
  AstNode* dest_expr = assign->assign.dest_expr;
  AstNode* source_expr = assign->assign.source_expr;
  
  if(dest_expr->kind == eAstNode_id)
  {
    if(success = IrPass_visit_expr(pass, scope, dest_expr) && IrPass_visit_expr(pass, scope, source_expr))
    {
      emit_assign(pass, eIrOp_None, source_expr->place, 0, dest_expr->place);
    }
  }
  else if(dest_expr->kind == eAstNode_index)
  {
    if(success = IrPass_visit_index_with_offset(pass, scope, dest_expr) && IrPass_visit_expr(pass, scope, source_expr))
    {
      dest_expr->place = dest_expr->index.place;
      emit_assign(pass, eIrOp_index_dest, source_expr->place, dest_expr->index.offset, dest_expr->index.place);
    }
  }
  else if(dest_expr->kind == eAstNode_unr_expr && dest_expr->unr_expr.op == eOperator_deref)
  {
    AstNode* operand = dest_expr->unr_expr.operand;
    if(success = IrPass_visit_expr(pass, scope, operand) && IrPass_visit_expr(pass, scope, source_expr))
    {
      dest_expr->place = operand->place;
      emit_assign(pass, eIrOp_deref_dest, source_expr->place, 0, dest_expr->place);
    }
  }
  else
    success = compile_error(pass->gp_arena, dest_expr->src_loc, "unsupported expression on the left-side of assignment");

  return success;
}

bool IrPass_visit_cast(IrPass* pass, Scope* scope, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;
  AstNode* to_type = cast->cast.to_type;
  AstNode* from_expr = cast->cast.from_expr;
  
  if(success = IrPass_visit_expr(pass, scope, from_expr))
  {
    cast->place = from_expr->place;
    
    if(type_eq(to_type->eval_ty, from_expr->eval_ty) ||
       ((to_type->eval_ty->kind == from_expr->eval_ty->kind) && (to_type->eval_ty->kind == eType_pointer)))
    {
      return success;
    }
    bool require_conv = true;
    if(type_eq(to_type->eval_ty, pass->basic_type_int))
    {
      // pointer -> int
      require_conv = from_expr->eval_ty->kind != eType_pointer;
    }
    else if(to_type->eval_ty->kind == eType_pointer)
    {
      // int -> pointer
      require_conv = !type_eq(from_expr->eval_ty, pass->basic_type_int);
    }
    if(require_conv)
    {
      cast->place = new_arg_temp_object(pass, scope, cast->eval_ty, cast->src_loc);

      eIrOp cast_op = eIrOp_None;

      if(type_eq(to_type->eval_ty, pass->basic_type_int))
      {
        if(type_eq(from_expr->eval_ty, pass->basic_type_float))
        {
          cast_op = eIrOp_ftoi; // float -> int 
        }
        else if(type_eq(from_expr->eval_ty, pass->basic_type_bool))
        {
          cast_op = eIrOp_btoi; // bool -> int 
        }
        else if(type_eq(from_expr->eval_ty, pass->basic_type_char))
        {
          cast_op = eIrOp_ctoi; // char -> int 
        }
        else assert(0);
      }
      else if(type_eq(to_type->eval_ty, pass->basic_type_float))
      {
        if(type_eq(from_expr->eval_ty, pass->basic_type_int))
        {
          cast_op = eIrOp_itof; // int -> float 
        }
        else assert(0);
      }
      else if(type_eq(to_type->eval_ty, pass->basic_type_char))
      {
        if(type_eq(from_expr->eval_ty, pass->basic_type_int))
        {
          cast_op = eIrOp_itoc; // int -> char 
        }
        else assert(0);
      }
      else if(type_eq(to_type->eval_ty, pass->basic_type_bool))
      {
        if(type_eq(from_expr->eval_ty, pass->basic_type_int))
        {
          cast_op = eIrOp_itob; // int -> bool 
        }
        else if(from_expr->eval_ty->kind == eType_pointer)
        {
          cast_op = eIrOp_itob; // pointer(T) -> bool 
        }
        else assert(0);
      }
      emit_assign(pass, cast_op, from_expr->place, 0, cast->place);
    }
  }

  return success;
}

bool IrPass_visit_expr(IrPass* pass, Scope* scope, AstNode* expr)
{
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode_bin_expr:
    {
      eOperator op = expr->bin_expr.op;
      if(is_operator_relation(op) || is_operator_logic(op))
      {
        expr->label_true = new_label(pass->gp_arena);
        expr->label_false = new_label(pass->gp_arena);
        expr->label_next = new_label(pass->gp_arena);

        Symbol* result_object = new_temp_object(pass, scope, expr->eval_ty, expr->src_loc);
        result_object->is_live_on_exit = true;
        result_object->is_live = true;
        expr->place = new_arg_existing_object(pass, result_object);

        IrPass_visit_bool_expr(pass, scope, expr);
        
        emit_label(pass, expr->label_true);
        emit_assign(pass, eIrOp_None, new_arg_existing_object(pass, pass->bool_true), 0, expr->place);
        emit_goto(pass, expr->label_next);
        emit_label(pass, expr->label_false);
        emit_assign(pass, eIrOp_None, new_arg_existing_object(pass, pass->bool_false), 0, expr->place);
        emit_label(pass, expr->label_next);
      }
      else
      {
        IrPass_visit_bin_expr(pass, scope, expr);
      }
    }
    break;
    
    case eAstNode_unr_expr:
    {
      eOperator op = expr->unr_expr.op;
      if(is_operator_relation(op) || is_operator_logic(op))
      {
        expr->label_true = new_label(pass->gp_arena);
        expr->label_false = new_label(pass->gp_arena);
        expr->label_next = new_label(pass->gp_arena);

        Symbol* result_object = new_temp_object(pass, scope,  expr->eval_ty, expr->src_loc);
        result_object->is_live_on_exit = true;
        result_object->is_live = true;
        expr->place = new_arg_existing_object(pass, result_object);

        IrPass_visit_bool_unr_expr(pass, scope, expr);

        emit_label(pass, expr->label_true);
        emit_assign(pass, eIrOp_None, new_arg_existing_object(pass, pass->bool_true), 0, expr->place);
        emit_goto(pass, expr->label_next);
        emit_label(pass, expr->label_false);
        emit_assign(pass, eIrOp_None, new_arg_existing_object(pass, pass->bool_false), 0, expr->place);
        emit_label(pass, expr->label_next);
      }
      else
      {
        IrPass_visit_unr_expr(pass, scope, expr);
      }
    }
    break;
    
    case eAstNode_id:
    {
      IrPass_visit_id(pass, expr);
    }
    break;
    
    case eAstNode_lit:
    {
      IrPass_visit_lit(pass, scope, expr);
    }
    break;
    
    case eAstNode_call:
    {
      IrPass_visit_call(pass, scope, expr);
    }
    break;
    
    case eAstNode_index:
    {
      if(success = IrPass_visit_index_with_offset(pass, scope, expr))
      {
        expr->place = new_arg_temp_object(pass, scope, expr->eval_ty, expr->src_loc);

        emit_assign(pass, eIrOp_index_source, expr->index.place, expr->index.offset, expr->place);
      }
    }
    break;
    
    case eAstNode_cast:
    {
      IrPass_visit_cast(pass, scope, expr);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool IrPass_visit_block_stmt(IrPass* pass, Scope* scope, AstNode* stmt);

bool IrPass_visit_block(IrPass* pass, Scope* scope, AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    IrPass_visit_block_stmt(pass, scope, stmt);
  }

  return success;
}

bool IrPass_visit_bool_bin_expr(IrPass* pass, Scope* scope, AstNode* bin_expr)
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
      if(success = IrPass_visit_expr(pass, scope, left_operand) && IrPass_visit_expr(pass, scope, right_operand))
      {
        emit_cond_goto(pass, conv_operator_to_ir_op(op), left_operand->place, right_operand->place, bin_expr->label_true);
        emit_goto(pass, bin_expr->label_false);
      }
    }
    break;
    
    case eOperator_logic_or:
    {
      assert(bin_expr->label_true);
      assert(bin_expr->label_false);

      left_operand->label_true = bin_expr->label_true;
      left_operand->label_false = new_label(pass->gp_arena);
      right_operand->label_true = bin_expr->label_true;
      right_operand->label_false = bin_expr->label_false;
      if(success = IrPass_visit_bool_expr(pass, scope, left_operand))
      {
        emit_label(pass, left_operand->label_false);
        success = IrPass_visit_bool_expr(pass, scope, right_operand);
      }
    }
    break;
    
    case eOperator_logic_and:
    {
      assert(bin_expr->label_true);
      assert(bin_expr->label_false);

      left_operand->label_true = new_label(pass->gp_arena);
      left_operand->label_false = bin_expr->label_false;
      right_operand->label_true = bin_expr->label_true;
      right_operand->label_false = bin_expr->label_false;
      if(success = IrPass_visit_bool_expr(pass, scope, left_operand))
      {
        emit_label(pass, left_operand->label_true);
        success = IrPass_visit_bool_expr(pass, scope, right_operand);
      }
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool IrPass_visit_bool_id(IrPass* pass, Scope* scope, AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;

  if(success = IrPass_visit_expr(pass, scope, id))
  {
    emit_cond_goto(pass, eIrOp_not_eq, id->place,
                      new_arg_existing_object(pass, pass->bool_false), id->label_true);
    emit_goto(pass, id->label_false);
  }

  return success;
}

bool IrPass_visit_bool_call(IrPass* pass, Scope* scope, AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  bool success = true;

  if(success = IrPass_visit_expr(pass, scope, call))
  {
    emit_cond_goto(pass, eIrOp_not_eq, call->place, new_arg_existing_object(pass, pass->bool_false), call->label_true);
    emit_goto(pass, call->label_false);
  }

  return success;
}

bool IrPass_visit_bool_cast(IrPass* pass, Scope* scope, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;

  if(success = IrPass_visit_cast(pass, scope, cast))
  {
    emit_cond_goto(pass, eIrOp_not_eq, cast->place, new_arg_existing_object(pass, pass->bool_false), cast->label_true);
    emit_goto(pass, cast->label_false);
  }

  return success;
}

void IrPass_visit_bool_lit(IrPass* pass, Scope* scope, AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));

  if(lit->lit.bool_val)
  {
    emit_goto(pass, lit->label_true);
  }
  else
  {
    emit_goto(pass, lit->label_false);
  }
}

bool IrPass_visit_bool_expr(IrPass* pass, Scope* scope, AstNode* expr)
{
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode_id:
    {
      success = IrPass_visit_bool_id(pass, scope, expr);
    }
    break;
    
    case eAstNode_lit:
    {
      IrPass_visit_bool_lit(pass, scope, expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = IrPass_visit_bool_bin_expr(pass, scope, expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = IrPass_visit_bool_unr_expr(pass, scope, expr);
    }
    break;
    
    case eAstNode_cast:
    {
      success = IrPass_visit_bool_cast(pass, scope, expr);
    }
    break;

    case eAstNode_call:
    {
      success = IrPass_visit_bool_call(pass, scope, expr);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool IrPass_visit_do_while(IrPass* pass, Scope* scope, AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  AstNode* cond_expr = do_while->do_while.cond_expr;
  AstNode* body = do_while->do_while.body;
  
  do_while->label_begin = new_label(pass->gp_arena);
  do_while->label_next = new_label(pass->gp_arena);
  cond_expr->label_true = new_label(pass->gp_arena);
  cond_expr->label_false = do_while->label_next;
  
  emit_label(pass, cond_expr->label_true);
  IrPass_visit_block_stmt(pass, scope, body);
  emit_label(pass, do_while->label_begin);
  if(success = IrPass_visit_bool_expr(pass, scope, cond_expr))
  {
    emit_label(pass, do_while->label_next);
  }

  return success;
}

bool IrPass_visit_while(IrPass* pass, Scope* scope, AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  
  AstNode* cond_expr = while_->while_.cond_expr;
  AstNode* body = while_->while_.body;
  
  while_->label_begin = new_label(pass->gp_arena);
  while_->label_next = new_label(pass->gp_arena);
  cond_expr->label_true = new_label(pass->gp_arena);
  cond_expr->label_false = while_->label_next;
  
  emit_label(pass, while_->label_begin);
  if(success = IrPass_visit_bool_expr(pass, scope, cond_expr))
  {
    emit_label(pass, cond_expr->label_true);
    IrPass_visit_block_stmt(pass, scope, body);
    emit_goto(pass, while_->label_begin);
    emit_label(pass, while_->label_next);
  }

  return success;
}

bool IrPass_visit_if(IrPass* pass, Scope* scope, AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  
  AstNode* cond_expr = if_->if_.cond_expr;
  AstNode* body = if_->if_.body;
  AstNode* else_body = if_->if_.else_body;
  
  if_->label_next = new_label(pass->gp_arena);
  if(else_body)
  {
    cond_expr->label_true = new_label(pass->gp_arena);
    cond_expr->label_false = new_label(pass->gp_arena);
    body->label_next = if_->label_next;
    else_body->label_next = if_->label_next;
    
    if(success = IrPass_visit_bool_expr(pass, scope, cond_expr))
    {
      emit_label(pass, cond_expr->label_true);
      IrPass_visit_block_stmt(pass, scope, body);
      emit_goto(pass, if_->label_next);
      emit_label(pass, cond_expr->label_false);
      IrPass_visit_block_stmt(pass, scope, else_body);
    }
  }
  else
  {
    cond_expr->label_true = new_label(pass->gp_arena);
    cond_expr->label_false = if_->label_next;
    body->label_next = if_->label_next;
    
    if(success = IrPass_visit_bool_expr(pass, scope, cond_expr))
    {
      emit_label(pass, cond_expr->label_true);
      IrPass_visit_block_stmt(pass, scope, body);
    }
  }
  if(success)
  {
    emit_label(pass, if_->label_next);
  }

  return success;
}

bool IrPass_visit_return(IrPass* pass, Scope* scope, AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;

  AstNode* ret_expr = ret->ret.expr;
  AstNode* proc = ret->ret.proc;

  if(ret_expr)
  {
    if(success = IrPass_visit_expr(pass, scope, ret_expr))
    {
      IrArg* retvar = new_arg_existing_object(pass, proc->proc.retvar);

      emit_assign(pass, eIrOp_None, ret_expr->place, 0, retvar);
    }
  }

  emit_goto(pass, proc->label_next);

  return success;
}

bool IrPass_visit_loop_ctrl(IrPass* pass, Scope* scope, AstNode* loop_ctrl)
{
  assert(KIND(loop_ctrl, eAstNode_loop_ctrl));
  bool success = true;

  AstNode* loop = loop_ctrl->loop_ctrl.loop;
  if(loop_ctrl->loop_ctrl.kind == eLoopCtrl_break)
  {
    emit_goto(pass, loop->label_next);
  }
  else if(loop_ctrl->loop_ctrl.kind == eLoopCtrl_continue)
  {
    emit_goto(pass, loop->label_begin);
  }
  else assert(0);

  return success;
}

bool IrPass_visit_var(IrPass* pass, Scope* scope, AstNode* var)
{
  bool success = true;
  assert(KIND(var, eAstNode_var));

  Symbol* object = var->var.decl_sym;
  alloc_data_object_incremental(pass, object, scope);
  add_object_to_memory(pass->x86_context, object);
  var->place = new_arg_existing_object(pass, object);

  AstNode* init_expr = var->var.init_expr;
  if(init_expr)
  {
    if(success = IrPass_visit_expr(pass, scope, init_expr))
    {
      emit_assign(pass, eIrOp_None, init_expr->place, 0, var->place);
    }
  }

  return success;
}

bool IrPass_visit_block_stmt(IrPass* pass, Scope* scope, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_assign:
    {
      success = IrPass_visit_assign(pass, scope, stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = IrPass_visit_cast(pass, scope, stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_index:
    case eAstNode_lit:
    {
      success = IrPass_visit_expr(pass, scope, stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = IrPass_visit_block(pass, stmt->block.scope, stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = IrPass_visit_if(pass, scope, stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = IrPass_visit_do_while(pass, scope, stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = IrPass_visit_while(pass, scope, stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = IrPass_visit_var(pass, scope, stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = IrPass_visit_return(pass, scope, stmt);
    }
    break;
    
    case eAstNode_empty:
    break;

    case eAstNode_loop_ctrl:
    {
      success = IrPass_visit_loop_ctrl(pass, scope, stmt);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

void IrPass_visit_formal_args(IrPass* pass, Scope* scope, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  for(ListItem* li = args->args.node_list.first;
      li;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    Symbol* arg_object = KIND(arg, eAstNode_var)->var.decl_sym;
    alloc_data_object(pass, arg_object, scope);
    add_object_to_memory(pass->x86_context, arg_object);
  }
}

int get_proc_arg_size(IrPass* pass, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  int size = 0;

  for(ListItem* li = args->args.node_list.first;
      li;
      li = li->next)
  {
    AstNode* var = KIND(li, eList_ast_node)->ast_node;
    assert(KIND(var, eAstNode_var));
    Symbol* arg_object = var->var.decl_sym;
    size += set_type_width(arg_object->ty);
  }

  return size;
}

bool IrPass_visit_proc(IrPass* pass, Scope* scope, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  
  proc->place = new_arg_existing_object(pass, proc->proc.retvar);
  IrPass_visit_formal_args(pass, proc->proc.param_scope, proc->proc.args);
  alloc_data_object(pass, proc->proc.retvar, proc->proc.param_scope);

  Label* label_name = &proc->proc.label_name;
  label_name->stmt_nr = 0;

  if(is_extern_proc(proc))
  {
    int arg_size = get_proc_arg_size(pass, proc->proc.args);
    char* name = proc->proc.name;
    String decorated_label = {};
    str_init(&decorated_label, pass->gp_arena);
    str_format(&decorated_label, "%s@%d", name, arg_size);

    label_name->name = str_cap(&decorated_label);
  }
  else
  {
    label_name->name = proc->proc.name;

    AstNode* body = proc->proc.body;
    assert(KIND(body, eAstNode_block));

    proc->label_begin = new_label(pass->gp_arena);
    proc->label_next = new_label(pass->gp_arena);

    emit_label(pass, proc->label_begin);

    if(success = IrPass_visit_block_stmt(pass, body->block.scope, body))
    {
      emit_label(pass, proc->label_next);
      emit_return(pass);
    }

    Scope* proc_scope = proc->proc.scope;
    proc_scope->allocd_size = pass->current_alloc_offset;
  }

  return success;
}

void IrPass_visit_module_var(IrPass* pass, Scope* scope, AstNode* var)
{
  assert(KIND(var, eAstNode_var));

  Symbol* object = var->var.decl_sym;
  add_object_to_memory(pass->x86_context, object);
}

bool IrPass_visit_module_stmt(IrPass* pass, Scope* scope, AstNode* stmt)
{
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode_proc:
    {
      if(success = IrPass_visit_proc(pass, scope, stmt))
      {
        stmt->proc.ir_stmt_array = pass->stmt_array;
        stmt->proc.ir_stmt_count = pass->stmt_count;

        reset_ir_pass(pass);
      }
    }
    break;
    
    case eAstNode_var:
      IrPass_visit_module_var(pass, scope, stmt);
    break;
    
    default: assert(0);
  }

  return success;
}

bool IrPass_visit_module(IrPass* pass, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  for(ListItem* li = module->module.nodes.first;
      li;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = IrPass_visit_module_stmt(pass, module->module.scope, stmt);
  }

  return success;
}

void DEBUG_print_ir_op(IrPass* pass, String* text, eIrOp op)
{
  switch(op)
  {
    case eIrOp_add:
      str_append(text, "+");
    break;
    
    case eIrOp_sub:
      str_append(text, "-");
    break;
    
    case eIrOp_mul:
      str_append(text, "*");
    break;
    
    case eIrOp_div:
      str_append(text, "/");
    break;
    
    case eIrOp_mod:
      str_append(text, "mod");
    break;
    
    case eIrOp_neg:
      str_append(text, "-");
    break;
    
    case eIrOp_eq:
      str_append(text, "==");
    break;
    
    case eIrOp_not_eq:
      str_append(text, "<>");
    break;
    
    case eIrOp_less:
      str_append(text, "<");
    break;
    
    case eIrOp_less_eq:
      str_append(text, "<=");
    break;
    
    case eIrOp_greater:
      str_append(text, ">");
    break;
    
    case eIrOp_greater_eq:
      str_append(text, ">=");
    break;
    
    case eIrOp_logic_and:
      str_append(text, "and");
    break;
    
    case eIrOp_logic_or:
      str_append(text, "or");
    break;
    
    case eIrOp_logic_not:
      str_append(text, "not");
    break;
    
    case eIrOp_bit_and:
      str_append(text, "&");
    break;
    
    case eIrOp_bit_or:
      str_append(text, "|");
    break;
    
    case eIrOp_bit_xor:
      str_append(text, "~");
    break;
    
    case eIrOp_bit_not:
      str_append(text, "!");
    break;
    
    case eIrOp_bit_shift_left:
      str_append(text, "<<");
    break;
    
    case eIrOp_bit_shift_right:
      str_append(text, ">>");
    break;
    
    case eIrOp_itof:
      str_append(text, "itof");
    break;
    
    case eIrOp_itoc:
      str_append(text, "itoc");
    break;
    
    case eIrOp_itob:
      str_append(text, "itob");
    break;
    
    case eIrOp_ftoi:
      str_append(text, "ftoi");
    break;
    
    case eIrOp_ctoi:
      str_append(text, "ctoi");
    break;
    
    case eIrOp_btoi:
      str_append(text, "btoi");
    break;
    
    default: assert(0);
  }
}

void DEBUG_print_ir_arg(IrPass* pass, String* text, IrArg* arg)
{
  Symbol* object = arg->object;

  switch(object->kind)
  {
    case eSymbol_None:
    {
      str_format(text, "%s", arg->object->name);
    }
    break;
    
    case eSymbol_constant:
    {
      if(type_eq(object->ty, pass->basic_type_int) || type_eq(object->ty, pass->basic_type_bool))
      {
        str_format(text, "%d", object->int_val);
      }
      else if(type_eq(object->ty, pass->basic_type_float))
      {
        str_format(text, "%f", object->float_val);
      }
      else if(type_eq(object->ty, pass->basic_type_char))
      {
        char buf[3] = {0};
        cstr_print_char(buf, object->char_val);
        str_format(text, "'%s'", buf);
      }
      else if(type_eq(object->ty, pass->basic_type_str))
      {
        str_format(text, "\"%s\"", object->str_val);
      }
      else assert(0);
    }
    break;
    
    default: assert(0);
  }
}

void DEBUG_print_ir_stmt(IrPass* pass, String* text, IrStmt* stmt)
{
  switch(stmt->kind)
  {
    case eIrStmt_assign:
    {
      IrStmt_Assign* assign = &stmt->assign;

      switch(assign->op)
      {
        case eIrOp_None:
        {
          DEBUG_print_ir_arg(pass, text, assign->result);
          str_append(text, " = ");
          DEBUG_print_ir_arg(pass, text, assign->arg1);
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
          DEBUG_print_ir_arg(pass, text, assign->result);
          str_append(text, " = ");
          DEBUG_print_ir_arg(pass, text, assign->arg1);
          str_append(text, " ");
          DEBUG_print_ir_op(pass, text, assign->op);
          str_append(text, " ");
          DEBUG_print_ir_arg(pass, text, assign->arg2);
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
          DEBUG_print_ir_arg(pass, text, assign->result);
          str_append(text, " = ");
          DEBUG_print_ir_op(pass, text, assign->op);
          str_append(text, " ");
          DEBUG_print_ir_arg(pass, text, assign->arg1);
        }
        break;
        
        case eIrOp_index_dest:
        {
          DEBUG_print_ir_arg(pass, text, assign->result);
          str_append(text, "[");
          DEBUG_print_ir_arg(pass, text, assign->arg2);
          str_append(text, "] = ");
          DEBUG_print_ir_arg(pass, text, assign->arg1);
        }
        break;
        
        case eIrOp_index_source:
        {
          DEBUG_print_ir_arg(pass, text, assign->result);
          str_append(text, " = ");
          DEBUG_print_ir_arg(pass, text, assign->arg1);
          str_append(text, "[");
          DEBUG_print_ir_arg(pass, text, assign->arg2);
          str_append(text, "]");
        }
        break;

        case eIrOp_address_of:
        {
          DEBUG_print_ir_arg(pass, text, assign->result);
          str_append(text, " = &");
          DEBUG_print_ir_arg(pass, text, assign->arg1);
        }
        break;

        case eIrOp_deref_dest:
        {
          str_append(text, "^");
          DEBUG_print_ir_arg(pass, text, assign->result);
          str_append(text, " = ");
          DEBUG_print_ir_arg(pass, text, assign->arg1);
        }
        break;

        case eIrOp_deref_source:
        {
          DEBUG_print_ir_arg(pass, text, assign->result);
          str_append(text, " = ^");
          DEBUG_print_ir_arg(pass, text, assign->arg1);
        }
        break;
        
        default: assert(0);
      }
    }
    break;
    
    case eIrStmt_cond_goto:
    {
      IrStmt_CondGoto* cond_goto = &stmt->cond_goto;
      str_append(text, "if ");
      DEBUG_print_ir_arg(pass, text, cond_goto->arg1);
      str_append(text, " ");
      DEBUG_print_ir_op(pass, text, cond_goto->relop);
      str_append(text, " ");
      DEBUG_print_ir_arg(pass, text, cond_goto->arg2);
      str_format(text, " goto %s", cond_goto->goto_label->name);
    }
    break;
    
    case eIrStmt_goto:
    {
      IrStmt_Goto* goto_ = &stmt->goto_;
      str_format(text, "goto %s", goto_->goto_label->name);
    }
    break;
    
    case eIrStmt_call:
    {
      IrStmt_Call* call = &stmt->call;
      str_format(text, "call %s", call->name->name);
    }
    break;
    
    case eIrStmt_return:
    {
      str_append(text, "return");
    }
    break;
    
    case eIrStmt_nop:
    {
      str_append(text, "nop");
    }
    break;
    
    default:
    {
      str_append(text, "???");
    }
  }
}

void DEBUG_print_basic_block(IrPass* pass, String* text, BasicBlock* bb)
{
  IrStmt** stmt_array = bb->stmt_array;
  for(int i = 0; i < bb->stmt_count; i++)
  {
    IrStmt* stmt = stmt_array[i];
    if(stmt->label)
    {
      str_format_nl(text, "%10s:", stmt->label->name);
    }
    str_format(text, "%10d: ", i);
    DEBUG_print_ir_stmt(pass, text, stmt);
    str_nl(text);
  }
}

void DEBUG_print_ir_code(IrPass* pass, List* procs, char* title)
{
  begin_temp_memory(&pass->gp_arena);
  String text = {};
  str_init(&text, pass->gp_arena);

  str_format(&text, "%s.ir", title);
  char* file_path = str_cap(&text);

  str_init(&text, pass->gp_arena);

  for(ListItem* li = procs->first;
      li;
      li = li->next)
  {
    AstNode* proc = KIND(li, eList_ast_node)->ast_node;

    if(is_extern_proc(proc))
    {
      ;//ok
    }
    else
    {
      str_format_nl(&text, "%10s:", proc->proc.name);

      List* basic_blocks = proc->proc.basic_blocks;
      for(ListItem* li = basic_blocks->first;
          li;
          li = li->next)
      {
        BasicBlock* bb = KIND(li, eList_basic_block)->basic_block;
        DEBUG_print_basic_block(pass, &text, bb);
      }
    }
  }

  str_dump_to_file(&text, file_path);
  end_temp_memory(&pass->gp_arena);
}

IrLeaderStmt* get_leader_stmt(List* leaders, int stmt_nr)
{
  IrLeaderStmt* leader = 0;

  for(ListItem* li = leaders->first;
      li;
      li = li->next)
  {
    leader = KIND(li, eList_ir_leader_stmt)->ir_leader_stmt;
    if(stmt_nr == leader->stmt_nr)
      break;

    leader = 0;
  }

  return leader;
}

IrLeaderStmt* new_leader_stmt(MemoryArena* arena, int stmt_nr, IrStmt* stmt)
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

void insert_leader_stmt(IrPass* pass, List* leaders, int stmt_nr, IrStmt* stmt)
{
  assert(stmt);

  ListItem* li = leaders->first;
  assert(li);

  IrLeaderStmt* leader = KIND(li, eList_ir_leader_stmt)->ir_leader_stmt;
  assert(leader->stmt_nr == 0);

  for(; li; li = li->next)
  {
    leader = KIND(li, eList_ir_leader_stmt)->ir_leader_stmt;
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
      list_insert_before(leaders, li, new_leader_stmt(leaders->arena, stmt_nr, stmt), eList_ir_leader_stmt);
    }
  }
  else
  {
    list_append(leaders, new_leader_stmt(leaders->arena, stmt_nr, stmt), eList_ir_leader_stmt);
  }
}

void start_basic_block(IrPass* pass, List* leaders, int at_stmt_nr, IrStmt* stmt_array, int stmt_count)
{
  if(at_stmt_nr < stmt_count)
  {
    insert_leader_stmt(pass, leaders, at_stmt_nr, &stmt_array[at_stmt_nr]);
  }
}

void update_object_live_info(IrStmt_Assign* assign)
{
  assign->result->object->is_live = assign->result->is_live;
  assign->result->object->next_use = assign->result->next_use;

  assign->arg1->object->is_live = assign->arg1->is_live;
  assign->arg1->object->next_use = assign->arg1->next_use;

  if(assign->arg2)
  {
    assign->arg2->object->is_live = assign->arg2->is_live;
    assign->arg2->object->next_use = assign->arg2->next_use;
  }
}

Label* normalize_jump_target_labels(IrStmt* stmt)
{
  Label* target_label = 0;

  if(stmt->kind == eIrStmt_cond_goto)
  {
    IrStmt_CondGoto* cond_goto = &stmt->cond_goto;
    target_label = cond_goto->goto_label;
    if(target_label->primary)
    {
      cond_goto->goto_label = target_label->primary;
      target_label = cond_goto->goto_label;
    }
  }
  else if(stmt->kind == eIrStmt_goto)
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

void partition_basic_blocks_proc(IrPass* pass, AstNode* proc)
{
  if(proc->proc.ir_stmt_count > 0)
  {
    List* leaders = list_new(pass->gp_arena, eList_ir_leader_stmt);

    IrStmt* stmt_array = proc->proc.ir_stmt_array;
    int stmt_count = proc->proc.ir_stmt_count;

    IrStmt* stmt = &stmt_array[0];
    list_append(leaders, new_leader_stmt(leaders->arena, 0, stmt), eList_ir_leader_stmt);
    
    for(int i = 0; i < proc->proc.ir_stmt_count; i++)
    {
      stmt = &stmt_array[i];
      if(stmt->kind == eIrStmt_cond_goto || stmt->kind == eIrStmt_goto)
      {
        start_basic_block(pass, leaders, i+1, stmt_array, stmt_count);
        Label* target_label = normalize_jump_target_labels(stmt);
        start_basic_block(pass, leaders, target_label->stmt_nr, stmt_array, stmt_count);
      }
    }

    //------//
    
    List* basic_blocks = proc->proc.basic_blocks = list_new(pass->stmt_arena, eList_basic_block);

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
      BasicBlock* block = push_struct(pass->stmt_arena, BasicBlock);
      list_append(basic_blocks, block, eList_basic_block);
      list_init(&block->pred_list, pass->stmt_arena, eList_basic_block);
      list_init(&block->succ_list, pass->stmt_arena, eList_basic_block);
      leader->block = block;
      block->stmt_array = push_array(pass->stmt_arena, IrStmt*, next_stmt_nr - leader->stmt_nr);
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
        Label* goto_label = 0;
        if(last_stmt->kind == eIrStmt_cond_goto)
        {
          goto_label = last_stmt->cond_goto.goto_label;
        }
        else if(last_stmt->kind == eIrStmt_goto)
        {
          goto_label = last_stmt->goto_.goto_label;
        }
        else assert(0);
        
        int stmt_nr = goto_label->stmt_nr;
        IrLeaderStmt* leader = get_leader_stmt(leaders, stmt_nr);

        list_append(&bb->succ_list, leader->block, eList_basic_block);
        list_append(&leader->block->pred_list, bb, eList_basic_block);

        if(last_stmt->kind != eIrStmt_goto)
        {
          list_append(&bb->succ_list, bb_next, eList_basic_block);
          list_append(&bb_next->pred_list, bb, eList_basic_block);
        }
      }
      else if(bb_next)
      {
        list_append(&bb->succ_list, bb_next, eList_basic_block);
        list_append(&bb_next->pred_list, bb, eList_basic_block);
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

void partition_basic_blocks_module(IrPass* pass, AstNode* module, char* title)
{
  List* procs = &module->module.procs;
  for(ListItem* li = procs->first;
      li;
      li = li->next)
  {
    AstNode* proc = KIND(li, eList_ast_node)->ast_node;

    partition_basic_blocks_proc(pass, proc);
  }

  if(DEBUG_enabled)
    DEBUG_print_ir_code(pass, &module->module.procs, title);
}
