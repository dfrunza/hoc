void gen_instr(List* instr_list, AstNode* node);

int print_instruction(String* text, char* instr_list, ...)
{
  int text_len = 0;
  static char strbuf[128] = {0};
  va_list args;

  va_start(args, instr_list);
  text_len = h_vsprintf(strbuf, instr_list, args);
  va_end(args);

  str_append(text, strbuf);
  str_append(text, "\n");
  text_len++;
  
  return text_len;
}

void emit_instr_reg(List* instr_list, Opcode opcode, RegName reg)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = ParamType_reg;
  instr->param.reg = reg;
  append_list_elem(instr_list, instr, List_vm_instr);
}

void emit_instr_int8(List* instr_list, Opcode opcode, int8 int_val)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = ParamType_int8;
  instr->param.int_val = int_val;
  append_list_elem(instr_list, instr, List_vm_instr);
}

void emit_instr_int32(List* instr_list, Opcode opcode, int32 int_val)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = ParamType_int32;
  instr->param.int_val = int_val;
  append_list_elem(instr_list, instr, List_vm_instr);
}

void emit_instr_float32(List* instr_list, Opcode opcode, float32 float_val)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = ParamType_float32;
  instr->param.float_val = float_val;
  append_list_elem(instr_list, instr, List_vm_instr);
}

void emit_instr(List* instr_list, Opcode opcode)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = ParamType_None;
  append_list_elem(instr_list, instr, List_vm_instr);
}

void emit_instr_str(List* instr_list, Opcode opcode, char* str)
{
  assert(str);
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = ParamType_str;
  instr->param.str = str;
  append_list_elem(instr_list, instr, List_vm_instr);
}

void gen_load_lvalue(List* instr_list, AstNode* node)
{
  assert(node->gen == Ast_gen1);

  if(node->kind == AstNode_var_occur || node->kind == AstNode_str
      || node->kind == AstNode_assign)
  {
    DataArea* link = ATTR(node, symbol, occur_sym)->data_area;
    DataArea* data = ATTR(node, symbol, decl_sym)->data_area;

    emit_instr_reg(instr_list, Opcode_PUSH_REG, RegName_FP);

    if(link && link->decl_scope_offset > 0)
    {
      // non-local
      assert(link->loc < 0); // relative to FP
      emit_instr_int32(instr_list, Opcode_PUSH_INT32, link->loc);
      emit_instr(instr_list, Opcode_ADD_INT32);
      emit_instr_int32(instr_list, Opcode_LOAD, 4); // access link is on the stack now
    }
    emit_instr_int32(instr_list, Opcode_PUSH_INT32, data->loc);
    emit_instr(instr_list, Opcode_ADD_INT32);
  }
  else
    assert(0);
}

void gen_load_rvalue(List* instr_list, AstNode* node)
{
  assert(node->gen == Ast_gen1);

  if(node->kind == AstNode_var_occur)
  {
    Type* type = ATTR(node, type, eval_type);
    gen_load_lvalue(instr_list, node);
    emit_instr_int32(instr_list, Opcode_LOAD, type->width);
  }
  else if(node->kind == AstNode_proc_occur)
  {
    gen_instr(instr_list, node);
  }
  else if(node->kind == AstNode_lit)
  {
    LiteralKind kind = ATTR(node, lit_kind, lit_kind);
    if(kind == Literal_int_val || kind == Literal_bool_val)
    {
      emit_instr_int32(instr_list, Opcode_PUSH_INT32, ATTR(node, int_val, int_val));
    }
    else if(kind == Literal_float_val)
    {
      emit_instr_float32(instr_list, Opcode_PUSH_FLOAT32, ATTR(node, float_val, float_val));
    }
    else if(kind == Literal_char_val)
    {
      emit_instr_int8(instr_list, Opcode_PUSH_INT8, ATTR(node, char_val, char_val));
    }
    else
      assert(0);
  }
  else if(node->kind == AstNode_str)
  {
    gen_load_lvalue(instr_list, node);
  }
  else if(node->kind == AstNode_bin_expr)
  {
    gen_instr(instr_list, node);
  }
  else if(node->kind == AstNode_un_expr)
  {
    gen_instr(instr_list, node);
  }
  else
    assert(0);
}

void gen_instr(List* instr_list, AstNode* node)
{
  assert(node->gen == Ast_gen1);

  if(node->kind == AstNode_module)
  {
    AstNode* body = ATTR(node, ast_node, body);
    Scope* scope = ATTR(body, scope, scope);
    DataArea* local_area = &scope->local_area;

    emit_instr(instr_list, Opcode_ENTER);
    emit_instr_int32(instr_list, Opcode_GROW, local_area->size);

    List* stmts_list = ATTR(body, list, stmts);
    for(ListItem* list_item = stmts_list->first;
        list_item && (list_item->prev != stmts_list->last);
        list_item = list_item->next)
    {
      AstNode* stmt = ITEM(list_item, ast_node); 
      gen_instr(instr_list, stmt);
    }

    emit_instr(instr_list, Opcode_LEAVE);
    emit_instr(instr_list, Opcode_HALT);

    List* procs_list = ATTR(body, list, procs);
    for(ListItem* list_item = procs_list->first;
        list_item && (list_item->prev != procs_list->last);
        list_item = list_item->next)
    {
      AstNode* proc = ITEM(list_item, ast_node);
      assert(proc->kind == AstNode_proc_decl);
      gen_instr(instr_list, proc);
    }
  }
  else if(node->kind == AstNode_block)
  {
    Scope* scope = ATTR(node, scope, scope);
    DataArea* link_area = &scope->link_area;
    DataArea* local_area = &scope->local_area;

    if(link_area->size > 0)
    {
      for(ListItem* list_item = link_area->subareas->first;
          list_item;
          list_item = list_item->next)
      {
        DataArea* link = ITEM(list_item, data_area); assert(link->kind == DataArea_link);
        emit_instr_reg(instr_list, Opcode_PUSH_REG, RegName_FP);

        int offset = link->decl_scope_offset - 1;
        while(offset--)
        {
          emit_instr_int32(instr_list, Opcode_PUSH_INT32, -4);
          emit_instr(instr_list, Opcode_ADD_INT32); // get the FP of the previous activation record
          emit_instr_int32(instr_list, Opcode_LOAD, 4);
        }
      }
    }

    emit_instr(instr_list, Opcode_ENTER);
    emit_instr_int32(instr_list, Opcode_GROW, local_area->size);

    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item;
        list_item = list_item->next)
    {
      gen_instr(instr_list, ITEM(list_item, ast_node));
    }

    emit_instr(instr_list, Opcode_LEAVE);
    emit_instr_int32(instr_list, Opcode_GROW, -link_area->size);
  }
  else if(node->kind == AstNode_proc_decl)
  {
    Scope* scope = ATTR(node, scope, scope);
    DataArea* local_area = &scope->local_area;

    emit_instr_str(instr_list, Opcode_LABEL, ATTR(node, str_val, name));
    emit_instr_int32(instr_list, Opcode_GROW, local_area->size);

    AstNode* body = ATTR(node, ast_node, body);
    List* stmts_list = ATTR(body, list, stmts);
    for(ListItem* list_item = stmts_list->first;
        list_item && (list_item->prev != stmts_list->last);
        list_item = list_item->next)
    {
      AstNode* stmt = ITEM(list_item, ast_node); 
      gen_instr(instr_list, stmt);
    }

#if 0
    emit_instr_str(instr_list, Opcode_LABEL, ATTR(node, str_val, label_end));
#endif
    emit_instr(instr_list, Opcode_RETURN);

    List* procs_list = ATTR(body, list, procs);
    for(ListItem* list_item = procs_list->first;
        list_item && (list_item->prev != procs_list->last);
        list_item = list_item->next)
    {
      AstNode* proc = ITEM(list_item, ast_node);
      assert(proc->kind == AstNode_proc_decl);
      gen_instr(instr_list, proc);
    }
  }
  else if(node->kind == AstNode_var_decl)
  {
    AstNode* init_expr = ATTR(node, ast_node, init_expr);
    if(init_expr)
    {
      AstNode* expr = ATTR(init_expr, ast_node, expr);
      Type* type = ATTR(init_expr, type, eval_type);

      gen_load_rvalue(instr_list, expr);
      gen_load_lvalue(instr_list, init_expr);
      emit_instr_int32(instr_list, Opcode_STORE, type->width);
      emit_instr_int32(instr_list, Opcode_GROW, -type->width);
    }
  }
  else if(node->kind == AstNode_ret_stmt)
  {
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    if(ret_expr)
    {
      AstNode* expr = ATTR(ret_expr, ast_node, expr);
      Type* type = ATTR(expr, type, eval_type);

      gen_load_rvalue(instr_list, expr);
      gen_load_lvalue(instr_list, ret_expr);
      emit_instr_int32(instr_list, Opcode_STORE, type->width);
      emit_instr_int32(instr_list, Opcode_GROW, -type->width);
    }

    int depth = ATTR(node, int_val, nesting_depth);
    while(depth-- > 0)
    {
      emit_instr(instr_list, Opcode_LEAVE);
    }

    emit_instr(instr_list, Opcode_RETURN);
#if 0
    AstNode* proc = ATTR(node, ast_node, proc_decl);
    emit_instr_str(instr_list, Opcode_GOTO, ATTR(proc, str_val, label_end));
#endif
  }
  else if(node->kind == AstNode_stmt)
  {
    AstNode* actual_stmt = ATTR(node, ast_node, stmt);
    Type* type = ATTR(actual_stmt, type, eval_type);
    gen_instr(instr_list, actual_stmt);
    emit_instr_int32(instr_list, Opcode_GROW, -type->width);
  }
  else if(node->kind == AstNode_while_stmt)
  {
    emit_instr_str(instr_list, Opcode_LABEL, ATTR(node, str_val, label_eval));
    gen_load_rvalue(instr_list, ATTR(node, ast_node, cond_expr));
    emit_instr_str(instr_list, Opcode_JUMPZ, ATTR(node, str_val, label_break));

    AstNode* body = ATTR(node, ast_node, body);
    if(body)
    {
      gen_instr(instr_list, body);
    }

    emit_instr_str(instr_list, Opcode_GOTO, ATTR(node, str_val, label_eval));
    emit_instr_str(instr_list, Opcode_LABEL, ATTR(node, str_val, label_break));
  }
  else if(node->kind == AstNode_bin_expr)
  {
    AstNode* left_operand = ATTR(node, ast_node, left_operand);
    AstNode* right_operand = ATTR(node, ast_node, right_operand);
    Type* type = ATTR(node, type, eval_type);
    Type* left_ty = ATTR(left_operand, type, eval_type);
    Type* right_ty = ATTR(right_operand, type, eval_type);
    OperatorKind bin_op = ATTR(node, op_kind, op_kind);

    if(bin_op == Operator_assign)
    {
      gen_load_rvalue(instr_list, right_operand);

      if(left_operand->kind == AstNode_var_occur)
      {
        gen_load_lvalue(instr_list, left_operand);
      }
      else if(left_operand->kind == AstNode_un_expr)
      {
        if(ATTR(left_operand, op_kind, op_kind) == Operator_deref)
        {
          gen_load_rvalue(instr_list, ATTR(left_operand, ast_node, operand));
        }
        else
          assert(0);
      }

      emit_instr_int32(instr_list, Opcode_STORE, type->width);
    }
    else if(bin_op == Operator_add)
    {
      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == Type_pointer)
      {
        emit_instr(instr_list, Opcode_ADD_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_instr(instr_list, Opcode_ADD_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_sub)
    {
      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == Type_pointer)
      {
        emit_instr(instr_list, Opcode_SUB_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_instr(instr_list, Opcode_SUB_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_mul)
    {
      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == Type_pointer)
      {
        emit_instr(instr_list, Opcode_MUL_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_instr(instr_list, Opcode_MUL_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_div)
    {
      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == Type_pointer)
      {
        emit_instr(instr_list, Opcode_DIV_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_instr(instr_list, Opcode_DIV_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_mod)
    {
      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == Type_pointer)
      {
        emit_instr(instr_list, Opcode_MOD_INT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_eq)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(instr_list, Opcode_CMPEQ_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(instr_list, Opcode_CMPEQ_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(instr_list, Opcode_CMPEQ_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_not_eq)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(instr_list, Opcode_CMPNEQ_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(instr_list, Opcode_CMPNEQ_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(instr_list, Opcode_CMPNEQ_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_less)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(instr_list, Opcode_CMPLSS_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(instr_list, Opcode_CMPLSS_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(instr_list, Opcode_CMPLSS_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_greater)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(instr_list, Opcode_CMPGRT_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(instr_list, Opcode_CMPGRT_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(instr_list, Opcode_CMPGRT_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_logic_and || bin_op == Operator_logic_or)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(instr_list, left_operand);
      emit_instr(instr_list, Opcode_DUP);

      if(bin_op == Operator_logic_and)
      {
        emit_instr_str(instr_list, Opcode_JUMPZ, ATTR(node, str_val, label_end));
      }
      else if(bin_op == Operator_logic_or)
      {
        emit_instr_str(instr_list, Opcode_JUMPNZ, ATTR(node, str_val, label_end));
      }
      else
        assert(0);

      emit_instr_int32(instr_list, Opcode_GROW, -type->width);
      gen_load_rvalue(instr_list, right_operand);
      emit_instr(instr_list, Opcode_DUP);

      if(bin_op == Operator_logic_and)
      {
        emit_instr_str(instr_list, Opcode_JUMPZ, ATTR(node, str_val, label_end));
      }
      else if(bin_op == Operator_logic_or)
      {
        emit_instr_str(instr_list, Opcode_JUMPNZ, ATTR(node, str_val, label_end));
      }
      else
        assert(0);

      emit_instr_str(instr_list, Opcode_LABEL, ATTR(node, str_val, label_end));
    }
    else if(bin_op == Operator_less_eq || bin_op == Operator_greater_eq)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(bin_op == Operator_less_eq)
      {
        if(types_are_equal(left_ty, basic_type_char))
        {
          emit_instr(instr_list, Opcode_CMPLSS_INT8);
        }
        else if(types_are_equal(left_ty, basic_type_int))
        {
          emit_instr(instr_list, Opcode_CMPLSS_INT32);
        }
        else if(types_are_equal(left_ty, basic_type_float))
        {
          emit_instr(instr_list, Opcode_CMPLSS_FLOAT32);
        }
        else
          assert(0);
      }
      else if(bin_op == Operator_greater_eq)
      {
        if(types_are_equal(left_ty, basic_type_char))
        {
          emit_instr(instr_list, Opcode_CMPGRT_INT8);
        }
        else if(types_are_equal(left_ty, basic_type_int))
        {
          emit_instr(instr_list, Opcode_CMPGRT_INT32);
        }
        else if(types_are_equal(left_ty, basic_type_float))
        {
          emit_instr(instr_list, Opcode_CMPGRT_FLOAT32);
        }
        else
          assert(0);
      }
      else
        assert(0);

      emit_instr(instr_list, Opcode_DUP);
      emit_instr_int32(instr_list, Opcode_GROW, -type->width);

      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(instr_list, Opcode_CMPNEQ_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(instr_list, Opcode_CMPNEQ_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(instr_list, Opcode_CMPNEQ_FLOAT32);
      }
      else
        assert(0);

      emit_instr_str(instr_list, Opcode_LABEL, ATTR(node, str_val, label_end));
    }
    else if(bin_op == Operator_cast)
    {
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_int) && types_are_equal(right_ty, basic_type_float))
      {
        emit_instr(instr_list, Opcode_FLOAT32_TO_INT32);
      }
      if(types_are_equal(left_ty, basic_type_float) && types_are_equal(right_ty, basic_type_int))
      {
        emit_instr(instr_list, Opcode_INT32_TO_FLOAT32);
      }
    }
    else if(bin_op == Operator_array_index)
    {
      Type* type = ATTR(node, type, eval_type);

      gen_load_lvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);
      emit_instr_int32(instr_list, Opcode_PUSH_INT32, type->width);
      emit_instr(instr_list, Opcode_MUL_INT32);
      emit_instr(instr_list, Opcode_ADD_INT32);
      emit_instr_int32(instr_list, Opcode_LOAD, type->width);
    }
    else
      assert(0);
  }
  else if(node->kind == AstNode_un_expr)
  {
    AstNode* operand = ATTR(node, ast_node, operand);
    Type* type = ATTR(node, type, eval_type);
    Type* operand_ty = ATTR(node, type, eval_type);
    OperatorKind un_op = ATTR(node, op_kind, op_kind);

    if(un_op == Operator_address_of)
    {
      if(operand->kind == AstNode_var_occur)
      {
        gen_load_lvalue(instr_list, operand);
      }
      else
        gen_load_rvalue(instr_list, operand);
    }
    else if(un_op == Operator_neg)
    {
      gen_load_rvalue(instr_list, operand);

      if(types_are_equal(operand_ty, basic_type_int))
      {
        emit_instr(instr_list, Opcode_NEG_INT32);
      }
      else if(types_are_equal(operand_ty, basic_type_float))
      {
        emit_instr(instr_list, Opcode_NEG_FLOAT32);
      }
      else
        assert(0);
    }
    else if(un_op == Operator_logic_not)
    {
      gen_load_rvalue(instr_list, operand);
      emit_instr_int32(instr_list, Opcode_PUSH_INT32, 0);
      emit_instr(instr_list, Opcode_CMPEQ_INT32);
    }
    else if(un_op == Operator_deref)
    {
      assert(ATTR(operand, type, type)->kind == Type_pointer);
      emit_instr_int32(instr_list, Opcode_LOAD, type->width);
    }
    else
      assert(0);
  }
  else if(node->kind == AstNode_proc_occur)
  {
    AstNode* proc = ATTR(node, ast_node, proc_decl);
    Scope* scope = ATTR(proc, scope, scope);
    DataArea* ret_area = &scope->ret_area;
    DataArea* args_area = &scope->args_area;
    DataArea* link_area = &scope->link_area;

    emit_instr_int32(instr_list, Opcode_GROW, ret_area->size);

    for(ListItem* list_item = ATTR(node, list, actual_args)->first;
        list_item;
        list_item = list_item->next)
    {
      gen_load_rvalue(instr_list, ITEM(list_item, ast_node));
    }
    //todo: assert that the 'data area size' == 'evaluated args size'

    emit_instr_str(instr_list, Opcode_CALL, ATTR(node, str_val, name));
    emit_instr_int32(instr_list, Opcode_GROW, -link_area->size);
    emit_instr_int32(instr_list, Opcode_GROW, -args_area->size);
  }
  else if(node->kind == AstNode_if_stmt)
  {
    gen_load_rvalue(instr_list, ATTR(node, ast_node, cond_expr));

    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      emit_instr_str(instr_list, Opcode_JUMPZ, ATTR(node, str_val, label_else));
    }
    else
    {
      emit_instr_str(instr_list, Opcode_JUMPZ, ATTR(node, str_val, label_end));
    }

    gen_instr(instr_list, ATTR(node, ast_node, body));
    emit_instr_str(instr_list, Opcode_GOTO, ATTR(node, str_val, label_end));

    if(else_body)
    {
      emit_instr_str(instr_list, Opcode_LABEL, ATTR(node, str_val, label_else));
      gen_instr(instr_list, else_body);
    }

    emit_instr_str(instr_list, Opcode_LABEL, ATTR(node, str_val, label_end));
  }
  else if(node->kind == AstNode_while_stmt)
  {
    emit_instr_str(instr_list, Opcode_LABEL, ATTR(node, str_val, label_eval));
    gen_load_rvalue(instr_list, ATTR(node, ast_node, cond_expr));
    emit_instr_str(instr_list, Opcode_JUMPZ, ATTR(node, str_val, label_break));

    AstNode* body = ATTR(node, ast_node, body);
    if(body)
    {
      gen_instr(instr_list, body);
    }

    emit_instr_str(instr_list, Opcode_GOTO, ATTR(node, str_val, label_eval));
    emit_instr_str(instr_list, Opcode_LABEL, ATTR(node, str_val, label_break));
  }
  else if(node->kind == AstNode_break_stmt)
  {
    AstNode* loop = ATTR(node, ast_node, loop);

    int depth = ATTR(node, int_val, nesting_depth);
    while(depth-- > 0)
    {
      emit_instr(instr_list, Opcode_LEAVE);
    }
    emit_instr_str(instr_list, Opcode_GOTO, ATTR(loop, str_val, label_break));
  }
  else if(node->kind == AstNode_empty)
  {
    ;//skip
  }
  else
    assert(0);
}

void sort_block_nodes(AstNode* node)
{
  if(node->kind == AstNode_module)
  {
    sort_block_nodes(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_proc_decl)
  {
    sort_block_nodes(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_while_stmt)
  {
    sort_block_nodes(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_if_stmt)
  {
    sort_block_nodes(ATTR(node, ast_node, body));
    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      sort_block_nodes(ATTR(node, ast_node, else_body));
    }
  }
  else if(node->kind == AstNode_block)
  {
    List* procs_list = ATTR(node, list, procs) = new_list(arena, List_ast_node);
    List* stmts_list = ATTR(node, list, stmts) = new_list(arena, List_ast_node);

    List* nodes_list = ATTR(node, list, nodes);
    for(ListItem* list_item = nodes_list->first;
        list_item;)
    {
      AstNode* stmt = ITEM(list_item, ast_node);
      sort_block_nodes(stmt);

      ListItem* next_list_item = list_item->next;
      remove_list_item(nodes_list, list_item);

      if(stmt->kind == AstNode_proc_decl)
      {
        append_list_item(procs_list, list_item);
      }
      else if(stmt->kind == AstNode_stmt
          || stmt->kind == AstNode_while_stmt || stmt->kind == AstNode_if_stmt
          || stmt->kind == AstNode_break_stmt || stmt->kind == AstNode_continue_stmt)
      {
        append_list_item(stmts_list, list_item);
      }
      else
        assert(0);

      list_item = next_list_item;
    }

    concat_lists(stmts_list, procs_list);

    *nodes_list = *stmts_list;
    if(procs_list->last)
    {
      nodes_list->last = procs_list->last;
    }
  }
}

void gen_labels(AstNode* node)
{
  if(node->kind == AstNode_module)
  {
    gen_labels(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_block)
  {
    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item;
        list_item = list_item->next)
    {
      gen_labels(ITEM(list_item, ast_node));
    }
  }
  else if(node->kind == AstNode_proc_decl)
  {
    String* label = str_new(arena);
    str_append(label, ATTR(node, str_val, name));
    str_append(label, "$proc_end");
    ATTR(node, str_val, label_end) = str_cap(label);

    gen_labels(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_stmt)
  {
    gen_labels(ATTR(node, ast_node, stmt));
  }
  else if(node->kind == AstNode_bin_expr)
  {
    char* label_id = make_unique_label();

    String* label = str_new(arena);
    str_append(label, label_id);
    str_append(label, "$logic_end");
    ATTR(node, str_val, label_end) = str_cap(label);

    gen_labels(ATTR(node, ast_node, left_operand));
    gen_labels(ATTR(node, ast_node, right_operand));
  }
  else if(node->kind == AstNode_un_expr)
  {
    gen_labels(ATTR(node, ast_node, operand));
  }
  else if(node->kind == AstNode_if_stmt)
  {
    gen_labels(ATTR(node, ast_node, cond_expr));

    char* label_id = make_unique_label();

    String* label = str_new(arena);
    str_append(label, label_id);
    str_append(label, "$if_else");
    ATTR(node, str_val, label_else) = str_cap(label);

    label = str_new(arena);
    str_append(label, label_id);
    str_append(label, "$if_end");
    ATTR(node, str_val, label_end) = str_cap(label);

    gen_labels(ATTR(node, ast_node, body));

    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      gen_labels(else_body);
    }
  }
  else if(node->kind == AstNode_while_stmt)
  {
    gen_labels(ATTR(node, ast_node, cond_expr));

    char* label_id = make_unique_label();

    String* label = str_new(arena);
    str_append(label, label_id);
    str_append(label, "$while_eval");
    ATTR(node, str_val, label_eval) = str_cap(label);

    label = str_new(arena);
    str_append(label, label_id);
    str_append(label, "$while_break");
    ATTR(node, str_val, label_break) = str_cap(label);

    AstNode* body = ATTR(node, ast_node, body);
    if(body)
    {
      gen_labels(body);
    }
  }
  else if(node->kind == AstNode_proc_occur)
  {
    for(ListItem* list_item = ATTR(node, list, actual_args)->first;
        list_item;
        list_item = list_item->next)
    {
      gen_labels(ITEM(list_item, ast_node));
    }
  }
  else if(node->kind == AstNode_ret_stmt)
  {
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    if(ret_expr)
    {
      gen_labels(ret_expr);
    }
  }
  else if(node->kind == AstNode_var_decl)
  {
    AstNode* init_expr = ATTR(node, ast_node, init_expr);
    if(init_expr)
    {
      gen_labels(init_expr);
    }
  }
}

void gen_program(VmProgram* vm_program, AstNode* module)
{
  assert(module->kind == AstNode_module);

  gen_labels(module);
  sort_block_nodes(module);
  gen_instr(vm_program->instr_list, module);

  AstNode* body = ATTR(module, ast_node, body);
  Scope* scope = ATTR(body, scope, scope);

  int data_size = 0;
  for(ListItem* list_item = scope->pre_fp_areas->first;
      list_item;
      list_item = list_item->next)
  {
    data_size += ITEM(list_item, data_area)->size;
  }
  
  int fp = data_size;
  DataArea* ctrl_area = &scope->ctrl_area;
  vm_program->sp = fp - ctrl_area->size;

  for(ListItem* list_item = scope->post_fp_areas->first;
      list_item;
      list_item = list_item->next)
  {
    data_size += ITEM(list_item, data_area)->size;
  }

  vm_program->data = mem_push_array(arena, uint8, data_size);
  vm_program->data_size = data_size;

  for(ListItem* list_item = scope->pre_fp_areas->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* area = ITEM(list_item, data_area);
    if(area->data)
    {
      uint8* p_data = (uint8*)area->data;
      for(int i = 0; i < area->size; i++)
      {
        vm_program->data[i + fp + area->loc] = p_data[i];
      }
    }
  }

  for(ListItem* list_item = scope->post_fp_areas->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* area = ITEM(list_item, data_area);
    if(area->data)
    {
      uint8* p_data = (uint8*)area->data;
      for(int i = 0; i < area->size; i++)
      {
        vm_program->data[i + fp + area->loc] = p_data[i];
      }
    }
  }

#if 0
  for(ListItem* list_item = scope->decls[Symbol_str]->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* area = ITEM(list_item, symbol)->data_area;
    if(area->data)
    {
      uint8* p_data = (uint8*)area->data;
      for(int i = 0; i < area->size; i++)
      {
        vm_program->data[i + area->loc] = p_data[i];
      }
    }
#if 0
    Symbol* str_sym = ITEM(list_item, symbol);
    DataArea* area = str_sym->data_area;
    if(str_sym->data)
    {
      for(int i = 0; i < area->size; i++)
      {
        vm_program->data[i + area->loc] = ((uint8*)str_sym->data)[i];
      }
    }
#endif
  }
#endif
}

char* get_regname_str(RegName reg)
{
  static char* reg_fp = "fp";
  static char* reg_sp = "sp";
  static char* reg_ip = "ip";
  char* regname = 0;

  if(reg == RegName_FP)
    regname = reg_fp;
  else if(reg == RegName_SP)
    regname = reg_sp;
  else if(reg == RegName_IP)
    regname = reg_ip;
  else
    assert(0);
  return regname;
}

void print_code(VmProgram* vm_program)
{
  String* text = str_new(arena);
  int text_len = 0;

  for(ListItem* list_item = vm_program->instr_list->first;
      list_item;
      list_item = list_item->next)
  {
    Instruction* instr = (Instruction*)list_item->elem;
    switch(instr->opcode)
    {
      case Opcode_PUSH_INT8:
      {
        if(instr->param_type == ParamType_int8)
        {
          text_len += print_instruction(text, "push_char %d", instr->param.int_val);
        }
        else
          assert(0);
      } break;

      case Opcode_PUSH_INT32:
      {
        if(instr->param_type == ParamType_int32)
        {
          text_len += print_instruction(text, "push_int %d", instr->param.int_val);
        }
        else
          assert(0);
      } break;

      case Opcode_PUSH_REG:
      {
        if(instr->param_type == ParamType_reg)
        {
          text_len += print_instruction(text, "push_reg %s", get_regname_str(instr->param.reg));
        }
        else
          assert(0);
      } break;

      case Opcode_PUSH_FLOAT32:
      {
        if(instr->param_type == ParamType_float32)
        {
          text_len += print_instruction(text, "push_float %f", instr->param.float_val);
        }
        else
          assert(0);
      } break;

      case Opcode_POP_REG:
      {
        if(instr->param_type == ParamType_reg)
        {
          text_len += print_instruction(text, "pop_reg %s", get_regname_str(instr->param.reg));
        }
        else
          assert(0);
      }

      case Opcode_DUP:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "dup");
      } break;

      case Opcode_ADD_INT32:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "add_int");
      } break;

      case Opcode_SUB_INT32:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "sub_int");
      } break;

      case Opcode_MUL_INT32:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "mul_int");
      } break;

      case Opcode_DIV_INT32:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "div_int");
      } break;

      case Opcode_ADD_FLOAT32:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "add_float");
      } break;

      case Opcode_SUB_FLOAT32:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "sub_float");
      } break;

      case Opcode_MUL_FLOAT32:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "mul_float");
      } break;

      case Opcode_DIV_FLOAT32:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "div_float");
      } break;

      case Opcode_NEG_FLOAT32:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "neg_float");
      } break;

      case Opcode_MOD_INT32:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "mod_int");
      } break;

      case Opcode_NEG_INT32:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "neg_int");
      } break;

      case Opcode_LOAD:
      {
        assert(instr->param_type == ParamType_int32);
        text_len += print_instruction(text, "load %d", instr->param.int_val);
      } break;

      case Opcode_STORE:
      {
        assert(instr->param_type == ParamType_int32);
        text_len += print_instruction(text, "store %d", instr->param.int_val);
      } break;

      case Opcode_LABEL:
      {
        assert(instr->param_type == ParamType_str);
        text_len += print_instruction(text, "label %s", instr->param.str);
      } break;

      case Opcode_RETURN:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "return");
      } break;

      case Opcode_GROW:
      {
        assert(instr->param_type == ParamType_int32);
        text_len += print_instruction(text, "grow %d", instr->param.int_val);
      } break;

#if 0
      case Opcode_NEW:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "new");
      } break;
#endif

      case Opcode_CALL:
      {
        assert(instr->param_type == ParamType_str);
        text_len += print_instruction(text, "call %s", instr->param.str);
      } break;

      case Opcode_HALT:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "halt");
      } break;

      case Opcode_GOTO:
      {
        assert(instr->param_type == ParamType_str);
        text_len += print_instruction(text, "goto %s", instr->param.str);
      } break;

      case Opcode_JUMPZ:
      {
        assert(instr->param_type == ParamType_str);
        text_len += print_instruction(text, "jumpz %s", instr->param.str);
      } break;

      case Opcode_JUMPNZ:
      {
        assert(instr->param_type == ParamType_str);
        text_len += print_instruction(text, "jumpnz %s", instr->param.str);
      } break;

      case Opcode_DECR_INT32:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "decr_int");
      } break;

      case Opcode_ENTER:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "enter");
      } break;

      case Opcode_LEAVE:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "leave");
      } break;

      case Opcode_NOOP:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "noop");
      } break;

      case Opcode_CMPEQ_INT8:
      case Opcode_CMPNEQ_INT8:
      case Opcode_CMPLSS_INT8:
      case Opcode_CMPGRT_INT8:
      case Opcode_CMPEQ_INT32:
      case Opcode_CMPNEQ_INT32:
      case Opcode_CMPLSS_INT32:
      case Opcode_CMPGRT_INT32:
      case Opcode_CMPEQ_FLOAT32:
      case Opcode_CMPNEQ_FLOAT32:
      case Opcode_CMPLSS_FLOAT32:
      case Opcode_CMPGRT_FLOAT32:
      {
        assert(instr->param_type == ParamType_None);
        if(instr->opcode == Opcode_CMPEQ_INT8)
        {
          text_len += print_instruction(text, "cmpeq_char");
        }
        else if(instr->opcode == Opcode_CMPNEQ_INT8)
        {
          text_len += print_instruction(text, "cmpneq_char");
        }
        else if(instr->opcode == Opcode_CMPLSS_INT8)
        {
          text_len += print_instruction(text, "cmplss_char");
        }
        else if(instr->opcode == Opcode_CMPGRT_INT8)
          text_len += print_instruction(text, "cmpgrt_char");
        else if(instr->opcode == Opcode_CMPEQ_INT32)
        {
          text_len += print_instruction(text, "cmpeq_int");
        }
        else if(instr->opcode == Opcode_CMPNEQ_INT32)
        {
          text_len += print_instruction(text, "cmpneq_int");
        }
        else if(instr->opcode == Opcode_CMPLSS_INT32)
        {
          text_len += print_instruction(text, "cmplss_int");
        }
        else if(instr->opcode == Opcode_CMPGRT_INT32)
        {
          text_len += print_instruction(text, "cmpgrt_int");
        }
        else if(instr->opcode == Opcode_CMPEQ_FLOAT32)
        {
          text_len += print_instruction(text, "cmpeq_float");
        }
        else if(instr->opcode == Opcode_CMPNEQ_FLOAT32)
        {
          text_len += print_instruction(text, "cmpneq_float");
        }
        else if(instr->opcode == Opcode_CMPLSS_FLOAT32)
        {
          text_len += print_instruction(text, "cmplss_float");
        }
        else if(instr->opcode == Opcode_CMPGRT_FLOAT32)
        {
          text_len += print_instruction(text, "cmpgrt_float");
        }
        else
          assert(0);
      } break;

      case Opcode_AND:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "and");
      } break;

      case Opcode_OR:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "or");
      } break;

      case Opcode_NOT:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "not");
      } break;

#if 0
      case Opcode_PUTC:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "putc");
      } break;
#endif

      case Opcode_FLOAT32_TO_INT32:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "float_to_int");
      } break;

      case Opcode_INT32_TO_FLOAT32:
      {
        assert(instr->param_type == ParamType_None);
        text_len += print_instruction(text, "int_to_float");
      } break;

      default:
        assert(0);
    }
  }

  vm_program->text = str_cap(text);
  vm_program->text_len = text_len;
}

