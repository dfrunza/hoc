bool convert_hasm_to_instructions(TargetCode* target_code);
void gen_load_rvalue(List* instr_list, AstNode* node);
bool gen_instr(List* instr_list, AstNode* node);

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

void emit_instr_reg(List* instr_list, eOpcode opcode, eRegName reg)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = eParamType_reg;
  instr->param.reg = reg;
  append_list_elem(instr_list, instr, eList_vm_instr);
}

void emit_instr_int8(List* instr_list, eOpcode opcode, int8 int_val)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = eParamType_int8;
  instr->param.int_val = int_val;
  append_list_elem(instr_list, instr, eList_vm_instr);
}

void emit_instr_int32(List* instr_list, eOpcode opcode, int32 int_val)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = eParamType_int32;
  instr->param.int_val = int_val;
  append_list_elem(instr_list, instr, eList_vm_instr);
}

void emit_instr_float32(List* instr_list, eOpcode opcode, float32 float_val)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = eParamType_float32;
  instr->param.float_val = float_val;
  append_list_elem(instr_list, instr, eList_vm_instr);
}

void emit_instr(List* instr_list, eOpcode opcode)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = eParamType_None;
  append_list_elem(instr_list, instr, eList_vm_instr);
}

void emit_instr_str(List* instr_list, eOpcode opcode, char* str)
{
  assert(str);
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = eParamType_str;
  instr->param.str = str;
  append_list_elem(instr_list, instr, eList_vm_instr);
}

void gen_load_lvalue(List* instr_list, AstNode* node)
{
  assert(node->gen == eAstGen_gen1);

  if(node->kind == eAstNode_var_occur)
  {
    Symbol* occur_sym = ATTR(node, symbol, occur_sym);
    Symbol* decl_sym = occur_sym->decl;
    Scope* scope = occur_sym->scope;
    DataArea* ctrl_area = &scope->ctrl_area;
    DataArea* link = occur_sym->data_area;
    DataArea* data = decl_sym->data_area;

    int decl_scope_offset = occur_sym->nesting_depth - decl_sym->nesting_depth;
    if(decl_scope_offset > 0)
    {
      // Non-local
      assert(link->loc < 0);
      emit_instr_reg(instr_list, eOpcode_PUSH_REG, eRegName_FP);
      emit_instr_int32(instr_list, eOpcode_PUSH_INT32, link->loc);
      emit_instr(instr_list, eOpcode_ADD_INT32);

      for(int i = decl_scope_offset; i > 0; i--)
      {
        emit_instr_int32(instr_list, eOpcode_LOAD, link->size);
      }

      // The FP is offset relative to the Access Link
      emit_instr_int32(instr_list, eOpcode_PUSH_INT32, ctrl_area->size + link->size);
      emit_instr(instr_list, eOpcode_ADD_INT32);
    }
    else
    {
      emit_instr_reg(instr_list, eOpcode_PUSH_REG, eRegName_FP);
    }

    emit_instr_int32(instr_list, eOpcode_PUSH_INT32, data->loc);
    emit_instr(instr_list, eOpcode_ADD_INT32);
  }
  else if(node->kind == eAstNode_un_expr)
  {
    AstNode* operand = ATTR(node, ast_node, operand);
    eOperator un_op = ATTR(node, op_kind, op_kind);

    if(un_op == eOperator_deref)
    {
      assert(ATTR(operand, type, eval_type)->kind == eType_pointer);
      gen_load_rvalue(instr_list, operand);
    }
    else
      assert(0);
  }
  else if(node->kind == eAstNode_bin_expr)
  {
    Type* type = ATTR(node, type, eval_type);
    AstNode* left_operand = ATTR(node, ast_node, left_operand);
    AstNode* right_operand = ATTR(node, ast_node, right_operand);
    eOperator bin_op = ATTR(node, op_kind, op_kind);

    if(bin_op == eOperator_index)
    {
      gen_load_lvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);
      emit_instr_int32(instr_list, eOpcode_PUSH_INT32, type->width);
      emit_instr(instr_list, eOpcode_MUL_INT32);
      emit_instr(instr_list, eOpcode_ADD_INT32);
    }
    else
      assert(0);
  }
  else
    assert(0);
}

void gen_load_rvalue(List* instr_list, AstNode* node)
{
  assert(node->gen == eAstGen_gen1);

  if(node->kind == eAstNode_var_occur)
  {
    Type* type = ATTR(node, type, eval_type);
    gen_load_lvalue(instr_list, node);
    emit_instr_int32(instr_list, eOpcode_LOAD, type->width);
  }
  else if(node->kind == eAstNode_proc_occur)
  {
    gen_instr(instr_list, node);
  }
  else if(node->kind == eAstNode_lit)
  {
    eLiteral kind = ATTR(node, lit_kind, lit_kind);
    if(kind == eLiteral_int_val)
    {
      emit_instr_int32(instr_list, eOpcode_PUSH_INT32, ATTR(node, int_val, int_val));
    }
    else if(kind == eLiteral_bool_val)
    {
      emit_instr_int32(instr_list, eOpcode_PUSH_INT32, ATTR(node, bool_val, bool_val));
    }
    else if(kind == eLiteral_float_val)
    {
      emit_instr_float32(instr_list, eOpcode_PUSH_FLOAT32, ATTR(node, float_val, float_val));
    }
    else if(kind == eLiteral_char_val)
    {
      emit_instr_int8(instr_list, eOpcode_PUSH_INT8, ATTR(node, char_val, char_val));
    }
    else
      assert(0);
  }
  else if(node->kind == eAstNode_bin_expr)
  {
    Type* type = ATTR(node, type, eval_type);
    eOperator bin_op = ATTR(node, op_kind, op_kind);

    if(bin_op == eOperator_index)
    {
      gen_load_lvalue(instr_list, node);
      emit_instr_int32(instr_list, eOpcode_LOAD, type->width);
    }
    else
    {
      gen_instr(instr_list, node);
    }
  }
  else if(node->kind == eAstNode_un_expr)
  {
    Type* type = ATTR(node, type, eval_type);
    AstNode* operand = ATTR(node, ast_node, operand);
    eOperator un_op = ATTR(node, op_kind, op_kind);

    if(un_op == eOperator_address_of)
    {
      if(operand->kind == eAstNode_var_occur)
      {
        gen_load_lvalue(instr_list, operand);
      }
      else
      {
        gen_load_rvalue(instr_list, operand);
      }
    }
    else if(un_op == eOperator_deref)
    {
      gen_load_lvalue(instr_list, node);
      emit_instr_int32(instr_list, eOpcode_LOAD, type->width);
    }
    else
    {
      gen_instr(instr_list, node);
    }
  }
  else
    assert(0);
}

bool gen_instr(List* instr_list, AstNode* node)
{
  assert(node->gen == eAstGen_gen1);
  bool success = true;

  if(node->kind == eAstNode_module)
  {
    AstNode* body = ATTR(node, ast_node, body);
    Scope* scope = ATTR(body, scope, scope);
    DataArea* local_area = &scope->local_area;

    emit_instr_reg(instr_list, eOpcode_PUSH_REG, eRegName_FP);
    emit_instr(instr_list, eOpcode_ENTER);
    emit_instr_int32(instr_list, eOpcode_GROWNZ, local_area->size);

    for(ListItem* list_item = ATTR(body, list, stmts)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* stmt = ITEM(list_item, ast_node); assert(stmt->kind == eAstNode_stmt);
      gen_instr(instr_list, stmt);
    }

    emit_instr(instr_list, eOpcode_LEAVE);
    emit_instr(instr_list, eOpcode_HALT);

    for(ListItem* list_item = ATTR(body, list, procs)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* proc = ITEM(list_item, ast_node);
      assert(proc->kind == eAstNode_proc_decl);
      gen_instr(instr_list, proc);
    }
  }
  else if(node->kind == eAstNode_proc_decl)
  {
    Scope* scope = ATTR(node, scope, scope);
    DataArea* local_area = &scope->local_area;

    emit_instr_str(instr_list, eOpcode_LABEL, ATTR(node, str_val, name));
    emit_instr_int32(instr_list, eOpcode_GROW, local_area->size);

    AstNode* body = ATTR(node, ast_node, body);

    for(ListItem* list_item = ATTR(body, list, stmts)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* stmt = ITEM(list_item, ast_node);
      gen_instr(instr_list, stmt);
    }

    emit_instr(instr_list, eOpcode_RETURN);

    for(ListItem* list_item = ATTR(body, list, procs)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* proc = ITEM(list_item, ast_node); assert(proc->kind == eAstNode_proc_decl);
      gen_instr(instr_list, proc);
    }
  }
  else if(node->kind == eAstNode_block)
  {
    Scope* scope = ATTR(node, scope, scope);
    DataArea* link_area = &scope->link_area;
    DataArea* local_area = &scope->local_area;

    // Setup the Access Link
    emit_instr_reg(instr_list, eOpcode_PUSH_REG, eRegName_FP);
    emit_instr_int32(instr_list, eOpcode_PUSH_INT32, link_area->loc);
    emit_instr(instr_list, eOpcode_ADD_INT32);

    emit_instr(instr_list, eOpcode_ENTER);
    emit_instr_int32(instr_list, eOpcode_GROW, local_area->size);

    for(ListItem* list_item = ATTR(node, list, stmts)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* stmt = ITEM(list_item, ast_node);
      gen_instr(instr_list, stmt);
    }

    emit_instr(instr_list, eOpcode_LEAVE);
    emit_instr_int32(instr_list, eOpcode_GROW, -link_area->size);

    for(ListItem* list_item = ATTR(node, list, procs)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* proc = ITEM(list_item, ast_node); assert(proc->kind == eAstNode_proc_decl);
      gen_instr(instr_list, proc);
    }
  }
  else if(node->kind == eAstNode_ret_stmt)
  {
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    if(ret_expr)
    {
      gen_instr(instr_list, ret_expr);
    }

    int depth = ATTR(node, int_val, nesting_depth);
    while(depth-- > 0)
    {
      emit_instr(instr_list, eOpcode_LEAVE);
    }

    emit_instr(instr_list, eOpcode_RETURN);
  }
  else if(node->kind == eAstNode_stmt)
  {
    AstNode* actual_stmt = ATTR(node, ast_node, stmt);
    Type* type = ATTR(actual_stmt, type, eval_type);
    gen_instr(instr_list, actual_stmt);
    emit_instr_int32(instr_list, eOpcode_GROW, -type->width);
  }
  else if(node->kind == eAstNode_while_stmt)
  {
    emit_instr_str(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_eval));
    gen_load_rvalue(instr_list, ATTR(node, ast_node, cond_expr));
    emit_instr_str(instr_list, eOpcode_JUMPZ, ATTR(node, str_val, label_break));

    AstNode* body = ATTR(node, ast_node, body);
    if(body)
    {
      gen_instr(instr_list, body);
    }

    emit_instr_str(instr_list, eOpcode_GOTO, ATTR(node, str_val, label_eval));
    emit_instr_str(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_break));
  }
  else if(node->kind == eAstNode_bin_expr)
  {
    AstNode* left_operand = ATTR(node, ast_node, left_operand);
    AstNode* right_operand = ATTR(node, ast_node, right_operand);
    Type* type = ATTR(node, type, eval_type);
    Type* left_ty = ATTR(left_operand, type, eval_type);
    Type* right_ty = ATTR(right_operand, type, eval_type);
    eOperator bin_op = ATTR(node, op_kind, op_kind);

    if(bin_op == eOperator_assign)
    {
      gen_load_rvalue(instr_list, right_operand);
      gen_load_lvalue(instr_list, left_operand);

      emit_instr_int32(instr_list, eOpcode_STORE, type->width);
    }
    else if(bin_op == eOperator_add)
    {
      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
      {
        emit_instr(instr_list, eOpcode_ADD_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_instr(instr_list, eOpcode_ADD_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_sub)
    {
      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
      {
        emit_instr(instr_list, eOpcode_SUB_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_instr(instr_list, eOpcode_SUB_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_mul)
    {
      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
      {
        emit_instr(instr_list, eOpcode_MUL_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_instr(instr_list, eOpcode_MUL_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_div)
    {
      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
      {
        emit_instr(instr_list, eOpcode_DIV_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_instr(instr_list, eOpcode_DIV_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_mod)
    {
      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
      {
        emit_instr(instr_list, eOpcode_MOD_INT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_eq)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(instr_list, eOpcode_CMPEQ_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(instr_list, eOpcode_CMPEQ_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(instr_list, eOpcode_CMPEQ_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_not_eq)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(instr_list, eOpcode_CMPNEQ_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(instr_list, eOpcode_CMPNEQ_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(instr_list, eOpcode_CMPNEQ_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_less)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(instr_list, eOpcode_CMPLSS_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(instr_list, eOpcode_CMPLSS_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(instr_list, eOpcode_CMPLSS_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_greater)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(instr_list, eOpcode_CMPGRT_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(instr_list, eOpcode_CMPGRT_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(instr_list, eOpcode_CMPGRT_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_logic_and || bin_op == eOperator_logic_or)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(instr_list, left_operand);
      emit_instr(instr_list, eOpcode_DUP);

      if(bin_op == eOperator_logic_and)
      {
        emit_instr_str(instr_list, eOpcode_JUMPZ, ATTR(node, str_val, label_end));
      }
      else if(bin_op == eOperator_logic_or)
      {
        emit_instr_str(instr_list, eOpcode_JUMPNZ, ATTR(node, str_val, label_end));
      }
      else
        assert(0);

      emit_instr_int32(instr_list, eOpcode_GROW, -type->width);
      gen_load_rvalue(instr_list, right_operand);
      emit_instr(instr_list, eOpcode_DUP);

      if(bin_op == eOperator_logic_and)
      {
        emit_instr_str(instr_list, eOpcode_JUMPZ, ATTR(node, str_val, label_end));
      }
      else if(bin_op == eOperator_logic_or)
      {
        emit_instr_str(instr_list, eOpcode_JUMPNZ, ATTR(node, str_val, label_end));
      }
      else
        assert(0);

      emit_instr_str(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_end));
    }
    else if(bin_op == eOperator_less_eq || bin_op == eOperator_greater_eq)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(bin_op == eOperator_less_eq)
      {
        if(types_are_equal(left_ty, basic_type_char))
        {
          emit_instr(instr_list, eOpcode_CMPLSS_INT8);
        }
        else if(types_are_equal(left_ty, basic_type_int))
        {
          emit_instr(instr_list, eOpcode_CMPLSS_INT32);
        }
        else if(types_are_equal(left_ty, basic_type_float))
        {
          emit_instr(instr_list, eOpcode_CMPLSS_FLOAT32);
        }
        else
          assert(0);
      }
      else if(bin_op == eOperator_greater_eq)
      {
        if(types_are_equal(left_ty, basic_type_char))
        {
          emit_instr(instr_list, eOpcode_CMPGRT_INT8);
        }
        else if(types_are_equal(left_ty, basic_type_int))
        {
          emit_instr(instr_list, eOpcode_CMPGRT_INT32);
        }
        else if(types_are_equal(left_ty, basic_type_float))
        {
          emit_instr(instr_list, eOpcode_CMPGRT_FLOAT32);
        }
        else
          assert(0);
      }
      else
        assert(0);

      emit_instr(instr_list, eOpcode_DUP);
      emit_instr_str(instr_list, eOpcode_JUMPNZ, ATTR(node, str_val, label_end));
      emit_instr_int32(instr_list, eOpcode_GROW, -type->width);

      gen_load_rvalue(instr_list, left_operand);
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(instr_list, eOpcode_CMPEQ_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(instr_list, eOpcode_CMPEQ_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(instr_list, eOpcode_CMPEQ_FLOAT32);
      }
      else
        assert(0);

      emit_instr_str(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_end));
    }
    else if(bin_op == eOperator_cast)
    {
      gen_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_int) && types_are_equal(right_ty, basic_type_float))
      {
        emit_instr(instr_list, eOpcode_FLOAT32_TO_INT32);
      }
      if(types_are_equal(left_ty, basic_type_float) && types_are_equal(right_ty, basic_type_int))
      {
        emit_instr(instr_list, eOpcode_INT32_TO_FLOAT32);
      }
    }
    else if(bin_op == eOperator_index)
    {
      gen_load_rvalue(instr_list, node);
    }
    else
      assert(0);
  }
  else if(node->kind == eAstNode_un_expr)
  {
    AstNode* operand = ATTR(node, ast_node, operand);
    Type* operand_ty = ATTR(operand, type, eval_type);
    eOperator un_op = ATTR(node, op_kind, op_kind);

    if(un_op == eOperator_address_of)
    {
      gen_load_rvalue(instr_list, node);
    }
    else if(un_op == eOperator_neg)
    {
      gen_load_rvalue(instr_list, operand);

      if(types_are_equal(operand_ty, basic_type_int))
      {
        emit_instr(instr_list, eOpcode_NEG_INT32);
      }
      else if(types_are_equal(operand_ty, basic_type_float))
      {
        emit_instr(instr_list, eOpcode_NEG_FLOAT32);
      }
      else
        assert(0);
    }
    else if(un_op == eOperator_logic_not)
    {
      gen_load_rvalue(instr_list, operand);
      emit_instr_int32(instr_list, eOpcode_PUSH_INT32, 0);
      emit_instr(instr_list, eOpcode_CMPEQ_INT32);
    }
    else if(un_op == eOperator_deref)
    {
      gen_load_rvalue(instr_list, node);
    }
    else
      assert(0);
  }
  else if(node->kind == eAstNode_proc_occur)
  {
    AstNode* callee_decl = ATTR(node, ast_node, proc_decl);
    Scope* callee_scope = ATTR(callee_decl, scope, scope);
    DataArea* ret_area = &callee_scope->ret_area;
    DataArea* args_area = &callee_scope->args_area;
    DataArea* link_area = &callee_scope->link_area;

    emit_instr_int32(instr_list, eOpcode_GROW, ret_area->size);

    for(ListItem* list_item = ATTR(node, list, actual_args)->first;
        list_item;
        list_item = list_item->next)
    {
      gen_load_rvalue(instr_list, ITEM(list_item, ast_node));
    }
    // TODO: Assert that the 'data area size' == 'evaluated args size'

    // Setup the Access Link
    Symbol* occur_sym = ATTR(node, symbol, occur_sym);
    Scope* caller_scope = occur_sym->scope;
    int callee_depth_offset = caller_scope->nesting_depth - callee_scope->nesting_depth;
    if(callee_depth_offset < 0)
    {
      emit_instr_reg(instr_list, eOpcode_PUSH_REG, eRegName_FP);
      emit_instr_int32(instr_list, eOpcode_PUSH_INT32, link_area->loc);
      emit_instr(instr_list, eOpcode_ADD_INT32);
    }
    else
    {
      emit_instr_reg(instr_list, eOpcode_PUSH_REG, eRegName_FP);
      emit_instr_int32(instr_list, eOpcode_PUSH_INT32, link_area->loc);
      emit_instr(instr_list, eOpcode_ADD_INT32);

      for(int i = callee_depth_offset; i >= 0; i--)
      {
        emit_instr_int32(instr_list, eOpcode_LOAD, link_area->size);
      }
    }

    emit_instr_str(instr_list, eOpcode_CALL, ATTR(node, str_val, name));

    emit_instr_int32(instr_list, eOpcode_GROW, -args_area->size);
    emit_instr_int32(instr_list, eOpcode_GROW, -link_area->size);
  }
  else if(node->kind == eAstNode_if_stmt)
  {
    gen_load_rvalue(instr_list, ATTR(node, ast_node, cond_expr));

    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      emit_instr_str(instr_list, eOpcode_JUMPZ, ATTR(node, str_val, label_else));
    }
    else
    {
      emit_instr_str(instr_list, eOpcode_JUMPZ, ATTR(node, str_val, label_end));
    }

    gen_instr(instr_list, ATTR(node, ast_node, body));
    emit_instr_str(instr_list, eOpcode_GOTO, ATTR(node, str_val, label_end));

    if(else_body)
    {
      emit_instr_str(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_else));
      gen_instr(instr_list, else_body);
    }

    emit_instr_str(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_end));
  }
  else if(node->kind == eAstNode_while_stmt)
  {
    emit_instr_str(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_eval));
    gen_load_rvalue(instr_list, ATTR(node, ast_node, cond_expr));
    emit_instr_str(instr_list, eOpcode_JUMPZ, ATTR(node, str_val, label_break));

    AstNode* body = ATTR(node, ast_node, body);
    if(body)
    {
      gen_instr(instr_list, body);
    }

    emit_instr_str(instr_list, eOpcode_GOTO, ATTR(node, str_val, label_eval));
    emit_instr_str(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_break));
  }
  else if(node->kind == eAstNode_break_stmt)
  {
    AstNode* loop = ATTR(node, ast_node, loop);

    int depth = ATTR(node, int_val, nesting_depth);
    while(depth-- > 0)
    {
      emit_instr(instr_list, eOpcode_LEAVE);
    }
    emit_instr_str(instr_list, eOpcode_GOTO, ATTR(loop, str_val, label_break));
  }
  else if(node->kind == eAstNode_empty)
  {
    ;//skip
  }
  else if(node->kind == eAstNode_asm_block)
  {
    TargetCode target_code = {0};
    target_code.text = ATTR(node, str_val, asm_text);

    if(success = convert_hasm_to_instructions(&target_code))
    {
      for(int i = 0; i < target_code.instr_count; i++)
      {
        append_list_elem(instr_list, &target_code.instructions[i], eList_vm_instr);
      }
    }
  }
  else
    assert(0);

  return success;
}

void gen_labels(AstNode* node)
{
  if(node->kind == eAstNode_module)
  {
    gen_labels(ATTR(node, ast_node, body));
  }
  else if(node->kind == eAstNode_block)
  {
    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item;
        list_item = list_item->next)
    {
      gen_labels(ITEM(list_item, ast_node));
    }
  }
  else if(node->kind == eAstNode_proc_decl)
  {
    Scope* proc_scope = ATTR(node, scope, scope);
    Scope* encl_proc_scope = find_scope(proc_scope->encl_scope, eScope_proc);
    if(encl_proc_scope)
    {
      AstNode* encl_proc = encl_proc_scope->ast_node; assert(encl_proc->kind == eAstNode_proc_decl);
      String qual_name; str_init(&qual_name, arena);
      str_append(&qual_name, ATTR(encl_proc, str_val, name));
      str_append(&qual_name, ".");
      str_append(&qual_name, ATTR(node, str_val, name));
      ATTR(node, str_val, name) = str_cap(&qual_name);
    }
    gen_labels(ATTR(node, ast_node, body));
  }
  else if(node->kind == eAstNode_proc_occur)
  {
    for(ListItem* list_item = ATTR(node, list, actual_args)->first;
        list_item;
        list_item = list_item->next)
    {
      gen_labels(ITEM(list_item, ast_node));
    }

    AstNode* proc_decl = ATTR(node, ast_node, proc_decl);
    ATTR(node, str_val, name) = ATTR(proc_decl, str_val, name);
  }
  else if(node->kind == eAstNode_stmt)
  {
    gen_labels(ATTR(node, ast_node, stmt));
  }
  else if(node->kind == eAstNode_bin_expr)
  {
    char* label_id = make_unique_label();

    String label; str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$logic_end");
    ATTR(node, str_val, label_end) = str_cap(&label);

    gen_labels(ATTR(node, ast_node, left_operand));
    gen_labels(ATTR(node, ast_node, right_operand));
  }
  else if(node->kind == eAstNode_un_expr)
  {
    gen_labels(ATTR(node, ast_node, operand));
  }
  else if(node->kind == eAstNode_if_stmt)
  {
    gen_labels(ATTR(node, ast_node, cond_expr));

    char* label_id = make_unique_label();

    String label; str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$if_else");
    ATTR(node, str_val, label_else) = str_cap(&label);

    str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$if_end");
    ATTR(node, str_val, label_end) = str_cap(&label);

    gen_labels(ATTR(node, ast_node, body));

    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      gen_labels(else_body);
    }
  }
  else if(node->kind == eAstNode_while_stmt)
  {
    gen_labels(ATTR(node, ast_node, cond_expr));

    char* label_id = make_unique_label();

    String label; str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$while_eval");
    ATTR(node, str_val, label_eval) = str_cap(&label);

    str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$while_break");
    ATTR(node, str_val, label_break) = str_cap(&label);

    AstNode* body = ATTR(node, ast_node, body);
    if(body)
    {
      gen_labels(body);
    }
  }
  else if(node->kind == eAstNode_ret_stmt)
  {
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    if(ret_expr)
    {
      gen_labels(ret_expr);
    }
  }
  else
    ;//skip
}

void copy_program_data(TargetCode* target_code, int fp, List* areas_list)
{
  for(ListItem* list_item = areas_list->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* area = ITEM(list_item, data_area);
    if(area->subareas)
    {
      assert(!area->data);
      copy_program_data(target_code, fp, area->subareas);
    }
    else
    {
      if(area->data)
      {
        uint8* p_data = (uint8*)area->data;
        for(int i = 0; i < area->size; i++)
        {
          target_code->data[i + fp + area->loc] = p_data[i];
        }
      }
    }
  }
}

bool gen_program(TargetCode* target_code, AstNode* module)
{
  assert(module->kind == eAstNode_module);
  bool success = true;

  gen_labels(module);

  if(success = gen_instr(target_code->instr_list, module))
  {
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
    DataArea* link_area = &scope->link_area;
    target_code->sp = fp - ctrl_area->size - link_area->size;

    for(ListItem* list_item = scope->post_fp_areas->first;
        list_item;
        list_item = list_item->next)
    {
      data_size += ITEM(list_item, data_area)->size;
    }

    target_code->data = mem_push_array(arena, uint8, data_size);
    target_code->data_size = data_size;

    copy_program_data(target_code, fp, scope->pre_fp_areas);
    copy_program_data(target_code, fp, scope->post_fp_areas);
  }
  return success;
}

char* get_regname_str(eRegName reg)
{
  static char* reg_fp = "fp";
  static char* reg_sp = "sp";
  static char* reg_ip = "ip";
  char* regname = 0;

  if(reg == eRegName_FP)
    regname = reg_fp;
  else if(reg == eRegName_SP)
    regname = reg_sp;
  else if(reg == eRegName_IP)
    regname = reg_ip;
  else
    assert(0);
  return regname;
}

void print_code(TargetCode* target_code)
{
  String text; str_init(&text, arena);
  int text_len = 0;

  for(ListItem* list_item = target_code->instr_list->first;
      list_item;
      list_item = list_item->next)
  {
    Instruction* instr = (Instruction*)list_item->elem;
    switch(instr->opcode)
    {
      case eOpcode_PUSH_INT8:
        {
          if(instr->param_type == eParamType_int8)
          {
            text_len += print_instruction(&text, "push_int8 %d", instr->param.int_val);
          }
          else
            assert(0);
        } break;

      case eOpcode_PUSH_INT32:
        {
          if(instr->param_type == eParamType_int32)
          {
            text_len += print_instruction(&text, "push_int32 %d", instr->param.int_val);
          }
          else
            assert(0);
        } break;

      case eOpcode_PUSH_REG:
        {
          if(instr->param_type == eParamType_reg)
          {
            text_len += print_instruction(&text, "push_reg %s", get_regname_str(instr->param.reg));
          }
          else
            assert(0);
        } break;

      case eOpcode_PUSH_FLOAT32:
        {
          if(instr->param_type == eParamType_float32)
          {
            text_len += print_instruction(&text, "push_float32 %f", instr->param.float_val);
          }
          else
            assert(0);
        } break;

      case eOpcode_POP_REG:
        {
          if(instr->param_type == eParamType_reg)
          {
            text_len += print_instruction(&text, "pop_reg %s", get_regname_str(instr->param.reg));
          }
          else
            assert(0);
        }

      case eOpcode_DUP:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "dup");
        } break;

      case eOpcode_ADD_INT32:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "add_int32");
        } break;

      case eOpcode_SUB_INT32:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "sub_int32");
        } break;

      case eOpcode_MUL_INT32:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "mul_int32");
        } break;

      case eOpcode_DIV_INT32:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "div_int32");
        } break;

      case eOpcode_ADD_FLOAT32:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "add_float32");
        } break;

      case eOpcode_SUB_FLOAT32:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "sub_float32");
        } break;

      case eOpcode_MUL_FLOAT32:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "mul_float32");
        } break;

      case eOpcode_DIV_FLOAT32:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "div_float32");
        } break;

      case eOpcode_NEG_FLOAT32:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "neg_float32");
        } break;

      case eOpcode_MOD_INT32:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "mod_int32");
        } break;

      case eOpcode_NEG_INT32:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "neg_int32");
        } break;

      case eOpcode_LOAD:
        {
          assert(instr->param_type == eParamType_int32);
          text_len += print_instruction(&text, "load %d", instr->param.int_val);
        } break;

      case eOpcode_STORE:
        {
          assert(instr->param_type == eParamType_int32);
          text_len += print_instruction(&text, "store %d", instr->param.int_val);
        } break;

      case eOpcode_LABEL:
        {
          assert(instr->param_type == eParamType_str);
          text_len += print_instruction(&text, "label %s", instr->param.str);
        } break;

      case eOpcode_RETURN:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "return");
        } break;

      case eOpcode_GROW:
        {
          assert(instr->param_type == eParamType_int32);
          text_len += print_instruction(&text, "grow %d", instr->param.int_val);
        } break;

      case eOpcode_GROWNZ:
        {
          assert(instr->param_type == eParamType_int32);
          text_len += print_instruction(&text, "grownz %d", instr->param.int_val);
        } break;

      case eOpcode_NEW:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "new");
        } break;

      case eOpcode_CALL:
        {
          assert(instr->param_type == eParamType_str);
          text_len += print_instruction(&text, "call %s", instr->param.str);
        } break;

      case eOpcode_HALT:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "halt");
        } break;

      case eOpcode_GOTO:
        {
          assert(instr->param_type == eParamType_str);
          text_len += print_instruction(&text, "goto %s", instr->param.str);
        } break;

      case eOpcode_JUMPZ:
        {
          assert(instr->param_type == eParamType_str);
          text_len += print_instruction(&text, "jumpz %s", instr->param.str);
        } break;

      case eOpcode_JUMPNZ:
        {
          assert(instr->param_type == eParamType_str);
          text_len += print_instruction(&text, "jumpnz %s", instr->param.str);
        } break;

      case eOpcode_ENTER:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "enter");
        } break;

      case eOpcode_LEAVE:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "leave");
        } break;

      case eOpcode_NOOP:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "noop");
        } break;

      case eOpcode_CMPEQ_INT8:
      case eOpcode_CMPNEQ_INT8:
      case eOpcode_CMPLSS_INT8:
      case eOpcode_CMPGRT_INT8:
      case eOpcode_CMPEQ_INT32:
      case eOpcode_CMPNEQ_INT32:
      case eOpcode_CMPLSS_INT32:
      case eOpcode_CMPGRT_INT32:
      case eOpcode_CMPEQ_FLOAT32:
      case eOpcode_CMPNEQ_FLOAT32:
      case eOpcode_CMPLSS_FLOAT32:
      case eOpcode_CMPGRT_FLOAT32:
        {
          assert(instr->param_type == eParamType_None);
          if(instr->opcode == eOpcode_CMPEQ_INT8)
          {
            text_len += print_instruction(&text, "cmpeq_int8");
          }
          else if(instr->opcode == eOpcode_CMPNEQ_INT8)
          {
            text_len += print_instruction(&text, "cmpneq_int8");
          }
          else if(instr->opcode == eOpcode_CMPLSS_INT8)
          {
            text_len += print_instruction(&text, "cmplss_int8");
          }
          else if(instr->opcode == eOpcode_CMPGRT_INT8)
          {
            text_len += print_instruction(&text, "cmpgrt_int8");
          }
          else if(instr->opcode == eOpcode_CMPEQ_INT32)
          {
            text_len += print_instruction(&text, "cmpeq_int32");
          }
          else if(instr->opcode == eOpcode_CMPNEQ_INT32)
          {
            text_len += print_instruction(&text, "cmpneq_int32");
          }
          else if(instr->opcode == eOpcode_CMPLSS_INT32)
          {
            text_len += print_instruction(&text, "cmplss_int32");
          }
          else if(instr->opcode == eOpcode_CMPGRT_INT32)
          {
            text_len += print_instruction(&text, "cmpgrt_int32");
          }
          else if(instr->opcode == eOpcode_CMPEQ_FLOAT32)
          {
            text_len += print_instruction(&text, "cmpeq_float32");
          }
          else if(instr->opcode == eOpcode_CMPNEQ_FLOAT32)
          {
            text_len += print_instruction(&text, "cmpneq_float32");
          }
          else if(instr->opcode == eOpcode_CMPLSS_FLOAT32)
          {
            text_len += print_instruction(&text, "cmplss_float32");
          }
          else if(instr->opcode == eOpcode_CMPGRT_FLOAT32)
          {
            text_len += print_instruction(&text, "cmpgrt_float32");
          }
          else
            assert(0);
        } break;

      case eOpcode_AND:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "and");
        } break;

      case eOpcode_OR:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "or");
        } break;

      case eOpcode_NOT:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "not");
        } break;

      case eOpcode_PUTC:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "putc");
        } break;

      case eOpcode_FLOAT32_TO_INT32:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "float32_to_int32");
        } break;

      case eOpcode_INT32_TO_FLOAT32:
        {
          assert(instr->param_type == eParamType_None);
          text_len += print_instruction(&text, "int32_to_float32");
        } break;

      default:
        assert(0);
    }
  }

  target_code->text = str_cap(&text);
  target_code->text_len = text_len;
}

