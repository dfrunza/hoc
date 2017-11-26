void gen_ir_load_rvalue(List* instr_list, AstNode* node);
bool gen_ir(List* instr_list, AstNode* node);

bool is_valid_label(char *label)
{
  char start_char = *label;
  return ('A' <= start_char && start_char <= 'Z') || ('a' <= start_char && start_char <= 'z') || start_char == '.';
}

void emit_ir_instr(List* instr_list, eOpcode opcode, ...)
{
  va_list args;
  va_start(args, opcode);

  IrInstruction* instr = mem_push_struct(arena, IrInstruction);
  append_list_elem(instr_list, instr, eList_ir_instr);
  instr->opcode = opcode;
  IrInstrParam* param = &instr->param;
  switch(opcode)
  {
    case eOpcode_PUSH_REG:
      {
        param->kind = eIrInstrParam_reg;
        param->reg = va_arg(args, eRegName);
      }
      break;
    case eOpcode_PUSH_INT8:
      {
        param->kind = eIrInstrParam_int8;
        param->int8_val = (int8)va_arg(args, int);
      }
      break;
    case eOpcode_PUSH_INT32:
    case eOpcode_GROW:
    case eOpcode_LOAD:
    case eOpcode_STORE:
    case eOpcode_ACLINK:
      {
        param->kind = eIrInstrParam_int32;
        param->int32_val = (int32)va_arg(args, int);
      }
      break;
    case eOpcode_PUSH_FLOAT32:
      {
        param->kind = eIrInstrParam_float32;
        param->float32_val = (float32)va_arg(args, double);
      }
      break;
    case eOpcode_LABEL:
    case eOpcode_CALL:
    case eOpcode_JUMPZ:
    case eOpcode_JUMPNZ:
    case eOpcode_GOTO:
      {
        param->kind = eIrInstrParam_id;
        param->id = va_arg(args, char*);
      }
      break;
    case eOpcode_ADD_INT32:
    case eOpcode_MUL_INT32:
    case eOpcode_SUB_INT32:
    case eOpcode_DIV_INT32:
    case eOpcode_MOD_INT32:
    case eOpcode_ADD_FLOAT32:
    case eOpcode_MUL_FLOAT32:
    case eOpcode_SUB_FLOAT32:
    case eOpcode_DIV_FLOAT32:
    case eOpcode_ENTER:
    case eOpcode_LEAVE:
    case eOpcode_HALT:
    case eOpcode_RETURN:
    case eOpcode_CMPEQ_INT8:
    case eOpcode_CMPEQ_INT32:
    case eOpcode_CMPEQ_FLOAT32:
    case eOpcode_CMPNEQ_INT8:
    case eOpcode_CMPNEQ_INT32:
    case eOpcode_CMPNEQ_FLOAT32:
    case eOpcode_CMPGRT_INT8:
    case eOpcode_CMPGRT_INT32:
    case eOpcode_CMPGRT_FLOAT32:
    case eOpcode_CMPLSS_INT8:
    case eOpcode_CMPLSS_INT32:
    case eOpcode_CMPLSS_FLOAT32:
    case eOpcode_DUP:
    case eOpcode_INT32_TO_FLOAT32:
    case eOpcode_FLOAT32_TO_INT32:
    case eOpcode_NEG_INT32:
    case eOpcode_NEG_FLOAT32:
      {
        ; //skip
      }
      break;
    default:
      assert(0);
  }
  va_end(args);
}

void make_ir_labels(AstNode* node)
{
  if(node->kind == eAstNode_module)
  {
    make_ir_labels(ATTR(node, ast_node, body));
  }
  else if(node->kind == eAstNode_block)
  {
    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item;
        list_item = list_item->next)
    {
      make_ir_labels(ITEM(list_item, ast_node));
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
      str_append(&qual_name, "$");
      str_append(&qual_name, ATTR(node, str_val, name));
      ATTR(node, str_val, name) = str_cap(&qual_name);
    }
    make_ir_labels(ATTR(node, ast_node, body));
  }
  else if(node->kind == eAstNode_proc_occur)
  {
    for(ListItem* list_item = ATTR(node, list, actual_args)->first;
        list_item;
        list_item = list_item->next)
    {
      make_ir_labels(ITEM(list_item, ast_node));
    }

    AstNode* proc_decl = ATTR(node, ast_node, proc_decl);
    ATTR(node, str_val, name) = ATTR(proc_decl, str_val, name);
  }
  else if(node->kind == eAstNode_stmt)
  {
    make_ir_labels(ATTR(node, ast_node, stmt));
  }
  else if(node->kind == eAstNode_bin_expr)
  {
    char* label_id = make_unique_label();

    String label; str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$logic_end");
    ATTR(node, str_val, label_end) = str_cap(&label);

    make_ir_labels(ATTR(node, ast_node, left_operand));
    make_ir_labels(ATTR(node, ast_node, right_operand));
  }
  else if(node->kind == eAstNode_un_expr)
  {
    make_ir_labels(ATTR(node, ast_node, operand));
  }
  else if(node->kind == eAstNode_if_stmt)
  {
    make_ir_labels(ATTR(node, ast_node, cond_expr));

    char* label_id = make_unique_label();

    String label; str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$if_else");
    ATTR(node, str_val, label_else) = str_cap(&label);

    str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$if_end");
    ATTR(node, str_val, label_end) = str_cap(&label);

    make_ir_labels(ATTR(node, ast_node, body));

    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      make_ir_labels(else_body);
    }
  }
  else if(node->kind == eAstNode_while_stmt)
  {
    make_ir_labels(ATTR(node, ast_node, cond_expr));

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
      make_ir_labels(body);
    }
  }
  else if(node->kind == eAstNode_ret_stmt)
  {
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    if(ret_expr)
    {
      make_ir_labels(ret_expr);
    }
  }
  else
    ;//skip
}

void gen_ir_load_lvalue(List* instr_list, AstNode* node)
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
      emit_ir_instr(instr_list, eOpcode_ACLINK, decl_scope_offset);
#if 0
      emit_ir_instr(instr_list, eOpcode_PUSH_REG, eRegName_FP);
      emit_ir_instr(instr_list, eOpcode_PUSH_INT32, link->loc);
      emit_ir_instr(instr_list, eOpcode_ADD_INT32);

      for(int i = decl_scope_offset; i > 0; i--)
      {
        emit_ir_instr(instr_list, eOpcode_LOAD, link->size);
      }
#endif

      // The FP is offset relative to the Access Link
      emit_ir_instr(instr_list, eOpcode_PUSH_INT32, ctrl_area->size + link->size);
      emit_ir_instr(instr_list, eOpcode_ADD_INT32);
    }
    else
    {
      emit_ir_instr(instr_list, eOpcode_PUSH_REG, eRegName_FP);
    }

    emit_ir_instr(instr_list, eOpcode_PUSH_INT32, data->loc);
    emit_ir_instr(instr_list, eOpcode_ADD_INT32);
  }
  else if(node->kind == eAstNode_un_expr)
  {
    AstNode* operand = ATTR(node, ast_node, operand);
    eOperator un_op = ATTR(node, op_kind, op_kind);

    if(un_op == eOperator_deref)
    {
      assert(ATTR(operand, type, eval_type)->kind == eType_pointer);
      gen_ir_load_rvalue(instr_list, operand);
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
      gen_ir_load_lvalue(instr_list, left_operand);
      gen_ir_load_rvalue(instr_list, right_operand);
      emit_ir_instr(instr_list, eOpcode_PUSH_INT32, type->width);
      emit_ir_instr(instr_list, eOpcode_MUL_INT32);
      emit_ir_instr(instr_list, eOpcode_ADD_INT32);
    }
    else
      assert(0);
  }
  else
    assert(0);
}

void gen_ir_load_rvalue(List* instr_list, AstNode* node)
{
  assert(node->gen == eAstGen_gen1);

  if(node->kind == eAstNode_var_occur)
  {
    Type* type = ATTR(node, type, eval_type);
    gen_ir_load_lvalue(instr_list, node);
    emit_ir_instr(instr_list, eOpcode_LOAD, type->width);
  }
  else if(node->kind == eAstNode_proc_occur)
  {
    gen_ir(instr_list, node);
  }
  else if(node->kind == eAstNode_lit)
  {
    eLiteral kind = ATTR(node, lit_kind, lit_kind);
    if(kind == eLiteral_int_val)
    {
      emit_ir_instr(instr_list, eOpcode_PUSH_INT32, ATTR(node, int_val, int_val));
    }
    else if(kind == eLiteral_bool_val)
    {
      emit_ir_instr(instr_list, eOpcode_PUSH_INT32, ATTR(node, bool_val, bool_val));
    }
    else if(kind == eLiteral_float_val)
    {
      emit_ir_instr(instr_list, eOpcode_PUSH_FLOAT32, ATTR(node, float_val, float_val));
    }
    else if(kind == eLiteral_char_val)
    {
      emit_ir_instr(instr_list, eOpcode_PUSH_INT8, ATTR(node, char_val, char_val));
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
      gen_ir_load_lvalue(instr_list, node);
      emit_ir_instr(instr_list, eOpcode_LOAD, type->width);
    }
    else
    {
      gen_ir(instr_list, node);
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
        gen_ir_load_lvalue(instr_list, operand);
      }
      else
      {
        gen_ir_load_rvalue(instr_list, operand);
      }
    }
    else if(un_op == eOperator_deref)
    {
      gen_ir_load_lvalue(instr_list, node);
      emit_ir_instr(instr_list, eOpcode_LOAD, type->width);
    }
    else
    {
      gen_ir(instr_list, node);
    }
  }
  else
    assert(0);
}

bool gen_ir(List* instr_list, AstNode* node)
{
  assert(node->gen == eAstGen_gen1);
  bool success = true;

  if(node->kind == eAstNode_module)
  {
    AstNode* body = ATTR(node, ast_node, body);
    Scope* scope = ATTR(body, scope, scope);
    DataArea* locals_area = &scope->locals_area;

    emit_ir_instr(instr_list, eOpcode_PUSH_REG, eRegName_FP);

    emit_ir_instr(instr_list, eOpcode_GROW, 4); // dummy IP
    emit_ir_instr(instr_list, eOpcode_ENTER);
    emit_ir_instr(instr_list, eOpcode_GROW, locals_area->size);

    for(ListItem* list_item = ATTR(body, list, stmts)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* stmt = ITEM(list_item, ast_node); assert(stmt->kind == eAstNode_stmt);
      gen_ir(instr_list, stmt);
    }

    emit_ir_instr(instr_list, eOpcode_LEAVE);
    emit_ir_instr(instr_list, eOpcode_GROW, -4); // dummy IP
    emit_ir_instr(instr_list, eOpcode_HALT);

    for(ListItem* list_item = ATTR(body, list, procs)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* proc = ITEM(list_item, ast_node);
      assert(proc->kind == eAstNode_proc_decl);
      gen_ir(instr_list, proc);
    }
  }
  else if(node->kind == eAstNode_proc_decl)
  {
    Scope* scope = ATTR(node, scope, scope);
    DataArea* locals_area = &scope->locals_area;

    emit_ir_instr(instr_list, eOpcode_LABEL, ATTR(node, str_val, name));
    emit_ir_instr(instr_list, eOpcode_ENTER);
    emit_ir_instr(instr_list, eOpcode_GROW, locals_area->size);

    AstNode* body = ATTR(node, ast_node, body);
    for(ListItem* list_item = ATTR(body, list, stmts)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* stmt = ITEM(list_item, ast_node);
      gen_ir(instr_list, stmt);
    }

    emit_ir_instr(instr_list, eOpcode_LEAVE);
    emit_ir_instr(instr_list, eOpcode_RETURN);

    for(ListItem* list_item = ATTR(body, list, procs)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* proc = ITEM(list_item, ast_node); assert(proc->kind == eAstNode_proc_decl);
      gen_ir(instr_list, proc);
    }
  }
  else if(node->kind == eAstNode_block)
  {
    Scope* scope = ATTR(node, scope, scope);
    DataArea* link_area = &scope->link_area;
    DataArea* locals_area = &scope->locals_area;

    // Setup the Access Link
    emit_ir_instr(instr_list, eOpcode_PUSH_REG, eRegName_FP);
    emit_ir_instr(instr_list, eOpcode_PUSH_INT32, link_area->loc);
    emit_ir_instr(instr_list, eOpcode_ADD_INT32);

    emit_ir_instr(instr_list, eOpcode_GROW, 4); // dummy IP
    emit_ir_instr(instr_list, eOpcode_ENTER);
    emit_ir_instr(instr_list, eOpcode_GROW, locals_area->size);

    for(ListItem* list_item = ATTR(node, list, stmts)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* stmt = ITEM(list_item, ast_node);
      gen_ir(instr_list, stmt);
    }

    emit_ir_instr(instr_list, eOpcode_LEAVE);
    emit_ir_instr(instr_list, eOpcode_GROW, -4); // dummy IP
    emit_ir_instr(instr_list, eOpcode_GROW, -link_area->size);

    for(ListItem* list_item = ATTR(node, list, procs)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* proc = ITEM(list_item, ast_node); assert(proc->kind == eAstNode_proc_decl);
      gen_ir(instr_list, proc);
    }
  }
  else if(node->kind == eAstNode_ret_stmt)
  {
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    if(ret_expr)
    {
      gen_ir(instr_list, ret_expr);
    }

    int depth = ATTR(node, int_val, nesting_depth);
    while(depth-- > 0)
    {
      emit_ir_instr(instr_list, eOpcode_LEAVE);
    }

    emit_ir_instr(instr_list, eOpcode_RETURN);
  }
  else if(node->kind == eAstNode_stmt)
  {
    AstNode* actual_stmt = ATTR(node, ast_node, stmt);
    Type* type = ATTR(actual_stmt, type, eval_type);
    gen_ir(instr_list, actual_stmt);
    emit_ir_instr(instr_list, eOpcode_GROW, -type->width);
  }
  else if(node->kind == eAstNode_while_stmt)
  {
    emit_ir_instr(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_eval));
    gen_ir_load_rvalue(instr_list, ATTR(node, ast_node, cond_expr));
    emit_ir_instr(instr_list, eOpcode_JUMPZ, ATTR(node, str_val, label_break));

    AstNode* body = ATTR(node, ast_node, body);
    if(body)
    {
      gen_ir(instr_list, body);
    }

    emit_ir_instr(instr_list, eOpcode_GOTO, ATTR(node, str_val, label_eval));
    emit_ir_instr(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_break));
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
      gen_ir_load_rvalue(instr_list, right_operand);
      gen_ir_load_lvalue(instr_list, left_operand);

      emit_ir_instr(instr_list, eOpcode_STORE, type->width);
    }
    else if(bin_op == eOperator_add)
    {
      gen_ir_load_rvalue(instr_list, left_operand);
      gen_ir_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
      {
        emit_ir_instr(instr_list, eOpcode_ADD_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_ir_instr(instr_list, eOpcode_ADD_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_sub)
    {
      gen_ir_load_rvalue(instr_list, left_operand);
      gen_ir_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
      {
        emit_ir_instr(instr_list, eOpcode_SUB_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_ir_instr(instr_list, eOpcode_SUB_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_mul)
    {
      gen_ir_load_rvalue(instr_list, left_operand);
      gen_ir_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
      {
        emit_ir_instr(instr_list, eOpcode_MUL_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_ir_instr(instr_list, eOpcode_MUL_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_div)
    {
      gen_ir_load_rvalue(instr_list, left_operand);
      gen_ir_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
      {
        emit_ir_instr(instr_list, eOpcode_DIV_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_ir_instr(instr_list, eOpcode_DIV_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_mod)
    {
      gen_ir_load_rvalue(instr_list, left_operand);
      gen_ir_load_rvalue(instr_list, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
      {
        emit_ir_instr(instr_list, eOpcode_MOD_INT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_eq)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_ir_load_rvalue(instr_list, left_operand);
      gen_ir_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_ir_instr(instr_list, eOpcode_CMPEQ_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_ir_instr(instr_list, eOpcode_CMPEQ_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_ir_instr(instr_list, eOpcode_CMPEQ_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_not_eq)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_ir_load_rvalue(instr_list, left_operand);
      gen_ir_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_ir_instr(instr_list, eOpcode_CMPNEQ_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_ir_instr(instr_list, eOpcode_CMPNEQ_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_ir_instr(instr_list, eOpcode_CMPNEQ_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_less)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_ir_load_rvalue(instr_list, left_operand);
      gen_ir_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_ir_instr(instr_list, eOpcode_CMPLSS_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_ir_instr(instr_list, eOpcode_CMPLSS_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_ir_instr(instr_list, eOpcode_CMPLSS_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_greater)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_ir_load_rvalue(instr_list, left_operand);
      gen_ir_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_ir_instr(instr_list, eOpcode_CMPGRT_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_ir_instr(instr_list, eOpcode_CMPGRT_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_ir_instr(instr_list, eOpcode_CMPGRT_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == eOperator_logic_and || bin_op == eOperator_logic_or)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_ir_load_rvalue(instr_list, left_operand);
      emit_ir_instr(instr_list, eOpcode_DUP);

      if(bin_op == eOperator_logic_and)
      {
        emit_ir_instr(instr_list, eOpcode_JUMPZ, ATTR(node, str_val, label_end));
      }
      else if(bin_op == eOperator_logic_or)
      {
        emit_ir_instr(instr_list, eOpcode_JUMPNZ, ATTR(node, str_val, label_end));
      }
      else
        assert(0);

      emit_ir_instr(instr_list, eOpcode_GROW, -type->width);
      gen_ir_load_rvalue(instr_list, right_operand);
      emit_ir_instr(instr_list, eOpcode_DUP);

      if(bin_op == eOperator_logic_and)
      {
        emit_ir_instr(instr_list, eOpcode_JUMPZ, ATTR(node, str_val, label_end));
      }
      else if(bin_op == eOperator_logic_or)
      {
        emit_ir_instr(instr_list, eOpcode_JUMPNZ, ATTR(node, str_val, label_end));
      }
      else
        assert(0);

      emit_ir_instr(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_end));
    }
    else if(bin_op == eOperator_less_eq || bin_op == eOperator_greater_eq)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_ir_load_rvalue(instr_list, left_operand);
      gen_ir_load_rvalue(instr_list, right_operand);

      if(bin_op == eOperator_less_eq)
      {
        if(types_are_equal(left_ty, basic_type_char))
        {
          emit_ir_instr(instr_list, eOpcode_CMPLSS_INT8);
        }
        else if(types_are_equal(left_ty, basic_type_int))
        {
          emit_ir_instr(instr_list, eOpcode_CMPLSS_INT32);
        }
        else if(types_are_equal(left_ty, basic_type_float))
        {
          emit_ir_instr(instr_list, eOpcode_CMPLSS_FLOAT32);
        }
        else
          assert(0);
      }
      else if(bin_op == eOperator_greater_eq)
      {
        if(types_are_equal(left_ty, basic_type_char))
        {
          emit_ir_instr(instr_list, eOpcode_CMPGRT_INT8);
        }
        else if(types_are_equal(left_ty, basic_type_int))
        {
          emit_ir_instr(instr_list, eOpcode_CMPGRT_INT32);
        }
        else if(types_are_equal(left_ty, basic_type_float))
        {
          emit_ir_instr(instr_list, eOpcode_CMPGRT_FLOAT32);
        }
        else
          assert(0);
      }
      else
        assert(0);

      emit_ir_instr(instr_list, eOpcode_DUP);
      emit_ir_instr(instr_list, eOpcode_JUMPNZ, ATTR(node, str_val, label_end));
      emit_ir_instr(instr_list, eOpcode_GROW, -type->width);

      gen_ir_load_rvalue(instr_list, left_operand);
      gen_ir_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_ir_instr(instr_list, eOpcode_CMPEQ_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_ir_instr(instr_list, eOpcode_CMPEQ_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_ir_instr(instr_list, eOpcode_CMPEQ_FLOAT32);
      }
      else
        assert(0);

      emit_ir_instr(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_end));
    }
    else if(bin_op == eOperator_cast)
    {
      gen_ir_load_rvalue(instr_list, right_operand);

      if(types_are_equal(left_ty, basic_type_int) && types_are_equal(right_ty, basic_type_float))
      {
        emit_ir_instr(instr_list, eOpcode_FLOAT32_TO_INT32);
      }
      if(types_are_equal(left_ty, basic_type_float) && types_are_equal(right_ty, basic_type_int))
      {
        emit_ir_instr(instr_list, eOpcode_INT32_TO_FLOAT32);
      }
    }
    else if(bin_op == eOperator_index)
    {
      gen_ir_load_rvalue(instr_list, node);
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
      gen_ir_load_rvalue(instr_list, node);
    }
    else if(un_op == eOperator_neg)
    {
      gen_ir_load_rvalue(instr_list, operand);

      if(types_are_equal(operand_ty, basic_type_int))
      {
        emit_ir_instr(instr_list, eOpcode_NEG_INT32);
      }
      else if(types_are_equal(operand_ty, basic_type_float))
      {
        emit_ir_instr(instr_list, eOpcode_NEG_FLOAT32);
      }
      else
        assert(0);
    }
    else if(un_op == eOperator_logic_not)
    {
      gen_ir_load_rvalue(instr_list, operand);
      emit_ir_instr(instr_list, eOpcode_PUSH_INT32, 0);
      emit_ir_instr(instr_list, eOpcode_CMPEQ_INT32);
    }
    else if(un_op == eOperator_deref)
    {
      gen_ir_load_rvalue(instr_list, node);
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

    emit_ir_instr(instr_list, eOpcode_GROW, ret_area->size);

    for(ListItem* list_item = ATTR(node, list, actual_args)->first;
        list_item;
        list_item = list_item->next)
    {
      gen_ir_load_rvalue(instr_list, ITEM(list_item, ast_node));
    }
    // TODO: Assert that the 'data area size' == 'evaluated args size'

    // Setup the Access Link
    Symbol* occur_sym = ATTR(node, symbol, occur_sym);
    Scope* caller_scope = occur_sym->scope;
    int callee_depth_offset = caller_scope->nesting_depth - callee_scope->nesting_depth;
    if(callee_depth_offset < 0)
    {
      emit_ir_instr(instr_list, eOpcode_ACLINK, 0);
#if 0
      emit_ir_instr(instr_list, eOpcode_PUSH_REG, eRegName_FP);
      emit_ir_instr(instr_list, eOpcode_PUSH_INT32, link_area->loc);
      emit_ir_instr(instr_list, eOpcode_ADD_INT32);
#endif
    }
    else
    {
      emit_ir_instr(instr_list, eOpcode_ACLINK, 1 + callee_depth_offset);
#if 0
      emit_ir_instr(instr_list, eOpcode_PUSH_REG, eRegName_FP);
      emit_ir_instr(instr_list, eOpcode_PUSH_INT32, link_area->loc);
      emit_ir_instr(instr_list, eOpcode_ADD_INT32);

      for(int i = callee_depth_offset; i >= 0; i--)
      {
        emit_ir_instr(instr_list, eOpcode_LOAD, link_area->size);
      }
#endif
    }

    emit_ir_instr(instr_list, eOpcode_CALL, ATTR(node, str_val, name));

    emit_ir_instr(instr_list, eOpcode_GROW, -args_area->size);
    emit_ir_instr(instr_list, eOpcode_GROW, -link_area->size);
  }
  else if(node->kind == eAstNode_if_stmt)
  {
    gen_ir_load_rvalue(instr_list, ATTR(node, ast_node, cond_expr));

    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      emit_ir_instr(instr_list, eOpcode_JUMPZ, ATTR(node, str_val, label_else));
    }
    else
    {
      emit_ir_instr(instr_list, eOpcode_JUMPZ, ATTR(node, str_val, label_end));
    }

    gen_ir(instr_list, ATTR(node, ast_node, body));
    emit_ir_instr(instr_list, eOpcode_GOTO, ATTR(node, str_val, label_end));

    if(else_body)
    {
      emit_ir_instr(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_else));
      gen_ir(instr_list, else_body);
    }

    emit_ir_instr(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_end));
  }
  else if(node->kind == eAstNode_while_stmt)
  {
    emit_ir_instr(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_eval));
    gen_ir_load_rvalue(instr_list, ATTR(node, ast_node, cond_expr));
    emit_ir_instr(instr_list, eOpcode_JUMPZ, ATTR(node, str_val, label_break));

    AstNode* body = ATTR(node, ast_node, body);
    if(body)
    {
      gen_ir(instr_list, body);
    }

    emit_ir_instr(instr_list, eOpcode_GOTO, ATTR(node, str_val, label_eval));
    emit_ir_instr(instr_list, eOpcode_LABEL, ATTR(node, str_val, label_break));
  }
  else if(node->kind == eAstNode_break_stmt)
  {
    AstNode* loop = ATTR(node, ast_node, loop);

    int depth = ATTR(node, int_val, nesting_depth);
    while(depth-- > 0)
    {
      emit_ir_instr(instr_list, eOpcode_LEAVE);
    }
    emit_ir_instr(instr_list, eOpcode_GOTO, ATTR(loop, str_val, label_break));
  }
  else if(node->kind == eAstNode_empty)
  {
    ;//skip
  }
  else if(node->kind == eAstNode_asm_block)
  {
#if 0
    IrProgram ir_program = {0};
    ir_program.text = ATTR(node, str_val, asm_text);

    if(success = convert_hasm_to_instructions(&ir_program))
    {
      for(int i = 0; i < ir_program.instr_count; i++)
      {
        append_list_elem(instr_list, &ir_program.instructions[i], eList_ir_instr);
      }
    }
#endif
  }
  else
    assert(0);

  return success;
}

void copy_program_data(IrProgram* ir_program, int fp, List* areas_list)
{
  for(ListItem* list_item = areas_list->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* area = ITEM(list_item, data_area);
    if(area->subareas)
    {
      assert(!area->data);
      copy_program_data(ir_program, fp, area->subareas);
    }
    else
    {
      if(area->data)
      {
        uint8* p_data = (uint8*)area->data;
        for(int i = 0; i < area->size; i++)
        {
          ir_program->data[i + fp + area->loc] = p_data[i];
        }
      }
    }
  }
}


