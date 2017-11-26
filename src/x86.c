void gen_x86_load_rvalue(String* code, AstNode* node);
bool gen_x86(String* code, AstNode* node);

void make_x86_labels(AstNode* node)
{
  if(node->kind == eAstNode_module)
  {
    make_x86_labels(ATTR(node, ast_node, body));
  }
  else if(node->kind == eAstNode_block)
  {
    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item;
        list_item = list_item->next)
    {
      make_x86_labels(ITEM(list_item, ast_node));
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
    make_x86_labels(ATTR(node, ast_node, body));
  }
  else if(node->kind == eAstNode_proc_occur)
  {
    for(ListItem* list_item = ATTR(node, list, actual_args)->first;
        list_item;
        list_item = list_item->next)
    {
      make_x86_labels(ITEM(list_item, ast_node));
    }

    AstNode* proc_decl = ATTR(node, ast_node, proc_decl);
    ATTR(node, str_val, name) = ATTR(proc_decl, str_val, name);
  }
  else if(node->kind == eAstNode_stmt)
  {
    make_x86_labels(ATTR(node, ast_node, stmt));
  }
  else if(node->kind == eAstNode_bin_expr)
  {
    char* label_id = make_unique_label();

    String label; str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$logic_end");
    ATTR(node, str_val, label_end) = str_cap(&label);

    make_x86_labels(ATTR(node, ast_node, left_operand));
    make_x86_labels(ATTR(node, ast_node, right_operand));
  }
  else if(node->kind == eAstNode_un_expr)
  {
    make_x86_labels(ATTR(node, ast_node, operand));
  }
  else if(node->kind == eAstNode_if_stmt)
  {
    make_x86_labels(ATTR(node, ast_node, cond_expr));

    char* label_id = make_unique_label();

    String label; str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$if_else");
    ATTR(node, str_val, label_else) = str_cap(&label);

    str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$if_end");
    ATTR(node, str_val, label_end) = str_cap(&label);

    make_x86_labels(ATTR(node, ast_node, body));

    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      make_x86_labels(else_body);
    }
  }
  else if(node->kind == eAstNode_while_stmt)
  {
    make_x86_labels(ATTR(node, ast_node, cond_expr));

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
      make_x86_labels(body);
    }
  }
  else if(node->kind == eAstNode_ret_stmt)
  {
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    if(ret_expr)
    {
      make_x86_labels(ret_expr);
    }
  }
  else
    ;//skip
}

void gen_x86_enter_frame(String* code, int locals_size)
{
  str_printfln(code, "push ebp");
  str_printfln(code, "mov ebp, esp");
  str_printfln(code, "sub esp, %d", locals_size);
}

void gen_x86_leave_frame(String* code)
{
  str_printfln(code, "mov esp, ebp");
  str_printfln(code, "pop ebp");
}

void gen_x86_leave_nframe(String* code, int frame_count)
{
  char* begin = make_unique_label();
  str_printfln(code, "mov ecx, %d", frame_count);
  str_printfln(code, "%s:", begin);
  str_printfln(code, "mov esp, ebp");
  str_printfln(code, "pop ebp");
  str_printfln(code, "loop %s", begin);
}

void gen_x86_load(String* code, int byte_count)
{
  str_printfln(code, "mov ecx, %d", byte_count);
  str_printfln(code, "mov esi, dword ptr [esp]");
  str_printfln(code, "mov edi, esp");
  str_printfln(code, "rep movsb");
}

void gen_x86_store(String* code, int byte_count)
{
  str_printfln(code, "mov ecx, %d", byte_count);
  str_printfln(code, "pop edi");
  str_printfln(code, "mov esi, esp");
  str_printfln(code, "rep movsb");
}

void gen_x86_load_lvalue(String* code, AstNode* node)
{
  assert(node->gen == eAstGen_gen1);

  switch(node->kind)
  {
    case eAstNode_var_occur:
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
          //assert(link->loc < 0 && (link->loc % 4) == 0);
          str_printfln(code, "push ebp");
          str_printfln(code, "add dword ptr [esp], %d", 8);

          str_printfln(code, "mov ecx, %d", decl_scope_offset);
          str_printfln(code, "mov esi, dword ptr [esp]");
          char* begin = make_unique_label();
          str_printfln(code, "%s:", begin);
          str_printfln(code, "mov esi, dword ptr[esi]");
          str_printfln(code, "loop %s", begin);
          str_printfln(code, "mov dword ptr [esp], esi");

          // Load the FP by taking the offset relative to the Access Link
          str_printfln(code, "sub dword ptr [esp], %d", ctrl_area->size);
        }
        else if(decl_scope_offset == 0)
        {
          str_printfln(code, "push ebp");
        }
        else
          assert(0);

        str_printfln(code, "sub dword ptr [esp], %d", data->loc + data->size);
      }
      break;

    case eAstNode_un_expr:
      {
        AstNode* operand = ATTR(node, ast_node, operand);
        eOperator un_op = ATTR(node, op_kind, op_kind);

        if(un_op == eOperator_deref)
        {
          assert(ATTR(operand, type, eval_type)->kind == eType_pointer);
          gen_x86_load_rvalue(code, operand);
        }
        else
          assert(0);
      }
      break;

    case eAstNode_bin_expr:
      {
        Type* type = ATTR(node, type, eval_type);
        AstNode* left_operand = ATTR(node, ast_node, left_operand);
        AstNode* right_operand = ATTR(node, ast_node, right_operand);
        eOperator bin_op = ATTR(node, op_kind, op_kind);

        if(bin_op == eOperator_index)
        {
          gen_x86_load_lvalue(code, left_operand);

          gen_x86_load_rvalue(code, right_operand);
          str_printfln(code, "pop eax");
          str_printfln(code, "mov ebx, %d", type->width);
          str_printfln(code, "imul ebx");

          str_printfln(code, "add dword ptr [esp], eax");
        }
        else
          assert(0);
      }
      break;

    default:
      assert(0);
  }
}

void gen_x86_load_rvalue(String* code, AstNode* node)
{
  assert(node->gen == eAstGen_gen1);

  switch(node->kind)
  {
    case eAstNode_var_occur:
      {
        Type* type = ATTR(node, type, eval_type);
        gen_x86_load_lvalue(code, node);
        gen_x86_load(code, type->width);
      }
      break;

    case eAstNode_proc_occur:
      {
        gen_x86(code, node);
      }
      break;

    case eAstNode_lit:
      {
        eLiteral kind = ATTR(node, lit_kind, lit_kind);
        if(kind == eLiteral_int_val)
        {
          str_printfln(code, "push %d", ATTR(node, int_val, int_val));
        }
        else if(kind == eLiteral_bool_val)
        {
          str_printfln(code, "push %d", ATTR(node, int_val, int_val));
        }
        else if(kind == eLiteral_float_val)
        {
          union BitcastF32ToI32
          {
            float32 float32_val;
            int32 int32_val;
          };

          union BitcastF32ToI32 val = {0};
          val.float32_val = ATTR(node, float_val, float_val); 
          str_printfln(code, "push %xh ; %f", val.int32_val, val.float32_val);
        }
        else if(kind == eLiteral_char_val)
        {
          str_printfln(code, "push %d", ATTR(node, char_val, char_val));
        }
        else
          assert(0);
      }
      break;

    case eAstNode_bin_expr:
      {
        Type* type = ATTR(node, type, eval_type);
        eOperator bin_op = ATTR(node, op_kind, op_kind);

        if(bin_op == eOperator_index)
        {
          gen_x86_load_lvalue(code, node);
          gen_x86_load(code, type->width);
        }
        else
        {
          gen_x86(code, node);
        }
      }
      break;

    case eAstNode_un_expr:
      {
        Type* type = ATTR(node, type, eval_type);
        AstNode* operand = ATTR(node, ast_node, operand);
        eOperator un_op = ATTR(node, op_kind, op_kind);

        if(un_op == eOperator_address_of)
        {
          if(operand->kind == eAstNode_var_occur)
          {
            gen_x86_load_lvalue(code, operand);
          }
          else
          {
            gen_x86_load_rvalue(code, operand);
          }
        }
        else if(un_op == eOperator_deref)
        {
          gen_x86_load_lvalue(code, node);
          gen_x86_load(code, type->width);
        }
        else
        {
          gen_x86(code, node);
        }
      }
      break;
  }
}

bool gen_x86(String* code, AstNode* node)
{
  assert(node->gen == eAstGen_gen1);
  bool success = true;

  switch(node->kind)
  {
    case eAstNode_module:
      {
        AstNode* body = ATTR(node, ast_node, body);
        Scope* scope = ATTR(body, scope, scope);
        DataArea* locals_area = &scope->locals_area;

        str_printfln(code, "push ebp");
        str_printfln(code, "sub esp, 4"); // dummy IP
        gen_x86_enter_frame(code, locals_area->size);

        for(ListItem* list_item = ATTR(body, list, stmts)->first;
            list_item;
            list_item = list_item->next)
        {
          AstNode* stmt = ITEM(list_item, ast_node);
          gen_x86(code, stmt);
        }

        gen_x86_leave_frame(code);
        str_printfln(code, "add esp, 4"); // dummy IP
        str_printfln(code, "pop ebp");
        str_printfln(code, "ret");

        for(ListItem* list_item = ATTR(body, list, procs)->first;
            list_item;
            list_item = list_item->next)
        {
          AstNode* proc = ITEM(list_item, ast_node);
          assert(proc->kind == eAstNode_proc_decl);
          gen_x86(code, proc);
        }
      }
      break;

    case eAstNode_proc_decl:
      {
        Scope* scope = ATTR(node, scope, scope);
        DataArea* locals_area = &scope->locals_area;

        str_printfln(code,"_%s:", ATTR(node, str_val, name));
        gen_x86_enter_frame(code, locals_area->size);

        AstNode* body = ATTR(node, ast_node, body);
        for(ListItem* list_item = ATTR(body, list, stmts)->first;
            list_item;
            list_item = list_item->next)
        {
          AstNode* stmt = ITEM(list_item, ast_node);
          gen_x86(code, stmt);
        }

        gen_x86_leave_frame(code);
        str_printfln(code, "ret");

        for(ListItem* list_item = ATTR(body, list, procs)->first;
            list_item;
            list_item = list_item->next)
        {
          AstNode* proc = ITEM(list_item, ast_node); assert(proc->kind == eAstNode_proc_decl);
          gen_x86(code, proc);
        }
      }
      break;

    case eAstNode_block:
      {
        Scope* scope = ATTR(node, scope, scope);
        DataArea* link_area = &scope->link_area;
        DataArea* locals_area = &scope->locals_area;

        //assert(link_area->loc < 0 && (link_area->loc % 4) == 0);
        str_printfln(code, "push ebp");
        str_printfln(code, "add dword ptr [esp], %d", 8);
        str_printfln(code, "sub esp, 4"); // dummy IP
        gen_x86_enter_frame(code, locals_area->size);

        for(ListItem* list_item = ATTR(node, list, stmts)->first;
            list_item;
            list_item = list_item->next)
        {
          AstNode* stmt = ITEM(list_item, ast_node);
          gen_x86(code, stmt);
        }

        gen_x86_leave_frame(code);
        str_printfln(code, "add esp, 4"); // dummy IP
        str_printfln(code, "add esp, %d", link_area->size);

        for(ListItem* list_item = ATTR(node, list, procs)->first;
            list_item;
            list_item = list_item->next)
        {
          AstNode* proc = ITEM(list_item, ast_node); assert(proc->kind == eAstNode_proc_decl);
          gen_x86(code, proc);
        }
      }
      break;

    case eAstNode_proc_occur:
      {
        AstNode* callee_decl = ATTR(node, ast_node, proc_decl);
        Scope* callee_scope = ATTR(callee_decl, scope, scope);
        DataArea* ret_area = &callee_scope->ret_area;
        DataArea* args_area = &callee_scope->args_area;
        DataArea* link_area = &callee_scope->link_area;

        str_printfln(code, "sub esp, %d", ret_area->size);

        for(ListItem* list_item = ATTR(node, list, actual_args)->first;
            list_item;
            list_item = list_item->next)
        {
          gen_x86_load_rvalue(code, ITEM(list_item, ast_node));
        }
        // TODO: Assert that the 'data area size' == 'evaluated args size'

        Symbol* occur_sym = ATTR(node, symbol, occur_sym);
        Scope* caller_scope = occur_sym->scope;
        
        //assert(link_area->loc < 0 && (link_area->loc % 4) == 0);
        str_printfln(code, "push ebp");
        str_printfln(code, "add dword ptr [esp], %d", 8);

        int callee_depth_offset = caller_scope->nesting_depth - callee_scope->nesting_depth;
        if(callee_depth_offset >= 0)
        {
          str_printfln(code, "mov ecx, %d", callee_depth_offset + 1);
          str_printfln(code, "mov esi, dword ptr [esp]");
          char* begin = make_unique_label();
          str_printfln(code, "%s:", begin);
          str_printfln(code, "mov esi, dword ptr[esi]");
          str_printfln(code, "loop %s", begin);
          str_printfln(code, "mov dword ptr [esp], esi");
        }
        str_printfln(code, "call _%s", ATTR(node, str_val, name));
        str_printfln(code, "add esp, %d", link_area->size + args_area->size);
      }
      break;

    case eAstNode_stmt:
      {
        AstNode* actual_stmt = ATTR(node, ast_node, stmt);
        Type* type = ATTR(actual_stmt, type, eval_type);
        gen_x86(code, actual_stmt);
        str_printfln(code, "add esp, %d", type->width);
      }
      break;

    case eAstNode_bin_expr:
      {
        AstNode* left_operand = ATTR(node, ast_node, left_operand);
        AstNode* right_operand = ATTR(node, ast_node, right_operand);
        Type* type = ATTR(node, type, eval_type);
        Type* left_ty = ATTR(left_operand, type, eval_type);
        Type* right_ty = ATTR(right_operand, type, eval_type);
        eOperator bin_op = ATTR(node, op_kind, op_kind);

        switch(bin_op)
        {
          case eOperator_assign:
            {
              gen_x86_load_rvalue(code, right_operand);
              gen_x86_load_lvalue(code, left_operand);
              gen_x86_store(code, type->width);
            }
            break;

          case eOperator_add:
            {
              gen_x86_load_rvalue(code, left_operand);
              gen_x86_load_rvalue(code, right_operand);

              if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
              {
                str_printfln(code, "pop eax");
                str_printfln(code, "add dword ptr [esp], eax");
              }
              else if(types_are_equal(type, basic_type_float))
              {
                assert(0);
              }
              else
                assert(0);
            }
            break;

          case eOperator_sub:
            {
              gen_x86_load_rvalue(code, left_operand);
              gen_x86_load_rvalue(code, right_operand);

              if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
              {
                str_printfln(code, "pop eax");
                str_printfln(code, "sub dword ptr [esp], eax");
              }
              else if(types_are_equal(type, basic_type_float))
              {
                assert(0);
              }
              else
                assert(0);
            }
            break;

          case eOperator_mul:
            {
              gen_x86_load_rvalue(code, right_operand);
              gen_x86_load_rvalue(code, left_operand);

              if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
              {
                str_printfln(code, "pop eax");
                str_printfln(code, "imul dword ptr [esp]");
                str_printfln(code, "mov dword ptr [esp], eax");
              }
              else if(types_are_equal(type, basic_type_float))
              {
                //assert(0);
              }
              else
                assert(0);
            }
            break;

          case eOperator_div:
            {
              gen_x86_load_rvalue(code, right_operand);
              gen_x86_load_rvalue(code, left_operand);

              if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
              {
                str_printfln(code, "pop eax");
                str_printfln(code, "cdq"); // extend EAX into EDX
                str_printfln(code, "idiv dword ptr [esp]");
                str_printfln(code, "mov dword ptr [esp], eax");
              }
              else if(types_are_equal(type, basic_type_float))
              {
                //assert(0);
              }
              else
                assert(0);
            }
            break;

          case eOperator_mod:
            {
              gen_x86_load_rvalue(code, right_operand);
              gen_x86_load_rvalue(code, left_operand);

              if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
              {
                str_printfln(code, "pop eax");
                str_printfln(code, "cdq"); // extend EAX into EDX
                str_printfln(code, "idiv dword ptr [esp]");
                str_printfln(code, "mov dword ptr [esp], edx");
              }
              else
                assert(0);
            }
            break;

          // TODO
        }
      }
      break;

    case eAstNode_un_expr:
      {
        AstNode* operand = ATTR(node, ast_node, operand);
        Type* operand_ty = ATTR(operand, type, eval_type);
        eOperator un_op = ATTR(node, op_kind, op_kind);

        if(un_op == eOperator_address_of)
        {
          gen_x86_load_rvalue(code, node);
        }
        else if(un_op == eOperator_neg)
        {
          gen_x86_load_rvalue(code, operand);

          if(types_are_equal(operand_ty, basic_type_int))
          {
            str_printfln(code, "neg dword ptr [esp]");
          }
          else if(types_are_equal(operand_ty, basic_type_float))
          {
            assert(0);
          }
          else
            assert(0);
        }
        else if(un_op == eOperator_logic_not)
        {
          assert(0);
        }
        else if(un_op == eOperator_deref)
        {
          gen_x86_load_rvalue(code, node);
        }
        else
          assert(0);
      }
      break;

    case eAstNode_ret_stmt:
      {
        AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
        if(ret_expr)
        {
          gen_x86(code, ret_expr);
        }

        int depth = ATTR(node, int_val, nesting_depth);
        if(depth > 0)
        {
          gen_x86_leave_nframe(code, depth);
        }
        str_printfln(code, "ret");
      }
      break;

    case eAstNode_empty:
      break; // skip

    case eAstNode_asm_block:
      {
        str_append(code, ATTR(node, str_val, asm_text));
      }
      break;

    //default:
      //assert(0);
  }
  return success;
}

#if 0
bool gen_x86(List* ir_code, String* x86_code)
{
  bool success = true;

  for(ListItem* list_item = ir_code->first;
      list_item;
      list_item = list_item->next)
  {
    IrInstruction* ir_instr = ITEM(list_item, ir_instr);
    eOpcode opcode = ir_instr->opcode;

    switch(opcode)
    {
      case eOpcode_ENTER:
        {
          str_printfln(x86_code, "push ebp");
          str_printfln(x86_code, "mov ebp, esp");
        }
        break;

      case eOpcode_LEAVE:
        {
          str_printfln(x86_code, "mov esp, ebp");
          str_printfln(x86_code, "pop ebp");
        }
        break;

      case eOpcode_CALL:
        {
          str_printfln(x86_code, "call _%s", ir_instr->param.id);
        }
        break;

      case eOpcode_RETURN:
        {
          str_printfln(x86_code, "ret");
        }
        break;

      case eOpcode_LOAD:
        {
          str_printfln(x86_code, "mov ecx, %d", ir_instr->param.int32_val);
          str_printfln(x86_code, "mov esi, dword ptr [esp]");
          str_printfln(x86_code, "mov edi, esp");
          str_printfln(x86_code, "rep movsb");
        }
        break;

      case eOpcode_STORE:
        {
          str_printfln(x86_code, "mov ecx, %d", ir_instr->param.int32_val);
          str_printfln(x86_code, "mov esi, esp");
          str_printfln(x86_code, "add esi, 4");
          str_printfln(x86_code, "mov edi, dword ptr [esp]");
          str_printfln(x86_code, "rep movsb");
        }
        break;

      case eOpcode_PUSH_INT8:
        {
          //TODO
          //str_printfln(x86_code, "push %d", ir_instr->param.int8_val);
        }
        break;

      case eOpcode_PUSH_INT32:
        {
          str_printfln(x86_code, "push %d", ir_instr->param.int32_val);
        }
        break;

      case eOpcode_PUSH_FLOAT32:
        {
          str_printfln(x86_code, "push %xh ; %f", ir_instr->param.int32_val, ir_instr->param.float32_val);
        }
        break;

      case eOpcode_PUSH_REG:
        {
          if(ir_instr->param.reg == eRegName_FP)
          {
            str_printfln(x86_code, "push ebp");
          }
          else if(ir_instr->param.reg == eRegName_SP)
          {
            str_printfln(x86_code, "push esp");
          }
          else
            assert(0);
        }
        break;

      case eOpcode_POP_REG:
        {
          if(ir_instr->param.reg == eRegName_FP)
          {
            str_printfln(x86_code, "pop ebp");
          }
          else if(ir_instr->param.reg == eRegName_SP)
          {
            str_printfln(x86_code, "pop esp");
          }
          else
            assert(0);
        }
        break;

      case eOpcode_ADD_INT32:
      case eOpcode_SUB_INT32:
      case eOpcode_MUL_INT32:
      case eOpcode_DIV_INT32:
        {
          str_printfln(x86_code, "mov eax, dword ptr [esp+4]");
          if(opcode == eOpcode_ADD_INT32)
          {
            str_printfln(x86_code, "add dword ptr [esp+8], eax");
          }
          else if(opcode == eOpcode_SUB_INT32)
          {
            str_printfln(x86_code, "sub dword ptr [esp+8], eax");
          }
          else if(opcode == eOpcode_MUL_INT32)
          {
            //TODO
          }
          else if(opcode == eOpcode_DIV_INT32)
          {
            //TODO
          }
        }
        break;

      case eOpcode_ADD_FLOAT32:
        {
          str_printfln(x86_code, "fld dword ptr [esp-4]");
        }
        break;

      case eOpcode_GROW:
      case eOpcode_GROWNZ:
        {
          if(ir_instr->param.int32_val >= 0)
          {
            str_printfln(x86_code, "sub esp, %d", ir_instr->param.int32_val);
          }
          else
          {
            str_printfln(x86_code, "add esp, %d", ir_instr->param.int32_val);
          }
        }
        break;

      case eOpcode_GOTO:
        {
          str_printfln(x86_code, "jmp _%s", ir_instr->param.id);
        }
        break;

      case eOpcode_LABEL:
        {
          str_printfln(x86_code, "_%s:", ir_instr->param.id);
        }
        break;

      case eOpcode_JUMPZ:
        {
          //TODO
        }
        break;

      case eOpcode_ACLINK:
        {
          str_printfln(x86_code, "push dword ptr [ebp-12]");
          if(ir_instr->param.int32_val > 0)
          {
            str_printfln(x86_code, "mov ecx, %d", ir_instr->param.int32_val);
            char* loop = make_unique_label();
            str_printfln(x86_code, "%s:", loop);
            str_printfln(x86_code, "mov eax, dword ptr [esp+4]");
            str_printfln(x86_code, "mov dword ptr [esp+4], eax");
            str_printfln(x86_code, "loop %s", loop);
          }
        }
        break;

      case eOpcode_HALT:
        {
          str_printfln(x86_code, "INVOKE ExitProcess, 0");
        }
        break;
    }
  }
  return success;
}
#endif

