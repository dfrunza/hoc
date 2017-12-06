void gen_x86_load_rvalue(String* code, AstNode* node);
bool gen_x86(String* code, AstNode* node);

void gen_x86_enter_frame(String* code, int locals_size)
{
  str_printfln(code, "push ebp");
  str_printfln(code, "mov ebp, esp");
  str_printfln(code, "sub esp, %d", locals_size);
}

void gen_x86_leave_frame(String* code, int depth)
{
  if(depth > 0)
  {
    Label label = make_unique_label();
    str_printfln(code, "mov ecx, %d", depth);
    str_printfln(code, "%s$loop:", label.id);
    str_printfln(code, "mov esp, ebp");
    str_printfln(code, "pop ebp");
    str_printfln(code, "loop %s$loop", label.id);
  }
  else if(depth == 0)
  {
    str_printfln(code, "mov esp, ebp");
    str_printfln(code, "pop ebp");
  }
  else
    assert(0);
}

void gen_x86_load(String* code, int int8_count)
{
  str_printfln(code, "mov ecx, %d", int8_count);
  str_printfln(code, "mov esi, dword ptr [esp]");
  str_printfln(code, "mov edi, esp");
  str_printfln(code, "rep movs byte ptr [edi], byte ptr [esi]");
#if 0
  int int32_count = int8_count / MACHINE_WORD_SIZE;
  int int8_remainder = int8_count % MACHINE_WORD_SIZE;

  if(int32_count > 0)
  {
    str_printfln(code, "mov ecx, %d", int32_count);
    str_printfln(code, "mov esi, dword ptr [esp]");
    str_printfln(code, "mov edi, esp");
    str_printfln(code, "rep movsd");
  }
  if(int8_remainder > 0)
  {
    str_printfln(code, "mov ecx, %d", int8_remainder);
    str_printfln(code, "mov esi, dword ptr [esp]");
    str_printfln(code, "mov edi, esp");
    str_printfln(code, "rep movsb");
  }
#endif
}

void gen_x86_store(String* code, int int8_count)
{
  str_printfln(code, "mov ecx, %d", int8_count);
  str_printfln(code, "pop edi");
  str_printfln(code, "mov esi, esp");
  str_printfln(code, "rep movs byte ptr [edi], byte ptr [esi]");
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

        if(decl_sym->is_global)
        {
          assert(decl_sym->data_loc >= 0);
          str_printfln(code, "push OFFSET(global_area) + %d", decl_sym->data_loc);
        }
        else
        {
          str_printfln(code, "push ebp");
          int decl_scope_offset = occur_sym->nesting_depth - decl_sym->nesting_depth;
          if(decl_scope_offset > 0)
          {
            // Non-local
            str_printfln(code, "add dword ptr [esp], %d", 2*MACHINE_WORD_SIZE);

            str_printfln(code, "mov ecx, %d", decl_scope_offset);
            str_printfln(code, "mov esi, dword ptr [esp]");
            Label label = make_unique_label();
            str_printfln(code, "%s$loop:", label.id);
            str_printfln(code, "mov esi, dword ptr[esi]");
            str_printfln(code, "loop %s$loop", label.id);
            str_printfln(code, "mov dword ptr [esp], esi");

            // Load the FP by taking the offset relative to the Access Link
            str_printfln(code, "sub dword ptr [esp], %d", 2*MACHINE_WORD_SIZE);
          }
          else if(decl_scope_offset < 0)
            assert(0);

          if(decl_sym->data_loc >= 0)
          {
            str_printfln(code, "add dword ptr [esp], %d", decl_sym->data_loc);
          }
          else
          {
            str_printfln(code, "sub dword ptr [esp], %d", -decl_sym->data_loc);
          }
        }
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
          str_printfln(code, "push %d", ATTR(node, bool_val, bool_val));
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

        for(ListItem* list_item = scope->decls[eSymbol_extern_proc]->first;
            list_item;
            list_item = list_item->next)
        {
          Symbol* decl_sym = ITEM(list_item, symbol);
          gen_x86(code, decl_sym->ast_node);
        }
        for(ListItem* list_item = scope->decls[eSymbol_proc]->first;
            list_item;
            list_item = list_item->next)
        {
          Symbol* decl_sym = ITEM(list_item, symbol);
          gen_x86(code, decl_sym->ast_node);
        }

        str_printfln(code, "startup PROC");
        str_printfln(code, "sub esp, %d", MACHINE_WORD_SIZE); // dummy IP
        str_printfln(code, "; {");
        gen_x86_enter_frame(code, scope->locals_area_size);

        for(ListItem* list_item = ATTR(body, list, stmts)->first;
            list_item;
            list_item = list_item->next)
        {
          AstNode* stmt = ITEM(list_item, ast_node);
          gen_x86(code, stmt);
        }

        gen_x86_leave_frame(code, 0);
        str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE); // dummy IP
        str_printfln(code, "ret");
        str_printfln(code, "startup ENDP");
        str_printfln(code, "; }");
      }
      break;

    case eAstNode_proc_decl:
      {
        Scope* scope = ATTR(node, scope, scope);

        char* label = ATTR(node, str_val, label);
        bool is_extern = ATTR(node, bool_val, is_extern);
        if(is_extern)
        {
          str_printfln(code, "EXTERN %s:PROC", label);
        }
        else
        {
          for(ListItem* list_item = scope->decls[eSymbol_extern_proc]->first;
              list_item;
              list_item = list_item->next)
          {
            Symbol* decl_sym = ITEM(list_item, symbol);
            gen_x86(code, decl_sym->ast_node);
          }
          for(ListItem* list_item = scope->decls[eSymbol_proc]->first;
              list_item;
              list_item = list_item->next)
          {
            Symbol* decl_sym = ITEM(list_item, symbol);
            gen_x86(code, decl_sym->ast_node);
          }

          str_printfln(code,"%s PROC", label);
          str_printfln(code, "; {");
          gen_x86_enter_frame(code, scope->locals_area_size);

          AstNode* body = ATTR(node, ast_node, body);
          for(ListItem* list_item = ATTR(body, list, stmts)->first;
              list_item;
              list_item = list_item->next)
          {
            AstNode* stmt = ITEM(list_item, ast_node);
            gen_x86(code, stmt);
          }

          gen_x86_leave_frame(code, 0);
          str_printfln(code, "ret");
          str_printfln(code, "%s ENDP", label);
          str_printfln(code, "; }");
        }
      }
      break;

    case eAstNode_block:
      {
        Scope* scope = ATTR(node, scope, scope);

        for(ListItem* list_item = scope->decls[eSymbol_extern_proc]->first;
            list_item;
            list_item = list_item->next)
        {
          Symbol* decl_sym = ITEM(list_item, symbol);
          gen_x86(code, decl_sym->ast_node);
        }
        for(ListItem* list_item = scope->decls[eSymbol_proc]->first;
            list_item;
            list_item = list_item->next)
        {
          Symbol* decl_sym = ITEM(list_item, symbol);
          gen_x86(code, decl_sym->ast_node);
        }

        str_printfln(code, "; {");
        str_printfln(code, "push ebp");
        str_printfln(code, "add dword ptr [esp], %d", 2*MACHINE_WORD_SIZE);
        str_printfln(code, "sub esp, %d", MACHINE_WORD_SIZE); // dummy IP
        gen_x86_enter_frame(code, scope->locals_area_size);

        for(ListItem* list_item = ATTR(node, list, stmts)->first;
            list_item;
            list_item = list_item->next)
        {
          AstNode* stmt = ITEM(list_item, ast_node);
          gen_x86(code, stmt);
        }

        gen_x86_leave_frame(code, 0);
        str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE);
        str_printfln(code, "; }");
      }
      break;

    case eAstNode_proc_occur:
      {
        AstNode* proc_decl = ATTR(node, ast_node, proc_decl);
        Scope* callee_scope = ATTR(proc_decl, scope, scope);

        str_printfln(code, "sub esp, %d", callee_scope->ret_area_size);
        for(ListItem* list_item = ATTR(node, list, actual_args)->last;
            list_item;
            list_item = list_item->prev)
        {
          gen_x86_load_rvalue(code, ITEM(list_item, ast_node));
        }

        char* label = ATTR(proc_decl, str_val, label);
        bool is_extern = ATTR(proc_decl, bool_val, is_extern);
        if(is_extern)
        {

          str_printfln(code, "call %s", label);

          if(callee_scope->ret_area_size == MACHINE_WORD_SIZE)
          {
            str_printfln(code, "mov dword ptr [esp], eax ; save the return value");
          }
          else if(callee_scope->ret_area_size != 0)
            assert(0); // don't know what to do
        }
        else
        {
          Symbol* occur_sym = ATTR(node, symbol, occur_sym);
          Scope* caller_scope = occur_sym->scope;

          str_printfln(code, "push ebp");
          str_printfln(code, "add dword ptr [esp], %d", 2*MACHINE_WORD_SIZE);

          int callee_depth_offset = caller_scope->nesting_depth - callee_scope->nesting_depth;
          if(callee_depth_offset >= 0)
          {
            str_printfln(code, "mov ecx, %d", callee_depth_offset + 1);
            str_printfln(code, "mov esi, dword ptr [esp]");
            Label label = make_unique_label();
            str_printfln(code, "%s$loop:", label.id);
            str_printfln(code, "mov esi, dword ptr[esi]");
            str_printfln(code, "loop %s$loop", label.id);
            str_printfln(code, "mov dword ptr [esp], esi");
          }

          str_printfln(code, "call %s", label);
          str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE + callee_scope->args_area_size);
        }
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
                str_printfln(code, "movss xmm0, dword ptr [esp]");
                str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                str_printfln(code, "addss xmm0, dword ptr [esp]");
                str_printfln(code, "movss dword ptr [esp], xmm0");
              }
              else
                assert(0);
            }
            break;

          case eOperator_sub:
            {
              if(types_are_equal(type, basic_type_int) || type->kind == eType_pointer)
              {
                gen_x86_load_rvalue(code, left_operand);
                gen_x86_load_rvalue(code, right_operand);

                str_printfln(code, "pop eax");
                str_printfln(code, "sub dword ptr [esp], eax");
              }
              else if(types_are_equal(type, basic_type_float))
              {
                gen_x86_load_rvalue(code, right_operand);
                gen_x86_load_rvalue(code, left_operand);

                str_printfln(code, "movss xmm0, dword ptr [esp]");
                str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                str_printfln(code, "subss xmm0, dword ptr [esp]");
                str_printfln(code, "movss dword ptr [esp], xmm0");
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
                str_printfln(code, "movss xmm0, dword ptr [esp]");
                str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                str_printfln(code, "mulss xmm0, dword ptr [esp]");
                str_printfln(code, "movss dword ptr [esp], xmm0");
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
                str_printfln(code, "movss xmm0, dword ptr [esp]");
                str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                str_printfln(code, "divss xmm0, dword ptr [esp]");
                str_printfln(code, "movss dword ptr [esp], xmm0");
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

          case eOperator_eq:
          case eOperator_not_eq:
            {
              gen_x86_load_rvalue(code, left_operand);
              gen_x86_load_rvalue(code, right_operand);

              if(types_are_equal(left_ty, basic_type_char) || types_are_equal(left_ty, basic_type_int))
              {
                if(types_are_equal(left_ty, basic_type_int))
                {
                  str_printfln(code, "pop eax");
                  str_printfln(code, "pop ebx");
                }
                else if(types_are_equal(left_ty, basic_type_char))
                {
                  str_printfln(code, "movzx eax, byte ptr [esp]");
                  str_printfln(code, "movzx ebx, byte ptr [esp+%d]", MACHINE_WORD_SIZE);
                  str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE);
                }

                str_printfln(code, "cmp ebx, eax");
              }
              else if(types_are_equal(left_ty, basic_type_float))
              {
                str_printfln(code, "movss xmm0, dword ptr [esp]");
                str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                str_printfln(code, "comiss xmm0, dword ptr [esp]");
              }
              else
                assert(0);

              Label label = make_unique_label();
              str_printfln(code, "push 1");
              if(bin_op == eOperator_eq)
              {
                str_printfln(code, "jz %s$cmp_eq", label.id);
                str_printfln(code, "xor dword ptr [esp], 1");
                str_printfln(code, "%s$cmp_eq:", label.id);
              }
              else if(bin_op == eOperator_not_eq)
              {
                str_printfln(code, "jnz %s$cmp_not_eq", label.id);
                str_printfln(code, "xor dword ptr [esp], 1");
                str_printfln(code, "%s$cmp_not_eq:", label.id);
              }
              else
                assert(0);
              //TODO: Do we need to explicitly handle the case when at least one of floating point operands is NaN,
              //or is there going to be some kind of CPU exception that will halt the program?
            }
            break;

          case eOperator_less:
          case eOperator_greater:
          case eOperator_less_eq:
          case eOperator_greater_eq:
            {
              if(types_are_equal(left_ty, basic_type_char) || types_are_equal(left_ty, basic_type_int))
              {
                gen_x86_load_rvalue(code, left_operand);
                gen_x86_load_rvalue(code, right_operand);

                if(types_are_equal(left_ty, basic_type_int))
                {
                  str_printfln(code, "pop eax");
                  str_printfln(code, "pop ebx");
                }
                else if(types_are_equal(left_ty, basic_type_char))
                {
                  str_printfln(code, "movzx eax, byte ptr [esp]");
                  str_printfln(code, "movzx ebx, byte ptr [esp+%d]", MACHINE_WORD_SIZE);
                  str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE);
                }

                str_printfln(code, "cmp ebx, eax");

                Label label = make_unique_label();
                str_printfln(code, "push 1");
                if(bin_op == eOperator_less)
                {
                  str_printfln(code, "jl %s$cmp_less", label.id);
                  str_printfln(code, "xor dword ptr [esp], 1");
                  str_printfln(code, "%s$cmp_less:", label.id);
                }
                else if(bin_op == eOperator_less_eq)
                {
                  str_printfln(code, "jle %s$cmp_less_eq", label.id);
                  str_printfln(code, "xor dword ptr [esp], 1");
                  str_printfln(code, "%s$cmp_less_eq:", label.id);
                }
                else if(bin_op == eOperator_greater)
                {
                  str_printfln(code, "jg %s$cmp_greater", label.id);
                  str_printfln(code, "xor dword ptr [esp], 1");
                  str_printfln(code, "%s$cmp_greater:", label.id);
                }
                else if(bin_op == eOperator_greater_eq)
                {
                  str_printfln(code, "jge %s$cmp_greater_eq", label.id);
                  str_printfln(code, "xor dword ptr [esp], 1");
                  str_printfln(code, "%s$cmp_greater_eq:", label.id);
                }
                else
                  assert(0);
              }
              else if(types_are_equal(left_ty, basic_type_float))
              {
                if(bin_op == eOperator_less)
                {
                  gen_x86_load_rvalue(code, right_operand);
                  gen_x86_load_rvalue(code, left_operand);
                  str_printfln(code, "movss xmm0, dword ptr [esp]");
                  str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                  str_printfln(code, "cmpss xmm0, dword ptr [esp], 1");
                }
                else if(bin_op == eOperator_less_eq)
                {
                  gen_x86_load_rvalue(code, right_operand);
                  gen_x86_load_rvalue(code, left_operand);
                  str_printfln(code, "movss xmm0, dword ptr [esp]");
                  str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                  str_printfln(code, "cmpss xmm0, dword ptr [esp], 2");
                }
                else if(bin_op == eOperator_greater)
                {
                  gen_x86_load_rvalue(code, left_operand);
                  gen_x86_load_rvalue(code, right_operand);
                  str_printfln(code, "movss xmm0, dword ptr [esp]");
                  str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                  str_printfln(code, "cmpss xmm0, dword ptr [esp], 1");
                }
                else if(bin_op == eOperator_greater_eq)
                {
                  gen_x86_load_rvalue(code, left_operand);
                  gen_x86_load_rvalue(code, right_operand);
                  str_printfln(code, "movss xmm0, dword ptr [esp]");
                  str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                  str_printfln(code, "cmpss xmm0, dword ptr [esp], 2");
                }
                else
                  assert(0);
                //TODO: Do we need to explicitly handle the case when at least one of floating point operands is NaN,
                //or is there going to be some kind of CPU exception that will halt the program?

                str_printfln(code, "movss dword ptr [esp], xmm0");
              }
              else
                assert(0);
            }
            break;

          case eOperator_logic_and:
          case eOperator_logic_or:
            {
              gen_x86_load_rvalue(code, left_operand);

              Label label = make_unique_label();
              str_printfln(code, "pop eax");
              if(bin_op == eOperator_logic_and)
              {
                str_printfln(code, "and eax, 1");
                str_printfln(code, "jz %s$logic_end", label.id);
              }
              else if(bin_op == eOperator_logic_or)
              {
                str_printfln(code, "or eax, 0");
                str_printfln(code, "jnz %s$logic_end", label.id);
              }
              else
                assert(0);

              gen_x86_load_rvalue(code, right_operand);

              str_printfln(code, "pop eax");
              if(bin_op == eOperator_logic_and)
              {
                str_printfln(code, "and eax, 1");
                str_printfln(code, "jz %s$logic_end", label.id);
              }
              else if(bin_op == eOperator_logic_or)
              {
                str_printfln(code, "or eax, 0");
                str_printfln(code, "jnz %s$logic_end", label.id);
              }
              else
                assert(0);

              str_printfln(code, "%s$logic_end:", label.id);
              str_printfln(code, "push eax");
            }
            break;

          case eOperator_bit_and:
          case eOperator_bit_or:
          case eOperator_bit_xor:
            {
              gen_x86_load_rvalue(code, left_operand);
              gen_x86_load_rvalue(code, right_operand);

              str_printfln(code, "pop eax");
              if(bin_op == eOperator_bit_and)
              {
                str_printfln(code, "and dword ptr [esp], eax");
              }
              else if(bin_op == eOperator_bit_or)
              {
                str_printfln(code, "or dword ptr [esp], eax");
              }
              else if(bin_op == eOperator_bit_xor)
              {
                str_printfln(code, "xor dword ptr [esp], eax");
              }
              else
                assert(0);
            }
            break;

          case eOperator_bit_shift_left:
          case eOperator_bit_shift_right:
            {
              gen_x86_load_rvalue(code, left_operand);
              gen_x86_load_rvalue(code, right_operand);

              str_printfln(code, "mov cl, byte ptr [esp]");
              str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);

              if(bin_op == eOperator_bit_shift_left)
              {
                str_printfln(code, "shl dword ptr [esp], cl");
              }
              else if(bin_op == eOperator_bit_shift_right)
              {
                str_printfln(code, "shr dword ptr [esp], cl");
              }
              else
                assert(0);
            }
            break;

          case eOperator_cast:
            {
              gen_x86_load_rvalue(code, right_operand);

              if(types_are_equal(left_ty, basic_type_int) && types_are_equal(right_ty, basic_type_float))
              {
                // int <- float
                str_printfln(code, "cvttss2si eax, dword ptr [esp]");
                str_printfln(code, "mov dword ptr [esp], eax");
              }
              if(types_are_equal(left_ty, basic_type_float) && types_are_equal(right_ty, basic_type_int))
              {
                // float <- int
                str_printfln(code, "cvtsi2ss xmm0, dword ptr [esp]");
                str_printfln(code, "movss dword ptr [esp], xmm0");
              }
            }
            break;

          case eOperator_index:
            {
              gen_x86_load_rvalue(code, node);
            }
            break;

          default:
            assert(0);
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
            str_printfln(code, "xor dword ptr [esp], %xh", 0x80000000);
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

    case eAstNode_while_stmt:
      {
        Label label = ATTR(node, label, label) = make_unique_label();
        str_printfln(code, "%s$while_eval:", label.id);
        gen_x86_load_rvalue(code, ATTR(node, ast_node, cond_expr));

        str_printfln(code, "pop eax");
        str_printfln(code, "and eax, 1");
        str_printfln(code, "jz %s$while_break", label.id);

        AstNode* body = ATTR(node, ast_node, body);
        if(body)
        {
          gen_x86(code, body);
        }

        str_printfln(code, "jmp %s$while_eval", label.id);
        str_printfln(code, "%s$while_break:", label.id);
      }
      break;

    case eAstNode_if_stmt:
      {
        gen_x86_load_rvalue(code, ATTR(node, ast_node, cond_expr));

        str_printfln(code, "pop eax");
        str_printfln(code, "and eax, 1");

        Label label = make_unique_label();
        AstNode* else_body = ATTR(node, ast_node, else_body);
        if(else_body)
        {
          str_printfln(code, "jz %s$if_else", label.id);
        }
        else
        {
          str_printfln(code, "jz %s$if_end", label.id);
        }

        gen_x86(code, ATTR(node, ast_node, body));
        str_printfln(code, "jmp %s$if_end", label.id);

        if(else_body)
        {
          str_printfln(code, "%s$if_else:", label.id);
          gen_x86(code, else_body);
        }

        str_printfln(code, "%s$if_end:", label.id);
      }
      break;

    case eAstNode_ret_stmt:
      {
        AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
        if(ret_expr)
        {
          gen_x86(code, ret_expr);
        }

        gen_x86_leave_frame(code, ATTR(node, int_val, nesting_depth));
        str_printfln(code, "ret");
      }
      break;

    case eAstNode_break_stmt:
    case eAstNode_continue_stmt:
      {
        AstNode* loop = ATTR(node, ast_node, loop);
        gen_x86_leave_frame(code, ATTR(node, int_val, nesting_depth));

        if(node->kind == eAstNode_break_stmt)
        {
          str_printfln(code, "jmp %s$while_break", ATTR(loop, label, label).id);
        }
        else if(node->kind == eAstNode_continue_stmt)
        {
          str_printfln(code, "jmp %s$while_eval", ATTR(loop, label, label).id);
        }
        else
          assert(0);
      }
      break;

    case eAstNode_empty:
      break; // skip

    case eAstNode_asm_block:
      {
        str_append(code, ATTR(node, str_val, asm_text));
      }
      break;

    default:
      assert(0);
  }
  return success;
}

