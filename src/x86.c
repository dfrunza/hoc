void gen_x86_load_rvalue(String* code, AstNode* node);
bool gen_x86(String* code, AstNode* node);

void gen_x86_leave_frame(String* code, int depth)
{
  if(depth > 0)
  {
    str_printfln(code, "push %d", depth);
    str_printfln(code, "call _rt_leave_frame");
  }
  else if(depth == 0)
  {
    str_printfln(code, "mov esp, ebp");
    str_printfln(code, "pop ebp");
  }
  else
    assert(0);
}

void gen_x86_load_lvalue(String* code, AstNode* node)
{
  switch(node->kind)
  {
#if 0
    case eAstNode_var_occur:
      {
        Symbol* occur_sym = node->var_occur.occur_sym;
        Symbol* decl_sym = occur_sym->decl;

        if(decl_sym->is_static_alloc)
        {
          assert(decl_sym->data_loc >=0 );
          str_printfln(code, "push OFFSET(static_area) + %d", decl_sym->data_loc);
        }
        else
        {
          str_printfln(code, "push ebp");

          int decl_scope_offset = occur_sym->scope->nesting_depth - decl_sym->scope->nesting_depth;
          if(decl_scope_offset > 0)
          {
            // Non-local
            str_printfln(code, "add dword ptr [esp], %d", 2*MACHINE_WORD_SIZE);
            str_printfln(code, "push %d", decl_scope_offset);
            str_printfln(code, "call _rt_load_access_link");

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
#endif

    case eAstNode_un_expr:
      {
        AstNode* operand = node->un_expr.operand;
        if(node->un_expr.op_kind == eOperator_deref)
        {
          assert(operand->ty->kind == eType_pointer);
          gen_x86_load_rvalue(code, operand);
        }
        else
          assert(0);
      }
      break;

    case eAstNode_bin_expr:
      {
        AstNode* left_operand = node->bin_expr.left_operand;
        AstNode* right_operand = node->bin_expr.right_operand;

        if(node->bin_expr.op_kind == eOperator_index)
        {
          gen_x86_load_lvalue(code, left_operand);
          gen_x86_load_rvalue(code, right_operand);

          str_printfln(code, "pop eax");
          str_printfln(code, "mov ebx, %d", node->eval_ty->width);
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
  switch(node->kind)
  {
#if 0
    case eAstNode_var_occur:
      {
        Type* type = ATTR(node, type, eval_type);
        gen_x86_load_lvalue(code, node);
        str_printfln(code, "push %d", type->width);
        str_printfln(code, "call _rt_load");
      }
      break;
#endif

    case eAstNode_call:
      gen_x86(code, node);
      break;

    case eAstNode_lit:
      {
        switch(node->lit.kind)
        {
          case eLiteral_int_val:
            str_printfln(code, "push %d", node->lit.int_val);
            break;
          case eLiteral_bool_val:
            str_printfln(code, "push %d", node->lit.bool_val);
            break;
          case eLiteral_float_val:
            {
              union BitcastF32ToI32
              {
                float32 float32_val;
                int32 int32_val;
              };

              union BitcastF32ToI32 val = {0};
              val.float32_val = node->lit.float_val;
              str_printfln(code, "push %xh ; %f", val.int32_val, val.float32_val);
            }
            break;
          case eLiteral_char_val:
            str_printfln(code, "push %d", node->lit.char_val);
            break;

          default:
            assert(0);
        }
      }
      break;

    case eAstNode_bin_expr:
      {
        if(node->bin_expr.op_kind == eOperator_index)
        {
          gen_x86_load_lvalue(code, node);
          str_printfln(code, "push %d", node->eval_ty->width);
          str_printfln(code, "call _rt_load");
        }
        else
        {
          gen_x86(code, node);
        }
      }
      break;

    case eAstNode_un_expr:
      {
        AstNode* operand = node->un_expr.operand;

        if(node->un_expr.op_kind == eOperator_address_of)
        {
#if 0
          if(operand->kind == eAstNode_var_occur)
          {
            gen_x86_load_lvalue(code, operand);
          }
          else
#endif
          {
            gen_x86_load_rvalue(code, operand);
          }
        }
        else if(node->un_expr.op_kind == eOperator_deref)
        {
          gen_x86_load_lvalue(code, node);
          str_printfln(code, "push %d", node->eval_ty->width);
          str_printfln(code, "call _rt_load");
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
  bool success = true;

  switch(node->kind)
  {
    case eAstNode_module:
      {
        Scope* module_scope = node->module.scope;
        for(ListItem* list_item = module_scope->decls[eSymbol_extern_proc]->first;
            list_item;
            list_item = list_item->next)
        {
          Symbol* decl_sym = ITEM(list_item, symbol);
          gen_x86(code, decl_sym->ast_node);
        }
        for(ListItem* list_item = module_scope->decls[eSymbol_proc]->first;
            list_item;
            list_item = list_item->next)
        {
          Symbol* decl_sym = ITEM(list_item, symbol);
          gen_x86(code, decl_sym->ast_node);
        }

        AstNode* body = node->module.body;
        Scope* body_scope = body->block.scope;

        str_printfln(code, "startup PROC");
        str_printfln(code, "call _rt_module_prologue");
        str_printfln(code, "sub esp, %d ;alloc locals", body_scope->locals_area_size);
        for(ListItem* list_item = body->block.stmts->first;
            list_item;
            list_item = list_item->next)
        {
          AstNode* stmt = ITEM(list_item, ast_node);
          gen_x86(code, stmt);
        }
        gen_x86_leave_frame(code, 0);
        str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE); // access link + dummy IP
        str_printfln(code, "ret");
        str_printfln(code, "startup ENDP");
      }
      break;

    case eAstNode_proc:
      {
        Scope* proc_scope = node->proc.scope;

        char* label = node->proc.label;
        if(node->proc.is_extern)
        {
          str_printfln(code, "EXTERN %s:PROC", label);
        }
        else
        {
          for(ListItem* list_item = proc_scope->decls[eSymbol_extern_proc]->first;
              list_item;
              list_item = list_item->next)
          {
            Symbol* decl_sym = ITEM(list_item, symbol);
            gen_x86(code, decl_sym->ast_node);
          }
          for(ListItem* list_item = proc_scope->decls[eSymbol_proc]->first;
              list_item;
              list_item = list_item->next)
          {
            Symbol* decl_sym = ITEM(list_item, symbol);
            gen_x86(code, decl_sym->ast_node);
          }

          AstNode* body = node->proc.body;
          Scope* body_scope = body->block.scope;
          str_printfln(code, "%s PROC", label);
          str_printfln(code, "push ebp");
          str_printfln(code, "mov ebp, esp");
          str_printfln(code, "sub esp, %d ;alloc locals", body_scope->locals_area_size);
          for(ListItem* list_item = body->block.stmts->first;
              list_item;
              list_item = list_item->next)
          {
            AstNode* stmt = ITEM(list_item, ast_node);
            gen_x86(code, stmt);
          }
          gen_x86_leave_frame(code, 0);
          str_printfln(code, "ret");
          str_printfln(code, "%s ENDP", label);
        }
      }
      break;

    case eAstNode_block:
      {
        Scope* scope = node->block.scope;

        str_printfln(code, "call _rt_block_prologue");
        str_printfln(code, "sub esp, %d", scope->locals_area_size);
        for(ListItem* list_item = node->block.stmts->first;
            list_item;
            list_item = list_item->next)
        {
          AstNode* stmt = ITEM(list_item, ast_node);
          gen_x86(code, stmt);
        }
        gen_x86_leave_frame(code, 0);
        str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE); // access link + dummy IP
      }
      break;

    case eAstNode_call:
      {
        AstNode* proc = node->call.proc;
        Scope* callee_scope = proc->proc.scope;
        char* label = proc->proc.label;

        str_printfln(code, ";%s()", label);
        str_printfln(code, "sub esp, %d ;alloc ret", callee_scope->ret_area_size);
        for(ListItem* list_item = node->call.actual_args->last;
            list_item;
            list_item = list_item->prev)
        {
          gen_x86_load_rvalue(code, ITEM(list_item, ast_node));
        }

        if(proc->proc.is_extern)
        {
          str_printfln(code, "call %s", label);

          if(callee_scope->ret_area_size == MACHINE_WORD_SIZE)
          {
            str_printfln(code, "mov dword ptr [esp], eax ; save the return value");
          }
          else if(callee_scope->ret_area_size != 0)
            assert(0); //FIXME: don't know what to do
        }
        else
        {
          Symbol* occur_sym = node->call.occur_sym;
          Scope* caller_scope = occur_sym->scope;

          str_printfln(code, "push ebp");
          str_printfln(code, "add dword ptr [esp], %d", 2*MACHINE_WORD_SIZE);

          int callee_depth_offset = caller_scope->nesting_depth - callee_scope->nesting_depth;
          if(callee_depth_offset >= 0)
          {
            str_printfln(code, "push %d", callee_depth_offset + 1);
            str_printfln(code, "call _rt_load_access_link");
          }

          str_printfln(code, "call %s", label);
          str_printfln(code, "add esp, %d ;dealloc args", MACHINE_WORD_SIZE + callee_scope->args_area_size);
        }
      }
      break;

    case eAstNode_stmt:
      {
        AstNode* actual_stmt = node->stmt.stmt;
        gen_x86(code, actual_stmt);
        str_printfln(code, "add esp, %d", actual_stmt->eval_ty->width);
      }
      break;

    case eAstNode_bin_expr:
      {
        AstNode* left_operand = node->bin_expr.left_operand;
        AstNode* right_operand = node->bin_expr.right_operand;

        switch(node->bin_expr.op_kind)
        {
          case eOperator_assign:
            {
              gen_x86_load_rvalue(code, right_operand);
              gen_x86_load_lvalue(code, left_operand);
              str_printfln(code, "push %d", node->eval_ty->width);
              str_printfln(code, "call _rt_store");
            }
            break;

          case eOperator_add:
            {
              gen_x86_load_rvalue(code, left_operand);
              gen_x86_load_rvalue(code, right_operand);

              if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
              {
                str_printfln(code, "pop eax");
                str_printfln(code, "add dword ptr [esp], eax");
              }
              else if(types_are_equal(node->eval_ty, basic_type_float))
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
              if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
              {
                gen_x86_load_rvalue(code, left_operand);
                gen_x86_load_rvalue(code, right_operand);

                str_printfln(code, "pop eax");
                str_printfln(code, "sub dword ptr [esp], eax");
              }
              else if(types_are_equal(node->eval_ty, basic_type_float))
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

              if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
              {
                str_printfln(code, "pop eax");
                str_printfln(code, "imul dword ptr [esp]");
                str_printfln(code, "mov dword ptr [esp], eax");
              }
              else if(types_are_equal(node->eval_ty, basic_type_float))
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

              if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
              {
                str_printfln(code, "pop eax");
                str_printfln(code, "cdq"); // extend EAX into EDX
                str_printfln(code, "idiv dword ptr [esp]");
                str_printfln(code, "mov dword ptr [esp], eax");
              }
              else if(types_are_equal(node->eval_ty, basic_type_float))
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

              if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
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

              if(types_are_equal(left_operand->eval_ty, basic_type_char) || types_are_equal(left_operand->eval_ty, basic_type_int))
              {
                if(types_are_equal(left_operand->eval_ty, basic_type_int))
                {
                  str_printfln(code, "pop eax");
                  str_printfln(code, "pop ebx");
                }
                else if(types_are_equal(left_operand->eval_ty, basic_type_char))
                {
                  str_printfln(code, "movzx eax, byte ptr [esp]");
                  str_printfln(code, "movzx ebx, byte ptr [esp+%d]", MACHINE_WORD_SIZE);
                  str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE);
                }

                str_printfln(code, "cmp ebx, eax");
              }
              else if(types_are_equal(left_operand->eval_ty, basic_type_float))
              {
                str_printfln(code, "movss xmm0, dword ptr [esp]");
                str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                str_printfln(code, "comiss xmm0, dword ptr [esp]");
              }
              else
                assert(0);

              Label label = make_unique_label();
              str_printfln(code, "push 1");
              if(node->bin_expr.op_kind == eOperator_eq)
              {
                str_printfln(code, "jz %s$cmp_eq", label.name);
                str_printfln(code, "xor dword ptr [esp], 1");
                str_printfln(code, "%s$cmp_eq:", label.name);
              }
              else if(node->bin_expr.op_kind == eOperator_not_eq)
              {
                str_printfln(code, "jnz %s$cmp_not_eq", label.name);
                str_printfln(code, "xor dword ptr [esp], 1");
                str_printfln(code, "%s$cmp_not_eq:", label.name);
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
              if(types_are_equal(left_operand->eval_ty, basic_type_char) || types_are_equal(left_operand->eval_ty, basic_type_int))
              {
                gen_x86_load_rvalue(code, left_operand);
                gen_x86_load_rvalue(code, right_operand);

                if(types_are_equal(left_operand->eval_ty, basic_type_int))
                {
                  str_printfln(code, "pop eax");
                  str_printfln(code, "pop ebx");
                }
                else if(types_are_equal(left_operand->eval_ty, basic_type_char))
                {
                  str_printfln(code, "movzx eax, byte ptr [esp]");
                  str_printfln(code, "movzx ebx, byte ptr [esp+%d]", MACHINE_WORD_SIZE);
                  str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE);
                }

                str_printfln(code, "cmp ebx, eax");

                Label label = make_unique_label();
                str_printfln(code, "push 1");
                if(node->bin_expr.op_kind == eOperator_less)
                {
                  str_printfln(code, "jl %s$cmp_less", label.name);
                  str_printfln(code, "xor dword ptr [esp], 1");
                  str_printfln(code, "%s$cmp_less:", label.name);
                }
                else if(node->bin_expr.op_kind == eOperator_less_eq)
                {
                  str_printfln(code, "jle %s$cmp_less_eq", label.name);
                  str_printfln(code, "xor dword ptr [esp], 1");
                  str_printfln(code, "%s$cmp_less_eq:", label.name);
                }
                else if(node->bin_expr.op_kind == eOperator_greater)
                {
                  str_printfln(code, "jg %s$cmp_greater", label.name);
                  str_printfln(code, "xor dword ptr [esp], 1");
                  str_printfln(code, "%s$cmp_greater:", label.name);
                }
                else if(node->bin_expr.op_kind == eOperator_greater_eq)
                {
                  str_printfln(code, "jge %s$cmp_greater_eq", label.name);
                  str_printfln(code, "xor dword ptr [esp], 1");
                  str_printfln(code, "%s$cmp_greater_eq:", label.name);
                }
                else
                  assert(0);
              }
              else if(types_are_equal(left_operand->eval_ty, basic_type_float))
              {
                if(node->bin_expr.op_kind == eOperator_less)
                {
                  gen_x86_load_rvalue(code, right_operand);
                  gen_x86_load_rvalue(code, left_operand);
                  str_printfln(code, "movss xmm0, dword ptr [esp]");
                  str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                  str_printfln(code, "cmpss xmm0, dword ptr [esp], 1");
                }
                else if(node->bin_expr.op_kind == eOperator_less_eq)
                {
                  gen_x86_load_rvalue(code, right_operand);
                  gen_x86_load_rvalue(code, left_operand);
                  str_printfln(code, "movss xmm0, dword ptr [esp]");
                  str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                  str_printfln(code, "cmpss xmm0, dword ptr [esp], 2");
                }
                else if(node->bin_expr.op_kind == eOperator_greater)
                {
                  gen_x86_load_rvalue(code, left_operand);
                  gen_x86_load_rvalue(code, right_operand);
                  str_printfln(code, "movss xmm0, dword ptr [esp]");
                  str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                  str_printfln(code, "cmpss xmm0, dword ptr [esp], 1");
                }
                else if(node->bin_expr.op_kind == eOperator_greater_eq)
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
              if(node->bin_expr.op_kind == eOperator_logic_and)
              {
                str_printfln(code, "and eax, 1");
                str_printfln(code, "jz %s$logic_end", label.name);
              }
              else if(node->bin_expr.op_kind == eOperator_logic_or)
              {
                str_printfln(code, "or eax, 0");
                str_printfln(code, "jnz %s$logic_end", label.name);
              }
              else
                assert(0);

              gen_x86_load_rvalue(code, right_operand);

              str_printfln(code, "pop eax");
              if(node->bin_expr.op_kind == eOperator_logic_and)
              {
                str_printfln(code, "and eax, 1");
                str_printfln(code, "jz %s$logic_end", label.name);
              }
              else if(node->bin_expr.op_kind == eOperator_logic_or)
              {
                str_printfln(code, "or eax, 0");
                str_printfln(code, "jnz %s$logic_end", label.name);
              }
              else
                assert(0);

              str_printfln(code, "%s$logic_end:", label.name);
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
              if(node->bin_expr.op_kind == eOperator_bit_and)
              {
                str_printfln(code, "and dword ptr [esp], eax");
              }
              else if(node->bin_expr.op_kind == eOperator_bit_or)
              {
                str_printfln(code, "or dword ptr [esp], eax");
              }
              else if(node->bin_expr.op_kind == eOperator_bit_xor)
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

              if(node->bin_expr.op_kind == eOperator_bit_shift_left)
              {
                str_printfln(code, "shl dword ptr [esp], cl");
              }
              else if(node->bin_expr.op_kind == eOperator_bit_shift_right)
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

              if(types_are_equal(left_operand->eval_ty, basic_type_int)
                  && types_are_equal(right_operand->eval_ty, basic_type_float))
              {
                // int <- float
                str_printfln(code, "cvttss2si eax, dword ptr [esp]");
                str_printfln(code, "mov dword ptr [esp], eax");
              }
              if(types_are_equal(left_operand->eval_ty, basic_type_float)
                  && types_are_equal(right_operand->eval_ty, basic_type_int))
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
        AstNode* operand = node->un_expr.operand;

        if(node->un_expr.op_kind == eOperator_address_of)
        {
          gen_x86_load_rvalue(code, node);
        }
        else if(node->un_expr.op_kind == eOperator_neg)
        {
          gen_x86_load_rvalue(code, operand);

          if(types_are_equal(operand->eval_ty, basic_type_int))
          {
            str_printfln(code, "neg dword ptr [esp]");
          }
          else if(types_are_equal(operand->eval_ty, basic_type_float))
          {
            str_printfln(code, "xor dword ptr [esp], %xh", 0x80000000);
          }
          else
            assert(0);
        }
        else if(node->un_expr.op_kind == eOperator_logic_not)
        {
          assert(0);
        }
        else if(node->un_expr.op_kind == eOperator_deref)
        {
          gen_x86_load_rvalue(code, node);
        }
        else
          assert(0);
      }
      break;

    case eAstNode_while_stmt:
      {
        Label label = node->while_stmt.label = make_unique_label();
        str_printfln(code, "%s$while_eval:", label.name);
        gen_x86_load_rvalue(code, node->while_stmt.cond_expr);

        str_printfln(code, "pop eax");
        str_printfln(code, "and eax, 1");
        str_printfln(code, "jz %s$while_break", label.name);

        if(node->while_stmt.body)
        {
          gen_x86(code, node->while_stmt.body);
        }

        str_printfln(code, "jmp %s$while_eval", label.name);
        str_printfln(code, "%s$while_break:", label.name);
      }
      break;

    case eAstNode_if_stmt:
      {
        gen_x86_load_rvalue(code, node->if_stmt.cond_expr);

        str_printfln(code, "pop eax");
        str_printfln(code, "and eax, 1");

        Label label = make_unique_label();
        if(node->if_stmt.else_body)
        {
          str_printfln(code, "jz %s$if_else", label.name);
        }
        else
        {
          str_printfln(code, "jz %s$if_end", label.name);
        }

        gen_x86(code, node->if_stmt.body);
        str_printfln(code, "jmp %s$if_end", label.name);

        if(node->if_stmt.else_body)
        {
          str_printfln(code, "%s$if_else:", label.name);
          gen_x86(code, node->if_stmt.else_body);
        }

        str_printfln(code, "%s$if_end:", label.name);
      }
      break;

    case eAstNode_ret_stmt:
      {
        if(node->ret_stmt.ret_expr)
        {
          gen_x86(code, node->ret_stmt.ret_expr);
        }
        gen_x86_leave_frame(code, node->ret_stmt.nesting_depth);
        str_printfln(code, "ret");
      }
      break;

    case eAstNode_loop_ctrl:
      {
        AstNode* loop = node->loop_ctrl.loop;
        gen_x86_leave_frame(code, node->loop_ctrl.nesting_depth);
        Label* label = 0;
        if(loop->kind == eAstNode_while_stmt)
        {
          label = &loop->while_stmt.label;
          if(node->loop_ctrl.kind == eLoopCtrl_break)
            str_printfln(code, "jmp %s$while_break", label->name);
          else if(node->loop_ctrl.kind == eLoopCtrl_continue)
            str_printfln(code, "jmp %s$while_eval", label->name);
          else
            assert(0);
        }
        else
          assert(0);
      }
      break;

    case eAstNode_empty:
      break; // skip

    case eAstNode_asm_block:
      str_append(code, node->asm_block.asm_text);
      break;

    default:
      assert(0);
  }
  return success;
}

