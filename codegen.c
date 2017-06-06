void
print_instruction(VmProgram* vm_program, char* code, ...)
{
  static char strbuf[128] = {0};
  va_list args;

  va_start(args, code);
  vm_program->text_len += vsprintf(strbuf, code, args);
  va_end(args);

  str_append(&vm_program->text, strbuf);
  str_append(&vm_program->text, "\n");
  vm_program->text_len++;
}

void
emit_instr_reg(MemoryArena* arena, List* instr_list, Opcode opcode, RegName reg)
{
  Instruction* instr = mem_push_struct(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType_Reg;
  instr->param.reg = reg;
  list_append(arena, instr_list, instr);
}

void
emit_instr_int(MemoryArena* arena, List* instr_list, Opcode opcode, int32 int_val)
{
  Instruction* instr = mem_push_struct(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType_Int32;
  instr->param.int_val = int_val;
  list_append(arena, instr_list, instr);
}

void
emit_instr(MemoryArena* arena, List* instr_list, Opcode opcode)
{
  Instruction* instr = mem_push_struct(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType__Null;
  list_append(arena, instr_list, instr);
}

void
emit_instr_str(MemoryArena* arena, List* instr_list, Opcode opcode, char* str)
{
  assert(str);
  Instruction* instr = mem_push_struct(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType_String;
  instr->param.str = str;
  list_append(arena, instr_list, instr);
}

void gen_load_rvalue(MemoryArena*, List*, AstNode*);
void gen_load_lvalue(MemoryArena*, List*, AstVarOccur*);
void gen_statement(MemoryArena*, List*, AstNode*);

void
gen_bin_expr(MemoryArena* arena, List* code, AstBinExpr* bin_expr)
{
  if(bin_expr->op == AstOpKind_Assign)
  {
    gen_load_rvalue(arena, code, bin_expr->right_operand);

    assert(bin_expr->left_operand->kind = AstNodeKind_VarOccur);
    gen_load_lvalue(arena, code, &bin_expr->left_operand->var_occur);

    emit_instr(arena, code, Opcode_STORE);
  }
  else
  {
    switch(bin_expr->op)
    {
      case AstOpKind_Add:
      case AstOpKind_Sub:
      case AstOpKind_Mul:
      case AstOpKind_Div:
      case AstOpKind_Mod:
      case AstOpKind_LogicEquals:
      case AstOpKind_LogicNotEquals:
      case AstOpKind_LogicLess:
      case AstOpKind_LogicGreater:
      {
        gen_load_rvalue(arena, code, bin_expr->left_operand);
        gen_load_rvalue(arena, code, bin_expr->right_operand);

        if(bin_expr->op == AstOpKind_Add)
        {
          emit_instr(arena, code, Opcode_ADD);
        }
        else if(bin_expr->op == AstOpKind_Sub)
        {
          emit_instr(arena, code, Opcode_SUB);
        }
        else if(bin_expr->op == AstOpKind_Mul)
        {
          emit_instr(arena, code, Opcode_MUL);
        }
        else if(bin_expr->op == AstOpKind_Div)
        {
          emit_instr(arena, code, Opcode_DIV);
        }
        else if(bin_expr->op == AstOpKind_Mod)
        {
          emit_instr(arena, code, Opcode_MOD);
        }
        else if(bin_expr->op == AstOpKind_LogicEquals)
        {
          emit_instr(arena, code, Opcode_CMPEQ);
        }
        else if(bin_expr->op == AstOpKind_LogicNotEquals)
        {
          emit_instr(arena, code, Opcode_CMPNEQ);
        }
        else if(bin_expr->op == AstOpKind_LogicLess)
        {
          emit_instr(arena, code, Opcode_CMPLSS);
        }
        else if(bin_expr->op == AstOpKind_LogicGreater)
        {
          emit_instr(arena, code, Opcode_CMPGRT);
        }
        else
          assert(false);
      } break;

      case AstOpKind_LogicAnd:
      case AstOpKind_LogicOr:
      {
        gen_load_rvalue(arena, code, bin_expr->left_operand);
        emit_instr(arena, code, Opcode_DUP);

        if(bin_expr->op == AstOpKind_LogicAnd)
        {
          emit_instr_str(arena, code, Opcode_JUMPZ, bin_expr->label_end);
        }
        else if(bin_expr->op == AstOpKind_LogicOr)
        {
          emit_instr_str(arena, code, Opcode_JUMPNZ, bin_expr->label_end);
        }
        else
          assert(false);

        emit_instr(arena, code, Opcode_POP);
        gen_load_rvalue(arena, code, bin_expr->right_operand);
        emit_instr_str(arena, code, Opcode_LABEL, bin_expr->label_end);
      } break;

      case AstOpKind_LogicLessEquals:
      case AstOpKind_LogicGreaterEquals:
      {
        gen_load_rvalue(arena, code, bin_expr->left_operand);
        gen_load_rvalue(arena, code, bin_expr->right_operand);
        if(bin_expr->op == AstOpKind_LogicLessEquals)
        {
          emit_instr(arena, code, Opcode_CMPLSS);
        }
        else if(bin_expr->op == AstOpKind_LogicGreaterEquals)
        {
          emit_instr(arena, code, Opcode_CMPGRT);
        }
        else
          assert(false);
        emit_instr(arena, code, Opcode_DUP);

        emit_instr_str(arena, code, Opcode_JUMPNZ, bin_expr->label_end);
        emit_instr(arena, code, Opcode_POP);
        gen_load_rvalue(arena, code, bin_expr->left_operand);
        gen_load_rvalue(arena, code, bin_expr->right_operand);
        emit_instr(arena, code, Opcode_CMPEQ);
        emit_instr_str(arena, code, Opcode_LABEL, bin_expr->label_end);
      } break;

      default:
        assert(false);
    }
  }
}

void gen_unr_expr(MemoryArena* arena, List* code, AstUnrExpr* unr_expr)
{
  gen_load_rvalue(arena, code, unr_expr->operand);
  if(unr_expr->op == AstOpKind_Neg)
  {
    emit_instr(arena, code, Opcode_NEG);
  }
  else if(unr_expr->op == AstOpKind_LogicNot)
  {
    emit_instr_int(arena, code, Opcode_PUSH, 0);
    emit_instr(arena, code, Opcode_CMPEQ);
  }
  else
    assert(false);
}

void
gen_call(MemoryArena* arena, List* code, AstCall* call)
{
  AstProc* proc = &call->proc->proc;
  emit_instr_int(arena, code, Opcode_ALLOC, proc->ret_size);

  for(ListItem* list_item = list_first_item(&call->actual_args);
      list_item;
      list_item = list_item->next)
  {
    AstNode* arg = list_item->elem;
    gen_load_rvalue(arena, code, arg);
  }

  emit_instr_str(arena, code, Opcode_CALL, proc->label);
  emit_instr_int(arena, code, Opcode_POP, proc->args_size); // discard args
}

void
gen_load_lvalue(MemoryArena* arena, List* code, AstVarOccur* var_occur)
{
  DataArea* data = var_occur->data;
  AccessLink* link = var_occur->link;

  emit_instr_reg(arena, code, Opcode_PUSH, RegName_FP);
  if(link) 
  {
    // this is a non-local
    assert(link->data.loc < 0); // relative to FP
    emit_instr_int(arena, code, Opcode_PUSH, link->data.loc);
    emit_instr(arena, code, Opcode_ADD);
    emit_instr(arena, code, Opcode_LOAD); // access link is on the stack now
  }
  emit_instr_int(arena, code, Opcode_PUSH, data->loc);
  emit_instr(arena, code, Opcode_ADD);
}

void
gen_load_rvalue(MemoryArena* arena, List* code, AstNode* node)
{
  if(node->kind == AstNodeKind_VarOccur)
  {
    gen_load_lvalue(arena, code, &node->var_occur);
    emit_instr(arena, code, Opcode_LOAD);
  }
  else if(node->kind == AstNodeKind_Call)
  {
    gen_call(arena, code, &node->call);
  }
  else if(node->kind == AstNodeKind_Literal)
  {
    if(node->literal.kind == AstLiteralKind_Int
       || node->literal.kind == AstLiteralKind_Bool)
    {
      emit_instr_int(arena, code, Opcode_PUSH, node->literal.int_val);
    }
    else if(node->literal.kind == AstLiteralKind_Float)
    {
      //FIXME: Emit proper float value.
      emit_instr_int(arena, code, Opcode_PUSH, (int)node->literal.float_val);
    }
    else
      assert(false);
  }
  else if(node->kind == AstNodeKind_BinExpr)
  {
    gen_bin_expr(arena, code, &node->bin_expr);
  }
  else if(node->kind == AstNodeKind_UnrExpr)
  {
    gen_unr_expr(arena, code, &node->unr_expr);
  }
  else if(node->kind == AstNodeKind_Cast)
  {
    gen_load_rvalue(arena, code, node->cast.expr);
  }
  else
    assert(false);
}

void
gen_return_stmt(MemoryArena* arena, List* code, AstReturnStmt* ret_stmt)
{
  if(ret_stmt->assgn_expr)
  {
    gen_bin_expr(arena, code, ret_stmt->assgn_expr);
    emit_instr(arena, code, Opcode_POP);
  }

  AstProc* proc = ret_stmt->proc;
  int depth = ret_stmt->depth;
  while(depth--)
    emit_instr(arena, code, Opcode_LEAVE);
  emit_instr_str(arena, code, Opcode_GOTO, proc->label_end);
}

void
gen_break_stmt(MemoryArena* arena, List* code, AstBreakStmt* break_stmt)
{
  AstWhileStmt* while_stmt = break_stmt->while_stmt;
  int depth = break_stmt->depth;
  while(depth--)
    emit_instr(arena, code, Opcode_LEAVE);
  emit_instr_str(arena, code, Opcode_GOTO, while_stmt->label_break);
}

void
gen_block(MemoryArena* arena, List* code, AstBlock* block)
{
  for(ListItem* list_item = list_first_item(&block->access_links);
      list_item;
      list_item = list_item->next)
  {
    AccessLink* link = list_item->elem;
    emit_instr_reg(arena, code, Opcode_PUSH, RegName_FP);
    assert(link->actv_rec_offset > 0);
    int offset = link->actv_rec_offset - 1;
    while(offset--)
    {
      emit_instr(arena, code, Opcode_DECR); // TODO: explain why
      emit_instr(arena, code, Opcode_LOAD);
    }
  }

  emit_instr(arena, code, Opcode_ENTER);

  emit_instr_int(arena, code, Opcode_ALLOC, block->locals_size);

  for(ListItem* list_item = list_first_item(&block->stmt_list);
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = list_item->elem;
    gen_statement(arena, code, node);
  }

  emit_instr(arena, code, Opcode_LEAVE);
}

void
gen_proc(MemoryArena* arena, List* code, AstProc* proc)
{
  emit_instr_str(arena, code, Opcode_LABEL, proc->label);
  emit_instr_int(arena, code, Opcode_ALLOC, proc->locals_size);

  AstBlock* body_block = &proc->body->block;
  for(ListItem* list_item = list_first_item(&body_block->stmt_list);
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = list_item->elem;
    gen_statement(arena, code, node);
  }

  emit_instr_str(arena, code, Opcode_LABEL, proc->label_end);
  emit_instr(arena, code, Opcode_RETURN);
}

void
gen_if_stmt(MemoryArena* arena, List* code, AstIfStmt* if_stmt)
{
  gen_load_rvalue(arena, code, if_stmt->cond_expr);

  if(if_stmt->else_body)
    emit_instr_str(arena, code, Opcode_JUMPZ, if_stmt->label_else);
  else
    emit_instr_str(arena, code, Opcode_JUMPZ, if_stmt->label_end);

  if(if_stmt->body->kind == AstNodeKind_Block)
    gen_block(arena, code, &if_stmt->body->block);
  else
    gen_statement(arena, code, if_stmt->body);

  emit_instr_str(arena, code, Opcode_GOTO, if_stmt->label_end);

  if(if_stmt->else_body)
  {
    emit_instr_str(arena, code, Opcode_LABEL, if_stmt->label_else);
    AstNode* else_body = if_stmt->else_body;
    if(else_body->kind == AstNodeKind_Block)
      gen_block(arena, code, &else_body->block);
    else
      gen_statement(arena, code, else_body);
  }

  emit_instr_str(arena, code, Opcode_LABEL, if_stmt->label_end);
}

void
gen_while_stmt(MemoryArena* arena, List* code, AstWhileStmt* while_stmt)
{
  emit_instr_str(arena, code, Opcode_LABEL, while_stmt->label_eval);
  gen_load_rvalue(arena, code, while_stmt->cond_expr);
  emit_instr_str(arena, code, Opcode_JUMPZ, while_stmt->label_break);
  if(while_stmt->body->kind == AstNodeKind_Block)
    gen_block(arena, code, &while_stmt->body->block);
  else
    gen_statement(arena, code, while_stmt->body);
  emit_instr_str(arena, code, Opcode_GOTO, while_stmt->label_eval);

  emit_instr_str(arena, code, Opcode_LABEL, while_stmt->label_break);
}

void
gen_print_stmt(MemoryArena* arena, List* code, AstPrintStmt* print_stmt)
{
  if(print_stmt->expr)
  {
    gen_load_rvalue(arena, code, print_stmt->expr);
    emit_instr(arena, code, Opcode_PRINT);
  }
  if(print_stmt->new_line)
    emit_instr(arena, code, Opcode_PRINTNL);
}

void
gen_empty_stmt(MemoryArena* arena, List* code)
{
  emit_instr(arena, code, Opcode_NOOP);
}

void
gen_statement(MemoryArena* arena, List* code, AstNode* stmt_node)
{
  if(stmt_node->kind == AstNodeKind_BinExpr)
  {
    AstBinExpr* bin_expr = &stmt_node->bin_expr;
    assert(bin_expr->op == AstOpKind_Assign);
    gen_bin_expr(arena, code, bin_expr);
    emit_instr(arena, code, Opcode_POP);
  }
  else if(stmt_node->kind == AstNodeKind_Call)
  {
    gen_call(arena, code, &stmt_node->call);
    emit_instr(arena, code, Opcode_POP);
  }
  else if(stmt_node->kind == AstNodeKind_ReturnStmt)
  {
    gen_return_stmt(arena, code, &stmt_node->ret_stmt);
  }
  else if(stmt_node->kind == AstNodeKind_BreakStmt)
  {
    gen_break_stmt(arena, code, &stmt_node->break_stmt);
  }
  else if(stmt_node->kind == AstNodeKind_Noop)
  {
    emit_instr(arena, code, Opcode_NOOP);
  }
  else if(stmt_node->kind == AstNodeKind_IfStmt)
  {
    gen_if_stmt(arena, code, &stmt_node->if_stmt);
  }
  else if(stmt_node->kind == AstNodeKind_WhileStmt)
  {
    gen_while_stmt(arena, code, &stmt_node->while_stmt);
  }
  else if(stmt_node->kind == AstNodeKind_PrintStmt)
  {
    gen_print_stmt(arena, code, &stmt_node->print_stmt);
  }
  else if(stmt_node->kind == AstNodeKind_EmptyStmt)
  {
    gen_empty_stmt(arena, code);
  }
  else
    assert(false);
}

void
gen_module(MemoryArena* arena, List* code, AstModule* module)
{
  gen_call(arena, code, &module->main_call->call);
  emit_instr(arena, code, Opcode_HALT);

  for(ListItem* list_item = list_first_item(&module->proc_list);
      list_item;
      list_item = list_item->next)
  {
    AstNode* proc_node = list_item->elem;
    assert(proc_node->kind == AstNodeKind_Proc);
    AstProc* proc = &proc_node->proc;

    gen_proc(arena, code, proc);
  }
}

char*
get_regname_str(RegName reg)
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
    assert(false);
  return regname;
}

void
print_code(VmProgram* vm_program)
{
  for(ListItem* list_item = list_first_item(&vm_program->instr_list);
      list_item;
      list_item = list_item->next)
  {
    Instruction* instr = list_item->elem;
    switch(instr->opcode)
    {
      case Opcode_PUSH:
      {
        if(instr->param_type == ParamType_Reg)
          print_instruction(vm_program, "push %s", get_regname_str(instr->param.reg));
        else if(instr->param_type == ParamType_Int32)
          print_instruction(vm_program, "push %d", instr->param.int_val);
        else
          assert(false);
      } break;

      case Opcode_POP:
      {
        if(instr->param_type == ParamType_Reg)
          print_instruction(vm_program, "pop %s", get_regname_str(instr->param.reg));
        else if(instr->param_type == ParamType__Null)
          print_instruction(vm_program, "pop");
        else if(instr->param_type == ParamType_Int32)
          print_instruction(vm_program, "pop %d", instr->param.int_val);
        else
          assert(false);
      } break;

      case Opcode_DUP:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "dup");
      } break;

      case Opcode_ADD:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "add");
      } break;

      case Opcode_SUB:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "sub");
      } break;

      case Opcode_MUL:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "mul");
      } break;

      case Opcode_DIV:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "div");
      } break;

      case Opcode_MOD:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "mod");
      } break;

      case Opcode_NEG:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "neg");
      } break;

      case Opcode_LOAD:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "load");
      } break;

      case Opcode_STORE:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "store");
      } break;

      case Opcode_LABEL:
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "label %s", instr->param.str);
      } break;

      case Opcode_RETURN:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "return");
      } break;

      case Opcode_ALLOC:
      {
        assert(instr->param_type == ParamType_Int32);
        print_instruction(vm_program, "alloc %d", instr->param.int_val);
      } break;

      case Opcode_CALL:
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "call %s", instr->param.str);
      } break;

      case Opcode_HALT:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "halt");
      } break;

      case Opcode_GOTO:
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "goto %s", instr->param.str);
      } break;

      case Opcode_JUMPZ:
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "jumpz %s", instr->param.str);
      } break;

      case Opcode_JUMPNZ:
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "jumpnz %s", instr->param.str);
      } break;

      case Opcode_DECR:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "decr");
      } break;

      case Opcode_ENTER:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "enter");
      } break;

      case Opcode_LEAVE:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "leave");
      } break;

      case Opcode_NOOP:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "noop");
      } break;

      case Opcode_CMPEQ:
      case Opcode_CMPNEQ:
      case Opcode_CMPLSS:
      case Opcode_CMPGRT:
      {
        assert(instr->param_type == ParamType__Null);
        if(instr->opcode == Opcode_CMPEQ)
          print_instruction(vm_program, "cmpeq");
        else if(instr->opcode == Opcode_CMPNEQ)
          print_instruction(vm_program, "cmpneq");
        else if(instr->opcode == Opcode_CMPLSS)
          print_instruction(vm_program, "cmplss");
        else if(instr->opcode == Opcode_CMPGRT)
          print_instruction(vm_program, "cmpgrt");
        else
          assert(false);
      } break;

      case Opcode_AND:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "and");
      } break;

      case Opcode_OR:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "or");
      } break;

      case Opcode_NOT:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "not");
      } break;

      case Opcode_PRINT:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "print");
      } break;

      case Opcode_PRINTNL:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "printnl");
      } break;

      default:
        assert(false);
    }
  }
}

