void gen_load_rvalue(List*, AstNode*);
void gen_load_lvalue(List*, AstNode*);
void gen_statement(List*, AstNode*);

void print_instruction(VmProgram* vm_program, char* code, ...)
{
  static char strbuf[128] = {0};
  va_list args;

  va_start(args, code);
  vm_program->text_len += h_vsprintf(strbuf, code, args);
  va_end(args);

  str_append(&vm_program->text, strbuf);
  str_append(&vm_program->text, "\n");
  vm_program->text_len++;
}

void emit_instr_reg(List* instr_list, Opcode opcode, RegName reg)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = ParamType_Reg;
  instr->param.reg = reg;
  append_list_elem(instr_list, instr, List_vm_instr);
}

void emit_instr_int(List* instr_list, Opcode opcode, int32 int_val)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = ParamType_Int32;
  instr->param.int_val = int_val;
  append_list_elem(instr_list, instr, List_vm_instr);
}

void emit_instr_float(List* instr_list, Opcode opcode, float32 float_val)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = ParamType_Float32;
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
  instr->param_type = ParamType_String;
  instr->param.str = str;
  append_list_elem(instr_list, instr, List_vm_instr);
}

#if 0
void gen_bin_expr(List* code, AstBinExpr* bin_expr)
{
  AstNode* left_operand = bin_expr->left_operand;
  AstNode* right_operand = bin_expr->right_operand;

  // assertion not holding for pointer arithmetic
  //assert(types_are_equal(left_operand->type, right_operand->type));

  if(bin_expr->op == AstOpKind_Assign)
  {
    gen_load_rvalue(code, right_operand);

    if(left_operand->kind == AstNodeKind_AstVarOccur)
      gen_load_lvalue(code, left_operand);
    else if(left_operand->kind == AstNodeKind_AstUnaryExpr)
    {
      AstUnaryExpr* unr_expr = (AstUnaryExpr*)left_operand;
      if(unr_expr->op == AstOpKind_PointerDeref)
        gen_load_rvalue(code, unr_expr->operand);
      else
        assert(0);
    }

    emit_instr_int(code, Opcode_STORE, compute_type_width(left_operand->type));
  }
  else
  {
    switch(bin_expr->op)
    {
      case AstOpKind_Add:
      {
        gen_load_rvalue(code, left_operand);
        gen_load_rvalue(code, right_operand);
        if(types_are_equal(left_operand->type, basic_type_int)
           || left_operand->type->kind == TypeKind_Pointer)
          emit_instr(code, Opcode_ADD_INT);
        else if(types_are_equal(left_operand->type, basic_type_float))
          emit_instr(code, Opcode_ADD_FLOAT);
        else
          assert(0);
      } break;
      
      case AstOpKind_Sub:
      {
        gen_load_rvalue(code, left_operand);
        gen_load_rvalue(code, right_operand);
        if(types_are_equal(left_operand->type, basic_type_int))
          emit_instr(code, Opcode_SUB_INT);
        else if(types_are_equal(left_operand->type, basic_type_float))
          emit_instr(code, Opcode_SUB_FLOAT);
        else
          assert(0);
      } break;
      
      case AstOpKind_Mul:
      {
        gen_load_rvalue(code, left_operand);
        gen_load_rvalue(code, right_operand);
        if(types_are_equal(left_operand->type, basic_type_int))
          emit_instr(code, Opcode_MUL_INT);
        else if(types_are_equal(left_operand->type, basic_type_float))
          emit_instr(code, Opcode_MUL_FLOAT);
        else
          assert(0);
      } break;

      case AstOpKind_Div:
      {
        gen_load_rvalue(code, left_operand);
        gen_load_rvalue(code, right_operand);
        if(types_are_equal(left_operand->type, basic_type_int))
          emit_instr(code, Opcode_DIV_INT);
        else if(types_are_equal(left_operand->type, basic_type_float))
          emit_instr(code, Opcode_DIV_FLOAT);
        else
          assert(0);
      } break;

      case AstOpKind_Mod:
      {
        gen_load_rvalue(code, left_operand);
        gen_load_rvalue(code, right_operand);
        if(types_are_equal(left_operand->type, basic_type_int))
          emit_instr(code, Opcode_MOD_INT);
        else
          assert(0);
      } break;

      case AstOpKind_Equals:
      case AstOpKind_NotEquals:
      case AstOpKind_Less:
      case AstOpKind_Greater:
      {
        gen_load_rvalue(code, left_operand);
        gen_load_rvalue(code, right_operand);
        if(bin_expr->op == AstOpKind_Equals)
        {
          if(types_are_equal(left_operand->type, basic_type_char))
            emit_instr(code, Opcode_CMPEQ_CHAR);
          else if(types_are_equal(left_operand->type, basic_type_int))
            emit_instr(code, Opcode_CMPEQ_INT);
          else if(types_are_equal(left_operand->type, basic_type_float))
            emit_instr(code, Opcode_CMPEQ_FLOAT);
          else
            assert(0);
        }
        else if(bin_expr->op == AstOpKind_NotEquals)
        {
          if(types_are_equal(left_operand->type, basic_type_char))
            emit_instr(code, Opcode_CMPNEQ_CHAR);
          else if(types_are_equal(left_operand->type, basic_type_int))
            emit_instr(code, Opcode_CMPNEQ_INT);
          else if(types_are_equal(left_operand->type, basic_type_float))
            emit_instr(code, Opcode_CMPNEQ_FLOAT);
          else
            assert(0);
        }
        else if(bin_expr->op == AstOpKind_Less)
        {
          if(types_are_equal(left_operand->type, basic_type_char))
            emit_instr(code, Opcode_CMPLSS_CHAR);
          else if(types_are_equal(left_operand->type, basic_type_int))
            emit_instr(code, Opcode_CMPLSS_INT);
          else if(types_are_equal(left_operand->type, basic_type_float))
            emit_instr(code, Opcode_CMPLSS_FLOAT);
          else
            assert(0);
        }
        else if(bin_expr->op == AstOpKind_Greater)
        {
          if(types_are_equal(left_operand->type, basic_type_char))
            emit_instr(code, Opcode_CMPGRT_CHAR);
          else if(types_are_equal(left_operand->type, basic_type_int))
            emit_instr(code, Opcode_CMPGRT_INT);
          else if(types_are_equal(left_operand->type, basic_type_float))
            emit_instr(code, Opcode_CMPGRT_FLOAT);
          else
            assert(0);
        }
        else
          assert(0);
      } break;

      case AstOpKind_LogicAnd:
      case AstOpKind_LogicOr:
      {
        gen_load_rvalue(code, left_operand);
        emit_instr(code, Opcode_DUP);

        if(bin_expr->op == AstOpKind_LogicAnd)
          emit_instr_str(code, Opcode_JUMPZ, bin_expr->label_end);
        else if(bin_expr->op == AstOpKind_LogicOr)
          emit_instr_str(code, Opcode_JUMPNZ, bin_expr->label_end);
        else
          assert(0);
        emit_instr_int(code, Opcode_GROW, -compute_type_width(left_operand->type));

        gen_load_rvalue(code, right_operand);
        emit_instr(code, Opcode_DUP);

        if(bin_expr->op == AstOpKind_LogicAnd)
          emit_instr_str(code, Opcode_JUMPZ, bin_expr->label_end);
        else if(bin_expr->op == AstOpKind_LogicOr)
          emit_instr_str(code, Opcode_JUMPNZ, bin_expr->label_end);
        else
          assert(0);

        emit_instr_str(code, Opcode_LABEL, bin_expr->label_end);
      } break;

      case AstOpKind_LessEquals:
      case AstOpKind_GreaterEquals:
      {
        gen_load_rvalue(code, left_operand);
        gen_load_rvalue(code, right_operand);
        if(bin_expr->op == AstOpKind_LessEquals)
        {
          if(types_are_equal(left_operand->type, basic_type_char))
            emit_instr(code, Opcode_CMPLSS_CHAR);
          else if(types_are_equal(left_operand->type, basic_type_int))
            emit_instr(code, Opcode_CMPLSS_INT);
          else if(types_are_equal(left_operand->type, basic_type_float))
            emit_instr(code, Opcode_CMPLSS_FLOAT);
          else
            assert(0);
        }
        else if(bin_expr->op == AstOpKind_GreaterEquals)
        {
          if(types_are_equal(left_operand->type, basic_type_char))
            emit_instr(code, Opcode_CMPGRT_CHAR);
          else if(types_are_equal(left_operand->type, basic_type_int))
            emit_instr(code, Opcode_CMPGRT_INT);
          else if(types_are_equal(left_operand->type, basic_type_float))
            emit_instr(code, Opcode_CMPGRT_FLOAT);
          else
            assert(0);
        }
        else
          assert(0);
        emit_instr(code, Opcode_DUP);

        emit_instr_str(code, Opcode_JUMPNZ, bin_expr->label_end);
        emit_instr_int(code, Opcode_GROW, -compute_type_width(left_operand->type));

        gen_load_rvalue(code, left_operand);
        gen_load_rvalue(code, right_operand);
        if(types_are_equal(left_operand->type, basic_type_char))
          emit_instr(code, Opcode_CMPEQ_CHAR);
        else if(types_are_equal(left_operand->type, basic_type_int))
          emit_instr(code, Opcode_CMPEQ_INT);
        else if(types_are_equal(left_operand->type, basic_type_float))
          emit_instr(code, Opcode_CMPEQ_FLOAT);
        else
          assert(0);

        emit_instr_str(code, Opcode_LABEL, bin_expr->label_end);
      } break;

      default:
        assert(0);
    }
  }
}

void gen_unr_expr(List* code, AstUnaryExpr* unr_expr)
{
  if(unr_expr->op == AstOpKind_AddressOf)
  {
    AstNode* operand = unr_expr->operand;
    if(operand->kind == AstNodeKind_AstVarOccur)
      gen_load_lvalue(code, operand);
    else
      gen_load_rvalue(code, operand);
  }
  else
  {
    gen_load_rvalue(code, unr_expr->operand);

    if(unr_expr->op == AstOpKind_Neg)
    {
      if(types_are_equal(unr_expr->type, basic_type_int))
         emit_instr(code, Opcode_NEG_INT);
      else if(types_are_equal(unr_expr->type, basic_type_float))
         emit_instr(code, Opcode_NEG_FLOAT);
      else
        assert(0);
    }
    else if(unr_expr->op == AstOpKind_LogicNot)
    {
      emit_instr_int(code, Opcode_PUSH_INT, 0);
      emit_instr( code, Opcode_CMPEQ_INT);
    }
    else if(unr_expr->op == AstOpKind_IntToFloat)
      emit_instr(code, Opcode_INT_TO_FLOAT);
    else if(unr_expr->op == AstOpKind_FloatToInt)
      emit_instr(code, Opcode_FLOAT_TO_INT);
    else if(unr_expr->op == AstOpKind_PointerDeref)
    {
      assert(unr_expr->operand->type->kind == TypeKind_Pointer);
      emit_instr_int(code, Opcode_LOAD, compute_type_width(unr_expr->type));
    }
    else
      assert(0);
  }
}

void gen_call(List* code, AstCall* call)
{
  AstProc* proc = (AstProc*)call->proc_sym->ast;
  assert(proc->kind == AstNodeKind_AstProc);

  emit_instr_int(code, Opcode_GROW, proc->ret_size);

  AstBlock* block = (AstBlock*)proc->body;

  List* args_list = &call->args->list;
  for(ListItem* list_item = args_list->first;
      list_item;
      list_item = list_item->next)
  {
    gen_load_rvalue(code, (AstNode*)list_item->elem);
  }

  emit_instr_str(code, Opcode_CALL, proc->label);
  emit_instr_int(code, Opcode_GROW, -block->links_size);
  emit_instr_int(code, Opcode_GROW, -proc->args_size);
}

void gen_load_lvalue(List* code, AstNode* ast)
{
  if(ast->kind == AstNodeKind_AstVarOccur)
  {
    AstVarOccur* var_occur = (AstVarOccur*)ast;
    DataArea* data = var_occur->data;
    AccessLink* link = var_occur->link;

    if(var_occur->decl_block_offset >= 0)
    {
      emit_instr_reg(code, Opcode_PUSH_REG, RegName_FP);
      if(link) 
      {
        // this is a non-local
        assert(link->data.loc < 0); // relative to FP
        emit_instr_int(code, Opcode_PUSH_INT, link->data.loc);
        emit_instr(code, Opcode_ADD_INT);
        emit_instr_int(code, Opcode_LOAD, 4); // access link is on the stack now
      }
      emit_instr_int(code, Opcode_PUSH_INT, data->loc);
      emit_instr(code, Opcode_ADD_INT);
    }
    else
      emit_instr_int(code, Opcode_PUSH_INT, data->loc);
  }
  else
    assert(0);
}

void gen_load_rvalue(List* code, AstNode* ast)
{
  if(ast->kind == AstNodeKind_AstVarOccur)
  {
    AstVarOccur* var_occur = (AstVarOccur*)ast;
    gen_load_lvalue(code, (AstNode*)var_occur);
    emit_instr_int(code, Opcode_LOAD, compute_type_width(var_occur->type));
  }
  else if(ast->kind == AstNodeKind_AstCall)
    gen_call(code, (AstCall*)ast);
  else if(ast->kind == AstNodeKind_AstLiteral)
  {
    AstLiteral* literal = (AstLiteral*)ast;
    if(literal->lit_kind == AstLiteralKind_Int || literal->lit_kind == AstLiteralKind_Bool)
      emit_instr_int(code, Opcode_PUSH_INT, literal->int_val);
    else if(literal->lit_kind == AstLiteralKind_Char)
      emit_instr_int(code, Opcode_PUSH_CHAR, literal->char_val);
    else if(literal->lit_kind == AstLiteralKind_Float)
      emit_instr_float(code, Opcode_PUSH_FLOAT, literal->float_val);
    else
      assert(0);
  }
  else if(ast->kind == AstNodeKind_AstBinExpr)
    gen_bin_expr(code, (AstBinExpr*)ast);
  else if(ast->kind == AstNodeKind_AstUnaryExpr)
    gen_unr_expr(code, (AstUnaryExpr*)ast);
  else if(ast->kind == AstNodeKind_AstNew)
  {
    AstNew* new_ast = (AstNew*)ast;
    gen_load_rvalue(code, (AstNode*)new_ast->size_expr);
    emit_instr(code, Opcode_NEW);
  }
  else
    assert(0);
}

void gen_return_stmt(List* code, AstReturnStmt* ret_stmt)
{
  if(ret_stmt->assign_expr)
  {
    AstBinExpr* assign_expr = (AstBinExpr*)ret_stmt->assign_expr;
    assert(assign_expr->kind == AstNodeKind_AstBinExpr);
    gen_bin_expr(code, assign_expr);
    emit_instr_int(code, Opcode_GROW, -compute_type_width(assign_expr->type));
  }

  AstProc* proc = (AstProc*)ret_stmt->proc;
  int depth = ret_stmt->nesting_depth;
  while(depth--)
    emit_instr(code, Opcode_LEAVE);
  emit_instr_str(code, Opcode_GOTO, proc->label_end);
}

void gen_break_stmt(List* code, AstBreakStmt* break_stmt)
{
  char* label_break = 0;
  AstNode* loop = break_stmt->loop;
  if(loop->kind == AstNodeKind_AstWhileStmt)
    label_break = ((AstWhileStmt*)loop)->label_break;
  else if(loop->kind == AstNodeKind_AstForStmt)
    label_break = ((AstForStmt*)loop)->label_break;
  else
    assert(0);

  int depth = break_stmt->nesting_depth;
  while(depth-- > 0)
    emit_instr(code, Opcode_LEAVE);
  emit_instr_str(code, Opcode_GOTO, label_break);
}

void gen_block(List* code, AstBlock* block)
{
  for(ListItem* list_item = block->access_links.first;
      list_item;
      list_item = list_item->next)
  {
    AccessLink* link = (AccessLink*)list_item->elem;
    emit_instr_reg(code, Opcode_PUSH_REG, RegName_FP);
    assert(link->actv_rec_offset > 0);
    int offset = link->actv_rec_offset - 1;
    while(offset--)
    {
      emit_instr_int(code, Opcode_PUSH_INT, -4);
      emit_instr(code, Opcode_ADD_INT); // Get the FP of the previous activ. record
      emit_instr_int(code, Opcode_LOAD, 4);
    }
  }

  emit_instr(code, Opcode_ENTER);

  emit_instr_int(code, Opcode_GROW, block->locals_size);

  for(ListItem* list_item = block->node_list.first;
      list_item;
      list_item = list_item->next)
  {
    gen_statement(code, (AstNode*)list_item->elem);
  }

  emit_instr(code, Opcode_LEAVE);
  emit_instr_int(code, Opcode_GROW, -block->links_size);
}

void gen_proc(List* code, AstProc* proc)
{
  AstBlock* block = (AstBlock*)proc->body;
  emit_instr_str(code, Opcode_LABEL, proc->label);
  emit_instr_int(code, Opcode_GROW, block->locals_size);

  for(ListItem* list_item = block->node_list.first;
      list_item;
      list_item = list_item->next)
  {
    gen_statement(code, (AstNode*)list_item->elem);
  }

  emit_instr_str(code, Opcode_LABEL, proc->label_end);
  emit_instr(code, Opcode_RETURN);
}

void gen_if_stmt(List* code, AstIfStmt* if_stmt)
{
  gen_load_rvalue(code, if_stmt->cond_expr);

  if(if_stmt->else_body)
    emit_instr_str(code, Opcode_JUMPZ, if_stmt->label_else);
  else
    emit_instr_str(code, Opcode_JUMPZ, if_stmt->label_end);

  if(if_stmt->body->kind == AstNodeKind_AstBlock)
    gen_block(code, (AstBlock*)if_stmt->body);
  else
    gen_statement(code, if_stmt->body);

  emit_instr_str(code, Opcode_GOTO, if_stmt->label_end);

  if(if_stmt->else_body)
  {
    emit_instr_str(code, Opcode_LABEL, if_stmt->label_else);
    AstNode* else_body = if_stmt->else_body;
    if(else_body->kind == AstNodeKind_AstBlock)
      gen_block(code, (AstBlock*)else_body);
    else
      gen_statement(code, else_body);
  }

  emit_instr_str(code, Opcode_LABEL, if_stmt->label_end);
}

void gen_while_stmt(List* code, AstWhileStmt* while_stmt)
{
  emit_instr_str(code, Opcode_LABEL, while_stmt->label_eval);
  gen_load_rvalue(code, while_stmt->cond_expr);
  emit_instr_str(code, Opcode_JUMPZ, while_stmt->label_break);
  if(while_stmt->body->kind == AstNodeKind_AstBlock)
    gen_block(code, (AstBlock*)while_stmt->body);
  else
    gen_statement(code, while_stmt->body);
  emit_instr_str(code, Opcode_GOTO, while_stmt->label_eval);

  emit_instr_str(code, Opcode_LABEL, while_stmt->label_break);
}

void gen_empty_stmt(List* code)
{
  emit_instr(code, Opcode_NOOP);
}

void gen_statement(List* code, AstNode* ast)
{
  if(ast->kind == AstNodeKind_AstBinExpr)
  {
    gen_bin_expr(code, (AstBinExpr*)ast);
    emit_instr_int(code, Opcode_GROW, -compute_type_width(ast->type));
  }
  else if(ast->kind == AstNodeKind_AstCall)
  {
    AstCall* call = (AstCall*)ast;
    gen_call(code, call);

    AstProc* proc = (AstProc*)call->proc_sym->ast;
    assert(proc->kind == AstNodeKind_AstProc);

    if(proc->ret_size > 0)
    {
      assert(compute_type_width(call->type) == proc->ret_size);
      emit_instr_int(code, Opcode_GROW, -proc->ret_size);
    }
  }
  else if(ast->kind == AstNodeKind_AstVarDecl)
  {
    AstVarDecl* var_decl = (AstVarDecl*)ast;
    if(var_decl->assign_expr)
      gen_statement(code, (AstNode*)var_decl->assign_expr);
  }
  else if(ast->kind == AstNodeKind_AstReturnStmt)
    gen_return_stmt(code, (AstReturnStmt*)ast);
  else if(ast->kind == AstNodeKind_AstBreakStmt)
    gen_break_stmt(code, (AstBreakStmt*)ast);
  else if(ast->kind == AstNodeKind_AstIfStmt)
    gen_if_stmt(code, (AstIfStmt*)ast);
  else if(ast->kind == AstNodeKind_AstWhileStmt)
    gen_while_stmt(code, (AstWhileStmt*)ast);
  else if(ast->kind == AstNodeKind_AstPutc)
  {
    AstPutc* putc_ast = (AstPutc*)ast;
    gen_load_rvalue(code, putc_ast->expr);
    emit_instr(code, Opcode_PUTC);
  }
  else if(ast->kind == AstNodeKind_AstBlock)
  {
    gen_block(code, (AstBlock*)ast);
  }
  else if(ast->kind == AstNodeKind_AstEmptyStmt)
    gen_empty_stmt(code);
  else
    assert(0);
}

void codegen(List* code, uint8** data, int* data_size, AstModule* module)
{
  AstBlock* module_block = module->body;

  for(ListItem* list_item = module_block->node_list.first;
      list_item;
      list_item = list_item->next)
  {
    AstNode* ast = (AstNode*)list_item->elem;
    gen_statement(code, ast);
  }
  emit_instr(code, Opcode_HALT);

  *data_size = module_block->locals_size + 1/*null ptr cell*/;
  *data = mem_push_count(arena, uint8, *data_size);

  for(ListItem* list_item = module_block->local_decls.first;
      list_item;
      list_item = list_item->next)
  {
    AstVarDecl* var_decl = (AstVarDecl*)list_item->elem;
    assert(var_decl->kind == AstNodeKind_AstVarDecl);

    AstNode* init_expr = var_decl->init_expr;
    if(init_expr && init_expr->kind == AstNodeKind_AstString)
    {
      AstString* str = (AstString*)init_expr;
      assert(str->len+1 == var_decl->data.size);
      char* loc = (char*)(*data + var_decl->data.loc);
      char* s = str->str;
      do
        *loc++ = *s++;
      while(*s);
      *loc = '\0';
    }
  }

  for(ListItem* list_item = module->proc_defs.first;
      list_item;
      list_item = list_item->next)
  {
    AstProc* proc = (AstProc*)list_item->elem;
    gen_proc(code, proc);
  }
}
#endif

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
  for(ListItem* list_item = vm_program->instr_list->first;
      list_item;
      list_item = list_item->next)
  {
    Instruction* instr = (Instruction*)list_item->elem;
    switch(instr->opcode)
    {
      case Opcode_PUSH_CHAR:
      {
        if(instr->param_type == ParamType_Int32)
          print_instruction(vm_program, "push_char %d", instr->param.int_val);
        else
          assert(0);
      } break;

      case Opcode_PUSH_INT:
      {
        if(instr->param_type == ParamType_Int32)
          print_instruction(vm_program, "push_int %d", instr->param.int_val);
        else
          assert(0);
      } break;

      case Opcode_PUSH_REG:
      {
        if(instr->param_type == ParamType_Reg)
          print_instruction(vm_program, "push_reg %s", get_regname_str(instr->param.reg));
        else
          assert(0);
      } break;

      case Opcode_PUSH_FLOAT:
      {
        if(instr->param_type == ParamType_Float32)
          print_instruction(vm_program, "push_float %f", instr->param.float_val);
        else
          assert(0);
      } break;

      case Opcode_POP_REG:
      {
        if(instr->param_type == ParamType_Reg)
          print_instruction(vm_program, "pop_reg %s", get_regname_str(instr->param.reg));
        else
          assert(0);
      }

      case Opcode_DUP:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "dup");
      } break;

      case Opcode_ADD_INT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "add_int");
      } break;

      case Opcode_SUB_INT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "sub_int");
      } break;

      case Opcode_MUL_INT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "mul_int");
      } break;

      case Opcode_DIV_INT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "div_int");
      } break;

      case Opcode_ADD_FLOAT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "add_float");
      } break;

      case Opcode_SUB_FLOAT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "sub_float");
      } break;

      case Opcode_MUL_FLOAT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "mul_float");
      } break;

      case Opcode_DIV_FLOAT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "div_float");
      } break;

      case Opcode_NEG_FLOAT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "neg_float");
      } break;

      case Opcode_MOD_INT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "mod_int");
      } break;

      case Opcode_NEG_INT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "neg_int");
      } break;

      case Opcode_LOAD:
      {
        assert(instr->param_type == ParamType_Int32);
        print_instruction(vm_program, "load %d", instr->param.int_val);
      } break;

      case Opcode_STORE:
      {
        assert(instr->param_type == ParamType_Int32);
        print_instruction(vm_program, "store %d", instr->param.int_val);
      } break;

      case Opcode_LABEL:
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "label %s", instr->param.str);
      } break;

      case Opcode_RETURN:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "return");
      } break;

      case Opcode_GROW:
      {
        assert(instr->param_type == ParamType_Int32);
        print_instruction(vm_program, "grow %d", instr->param.int_val);
      } break;

      case Opcode_NEW:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "new");
      } break;

      case Opcode_CALL:
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "call %s", instr->param.str);
      } break;

      case Opcode_HALT:
      {
        assert(instr->param_type == ParamType_None);
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

      case Opcode_DECR_INT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "decr_int");
      } break;

      case Opcode_ENTER:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "enter");
      } break;

      case Opcode_LEAVE:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "leave");
      } break;

      case Opcode_NOOP:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "noop");
      } break;

      case Opcode_CMPEQ_CHAR:
      case Opcode_CMPNEQ_CHAR:
      case Opcode_CMPLSS_CHAR:
      case Opcode_CMPGRT_CHAR:
      case Opcode_CMPEQ_INT:
      case Opcode_CMPNEQ_INT:
      case Opcode_CMPLSS_INT:
      case Opcode_CMPGRT_INT:
      case Opcode_CMPEQ_FLOAT:
      case Opcode_CMPNEQ_FLOAT:
      case Opcode_CMPLSS_FLOAT:
      case Opcode_CMPGRT_FLOAT:
      {
        assert(instr->param_type == ParamType_None);
        if(instr->opcode == Opcode_CMPEQ_CHAR)
          print_instruction(vm_program, "cmpeq_char");
        else if(instr->opcode == Opcode_CMPNEQ_CHAR)
          print_instruction(vm_program, "cmpneq_char");
        else if(instr->opcode == Opcode_CMPLSS_CHAR)
          print_instruction(vm_program, "cmplss_char");
        else if(instr->opcode == Opcode_CMPGRT_CHAR)
          print_instruction(vm_program, "cmpgrt_char");
        else if(instr->opcode == Opcode_CMPEQ_INT)
          print_instruction(vm_program, "cmpeq_int");
        else if(instr->opcode == Opcode_CMPNEQ_INT)
          print_instruction(vm_program, "cmpneq_int");
        else if(instr->opcode == Opcode_CMPLSS_INT)
          print_instruction(vm_program, "cmplss_int");
        else if(instr->opcode == Opcode_CMPGRT_INT)
          print_instruction(vm_program, "cmpgrt_int");
        else if(instr->opcode == Opcode_CMPEQ_FLOAT)
          print_instruction(vm_program, "cmpeq_float");
        else if(instr->opcode == Opcode_CMPNEQ_FLOAT)
          print_instruction(vm_program, "cmpneq_float");
        else if(instr->opcode == Opcode_CMPLSS_FLOAT)
          print_instruction(vm_program, "cmplss_float");
        else if(instr->opcode == Opcode_CMPGRT_FLOAT)
          print_instruction(vm_program, "cmpgrt_float");
        else
          assert(0);
      } break;

      case Opcode_AND:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "and");
      } break;

      case Opcode_OR:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "or");
      } break;

      case Opcode_NOT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "not");
      } break;

      case Opcode_PUTC:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "putc");
      } break;

      case Opcode_FLOAT_TO_INT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "float_to_int");
      } break;

      case Opcode_INT_TO_FLOAT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "int_to_float");
      } break;

      default:
        assert(0);
    }
  }
}

#if 0
void rt_statement(AstNode* ast);
void rt_block_stmts(List* stmt_list);

void make_unique_label(String* label)
{
  sprintf(label->head, "L%d", last_label_id++);
  int len = cstr_len(label->head);
  label->end = label->head + len;
  MemoryArena* arena = label->arena;
  arena->free = (uint8*)label->end + 1;
}

void rt_call(AstCall* call)
{
  List* args_list = &call->args->list;
  for(ListItem* list_item = args_list->first;
      list_item;
      list_item = list_item->next)
  {
    rt_statement((AstNode*)list_item->elem);
  }
}

void rt_while_stmt(AstWhileStmt* while_stmt)
{
  rt_statement(while_stmt->cond_expr);

  {
    String* label_id = str_new(arena);
    make_unique_label(label_id);

    String* label = str_new(arena);
    str_append(label, label_id->head);
    str_append(label, ".while-expr");
    while_stmt->label_eval = str_cap(label);

    label = str_new(arena);
    str_append(label, label_id->head);
    str_append(label, ".while-break");
    while_stmt->label_break = str_cap(label);
  }

  if(while_stmt->body->kind == AstNodeKind_AstBlock)
    rt_block((AstBlock*)while_stmt->body);
  else
    rt_statement(while_stmt->body);
}

void rt_if_stmt(AstIfStmt* if_stmt)
{
  rt_statement(if_stmt->cond_expr);

  {
    String* label_id = str_new(arena);
    make_unique_label(label_id);

    String* label = str_new(arena);
    str_append(label, label_id->head);
    str_append(label, ".if-else");
    if_stmt->label_else = str_cap(label);

    str_init(label, arena);
    str_append(label, label_id->head);
    str_append(label, ".if-end");
    if_stmt->label_end = str_cap(label);
  }

  if(if_stmt->body->kind == AstNodeKind_AstBlock)
    rt_block((AstBlock*)if_stmt->body);
  else
    rt_statement(if_stmt->body);

  if(if_stmt->else_body)
  {
    AstNode* else_node = if_stmt->else_body;
    if(else_node->kind == AstNodeKind_AstBlock)
      rt_block((AstBlock*)else_node);
    else if(else_node->kind == AstNodeKind_AstIfStmt)
      rt_if_stmt((AstIfStmt*)else_node);
    else
      rt_statement(else_node);
  }
}

void rt_bin_expr(AstBinExpr* bin_expr)
{
  String* label_id = str_new(arena);
  make_unique_label(label_id);

  String* label = str_new(arena);
  str_append(label, label_id->head);
  str_append(label, ".logic-end");
  bin_expr->label_end = str_cap(label);

  rt_statement(bin_expr->left_operand);
  rt_statement(bin_expr->right_operand);
}

void rt_unr_expr(AstUnaryExpr* unr_expr)
{
  rt_statement(unr_expr->operand);
}

void rt_statement(AstNode* ast)
{
  if(ast->kind == AstNodeKind_AstBinExpr)
    rt_bin_expr((AstBinExpr*)ast);
  else if(ast->kind == AstNodeKind_AstUnaryExpr)
    rt_unr_expr((AstUnaryExpr*)ast);
  else if(ast->kind == AstNodeKind_AstCall)
    rt_call((AstCall*)ast);
  else if(ast->kind == AstNodeKind_AstIfStmt)
    rt_if_stmt((AstIfStmt*)ast);
  else if(ast->kind == AstNodeKind_AstWhileStmt)
    rt_while_stmt((AstWhileStmt*)ast);
  else if(ast->kind == AstNodeKind_AstReturnStmt)
  {
    AstReturnStmt* ret_stmt = (AstReturnStmt*)ast;
    if(ret_stmt->assign_expr)
      rt_statement((AstNode*)ret_stmt->assign_expr);
  }
  else if(ast->kind == AstNodeKind_AstCast)
  {
    AstCast* cast = (AstCast*)ast;
    rt_statement(cast->expr);
  }
  else if(ast->kind == AstNodeKind_AstVarDecl)
  {
    AstVarDecl* var_decl = (AstVarDecl*)ast;
    if(var_decl->assign_expr)
      rt_statement((AstNode*)var_decl->assign_expr);
  }
  else if(ast->kind == AstNodeKind_AstPutc)
  {
    AstPutc* putc_ast = (AstPutc*)ast;
    rt_statement(putc_ast->expr);
  }
  else if(ast->kind == AstNodeKind_AstBlock)
  {
    rt_block((AstBlock*)ast);
  }
  else if(ast->kind == AstNodeKind_AstVarOccur
          || ast->kind == AstNodeKind_AstBreakStmt
          || ast->kind == AstNodeKind_AstContinueStmt
          || ast->kind == AstNodeKind_AstLiteral
          || ast->kind == AstNodeKind_AstString
          || ast->kind == AstNodeKind_AstNew
          || ast->kind == AstNodeKind_AstEmptyStmt)
    ;//OK
  else
    assert(0);
}

void rt_block_stmts(List* stmt_list)
{
  for(ListItem* list_item = stmt_list->first;
      list_item;
      list_item = list_item->next)
  {
    rt_statement((AstNode*)list_item->elem);
  }
}
#endif

void gen_build_labels(AstNode* node)
{
  if(node->kind == AstNode_module)
  {
    gen_build_labels(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_block)
  {
    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item;
        list_item = list_item->next)
    {
      gen_build_labels(ITEM(list_item, ast_node));
    }
  }
  else if(node->kind == AstNode_stmt)
  {
    gen_build_labels(ATTR(node, ast_node, stmt));
  }
  else if(node->kind == AstNode_bin_expr)
  {
    String* label = str_new(arena);
    make_unique_label(label);
    str_append(label, ".logic-end");
    ATTR(node, str, label_end) = str_cap(label);

    gen_build_labels(ATTR(node, ast_node, left_operand));
    gen_build_labels(ATTR(node, ast_node, right_operand));
  }
  else if(node->kind == AstNode_un_expr)
  {
    gen_build_labels(ATTR(node, ast_node, operand));
  }
  else if(node->kind == AstNode_if_stmt)
  {
    gen_build_labels(ATTR(node, ast_node, cond_expr));

    String* label = str_new(arena);
    make_unique_label(label);
    str_append(label, ".if-else");
    ATTR(node, str, label_else) = str_cap(label);

    label = str_new(arena);
    make_unique_label(label);
    str_append(label, ".if-end");
    ATTR(node, str, label_end) = str_cap(label);

    gen_build_labels(ATTR(node, ast_node, body));

    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      gen_build_labels(else_body);
    }
  }
  else if(node->kind == AstNode_while_stmt)
  {
    
  }
}


