void gen_load_lvalue(List*, AstNode*);
void gen_code(List* code, AstNode* node);

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
          emit_instr(code, Opcode_ADD_INT32);
        else if(types_are_equal(left_operand->type, basic_type_float))
          emit_instr(code, Opcode_ADD_FLOAT32);
        else
          assert(0);
      } break;
      
      case AstOpKind_Sub:
      {
        gen_load_rvalue(code, left_operand);
        gen_load_rvalue(code, right_operand);
        if(types_are_equal(left_operand->type, basic_type_int))
          emit_instr(code, Opcode_SUB_INT32);
        else if(types_are_equal(left_operand->type, basic_type_float))
          emit_instr(code, Opcode_SUB_FLOAT32);
        else
          assert(0);
      } break;
      
      case AstOpKind_Mul:
      {
        gen_load_rvalue(code, left_operand);
        gen_load_rvalue(code, right_operand);
        if(types_are_equal(left_operand->type, basic_type_int))
          emit_instr(code, Opcode_MUL_INT32);
        else if(types_are_equal(left_operand->type, basic_type_float))
          emit_instr(code, Opcode_MUL_FLOAT32);
        else
          assert(0);
      } break;

      case AstOpKind_Div:
      {
        gen_load_rvalue(code, left_operand);
        gen_load_rvalue(code, right_operand);
        if(types_are_equal(left_operand->type, basic_type_int))
          emit_instr(code, Opcode_DIV_INT32);
        else if(types_are_equal(left_operand->type, basic_type_float))
          emit_instr(code, Opcode_DIV_FLOAT32);
        else
          assert(0);
      } break;

      case AstOpKind_Mod:
      {
        gen_load_rvalue(code, left_operand);
        gen_load_rvalue(code, right_operand);
        if(types_are_equal(left_operand->type, basic_type_int))
          emit_instr(code, Opcode_MOD_INT32);
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
            emit_instr(code, Opcode_CMPEQ_INT8);
          else if(types_are_equal(left_operand->type, basic_type_int))
            emit_instr(code, Opcode_CMPEQ_INT32);
          else if(types_are_equal(left_operand->type, basic_type_float))
            emit_instr(code, Opcode_CMPEQ_FLOAT32);
          else
            assert(0);
        }
        else if(bin_expr->op == AstOpKind_NotEquals)
        {
          if(types_are_equal(left_operand->type, basic_type_char))
            emit_instr(code, Opcode_CMPNEQ_INT8);
          else if(types_are_equal(left_operand->type, basic_type_int))
            emit_instr(code, Opcode_CMPNEQ_INT32);
          else if(types_are_equal(left_operand->type, basic_type_float))
            emit_instr(code, Opcode_CMPNEQ_FLOAT32);
          else
            assert(0);
        }
        else if(bin_expr->op == AstOpKind_Less)
        {
          if(types_are_equal(left_operand->type, basic_type_char))
            emit_instr(code, Opcode_CMPLSS_INT8);
          else if(types_are_equal(left_operand->type, basic_type_int))
            emit_instr(code, Opcode_CMPLSS_INT32);
          else if(types_are_equal(left_operand->type, basic_type_float))
            emit_instr(code, Opcode_CMPLSS_FLOAT32);
          else
            assert(0);
        }
        else if(bin_expr->op == AstOpKind_Greater)
        {
          if(types_are_equal(left_operand->type, basic_type_char))
            emit_instr(code, Opcode_CMPGRT_INT8);
          else if(types_are_equal(left_operand->type, basic_type_int))
            emit_instr(code, Opcode_CMPGRT_INT32);
          else if(types_are_equal(left_operand->type, basic_type_float))
            emit_instr(code, Opcode_CMPGRT_FLOAT32);
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
            emit_instr(code, Opcode_CMPLSS_INT8);
          else if(types_are_equal(left_operand->type, basic_type_int))
            emit_instr(code, Opcode_CMPLSS_INT32);
          else if(types_are_equal(left_operand->type, basic_type_float))
            emit_instr(code, Opcode_CMPLSS_FLOAT32);
          else
            assert(0);
        }
        else if(bin_expr->op == AstOpKind_GreaterEquals)
        {
          if(types_are_equal(left_operand->type, basic_type_char))
            emit_instr(code, Opcode_CMPGRT_INT8);
          else if(types_are_equal(left_operand->type, basic_type_int))
            emit_instr(code, Opcode_CMPGRT_INT32);
          else if(types_are_equal(left_operand->type, basic_type_float))
            emit_instr(code, Opcode_CMPGRT_FLOAT32);
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
          emit_instr(code, Opcode_CMPEQ_INT8);
        else if(types_are_equal(left_operand->type, basic_type_int))
          emit_instr(code, Opcode_CMPEQ_INT32);
        else if(types_are_equal(left_operand->type, basic_type_float))
          emit_instr(code, Opcode_CMPEQ_FLOAT32);
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
         emit_instr(code, Opcode_NEG_INT32);
      else if(types_are_equal(unr_expr->type, basic_type_float))
         emit_instr(code, Opcode_NEG_FLOAT32);
      else
        assert(0);
    }
    else if(unr_expr->op == AstOpKind_LogicNot)
    {
      emit_instr_int(code, Opcode_PUSH_INT32, 0);
      emit_instr( code, Opcode_CMPEQ_INT32);
    }
    else if(unr_expr->op == AstOpKind_IntToFloat)
      emit_instr(code, Opcode_INT32_TO_FLOAT32);
    else if(unr_expr->op == AstOpKind_FloatToInt)
      emit_instr(code, Opcode_FLOAT32_TO_INT32);
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
        emit_instr_int(code, Opcode_PUSH_INT32, link->data.loc);
        emit_instr(code, Opcode_ADD_INT32);
        emit_instr_int(code, Opcode_LOAD, 4); // access link is on the stack now
      }
      emit_instr_int(code, Opcode_PUSH_INT32, data->loc);
      emit_instr(code, Opcode_ADD_INT32);
    }
    else
      emit_instr_int(code, Opcode_PUSH_INT32, data->loc);
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
      emit_instr_int(code, Opcode_PUSH_INT32, literal->int_val);
    else if(literal->lit_kind == AstLiteralKind_Char)
      emit_instr_int(code, Opcode_PUSH_INT8, literal->char_val);
    else if(literal->lit_kind == AstLiteralKind_Float)
      emit_instr_float(code, Opcode_PUSH_FLOAT32, literal->float_val);
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
      emit_instr_int(code, Opcode_PUSH_INT32, -4);
      emit_instr(code, Opcode_ADD_INT32); // Get the FP of the previous activ. record
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

void gen_load_lvalue(List* code, AstNode* node)
{
  if(node->kind == AstNode_var_occur)
  {
    DataArea* link = ATTR(node, symbol, occur_sym)->data_area;
    DataArea* data = ATTR(node, symbol, decl_sym)->data_area;

    emit_instr_reg(code, Opcode_PUSH_REG, RegName_FP);

    if(link && link->decl_scope_offset > 0)
    {
      // non-local
      assert(link->loc < 0); // relative to FP
      emit_instr_int32(code, Opcode_PUSH_INT32, link->loc);
      emit_instr(code, Opcode_ADD_INT32);
      emit_instr_int32(code, Opcode_LOAD, 4); // access link is on the stack now
    }
    emit_instr_int32(code, Opcode_PUSH_INT32, data->loc);
    emit_instr(code, Opcode_ADD_INT32);
  }
  else
    assert(0);
}

void gen_load_rvalue(List* code, AstNode* node)
{
  if(node->kind == AstNode_var_occur)
  {
    gen_load_lvalue(code, node);
    emit_instr_int32(code, Opcode_LOAD, ATTR(node, type, eval_type)->width);
  }
  else if(node->kind == AstNode_proc_occur)
  {
    gen_code(code, node);
  }
  else if(node->kind == AstNode_lit)
  {
    LiteralKind kind = ATTR(node, lit_kind, lit_kind);
    if(kind == Literal_int_val || kind == Literal_bool_val)
    {
      emit_instr_int32(code, Opcode_PUSH_INT32, ATTR(node, int_val, int_val));
    }
    else if(kind == Literal_float_val)
    {
      emit_instr_float32(code, Opcode_PUSH_FLOAT32, ATTR(node, float_val, float_val));
    }
    else if(kind == Literal_char_val)
    {
      emit_instr_int8(code, Opcode_PUSH_INT8, ATTR(node, char_val, char_val));
    }
    else
      assert(0);
  }
  else if(node->kind == AstNode_bin_expr)
  {
    gen_code(code, node);
  }
  else if(node->kind == AstNode_un_expr)
  {
    gen_code(code, node);
  }
  else
    assert(0);
}

void gen_code(List* code, AstNode* node)
{
  if(node->kind == AstNode_module)
  {
    gen_code(code, ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_block)
  {
    Scope* scope = ATTR(node, scope, scope);
    for(ListItem* list_item = scope->access_links->first;
        list_item;
        list_item = list_item->next)
    {
      DataArea* link = ITEM(list_item, data_area); assert(link->kind == DataArea_link);
      emit_instr_reg(code, Opcode_PUSH_REG, RegName_FP);

      int offset = link->decl_scope_offset - 1;
      while(offset--)
      {
        emit_instr_int32(code, Opcode_PUSH_INT32, -4);
        emit_instr(code, Opcode_ADD_INT32); // ge the FP of the previous activation record
        emit_instr_int32(code, Opcode_LOAD, 4);
      }
    }

    emit_instr(code, Opcode_ENTER);
    emit_instr_int32(code, Opcode_GROW, scope->local_area_size);

    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item;
        list_item = list_item->next)
    {
      gen_code(code, ITEM(list_item, ast_node));
    }

    emit_instr(code, Opcode_LEAVE);
    emit_instr_int32(code, Opcode_GROW, -scope->link_area_size);
  }
  else if(node->kind == AstNode_proc_decl)
  {
    Scope* scope = ATTR(node, scope, scope);
    emit_instr_str(code, Opcode_LABEL, ATTR(node, str, name));
    emit_instr_int32(code, Opcode_GROW, scope->local_area_size);

    AstNode* body = ATTR(node, ast_node, body);
    for(ListItem* list_item = ATTR(body, list, nodes)->first;
        list_item;
        list_item = list_item->next)
    {
      gen_code(code, ITEM(list_item, ast_node));
    }

    emit_instr_str(code, Opcode_LABEL, ATTR(node, str, label_end));
    emit_instr(code, Opcode_RETURN);
  }
  else if(node->kind == AstNode_var_decl)
  {
    //todo
#if 0
    AstNode* init_expr = ATTR(node, ast_node, init_expr);
    if(init_expr)
    {
      gen_code(code, init_expr);
    }
#endif
  }
  else if(node->kind == AstNode_stmt)
  {
    gen_code(code, ATTR(node, ast_node, stmt));
  }
  else if(node->kind == AstNode_while_stmt)
  {
    emit_instr_str(code, Opcode_LABEL, ATTR(node, str, label_eval));
    gen_load_rvalue(code, ATTR(node, ast_node, cond_expr));
    emit_instr_str(code, Opcode_JUMPZ, ATTR(node, str, label_break));

    AstNode* body = ATTR(node, ast_node, body);
    if(body)
    {
      gen_code(code, body);
    }

    emit_instr_str(code, Opcode_GOTO, ATTR(node, str, label_eval));
    emit_instr_str(code, Opcode_LABEL, ATTR(node, str, label_break));
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
      gen_load_rvalue(code, right_operand);

      if(left_operand->kind == AstNode_var_occur)
      {
        gen_load_lvalue(code, left_operand);
      }
      else if(left_operand->kind == AstNode_un_expr)
      {
        if(ATTR(left_operand, op_kind, op_kind) == Operator_deref)
        {
          gen_load_rvalue(code, ATTR(left_operand, ast_node, operand));
        }
        else
          assert(0);
      }

      emit_instr_int32(code, Opcode_STORE, type->width);
    }
    else if(bin_op == Operator_add)
    {
      gen_load_rvalue(code, left_operand);
      gen_load_rvalue(code, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == Type_pointer)
      {
        emit_instr(code, Opcode_ADD_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_instr(code, Opcode_ADD_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_sub)
    {
      gen_load_rvalue(code, left_operand);
      gen_load_rvalue(code, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == Type_pointer)
      {
        emit_instr(code, Opcode_SUB_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_instr(code, Opcode_SUB_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_mul)
    {
      gen_load_rvalue(code, left_operand);
      gen_load_rvalue(code, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == Type_pointer)
      {
        emit_instr(code, Opcode_MUL_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_instr(code, Opcode_MUL_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_div)
    {
      gen_load_rvalue(code, left_operand);
      gen_load_rvalue(code, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == Type_pointer)
      {
        emit_instr(code, Opcode_DIV_INT32);
      }
      else if(types_are_equal(type, basic_type_float))
      {
        emit_instr(code, Opcode_DIV_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_mod)
    {
      gen_load_rvalue(code, left_operand);
      gen_load_rvalue(code, right_operand);

      if(types_are_equal(type, basic_type_int) || type->kind == Type_pointer)
      {
        emit_instr(code, Opcode_MOD_INT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_eq)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(code, left_operand);
      gen_load_rvalue(code, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(code, Opcode_CMPEQ_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(code, Opcode_CMPEQ_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(code, Opcode_CMPEQ_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_not_eq)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(code, left_operand);
      gen_load_rvalue(code, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(code, Opcode_CMPNEQ_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(code, Opcode_CMPNEQ_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(code, Opcode_CMPNEQ_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_less)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(code, left_operand);
      gen_load_rvalue(code, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(code, Opcode_CMPLSS_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(code, Opcode_CMPLSS_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(code, Opcode_CMPLSS_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_greater)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(code, left_operand);
      gen_load_rvalue(code, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(code, Opcode_CMPGRT_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(code, Opcode_CMPGRT_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(code, Opcode_CMPGRT_FLOAT32);
      }
      else
        assert(0);
    }
    else if(bin_op == Operator_logic_and || bin_op == Operator_logic_or)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(code, left_operand);
      emit_instr(code, Opcode_DUP);

      if(bin_op == Operator_logic_and)
      {
        emit_instr_str(code, Opcode_JUMPZ, ATTR(node, str, label_end));
      }
      else if(bin_op == Operator_logic_or)
      {
        emit_instr_str(code, Opcode_JUMPNZ, ATTR(node, str, label_end));
      }
      else
        assert(0);

      emit_instr_int32(code, Opcode_GROW, -type->width);
      gen_load_rvalue(code, right_operand);
      emit_instr(code, Opcode_DUP);

      if(bin_op == Operator_logic_and)
      {
        emit_instr_str(code, Opcode_JUMPZ, ATTR(node, str, label_end));
      }
      else if(bin_op == Operator_logic_or)
      {
        emit_instr_str(code, Opcode_JUMPNZ, ATTR(node, str, label_end));
      }
      else
        assert(0);

      emit_instr_str(code, Opcode_LABEL, ATTR(node, str, label_end));
    }
    else if(bin_op == Operator_less_eq || bin_op == Operator_greater_eq)
    {
      assert(types_are_equal(left_ty, right_ty));

      gen_load_rvalue(code, left_operand);
      gen_load_rvalue(code, right_operand);

      if(bin_op == Operator_less_eq)
      {
        if(types_are_equal(left_ty, basic_type_char))
        {
          emit_instr(code, Opcode_CMPLSS_INT8);
        }
        else if(types_are_equal(left_ty, basic_type_int))
        {
          emit_instr(code, Opcode_CMPLSS_INT32);
        }
        else if(types_are_equal(left_ty, basic_type_float))
        {
          emit_instr(code, Opcode_CMPLSS_FLOAT32);
        }
        else
          assert(0);
      }
      else if(bin_op == Operator_greater_eq)
      {
        if(types_are_equal(left_ty, basic_type_char))
        {
          emit_instr(code, Opcode_CMPGRT_INT8);
        }
        else if(types_are_equal(left_ty, basic_type_int))
        {
          emit_instr(code, Opcode_CMPGRT_INT32);
        }
        else if(types_are_equal(left_ty, basic_type_float))
        {
          emit_instr(code, Opcode_CMPGRT_FLOAT32);
        }
        else
          assert(0);
      }
      else
        assert(0);

      emit_instr(code, Opcode_DUP);
      emit_instr_int32(code, Opcode_GROW, -type->width);

      gen_load_rvalue(code, left_operand);
      gen_load_rvalue(code, right_operand);

      if(types_are_equal(left_ty, basic_type_char))
      {
        emit_instr(code, Opcode_CMPNEQ_INT8);
      }
      else if(types_are_equal(left_ty, basic_type_int))
      {
        emit_instr(code, Opcode_CMPNEQ_INT32);
      }
      else if(types_are_equal(left_ty, basic_type_float))
      {
        emit_instr(code, Opcode_CMPNEQ_FLOAT32);
      }
      else
        assert(0);

      emit_instr_str(code, Opcode_LABEL, ATTR(node, str, label_end));
    }
    else if(bin_op == Operator_cast)
    {
      if(types_are_equal(left_ty, basic_type_int) && types_are_equal(right_ty, basic_type_float))
      {
        emit_instr(code, Opcode_FLOAT32_TO_INT32);
      }
      if(types_are_equal(left_ty, basic_type_float) && types_are_equal(right_ty, basic_type_int))
      {
        emit_instr(code, Opcode_INT32_TO_FLOAT32);
      }
    }
    else if(bin_op == Operator_array_index)
    {
      Type* type = ATTR(node, type, eval_type);

      gen_load_lvalue(code, left_operand);
      gen_load_rvalue(code, right_operand);
      emit_instr_int32(code, Opcode_PUSH_INT32, type->width);
      emit_instr(code, Opcode_MUL_INT32);
      emit_instr(code, Opcode_ADD_INT32);
      emit_instr_int32(code, Opcode_LOAD, type->width);
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
        gen_load_lvalue(code, operand);
      }
      else
        gen_load_rvalue(code, operand);
    }
    else if(un_op == Operator_neg)
    {
      gen_load_rvalue(code, operand);

      if(types_are_equal(operand_ty, basic_type_int))
      {
        emit_instr(code, Opcode_NEG_INT32);
      }
      else if(types_are_equal(operand_ty, basic_type_float))
      {
        emit_instr(code, Opcode_NEG_FLOAT32);
      }
      else
        assert(0);
    }
    else if(un_op == Operator_logic_not)
    {
      gen_load_rvalue(code, operand);
      emit_instr_int32(code, Opcode_PUSH_INT32, 0);
      emit_instr(code, Opcode_CMPEQ_INT32);
    }
    else if(un_op == Operator_deref)
    {
      assert(ATTR(operand, type, type)->kind == Type_pointer);
      emit_instr_int32(code, Opcode_LOAD, type->width);
    }
    else
      assert(0);
  }
  else if(node->kind == AstNode_proc_occur)
  {
    Type* type = ATTR(node, type, type); assert(type->kind == Type_proc);
    Type* ret_type = type->proc.ret;
    Type* args_type = type->proc.args;
    AstNode* proc = ATTR(node, ast_node, proc_decl);
    Scope* scope = ATTR(proc, scope, scope);

    emit_instr_int32(code, Opcode_GROW, ret_type->width);

    for(ListItem* list_item = ATTR(node, list, actual_args)->first;
        list_item;
        list_item = list_item->next)
    {
      gen_load_rvalue(code, ITEM(list_item, ast_node));
    }

    emit_instr_str(code, Opcode_CALL, ATTR(node, str, name));
    emit_instr_int32(code, Opcode_GROW, -scope->link_area_size);
    emit_instr_int32(code, Opcode_GROW, -args_type->width);
  }
  else if(node->kind == AstNode_return_stmt)
  {
#if 0
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    AstNode* proc = ATTR(node, ast_node, proc);

    if(ret_expr)
    {
      AstNode* ret_var = ATTR(proc, ast_node, ret_var);
      Type* ret_type = ATTR(ret_var, type, eval_type);

      gen_load_rvalue(code, ret_expr);
      gen_load_lvalue(code, ret_var);
      emit_instr_int32(code, Opcode_STORE, ret_type->width);
    }

    int depth = ATTR(node, int_val, nesting_depth);
    while(depth-- > 0)
    {
      emit_instr(code, Opcode_LEAVE);
    }

    emit_instr_str(code, Opcode_GOTO, ATTR(proc, str, label_end));
#endif
  }
  else if(node->kind == AstNode_if_stmt)
  {
    gen_load_rvalue(code, ATTR(node, ast_node, cond_expr));

    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      emit_instr_str(code, Opcode_JUMPZ, ATTR(node, str, label_else));
    }
    else
    {
      emit_instr_str(code, Opcode_JUMPZ, ATTR(node, str, label_end));
    }

    gen_code(code, ATTR(node, ast_node, body));
    emit_instr_str(code, Opcode_GOTO, ATTR(node, str, label_end));

    if(else_body)
    {
      emit_instr_str(code, Opcode_LABEL, ATTR(node, str, label_else));
      gen_code(code, else_body);
    }

    emit_instr_str(code, Opcode_LABEL, ATTR(node, str, label_end));
  }
  else if(node->kind == AstNode_while_stmt)
  {
    emit_instr_str(code, Opcode_LABEL, ATTR(node, str, label_eval));
    gen_load_rvalue(code, ATTR(node, ast_node, cond_expr));
    emit_instr_str(code, Opcode_JUMPZ, ATTR(node, str, label_break));

    AstNode* body = ATTR(node, ast_node, body);
    if(body)
    {
      gen_code(code, body);
    }

    emit_instr_str(code, Opcode_GOTO, ATTR(node, str, label_eval));
    emit_instr_str(code, Opcode_LABEL, ATTR(node, str, label_break));
  }
  else if(node->kind == AstNode_break_stmt)
  {
    AstNode* loop = ATTR(node, ast_node, loop);

    int depth = ATTR(node, int_val, nesting_depth);
    while(depth-- > 0)
    {
      emit_instr(code, Opcode_LEAVE);
    }
    emit_instr_str(code, Opcode_GOTO, ATTR(loop, str, label_break));
  }
  else
    assert(0);
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
          print_instruction(vm_program, "push_char %d", instr->param.int_val);
        else
          assert(0);
      } break;

      case Opcode_PUSH_INT32:
      {
        if(instr->param_type == ParamType_int32)
          print_instruction(vm_program, "push_int %d", instr->param.int_val);
        else
          assert(0);
      } break;

      case Opcode_PUSH_REG:
      {
        if(instr->param_type == ParamType_reg)
          print_instruction(vm_program, "push_reg %s", get_regname_str(instr->param.reg));
        else
          assert(0);
      } break;

      case Opcode_PUSH_FLOAT32:
      {
        if(instr->param_type == ParamType_float32)
          print_instruction(vm_program, "push_float %f", instr->param.float_val);
        else
          assert(0);
      } break;

      case Opcode_POP_REG:
      {
        if(instr->param_type == ParamType_reg)
          print_instruction(vm_program, "pop_reg %s", get_regname_str(instr->param.reg));
        else
          assert(0);
      }

      case Opcode_DUP:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "dup");
      } break;

      case Opcode_ADD_INT32:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "add_int");
      } break;

      case Opcode_SUB_INT32:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "sub_int");
      } break;

      case Opcode_MUL_INT32:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "mul_int");
      } break;

      case Opcode_DIV_INT32:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "div_int");
      } break;

      case Opcode_ADD_FLOAT32:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "add_float");
      } break;

      case Opcode_SUB_FLOAT32:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "sub_float");
      } break;

      case Opcode_MUL_FLOAT32:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "mul_float");
      } break;

      case Opcode_DIV_FLOAT32:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "div_float");
      } break;

      case Opcode_NEG_FLOAT32:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "neg_float");
      } break;

      case Opcode_MOD_INT32:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "mod_int");
      } break;

      case Opcode_NEG_INT32:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "neg_int");
      } break;

      case Opcode_LOAD:
      {
        assert(instr->param_type == ParamType_int32);
        print_instruction(vm_program, "load %d", instr->param.int_val);
      } break;

      case Opcode_STORE:
      {
        assert(instr->param_type == ParamType_int32);
        print_instruction(vm_program, "store %d", instr->param.int_val);
      } break;

      case Opcode_LABEL:
      {
        assert(instr->param_type == ParamType_str);
        print_instruction(vm_program, "label %s", instr->param.str);
      } break;

      case Opcode_RETURN:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "return");
      } break;

      case Opcode_GROW:
      {
        assert(instr->param_type == ParamType_int32);
        print_instruction(vm_program, "grow %d", instr->param.int_val);
      } break;

#if 0
      case Opcode_NEW:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "new");
      } break;
#endif

      case Opcode_CALL:
      {
        assert(instr->param_type == ParamType_str);
        print_instruction(vm_program, "call %s", instr->param.str);
      } break;

      case Opcode_HALT:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "halt");
      } break;

      case Opcode_GOTO:
      {
        assert(instr->param_type == ParamType_str);
        print_instruction(vm_program, "goto %s", instr->param.str);
      } break;

      case Opcode_JUMPZ:
      {
        assert(instr->param_type == ParamType_str);
        print_instruction(vm_program, "jumpz %s", instr->param.str);
      } break;

      case Opcode_JUMPNZ:
      {
        assert(instr->param_type == ParamType_str);
        print_instruction(vm_program, "jumpnz %s", instr->param.str);
      } break;

      case Opcode_DECR_INT32:
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
          print_instruction(vm_program, "cmpeq_char");
        else if(instr->opcode == Opcode_CMPNEQ_INT8)
          print_instruction(vm_program, "cmpneq_char");
        else if(instr->opcode == Opcode_CMPLSS_INT8)
          print_instruction(vm_program, "cmplss_char");
        else if(instr->opcode == Opcode_CMPGRT_INT8)
          print_instruction(vm_program, "cmpgrt_char");
        else if(instr->opcode == Opcode_CMPEQ_INT32)
          print_instruction(vm_program, "cmpeq_int");
        else if(instr->opcode == Opcode_CMPNEQ_INT32)
          print_instruction(vm_program, "cmpneq_int");
        else if(instr->opcode == Opcode_CMPLSS_INT32)
          print_instruction(vm_program, "cmplss_int");
        else if(instr->opcode == Opcode_CMPGRT_INT32)
          print_instruction(vm_program, "cmpgrt_int");
        else if(instr->opcode == Opcode_CMPEQ_FLOAT32)
          print_instruction(vm_program, "cmpeq_float");
        else if(instr->opcode == Opcode_CMPNEQ_FLOAT32)
          print_instruction(vm_program, "cmpneq_float");
        else if(instr->opcode == Opcode_CMPLSS_FLOAT32)
          print_instruction(vm_program, "cmplss_float");
        else if(instr->opcode == Opcode_CMPGRT_FLOAT32)
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

#if 0
      case Opcode_PUTC:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "putc");
      } break;
#endif

      case Opcode_FLOAT32_TO_INT32:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "float_to_int");
      } break;

      case Opcode_INT32_TO_FLOAT32:
      {
        assert(instr->param_type == ParamType_None);
        print_instruction(vm_program, "int_to_float");
      } break;

      default:
        assert(0);
    }
  }
}

void build_gen_labels(AstNode* node)
{
  if(node->kind == AstNode_module)
  {
    build_gen_labels(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_block)
  {
    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item;
        list_item = list_item->next)
    {
      build_gen_labels(ITEM(list_item, ast_node));
    }
  }
  else if(node->kind == AstNode_proc_decl)
  {
    String* label = str_new(arena);
    str_append(label, ATTR(node, str, name));
    str_append(label, ".proc-end");
    ATTR(node, str, label_end) = str_cap(label);

    build_gen_labels(ATTR(node, ast_node, body));
  }
  else if(node->kind == AstNode_stmt)
  {
    build_gen_labels(ATTR(node, ast_node, stmt));
  }
  else if(node->kind == AstNode_bin_expr)
  {
    char* label_id = make_unique_label();

    String* label = str_new(arena);
    str_append(label, label_id);
    str_append(label, ".logic-end");
    ATTR(node, str, label_end) = str_cap(label);

    build_gen_labels(ATTR(node, ast_node, left_operand));
    build_gen_labels(ATTR(node, ast_node, right_operand));
  }
  else if(node->kind == AstNode_un_expr)
  {
    build_gen_labels(ATTR(node, ast_node, operand));
  }
  else if(node->kind == AstNode_if_stmt)
  {
    build_gen_labels(ATTR(node, ast_node, cond_expr));

    char* label_id = make_unique_label();

    String* label = str_new(arena);
    str_append(label, label_id);
    str_append(label, ".if-else");
    ATTR(node, str, label_else) = str_cap(label);

    label = str_new(arena);
    str_append(label, label_id);
    str_append(label, ".if-end");
    ATTR(node, str, label_end) = str_cap(label);

    build_gen_labels(ATTR(node, ast_node, body));

    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      build_gen_labels(else_body);
    }
  }
  else if(node->kind == AstNode_while_stmt)
  {
    build_gen_labels(ATTR(node, ast_node, cond_expr));

    char* label_id = make_unique_label();

    String* label = str_new(arena);
    str_append(label, label_id);
    str_append(label, ".while-eval");
    ATTR(node, str, label_eval) = str_cap(label);

    label = str_new(arena);
    str_append(label, label_id);
    str_append(label, ".while-break");
    ATTR(node, str, label_break) = str_cap(label);

    AstNode* body = ATTR(node, ast_node, body);
    if(body)
    {
      build_gen_labels(body);
    }
  }
  else if(node->kind == AstNode_proc_occur)
  {
    for(ListItem* list_item = ATTR(node, list, actual_args)->first;
        list_item;
        list_item = list_item->next)
    {
      build_gen_labels(ITEM(list_item, ast_node));
    }
  }
  else if(node->kind == AstNode_return_stmt)
  {
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    if(ret_expr)
    {
      build_gen_labels(ret_expr);
    }
  }
  else if(node->kind == AstNode_var_decl)
  {
    AstNode* init_expr = ATTR(node, ast_node, init_expr);
    if(init_expr)
    {
      build_gen_labels(init_expr);
    }
  }
}


