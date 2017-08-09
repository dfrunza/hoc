#include "hocc.h"

extern MemoryArena* arena;

local void gen_load_rvalue(List*, AstNode*);
local void gen_load_lvalue(List*, AstVarOccur*);
local void gen_statement(List*, AstNode*);

local void
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

local void
emit_instr_reg(List* instr_list, Opcode opcode, RegName reg)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = ParamType_Reg;
  instr->param.reg = reg;
  list_append(arena, instr_list, instr);
}

local void
emit_instr_int(List* instr_list, Opcode opcode, int32 int_val)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = ParamType_Int32;
  instr->param.int_val = int_val;
  list_append(arena, instr_list, instr);
}

local void
emit_instr_float(List* instr_list, Opcode opcode, float32 float_val)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = ParamType_Float32;
  instr->param.float_val = float_val;
  list_append(arena, instr_list, instr);
}

local void
emit_instr(List* instr_list, Opcode opcode)
{
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = ParamType__Null;
  list_append(arena, instr_list, instr);
}

local void
emit_instr_str(List* instr_list, Opcode opcode, char* str)
{
  assert(str);
  Instruction* instr = mem_push_struct(arena, Instruction);
  instr->opcode = opcode;
  instr->param_type = ParamType_String;
  instr->param.str = str;
  list_append(arena, instr_list, instr);
}

local void
gen_bin_expr(List* code, AstBinExpr* bin_expr)
{
  AstNode* left_operand = bin_expr->left_operand;
  AstNode* right_operand = bin_expr->right_operand;

  if(bin_expr->op == AstOpKind_Assign)
  {
    gen_load_rvalue(code, right_operand);

    if(left_operand->kind == AstNodeKind_VarOccur)
      gen_load_lvalue(code, (AstVarOccur*)left_operand);
    else if(left_operand->kind == AstNodeKind_UnrExpr)
    {
      AstUnrExpr* unr_expr = (AstUnrExpr*)left_operand;
      if(unr_expr->op == AstOpKind_PtrDeref)
        gen_load_rvalue(code, unr_expr->operand);
      else
        assert(0);
    }

    emit_instr_int(code, Opcode_STORE, compute_type_width(right_operand->type));
  }
  else
  {
    gen_load_rvalue(code, left_operand);
    gen_load_rvalue(code, right_operand);

    switch(bin_expr->op)
    {
      case AstOpKind_Add:
        emit_instr(code, Opcode_ADD); break;
      case AstOpKind_Sub:
        emit_instr(code, Opcode_SUB); break;
      case AstOpKind_Mul:
        emit_instr(code, Opcode_MUL); break;
      case AstOpKind_Div:
        emit_instr(code, Opcode_DIV); break;
      case AstOpKind_Mod:
        emit_instr(code, Opcode_MOD); break;
      case AstOpKind_AddFloat:
        emit_instr(code, Opcode_ADDF); break;
      case AstOpKind_SubFloat:
        emit_instr(code, Opcode_SUBF); break;
      case AstOpKind_MulFloat:
        emit_instr(code, Opcode_MULF); break;
      case AstOpKind_DivFloat:
        emit_instr(code, Opcode_DIVF); break;

      case AstOpKind_Equals:
      case AstOpKind_NotEquals:
      case AstOpKind_Less:
      case AstOpKind_Greater:
      {
        if(bin_expr->op == AstOpKind_Equals)
          emit_instr(code, Opcode_CMPEQ);
        else if(bin_expr->op == AstOpKind_NotEquals)
          emit_instr(code, Opcode_CMPNEQ);
        else if(bin_expr->op == AstOpKind_Less)
          emit_instr(code, Opcode_CMPLSS);
        else if(bin_expr->op == AstOpKind_Greater)
          emit_instr(code, Opcode_CMPGRT);
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
        emit_instr_str(code, Opcode_LABEL, bin_expr->label_end);
      } break;

      case AstOpKind_LessEquals:
      case AstOpKind_GreaterEquals:
      {
        gen_load_rvalue(code, left_operand);
        gen_load_rvalue(code, right_operand);
        if(bin_expr->op == AstOpKind_LessEquals)
          emit_instr(code, Opcode_CMPLSS);
        else if(bin_expr->op == AstOpKind_GreaterEquals)
          emit_instr(code, Opcode_CMPGRT);
        else
          assert(0);
        emit_instr(code, Opcode_DUP);

        emit_instr_str(code, Opcode_JUMPNZ, bin_expr->label_end);
        emit_instr_int(code, Opcode_GROW, -compute_type_width(left_operand->type));
        gen_load_rvalue(code, left_operand);
        gen_load_rvalue(code, right_operand);
        emit_instr(code, Opcode_CMPEQ);
        emit_instr_str(code, Opcode_LABEL, bin_expr->label_end);
      } break;

      default:
        assert(0);
    }
  }
}

local void
gen_unr_expr(List* code, AstUnrExpr* unr_expr)
{
  if(unr_expr->op == AstOpKind_AddressOf)
  {
    assert(unr_expr->operand->kind == AstNodeKind_VarOccur);
    gen_load_lvalue(code, (AstVarOccur*)unr_expr->operand);
  }
  else
  {
    gen_load_rvalue(code, unr_expr->operand);

    if(unr_expr->op == AstOpKind_Neg)
      emit_instr(code, Opcode_NEG);
    else if(unr_expr->op == AstOpKind_NegFloat)
      emit_instr(code, Opcode_NEGF);
    else if(unr_expr->op == AstOpKind_LogicNot)
    {
      emit_instr_int(code, Opcode_PUSH_I32, 0);
      emit_instr( code, Opcode_CMPEQ);
    }
    else if(unr_expr->op == AstOpKind_IntToFloat)
      emit_instr(code, Opcode_INT_TO_FLOAT);
    else if(unr_expr->op == AstOpKind_FloatToInt)
      emit_instr(code, Opcode_FLOAT_TO_INT);
    else if(unr_expr->op == AstOpKind_PtrDeref)
      emit_instr_int(code, Opcode_LOAD, 4);
    else
      assert(0);
  }
}

local void
gen_call(List* code, AstCall* call)
{
  AstProc* proc = (AstProc*)call->proc_sym->ast;
  emit_instr_int(code, Opcode_GROW, proc->ret_size);

  AstBlock* block = proc->body;

  for(ListItem* list_item = call->args.first;
      list_item;
      list_item = list_item->next)
  {
    gen_load_rvalue(code, (AstNode*)list_item->elem);
  }

  emit_instr_str(code, Opcode_CALL, proc->label);
  emit_instr_int(code, Opcode_GROW, -block->links_size);
  emit_instr_int(code, Opcode_GROW, -proc->args_size);
}

local void
gen_load_lvalue(List* code, AstVarOccur* var_occur)
{
  DataArea* data = var_occur->data;
  AccessLink* link = var_occur->link;

  if(var_occur->decl_block_offset >= 0)
  {
    emit_instr_reg(code, Opcode_PUSH_R, RegName_FP);
    if(link) 
    {
      // this is a non-local
      assert(link->data.loc < 0); // relative to FP
      emit_instr_int(code, Opcode_PUSH_I32, link->data.loc);
      emit_instr(code, Opcode_ADD);
      emit_instr_int(code, Opcode_LOAD, 4); // access link is on the stack now
    }
    emit_instr_int(code, Opcode_PUSH_I32, data->loc);
    emit_instr(code, Opcode_ADD);
  }
  else
    emit_instr_int(code, Opcode_PUSH_I32, data->loc);
}

local void
gen_load_rvalue(List* code, AstNode* ast)
{
  if(ast->kind == AstNodeKind_VarOccur)
  {
    AstVarOccur* var_occur = (AstVarOccur*)ast;
    gen_load_lvalue(code, var_occur);
    emit_instr_int(code, Opcode_LOAD, compute_type_width(var_occur->type));
  }
  else if(ast->kind == AstNodeKind_Call)
    gen_call(code, (AstCall*)ast);
  else if(ast->kind == AstNodeKind_Literal)
  {
    AstLiteral* literal = (AstLiteral*)ast;
    if(literal->lit_kind == AstLiteralKind_Int || literal->lit_kind == AstLiteralKind_Bool)
      emit_instr_int(code, Opcode_PUSH_I32, literal->int_val);
    else if(literal->lit_kind == AstLiteralKind_Char)
      emit_instr_int(code, Opcode_PUSH_I8, literal->char_val);
    else if(literal->lit_kind == AstLiteralKind_Float)
      emit_instr_float(code, Opcode_PUSH_F32, literal->float_val);
    else
      assert(0);
  }
  else if(ast->kind == AstNodeKind_BinExpr)
    gen_bin_expr(code, (AstBinExpr*)ast);
  else if(ast->kind == AstNodeKind_UnrExpr)
    gen_unr_expr(code, (AstUnrExpr*)ast);
  else if(ast->kind == AstNodeKind_New)
    emit_instr_int(code, Opcode_NEW, ((AstNew*)ast)->storage_size);
  else
    assert(0);
}

local void
gen_return_stmt(List* code, AstReturnStmt* ret_stmt)
{
  if(ret_stmt->assign_expr)
  {
    AstBinExpr* assign_expr = (AstBinExpr*)ret_stmt->assign_expr;
    assert(assign_expr->kind == AstNodeKind_BinExpr);
    gen_bin_expr(code, assign_expr);
    emit_instr_int(code, Opcode_GROW, -compute_type_width(assign_expr->type));
  }

  AstProc* proc = (AstProc*)ret_stmt->proc;
  int depth = ret_stmt->nesting_depth;
  while(depth--)
    emit_instr(code, Opcode_LEAVE);
  emit_instr_str(code, Opcode_GOTO, proc->label_end);
}

local void
gen_break_stmt(List* code, AstBreakStmt* break_stmt)
{
  char* label_break = 0;
  AstNode* loop = break_stmt->loop;
  if(loop->kind == AstNodeKind_WhileStmt)
    label_break = ((AstWhileStmt*)loop)->label_break;
  else if(loop->kind == AstNodeKind_ForStmt)
    label_break = ((AstForStmt*)loop)->label_break;
  else
    assert(0);

  int depth = break_stmt->nesting_depth;
  while(depth--)
    emit_instr(code, Opcode_LEAVE);
  emit_instr_str(code, Opcode_GOTO, label_break);
}

local void
gen_block(List* code, AstBlock* block)
{
  for(ListItem* list_item = block->access_links.first;
      list_item;
      list_item = list_item->next)
  {
    AccessLink* link = (AccessLink*)list_item->elem;
    emit_instr_reg(code, Opcode_PUSH_R, RegName_FP);
    assert(link->actv_rec_offset > 0);
    int offset = link->actv_rec_offset - 1;
    while(offset--)
    {
      emit_instr(code, Opcode_DECR); // TODO: explain why
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
}

local void
gen_proc(List* code, AstProc* proc)
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

local void
gen_if_stmt(List* code, AstIfStmt* if_stmt)
{
  gen_load_rvalue(code, if_stmt->cond_expr);

  if(if_stmt->else_body)
    emit_instr_str(code, Opcode_JUMPZ, if_stmt->label_else);
  else
    emit_instr_str(code, Opcode_JUMPZ, if_stmt->label_end);

  if(if_stmt->body->kind == AstNodeKind_Block)
    gen_block(code, (AstBlock*)if_stmt->body);
  else
    gen_statement(code, if_stmt->body);

  emit_instr_str(code, Opcode_GOTO, if_stmt->label_end);

  if(if_stmt->else_body)
  {
    emit_instr_str(code, Opcode_LABEL, if_stmt->label_else);
    AstNode* else_body = if_stmt->else_body;
    if(else_body->kind == AstNodeKind_Block)
      gen_block(code, (AstBlock*)else_body);
    else
      gen_statement(code, else_body);
  }

  emit_instr_str(code, Opcode_LABEL, if_stmt->label_end);
}

local void
gen_while_stmt(List* code, AstWhileStmt* while_stmt)
{
  emit_instr_str(code, Opcode_LABEL, while_stmt->label_eval);
  gen_load_rvalue(code, while_stmt->cond_expr);
  emit_instr_str(code, Opcode_JUMPZ, while_stmt->label_break);
  if(while_stmt->body->kind == AstNodeKind_Block)
    gen_block(code, (AstBlock*)while_stmt->body);
  else
    gen_statement(code, while_stmt->body);
  emit_instr_str(code, Opcode_GOTO, while_stmt->label_eval);

  emit_instr_str(code, Opcode_LABEL, while_stmt->label_break);
}

local void
gen_empty_stmt(List* code)
{
  emit_instr(code, Opcode_NOOP);
}

local void
gen_statement(List* code, AstNode* ast)
{
  if(ast->kind == AstNodeKind_BinExpr)
  {
    gen_bin_expr(code, (AstBinExpr*)ast);
    emit_instr_int(code, Opcode_GROW, -compute_type_width(ast->type));
  }
  else if(ast->kind == AstNodeKind_Call)
  {
    AstCall* call = (AstCall*)ast;
    gen_call(code, call);

    AstProc* proc = (AstProc*)call->proc_sym->ast;
    if(proc->ret_size > 0)
    {
      assert(compute_type_width(call->type) == proc->ret_size);
      emit_instr_int(code, Opcode_GROW, -proc->ret_size);
    }
  }
  else if(ast->kind == AstNodeKind_VarDecl)
  {
    AstVarDecl* var_decl = (AstVarDecl*)ast;
    if(var_decl->assign_expr)
      gen_statement(code, (AstNode*)var_decl->assign_expr);
  }
  else if(ast->kind == AstNodeKind_ReturnStmt)
    gen_return_stmt(code, (AstReturnStmt*)ast);
  else if(ast->kind == AstNodeKind_BreakStmt)
    gen_break_stmt(code, (AstBreakStmt*)ast);
  else if(ast->kind == AstNodeKind_IfStmt)
    gen_if_stmt(code, (AstIfStmt*)ast);
  else if(ast->kind == AstNodeKind_WhileStmt)
    gen_while_stmt(code, (AstWhileStmt*)ast);
  else if(ast->kind == AstNodeKind_Putc)
  {
    AstPutc* putc_ast = (AstPutc*)ast;
    gen_load_rvalue(code, putc_ast->expr);
    emit_instr(code, Opcode_PUTC);
  }
  else if(ast->kind == AstNodeKind_EmptyStmt)
    gen_empty_stmt(code);
  else
    assert(0);
}

void
codegen(List* code, uint8** data, int* data_size, AstModule* module)
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
    assert(var_decl->kind == AstNodeKind_VarDecl);

    AstNode* init_expr = var_decl->init_expr;
    if(init_expr && init_expr->kind == AstNodeKind_String)
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

local char*
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
    assert(0);
  return regname;
}

void
print_code(VmProgram* vm_program)
{
  for(ListItem* list_item = vm_program->instr_list.first;
      list_item;
      list_item = list_item->next)
  {
    Instruction* instr = (Instruction*)list_item->elem;
    switch(instr->opcode)
    {
      case Opcode_PUSH_I8:
      {
        if(instr->param_type == ParamType_Int32)
          print_instruction(vm_program, "push_i8 %d", instr->param.int_val);
        else
          assert(0);
      } break;

      case Opcode_PUSH_I32:
      {
        if(instr->param_type == ParamType_Int32)
          print_instruction(vm_program, "push_i32 %d", instr->param.int_val);
        else
          assert(0);
      } break;

      case Opcode_PUSH_R:
      {
        if(instr->param_type == ParamType_Reg)
          print_instruction(vm_program, "push_r %s", get_regname_str(instr->param.reg));
        else
          assert(0);
      } break;

      case Opcode_PUSH_F32:
      {
        if(instr->param_type == ParamType_Float32)
          print_instruction(vm_program, "push_f32 %f", instr->param.float_val);
        else
          assert(0);
      } break;

      case Opcode_POP_R:
      {
        if(instr->param_type == ParamType_Reg)
          print_instruction(vm_program, "pop_r %s", get_regname_str(instr->param.reg));
        else
          assert(0);
      }

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

      case Opcode_ADDF:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "addf");
      } break;

      case Opcode_SUBF:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "subf");
      } break;

      case Opcode_MULF:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "mulf");
      } break;

      case Opcode_DIVF:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "divf");
      } break;

      case Opcode_NEGF:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "negf");
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
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "return");
      } break;

      case Opcode_GROW:
      {
        assert(instr->param_type == ParamType_Int32);
        print_instruction(vm_program, "grow %d", instr->param.int_val);
      } break;

      case Opcode_NEW:
      {
        assert(instr->param_type == ParamType_Int32);
        print_instruction(vm_program, "new %d", instr->param.int_val);
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
          assert(0);
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

      case Opcode_PUTC:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "putc");
      } break;

      case Opcode_FLOAT_TO_INT:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "float_to_int");
      } break;

      case Opcode_INT_TO_FLOAT:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "int_to_float");
      } break;

      default:
        assert(0);
    }
  }
}


