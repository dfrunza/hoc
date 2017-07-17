#include "hocc.h"

extern MemoryArena* arena;

local void gen_load_rvalue(List*, AstNode*);
local void gen_load_lvalue(List*, AstNode*);
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
gen_bin_expr(List* code, AstNode* expr_ast)
{
  assert(expr_ast->kind == AstNodeKind_BinExpr);
  use(expr_ast, bin_expr);

  if(bin_expr->op == AstOpKind_Assign)
  {
    gen_load_rvalue(code, bin_expr->rhs);

    assert(bin_expr->lhs->kind = AstNodeKind_VarOccur);
    gen_load_lvalue(code, bin_expr->lhs);

    emit_instr(code, Opcode_STORE);
  }
  else
  {
    gen_load_rvalue(code, bin_expr->lhs);
    gen_load_rvalue(code, bin_expr->rhs);

    switch(bin_expr->op)
    {
      case AstOpKind_Add:
      case AstOpKind_Sub:
      case AstOpKind_Mul:
      case AstOpKind_Div:
      case AstOpKind_Mod:
      {
        Type* expr_type = expr_ast->type;
        if(expr_type->kind == TypeKind_Basic)
        {
          use(expr_type, basic);
          if(basic->kind == BasicTypeKind_Int)
          {
            if(bin_expr->op == AstOpKind_Add)
              emit_instr(code, Opcode_ADD);
            else if(bin_expr->op == AstOpKind_Sub)
              emit_instr(code, Opcode_SUB);
            else if(bin_expr->op == AstOpKind_Mul)
              emit_instr(code, Opcode_MUL);
            else if(bin_expr->op == AstOpKind_Div)
              emit_instr(code, Opcode_DIV);
            else if(bin_expr->op == AstOpKind_Mod)
              emit_instr(code, Opcode_MOD);
          }
          else if(basic->kind == BasicTypeKind_Float)
          {
            if(bin_expr->op == AstOpKind_Add)
              emit_instr(code, Opcode_ADDF);
            else if(bin_expr->op == AstOpKind_Sub)
              emit_instr(code, Opcode_SUBF);
            else if(bin_expr->op == AstOpKind_Mul)
              emit_instr(code, Opcode_MULF);
            else if(bin_expr->op == AstOpKind_Div)
              emit_instr(code, Opcode_DIVF);
          }
        }
        else
          assert(false);
      }
      break;

      case AstOpKind_LogicEquals:
      case AstOpKind_LogicNotEquals:
      case AstOpKind_LogicLess:
      case AstOpKind_LogicGreater:
      {
        if(bin_expr->op == AstOpKind_LogicEquals)
          emit_instr(code, Opcode_CMPEQ);
        else if(bin_expr->op == AstOpKind_LogicNotEquals)
          emit_instr(code, Opcode_CMPNEQ);
        else if(bin_expr->op == AstOpKind_LogicLess)
          emit_instr(code, Opcode_CMPLSS);
        else if(bin_expr->op == AstOpKind_LogicGreater)
          emit_instr(code, Opcode_CMPGRT);
        else
          assert(false);
      } break;

      case AstOpKind_LogicAnd:
      case AstOpKind_LogicOr:
      {
        gen_load_rvalue(code, bin_expr->lhs);
        emit_instr(code, Opcode_DUP);

        if(bin_expr->op == AstOpKind_LogicAnd)
          emit_instr_str(code, Opcode_JUMPZ, bin_expr->label_end);
        else if(bin_expr->op == AstOpKind_LogicOr)
          emit_instr_str(code, Opcode_JUMPNZ, bin_expr->label_end);
        else
          assert(false);

        emit_instr(code, Opcode_POP);
        gen_load_rvalue(code, bin_expr->rhs);
        emit_instr_str(code, Opcode_LABEL, bin_expr->label_end);
      } break;

      case AstOpKind_LogicLessEquals:
      case AstOpKind_LogicGreaterEquals:
      {
        gen_load_rvalue(code, bin_expr->lhs);
        gen_load_rvalue(code, bin_expr->rhs);
        if(bin_expr->op == AstOpKind_LogicLessEquals)
          emit_instr(code, Opcode_CMPLSS);
        else if(bin_expr->op == AstOpKind_LogicGreaterEquals)
          emit_instr(code, Opcode_CMPGRT);
        else
          assert(false);
        emit_instr(code, Opcode_DUP);

        emit_instr_str(code, Opcode_JUMPNZ, bin_expr->label_end);
        emit_instr(code, Opcode_POP);
        gen_load_rvalue(code, bin_expr->lhs);
        gen_load_rvalue(code, bin_expr->rhs);
        emit_instr(code, Opcode_CMPEQ);
        emit_instr_str(code, Opcode_LABEL, bin_expr->label_end);
      } break;

      default:
        assert(false);
    }
  }
}

local void
gen_unr_expr(List* code, AstNode* expr_ast)
{
  assert(expr_ast->kind == AstNodeKind_UnrExpr);
  use(expr_ast, unr_expr);

  gen_load_rvalue(code, unr_expr->operand);
  if(unr_expr->op == AstOpKind_Neg)
  {
    Type* expr_type = expr_ast->type;
    if(expr_type->kind == TypeKind_Basic)
    {
      if(expr_type->basic.kind == BasicTypeKind_Int)
        emit_instr(code, Opcode_NEG);
      else if(expr_type->basic.kind == BasicTypeKind_Float)
        emit_instr(code, Opcode_NEGF);
      else
        assert(false);
    }
    else
      assert(false);
  }
  else if(unr_expr->op == AstOpKind_LogicNot)
  {
    emit_instr_int(code, Opcode_PUSH, 0);
    emit_instr( code, Opcode_CMPEQ);
  }
  else
    assert(false);
}

local void
gen_call(List* code, AstNode* call_ast)
{
  assert(call_ast->kind == AstNodeKind_Call);
  use(call_ast, call);
  AstNode* proc_ast = call->proc_sym->ast;
  use(proc_ast, proc);
  emit_instr_int(code, Opcode_ALLOC, proc->ret_size);

  for(ListItem* list_item = call->args.first;
      list_item;
      list_item = list_item->next)
  {
    gen_load_rvalue(code, (AstNode*)list_item->elem);
  }

  emit_instr_str(code, Opcode_CALL, proc->label);
  emit_instr_int(code, Opcode_POP, proc->args_size); // discard args
}

local void
gen_cast(List* code, AstNode* cast_ast)
{
  assert(cast_ast->kind == AstNodeKind_Cast);
  use(cast_ast, cast);

  gen_load_rvalue(code, cast->expr);

  Type* to_type = cast->to_type->type;
  Type* from_type = cast->expr->type;
  if(to_type->kind == TypeKind_Basic)
  {
    if(from_type->basic.kind != to_type->basic.kind)
    {
      if(to_type->basic.kind == BasicTypeKind_Float)
      {
        if(from_type->basic.kind == BasicTypeKind_Int)
          emit_instr(code, Opcode_INT_TO_FLOAT);
        else assert(false);
      }
      else if(to_type->basic.kind == BasicTypeKind_Int)
      {
        if(from_type->basic.kind == BasicTypeKind_Float)
          emit_instr(code, Opcode_FLOAT_TO_INT);
        else assert(false);
      }
      else if(to_type->basic.kind == BasicTypeKind_Bool)
      {
        if(from_type->basic.kind != BasicTypeKind_Int)
          assert(false);
      }
      else assert(false);
    }
  }
  else assert(false);
}

local void
gen_load_lvalue(List* code, AstNode* var_ast)
{
  assert(var_ast->kind == AstNodeKind_VarOccur);
  use(var_ast, var_occur);
  DataArea* data = var_occur->data;
  AccessLink* link = var_occur->link;

  emit_instr_reg(code, Opcode_PUSH, RegName_FP);
  if(link) 
  {
    // this is a non-local
    assert(link->data.loc < 0); // relative to FP
    emit_instr_int(code, Opcode_PUSH, link->data.loc);
    emit_instr(code, Opcode_ADD);
    emit_instr(code, Opcode_LOAD); // access link is on the stack now
  }
  emit_instr_int(code, Opcode_PUSH, data->loc);
  emit_instr(code, Opcode_ADD);
}

local void
gen_load_rvalue(List* code, AstNode* ast)
{
  if(ast->kind == AstNodeKind_VarOccur)
  {
    gen_load_lvalue(code, ast);
    emit_instr(code, Opcode_LOAD);
  }
  else if(ast->kind == AstNodeKind_Call)
    gen_call(code, ast);
  else if(ast->kind == AstNodeKind_Literal)
  {
    if(ast->literal.kind == AstLiteralKind_Int || ast->literal.kind == AstLiteralKind_Bool)
      emit_instr_int(code, Opcode_PUSH, ast->literal.int_val);
    else if(ast->literal.kind == AstLiteralKind_Float)
      emit_instr_float(code, Opcode_PUSHF, ast->literal.float_val);
    else
      assert(false);
  }
  else if(ast->kind == AstNodeKind_BinExpr)
    gen_bin_expr(code, ast);
  else if(ast->kind == AstNodeKind_UnrExpr)
    gen_unr_expr(code, ast);
  else if(ast->kind == AstNodeKind_Cast)
    gen_cast(code, ast);
  else
    assert(false);
}

local void
gen_return_stmt(List* code, AstNode* ret_ast)
{
  assert(ret_ast->kind == AstNodeKind_ReturnStmt);
  use(ret_ast, ret_stmt);

  if(ret_stmt->assign_expr)
  {
    gen_bin_expr(code, ret_stmt->assign_expr);
    emit_instr(code, Opcode_POP);
  }

  use(ret_stmt->proc, proc);
  int depth = ret_stmt->nesting_depth;
  while(depth--)
    emit_instr(code, Opcode_LEAVE);
  emit_instr_str(code, Opcode_GOTO, proc->label_end);
}

local void
gen_break_stmt(List* code, AstNode* break_ast)
{
  assert(break_ast->kind == AstNodeKind_BreakStmt);
  use(break_ast, loop_ctrl);

  char* label_break = 0;
  auto loop = loop_ctrl->loop;
  if(loop_ctrl->loop->kind == AstNodeKind_WhileStmt)
    label_break = loop->while_stmt.label_break;
  else if(loop_ctrl->loop->kind == AstNodeKind_WhileStmt)
    label_break = loop->for_stmt.label_break;
  else
    assert(false);

  int depth = loop_ctrl->nesting_depth;
  while(depth--)
    emit_instr(code, Opcode_LEAVE);
  emit_instr_str(code, Opcode_GOTO, label_break);
}

local void
gen_block(List* code, AstNode* block_ast)
{
  assert(block_ast->kind == AstNodeKind_Block);
  use(block_ast, block);

  for(ListItem* list_item = block->access_links.first;
      list_item;
      list_item = list_item->next)
  {
    auto link = (AccessLink*)list_item->elem;
    emit_instr_reg(code, Opcode_PUSH, RegName_FP);
    assert(link->actv_rec_offset > 0);
    int offset = link->actv_rec_offset - 1;
    while(offset--)
    {
      emit_instr(code, Opcode_DECR); // TODO: explain why
      emit_instr(code, Opcode_LOAD);
    }
  }

  emit_instr(code, Opcode_ENTER);

  emit_instr_int(code, Opcode_ALLOC, block->locals_size);

  for(ListItem* list_item = block->node_list.first;
      list_item;
      list_item = list_item->next)
  {
    gen_statement(code, (AstNode*)list_item->elem);
  }

  emit_instr(code, Opcode_LEAVE);
}

local void
gen_proc(List* code, AstNode* proc_ast)
{
  assert(proc_ast->kind == AstNodeKind_Proc);
  use(proc_ast, proc);

  emit_instr_str(code, Opcode_LABEL, proc->label);
  emit_instr_int(code, Opcode_ALLOC, proc->locals_size);

  auto body_block = &proc->body->block;
  for(ListItem* list_item = body_block->node_list.first;
      list_item;
      list_item = list_item->next)
  {
    gen_statement(code, (AstNode*)list_item->elem);
  }

  emit_instr_str(code, Opcode_LABEL, proc->label_end);
  emit_instr(code, Opcode_RETURN);
}

local void
gen_if_stmt(List* code, AstNode* if_ast)
{
  assert(if_ast->kind == AstNodeKind_IfStmt);
  use(if_ast, if_stmt);

  gen_load_rvalue(code, if_stmt->cond_expr);

  if(if_stmt->else_body)
    emit_instr_str(code, Opcode_JUMPZ, if_stmt->label_else);
  else
    emit_instr_str(code, Opcode_JUMPZ, if_stmt->label_end);

  if(if_stmt->body->kind == AstNodeKind_Block)
    gen_block(code, if_stmt->body);
  else
    gen_statement(code, if_stmt->body);

  emit_instr_str(code, Opcode_GOTO, if_stmt->label_end);

  if(if_stmt->else_body)
  {
    emit_instr_str(code, Opcode_LABEL, if_stmt->label_else);
    AstNode* else_body = if_stmt->else_body;
    if(else_body->kind == AstNodeKind_Block)
      gen_block(code, else_body);
    else
      gen_statement(code, else_body);
  }

  emit_instr_str(code, Opcode_LABEL, if_stmt->label_end);
}

local void
gen_while_stmt(List* code, AstNode* while_ast)
{
  assert(while_ast->kind == AstNodeKind_WhileStmt);
  use(while_ast, while_stmt);

  emit_instr_str(code, Opcode_LABEL, while_stmt->label_eval);
  gen_load_rvalue(code, while_stmt->cond_expr);
  emit_instr_str(code, Opcode_JUMPZ, while_stmt->label_break);
  if(while_stmt->body->kind == AstNodeKind_Block)
    gen_block(code, while_stmt->body);
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
    assert(ast->bin_expr.op == AstOpKind_Assign);
    gen_bin_expr(code, ast);
    emit_instr(code, Opcode_POP);
  }
  else if(ast->kind == AstNodeKind_Call)
  {
    gen_call(code, ast);
    emit_instr(code, Opcode_POP);
  }
  else if(ast->kind == AstNodeKind_VarDecl)
  {
    use(ast, var_decl);
    if(var_decl->assign_expr)
      gen_statement(code, var_decl->assign_expr);
  }
  else if(ast->kind == AstNodeKind_ReturnStmt)
    gen_return_stmt(code, ast);
  else if(ast->kind == AstNodeKind_BreakStmt)
    gen_break_stmt(code, ast);
  else if(ast->kind == AstNodeKind_IfStmt)
    gen_if_stmt(code, ast);
  else if(ast->kind == AstNodeKind_WhileStmt)
    gen_while_stmt(code, ast);
  else if(ast->kind == AstNodeKind_EmptyStmt)
    gen_empty_stmt(code);
  else
    assert(false);
}

void
codegen(List* code, AstNode* module_ast)
{
  assert(module_ast->kind == AstNodeKind_Module);
  use(module_ast, module);

  gen_statement(code, module->main_stmt);
  emit_instr(code, Opcode_HALT);

  auto body_block = &module->body->block;
  for(ListItem* list_item = body_block->node_list.first;
      list_item;
      list_item = list_item->next)
  {
    auto ast = (AstNode*)list_item->elem;
    if(ast->kind == AstNodeKind_Proc)
    {
      //FIXME: Remove proc decls from the AST
      if(!ast->proc.is_decl)
        gen_proc(code, ast);
    }
    else
      fail("not implemented");
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
    assert(false);
  return regname;
}

void
print_code(VmProgram* vm_program)
{
  for(ListItem* list_item = vm_program->instr_list.first;
      list_item;
      list_item = list_item->next)
  {
    auto instr = (Instruction*)list_item->elem;
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

      case Opcode_PUSHF:
      {
        if(instr->param_type == ParamType_Float32)
          print_instruction(vm_program, "pushf %f", instr->param.float_val);
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
        assert(false);
    }
  }
}


