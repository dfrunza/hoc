#include "hocc.h"

extern MemoryArena* arena;

SourceLocation* deflt_src_loc;

local bool32 do_initializer(TokenStream*, AstNode**);
local bool32 do_expression(TokenStream*, AstNode**);
local bool32 do_statement(TokenStream*, AstNode**);
local bool32 do_accessor(TokenStream*, AstNode**);
local bool32 do_unary_expr(TokenStream*, AstNode**);
local bool32 do_module(TokenStream*, List*);
local bool32 do_struct_member_list(TokenStream*, List*);

AstBlock*
new_block(SourceLocation* src_loc)
{
  AstBlock* node = mem_push_struct(arena, AstBlock);
  node->kind = AstNodeKind_Block;
  node->src_loc = *src_loc;
  list_init(&node->node_list);
  list_init(&node->decl_vars);
  return node;
}

local AstModule*
new_module(SourceLocation* src_loc)
{
  AstModule* node = mem_push_struct(arena, AstModule);
  node->kind = AstNodeKind_Module;
  node->src_loc = *src_loc;
  node->body = new_block(src_loc);
  return node;
}

AstId*
new_id(SourceLocation* src_loc, char* name)
{
  AstId* node = mem_push_struct(arena, AstId);
  node->kind = AstNodeKind_Id;
  node->src_loc = *src_loc;
  node->name = name;
  return node;
}

AstNode*
clone_ast_node(AstNode* node)
{
  AstNode* clone = 0;
  if(node->kind == AstNodeKind_Id)
  {
    clone = (AstNode*)mem_push_struct(arena, AstId);
    *(AstId*)clone = *(AstId*)node;
  }
  else if(node->kind == AstNodeKind_Pointer)
  {
    clone = (AstNode*)mem_push_struct(arena, AstPointer);
    ((AstPointer*)clone)->expr = clone_ast_node(((AstPointer*)node)->expr);
  }
  else
    fail("not implemented");
  return clone;
}

local AstEnum*
new_enum(SourceLocation* src_loc)
{
  AstEnum* node = mem_push_struct(arena, AstEnum);
  node->kind = AstNodeKind_Enum;
  list_init(&node->member_list);
  node->src_loc = *src_loc;
  return node;
}

local AstPointer*
new_pointer(SourceLocation* src_loc)
{
  AstPointer* node = mem_push_struct(arena, AstPointer);
  node->kind = AstNodeKind_Pointer;
  node->src_loc = *src_loc;
  return node;
}

AstCall*
new_call(SourceLocation* src_loc)
{
  AstCall* node = mem_push_struct(arena, AstCall);
  node->kind = AstNodeKind_Call;
  list_init(&node->args);
  node->src_loc = *src_loc;
  return node;
}

local AstArray*
new_array(SourceLocation* src_loc)
{
  AstArray* node = mem_push_struct(arena, AstArray);
  node->kind = AstNodeKind_Array;
  node->src_loc = *src_loc;
  return node;
}

local AstProc*
new_proc(SourceLocation* src_loc)
{
  AstProc* node = mem_push_struct(arena, AstProc);
  node->kind = AstNodeKind_Proc;
  list_init(&node->args);
  node->src_loc = *src_loc;
  return node;
}

AstBinExpr*
new_bin_expr(SourceLocation* src_loc)
{
  AstBinExpr* node = mem_push_struct(arena, AstBinExpr);
  node->kind = AstNodeKind_BinExpr;
  node->src_loc = *src_loc;
  return node;
}

local AstUnrExpr*
new_unr_expr(SourceLocation* src_loc)
{
  AstUnrExpr* node = mem_push_struct(arena, AstUnrExpr);
  node->kind = AstNodeKind_UnrExpr;
  node->src_loc = *src_loc;
  return node;
}

local AstLiteral*
new_int_literal(SourceLocation* src_loc)
{
  AstLiteral* node = mem_push_struct(arena, AstLiteral);
  node->kind = AstNodeKind_Literal;
  node->lit_kind = AstLiteralKind_Int;
  node->src_loc = *src_loc;
  return node;
}

AstVarDecl*
new_var_decl(SourceLocation* src_loc)
{
  AstVarDecl* node = mem_push_struct(arena, AstVarDecl);
  node->kind = AstNodeKind_VarDecl;
  node->src_loc = *src_loc;
  return node;
}

AstVarOccur*
new_var_occur(SourceLocation* src_loc)
{
  AstVarOccur* node = mem_push_struct(arena, AstVarOccur);
  node->kind = AstNodeKind_VarOccur;
  node->src_loc = *src_loc;
  return node;
}

local AstWhileStmt*
new_while_stmt(SourceLocation* src_loc)
{
  AstWhileStmt* node = mem_push_struct(arena, AstWhileStmt);
  node->kind = AstNodeKind_WhileStmt;
  node->src_loc = *src_loc;
  return node;
}

local AstForStmt*
new_for_stmt(SourceLocation* src_loc)
{
  AstForStmt* node = mem_push_struct(arena, AstForStmt);
  node->kind = AstNodeKind_ForStmt;
  node->src_loc = *src_loc;
  return node;
}

local AstIfStmt*
new_if_stmt(SourceLocation* src_loc)
{
  AstIfStmt* node = mem_push_struct(arena, AstIfStmt);
  node->kind = AstNodeKind_IfStmt;
  node->src_loc = *src_loc;
  return node;
}

local AstReturnStmt*
new_return_stmt(SourceLocation* src_loc)
{
  AstReturnStmt* node = mem_push_struct(arena, AstReturnStmt);
  node->kind = AstNodeKind_ReturnStmt;
  node->src_loc = *src_loc;
  return node;
}

local AstGotoStmt*
new_goto_stmt(SourceLocation* src_loc)
{
  AstGotoStmt* node = mem_push_struct(arena, AstGotoStmt);
  node->kind = AstNodeKind_GotoStmt;
  node->src_loc = *src_loc;
  return node;
}

local AstLabel*
new_label(SourceLocation* src_loc)
{
  AstLabel* node = mem_push_struct(arena, AstLabel);
  node->kind = AstNodeKind_Label;
  node->src_loc = *src_loc;
  return node;
}

local AstContinueStmt*
new_continue_stmt(SourceLocation* src_loc)
{
  AstContinueStmt* node = mem_push_struct(arena, AstContinueStmt);
  node->kind = AstNodeKind_ContinueStmt;
  node->src_loc = *src_loc;
  return node;
}

local AstBreakStmt*
new_break_stmt(SourceLocation* src_loc)
{
  AstBreakStmt* node = mem_push_struct(arena, AstBreakStmt);
  node->kind = AstNodeKind_BreakStmt;
  node->src_loc = *src_loc;
  return node;
}

local AstIncludeStmt*
new_include_stmt(SourceLocation* src_loc)
{
  AstIncludeStmt* node = mem_push_struct(arena, AstIncludeStmt);
  node->kind = AstNodeKind_IncludeStmt;
  node->src_loc = *src_loc;
  node->body = new_block(src_loc);
  return node;
}

local AstUnion*
new_union(SourceLocation* src_loc)
{
  AstUnion* node = mem_push_struct(arena, AstUnion);
  node->kind = AstNodeKind_Union;
  list_init(&node->member_list);
  node->src_loc = *src_loc;
  return node;
}

local AstStruct*
new_struct(SourceLocation* src_loc)
{
  AstStruct* node = mem_push_struct(arena, AstStruct);
  node->kind = AstNodeKind_Struct;
  list_init(&node->member_list);
  node->src_loc = *src_loc;
  return node;
}

local AstIniter*
new_initializer(SourceLocation* src_loc)
{
  AstIniter* node = mem_push_struct(arena, AstIniter);
  node->kind = AstNodeKind_Initializer;
  list_init(&node->member_list);
  node->src_loc = *src_loc;
  return node;
}

AstCast*
new_cast(SourceLocation* src_loc)
{
  AstCast* node = mem_push_struct(arena, AstCast);
  node->kind = AstNodeKind_Cast;
  node->src_loc = *src_loc;
  return node;
}

local AstEmptyStmt*
new_empty_stmt(SourceLocation* src_loc)
{
  AstEmptyStmt* node = mem_push_struct(arena, AstEmptyStmt);
  node->kind = AstNodeKind_EmptyStmt;
  node->src_loc = *src_loc;
  return node;
}

char*
get_ast_op_printstr(AstOpKind op)
{
  char* result = "???";
  if(op == AstOpKind__Null)
    result = "_Null";
  else if(op == AstOpKind_Add)
    result = "Add";
  else if(op == AstOpKind_Sub)
    result = "Sub";
  else if(op == AstOpKind_Div)
    result = "Div";
  else if(op == AstOpKind_Mul)
    result = "Mul";
  else if(op == AstOpKind_Mod)
    result = "Mod";
  else if(op == AstOpKind_Neg)
    result = "Neg";
  else if(op == AstOpKind_Assign)
    result = "Assign";
  else if(op == AstOpKind_PtrDeref)
    result = "PtrDeref";
  else if(op == AstOpKind_AddressOf)
    result = "AddressOf";
  else if(op == AstOpKind_MemberAccess)
    result = "MemberAccess";
  else if(op == AstOpKind_PtrMemberAccess)
    result = "PtrMemberAccess";
  else if(op == AstOpKind_PreDecrement)
    result = "PreDecrement";
  else if(op == AstOpKind_PostDecrement)
    result = "PostDecrement";
  else if(op == AstOpKind_PreIncrement)
    result = "PreIncrement";
  else if(op == AstOpKind_PostIncrement)
    result = "PostIncrement";
  else if(op == AstOpKind_LogicEquals)
    result = "LogicEquals";
  else if(op == AstOpKind_LogicNotEquals)
    result = "LogicNotEquals";
  else if(op == AstOpKind_LogicLess)
    result = "LogicLess";
  else if(op == AstOpKind_LogicLessEquals)
    result = "LogicLessEquals";
  else if(op == AstOpKind_LogicGreater)
    result = "LogicGreater";
  else if(op == AstOpKind_LogicGreaterEquals)
    result = "LogicGreaterEquals";
  else if(op == AstOpKind_LogicAnd)
    result = "LogicAnd";
  else if(op == AstOpKind_LogicOr)
    result = "LogicOr";
  else if(op == AstOpKind_LogicNot)
    result = "LogicNot";
  else if(op == AstOpKind_BitwiseAnd)
    result = "BitwiseAnd";
  else if(op == AstOpKind_BitwiseOr)
    result = "BitwiseOr";

  return result;
}

char*
get_ast_kind_printstr(AstNodeKind kind)
{
  char* result = "???";
  if(kind == AstNodeKind__Null)
    result = "_Null";
  else if(kind == AstNodeKind_BinExpr)
    result = "BinExpr";
  else if(kind == AstNodeKind_UnrExpr)
    result = "UnrExpr";
  else if(kind == AstNodeKind_Literal)
    result = "Literal";
  else if(kind == AstNodeKind_VarDecl)
    result = "VarDecl";
  else if(kind == AstNodeKind_VarOccur)
    result = "VarOccur";
  else if(kind == AstNodeKind_Block)
    result = "Block";
  else if(kind == AstNodeKind_Proc)
    result = "Proc";
  else if(kind == AstNodeKind_Id)
    result = "Id";
  else if(kind == AstNodeKind_WhileStmt)
    result = "WhileStmt";
  else if(kind == AstNodeKind_ForStmt)
    result = "ForStmt";
  else if(kind == AstNodeKind_IfStmt)
    result = "IfStmt";
  else if(kind == AstNodeKind_ReturnStmt)
    result = "ReturnStmt";
  else if(kind == AstNodeKind_BreakStmt)
    result = "BreakStmt";
  else if(kind == AstNodeKind_ContinueStmt)
    result = "ContinueStmt";
  else if(kind == AstNodeKind_GotoStmt)
    result = "GotoStmt";
  else if(kind == AstNodeKind_Label)
    result = "Label";
  else if(kind == AstNodeKind_IncludeStmt)
    result = "IncludeStmt";
  else if(kind == AstNodeKind_Module)
    result = "Module";
  else if(kind == AstNodeKind_Cast)
    result = "Cast";
  else if(kind == AstNodeKind_Call)
    result = "Call";
  else if(kind == AstNodeKind_Array)
    result = "Array";
  else if(kind == AstNodeKind_Pointer)
    result = "Pointer";
  else if(kind == AstNodeKind_Struct)
    result = "Struct";
  else if(kind == AstNodeKind_Union)
    result = "Union";
  else if(kind == AstNodeKind_Enum)
    result = "Enum";
  else if(kind == AstNodeKind_Initializer)
    result = "Initializer";
  else if(kind == AstNodeKind_EmptyStmt)
    result = "EmptyStmt";

  return result;
}

local char*
get_literal_printstr(AstLiteralKind kind)
{
  char* result = "???";
  if(kind == AstLiteralKind__Null)
    result = "_Null";
  else if(kind == AstLiteralKind_Int)
    result = "AstLiteralKind_Int";
  else if(kind == AstLiteralKind_Float)
    result = "AstLiteralKind_Float";
  else if(kind == AstLiteralKind_Bool)
    result = "AstLiteralKind_Bool";
  else if(kind == AstLiteralKind_String)
    result = "AstLiteralKind_String";
  else if(kind == AstLiteralKind_Char)
    result = "AstLiteralKind_Char";

  return result;
}

local bool32
do_semicolon(TokenStream* input)
{
  bool32 success = true;
  if(input->token.kind == TokenKind_Semicolon)
    success = get_next_token(input);
  else
    success = compile_error(&input->src_loc, "Expected `;`, actual `%s`", get_token_printstr(&input->token));
  return success;
}

local bool32
do_initializer_member_list(TokenStream* input, List* member_list)
{
  bool32 success = true;

  AstNode* member = 0;
  do
  {
    member = 0;
    if(input->token.kind == TokenKind_OpenBrace)
    {
      if(success = do_initializer(input, &member))
      {
        if(input->token.kind == TokenKind_Comma)
          success = get_next_token(input);
      }
    }
    if(success)
    {
      if(!member)
        success = do_expression(input, &member);

      if(success && member)
      {
        list_append(arena, member_list, member);
        if(input->token.kind == TokenKind_Comma)
          success = get_next_token(input);
      }
    }
  }
  while(success && member);
  return success;
}

local bool32
do_initializer(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    AstIniter* initer = new_initializer(&input->src_loc);
    *node = (AstNode*)initer;

    if(success = get_next_token(input) && do_initializer_member_list(input, &initer->member_list))
    {
      if(input->token.kind == TokenKind_CloseBrace)
        success = get_next_token(input);
      else
        success = compile_error(&input->src_loc, "Expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

local bool32
do_actual_arg_list(TokenStream* input, List* arg_list)
{
  bool32 success = true;

  AstNode* arg = 0;
  do
  {
    arg = 0;
    success = do_expression(input, &arg);
    if(success && arg)
    {
      list_append(arena, arg_list, arg);
      if(input->token.kind == TokenKind_Comma)
      {
        if((success = get_next_token(input)) && input->token.kind == TokenKind_CloseParens)
          success = compile_error(&input->src_loc, "Identifier expected, actual `%s`", get_token_printstr(&input->token));
      }
    }
  }
  while(success && arg);
  return success;
}

local bool32
do_statement_list(TokenStream* input, List* stmt_list)
{
  bool32 success = true;

  AstNode* stmt = 0;
  do
  {
    while(input->token.kind == TokenKind_Semicolon && (success = get_next_token(input)))
      ;

    if((success = do_statement(input, &stmt)) && stmt)
      list_append(arena, stmt_list, stmt);
  }
  while(success && stmt);
  return success;
}

local bool32
do_block(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    AstBlock* block = new_block(&input->src_loc);
    *node = (AstNode*)block;

    if(success = get_next_token(input) && do_statement_list(input, &block->node_list))
    {
      if(input->token.kind == TokenKind_CloseBrace)
        success = get_next_token(input);
      else
        success = compile_error(&input->src_loc, "Expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

local bool32
do_rest_of_id(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    // procedure call
    assert(left_node->kind == AstNodeKind_Id);
    AstCall* call = new_call(&input->src_loc);
    *node = (AstNode*)call;
    if(left_node->kind != AstNodeKind_Id)
      fail("the proc call can be applied to plain identifiers only");
    call->id = (AstId*)left_node;

    if(success = get_next_token(input) && do_actual_arg_list(input, &call->args))
    {
      if(input->token.kind == TokenKind_CloseParens)
        success = get_next_token(input);
      else
        success = compile_error(&input->src_loc, "Expected `)`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  else if(input->token.kind == TokenKind_OpenBracket)
  {
    // array
    AstArray* array = new_array(&input->src_loc);
    *node = (AstNode*)array;
    array->expr = left_node;

    if(success = get_next_token(input) && do_expression(input, &array->index))
    {
      if(input->token.kind == TokenKind_CloseBracket)
        success = get_next_token(input) && do_rest_of_id(input, *node, node);
      else
        success = compile_error(&input->src_loc, "Expected `]`, actual `%s`", get_token_printstr(&input->token));
    }
  }

  return success;
}

local bool32
do_rest_of_accessor(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool32 success = true;

  if(input->token.kind == TokenKind_Dot ||
     input->token.kind == TokenKind_ArrowRight)
  {
    AstBinExpr* bin_expr = new_bin_expr(&input->src_loc);
    *node = (AstNode*)bin_expr;
    bin_expr->left_operand = left_node;

    if(input->token.kind == TokenKind_Dot)
      bin_expr->op = AstOpKind_MemberAccess;
    else if(input->token.kind == TokenKind_ArrowRight)
      bin_expr->op = AstOpKind_PtrMemberAccess;

    if(success = get_next_token(input) && do_accessor(input, &bin_expr->right_operand))
    {
      if(bin_expr->right_operand)
        success = do_rest_of_accessor(input, *node, node);
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "Operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else if(input->token.kind == TokenKind_PlusPlus ||
          input->token.kind == TokenKind_MinusMinus)
  {
    AstUnrExpr* unr_expr = new_unr_expr(&input->src_loc);
    *node = (AstNode*)unr_expr;
    unr_expr->operand = left_node;

    if(input->token.kind == TokenKind_MinusMinus)
      unr_expr->op = AstOpKind_PostDecrement;
    else if(input->token.kind == TokenKind_PlusPlus)
      unr_expr->op = AstOpKind_PostIncrement;
    else
      assert(false);

    success = get_next_token(input);
  }

  return success;
}

local bool32
do_factor(TokenStream* input, AstNode** node)
{
  bool32 success = true;

  if((success = do_unary_expr(input, node)) && *node)
    success = do_rest_of_accessor(input, *node, node);
  return success;
}

local bool32
do_rest_of_factor(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool32 success = true;

  if(input->token.kind == TokenKind_Star ||
     input->token.kind == TokenKind_FwdSlash ||
     input->token.kind == TokenKind_Percent)
  {
    AstBinExpr* bin_expr = new_bin_expr(&input->src_loc);
    *node = (AstNode*)bin_expr;
    bin_expr->left_operand = left_node;

    if(input->token.kind == TokenKind_Star)
      bin_expr->op = AstOpKind_Mul;
    else if(input->token.kind == TokenKind_FwdSlash)
      bin_expr->op = AstOpKind_Div;
    else if(input->token.kind == TokenKind_Percent)
      bin_expr->op = AstOpKind_Mod;
    else
      assert(false);

    if(success = get_next_token(input) && do_factor(input, &bin_expr->right_operand))
    {
      if(bin_expr->right_operand)
        success = do_rest_of_factor(input, *node, node);
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "Operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }

  return success;
}

local bool32
do_term(TokenStream* input, AstNode** node)
{
  bool32 success = true;

  if((success = do_factor(input, node)) && *node)
    success = do_rest_of_factor(input, *node, node);
  return success;
}

local bool32
do_rest_of_term(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool32 success = true;

  if(input->token.kind == TokenKind_Plus ||
     input->token.kind == TokenKind_Minus ||
     input->token.kind == TokenKind_Pipe ||
     input->token.kind == TokenKind_PipePipe ||
     input->token.kind == TokenKind_Ampersand ||
     input->token.kind == TokenKind_AmpersandAmpersand)
  {
    AstBinExpr* bin_expr = new_bin_expr(&input->src_loc);
    *node = (AstNode*)bin_expr;
    bin_expr->left_operand = left_node;

    if(input->token.kind == TokenKind_Plus)
      bin_expr->op = AstOpKind_Add;
    else if(input->token.kind == TokenKind_Minus)
      bin_expr->op = AstOpKind_Sub;
    else if(input->token.kind == TokenKind_Pipe)
      bin_expr->op = AstOpKind_BitwiseOr;
    else if(input->token.kind == TokenKind_PipePipe)
      bin_expr->op = AstOpKind_LogicOr;
    else if(input->token.kind == TokenKind_Ampersand)
      bin_expr->op = AstOpKind_BitwiseAnd;
    else if(input->token.kind == TokenKind_AmpersandAmpersand)
      bin_expr->op = AstOpKind_LogicAnd;
    else
      assert(false);

    if(success = get_next_token(input) && do_term(input, &bin_expr->right_operand))
    {
      if(bin_expr->right_operand)
        success = do_rest_of_term(input, *node, node);
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "Operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }

  return success;
}

local bool32
do_assignment(TokenStream* input, AstNode** node)
{
  bool32 success = true;

  if((success = do_term(input, node)) && *node)
    success = do_rest_of_term(input, *node, node);
  return success;
}

local bool32
do_rest_of_assignment(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool32 success = true;

  if(input->token.kind == TokenKind_Equals ||
     input->token.kind == TokenKind_EqualsEquals ||
     input->token.kind == TokenKind_ExclamEquals ||
     input->token.kind == TokenKind_AngleLeft ||
     input->token.kind == TokenKind_AngleLeftEquals ||
     input->token.kind == TokenKind_AngleRight ||
     input->token.kind == TokenKind_AngleRightEquals)
  {
    AstBinExpr* bin_expr = new_bin_expr(&input->src_loc);
    *node = (AstNode*)bin_expr;
    bin_expr->left_operand = left_node;

    if(input->token.kind == TokenKind_Equals)
      bin_expr->op = AstOpKind_Assign;
    else if(input->token.kind == TokenKind_EqualsEquals)
      bin_expr->op = AstOpKind_LogicEquals;
    else if(input->token.kind == TokenKind_ExclamEquals)
      bin_expr->op = AstOpKind_LogicNotEquals;
    else if(input->token.kind == TokenKind_AngleLeft)
      bin_expr->op = AstOpKind_LogicLess;
    else if(input->token.kind == TokenKind_AngleLeftEquals)
      bin_expr->op = AstOpKind_LogicLessEquals;
    else if(input->token.kind == TokenKind_AngleRight)
      bin_expr->op = AstOpKind_LogicGreater;
    else if(input->token.kind == TokenKind_AngleRightEquals)
      bin_expr->op = AstOpKind_LogicGreaterEquals;

    if(success = get_next_token(input) && do_expression(input, &bin_expr->right_operand))
    {
      if(!bin_expr->right_operand)
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "Operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }

  return success;
}

local bool32
do_accessor(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    if(success = get_next_token(input) && do_expression(input, node))
    {
      if(*node)
      {
        if(input->token.kind == TokenKind_CloseParens)
          success = get_next_token(input);
        else
          success = compile_error(&input->src_loc, "Expected `)`, actual `%s`", get_token_printstr(&input->token));
      }
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "Expression expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else if(is_literal_token(input->token.kind) ||
          input->token.kind == TokenKind_True ||
          input->token.kind == TokenKind_False)
  {
    AstLiteral* literal = new_int_literal(&input->src_loc);
    *node = (AstNode*)literal;

    if(input->token.kind == TokenKind_IntNum)
    {
      literal->lit_kind = AstLiteralKind_Int;
      literal->int_val = *input->token.int_val;
    }
    else if(input->token.kind == TokenKind_FloatNum)
    {
      literal->lit_kind = AstLiteralKind_Float;
      literal->float_val = *input->token.float_val;
    }
    else if(input->token.kind == TokenKind_True ||
            input->token.kind == TokenKind_False)
    {
      literal->lit_kind = AstLiteralKind_Bool;
      literal->bool_val = (input->token.kind == TokenKind_True ? 1 : 0);
    }
    else if(input->token.kind == TokenKind_Char)
    {
      literal->lit_kind = AstLiteralKind_Char;
      literal->char_val = input->token.char_val;
    }
    else if(input->token.kind == TokenKind_String)
    {
      literal->lit_kind = AstLiteralKind_String;
      literal->str = input->token.str;
    }
    else
      assert(false);

    success = get_next_token(input);
  }
  else if(input->token.kind == TokenKind_Id)
  {
    AstId* id = new_id(&input->src_loc, input->token.lexeme);
    *node = (AstNode*)id;
    success = get_next_token(input) && do_rest_of_id(input, *node, node);
  }

  return success;
}

local bool32
do_rest_of_type_expr(TokenStream* input, AstNode* expr, AstNode** node)
{
  *node = expr;
  bool32 success = true;
  
  if(input->token.kind == TokenKind_Star)
  {
    AstPointer* ptr = new_pointer(&input->src_loc);
    *node = (AstNode*)ptr;
    ptr->expr = expr;

    success = get_next_token(input) && do_rest_of_type_expr(input, *node, node);
  }
  else if(input->token.kind == TokenKind_OpenBracket)
  {
    AstArray* array = new_array(&input->src_loc);
    *node = (AstNode*)array;
    array->expr = expr;

    if(success = get_next_token(input) && do_expression(input, &array->index))
    {
      if(input->token.kind == TokenKind_CloseBracket)
        success = get_next_token(input) && do_rest_of_type_expr(input, *node, node);
      else
        success = compile_error(&input->src_loc,  "Expected `]`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return true;
}

local bool32
do_type_expr(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Id)
  {
    AstId* id = new_id(&input->src_loc, input->token.lexeme);
    *node = (AstNode*)id;
    success = get_next_token(input) && do_rest_of_type_expr(input, *node, node);
  }
  return success;
}

local bool32
do_formal_arg(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  AstNode* type = 0;
  if((success = do_type_expr(input, &type)) && type)
  {
    AstVarDecl* var_decl = new_var_decl(&input->src_loc);
    *node = (AstNode*)var_decl;
    var_decl->type_expr = type;

    if(input->token.kind == TokenKind_Id)
    {
      var_decl->id = new_id(&input->src_loc, input->token.lexeme);
      success = get_next_token(input);
    }
  }
  return success;
}

local bool32
do_formal_arg_list(TokenStream* input, List* arg_list)
{
  bool32 success = true;

  AstNode* arg = 0;
  do
  {
    arg = 0;
    if((success = do_formal_arg(input, &arg)) && arg)
    {
      list_append(arena, arg_list, arg);
      if(input->token.kind == TokenKind_Comma)
        success = get_next_token(input);
      else if(input->token.kind != TokenKind_CloseParens)
        success = compile_error(&input->src_loc, "Expected `,`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  while(success && arg);
  return success;
}

local bool32
do_unary_expr(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Exclam ||
     input->token.kind == TokenKind_Star ||
     input->token.kind == TokenKind_Ampersand ||
     input->token.kind == TokenKind_Minus ||
     input->token.kind == TokenKind_MinusMinus ||
     input->token.kind == TokenKind_PlusPlus)
  {
    AstUnrExpr* unr_expr = new_unr_expr(&input->src_loc);
    *node = (AstNode*)unr_expr;

    if(input->token.kind == TokenKind_Exclam)
      unr_expr->op = AstOpKind_LogicNot;
    else if(input->token.kind == TokenKind_Star)
      unr_expr->op = AstOpKind_PtrDeref;
    else if(input->token.kind == TokenKind_Ampersand)
      unr_expr->op = AstOpKind_AddressOf;
    else if(input->token.kind == TokenKind_Minus)
      unr_expr->op = AstOpKind_Neg;
    else if(input->token.kind == TokenKind_MinusMinus)
      unr_expr->op = AstOpKind_PreDecrement;
    else if(input->token.kind == TokenKind_PlusPlus)
      unr_expr->op = AstOpKind_PreIncrement;
    else
      assert(false);

    if(success = get_next_token(input) && do_factor(input, &unr_expr->operand))
    {
      if(!unr_expr->operand)
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "Operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else if(input->token.kind == TokenKind_Cast)
  {
    // cast
    AstCast* cast = new_cast(&input->src_loc);
    *node = (AstNode*)cast;

    if(success = get_next_token(input) && input->token.kind == TokenKind_OpenParens)
    {
      if(success = get_next_token(input) && do_type_expr(input, &cast->type_to))
      {
        if(input->token.kind == TokenKind_CloseParens)
        {
          if(cast->type_to)
            success = get_next_token(input) && do_unary_expr(input, &cast->expr);
          else
          {
            putback_token(input);
            success = compile_error(&input->src_loc, "Type expression required, at `%s`", get_token_printstr(&input->token));
          }
        }
        else
          success = compile_error(&input->src_loc, "Expected `)`, actual `%s`", get_token_printstr(&input->token));
      }
    }
    else
      success = compile_error(&input->src_loc, "Expected `(`, actual `%s`", get_token_printstr(&input->token));
  }
  else
    success = do_accessor(input, node);

  return success;
}

local bool32
do_expression(TokenStream* input, AstNode** node)
{
  bool32 success = true;

  if((success = do_assignment(input, node)) && *node)
    success = do_rest_of_assignment(input, *node, node);
  return success;
}

local bool32
do_var_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  AstNode* type = 0;
  if(input->token.kind == TokenKind_Id)
  {
    if((success = do_type_expr(input, &type)) && type)
    {
      if(input->token.kind == TokenKind_Id)
      {
        AstVarDecl* var_decl = new_var_decl(&input->src_loc);
        *node = (AstNode*)var_decl;

        var_decl->type_expr = type;
        var_decl->id = new_id(&input->src_loc, input->token.lexeme);

        if((success = get_next_token(input)) && input->token.kind == TokenKind_Equals
           && (success = get_next_token(input)))
        {
          if(success = do_initializer(input, &var_decl->init_expr))
          {
            if(!var_decl->init_expr)
            {
              if(success = do_expression(input, &var_decl->init_expr))
              {
                if(!var_decl->init_expr)
                {
                  putback_token(input);
                  success = compile_error(&input->src_loc, "Expression required, at `%s`", get_token_printstr(&input->token));
                }
              }
            }
          }
        }
      }
      else
        success = compile_error(&input->src_loc, "Identifier expected, actual `%s`", get_token_printstr(&input->token));
    }
  }
  else
    success = compile_error(&input->src_loc, "Identifier expected, actual `%s`", get_token_printstr(&input->token));
  return success;
}

local bool32
do_for_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_For)
  {
    AstForStmt* for_stmt = new_for_stmt(&input->src_loc);
    *node = (AstNode*)for_stmt;

    if(!(success = get_next_token(input))) return success;

    if(input->token.kind == TokenKind_OpenParens)
    {
      success = get_next_token(input) && do_var_decl(input, &(AstNode*)for_stmt->decl_expr)
        && do_semicolon(input)
        && do_expression(input, &for_stmt->cond_expr)
        && do_semicolon(input)
        && do_expression(input, &for_stmt->loop_expr);
      if(!success) return success;

      if(input->token.kind == TokenKind_CloseParens)
      {
        if(success = get_next_token(input) && do_block(input, &(AstNode*)for_stmt->body))
        {
          if(!for_stmt->body)
          {
            if(success = do_statement(input, &(AstNode*)for_stmt->body))
            {
              if(for_stmt->body)
              {
                AstBlock* block = new_block(&input->src_loc);
                list_append(arena, &block->node_list, for_stmt->body);
                for_stmt->body = block;
              }
              else
              {
                putback_token(input);
                success = compile_error(&input->src_loc, "Statement required, at `%s`", get_token_printstr(&input->token));
              }
            }
          }
        }
      }
      else
        success = compile_error(&input->src_loc, "Expected `)`, actual `%s`", get_token_printstr(&input->token));
    }
    else
      success = compile_error(&input->src_loc, "Expected `(`, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

local bool32
do_while_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_While)
  {
    AstWhileStmt* while_stmt = new_while_stmt(&input->src_loc);
    *node = (AstNode*)while_stmt;

    if(!(success = get_next_token(input))) return success;

    if(input->token.kind == TokenKind_OpenParens)
    {
      success = get_next_token(input) && do_expression(input, &while_stmt->cond_expr);
      if(!success) return success;

      if(while_stmt->cond_expr)
      {
        if(input->token.kind == TokenKind_CloseParens)
        {
          if(!(success = get_next_token(input))) return success;

          if(!(success = do_block(input, &while_stmt->body))) return success;

          if(!while_stmt->body)
          {
            if((success = do_statement(input, &while_stmt->body)) && !while_stmt->body)
            {
              putback_token(input);
              success = compile_error(&input->src_loc, "Statement required, at `%s`", get_token_printstr(&input->token));
            }
          }
        }
        else
          success = compile_error(&input->src_loc, "Expected `)`, actual `%s`", get_token_printstr(&input->token));
      }
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "Expression required, at `%s`", get_token_printstr(&input->token));
      }
    }
    else
      success = compile_error(&input->src_loc, "Expected `(`, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

local bool32
do_else_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Else)
  {
    if(success = get_next_token(input) && do_block(input, node))
    {
      if(!*node)
      {
        if((success = do_statement(input, node)) && !*node)
        {
          putback_token(input);
          success = compile_error(&input->src_loc, "Statement required, at `%s`", get_token_printstr(&input->token));
        }
      }
    }
  }
  return success;
}

local bool32
do_if_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_If)
  {
    AstIfStmt* if_stmt = new_if_stmt(&input->src_loc);
    *node = (AstNode*)if_stmt;

    if(!(success = get_next_token(input))) return success;
    if(input->token.kind == TokenKind_OpenParens)
    {
      success = get_next_token(input) && do_expression(input, &if_stmt->cond_expr);
      if(!success) return success;

      if(input->token.kind == TokenKind_CloseParens)
      {
        if(if_stmt->cond_expr)
        {
          success = get_next_token(input) && do_block(input, &if_stmt->body);
          if(!success) return success;

          if(!if_stmt->body)
            success = do_statement(input, &if_stmt->body);

          if(success)
          {
            if(if_stmt->body)
              success = do_else_stmt(input, &if_stmt->else_body);
            else
            {
              putback_token(input);
              success = compile_error(&input->src_loc, "Statement required, at `%s`", get_token_printstr(&input->token));
            }
          }
        }
        else
        {
          putback_token(input);
          success = compile_error(&input->src_loc, "Expression required, at `%s`", get_token_printstr(&input->token));
        }
      }
      else
        success = compile_error(&input->src_loc, "Expected `)`, actual `%s`", get_token_printstr(&input->token));
    }
    else
      success = compile_error(&input->src_loc, "Expected `(`, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

local bool32
do_proc_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Proc)
  {
    AstProc* proc = new_proc(&input->src_loc);
    *node = (AstNode*)proc;

    if(success = get_next_token(input) && do_type_expr(input, &proc->ret_type_expr))
    {
      if(proc->ret_type_expr)
      {
        if(input->token.kind == TokenKind_Id)
        {
          proc->id = new_id(&input->src_loc, input->token.lexeme);

          if(!(success = get_next_token(input))) return success;

          if(input->token.kind == TokenKind_OpenParens)
          {
            if(!(success = get_next_token(input))) return success;

            if(success = do_formal_arg_list(input, &proc->args))
            {
              if(input->token.kind == TokenKind_CloseParens)
              {
                if(success = get_next_token(input) && do_block(input, &(AstNode*)proc->body))
                {
                  if(!proc->body && (proc->is_decl = success = do_semicolon(input)))
                    proc->body = new_block(&(*node)->src_loc);
                }
              }
              else
                success = compile_error(&input->src_loc, "Expected `)`, actual `%s`", get_token_printstr(&input->token));
            }
          }
          else
            success = compile_error(&input->src_loc, "Expected `(`, actual `%s`", get_token_printstr(&input->token));
        }
        else
          success = compile_error(&input->src_loc, "Identifier expected, actual `%s`", get_token_printstr(&input->token));
      }
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "Type expression required, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  return success;
}

local bool32
do_include_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Include)
  {
    if(!(success = get_next_token(input))) return success;
    if(input->token.kind == TokenKind_String)
    {
      AstIncludeStmt* incl_stmt = new_include_stmt(&input->src_loc);
      *node = (AstNode*)incl_stmt;

      String str = {0};
      str_init(&str, arena);
      str_append(&str, input->src_loc.file_path);
      path_make_dir(str.head);
      str_tidyup(&str);
      str_append(&str, input->token.str);
      incl_stmt->file_path = str_cap(&str);

      if(!(success = get_next_token(input))) return success;
      char* hoc_text = file_read_text(arena, incl_stmt->file_path);
      if(hoc_text)
      {
        TokenStream* inc_input = mem_push_struct(arena, TokenStream);
        init_token_stream(inc_input, hoc_text, incl_stmt->file_path);

        if(success = get_next_token(inc_input))
        {
          AstBlock* block = (AstBlock*)incl_stmt->body;
          success = do_module(inc_input, &block->node_list);
        }
      }
      else
        success = compile_error(&input->src_loc, "Could not read file `%s`", incl_stmt->file_path);
    }
    else
      success = compile_error(&input->src_loc, "String expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

local bool32
do_enum_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Enum)
  {
    if(!(success = get_next_token(input))) return success;

    if(input->token.kind == TokenKind_Id)
    {
      AstEnum* enum_decl = new_enum(&input->src_loc);
      *node = (AstNode*)enum_decl;
      enum_decl->id = new_id(&input->src_loc, input->token.lexeme);

      if(!(success = get_next_token(input))) return success;
      if(input->token.kind == TokenKind_OpenBrace)
      {
        if(!(success = get_next_token(input))) return success;
        AstNode* member = 0;
        do
        {
          member = 0;
          if(input->token.kind == TokenKind_Id)
          {
            member = (AstNode*)new_id(&input->src_loc, input->token.lexeme);
            list_append(arena, &enum_decl->member_list, member);

            if((success = get_next_token(input)) && input->token.kind == TokenKind_Comma)
              success = get_next_token(input);
            else if(input->token.kind != TokenKind_CloseBrace)
              member = 0;
          }
        }
        while(member && success);

        if(input->token.kind == TokenKind_CloseBrace)
          success = get_next_token(input);
        else
          success = compile_error(&input->src_loc, "Expected `}`, actual `%s`", get_token_printstr(&input->token));
      }
      else
        success = compile_error(&input->src_loc, "Expected `{`, actual `%s`", get_token_printstr(&input->token));
    }
    else
      success = compile_error(&input->src_loc, "Identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

local bool32
do_union_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Union)
  {
    AstUnion* union_decl = new_union(&input->src_loc);
    *node = (AstNode*)union_decl;

    if((success = get_next_token(input)) && input->token.kind == TokenKind_Id)
    {
      union_decl->id = new_id(&input->src_loc, input->token.lexeme);
      success = get_next_token(input);
    }

    if(success)
      success = do_struct_member_list(input, &union_decl->member_list);
  }
  return success;
}

local bool32
do_struct_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Struct)
  {
    AstStruct* struct_decl = new_struct(&input->src_loc);
    *node = (AstNode*)struct_decl;

    if((success = get_next_token(input)) && input->token.kind == TokenKind_Id)
    {
      struct_decl->id = new_id(&input->src_loc, input->token.lexeme);
      success = get_next_token(input);
    }

    if(success)
      success = do_struct_member_list(input, &struct_decl->member_list);
  }
  return success;
}

local bool32
do_struct_member_list(TokenStream* input, List* member_list)
{
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    if(!(success = get_next_token(input))) return success;
    AstNode* member = 0;
    do
    {
      member = 0;
      AstNode* type = 0;

      if(success = do_type_expr(input, &type))
      {
        if(!type)
        {
          if(input->token.kind == TokenKind_Union)
            success = do_union_decl(input, &type);
          else if(input->token.kind == TokenKind_Struct)
            success = do_struct_decl(input, &type);
        }

        if(success && type)
        {
          AstVarDecl* var_decl = new_var_decl(&input->src_loc);
          member = (AstNode*)var_decl;
          var_decl->type_expr = type;

          if(input->token.kind == TokenKind_Id)
          {
            var_decl->id = new_id(&input->src_loc, input->token.lexeme);
            success = get_next_token(input);
          }
          else if(type->kind == AstNodeKind_Struct ||
                  type->kind == AstNodeKind_Union)
          {
            /* anonymous struct/union */
          }
          else
            success = compile_error(&input->src_loc, "Identifier expected, actual `%s`", get_token_printstr(&input->token));

          if(success)
          {
            list_append(arena, member_list, member);
            success = do_semicolon(input);
          }
        }
      }
    }
    while(member && success);

    if(success)
    {
      if(input->token.kind == TokenKind_CloseBrace)
        success = get_next_token(input);
      else
        success = compile_error(&input->src_loc, "Expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  else
    success = compile_error(&input->src_loc, "Expected `{`, actual `%s`", get_token_printstr(&input->token));
  return success;
}

local bool32
do_var_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = false;

  if(input->token.kind == TokenKind_Var)
    success = get_next_token(input) && do_var_decl(input, node);
  return success;
}

local bool32
do_module_element(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Var)
    success = do_var_stmt(input, node) && do_semicolon(input);
  else if(input->token.kind == TokenKind_Include)
    success = do_include_stmt(input, node) && do_semicolon(input);
  else if(input->token.kind == TokenKind_Proc)
    success = do_proc_decl(input, node);
  else if(input->token.kind == TokenKind_Struct)
    success = do_struct_decl(input, node);
  else if(input->token.kind == TokenKind_Union)
    success = do_union_decl(input, node);
  else if(input->token.kind == TokenKind_Enum)
    success = do_enum_decl(input, node);
  else
  {
    if((success = do_expression(input, node)) && *node)
      success = do_semicolon(input);
  }
  return success;
}

local bool32
do_module(TokenStream* input, List* node_list)
{
  bool32 success = true;

  AstNode* node = 0;
  do
  {
    if((success = do_module_element(input, &node)) && node)
      list_append(arena, node_list, node);
  }
  while(success && node);

  if(success && !node)
  {
    if(input->token.kind != TokenKind_EndOfInput)
    {
      success = compile_error(&input->src_loc, "Unexpected token `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

local bool32
do_return_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Return)
  {
    AstReturnStmt* ret_stmt = new_return_stmt(&input->src_loc);
    *node = (AstNode*)ret_stmt;
    success = get_next_token(input) && do_expression(input, &ret_stmt->expr);
  }

  return success;
}

local bool32
do_label(TokenStream* input, AstNode* id, AstNode** node)
{
  *node = id;
  bool32 success = true;

  if(input->token.kind == TokenKind_Colon && (success = get_next_token(input)))
  {
    if(id->kind == AstNodeKind_Id)
    {
      AstLabel* label = new_label(&input->src_loc);
      *node = (AstNode*)label;
      label->id = (AstId*)id;
    }
    else
      success = compile_error(&input->src_loc, "Label identifier expected");
  }
  return success;
}

local bool32
do_goto_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Goto)
  {
    AstGotoStmt* goto_stmt = new_goto_stmt(&input->src_loc);
    *node = (AstNode*)goto_stmt;

    if(!(success = get_next_token(input))) return success;
    if(input->token.kind == TokenKind_Id)
    {
      goto_stmt->id = new_id(&input->src_loc, input->token.lexeme);
      success = get_next_token(input);
    }
    else
      success = compile_error(&input->src_loc, "Identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

local bool32
do_continue_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Continue)
  {
    *node = (AstNode*)new_continue_stmt(&input->src_loc);
    success = get_next_token(input);
  }
  return success;
}

local bool32
do_break_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Break)
  {
    *node = (AstNode*)new_break_stmt(&input->src_loc);
    success = get_next_token(input);
  }
  return success;
}

local bool32
do_statement(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Var)
    success = do_var_stmt(input, node) && do_semicolon(input);
  else if(input->token.kind == TokenKind_If)
    success = do_if_stmt(input, node);
  else if(input->token.kind == TokenKind_Else)
    success = compile_error(&input->src_loc, "Unmatched `else`");
  else if(input->token.kind == TokenKind_While)
    success = do_while_stmt(input, node);
  else if(input->token.kind == TokenKind_For)
    success = do_for_stmt(input, node);
  else if(input->token.kind == TokenKind_Return)
    success = do_return_stmt(input, node) && do_semicolon(input);
  else if(input->token.kind == TokenKind_Break)
    success = do_break_stmt(input, node) && do_semicolon(input);
  else if(input->token.kind == TokenKind_Continue)
    success = do_continue_stmt(input, node) && do_semicolon(input);
  else if(input->token.kind == TokenKind_Goto)
    success = do_goto_stmt(input, node) && do_semicolon(input);
  else if(input->token.kind == TokenKind_Semicolon)
  {
    *node = new_empty_stmt(&input->src_loc);
    success = get_next_token(input);
  }
  else if(input->token.kind == TokenKind_OpenBrace)
    success = do_block(input, node);
  else
  {
    if(success = do_expression(input, node))
    {
      if(*node)
      {
        if(input->token.kind == TokenKind_Colon)
          success = do_label(input, *node, node);
        else
          success = do_semicolon(input);
      }
    }
  }
  return success;
}

bool32
parse(TokenStream* input, AstNode** node)
{
  bool32 success = true;

  AstModule* module = new_module(&input->src_loc);
  *node = (AstNode*)module;
  module->file_path = input->src_loc.file_path;

  deflt_src_loc = mem_push_struct(arena, SourceLocation);
  deflt_src_loc->file_path = module->file_path;

  AstBlock* block = (AstBlock*)module->body;
  success = do_module(input, &block->node_list);
  return success;
}

local void
DEBUG_print_line(String* str, int indent_level, char* message, ...)
{
  for(int i = 0; i < indent_level; i++)
  {
    str_append(str, " ");
  }

  va_list varargs;
  va_start(varargs, message);
  str_printf_va(str, message, varargs);
  va_end(varargs);

  str_append(str, "\n");
}

local void
DEBUG_print_ast_node_list(String* str, int indent_level, List* node_list, char* tag)
{
  if(node_list->first)
  {
    if(tag)
    {
      DEBUG_print_line(str, indent_level, tag);
      ++indent_level;
    }
    for(ListItem* list_item = node_list->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* node = (AstNode*)list_item->elem;
      DEBUG_print_ast_node(str, indent_level, node, 0);
    }
  }
}

void
DEBUG_print_ast_node(String* str, int indent_level, AstNode* node, char* tag)
{
  if(node)
  {
    if(tag)
    {
      DEBUG_print_line(str, indent_level, tag);
      ++indent_level;
    }
    DEBUG_print_line(str, indent_level, get_ast_kind_printstr(node->kind));
    ++indent_level;

    if(node->kind == AstNodeKind_Module)
    {
      AstModule* module = (AstModule*)node;
      DEBUG_print_line(str, indent_level, "file_path: \"%s\"", module->file_path);
      DEBUG_print_ast_node(str, indent_level, (AstNode*)module->body, "body");
    }
    else if(node->kind == AstNodeKind_IncludeStmt)
    {
      AstIncludeStmt* incl_stmt = (AstIncludeStmt*)node;
      DEBUG_print_line(str, indent_level, "file_path: \"%s\"", incl_stmt->file_path);
      DEBUG_print_ast_node(str, indent_level, (AstNode*)incl_stmt->body, "body");
    }
    else if(node->kind == AstNodeKind_Proc)
    {
      AstProc* proc = (AstProc*)node;
      DEBUG_print_ast_node(str, indent_level, proc->ret_type_expr, "ret_type");
      DEBUG_print_ast_node(str, indent_level, (AstNode*)proc->id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &proc->args, "args");
      DEBUG_print_ast_node(str, indent_level, (AstNode*)proc->body, "body");
    }
    else if(node->kind == AstNodeKind_VarDecl)
    {
      AstVarDecl* var_decl = (AstVarDecl*)node;
      DEBUG_print_ast_node(str, indent_level, var_decl->type_expr, "type_id");
      DEBUG_print_ast_node(str, indent_level, (AstNode*)var_decl->id, "id");
      DEBUG_print_ast_node(str, indent_level, var_decl->init_expr, "init_expr");
    }
    else if(node->kind == AstNodeKind_VarOccur)
    {
      AstVarOccur* var_occur = (AstVarOccur*)node;
      DEBUG_print_ast_node(str, indent_level, var_occur->id, "id");
      DEBUG_print_line(str, indent_level, "decl_block_offset: %d", var_occur->decl_block_offset);
    }
    else if(node->kind == AstNodeKind_Id)
    {
      AstId* id = (AstId*)node;
      DEBUG_print_line(str, indent_level, "name: %s", id->name);
    }
    else if(node->kind == AstNodeKind_Block)
    {
      AstBlock* block = (AstBlock*)node;
      DEBUG_print_ast_node_list(str, indent_level, &block->node_list, "node_list");
    }
    else if(node->kind == AstNodeKind_BinExpr)
    {
      AstBinExpr* bin_expr = (AstBinExpr*)node;
      DEBUG_print_line(str, indent_level, "op: %s", get_ast_op_printstr(bin_expr->op));
      DEBUG_print_ast_node(str, indent_level, bin_expr->left_operand, "left_operand");
      DEBUG_print_ast_node(str, indent_level, bin_expr->right_operand, "right_operand");
    }
    else if(node->kind == AstNodeKind_UnrExpr)
    {
      AstUnrExpr* unr_expr = (AstUnrExpr*)node;
      DEBUG_print_line(str, indent_level, "op: %s", get_ast_op_printstr(unr_expr->op));
      DEBUG_print_ast_node(str, indent_level, unr_expr->operand, "operand");
    }
    else if(node->kind == AstNodeKind_IfStmt)
    {
      AstIfStmt* if_stmt = (AstIfStmt*)node;
      DEBUG_print_ast_node(str, indent_level, if_stmt->cond_expr, "cond_expr");
      DEBUG_print_ast_node(str, indent_level, if_stmt->body, "body");
      DEBUG_print_ast_node(str, indent_level, if_stmt->else_body, "else_body");
    }
    else if(node->kind == AstNodeKind_ReturnStmt)
    {
      AstReturnStmt* ret_stmt = (AstReturnStmt*)node;
      DEBUG_print_ast_node(str, indent_level, ret_stmt->expr, "expr");
    }
    else if(node->kind == AstNodeKind_Literal)
    {
      AstLiteral* literal = (AstLiteral*)node;
      DEBUG_print_line(str, indent_level, get_literal_printstr(literal->lit_kind));
      if(literal->lit_kind == AstLiteralKind_Int)
        DEBUG_print_line(str, indent_level, "int_val: %d", literal->int_val);
      else if(literal->lit_kind == AstLiteralKind_Float)
        DEBUG_print_line(str, indent_level, "float_val: %f", literal->float_val);
      else if(literal->lit_kind == AstLiteralKind_Bool)
        DEBUG_print_line(str, indent_level, "bool_val: %d", literal->bool_val);
      else if(literal->lit_kind == AstLiteralKind_Char)
      {
        char buf[3] = {0};
        print_char(buf, literal->char_val);
        DEBUG_print_line(str, indent_level, "char_val: '%s'", buf);
      }
      else if(literal->lit_kind == AstLiteralKind_String)
        DEBUG_print_line(str, indent_level, "str: \"%s\"", literal->str);
      else
        assert(false);
    }
    else if(node->kind == AstNodeKind_WhileStmt)
    {
      AstWhileStmt* while_stmt = (AstWhileStmt*)node;
      DEBUG_print_ast_node(str, indent_level, while_stmt->cond_expr, "cond_expr");
      DEBUG_print_ast_node(str, indent_level, while_stmt->body, "body");
    }
    else if(node->kind == AstNodeKind_ForStmt)
    {
      AstForStmt* for_stmt = (AstForStmt*)node;
      DEBUG_print_ast_node(str, indent_level, (AstNode*)for_stmt->decl_expr, "decl_expr");
      DEBUG_print_ast_node(str, indent_level, for_stmt->cond_expr, "cond_expr");
      DEBUG_print_ast_node(str, indent_level, for_stmt->loop_expr, "loop_expr");
      DEBUG_print_ast_node(str, indent_level, (AstNode*)for_stmt->body, "body");
    }
    else if(node->kind == AstNodeKind_Cast)
    {
      AstCast* cast = (AstCast*)node;
      DEBUG_print_ast_node(str, indent_level, cast->type_to, "type_to");
      DEBUG_print_ast_node(str, indent_level, cast->expr, "expr");
    }
    else if(node->kind == AstNodeKind_Array)
    {
      AstArray* array = (AstArray*)node;
      DEBUG_print_ast_node(str, indent_level, array->expr, "expr");
      DEBUG_print_ast_node(str, indent_level, array->index, "index");
    }
    else if(node->kind == AstNodeKind_Pointer)
    {
      AstPointer* ptr = (AstPointer*)node;
      DEBUG_print_ast_node(str, indent_level, ptr->expr, "expr");
    }
    else if(node->kind == AstNodeKind_Call)
    {
      AstCall* call = (AstCall*)node;
      DEBUG_print_ast_node(str, indent_level, (AstNode*)call->id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &call->args, "args");
    }
    else if(node->kind == AstNodeKind_BreakStmt
            || node->kind == AstNodeKind_ContinueStmt
            || node->kind == AstNodeKind_EmptyStmt)
    {
      /* nothing to do */
    }
    else if(node->kind == AstNodeKind_Struct)
    {
      AstStruct* struct_decl = (AstStruct*)node;
      DEBUG_print_ast_node(str, indent_level, (AstNode*)struct_decl->id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &struct_decl->member_list, "member_list");
    }
    else if(node->kind == AstNodeKind_Union)
    {
      AstUnion* union_decl = (AstUnion*)node;
      DEBUG_print_ast_node(str, indent_level, (AstNode*)union_decl->id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &union_decl->member_list, "member_list");
    }
    else if(node->kind == AstNodeKind_Enum)
    {
      AstEnum* enum_decl = (AstEnum*)node;
      DEBUG_print_ast_node(str, indent_level, (AstNode*)enum_decl->id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &enum_decl->member_list, "member_list");
    }
    else if(node->kind == AstNodeKind_Initializer)
    {
      AstIniter* initer = (AstIniter*)node;
      DEBUG_print_ast_node_list(str, indent_level, &initer->member_list, "member_list");
    }
    else if(node->kind == AstNodeKind_GotoStmt)
    {
      AstGotoStmt* goto_stmt = (AstGotoStmt*)node;
      DEBUG_print_ast_node(str, indent_level, (AstNode*)goto_stmt->id, "id");
    }
    else if(node->kind == AstNodeKind_Label)
    {
      AstLabel* label = (AstLabel*)node;
      DEBUG_print_ast_node(str, indent_level, (AstNode*)label->id, "id");
    }
    else
    {
      assert(false);
    } 
  }
}
