#include "hocc.h"

extern MemoryArena* arena;

internal bool
is_logical_operator(AstOpKind op)
{
  return op >= AstOpKind_LogicEquals && op <= AstOpKind_LogicNot;
}

internal AstNode*
new_node(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_block(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Block;
  node->src_loc = *src_loc;
  list_init(&node->block.node_list);
  list_init(&node->block.decl_vars);
  list_init(&node->block.stmts);
  return node;
}

internal AstNode*
new_module(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Module;
  node->src_loc = *src_loc;
  node->module.body = new_block(src_loc);
  return node;
}

AstNode*
new_id(SourceLocation* src_loc, char* name)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Id;
  node->src_loc = *src_loc;
  node->id.name = name;
  return node;
}

internal AstNode*
new_enum(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Enum;
  list_init(&node->enum_decl.member_list);
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_pointer(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Pointer;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_call(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Call;
  list_init(&node->call.actual_args);
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_array(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Array;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_proc(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Proc;
  list_init(&node->proc.formal_args);
  node->src_loc = *src_loc;
  return node;
}

AstNode*
new_bin_expr(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_BinExpr;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_unr_expr(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_UnrExpr;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_int_literal(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_Int;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_float_literal(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_Float;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_bool_literal(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_Bool;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_string_literal(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_String;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
new_var_decl(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_VarDecl;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_while_stmt(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_WhileStmt;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_for_stmt(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_ForStmt;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_if_stmt(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_IfStmt;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_return_stmt(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_ReturnStmt;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_goto_stmt(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_GotoStmt;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_label(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Label;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_continue_stmt(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_ContinueStmt;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_break_stmt(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_BreakStmt;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_include_stmt(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_IncludeStmt;
  node->src_loc = *src_loc;
  node->incl_stmt.body = new_block(src_loc);
  return node;
}

internal AstNode*
new_union(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Union;
  list_init(&node->union_decl.member_list);
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_struct(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Struct;
  list_init(&node->struct_decl.member_list);
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_initializer(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Initializer;
  list_init(&node->initer.member_list);
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_cast(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Cast;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_empty_stmt(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
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

internal char*
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

internal bool do_initializer(TokenStream*, AstNode**);
internal bool do_expression(TokenStream*, AstNode**);
internal bool do_statement(TokenStream*, AstNode**);
internal bool do_formal_arg_decl(TokenStream*, AstNode**);
internal bool do_unary_expr(TokenStream*, AstNode**);
internal bool do_module(TokenStream*, List*);
internal bool do_struct_member_list(TokenStream*, List*);
internal bool do_accessor(TokenStream*, AstNode**);
internal bool do_type_expr(TokenStream*, AstNode**);
internal bool do_var_decl(TokenStream*, AstNode**);

internal bool
do_semicolon(TokenStream* input)
{
  bool success = true;
  if(input->token.kind == TokenKind_Semicolon)
    success = get_next_token(input);
  else
    success = compile_error(&input->src_loc, __FILE__, __LINE__,
                            "Expected `;`, actual `%s`", get_token_printstr(&input->token));
  return success;
}

internal bool
do_initializer_member_list(TokenStream* input, List* member_list)
{
  bool success = true;

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

internal bool
do_initializer(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    *node = new_initializer(&input->src_loc);
    AstInitializer* initer = &(*node)->initer;

    if(success = get_next_token(input) && do_initializer_member_list(input, &initer->member_list))
    {
      if(input->token.kind == TokenKind_CloseBrace)
        success = get_next_token(input);
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

internal bool
do_actual_arg_list(TokenStream* input, List* arg_list)
{
  bool success = true;

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
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Identifier expected, actual `%s`", get_token_printstr(&input->token));
      }
    }
  }
  while(success && arg);
  return success;
}

internal bool
do_formal_arg_list(TokenStream* input, List* arg_list)
{
  bool success = true;

  AstNode* arg = 0;
  do
  {
    arg = 0;
    if((success = do_formal_arg_decl(input, &arg)) && arg)
    {
      list_append(arena, arg_list, arg);
      if(input->token.kind == TokenKind_Comma)
        success = get_next_token(input);
      else if(input->token.kind != TokenKind_CloseParens)
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `,`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  while(success && arg);
  return success;
}

internal bool
do_statement_list(TokenStream* input, List* stmt_list)
{
  bool success = true;

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

internal bool
do_block(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    *node = new_block(&input->src_loc);
    if(success = get_next_token(input) && do_statement_list(input, &(*node)->block.node_list))
    {
      if(input->token.kind == TokenKind_CloseBrace)
        success = get_next_token(input);
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__, "Expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

internal bool
do_rest_of_id(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    // procedure call
    *node = new_call(&input->src_loc);
    AstCall* call = &(*node)->call;
    call->id = left_node;

    if(success = get_next_token(input) && do_actual_arg_list(input, &call->actual_args))
    {
      if(input->token.kind == TokenKind_CloseParens)
        success = get_next_token(input);
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `)`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  else if(input->token.kind == TokenKind_OpenBracket)
  {
    // array
    *node = new_array(&input->src_loc);
    AstArray* array = &(*node)->array;
    array->expr = left_node;

    if(success = get_next_token(input) && do_expression(input, &array->index))
    {
      if(input->token.kind == TokenKind_CloseBracket)
        success = get_next_token(input) && do_rest_of_id(input, *node, node);
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__, "Expected `]`, actual `%s`", get_token_printstr(&input->token));
    }
  }

  return success;
}

internal bool
do_rest_of_accessor(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == TokenKind_Dot ||
     input->token.kind == TokenKind_ArrowRight)
  {
    *node = new_bin_expr(&input->src_loc);
    AstBinExpr* expr = &(*node)->bin_expr;
    expr->lhs = left_node;

    if(input->token.kind == TokenKind_Dot)
      expr->op = AstOpKind_MemberAccess;
    else if(input->token.kind == TokenKind_ArrowRight)
      expr->op = AstOpKind_PtrMemberAccess;

    if(success = get_next_token(input) && do_accessor(input, &expr->rhs))
    {
      if(expr->rhs)
        success = do_rest_of_accessor(input, *node, node);
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else if(input->token.kind == TokenKind_PlusPlus ||
          input->token.kind == TokenKind_MinusMinus)
  {
    *node = new_unr_expr(&input->src_loc);
    AstUnrExpr* expr = &(*node)->unr_expr;
    expr->operand = left_node;

    if(input->token.kind == TokenKind_MinusMinus)
      expr->op = AstOpKind_PostDecrement;
    else if(input->token.kind == TokenKind_PlusPlus)
      expr->op = AstOpKind_PostIncrement;
    else
      assert(false);

    success = get_next_token(input);
  }

  return success;
}

internal bool
do_factor(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = do_unary_expr(input, node)) && *node)
    success = do_rest_of_accessor(input, *node, node);
  return success;
}

internal bool
do_rest_of_factor(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == TokenKind_Star ||
     input->token.kind == TokenKind_FwdSlash ||
     input->token.kind == TokenKind_Percent)
  {
    *node = new_bin_expr(&input->src_loc);
    AstBinExpr* expr = &(*node)->bin_expr;
    expr->lhs = left_node;

    if(input->token.kind == TokenKind_Star)
      expr->op = AstOpKind_Mul;
    else if(input->token.kind == TokenKind_FwdSlash)
      expr->op = AstOpKind_Div;
    else if(input->token.kind == TokenKind_Percent)
      expr->op = AstOpKind_Mod;
    else
      assert(false);

    if(success = get_next_token(input) && do_factor(input, &expr->rhs))
    {
      if(expr->rhs)
        success = do_rest_of_factor(input, *node, node);
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }

  return success;
}

internal bool
do_term(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = do_factor(input, node)) && *node)
    success = do_rest_of_factor(input, *node, node);
  return success;
}

internal bool
do_rest_of_term(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == TokenKind_Plus ||
     input->token.kind == TokenKind_Minus ||
     input->token.kind == TokenKind_Pipe ||
     input->token.kind == TokenKind_PipePipe ||
     input->token.kind == TokenKind_Ampersand ||
     input->token.kind == TokenKind_AmpersandAmpersand)
  {
    *node = new_bin_expr(&input->src_loc);
    AstBinExpr* expr = &(*node)->bin_expr;
    expr->lhs = left_node;

    if(input->token.kind == TokenKind_Plus)
      expr->op = AstOpKind_Add;
    else if(input->token.kind == TokenKind_Minus)
      expr->op = AstOpKind_Sub;
    else if(input->token.kind == TokenKind_Pipe)
      expr->op = AstOpKind_BitwiseOr;
    else if(input->token.kind == TokenKind_PipePipe)
      expr->op = AstOpKind_LogicOr;
    else if(input->token.kind == TokenKind_Ampersand)
      expr->op = AstOpKind_BitwiseAnd;
    else if(input->token.kind == TokenKind_AmpersandAmpersand)
      expr->op = AstOpKind_LogicAnd;
    else
      assert(false);

    if(success = get_next_token(input) && do_term(input, &expr->rhs))
    {
      if(expr->rhs)
        success = do_rest_of_term(input, *node, node);
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }

  return success;
}

internal bool
do_assignment(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = do_term(input, node)) && *node)
    success = do_rest_of_term(input, *node, node);
  return success;
}

internal bool
do_rest_of_assignment(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == TokenKind_Equals ||
     input->token.kind == TokenKind_EqualsEquals ||
     input->token.kind == TokenKind_ExclamEquals ||
     input->token.kind == TokenKind_AngleLeft ||
     input->token.kind == TokenKind_AngleLeftEquals ||
     input->token.kind == TokenKind_AngleRight ||
     input->token.kind == TokenKind_AngleRightEquals)
  {
    *node = new_bin_expr(&input->src_loc);
    AstBinExpr* expr = &(*node)->bin_expr;
    expr->lhs = left_node;

    if(input->token.kind == TokenKind_Equals)
      expr->op = AstOpKind_Assign;
    else if(input->token.kind == TokenKind_EqualsEquals)
      expr->op = AstOpKind_LogicEquals;
    else if(input->token.kind == TokenKind_ExclamEquals)
      expr->op = AstOpKind_LogicNotEquals;
    else if(input->token.kind == TokenKind_AngleLeft)
      expr->op = AstOpKind_LogicLess;
    else if(input->token.kind == TokenKind_AngleLeftEquals)
      expr->op = AstOpKind_LogicLessEquals;
    else if(input->token.kind == TokenKind_AngleRight)
      expr->op = AstOpKind_LogicGreater;
    else if(input->token.kind == TokenKind_AngleRightEquals)
      expr->op = AstOpKind_LogicGreaterEquals;

    if(success = get_next_token(input) && do_expression(input, &expr->rhs))
    {
      if(!expr->rhs)
      {
        putback_token(input);
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }

  return success;
}

internal bool
do_accessor(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    if(success = get_next_token(input) && do_expression(input, node))
    {
      if(*node)
      {
        if(input->token.kind == TokenKind_CloseParens)
          success = get_next_token(input);
        else
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Expected `)`, actual `%s`", get_token_printstr(&input->token));
      }
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expression expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else if(is_literal_token(input->token.kind) ||
          input->token.kind == TokenKind_True ||
          input->token.kind == TokenKind_False)
  {
    *node = new_int_literal(&input->src_loc);
    AstLiteral* literal = &(*node)->literal;

    if(input->token.kind == TokenKind_IntNum)
    {
      literal->kind = AstLiteralKind_Int;
      literal->int_val = *input->token.int_val;
    }
    else if(input->token.kind == TokenKind_FloatNum)
    {
      literal->kind = AstLiteralKind_Float;
      literal->float_val = *input->token.float_val;
    }
    else if(input->token.kind == TokenKind_True ||
            input->token.kind == TokenKind_False)
    {
      literal->kind = AstLiteralKind_Bool;
      literal->bool_val = (input->token.kind == TokenKind_True ? 1 : 0);
    }
    else if(input->token.kind == TokenKind_Char)
    {
      literal->kind = AstLiteralKind_Char;
      literal->char_val = input->token.char_val;
    }
    else if(input->token.kind == TokenKind_String)
    {
      literal->kind = AstLiteralKind_String;
      literal->str = input->token.str;
    }
    else
      assert(false);

    success = get_next_token(input);
  }
  else if(input->token.kind == TokenKind_Id)
  {
    *node = new_id(&input->src_loc, input->token.lexeme);
    success = get_next_token(input) && do_rest_of_id(input, *node, node);
  }

  return success;
}

internal bool
do_unary_expr(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Exclam ||
     input->token.kind == TokenKind_Star ||
     input->token.kind == TokenKind_Ampersand ||
     input->token.kind == TokenKind_Minus ||
     input->token.kind == TokenKind_MinusMinus ||
     input->token.kind == TokenKind_PlusPlus)
  {
    *node = new_unr_expr(&input->src_loc);
    AstUnrExpr* expr = &(*node)->unr_expr;

    if(input->token.kind == TokenKind_Exclam)
      expr->op = AstOpKind_LogicNot;
    else if(input->token.kind == TokenKind_Star)
      expr->op = AstOpKind_PtrDeref;
    else if(input->token.kind == TokenKind_Ampersand)
      expr->op = AstOpKind_AddressOf;
    else if(input->token.kind == TokenKind_Minus)
      expr->op = AstOpKind_Neg;
    else if(input->token.kind == TokenKind_MinusMinus)
      expr->op = AstOpKind_PreDecrement;
    else if(input->token.kind == TokenKind_PlusPlus)
      expr->op = AstOpKind_PreIncrement;
    else
      assert(false);

    if(success = get_next_token(input) && do_factor(input, &expr->operand))
    {
      if(!expr->operand)
      {
        putback_token(input);
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else if(input->token.kind == TokenKind_AngleLeft)
  {
    // cast
    *node = new_cast(&input->src_loc);
    AstCast* cast = &(*node)->cast;

    if(success = get_next_token(input) && do_type_expr(input, &cast->type))
    {
      if(input->token.kind == TokenKind_AngleRight)
      {
        if(cast->type)
          success = get_next_token(input) && do_unary_expr(input, &cast->expr);
        else
        {
          putback_token(input);
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Type expression required, at `%s`", get_token_printstr(&input->token));
        }
      }
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `>`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  else
    success = do_accessor(input, node);

  return success;
}

internal bool
do_expression(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = do_assignment(input, node)) && *node)
    success = do_rest_of_assignment(input, *node, node);
  return success;
}

internal bool
do_formal_arg_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* type = 0;
  if((success = do_type_expr(input, &type)) && type)
  {
    *node = new_var_decl(&input->src_loc);
    AstVarDecl* var_decl = &(*node)->var_decl;
    var_decl->type = type;

    if(input->token.kind == TokenKind_Id)
    {
      var_decl->id = new_id(&input->src_loc, input->token.lexeme);
      success = get_next_token(input);
    }
  }
  return success;
}

internal bool
do_for_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_For)
  {
    *node = new_for_stmt(&input->src_loc);
    AstForStmt* for_stmt = &(*node)->for_stmt;

    if(!(success = get_next_token(input))) return success;

    if(input->token.kind == TokenKind_OpenParens)
    {
      success = get_next_token(input) && do_var_decl(input, &for_stmt->decl_expr)
        && do_semicolon(input)
        && do_expression(input, &for_stmt->cond_expr)
        && do_semicolon(input)
        && do_expression(input, &for_stmt->loop_expr);
      if(!success) return success;

      if(input->token.kind == TokenKind_CloseParens)
      {
        if(success = get_next_token(input) && do_block(input, &for_stmt->body))
        {
          if(!for_stmt->body)
          {
            if(success = do_statement(input, &for_stmt->body))
            {
              if(for_stmt->body)
              {
                AstNode* block = new_block(&input->src_loc);
                list_append(arena, &block->block.node_list, for_stmt->body);
                for_stmt->body = block;
              }
              else
              {
                putback_token(input);
                success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                        "Statement required, at `%s`", get_token_printstr(&input->token));
              }
            }
          }
        }
      }
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `)`, actual `%s`", get_token_printstr(&input->token));
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Expected `(`, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

internal bool
do_while_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_While)
  {
    *node = new_while_stmt(&input->src_loc);
    AstWhileStmt* while_stmt = &(*node)->while_stmt;

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
              success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                      "Statement required, at `%s`", get_token_printstr(&input->token));
            }
          }
        }
        else
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Expected `)`, actual `%s`", get_token_printstr(&input->token));
      }
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expression required, at `%s`", get_token_printstr(&input->token));
      }
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Expected `(`, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

internal bool
do_else_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Else)
  {
    if(success = get_next_token(input) && do_block(input, node))
    {
      if(!*node)
      {
        if((success = do_statement(input, node)) && !*node)
        {
          putback_token(input);
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Statement required, at `%s`", get_token_printstr(&input->token));
        }
      }
    }
  }
  return success;
}

internal bool
do_if_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_If)
  {
    *node = new_if_stmt(&input->src_loc);
    AstIfStmt* if_stmt = &(*node)->if_stmt;

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
              success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                      "Statement required, at `%s`", get_token_printstr(&input->token));
            }
          }
        }
        else
        {
          putback_token(input);
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Expression required, at `%s`", get_token_printstr(&input->token));
        }
      }
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `)`, actual `%s`", get_token_printstr(&input->token));
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Expected `(`, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

internal bool
do_proc_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Proc)
  {
    *node = new_proc(&input->src_loc);
    AstProc* proc = &(*node)->proc;

    if(success = get_next_token(input) && do_type_expr(input, &proc->ret_type))
    {
      if(proc->ret_type)
      {
        if(input->token.kind == TokenKind_Id)
        {
          proc->id = new_id(&input->src_loc, input->token.lexeme);

          if(!(success = get_next_token(input))) return success;

          if(input->token.kind == TokenKind_OpenParens)
          {
            if(!(success = get_next_token(input))) return success;

            if(success = do_formal_arg_list(input, &proc->formal_args))
            {
              if(input->token.kind == TokenKind_CloseParens)
              {
                if(success = get_next_token(input) && do_block(input, &proc->body))
                {
                  if(!proc->body)
                    success = do_semicolon(input);
                }
              }
              else
                success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                        "Expected `)`, actual `%s`", get_token_printstr(&input->token));
            }
          }
          else
            success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                    "Expected `(`, actual `%s`", get_token_printstr(&input->token));
        }
        else
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Identifier expected, actual `%s`", get_token_printstr(&input->token));
      }
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Type expression required, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  return success;
}

internal bool
do_include_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Include)
  {
    if(!(success = get_next_token(input))) return success;
    if(input->token.kind == TokenKind_String)
    {
      *node = new_include_stmt(&input->src_loc);
      AstIncludeStmt* incl_stmt = &(*node)->incl_stmt;

      String str = {0};
      str_init(&str, arena);
      str_append(&str, input->src_loc.file_path);
      path_make_dir(str.head);
      str_tidyup(&str);
      str_append(&str, input->token.str);
      incl_stmt->file_path = str.head;

      if(!(success = get_next_token(input))) return success;
      char* hoc_text = file_read_text(arena, incl_stmt->file_path);
      if(hoc_text)
      {
        TokenStream* inc_input = mem_push_struct(arena, TokenStream, 1);
        init_token_stream(inc_input, hoc_text, incl_stmt->file_path);

        if(success = get_next_token(inc_input))
        {
          AstBlock* block = &incl_stmt->body->block;
          success = do_module(inc_input, &block->node_list);
        }
      }
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Could not read file `%s`", incl_stmt->file_path);
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "String expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

internal bool
do_enum_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Enum)
  {
    if(!(success = get_next_token(input))) return success;

    if(input->token.kind == TokenKind_Id)
    {
      *node = new_enum(&input->src_loc);
      AstEnum* enum_decl = &(*node)->enum_decl;
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
            member = new_id(&input->src_loc, input->token.lexeme);
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
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Expected `}`, actual `%s`", get_token_printstr(&input->token));
      }
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `{`, actual `%s`", get_token_printstr(&input->token));
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

internal bool
do_union_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Union)
  {
    *node = new_union(&input->src_loc);
    AstUnion* union_decl = &(*node)->union_decl;

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

internal bool
do_struct_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Struct)
  {
    *node = new_struct(&input->src_loc);
    AstStruct* struct_decl = &(*node)->struct_decl;

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

internal bool
do_struct_member_list(TokenStream* input, List* member_list)
{
  bool success = true;

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
          member = new_var_decl(&input->src_loc);
          AstVarDecl* var_decl = &member->var_decl;
          var_decl->type = type;

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
          {
            success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                    "Identifier expected, actual `%s`", get_token_printstr(&input->token));
          }

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
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  else
    success = compile_error(&input->src_loc, __FILE__, __LINE__,
                            "Expected `{`, actual `%s`", get_token_printstr(&input->token));
  return success;
}

internal bool
do_rest_of_type_expr(TokenStream* input, AstNode* expr, AstNode** node)
{
  *node = expr;
  bool success = true;
  
  if(input->token.kind == TokenKind_Star)
  {
    *node = new_pointer(&input->src_loc);
    AstPointer* ptr = &(*node)->ptr;
    ptr->expr = expr;

    success = get_next_token(input) && do_rest_of_type_expr(input, *node, node);
  }
  else if(input->token.kind == TokenKind_OpenBracket)
  {
    *node = new_array(&input->src_loc);
    AstArray* array = &(*node)->array;
    array->expr = expr;

    if(success = get_next_token(input) && do_expression(input, &array->index))
    {
      if(input->token.kind == TokenKind_CloseBracket)
        success = get_next_token(input) && do_rest_of_type_expr(input, *node, node);
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__, "Expected `]`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return true;
}

internal bool
do_type_expr(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Id)
  {
    *node = new_id(&input->src_loc, input->token.lexeme);
    success = get_next_token(input) && do_rest_of_type_expr(input, *node, node);
  }
  return success;
}

internal bool
do_var_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* type = 0;
  if((success = do_type_expr(input, &type)) && type)
  {
    if(input->token.kind == TokenKind_Id)
    {
      *node = new_var_decl(&input->src_loc);
      AstVarDecl* var_decl = &(*node)->var_decl;
      var_decl->type = type;
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
                success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                        "Expression required, at `%s`", get_token_printstr(&input->token));
              }
            }
          }
        }
      }
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

internal bool
do_var_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = false;

  if(input->token.kind == TokenKind_Var)
  {
    if(success = get_next_token(input) && do_var_decl(input, node))
    {
      if(!*node)
      {
        putback_token(input);
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Variable declaration required, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  return success;
}

internal bool
do_module_element(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

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

internal bool
do_module(TokenStream* input, List* node_list)
{
  bool success = true;

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
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Unexpected token `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

internal bool
do_return_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Return)
  {
    *node = new_return_stmt(&input->src_loc);
    success = get_next_token(input) && do_expression(input, &(*node)->ret_stmt.expr);
  }

  return success;
}

internal bool
do_label(TokenStream* input, AstNode* id, AstNode** node)
{
  *node = id;
  bool success = true;

  if(input->token.kind == TokenKind_Colon && (success = get_next_token(input)))
  {
    if(id->kind == AstNodeKind_Id)
    {
      *node = new_label(&input->src_loc);
      AstLabel* label = &(*node)->label;
      label->id = id;
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__, "Label identifier expected");
  }
  return success;
}

internal bool
do_goto_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Goto)
  {
    *node = new_goto_stmt(&input->src_loc);
    AstGotoStmt* goto_stmt = &(*node)->goto_stmt;

    if(!(success = get_next_token(input))) return success;
    if(input->token.kind == TokenKind_Id)
    {
      goto_stmt->id = new_id(&input->src_loc, input->token.lexeme);
      success = get_next_token(input);
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

internal bool
do_continue_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Continue)
  {
    *node = new_continue_stmt(&input->src_loc);
    success = get_next_token(input);
  }
  return success;
}

internal bool
do_break_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Break)
  {
    *node = new_break_stmt(&input->src_loc);
    success = get_next_token(input);
  }
  return success;
}

internal bool
do_statement(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Var)
    success = do_var_stmt(input, node) && do_semicolon(input);
  else if(input->token.kind == TokenKind_If)
    success = do_if_stmt(input, node);
  else if(input->token.kind == TokenKind_Else)
    success = compile_error(&input->src_loc, __FILE__, __LINE__, "Unmatched `else`");
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

bool
parse(TokenStream* input, AstNode** node)
{
  bool success = true;

  *node = new_module(&input->src_loc);
  AstModule* module = &(*node)->module;
  module->file_path = input->src_loc.file_path;

  AstBlock* block = &module->body->block;
  success = do_module(input, &block->node_list);
  return success;
}

internal void
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

internal void
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
      AstNode* node = list_item->elem;
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
      DEBUG_print_line(str, indent_level, "file_path: \"%s\"", node->module.file_path);
      DEBUG_print_ast_node(str, indent_level, node->module.body, "body");
    }
    else if(node->kind == AstNodeKind_IncludeStmt)
    {
      DEBUG_print_line(str, indent_level, "file_path: \"%s\"", node->incl_stmt.file_path);
      DEBUG_print_ast_node(str, indent_level, node->incl_stmt.body, "body");
    }
    else if(node->kind == AstNodeKind_Proc)
    {
      DEBUG_print_ast_node(str, indent_level, node->proc.ret_type, "ret_type");
      DEBUG_print_ast_node(str, indent_level, node->proc.id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &node->proc.formal_args, "formal_args");
      DEBUG_print_ast_node(str, indent_level, node->proc.body, "body");
    }
    else if(node->kind == AstNodeKind_VarDecl)
    {
      DEBUG_print_ast_node(str, indent_level, node->var_decl.type, "type");
      DEBUG_print_ast_node(str, indent_level, node->var_decl.id, "id");
      DEBUG_print_ast_node(str, indent_level, node->var_decl.init_expr, "init_expr");
    }
    else if(node->kind == AstNodeKind_VarOccur)
    {
      DEBUG_print_line(str, indent_level, "name: %s", node->var_occur.name);
    }
    else if(node->kind == AstNodeKind_Id)
    {
      DEBUG_print_line(str, indent_level, "name: %s", node->id.name);
    }
    else if(node->kind == AstNodeKind_Block)
    {
      DEBUG_print_ast_node_list(str, indent_level, &node->block.node_list, "node_list");
    }
    else if(node->kind == AstNodeKind_BinExpr)
    {
      DEBUG_print_line(str, indent_level, "op: %s", get_ast_op_printstr(node->bin_expr.op));
      DEBUG_print_ast_node(str, indent_level, node->bin_expr.lhs, "lhs");
      DEBUG_print_ast_node(str, indent_level, node->bin_expr.rhs, "rhs");
    }
    else if(node->kind == AstNodeKind_UnrExpr)
    {
      DEBUG_print_line(str, indent_level, "op: %s", get_ast_op_printstr(node->unr_expr.op));
      DEBUG_print_ast_node(str, indent_level, node->unr_expr.operand, "operand");
    }
    else if(node->kind == AstNodeKind_IfStmt)
    {
      DEBUG_print_ast_node(str, indent_level, node->if_stmt.cond_expr, "cond_expr");
      DEBUG_print_ast_node(str, indent_level, node->if_stmt.body, "body");
      DEBUG_print_ast_node(str, indent_level, node->if_stmt.else_body, "else_body");
    }
    else if(node->kind == AstNodeKind_ReturnStmt)
    {
      DEBUG_print_ast_node(str, indent_level, node->ret_stmt.expr, "expr");
    }
    else if(node->kind == AstNodeKind_Literal)
    {
      DEBUG_print_line(str, indent_level, get_literal_printstr(node->literal.kind));
      if(node->literal.kind == AstLiteralKind_Int)
        DEBUG_print_line(str, indent_level, "int_val: %d", node->literal.int_val);
      else if(node->literal.kind == AstLiteralKind_Float)
        DEBUG_print_line(str, indent_level, "float_val: %f", node->literal.float_val);
      else if(node->literal.kind == AstLiteralKind_Bool)
        DEBUG_print_line(str, indent_level, "bool_val: %d", node->literal.bool_val);
      else if(node->literal.kind == AstLiteralKind_Char)
      {
        char buf[3] = {0};
        print_char(buf, node->literal.char_val);
        DEBUG_print_line(str, indent_level, "char_val: '%s'", buf);
      }
      else if(node->literal.kind == AstLiteralKind_String)
        DEBUG_print_line(str, indent_level, "str: \"%s\"", node->literal.str);
      else
        assert(false);
    }
    else if(node->kind == AstNodeKind_WhileStmt)
    {
      DEBUG_print_ast_node(str, indent_level, node->while_stmt.cond_expr, "cond_expr");
      DEBUG_print_ast_node(str, indent_level, node->while_stmt.body, "body");
    }
    else if(node->kind == AstNodeKind_ForStmt)
    {
      DEBUG_print_ast_node(str, indent_level, node->for_stmt.decl_expr, "decl_expr");
      DEBUG_print_ast_node(str, indent_level, node->for_stmt.cond_expr, "cond_expr");
      DEBUG_print_ast_node(str, indent_level, node->for_stmt.loop_expr, "loop_expr");
      DEBUG_print_ast_node(str, indent_level, node->for_stmt.body, "body");
    }
    else if(node->kind == AstNodeKind_Cast)
    {
      DEBUG_print_ast_node(str, indent_level, node->cast.type, "type");
      DEBUG_print_ast_node(str, indent_level, node->cast.expr, "expr");
    }
    else if(node->kind == AstNodeKind_Array)
    {
      DEBUG_print_ast_node(str, indent_level, node->array.expr, "expr");
      DEBUG_print_ast_node(str, indent_level, node->array.index, "index");
    }
    else if(node->kind == AstNodeKind_Pointer)
    {
      DEBUG_print_ast_node(str, indent_level, node->ptr.expr, "expr");
    }
    else if(node->kind == AstNodeKind_Call)
    {
      DEBUG_print_ast_node(str, indent_level, node->call.id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &node->call.actual_args, "actual_args");
    }
    else if(node->kind == AstNodeKind_BreakStmt
            || node->kind == AstNodeKind_ContinueStmt
            || node->kind == AstNodeKind_EmptyStmt)
    {
      /* nothing to do */
    }
    else if(node->kind == AstNodeKind_Struct)
    {
      DEBUG_print_ast_node(str, indent_level, node->struct_decl.id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &node->struct_decl.member_list, "member_list");
    }
    else if(node->kind == AstNodeKind_Union)
    {
      DEBUG_print_ast_node(str, indent_level, node->union_decl.id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &node->union_decl.member_list, "member_list");
    }
    else if(node->kind == AstNodeKind_Enum)
    {
      DEBUG_print_ast_node(str, indent_level, node->enum_decl.id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &node->enum_decl.member_list, "member_list");
    }
    else if(node->kind == AstNodeKind_Initializer)
    {
      DEBUG_print_ast_node_list(str, indent_level, &node->initer.member_list, "member_list");
    }
    else if(node->kind == AstNodeKind_GotoStmt)
    {
      DEBUG_print_ast_node(str, indent_level, node->goto_stmt.id, "id");
    }
    else if(node->kind == AstNodeKind_Label)
    {
      DEBUG_print_ast_node(str, indent_level, node->label.id, "id");
    }
    else
    {
      assert(false);
    } 
  }
}
