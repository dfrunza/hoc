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
new_module(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Module;
  node->src_loc = *src_loc;
  list_init(&node->module.node_list);
  return node;
}

/*
AstNode*
new_block(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Block;
  //node->type = basic_type_void;
  node->src_loc = *src_loc;

  AstBlock* block = &node->block;
  list_init(&block->decl_vars);
  list_init(&block->local_occurs);
  list_init(&block->nonlocal_occurs);
  list_init(&block->stmt_list);
  list_init(&block->access_links);
  block->block_id = symbol_table->scope_id;
  block->nesting_depth = symbol_table->nesting_depth;
  return node;
}
*/
internal AstNode*
new_block(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Block;
  node->src_loc = *src_loc;
  list_init(&node->block.stmt_list);
  return node;
}

internal AstNode*
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

internal AstNode*
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

internal AstNode*
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
  list_init(&node->include_stmt.node_list);
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

internal bool
semicolon(TokenStream* input)
{
  bool success = true;
  if(input->token.kind == TokenKind_Semicolon)
    get_next_token(input);
  else
    success = compile_error(&input->src_loc, __FILE__, __LINE__,
                            "Expected `;`, actual `%s`", input->token.lexeme);
  return success;
}

internal bool
rest_of_type_id(TokenStream* input,
                AstNode* expr, AstNode** node)
{
  *node = expr;
  bool success = true;
  
  if(input->token.kind == TokenKind_Star)
  {
    *node = new_pointer(&input->src_loc);
    AstPointer* ptr = &(*node)->pointer;
    ptr->expr = expr;

    get_next_token(input);
    success = rest_of_type_id(input, *node, node);
  }
  return true;
}

internal bool
type_id(TokenStream* input,
        AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Id)
  {
    *node = new_id(&input->src_loc, input->token.lexeme);

    if(get_next_token(input)->kind == TokenKind_Star)
      success = rest_of_type_id(input, *node, node);
  }
  return success;
}

internal bool initializer(TokenStream*, AstNode**);
internal bool expression(TokenStream*, AstNode**);
internal bool statement(TokenStream*, AstNode**);
internal bool formal_arg_decl(TokenStream*, AstNode**);
internal bool unary_expr(TokenStream*, AstNode**);
internal bool module(TokenStream*, List*);
internal bool struct_member_list(TokenStream*, List*);

internal bool
initializer_member_list(TokenStream* input, List* member_list)
{
  bool success = true;

  AstNode* member = 0;
  do
  {
    member = 0;
    if(input->token.kind == TokenKind_OpenBrace)
    {
      if(success = initializer(input, &member))
      {
        if(input->token.kind == TokenKind_Comma)
          get_next_token(input);
      }
    }
    if(success)
    {
      if(!member)
        success = expression(input, &member);

      if(success && member)
      {
        list_append(arena, member_list, member);
        if(input->token.kind == TokenKind_Comma)
          get_next_token(input);
      }
    }
  }
  while(success && member);
  return success;
}

internal bool
initializer(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    get_next_token(input);
    *node = new_initializer(&input->src_loc);
    AstInitializer* initer = &(*node)->initer;

    if(success = initializer_member_list(input, &initer->member_list))
    {
      if(input->token.kind == TokenKind_CloseBrace)
        get_next_token(input);
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `}`, actual `%s`", input->token.lexeme);
    }
  }

  return success;
}

internal bool
actual_arg_list(TokenStream* input, List* arg_list)
{
  bool success = true;

  AstNode* arg = 0;
  do
  {
    arg = 0;
    success = expression(input, &arg);
    if(success && arg)
    {
      list_append(arena, arg_list, arg);
      if(input->token.kind == TokenKind_Comma)
      {
        if(get_next_token(input)->kind == TokenKind_CloseParens)
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Identifier expected, actual `%s`", input->token.lexeme);
      }
    }
  }
  while(success && arg);
  return success;
}

internal bool
formal_arg_list(TokenStream* input, List* arg_list)
{
  bool success = true;

  AstNode* arg = 0;
  do
  {
    arg = 0;
    success = formal_arg_decl(input, &arg);
    if(arg)
    {
      list_append(arena, arg_list, arg);
      if(input->token.kind == TokenKind_Comma)
        get_next_token(input);
      else if(input->token.kind != TokenKind_CloseParens)
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `,`, actual `%s`", input->token.lexeme);
    }
  }
  while(success && arg);
  return success;
}

internal bool
statement_list(TokenStream* input, List* stmt_list)
{
  bool success = true;

  while(input->token.kind == TokenKind_Semicolon)
    get_next_token(input);

  AstNode* stmt = 0;
  do
  {
    if((success = statement(input, &stmt)) && stmt)
      list_append(arena, stmt_list, stmt);
  }
  while(success && stmt);
  return success;
}

internal bool
block(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    *node = new_block(&input->src_loc);
    get_next_token(input);
    if(success = statement_list(input, &(*node)->block.stmt_list))
    {
      if(input->token.kind == TokenKind_CloseBrace)
        get_next_token(input);
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__, "Expected `}`, actual `%s`", input->token.lexeme);
    }
  }

  return success;
}

internal bool
array_index(TokenStream* input, AstNode* expr, AstNode** node)
{
  *node = expr;
  bool success = true;

  if(input->token.kind == TokenKind_OpenBracket)
  {
    *node = new_array(&input->src_loc);
    AstArray* array = &(*node)->array;
    array->expr = expr;

    get_next_token(input);
    if(success = expression(input, &array->index))
    {
      if(input->token.kind == TokenKind_CloseBracket)
      {
        get_next_token(input);
        success = array_index(input, *node, node);
      }
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__, "Expected `]`, actual `%s`", input->token.lexeme);
    }
  }
  return success;
}

internal bool
rest_of_id(TokenStream* input, AstNode* id, AstNode** node)
{
  *node = id;
  bool success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    *node = new_call(&input->src_loc);
    AstCall* call = &(*node)->call;
    call->id = id;

    get_next_token(input);
    if(success = actual_arg_list(input, &call->actual_args))
    {
      if(input->token.kind == TokenKind_CloseParens)
        get_next_token(input);
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `)`, actual `%s`", input->token.lexeme);
    }
  }
  else if(input->token.kind == TokenKind_OpenBracket)
    success = array_index(input, *node, node);

  return success;
}

internal void
postfix(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;

  if(input->token.kind == TokenKind_PlusPlus ||
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

    get_next_token(input);
  }
}

internal bool
rest_of_accessor(TokenStream* input, AstNode* left_node, AstNode** node)
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

    get_next_token(input);
    if(success = unary_expr(input, &expr->rhs))
    {
      if(expr->rhs)
        success = rest_of_accessor(input, *node, node);
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Operand expected, actual `%s`", input->token.lexeme);
    }
  }

  return success;
}

internal bool
factor(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = unary_expr(input, node)) && *node)
    success = rest_of_accessor(input, *node, node);
  return success;
}

internal bool
rest_of_factor(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == TokenKind_Star ||
     input->token.kind == TokenKind_FwdSlash ||
     input->token.kind == TokenKind_Percent ||
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

    if(input->token.kind == TokenKind_Star)
      expr->op = AstOpKind_Mul;
    else if(input->token.kind == TokenKind_FwdSlash)
      expr->op = AstOpKind_Div;
    else if(input->token.kind == TokenKind_Percent)
      expr->op = AstOpKind_Mod;
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
    else
      assert(false);

    get_next_token(input);
    if(success = factor(input, &expr->rhs))
    {
      if(expr->rhs)
        success = rest_of_factor(input, *node, node);
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Operand expected, actual `%s`", input->token.lexeme);
    }
  }

  return success;
}

internal bool
term(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = factor(input, node)) && *node)
    success = rest_of_factor(input, *node, node);
  return success;
}

internal bool
rest_of_term(TokenStream* input, AstNode* left_node, AstNode** node)
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

    get_next_token(input);
    if(success = term(input, &expr->rhs))
    {
      if(expr->rhs)
        success = rest_of_term(input, *node, node);
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Operand expected, actual `%s`", input->token.lexeme);
    }
  }

  return success;
}

internal bool
assignment(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = term(input, node)) && *node)
    success = rest_of_term(input, *node, node);
  return success;
}

internal bool
rest_of_assignment(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == TokenKind_Equals)
  {
    *node = new_bin_expr(&input->src_loc);
    AstBinExpr* expr = &(*node)->bin_expr;
    expr->lhs = left_node;
    expr->op = AstOpKind_Assign;

    get_next_token(input);
    if(success = expression(input, &expr->rhs))
    {
      if(!expr->rhs)
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Operand expected, actual `%s`", input->token.lexeme);
    }
  }

  return success;
}

internal bool
accessor(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    get_next_token(input);

    if(success = expression(input, node))
    {
      if(input->token.kind == TokenKind_CloseParens)
      {
        get_next_token(input);

        AstNode* rest = 0;
        if((success = accessor(input, &rest)) && rest)
        {
          AstNode* type = *node;
          *node = new_cast(&input->src_loc);
          AstCast* cast = &(*node)->cast;
          cast->type = type;
          cast->expr = rest;
        }
      }
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `)`, actual `%s`", input->token.lexeme);
    }
  }
  else if(input->token.kind == TokenKind_IntNum ||
          input->token.kind == TokenKind_FloatNum ||
          input->token.kind == TokenKind_Char ||
          input->token.kind == TokenKind_String ||
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

    get_next_token(input);
  }
  else if(input->token.kind == TokenKind_Id)
  {
    *node = new_id(&input->src_loc, input->token.lexeme);

    get_next_token(input);
    success = rest_of_id(input, *node, node);
#if 0
    {
      *node = new_var_occur(&input->src_loc);
      AstVarOccur* var_occur = &(*node)->var_occur;
      Symbol* symbol = lookup_symbol(symbol_table, id_name, SymbolKind_Var);
      if(symbol)
      {
        var_occur->name = symbol->name;

        AstVarDecl* var_decl = &symbol->node->var_decl;
        var_occur->data = &var_decl->data;
        var_occur->decl_block_offset = (symbol_table->nesting_depth - symbol->nesting_depth);
        var_occur->var_decl = symbol->node;

        if(var_occur->decl_block_offset > 0)
          list_append(arena, &enclosing_block->nonlocal_occurs, *node);
        else if(var_occur->decl_block_offset == 0)
          list_append(arena, &enclosing_block->local_occurs, *node);
        else
          assert(false);
      }
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__, "Unknown identifier `%s`", id_name);
    }
#endif
  }

  return success;
}

internal bool
unary_expr(TokenStream* input, AstNode** node)
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
      expr->op = AstOpKind_Pointer;
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

    get_next_token(input);
    if(success = factor(input, &expr->operand))
    {
      if(expr->operand)
        postfix(input, expr->operand, &expr->operand);
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Operand expected, actual `%s`", input->token.lexeme);
    }
  }
  else
  {
    if(success = accessor(input, node))
      postfix(input, *node, node);
  }

  return success;
}

internal bool
expression(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = assignment(input, node)) && *node)
    success = rest_of_assignment(input, *node, node);
  return success;
}

internal bool
formal_arg_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* type = 0;
  if((success = type_id(input, &type)) && type)
  {
    *node = new_var_decl(&input->src_loc);
    AstVarDecl* var_decl = &(*node)->var_decl;
    var_decl->type = type;

    if(input->token.kind == TokenKind_Id)
    {
      var_decl->id = new_id(&input->src_loc, input->token.lexeme);

      if(get_next_token(input)->kind == TokenKind_OpenBracket)
        success = array_index(input, var_decl->id, &var_decl->id);
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Identifier expected, actual `%s`", input->token.lexeme);
  }
  return success;
}

internal bool
var_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* type = 0;
  success = type_id(input, &type);
  if(!success) goto end;

  if(type)
  {
    if(input->token.kind == TokenKind_Id)
    {
      *node = new_var_decl(&input->src_loc);
      AstVarDecl* var_decl = &(*node)->var_decl;
      var_decl->type = type;
      var_decl->id = new_id(&input->src_loc, input->token.lexeme);

      if(get_next_token(input)->kind == TokenKind_OpenBracket)
      {
        success = array_index(input, var_decl->id, &var_decl->id);
        if(!success) goto end;
      }

      if(input->token.kind == TokenKind_Equals)
      {
        get_next_token(input);

        success = initializer(input, &var_decl->init_expr);
        if(!success) goto end;

        if(!var_decl->init_expr)
        {
          if(success = expression(input, &var_decl->init_expr))
          {
            if(!var_decl->init_expr)
              success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                      "Expression expected, actual `%s`", input->token.lexeme);
          }
        }
      }
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Identifier expected, actual `%s`", input->token.lexeme);
  }
end:
  return success;
}

internal bool
for_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_For)
  {
    *node = new_for_stmt(&input->src_loc);
    AstForStmt* for_stmt = &(*node)->for_stmt;

    if(get_next_token(input)->kind == TokenKind_OpenParens)
    {
      get_next_token(input);

      success = var_decl(input, &for_stmt->decl) && semicolon(input);
      if(!success) goto end;

      success = expression(input, &for_stmt->cond_expr) && semicolon(input);
      if(!success) goto end;

      success = expression(input, &for_stmt->loop_expr);
      if(!success) goto end;

      if(input->token.kind == TokenKind_CloseParens)
        get_next_token(input);
      else
      {
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `)`, actual `%s`", input->token.lexeme);
        goto end;
      }

      success = block(input, &for_stmt->body);
      if(!success) goto end;

      if(!for_stmt->body)
      {
        success = statement(input, &for_stmt->body);
        if(!success) goto end;

        if(!for_stmt->body)
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Statement(s) expected, actual `%s`", input->token.lexeme);
      }
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Expected `(`, actual `%s`", input->token.lexeme);
  }
end:
  return success;
}

internal bool
while_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_While)
  {
    *node = new_while_stmt(&input->src_loc);
    AstWhileStmt* while_stmt = &(*node)->while_stmt;

    if(get_next_token(input)->kind == TokenKind_OpenParens)
    {
      get_next_token(input);

      success = expression(input, &while_stmt->cond_expr);
      if(!success) goto end;

      if(input->token.kind == TokenKind_CloseParens)
      {
        get_next_token(input);
        if(while_stmt->cond_expr)
        {
          success = block(input, &while_stmt->body);
          if(!success) goto end;

          if(!while_stmt->body)
          {
            success = statement(input, &while_stmt->body);
            if(!success) goto end;

            if(!while_stmt->body)
              success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                      "Statement(s) expected, actual `%s`", input->token.lexeme);
          }
        }
        else
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Expression expected, actual `%s`", input->token.lexeme);
      }
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `)`, actual `%s`", input->token.lexeme);
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Expected `(`, actual `%s`", input->token.lexeme);
  }
end:
  return success;
}

internal bool
else_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Else)
  {
    get_next_token(input);

    success = block(input, node);
    if(!success) goto end;

    if(!(*node))
    {
      success = statement(input, node);
      if(!success) goto end;

      if(!(*node))
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Statement(s) expected, actual `%s`", input->token.lexeme);
    }
  }
end:
  return success;
}

internal bool
if_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_If)
  {
    *node = new_if_stmt(&input->src_loc);
    AstIfStmt* if_stmt = &(*node)->if_stmt;

    if(get_next_token(input)->kind == TokenKind_OpenParens)
    {
      get_next_token(input);

      success = expression(input, &if_stmt->cond_expr);
      if(!success) goto end;

      if(input->token.kind == TokenKind_CloseParens)
      {
        get_next_token(input);
        if(if_stmt->cond_expr)
        {
          success = block(input, &if_stmt->body);
          if(!success) goto end;

          if(!if_stmt->body)
          {
            success = statement(input, &if_stmt->body);

            if(!if_stmt->body)
            {
              success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                      "Statement(s) expected, actual `%s`", input->token.lexeme);
              goto end;
            }
          }

          assert(if_stmt->body);
          success = else_stmt(input, &if_stmt->else_body);
        }
        else
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Expression expected, actual `%s`", input->token.lexeme);
      }
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `)`, actual `%s`", input->token.lexeme);
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Expected `(`, actual `%s`", input->token.lexeme);
  }
end:
  return success;
}

internal bool
proc_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Proc)
  {
    get_next_token(input);

    *node = new_proc(&input->src_loc);
    AstProc* proc = &(*node)->proc;

    success = type_id(input, &proc->ret_type);
    if(!success) goto end;

    if(proc->ret_type)
    {
      if(input->token.kind == TokenKind_Id)
      {
        proc->id = new_id(&input->src_loc, input->token.lexeme);

        if(get_next_token(input)->kind == TokenKind_OpenParens)
        {
          get_next_token(input);

          success = formal_arg_list(input, &proc->formal_args);
          if(!success) goto end;

          if(input->token.kind == TokenKind_CloseParens)
          {
            get_next_token(input);

            success = block(input, &proc->body);
            if(!success) goto end;

            if(!proc->body)
              success = semicolon(input);
          }
          else
            success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                    "Expected `)`, actual `%s`", input->token.lexeme);
        }
        else
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Expected `(`, actual `%s`", input->token.lexeme);
      }
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Identifier expected, actual `%s`", input->token.lexeme);
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Type identifier expected, actual `%s`", input->token.lexeme);
  }
end:
  return success;
}

internal bool
include_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Include)
  {
    if(get_next_token(input)->kind == TokenKind_String)
    {
      *node = new_include_stmt(&input->src_loc);
      AstIncludeStmt* include_stmt = &(*node)->include_stmt;

      String str = {0};
      str_init(&str, arena);
      str_append(&str, input->src_loc.file_path);
      path_make_dir(str.head);
      str_tidyup(&str);
      str_append(&str, input->token.str);
      include_stmt->file_path = str.head;

      get_next_token(input);

      char* hoc_text = file_read_text(arena, include_stmt->file_path);
      if(hoc_text)
      {
        TokenStream* inc_input = mem_push_struct(arena, TokenStream, 1);
        init_token_stream(inc_input, hoc_text, include_stmt->file_path);

        get_next_token(inc_input);
        success = module(inc_input, &include_stmt->node_list);
      }
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Could not read file `%s`", include_stmt->file_path);
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "String expected, actual `%s`", input->token.lexeme);
  }
  return success;
}

internal bool
enum_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Enum)
  {
    if(get_next_token(input)->kind == TokenKind_Id)
    {
      *node = new_enum(&input->src_loc);
      AstEnum* enum_decl = &(*node)->enum_decl;
      enum_decl->id = new_id(&input->src_loc, input->token.lexeme);

      if(get_next_token(input)->kind == TokenKind_OpenBrace)
      {
        get_next_token(input);

        AstNode* member = 0;
        do
        {
          member = 0;
          if(input->token.kind == TokenKind_Id)
          {
            member = new_id(&input->src_loc, input->token.lexeme);
            list_append(arena, &enum_decl->member_list, member);

            if(get_next_token(input)->kind == TokenKind_Comma)
              get_next_token(input);
            else if(input->token.kind != TokenKind_CloseBrace)
              member = 0;
          }
        }
        while(member);

        if(input->token.kind == TokenKind_CloseBrace)
          get_next_token(input);
        else
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Expected `}`, actual `%s`", input->token.lexeme);
      }
      else
        success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                "Expected `{`, actual `%s`", input->token.lexeme);
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Identifier expected, actual `%s`", input->token.lexeme);
  }
  return success;
}

internal bool
union_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Union)
  {
    *node = new_union(&input->src_loc);
    AstUnion* union_decl = &(*node)->union_decl;

    if(get_next_token(input)->kind == TokenKind_Id)
    {
      union_decl->id = new_id(&input->src_loc, input->token.lexeme);
      get_next_token(input);
    }

    success = struct_member_list(input, &union_decl->member_list);
  }
  return success;
}

internal bool
struct_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Struct)
  {
    *node = new_struct(&input->src_loc);
    AstStruct* struct_decl = &(*node)->struct_decl;

    if(get_next_token(input)->kind == TokenKind_Id)
    {
      struct_decl->id = new_id(&input->src_loc, input->token.lexeme);
      get_next_token(input);
    }

    success = struct_member_list(input, &struct_decl->member_list);
  }
  return success;
}

internal bool
struct_member_list(TokenStream* input, List* member_list)
{
  bool success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    get_next_token(input);

    AstNode* member = 0;
    do
    {
      member = 0;
      AstNode* type = 0;

      success = type_id(input, &type);
      if(!success) goto end;

      if(!type)
      {
        if(input->token.kind == TokenKind_Union)
          success = union_decl(input, &type);
        else if(input->token.kind == TokenKind_Struct)
          success = struct_decl(input, &type);

        if(!success) goto end;
      }

      if(type)
      {
        member = new_var_decl(&input->src_loc);
        AstVarDecl* var_decl = &member->var_decl;
        var_decl->type = type;

        if(input->token.kind == TokenKind_Id)
        {
          var_decl->id = new_id(&input->src_loc, input->token.lexeme);

          if(get_next_token(input)->kind == TokenKind_OpenBracket)
          {
            success = array_index(input, member, &member);
            if(!success) goto end;
          }
        }
        else if(type->kind == AstNodeKind_Struct ||
                type->kind == AstNodeKind_Union)
        {
          /* anonymous struct/union */
        }
        else
        {
          success = compile_error(&input->src_loc, __FILE__, __LINE__,
                                  "Identifier expected, actual `%s`", input->token.lexeme);
          goto end;
        }

        list_append(arena, member_list, member);
        success = semicolon(input);
        if(!success) goto end;
      }
    }
    while(member);

    if(input->token.kind == TokenKind_CloseBrace)
      get_next_token(input);
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Expected `}`, actual `%s`", input->token.lexeme);
  }
  else
    success = compile_error(&input->src_loc, __FILE__, __LINE__,
                            "Expected `{`, actual `%s`", input->token.lexeme);
end:
  return success;
}

internal bool
var_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = false;

  get_next_token(input);
  success = var_decl(input, node);
  if(success && !node)
    success = compile_error(&input->src_loc, __FILE__, __LINE__,
                            "Type identifier expected, actual `%s`", input->token.lexeme);
  return success;
}

internal bool
module_element(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Include)
    success = include_stmt(input, node) && semicolon(input);
  else if(input->token.kind == TokenKind_Proc)
    success = proc_decl(input, node);
  else if(input->token.kind == TokenKind_Var)
    success = var_stmt(input, node) && semicolon(input);
  else if(input->token.kind == TokenKind_Struct)
    success = struct_decl(input, node);
  else if(input->token.kind == TokenKind_Union)
    success = union_decl(input, node);
  else if(input->token.kind == TokenKind_Enum)
    success = enum_decl(input, node);
  return success;
}

internal bool
module(TokenStream* input, List* node_list)
{
  bool success = true;

  AstNode* node = 0;
  do
  {
    if((success = module_element(input, &node)) && node)
      list_append(arena, node_list, node);
  }
  while(success && node);

  if(success && !node)
  {
    if(input->token.kind != TokenKind_EndOfInput)
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Unexpected token `%s`", input->token.lexeme);
  }
  return success;
}

internal bool
return_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Return)
  {
    *node = new_return_stmt(&input->src_loc);

    get_next_token(input);
    success = expression(input, &(*node)->ret_stmt.expr);
    /*
    if(success)
    {
      AstNode* owner = 0;
      int depth = block_find_owner(enclosing_block, AstNodeKind_Proc, &owner);
      if(owner)
      {
        AstProc* ret_proc = &owner->proc;
        ret_stmt->proc = ret_proc;
        ret_stmt->depth = depth;

        if(ret_expr)
        {
          AstNode* var_node = new_var_occur(&input->src_loc);
          AstVarOccur* var_occur = &var_node->var_occur;
          var_occur->data = &ret_proc->ret_var.data;
          var_occur->decl_block_offset = depth;
          var_node->type = ret_proc->ret_type;

          if(depth > 0)
          {
            list_append(arena, &enclosing_block->nonlocal_occurs, var_node);
          }
          else
          {
            list_append(arena, &enclosing_block->local_occurs, var_node);
          }

          AstNode* assgn_node = new_bin_expr(&input->src_loc);
          AstBinExpr* assgn_expr = &assgn_node->bin_expr;
          assgn_expr->op = AstOpKind_Assign;
          assgn_expr->left_operand = var_node;
          assgn_expr->right_operand = ret_expr;

          ret_stmt->assgn_expr = assgn_node;
        }
      }
      else
      {
        compile_error(&input->src_loc, "`return` : enclosing procedure not found");
        success = false;
      }
    }
    */
  }

  return success;
}

internal bool
label(TokenStream* input, AstNode* id, AstNode** node)
{
  *node = id;
  bool success = true;

  if(input->token.kind == TokenKind_Colon &&
     id->kind == AstNodeKind_Id)
  {
    *node = new_label(&input->src_loc);
    AstLabel* label = &(*node)->label;
    label->id = id;

    if(input->token.kind == TokenKind_Colon)
      get_next_token(input);
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Expected `:`, actual `%s`", input->token.lexeme);
  }
  return success;
}

internal bool
goto_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Goto)
  {
    *node = new_goto_stmt(&input->src_loc);
    AstGotoStmt* goto_stmt = &(*node)->goto_stmt;

    get_next_token(input);
    if(input->token.kind == TokenKind_Id)
    {
      goto_stmt->id = new_id(&input->src_loc, input->token.lexeme);
      get_next_token(input);
    }
    else
      success = compile_error(&input->src_loc, __FILE__, __LINE__,
                              "Identifier expected, actual `%s`", input->token.lexeme);
  }
  return success;
}

internal bool
continue_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Continue)
  {
    *node = new_continue_stmt(&input->src_loc);
    get_next_token(input);
  }
  return success;
}

internal bool
break_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Break)
  {
    *node = new_break_stmt(&input->src_loc);
    get_next_token(input);
    /*
    AstBreakStmt* break_stmt = &(*node)->break_stmt;

    AstNode* owner = 0;
    int depth = block_find_owner(enclosing_block, AstNodeKind_WhileStmt, &owner);
    if(owner)
    {
      break_stmt->while_stmt = &owner->while_stmt;
      break_stmt->depth = depth + 1;
    }
    else {
      compile_error(&input->src_loc, "`break`: enclosing `while` statement not found");
      success = false;
    }
    */
  }
  return success;
}

internal bool
statement(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_If)
    success = if_stmt(input, node);
  else if(input->token.kind == TokenKind_While)
    success = while_stmt(input, node);
  else if(input->token.kind == TokenKind_For)
    success = for_stmt(input, node);
  else if(input->token.kind == TokenKind_Return)
    success = return_stmt(input, node) && semicolon(input);
  else if(input->token.kind == TokenKind_Break)
    success = break_stmt(input, node) && semicolon(input);
  else if(input->token.kind == TokenKind_Continue)
    success = continue_stmt(input, node) && semicolon(input);
  else if(input->token.kind == TokenKind_Goto)
    success = goto_stmt(input, node) && semicolon(input);
  else if(input->token.kind == TokenKind_Var)
    success = var_stmt(input, node) && semicolon(input);
  else if(input->token.kind == TokenKind_Semicolon)
  {
    get_next_token(input);
    *node = new_empty_stmt(&input->src_loc);
  }
  else if(input->token.kind == TokenKind_OpenBrace)
    success = block(input, node);
  else
  {
    if(success = expression(input, node))
    {
      if(*node)
      {
        if((*node)->kind == AstNodeKind_Id)
          label(input, *node, node);
        else
          success = semicolon(input);
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
  success = module(input, &(*node)->module.node_list);
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
    DEBUG_print_line(str, indent_level, DEBUG_AstNodeKind_tags[node->kind]);
    ++indent_level;

    if(node->kind == AstNodeKind_Module)
    {
      AstModule* module = &node->module;
      DEBUG_print_ast_node_list(str, indent_level, &module->node_list, "node_list");
    }
    else if(node->kind == AstNodeKind_IncludeStmt)
    {
      AstIncludeStmt* inc = &node->include_stmt;
      DEBUG_print_line(str, indent_level, "file_path: \"%s\"", inc->file_path);
      DEBUG_print_ast_node_list(str, indent_level, &inc->node_list, "node_list");
    }
    else if(node->kind == AstNodeKind_Proc)
    {
      AstProc* proc = &node->proc;
      DEBUG_print_ast_node(str, indent_level, proc->ret_type, "ret_type");
      DEBUG_print_ast_node(str, indent_level, proc->id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &proc->formal_args, "formal_args");
      DEBUG_print_ast_node(str, indent_level, proc->body, "body");
    }
    else if(node->kind == AstNodeKind_VarDecl)
    {
      AstVarDecl* var_decl = &node->var_decl;
      DEBUG_print_ast_node(str, indent_level, var_decl->type, "type");
      DEBUG_print_ast_node(str, indent_level, var_decl->id, "id");
      DEBUG_print_ast_node(str, indent_level, var_decl->init_expr, "init_expr");
    }
    else if(node->kind == AstNodeKind_Id)
    {
      AstId* id = &node->id;
      DEBUG_print_line(str, indent_level, "name: %s", id->name);
    }
    else if(node->kind == AstNodeKind_Block)
    {
      DEBUG_print_ast_node_list(str, indent_level, &node->block.stmt_list, "stmt_list");
    }
    else if(node->kind == AstNodeKind_BinExpr)
    {
      AstBinExpr* bin_expr = &node->bin_expr;
      DEBUG_print_line(str, indent_level, "op: %s", DEBUG_AstOpKind_tags[bin_expr->op]);
      DEBUG_print_ast_node(str, indent_level, bin_expr->lhs, "lhs");
      DEBUG_print_ast_node(str, indent_level, bin_expr->rhs, "rhs");
    }
    else if(node->kind == AstNodeKind_UnrExpr)
    {
      AstUnrExpr* unr_expr = &node->unr_expr;
      DEBUG_print_line(str, indent_level, "op: %s", DEBUG_AstOpKind_tags[unr_expr->op]);
      DEBUG_print_ast_node(str, indent_level, unr_expr->operand, "operand");
    }
    else if(node->kind == AstNodeKind_IfStmt)
    {
      AstIfStmt* if_stmt = &node->if_stmt;
      DEBUG_print_ast_node(str, indent_level, if_stmt->cond_expr, "cond_expr");
      DEBUG_print_ast_node(str, indent_level, if_stmt->body, "body");
      DEBUG_print_ast_node(str, indent_level, if_stmt->else_body, "else_body");
    }
    else if(node->kind == AstNodeKind_ReturnStmt)
    {
      AstReturnStmt* ret_stmt = &node->ret_stmt;
      DEBUG_print_ast_node(str, indent_level, ret_stmt->expr, "expr");
    }
    else if(node->kind == AstNodeKind_Literal)
    {
      AstLiteral* lit = &node->literal;
      DEBUG_print_line(str, indent_level, DEBUG_AstLiteralKind_tags[lit->kind]);
      if(lit->kind == AstLiteralKind_Int)
        DEBUG_print_line(str, indent_level, "int_val: %d", lit->int_val);
      else if(lit->kind == AstLiteralKind_Float)
        DEBUG_print_line(str, indent_level, "float_val: %f", lit->float_val);
      else if(lit->kind == AstLiteralKind_Bool)
        DEBUG_print_line(str, indent_level, "bool_val: %d", lit->bool_val);
      else if(lit->kind == AstLiteralKind_Char)
      {
        char buf[3] = {0};

        if(lit->char_val == '\0')
          cstr_copy(buf, "\\0");
        else if(lit->char_val == '\t')
          cstr_copy(buf, "\\t");
        else if(lit->char_val == '\n')
          cstr_copy(buf, "\\n");
        else if(lit->char_val == '\r')
          cstr_copy(buf, "\\r");
        else if(lit->char_val == '\'')
          cstr_copy(buf, "\\'");
        else
          *buf = lit->char_val;

        DEBUG_print_line(str, indent_level, "char_val: '%s'", buf);
      }
      else if(lit->kind == AstLiteralKind_String)
        DEBUG_print_line(str, indent_level, "str: \"%s\"", lit->str);
      else
        assert(false);
    }
    else if(node->kind == AstNodeKind_WhileStmt)
    {
      AstWhileStmt* while_stmt = &node->while_stmt;
      DEBUG_print_ast_node(str, indent_level, while_stmt->cond_expr, "cond_expr");
      DEBUG_print_ast_node(str, indent_level, while_stmt->body, "body");
    }
    else if(node->kind == AstNodeKind_ForStmt)
    {
      AstForStmt* for_stmt = &node->for_stmt;
      DEBUG_print_ast_node(str, indent_level, for_stmt->decl, "decl");
      DEBUG_print_ast_node(str, indent_level, for_stmt->cond_expr, "cond_expr");
      DEBUG_print_ast_node(str, indent_level, for_stmt->loop_expr, "loop_expr");
      DEBUG_print_ast_node(str, indent_level, for_stmt->body, "body");
    }
    else if(node->kind == AstNodeKind_Cast)
    {
      AstCast* cast = &node->cast;
      DEBUG_print_ast_node(str, indent_level, cast->type, "type");
      DEBUG_print_ast_node(str, indent_level, cast->expr, "expr");
    }
    else if(node->kind == AstNodeKind_Array)
    {
      AstArray* array = &node->array;
      DEBUG_print_ast_node(str, indent_level, array->expr, "expr");
      DEBUG_print_ast_node(str, indent_level, array->index, "index");
    }
    else if(node->kind == AstNodeKind_Pointer)
    {
      AstPointer* ptr = &node->pointer;
      DEBUG_print_ast_node(str, indent_level, ptr->expr, "expr");
    }
    else if(node->kind == AstNodeKind_Call)
    {
      AstCall* call = &node->call;
      DEBUG_print_ast_node(str, indent_level, call->id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &call->actual_args, "actual_args");
    }
    else if(node->kind == AstNodeKind_EmptyStmt ||
            node->kind == AstNodeKind_BreakStmt ||
            node->kind == AstNodeKind_ContinueStmt)
    {
      /* nothing to do */
    }
    else if(node->kind == AstNodeKind_Struct)
    {
      AstStruct* struct_decl = &node->struct_decl;
      DEBUG_print_ast_node(str, indent_level, struct_decl->id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &struct_decl->member_list, "member_list");
    }
    else if(node->kind == AstNodeKind_Union)
    {
      AstUnion* union_decl = &node->union_decl;
      DEBUG_print_ast_node(str, indent_level, union_decl->id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &union_decl->member_list, "member_list");
    }
    else if(node->kind == AstNodeKind_Enum)
    {
      AstEnum* enum_decl = &node->enum_decl;
      DEBUG_print_ast_node(str, indent_level, enum_decl->id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &enum_decl->member_list, "member_list");
    }
    else if(node->kind == AstNodeKind_Initializer)
    {
      AstInitializer* initer = &node->initer;
      DEBUG_print_ast_node_list(str, indent_level, &initer->member_list, "member_list");
    }
    else if(node->kind == AstNodeKind_GotoStmt)
    {
      AstGotoStmt* goto_stmt = &node->goto_stmt;
      DEBUG_print_ast_node(str, indent_level, goto_stmt->id, "id");
    }
    else if(node->kind == AstNodeKind_Label)
    {
      AstLabel* label = &node->label;
      DEBUG_print_ast_node(str, indent_level, label->id, "id");
    }
    else
    {
      assert(false);
    } 
  }
}
