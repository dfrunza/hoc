#include "syntax.h"

internal bool32
is_logical_operator(AstOpKind op)
{
  return op >= AstOpKind_LogicEquals && op <= AstOpKind_LogicNot;
}

internal AstNode*
new_node(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_module(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Module;
  node->src_loc = *src_loc;
  list_init(&node->module.node_list);
  return node;
}

/*
AstNode*
new_block(MemoryArena* arena, SourceLocation* src_loc)
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
new_block(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Block;
  node->src_loc = *src_loc;
  list_init(&node->block.stmt_list);
  return node;
}

internal AstNode*
new_id(MemoryArena* arena, SourceLocation* src_loc, char* name)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Id;
  node->src_loc = *src_loc;
  node->id.name = name;
  return node;
}

internal AstNode*
new_enum(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Enum;
  list_init(&node->enum_.member_list);
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_pointer(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Pointer;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_call(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Call;
  list_init(&node->call.actual_args);
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_array(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Array;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_proc(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Proc;
  list_init(&node->proc.formal_args);
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_bin_expr(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_BinExpr;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_unr_expr(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_UnrExpr;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_int_literal(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_Int;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_float_literal(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_Float;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_bool_literal(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_Bool;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_string_literal(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_String;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_var_decl(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_VarDecl;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_while_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_WhileStmt;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_for_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_ForStmt;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_if_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_IfStmt;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_return_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_ReturnStmt;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_break_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_BreakStmt;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_include_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_IncludeStmt;
  node->src_loc = *src_loc;
  list_init(&node->include.node_list);
  return node;
}

internal AstNode*
new_empty_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_EmptyStmt;
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_struct(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Struct;
  list_init(&node->struct_.member_list);
  node->src_loc = *src_loc;
  return node;
}

internal AstNode*
new_cast(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Cast;
  node->src_loc = *src_loc;
  return node;
}

internal bool32
semicolon(MemoryArena* arena, TokenStream* input)
{
  bool32 success = true;
  if(input->token.kind == TokenKind_Semicolon)
  {
    get_next_token(arena, input);
  }
  else
  {
    compile_error(&input->src_loc, "(%d) Missing `;`", __LINE__);
    success = false;
  }
  return success;
}

internal bool32
rest_of_type_id(MemoryArena* arena, TokenStream* input,
                AstNode* expr, AstNode** node)
{
  *node = expr;
  bool32 success = true;
  
  if(input->token.kind == TokenKind_Star ||
     input->token.kind == TokenKind_Pointer)
  {
    *node = new_pointer(arena, &input->src_loc);
    AstPointer* ptr = &(*node)->pointer;
    ptr->expr = expr;

    get_next_token(arena, input);
    success = rest_of_type_id(arena, input, *node, node);
  }
  return true;
}

internal bool32
type_id(MemoryArena* arena, TokenStream* input,
        AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Id)
  {
    *node = new_id(arena, &input->src_loc, input->token.lexeme);

    get_next_token(arena, input);
    if(input->token.kind == TokenKind_Star)
    {
      success = rest_of_type_id(arena, input, *node, node);
    }
  }
  return success;
}

internal bool32
actual_argument_list(MemoryArena* arena, TokenStream* input,
                     List* arg_list)
{
  bool32 success = true;
  
  AstNode* arg_node = 0;
  success = expression(arena, input, &arg_node) && arg_node;
  if(success)
  {
    list_append(arena, arg_list, arg_node);

    if(input->token.kind == TokenKind_Comma)
    {
      get_next_token(arena, input);
      success = actual_argument_list(arena, input, arg_list);
    }
  }
  return success;
}

internal bool32
statement_list(MemoryArena* arena, TokenStream* input,
               List* stmt_list)
{
  bool32 success = true;

  while(input->token.kind == TokenKind_Semicolon)
    get_next_token(arena, input);

  AstNode* stmt_node = 0;
  do
  {
    if((success = statement(arena, input, &stmt_node)) && stmt_node)
    {
      list_append(arena, stmt_list, stmt_node);
    }
  }
  while(success && stmt_node);
  return success;
}

internal bool32
block(MemoryArena* arena, TokenStream* input,
      AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    *node = new_block(arena, &input->src_loc);

    get_next_token(arena, input);
    if(success = statement_list(arena, input, &(*node)->block.stmt_list))
    {
      if(input->token.kind == TokenKind_CloseBrace)
      {
        get_next_token(arena, input);
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Missing `}`", __LINE__);
        success = false;
      }
    }
  }

  return success;
}

internal bool32
array_index(MemoryArena* arena, TokenStream* input,
            AstNode* expr, AstNode** node)
{
  *node = expr;
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenBracket)
  {
    *node = new_array(arena, &input->src_loc);
    AstArray* array = &(*node)->array;
    array->expr = expr;

    get_next_token(arena, input);
    if(success = expression(arena, input, &array->index))
    {
      if(input->token.kind == TokenKind_CloseBracket)
      {
        get_next_token(arena, input);

        success = array_index(arena, input, *node, node);
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Missing `]`", __LINE__);
        success = false;
      }
    }
  }
  return success;
}

internal bool32
rest_of_id(MemoryArena* arena, TokenStream* input,
           AstNode* id, AstNode** node)
{
  *node = id;
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    *node = new_call(arena, &input->src_loc);
    AstCall* call = &(*node)->call;
    call->id = id;

    get_next_token(arena, input);
    if(success = actual_argument_list(arena, input, &call->actual_args))
    {
      if(input->token.kind == TokenKind_CloseParens)
      {
        get_next_token(arena, input);
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Missing `)`", __LINE__);
        success = false;
      }
    }
  }
  else if(input->token.kind == TokenKind_OpenBracket)
  {
    success = array_index(arena, input, *node, node);
  }

  return success;
}

internal bool32
accessor(MemoryArena* arena, TokenStream* input,
         AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    get_next_token(arena, input);

    if(success = expression(arena, input, node))
    {
      if(input->token.kind == TokenKind_CloseParens)
      {
        get_next_token(arena, input);
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Missing `)`", __LINE__);
        success = false;
      }
    }
  }
  else if(input->token.kind == TokenKind_IntNum ||
          input->token.kind == TokenKind_FloatNum ||
          input->token.kind == TokenKind_Char ||
          input->token.kind == TokenKind_String ||
          input->token.kind == TokenKind_True ||
          input->token.kind == TokenKind_False)
  {
    *node = new_int_literal(arena, &input->src_loc);
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

    get_next_token(arena, input);
  }
  else if(input->token.kind == TokenKind_Id)
  {
    *node = new_id(arena, &input->src_loc, input->token.lexeme);

    get_next_token(arena, input);
    success = rest_of_id(arena, input, *node, node);
#if 0
    {
      *node = new_var_occur(arena, &input->src_loc);
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
        {
          list_append(arena, &enclosing_block->nonlocal_occurs, *node);
        }
        else if(var_occur->decl_block_offset == 0)
        {
          list_append(arena, &enclosing_block->local_occurs, *node);
        }
        else
          assert(false);
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Unknown identifier: %s", __LINE__, id_name);
        success = false;
      }
    }
#endif
  }

  return success;
}

internal void
postfix(MemoryArena* arena, TokenStream* input,
        AstNode* left_node, AstNode** node)
{
  *node = left_node;

  if(input->token.kind == TokenKind_PlusPlus ||
     input->token.kind == TokenKind_MinusMinus)
  {
    *node = new_unr_expr(arena, &input->src_loc);
    AstUnrExpr* expr = &(*node)->unr_expr;
    expr->operand = left_node;

    if(input->token.kind == TokenKind_MinusMinus)
    {
      expr->op = AstOpKind_PostDecrement;
    }
    else if(input->token.kind == TokenKind_PlusPlus)
    {
      expr->op = AstOpKind_PostIncrement;
    }
    else
      assert(false);

    get_next_token(arena, input);
  }
}

internal bool32
unary_expr(MemoryArena* arena, TokenStream* input,
           AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Exclam ||
     input->token.kind == TokenKind_Pointer ||
     input->token.kind == TokenKind_AddressOf ||
     input->token.kind == TokenKind_NegativeSign ||
     input->token.kind == TokenKind_MinusMinus ||
     input->token.kind == TokenKind_PlusPlus)
  {
    *node = new_unr_expr(arena, &input->src_loc);
    AstUnrExpr* expr = &(*node)->unr_expr;

    if(input->token.kind == TokenKind_Exclam)
    {
      expr->op = AstOpKind_LogicNot;
    }
    else if(input->token.kind == TokenKind_Pointer)
    {
      expr->op = AstOpKind_Deref;
    }
    else if(input->token.kind == TokenKind_AddressOf)
    {
      expr->op = AstOpKind_AddressOf;
    }
    else if(input->token.kind == TokenKind_NegativeSign)
    {
      expr->op = AstOpKind_Neg;
    }
    else if(input->token.kind == TokenKind_MinusMinus)
    {
      expr->op = AstOpKind_PreDecrement;
    }
    else if(input->token.kind == TokenKind_PlusPlus)
    {
      expr->op = AstOpKind_PreIncrement;
    }
    else
      assert(false);

    get_next_token(arena, input);
    if(success = factor(arena, input, &expr->operand))
    {
      if(expr->operand)
      {
        postfix(arena, input, expr->operand, &expr->operand);
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Operand expected", __LINE__);
        success = false;
      }
    }
  }
  else if(input->token.kind == TokenKind_Cast)
  {
    *node = new_cast(arena, &input->src_loc);
    AstCast* cast = &(*node)->cast;

    get_next_token(arena, input);
    success = type_id(arena, input, &cast->type);
    if(!success) goto end;

    if(cast->type)
    {
      if(input->token.kind == TokenKind_AngleRight)
      {
        get_next_token(arena, input);

        if(success = factor(arena, input, &cast->expr))
        {
          if(cast->expr)
          {
            postfix(arena, input, cast->expr, &cast->expr);
          }
          else
          {
            compile_error(&input->src_loc, "(%d) Operand expected", __LINE__);
            success = false;
          }
        }
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Missing `>`", __LINE__);
        success = false;
      }
    }
    else
    {
      compile_error(&input->src_loc, "(%d) Expected type identifier", __LINE__);
      success = false;
    }
  }
  else
  {
    if(success = accessor(arena, input, node))
    {
      postfix(arena, input, *node, node);
    }
  }
end:
  return success;
}

internal bool32
rest_of_accessor(MemoryArena* arena, TokenStream* input,
                 AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool32 success = true;

  if(input->token.kind == TokenKind_Dot)
  {
    *node = new_bin_expr(arena, &input->src_loc);
    AstBinExpr* expr = &(*node)->bin_expr;
    expr->lhs = left_node;
    expr->op = AstOpKind_MemberAccess;

    get_next_token(arena, input);
    if(success = unary_expr(arena, input, &expr->rhs))
    {
      if(expr->rhs)
      {
        success = rest_of_accessor(arena, input, *node, node);
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Operand expected", __LINE__);
        success = false;
      }
    }
  }

  return success;
}

internal bool32
factor(MemoryArena* arena, TokenStream* input,
       AstNode** node)
{
  bool32 success = true;

  if((success = unary_expr(arena, input, node)) && *node)
  {
    success = rest_of_accessor(arena, input, *node, node);
  }
  return success;
}

internal bool32
rest_of_factor(MemoryArena* arena, TokenStream* input,
               AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool32 success = true;

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
    *node = new_bin_expr(arena, &input->src_loc);
    AstBinExpr* expr = &(*node)->bin_expr;
    expr->lhs = left_node;

    if(input->token.kind == TokenKind_Star)
    {
      expr->op = AstOpKind_Mul;
    }
    else if(input->token.kind == TokenKind_FwdSlash)
    {
      expr->op = AstOpKind_Div;
    }
    else if(input->token.kind == TokenKind_Percent)
    {
      expr->op = AstOpKind_Mod;
    }
    else if(input->token.kind == TokenKind_EqualsEquals)
    {
      expr->op = AstOpKind_LogicEquals;
    }
    else if(input->token.kind == TokenKind_ExclamEquals)
    {
      expr->op = AstOpKind_LogicNotEquals;
    }
    else if(input->token.kind == TokenKind_AngleLeft)
    {
      expr->op = AstOpKind_LogicLess;
    }
    else if(input->token.kind == TokenKind_AngleLeftEquals)
    {
      expr->op = AstOpKind_LogicLessEquals;
    }
    else if(input->token.kind == TokenKind_AngleRight)
    {
      expr->op = AstOpKind_LogicGreater;
    }
    else if(input->token.kind == TokenKind_AngleRightEquals)
    {
      expr->op = AstOpKind_LogicGreaterEquals;
    }
    else
      assert(false);

    get_next_token(arena, input);
    if(success = factor(arena, input, &expr->rhs))
    {
      if(expr->rhs)
      {
        success = rest_of_factor(arena, input, *node, node);
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Operand expected", __LINE__);
        success = false;
      }
    }
  }

  return success;
}

internal bool32
term(MemoryArena* arena, TokenStream* input,
     AstNode** node)
{
  bool32 success = true;

  if((success = factor(arena, input, node)) && *node)
  {
    success = rest_of_factor(arena, input, *node, node);
  }
  return success;
}

internal bool32
rest_of_term(MemoryArena* arena, TokenStream* input,
             AstNode* left_node, AstNode** node)
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
    *node = new_bin_expr(arena, &input->src_loc);
    AstBinExpr* expr = &(*node)->bin_expr;
    expr->lhs = left_node;

    if(input->token.kind == TokenKind_Plus)
    {
      expr->op = AstOpKind_Add;
    }
    else if(input->token.kind == TokenKind_Minus)
    {
      expr->op = AstOpKind_Sub;
    }
    else if(input->token.kind == TokenKind_Pipe)
    {
      expr->op = AstOpKind_BitwiseOr;
    }
    else if(input->token.kind == TokenKind_PipePipe)
    {
      expr->op = AstOpKind_LogicOr;
    }
    else if(input->token.kind == TokenKind_Ampersand)
    {
      expr->op = AstOpKind_BitwiseAnd;
    }
    else if(input->token.kind == TokenKind_AmpersandAmpersand)
    {
      expr->op = AstOpKind_LogicAnd;
    }
    else
      assert(false);

    get_next_token(arena, input);

    if(success = term(arena, input, &expr->rhs))
    {
      if(expr->rhs)
      {
        success = rest_of_term(arena, input, *node, node);
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Operand expected", __LINE__);
        success = false;
      }
    }
  }

  return success;
}

internal bool32
assignment(MemoryArena* arena, TokenStream* input,
           AstNode** node)
{
  bool32 success = true;

  if((success = term(arena, input, node)) && *node)
  {
    success = rest_of_term(arena, input, *node, node);
  }
  return success;
}

internal bool32
rest_of_assignment(MemoryArena* arena, TokenStream* input,
                   AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool32 success = true;

  if(input->token.kind == TokenKind_Equals)
  {
    *node = new_bin_expr(arena, &input->src_loc);
    AstBinExpr* expr = &(*node)->bin_expr;
    expr->lhs = left_node;
    expr->op = AstOpKind_Assign;

    get_next_token(arena, input);
    if(success = expression(arena, input, &expr->rhs))
    {
      if(!expr->rhs)
      {
        compile_error(&input->src_loc, "(%d) Missing right side of assignment", __LINE__);
        success = false;
      }
    }
  }

  return success;
}

internal bool32
expression(MemoryArena* arena, TokenStream* input,
           AstNode** node)
{
  bool32 success = true;

  if((success = assignment(arena, input, node)) && *node)
  {
    success = rest_of_assignment(arena, input, *node, node);
  }
  return success;
}

internal bool32
formal_arg_decl(MemoryArena* arena, TokenStream* input,
                AstNode** node)
{
  *node = 0;
  bool32 success = true;

  AstNode* type = 0;
  if((success = type_id(arena, input, &type)) && type)
  {
    *node = new_var_decl(arena, &input->src_loc);
    AstVarDecl* var_decl = &(*node)->var_decl;
    var_decl->type = type;

    if(input->token.kind == TokenKind_Id)
    {
      var_decl->id = new_id(arena, &input->src_loc, input->token.lexeme);

      get_next_token(arena, input);
      if(input->token.kind == TokenKind_OpenBracket)
      {
        success = array_index(arena, input, var_decl->id, &var_decl->id);
      }
    }
    else
    {
      compile_error(&input->src_loc, "(%d) Identifier expected", __LINE__);
      success = false;
    }
  }
  return success;
}

internal bool32
var_decl(MemoryArena* arena, TokenStream* input,
         AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Var)
  {
    get_next_token(arena, input);

    *node = new_var_decl(arena, &input->src_loc);
    AstVarDecl* var_decl = &(*node)->var_decl;

    success = type_id(arena, input, &var_decl->type);
    if(!success) goto end;

    if(var_decl->type)
    {
      if(input->token.kind == TokenKind_Id)
      {
        var_decl->id = new_id(arena, &input->src_loc, input->token.lexeme);

        get_next_token(arena, input);
        if(input->token.kind == TokenKind_OpenBracket)
        {
          success = array_index(arena, input, var_decl->id, &var_decl->id);
          if(!success) goto end;
        }

        if(input->token.kind == TokenKind_Equals)
        {
          get_next_token(arena, input);

          if(success = expression(arena, input, &var_decl->init_expr))
          {
            if(!var_decl->init_expr)
            {
              compile_error(&input->src_loc, "(%d) Expression expected", __LINE__);
              success = false;
            }
          }
        }
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Identifier expected", __LINE__);
        success = false;
      }
    }
    else
    {
      compile_error(&input->src_loc, "(%d) Type identifier expected", __LINE__);
      success = false;
    }
  }
end:
  return success;
}

internal bool32
for_stmt(MemoryArena* arena, TokenStream* input,
         AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_For)
  {
    *node = new_for_stmt(arena, &input->src_loc);
    AstForStmt* for_stmt = &(*node)->for_stmt;

    get_next_token(arena, input);
    if(input->token.kind == TokenKind_OpenParens)
    {
      get_next_token(arena, input);

      {
        AstNode* node = new_var_decl(arena, &input->src_loc);
        AstVarDecl* var_decl = &node->var_decl;

        success = type_id(arena, input, &var_decl->type);
        if(!success) goto end;

        if(var_decl->type)
        {
          if(input->token.kind == TokenKind_Id)
          {
            var_decl->id = new_id(arena, &input->src_loc, input->token.lexeme);

            get_next_token(arena, input);
            if(input->token.kind == TokenKind_OpenBracket)
            {
              success = array_index(arena, input, var_decl->id, &var_decl->id);
              if(!success) goto end;
            }

            if(input->token.kind == TokenKind_Equals)
            {
              get_next_token(arena, input);

              success = expression(arena, input, &var_decl->init_expr) && semicolon(arena, input);
              if(!success) goto end;

              if(!var_decl->init_expr)
              {
                compile_error(&input->src_loc, "(%d) Expression expected", __LINE__);
                success = false; goto end;
              }
            }
            for_stmt->decl = node;
          }
        }
      }

      success = expression(arena, input, &for_stmt->cond_expr) && semicolon(arena, input);
      if(!success) goto end;

      success = expression(arena, input, &for_stmt->loop_expr);
      if(!success) goto end;
      if(input->token.kind == TokenKind_CloseParens)
      {
        get_next_token(arena, input);
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Expected `)`", __LINE__);
        success = false; goto end;
      }

      success = block(arena, input, &for_stmt->body);
      if(!success) goto end;

      if(!for_stmt->body)
      {
        success = statement(arena, input, &for_stmt->body);
        if(!success) goto end;

        if(!for_stmt->body)
        {
          compile_error(&input->src_loc, "(%d) Statement(s) expected", __LINE__);
          success = false;
        }
      }
    }
    else
    {
      compile_error(&input->src_loc, "(%d) Missing `(`", __LINE__);
      success = false;
    }
  }
end:
  return success;
}

internal bool32
while_stmt(MemoryArena* arena, TokenStream* input,
           AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_While)
  {
    *node = new_while_stmt(arena, &input->src_loc);
    AstWhileStmt* while_stmt = &(*node)->while_stmt;

    get_next_token(arena, input);
    if(input->token.kind == TokenKind_OpenParens)
    {
      get_next_token(arena, input);

      success = expression(arena, input, &while_stmt->cond_expr);
      if(!success) goto end;

      if(input->token.kind == TokenKind_CloseParens)
      {
        get_next_token(arena, input);

        if(while_stmt->cond_expr)
        {
          success = block(arena, input, &while_stmt->body);
          if(!success) goto end;

          if(!while_stmt->body)
          {
            success = statement(arena, input, &while_stmt->body);
            if(!success) goto end;

            if(!while_stmt->body)
            {
              compile_error(&input->src_loc, "(%d) Statement(s) expected", __LINE__);
              success = false;
            }
          }
        }
        else
        {
          compile_error(&input->src_loc, "(%d) Expression expected", __LINE__);
          success = false;
        }
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Missing `)`", __LINE__);
        success = false;
      }
    }
    else
    {
      compile_error(&input->src_loc, "(%d) Missing `(`", __LINE__);
      success = false;
    }
  }
end:
  return success;
}

internal bool32
else_stmt(MemoryArena* arena, TokenStream* input,
          AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Else)
  {
    get_next_token(arena, input);

    success = block(arena, input, node);
    if(!success) goto end;

    if(!(*node))
    {
      success = statement(arena, input, node);
      if(!success) goto end;

      if(!(*node))
      {
        compile_error(&input->src_loc, "(%d) Statement(s) expected", __LINE__);
        success = false;
      }
    }
  }
end:
  return success;
}

internal bool32
if_stmt(MemoryArena* arena, TokenStream* input,
        AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_If)
  {
    *node = new_if_stmt(arena, &input->src_loc);
    AstIfStmt* if_stmt = &(*node)->if_stmt;

    get_next_token(arena, input);
    if(input->token.kind == TokenKind_OpenParens)
    {
      get_next_token(arena, input);

      success = expression(arena, input, &if_stmt->cond_expr);
      if(!success) goto end;

      if(input->token.kind == TokenKind_CloseParens)
      {
        get_next_token(arena, input);

        if(if_stmt->cond_expr)
        {
          success = block(arena, input, &if_stmt->body);
          if(!success) goto end;

          if(!if_stmt->body)
          {
            success = statement(arena, input, &if_stmt->body);

            if(!if_stmt->body)
            {
              compile_error(&input->src_loc, "(%d) Statement(s) expected", __LINE__);
              success = false; goto end;
            }
          }

          assert(if_stmt->body);
          success = else_stmt(arena, input, &if_stmt->else_body);
        }
        else
        {
          compile_error(&input->src_loc, "(%d) Expression expected", __LINE__);
          success = false;
        }
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Missing `)`", __LINE__);
        success = false;
      }
    }
    else
    {
      compile_error(&input->src_loc, "(%d) Missing `(`", __LINE__);
      success = false;
    }
  }
end:
  return success;
}

internal bool32
proc_decl(MemoryArena* arena, TokenStream* input,
          AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Proc)
  {
    get_next_token(arena, input);

    *node = new_proc(arena, &input->src_loc);
    AstProc* proc = &(*node)->proc;

    success = type_id(arena, input, &proc->ret_type);
    if(!success) goto end;

    if(proc->ret_type)
    {
      if(input->token.kind == TokenKind_Id)
      {
        proc->id = new_id(arena, &input->src_loc, input->token.lexeme);

        get_next_token(arena, input);
        if(input->token.kind == TokenKind_OpenParens)
        {
          get_next_token(arena, input);

          AstNode* arg = 0;
          do
          {
            success = formal_arg_decl(arena, input, &arg);
            if(!success) goto end;

            if(arg)
            {
              list_append(arena, &proc->formal_args, arg);
              if(input->token.kind == TokenKind_Comma)
              {
                get_next_token(arena, input);
              }
              else if(input->token.kind != TokenKind_CloseParens)
              {
                compile_error(&input->src_loc, "(%d) Missing `,`", __LINE__);
                success = false; goto end;
              }
            }
          }
          while(arg);

          if(input->token.kind == TokenKind_CloseParens)
          {
            get_next_token(arena, input);

            success = block(arena, input, &proc->body);
            if(!success) goto end;

            if(!proc->body)
            {
              success = semicolon(arena, input);
            }
          }
          else
          {
            compile_error(&input->src_loc, "(%d) Missing `)`", __LINE__);
            success = false;
          }
        }
        else
        {
          compile_error(&input->src_loc, "(%d) Expected `(`", __LINE__);
          success = false;
        }
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Expected identifier", __LINE__);
        success = false;
      }
    }
    else
    {
      compile_error(&input->src_loc, "(%d) Expected type identifier", __LINE__);
      success = false;
    }
  }
end:
  return success;
}

internal bool32
include_stmt(MemoryArena* arena, TokenStream* input,
             AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Include)
  {
    get_next_token(arena, input);

    if(input->token.kind == TokenKind_String)
    {
      *node = new_include_stmt(arena, &input->src_loc);
      AstIncludeStmt* include = &(*node)->include;

      String str = {0};
      str_init(&str, arena);
      str_append(&str, input->src_loc.file_path);
      path_make_dir(str.head);
      str_tidyup(&str);
      str_append(&str, input->token.str);
      include->file_path = str.head;

      get_next_token(arena, input);

      char* hoc_text = file_read_text(arena, include->file_path);
      if(hoc_text)
      {
        TokenStream* inc_input = mem_push_struct(arena, TokenStream, 1);
        token_stream_init(inc_input, hoc_text, include->file_path);

        get_next_token(arena, inc_input);
        if(success = module(arena, inc_input, &include->node_list))
        {
          if(inc_input->token.kind != TokenKind_EndOfInput)
          {
            compile_error(&input->src_loc, "(%d) Unexpected token: %s", __LINE__, input->token.lexeme);
            success = false;
          }
        }
      }
      else
      {
        compile_error(&input->src_loc, "(%d) File could not be read: %s", __LINE__, include->file_path);
        success = false;
      }
    }
    else
    {
      compile_error(&input->src_loc, "(%d) String expected after `include`", __LINE__);
      success = false;
    }
  }
  return success;
}

internal bool32
enum_decl(MemoryArena* arena, TokenStream* input,
          AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Enum)
  {
    get_next_token(arena, input);

    if(input->token.kind == TokenKind_Id)
    {
      *node = new_enum(arena, &input->src_loc);
      AstEnum* enum_ = &(*node)->enum_;
      enum_->id = new_id(arena, &input->src_loc, input->token.lexeme);

      get_next_token(arena, input);
      if(input->token.kind == TokenKind_OpenBrace)
      {
        get_next_token(arena, input);

        AstNode* member = 0;
        do
        {
          member = 0;
          if(input->token.kind == TokenKind_Id)
          {
            member = new_id(arena, &input->src_loc, input->token.lexeme);
            list_append(arena, &enum_->member_list, member);

            get_next_token(arena, input);
            if(input->token.kind == TokenKind_Comma)
            {
              get_next_token(arena, input);
            }
            else if(input->token.kind != TokenKind_CloseBrace)
            {
              member = 0;
            }
          }
        }
        while(member);

        if(input->token.kind == TokenKind_CloseBrace)
        {
          get_next_token(arena, input);
        }
        else
        {
          compile_error(&input->src_loc, "(%d) Missing `}`", __LINE__);
          success = false;
        }
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Expected `{`", __LINE__);
        success = false;
      }
    }
    else
    {
      compile_error(&input->src_loc, "(%d) Identifier expected", __LINE__);
      success = false;
    }
  }
  return success;
}

internal bool32
struct_decl(MemoryArena* arena, TokenStream* input,
            AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Struct)
  {
    get_next_token(arena, input);

    if(input->token.kind == TokenKind_Id)
    {
      *node = new_struct(arena, &input->src_loc);
      AstStruct* struct_ = &(*node)->struct_;
      struct_->id = new_id(arena, &input->src_loc, input->token.lexeme);

      get_next_token(arena, input);
      if(input->token.kind == TokenKind_OpenBrace)
      {
        get_next_token(arena, input);

        AstNode* member = 0;
        do
        {
          member = 0;
          AstNode* type = 0;

          success = type_id(arena, input, &type);
          if(!success) goto end;

          if(type)
          {
            if(input->token.kind == TokenKind_Id)
            {
              member = new_var_decl(arena, &input->src_loc);
              AstVarDecl* var_decl = &member->var_decl;
              var_decl->type = type;

              var_decl->id = new_id(arena, &input->src_loc, input->token.lexeme);

              get_next_token(arena, input);
              if(input->token.kind == TokenKind_OpenBracket)
              {
                success = array_index(arena, input, member, &member);
                if(!success) goto end;
              }

              assert(member);
              list_append(arena, &struct_->member_list, member);
              success = semicolon(arena, input);
            }
            else
            {
              compile_error(&input->src_loc, "(%d) Identifier expected", __LINE__);
              success = false; goto end;
            }
          }
        }
        while(member);

        if(input->token.kind == TokenKind_CloseBrace)
        {
          get_next_token(arena, input);
        }
        else
        {
          compile_error(&input->src_loc, "(%d) Missing `}`", __LINE__);
          success = false;
        }
      }
      else
      {
        compile_error(&input->src_loc, "(%d) Expected `{`", __LINE__);
        success = false;
      }
    }
    else
    {
      compile_error(&input->src_loc, "(%d) Identifier expected", __LINE__);
      success = false;
    }
  }
end:
  return success;
}

internal bool32
module_element(MemoryArena* arena, TokenStream* input,
               AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Include)
  {
    success = include_stmt(arena, input, node) && semicolon(arena, input);
  }
  else if(input->token.kind == TokenKind_Proc)
  {
    success = proc_decl(arena, input, node);
  }
  else if(input->token.kind == TokenKind_Var)
  {
    success = var_decl(arena, input, node) && semicolon(arena, input);
  }
  else if(input->token.kind == TokenKind_Struct)
  {
    success = struct_decl(arena, input, node);
  }
  else if(input->token.kind == TokenKind_Enum)
  {
    success = enum_decl(arena, input, node);
  }
  return success;
}

internal bool32
module(MemoryArena* arena, TokenStream* input,
       List* node_list)
{
  bool32 success = true;

  AstNode* node = 0;
  do
  {
    if((success = module_element(arena, input, &node)) && node)
    {
      list_append(arena, node_list, node);
    }
  }
  while(success && node);
  return success;
}

internal bool32
return_stmt(MemoryArena* arena, TokenStream* input,
            AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Return)
  {
    *node = new_return_stmt(arena, &input->src_loc);

    get_next_token(arena, input);
    success = expression(arena, input, &(*node)->ret_stmt.expr);
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
          AstNode* var_node = new_var_occur(arena, &input->src_loc);
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

          AstNode* assgn_node = new_bin_expr(arena, &input->src_loc);
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

internal bool32
break_stmt(MemoryArena* arena, TokenStream* input,
           AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Break)
  {
    *node = new_break_stmt(arena, &input->src_loc);
    get_next_token(arena, input);
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

internal bool32
statement(MemoryArena* arena, TokenStream* input,
          AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_If)
  {
    success = if_stmt(arena, input, node);
  }
  else if(input->token.kind == TokenKind_While)
  {
    success = while_stmt(arena, input, node);
  }
  else if(input->token.kind == TokenKind_For)
  {
    success = for_stmt(arena, input, node);
  }
  else if(input->token.kind == TokenKind_Return)
  {
    success = return_stmt(arena, input, node) && semicolon(arena, input);
  }
  else if(input->token.kind == TokenKind_Break)
  {
    success = break_stmt(arena, input, node) && semicolon(arena, input);
  }
  else if(input->token.kind == TokenKind_Var)
  {
    success = var_decl(arena, input, node) && semicolon(arena, input);
  }
  else if(input->token.kind == TokenKind_Semicolon)
  {
    get_next_token(arena, input);
    *node = new_empty_stmt(arena, &input->src_loc);
  }
  else
  {
    if((success = expression(arena, input, node)) && *node)
    {
      success = semicolon(arena, input);
    }
  }
  return success;
}

bool32
parse(MemoryArena* arena, TokenStream* input, AstNode** node)
{
  bool32 success = true;

  *node = new_module(arena, &input->src_loc);

  success = module(arena, input, &(*node)->module.node_list);
  if(success)
  {
    if(input->token.kind != TokenKind_EndOfInput)
    {
      compile_error(&input->src_loc, "(%d) Unexpected token: %s", __LINE__, input->token.lexeme);
      success = false;
    }
  }

  return success;
}

internal void
DEBUG_print_tree_node(String* str, int indent_level, char* message, ...)
{
  for(int i = 0; i < indent_level; i++)
  {
    str_append(str, " ");
  }

  va_list varargs;
  va_start(varargs, message);
  str_printf(str, message, varargs);
  va_end(varargs);

  str_append(str, "\n");
}

internal void
DEBUG_print_ast_node_list(String* str, int indent_level, List* node_list, char* tag)
{
  if(node_list->count > 0)
  {
    if(tag)
    {
      DEBUG_print_tree_node(str, indent_level, tag);
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
      DEBUG_print_tree_node(str, indent_level, tag);
      ++indent_level;
    }
    DEBUG_print_tree_node(str, indent_level, DEBUG_AstNodeKind_tags[node->kind]);
    ++indent_level;

    if(node->kind == AstNodeKind_Module)
    {
      AstModule* module = &node->module;
      DEBUG_print_ast_node_list(str, indent_level, &module->node_list, "node_list");
    }
    else if(node->kind == AstNodeKind_IncludeStmt)
    {
      AstIncludeStmt* inc = &node->include;
      DEBUG_print_tree_node(str, indent_level, "file_path: \"%s\"", inc->file_path);
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
      DEBUG_print_tree_node(str, indent_level, "name: %s", id->name);
    }
    else if(node->kind == AstNodeKind_Block)
    {
      DEBUG_print_ast_node_list(str, indent_level, &node->block.stmt_list, "stmt_list");
    }
    else if(node->kind == AstNodeKind_BinExpr)
    {
      AstBinExpr* bin_expr = &node->bin_expr;
      DEBUG_print_tree_node(str, indent_level, "op: %s", DEBUG_AstOpKind_tags[bin_expr->op]);
      DEBUG_print_ast_node(str, indent_level, bin_expr->lhs, "lhs");
      DEBUG_print_ast_node(str, indent_level, bin_expr->rhs, "rhs");
    }
    else if(node->kind == AstNodeKind_UnrExpr)
    {
      AstUnrExpr* unr_expr = &node->unr_expr;
      DEBUG_print_tree_node(str, indent_level, "op: %s", DEBUG_AstOpKind_tags[unr_expr->op]);
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
      DEBUG_print_tree_node(str, indent_level, DEBUG_AstLiteralKind_tags[lit->kind]);
      if(lit->kind == AstLiteralKind_Int)
        DEBUG_print_tree_node(str, indent_level, "int_val: %d", lit->int_val);
      else if(lit->kind == AstLiteralKind_Float)
        DEBUG_print_tree_node(str, indent_level, "float_val: %f", lit->float_val);
      else if(lit->kind == AstLiteralKind_Bool)
        DEBUG_print_tree_node(str, indent_level, "bool_val: %d", lit->bool_val);
      else if(lit->kind == AstLiteralKind_Char)
        DEBUG_print_tree_node(str, indent_level, "char_val: '%c'", lit->char_val);
      else if(lit->kind == AstLiteralKind_String)
        DEBUG_print_tree_node(str, indent_level, "str: \"%s\"", lit->str);
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
            node->kind == AstNodeKind_BreakStmt)
    {
      // do nothing
    }
    else if(node->kind == AstNodeKind_Struct)
    {
      AstStruct* struct_ = &node->struct_;
      DEBUG_print_ast_node(str, indent_level, struct_->id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &struct_->member_list, "member_list");
    }
    else if(node->kind == AstNodeKind_Enum)
    {
      AstEnum* enum_ = &node->enum_;
      DEBUG_print_ast_node(str, indent_level, enum_->id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &enum_->member_list, "member_list");
    }
    else
    {
      assert(false);
    } 
  }
}
