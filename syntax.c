bool32
is_logical_operator(AstOpKind op)
{
  return op >= AstOpKind_LogicEquals && op <= AstOpKind_LogicNot;
}

AstNode*
ast_new_node(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_module(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Module;
  node->src_loc = *src_loc;
  list_init(&node->module.node_list);
  return node;
}

/*
AstNode*
ast_new_block(MemoryArena* arena, SourceLocation* src_loc)
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
AstNode*
ast_new_block(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Block;
  node->src_loc = *src_loc;
  list_init(&node->block.stmt_list);
  return node;
}

AstNode*
ast_new_id(MemoryArena* arena, SourceLocation* src_loc, char* name)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Id;
  node->src_loc = *src_loc;
  node->id.name = name;
  return node;
}

AstNode*
ast_new_enum(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Enum;
  list_init(&node->enum_.member_list);
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_pointer(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Pointer;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_call(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Call;
  list_init(&node->call.actual_args);
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_array(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Array;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_proc(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Proc;
  list_init(&node->proc.formal_args);
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_bin_expr(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_BinExpr;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_unr_expr(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_UnrExpr;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_int_literal(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_Int;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_float_literal(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_Float;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_bool_literal(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_Bool;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_string_literal(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_String;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_var_decl(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_VarDecl;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_while_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_WhileStmt;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_if_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_IfStmt;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_return_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_ReturnStmt;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_break_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_BreakStmt;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_include_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_IncludeStmt;
  node->src_loc = *src_loc;
  list_init(&node->inc_stmt.node_list);
  return node;
}

AstNode*
ast_new_empty_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_EmptyStmt;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_struct(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Struct;
  list_init(&node->struct_.member_list);
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_cast(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Cast;
  node->src_loc = *src_loc;
  return node;
}

void
expect_semicolon(MemoryArena* arena, TokenStream* input, bool32* success)
{
  if(input->token.kind == TokenKind_Semicolon)
  {
    get_next_token(arena, input);
  }
  else
  {
    compile_error(&input->src_loc, "Missing `;`");
    *success = false;
  }
}

bool32
parse_rest_of_type_id(MemoryArena* arena, TokenStream* input,
                      AstNode* expr, AstNode** node)
{
  *node = expr;
  bool32 success = true;
  
  if(input->token.kind == TokenKind_Star ||
     input->token.kind == TokenKind_Pointer)
  {
    *node = ast_new_pointer(arena, &input->src_loc);
    AstPointer* ptr = &(*node)->pointer;
    ptr->expr = expr;

    get_next_token(arena, input);
    success = parse_rest_of_type_id(arena, input, *node, node);
  }
  return true;
}

bool32
parse_type_id(MemoryArena* arena, TokenStream* input,
              AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Id)
  {
    *node = ast_new_id(arena, &input->src_loc, input->token.lexeme);

    get_next_token(arena, input);
    if(input->token.kind == TokenKind_Star)
    {
      success = parse_rest_of_type_id(arena, input, *node, node);
    }
  }
  return success;
}

bool32
parse_actual_argument_list(MemoryArena* arena, TokenStream* input,
                           List* arg_list)
{
  bool32 success = true;
  
  AstNode* arg_node = 0;
  success = parse_expression(arena, input, &arg_node);
  if(success && arg_node)
  {
    list_append(arena, arg_list, arg_node);

    if(input->token.kind == TokenKind_Comma)
    {
      get_next_token(arena, input);
      success = parse_actual_argument_list(arena, input, arg_list);
    }
  }
  return success;
}

bool32
parse_statement_list(MemoryArena* arena, TokenStream* input,
                     List* stmt_list)
{
  bool32 success = true;

  while(input->token.kind == TokenKind_Semicolon)
    get_next_token(arena, input);

  AstNode* stmt_node = 0;
  do
  {
    if((success = parse_statement(arena, input, &stmt_node)) && stmt_node)
    {
      list_append(arena, stmt_list, stmt_node);
    }
  }
  while(success && stmt_node);
  return success;
}

bool32
parse_block(MemoryArena* arena, TokenStream* input,
            AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    *node = ast_new_block(arena, &input->src_loc);

    get_next_token(arena, input);
    if(success = parse_statement_list(arena, input, &(*node)->block.stmt_list))
    {
      if(input->token.kind == TokenKind_CloseBrace)
      {
        get_next_token(arena, input);
      }
      else
      {
        compile_error(&input->src_loc, "Missing `}`");
        success = false;
      }
    }
  }

  return success;
}

bool32
parse_array_index(MemoryArena* arena, TokenStream* input,
                  AstNode* expr, AstNode** node)
{
  *node = expr;
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenBracket)
  {
    *node = ast_new_array(arena, &input->src_loc);
    AstArray* array = &(*node)->array;
    array->expr = expr;

    get_next_token(arena, input);
    if(success = parse_expression(arena, input, &array->index))
    {
      if(input->token.kind == TokenKind_CloseBracket)
      {
        get_next_token(arena, input);

        success = parse_array_index(arena, input, *node, node);
      }
      else
      {
        compile_error(&input->src_loc, "Missing `]`");
        success = false;
      }
    }
  }
  return success;
}

bool32
parse_rest_of_id(MemoryArena* arena, TokenStream* input,
                 AstNode* id, AstNode** node)
{
  *node = id;
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    *node = ast_new_call(arena, &input->src_loc);
    AstCall* call = &(*node)->call;
    call->id = id;

    get_next_token(arena, input);
    if(success = parse_actual_argument_list(arena, input, &call->actual_args))
    {
      if(input->token.kind == TokenKind_CloseParens)
      {
        get_next_token(arena, input);
      }
      else
      {
        compile_error(&input->src_loc, "Missing `)`");
        success = false;
      }
    }
  }
  else if(input->token.kind == TokenKind_OpenBracket)
  {
    success = parse_array_index(arena, input, *node, node);
  }

  return success;
}

bool32
parse_factor(MemoryArena* arena, TokenStream* input,
             AstNode** node)
{
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    get_next_token(arena, input);

    if(success = parse_expression(arena, input, node))
    {
      if(input->token.kind == TokenKind_CloseParens)
      {
        get_next_token(arena, input);
      }
      else {
        compile_error(&input->src_loc, "Missing `)`");
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
    *node = ast_new_int_literal(arena, &input->src_loc);
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

    get_next_token(arena, input);
  }
  else if(input->token.kind == TokenKind_Id)
  {
    *node = ast_new_id(arena, &input->src_loc, input->token.lexeme);

    get_next_token(arena, input);
    success = parse_rest_of_id(arena, input, *node, node);
#if 0
    {
      *node = ast_new_var_occur(arena, &input->src_loc);
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
        compile_error(&input->src_loc, "Unknown identifier: %s", id_name);
        success = false;
      }
    }
#endif
  }
  else if(input->token.kind == TokenKind_Cast)
  {
    *node = ast_new_cast(arena, &input->src_loc);
    AstCast* cast = &(*node)->cast;

    get_next_token(arena, input);
    if(input->token.kind == TokenKind_OpenParens)
    {
      get_next_token(arena, input);

      if((success = parse_type_id(arena, input, &cast->type)) && (&cast->type))
      {
        if(input->token.kind == TokenKind_CloseParens)
        {
          get_next_token(arena, input);

          success = parse_expression(arena, input, &cast->expr);
          if(!cast->expr)
          {
            compile_error(&input->src_loc, "Invalid expression after cast(..)");
            success = false;
          }
        }
        else
        {
          compile_error(&input->src_loc, "Missing `)`");
          success = false;
        }
      }
      else
      {
        compile_error(&input->src_loc, "Invalid type expression in cast(..)");
        success = false;
      }
    }
    else
    {
      compile_error(&input->src_loc, "Missing `(`");
      success = false;
    }
  }

  return success;
}

bool32
parse_unary_factor(MemoryArena* arena, TokenStream* input,
                   AstNode** node)
{
  bool32 success = true;
  *node = 0;

  if(input->token.kind == TokenKind_Bang ||
     input->token.kind == TokenKind_Pointer ||
     input->token.kind == TokenKind_AddressOf ||
     input->token.kind == TokenKind_UnaryMinus)
  {
    *node = ast_new_unr_expr(arena, &input->src_loc);
    AstUnrExpr* expr = &(*node)->unr_expr;

    if(input->token.kind == TokenKind_Bang)
      expr->op = AstOpKind_LogicNot;
    else if(input->token.kind == TokenKind_Pointer)
      expr->op = AstOpKind_Deref;
    else if(input->token.kind == TokenKind_AddressOf)
      expr->op = AstOpKind_AddressOf;
    else if(input->token.kind == TokenKind_UnaryMinus)
      expr->op = AstOpKind_Neg;

    get_next_token(arena, input);
    if(success = parse_unary_factor(arena, input, &expr->operand))
    {
      if(!expr->operand)
      {
        compile_error(&input->src_loc, "Expression expected");
        success = false;
      }
    }
  }
  else
  {
    success = parse_factor(arena, input, node);
  }
  return success;
}

bool32
parse_rest_of_factors(MemoryArena* arena, TokenStream* input,
                      AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool32 success = true;

  if(input->token.kind == TokenKind_Star ||
     input->token.kind == TokenKind_FwdSlash ||
     input->token.kind == TokenKind_Percent ||
     input->token.kind == TokenKind_EqualsEquals ||
     input->token.kind == TokenKind_BangEquals ||
     input->token.kind == TokenKind_AmprsndAmprsnd ||
     input->token.kind == TokenKind_PipePipe ||
     input->token.kind == TokenKind_AngleLeft ||
     input->token.kind == TokenKind_AngleLeftEquals ||
     input->token.kind == TokenKind_AngleRight ||
     input->token.kind == TokenKind_AngleRightEquals ||
     input->token.kind == TokenKind_Dot)
  {
    *node = ast_new_bin_expr(arena, &input->src_loc);
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
    else if(input->token.kind == TokenKind_BangEquals)
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
    else if(input->token.kind == TokenKind_AmprsndAmprsnd)
    {
      expr->op = AstOpKind_LogicAnd;
    }
    else if(input->token.kind == TokenKind_PipePipe)
    {
      expr->op = AstOpKind_LogicOr;
    }
    else if(input->token.kind == TokenKind_Dot)
    {
      expr->op = AstOpKind_MemberAccess;
    }
    else
      assert(false);

    get_next_token(arena, input);
    if(success = parse_unary_factor(arena, input, &expr->rhs))
    {
      if(expr->rhs)
      {
        success = parse_rest_of_factors(arena, input, *node, node);
      }
      else {
        compile_error(&input->src_loc, "Factor expected");
        success = false;
      }
    }
  }

  return success;
}

bool32
parse_term(MemoryArena* arena, TokenStream* input,
           AstNode** node)
{
  bool32 success = true;

  if((success = parse_unary_factor(arena, input, node)) && (*node))
  {
    success = parse_rest_of_factors(arena, input, *node, node);
  }
  return success;
}

bool32
parse_rest_of_terms(MemoryArena* arena, TokenStream* input,
                    AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool32 success = true;

  if(input->token.kind == TokenKind_Plus || input->token.kind == TokenKind_Minus)
  {
    *node = ast_new_bin_expr(arena, &input->src_loc);
    AstBinExpr* expr = &(*node)->bin_expr;
    expr->lhs = left_node;

    if(input->token.kind == TokenKind_Plus)
      expr->op = AstOpKind_Add;
    else if(input->token.kind == TokenKind_Minus)
      expr->op = AstOpKind_Sub;
    else
      assert(false);

    get_next_token(arena, input);

    if((success = parse_term(arena, input, &expr->rhs)) && expr->rhs)
    {
      success = parse_rest_of_terms(arena, input, *node, node);
    }
    else
    {
      compile_error(&input->src_loc, "Expression term expected");
      success = false;
    }
  }

  return success;
}

bool32
parse_assignment_term(MemoryArena* arena, TokenStream* input,
                      AstNode** node)
{
  bool32 success = true;

  if((success = parse_term(arena, input, node)) && (*node))
  {
    success = parse_rest_of_terms(arena, input, *node, node);
  }
  return success;
}

bool32
parse_rest_of_assignment_terms(MemoryArena* arena, TokenStream* input,
                               AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool32 success = true;

  if(input->token.kind == TokenKind_Equals)
  {
    *node = ast_new_bin_expr(arena, &input->src_loc);
    AstBinExpr* expr = &(*node)->bin_expr;
    expr->op = AstOpKind_Assign;
    expr->lhs = left_node;

    get_next_token(arena, input);
    if(success = parse_expression(arena, input, &expr->rhs))
    {
      if(!expr->rhs)
      {
        compile_error(&input->src_loc, "Missing right side of assignment");
        success = false;
      }
    }
  }

  return success;
}

bool32
parse_expression(MemoryArena* arena, TokenStream* input,
                 AstNode** node)
{
  bool32 success = true;

  if((success = parse_assignment_term(arena, input, node)) && (*node))
  {
    success = parse_rest_of_assignment_terms(arena, input, *node, node);
  }
  return success;
}

bool32
parse_formal_arg_decl(MemoryArena* arena, TokenStream* input,
                      AstNode** node)
{
  *node = 0;
  bool32 success = true;

  AstNode* type = 0;
  if((success = parse_type_id(arena, input, &type)) && type)
  {
    *node = ast_new_var_decl(arena, &input->src_loc);
    AstVarDecl* var_decl = &(*node)->var_decl;
    var_decl->type = type;

    if(input->token.kind == TokenKind_Id)
    {
      var_decl->id = ast_new_id(arena, &input->src_loc, input->token.lexeme);

      get_next_token(arena, input);
      if(input->token.kind == TokenKind_OpenBracket)
      {
        success = parse_array_index(arena, input, var_decl->id, &var_decl->id);
      }
    }
    else
    {
      compile_error(&input->src_loc, "Identifier expected");
      success = false;
    }
  }
  return success;
}

bool32
parse_var_decl(MemoryArena* arena, TokenStream* input,
               AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Var)
  {
    get_next_token(arena, input);

    *node = ast_new_var_decl(arena, &input->src_loc);
    AstVarDecl* var_decl = &(*node)->var_decl;
    if((success = parse_type_id(arena, input, &var_decl->type)) && var_decl->type)
    {
      if(input->token.kind == TokenKind_Id)
      {
        var_decl->id = ast_new_id(arena, &input->src_loc, input->token.lexeme);

        get_next_token(arena, input);
        if(input->token.kind == TokenKind_OpenBracket)
        {
          success = parse_array_index(arena, input, var_decl->id, &var_decl->id);
        }

        if(success && (input->token.kind == TokenKind_Equals))
        {
          get_next_token(arena, input);

          if(success = parse_expression(arena, input, &var_decl->init_expr))
          {
            if(!var_decl->init_expr)
            {
              compile_error(&input->src_loc, "Expression expected");
              success = false;
            }
          }
        }
      }
      else
      {
        compile_error(&input->src_loc, "Identifier expected");
        success = false;
      }
    }
  }
  return success;
}

bool32
parse_while_stmt(MemoryArena* arena, TokenStream* input,
                 AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_While)
  {
    *node = ast_new_while_stmt(arena, &input->src_loc);
    AstWhileStmt* while_stmt = &(*node)->while_stmt;

    get_next_token(arena, input);
    if(input->token.kind == TokenKind_OpenParens)
    {
      get_next_token(arena, input);

      if(success = parse_expression(arena, input, &while_stmt->cond_expr))
      {
        if(input->token.kind == TokenKind_CloseParens)
        {
          get_next_token(arena, input);

          if(while_stmt->cond_expr)
          {
            if(success = parse_block(arena, input, &while_stmt->body))
            {
              if(!while_stmt->body)
              {
                if(success = parse_statement(arena, input, &while_stmt->body))
                {
                  if(!while_stmt->body)
                  {
                    compile_error(&input->src_loc, "Statement(s) required");
                    success = false;
                  }
                }
              }
            }
          }
          else
          {
            compile_error(&input->src_loc, "Expression required");
            success = false;
          }
        }
        else
        {
          compile_error(&input->src_loc, "Missing `)`");
          success = false;
        }
      }
    }
    else
    {
      compile_error(&input->src_loc, "Missing `(`");
      success = false;
    }
  }

  return success;
}

bool32
parse_else_stmt(MemoryArena* arena, TokenStream* input,
                AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Else)
  {
    get_next_token(arena, input);

    if(success = parse_block(arena, input, node))
    {
      if(!(*node))
      {
        if(success = parse_statement(arena, input, node))
        {
          if(!(*node))
          {
            compile_error(&input->src_loc, "Statement(s) required");
            success = false;
          }
        }
      }
    }
  }
  return success;
}

bool32
parse_if_stmt(MemoryArena* arena, TokenStream* input,
              AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_If)
  {
    *node = ast_new_if_stmt(arena, &input->src_loc);
    AstIfStmt* if_stmt = &(*node)->if_stmt;

    get_next_token(arena, input);
    if(input->token.kind == TokenKind_OpenParens)
    {
      get_next_token(arena, input);

      if(success = parse_expression(arena, input, &if_stmt->cond_expr))
      {
        if(input->token.kind == TokenKind_CloseParens)
        {
          get_next_token(arena, input);

          if(if_stmt->cond_expr)
          {
            if(success = parse_block(arena, input, &if_stmt->body))
            {
              if(!if_stmt->body)
              {
                if(success = parse_statement(arena, input, &if_stmt->body))
                {
                  if(!if_stmt->body)
                  {
                    compile_error(&input->src_loc, "Statement(s) required");
                    success = false;
                  }
                }
              }
              if(success)
              {
                assert(if_stmt->body);
                success = parse_else_stmt(arena, input, &if_stmt->else_body);
              }
            }
          }
          else
          {
            compile_error(&input->src_loc, "Expression required");
            success = false;
          }
        }
        else
        {
          compile_error(&input->src_loc, "Missing `)`");
          success = false;
        }
      }
    }
    else
    {
      compile_error(&input->src_loc, "Missing `(`");
      success = false;
    }
  }
  return success;
}

bool32
parse_proc_decl(MemoryArena* arena, TokenStream* input,
                AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Proc)
  {
    get_next_token(arena, input);

    *node = ast_new_proc(arena, &input->src_loc);
    AstProc* proc = &(*node)->proc;

    if((success = parse_type_id(arena, input, &proc->ret_type)) && proc->ret_type)
    {
      if(input->token.kind == TokenKind_Id)
      {
        proc->id = ast_new_id(arena, &input->src_loc, input->token.lexeme);

        get_next_token(arena, input);
        if(input->token.kind == TokenKind_OpenParens)
        {
          get_next_token(arena, input);

          AstNode* arg = 0;
          do
          {
            if((success = parse_formal_arg_decl(arena, input, &arg)) && arg)
            {
              list_append(arena, &proc->formal_args, arg);
              if(input->token.kind == TokenKind_Comma)
              {
                get_next_token(arena, input);
              }
              else if(input->token.kind != TokenKind_CloseParens)
              {
                compile_error(&input->src_loc, "Missing `,`");
                success = false;
              }
            }
          }
          while(success && arg);

          if(success)
          {
            if(input->token.kind == TokenKind_CloseParens)
            {
              get_next_token(arena, input);
              if(success = parse_block(arena, input, &proc->body))
              {
                if(!proc->body)
                {
                  expect_semicolon(arena, input, &success);
                }
              }
            }
            else
            {
              compile_error(&input->src_loc, "Missing `)`");
              success = false;
            }
          }
        }
        else
        {
          compile_error(&input->src_loc, "Expected `(`");
          success = false;
        }
      }
    }
  }
  return success;
}

bool32
parse_include_stmt(MemoryArena* arena, TokenStream* input,
                   AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Include)
  {
    get_next_token(arena, input);

    if(input->token.kind == TokenKind_String)
    {
      *node = ast_new_include_stmt(arena, &input->src_loc);
      AstIncludeStmt* inc_stmt = &(*node)->inc_stmt;

      String str = {0};
      str_init(&str, arena);
      str_append(&str, input->src_loc.file_path);
      path_make_dir(str.head);
      str_tidyup(&str);
      str_append(&str, input->token.str);
      inc_stmt->file_path = str.head;

      get_next_token(arena, input);

      char* hoc_text = file_read_text(arena, inc_stmt->file_path);
      if(hoc_text)
      {
        TokenStream* inc_input = mem_push_struct(arena, TokenStream, 1);
        token_stream_init(inc_input, hoc_text, inc_stmt->file_path);

        get_next_token(arena, inc_input);
        success = parse_module(arena, inc_input, &inc_stmt->node_list);
        if(inc_input->token.kind != TokenKind_EndOfInput)
        {
          compile_error(&input->src_loc, "Unexpected token: %s", input->token.lexeme);
          success = false;
        }
      }
      else
      {
        compile_error(&input->src_loc, "File could not be read: %s", inc_stmt->file_path);
        success = false;
      }
    }
    else
    {
      compile_error(&input->src_loc, "String required after `include`\n");
      success = false;
    }
  }
  return success;
}

bool32
parse_enum(MemoryArena* arena, TokenStream* input,
           AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Enum)
  {
    get_next_token(arena, input);

    if(input->token.kind == TokenKind_Id)
    {
      *node = ast_new_enum(arena, &input->src_loc);
      AstEnum* enum_ = &(*node)->enum_;
      enum_->id = ast_new_id(arena, &input->src_loc, input->token.lexeme);

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
            member = ast_new_id(arena, &input->src_loc, input->token.lexeme);
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
          compile_error(&input->src_loc, "Missing `}`");
          success = false;
        }
      }
    }
    else
    {
      compile_error(&input->src_loc, "Identifier expected");
      success = false;
    }
  }
  return success;
}

bool32
parse_struct(MemoryArena* arena, TokenStream* input,
             AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Struct)
  {
    get_next_token(arena, input);

    if(input->token.kind == TokenKind_Id)
    {
      *node = ast_new_struct(arena, &input->src_loc);
      AstStruct* struct_ = &(*node)->struct_;
      struct_->id = ast_new_id(arena, &input->src_loc, input->token.lexeme);

      get_next_token(arena, input);
      if(input->token.kind == TokenKind_OpenBrace)
      {
        get_next_token(arena, input);

        AstNode* member = 0;
        do
        {
          member = 0;
          AstNode* type = 0;
          if((success = parse_type_id(arena, input, &type)) && type)
          {
            member = ast_new_var_decl(arena, &input->src_loc);
            AstVarDecl* var_decl = &member->var_decl;
            var_decl->type = type;

            if(input->token.kind == TokenKind_Id)
            {
              var_decl->id = ast_new_id(arena, &input->src_loc, input->token.lexeme);

              get_next_token(arena, input);
              if(input->token.kind == TokenKind_OpenBracket)
              {
                success = parse_array_index(arena, input, member, &member);
              }
            }

            if(success)
            {
              list_append(arena, &struct_->member_list, member);
              expect_semicolon(arena, input, &success);
            }
          }
        }
        while(success && member);

        if(success)
        {
          if(input->token.kind == TokenKind_CloseBrace)
          {
            get_next_token(arena, input);
          }
          else
          {
            compile_error(&input->src_loc, "Missing `}`");
            success = false;
          }
        }
      }
      else
      {
        compile_error(&input->src_loc, "Expected `{`");
        success = false;
      }
    }
  }
  return success;
}

bool32
parse_module_element(MemoryArena* arena, TokenStream* input,
                     AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Include)
  {
    if(success = parse_include_stmt(arena, input, node))
    {
      expect_semicolon(arena, input, &success);
    }
  }
  else if(input->token.kind == TokenKind_Proc)
  {
    success = parse_proc_decl(arena, input, node);
  }
  else if(input->token.kind == TokenKind_Var)
  {
    if(success = parse_var_decl(arena, input, node))
    {
      expect_semicolon(arena, input, &success);
    }
  }
  else if(input->token.kind == TokenKind_Struct)
  {
    success = parse_struct(arena, input, node);
  }
  else if(input->token.kind == TokenKind_Enum)
  {
    success = parse_enum(arena, input, node);
  }

  return success;
}

bool32
parse_module(MemoryArena* arena, TokenStream* input,
             List* node_list)
{
  bool32 success = true;

  AstNode* node = 0;
  do
  {
    if((success = parse_module_element(arena, input, &node)) && node)
    {
      list_append(arena, node_list, node);
    }
  }
  while(success && node);
  return success;
}

bool32
parse_return_stmt(MemoryArena* arena, TokenStream* input,
                  AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Return)
  {
    *node = ast_new_return_stmt(arena, &input->src_loc);

    get_next_token(arena, input);
    if(success = parse_expression(arena, input, &(*node)->ret_stmt.expr))
    {
      /*
      AstNode* owner = 0;
      int depth = block_find_owner(enclosing_block, AstNodeKind_Proc, &owner);
      if(owner)
      {
        AstProc* ret_proc = &owner->proc;
        ret_stmt->proc = ret_proc;
        ret_stmt->depth = depth;

        if(ret_expr)
        {
          AstNode* var_node = ast_new_var_occur(arena, &input->src_loc);
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

          AstNode* assgn_node = ast_new_bin_expr(arena, &input->src_loc);
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
      */
    }
  }

  return success;
}

bool32
parse_break_stmt(MemoryArena* arena, TokenStream* input,
                 AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Break)
  {
    *node = ast_new_break_stmt(arena, &input->src_loc);
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

bool32
parse_statement(MemoryArena* arena, TokenStream* input,
                AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_If)
  {
    success = parse_if_stmt(arena, input, node);
  }
  else if(input->token.kind == TokenKind_While)
  {
    success = parse_while_stmt(arena, input, node);
  }
  else if(input->token.kind == TokenKind_Return)
  {
    if(success = parse_return_stmt(arena, input, node))
    {
      expect_semicolon(arena, input, &success);
    }
  }
  else if(input->token.kind == TokenKind_Break)
  {
    if(success = parse_break_stmt(arena, input, node))
    {
      expect_semicolon(arena, input, &success);
    }
  }
  else if(input->token.kind == TokenKind_Var)
  {
    if(success = parse_var_decl(arena, input, node))
    {
      expect_semicolon(arena, input, &success);
    }
  }
  else if(input->token.kind == TokenKind_Semicolon)
  {
    get_next_token(arena, input);
    *node = ast_new_empty_stmt(arena, &input->src_loc);
  }
  else
  {
    if((success = parse_expression(arena, input, node)) && (*node))
    {
      expect_semicolon(arena, input, &success);
    }
  }
  return success;
}

bool32
parse(MemoryArena* arena, TokenStream* input, AstNode** node)
{
  bool32 success = true;

  *node = ast_new_module(arena, &input->src_loc);

  if(success = parse_module(arena, input, &(*node)->module.node_list))
  {
    if(input->token.kind != TokenKind_EndOfInput)
    {
      compile_error(&input->src_loc, "Unexpected token: %s", input->token.lexeme);
      success = false;
    }
  }

  return success;
}

void
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

void
DEBUG_print_ast_node_list(String* str, int indent_level, List* node_list, char* tag)
{
  if(node_list->count > 0)
  {
    if(tag)
    {
      DEBUG_print_tree_node(str, indent_level, tag);
      ++indent_level;
    }
    for(ListItem* list_item = list_first_item(node_list);
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
      AstIncludeStmt* inc = &node->inc_stmt;
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
      //++indent_level;
      DEBUG_print_ast_node(str, indent_level, call->id, "id");
      DEBUG_print_ast_node_list(str, indent_level, &call->actual_args, "actual_args");
    }
    else if(node->kind == AstNodeKind_EmptyStmt)
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
