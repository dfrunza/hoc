////////////////////////////////////////////////////////////////////////////////
//  Symbol table
//

Symbol*
lookup_symbol(SymbolTable* symbol_table, char* name, SymbolKind kind)
{
  Symbol* result = 0;

  Symbol* symbol = symbol_table->symbol;
  while(symbol)
  {
    if(symbol->kind == kind && cstr_match(symbol->name, name))
    {
      result = symbol;
      break;
    }
    symbol = symbol->next_symbol;
  }
  return result;
}

Symbol*
add_symbol(MemoryArena* arena, SymbolTable* symbol_table, char* name, SymbolKind kind)
{
  Symbol* symbol = mem_push_struct(arena, Symbol, 1);
  symbol->kind = kind;
  symbol->name = name;
  symbol->block_id = symbol_table->scope_id;
  symbol->nesting_depth = symbol_table->nesting_depth;
  symbol->next_symbol = symbol_table->symbol;
  symbol_table->symbol = symbol;
  return symbol;
}

bool32
is_logical_operator(AstOpKind op)
{
  return op >= AstOpKind_LogicEquals && op <= AstOpKind_LogicNot;
}

Symbol*
add_builtin_type(MemoryArena* arena, SymbolTable* symbol_table, char* name, Type* type)
{
  assert(type->kind == TypeKind_Basic);
  Symbol* symbol = add_symbol(arena, symbol_table, name, SymbolKind_Type);
  symbol->type = type;
  return symbol;
}

Symbol*
add_keyword(MemoryArena* arena, SymbolTable* symbol_table, char* name, TokenKind token_kind)
{
  Symbol* symbol = add_symbol(arena, symbol_table, name, SymbolKind_Keyword);
  symbol->keyword = token_kind;
  return symbol;
}

void
add_keyword_list(MemoryArena* arena, SymbolTable* symbol_table)
{
  add_builtin_type(arena, symbol_table, "bool", g_basic_type_bool);
  add_builtin_type(arena, symbol_table, "int", g_basic_type_int);
  add_builtin_type(arena, symbol_table, "char", g_basic_type_char);
  add_builtin_type(arena, symbol_table, "float", g_basic_type_float);
  add_builtin_type(arena, symbol_table, "void", g_basic_type_void);

  add_keyword(arena, symbol_table, "var", TokenKind_Var);
  add_keyword(arena, symbol_table, "proc", TokenKind_Proc);
  add_keyword(arena, symbol_table, "type", TokenKind_Type);
  add_keyword(arena, symbol_table, "struct", TokenKind_Type);
  add_keyword(arena, symbol_table, "array", TokenKind_Array);
  add_keyword(arena, symbol_table, "of", TokenKind_Of);
  add_keyword(arena, symbol_table, "if", TokenKind_If);
  add_keyword(arena, symbol_table, "else", TokenKind_Else);
  add_keyword(arena, symbol_table, "while", TokenKind_While);
  add_keyword(arena, symbol_table, "return", TokenKind_Return);
  add_keyword(arena, symbol_table, "break", TokenKind_Break);
  add_keyword(arena, symbol_table, "include", TokenKind_Include);
  add_keyword(arena, symbol_table, "true", TokenKind_True);
  add_keyword(arena, symbol_table, "false", TokenKind_False);
  add_keyword(arena, symbol_table, "print", TokenKind_Print);
  add_keyword(arena, symbol_table, "cast", TokenKind_Cast);
}

void
init_global_basic_types(MemoryArena* arena)
{
  g_basic_type_bool = new_basic_type(arena, BasicTypeKind_Bool);
  g_basic_type_int = new_basic_type(arena, BasicTypeKind_Int);
  g_basic_type_char = new_basic_type(arena, BasicTypeKind_Char);
  g_basic_type_float = new_basic_type(arena, BasicTypeKind_Float);
  g_basic_type_void = new_basic_type(arena, BasicTypeKind_Void);
}

bool32
scope_begin(SymbolTable* symbol_table)
{
  int scope_id = ++symbol_table->last_scope_id;
  symbol_table->scope_id = scope_id;

  int nesting_depth = ++symbol_table->nesting_depth;
  if(nesting_depth < sizeof_array(symbol_table->active_scopes))
  {
    symbol_table->active_scopes[nesting_depth] = scope_id;
  }
  else {
    error("Maximum scope nesting depth has been reached: %d", sizeof_array(symbol_table->active_scopes));
    return false;
  }

  return true;
}

void
scope_end(SymbolTable* symbol_table)
{
  int nesting_depth = --symbol_table->nesting_depth;
  int scope_id = symbol_table->active_scopes[nesting_depth];
  assert(scope_id >= 0);
  symbol_table->scope_id = scope_id;

  Symbol* symbol = symbol_table->symbol;
  while(symbol && symbol->block_id > symbol_table->scope_id)
    symbol = symbol->next_symbol;
  symbol_table->symbol = symbol;
}

////////////////////////////////////////////////////////////////////////////////
//  Syntax analysis
//

AstNode*
ast_new_module(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  list_init(&node->module.proc_list);
  node->kind = AstNodeKind_Module;
  node->type = g_basic_type_void;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_block(MemoryArena* arena, SymbolTable* symbol_table, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Block;
  node->type = g_basic_type_void;
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

AstNode*
ast_new_call(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Call;
  node->type = new_typevar(arena);
  node->src_loc = *src_loc;
  list_init(&node->call.actual_args);
  return node;
}

AstNode*
ast_new_proc(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Proc;
  node->type = new_typevar(arena);
  node->src_loc = *src_loc;
  list_init(&node->proc.formal_args);
  return node;
}

AstNode*
ast_new_bin_expr(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_BinExpr;
  node->type = new_typevar(arena);
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_unr_expr(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_UnrExpr;
  node->type = new_typevar(arena);
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_int_literal(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_Int;
  node->type = g_basic_type_int;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_float_literal(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_Float;
  node->type = g_basic_type_float;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_bool_literal(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Literal;
  node->literal.kind = AstLiteralKind_Bool;
  node->type = g_basic_type_bool;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_var_decl(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_VarDecl;
  node->type = new_typevar(arena);
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_var_occur(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_VarOccur;
  node->type = new_typevar(arena);
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_while_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_WhileStmt;
  node->type = g_basic_type_void;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_if_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_IfStmt;
  node->type = g_basic_type_void;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_return_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_ReturnStmt;
  node->type = new_typevar(arena);
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_break_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_BreakStmt;
  node->type = g_basic_type_void;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_print_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_PrintStmt;
  node->type = g_basic_type_void;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_include_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_IncludeStmt;
  node->type = g_basic_type_void;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_empty_stmt(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_EmptyStmt;
  node->type = g_basic_type_void;
  node->src_loc = *src_loc;
  return node;
}

AstNode*
ast_new_cast(MemoryArena* arena, SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Cast;
  node->type = g_basic_type_void;
  node->src_loc = *src_loc;
  return node;
}

bool32
parse_cast(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
            AstBlock* enclosing_block, AstNode** node)
{
  if(input->token.kind != TokenKind_Cast)
    return true;
  
  consume_token(arena, input, symbol_table);
  if(input->token.kind != TokenKind_OpenParens)
  {
    compile_error(&input->src_loc, "Missing '('");
    return false;
  }

  consume_token(arena, input, symbol_table);
  if(input->token.kind != TokenKind_Id)
  {
    compile_error(&input->src_loc, "Identifier required");
    return false;
  }

  Symbol* type_symbol = lookup_symbol(symbol_table, input->token.lexeme, SymbolKind_Type);
  if(!type_symbol)
  {
    compile_error(&input->src_loc, "Unknown type : %s", input->token.lexeme);
    return false;
  }

  *node = ast_new_cast(arena, &input->src_loc);
  (*node)->cast.to_type = type_symbol->type;

  consume_token(arena, input, symbol_table);
  if(input->token.kind != TokenKind_CloseParens)
  {
    compile_error(&input->src_loc, "Missing ')'");
    return false;
  }

  consume_token(arena, input, symbol_table);
  if(!parse_expression(arena, input, symbol_table, enclosing_block, &(*node)->cast.expr))
    return false;

  if(!(*node)->cast.expr)
  {
    compile_error(&input->src_loc, "Expression required");
    return false;
  }

  return true;
}

bool32
parse_actual_argument_list(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                           AstBlock* enclosing_block, AstCall* call)
{
  bool32 success = true;

  AstNode* arg_node = 0;
  if((success = parse_expression(arena, input, symbol_table, enclosing_block, &arg_node))
     && arg_node)
  {
    list_append(arena, &call->actual_args, arg_node);

    if(input->token.kind == TokenKind_Comma)
    {
      consume_token(arena, input, symbol_table);
      success = parse_actual_argument_list(arena, input, symbol_table, enclosing_block, call);
    }
  }

  return success;
}

bool32
parse_statement_list(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                     AstBlock* block)
{
  bool32 success = true;

  while(input->token.kind == TokenKind_Semicolon)
    consume_token(arena, input, symbol_table);

  AstNode* stmt_node = 0;
  if((success = parse_statement(arena, input, symbol_table, block, &stmt_node))
     && stmt_node)
  {
    if(stmt_node->kind == AstNodeKind_VarDecl)
    {
      list_append(arena, &block->decl_vars, stmt_node);
      AstVarDecl* var_decl = &stmt_node->var_decl;
      if(var_decl->init_expr)
      {
        list_append(arena, &block->stmt_list, var_decl->init_expr);
      }
    }
    else
      list_append(arena, &block->stmt_list, stmt_node);

    success = parse_statement_list(arena, input, symbol_table, block);
  }
  return success;
}

bool32
parse_block(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
            AstBlock* enclosing_block, AstNode* owner, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    consume_token(arena, input, symbol_table);

    if(success = scope_begin(symbol_table))
    {
      *node = ast_new_block(arena, symbol_table, &input->src_loc);
      AstBlock* block = &(*node)->block;
      block->owner = owner;
      block->enclosing_block = enclosing_block;

      if(success = parse_statement_list(arena, input, symbol_table, block))
      {
        if(input->token.kind == TokenKind_CloseBrace)
        {
          consume_token(arena, input, symbol_table);
          scope_end(symbol_table);
        }
        else {
          compile_error(&input->src_loc, "Missing '}'");
          success = false;
        }
      }
    }
  }

  return success;
}

bool32
parse_factor(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
             AstBlock* enclosing_block, AstNode** node)
{
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    consume_token(arena, input, symbol_table);

    if(success = parse_expression(arena, input, symbol_table, enclosing_block, node))
    {
      if(input->token.kind == TokenKind_CloseParens)
      {
        consume_token(arena, input, symbol_table);
      }
      else {
        compile_error(&input->src_loc, "Missing ')'");
        success = false;
      }
    }
  }
  else if(input->token.kind == TokenKind_UnaryMinus)
  {
    consume_token(arena, input, symbol_table);

    AstNode* operand = 0;
    if(success = parse_term(arena, input, symbol_table, enclosing_block, &operand))
    {
      if(operand)
      {
        *node = ast_new_unr_expr(arena, &input->src_loc);
        AstUnrExpr* expr = &(*node)->unr_expr;
        expr->op = AstOpKind_Neg;
        expr->operand = operand;
      }
      else {
        compile_error(&input->src_loc, "Expression expected after '-'");
        success = false;
      }
    }
  }
  else if(input->token.kind == TokenKind_Bang)
  {
    consume_token(arena, input, symbol_table);

    AstNode* operand = 0;
    if(success = parse_term(arena, input, symbol_table, enclosing_block, &operand))
    {
      if(operand)
      {
        *node = ast_new_unr_expr(arena, &input->src_loc);
        AstUnrExpr* expr = &(*node)->unr_expr;
        expr->op = AstOpKind_LogicNot;
        expr->operand = operand;
      }
      else {
        compile_error(&input->src_loc, "Expression expected after '!'");
        success = false;
      }
    }
  }
  else if(input->token.kind == TokenKind_IntNum)
  {
    *node = ast_new_int_literal(arena, &input->src_loc);
    AstLiteral* literal = &(*node)->literal;
    literal->int_val = *input->token.int_val;

    consume_token(arena, input, symbol_table);
  }
  else if(input->token.kind == TokenKind_FloatNum)
  {
    *node = ast_new_float_literal(arena, &input->src_loc);
    AstLiteral* literal = &(*node)->literal;
    literal->float_val = *input->token.float_val;

    consume_token(arena, input, symbol_table);
  }
  else if(input->token.kind == TokenKind_Id)
  {
    char* id_name = input->token.lexeme;
    consume_token(arena, input, symbol_table);

    if(input->token.kind == TokenKind_OpenParens)
    {
      *node = ast_new_call(arena, &input->src_loc);
      AstCall* call = &(*node)->call;

      Symbol* symbol = lookup_symbol(symbol_table, id_name, SymbolKind_Proc);
      if(symbol)
      {
        AstNode* proc_node = symbol->node;
        call->name = symbol->name;
        call->proc = proc_node;

        consume_token(arena, input, symbol_table);
        if(success = parse_actual_argument_list(arena, input, symbol_table, enclosing_block, call))
        {
          if(input->token.kind == TokenKind_CloseParens)
          {
            consume_token(arena, input, symbol_table);

            List* formal_args = &proc_node->proc.formal_args;
            List* actual_args = &call->actual_args;
            if(formal_args->count != actual_args->count)
            {
              compile_error(&input->src_loc, "Expected %d arguments in the call: %s(..)", formal_args->count, proc_node->proc.name);
              success = false;
            }
          }
          else {
            compile_error(&input->src_loc, "Missing ')' in procedure call");
            success = false;
          }
        }
        else {
          compile_error(&input->src_loc, "Missing '(' in procedure call");
          success = false;
        }
      }
      else {
        compile_error(&input->src_loc, "Unknown procedure: %s", input->token.lexeme);
        success = false;
      }
    }
    else
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
  }
  else if(input->token.kind == TokenKind_Cast)
  {
    success = parse_cast(arena, input, symbol_table, enclosing_block, node);
  }
  else if(input->token.kind == TokenKind_True || input->token.kind == TokenKind_False)
  {
    *node = ast_new_bool_literal(arena, &input->src_loc);
    AstLiteral* literal = &(*node)->literal;
    literal->bool_val = (input->token.kind == TokenKind_True ? 1 : 0);

    consume_token(arena, input, symbol_table);
  }

  return success;
}

bool32
parse_rest_of_factors(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                      AstBlock* enclosing_block, AstNode* left_node, AstNode** node)
{
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
     input->token.kind == TokenKind_AngleRightEquals)
  {
    *node = ast_new_bin_expr(arena, &input->src_loc);
    AstBinExpr* expr = &(*node)->bin_expr;

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
    else
      assert(false);

    consume_token(arena, input, symbol_table);

    AstNode* factor_node = 0;
    if(success = parse_factor(arena, input, symbol_table, enclosing_block, &factor_node))
    {
      if(factor_node)
      {
        expr->right_operand = factor_node;
        expr->left_operand = left_node;
        success = parse_rest_of_factors(arena, input, symbol_table, enclosing_block, *node, node);
      }
      else {
        compile_error(&input->src_loc, "Factor expected");
        success = false;
      }
    }
  }
  else
    *node = left_node;

  return success;
}

bool32
parse_term(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
           AstBlock* enclosing_block, AstNode** node)
{
  bool32 success = true;
  AstNode* factor_node = 0;
  AstNode* expr_node = 0;

  if((success = parse_factor(arena, input, symbol_table, enclosing_block, &factor_node))
     && factor_node)
  {
    success = parse_rest_of_factors(arena, input, symbol_table,
                                    enclosing_block, factor_node, &expr_node);
  }

  *node = expr_node;
  return success;
}

bool32
parse_rest_of_terms(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                    AstBlock* enclosing_block, AstNode* left_node, AstNode** node)
{
  bool32 success = true;

  if(input->token.kind == TokenKind_Plus || input->token.kind == TokenKind_Minus)
  {
    *node = ast_new_bin_expr(arena, &input->src_loc);
    AstBinExpr* expr = &(*node)->bin_expr;

    if(input->token.kind == TokenKind_Plus)
      expr->op = AstOpKind_Add;
    else if(input->token.kind == TokenKind_Minus)
      expr->op = AstOpKind_Sub;
    else
      assert(false);

    consume_token(arena, input, symbol_table);

    AstNode* term_node = 0;
    if((success = parse_term(arena, input, symbol_table, enclosing_block, &term_node))
       && term_node)
    {
      expr->right_operand = term_node;
      expr->left_operand = left_node;
      success = parse_rest_of_terms(arena, input, symbol_table, enclosing_block, *node, node);
    }
    else {
      compile_error(&input->src_loc, "Expression term expected");
      success = false;
    }
  }
  else
    *node = left_node;

  return success;
}

bool32
parse_assignment_term(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                      AstBlock* enclosing_block, AstNode** node)
{
  bool32 success = true;
  AstNode* term_node = 0;
  AstNode* expr_node = 0;

  if((success = parse_term(arena, input, symbol_table, enclosing_block, &term_node))
     && term_node)
  {
    success = parse_rest_of_terms(arena, input, symbol_table, enclosing_block, term_node, &expr_node);
  }

  *node = expr_node;
  return success;
}

bool32
parse_rest_of_assignment_terms(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                               AstBlock* enclosing_block, AstNode* left_node, AstNode** node)
{
  bool32 success = true;

  if(input->token.kind == TokenKind_Equals)
  {
    consume_token(arena, input, symbol_table);

    AstNode* right_side = 0;
    if(success = parse_expression(arena, input, symbol_table, enclosing_block, &right_side))
    {
      if(right_side)
      {
        if(left_node->kind == AstNodeKind_VarOccur)
        {
          *node = ast_new_bin_expr(arena, &input->src_loc);
          AstBinExpr* expr = &(*node)->bin_expr;
          expr->op = AstOpKind_Assign;
          expr->left_operand = left_node;
          expr->right_operand = right_side;
        }
        else {
          compile_error(&input->src_loc, "Variable required on the left side of assignment");
          success = false;
        }
      }
      else {
        compile_error(&input->src_loc, "Missing right side of assignment");
        success = false;
      }
    }
  }
  else
    *node = left_node;

  return success;
}

bool32
parse_expression(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                 AstBlock* enclosing_block, AstNode** node)
{
  bool32 success = true;
  AstNode* assgn_node = 0;
  AstNode* expr_node = 0;

  if((success = parse_assignment_term(arena, input, symbol_table, enclosing_block, &assgn_node))
     && assgn_node)
  {
    success = parse_rest_of_assignment_terms(arena, input, symbol_table,
                                             enclosing_block, assgn_node, &expr_node);
  }

  *node = expr_node;
  return success;
}

bool32
parse_var_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                    AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Var)
  {
    *node = ast_new_var_decl(arena, &input->src_loc);
    AstVarDecl* var_decl = &(*node)->var_decl;
    var_decl->data.size = 1;

    consume_token(arena, input, symbol_table);

    if(input->token.kind == TokenKind_Id)
    {
      Symbol* type_symbol = lookup_symbol(symbol_table, input->token.lexeme, SymbolKind_Type);
      if(type_symbol)
      {
        var_decl->var_type = type_symbol->type;

        consume_token(arena, input, symbol_table);
        if(input->token.kind == TokenKind_Id)
        {
          Symbol* var_symbol = add_symbol(arena, symbol_table, input->token.lexeme, SymbolKind_Var);
          var_symbol->node = *node;
          var_decl->name = var_symbol->name;

          consume_token(arena, input, symbol_table);
          if(input->token.kind == TokenKind_Equals)
          {
            AstNode* var_node = ast_new_var_occur(arena, &input->src_loc);
            AstVarOccur* var_occur = &var_node->var_occur;
            var_occur->name = var_decl->name;
            var_occur->data = &var_decl->data;
            var_occur->decl_block_offset = 0;
            var_occur->var_decl = *node;
            var_node->type = (*node)->type;

            AstNode* init_expr = 0;
            if(success = parse_rest_of_assignment_terms(arena, input, symbol_table,
                                                        enclosing_block, var_node, &init_expr))
            {
              var_decl->init_expr = init_expr;
            }
          }
        }
        else {
          compile_error(&input->src_loc, "Identifier expected");
          success = false;
        }
      }
      else {
        compile_error(&input->src_loc, "Unknown type: %s", input->token.lexeme);
        success = false;
      }
    }
    else {
      compile_error(&input->src_loc, "Identifier expected");
      success = false;
    }
  }

  return success;
}

bool32
parse_formal_argument_list(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                           AstBlock* enclosing_block, AstProc* proc)
{
  bool32 success = true;

  AstNode* arg_node = 0;
  if((success = parse_var_statement(arena, input, symbol_table, enclosing_block, &arg_node))
     && arg_node)
  {
    assert(arg_node->kind == AstNodeKind_VarDecl);
    if(!arg_node->var_decl.init_expr)
    {
      list_append(arena, &proc->formal_args, arg_node);

      if(input->token.kind == TokenKind_Comma)
      {
        consume_token(arena, input, symbol_table);
        success = parse_formal_argument_list(arena, input, symbol_table, enclosing_block, proc);
      }
    }
    else {
      compile_error(&input->src_loc, "Variable initializer not allowed here");
      success = false;
    }
  }

  return success;
}

bool32
parse_while_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                 AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_While)
  {
    consume_token(arena, input, symbol_table);

    if(input->token.kind == TokenKind_OpenParens)
    {
      consume_token(arena, input, symbol_table);

      AstNode* expr_node = 0;
      if(success = parse_expression(arena, input, symbol_table, enclosing_block, &expr_node))
      {
        if(input->token.kind == TokenKind_CloseParens)
        {
          consume_token(arena, input, symbol_table);

          if(expr_node)
          {
            *node = ast_new_while_stmt(arena, &input->src_loc);
            AstWhileStmt* while_stmt = &(*node)->while_stmt;
            while_stmt->cond_expr = expr_node;

            AstNode* body_node = 0;
            if(success = parse_block(arena, input, symbol_table, enclosing_block, *node, &body_node))
            {
              if(body_node)
              {
                while_stmt->body = body_node;
              }
              else
              {
                if(success = parse_statement(arena, input, symbol_table, enclosing_block, &body_node))
                {
                  if(body_node)
                  {
                    while_stmt->body = body_node;
                  }
                  else {
                    compile_error(&input->src_loc, "Statement(s) required");
                    success = false;
                  }
                }
              }
            }
          }
          else {
            compile_error(&input->src_loc, "Expression required");
            success = false;
          }
        }
        else {
          compile_error(&input->src_loc, "Missing ')'");
          success = false;
        }
      }
    }
    else {
      compile_error(&input->src_loc, "Missing '('");
      success = false;
    }
  }

  return success;
}

bool32
parse_else_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                     AstBlock* enclosing_block, AstNode* owner, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Else)
  {
    consume_token(arena, input, symbol_table);

    AstNode* else_node = 0;
    if(success = parse_if_stmt(arena, input, symbol_table, enclosing_block, &else_node))
    {
      if(else_node)
      {
        *node = else_node;
      }
      else
      {
        if(success = parse_block(arena, input, symbol_table, enclosing_block, owner, &else_node))
        {
          if(else_node)
          {
            *node = else_node;
          }
          else
          {
            if(success = parse_statement(arena, input, symbol_table, enclosing_block, &else_node))
            {
              *node = else_node;
            }
            else {
              compile_error(&input->src_loc, "Statement(s) required");
              success = false;
            }
          }
        }
      }
    }
  }
  return success;
}

bool32
parse_if_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
              AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_If)
  {
    consume_token(arena, input, symbol_table);

    if(input->token.kind == TokenKind_OpenParens)
    {
      consume_token(arena, input, symbol_table);

      AstNode* expr_node = 0;
      if(success = parse_expression(arena, input, symbol_table, enclosing_block, &expr_node))
      {
        if(input->token.kind == TokenKind_CloseParens)
        {
          consume_token(arena, input, symbol_table);

          if(expr_node)
          {
            *node = ast_new_if_stmt(arena, &input->src_loc);
            AstIfStmt* if_stmt = &(*node)->if_stmt;
            if_stmt->cond_expr = expr_node;

            AstNode* body_node = 0;
            if(success = parse_block(arena, input, symbol_table, enclosing_block, *node, &body_node))
            {
              if(body_node)
              {
                if_stmt->body = body_node;

                AstNode* else_node = 0;
                if(success = parse_else_statement(arena, input, symbol_table, enclosing_block, *node, &else_node))
                {
                  if_stmt->else_body = else_node;
                }
              }
              else
              {
                if(success = parse_statement(arena, input, symbol_table, enclosing_block, &body_node))
                {
                  if(body_node)
                  {
                    if(body_node->kind != AstNodeKind_VarDecl)
                    {
                      if_stmt->body = body_node;

                      AstNode* else_node = 0;
                      if(success = parse_else_statement(arena, input, symbol_table, enclosing_block, *node, &else_node))
                      {
                        if_stmt->else_body = else_node;
                      }
                    }
                    else {
                      compile_error(&input->src_loc, "Var statement not allowed here");
                      success = false;
                    }
                  }
                  else {
                    compile_error(&input->src_loc, "Statement(s) required");
                    success = false;
                  }
                }
              }
            }
          }
          else {
            compile_error(&input->src_loc, "Expression required");
            success = false;
          }
        }
        else {
          compile_error(&input->src_loc, "Missing ')'");
          success = false;
        }
      }
    }
    else {
      compile_error(&input->src_loc, "Missing '('");
      success = false;
    }
  }
  return success;
}

bool32
parse_procedure(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Proc)
  {
    *node = ast_new_proc(arena, &input->src_loc);
    AstProc* proc = &(*node)->proc;

    consume_token(arena, input, symbol_table);
    if(input->token.kind == TokenKind_Id)
    {
      Symbol* type_symbol = lookup_symbol(symbol_table, input->token.lexeme, SymbolKind_Type);
      if(type_symbol)
      {
        proc->ret_type = type_symbol->type;

        consume_token(arena, input, symbol_table);
        if(input->token.kind == TokenKind_Id)
        {
          Symbol* symbol = lookup_symbol(symbol_table, input->token.lexeme, SymbolKind_Proc);
          if(!symbol)
          {
            symbol = add_symbol(arena, symbol_table, input->token.lexeme, SymbolKind_Proc);
            symbol->node = *node;

            proc->name = symbol->name;
            proc->ret_var.data.size = 1;

            consume_token(arena, input, symbol_table);
            if(input->token.kind == TokenKind_OpenParens)
            {
              consume_token(arena, input, symbol_table);

              // arguments
              if(success = scope_begin(symbol_table))
              {
                AstNode* block_node = ast_new_block(arena, symbol_table, &input->src_loc);
                AstBlock* block = &block_node->block;
                proc->body = block_node;
                block->owner = *node;

                if(success = parse_formal_argument_list(arena, input, symbol_table, block, proc))
                {
                  for(ListItem* list_item = list_first_item(&proc->formal_args);
                      list_item;
                      list_item = list_item->next)
                  {
                    AstNode* arg_node = list_item->elem;
                    assert(arg_node->kind == AstNodeKind_VarDecl);
                    AstVarDecl* arg = &arg_node->var_decl;
                    arg->data.size = 1;
                  }

                  if(input->token.kind == TokenKind_CloseParens)
                  {
                    consume_token(arena, input, symbol_table);

                    if(input->token.kind == TokenKind_OpenBrace)
                    {
                      // body
                      consume_token(arena, input, symbol_table);

                      if(success = parse_statement_list(arena, input, symbol_table, block))
                      {
                        if(input->token.kind == TokenKind_CloseBrace)
                        {
                          consume_token(arena, input, symbol_table);
                          scope_end(symbol_table); // body
                        }
                        else {
                          compile_error(&input->src_loc, "Missing '}'");
                          success = false;
                        }
                      }
                    }
                    else {
                      compile_error(&input->src_loc, "Missing '{'");
                      success = false;
                    }
                  }
                  else {
                    if(input->token.kind == TokenKind_Id)
                    {
                      compile_error(&input->src_loc, "Missing 'var' keyword", input->token.lexeme);
                    }
                    else
                    {
                      compile_error(&input->src_loc, "Missing ')'");
                    }
                    success = false;
                  }
                }
              }
            }
            else {
              compile_error(&input->src_loc, "Missing '('");
              success = false;
            }
          }
          else {
            compile_error(&input->src_loc, "Redeclaration of procedure: %s", input->token.lexeme);
            success = false;
          }
        }
        else {
          compile_error(&input->src_loc, "Identifier expected");
          success = false;
        }
      }
      else {
        compile_error(&input->src_loc, "Unknown type: %s", input->token.lexeme);
        success = false;
      }
    }
    else {
      compile_error(&input->src_loc, "Identifier expected");
      success = false;
    }
  }

  return success;
}

bool32
parse_include_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                   AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Include)
  {
    consume_token(arena, input, symbol_table);

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

      consume_token(arena, input, symbol_table);
    }
    else {
      compile_error(&input->src_loc, "String required after 'include'\n");
      success = false;
    }
  }
  return success;
}

bool32
parse_module(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
             AstBlock* enclosing_block, AstModule* module)
{
  bool32 success = true;

  AstNode* node = 0;
  if(success = parse_procedure(arena, input, symbol_table, enclosing_block, &node))
  {
    if(node)
    {
      list_append(arena, &module->proc_list, node);
      success = parse_module(arena, input, symbol_table, enclosing_block, module);
    }
    else
    {
      if(success = parse_include_stmt(arena, input, symbol_table, enclosing_block, &node))
      {
        if(node)
        {
          AstIncludeStmt* inc_stmt = &node->inc_stmt;

          char* hoc_text = file_read_text(arena, inc_stmt->file_path);
          if(hoc_text)
          {
            TokenStream* inc_input = mem_push_struct(arena, TokenStream, 1);
            token_stream_init(inc_input, hoc_text, inc_stmt->file_path);

            consume_token(arena, inc_input, symbol_table);

            if(success = parse_module(arena, inc_input, symbol_table, enclosing_block, module))
            {
              if(inc_input->token.kind == TokenKind_EndOfInput)
              {
                success = parse_module(arena, input, symbol_table, enclosing_block, module);
              }
              else {
                compile_error(&inc_input->src_loc, "Unexpected token");
                success = false;
              }
            }
          }
          else {
            compile_error(&input->src_loc, "File could not be read: %s", inc_stmt->file_path);
            success = false;
          }
        }
      }
    }
  }

  return success;
}

bool32
parse_print_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                 AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Print)
  {
    consume_token(arena, input, symbol_table);

    AstNode* expr_node = 0;
    if(success = parse_expression(arena, input, symbol_table, enclosing_block, &expr_node))
    {
      *node = ast_new_print_stmt(arena, &input->src_loc);
      AstPrintStmt* print_stmt = &(*node)->print_stmt;

      if(expr_node)
      {
        print_stmt->expr = expr_node;
      }/* else {
        compile_error(input, "Expression required after 'print'");
        success = false;
      }*/

      if(input->token.kind == TokenKind_BackSlash)
      {
        consume_token(arena, input, symbol_table);

        if(input->token.kind == TokenKind_Id)
        {
          if(cstr_match("n", input->token.lexeme))
          {
            print_stmt->new_line = true;
            consume_token(arena, input, symbol_table);
          }
          else {
            compile_error(&input->src_loc, "Expected new line char '\n'");
            success = false;
          }
        }
      }
    }
  }
  return success;
}

int
block_find_owner(AstBlock* block, AstNodeKind kind, AstNode** result)
{
  int depth = 0;
  AstNode* owner = 0;
  while(block)
  {
    owner = block->owner;
    if(owner->kind == kind)
      break;
    depth++;
    block = block->enclosing_block;
  }
  *result = owner;
  return depth;
}

bool32
parse_return_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                  AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Return)
  {
    consume_token(arena, input, symbol_table);

    AstNode* ret_expr = 0;
    if(success = parse_expression(arena, input, symbol_table, enclosing_block, &ret_expr))
    {
      *node = ast_new_return_stmt(arena, &input->src_loc);
      AstReturnStmt* ret_stmt = &(*node)->ret_stmt;
      ret_stmt->ret_expr = ret_expr;

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
      else {
        compile_error(&input->src_loc, "'return' : enclosing procedure not found");
        success = false;
      }
    }
  }

  return success;
}

bool32
parse_break_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                 AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Break)
  {
    consume_token(arena, input, symbol_table);

    *node = ast_new_break_stmt(arena, &input->src_loc);
    AstBreakStmt* break_stmt = &(*node)->break_stmt;

    AstNode* owner = 0;
    int depth = block_find_owner(enclosing_block, AstNodeKind_WhileStmt, &owner);
    if(owner)
    {
      break_stmt->while_stmt = &owner->while_stmt;
      break_stmt->depth = depth + 1;
    }
    else {
      compile_error(&input->src_loc, "'break': enclosing 'while' statement not found");
      success = false;
    }
  }
  return success;
}

bool32
parse_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  typedef enum
  {
    Alt__Null,
    Alt_Var,
    Alt_Expr,
    Alt_If,
    Alt_While,
    Alt_Return,
    Alt_Break,
    Alt_Print,
    Alt_EmptyStmt,
  } Alternative;

  Alternative alt = (Alternative)1;
  AstNode* stmt_node = 0;

  while(alt) {
    switch(alt) {
      case Alt_Expr:
      {
        if(success = parse_expression(arena, input, symbol_table, enclosing_block, &stmt_node))
        {
          if(stmt_node)
          {
            alt = Alt__Null;
            if(input->token.kind == TokenKind_Semicolon)
            {
              consume_token(arena, input, symbol_table);

              if(stmt_node->kind == AstNodeKind_BinExpr)
              {
                AstBinExpr* expr = &stmt_node->bin_expr;
                if(expr->op != AstOpKind_Assign)
                {
                  compile_error(&input->src_loc, "Assignment expression required");
                  success = false;
                }
              }
              else if(stmt_node->kind != AstNodeKind_Call)
              {
                compile_error(&input->src_loc, "Expression is not a statement");
                success = false;
              }
            }
            else {
              compile_error(&input->src_loc, "Missing ';'");
              success = false;
            }
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_If:
      {
        if(success = parse_if_stmt(arena, input, symbol_table, enclosing_block, &stmt_node))
        {
          if(stmt_node)
          {
            alt = Alt__Null;
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_While:
      {
        if(success = parse_while_stmt(arena, input, symbol_table, enclosing_block, &stmt_node))
        {
          if(stmt_node)
          {
            alt = Alt__Null;
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_Return:
      {
        if(success = parse_return_stmt(arena, input, symbol_table, enclosing_block, &stmt_node))
        {
          if(stmt_node)
          {
            alt = Alt__Null;
            if(input->token.kind == TokenKind_Semicolon)
            {
              consume_token(arena, input, symbol_table);
            }
            else {
              compile_error(&input->src_loc, "Missing ';'");
              success = false;
            }
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_Break:
      {
        if(success = parse_break_stmt(arena, input, symbol_table, enclosing_block, &stmt_node))
        {
          if(stmt_node)
          {
            alt = Alt__Null;
            if(input->token.kind == TokenKind_Semicolon)
              consume_token(arena, input, symbol_table);
            else {
              compile_error(&input->src_loc, "Missing ';'");
              success = false;
            }
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_Var:
      {
        if(success = parse_var_statement(arena, input, symbol_table, enclosing_block, &stmt_node))
        {
          if(stmt_node)
          {
            alt = Alt__Null;
            if(input->token.kind == TokenKind_Semicolon)
            {
              consume_token(arena, input, symbol_table);
            }
            else {
              compile_error(&input->src_loc, "Missing ';'");
              success = false;
            }
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_Print:
      {
        if(success = parse_print_stmt(arena, input, symbol_table, enclosing_block, &stmt_node))
        {
          if(stmt_node)
          {
            alt = Alt__Null;
            if(input->token.kind == TokenKind_Semicolon)
            {
              consume_token(arena, input, symbol_table);
            }
            else {
              compile_error(&input->src_loc, "Missing ';'");
              success = false;
            }
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_EmptyStmt:
      {
        if(input->token.kind == TokenKind_Semicolon)
        {
          consume_token(arena, input, symbol_table);
          stmt_node = ast_new_empty_stmt(arena, &input->src_loc);
        } else
          alt = (Alternative)((int)alt+1);
      } break;

      default:
        alt = Alt__Null;
        break;
    }
  }

  *node = stmt_node;
  return success;
}

bool32
parse(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(success = scope_begin(symbol_table))
  {
    *node = ast_new_module(arena, &input->src_loc);
    AstModule* module = &(*node)->module;

    AstNode* block_node = ast_new_block(arena, symbol_table, &input->src_loc);
    AstBlock* block = &block_node->block;
    block->owner = *node;
    module->body = block_node;

    success = parse_module(arena, input, symbol_table, block, module);
    if(success)
    {
      scope_end(symbol_table);

      if(input->token.kind != TokenKind_EndOfInput)
      {
        compile_error(&input->src_loc, "Unexpected token: %s", input->token.lexeme);
        success = false;
      }
    }
  }

  return success;
}

