boole parse_initializer_list(TokenStream*, CstNode**);
boole parse_expression(TokenStream*, CstNode**);
boole parse_statement(TokenStream*, CstNode**);
boole parse_accessor(TokenStream*, CstNode**);
boole parse_unary_expr(TokenStream*, CstNode**);
boole parse_struct_member_list(TokenStream*, List*);
void DEBUG_print_cst_node(String* str, int indent_level, CstNode* node, char* tag);

CstNode*
new_node(SourceLocation* src_loc, CstNodeKind kind)
{
  CstNode* node = mem_push_struct(arena, CstNode);
  node->kind = kind;
  node->src_loc = *src_loc;
  return node;
}

CstNode*
new_statement(SourceLocation* src_loc, CstNode* stmt)
{
  CstNode* node = new_node(src_loc, CstNodeKind_CstStatement);
  node->stmt.stmt = stmt;
  return node;
}

CstNode*
new_block(SourceLocation* src_loc)
{
  CstNode* node = new_node(src_loc, CstNodeKind_CstBlock);
  node->block.stmts = new_list(arena, ListKind_Cst);
  return node;
}

CstNode*
new_module(SourceLocation* src_loc, char* file_path)
{
  CstNode* node = new_node(src_loc, CstNodeKind_CstModule);
  node->module.file_path = file_path;
  node->module.body = new_block(src_loc);
  return node;
}

CstNode*
new_include(SourceLocation* src_loc, char* file_path)
{
  CstNode* node = new_node(src_loc, CstNodeKind_CstInclude);
  node->module.file_path = file_path;
  node->module.body = new_block(src_loc);
  return node;
}

CstNode*
new_id(SourceLocation* src_loc, char* name)
{
  CstNode* node = new_node(src_loc, CstNodeKind_CstId);
  node->id.name = name;
  return node;
}

CstNode*
new_proc(SourceLocation* src_loc)
{
  CstNode* node = new_node(src_loc, CstNodeKind_CstProc);
  node->proc.args = new_list(arena, ListKind_Cst);
  return node;
}

CstNode*
new_call(SourceLocation* src_loc)
{
  CstNode* node = new_node(src_loc, CstNodeKind_CstCall);
  node->call.args = new_list(arena, ListKind_Cst);
  return node;
}

CstNode*
new_enum(SourceLocation* src_loc)
{
  CstNode* node = new_node(src_loc, CstNodeKind_CstEnum);
  node->enum_decl.members = new_list(arena, ListKind_Cst);
  return node;
}

CstNode*
new_struct(SourceLocation* src_loc)
{
  CstNode* node = new_node(src_loc, CstNodeKind_CstStruct);
  node->struct_decl.members = new_list(arena, ListKind_Cst);
  return node;
}

CstNode*
new_union(SourceLocation* src_loc)
{
  CstNode* node = new_node(src_loc, CstNodeKind_CstUnion);
  node->union_decl.members = new_list(arena, ListKind_Cst);
  return node;
}

CstNode*
new_initializer_list(SourceLocation* src_loc)
{
  CstNode* node = new_node(src_loc, CstNodeKind_CstInitList);
  node->init_list.members = new_list(arena, ListKind_Cst);
  return node;
}

CstNode*
new_bin_expr(SourceLocation* src_loc, CstOpKind op)
{
  CstNode* node = new_node(src_loc, CstNodeKind_CstBinExpr);
  node->bin_expr.op = op;
  return node;
}

CstNode*
new_unr_expr(SourceLocation* src_loc, CstOpKind op)
{
  CstNode* node = new_node(src_loc, CstNodeKind_CstUnaryExpr);
  node->unary_expr.op = op;
  return node;
}

char*
get_cst_op_printstr(CstOpKind op)
{
  char* result = "???";
  if(op == CstOpKind__None)
    result = "_None";
  else if(op == CstOpKind_Add)
    result = "Add";
  else if(op == CstOpKind_Sub)
    result = "Sub";
  else if(op == CstOpKind_Div)
    result = "Div";
  else if(op == CstOpKind_Mul)
    result = "Mul";
  else if(op == CstOpKind_Mod)
    result = "Mod";
  else if(op == CstOpKind_Neg)
    result = "Neg";
  else if(op == CstOpKind_Assign)
    result = "Assign";
  else if(op == CstOpKind_PointerDeref)
    result = "PointerDeref";
  else if(op == CstOpKind_AddressOf)
    result = "AddressOf";
  else if(op == CstOpKind_MemberAccess)
    result = "MemberAccess";
  else if(op == CstOpKind_PtrMemberAccess)
    result = "PtrMemberAccess";
  else if(op == CstOpKind_ArrayIndex)
    result = "ArrayIndex";
  else if(op == CstOpKind_PreDecrement)
    result = "PreDecrement";
  else if(op == CstOpKind_PostDecrement)
    result = "PostDecrement";
  else if(op == CstOpKind_PreIncrement)
    result = "PreIncrement";
  else if(op == CstOpKind_PostIncrement)
    result = "PostIncrement";
  else if(op == CstOpKind_Equals)
    result = "Equals";
  else if(op == CstOpKind_NotEquals)
    result = "NotEquals";
  else if(op == CstOpKind_Less)
    result = "Less";
  else if(op == CstOpKind_LessEquals)
    result = "LessEquals";
  else if(op == CstOpKind_Greater)
    result = "Greater";
  else if(op == CstOpKind_GreaterEquals)
    result = "GreaterEquals";
  else if(op == CstOpKind_LogicAnd)
    result = "LogicAnd";
  else if(op == CstOpKind_LogicOr)
    result = "LogicOr";
  else if(op == CstOpKind_LogicNot)
    result = "LogicNot";
  else if(op == CstOpKind_BitwiseAnd)
    result = "BitwiseAnd";
  else if(op == CstOpKind_BitwiseOr)
    result = "BitwiseOr";

  return result;
}

char*
get_cst_kind_printstr(CstNodeKind kind)
{
  char* result = "???";
  if(kind == CstNodeKind__None)
    result = "_None";
  else if(kind == CstNodeKind_CstBinExpr)
    result = "BinExpr";
  else if(kind == CstNodeKind_CstUnaryExpr)
    result = "UnrExpr";
  else if(kind == CstNodeKind_CstLiteral)
    result = "Literal";
  else if(kind == CstNodeKind_CstVarDecl)
    result = "VarDecl";
  else if(kind == CstNodeKind_CstStatement)
    result = "Statement";
  else if(kind == CstNodeKind_CstBlock)
    result = "Block";
  else if(kind == CstNodeKind_CstProc)
    result = "Proc";
  else if(kind == CstNodeKind_CstId)
    result = "Id";
  else if(kind == CstNodeKind_CstWhileStmt)
    result = "WhileStmt";
  else if(kind == CstNodeKind_CstDoWhileStmt)
    result = "DoWhileStmt";
  else if(kind == CstNodeKind_CstForStmt)
    result = "ForStmt";
  else if(kind == CstNodeKind_CstIfStmt)
    result = "IfStmt";
  else if(kind == CstNodeKind_CstReturnStmt)
    result = "ReturnStmt";
  else if(kind == CstNodeKind_CstBreakStmt)
    result = "BreakStmt";
  else if(kind == CstNodeKind_CstContinueStmt)
    result = "ContinueStmt";
  else if(kind == CstNodeKind_CstGotoStmt)
    result = "GotoStmt";
  else if(kind == CstNodeKind_CstLabel)
    result = "Label";
  else if(kind == CstNodeKind_CstInclude)
    result = "Include";
  else if(kind == CstNodeKind_CstModule)
    result = "Module";
  else if(kind == CstNodeKind_CstCast)
    result = "Cast";
  else if(kind == CstNodeKind_CstNew)
    result = "New";
  else if(kind == CstNodeKind_CstCall)
    result = "Call";
  else if(kind == CstNodeKind_CstArray)
    result = "Array";
  else if(kind == CstNodeKind_CstPointer)
    result = "Pointer";
  else if(kind == CstNodeKind_CstStruct)
    result = "Struct";
  else if(kind == CstNodeKind_CstUnion)
    result = "Union";
  else if(kind == CstNodeKind_CstEnum)
    result = "Enum";
  else if(kind == CstNodeKind_CstInitList)
    result = "InitList";
  else if(kind == CstNodeKind_CstEmptyStmt)
    result = "EmptyStmt";
  else if(kind == CstNodeKind_CstPutc)
    result = "Putc";

  return result;
}

char*
get_literal_printstr(CstLiteralKind kind)
{
  char* result = "???";
  if(kind == CstLiteralKind__None)
    result = "_None";
  else if(kind == CstLiteralKind_Int)
    result = "CstLiteralKind_Int";
  else if(kind == CstLiteralKind_Float)
    result = "CstLiteralKind_Float";
  else if(kind == CstLiteralKind_Bool)
    result = "CstLiteralKind_Bool";
  else if(kind == CstLiteralKind_String)
    result = "CstLiteralKind_String";
  else if(kind == CstLiteralKind_Char)
    result = "CstLiteralKind_Char";

  return result;
}

boole
parse_semicolon(TokenStream* input)
{
  boole success = true;
  if(input->token.kind == TokenKind_Semicolon)
    success = get_next_token(input);
  else
    success = compile_error(&input->src_loc, "expected `;`, actual `%s`", get_token_printstr(&input->token));
  return success;
}

boole
parse_initializer_member_list(TokenStream* input, List* member_list)
{
  boole success = true;

  CstNode* member = 0;
  do
  {
    member = 0;
    if(input->token.kind == TokenKind_OpenBrace)
    {
      if(success = parse_initializer_list(input, &member))
      {
        if(input->token.kind == TokenKind_Comma)
          success = get_next_token(input);
      }
    }
    if(success)
    {
      if(!member)
        success = parse_expression(input, &member);

      if(success && member)
      {
        append_list_elem(arena, member_list, member);
        if(input->token.kind == TokenKind_Comma)
          success = get_next_token(input);
      }
    }
  }
  while(success && member);
  return success;
}

boole
parse_initializer_list(TokenStream* input, CstNode** node)
{
  node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    *node = new_initializer_list(&input->src_loc);
    CstInitList* init_list = &(*node)->init_list;

    if(success = get_next_token(input) && parse_initializer_member_list(input, init_list->members))
    {
      if(input->token.kind == TokenKind_CloseBrace)
        success = get_next_token(input);
      else
        success = compile_error(&input->src_loc, "expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

boole
parse_actual_arg_list(TokenStream* input, List* args)
{
  boole success = true;

  CstNode* arg = 0;
  do
  {
    arg = 0;
    success = parse_expression(input, &arg);
    if(success && arg)
    {
      append_list_elem(arena, args, arg);
      if(input->token.kind == TokenKind_Comma)
      {
        if((success = get_next_token(input)) && input->token.kind == TokenKind_CloseParens)
          success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
      }
    }
  }
  while(success && arg);
  return success;
}

boole
parse_statement_list(TokenStream* input, List* stmt_list)
{
  boole success = true;

  CstNode* stmt = 0;
  do
  {
    stmt = 0;
    while(input->token.kind == TokenKind_Semicolon && (success = get_next_token(input)))
      ;

    if((success = parse_statement(input, &stmt)) && stmt)
    {
      append_list_elem(arena, stmt_list, stmt);
    }
  }
  while(success && stmt);

  return success;
}

boole
parse_block(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    *node = new_block(&input->src_loc);
    CstBlock* block = &(*node)->block;

    if(success = get_next_token(input) && parse_statement_list(input, block->stmts))
    {
      if(input->token.kind == TokenKind_CloseBrace)
        success = get_next_token(input);
      else
        success = compile_error(&input->src_loc, "expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

boole
parse_rest_of_id(TokenStream* input, CstNode* left_node, CstNode** node)
{
  *node = left_node;
  boole success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    // procedure call
    if(left_node->kind == CstNodeKind_CstId)
    {
      *node = new_call(&input->src_loc);
      CstCall* call = &(*node)->call;
      call->id = left_node;

      if(success = get_next_token(input) && parse_actual_arg_list(input, call->args))
      {
        if(input->token.kind == TokenKind_CloseParens)
          success = get_next_token(input);
        else
          success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
      }
    }
    else
      success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  else if(input->token.kind == TokenKind_OpenBracket)
  {
    // array
    *node = new_bin_expr(&input->src_loc, CstOpKind_ArrayIndex);
    CstBinExpr* index = &(*node)->bin_expr;

    index->left_operand = left_node;

    if(success = get_next_token(input) && parse_expression(input, &index->right_operand))
    {
      if(input->token.kind == TokenKind_CloseBracket)
      {
        if(index->right_operand)
          success = get_next_token(input) && parse_rest_of_id(input, *node, node);
        else
        {
          putback_token(input);
          success = compile_error(&input->src_loc, "[] : expression required between the brackets");
        }
      }
      else
        success = compile_error(&input->src_loc, "expected `]`, actual `%s`", get_token_printstr(&input->token));
    }
  }

  return success;
}

boole
parse_rest_of_accessor(TokenStream* input, CstNode* left_node, CstNode** node)
{
  *node = left_node;
  boole success = true;

  if(input->token.kind == TokenKind_Dot ||
     input->token.kind == TokenKind_ArrowRight)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstBinExpr);
    CstBinExpr* bin_expr = &(*node)->bin_expr;

    bin_expr->left_operand = left_node;

    if(input->token.kind == TokenKind_Dot)
      bin_expr->op = CstOpKind_MemberAccess;
    else if(input->token.kind == TokenKind_ArrowRight)
      bin_expr->op = CstOpKind_PtrMemberAccess;

    if(success = get_next_token(input) && parse_accessor(input, &bin_expr->right_operand))
    {
      if(bin_expr->right_operand)
        success = parse_rest_of_accessor(input, *node, node);
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else if(input->token.kind == TokenKind_PlusPlus ||
          input->token.kind == TokenKind_MinusMinus)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstUnaryExpr);
    CstUnaryExpr* unary_expr = &(*node)->unary_expr;

    unary_expr->operand = left_node;

    if(input->token.kind == TokenKind_MinusMinus)
    {
#if 0
      unary_expr->op = CstOpKind_PostDecrement;
#else
      success = compile_error(&input->src_loc, "`--` not supported");
#endif
    }
    else if(input->token.kind == TokenKind_PlusPlus)
    {
#if 0
      unary_expr->op = CstOpKind_PostIncrement;
#else
      success = compile_error(&input->src_loc, "`++` not supported");
#endif
    }
    else
      assert(0);

    success = get_next_token(input);
  }

  return success;
}

boole
parse_factor(TokenStream* input, CstNode** node)
{
  boole success = true;

  if((success = parse_unary_expr(input, node)) && *node)
    success = parse_rest_of_accessor(input, *node, node);
  return success;
}

boole
parse_rest_of_factor(TokenStream* input, CstNode* left_node, CstNode** node)
{
  *node = left_node;
  boole success = true;

  if(input->token.kind == TokenKind_Star ||
     input->token.kind == TokenKind_FwdSlash ||
     input->token.kind == TokenKind_Percent)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstBinExpr);
    CstBinExpr* bin_expr = &(*node)->bin_expr;

    bin_expr->left_operand = left_node;

    if(input->token.kind == TokenKind_Star)
      bin_expr->op = CstOpKind_Mul;
    else if(input->token.kind == TokenKind_FwdSlash)
      bin_expr->op = CstOpKind_Div;
    else if(input->token.kind == TokenKind_Percent)
      bin_expr->op = CstOpKind_Mod;
    else
      assert(0);

    if(success = get_next_token(input) && parse_factor(input, &bin_expr->right_operand))
    {
      if(bin_expr->right_operand)
        success = parse_rest_of_factor(input, *node, node);
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }

  return success;
}

boole
parse_term(TokenStream* input, CstNode** node)
{
  boole success = true;

  if((success = parse_factor(input, node)) && *node)
    success = parse_rest_of_factor(input, *node, node);
  return success;
}

boole
parse_rest_of_term(TokenStream* input, CstNode* left_node, CstNode** node)
{
  *node = left_node;
  boole success = true;

  if(input->token.kind == TokenKind_Plus ||
     input->token.kind == TokenKind_Minus ||
     input->token.kind == TokenKind_Pipe ||
     input->token.kind == TokenKind_PipePipe ||
     input->token.kind == TokenKind_Ampersand ||
     input->token.kind == TokenKind_AmpersandAmpersand)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstBinExpr);
    CstBinExpr* bin_expr = &(*node)->bin_expr;

    bin_expr->left_operand = left_node;

    if(input->token.kind == TokenKind_Plus)
      bin_expr->op = CstOpKind_Add;
    else if(input->token.kind == TokenKind_Minus)
      bin_expr->op = CstOpKind_Sub;
    else if(input->token.kind == TokenKind_Pipe)
      bin_expr->op = CstOpKind_BitwiseOr;
    else if(input->token.kind == TokenKind_PipePipe)
      bin_expr->op = CstOpKind_LogicOr;
    else if(input->token.kind == TokenKind_Ampersand)
      bin_expr->op = CstOpKind_BitwiseAnd;
    else if(input->token.kind == TokenKind_AmpersandAmpersand)
      bin_expr->op = CstOpKind_LogicAnd;
    else
      assert(0);

    if(success = get_next_token(input) && parse_term(input, &bin_expr->right_operand))
    {
      if(bin_expr->right_operand)
        success = parse_rest_of_term(input, *node, node);
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }

  return success;
}

boole
parse_assignment(TokenStream* input, CstNode** node)
{
  boole success = true;

  if((success = parse_term(input, node)) && *node)
    success = parse_rest_of_term(input, *node, node);
  return success;
}

boole
parse_rest_of_assignment(TokenStream* input, CstNode* left_node, CstNode** node)
{
  *node = left_node;
  boole success = true;

  if(input->token.kind == TokenKind_Equals ||
     input->token.kind == TokenKind_EqualsEquals ||
     input->token.kind == TokenKind_ExclamEquals ||
     input->token.kind == TokenKind_AngleLeft ||
     input->token.kind == TokenKind_AngleLeftEquals ||
     input->token.kind == TokenKind_AngleRight ||
     input->token.kind == TokenKind_AngleRightEquals)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstBinExpr);
    CstBinExpr* bin_expr = &(*node)->bin_expr;

    bin_expr->left_operand = left_node;

    if(input->token.kind == TokenKind_Equals)
      bin_expr->op = CstOpKind_Assign;
    else if(input->token.kind == TokenKind_EqualsEquals)
      bin_expr->op = CstOpKind_Equals;
    else if(input->token.kind == TokenKind_ExclamEquals)
      bin_expr->op = CstOpKind_NotEquals;
    else if(input->token.kind == TokenKind_AngleLeft)
      bin_expr->op = CstOpKind_Less;
    else if(input->token.kind == TokenKind_AngleLeftEquals)
      bin_expr->op = CstOpKind_LessEquals;
    else if(input->token.kind == TokenKind_AngleRight)
      bin_expr->op = CstOpKind_Greater;
    else if(input->token.kind == TokenKind_AngleRightEquals)
      bin_expr->op = CstOpKind_GreaterEquals;

    if(success = get_next_token(input) && parse_expression(input, &bin_expr->right_operand))
    {
      if(!bin_expr->right_operand)
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }

  return success;
}

boole
parse_type_expr_pointer(TokenStream* input, CstNode* expr, CstNode** node)
{
  *node = expr;
  boole success = true;
  
  if(input->token.kind == TokenKind_Star)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstPointer);
    CstPointer* ptr = &(*node)->pointer;

    ptr->type_expr = expr;

    success = get_next_token(input) && parse_type_expr_pointer(input, *node, node);
  }
  return true;
}

boole
parse_type_expr_id(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_Id)
  {
    *node = new_id(&input->src_loc, input->token.lexeme);

    success = get_next_token(input) && parse_type_expr_pointer(input, *node, node);
  }
  return success;
}

boole
parse_type_expr(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_OpenBracket)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstArray);
    CstArray* array = &(*node)->array;

    if(success = get_next_token(input) && parse_expression(input, &array->size_expr))
    {
      if(input->token.kind == TokenKind_CloseBracket)
      {
        if(array->size_expr)
        {
          if(success = get_next_token(input) && parse_type_expr(input, &array->type_expr))
          {
            if(!array->type_expr)
            {
              putback_token(input);
              success = compile_error(&input->src_loc, "incomplete type expression, at `%s`", get_token_printstr(&input->token));
            }
          }
        }
        else
          success = compile_error(&input->src_loc, "[] : expression required between brackets");
      }
      else
        success = compile_error(&input->src_loc,  "expected `]`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  else
    success = parse_type_expr_id(input, node);

  return success;
}

boole
parse_new(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = false;

  if(input->token.kind == TokenKind_New && (success = get_next_token(input)))
  {
    if(input->token.kind == TokenKind_OpenParens)
    {
      *node = new_node(&input->src_loc, CstNodeKind_CstNew);
      CstNew* new = &(*node)->new;

      if(success = get_next_token(input) && parse_type_expr(input, &new->type_expr))
      {
#if 1
        if(input->token.kind == TokenKind_Comma)
        {
          if(success = get_next_token(input) && parse_expression(input, &new->count_expr))
          {
            if(input->token.kind == TokenKind_CloseParens)
            {
              if(new->count_expr)
                success = get_next_token(input);
              else
              {
                putback_token(input);
                success = compile_error(&input->src_loc, "expression required, at `%s`", get_token_printstr(&input->token));
              }
            }
            else
              success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
          }
        }
        else
          success = compile_error(&input->src_loc,
                                  "expected `,`, actual `%s`", get_token_printstr(&input->token));
#else
        if(input->token.kind == TokenKind_CloseParens)
        {
          if(new_cst->type_expr)
            success = get_next_token(input);
          else
          {
            putback_token(input);
            success = compile_error(&input->src_loc, "type expression required, at `%s`", get_token_printstr(&input->token));
          }
        }
        else
          success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
#endif
      }
    }
    else
      success = compile_error(&input->src_loc, "expected `(`, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

boole
parse_putc(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = false;

  if(input->token.kind == TokenKind_Putc && (success = get_next_token(input)))
  {
    if(input->token.kind == TokenKind_OpenParens)
    {
      *node = new_node(&input->src_loc, CstNodeKind_CstPutc);
      CstPutc* putc = &(*node)->putc;

      if(success = get_next_token(input) && parse_expression(input, &putc->expr))
      {
        if(input->token.kind == TokenKind_CloseParens)
        {
          if(putc->expr)
            success = get_next_token(input);
          else
          {
            putback_token(input);
            success = compile_error(&input->src_loc, "expression required, at `%s`", get_token_printstr(&input->token));
          }
        }
        else
          success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
      }
    }
    else
      success = compile_error(&input->src_loc, "expected `(`, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

boole
parse_accessor(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    if(success = get_next_token(input) && parse_expression(input, node))
    {
      if(*node)
      {
        if(input->token.kind == TokenKind_CloseParens)
          success = get_next_token(input);
        else
          success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
      }
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "expression expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else if(is_literal_token(input->token.kind) ||
          input->token.kind == TokenKind_True ||
          input->token.kind == TokenKind_False)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstLiteral);
    CstLiteral* lit = &(*node)->lit;

    if(input->token.kind == TokenKind_IntNum)
    {
      lit->kind = CstLiteralKind_Int;
      lit->int_val = *input->token.int_val;
    }
    else if(input->token.kind == TokenKind_FloatNum)
    {
      lit->kind = CstLiteralKind_Float;
      lit->float_val = *input->token.float_val;
    }
    else if(input->token.kind == TokenKind_True ||
            input->token.kind == TokenKind_False)
    {
      lit->kind = CstLiteralKind_Bool;
      lit->bool_val = (input->token.kind == TokenKind_True ? 1 : 0);
    }
    else if(input->token.kind == TokenKind_Char)
    {
      lit->kind = CstLiteralKind_Char;
      lit->char_val = input->token.char_val;
    }
    else if(input->token.kind == TokenKind_String)
    {
      lit->kind = CstLiteralKind_String;
      lit->str = input->token.str;
    }
    else
      assert(0);

    success = get_next_token(input);
  }
  else if(input->token.kind == TokenKind_Id)
  {
    *node = new_id(&input->src_loc, input->token.lexeme);
    success = get_next_token(input) && parse_rest_of_id(input, *node, node);
  }
  else if(input->token.kind == TokenKind_New)
    success = parse_new(input, node);
  else if(input->token.kind == TokenKind_Putc)
    success = parse_putc(input, node);

  return success;
}

boole
parse_formal_arg(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  CstNode* type = 0;
  if((success = parse_type_expr(input, &type)) && type)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstVarDecl);
    CstVarDecl* var_decl = &(*node)->var_decl;

    var_decl->type_expr = type;

    if(input->token.kind == TokenKind_Id)
    {
      var_decl->id = new_id(&input->src_loc, input->token.lexeme);
      success = get_next_token(input);
    }
    else
      success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

boole
parse_formal_arg_list(TokenStream* input, List* arg_list)
{
  boole success = true;

  CstNode* arg = 0;
  do
  {
    arg = 0;
    if((success = parse_formal_arg(input, &arg)) && arg)
    {
      append_list_elem(arena, arg_list, arg);
      if(input->token.kind == TokenKind_Comma)
      {
        success = get_next_token(input);
      }
      else if(input->token.kind != TokenKind_CloseParens)
      {
        success = compile_error(&input->src_loc, "expected `,`, actual `%s`", get_token_printstr(&input->token));
      }
    }
  }
  while(success && arg);
  return success;
}

boole
parse_unary_expr(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_Exclam ||
     input->token.kind == TokenKind_Star ||
     input->token.kind == TokenKind_Ampersand ||
     input->token.kind == TokenKind_Minus ||
     input->token.kind == TokenKind_MinusMinus ||
     input->token.kind == TokenKind_PlusPlus)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstUnaryExpr);
    CstUnaryExpr* unary_expr = &(*node)->unary_expr;

    if(input->token.kind == TokenKind_Exclam)
      unary_expr->op = CstOpKind_LogicNot;
    else if(input->token.kind == TokenKind_Star)
      unary_expr->op = CstOpKind_PointerDeref;
    else if(input->token.kind == TokenKind_Ampersand)
      unary_expr->op = CstOpKind_AddressOf;
    else if(input->token.kind == TokenKind_Minus)
      unary_expr->op = CstOpKind_Neg;
    else if(input->token.kind == TokenKind_MinusMinus)
    {
#if 0
      unary_expr->op = CstOpKind_PreDecrement;
#else
      success = compile_error(&input->src_loc, "`--` not supported");
#endif
    }
    else if(input->token.kind == TokenKind_PlusPlus)
    {
#if 0
      unary_expr->op = CstOpKind_PreIncrement;
#else
      success = compile_error(&input->src_loc, "`++` not supported");
#endif
    }
    else
      assert(0);

    if(success && (success = get_next_token(input)) && parse_factor(input, &unary_expr->operand))
    {
      if(!unary_expr->operand)
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else if(input->token.kind == TokenKind_Cast)
  {
    // cast
    *node = new_node(&input->src_loc, CstNodeKind_CstCast);
    CstCast* cast = &(*node)->cast;

    if(success = get_next_token(input))
    {
      if(input->token.kind == TokenKind_OpenParens)
      {
        if(success = get_next_token(input) && parse_type_expr(input, &cast->type_expr))
        {
          if(input->token.kind == TokenKind_CloseParens)
          {
            if(cast->type_expr)
              success = get_next_token(input) && parse_unary_expr(input, &cast->expr);
            else
            {
              putback_token(input);
              success = compile_error(&input->src_loc, "type expression required, at `%s`", get_token_printstr(&input->token));
            }
          }
          else
            success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
        }
      }
      else
        success = compile_error(&input->src_loc, "expected `(`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  else
    success = parse_accessor(input, node);

  return success;
}

boole
parse_expression(TokenStream* input, CstNode** node)
{
  boole success = true;

  if((success = parse_assignment(input, node)) && *node)
    success = parse_rest_of_assignment(input, *node, node);
  return success;
}

boole
parse_var_decl(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  CstNode* type = 0;
  if((success = parse_type_expr(input, &type)) && type)
  {
    if(input->token.kind == TokenKind_Id)
    {
      *node = new_node(&input->src_loc, CstNodeKind_CstVarDecl);
      CstVarDecl* var_decl = &(*node)->var_decl;

      var_decl->type_expr = type;
      var_decl->id = new_id(&input->src_loc, input->token.lexeme);

      if((success = get_next_token(input)) && input->token.kind == TokenKind_Equals
          && (success = get_next_token(input)))
      {
        if(success = parse_initializer_list(input, &var_decl->init_expr))
        {
          if(!var_decl->init_expr)
          {
            if(success = parse_expression(input, &var_decl->init_expr))
            {
              if(!var_decl->init_expr)
              {
                putback_token(input);
                success = compile_error(&input->src_loc, "expression required, at `%s`", get_token_printstr(&input->token));
              }
            }
          }
        }
      }
    }
    else
      success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

boole
parse_for_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_For)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstForStmt);
    CstForStmt* for_stmt = &(*node)->for_stmt;

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == TokenKind_OpenParens)
    {
      success = get_next_token(input) && parse_var_decl(input, &for_stmt->decl_expr)
        && parse_semicolon(input)
        && parse_expression(input, &for_stmt->cond_expr)
        && parse_semicolon(input)
        && parse_expression(input, &for_stmt->loop_expr);
      if(!success)
        return success;

      if(input->token.kind == TokenKind_CloseParens)
      {
        if(success = get_next_token(input) && parse_block(input, &for_stmt->body))
        {
          if(!for_stmt->body)
          {
            if(success = parse_statement(input, &for_stmt->body))
            {
              if(for_stmt->body)
              {
                CstNode* single_stmt = for_stmt->body;
                for_stmt->body = new_block(&input->src_loc);
                CstBlock* block = &for_stmt->body->block;
                append_list_elem(arena, block->stmts, single_stmt);
              }
              else
              {
                putback_token(input);
                success = compile_error(&input->src_loc, "statement required, at `%s`", get_token_printstr(&input->token));
              }
            }
          }
        }
      }
      else
        success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
    }
    else
      success = compile_error(&input->src_loc, "expected `(`, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

boole
parse_while_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_While)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstWhileStmt);
    CstWhileStmt* while_stmt = &(*node)->while_stmt;

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == TokenKind_OpenParens)
    {
      success = get_next_token(input) && parse_expression(input, &while_stmt->cond_expr);
      if(!success)
        return success;

      if(while_stmt->cond_expr)
      {
        if(input->token.kind == TokenKind_CloseParens)
        {
          if(!(success = get_next_token(input)))
            return success;

          if(!(success = parse_block(input, &while_stmt->body)))
            return success;

          if(!while_stmt->body)
          {
            if((success = parse_statement(input, &while_stmt->body)) && !while_stmt->body)
            {
              putback_token(input);
              success = compile_error(&input->src_loc, "statement required, at `%s`", get_token_printstr(&input->token));
            }
          }
        }
        else
          success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
      }
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "expression required, at `%s`", get_token_printstr(&input->token));
      }
    }
    else
      success = compile_error(&input->src_loc, "expected `(`, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

boole
parse_else_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_Else)
  {
    if(success = get_next_token(input) && parse_block(input, node))
    {
      if(!*node)
      {
        if((success = parse_statement(input, node)) && !*node)
        {
          putback_token(input);
          success = compile_error(&input->src_loc, "statement required, at `%s`", get_token_printstr(&input->token));
        }
      }
    }
  }
  return success;
}

boole
parse_if_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_If)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstIfStmt);
    CstIfStmt* if_stmt = &(*node)->if_stmt;

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == TokenKind_OpenParens)
    {
      success = get_next_token(input) && parse_expression(input, &if_stmt->cond_expr);
      if(!success)
        return success;

      if(input->token.kind == TokenKind_CloseParens)
      {
        if(if_stmt->cond_expr)
        {
          success = get_next_token(input) && parse_block(input, &if_stmt->body);
          if(!success)
            return success;

          if(!if_stmt->body)
            success = parse_statement(input, &if_stmt->body);

          if(success)
          {
            if(if_stmt->body)
              success = parse_else_stmt(input, &if_stmt->else_body);
            else
            {
              putback_token(input);
              success = compile_error(&input->src_loc, "statement required, at `%s`", get_token_printstr(&input->token));
            }
          }
        }
        else
        {
          putback_token(input);
          success = compile_error(&input->src_loc, "expression required, at `%s`", get_token_printstr(&input->token));
        }
      }
      else
        success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
    }
    else
      success = compile_error(&input->src_loc, "expected `(`, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

boole
parse_proc_decl(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_Proc)
  {
    *node = new_proc(&input->src_loc);
    CstProc* proc = &(*node)->proc;

    if(success = get_next_token(input) && parse_type_expr(input, &proc->ret_type_expr))
    {
      if(proc->ret_type_expr)
      {
        if(input->token.kind == TokenKind_Id)
        {
          proc->id = new_id(&input->src_loc, input->token.lexeme);

          if(!(success = get_next_token(input)))
            return success;

          if(input->token.kind == TokenKind_OpenParens)
          {
            if(!(success = get_next_token(input)))
              return success;

            if(success = parse_formal_arg_list(input, proc->args))
            {
              if(input->token.kind == TokenKind_CloseParens)
              {
                if(success = get_next_token(input) && parse_block(input, &proc->body))
                {
                  if(!proc->body && (proc->is_decl = success = parse_semicolon(input)))
                    proc->body = (CstNode*)new_block(&(*node)->src_loc);
                }
              }
              else
                success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
            }
          }
          else
            success = compile_error(&input->src_loc, "expected `(`, actual `%s`", get_token_printstr(&input->token));
        }
        else
          success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
      }
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "type expression required, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  return success;
}

boole
parse_include_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_Include)
  {
    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == TokenKind_String)
    {
      *node = new_include(&input->src_loc, input->src_loc.file_path);
      CstInclude* include = &(*node)->include;

      String* str = str_new(arena);
      str_append(str, input->src_loc.file_path);
      path_make_dir(str->head);
      str_tidyup(str);
      str_append(str, input->token.str);
      include->file_path = str_cap(str);

      if(!(success = get_next_token(input)))
        return success;

      char* hoc_text = file_read_text(arena, include->file_path);
      if(hoc_text)
      {
        TokenStream* incl_input = mem_push_struct(arena, TokenStream);
        init_token_stream(incl_input, hoc_text, include->file_path);

        if(success = get_next_token(incl_input))
        {
          CstBlock* block = &include->body->block;
          success = parse_statement_list(incl_input, block->stmts);
        }
      }
      else
        success = compile_error(&input->src_loc, "could not read file `%s`", include->file_path);
    }
    else
      success = compile_error(&input->src_loc, "string expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

boole
parse_enum_decl(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_Enum)
  {
    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == TokenKind_Id)
    {
      *node = new_enum(&input->src_loc);
      CstEnum* enum_decl = &(*node)->enum_decl;

      enum_decl->id = new_id(&input->src_loc, input->token.lexeme);

      if(!(success = get_next_token(input)))
        return success;

      if(input->token.kind == TokenKind_OpenBrace)
      {
        if(!(success = get_next_token(input)))
          return success;

        CstNode* member = 0;
        do
        {
          member = 0;
          if(input->token.kind == TokenKind_Id)
          {
            member = (CstNode*)new_id(&input->src_loc, input->token.lexeme);
            append_list_elem(arena, enum_decl->members, member);

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
          success = compile_error(&input->src_loc, "expected `}`, actual `%s`", get_token_printstr(&input->token));
      }
      else
        success = compile_error(&input->src_loc, "expected `{`, actual `%s`", get_token_printstr(&input->token));
    }
    else
      success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

boole
parse_union_decl(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_Union)
  {
    *node = new_union(&input->src_loc);
    CstUnion* union_decl = &(*node)->union_decl;

    if((success = get_next_token(input)) && input->token.kind == TokenKind_Id)
    {
      union_decl->id = new_id(&input->src_loc, input->token.lexeme);
      success = get_next_token(input);
    }

    if(success)
      success = parse_struct_member_list(input, union_decl->members);
  }
  return success;
}

boole
parse_struct_decl(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_Struct)
  {
    *node = new_struct(&input->src_loc);
    CstStruct* struct_decl = &(*node)->struct_decl;

    if((success = get_next_token(input)) && input->token.kind == TokenKind_Id)
    {
      struct_decl->id = new_id(&input->src_loc, input->token.lexeme);
      success = get_next_token(input);
    }

    if(success)
      success = parse_struct_member_list(input, struct_decl->members);
  }
  return success;
}

boole
parse_struct_member_list(TokenStream* input, List* member_list)
{
  boole success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    if(!(success = get_next_token(input)))
      return success;

    CstNode* member = 0;
    do
    {
      member = 0;

      CstNode* type = 0;
      if(success = parse_type_expr(input, &type))
      {
        if(!type)
        {
          if(input->token.kind == TokenKind_Union)
            success = parse_union_decl(input, &type);
          else if(input->token.kind == TokenKind_Struct)
            success = parse_struct_decl(input, &type);
        }

        if(success && type)
        {
          member = new_node(&input->src_loc, CstNodeKind_CstVarDecl);
          CstVarDecl* var_decl = &member->var_decl;

          var_decl->type_expr = type;

          if(input->token.kind == TokenKind_Id)
          {
            var_decl->id = new_id(&input->src_loc, input->token.lexeme);
            success = get_next_token(input);
          }
          else if(type->kind == CstNodeKind_CstStruct ||
                  type->kind == CstNodeKind_CstUnion)
          {
            /* anonymous struct/union */
          }
          else
            success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));

          if(success)
          {
            append_list_elem(arena, member_list, member);
            success = parse_semicolon(input);
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
        success = compile_error(&input->src_loc, "expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  else
    success = compile_error(&input->src_loc, "expected `{`, actual `%s`", get_token_printstr(&input->token));
  return success;
}

boole
parse_var_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = false;

  if(input->token.kind == TokenKind_Var)
    success = get_next_token(input) && parse_var_decl(input, node);
  return success;
}

boole
parse_return_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_Return)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstReturnStmt);
    CstReturnStmt* ret_stmt = &(*node)->ret_stmt;
    success = get_next_token(input) && parse_expression(input, &ret_stmt->expr);
  }

  return success;
}

boole
parse_label(TokenStream* input, CstNode* id, CstNode** node)
{
  *node = id;
  boole success = true;

  if(input->token.kind == TokenKind_Colon && (success = get_next_token(input)))
  {
    if(id->kind == CstNodeKind_CstId)
    {
      *node = new_node(&input->src_loc, CstNodeKind_CstLabel);
      CstLabel* label = &(*node)->label;
      label->id = id;
    }
    else
      success = compile_error(&input->src_loc, "label identifier expected");
  }
  return success;
}

boole
parse_goto_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_Goto)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstGotoStmt);
    CstGotoStmt* goto_stmt = &(*node)->goto_stmt;

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == TokenKind_Id)
    {
      goto_stmt->id = new_id(&input->src_loc, input->token.lexeme);
      success = get_next_token(input);
    }
    else
      success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

boole
parse_continue_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_Continue)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstContinueStmt);
    success = get_next_token(input);
  }
  return success;
}

boole
parse_break_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  if(input->token.kind == TokenKind_Break)
  {
    *node = new_node(&input->src_loc, CstNodeKind_CstBreakStmt);
    success = get_next_token(input);
  }
  return success;
}

boole
parse_statement(TokenStream* input, CstNode** node)
{
  *node = 0;
  boole success = true;

  CstNode* stmt = 0;
  if(input->token.kind == TokenKind_Var)
  {
    success = parse_var_stmt(input, &stmt) && parse_semicolon(input);
  }
  else if(input->token.kind == TokenKind_Include)
  {
    success = parse_include_stmt(input, &stmt) && parse_semicolon(input);
  }
  else if(input->token.kind == TokenKind_Proc)
  {
    success = parse_proc_decl(input, &stmt);
  }
  else if(input->token.kind == TokenKind_If)
  {
    success = parse_if_stmt(input, &stmt);
  }
  else if(input->token.kind == TokenKind_Else)
  {
    success = compile_error(&input->src_loc, "unmatched `else`");
  }
  else if(input->token.kind == TokenKind_While)
  {
    success = parse_while_stmt(input, &stmt);
  }
#if 0
  else if(input->token.kind == TokenKind_For)
  {
    success = parse_for_stmt(input, &stmt);
  }
#endif
  else if(input->token.kind == TokenKind_Return)
  {
    success = parse_return_stmt(input, &stmt) && parse_semicolon(input);
  }
  else if(input->token.kind == TokenKind_Break)
  {
    success = parse_break_stmt(input, &stmt) && parse_semicolon(input);
  }
  else if(input->token.kind == TokenKind_Continue)
  {
    success = parse_continue_stmt(input, &stmt) && parse_semicolon(input);
  }
#if 0
  else if(input->token.kind == TokenKind_Goto)
  {
    success = parse_goto_stmt(input, &stmt) && parse_semicolon(input);
  }
#endif
  else if(input->token.kind == TokenKind_Semicolon)
  {
    success = get_next_token(input);
  }
  else if(input->token.kind == TokenKind_OpenBrace)
  {
    success = parse_block(input, &stmt);
  }
  else
  {
    if(success = parse_expression(input, &stmt))
    {
      if(stmt)
      {
#if 0
        if(input->token.kind == TokenKind_Colon)
          success = parse_label(input, &stmt, stmt);
        else
          success = parse_semicolon(input);
#else
        success = parse_semicolon(input);
#endif
      }
    }
  }

  if(success && stmt)
  {
    *node = new_statement(&input->src_loc, stmt);
  }
  return success;
}

boole
parse(TokenStream* input, CstNode** node)
{
  boole success = true;

  *node = new_module(&input->src_loc, input->src_loc.file_path);
  CstModule* module = &(*node)->module;

  deflt_src_loc = &(*node)->src_loc;

  CstBlock* block = &module->body->block;
  if((success = parse_statement_list(input, block->stmts))
     && input->token.kind != TokenKind_EndOfInput)
  {
    success = compile_error(&input->src_loc, "expected `end-of-input`, at `%s`", get_token_printstr(&input->token));
  }
  return success;
}

void
DEBUG_print_line(String* str, int indent_level, char* message, ...)
{
  for(int i = 0; i < indent_level; i++)
  {
    str_append(str, "  ");
  }

  va_list varargs;
  va_start(varargs, message);
  str_printf_va(str, message, varargs);
  va_end(varargs);

  str_append(str, "\n");
}

void
DEBUG_print_cst_node_list(String* str, int indent_level, List* node_list, char* tag)
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
      CstNode* node = (CstNode*)list_item->elem;
      DEBUG_print_cst_node(str, indent_level, node, 0);
    }
  }
}

void
DEBUG_print_cst_node(String* str, int indent_level, CstNode* node, char* tag)
{
  if(node)
  {
    if(tag)
    {
      DEBUG_print_line(str, indent_level, tag);
      ++indent_level;
    }
#if 1
    DEBUG_print_line(str, indent_level, "%s src_line=\"%s:%d\"",
                     get_cst_kind_printstr(node->kind), node->src_loc.file_path, node->src_loc.line_nr);
#else
    DEBUG_print_line(str, indent_level, "%s", get_cst_kind_printstr(node->kind));
#endif
    ++indent_level;

    if(node->kind == CstNodeKind_CstModule)
    {
      CstModule* module = &node->module;
      DEBUG_print_line(str, indent_level, "file_path: \"%s\"", module->file_path);
      DEBUG_print_cst_node(str, indent_level, module->body, "body");
    }
    else if(node->kind == CstNodeKind_CstInclude)
    {
      CstInclude* include = &node->include;
      DEBUG_print_line(str, indent_level, "file_path: \"%s\"", include->file_path);
      DEBUG_print_cst_node(str, indent_level, include->body, "body");
    }
    else if(node->kind == CstNodeKind_CstProc)
    {
      CstProc* proc = &node->proc;
      DEBUG_print_cst_node(str, indent_level, proc->ret_type_expr, "ret_type");
      DEBUG_print_cst_node(str, indent_level, proc->id, "id");
      DEBUG_print_cst_node_list(str, indent_level, proc->args, "args");
      DEBUG_print_cst_node(str, indent_level, proc->body, "body");
    }
    else if(node->kind == CstNodeKind_CstVarDecl)
    {
      CstVarDecl* var_decl = &node->var_decl;
      DEBUG_print_cst_node(str, indent_level, var_decl->type_expr, "type_id");
      DEBUG_print_cst_node(str, indent_level, (CstNode*)var_decl->id, "id");
      DEBUG_print_cst_node(str, indent_level, var_decl->init_expr, "init_expr");
    }
#if 0
    else if(node->kind == CstNodeKind_CstVarOccur)
    {
      CstVarOccur* var_occur = (CstVarOccur*)node;
      DEBUG_print_cst_node(str, indent_level, var_occur->id, "id");
      DEBUG_print_line(str, indent_level, "decl_block_offset: %d", var_occur->decl_block_offset);
    }
#endif
    else if(node->kind == CstNodeKind_CstId)
    {
      CstId* id = &node->id;
      DEBUG_print_line(str, indent_level, "name: %s", id->name);
    }
    else if(node->kind == CstNodeKind_CstBlock)
    {
      CstBlock* block = &node->block;
      DEBUG_print_cst_node_list(str, indent_level, block->stmts, "stmts");
    }
    else if(node->kind == CstNodeKind_CstBinExpr)
    {
      CstBinExpr* bin_expr = &node->bin_expr;
      DEBUG_print_line(str, indent_level, "op: %s", get_cst_op_printstr(bin_expr->op));
      DEBUG_print_cst_node(str, indent_level, bin_expr->left_operand, "left_operand");
      DEBUG_print_cst_node(str, indent_level, bin_expr->right_operand, "right_operand");
    }
    else if(node->kind == CstNodeKind_CstUnaryExpr)
    {
      CstUnaryExpr* unary_expr = &node->unary_expr;
      DEBUG_print_line(str, indent_level, "op: %s", get_cst_op_printstr(unary_expr->op));
      DEBUG_print_cst_node(str, indent_level, unary_expr->operand, "operand");
    }
    else if(node->kind == CstNodeKind_CstStatement)
    {
      CstStatement* stmt = &node->stmt;
      DEBUG_print_cst_node(str, indent_level, stmt->stmt, "stmt");
    }
    else if(node->kind == CstNodeKind_CstIfStmt)
    {
      CstIfStmt* if_stmt = &node->if_stmt;
      DEBUG_print_cst_node(str, indent_level, if_stmt->cond_expr, "cond_expr");
      DEBUG_print_cst_node(str, indent_level, if_stmt->body, "body");
      DEBUG_print_cst_node(str, indent_level, if_stmt->else_body, "else_body");
    }
    else if(node->kind == CstNodeKind_CstReturnStmt)
    {
      CstReturnStmt* ret_stmt = &node->ret_stmt;
      DEBUG_print_cst_node(str, indent_level, ret_stmt->expr, "expr");
    }
    else if(node->kind == CstNodeKind_CstLiteral)
    {
      CstLiteral* lit = &node->lit;
      DEBUG_print_line(str, indent_level, get_literal_printstr(lit->kind));
      if(lit->kind == CstLiteralKind_Int)
        DEBUG_print_line(str, indent_level, "int_val: %d", lit->int_val);
      else if(lit->kind == CstLiteralKind_Float)
        DEBUG_print_line(str, indent_level, "float_val: %f", lit->float_val);
      else if(lit->kind == CstLiteralKind_Bool)
        DEBUG_print_line(str, indent_level, "bool_val: %d", lit->bool_val);
      else if(lit->kind == CstLiteralKind_Char)
      {
        char buf[3] = {0};
        print_char(buf, lit->char_val);
        DEBUG_print_line(str, indent_level, "char_val: '%s'", buf);
      }
      else if(lit->kind == CstLiteralKind_String)
        DEBUG_print_line(str, indent_level, "str: \"%s\"", lit->str);
      else
        assert(0);
    }
    else if(node->kind == CstNodeKind_CstWhileStmt)
    {
      CstWhileStmt* while_stmt = &node->while_stmt;
      DEBUG_print_cst_node(str, indent_level, while_stmt->cond_expr, "cond_expr");
      DEBUG_print_cst_node(str, indent_level, while_stmt->body, "body");
    }
    else if(node->kind == CstNodeKind_CstForStmt)
    {
      CstForStmt* for_stmt = &node->for_stmt;
      DEBUG_print_cst_node(str, indent_level, for_stmt->decl_expr, "decl_expr");
      DEBUG_print_cst_node(str, indent_level, for_stmt->cond_expr, "cond_expr");
      DEBUG_print_cst_node(str, indent_level, for_stmt->loop_expr, "loop_expr");
      DEBUG_print_cst_node(str, indent_level, for_stmt->body, "body");
    }
    else if(node->kind == CstNodeKind_CstCast)
    {
      CstCast* cast = &node->cast;
      DEBUG_print_cst_node(str, indent_level, cast->type_expr, "type_expr");
      DEBUG_print_cst_node(str, indent_level, cast->expr, "expr");
    }
    else if(node->kind == CstNodeKind_CstArray)
    {
      CstArray* array = &node->array;
      DEBUG_print_cst_node(str, indent_level, array->type_expr, "type_expr");
      DEBUG_print_cst_node(str, indent_level, array->size_expr, "size_expr");
    }
    else if(node->kind == CstNodeKind_CstPointer)
    {
      CstPointer* ptr = &node->pointer;
      DEBUG_print_cst_node(str, indent_level, ptr->type_expr, "type_expr");
    }
    else if(node->kind == CstNodeKind_CstCall)
    {
      CstCall* call = &node->call;
      DEBUG_print_cst_node(str, indent_level, call->id, "id");
      DEBUG_print_cst_node_list(str, indent_level, call->args, "args");
    }
    else if(node->kind == CstNodeKind_CstBreakStmt
            || node->kind == CstNodeKind_CstContinueStmt
            || node->kind == CstNodeKind_CstEmptyStmt)
    {
      /* no extra info to print */
    }
    else if(node->kind == CstNodeKind_CstStruct)
    {
      CstStruct* struct_decl = &node->struct_decl;
      DEBUG_print_cst_node(str, indent_level, struct_decl->id, "id");
      DEBUG_print_cst_node_list(str, indent_level, struct_decl->members, "members");
    }
    else if(node->kind == CstNodeKind_CstUnion)
    {
      CstUnion* union_decl = &node->union_decl;
      DEBUG_print_cst_node(str, indent_level, union_decl->id, "id");
      DEBUG_print_cst_node_list(str, indent_level, union_decl->members, "members");
    }
    else if(node->kind == CstNodeKind_CstEnum)
    {
      CstEnum* enum_decl = &node->enum_decl;
      DEBUG_print_cst_node(str, indent_level, enum_decl->id, "id");
      DEBUG_print_cst_node_list(str, indent_level, enum_decl->members, "members");
    }
    else if(node->kind == CstNodeKind_CstInitList)
    {
      CstInitList* init_list = &node->init_list;
      DEBUG_print_cst_node_list(str, indent_level, init_list->members, "members");
    }
    else if(node->kind == CstNodeKind_CstGotoStmt)
    {
      CstGotoStmt* goto_stmt = &node->goto_stmt;
      DEBUG_print_cst_node(str, indent_level, goto_stmt->id, "id");
    }
    else if(node->kind == CstNodeKind_CstLabel)
    {
      CstLabel* label = &node->label;
      DEBUG_print_cst_node(str, indent_level, label->id, "id");
    }
    else if(node->kind == CstNodeKind_CstNew)
    {
      CstNew* new = &node->new;
      DEBUG_print_cst_node(str, indent_level, new->type_expr, "type_expr");
      DEBUG_print_cst_node(str, indent_level, new->count_expr, "count_expr");
    }
    else if(node->kind == CstNodeKind_CstPutc)
    {
      CstPutc* putc = &node->putc;
      DEBUG_print_cst_node(str, indent_level, putc->expr, "expr");
    }
    else
      assert(0);
  }
}
