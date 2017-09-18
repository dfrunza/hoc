bool parse_initializer_list(TokenStream*, AstNode**);
bool parse_expression(TokenStream*, AstNode**);
bool parse_node(TokenStream*, AstNode**);
bool parse_selector(TokenStream*, AstNode**);
bool parse_un_expr(TokenStream*, AstNode**);
bool parse_struct_member_list(TokenStream*, List*);

char* OperatorKind_strings[] =
{
#define ENUM_MEMBER(NAME) #NAME
  OperatorKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

char*
get_op_kind_printstr(OperatorKind op)
{
  return OperatorKind_strings[op];
}

char* LiteralKind_strings[] =
{
#define ENUM_MEMBER(NAME) #NAME
  LiteralKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

char*
get_ast_literal_printstr(LiteralKind kind)
{
  return LiteralKind_strings[kind];
}

bool
consume_semicolon(TokenStream* input)
{
  bool success = true;
  if(input->token.kind == Token_Semicolon)
    success = get_next_token(input);
  else
    success = compile_error(&input->src_loc, "expected `;`, actual `%s`", get_token_printstr(&input->token));
  return success;
}

bool
parse_initializer_member_list(TokenStream* input, List* member_list)
{
  bool success = true;

  AstNode* member = 0;
  do
  {
    member = 0;
    if(input->token.kind == Token_OpenBrace)
    {
      if(success = parse_initializer_list(input, &member))
      {
        if(input->token.kind == Token_Comma)
          success = get_next_token(input);
      }
    }
    if(success)
    {
      if(!member)
        success = parse_expression(input, &member);

      if(success && member)
      {
        append_list_elem(arena, member_list, member, List_ast_node);
        if(input->token.kind == Token_Comma)
          success = get_next_token(input);
      }
    }
  }
  while(success && member);
  return success;
}

bool
parse_initializer_list(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_OpenBrace)
  {
    AstNode* init_list = *node = new_ast_node(0, AstNode_init_list, &input->src_loc);
    ATTR(init_list, members, list) = new_list(arena, List_ast_node);

    if(success = get_next_token(input) && parse_initializer_member_list(input, ATTR(init_list, members, list)))
    {
      if(input->token.kind == Token_CloseBrace)
        success = get_next_token(input);
      else
        success = compile_error(&input->src_loc, "expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

bool
parse_actual_arg_list(TokenStream* input, List* args)
{
  bool success = true;

  AstNode* arg = 0;
  do
  {
    arg = 0;
    success = parse_expression(input, &arg);
    if(success && arg)
    {
      append_list_elem(arena, args, arg, List_ast_node);
      if(input->token.kind == Token_Comma)
      {
        if((success = get_next_token(input)) && input->token.kind == Token_CloseParens)
          success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
      }
    }
  }
  while(success && arg);
  return success;
}

bool
parse_node_list(TokenStream* input, List* node_list)
{
  bool success = true;

  AstNode* node = 0;
  do
  {
    node = 0;
    while(input->token.kind == Token_Semicolon && (success = get_next_token(input)))
      ; // skip

    if((success = parse_node(input, &node)) && node)
    {
      append_list_elem(arena, node_list, node, List_ast_node);
    }
  }
  while(success && node);

  return success;
}

bool
parse_block(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_OpenBrace)
  {
    AstNode* block = *node = new_ast_node(0, AstNode_block, &input->src_loc);
    ATTR(block, nodes, list) = new_list(arena, List_ast_node);

    if(success = get_next_token(input) && parse_node_list(input, ATTR(block, nodes, list)))
    {
      if(input->token.kind == Token_CloseBrace)
      {
        success = get_next_token(input);
      }
      else
      {
        success = compile_error(&input->src_loc, "expected `}`, actual `%s`", get_token_printstr(&input->token));
      }
    }
  }
  return success;
}

bool
parse_rest_of_id(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == Token_OpenParens)
  {
    // procedure call
    if(left_node->kind == AstNode_id)
    {
      //auto* call = AST1(*node = new_ast1_call(&input->src_loc), call);
      AstNode* call = *node = new_ast_node(0, AstNode_call, &input->src_loc);
      //call->id = left_node;
      ATTR(call, id, ast_node) = left_node;
      ATTR(call, args, list) = new_list(arena, List_ast_node);

      if(success = get_next_token(input) && parse_actual_arg_list(input, ATTR(call, args, list)))
      {
        if(input->token.kind == Token_CloseParens)
          success = get_next_token(input);
        else
          success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
      }
    }
    else
      success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  else if(input->token.kind == Token_OpenBracket)
  {
    // array
    AstNode* index = *node = new_ast_node(0, AstNode_bin_expr, &input->src_loc);
    ATTR(index, op_kind, op_kind) = Operator_ArrayIndex;
    ATTR(index, left_operand, ast_node) = left_node;

    if(success = get_next_token(input) && parse_expression(input, &ATTR(index, right_operand, ast_node)))
    {
      if(input->token.kind == Token_CloseBracket)
      {
        if(ATTR(index, right_operand, ast_node))
        {
          success = get_next_token(input) && parse_rest_of_id(input, *node, node);
        }
        else
        {
          putback_token(input);
          success = compile_error(&input->src_loc, "[] : expression required inside the brackets");
        }
      }
      else
        success = compile_error(&input->src_loc, "expected `]`, actual `%s`", get_token_printstr(&input->token));
    }
  }

  return success;
}

bool
parse_rest_of_selector(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == Token_Dot ||
     input->token.kind == Token_ArrowRight)
  {
    AstNode* bin_expr = *node = new_ast_node(0, AstNode_bin_expr, &input->src_loc);
    ATTR(bin_expr, left_operand, ast_node) = left_node;

    if(input->token.kind == Token_Dot)
      ATTR(bin_expr, op_kind, op_kind) = Operator_MemberSelect;
    else if(input->token.kind == Token_ArrowRight)
      ATTR(bin_expr, op_kind, op_kind) = Operator_PtrMemberSelect;

    if(success = get_next_token(input) && parse_selector(input, &ATTR(bin_expr, right_operand, ast_node)))
    {
      if(ATTR(bin_expr, right_operand, ast_node))
        success = parse_rest_of_selector(input, *node, node);
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else if(input->token.kind == Token_PlusPlus ||
          input->token.kind == Token_MinusMinus)
  {
    AstNode* un_expr = *node = new_ast_node(0, AstNode_un_expr, &input->src_loc);
    ATTR(un_expr, operand, ast_node) = left_node;

    if(input->token.kind == Token_MinusMinus)
    {
#if 0
      un_expr->op = OperatorKin_PostDecrement;
#else
      success = compile_error(&input->src_loc, "`--` not supported");
#endif
    }
    else if(input->token.kind == Token_PlusPlus)
    {
#if 0
      un_expr->op = Operator_PostIncrement;
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

bool
parse_factor(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = parse_un_expr(input, node)) && *node)
    success = parse_rest_of_selector(input, *node, node);
  return success;
}

bool
parse_rest_of_factor(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == Token_Star ||
     input->token.kind == Token_FwdSlash ||
     input->token.kind == Token_Percent)
  {
    AstNode* bin_expr = *node = new_ast_node(0, AstNode_bin_expr, &input->src_loc);
    ATTR(bin_expr, left_operand, ast_node) = left_node;

    if(input->token.kind == Token_Star)
      ATTR(bin_expr, op_kind, op_kind) = Operator_Mul;
    else if(input->token.kind == Token_FwdSlash)
      ATTR(bin_expr, op_kind, op_kind) = Operator_Div;
    else if(input->token.kind == Token_Percent)
      ATTR(bin_expr, op_kind, op_kind) = Operator_Mod;
    else
      assert(0);

    if(success = get_next_token(input) && parse_factor(input, &ATTR(bin_expr, right_operand, ast_node)))
    {
      if(ATTR(bin_expr, right_operand, ast_node))
      {
        success = parse_rest_of_factor(input, *node, node);
      }
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }

  return success;
}

bool
parse_term(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = parse_factor(input, node)) && *node)
    success = parse_rest_of_factor(input, *node, node);
  return success;
}

bool
parse_rest_of_term(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == Token_Plus ||
     input->token.kind == Token_Minus ||
     input->token.kind == Token_Pipe ||
     input->token.kind == Token_PipePipe ||
     input->token.kind == Token_Ampersand ||
     input->token.kind == Token_AmpersandAmpersand)
  {
    AstNode* bin_expr = *node = new_ast_node(0, AstNode_bin_expr, &input->src_loc);
    ATTR(bin_expr, left_operand, ast_node) = left_node;

    if(input->token.kind == Token_Plus)
      ATTR(bin_expr, op_kind, op_kind) = Operator_Add;
    else if(input->token.kind == Token_Minus)
      ATTR(bin_expr, op_kind, op_kind) = Operator_Sub;
    else if(input->token.kind == Token_Pipe)
      ATTR(bin_expr, op_kind, op_kind) = Operator_BitwiseOr;
    else if(input->token.kind == Token_PipePipe)
      ATTR(bin_expr, op_kind, op_kind) = Operator_LogicOr;
    else if(input->token.kind == Token_Ampersand)
      ATTR(bin_expr, op_kind, op_kind) = Operator_BitwiseAnd;
    else if(input->token.kind == Token_AmpersandAmpersand)
      ATTR(bin_expr, op_kind, op_kind) = Operator_LogicAnd;
    else
      assert(0);

    if(success = get_next_token(input) && parse_term(input, &ATTR(bin_expr, right_operand, ast_node)))
    {
      if(ATTR(bin_expr, right_operand, ast_node))
      {
        success = parse_rest_of_term(input, *node, node);
      }
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }

  return success;
}

bool
parse_assignment(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = parse_term(input, node)) && *node)
    success = parse_rest_of_term(input, *node, node);
  return success;
}

bool
parse_rest_of_assignment(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == Token_Equals ||
     input->token.kind == Token_EqualsEquals ||
     input->token.kind == Token_ExclamEquals ||
     input->token.kind == Token_AngleLeft ||
     input->token.kind == Token_AngleLeftEquals ||
     input->token.kind == Token_AngleRight ||
     input->token.kind == Token_AngleRightEquals)
  {
    AstNode* bin_expr = *node = new_ast_node(0, AstNode_bin_expr, &input->src_loc);
    ATTR(bin_expr, left_operand, ast_node) = left_node;

    if(input->token.kind == Token_Equals)
      ATTR(bin_expr, op_kind, op_kind) = Operator_Assign;
    else if(input->token.kind == Token_EqualsEquals)
      ATTR(bin_expr, op_kind, op_kind) = Operator_Equals;
    else if(input->token.kind == Token_ExclamEquals)
      ATTR(bin_expr, op_kind, op_kind) = Operator_NotEquals;
    else if(input->token.kind == Token_AngleLeft)
      ATTR(bin_expr, op_kind, op_kind) = Operator_Less;
    else if(input->token.kind == Token_AngleLeftEquals)
      ATTR(bin_expr, op_kind, op_kind) = Operator_LessEquals;
    else if(input->token.kind == Token_AngleRight)
      ATTR(bin_expr, op_kind, op_kind) = Operator_Greater;
    else if(input->token.kind == Token_AngleRightEquals)
      ATTR(bin_expr, op_kind, op_kind) = Operator_GreaterEquals;

    if(success = get_next_token(input) && parse_expression(input, &ATTR(bin_expr, right_operand, ast_node)))
    {
      if(!ATTR(bin_expr, right_operand, ast_node))
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }

  return success;
}

bool
parse_type_expr_pointer(TokenStream* input, AstNode* expr, AstNode** node)
{
  *node = expr;
  bool success = true;
  
  if(input->token.kind == Token_Star)
  {
    AstNode* ptr = *node = new_ast_node(0, AstNode_pointer, &input->src_loc);
    ATTR(ptr, type_expr, ast_node) = expr;

    success = get_next_token(input) && parse_type_expr_pointer(input, *node, node);
  }
  return true;
}

bool
parse_type_expr_id(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_Id)
  {
    ATTR(*node = new_ast_node(0, AstNode_id, &input->src_loc), name, str) = input->token.lexeme;

    success = get_next_token(input) && parse_type_expr_pointer(input, *node, node);
  }
  return success;
}

bool
parse_type_expr(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_OpenBracket)
  {
    auto* array = *node = new_ast_node(0, AstNode_array, &input->src_loc);

    if(success = get_next_token(input) && parse_expression(input, &ATTR(array, size_expr, ast_node)))
    {
      if(input->token.kind == Token_CloseBracket)
      {
        if(ATTR(array, size_expr, ast_node))
        {
          if(success = get_next_token(input) && parse_type_expr(input, &ATTR(array, type_expr, ast_node)))
          {
            if(!ATTR(array, type_expr, ast_node))
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

bool
parse_new(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = false;

  if(input->token.kind == Token_New && (success = get_next_token(input)))
  {
    if(input->token.kind == Token_OpenParens)
    {
      AstNode* hoc_new = *node = new_ast_node(0, AstNode_hoc_new, &input->src_loc);

      if(success = get_next_token(input) && parse_type_expr(input, &ATTR(hoc_new, type_expr, ast_node)))
      {
        if(input->token.kind == Token_Comma)
        {
          if(success = get_next_token(input) && parse_expression(input, &ATTR(hoc_new, count_expr, ast_node)))
          {
            if(input->token.kind == Token_CloseParens)
            {
              if(ATTR(hoc_new, count_expr, ast_node))
              {
                success = get_next_token(input);
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
        }
        else
          success = compile_error(&input->src_loc, "expected `,`, actual `%s`", get_token_printstr(&input->token));
      }
    }
    else
      success = compile_error(&input->src_loc, "expected `(`, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

bool
parse_putc(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = false;

  if(input->token.kind == Token_Putc && (success = get_next_token(input)))
  {
    if(input->token.kind == Token_OpenParens)
    {
      AstNode* hoc_putc = *node = new_ast_node(0, AstNode_hoc_putc, &input->src_loc);

      if(success = get_next_token(input) && parse_expression(input, &ATTR(hoc_putc, expr, ast_node)))
      {
        if(input->token.kind == Token_CloseParens)
        {
          if(ATTR(hoc_putc, expr, ast_node))
          {
            success = get_next_token(input);
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
    }
    else
      success = compile_error(&input->src_loc, "expected `(`, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

bool
parse_selector(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_OpenParens)
  {
    if(success = get_next_token(input) && parse_expression(input, node))
    {
      if(*node)
      {
        if(input->token.kind == Token_CloseParens)
        {
          success = get_next_token(input);
        }
        else
        {
          success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
        }
      }
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "expression expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else if(is_literal_token(input->token.kind)
          || input->token.kind == Token_True
          || input->token.kind == Token_False)
  {
    AstNode* lit = *node = new_ast_node(0, AstNode_lit, &input->src_loc);

    if(input->token.kind == Token_IntNum)
    {
      ATTR(lit, lit_kind, lit_kind) = Literal_int_val;
      ATTR(lit, int_val, int_val) = *input->token.int_val;
    }
    else if(input->token.kind == Token_FloatNum)
    {
      ATTR(lit, lit_kind, lit_kind) = Literal_float_val;
      ATTR(lit, float_val, float_val) = *input->token.float_val;
    }
    else if(input->token.kind == Token_True ||
            input->token.kind == Token_False)
    {
      ATTR(lit, lit_kind, lit_kind) = Literal_bool_val;
      ATTR(lit, bool_val, bool_val) = (input->token.kind == Token_True ? 1 : 0);
    }
    else if(input->token.kind == Token_Char)
    {
      ATTR(lit, lit_kind, lit_kind) = Literal_char_val;
      ATTR(lit, char_val, char_val) = input->token.char_val;
    }
    else if(input->token.kind == Token_String)
    {
      ATTR(lit, lit_kind, lit_kind) = Literal_str;
      ATTR(lit, str, str) = input->token.str;
    }
    else
      assert(0);

    success = get_next_token(input);
  }
  else if(input->token.kind == Token_Id)
  {
    ATTR(*node = new_ast_node(0, AstNode_id, &input->src_loc), name, str) = input->token.lexeme;
    success = get_next_token(input) && parse_rest_of_id(input, *node, node);
  }
  else if(input->token.kind == Token_New)
    success = parse_new(input, node);
  else if(input->token.kind == Token_Putc)
    success = parse_putc(input, node);

  return success;
}

bool
parse_formal_arg(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* type = 0;
  if((success = parse_type_expr(input, &type)) && type)
  {
    AstNode* var = *node = new_ast_node(0, AstNode_var, &input->src_loc);
    ATTR(var, type_expr, ast_node) = type;

    if(input->token.kind == Token_Id)
    {
      AstNode* id = ATTR(var, id, ast_node) = new_ast_node(0, AstNode_id, &input->src_loc);
      ATTR(id, name, str) = input->token.lexeme;
      success = get_next_token(input);
    }
    else
      success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

bool
parse_formal_arg_list(TokenStream* input, List* arg_list)
{
  bool success = true;

  AstNode* arg = 0;
  do
  {
    arg = 0;
    if((success = parse_formal_arg(input, &arg)) && arg)
    {
      append_list_elem(arena, arg_list, arg, List_ast_node);
      if(input->token.kind == Token_Comma)
      {
        success = get_next_token(input);
      }
      else if(input->token.kind != Token_CloseParens)
      {
        success = compile_error(&input->src_loc, "expected `,`, actual `%s`", get_token_printstr(&input->token));
      }
    }
  }
  while(success && arg);
  return success;
}

bool
parse_un_expr(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_Exclam ||
     input->token.kind == Token_Star ||
     input->token.kind == Token_Ampersand ||
     input->token.kind == Token_Minus ||
     input->token.kind == Token_MinusMinus ||
     input->token.kind == Token_PlusPlus)
  {
    AstNode* un_expr = *node = new_ast_node(0, AstNode_un_expr, &input->src_loc);

    if(input->token.kind == Token_Exclam)
      ATTR(un_expr, op_kind, op_kind) = Operator_LogicNot;
    else if(input->token.kind == Token_Star)
      ATTR(un_expr, op_kind, op_kind) = Operator_PointerDeref;
    else if(input->token.kind == Token_Ampersand)
      ATTR(un_expr, op_kind, op_kind) = Operator_AddressOf;
    else if(input->token.kind == Token_Minus)
      ATTR(un_expr, op_kind, op_kind) = Operator_Neg;
    else if(input->token.kind == Token_MinusMinus)
    {
#if 0
      un_expr->op = Operator_PreDecrement;
#else
      success = compile_error(&input->src_loc, "`--` not supported");
#endif
    }
    else if(input->token.kind == Token_PlusPlus)
    {
#if 0
      un_expr->op = Operator_PreIncrement;
#else
      success = compile_error(&input->src_loc, "`++` not supported");
#endif
    }
    else
      assert(0);

    if(success && (success = get_next_token(input)) && parse_factor(input, &ATTR(un_expr, operand, ast_node)))
    {
      if(!ATTR(un_expr, operand, ast_node))
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else if(input->token.kind == Token_Cast)
  {
    AstNode* cast = *node = new_ast_node(0, AstNode_cast, &input->src_loc);

    if(success = get_next_token(input))
    {
      if(input->token.kind == Token_OpenParens)
      {
        if(success = get_next_token(input) && parse_type_expr(input, &ATTR(cast, type_expr, ast_node)))
        {
          if(input->token.kind == Token_CloseParens)
          {
            if(ATTR(cast, type_expr, ast_node))
            {
              success = get_next_token(input) && parse_un_expr(input, &ATTR(cast, expr, ast_node));
            }
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
    success = parse_selector(input, node);

  return success;
}

bool
parse_expression(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = parse_assignment(input, node)) && *node)
    success = parse_rest_of_assignment(input, *node, node);
  return success;
}

bool
parse_var_decl(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* type = 0;
  if((success = parse_type_expr(input, &type)) && type)
  {
    if(input->token.kind == Token_Id)
    {
      AstNode* var = *node = new_ast_node(0, AstNode_var, &input->src_loc);
      ATTR(var, type_expr, ast_node) = type;
      AstNode* id = ATTR(var, id, ast_node) = new_ast_node(0, AstNode_id, &input->src_loc);
      ATTR(id, name, str) = input->token.lexeme;

      if((success = get_next_token(input)) && input->token.kind == Token_Equals
          && (success = get_next_token(input)))
      {
        if(success = parse_initializer_list(input, &ATTR(var, init_expr, ast_node)))
        {
          if(!ATTR(var, init_expr, ast_node))
          {
            if(success = parse_expression(input, &ATTR(var, init_expr, ast_node)))
            {
              if(!ATTR(var, init_expr, ast_node))
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

#if 0
bool
parse_for(TokenStream* input, AstNode1** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_For)
  {
    auto* for_stmt = AST1(*node = new_ast1_node(&input->src_loc, AstNode_for_stmt), for_stmt);

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == Token_OpenParens)
    {
      success = get_next_token(input) && parse_var_decl(input, &for_stmt->decl_expr)
        && consume_semicolon(input)
        && parse_expression(input, &for_stmt->cond_expr)
        && consume_semicolon(input)
        && parse_expression(input, &for_stmt->loop_expr);
      if(!success)
        return success;

      if(input->token.kind == Token_CloseParens)
      {
        if(success = get_next_token(input) && parse_block(input, &for_stmt->body))
        {
          if(!for_stmt->body)
          {
            if(success = parse_node(input, &for_stmt->body))
            {
              if(for_stmt->body)
              {
                AstNode1* single_stmt = for_stmt->body;
                auto* block = AST1(for_stmt->body = new_ast1_block(&input->src_loc), block);
                append_list_elem(arena, block->nodes, single_stmt, List_ast1_node);
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
#endif

bool
parse_while(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_While)
  {
    AstNode* while_stmt = *node = new_ast_node(0, AstNode_while_stmt, &input->src_loc);

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == Token_OpenParens)
    {
      success = get_next_token(input) && parse_expression(input, &ATTR(while_stmt, cond_expr, ast_node));
      if(!success)
        return success;

      if(ATTR(while_stmt, cond_expr, ast_node))
      {
        if(input->token.kind == Token_CloseParens)
        {
          if(!(success = get_next_token(input)))
            return success;

          if(!(success = parse_block(input, &ATTR(while_stmt, body, ast_node))))
            return success;

          if(!ATTR(while_stmt, body, ast_node))
          {
            if((success = parse_node(input, &ATTR(while_stmt, body, ast_node))) && !ATTR(while_stmt, body, ast_node))
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

bool
parse_else(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_Else)
  {
    if(success = get_next_token(input) && parse_block(input, node))
    {
      if(!*node)
      {
        if((success = parse_node(input, node)) && !*node)
        {
          putback_token(input);
          success = compile_error(&input->src_loc, "statement required, at `%s`", get_token_printstr(&input->token));
        }
      }
    }
  }
  return success;
}

bool
parse_if(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_If)
  {
    AstNode* if_stmt = *node = new_ast_node(0, AstNode_if_stmt, &input->src_loc);

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == Token_OpenParens)
    {
      success = get_next_token(input) && parse_expression(input, &ATTR(if_stmt, cond_expr, ast_node));
      if(!success)
        return success;

      if(input->token.kind == Token_CloseParens)
      {
        if(ATTR(if_stmt, cond_expr, ast_node))
        {
          success = get_next_token(input) && parse_block(input, &ATTR(if_stmt, body, ast_node));
          if(!success)
            return success;

          if(!ATTR(if_stmt, body, ast_node))
            success = parse_node(input, &ATTR(if_stmt, body, ast_node));

          if(success)
          {
            if(ATTR(if_stmt, body, ast_node))
            {
              success = parse_else(input, &ATTR(if_stmt, else_body, ast_node));
            }
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

bool
parse_proc(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_Proc)
  {
    AstNode* proc = *node = new_ast_node(0, AstNode_proc, &input->src_loc);

    if(success = get_next_token(input) && parse_type_expr(input, &ATTR(proc, ret_type_expr, ast_node)))
    {
      if(ATTR(proc, ret_type_expr, ast_node))
      {
        if(input->token.kind == Token_Id)
        {
          AstNode* id = ATTR(proc, id, ast_node) = new_ast_node(0, AstNode_id, &input->src_loc);
          ATTR(id, name, str) = input->token.lexeme;

          if(!(success = get_next_token(input)))
            return success;

          if(input->token.kind == Token_OpenParens)
          {
            if(!(success = get_next_token(input)))
              return success;

            ATTR(proc, args, list) = new_list(arena, List_ast_node);
            if(success = parse_formal_arg_list(input, ATTR(proc, args, list)))
            {
              if(input->token.kind == Token_CloseParens)
              {
#if 0
                if(success = get_next_token(input) && parse_block(input, &proc->body))
                {
                  if(!proc->body && (proc->is_decl = success = consume_semicolon(input)))
                    proc->body = (AstNode1*)new_ast1_block(&(*node)->src_loc);
                }
#else
                if(success = get_next_token(input) && parse_block(input, &ATTR(proc, body, ast_node)))
                {
                  if(!ATTR(proc, body, ast_node))
                    success = consume_semicolon(input);
                }
#endif
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

bool
parse_include(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_Include)
  {
    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == Token_String)
    {
      AstNode* include = *node = new_ast_node(0, AstNode_include, &input->src_loc);

      String* str = str_new(arena);
      str_append(str, input->src_loc.file_path);
      path_make_dir(str->head);
      str_tidyup(str);
      str_append(str, input->token.str);
      ATTR(include, file_path, str) = str_cap(str);

      if(!(success = get_next_token(input)))
        return success;

      char* hoc_text = file_read_text(arena, ATTR(include, file_path, str));
      if(hoc_text)
      {
        TokenStream* incl_input = mem_push_struct(arena, TokenStream);
        init_token_stream(incl_input, hoc_text, ATTR(include, file_path, str));

        if(success = get_next_token(incl_input))
        {
          AstNode* block = ATTR(include, body, ast_node);
          ATTR(block, nodes, list) = new_list(arena, List_ast_node);
          success = parse_node_list(incl_input, ATTR(block, nodes, list));
        }
      }
      else
        success = compile_error(&input->src_loc, "could not read file `%s`", ATTR(include, file_path, str));
    }
    else
      success = compile_error(&input->src_loc, "string expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

bool
parse_enum(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_Enum)
  {
    AstNode* enum_decl = *node = new_ast_node(0, AstNode_enum_decl, &input->src_loc);

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == Token_Id)
    {
      AstNode* id = ATTR(enum_decl, id, ast_node) = new_ast_node(0, AstNode_id, &input->src_loc);
      ATTR(id, name, str) = input->token.lexeme;

      if(!(success = get_next_token(input)))
        return success;

      if(input->token.kind == Token_OpenBrace)
      {

        if(!(success = get_next_token(input)))
          return success;

        ATTR(enum_decl, members, list) = new_list(arena, List_ast_node);
        AstNode* member = 0;
        do
        {
          member = 0;
          if(input->token.kind == Token_Id)
          {
            member = new_ast_node(0, AstNode_id, &input->src_loc);
            ATTR(member, name, str) = input->token.lexeme;
            append_list_elem(arena, ATTR(enum_decl, members, list), member, List_ast_node);

            if((success = get_next_token(input)) && input->token.kind == Token_Comma)
            {
              success = get_next_token(input);
            }
            else if(input->token.kind != Token_CloseBrace)
              member = 0;
          }
        }
        while(member && success);

        if(input->token.kind == Token_CloseBrace)
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

bool
parse_union(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_Union)
  {
    AstNode* union_decl = *node = new_ast_node(0, AstNode_union_decl, &input->src_loc);

    if((success = get_next_token(input)) && input->token.kind == Token_Id)
    {
      AstNode* id = ATTR(union_decl, id, ast_node) = new_ast_node(0, AstNode_id, &input->src_loc);
      ATTR(id, name, str) = input->token.lexeme;
      success = get_next_token(input);
    }

    if(success)
    {
      ATTR(union_decl, members, list) = new_list(arena, List_ast_node);
      success = parse_struct_member_list(input, ATTR(union_decl, members, list));
    }
  }
  return success;
}

bool
parse_struct(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_Struct)
  {
    AstNode* struct_decl = *node = new_ast_node(0, AstNode_struct_decl, &input->src_loc);

    if((success = get_next_token(input)) && input->token.kind == Token_Id)
    {
      AstNode* id = ATTR(struct_decl, id, ast_node) = new_ast_node(0, AstNode_id, &input->src_loc);
      ATTR(id, name, str) = input->token.lexeme;
      success = get_next_token(input);
    }

    if(success)
    {
      ATTR(struct_decl, members, list) = new_list(arena, List_ast_node);
      success = parse_struct_member_list(input, ATTR(struct_decl, members, list));
    }
  }
  return success;
}

bool
parse_struct_member_list(TokenStream* input, List* member_list)
{
  bool success = true;

  if(input->token.kind == Token_OpenBrace)
  {
    if(!(success = get_next_token(input)))
      return success;

    AstNode* member = 0;
    do
    {
      member = 0;

      AstNode* type = 0;
      if(success = parse_type_expr(input, &type))
      {
        if(!type)
        {
          if(input->token.kind == Token_Union)
          {
            success = parse_union(input, &type);
          }
          else if(input->token.kind == Token_Struct)
          {
            success = parse_struct(input, &type);
          }
        }

        if(success && type)
        {
          AstNode* var = member = new_ast_node(0, AstNode_var, &input->src_loc);
          ATTR(var, type_expr, ast_node) = type;

          if(input->token.kind == Token_Id)
          {
            AstNode* id = ATTR(var, id, ast_node) = new_ast_node(0, AstNode_id, &input->src_loc);
            ATTR(id, name, str) = input->token.lexeme;
            success = get_next_token(input);
          }
          else if(type->kind == AstNode_struct_decl
                  || type->kind == AstNode_union_decl)
          {
            /* anonymous struct/union */
          }
          else
            success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));

          if(success)
          {
            append_list_elem(arena, member_list, member, List_ast_node);
            success = consume_semicolon(input);
          }
        }
      }
    }
    while(member && success);

    if(success)
    {
      if(input->token.kind == Token_CloseBrace)
        success = get_next_token(input);
      else
        success = compile_error(&input->src_loc, "expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  else
    success = compile_error(&input->src_loc, "expected `{`, actual `%s`", get_token_printstr(&input->token));
  return success;
}

bool
parse_var(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = false;

  if(input->token.kind == Token_Var)
    success = get_next_token(input) && parse_var_decl(input, node);
  return success;
}

bool
parse_return(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_Return)
  {
    AstNode* return_stmt = *node = new_ast_node(0, AstNode_return_stmt, &input->src_loc);
    success = get_next_token(input) && parse_expression(input, &ATTR(return_stmt, expr, ast_node));
  }

  return success;
}

#if 0
bool
parse_label(TokenStream* input, AstNode1* id, AstNode1** node)
{
  *node = id;
  bool success = true;

  if(input->token.kind == Token_Colon && (success = get_next_token(input)))
  {
    if(id->kind == AstNode_id)
    {
      auto* label = AST1(*node = new_ast1_node(&input->src_loc, AstNode_label), label);
      label->id = id;
    }
    else
      success = compile_error(&input->src_loc, "label identifier expected");
  }
  return success;
}

bool
parse_goto(TokenStream* input, AstNode1** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_Goto)
  {
    auto* goto_stmt = AST1(*node = new_ast1_node(&input->src_loc, AstNode_goto_stmt), goto_stmt);

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == Token_Id)
    {
      goto_stmt->id = new_ast1_id(&input->src_loc, input->token.lexeme);
      success = get_next_token(input);
    }
    else
      success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}
#endif

bool
parse_continue(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_Continue)
  {
    *node = new_ast_node(0, AstNode_continue_stmt, &input->src_loc);
    success = get_next_token(input);
  }
  return success;
}

bool
parse_break(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_Break)
  {
    *node = new_ast_node(0, AstNode_break_stmt, &input->src_loc);
    success = get_next_token(input);
  }
  return success;
}

bool
parse_node(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_Var)
  {
    success = parse_var(input, node) && consume_semicolon(input);
  }
  else if(input->token.kind == Token_Include)
  {
    success = parse_include(input, node) && consume_semicolon(input);
  }
  else if(input->token.kind == Token_Proc)
  {
    success = parse_proc(input, node);
  }
  else if(input->token.kind == Token_If)
  {
    if(success = parse_if(input, node))
    {
      AstNode* stmt = *node;
      ATTR(*node = new_ast_node(0, AstNode_stmt, stmt->src_loc), stmt, ast_node) = stmt;
    }
  }
  else if(input->token.kind == Token_Else)
  {
    success = compile_error(&input->src_loc, "unmatched `else`");
  }
  else if(input->token.kind == Token_While)
  {
    if(success = parse_while(input, node))
    {
      AstNode* stmt = *node;
      ATTR(*node = new_ast_node(0, AstNode_stmt, stmt->src_loc), stmt, ast_node) = stmt;
    }
  }
#if 0
  else if(input->token.kind == Token_For)
  {
    success = parse_for(input, &node);
  }
#endif
  else if(input->token.kind == Token_Return)
  {
    if((success = parse_return(input, node)) && consume_semicolon(input))
    {
      AstNode* stmt = *node;
      ATTR(*node = new_ast_node(0, AstNode_stmt, stmt->src_loc), stmt, ast_node) = stmt;
    }
  }
  else if(input->token.kind == Token_Break)
  {
    if((success = parse_break(input, node)) && consume_semicolon(input))
    {
      AstNode* stmt = *node;
      ATTR(*node = new_ast_node(0, AstNode_stmt, stmt->src_loc), stmt, ast_node) = stmt;
    }
  }
  else if(input->token.kind == Token_Continue)
  {
    if((success = parse_continue(input, node)) && consume_semicolon(input))
    {
      AstNode* stmt = *node;
      ATTR(*node = new_ast_node(0, AstNode_stmt, stmt->src_loc), stmt, ast_node) = stmt;
    }
  }
#if 0
  else if(input->token.kind == Token_Goto)
  {
    success = parse_goto(input, &node) && consume_semicolon(input);
  }
#endif
  else if(input->token.kind == Token_Semicolon)
  {
    if(success = consume_semicolon(input))
    {
      *node = new_ast_node(0, AstNode_stmt, &input->src_loc);
    }
  }
  else if(input->token.kind == Token_OpenBrace)
  {
    if(success = parse_block(input, node))
    {
      AstNode* stmt = *node;
      ATTR(*node = new_ast_node(0, AstNode_stmt, stmt->src_loc), stmt, ast_node) = stmt;
    }
  }
  else
  {
    if(success = parse_expression(input, node))
    {
      if(*node)
      {
#if 0
        if(input->token.kind == Token_Colon)
          success = parse_label(input, &node, node);
        else
          success = consume_semicolon(input);
#else
        if(success = consume_semicolon(input))
        {
          AstNode* stmt = *node;
          ATTR(*node = new_ast_node(0, AstNode_stmt, stmt->src_loc), stmt, ast_node) = stmt;
        }
#endif
      }
    }
  }

  return success;
}

bool
parse(TokenStream* input, AstNode** node)
{
  bool success = true;

  *node = new_ast_node(0, AstNode_module, &input->src_loc);
  ATTR(*node, file_path, str) = input->src_loc.file_path;
  AstNode* block = ATTR(*node, body, ast_node) = new_ast_node(0, AstNode_block, &input->src_loc);
  ATTR(block, nodes, list) = new_list(arena, List_ast_node);
#if 1
  if((success = parse_node_list(input, ATTR(block, nodes, list)))
      && input->token.kind != Token_EndOfInput)
  {
    success = compile_error(&input->src_loc, "expected `end-of-input`, at `%s`", get_token_printstr(&input->token));
  }
#else
  auto* module = AST1(*node = new_ast1_module(&input->src_loc, input->src_loc.file_path), module);

  if((success = parse_node_list(input, AST1(module->body, block)->nodes))
     && input->token.kind != Token_EndOfInput)
  {
    success = compile_error(&input->src_loc, "expected `end-of-input`, at `%s`", get_token_printstr(&input->token));
  }
#endif
  return success;
}

#if 0
void
DEBUG_print_ast1_node(String* str, int indent_level, char* tag, AstNode1* node)
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
                     get_ast1_kind_printstr(node->kind), node->src_loc->file_path, node->src_loc->line_nr);
#else
    DEBUG_print_line(str, indent_level, "src_line=\"%s:%d\"", node->src_loc->file_path, node->src_loc->line_nr);
#endif
    ++indent_level;

    if(node->kind == AstKind1_module)
    {
      auto* module = AST1(node, module);
      DEBUG_print_line(str, indent_level, "file_path: \"%s\"", module->file_path);
      DEBUG_print_ast1_node(str, indent_level, "body", module->body);
    }
    else if(node->kind == AstKind1_include)
    {
      auto* include = AST1(node, include);
      DEBUG_print_line(str, indent_level, "file_path: \"%s\"", include->file_path);
      DEBUG_print_ast1_node(str, indent_level, "body", include->body);
    }
    else if(node->kind == AstKind1_proc)
    {
      auto* proc = AST1(node, proc);
      DEBUG_print_ast1_node(str, indent_level, "ret_type", proc->ret_type_expr);
      DEBUG_print_ast1_node(str, indent_level, "id", proc->id);
      DEBUG_print_ast_node_list(str, indent_level, "args", proc->args);
      DEBUG_print_ast1_node(str, indent_level, "body", proc->body);
    }
    else if(node->kind == AstKind1_var)
    {
      auto* var = AST1(node, var);
      DEBUG_print_ast1_node(str, indent_level, "type_expr", var->type_expr);
      DEBUG_print_ast1_node(str, indent_level, "id", var->id);
      DEBUG_print_ast1_node(str, indent_level, "init_expr", var->init_expr);
    }
    else if(node->kind == AstKind1_id)
    {
      auto* id = AST1(node, id);
      DEBUG_print_line(str, indent_level, "name: %s", id->name);
    }
    else if(node->kind == AstKind1_block)
    {
      auto* block = AST1(node, block);
      DEBUG_print_ast_node_list(str, indent_level, "nodes", block->nodes);
    }
    else if(node->kind == AstKind1_bin_expr)
    {
      auto* bin_expr = AST1(node, bin_expr);
      DEBUG_print_line(str, indent_level, "op: %s", get_op_kind_printstr(bin_expr->op));
      DEBUG_print_ast1_node(str, indent_level, "left_operand", bin_expr->left_operand);
      DEBUG_print_ast1_node(str, indent_level, "right_operand", bin_expr->right_operand);
    }
    else if(node->kind == AstKind1_un_expr)
    {
      auto* un_expr = AST1(node, un_expr);
      DEBUG_print_line(str, indent_level, "op: %s", get_op_kind_printstr(un_expr->op));
      DEBUG_print_ast1_node(str, indent_level, "operand", un_expr->operand);
    }
    else if(node->kind == AstKind1_stmt)
    {
      auto* stmt = AST1(node, stmt);
      DEBUG_print_ast1_node(str, indent_level, "stmt", stmt->stmt);
    }
    else if(node->kind == AstKind1_if_stmt)
    {
      auto* if_stmt = AST1(node, if_stmt);
      DEBUG_print_ast1_node(str, indent_level, "cond_expr", if_stmt->cond_expr);
      DEBUG_print_ast1_node(str, indent_level, "body", if_stmt->body);
      DEBUG_print_ast1_node(str, indent_level, "else_body", if_stmt->else_body);
    }
    else if(node->kind == AstKind1_return_stmt)
    {
      auto* return_stmt = AST1(node, return_stmt);
      DEBUG_print_ast1_node(str, indent_level, "expr", return_stmt->expr);
    }
    else if(node->kind == AstKind1_lit)
    {
      auto* lit = AST1(node, lit);
      DEBUG_print_line(str, indent_level, get_ast1_literal_printstr(lit->kind));
      if(lit->kind == Literal_int_val)
        DEBUG_print_line(str, indent_level, "int_val: %d", lit->int_val);
      else if(lit->kind == Literal_float_val)
        DEBUG_print_line(str, indent_level, "float_val: %f", lit->float_val);
      else if(lit->kind == Literal_bool_val)
        DEBUG_print_line(str, indent_level, "bool_val: %d", lit->bool_val);
      else if(lit->kind == Literal_char_val)
      {
        char buf[3] = {0};
        print_char(buf, lit->char_val);
        DEBUG_print_line(str, indent_level, "char_val: '%s'", buf);
      }
      else if(lit->kind == Literal_str)
        DEBUG_print_line(str, indent_level, "str: \"%s\"", lit->str);
      else
        assert(0);
    }
    else if(node->kind == AstKind1_while_stmt)
    {
      auto* while_stmt = AST1(node, while_stmt);
      DEBUG_print_ast1_node(str, indent_level, "cond_expr", while_stmt->cond_expr);
      DEBUG_print_ast1_node(str, indent_level, "body", while_stmt->body);
    }
    else if(node->kind == AstKind1_for_stmt)
    {
      auto* for_stmt = AST1(node, for_stmt);
      DEBUG_print_ast1_node(str, indent_level, "decl_expr", for_stmt->decl_expr);
      DEBUG_print_ast1_node(str, indent_level, "cond_expr", for_stmt->cond_expr);
      DEBUG_print_ast1_node(str, indent_level, "loop_expr", for_stmt->loop_expr);
      DEBUG_print_ast1_node(str, indent_level, "body", for_stmt->body);
    }
    else if(node->kind == AstKind1_cast)
    {
      auto* cast = AST1(node, cast);
      DEBUG_print_ast1_node(str, indent_level, "type_expr", cast->type_expr);
      DEBUG_print_ast1_node(str, indent_level, "expr", cast->expr);
    }
    else if(node->kind == AstKind1_array)
    {
      auto* array = AST1(node, array);
      DEBUG_print_ast1_node(str, indent_level, "type_expr", array->type_expr);
      DEBUG_print_ast1_node(str, indent_level, "size_expr", array->size_expr);
    }
    else if(node->kind == AstKind1_pointer)
    {
      auto* ptr = AST1(node, pointer);
      DEBUG_print_ast1_node(str, indent_level, "type_expr", ptr->type_expr);
    }
    else if(node->kind == AstKind1_call)
    {
      auto* call = AST1(node, call);
      DEBUG_print_ast1_node(str, indent_level, "id", call->id);
      DEBUG_print_ast_node_list(str, indent_level, "args", call->args);
    }
    else if(node->kind == AstKind1_break_stmt
            || node->kind == AstKind1_continue_stmt)
    {
      /* no extra info to print */
    }
    else if(node->kind == AstKind1_struct_decl)
    {
      auto* struct_decl = AST1(node, struct_decl);
      DEBUG_print_ast1_node(str, indent_level, "id", struct_decl->id);
      DEBUG_print_ast_node_list(str, indent_level, "members", struct_decl->members);
    }
    else if(node->kind == AstKind1_union_decl)
    {
      auto* union_decl = AST1(node, union_decl);
      DEBUG_print_ast1_node(str, indent_level, "id", union_decl->id);
      DEBUG_print_ast_node_list(str, indent_level, "members", union_decl->members);
    }
    else if(node->kind == AstKind1_enum_decl)
    {
      auto* enum_decl = AST1(node, enum_decl);
      DEBUG_print_ast1_node(str, indent_level, "id", enum_decl->id);
      DEBUG_print_ast_node_list(str, indent_level, "members", enum_decl->members);
    }
    else if(node->kind == AstKind1_init_list)
    {
      auto* init_list = AST1(node, init_list);
      DEBUG_print_ast_node_list(str, indent_level, "members", init_list->members);
    }
    else if(node->kind == AstKind1_goto_stmt)
    {
      auto* goto_stmt = AST1(node, goto_stmt);
      DEBUG_print_ast1_node(str, indent_level, "id", goto_stmt->id);
    }
    else if(node->kind == AstKind1_label)
    {
      auto* label = AST1(node, label);
      DEBUG_print_ast1_node(str, indent_level, "id", label->id);
    }
    else if(node->kind == AstKind1_hoc_new)
    {
      auto* hoc_new = AST1(node, hoc_new);
      DEBUG_print_ast1_node(str, indent_level, "type_expr", hoc_new->type_expr);
      DEBUG_print_ast1_node(str, indent_level, "count_expr", hoc_new->count_expr);
    }
    else if(node->kind == AstKind1_hoc_putc)
    {
      auto* hoc_putc = AST1(node, hoc_putc);
      DEBUG_print_ast1_node(str, indent_level, "expr", hoc_putc->expr);
    }
    else
      assert(0);
  }
}
#endif
