bool parse_initializer_list(TokenStream*, AstNode**);
bool parse_expression(TokenStream*, AstNode**);
bool parse_node(TokenStream*, AstNode**);
bool parse_selector(TokenStream*, AstNode**);
bool parse_un_expr(TokenStream*, AstNode**);
bool parse_struct_member_list(TokenStream*, List*);

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
    ATTR(init_list, list, members) = new_list(arena, List_ast_node);

    if(success = get_next_token(input) && parse_initializer_member_list(input, ATTR(init_list, list, members)))
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
    ATTR(block, list, nodes) = new_list(arena, List_ast_node);

    if(success = get_next_token(input) && parse_node_list(input, ATTR(block, list, nodes)))
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
      AstNode* call = *node = new_ast_node(0, AstNode_call, &input->src_loc);
      ATTR(call, ast_node, id) = left_node;
      ATTR(call, list, args) = new_list(arena, List_ast_node);

      if(success = get_next_token(input) && parse_actual_arg_list(input, ATTR(call, list, args)))
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
    ATTR(index, op_kind, op_kind) = OperatorKind_ArrayIndex;
    ATTR(index, ast_node, left_operand) = left_node;

    if(success = get_next_token(input) && parse_expression(input, &ATTR(index, ast_node, right_operand)))
    {
      if(input->token.kind == Token_CloseBracket)
      {
        if(ATTR(index, ast_node, right_operand))
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
    ATTR(bin_expr, ast_node, left_operand) = left_node;

    if(input->token.kind == Token_Dot)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_MemberSelect;
    else if(input->token.kind == Token_ArrowRight)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_PtrMemberSelect;

    if(success = get_next_token(input) && parse_selector(input, &ATTR(bin_expr, ast_node, right_operand)))
    {
      if(ATTR(bin_expr, ast_node, right_operand))
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
    ATTR(un_expr, ast_node, operand) = left_node;

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
      un_expr->op = OperatorKind_PostIncrement;
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
    ATTR(bin_expr, ast_node, left_operand) = left_node;

    if(input->token.kind == Token_Star)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_Mul;
    else if(input->token.kind == Token_FwdSlash)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_Div;
    else if(input->token.kind == Token_Percent)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_Mod;
    else
      assert(0);

    if(success = get_next_token(input) && parse_factor(input, &ATTR(bin_expr, ast_node, right_operand)))
    {
      if(ATTR(bin_expr, ast_node, right_operand))
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
    ATTR(bin_expr, ast_node, left_operand) = left_node;

    if(input->token.kind == Token_Plus)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_Add;
    else if(input->token.kind == Token_Minus)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_Sub;
    else if(input->token.kind == Token_Pipe)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_BitwiseOr;
    else if(input->token.kind == Token_PipePipe)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_LogicOr;
    else if(input->token.kind == Token_Ampersand)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_BitwiseAnd;
    else if(input->token.kind == Token_AmpersandAmpersand)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_LogicAnd;
    else
      assert(0);

    if(success = get_next_token(input) && parse_term(input, &ATTR(bin_expr, ast_node, right_operand)))
    {
      if(ATTR(bin_expr, ast_node, right_operand))
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
    ATTR(bin_expr, ast_node, left_operand) = left_node;

    if(input->token.kind == Token_Equals)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_Assign;
    else if(input->token.kind == Token_EqualsEquals)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_Equals;
    else if(input->token.kind == Token_ExclamEquals)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_NotEquals;
    else if(input->token.kind == Token_AngleLeft)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_Less;
    else if(input->token.kind == Token_AngleLeftEquals)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_LessEquals;
    else if(input->token.kind == Token_AngleRight)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_Greater;
    else if(input->token.kind == Token_AngleRightEquals)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_GreaterEquals;

    if(success = get_next_token(input) && parse_expression(input, &ATTR(bin_expr, ast_node, right_operand)))
    {
      if(!ATTR(bin_expr, ast_node, right_operand))
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
    ATTR(ptr, ast_node, type_expr) = expr;

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
    ATTR(*node = new_ast_node(0, AstNode_id, &input->src_loc), str, name) = input->token.lexeme;

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

    if(success = get_next_token(input) && parse_expression(input, &ATTR(array, ast_node, size_expr)))
    {
      if(input->token.kind == Token_CloseBracket)
      {
        if(ATTR(array, ast_node, size_expr))
        {
          if(success = get_next_token(input) && parse_type_expr(input, &ATTR(array, ast_node, type_expr)))
          {
            if(!ATTR(array, ast_node, type_expr))
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

      if(success = get_next_token(input) && parse_type_expr(input, &ATTR(hoc_new, ast_node, type_expr)))
      {
        if(input->token.kind == Token_Comma)
        {
          if(success = get_next_token(input) && parse_expression(input, &ATTR(hoc_new, ast_node, count_expr)))
          {
            if(input->token.kind == Token_CloseParens)
            {
              if(ATTR(hoc_new, ast_node, count_expr))
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

      if(success = get_next_token(input) && parse_expression(input, &ATTR(hoc_putc, ast_node, expr)))
      {
        if(input->token.kind == Token_CloseParens)
        {
          if(ATTR(hoc_putc, ast_node, expr))
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
    ATTR(*node = new_ast_node(0, AstNode_id, &input->src_loc), str, name) = input->token.lexeme;
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
    ATTR(var, ast_node, type_expr) = type;

    if(input->token.kind == Token_Id)
    {
      AstNode* id = ATTR(var, ast_node, id) = new_ast_node(0, AstNode_id, &input->src_loc);
      ATTR(id, str, name) = input->token.lexeme;
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
      ATTR(un_expr, op_kind, op_kind) = OperatorKind_LogicNot;
    else if(input->token.kind == Token_Star)
      ATTR(un_expr, op_kind, op_kind) = OperatorKind_PointerDeref;
    else if(input->token.kind == Token_Ampersand)
      ATTR(un_expr, op_kind, op_kind) = OperatorKind_AddressOf;
    else if(input->token.kind == Token_Minus)
      ATTR(un_expr, op_kind, op_kind) = OperatorKind_Neg;
    else if(input->token.kind == Token_MinusMinus)
    {
#if 0
      un_expr->op = OperatorKind_PreDecrement;
#else
      success = compile_error(&input->src_loc, "`--` not supported");
#endif
    }
    else if(input->token.kind == Token_PlusPlus)
    {
#if 0
      un_expr->op = OperatorKind_PreIncrement;
#else
      success = compile_error(&input->src_loc, "`++` not supported");
#endif
    }
    else
      assert(0);

    if(success && (success = get_next_token(input)) && parse_factor(input, &ATTR(un_expr, ast_node, operand)))
    {
      if(!ATTR(un_expr, ast_node, operand))
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
        if(success = get_next_token(input) && parse_type_expr(input, &ATTR(cast, ast_node, type_expr)))
        {
          if(input->token.kind == Token_CloseParens)
          {
            if(ATTR(cast, ast_node, type_expr))
            {
              success = get_next_token(input) && parse_un_expr(input, &ATTR(cast, ast_node, expr));
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
      ATTR(var, ast_node, type_expr) = type;
      AstNode* id = ATTR(var, ast_node, id) = new_ast_node(0, AstNode_id, &input->src_loc);
      ATTR(id, str, name) = input->token.lexeme;

      if((success = get_next_token(input)) && input->token.kind == Token_Equals
          && (success = get_next_token(input)))
      {
        AstNode* init_expr = ATTR(var, ast_node, init_expr) = new_ast_node(0, AstNode_bin_expr, &input->src_loc);
        ATTR(init_expr, op_kind, op_kind) = OperatorKind_Assign;
        ATTR(init_expr, ast_node, left_operand) = id;

        if(success = parse_initializer_list(input, &ATTR(init_expr, ast_node, right_operand)))
        {
          if(!ATTR(init_expr, ast_node, right_operand))
          {
            if(success = parse_expression(input, &ATTR(init_expr, ast_node, right_operand)))
            {
              if(!ATTR(init_expr, ast_node, right_operand))
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
      success = get_next_token(input) && parse_expression(input, &ATTR(while_stmt, ast_node, cond_expr));
      if(!success)
        return success;

      if(ATTR(while_stmt, ast_node, cond_expr))
      {
        if(input->token.kind == Token_CloseParens)
        {
          if(!(success = get_next_token(input)))
            return success;

          if(!(success = parse_block(input, &ATTR(while_stmt, ast_node, body))))
            return success;

          if(!ATTR(while_stmt, ast_node, body))
          {
            if((success = parse_node(input, &ATTR(while_stmt, ast_node, body))) && !ATTR(while_stmt, ast_node, body))
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
      success = get_next_token(input) && parse_expression(input, &ATTR(if_stmt, ast_node, cond_expr));
      if(!success)
        return success;

      if(input->token.kind == Token_CloseParens)
      {
        if(ATTR(if_stmt, ast_node, cond_expr))
        {
          success = get_next_token(input) && parse_block(input, &ATTR(if_stmt, ast_node, body));
          if(!success)
            return success;

          if(!ATTR(if_stmt, ast_node, body))
            success = parse_node(input, &ATTR(if_stmt, ast_node, body));

          if(success)
          {
            if(ATTR(if_stmt, ast_node, body))
            {
              success = parse_else(input, &ATTR(if_stmt, ast_node, else_body));
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

    if(success = get_next_token(input) && parse_type_expr(input, &ATTR(proc, ast_node, ret_type_expr)))
    {
      if(ATTR(proc, ast_node, ret_type_expr))
      {
        if(input->token.kind == Token_Id)
        {
          AstNode* id = ATTR(proc, ast_node, id) = new_ast_node(0, AstNode_id, &input->src_loc);
          ATTR(id, str, name) = input->token.lexeme;

          if(!(success = get_next_token(input)))
            return success;

          if(input->token.kind == Token_OpenParens)
          {
            if(!(success = get_next_token(input)))
              return success;

            ATTR(proc, list, args) = new_list(arena, List_ast_node);
            if(success = parse_formal_arg_list(input, ATTR(proc, list, args)))
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
                if(success = get_next_token(input) && parse_block(input, &ATTR(proc, ast_node, body)))
                {
                  if(!ATTR(proc, ast_node, body))
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
      ATTR(include, str, file_path) = str_cap(str);

      if(!(success = get_next_token(input)))
        return success;

      char* hoc_text = file_read_text(arena, ATTR(include, str, file_path));
      if(hoc_text)
      {
        TokenStream* incl_input = mem_push_struct(arena, TokenStream);
        init_token_stream(incl_input, hoc_text, ATTR(include, str, file_path));

        if(success = get_next_token(incl_input))
        {
          AstNode* block = ATTR(include, ast_node, body) = new_ast_node(0, AstNode_block, &incl_input->src_loc);
          ATTR(block, list, nodes) = new_list(arena, List_ast_node);
          success = parse_node_list(incl_input, ATTR(block, list, nodes));
        }
      }
      else
        success = compile_error(&input->src_loc, "could not read file `%s`", ATTR(include, str, file_path));
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
      AstNode* id = ATTR(enum_decl, ast_node, id) = new_ast_node(0, AstNode_id, &input->src_loc);
      ATTR(id, str, name) = input->token.lexeme;

      if(!(success = get_next_token(input)))
        return success;

      if(input->token.kind == Token_OpenBrace)
      {

        if(!(success = get_next_token(input)))
          return success;

        ATTR(enum_decl, list, members) = new_list(arena, List_ast_node);
        AstNode* member = 0;
        do
        {
          member = 0;
          if(input->token.kind == Token_Id)
          {
            member = new_ast_node(0, AstNode_id, &input->src_loc);
            ATTR(member, str, name) = input->token.lexeme;
            append_list_elem(arena, ATTR(enum_decl, list, members), member, List_ast_node);

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
      AstNode* id = ATTR(union_decl, ast_node, id) = new_ast_node(0, AstNode_id, &input->src_loc);
      ATTR(id, str, name) = input->token.lexeme;
      success = get_next_token(input);
    }

    if(success)
    {
      ATTR(union_decl, list, members) = new_list(arena, List_ast_node);
      success = parse_struct_member_list(input, ATTR(union_decl, list, members));
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
      AstNode* id = ATTR(struct_decl, ast_node, id) = new_ast_node(0, AstNode_id, &input->src_loc);
      ATTR(id, str, name) = input->token.lexeme;
      success = get_next_token(input);
    }

    if(success)
    {
      ATTR(struct_decl, list, members) = new_list(arena, List_ast_node);
      success = parse_struct_member_list(input, ATTR(struct_decl, list, members));
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
          ATTR(var, ast_node, type_expr) = type;

          if(input->token.kind == Token_Id)
          {
            AstNode* id = ATTR(var, ast_node, id) = new_ast_node(0, AstNode_id, &input->src_loc);
            ATTR(id, str, name) = input->token.lexeme;
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
    success = get_next_token(input) && parse_expression(input, &ATTR(return_stmt, ast_node, expr));
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
      ATTR(*node = new_ast_node(0, AstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
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
      ATTR(*node = new_ast_node(0, AstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
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
      ATTR(*node = new_ast_node(0, AstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
    }
  }
  else if(input->token.kind == Token_Break)
  {
    if((success = parse_break(input, node)) && consume_semicolon(input))
    {
      AstNode* stmt = *node;
      ATTR(*node = new_ast_node(0, AstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
    }
  }
  else if(input->token.kind == Token_Continue)
  {
    if((success = parse_continue(input, node)) && consume_semicolon(input))
    {
      AstNode* stmt = *node;
      ATTR(*node = new_ast_node(0, AstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
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
      ATTR(*node = new_ast_node(0, AstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
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
          ATTR(*node = new_ast_node(0, AstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
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
  ATTR(*node, str, file_path) = input->src_loc.file_path;
  AstNode* block = ATTR(*node, ast_node, body) = new_ast_node(0, AstNode_block, &input->src_loc);
  ATTR(block, list, nodes) = new_list(arena, List_ast_node);

  if((success = parse_node_list(input, ATTR(block, list, nodes)))
      && input->token.kind != Token_EndOfInput)
  {
    success = compile_error(&input->src_loc, "expected `end-of-input`, at `%s`", get_token_printstr(&input->token));
  }

  return success;
}

