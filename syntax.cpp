bool parse_initializer_list(TokenStream*, AstNode**);
bool parse_expr(TokenStream*, AstNode**);
bool parse_node(TokenStream*, AstNode**);
bool parse_selector(TokenStream*, AstNode**);
bool parse_un_expr(TokenStream*, AstNode**);
bool parse_struct_member_list(TokenStream*, List*);

SourceLoc* clone_source_loc(SourceLoc* src_loc)
{
  SourceLoc* clone = mem_push_struct(arena, SourceLoc);
  *clone = *src_loc;
  return clone;
}

bool consume_semicolon(TokenStream* input)
{
  bool success = true;
  if(input->token.kind == Token_semicolon)
    success = get_next_token(input);
  else
    success = compile_error(&input->src_loc, "expected `;`, actual `%s`", get_token_printstr(&input->token));
  return success;
}

bool parse_rest_of_type_expr(TokenStream* input, AstNode* expr, AstNode** node)
{
  *node = expr;
  bool success = true;
  
  if(input->token.kind == Token_star)
  {
    AstNode* ptr = *node = new_ast_node(Ast_gen0, AstNode_pointer, clone_source_loc(&input->src_loc));
    ATTR(ptr, ast_node, type_expr) = expr;

    success = get_next_token(input) && parse_rest_of_type_expr(input, *node, node);
  }
  return success;
}

bool parse_type_expr(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_open_bracket)
  {
    AstNode* array = *node = new_ast_node(Ast_gen0, AstNode_array, clone_source_loc(&input->src_loc));

    if(success = get_next_token(input) && parse_expr(input, &ATTR(array, ast_node, size_expr)))
    {
      if(input->token.kind == Token_close_bracket)
      {
#if 0
        if(ATTR(array, ast_node, size_expr))
        {
#endif
          if(success = get_next_token(input) && parse_type_expr(input, &ATTR(array, ast_node, type_expr)))
          {
            if(!ATTR(array, ast_node, type_expr))
            {
              putback_token(input);
              success = compile_error(&input->src_loc, "type expression required, at `%s`", get_token_printstr(&input->token));
            }
          }
#if 0
        }
        else
          success = compile_error(&input->src_loc, "[] : expression required between brackets");
#endif
      }
      else
        success = compile_error(&input->src_loc,  "expected `]`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  else if(input->token.kind == Token_id)
  {
    AstNode* id = *node = new_ast_node(Ast_gen0, AstNode_id, clone_source_loc(&input->src_loc));
    ATTR(id, str, name) = input->token.lexeme;

    success = get_next_token(input) && parse_rest_of_type_expr(input, *node, node);
  }
  else if(input->token.kind == Token_open_parens)
  {
    if(success = get_next_token(input) && parse_type_expr(input, node))
    {
      if(*node)
      {
        if(input->token.kind == Token_close_parens)
        {
          success = get_next_token(input) && parse_rest_of_type_expr(input, *node, node);
        }
        else
          success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
      }
      else
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "type expression expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }

  return success;
}

bool parse_type(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* type_expr = 0;
  if(success = parse_type_expr(input, &type_expr))
  {
    if(type_expr)
    {
      AstNode* type = *node = new_ast_node(Ast_gen0, AstNode_type, clone_source_loc(&input->src_loc));
      ATTR(type, ast_node, type_expr) = type_expr;
    }
  }
  return success;
}


bool parse_initializer_member_list(TokenStream* input, List* member_list)
{
  bool success = true;

  AstNode* member = 0;
  do
  {
    member = 0;
    if(input->token.kind == Token_open_brace)
    {
      if(success = parse_initializer_list(input, &member))
      {
        if(input->token.kind == Token_comma)
          success = get_next_token(input);
      }
    }
    if(success)
    {
      if(!member)
        success = parse_expr(input, &member);

      if(success && member)
      {
        append_list_elem(member_list, member, List_ast_node);
        if(input->token.kind == Token_comma)
          success = get_next_token(input);
      }
    }
  }
  while(success && member);
  return success;
}

bool parse_initializer_list(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_open_brace)
  {
    AstNode* init_list = *node = new_ast_node(Ast_gen0, AstNode_init_list, clone_source_loc(&input->src_loc));
    ATTR(init_list, list, members) = new_list(arena, List_ast_node);

    if(success = get_next_token(input) && parse_initializer_member_list(input, ATTR(init_list, list, members)))
    {
      if(input->token.kind == Token_close_brace)
        success = get_next_token(input);
      else
        success = compile_error(&input->src_loc, "expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

bool parse_actual_arg_list(TokenStream* input, List* args)
{
  bool success = true;

  AstNode* arg = 0;
  do
  {
    arg = 0;
    success = parse_expr(input, &arg);

    if(success && arg)
    {
      append_list_elem(args, arg, List_ast_node);
      if(input->token.kind == Token_comma)
      {
        if((success = get_next_token(input)) && input->token.kind == Token_close_parens)
          success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
      }
    }
  }
  while(success && arg);
  return success;
}

bool parse_node_list(TokenStream* input, List* node_list)
{
  bool success = true;

  AstNode* node = 0;
  do
  {
    node = 0;
    while(input->token.kind == Token_semicolon && (success = get_next_token(input)))
      ; // skip

    if((success = parse_node(input, &node)) && node)
    {
      append_list_elem(node_list, node, List_ast_node);
    }
  }
  while(success && node);

  return success;
}

bool parse_block(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_open_brace)
  {
    AstNode* block = *node = new_ast_node(Ast_gen0, AstNode_block, clone_source_loc(&input->src_loc));
    ATTR(block, list, nodes) = new_list(arena, List_ast_node);

    if(success = get_next_token(input) && parse_node_list(input, ATTR(block, list, nodes)))
    {
      if(input->token.kind == Token_close_brace)
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

bool parse_rest_of_id(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == Token_open_parens)
  {
    // procedure call
    if(left_node->kind == AstNode_id)
    {
      AstNode* proc_occur = *node = new_ast_node(Ast_gen0, AstNode_proc_occur, clone_source_loc(&input->src_loc));
      ATTR(proc_occur, ast_node, id) = left_node;
      ATTR(proc_occur, list, actual_args) = new_list(arena, List_ast_node);

      if(success = get_next_token(input) && parse_actual_arg_list(input, ATTR(proc_occur, list, actual_args)))
      {
        if(input->token.kind == Token_close_parens)
          success = get_next_token(input);
        else
          success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
      }
    }
    else
      success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  else if(input->token.kind == Token_open_bracket)
  {
    // array
    AstNode* index = *node = new_ast_node(Ast_gen0, AstNode_bin_expr, clone_source_loc(&input->src_loc));
    ATTR(index, op_kind, op_kind) = OperatorKind_array_index;
    ATTR(index, ast_node, left_operand) = left_node;

    if(success = get_next_token(input) && parse_expr(input, &ATTR(index, ast_node, right_operand)))
    {
      if(input->token.kind == Token_close_bracket)
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

bool parse_rest_of_selector(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == Token_dot ||
     input->token.kind == Token_arrow_right)
  {
    AstNode* bin_expr = *node = new_ast_node(Ast_gen0, AstNode_bin_expr, clone_source_loc(&input->src_loc));
    ATTR(bin_expr, ast_node, left_operand) = left_node;

    if(input->token.kind == Token_dot)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_member_select;
    else if(input->token.kind == Token_arrow_right)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_ptr_member_select;

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
  else if(input->token.kind == Token_plus_plus ||
          input->token.kind == Token_minus_minus)
  {
    AstNode* un_expr = *node = new_ast_node(Ast_gen0, AstNode_un_expr, clone_source_loc(&input->src_loc));
    ATTR(un_expr, ast_node, operand) = left_node;

    if(input->token.kind == Token_minus_minus)
    {
#if 0
      un_expr->op = OperatorKin_PostDecrement;
#else
      success = compile_error(&input->src_loc, "`--` not supported");
#endif
    }
    else if(input->token.kind == Token_plus_plus)
    {
#if 0
      un_expr->op = OperatorKind_PostIncrement;
#else
      success = compile_error(&input->src_loc, "`++` not supported");
#endif
    }
    else
      assert(0);

    success = success && get_next_token(input);
  }

  return success;
}

bool parse_factor(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = parse_un_expr(input, node)) && *node)
    success = parse_rest_of_selector(input, *node, node);
  return success;
}

bool parse_rest_of_factor(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == Token_star ||
     input->token.kind == Token_fwd_slash ||
     input->token.kind == Token_percent)
  {
    AstNode* bin_expr = *node = new_ast_node(Ast_gen0, AstNode_bin_expr, clone_source_loc(&input->src_loc));
    ATTR(bin_expr, ast_node, left_operand) = left_node;

    if(input->token.kind == Token_star)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_mul;
    else if(input->token.kind == Token_fwd_slash)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_div;
    else if(input->token.kind == Token_percent)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_mod;
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

bool parse_term(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = parse_factor(input, node)) && *node)
    success = parse_rest_of_factor(input, *node, node);
  return success;
}

bool parse_rest_of_term(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == Token_plus ||
     input->token.kind == Token_minus ||
     input->token.kind == Token_pipe ||
     input->token.kind == Token_pipe_pipe ||
     input->token.kind == Token_ampersand ||
     input->token.kind == Token_ampersand_ampersand)
  {
    AstNode* bin_expr = *node = new_ast_node(Ast_gen0, AstNode_bin_expr, clone_source_loc(&input->src_loc));
    ATTR(bin_expr, ast_node, left_operand) = left_node;

    if(input->token.kind == Token_plus)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_add;
    else if(input->token.kind == Token_minus)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_sub;
    else if(input->token.kind == Token_pipe)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_bit_or;
    else if(input->token.kind == Token_pipe_pipe)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_logic_or;
    else if(input->token.kind == Token_ampersand)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_bit_and;
    else if(input->token.kind == Token_ampersand_ampersand)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_logic_and;
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

bool parse_assignment(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = parse_term(input, node)) && *node)
    success = parse_rest_of_term(input, *node, node);
  return success;
}

bool parse_rest_of_assignment(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == Token_eq ||
     input->token.kind == Token_eq_eq ||
     input->token.kind == Token_exclam_eq ||
     input->token.kind == Token_angle_left ||
     input->token.kind == Token_angle_left_eq ||
     input->token.kind == Token_angle_right ||
     input->token.kind == Token_angle_right_eq)
  {
    AstNode* bin_expr = *node = new_ast_node(Ast_gen0, AstNode_bin_expr, clone_source_loc(&input->src_loc));
    ATTR(bin_expr, ast_node, left_operand) = left_node;

    if(input->token.kind == Token_eq)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_assign;
    else if(input->token.kind == Token_eq_eq)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_eq;
    else if(input->token.kind == Token_exclam_eq)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_not_eq;
    else if(input->token.kind == Token_angle_left)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_less;
    else if(input->token.kind == Token_angle_left_eq)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_less_eq;
    else if(input->token.kind == Token_angle_right)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_greater;
    else if(input->token.kind == Token_angle_right_eq)
      ATTR(bin_expr, op_kind, op_kind) = OperatorKind_greater_eq;

    if(success = get_next_token(input) && parse_expr(input, &ATTR(bin_expr, ast_node, right_operand)))
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

#if 0
bool
parse_new(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = false;

  if(input->token.kind == Token_new && (success = get_next_token(input)))
  {
    if(input->token.kind == Token_open_parens)
    {
      AstNode* new_proc = *node = new_ast_node(Ast_gen0, AstNode_new_proc, clone_source_loc(&input->src_loc));

      if(success = get_next_token(input) && parse_type(input, &ATTR(new_proc, ast_node, type)))
      {
        if(input->token.kind == Token_comma)
        {
          if(success = get_next_token(input) && parse_expr(input, &ATTR(new_proc, ast_node, count_expr)))
          {
            if(input->token.kind == Token_close_parens)
            {
              if(ATTR(new_proc, ast_node, count_expr))
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

bool parse_putc(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = false;

  if(input->token.kind == Token_putc && (success = get_next_token(input)))
  {
    if(input->token.kind == Token_open_parens)
    {
      AstNode* putc_proc = *node = new_ast_node(Ast_gen0, AstNode_putc_proc, clone_source_loc(&input->src_loc));

      if(success = get_next_token(input) && parse_expr(input, &ATTR(putc_proc, ast_node, expr)))
      {
        if(input->token.kind == Token_close_parens)
        {
          if(ATTR(putc_proc, ast_node, expr))
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
#endif

bool parse_selector(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_open_parens)
  {
    if(success = get_next_token(input) && parse_expr(input, node))
    {
      AstNode* expr = *node;
      if(expr)
      {
        if(input->token.kind == Token_close_parens)
        {
          if((success = get_next_token(input)) && expr->kind == AstNode_type)
          {
            AstNode* cast = *node = new_ast_node(Ast_gen0, AstNode_bin_expr, expr->src_loc);
            ATTR(cast, op_kind, op_kind) = OperatorKind_cast;
            ATTR(cast, ast_node, left_operand) = expr;

            if(success = parse_expr(input, &ATTR(cast, ast_node, right_operand)))
            {
              if(!ATTR(cast, ast_node, right_operand))
              {
                putback_token(input);
                success = compile_error(&input->src_loc, "expression expected, at `%s`", get_token_printstr(&input->token));
              }
            }
          }
        }
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
  else if(is_literal_token(input->token.kind)
          || input->token.kind == Token_true
          || input->token.kind == Token_false)
  {
    AstNode* lit = *node = new_ast_node(Ast_gen0, AstNode_lit, clone_source_loc(&input->src_loc));

    if(input->token.kind == Token_int_num)
    {
      ATTR(lit, lit_kind, lit_kind) = Literal_int_val;
      ATTR(lit, int_val, int_val) = *input->token.int_val;
    }
    else if(input->token.kind == Token_float_num)
    {
      ATTR(lit, lit_kind, lit_kind) = Literal_float_val;
      ATTR(lit, float_val, float_val) = *input->token.float_val;
    }
    else if(input->token.kind == Token_true ||
            input->token.kind == Token_false)
    {
      ATTR(lit, lit_kind, lit_kind) = Literal_bool_val;
      ATTR(lit, bool_val, bool_val) = (input->token.kind == Token_true ? 1 : 0);
    }
    else if(input->token.kind == Token_char)
    {
      ATTR(lit, lit_kind, lit_kind) = Literal_char_val;
      ATTR(lit, char_val, char_val) = input->token.char_val;
    }
    else if(input->token.kind == Token_string)
    {
      ATTR(lit, lit_kind, lit_kind) = Literal_str;
      ATTR(lit, str, str) = input->token.str;
    }
    else
      assert(0);

    success = get_next_token(input);
  }
  else if(input->token.kind == Token_id)
  {
    AstNode* id = *node = new_ast_node(Ast_gen0, AstNode_id, clone_source_loc(&input->src_loc));
    ATTR(id, str, name) = input->token.lexeme;
    success = get_next_token(input) && parse_rest_of_id(input, *node, node);
  }
  else if(input->token.kind == Token_type)
  {
    if(success = get_next_token(input) && parse_type(input, node))
    {
      if(!*node)
        success = compile_error(&input->src_loc, "type expression required, at `%s`", get_token_printstr(&input->token));
    }
  }
#if 0
  else if(input->token.kind == Token_new)
    success = parse_new(input, node);
  else if(input->token.kind == Token_putc)
    success = parse_putc(input, node);
#endif

  return success;
}

bool parse_formal_arg(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* type = 0;
  if((success = parse_type(input, &type)) && type)
  {
    AstNode* var_decl = *node = new_ast_node(Ast_gen0, AstNode_var_decl, clone_source_loc(&input->src_loc));
    ATTR(var_decl, ast_node, type) = type;

    if(input->token.kind == Token_id)
    {
      AstNode* id = ATTR(var_decl, ast_node, id) = new_ast_node(Ast_gen0, AstNode_id, clone_source_loc(&input->src_loc));
      ATTR(id, str, name) = input->token.lexeme;
      success = get_next_token(input);
    }
    else
      success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

bool parse_formal_arg_list(TokenStream* input, List* arg_list)
{
  bool success = true;

  AstNode* arg = 0;
  do
  {
    arg = 0;
    if((success = parse_formal_arg(input, &arg)) && arg)
    {
      append_list_elem(arg_list, arg, List_ast_node);
      if(input->token.kind == Token_comma)
      {
        success = get_next_token(input);
      }
      else if(input->token.kind != Token_close_parens)
      {
        success = compile_error(&input->src_loc, "expected `,`, actual `%s`", get_token_printstr(&input->token));
      }
    }
  }
  while(success && arg);
  return success;
}

bool parse_un_expr(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_exclam ||
     input->token.kind == Token_star ||
     input->token.kind == Token_ampersand ||
     input->token.kind == Token_minus ||
     input->token.kind == Token_minus_minus ||
     input->token.kind == Token_plus_plus)
  {
    AstNode* un_expr = *node = new_ast_node(Ast_gen0, AstNode_un_expr, clone_source_loc(&input->src_loc));

    if(input->token.kind == Token_exclam)
      ATTR(un_expr, op_kind, op_kind) = OperatorKind_logic_not;
    else if(input->token.kind == Token_star)
      ATTR(un_expr, op_kind, op_kind) = OperatorKind_ptr_deref;
    else if(input->token.kind == Token_ampersand)
      ATTR(un_expr, op_kind, op_kind) = OperatorKind_address_of;
    else if(input->token.kind == Token_minus)
      ATTR(un_expr, op_kind, op_kind) = OperatorKind_neg;
    else if(input->token.kind == Token_minus_minus)
    {
#if 0
      un_expr->op = OperatorKind_PreDecrement;
#else
      success = compile_error(&input->src_loc, "`--` not supported");
#endif
    }
    else if(input->token.kind == Token_plus_plus)
    {
#if 0
      un_expr->op = OperatorKind_PreIncrement;
#else
      success = compile_error(&input->src_loc, "`++` not supported");
#endif
    }
    else
      assert(0);

    if(success && (success = get_next_token(input) && parse_factor(input, &ATTR(un_expr, ast_node, operand))))
    {
      if(!ATTR(un_expr, ast_node, operand))
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else
    success = parse_selector(input, node);

  return success;
}

bool parse_expr(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = parse_assignment(input, node)) && *node)
    success = parse_rest_of_assignment(input, *node, node);
  return success;
}

bool parse_var(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* type = 0;
  if(success = parse_type(input, &type))
  {
    if(type)
    {
      AstNode* var_decl = *node = new_ast_node(Ast_gen0, AstNode_var_decl, clone_source_loc(&input->src_loc));
      ATTR(var_decl, ast_node, type) = type;

      if(input->token.kind == Token_id)
      {
        AstNode* id = ATTR(var_decl, ast_node, id) = new_ast_node(Ast_gen0, AstNode_id, clone_source_loc(&input->src_loc));
        ATTR(id, str, name) = input->token.lexeme;

        if((success = get_next_token(input)) && input->token.kind == Token_eq
            && (success = get_next_token(input)))
        {
#if 0
          AstNode* init_expr = ATTR(var_decl, ast_node, init_expr) = new_ast_node(Ast_gen0, AstNode_bin_expr, clone_source_loc(&input->src_loc));
          ATTR(init_expr, op_kind, op_kind) = OperatorKind_assign;
          ATTR(init_expr, ast_node, left_operand) = id;
#endif

          AstNode* init_expr = 0;
          if(success = parse_initializer_list(input, &init_expr))
          {
            if(!init_expr && (success = parse_expr(input, &init_expr)))
            {
              if(init_expr)
              {
                ATTR(var_decl, ast_node, init_expr) = init_expr;
              }
              else
              {
                putback_token(input);
                success = compile_error(&input->src_loc, "expression required, at `%s`", get_token_printstr(&input->token));
              }
            }
          }
        }
      }
      else
        success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
    }
    else
      success = compile_error(&input->src_loc, "type expression required, at `%s`", get_token_printstr(&input->token));
  }
  return success;
}

#if 0
bool parse_for(TokenStream* input, AstNode1** node)
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
      success = get_next_token(input) && parse_var(input, &for_stmt->decl_expr)
        && consume_semicolon(input)
        && parse_expr(input, &for_stmt->cond_expr)
        && consume_semicolon(input)
        && parse_expr(input, &for_stmt->loop_expr);
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

bool parse_while(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_while)
  {
    AstNode* while_stmt = *node = new_ast_node(Ast_gen0, AstNode_while_stmt, clone_source_loc(&input->src_loc));

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == Token_open_parens)
    {
      success = get_next_token(input) && parse_expr(input, &ATTR(while_stmt, ast_node, cond_expr));
      if(!success)
        return success;

      if(ATTR(while_stmt, ast_node, cond_expr))
      {
        if(input->token.kind == Token_close_parens)
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

bool parse_else(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_else)
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

bool parse_if(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_if)
  {
    AstNode* if_stmt = *node = new_ast_node(Ast_gen0, AstNode_if_stmt, clone_source_loc(&input->src_loc));

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == Token_open_parens)
    {
      success = get_next_token(input) && parse_expr(input, &ATTR(if_stmt, ast_node, cond_expr));
      if(!success)
        return success;

      if(input->token.kind == Token_close_parens)
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

bool parse_proc(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* ret_type = 0;
  if(success = parse_type(input, &ret_type))
  {
    if(ret_type)
    {
      AstNode* proc_decl = *node = new_ast_node(Ast_gen0, AstNode_proc_decl, clone_source_loc(&input->src_loc));
      ATTR(proc_decl, ast_node, ret_type) = ret_type;

      if(input->token.kind == Token_id)
      {
        AstNode* id = ATTR(proc_decl, ast_node, id) = new_ast_node(Ast_gen0, AstNode_id, clone_source_loc(&input->src_loc));
        ATTR(id, str, name) = input->token.lexeme;

        if(!(success = get_next_token(input)))
          return success;

        if(input->token.kind == Token_open_parens)
        {
          if(!(success = get_next_token(input)))
            return success;

          ATTR(proc_decl, list, formal_args) = new_list(arena, List_ast_node);
          if(success = parse_formal_arg_list(input, ATTR(proc_decl, list, formal_args)))
          {
            if(input->token.kind == Token_close_parens)
            {
#if 0
              if(success = get_next_token(input) && parse_block(input, &proc_decl->body))
              {
                if(!proc_decl->body && (proc_decl->is_decl = success = consume_semicolon(input)))
                  proc_decl->body = (AstNode1*)new_ast1_block(&(*node)->src_loc);
              }
#else
              if(success = get_next_token(input) && parse_block(input, &ATTR(proc_decl, ast_node, body)))
              {
                if(!ATTR(proc_decl, ast_node, body))
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
      success = compile_error(&input->src_loc, "type expression required, at `%s`", get_token_printstr(&input->token));
  }
  return success;
}

bool parse_include(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* include = *node = new_ast_node(Ast_gen0, AstNode_include, clone_source_loc(&input->src_loc));

  if(input->token.kind == Token_string)
  {

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
        AstNode* block = ATTR(include, ast_node, body) = new_ast_node(Ast_gen0, AstNode_block, clone_source_loc(&incl_input->src_loc));
        ATTR(block, list, nodes) = new_list(arena, List_ast_node);
        success = parse_node_list(incl_input, ATTR(block, list, nodes));
      }
    }
    else
      success = compile_error(&input->src_loc, "could not read file `%s`", ATTR(include, str, file_path));
  }
  else
    success = compile_error(&input->src_loc, "string expected, actual `%s`", get_token_printstr(&input->token));
  return success;
}

bool parse_enum(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_enum)
  {
    AstNode* enum_decl = *node = new_ast_node(Ast_gen0, AstNode_enum_decl, clone_source_loc(&input->src_loc));

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == Token_id)
    {
      AstNode* id = ATTR(enum_decl, ast_node, id) = new_ast_node(Ast_gen0, AstNode_id, clone_source_loc(&input->src_loc));
      ATTR(id, str, name) = input->token.lexeme;

      if(!(success = get_next_token(input)))
        return success;

      if(input->token.kind == Token_open_brace)
      {

        if(!(success = get_next_token(input)))
          return success;

        ATTR(enum_decl, list, members) = new_list(arena, List_ast_node);
        AstNode* member = 0;
        do
        {
          member = 0;
          if(input->token.kind == Token_id)
          {
            member = new_ast_node(Ast_gen0, AstNode_id, clone_source_loc(&input->src_loc));
            ATTR(member, str, name) = input->token.lexeme;
            append_list_elem(ATTR(enum_decl, list, members), member, List_ast_node);

            if((success = get_next_token(input)) && input->token.kind == Token_comma)
            {
              success = get_next_token(input);
            }
            else if(input->token.kind != Token_close_brace)
              member = 0;
          }
        }
        while(member && success);

        if(input->token.kind == Token_close_brace)
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

bool parse_union(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_union)
  {
    AstNode* union_decl = *node = new_ast_node(Ast_gen0, AstNode_union_decl, clone_source_loc(&input->src_loc));

    if((success = get_next_token(input)) && input->token.kind == Token_id)
    {
      AstNode* id = ATTR(union_decl, ast_node, id) = new_ast_node(Ast_gen0, AstNode_id, clone_source_loc(&input->src_loc));
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

bool parse_struct(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_struct)
  {
    AstNode* struct_decl = *node = new_ast_node(Ast_gen0, AstNode_struct_decl, clone_source_loc(&input->src_loc));

    if((success = get_next_token(input)) && input->token.kind == Token_id)
    {
      AstNode* id = ATTR(struct_decl, ast_node, id) = new_ast_node(Ast_gen0, AstNode_id, clone_source_loc(&input->src_loc));
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

bool parse_struct_member_list(TokenStream* input, List* member_list)
{
  bool success = true;

  if(input->token.kind == Token_open_brace)
  {
    if(!(success = get_next_token(input)))
      return success;

    AstNode* member = 0;
    do
    {
      member = 0;

      AstNode* type = 0;
      if(success = parse_type(input, &type))
      {
        if(!type)
        {
          if(input->token.kind == Token_union)
          {
            success = parse_union(input, &type);
          }
          else if(input->token.kind == Token_struct)
          {
            success = parse_struct(input, &type);
          }
        }

        if(success && type)
        {
          AstNode* var_decl = member = new_ast_node(Ast_gen0, AstNode_var_decl, clone_source_loc(&input->src_loc));
          ATTR(var_decl, ast_node, type) = type;

          if(input->token.kind == Token_id)
          {
            AstNode* id = ATTR(var_decl, ast_node, id) = new_ast_node(Ast_gen0, AstNode_id, clone_source_loc(&input->src_loc));
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
            append_list_elem(member_list, member, List_ast_node);
            success = consume_semicolon(input);
          }
        }
      }
    }
    while(member && success);

    if(success)
    {
      if(input->token.kind == Token_close_brace)
        success = get_next_token(input);
      else
        success = compile_error(&input->src_loc, "expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  else
    success = compile_error(&input->src_loc, "expected `{`, actual `%s`", get_token_printstr(&input->token));
  return success;
}

bool parse_return(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_return)
  {
    AstNode* return_stmt = *node = new_ast_node(Ast_gen0, AstNode_return_stmt, clone_source_loc(&input->src_loc));
    success = get_next_token(input) && parse_expr(input, &ATTR(return_stmt, ast_node, ret_expr));
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

bool parse_continue(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_continue)
  {
    *node = new_ast_node(Ast_gen0, AstNode_continue_stmt, clone_source_loc(&input->src_loc));
    success = get_next_token(input);
  }
  return success;
}

bool parse_break(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_break)
  {
    *node = new_ast_node(Ast_gen0, AstNode_break_stmt, clone_source_loc(&input->src_loc));
    success = get_next_token(input);
  }
  return success;
}

bool parse_node(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == Token_var)
  {
    success = get_next_token(input) && parse_var(input, node) && consume_semicolon(input);
  }
  else if(input->token.kind == Token_include)
  {
    success = get_next_token(input) && parse_include(input, node) && consume_semicolon(input);
  }
  else if(input->token.kind == Token_proc)
  {
    success = get_next_token(input) && parse_proc(input, node);
  }
  else if(input->token.kind == Token_if)
  {
    if(success = parse_if(input, node))
    {
      AstNode* stmt = *node;
      ATTR(*node = new_ast_node(Ast_gen0, AstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
    }
  }
  else if(input->token.kind == Token_else)
  {
    success = compile_error(&input->src_loc, "unmatched `else`");
  }
  else if(input->token.kind == Token_while)
  {
    if(success = parse_while(input, node))
    {
      AstNode* stmt = *node;
      ATTR(*node = new_ast_node(Ast_gen0, AstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
    }
  }
#if 0
  else if(input->token.kind == Token_for)
  {
    success = parse_for(input, &node);
  }
#endif
  else if(input->token.kind == Token_return)
  {
    if((success = parse_return(input, node)) && consume_semicolon(input))
    {
      AstNode* stmt = *node;
      ATTR(*node = new_ast_node(Ast_gen0, AstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
    }
  }
  else if(input->token.kind == Token_break)
  {
    if((success = parse_break(input, node)) && consume_semicolon(input))
    {
      AstNode* stmt = *node;
      ATTR(*node = new_ast_node(Ast_gen0, AstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
    }
  }
  else if(input->token.kind == Token_continue)
  {
    if((success = parse_continue(input, node)) && consume_semicolon(input))
    {
      AstNode* stmt = *node;
      ATTR(*node = new_ast_node(Ast_gen0, AstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
    }
  }
#if 0
  else if(input->token.kind == Token_goto)
  {
    success = parse_goto(input, &node) && consume_semicolon(input);
  }
#endif
  else if(input->token.kind == Token_semicolon)
  {
    if(success = consume_semicolon(input))
    {
      *node = new_ast_node(Ast_gen0, AstNode_stmt, clone_source_loc(&input->src_loc));
    }
  }
  else if(input->token.kind == Token_open_brace)
  {
    if(success = parse_block(input, node))
    {
      AstNode* stmt = *node;
      ATTR(*node = new_ast_node(Ast_gen0, AstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
    }
  }
  else
  {
    if(success = parse_expr(input, node))
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
          ATTR(*node = new_ast_node(Ast_gen0, AstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
        }
#endif
      }
    }
  }

  return success;
}

bool parse(TokenStream* input, AstNode** node)
{
  bool success = true;

  AstNode* module = *node = new_ast_node(Ast_gen0, AstNode_module, clone_source_loc(&input->src_loc));
  ATTR(module, str, file_path) = input->src_loc.file_path;
  AstNode* block = ATTR(module, ast_node, body) = new_ast_node(Ast_gen0, AstNode_block, clone_source_loc(&input->src_loc));
  ATTR(block, list, nodes) = new_list(arena, List_ast_node);

  if((success = parse_node_list(input, ATTR(block, list, nodes)))
      && input->token.kind != Token_end_of_input)
  {
    success = compile_error(&input->src_loc, "expected `end-of-input`, at `%s`", get_token_printstr(&input->token));
  }

  return success;
}

