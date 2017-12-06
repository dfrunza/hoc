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
  if(input->token.kind == eToken_semicolon)
  {
    success = get_next_token(input);
  }
  else
    success = compile_error(&input->src_loc, "expected `;`, actual `%s`", get_token_printstr(&input->token));
  return success;
}

bool parse_rest_of_type_expr(TokenStream* input, AstNode* expr, AstNode** node)
{
  *node = expr;
  bool success = true;
  
  if(input->token.kind == eToken_star)
  {
    AstNode* ptr = *node = new_ast_node(eAstGen_gen0, eAstNode_pointer, clone_source_loc(&input->src_loc));
    ATTR(ptr, ast_node, pointee_expr) = expr;

    success = get_next_token(input) && parse_rest_of_type_expr(input, *node, node);
  }
  return success;
}

bool parse_type_expr(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(input->token.kind)
  {
    case eToken_open_bracket:
      {
        AstNode* array = *node = new_ast_node(eAstGen_gen0, eAstNode_array, clone_source_loc(&input->src_loc));

        AstNode* size_expr = 0;
        if(success = get_next_token(input) && parse_expr(input, &size_expr))
        {
          ATTR(array, ast_node, size_expr) = size_expr;
          if(input->token.kind == eToken_close_bracket)
          {
            AstNode* elem_expr = 0;
            if(success = get_next_token(input) && parse_type_expr(input, &elem_expr))
            {
              ATTR(array, ast_node, elem_expr) = elem_expr;
              if(!elem_expr)
              {
                putback_token(input);
                success = compile_error(&input->src_loc, "type expression required, at `%s`", get_token_printstr(&input->token));
              }
            }
          }
          else
            success = compile_error(&input->src_loc,  "expected `]`, actual `%s`", get_token_printstr(&input->token));
        }
      }
      break;

    case eToken_id:
      {
        AstNode* id = *node = new_ast_node(eAstGen_gen0, eAstNode_id, clone_source_loc(&input->src_loc));
        ATTR(id, str_val, name) = input->token.lexeme;

        success = get_next_token(input) && parse_rest_of_type_expr(input, *node, node);
      }
      break;

    case eToken_open_parens:
      {
        if(success = get_next_token(input) && parse_type_expr(input, node))
        {
          if(*node)
          {
            if(input->token.kind == eToken_close_parens)
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
      break;
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
      AstNode* type = *node = new_ast_node(eAstGen_gen0, eAstNode_type_decl, clone_source_loc(&input->src_loc));
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
    if(input->token.kind == eToken_open_brace)
    {
      if(success = parse_initializer_list(input, &member))
      {
        if(input->token.kind == eToken_comma)
        {
          success = get_next_token(input);
        }
      }
    }
    if(success)
    {
      if(!member)
      {
        success = parse_expr(input, &member);
      }

      if(success && member)
      {
        append_list_elem(member_list, member, eList_ast_node);
        if(input->token.kind == eToken_comma)
        {
          success = get_next_token(input);
        }
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

  if(input->token.kind == eToken_open_brace)
  {
    AstNode* init_list = *node = new_ast_node(eAstGen_gen0, eAstNode_init_list, clone_source_loc(&input->src_loc));
    ATTR(init_list, list, members) = new_list(arena, eList_ast_node);

    if(success = get_next_token(input) && parse_initializer_member_list(input, ATTR(init_list, list, members)))
    {
      if(input->token.kind == eToken_close_brace)
      {
        success = get_next_token(input);
      }
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
      append_list_elem(args, arg, eList_ast_node);
      if(input->token.kind == eToken_comma)
      {
        if((success = get_next_token(input)) && input->token.kind == eToken_close_parens)
        {
          success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
        }
      }
    }
  }
  while(success && arg);
  return success;
}

bool parse_node_list(TokenStream* input, List* nodes)
{
  bool success = true;

  AstNode* node = 0;
  do
  {
    node = 0;
    while(input->token.kind == eToken_semicolon && (success = get_next_token(input)))
      ; // skip

    if((success = parse_node(input, &node)) && node)
    {
      append_list_elem(nodes, node, eList_ast_node);
    }
  }
  while(success && node);

  return success;
}

bool parse_block(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == eToken_open_brace)
  {
    AstNode* block = *node = new_ast_node(eAstGen_gen0, eAstNode_block, clone_source_loc(&input->src_loc));
    List* nodes = ATTR(block, list, nodes) = new_list(arena, eList_ast_node);

    if(success = get_next_token(input) && parse_node_list(input, nodes))
    {
      if(input->token.kind == eToken_close_brace)
      {
        success = get_next_token(input);
      }
      else
        success = compile_error(&input->src_loc, "expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

bool parse_rest_of_id(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(input->token.kind)
  {
    case eToken_open_parens:
      {
        // procedure call
        if(left_node->kind == eAstNode_id)
        {
          AstNode* proc_occur = *node = new_ast_node(eAstGen_gen0, eAstNode_proc_occur, clone_source_loc(&input->src_loc));
          ATTR(proc_occur, ast_node, id) = left_node;
          ATTR(proc_occur, list, actual_args) = new_list(arena, eList_ast_node);

          if(success = get_next_token(input) && parse_actual_arg_list(input, ATTR(proc_occur, list, actual_args)))
          {
            if(input->token.kind == eToken_close_parens)
            {
              success = get_next_token(input);
            }
            else
              success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
          }
        }
        else
          success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
      }
      break;

    case eToken_open_bracket:
      {
        // array
        AstNode* index = *node = new_ast_node(eAstGen_gen0, eAstNode_bin_expr, clone_source_loc(&input->src_loc));
        ATTR(index, op_kind, op_kind) = eOperator_index;
        ATTR(index, ast_node, left_operand) = left_node;

        if(success = get_next_token(input) && parse_expr(input, &ATTR(index, ast_node, right_operand)))
        {
          if(input->token.kind == eToken_close_bracket)
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
          {
            success = compile_error(&input->src_loc, "expected `]`, actual `%s`", get_token_printstr(&input->token));
          }
        }
      }
      break;
  }
  return success;
}

bool parse_rest_of_selector(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(input->token.kind)
  {
    case eToken_dot:
    case eToken_arrow_right:
      {
        AstNode* bin_expr = *node = new_ast_node(eAstGen_gen0, eAstNode_bin_expr, clone_source_loc(&input->src_loc));
        ATTR(bin_expr, ast_node, left_operand) = left_node;

        if(input->token.kind == eToken_dot)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_member_select;
        }
        else if(input->token.kind == eToken_arrow_right)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_ptr_member_select;
        }

        if(success = get_next_token(input) && parse_selector(input, &ATTR(bin_expr, ast_node, right_operand)))
        {
          if(ATTR(bin_expr, ast_node, right_operand))
          {
            success = parse_rest_of_selector(input, *node, node);
          }
          else
          {
            putback_token(input);
            success = compile_error(&input->src_loc, "operand expected, at `%s`", get_token_printstr(&input->token));
          }
        }
      }
      break;

    case eToken_plus_plus:
    case eToken_minus_minus:
      {
        AstNode* un_expr = *node = new_ast_node(eAstGen_gen0, eAstNode_un_expr, clone_source_loc(&input->src_loc));
        ATTR(un_expr, ast_node, operand) = left_node;

        if(input->token.kind == eToken_minus_minus)
        {
#if 0
          un_expr->op = eOperator_PostDecrement;
#else
          success = compile_error(&input->src_loc, "`--` not supported");
#endif
        }
        else if(input->token.kind == eToken_plus_plus)
        {
#if 0
          un_expr->op = eOperator_PostIncrement;
#else
          success = compile_error(&input->src_loc, "`++` not supported");
#endif
        }
        else
          assert(0);

        success = success && get_next_token(input);
      }
      break;
  }
  return success;
}

bool parse_factor(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = parse_un_expr(input, node)) && *node)
  {
    success = parse_rest_of_selector(input, *node, node);
  }
  return success;
}

bool parse_rest_of_factor(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(input->token.kind)
  {
    case eToken_star:
    case eToken_fwd_slash:
    case eToken_percent:
      {
        AstNode* bin_expr = *node = new_ast_node(eAstGen_gen0, eAstNode_bin_expr, clone_source_loc(&input->src_loc));
        ATTR(bin_expr, ast_node, left_operand) = left_node;

        if(input->token.kind == eToken_star)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_mul;
        }
        else if(input->token.kind == eToken_fwd_slash)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_div;
        }
        else if(input->token.kind == eToken_percent)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_mod;
        }
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
      break;
  }
  return success;
}

bool parse_term(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = parse_factor(input, node)) && *node)
  {
    success = parse_rest_of_factor(input, *node, node);
  }
  return success;
}

bool parse_rest_of_term(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(input->token.kind)
  {
    case eToken_plus:
    case eToken_minus:
    case eToken_pipe:
    case eToken_pipe_pipe:
    case eToken_ampersand:
    case eToken_ampersand_ampersand:
    case eToken_angle_right_right:
    case eToken_angle_left_left:
    case eToken_circumflex:
      {
        AstNode* bin_expr = *node = new_ast_node(eAstGen_gen0, eAstNode_bin_expr, clone_source_loc(&input->src_loc));
        ATTR(bin_expr, ast_node, left_operand) = left_node;

        if(input->token.kind == eToken_plus)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_add;
        }
        else if(input->token.kind == eToken_minus)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_sub;
        }
        else if(input->token.kind == eToken_pipe)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_bit_or;
        }
        else if(input->token.kind == eToken_pipe_pipe)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_logic_or;
        }
        else if(input->token.kind == eToken_ampersand)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_bit_and;
        }
        else if(input->token.kind == eToken_ampersand_ampersand)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_logic_and;
        }
        else if(input->token.kind == eToken_angle_left_left)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_bit_shift_left;
        }
        else if(input->token.kind == eToken_angle_right_right)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_bit_shift_right;
        }
        else if(input->token.kind == eToken_circumflex)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_bit_xor;
        }
        else
          assert(0);

        if(success && (success = get_next_token(input) && parse_term(input, &ATTR(bin_expr, ast_node, right_operand))))
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
      break;
  }
  return success;
}

bool parse_assignment(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = parse_term(input, node)) && *node)
  {
    success = parse_rest_of_term(input, *node, node);
  }
  return success;
}

bool parse_rest_of_assignment(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(input->token.kind)
  {
    case eToken_eq:
    case eToken_eq_eq:
    case eToken_exclam_eq:
    case eToken_angle_left:
    case eToken_angle_left_eq:
    case eToken_angle_right:
    case eToken_angle_right_eq:
      {
        AstNode* bin_expr = *node = new_ast_node(eAstGen_gen0, eAstNode_bin_expr, clone_source_loc(&input->src_loc));
        ATTR(bin_expr, ast_node, left_operand) = left_node;

        if(input->token.kind == eToken_eq)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_assign;
        }
        else if(input->token.kind == eToken_eq_eq)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_eq;
        }
        else if(input->token.kind == eToken_exclam_eq)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_not_eq;
        }
        else if(input->token.kind == eToken_angle_left)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_less;
        }
        else if(input->token.kind == eToken_angle_left_eq)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_less_eq;
        }
        else if(input->token.kind == eToken_angle_right)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_greater;
        }
        else if(input->token.kind == eToken_angle_right_eq)
        {
          ATTR(bin_expr, op_kind, op_kind) = eOperator_greater_eq;
        }
        else
          assert(0);

        if(success = get_next_token(input) && parse_expr(input, &ATTR(bin_expr, ast_node, right_operand)))
        {
          if(!ATTR(bin_expr, ast_node, right_operand))
          {
            putback_token(input);
            success = compile_error(&input->src_loc, "operand expected, at `%s`", get_token_printstr(&input->token));
          }
        }
      }
      break;
  }
  return success;
}

bool parse_selector(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(input->token.kind)
  {
    case eToken_open_parens:
      {
        if(success = get_next_token(input) && parse_expr(input, node))
        {
          AstNode* expr = *node;
          if(expr)
          {
            if(input->token.kind == eToken_close_parens)
            {
              if((success = get_next_token(input)) && expr->kind == eAstNode_type_decl)
              {
                AstNode* cast = *node = new_ast_node(eAstGen_gen0, eAstNode_bin_expr, expr->src_loc);
                ATTR(cast, op_kind, op_kind) = eOperator_cast;
                ATTR(cast, ast_node, left_operand) = expr;

                if(success = parse_un_expr(input, &ATTR(cast, ast_node, right_operand)))
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
      break;

    case eToken_true:
    case eToken_false:
    case eToken_int_num:
    case eToken_float_num:
    case eToken_string:
    case eToken_char:
      {
        AstNode* lit = *node = new_ast_node(eAstGen_gen0, eAstNode_lit, clone_source_loc(&input->src_loc));

        if(input->token.kind == eToken_int_num)
        {
          ATTR(lit, lit_kind, lit_kind) = eLiteral_int_val;
          ATTR(lit, int_val, int_val) = *input->token.int_val;
        }
        else if(input->token.kind == eToken_float_num)
        {
          ATTR(lit, lit_kind, lit_kind) = eLiteral_float_val;
          ATTR(lit, float_val, float_val) = *input->token.float_val;
        }
        else if(input->token.kind == eToken_true ||
            input->token.kind == eToken_false)
        {
          ATTR(lit, lit_kind, lit_kind) = eLiteral_bool_val;
          ATTR(lit, bool_val, bool_val) = (input->token.kind == eToken_true ? 1 : 0);
        }
        else if(input->token.kind == eToken_char)
        {
          ATTR(lit, lit_kind, lit_kind) = eLiteral_char_val;
          ATTR(lit, char_val, char_val) = input->token.char_val;
        }
        else if(input->token.kind == eToken_string)
        {
          ATTR(lit, lit_kind, lit_kind) = eLiteral_str_val;
          ATTR(lit, str_val, str_val) = input->token.str_val;

#if 0
          AstNode* str = *node = new_ast_node(eAstGen_gen0, eAstNode_str, clone_source_loc(&input->src_loc));
          ATTR(str, ast_node, str_lit) = lit;
#endif
        }
        else
          assert(0);

        success = get_next_token(input);
      }
      break;

    case eToken_id:
      {
        AstNode* id = *node = new_ast_node(eAstGen_gen0, eAstNode_id, clone_source_loc(&input->src_loc));
        ATTR(id, str_val, name) = input->token.lexeme;
        success = get_next_token(input) && parse_rest_of_id(input, *node, node);
      }
      break;

    case eToken_type:
      {
        if(success = get_next_token(input) && parse_type(input, node))
        {
          if(!*node)
          {
            success = compile_error(&input->src_loc, "type expression required, at `%s`", get_token_printstr(&input->token));
          }
        }
      }
      break;
  }
  return success;
}

bool parse_formal_arg(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* type = 0;
  if((success = parse_type(input, &type)) && type)
  {
    AstNode* var_decl = *node = new_ast_node(eAstGen_gen0, eAstNode_var_decl, clone_source_loc(&input->src_loc));
    ATTR(var_decl, ast_node, type) = type;

    if(input->token.kind == eToken_id)
    {
      AstNode* id = ATTR(var_decl, ast_node, id) = new_ast_node(eAstGen_gen0, eAstNode_id, clone_source_loc(&input->src_loc));
      ATTR(id, str_val, name) = input->token.lexeme;
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
      append_list_elem(arg_list, arg, eList_ast_node);
      if(input->token.kind == eToken_comma)
      {
        success = get_next_token(input);
      }
      else if(input->token.kind != eToken_close_parens)
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

  switch(input->token.kind)
  {
    case eToken_exclam:
    case eToken_star:
    case eToken_ampersand:
    case eToken_minus:
    case eToken_minus_minus:
    case eToken_plus_plus:
      {
        AstNode* un_expr = *node = new_ast_node(eAstGen_gen0, eAstNode_un_expr, clone_source_loc(&input->src_loc));

        if(input->token.kind == eToken_exclam)
        {
          ATTR(un_expr, op_kind, op_kind) = eOperator_logic_not;
        }
        else if(input->token.kind == eToken_star)
        {
          ATTR(un_expr, op_kind, op_kind) = eOperator_deref;
        }
        else if(input->token.kind == eToken_ampersand)
        {
          ATTR(un_expr, op_kind, op_kind) = eOperator_address_of;
        }
        else if(input->token.kind == eToken_minus)
        {
          ATTR(un_expr, op_kind, op_kind) = eOperator_neg;
        }
        else if(input->token.kind == eToken_minus_minus)
        {
#if 0
          un_expr->op = eOperator_pre_decr;
#else
          success = compile_error(&input->src_loc, "`--` not supported");
#endif
        }
        else if(input->token.kind == eToken_plus_plus)
        {
#if 0
          un_expr->op = eOperator_pre_incr;
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
      break;

    default:
      success = parse_selector(input, node);
      break;
  }
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
      AstNode* var_decl = *node = new_ast_node(eAstGen_gen0, eAstNode_var_decl, clone_source_loc(&input->src_loc));
      ATTR(var_decl, ast_node, type) = type;

      if(input->token.kind == eToken_id)
      {
        AstNode* id = ATTR(var_decl, ast_node, id) = new_ast_node(eAstGen_gen0, eAstNode_id, clone_source_loc(&input->src_loc));
        ATTR(id, str_val, name) = input->token.lexeme;

        if((success = get_next_token(input)) && input->token.kind == eToken_eq
            && (success = get_next_token(input)))
        {
          AstNode* occur_id = new_ast_node(eAstGen_gen0, eAstNode_id, id->src_loc);
          ATTR(occur_id, str_val, name) = ATTR(id, str_val, name);

          AstNode* assign = new_ast_node(eAstGen_gen0, eAstNode_bin_expr, clone_source_loc(&input->src_loc));
          ATTR(assign, op_kind, op_kind) = eOperator_assign;
          ATTR(assign, ast_node, left_operand) = occur_id;

          AstNode* init_expr = 0;
          if(success = parse_expr(input, &init_expr))
          {
            if(init_expr)
            {
              ATTR(assign, ast_node, right_operand) = init_expr;

              init_expr = new_ast_node(eAstGen_gen0, eAstNode_stmt, assign->src_loc);
              ATTR(init_expr, ast_node, stmt) = assign;
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

  if(input->token.kind == eToken_For)
  {
    auto* for_stmt = AST1(*node = new_ast1_node(&input->src_loc, eAstNode_for_stmt), for_stmt);

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == eToken_OpenParens)
    {
      success = get_next_token(input) && parse_var(input, &for_stmt->decl_expr)
        && consume_semicolon(input)
        && parse_expr(input, &for_stmt->cond_expr)
        && consume_semicolon(input)
        && parse_expr(input, &for_stmt->loop_expr);
      if(!success)
      {
        return success;
      }

      if(input->token.kind == eToken_CloseParens)
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

  if(input->token.kind == eToken_while)
  {
    AstNode* while_stmt = *node = new_ast_node(eAstGen_gen0, eAstNode_while_stmt, clone_source_loc(&input->src_loc));

    if(!(success = get_next_token(input)))
    {
      return success;
    }

    if(input->token.kind == eToken_open_parens)
    {
      success = get_next_token(input) && parse_expr(input, &ATTR(while_stmt, ast_node, cond_expr));
      if(!success)
        return success;

      if(ATTR(while_stmt, ast_node, cond_expr))
      {
        if(input->token.kind == eToken_close_parens)
        {
          AstNode* body = 0;
          success = get_next_token(input) && parse_block(input, &body);
          if(!success)
          {
            return success;
          }

          if(body)
          {
            ATTR(while_stmt, ast_node, body) = body;
          }
          else
          {
            if(success = parse_node(input, &body))
            {
              if(body)
              {
                AstNode* block = new_ast_node(eAstGen_gen0, eAstNode_block, clone_source_loc(&input->src_loc));
                ATTR(block, list, nodes) = new_list(arena, eList_ast_node);
                append_list_elem(ATTR(block, list, nodes), body, eList_ast_node);
                ATTR(while_stmt, ast_node, body) = block;
              }
              else
              {
                putback_token(input);
                success = compile_error(&input->src_loc, "statement required, at `%s`", get_token_printstr(&input->token));
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

  if(input->token.kind == eToken_else)
  {
    AstNode* body = 0;
    if(success = get_next_token(input) && parse_block(input, &body))
    {
      if(body)
      {
        *node = body;
      }
      else
      {
        if(success = parse_node(input, &body))
        {
          if(body)
          {
            AstNode* block = new_ast_node(eAstGen_gen0, eAstNode_block, clone_source_loc(&input->src_loc));
            ATTR(block, list, nodes) = new_list(arena, eList_ast_node);
            append_list_elem(ATTR(block, list, nodes), body, eList_ast_node);
            *node = block;
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
  return success;
}

bool parse_if(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == eToken_if)
  {
    AstNode* if_stmt = *node = new_ast_node(eAstGen_gen0, eAstNode_if_stmt, clone_source_loc(&input->src_loc));

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == eToken_open_parens)
    {
      success = get_next_token(input) && parse_expr(input, &ATTR(if_stmt, ast_node, cond_expr));
      if(!success)
        return success;

      if(input->token.kind == eToken_close_parens)
      {
        if(ATTR(if_stmt, ast_node, cond_expr))
        {
          AstNode* body = 0;
          success = get_next_token(input) && parse_block(input, &body);
          if(!success)
            return success;

          if(body)
          {
            ATTR(if_stmt, ast_node, body) = body;
          }
          else if(success = parse_node(input, &body))
          {
            if(body)
            {
              AstNode* block = new_ast_node(eAstGen_gen0, eAstNode_block, clone_source_loc(&input->src_loc));
              ATTR(block, list, nodes) = new_list(arena, eList_ast_node);
              append_list_elem(ATTR(block, list, nodes), body, eList_ast_node);
              ATTR(if_stmt, ast_node, body) = block;
            }
            else
            {
              putback_token(input);
              success = compile_error(&input->src_loc, "statement required, at `%s`", get_token_printstr(&input->token));
            }
          }

          if(success)
          {
            success = parse_else(input, &ATTR(if_stmt, ast_node, else_body));
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

  AstNode* proc = *node = new_ast_node(eAstGen_gen0, eAstNode_proc_decl, clone_source_loc(&input->src_loc));
  AstNode* ret_type = 0;
  if(success = parse_type(input, &ret_type))
  {
    if(ret_type)
    {
      AstNode* ret_var = ATTR(proc, ast_node, ret_var) = new_ast_node(eAstGen_gen0, eAstNode_var_decl, ret_type->src_loc);
      AstNode* ret_id = ATTR(ret_var, ast_node, id) = new_ast_node(eAstGen_gen0, eAstNode_id, ret_type->src_loc);
      ATTR(ret_id, str_val, name) = make_temp_name("ret");
      ATTR(ret_var, ast_node, type) = ret_type;

      if(input->token.kind == eToken_id)
      {
        AstNode* id = ATTR(proc, ast_node, id) = new_ast_node(eAstGen_gen0, eAstNode_id, clone_source_loc(&input->src_loc));
        ATTR(id, str_val, name) = input->token.lexeme;

        if(!(success = get_next_token(input)))
          return success;

        if(input->token.kind == eToken_open_parens)
        {
          if(!(success = get_next_token(input)))
            return success;

          ATTR(proc, list, formal_args) = new_list(arena, eList_ast_node);
          if(success = parse_formal_arg_list(input, ATTR(proc, list, formal_args)))
          {
            if(input->token.kind == eToken_close_parens)
            {
              if(success = get_next_token(input) && parse_block(input, &ATTR(proc, ast_node, body)))
              {
                if(!ATTR(proc, ast_node, body))
                {
                  success = consume_semicolon(input);
                }
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
      success = compile_error(&input->src_loc, "type expression required, at `%s`", get_token_printstr(&input->token));
  }
  return success;
}

bool parse_include(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* include = *node = new_ast_node(eAstGen_gen0, eAstNode_include, clone_source_loc(&input->src_loc));

  if(input->token.kind == eToken_string)
  {

    String str; str_init(&str, arena);
    str_append(&str, input->src_loc.file_path);
    path_make_dir(str.head);
    str_tidyup(&str);
    str_append(&str, input->token.str_val);
    ATTR(include, str_val, file_path) = str_cap(&str);

    if(!(success = get_next_token(input)))
      return success;

    char* hoc_text = file_read_text(arena, ATTR(include, str_val, file_path));
    if(hoc_text)
    {
      TokenStream* incl_input = mem_push_struct(arena, TokenStream);
      init_token_stream(incl_input, hoc_text, ATTR(include, str_val, file_path));

      if(success = get_next_token(incl_input))
      {
        AstNode* block = ATTR(include, ast_node, body) = new_ast_node(eAstGen_gen0, eAstNode_block, clone_source_loc(&incl_input->src_loc));
        ATTR(block, list, nodes) = new_list(arena, eList_ast_node);
        success = parse_node_list(incl_input, ATTR(block, list, nodes));
      }
    }
    else
      success = compile_error(&input->src_loc, "could not read file `%s`", ATTR(include, str_val, file_path));
  }
  else
    success = compile_error(&input->src_loc, "string expected, actual `%s`", get_token_printstr(&input->token));
  return success;
}

bool parse_enum(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == eToken_enum)
  {
    AstNode* enum_decl = *node = new_ast_node(eAstGen_gen0, eAstNode_enum_decl, clone_source_loc(&input->src_loc));

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == eToken_id)
    {
      AstNode* id = ATTR(enum_decl, ast_node, id) = new_ast_node(eAstGen_gen0, eAstNode_id, clone_source_loc(&input->src_loc));
      ATTR(id, str_val, name) = input->token.lexeme;

      if(!(success = get_next_token(input)))
        return success;

      if(input->token.kind == eToken_open_brace)
      {

        if(!(success = get_next_token(input)))
          return success;

        ATTR(enum_decl, list, members) = new_list(arena, eList_ast_node);
        AstNode* member = 0;
        do
        {
          member = 0;
          if(input->token.kind == eToken_id)
          {
            member = new_ast_node(eAstGen_gen0, eAstNode_id, clone_source_loc(&input->src_loc));
            ATTR(member, str_val, name) = input->token.lexeme;
            append_list_elem(ATTR(enum_decl, list, members), member, eList_ast_node);

            if((success = get_next_token(input)) && input->token.kind == eToken_comma)
            {
              success = get_next_token(input);
            }
            else if(input->token.kind != eToken_close_brace)
              member = 0;
          }
        }
        while(member && success);

        if(input->token.kind == eToken_close_brace)
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

  if(input->token.kind == eToken_union)
  {
    AstNode* union_decl = *node = new_ast_node(eAstGen_gen0, eAstNode_union_decl, clone_source_loc(&input->src_loc));

    if((success = get_next_token(input)) && input->token.kind == eToken_id)
    {
      AstNode* id = ATTR(union_decl, ast_node, id) = new_ast_node(eAstGen_gen0, eAstNode_id, clone_source_loc(&input->src_loc));
      ATTR(id, str_val, name) = input->token.lexeme;
      success = get_next_token(input);
    }

    if(success)
    {
      ATTR(union_decl, list, members) = new_list(arena, eList_ast_node);
      success = parse_struct_member_list(input, ATTR(union_decl, list, members));
    }
  }
  return success;
}

bool parse_struct(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == eToken_struct)
  {
    AstNode* struct_decl = *node = new_ast_node(eAstGen_gen0, eAstNode_struct_decl, clone_source_loc(&input->src_loc));

    if((success = get_next_token(input)) && input->token.kind == eToken_id)
    {
      AstNode* id = ATTR(struct_decl, ast_node, id) = new_ast_node(eAstGen_gen0, eAstNode_id, clone_source_loc(&input->src_loc));
      ATTR(id, str_val, name) = input->token.lexeme;
      success = get_next_token(input);
    }

    if(success)
    {
      ATTR(struct_decl, list, members) = new_list(arena, eList_ast_node);
      success = parse_struct_member_list(input, ATTR(struct_decl, list, members));
    }
  }
  return success;
}

bool parse_struct_member_list(TokenStream* input, List* member_list)
{
  bool success = true;

  if(input->token.kind == eToken_open_brace)
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
          if(input->token.kind == eToken_union)
          {
            success = parse_union(input, &type);
          }
          else if(input->token.kind == eToken_struct)
          {
            success = parse_struct(input, &type);
          }
        }

        if(success && type)
        {
          AstNode* var_decl = member = new_ast_node(eAstGen_gen0, eAstNode_var_decl, clone_source_loc(&input->src_loc));
          ATTR(var_decl, ast_node, type) = type;

          if(input->token.kind == eToken_id)
          {
            AstNode* id = ATTR(var_decl, ast_node, id) = new_ast_node(eAstGen_gen0, eAstNode_id, clone_source_loc(&input->src_loc));
            ATTR(id, str_val, name) = input->token.lexeme;
            success = get_next_token(input);
          }
          else if(type->kind == eAstNode_struct_decl
                  || type->kind == eAstNode_union_decl)
          {
            /* anonymous struct/union */
          }
          else
            success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));

          if(success)
          {
            append_list_elem(member_list, member, eList_ast_node);
            success = consume_semicolon(input);
          }
        }
      }
    }
    while(member && success);

    if(success)
    {
      if(input->token.kind == eToken_close_brace)
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

  if(input->token.kind == eToken_return)
  {
    AstNode* ret_stmt = *node = new_ast_node(eAstGen_gen0, eAstNode_ret_stmt, clone_source_loc(&input->src_loc));
    if(success = get_next_token(input))
    {
      AstNode* ret_expr = 0;
      if(success = parse_expr(input, &ret_expr))
      {
        if(ret_expr)
        {
          ATTR(ret_stmt, ast_node, ret_expr) = ret_expr;
        }
      }
    }
  }

  return success;
}

#if 0
bool
parse_label(TokenStream* input, AstNode1* id, AstNode1** node)
{
  *node = id;
  bool success = true;

  if(input->token.kind == eToken_Colon && (success = get_next_token(input)))
  {
    if(id->kind == eAstNode_id)
    {
      auto* label = AST1(*node = new_ast1_node(&input->src_loc, eAstNode_label), label);
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

  if(input->token.kind == eToken_Goto)
  {
    auto* goto_stmt = AST1(*node = new_ast1_node(&input->src_loc, eAstNode_goto_stmt), goto_stmt);

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == eToken_Id)
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

  if(input->token.kind == eToken_continue)
  {
    *node = new_ast_node(eAstGen_gen0, eAstNode_continue_stmt, clone_source_loc(&input->src_loc));
    success = get_next_token(input);
  }
  return success;
}

bool parse_break(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == eToken_break)
  {
    *node = new_ast_node(eAstGen_gen0, eAstNode_break_stmt, clone_source_loc(&input->src_loc));
    success = get_next_token(input);
  }
  return success;
}

bool parse_asm_block(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == eToken_open_brace)
  {
    AstNode* asm_block = *node = new_ast_node(eAstGen_gen0, eAstNode_asm_block, clone_source_loc(&input->src_loc));

    if(success = get_asm_text(input))
    {
      ATTR(asm_block, str_val, asm_text) = input->token.lexeme;
      if(success = get_next_token(input))
      {
        if(input->token.kind == eToken_close_brace)
        {
          success = get_next_token(input);
        }
        else
          success = compile_error(&input->src_loc, "expected `}`, actual `%s`", get_token_printstr(&input->token));
      }
    }
  }
  return success;
}

bool parse_node(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(input->token.kind)
  {
    case eToken_var:
      success = get_next_token(input) && parse_var(input, node) && consume_semicolon(input);
      break;

    case eToken_asm:
      success = get_next_token(input) && parse_asm_block(input, node);
      break;

    case eToken_include:
      success = get_next_token(input) && parse_include(input, node) && consume_semicolon(input);
      break;

    case eToken_proc:
      success = get_next_token(input) && parse_proc(input, node);
      break;

    case eToken_extern:
      success = get_next_token(input) && parse_node(input, node);
      if(!success)
        break;
      if((*node)->kind == eAstNode_proc_decl)
      {
        ATTR(*node, bool_val, is_extern) = true;
      }
      else
        success = compile_error((*node)->src_loc, "`extern` can only be applied to procs");
      break;

    case eToken_if:
      success = parse_if(input, node);
      break;

    case eToken_else:
      success = compile_error(&input->src_loc, "unmatched `else`");
      break;

    case eToken_while:
      success = parse_while(input, node);
      break;
#if 0
    case eToken_for:
      success = parse_for(input, &node);
      break;
#endif
    case eToken_return:
      success = parse_return(input, node) && consume_semicolon(input);
      break;

    case eToken_break:
      success = parse_break(input, node) && consume_semicolon(input);
      break;

    case eToken_continue:
      success = parse_continue(input, node) && consume_semicolon(input);
      break;
#if 0
    case eToken_goto:
      success = parse_goto(input, &node) && consume_semicolon(input);
      break;
#endif
    case eToken_semicolon:
      if(success = consume_semicolon(input))
      {
        AstNode* stmt = new_ast_node(eAstGen_gen0, eAstNode_empty, clone_source_loc(&input->src_loc));
        ATTR(*node = new_ast_node(eAstGen_gen0, eAstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
      }
      break;

    case eToken_open_brace:
      success = parse_block(input, node);
      break;

    default:
      if(success = parse_expr(input, node))
      {
        if(*node)
        {
#if 0
          if(input->token.kind == eToken_Colon)
            success = parse_label(input, &node, node);
          else
            success = consume_semicolon(input);
#else
          if(success = consume_semicolon(input))
          {
            AstNode* stmt = *node;
            ATTR(*node = new_ast_node(eAstGen_gen0, eAstNode_stmt, stmt->src_loc), ast_node, stmt) = stmt;
          }
#endif
        }
      }
      break;
  }
  return success;
}

bool parse(TokenStream* input, AstNode** node)
{
  bool success = true;

  AstNode* module = *node = new_ast_node(eAstGen_gen0, eAstNode_module, clone_source_loc(&input->src_loc));
  ATTR(module, str_val, file_path) = input->src_loc.file_path;
  AstNode* body = ATTR(module, ast_node, body) = new_ast_node(eAstGen_gen0, eAstNode_block, clone_source_loc(&input->src_loc));
  ATTR(body, list, nodes) = new_list(arena, eList_ast_node);

  if((success = parse_node_list(input, ATTR(body, list, nodes))) && input->token.kind != eToken_end_of_input)
  {
    success = compile_error(&input->src_loc, "expected `end-of-input`, at `%s`", get_token_printstr(&input->token));
  }

  return success;
}

