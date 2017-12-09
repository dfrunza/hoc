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

#if 0
bool parse_rest_of_type_expr(TokenStream* input, AstNode* expr, AstNode** node)
{
  *node = expr;
  bool success = true;
  
  if(input->token.kind == eToken_star)
  {
    AstNode* ptr = *node = new_ast_node(eAstNode_pointer, clone_source_loc(&input->src_loc));
    ptr->pointer.pointee_expr = expr;

    success = get_next_token(input) && parse_rest_of_type_expr(input, *node, node);
  }
  return success;
}
#endif

bool parse_type_expr(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(input->token.kind)
  {
    case eToken_open_bracket:
      {
        AstNode* array = *node = new_ast_node(eAstNode_array, clone_source_loc(&input->src_loc));

        AstNode* size_expr = 0;
        if(success = get_next_token(input) && parse_expr(input, &size_expr))
        {
          array->array.size_expr = size_expr;
          if(input->token.kind == eToken_close_bracket)
          {
            AstNode* elem_expr = 0;
            if(success = get_next_token(input) && parse_type_expr(input, &elem_expr))
            {
              array->array.elem_expr = elem_expr;
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

#if 0
    case eToken_id:
      {
        AstNode* id = *node = new_ast_node(eAstNode_id, clone_source_loc(&input->src_loc));
        id->id.name = input->token.lexeme;

        success = get_next_token(input) && parse_rest_of_type_expr(input, *node, node);
      }
      break;
#endif

#if 0
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
#endif
  }
  return success;
}

bool parse_rest_of_type(TokenStream* input, AstNode* type, AstNode** node)
{
  *node = type;
  bool success = true;
  if(input->token.kind == eToken_star)
  {
    AstNode* pointer = *node = new_ast_node(eAstNode_type, clone_source_loc(&input->src_loc));
    pointer->type.kind = eType_pointer;
    pointer->type.pointer.pointee = type;
    success = get_next_token(input) && parse_rest_of_type(input, *node, node);
  }
  return success;
}

bool parse_type(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* type = *node = new_ast_node(eAstNode_type, clone_source_loc(&input->src_loc));
  switch(input->token.kind)
  {
    case eToken_open_bracket:
      type->type.kind = eType_array;
      if(success = (get_next_token(input) && parse_expr(input, &type->type.array.size)))
      {
        if(input->token.kind == eToken_close_bracket)
        {
          success = get_next_token(input) && parse_type(input, &type->type.array.elem);
        }
        else
          success = compile_error(&input->src_loc,  "expected `]`, actual `%s`", get_token_printstr(&input->token));
      }
      break;
    case eToken_id:
      type->type.kind = eType_basic;
      type->type.name = input->token.lexeme;
      success = get_next_token(input) && parse_rest_of_type(input, *node, node);
      break;

    default:
      success = compile_error(&input->src_loc,  "expected `[` or identifier, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

#if 0
bool parse_type(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* type_expr = 0;
  if(success = parse_type_expr(input, &type_expr))
  {
    if(type_expr)
    {
      AstNode* type = *node = new_ast_node(eAstNode_type, clone_source_loc(&input->src_loc));
      type->type.type_expr = type_expr;
    }
  }
  return success;
}
#endif

bool parse_initializer_member_list(TokenStream* input, List* member_list)
{
  bool success = true;

  AstNode* member = 0;
  do
  {
    member = 0;
#if 0
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
#endif
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

#if 0
bool parse_block(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == eToken_open_brace)
  {
    AstNode* block = *node = new_ast_node(eAstNode_block, clone_source_loc(&input->src_loc));
    List* nodes = block->block.nodes = new_list(arena, eList_ast_node);

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
#endif

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
          AstNode* call = *node = new_ast_node(eAstNode_call, clone_source_loc(&input->src_loc));
          call->call.id = left_node;
          call->call.actual_args = new_list(arena, eList_ast_node);

          if(success = get_next_token(input) && parse_actual_arg_list(input, call->call.actual_args))
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
        AstNode* index = *node = new_ast_node(eAstNode_bin_expr, clone_source_loc(&input->src_loc));
        index->bin_expr.op_kind = eOperator_index;
        index->bin_expr.left_operand = left_node;

        if(success = get_next_token(input) && parse_expr(input, &index->bin_expr.right_operand))
        {
          if(input->token.kind == eToken_close_bracket)
          {
            if(index->bin_expr.right_operand)
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
        AstNode* bin_expr = *node = new_ast_node(eAstNode_bin_expr, clone_source_loc(&input->src_loc));
        bin_expr->bin_expr.left_operand = left_node;

        if(input->token.kind == eToken_dot)
        {
          bin_expr->bin_expr.op_kind = eOperator_member_select;
        }
        else if(input->token.kind == eToken_arrow_right)
        {
          bin_expr->bin_expr.op_kind = eOperator_ptr_member_select;
        }

        if(success = get_next_token(input) && parse_selector(input, &bin_expr->bin_expr.right_operand))
        {
          if(bin_expr->bin_expr.right_operand)
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
        AstNode* un_expr = *node = new_ast_node(eAstNode_un_expr, clone_source_loc(&input->src_loc));
        un_expr->un_expr.operand = left_node;

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
        AstNode* bin_expr = *node = new_ast_node(eAstNode_bin_expr, clone_source_loc(&input->src_loc));
        bin_expr->bin_expr.left_operand = left_node;

        if(input->token.kind == eToken_star)
        {
          bin_expr->bin_expr.op_kind = eOperator_mul;
        }
        else if(input->token.kind == eToken_fwd_slash)
        {
          bin_expr->bin_expr.op_kind = eOperator_div;
        }
        else if(input->token.kind == eToken_percent)
        {
          bin_expr->bin_expr.op_kind = eOperator_mod;
        }
        else
          assert(0);

        if(success = get_next_token(input) && parse_factor(input, &bin_expr->bin_expr.right_operand))
        {
          if(bin_expr->bin_expr.right_operand)
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
        AstNode* bin_expr = *node = new_ast_node(eAstNode_bin_expr, clone_source_loc(&input->src_loc));
        bin_expr->bin_expr.left_operand = left_node;

        if(input->token.kind == eToken_plus)
        {
          bin_expr->bin_expr.op_kind = eOperator_add;
        }
        else if(input->token.kind == eToken_minus)
        {
          bin_expr->bin_expr.op_kind = eOperator_sub;
        }
        else if(input->token.kind == eToken_pipe)
        {
          bin_expr->bin_expr.op_kind = eOperator_bit_or;
        }
        else if(input->token.kind == eToken_pipe_pipe)
        {
          bin_expr->bin_expr.op_kind = eOperator_logic_or;
        }
        else if(input->token.kind == eToken_ampersand)
        {
          bin_expr->bin_expr.op_kind = eOperator_bit_and;
        }
        else if(input->token.kind == eToken_ampersand_ampersand)
        {
          bin_expr->bin_expr.op_kind = eOperator_logic_and;
        }
        else if(input->token.kind == eToken_angle_left_left)
        {
          bin_expr->bin_expr.op_kind = eOperator_bit_shift_left;
        }
        else if(input->token.kind == eToken_angle_right_right)
        {
          bin_expr->bin_expr.op_kind = eOperator_bit_shift_right;
        }
        else if(input->token.kind == eToken_circumflex)
        {
          bin_expr->bin_expr.op_kind = eOperator_bit_xor;
        }
        else
          assert(0);

        if(success && (success = get_next_token(input) && parse_term(input, &bin_expr->bin_expr.right_operand)))
        {
          if(bin_expr->bin_expr.right_operand)
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
        AstNode* bin_expr = *node = new_ast_node(eAstNode_bin_expr, clone_source_loc(&input->src_loc));
        bin_expr->bin_expr.left_operand = left_node;

        if(input->token.kind == eToken_eq)
        {
          bin_expr->bin_expr.op_kind = eOperator_assign;
        }
        else if(input->token.kind == eToken_eq_eq)
        {
          bin_expr->bin_expr.op_kind = eOperator_eq;
        }
        else if(input->token.kind == eToken_exclam_eq)
        {
          bin_expr->bin_expr.op_kind = eOperator_not_eq;
        }
        else if(input->token.kind == eToken_angle_left)
        {
          bin_expr->bin_expr.op_kind = eOperator_less;
        }
        else if(input->token.kind == eToken_angle_left_eq)
        {
          bin_expr->bin_expr.op_kind = eOperator_less_eq;
        }
        else if(input->token.kind == eToken_angle_right)
        {
          bin_expr->bin_expr.op_kind = eOperator_greater;
        }
        else if(input->token.kind == eToken_angle_right_eq)
        {
          bin_expr->bin_expr.op_kind = eOperator_greater_eq;
        }
        else
          assert(0);

        if(success = get_next_token(input) && parse_expr(input, &bin_expr->bin_expr.right_operand))
        {
          if(!bin_expr->bin_expr.right_operand)
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
              if((success = get_next_token(input)) && expr->kind == eAstNode_type)
              {
                AstNode* cast = *node = new_ast_node(eAstNode_bin_expr, expr->src_loc);
                cast->bin_expr.op_kind = eOperator_cast;
                cast->bin_expr.left_operand = expr;

                if(success = parse_un_expr(input, &cast->bin_expr.right_operand))
                {
                  if(!cast->bin_expr.right_operand)
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
        AstNode* lit = *node = new_ast_node(eAstNode_lit, clone_source_loc(&input->src_loc));

        if(input->token.kind == eToken_int_num)
        {
          lit->lit.kind = eLiteral_int_val;
          lit->lit.int_val = *input->token.int_val;
        }
        else if(input->token.kind == eToken_float_num)
        {
          lit->lit.kind = eLiteral_float_val;
          lit->lit.float_val = *input->token.float_val;
        }
        else if(input->token.kind == eToken_true ||
            input->token.kind == eToken_false)
        {
          lit->lit.kind = eLiteral_bool_val;
          lit->lit.bool_val = (input->token.kind == eToken_true ? 1 : 0);
        }
        else if(input->token.kind == eToken_char)
        {
          lit->lit.kind = eLiteral_char_val;
          lit->lit.char_val = input->token.char_val;
        }
        else if(input->token.kind == eToken_string)
        {
          lit->lit.kind = eLiteral_str_val;
          lit->lit.str_val = input->token.str_val;
        }
        else
          assert(0);

        success = get_next_token(input);
      }
      break;

    case eToken_id:
      {
        AstNode* id = *node = new_ast_node(eAstNode_id, clone_source_loc(&input->src_loc));
        id->id.name = input->token.lexeme;
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

  if(input->token.kind == eToken_open_bracket || input->token.kind == eToken_id)
  {
    AstNode* var = *node = new_ast_node(eAstNode_var, clone_source_loc(&input->src_loc));
    if(success = parse_type(input, &var->var.type))
    {
      if(input->token.kind == eToken_id)
      {
        var->var.name = input->token.lexeme;
        success = get_next_token(input);
      }
      else
        success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

#if 0
bool parse_formal_arg(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* type = 0;
  if((success = parse_type(input, &type)) && type)
  {
    AstNode* var = *node = new_ast_node(eAstNode_var, clone_source_loc(&input->src_loc));
    var->var.type = type;

    if(input->token.kind == eToken_id)
    {
      //AstNode* id = var->var.id = new_ast_node(eAstNode_id, clone_source_loc(&input->src_loc));
      //id->id.name = input->token.lexeme;
      success = get_next_token(input);
    }
    else
      success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}
#endif

#if 0
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
#endif

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
        AstNode* un_expr = *node = new_ast_node(eAstNode_un_expr, clone_source_loc(&input->src_loc));

        if(input->token.kind == eToken_exclam)
        {
          un_expr->un_expr.op_kind = eOperator_logic_not;
        }
        else if(input->token.kind == eToken_star)
        {
          un_expr->un_expr.op_kind = eOperator_deref;
        }
        else if(input->token.kind == eToken_ampersand)
        {
          un_expr->un_expr.op_kind = eOperator_address_of;
        }
        else if(input->token.kind == eToken_minus)
        {
          un_expr->un_expr.op_kind = eOperator_neg;
        }
        else if(input->token.kind == eToken_minus_minus)
        {
#if 0
          un_expr->un_expr.op_kind = eOperator_pre_decr;
#else
          success = compile_error(&input->src_loc, "`--` not supported");
#endif
        }
        else if(input->token.kind == eToken_plus_plus)
        {
#if 0
          un_expr->un_expr.op_kind = eOperator_pre_incr;
#else
          success = compile_error(&input->src_loc, "`++` not supported");
#endif
        }
        else
          assert(0);

        if(success && (success = get_next_token(input) && parse_factor(input, &un_expr->un_expr.operand)))
        {
          if(!un_expr->un_expr.operand)
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

#if 0
bool parse_expr(TokenStream* input, AstNode** node)
{
  bool success = true;

  if((success = parse_assignment(input, node)) && *node)
    success = parse_rest_of_assignment(input, *node, node);
  return success;
}
#endif

bool parse_var(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == eToken_var)
  {
    if(success = get_next_token(input))
    {
      AstNode* var = *node = new_ast_node(eAstNode_var, clone_source_loc(&input->src_loc));
      if(success = parse_type(input, &var->var.type))
      {
        if(input->token.kind == eToken_id)
        {
          var->var.name = input->token.lexeme;
          success = get_next_token(input);
        }
        else
          success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else
    success = compile_error(&input->src_loc, "expected `var`, actual `%s`", get_token_printstr(&input->token));

  return success;
}

#if 0
bool parse_var(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* type = 0;
  if(success = parse_type(input, &type))
  {
    if(type)
    {
      AstNode* var = *node = new_ast_node(eAstNode_var, clone_source_loc(&input->src_loc));
      var->var.type = type;

      if(input->token.kind == eToken_id)
      {
        AstNode* id = var->var.id = new_ast_node(eAstNode_id, clone_source_loc(&input->src_loc));
        id->id.name = input->token.lexeme;

        if((success = get_next_token(input)) && input->token.kind == eToken_eq
            && (success = get_next_token(input)))
        {
          AstNode* occur_id = new_ast_node(eAstNode_id, id->src_loc);
          occur_id->id.name = id->id.name;

          AstNode* assign = new_ast_node(eAstNode_bin_expr, clone_source_loc(&input->src_loc));
          assign->bin_expr.op_kind = eOperator_assign;
          assign->bin_expr.left_operand = occur_id;

          AstNode* init_expr = 0;
          if(success = parse_expr(input, &init_expr))
          {
            if(init_expr)
            {
              assign->bin_expr.right_operand = init_expr;

              init_expr = new_ast_node(eAstNode_stmt, assign->src_loc);
              init_expr->stmt.stmt = assign;
              var->var.init_expr = init_expr;
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
#endif

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

#if 0
bool parse_while(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == eToken_while)
  {
    AstNode* while_stmt = *node = new_ast_node(eAstNode_while_stmt, clone_source_loc(&input->src_loc));

    if(!(success = get_next_token(input)))
    {
      return success;
    }

    if(input->token.kind == eToken_open_parens)
    {
      success = get_next_token(input) && parse_expr(input, &while_stmt->while_stmt.cond_expr);
      if(!success)
        return success;

      if(while_stmt->while_stmt.cond_expr)
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
            while_stmt->while_stmt.body = body;
          }
          else
          {
            if(success = parse_node(input, &body))
            {
              if(body)
              {
                AstNode* block = new_ast_node(eAstNode_block, clone_source_loc(&input->src_loc));
                block->block.nodes = new_list(arena, eList_ast_node);
                append_list_elem(block->block.nodes, body, eList_ast_node);
                while_stmt->while_stmt.body = block;
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
#endif

#if 0
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
            AstNode* block = new_ast_node(eAstNode_block, clone_source_loc(&input->src_loc));
            block->block.nodes = new_list(arena, eList_ast_node);
            append_list_elem(block->block.nodes, body, eList_ast_node);
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
#endif

#if 0
bool parse_if(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == eToken_if)
  {
    AstNode* if_stmt = *node = new_ast_node(eAstNode_if_stmt, clone_source_loc(&input->src_loc));

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == eToken_open_parens)
    {
      success = get_next_token(input) && parse_expr(input, &if_stmt->if_stmt.cond_expr);
      if(!success)
        return success;

      if(input->token.kind == eToken_close_parens)
      {
        if(if_stmt->if_stmt.cond_expr)
        {
          AstNode* body = 0;
          success = get_next_token(input) && parse_block(input, &body);
          if(!success)
            return success;

          if(body)
          {
            if_stmt->if_stmt.body = body;
          }
          else if(success = parse_node(input, &body))
          {
            if(body)
            {
              AstNode* block = new_ast_node(eAstNode_block, clone_source_loc(&input->src_loc));
              block->block.nodes = new_list(arena, eList_ast_node);
              append_list_elem(block->block.nodes, body, eList_ast_node);
              if_stmt->if_stmt.body = block;
            }
            else
            {
              putback_token(input);
              success = compile_error(&input->src_loc, "statement required, at `%s`", get_token_printstr(&input->token));
            }
          }

          if(success)
          {
            success = parse_else(input, &if_stmt->if_stmt.else_body);
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
#endif

bool parse_proc_modifier(TokenStream* input, eProcModifier* modifier)
{
  bool success = true;
  *modifier = eProcModifier_None;
  if(input->token.kind == eToken_extern)
  {
    *modifier = eProcModifier_extern;
    success = get_next_token(input);
  }
  return success;
}

bool parse_formal_arg_list(TokenStream* input, AstNode* proc);

bool parse_rest_of_formal_arg_list(TokenStream* input, AstNode* proc)
{
  bool success = true;
  if(input->token.kind == eToken_comma && (success = get_next_token(input)))
  {
    success = parse_formal_arg_list(input, proc);
  }
  return success;
}

bool parse_formal_arg_list(TokenStream* input, AstNode* proc)
{
  bool success = true;
  AstNode* arg = 0;
  if(success = parse_formal_arg(input, &arg))
  {
    if(arg)
    {
      append_list_elem(&proc->proc.formal_arg_list, arg, eList_ast_node);
      success = parse_rest_of_formal_arg_list(input, proc);
    }
  }
  return success;
}

bool parse_expr(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;
  return success;
}

bool parse_if(TokenStream* input, AstNode** node);
bool parse_while(TokenStream* input, AstNode** node);

bool parse_if_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;
  switch(input->token.kind)
  {
    case eToken_var:
      success = parse_var(input, node);
      break;
    case eToken_if:
      success = parse_if(input, node);
      break;
    case eToken_while:
      success = parse_while(input, node);
      break;
  }
  return success;
}

bool parse_empty(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(input->token.kind == eToken_semicolon)
  {
    *node = new_ast_node(eAstNode_empty, clone_source_loc(&input->src_loc));
    success = get_next_token(input);
  }
  return success;
}

bool parse_block(TokenStream* input, AstNode** node);

bool parse_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;
  switch(input->token.kind)
  {
    case eToken_open_brace:
      success = parse_block(input, node);
      break;
    case eToken_var:
      success = parse_var(input, node) && consume_semicolon(input);
      break;
    case eToken_if:
      success = parse_if(input, node);
      break;
    case eToken_while:
      success = parse_while(input, node);
      break;
    case eToken_semicolon:
      success = parse_empty(input, node);
      break;
  }
  return success;
}

bool parse_stmt_list(TokenStream* input, AstNode* block)
{
  bool success = true;
  AstNode* stmt = 0;
  if(success = parse_stmt(input, &stmt))
  {
    if(stmt)
    {
      switch(stmt->kind)
      {
        case eAstNode_var:
          append_list_elem(&block->block.var_list, stmt, eList_ast_node);
          break;
        case eAstNode_if_stmt:
        case eAstNode_while_stmt:
        case eAstNode_bin_expr:
        case eAstNode_un_expr:
        case eAstNode_block:
          append_list_elem(&block->block.stmt_list, stmt, eList_ast_node);
          break;
        case eAstNode_empty:
          break;
        default:
          assert(0);
      }
      success = parse_stmt_list(input, block);
    }
  }
  return success;
}

bool parse_block(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(input->token.kind == eToken_open_brace)
  {
    AstNode* block = *node = new_ast_node(eAstNode_block, clone_source_loc(&input->src_loc));
    init_list(&block->block.var_list, arena, eList_ast_node);
    init_list(&block->block.stmt_list, arena, eList_ast_node);
    if(success = (get_next_token(input) && parse_stmt_list(input, block)))
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

bool parse_else_body(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;
  switch(input->token.kind)
  {
    case eToken_open_brace:
    case eToken_var:
    case eToken_if:
    case eToken_while:
    case eToken_semicolon:
      success = parse_stmt(input, node);
      break;
    default:
      success = compile_error(&input->src_loc, "unexpected `%s`", get_token_printstr(&input->token));
      break;
  }
  return success;
}

bool parse_else(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(input->token.kind == eToken_else)
  {
    success = get_next_token(input) && parse_else_body(input, node);
  }
  return success;
}

bool parse_if_body(TokenStream* input, AstNode** node)
{
  bool success = true;
  switch(input->token.kind)
  {
    case eToken_open_brace:
    case eToken_var:
    case eToken_if:
    case eToken_while:
    case eToken_semicolon:
      success = parse_stmt(input, node);
      break;
    default:
      success = compile_error(&input->src_loc, "unexpected `%s`", get_token_printstr(&input->token));
      break;
  }
  return success;
}

bool parse_if(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(input->token.kind == eToken_if)
  {
    AstNode* if_ = *node = new_ast_node(eAstNode_if_stmt, clone_source_loc(&input->src_loc));
    init_list(&if_->if_stmt.var_list, arena, eList_ast_node);
    init_list(&if_->if_stmt.stmt_list, arena, eList_ast_node);
    if(success = get_next_token(input))
    {
      if(input->token.kind == eToken_open_parens)
      {
        if(success = (get_next_token(input) && parse_expr(input, &if_->if_stmt.cond_expr)))
        {
          if(input->token.kind == eToken_close_parens)
          {
            success = get_next_token(input) && parse_if_body(input, &if_->if_stmt.body) && parse_else(input, &if_->if_stmt.else_body);
          }
          else
            success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
        }
      }
      else
        success = compile_error(&input->src_loc, "expected `(`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

bool parse_while(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;
  return success;
}

bool parse_proc_body(TokenStream* input, AstNode* proc)
{
  bool success = true;
  switch(input->token.kind)
  {
    case eToken_open_brace:
      success = parse_block(input, &proc->proc.body);
      break;
    case eToken_semicolon:
      success = parse_empty(input, &proc->proc.body);
      break;
    default:
      success = compile_error(&input->src_loc, "unexpected `%s`", get_token_printstr(&input->token));
      break;
  }
  return success;
}

bool parse_proc(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* proc = *node = new_ast_node(eAstNode_proc, clone_source_loc(&input->src_loc));
  init_list(&proc->proc.formal_arg_list, arena, eList_ast_node);
  init_list(&proc->proc.var_list, arena, eList_ast_node);
  init_list(&proc->proc.stmt_list, arena, eList_ast_node);
  if(success = parse_proc_modifier(input, &proc->proc.modifier))
  {
    if(input->token.kind == eToken_proc)
    {
      if(success = get_next_token(input))
      {
        if(success = parse_type(input, &proc->proc.ret_type))
        {
          if(input->token.kind == eToken_id)
          {
            proc->proc.name = input->token.lexeme;
            if(success = get_next_token(input))
            {
              if(input->token.kind == eToken_open_parens)
              {
                if(success = (get_next_token(input) && parse_formal_arg_list(input, proc)))
                {
                  if(input->token.kind == eToken_close_parens)
                  {
                    success = get_next_token(input) && parse_proc_body(input, proc);
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
            success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
        }
      }
    }
    else
      success = compile_error(&input->src_loc, "expected `proc`, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

#if 0
bool parse_proc(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* proc = *node = new_ast_node(eAstNode_proc, clone_source_loc(&input->src_loc));
  AstNode* ret_type = 0;
  if(success = parse_type(input, &ret_type))
  {
    if(ret_type)
    {
      AstNode* ret_var = proc->proc.ret_var = new_ast_node(eAstNode_var, ret_type->src_loc);
      //AstNode* ret_id = ret_var->var.id = new_ast_node(eAstNode_id, ret_type->src_loc);
      //ret_id->id.name = make_temp_name("ret");
      ret_var->var.type = ret_type;

      if(input->token.kind == eToken_id)
      {
        AstNode* id = proc->proc.id = new_ast_node(eAstNode_id, clone_source_loc(&input->src_loc));
        id->id.name = input->token.lexeme;

        if(!(success = get_next_token(input)))
          return success;

        if(input->token.kind == eToken_open_parens)
        {
          if(!(success = get_next_token(input)))
            return success;

          proc->proc.formal_args = new_list(arena, eList_ast_node);
          if(success = parse_formal_arg_list(input, proc->proc.formal_args))
          {
            if(input->token.kind == eToken_close_parens)
            {
              if(success = get_next_token(input) && parse_block(input, &proc->proc.body))
              {
                if(!proc->proc.body)
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
#endif

bool parse_include(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == eToken_include)
  {
    if(success = get_next_token(input))
    {
      AstNode* include = *node = new_ast_node(eAstNode_include, clone_source_loc(&input->src_loc));
      if(input->token.kind == eToken_string)
      {
        include->include.file_path = input->token.str_val;
        success = get_next_token(input);
      }
      else
        success = compile_error(&input->src_loc, "expected string literal, actual `%s`", get_token_printstr(&input->token));
    }
  }
  else
    success = compile_error(&input->src_loc, "expected `include`, actual `%s`", get_token_printstr(&input->token));

  return success;
}

#if 0
bool parse_include(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* include = *node = new_ast_node(eAstNode_include, clone_source_loc(&input->src_loc));

  if(input->token.kind == eToken_string)
  {
    String str; str_init(&str, arena);
    str_append(&str, input->src_loc.file_path);
    path_make_dir(str.head);
    str_tidyup(&str);
    str_append(&str, input->token.str_val);
    include->include.file_path = str_cap(&str);

    if(!get_next_token(input))
      return success = false;

    char* hoc_text = file_read_text(arena, include->include.file_path);
    if(hoc_text)
    {
      TokenStream* incl_input = mem_push_struct(arena, TokenStream);
      init_token_stream(incl_input, hoc_text, include->include.file_path);

      if(success = get_next_token(incl_input))
      {
        AstNode* block = include->include.body = new_ast_node(eAstNode_block, clone_source_loc(&incl_input->src_loc));
        block->block.nodes = new_list(arena, eList_ast_node);
        success = parse_node_list(incl_input, block->block.nodes);
      }
    }
    else
      success = compile_error(&input->src_loc, "could not read file `%s`", include->include.file_path);
  }
  else
    success = compile_error(&input->src_loc, "string expected, actual `%s`", get_token_printstr(&input->token));
  return success;
}
#endif

bool parse_enum(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == eToken_enum)
  {
    AstNode* enum_decl = *node = new_ast_node(eAstNode_enum_decl, clone_source_loc(&input->src_loc));

    if(!get_next_token(input))
      return success = false;

    if(input->token.kind == eToken_id)
    {
      AstNode* id = enum_decl->enum_decl.id = new_ast_node(eAstNode_id, clone_source_loc(&input->src_loc));
      id->id.name = input->token.lexeme;

      if(!get_next_token(input))
        return success = false;

      if(input->token.kind == eToken_open_brace)
      {

        if(!get_next_token(input))
          return success = false;

        enum_decl->enum_decl.members = new_list(arena, eList_ast_node);
        AstNode* member = 0;
        do
        {
          member = 0;
          if(input->token.kind == eToken_id)
          {
            member = new_ast_node(eAstNode_id, clone_source_loc(&input->src_loc));
            member->id.name = input->token.lexeme;
            append_list_elem(enum_decl->enum_decl.members, member, eList_ast_node);

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
    AstNode* union_decl = *node = new_ast_node(eAstNode_union_decl, clone_source_loc(&input->src_loc));

    if((success = get_next_token(input)) && input->token.kind == eToken_id)
    {
      AstNode* id = union_decl->union_decl.id = new_ast_node(eAstNode_id, clone_source_loc(&input->src_loc));
      id->id.name = input->token.lexeme;
      success = get_next_token(input);
    }

    if(success)
    {
      union_decl->union_decl.members = new_list(arena, eList_ast_node);
      success = parse_struct_member_list(input, union_decl->union_decl.members);
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
    AstNode* struct_decl = *node = new_ast_node(eAstNode_struct_decl, clone_source_loc(&input->src_loc));

    if((success = get_next_token(input)) && input->token.kind == eToken_id)
    {
      AstNode* id = struct_decl->struct_decl.id = new_ast_node(eAstNode_id, clone_source_loc(&input->src_loc));
      id->id.name = input->token.lexeme;
      success = get_next_token(input);
    }

    if(success)
    {
      struct_decl->struct_decl.members = new_list(arena, eList_ast_node);
      success = parse_struct_member_list(input, struct_decl->struct_decl.members);
    }
  }
  return success;
}

bool parse_struct_member_list(TokenStream* input, List* member_list)
{
  bool success = true;

  if(input->token.kind == eToken_open_brace)
  {
    if(!get_next_token(input))
      return success = false;

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
          AstNode* var = member = new_ast_node(eAstNode_var, clone_source_loc(&input->src_loc));
          var->var.type = type;

          if(input->token.kind == eToken_id)
          {
            //AstNode* id = var->var.id = new_ast_node(eAstNode_id, clone_source_loc(&input->src_loc));
            //id->id.name = input->token.lexeme;
            success = get_next_token(input);
          }
          else if(type->kind == eAstNode_struct_decl || type->kind == eAstNode_union_decl)
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
    if(!success)
      return success;

    if(input->token.kind == eToken_close_brace)
      success = get_next_token(input);
    else
      success = compile_error(&input->src_loc, "expected `}`, actual `%s`", get_token_printstr(&input->token));
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
    AstNode* ret_stmt = *node = new_ast_node(eAstNode_ret_stmt, clone_source_loc(&input->src_loc));
    if(success = get_next_token(input))
    {
      AstNode* ret_expr = 0;
      if(success = parse_expr(input, &ret_expr))
      {
        if(ret_expr)
        {
          ret_stmt->ret_stmt.ret_expr = ret_expr;
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
    *node = new_ast_node(eAstNode_loop_ctrl, clone_source_loc(&input->src_loc));
    (*node)->loop_ctrl.kind = eLoopCtrl_continue;
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
    *node = new_ast_node(eAstNode_loop_ctrl, clone_source_loc(&input->src_loc));
    (*node)->loop_ctrl.kind = eLoopCtrl_break;
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
    AstNode* asm_block = *node = new_ast_node(eAstNode_asm_block, clone_source_loc(&input->src_loc));

    if(success = get_asm_text(input))
    {
      asm_block->asm_block.asm_text = input->token.lexeme;
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
    case eToken_include:
      success = get_next_token(input) && parse_include(input, node) && consume_semicolon(input);
      break;
    case eToken_proc:
      success = get_next_token(input) && parse_proc(input, node);
      break;
  }
  return success;
}

#if 0
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
      if((*node)->kind == eAstNode_proc)
      {
        (*node)->proc.is_extern = true;
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
    case eToken_struct:
      success = parse_struct(input, node) && consume_semicolon(input);
      break;

    case eToken_semicolon:
      if(success = consume_semicolon(input))
      {
        *node = new_ast_node(eAstNode_stmt, clone_source_loc(&input->src_loc));
        (*node)->stmt.stmt = new_ast_node(eAstNode_empty, clone_source_loc(&input->src_loc));
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
            *node = new_ast_node(eAstNode_stmt, clone_source_loc(stmt->src_loc));
            (*node)->stmt.stmt = stmt;
          }
#endif
        }
      }
      break;
  }
  return success;
}
#endif

bool parse_proc(TokenStream* input, AstNode** node);
bool parse_var(TokenStream* input, AstNode** node);
bool parse_include(TokenStream* input, AstNode** node);

bool parse_module_stmt(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;
  switch(input->token.kind)
  {
    case eToken_extern:
    case eToken_proc:
      success = parse_proc(input, node);
      break;
    case eToken_var:
      success = parse_var(input, node);
      break;
    case eToken_include:
      success = parse_include(input, node);
      break;
    case eToken_semicolon:
      success = parse_empty(input, node);
      break;
  }
  return success;
}

bool parse_module_stmt_list(TokenStream* input, AstNode* module)
{
  bool success = true;
  AstNode* stmt = 0;
  if(success = parse_module_stmt(input, &stmt))
  {
    if(stmt)
    {
      switch(stmt->kind)
      {
        case eAstNode_proc:
          append_list_elem(&module->module.proc_list, stmt, eList_ast_node);
          break;
        case eAstNode_var:
          append_list_elem(&module->module.var_list, stmt, eList_ast_node);
          break;
        case eAstNode_include:
          append_list_elem(&module->module.include_list, stmt, eList_ast_node);
          break;
        case eAstNode_empty:
          break;
        default:
          assert(0);
      }
      success = parse_module_stmt_list(input, module);
    }
  }
  return success;
}

bool parse_module_body(TokenStream* input, AstNode* module)
{
  bool success = true;
  if(success = parse_module_stmt_list(input, module))
  {
    if(input->token.kind == eToken_end_of_input)
    {
      //TODO: reset the input
    }
    else
      success = compile_error(&input->src_loc, "expected `end-of-input`, at `%s`", get_token_printstr(&input->token));
  }
  return success;
}

bool parse_module(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* module = *node = new_ast_node(eAstNode_module, clone_source_loc(&input->src_loc));
  module->module.file_path = input->src_loc.file_path;
  init_list(&module->module.proc_list, arena, eList_ast_node);
  init_list(&module->module.var_list, arena, eList_ast_node);
  init_list(&module->module.include_list, arena, eList_ast_node);
  success = parse_module_body(input, module);
  return success;
}

