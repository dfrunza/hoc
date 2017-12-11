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

bool parse_expr(TokenStream*, AstNode**);

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
      success = compile_error(&input->src_loc, "unexpected `%s`", get_token_printstr(&input->token));
      break;
  }
  return success;
}

bool parse_actual_arg(TokenStream* input, AstNode** node)
{
  *node = 0;
  return parse_expr(input, node);
}

bool parse_actual_arg_list(TokenStream* input, AstNode* call);

bool parse_rest_of_actual_arg_list(TokenStream* input, AstNode* call)
{
  bool success = true;
  if(input->token.kind == eToken_comma && (success = get_next_token(input)))
  {
    success = parse_actual_arg_list(input, call);
  }
  return success;
}

bool parse_actual_arg_list(TokenStream* input, AstNode* call)
{
  bool success = false;
  AstNode* arg = 0;
  if(success = parse_actual_arg(input, &arg))
  {
    if(arg)
    {
      append_list_elem(&call->call.actual_arg_list, arg, eList_ast_node);
      success = parse_rest_of_actual_arg_list(input, call);
    }
  }
  return success;
}

bool parse_rest_of_selector(TokenStream* input, AstNode* left_node, AstNode** node);

bool parse_call(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == eToken_open_parens)
  {
    AstNode* call = *node = new_ast_node(eAstNode_call, clone_source_loc(&input->src_loc));
    call->call.id = left_node;
    init_list(&call->call.actual_arg_list, arena, eList_ast_node);

    if(success = get_next_token(input) && parse_actual_arg_list(input, call))
    {
      if(input->token.kind == eToken_close_parens)
      {
        success = get_next_token(input) && parse_rest_of_selector(input, *node, node);
      }
      else
        success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

bool parse_indexer(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == eToken_open_bracket)
  {
    AstNode* index = *node = new_ast_node(eAstNode_bin_expr, clone_source_loc(&input->src_loc));
    index->bin_expr.op_kind = eOperator_indexer;
    index->bin_expr.left_operand = left_node;

    if(success = get_next_token(input) && parse_expr(input, &index->bin_expr.right_operand))
    {
      if(input->token.kind == eToken_close_bracket)
      {
        if(index->bin_expr.right_operand)
        {
          success = get_next_token(input) && parse_rest_of_selector(input, *node, node);
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
  return success;
}

bool parse_selector(TokenStream*, AstNode**);

bool parse_rest_of_unr_expr(TokenStream* input, AstNode* left_node, AstNode** node)
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

        switch(input->token.kind)
        {
          case eToken_dot:
            bin_expr->bin_expr.op_kind = eOperator_selector;
            break;
          case eToken_arrow_right:
            bin_expr->bin_expr.op_kind = eOperator_indirect_selector;
            break;
          default:
            assert(0);
        }
        if(success = get_next_token(input) && parse_selector(input, &bin_expr->bin_expr.right_operand))
        {
          if(bin_expr->bin_expr.right_operand)
          {
            success = parse_rest_of_unr_expr(input, *node, node);
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

bool parse_unr_expr(TokenStream*, AstNode**);

bool parse_factor(TokenStream* input, AstNode** node)
{
  bool success = true;
  if((success = parse_unr_expr(input, node)) && *node)
  {
    success = parse_rest_of_unr_expr(input, *node, node);
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

        switch(input->token.kind)
        {
          case eToken_star:
            bin_expr->bin_expr.op_kind = eOperator_mul;
            break;
          case eToken_fwd_slash:
            bin_expr->bin_expr.op_kind = eOperator_div;
            break;
          case eToken_percent:
            bin_expr->bin_expr.op_kind = eOperator_mod;
            break;
          default:
            assert(0);
        }

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

        switch(input->token.kind)
        {
          case eToken_plus:
            bin_expr->bin_expr.op_kind = eOperator_add;
            break;
          case eToken_minus:
            bin_expr->bin_expr.op_kind = eOperator_sub;
            break;
          case eToken_pipe:
            bin_expr->bin_expr.op_kind = eOperator_bit_or;
            break;
          case eToken_pipe_pipe:
            bin_expr->bin_expr.op_kind = eOperator_logic_or;
            break;
          case eToken_ampersand:
            bin_expr->bin_expr.op_kind = eOperator_bit_and;
            break;
          case eToken_ampersand_ampersand:
            bin_expr->bin_expr.op_kind = eOperator_logic_and;
            break;
          case eToken_angle_left_left:
            bin_expr->bin_expr.op_kind = eOperator_bit_shift_left;
            break;
          case eToken_angle_right_right:
            bin_expr->bin_expr.op_kind = eOperator_bit_shift_right;
            break;
          case eToken_circumflex:
            bin_expr->bin_expr.op_kind = eOperator_bit_xor;
            break;
          default:
            assert(0);
        }

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

        switch(input->token.kind)
        {
          case eToken_eq:
            bin_expr->bin_expr.op_kind = eOperator_assign;
            break;
          case eToken_eq_eq:
            bin_expr->bin_expr.op_kind = eOperator_eq;
            break;
          case eToken_exclam_eq:
            bin_expr->bin_expr.op_kind = eOperator_not_eq;
            break;
          case eToken_angle_left:
            bin_expr->bin_expr.op_kind = eOperator_less;
            break;
          case eToken_angle_left_eq:
            bin_expr->bin_expr.op_kind = eOperator_less_eq;
            break;
          case eToken_angle_right:
            bin_expr->bin_expr.op_kind = eOperator_greater;
            break;
          case eToken_angle_right_eq:
            bin_expr->bin_expr.op_kind = eOperator_greater_eq;
            break;
          default:
            assert(0);
        }

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

bool parse_cast(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(input->token.kind == eToken_cast && (success = get_next_token(input)))
  {
    AstNode* cast = *node = new_ast_node(eAstNode_bin_expr, clone_source_loc(&input->src_loc));
    cast->bin_expr.op_kind = eOperator_cast;

    if(input->token.kind == eToken_open_parens && (success = get_next_token(input)))
    {
      if(success = parse_type(input, &cast->bin_expr.left_operand))
      {
        if(input->token.kind == eToken_close_parens)
        {
          if(success = (get_next_token(input) && parse_unr_expr(input, &cast->bin_expr.right_operand)))
          {
            if(!cast->bin_expr.right_operand)
            {
              putback_token(input);
              success = compile_error(&input->src_loc, "expression expected, at `%s`", get_token_printstr(&input->token));
            }
          }
        }
        else
          success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
      }
    }
  }
  return success;
}

bool parse_rest_of_selector(TokenStream* input, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(input->token.kind)
  {
    case eToken_open_parens:
      success = parse_call(input, left_node, node);
      break;
    case eToken_open_bracket:
      success = parse_indexer(input, left_node, node);
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
        if(success = (get_next_token(input) && parse_expr(input, node)))
        {
          if(input->token.kind == eToken_close_parens && (success = get_next_token(input)))
          {
            success = parse_rest_of_selector(input, *node, node);
          }
          else
            success = compile_error(&input->src_loc, "expected `)`, actual `%s`", get_token_printstr(&input->token));
        }
      }
      break;

    case eToken_cast:
      success = parse_cast(input, node) && parse_rest_of_selector(input, *node, node);
      break;

    case eToken_true:
    case eToken_false:
    case eToken_int:
    case eToken_float:
    case eToken_string:
    case eToken_char:
      {
        AstNode* lit = *node = new_ast_node(eAstNode_lit, clone_source_loc(&input->src_loc));

        switch(input->token.kind)
        {
          case eToken_int:
            lit->lit.kind = eLiteral_int;
            lit->lit.int_val = *input->token.int_val;
            break;
          case eToken_float:
            lit->lit.kind = eLiteral_float;
            lit->lit.float_val = *input->token.float_val;
            break;
          case eToken_true:
          case eToken_false:
            lit->lit.kind = eLiteral_bool;
            lit->lit.bool_val = (input->token.kind == eToken_true ? 1 : 0);
            break;
          case eToken_char:
            lit->lit.kind = eLiteral_char;
            lit->lit.char_val = input->token.char_val;
            break;
          case eToken_string:
            lit->lit.kind = eLiteral_string;
            lit->lit.str_val = input->token.str_val;
            break;
          default:
            assert(0);
        }

        success = get_next_token(input);
      }
      break;

    case eToken_id:
      {
        AstNode* id = *node = new_ast_node(eAstNode_id, clone_source_loc(&input->src_loc));
        id->id.name = input->token.lexeme;
        success = get_next_token(input) && parse_rest_of_selector(input, *node, node);
      }
      break;
  }
  return success;
}

bool parse_formal_arg(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(input->token.kind)
  {
    case eToken_open_bracket:
    case eToken_id:
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
      break;
  }
  return success;
}

bool parse_unr_expr(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(input->token.kind)
  {
    case eToken_exclam:
    case eToken_star:
    case eToken_ampersand:
    case eToken_minus:
      {
        AstNode* unr_expr = *node = new_ast_node(eAstNode_unr_expr, clone_source_loc(&input->src_loc));

        switch(input->token.kind)
        {
          case eToken_exclam:
            unr_expr->unr_expr.op_kind = eOperator_logic_not;
            break;
          case eToken_star:
            unr_expr->unr_expr.op_kind = eOperator_deref;
            break;
          case eToken_ampersand:
            unr_expr->unr_expr.op_kind = eOperator_address_of;
            break;
          case eToken_minus:
            unr_expr->unr_expr.op_kind = eOperator_neg;
            break;
          default:
            assert(0);
        }

        if(success && (success = get_next_token(input) && parse_factor(input, &unr_expr->unr_expr.operand)))
        {
          if(!unr_expr->unr_expr.operand)
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
  *node = 0;
  bool success = true;
  if((success = parse_assignment(input, node)) && *node)
  {
    success = parse_rest_of_assignment(input, *node, node);
  }
  return success;
}

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

bool parse_stmt_list(TokenStream* input, AstNode* block);
bool parse_stmt(TokenStream* input, AstNode** node);

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
  return parse_stmt(input, node);
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
  *node = 0;
  return parse_stmt(input, node);
}

bool parse_if(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(input->token.kind == eToken_if)
  {
    AstNode* if_ = *node = new_ast_node(eAstNode_if_, clone_source_loc(&input->src_loc));
    if(success = get_next_token(input))
    {
      if(input->token.kind == eToken_open_parens)
      {
        if(success = (get_next_token(input) && parse_expr(input, &if_->if_.cond_expr)))
        {
          if(input->token.kind == eToken_close_parens)
          {
            success = get_next_token(input) && parse_if_body(input, &if_->if_.body) && parse_else(input, &if_->if_.else_body);
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

bool parse_while_body(TokenStream* input, AstNode** node)
{
  *node = 0;
  return parse_stmt(input, node);
}

bool parse_while(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(input->token.kind == eToken_while)
  {
    AstNode* while_ = *node = new_ast_node(eAstNode_while_, clone_source_loc(&input->src_loc));
    if(success = get_next_token(input))
    {
      if(input->token.kind == eToken_open_parens)
      {
        if(success = (get_next_token(input) && parse_expr(input, &while_->while_.cond_expr)))
        {
          if(input->token.kind == eToken_close_parens)
          {
            success = get_next_token(input) && parse_while_body(input, &while_->while_.body);
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

bool parse_return(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == eToken_return)
  {
    AstNode* ret = *node = new_ast_node(eAstNode_ret, clone_source_loc(&input->src_loc));
    if(success = get_next_token(input))
    {
      success = parse_expr(input, &ret->ret.ret_expr);
    }
  }
  return success;
}

bool parse_continue(TokenStream* input, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == eToken_continue)
  {
    AstNode* loop_ctrl = *node = new_ast_node(eAstNode_loop_ctrl, clone_source_loc(&input->src_loc));
    loop_ctrl->loop_ctrl.kind = eLoopCtrl_continue;
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
    AstNode* loop_ctrl = *node = new_ast_node(eAstNode_loop_ctrl, clone_source_loc(&input->src_loc));
    loop_ctrl->loop_ctrl.kind = eLoopCtrl_break;
    success = get_next_token(input);
  }
  return success;
}

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
    case eToken_return:
      success = parse_return(input, node) && consume_semicolon(input);
      break;
    case eToken_break:
      success = parse_break(input, node) && consume_semicolon(input);
      break;
    case eToken_continue:
      success = parse_continue(input, node) && consume_semicolon(input);
      break;
    default:
      if((success = parse_expr(input, node)) && *node)
      {
        success = consume_semicolon(input);
      }
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
        case eAstNode_if_:
        case eAstNode_while_:
        case eAstNode_bin_expr:
        case eAstNode_unr_expr:
        case eAstNode_block:
        case eAstNode_id:
        case eAstNode_call:
        case eAstNode_ret:
        case eAstNode_loop_ctrl:
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

bool parse_struct_member_list(TokenStream*, List*);

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
            var->var.name = input->token.lexeme;
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
#endif

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

