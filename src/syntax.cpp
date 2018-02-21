char* get_operator_printstr(eOperator op)
{
  char* str = "???";
  switch(op)
  {
    case eOperator_add:
      str = "+";
    break;
    
    case eOperator_sub:
      str = "-";
    break;
    
    case eOperator_mul:
      str = "*";
    break;
    
    case eOperator_div:
      str = "/";
    break;
    
    case eOperator_mod:
      str = "%";
    break;
    
    case eOperator_neg:
      str = "-";
    break;
    
    case eOperator_deref:
      str = "^";
    break;
    
    case eOperator_address_of:
      str = "&";
    break;
    
    case eOperator_selector:
      str = ".";
    break;
    
    case eOperator_indirect_selector:
      str = "->";
    break;
#if 0
    case eOperator_pre_decr:
    case eOperator_post_decr:
    str = "--";
    break;
    
    case eOperator_pre_incr:
    case eOperator_post_incr:
    str = "++";
    break;
#endif
    case eOperator_eq:
      str = "==";
    break;
    
    case eOperator_not_eq:
      str = "<>";
    break;
    
    case eOperator_less:
      str = "<";
    break;
    
    case eOperator_less_eq:
      str = "<=";
    break;
    
    case eOperator_greater:
      str = ">";
    break;
    
    case eOperator_greater_eq:
      str = ">=";
    break;
    
    case eOperator_logic_and:
      str = "and";
    break;
    
    case eOperator_logic_or:
      str = "or";
    break;
    
    case eOperator_logic_not:
      str = "not";
    break;
    
    case eOperator_bit_and:
      str = "&";
    break;
    
    case eOperator_bit_or:
      str = "|";
    break;
    
    case eOperator_bit_xor:
      str = "~";
    break;
    
    case eOperator_bit_not:
      str = "!";
    break;
  }
  return str;
}

SourceLoc* clone_source_loc(Parser* parser)
{
  SourceLoc* clone = push_struct(parser->arena, SourceLoc);
  *clone = *parser->src_loc;
  return clone;
}

AstNode* new_ast_node(Parser* parser, eAstNode kind)
{
  AstNode* node = push_struct(parser->arena, AstNode);
  node->src_loc = clone_source_loc(parser);
  node->kind = kind;
  return node;
}

Parser* new_parser(MemoryArena* arena)
{
  Parser* parser = push_struct(arena, Parser);
  parser->arena = arena;
  parser->includes = list_new(arena, eList_ast_node);

  Lexer* lexer = parser->lexer = new_lexer(arena);
  parser->token = &lexer->token;
  parser->src_loc = &lexer->src_loc;

  return parser;
}

void set_parser_input(Parser* parser, char* text, PlatformFile* file)
{
  parser->file = file;
  set_lexer_input(parser->lexer, text, file->path);
}

Parser* new_included_parser(Parser* enclosing_parser)
{
  Parser* included_parser = new_parser(enclosing_parser->arena);
  included_parser->includes = enclosing_parser->includes;

  return included_parser;
}

bool consume_semicolon(Parser* parser)
{
  bool success = true;
  if(parser->token->kind == eToken_semicolon)
  {
    success = get_next_token(parser->lexer);
  }
  else
    success = compile_error(parser->arena, parser->src_loc, "`;` was expected at `%s`", get_token_printstr(parser->token));

  return success;
}

bool is_ast_node_valid_expr_operand(AstNode* node)
{
  bool valid = false;
  switch(node->kind)
  {
    case eAstNode_lit:
    case eAstNode_id:
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_call:
    case eAstNode_index:
    case eAstNode_cast:
      valid = true;
    break;
  }
  return valid;
}

bool parse_actual_args(Parser* parser, AstNode* args);

bool parse_rest_of_actual_args(Parser* parser, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  if((parser->token->kind == eToken_comma) && (success = get_next_token(parser->lexer)))
  {
    success = parse_actual_args(parser, args);
  }
  return success;
}

bool parse_expr(Parser* parser, AstNode** node);

bool parse_actual_args(Parser* parser, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;

  AstNode* expr = 0;
  if(success = parse_expr(parser, &expr))
  {
    if(expr)
    {
      AstNode* call_arg = new_ast_node(parser, eAstNode_call_arg);
      call_arg->call_arg.expr = expr;

      list_append(&args->args.node_list, call_arg, eList_ast_node);
      success = parse_rest_of_actual_args(parser, args);
    }
  }
  return success;
}

bool parse_rest_of_selector(Parser* parser, AstNode* left_node, AstNode** node);

bool parse_call(Parser* parser, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(parser->token->kind == eToken_open_parens)
  {
    AstNode* call = *node = new_ast_node(parser, eAstNode_call);
    call->call.expr = left_node;
    AstNode* args = call->call.args = new_ast_node(parser, eAstNode_node_list);
    list_init(&args->args.node_list, parser->arena, eList_ast_node);

    if(success = get_next_token(parser->lexer) && parse_actual_args(parser, call->call.args))
    {
      if(parser->token->kind == eToken_close_parens)
      {
        if(success = get_next_token(parser->lexer))
        {
          success = parse_rest_of_selector(parser, *node, node);
        }
      }
      else
        success = compile_error(parser->arena, parser->src_loc, "`)` was expected at `%s`", get_token_printstr(parser->token));
    }
  }
  return success;
}

bool parse_index_recursive(Parser* parser, AstNode* left_node, AstNode** node, int* ndim)
{
  *node = left_node;
  bool success = true;

  if(parser->token->kind == eToken_open_bracket)
  {
    AstNode* index = *node = new_ast_node(parser, eAstNode_index);
    index->index.array_expr = left_node;
    index->index.ndim = *ndim;

    if(success = get_next_token(parser->lexer) && parse_expr(parser, &index->index.i_expr))
    {
      if(parser->token->kind == eToken_close_bracket)
      {
        if(index->index.i_expr)
        {
          *ndim = *ndim + 1;
          if(success = get_next_token(parser->lexer) && parse_index_recursive(parser, *node, node, ndim))
          {
            index->index.ndim = *ndim - index->index.ndim;
          }
        }
        else
          success = compile_error(parser->arena, parser->src_loc, "expression was expected at `%s`", get_token_printstr(parser->token));
      }
      else
      {
        success = compile_error(parser->arena, parser->src_loc, "`]` was expected at `%s`", get_token_printstr(parser->token));
      }
    }
  }
  return success;
}

bool parse_index(Parser* parser, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  int ndim = 1;
  success = parse_index_recursive(parser, left_node, node, &ndim);
  return success;
}

bool parse_selector(Parser* parser, AstNode** node);

bool parse_rest_of_unr_expr(Parser* parser, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken_dot:
    case eToken_arrow_right:
      {
        AstNode* bin_expr = *node = new_ast_node(parser, eAstNode_bin_expr);
        bin_expr->bin_expr.left_operand = left_node;

        switch(parser->token->kind)
        {
          case eToken_dot:
            bin_expr->bin_expr.op = eOperator_selector;
          break;

          case eToken_arrow_right:
            bin_expr->bin_expr.op = eOperator_indirect_selector;
          break;

          default:
            assert(0);
        }

        AstNode* right_operand = 0;
        if(success = get_next_token(parser->lexer) && parse_selector(parser, &right_operand))
        {
          if(right_operand)
          {
            switch(right_operand->kind)
            {
              case eAstNode_id:
                bin_expr->bin_expr.right_operand = right_operand;
                success = parse_rest_of_unr_expr(parser, *node, node);
              break;

              default:
                success = compile_error(parser->arena, right_operand->src_loc, "invalid operand");
                break;
            }
          }
          else
            success = compile_error(parser->arena, parser->src_loc, "operand was expected at `%s`", get_token_printstr(parser->token));
        }
      }
      break;
  }
  return success;
}

bool parse_unr_expr(Parser* parser, AstNode** node);

bool parse_factor(Parser* parser, AstNode** node)
{
  bool success = true;
  if((success = parse_unr_expr(parser, node)) && *node)
  {
    success = parse_rest_of_unr_expr(parser, *node, node);
  }
  return success;
}

bool parse_rest_of_factor(Parser* parser, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken_star:
    case eToken_fwd_slash:
    case eToken_percent:
    case eToken_and:
    case eToken_ampersand:
    {
      AstNode* bin_expr = *node = new_ast_node(parser, eAstNode_bin_expr);
      bin_expr->bin_expr.left_operand = left_node;

      switch(parser->token->kind)
      {
        case eToken_star:
          bin_expr->bin_expr.op = eOperator_mul;
        break;

        case eToken_fwd_slash:
          bin_expr->bin_expr.op = eOperator_div;
        break;

        case eToken_percent:
          bin_expr->bin_expr.op = eOperator_mod;
        break;

        case eToken_and:
          bin_expr->bin_expr.op = eOperator_logic_and;
        break;

        case eToken_ampersand:
          bin_expr->bin_expr.op = eOperator_bit_and;
        break;

        default:
          assert(0);
      }

      AstNode* right_operand = 0;
      if(success = get_next_token(parser->lexer) && parse_factor(parser, &right_operand))
      {
        if(right_operand)
        {
          if(is_ast_node_valid_expr_operand(right_operand))
          {
            bin_expr->bin_expr.right_operand = right_operand;
            success = parse_rest_of_factor(parser, *node, node); // left-associativity
          }
          else
            success = compile_error(parser->arena, right_operand->src_loc, "invalid operand");
        }
        else
          success = compile_error(parser->arena, parser->src_loc, "operand was expected at `%s`", get_token_printstr(parser->token));
      }
    }
      break;
  }
  return success;
}

bool parse_term(Parser* parser, AstNode** node)
{
  bool success = true;
  if((success = parse_factor(parser, node)) && *node)
  {
    success = parse_rest_of_factor(parser, *node, node);
  }
  return success;
}

bool parse_rest_of_term(Parser* parser, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken_plus:
    case eToken_minus:
    case eToken_pipe:
    case eToken_angle_right_right:
    case eToken_angle_left_left:
    case eToken_tilde:
    case eToken_eq_eq:
    case eToken_angle_left:
    case eToken_angle_left_eq:
    case eToken_angle_right:
    case eToken_angle_right_eq:
    case eToken_angle_left_right:
    case eToken_or:
      {
        AstNode* bin_expr = *node = new_ast_node(parser, eAstNode_bin_expr);
        bin_expr->bin_expr.left_operand = left_node;

        switch(parser->token->kind)
        {
          case eToken_plus:
            bin_expr->bin_expr.op = eOperator_add;
          break;

          case eToken_minus:
            bin_expr->bin_expr.op = eOperator_sub;
          break;

          case eToken_pipe:
            bin_expr->bin_expr.op = eOperator_bit_or;
          break;

          case eToken_angle_left_left:
            bin_expr->bin_expr.op = eOperator_bit_shift_left;
          break;

          case eToken_angle_right_right:
            bin_expr->bin_expr.op = eOperator_bit_shift_right;
          break;

          case eToken_tilde:
            bin_expr->bin_expr.op = eOperator_bit_xor;
          break;

          case eToken_eq_eq:
            bin_expr->bin_expr.op = eOperator_eq;
          break;

          case eToken_angle_left_right:
            bin_expr->bin_expr.op = eOperator_not_eq;
          break;

          case eToken_angle_left:
            bin_expr->bin_expr.op = eOperator_less;
          break;

          case eToken_angle_left_eq:
            bin_expr->bin_expr.op = eOperator_less_eq;
          break;

          case eToken_angle_right:
            bin_expr->bin_expr.op = eOperator_greater;
          break;

          case eToken_angle_right_eq:
            bin_expr->bin_expr.op = eOperator_greater_eq;
          break;

          case eToken_or:
            bin_expr->bin_expr.op = eOperator_logic_or;
          break;

          default:
            assert(0);
        }

        AstNode* right_operand = 0;
        if(success && (success = get_next_token(parser->lexer) && parse_term(parser, &right_operand)))
        {
          if(right_operand)
          {
            if(is_ast_node_valid_expr_operand(right_operand))
            {
              bin_expr->bin_expr.right_operand = right_operand;
              success = parse_rest_of_term(parser, *node, node); // left-associativity
            }
            else
              success = compile_error(parser->arena, right_operand->src_loc, "invalid operand");
          }
          else
            success = compile_error(parser->arena, parser->src_loc, "operand was expected at `%s`", get_token_printstr(parser->token));
        }
      }
      break;
  }
  return success;
}

bool parse_assignment(Parser* parser, AstNode** node)
{
  bool success = true;
  if((success = parse_term(parser, node)) && *node)
  {
    success = parse_rest_of_term(parser, *node, node);
  }
  return success;
}

bool parse_rest_of_assignment(Parser* parser, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken_eq:
      {
        AstNode* assign = *node = new_ast_node(parser, eAstNode_assign);
        assign->assign.dest_expr = left_node;

        AstNode* source_expr = 0;
        if(success = get_next_token(parser->lexer) && parse_expr(parser, &source_expr))
        {
          if(source_expr)
          {
            if(is_ast_node_valid_expr_operand(source_expr))
            {
              assign->assign.source_expr = source_expr;
            }
            else
              success = compile_error(parser->arena, source_expr->src_loc, "invalid operand in assignment expression");
          }
          else
            success = compile_error(parser->arena, parser->src_loc, "operand was expected at `%s`", get_token_printstr(parser->token));
        }
      }
      break;
  }
  return success;
}

bool parse_id(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token->kind == eToken_id)
  {
    AstNode* id = *node = new_ast_node(parser, eAstNode_id);
    id->id.name = parser->token->lexeme;
    success = get_next_token(parser->lexer);
  }
  return success;
}

bool parse_basic_type(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken_int:
    case eToken_float:
    case eToken_bool:
    case eToken_char:
    case eToken_void:
    case eToken_auto:
    {
      AstNode* basic_type = *node = new_ast_node(parser, eAstNode_basic_type);
      switch(parser->token->kind)
      {
        case eToken_int:
          basic_type->basic_type.kind = eBasicType_int;
          break;
        case eToken_float:
          basic_type->basic_type.kind = eBasicType_float;
          break;
        case eToken_bool:
          basic_type->basic_type.kind = eBasicType_bool;
          break;
        case eToken_char:
          basic_type->basic_type.kind = eBasicType_char;
          break;
        case eToken_void:
          basic_type->basic_type.kind = eBasicType_void;
          break;
        case eToken_auto:
          basic_type->basic_type.kind = eBasicType_auto;
          break;
        default:
          assert(0);
      }
      success = get_next_token(parser->lexer);
    }
    break;
  }
  return success;
}

bool parse_rest_of_cast(Parser* parser, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

#if 0
  AstNode* right_node = 0;
  if((success = parse_unr_expr(parser, &right_node)) && right_node)
  {
    AstNode* cast = *node = new_ast_node(eAstNode_bin_expr, clone_source_loc());
    cast->bin_expr.op = eOperator_cast;
    cast->bin_expr.left_operand = left_node;
    cast->bin_expr.right_operand = right_node;
  }
#else
  switch(parser->token->kind)
  {
    case eToken_cast:
    {
      AstNode* cast = *node = new_ast_node(parser, eAstNode_cast);
      cast->cast.to_type = left_node;

      if(success = get_next_token(parser->lexer) && parse_unr_expr(parser, &cast->cast.from_expr))
      {
        if(!cast->cast.from_expr)
        {
          success = compile_error(parser->arena, parser->src_loc, "expression was expected at `%s`", get_token_printstr(parser->token));
        }
      }
    }
    break;
  }
#endif
  return success;
}

bool parse_deref(Parser* parser, AstNode** node);

bool parse_rest_of_deref(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken_circumflex:
      success = parse_deref(parser, node);
    break;

    case eToken_id:
      success = parse_id(parser, node);
    break;

    case eToken_void:
    case eToken_int:
    case eToken_float:
    case eToken_bool:
    case eToken_char:
    case eToken_auto:
      success = parse_basic_type(parser, node);
    break;

    case eToken_open_parens:
    {
      if(success = get_next_token(parser->lexer) && parse_expr(parser, node))
      {
        if(parser->token->kind == eToken_close_parens)
        {
          success = get_next_token(parser->lexer) && parse_rest_of_cast(parser, *node, node);
        }
        else
          success = compile_error(parser->arena, parser->src_loc, "`)` was expected at `%s`", get_token_printstr(parser->token));
      }
    }
    break;
  }
  return success;
}

bool parse_deref(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* deref = *node = new_ast_node(parser, eAstNode_unr_expr);
  deref->unr_expr.op = eOperator_deref;
  if(success = get_next_token(parser->lexer) && parse_rest_of_deref(parser, &deref->unr_expr.operand))
  {
    if(!deref->unr_expr.operand)
    {
      success = compile_error(parser->arena, parser->src_loc, "expression was expected at `%s`", get_token_printstr(parser->token));
    }
  }
  return success;
}

bool parse_array(Parser* parser, AstNode** node);
bool parse_pointer(Parser* parser, AstNode* left_node, AstNode** node);

bool parse_rest_of_array(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken_open_bracket:
      success = parse_array(parser, node);
    break;

    case eToken_id:
      success = compile_error(parser->arena, parser->src_loc, "unknown type id `%s`", get_token_printstr(parser->token));
    break;

    case eToken_void:
    case eToken_int:
    case eToken_float:
    case eToken_bool:
    case eToken_char:
    case eToken_auto:
      success = parse_basic_type(parser, node);
    break;

    case eToken_open_parens:
    {
      if(success = get_next_token(parser->lexer) && parse_array(parser, node) && parse_pointer(parser, *node, node))
      {
        if(parser->token->kind == eToken_close_parens)
        {
          success = get_next_token(parser->lexer);
        }
        else
          success = compile_error(parser->arena, parser->src_loc, "`)` was expected at `%s`", get_token_printstr(parser->token));
      }
    }
    break;
  }
  return success;
}

bool parse_array(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token->kind == eToken_open_bracket)
  {
    AstNode* array = *node = new_ast_node(parser, eAstNode_array);
    if(success = get_next_token(parser->lexer) && parse_expr(parser, &array->array.size_expr))
    {
      if(parser->token->kind == eToken_close_bracket)
      {
        if(array->array.size_expr)
        {
          if(success = get_next_token(parser->lexer) && parse_rest_of_array(parser, &array->array.elem_expr))
          {
            if(!array->array.elem_expr)
            {
              success = compile_error(parser->arena, parser->src_loc,  "expression was expected at `%s`", get_token_printstr(parser->token));
            }
          }
        }
        else
          success = compile_error(parser->arena, parser->src_loc, "expression was expected at `%s`", get_token_printstr(parser->token));
      }
      else
        success = compile_error(parser->arena, parser->src_loc,  "`]` was expected at `%s`", get_token_printstr(parser->token));
    }
  }
  return success;
}

bool parse_cast(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken_open_parens:
    {
      if(success = get_next_token(parser->lexer) && parse_expr(parser, node))
      {
        if(parser->token->kind == eToken_close_parens)
        {
          if(*node)
          {
            success = get_next_token(parser->lexer) && parse_rest_of_cast(parser, *node, node);
          }
          else
            success = compile_error(parser->arena, parser->src_loc, "expression was expected at `%s`", get_token_printstr(parser->token));
        }
        else
          success = compile_error(parser->arena, parser->src_loc, "`)` was expected at `%s`", get_token_printstr(parser->token));
      }
    }
    break;

    case eToken_circumflex:
      success = parse_deref(parser, node) && parse_index(parser, *node, node);
    break;

    case eToken_open_bracket:
      success = parse_array(parser, node) && parse_pointer(parser, *node, node);
    break;

    case eToken_id:
      success = parse_id(parser, node) && parse_pointer(parser, *node, node) && parse_index(parser, *node, node);
    break;

    case eToken_void:
    case eToken_int:
    case eToken_float:
    case eToken_bool:
    case eToken_char:
    case eToken_auto:
      success = parse_basic_type(parser, node) && parse_pointer(parser, *node, node) && parse_index(parser, *node, node);
    break;
  }
  return success;
}

bool parse_pointer(Parser* parser, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(parser->token->kind == eToken_circumflex)
  {
    AstNode* pointer = *node = new_ast_node(parser, eAstNode_pointer);
    pointer->pointer.pointee = left_node;

    if((success = get_next_token(parser->lexer)) && parser->token->kind == eToken_circumflex)
    {
      success = parse_pointer(parser, *node, node);
    }
  }
  return success;
}

bool parse_rest_of_selector(Parser* parser, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken_open_parens:
      success = parse_call(parser, left_node, node);
    break;

    case eToken_open_bracket:
      success = parse_index(parser, left_node, node);
    break;
  }
  return success;
}

bool parse_lit(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* lit = *node = new_ast_node(parser, eAstNode_lit);

  switch(parser->token->kind)
  {
    case eToken_int_val:
    {
      lit->lit.kind = eLiteral_int;
      lit->lit.int_val = *parser->token->int_val;
    }
    break;

    case eToken_float_val:
    {
      lit->lit.kind = eLiteral_float;
      lit->lit.float_val = *parser->token->float_val;
    }
    break;

    case eToken_true:
    case eToken_false:
    {
      lit->lit.kind = eLiteral_bool;
      lit->lit.bool_val = (parser->token->kind == eToken_true ? 1 : 0);
    }
    break;

    case eToken_char_val:
    {
      lit->lit.kind = eLiteral_char;
      lit->lit.char_val = parser->token->char_val;
    }
    break;

    case eToken_str_val:
    {
      lit->lit.kind = eLiteral_str;
      lit->lit.str_val = parser->token->str_val;
    }
    break;

    default: assert(0);
  }

  success = get_next_token(parser->lexer);

  return success;
}

bool parse_selector(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken_true:
    case eToken_false:
    case eToken_int_val:
    case eToken_float_val:
    case eToken_char_val:
    case eToken_str_val:
      success = parse_lit(parser, node);
    break;

    case eToken_open_parens:
    case eToken_open_bracket:
    case eToken_circumflex:
    case eToken_id:
    case eToken_int:
    case eToken_float:
    case eToken_bool:
    case eToken_char:
    case eToken_void:
    case eToken_auto:
      success = parse_cast(parser, node) && parse_rest_of_selector(parser, *node, node);
    break;
  }
  return success;
}

bool parse_formal_arg(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if((success = parse_expr(parser, node)) && *node)
  {
    if(parser->token->kind == eToken_id)
    {
      AstNode* type = *node;
      AstNode* var = *node = new_ast_node(parser, eAstNode_var);
      var->var.type = type;
      var->var.name = parser->token->lexeme;

      success = get_next_token(parser->lexer);
    }
    else
      success = compile_error(parser->arena, parser->src_loc, "identifier was expected at `%s`", get_token_printstr(parser->token));
  }
  return success;
}

bool parse_unr_expr(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken_exclam:
    case eToken_not:
    case eToken_ampersand:
    case eToken_minus:
      {
        AstNode* unr_expr = *node = new_ast_node(parser, eAstNode_unr_expr);

        switch(parser->token->kind)
        {
          case eToken_exclam:
            unr_expr->unr_expr.op = eOperator_bit_not;
            break;
          case eToken_not:
            unr_expr->unr_expr.op = eOperator_logic_not;
            break;
          case eToken_ampersand:
            unr_expr->unr_expr.op = eOperator_address_of;
            break;
          case eToken_minus:
            unr_expr->unr_expr.op = eOperator_neg;
            break;
          default:
            assert(0);
        }

        if(success && (success = get_next_token(parser->lexer) && parse_factor(parser, &unr_expr->unr_expr.operand)))
        {
          if(!unr_expr->unr_expr.operand)
          {
            success = compile_error(parser->arena, parser->src_loc, "operand was expected at `%s`", get_token_printstr(parser->token));
          }
        }
      }
      break;

    default:
      success = parse_selector(parser, node);
      break;
  }
  return success;
}

bool parse_expr(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;
  if((success = parse_assignment(parser, node)) && *node)
  {
    success = parse_rest_of_assignment(parser, *node, node);
  }
  return success;
}

bool parse_modifier(Parser* parser, eModifier* modifier)
{
  bool success = true;
  *modifier = eModifier_None;

  switch(parser->token->kind)
  {
    case eToken_extern:
    {
      *modifier = eModifier_extern;
      success = get_next_token(parser->lexer);
    }
    break;

    case eToken_const:
    {
      *modifier = eModifier_const;
      success = get_next_token(parser->lexer);
    }
    break;
  }

  return success;
}

bool parse_formal_args(Parser* parser, AstNode* args);

bool parse_rest_of_formal_args(Parser* parser, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  if((parser->token->kind == eToken_comma) && (success = get_next_token(parser->lexer)))
  {
    success = parse_formal_args(parser, args);
  }
  return success;
}

bool parse_formal_args(Parser* parser, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;

  AstNode* arg = 0;
  if(success = parse_formal_arg(parser, &arg))
  {
    if(arg)
    {
      list_append(&args->args.node_list, arg, eList_ast_node);
      success = parse_rest_of_formal_args(parser, args);
    }
  }
  return success;
}

bool parse_empty(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(parser->token->kind == eToken_semicolon)
  {
    *node = new_ast_node(parser, eAstNode_empty);
    success = get_next_token(parser->lexer);
  }
  return success;
}

bool parse_block_stmts(Parser* parser, AstNode* block);

bool parse_block(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(parser->token->kind == eToken_open_brace)
  {
    AstNode* block = *node = new_ast_node(parser, eAstNode_block);
    list_init(&block->block.nodes, parser->arena, eList_ast_node);
    list_init(&block->block.vars, parser->arena, eList_ast_node);
    list_init(&block->block.stmts, parser->arena, eList_ast_node);

    if(success = (get_next_token(parser->lexer) && parse_block_stmts(parser, block)))
    {
      if(parser->token->kind == eToken_close_brace)
      {
        success = get_next_token(parser->lexer);
      }
      else
        success = compile_error(parser->arena, parser->src_loc, "`}` was expected at `%s`", get_token_printstr(parser->token));
    }
  }
  return success;
}

bool parse_block_stmt(Parser* parser, AstNode** node);

bool parse_else(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(parser->token->kind == eToken_else)
  {
    success = get_next_token(parser->lexer) && parse_block_stmt(parser, node);
  }
  return success;
}

bool parse_if(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(parser->token->kind == eToken_if)
  {
    AstNode* if_ = *node = new_ast_node(parser, eAstNode_if);
    if(success = get_next_token(parser->lexer))
    {
      if(parser->token->kind == eToken_open_parens)
      {
        if(success = (get_next_token(parser->lexer) && parse_expr(parser, &if_->if_.cond_expr)))
        {
          if(if_->if_.cond_expr)
          {
            if(parser->token->kind == eToken_close_parens)
            {
              success = get_next_token(parser->lexer) && parse_block_stmt(parser, &if_->if_.body) && parse_else(parser, &if_->if_.else_body);
            }
            else
              success = compile_error(parser->arena, parser->src_loc, "`)` was expected at `%s`", get_token_printstr(parser->token));
          }
          else
            success = compile_error(parser->arena, parser->src_loc, "expression was expected at `%s`", get_token_printstr(parser->token));
        }
      }
      else
        success = compile_error(parser->arena, parser->src_loc, "`(` was expected at `%s`", get_token_printstr(parser->token));
    }
  }
  return success;
}

bool parse_do_while(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(parser->token->kind == eToken_do)
  {
    AstNode* do_while = *node = new_ast_node(parser, eAstNode_do_while);
    if(success = get_next_token(parser->lexer) && parse_block_stmt(parser, &do_while->do_while.body))
    {
      if(parser->token->kind == eToken_while)
      {
        if(success = get_next_token(parser->lexer))
        {
          if(parser->token->kind == eToken_open_parens)
          {
            if(success = get_next_token(parser->lexer) && parse_expr(parser, &do_while->do_while.cond_expr))
            {
              if(do_while->do_while.cond_expr)
              {
                if(parser->token->kind == eToken_close_parens)
                  success = get_next_token(parser->lexer);
                else
                  success = compile_error(parser->arena, parser->src_loc, "`)` was expected at `%s`", get_token_printstr(parser->token));
              }
              else
                success = compile_error(parser->arena, parser->src_loc, "expression was expected at `%s`", get_token_printstr(parser->token));
            }
          }
          else
            success = compile_error(parser->arena, parser->src_loc, "`(` was expected at `%s`", get_token_printstr(parser->token));
        }
      }
      else
        success = compile_error(parser->arena, parser->src_loc, "`while` was expected at `%s`", get_token_printstr(parser->token));
    }
  }
  return success;
}

bool parse_while(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(parser->token->kind == eToken_while)
  {
    AstNode* while_ = *node = new_ast_node(parser, eAstNode_while);
    if(success = get_next_token(parser->lexer))
    {
      if(parser->token->kind == eToken_open_parens)
      {
        if(success = get_next_token(parser->lexer) && parse_expr(parser, &while_->while_.cond_expr))
        {
          if(while_->while_.cond_expr)
          {
            if(parser->token->kind == eToken_close_parens)
            {
              success = get_next_token(parser->lexer) && parse_block_stmt(parser, &while_->while_.body);
            }
            else
              success = compile_error(parser->arena, parser->src_loc, "`)` was expected at `%s`", get_token_printstr(parser->token));
          }
          else
            success = compile_error(parser->arena, parser->src_loc, "expression was expected at `%s`", get_token_printstr(parser->token));
        }
      }
      else
        success = compile_error(parser->arena, parser->src_loc, "`(` was expected at `%s`", get_token_printstr(parser->token));
    }
  }
  return success;
}

bool parse_return(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token->kind == eToken_return)
  {
    AstNode* ret = *node = new_ast_node(parser, eAstNode_return);
    AstNode* ret_expr = 0;
    if(success = get_next_token(parser->lexer) && parse_expr(parser, &ret_expr))
    {
      if(ret_expr)
      {
        if(is_ast_node_valid_expr_operand(ret_expr))
          ret->ret.expr = ret_expr;
        else
          success = compile_error(parser->arena, ret_expr->src_loc, "invalid return expression");
      }
    }
  }
  return success;
}

bool parse_continue(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token->kind == eToken_continue)
  {
    AstNode* loop_ctrl = *node = new_ast_node(parser, eAstNode_loop_ctrl);
    loop_ctrl->loop_ctrl.kind = eLoopCtrl_continue;
    success = get_next_token(parser->lexer);
  }
  return success;
}

bool parse_break(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token->kind == eToken_break)
  {
    AstNode* loop_ctrl = *node = new_ast_node(parser, eAstNode_loop_ctrl);
    loop_ctrl->loop_ctrl.kind = eLoopCtrl_break;
    success = get_next_token(parser->lexer);
  }
  return success;
}

bool parse_block_var(Parser* parser, char* name, eModifier modifier, AstNode* var_type, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token->kind == eToken_id)
  {
    AstNode* var = *node = new_ast_node(parser, eAstNode_var);
    var->var.type = var_type;
    var->var.name = parser->token->lexeme;

    if(success = get_next_token(parser->lexer))
    {
      if(parser->token->kind == eToken_eq)
      {
        success = get_next_token(parser->lexer) && parse_expr(parser, &var->var.init_expr);
      }
    }
  }

  return success;
}

bool parse_block_stmt(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;
  switch(parser->token->kind)
  {
    case eToken_open_brace:
      success = parse_block(parser, node);
    break;

    case eToken_if:
      success = parse_if(parser, node);
    break;

    case eToken_do:
      success = parse_do_while(parser, node);
    break;

    case eToken_while:
      success = parse_while(parser, node);
    break;

    case eToken_semicolon:
      success = parse_empty(parser, node);
    break;

    case eToken_return:
      success = parse_return(parser, node) && consume_semicolon(parser);
    break;

    case eToken_break:
      success = parse_break(parser, node) && consume_semicolon(parser);
    break;

    case eToken_continue:
      success = parse_continue(parser, node) && consume_semicolon(parser);
    break;

    default:
    {
      eModifier modifier = eModifier_None;

      if(success = (parse_modifier(parser, &modifier) && parse_expr(parser, node)))
      {
        if(*node)
        {
          if(parser->token->kind == eToken_id)
          {
            char* var_name = parser->token->lexeme;
            success = parse_block_var(parser, var_name, modifier, *node, node) && consume_semicolon(parser);
          }
          else
          {
            success = consume_semicolon(parser);
          }
        }
        else if(modifier != eModifier_None)
          success = compile_error(parser->arena, parser->src_loc, "statement was expected at `%s`", get_token_printstr(parser->token));
      }
    }
    break;
  }
  return success;
}

bool parse_block_stmts(Parser* parser, AstNode* block)
{
  assert(KIND(block, eAstNode_block));

  bool success = true;
  AstNode* stmt = 0;
  if(success = parse_block_stmt(parser, &stmt))
  {
    if(stmt)
    {
      list_append(&block->block.nodes, stmt, eList_ast_node);
      switch(stmt->kind)
      {
        case eAstNode_var:
          list_append(&block->block.vars, stmt, eList_ast_node);
        break;

        case eAstNode_empty:
        break;

        default:
          list_append(&block->block.stmts, stmt, eList_ast_node);
        break;
      }
      success = parse_block_stmts(parser, block);
    }
  }
  return success;
}

bool parse_proc_body(Parser* parser, AstNode* proc)
{
  bool success = true;
  switch(parser->token->kind)
  {
    case eToken_open_brace:
      success = parse_block(parser, &proc->proc.body);
    break;

    case eToken_semicolon:
      success = parse_empty(parser, &proc->proc.body);
    break;

    default:
      success = compile_error(parser->arena, parser->src_loc, "unexpected `%s`", get_token_printstr(parser->token));
      break;
  }
  return success;
}

#if 0
bool parse_enum(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(token.kind == eToken_enum)
  {
    AstNode* enum_decl = *node = new_ast_node(eAstNode_enum_decl, clone_source_loc(&parser->src_loc));

    if(!get_next_token(parser))
      return success = false;

    if(parser->token.kind == eToken_id)
    {
      AstNode* id = enum_decl->enum_decl.id = new_ast_node(eAstNode_id, clone_source_loc(&parser->src_loc));
      id->id.name = parser->token.lexeme;

      if(!get_next_token(parser))
        return success = false;

      if(parser->token.kind == eToken_open_brace)
      {

        if(!get_next_token(parser))
          return success = false;

        enum_decl->enum_decl.members = new_list(arena, eList_ast_node);
        AstNode* member = 0;
        do
        {
          member = 0;
          if(parser->token.kind == eToken_id)
          {
            member = new_ast_node(eAstNode_id, clone_source_loc(&parser->src_loc));
            member->id.name = parser->token.lexeme;
            append_list_elem(enum_decl->enum_decl.members, member, eList_ast_node);

            if((success = get_next_token(parser)) && parser->token.kind == eToken_comma)
            {
              success = get_next_token(parser);
            }
            else if(parser->token.kind != eToken_close_brace)
              member = 0;
          }
        }
        while(member && success);

        if(parser->token.kind == eToken_close_brace)
          success = get_next_token(parser);
        else
          success = compile_error(parser->arena, &parser->src_loc, "expected `}`, actual `%s`", get_token_printstr(parser->token));
      }
      else
        success = compile_error(parser->arena, &parser->src_loc, "expected `{`, actual `%s`", get_token_printstr(parser->token));
    }
    else
      success = compile_error(parser->arena, &parser->src_loc, "identifier expected, actual `%s`", get_token_printstr(parser->token));
  }
  return success;
}

bool parse_struct_member_list(Parser*, List*);

bool parse_union(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token.kind == eToken_union)
  {
    AstNode* union_decl = *node = new_ast_node(eAstNode_union_decl, clone_source_loc(&parser->src_loc));

    if((success = get_next_token(parser)) && parser->token.kind == eToken_id)
    {
      AstNode* id = union_decl->union_decl.id = new_ast_node(eAstNode_id, clone_source_loc(&parser->src_loc));
      id->id.name = parser->token.lexeme;
      success = get_next_token(parser);
    }

    if(success)
    {
      union_decl->union_decl.members = new_list(arena, eList_ast_node);
      success = parse_struct_member_list(parser, union_decl->union_decl.members);
    }
  }
  return success;
}

bool parse_struct(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token.kind == eToken_struct)
  {
    AstNode* struct_decl = *node = new_ast_node(eAstNode_struct_decl, clone_source_loc(&parser->src_loc));

    if((success = get_next_token(parser)) && parser->token.kind == eToken_id)
    {
      AstNode* id = struct_decl->struct_decl.id = new_ast_node(eAstNode_id, clone_source_loc(&parser->src_loc));
      id->id.name = parser->token.lexeme;
      success = get_next_token(parser);
    }

    if(success)
    {
      struct_decl->struct_decl.members = new_list(arena, eList_ast_node);
      success = parse_struct_member_list(parser, struct_decl->struct_decl.members);
    }
  }
  return success;
}

bool parse_struct_member_list(Parser* parser, List* member_list)
{
  bool success = true;

  if(parser->token.kind == eToken_open_brace)
  {
    if(!get_next_token(parser))
      return success = false;

    AstNode* member = 0;
    do
    {
      member = 0;

      AstNode* type = 0;
      if(success = parse_type(parser, &type))
      {
        if(!type)
        {
          if(parser->token.kind == eToken_union)
          {
            success = parse_union(parser, &type);
          }
          else if(parser->token.kind == eToken_struct)
          {
            success = parse_struct(parser, &type);
          }
        }

        if(success && type)
        {
          AstNode* var = member = new_ast_node(eAstNode_var, clone_source_loc(&parser->src_loc));
          var->var.type = type;

          if(parser->token.kind == eToken_id)
          {
            var->var.name = parser->token.lexeme;
            success = get_next_token(parser);
          }
          else if(type->kind == eAstNode_struct_decl || type->kind == eAstNode_union_decl)
          {
            /* anonymous struct/union */
          }
          else
            success = compile_error(parser->arena, &parser->src_loc, "identifier was expected at `%s`", get_token_printstr(parser->token));

          if(success)
          {
            append_list_elem(member_list, member, eList_ast_node);
            success = consume_semicolon(parser);
          }
        }
      }
    }
    while(member && success);
    if(!success)
      return success;

    if(parser->token.kind == eToken_close_brace)
      success = get_next_token(parser);
    else
      success = compile_error(parser->arena, &parser->src_loc, "`}` was expected at `%s`", get_token_printstr(parser->token));
  }
  else
    success = compile_error(parser->arena, &parser->src_loc, "`{` was expected at `%s`", get_token_printstr(parser->token));
  return success;
}

bool parse_asm_block(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token.kind == eToken_open_brace)
  {
    AstNode* asm_block = *node = new_ast_node(eAstNode_asm_block, clone_source_loc(&parser->src_loc));

    if(success = get_asm_text(parser))
    {
      asm_block->asm_block.asm_text = parser->token.lexeme;
      if(success = get_next_token(parser))
      {
        if(parser->token.kind == eToken_close_brace)
        {
          success = get_next_token(parser);
        }
        else
          success = compile_error(parser->arena, &parser->src_loc, "`}` was expected at `%s`", parser->get_token_printstr(token));
      }
    }
  }
  return success;
}
#endif

bool parse_module_proc(Parser* parser, char* name, eModifier modifier, AstNode* ret_type, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token->kind == eToken_open_parens)
  {
    AstNode* proc = *node = new_ast_node(parser, eAstNode_proc);
    proc->proc.ret_type = ret_type;
    proc->proc.name = name;
    proc->proc.modifier = modifier;

    if(success = get_next_token(parser->lexer))
    {
      AstNode* args = proc->proc.args = new_ast_node(parser, eAstNode_node_list);
      list_init(&args->args.node_list, parser->arena, eList_ast_node);

      if(success = parse_formal_args(parser, proc->proc.args))
      {
        if(parser->token->kind == eToken_close_parens)
        {
          success = get_next_token(parser->lexer) && parse_proc_body(parser, proc);
        }
        else
          success = compile_error(parser->arena, parser->src_loc, "`)` was expected at `%s`", get_token_printstr(parser->token));
      }
    }
  }

  return success;
}

bool parse_module_var(Parser* parser, char* name, eModifier modifier, AstNode* var_type, AstNode** node)
{
  bool success = true;

  AstNode* var = *node = new_ast_node(parser, eAstNode_var);
  var->var.type = var_type;
  var->var.name = name;
  var->var.modifier = modifier;

  return success;
}

AstNode* find_include(Parser* parser, PlatformFile* file)
{
  AstNode* include = 0;

  for(ListItem* li = parser->includes->first;
      li && !include;
      li = li->next)
  {
    include = KIND(li, eList_ast_node)->ast_node;
    if(platform_file_identity(file, include->include.file))
      break;
    include = 0;
  }

  return include;
}

void merge_module(AstNode* main_module, AstNode* merged_module)
{
  list_join(&main_module->module.nodes, &merged_module->module.nodes);
  list_join(&main_module->module.procs, &merged_module->module.procs);
  list_join(&main_module->module.vars, &merged_module->module.vars);
}

bool parse_module(Parser* parser);

bool parse_module_include(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token->kind == eToken_include)
  {
    if(success = get_next_token(parser->lexer))
    {
      AstNode* include = *node = new_ast_node(parser, eAstNode_include);
      if(parser->token->kind == eToken_str_val)
      {
        /* Make the full path to the included file, relative to the location of the current file. */
        String str = {};
        str_init(&str, parser->arena);
        str_append(&str, parser->src_loc->file_path);
        platform_path_make_dir(str.head);
        str_tidyup(&str);
        str_append(&str, parser->token->str_val);

        include->include.file_path = str_cap(&str);

        if(success = get_next_token(parser->lexer) && consume_semicolon(parser))
        {
          PlatformFile* included_file = include->include.file = platform_file_open(parser->arena, include->include.file_path);
          if(included_file)
          {
            AstNode* previous_include = find_include(parser, included_file);
            if(!previous_include)
            {
              list_append(parser->includes, include, eList_ast_node);

              char* hoc_text = platform_file_read_text(parser->arena, included_file->path);
              if(hoc_text)
              {
                Parser* included_parser = new_included_parser(parser);
                set_parser_input(included_parser, hoc_text, included_file);

                if(success = parse_module(included_parser))
                {
                  merge_module(parser->module, included_parser->module);
                }
              }
            }
            else
            {
              success = compile_error(parser->arena, include->src_loc, "file `%s` has already been included", include->include.file_path);
              if(previous_include->src_loc) // main file does not have an inclusion point
              {
                compile_error(parser->arena, previous_include->src_loc, "see the previous inclusion point");
              }
            }
          }
          else
          {
            putback_token(parser->lexer);
            success = compile_error(parser->arena, parser->src_loc, "file `%s` could not be opened", include->include.file_path);
          }
        }
        else
          success = compile_error(parser->arena, parser->src_loc, "could not read file `%s`", include->include.file_path);
      }
      else
        success = compile_error(parser->arena, parser->src_loc, "string literal was expected at `%s`", get_token_printstr(parser->token));
    }
  }

  return success;
}

bool parse_module_stmt(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken_include:
    {
      success = parse_module_include(parser, node);
    }
    break;

    default:
    {
      eModifier modifier = eModifier_None;

      if(success = parse_modifier(parser, &modifier) && parse_expr(parser, node))
      {
        if(*node)
        {
          if(parser->token->kind == eToken_id)
          {
            char* name = parser->token->lexeme;
            if(success = get_next_token(parser->lexer))
            {
              if(parser->token->kind == eToken_open_parens)
              {
                success = parse_module_proc(parser, name, modifier, *node, node);
              }
              else
              {
                success = parse_module_var(parser, name, modifier, *node, node) && consume_semicolon(parser);
              }
            }
          }
          else
            success = compile_error(parser->arena, parser->src_loc, "identifier was expected at `%s`", get_token_printstr(parser->token));
        }
        else if(modifier != eModifier_None)
          success = compile_error(parser->arena, parser->src_loc, "statement was expected at `%s`", get_token_printstr(parser->token));
      }
    }
    break;
  }
  return success;
}

bool parse_module_stmts(Parser* parser, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  AstNode* stmt = 0;

  if(success = parse_module_stmt(parser, &stmt))
  {
    if(stmt)
    {
      switch(stmt->kind)
      {
        case eAstNode_proc:
        {
          list_append(&module->module.nodes, stmt, eList_ast_node);
          list_append(&module->module.procs, stmt, eList_ast_node);
        }
        break;

        case eAstNode_var:
        {
          list_append(&module->module.nodes, stmt, eList_ast_node);
          list_append(&module->module.vars, stmt, eList_ast_node);
        }
        break;
          
        case eAstNode_include:
        case eAstNode_empty:
          break;

        default:
          assert(0);
      }

      success = parse_module_stmts(parser, module);
    }
  }
  return success;
}

bool parse_module_body(Parser* parser, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;

  if(success = parse_module_stmts(parser, module))
  {
    if(parser->token->kind == eToken_end_of_input)
    {
      //TODO: reset the parser
    }
    else
      success = compile_error(parser->arena, parser->src_loc, "`end-of-input` was expected at `%s`", get_token_printstr(parser->token));
  }

  return success;
}

bool parse_module(Parser* parser)
{
  bool success = true;

  if(success = get_next_token(parser->lexer))
  {
    AstNode* module = parser->module = new_ast_node(parser, eAstNode_module);
    module->module.file_path = parser->file->path;
    list_init(&module->module.nodes, parser->arena, eList_ast_node);
    list_init(&module->module.procs, parser->arena, eList_ast_node);
    list_init(&module->module.vars, parser->arena, eList_ast_node);

    AstNode* include = new_ast_node(parser, eAstNode_include);
    include->src_loc = 0;
    include->include.file = parser->file;
    include->include.file_path = parser->file->path;
    list_append(parser->includes, include, eList_ast_node);

    success = parse_module_body(parser, module);
  }

  return success;
}

