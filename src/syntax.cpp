char* Parser::get_operator_printstr(eOperator op)
{
  char* str = "???";
  switch(op)
  {
    case eOperator::add:
      str = "+";
    break;
    
    case eOperator::sub:
      str = "-";
    break;
    
    case eOperator::mul:
      str = "*";
    break;
    
    case eOperator::div:
      str = "/";
    break;
    
    case eOperator::mod:
      str = "mod";
    break;
    
    case eOperator::neg:
      str = "-";
    break;
    
    case eOperator::deref:
#if 0
    case eOperator::pointer:
#endif
    str = "^";
    break;
    
    case eOperator::address_of:
      str = "&";
    break;
    
    case eOperator::selector:
      str = ".";
    break;
    
    case eOperator::indirect_selector:
      str = "->";
    break;
#if 0
    case eOperator::pre_decr:
    case eOperator::post_decr:
    str = "--";
    break;
    
    case eOperator::pre_incr:
    case eOperator::post_incr:
    str = "++";
    break;
#endif
    case eOperator::eq:
      str = "==";
    break;
    
    case eOperator::not_eq_:
      str = "<>";
    break;
    
    case eOperator::less:
      str = "<";
    break;
    
    case eOperator::less_eq:
      str = "<=";
    break;
    
    case eOperator::greater:
      str = ">";
    break;
    
    case eOperator::greater_eq:
      str = ">=";
    break;
    
    case eOperator::logic_and:
      str = "and";
    break;
    
    case eOperator::logic_or:
      str = "or";
    break;
    
    case eOperator::logic_not:
      str = "not";
    break;
    
    case eOperator::bit_and:
      str = "&";
    break;
    
    case eOperator::bit_or:
      str = "|";
    break;
    
    case eOperator::bit_xor:
      str = "~";
    break;
    
    case eOperator::bit_not:
      str = "!";
    break;
  }
  return str;
}

AstNode* Parser::create_ast_node(eAstNode kind, SourceLoc* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode);
  node->src_loc = src_loc;
  node->kind = kind;
  return node;
}

SourceLoc* Parser::clone_source_loc()
{
  SourceLoc* clone = mem_push_struct(arena, SourceLoc);
  *clone = *src_loc;
  return clone;
}

Parser* Parser::create(MemoryArena* arena)
{
  Parser* parser = mem_push_struct(arena, Parser);
  parser->arena = arena;
  parser->includes = List::create(arena, eList::ast_node);

  Lexer* lexer = parser->lexer = Lexer::create(arena);
  parser->token = &lexer->token;
  parser->src_loc = &lexer->src_loc;

  return parser;
}

void Parser::set_input(char* text, HFile* file)
{
  this->file = file;
  lexer->set_input(text, file->path);
}

bool Parser::get_next_token()
{
  bool result = lexer->get_next_token();
  return result;
}

void Parser::putback_token()
{
  lexer->putback_token();
}

Parser* Parser::create_included()
{
  Parser* included_parser = create(arena);
  included_parser->includes = includes;

  return included_parser;
}

bool Parser::consume_semicolon()
{
  bool success = true;
  if(token->kind == eToken::semicolon)
  {
    success = get_next_token();
  }
  else
    success = compile_error(arena, src_loc, "`;` was expected at `%s`", token->get_printstr());

  return success;
}

bool AstNode::is_valid_expr_operand()
{
  bool valid = false;
  switch(kind)
  {
    case eAstNode::lit:
    case eAstNode::id:
    case eAstNode::bin_expr:
    case eAstNode::unr_expr:
    case eAstNode::call:
    case eAstNode::index:
    case eAstNode::cast:
      valid = true;
    break;
  }
  return valid;
}

bool Parser::parse_rest_of_actual_args(AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  bool success = true;
  if((token->kind == eToken::comma) && (success = get_next_token()))
  {
    success = parse_actual_args(args);
  }
  return success;
}

bool Parser::parse_actual_args(AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  bool success = true;

  AstNode* expr = 0;
  if(success = parse_expr(&expr))
  {
    if(expr)
    {
      AstNode* call_arg = create_ast_node(eAstNode::call_arg, clone_source_loc());
      call_arg->call_arg.expr = expr;

      args->args.node_list.append(call_arg, eList::ast_node);
      success = parse_rest_of_actual_args(args);
    }
  }
  return success;
}

bool Parser::parse_call(AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(token->kind == eToken::open_parens)
  {
    AstNode* call = *node = create_ast_node(eAstNode::call, clone_source_loc());
    call->call.expr = left_node;
    AstNode* args = call->call.args = create_ast_node(eAstNode::node_list, clone_source_loc());
    args->args.node_list.init(arena, eList::ast_node);

    if(success = get_next_token() && parse_actual_args(call->call.args))
    {
      if(token->kind == eToken::close_parens)
      {
        if(success = get_next_token())
        {
          success = parse_rest_of_selector(*node, node);
        }
      }
      else
        success = compile_error(arena, src_loc, "`)` was expected at `%s`", token->get_printstr());
    }
  }
  return success;
}

bool Parser::parse_index_recursive(AstNode* left_node, AstNode** node, int* ndim)
{
  *node = left_node;
  bool success = true;

  if(token->kind == eToken::open_bracket)
  {
    AstNode* index = *node = create_ast_node(eAstNode::index, clone_source_loc());
    index->index.array_expr = left_node;
    index->index.ndim = *ndim;

    if(success = get_next_token() && parse_expr(&index->index.i_expr))
    {
      if(token->kind == eToken::close_bracket)
      {
        if(index->index.i_expr)
        {
          *ndim = *ndim + 1;
          if(success = get_next_token() && parse_index_recursive(*node, node, ndim))
          {
            index->index.ndim = *ndim - index->index.ndim;
          }
        }
        else
          success = compile_error(arena, src_loc, "expression was expected at %s", token->get_printstr());
      }
      else
      {
        success = compile_error(arena, src_loc, "`]` was expected at `%s`", token->get_printstr());
      }
    }
  }
  return success;
}

bool Parser::parse_index(AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  int ndim = 1;
  success = parse_index_recursive(left_node, node, &ndim);
  return success;
}

bool Parser::parse_rest_of_unr_expr(AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(token->kind)
  {
    case eToken::dot:
    case eToken::arrow_right:
      {
        AstNode* bin_expr = *node = create_ast_node(eAstNode::bin_expr, clone_source_loc());
        bin_expr->bin_expr.left_operand = left_node;

        switch(token->kind)
        {
          case eToken::dot:
            bin_expr->bin_expr.op = eOperator::selector;
          break;

          case eToken::arrow_right:
            bin_expr->bin_expr.op = eOperator::indirect_selector;
          break;

          default:
            assert(0);
        }

        AstNode* right_operand = 0;
        if(success = get_next_token() && parse_selector(&right_operand))
        {
          if(right_operand)
          {
            switch(right_operand->kind)
            {
              case eAstNode::id:
                bin_expr->bin_expr.right_operand = right_operand;
                success = parse_rest_of_unr_expr(*node, node);
              break;

              default:
                success = compile_error(arena, right_operand->src_loc, "invalid operand");
                break;
            }
          }
          else
            success = compile_error(arena, src_loc, "operand was expected at `%s`", token->get_printstr());
        }
      }
      break;
  }
  return success;
}

bool Parser::parse_factor(AstNode** node)
{
  bool success = true;
  if((success = parse_unr_expr(node)) && *node)
  {
    success = parse_rest_of_unr_expr(*node, node);
  }
  return success;
}

bool Parser::parse_rest_of_factor(AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(token->kind)
  {
    case eToken::star:
    case eToken::fwd_slash:
    case eToken::mod:
    case eToken::and_:
    case eToken::ampersand:
    {
      AstNode* bin_expr = *node = create_ast_node(eAstNode::bin_expr, clone_source_loc());
      bin_expr->bin_expr.left_operand = left_node;

      switch(token->kind)
      {
        case eToken::star:
          bin_expr->bin_expr.op = eOperator::mul;
        break;

        case eToken::fwd_slash:
          bin_expr->bin_expr.op = eOperator::div;
        break;

        case eToken::mod:
          bin_expr->bin_expr.op = eOperator::mod;
        break;

        case eToken::and_:
          bin_expr->bin_expr.op = eOperator::logic_and;
        break;

        case eToken::ampersand:
          bin_expr->bin_expr.op = eOperator::bit_and;
        break;

        default:
          assert(0);
      }

      AstNode* right_operand = 0;
      if(success = get_next_token() && parse_factor(&right_operand))
      {
        if(right_operand)
        {
          if(right_operand->is_valid_expr_operand())
          {
            bin_expr->bin_expr.right_operand = right_operand;
            success = parse_rest_of_factor(*node, node); // left-associativity
          }
          else
            success = compile_error(arena, right_operand->src_loc, "invalid operand");
        }
        else
          success = compile_error(arena, src_loc, "operand was expected at `%s`", token->get_printstr());
      }
    }
      break;
  }
  return success;
}

bool Parser::parse_term(AstNode** node)
{
  bool success = true;
  if((success = parse_factor(node)) && *node)
  {
    success = parse_rest_of_factor(*node, node);
  }
  return success;
}

bool Parser::parse_rest_of_term(AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(token->kind)
  {
    case eToken::plus:
    case eToken::minus:
    case eToken::pipe:
    case eToken::angle_right_right:
    case eToken::angle_left_left:
    case eToken::tilde:
    case eToken::eq_eq:
    case eToken::angle_left:
    case eToken::angle_left_eq:
    case eToken::angle_right:
    case eToken::angle_right_eq:
    case eToken::angle_left_right:
    case eToken::or_:
      {
        AstNode* bin_expr = *node = create_ast_node(eAstNode::bin_expr, clone_source_loc());
        bin_expr->bin_expr.left_operand = left_node;

        switch(token->kind)
        {
          case eToken::plus:
            bin_expr->bin_expr.op = eOperator::add;
          break;

          case eToken::minus:
            bin_expr->bin_expr.op = eOperator::sub;
          break;

          case eToken::pipe:
            bin_expr->bin_expr.op = eOperator::bit_or;
          break;

          case eToken::angle_left_left:
            bin_expr->bin_expr.op = eOperator::bit_shift_left;
          break;

          case eToken::angle_right_right:
            bin_expr->bin_expr.op = eOperator::bit_shift_right;
          break;

          case eToken::tilde:
            bin_expr->bin_expr.op = eOperator::bit_xor;
          break;

          case eToken::eq_eq:
            bin_expr->bin_expr.op = eOperator::eq;
          break;

          case eToken::angle_left_right:
            bin_expr->bin_expr.op = eOperator::not_eq_;
          break;

          case eToken::angle_left:
            bin_expr->bin_expr.op = eOperator::less;
          break;

          case eToken::angle_left_eq:
            bin_expr->bin_expr.op = eOperator::less_eq;
          break;

          case eToken::angle_right:
            bin_expr->bin_expr.op = eOperator::greater;
          break;

          case eToken::angle_right_eq:
            bin_expr->bin_expr.op = eOperator::greater_eq;
          break;

          case eToken::or_:
            bin_expr->bin_expr.op = eOperator::logic_or;
          break;

          default:
            assert(0);
        }

        AstNode* right_operand = 0;
        if(success && (success = get_next_token() && parse_term(&right_operand)))
        {
          if(right_operand)
          {
            if(right_operand->is_valid_expr_operand())
            {
              bin_expr->bin_expr.right_operand = right_operand;
              success = parse_rest_of_term(*node, node); // left-associativity
            }
            else
              success = compile_error(arena, right_operand->src_loc, "invalid operand");
          }
          else
            success = compile_error(arena, src_loc, "operand was expected at `%s`", token->get_printstr());
        }
      }
      break;
  }
  return success;
}

bool Parser::parse_assignment(AstNode** node)
{
  bool success = true;
  if((success = parse_term(node)) && *node)
  {
    success = parse_rest_of_term(*node, node);
  }
  return success;
}

bool Parser::parse_rest_of_assignment(AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(token->kind)
  {
    case eToken::eq:
      {
        AstNode* assign = *node = create_ast_node(eAstNode::assign, clone_source_loc());
        assign->assign.dest_expr = left_node;

        AstNode* source_expr = 0;
        if(success = get_next_token() && parse_expr(&source_expr))
        {
          if(source_expr)
          {
            if(source_expr->is_valid_expr_operand())
            {
              assign->assign.source_expr = source_expr;
            }
            else
              success = compile_error(arena, source_expr->src_loc, "invalid operand in assignment expression");
          }
          else
            success = compile_error(arena, src_loc, "operand was expected at `%s`", token->get_printstr());
        }
      }
      break;
  }
  return success;
}

bool Parser::parse_id(AstNode** node)
{
  *node = 0;
  bool success = true;

  if(token->kind == eToken::id)
  {
    AstNode* id = *node = create_ast_node(eAstNode::id, clone_source_loc());
    id->id.name = token->lexeme;
    success = get_next_token();
  }
  return success;
}

bool Parser::parse_basic_type(AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(token->kind)
  {
    case eToken::int_:
    case eToken::float_:
    case eToken::bool_:
    case eToken::char_:
    case eToken::void_:
    case eToken::auto_:
    {
      AstNode* basic_type = *node = create_ast_node(eAstNode::basic_type, clone_source_loc());
      switch(token->kind)
      {
        case eToken::int_:
          basic_type->basic_type.kind = eBasicType::int_;
          break;
        case eToken::float_:
          basic_type->basic_type.kind = eBasicType::float_;
          break;
        case eToken::bool_:
          basic_type->basic_type.kind = eBasicType::bool_;
          break;
        case eToken::char_:
          basic_type->basic_type.kind = eBasicType::char_;
          break;
        case eToken::void_:
          basic_type->basic_type.kind = eBasicType::void_;
          break;
        case eToken::auto_:
          basic_type->basic_type.kind = eBasicType::auto_;
          break;
        default:
          assert(0);
      }
      success = get_next_token();
    }
    break;
  }
  return success;
}

bool Parser::parse_rest_of_cast(AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

#if 0
  AstNode* right_node = 0;
  if((success = parse_unr_expr(parser, &right_node)) && right_node)
  {
    AstNode* cast = *node = new_ast_node(eAstNode::bin_expr, clone_source_loc());
    cast->bin_expr.op = eOperator::cast;
    cast->bin_expr.left_operand = left_node;
    cast->bin_expr.right_operand = right_node;
  }
#else
  switch(token->kind)
  {
    case eToken::colon:
    {
      AstNode* cast = *node = create_ast_node(eAstNode::cast, clone_source_loc());
      cast->cast.to_type = left_node;

      if(success = get_next_token() && parse_unr_expr(&cast->cast.from_expr))
      {
        if(!cast->cast.from_expr)
        {
          success = compile_error(arena, src_loc, "expression was expected at `%s`", token->get_printstr());
        }
      }
    }
    break;
  }
#endif
  return success;
}

bool Parser::parse_rest_of_deref(AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(token->kind)
  {
    case eToken::circumflex:
      success = parse_deref(node);
    break;

    case eToken::id:
      success = parse_id(node);
    break;

    case eToken::void_:
    case eToken::int_:
    case eToken::float_:
    case eToken::bool_:
    case eToken::char_:
    case eToken::auto_:
      success = parse_basic_type(node);
    break;

    case eToken::open_parens:
    {
      if(success = get_next_token() && parse_expr(node))
      {
        if(token->kind == eToken::close_parens)
        {
          success = get_next_token() && parse_rest_of_cast(*node, node);
        }
        else
          success = compile_error(arena, src_loc, "`)` was expected at `%s`", token->get_printstr());
      }
    }
    break;
  }
  return success;
}

bool Parser::parse_deref(AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* deref = *node = create_ast_node(eAstNode::unr_expr, clone_source_loc());
  deref->unr_expr.op = eOperator::deref;
  if(success = get_next_token() && parse_rest_of_deref(&deref->unr_expr.operand))
  {
    if(!deref->unr_expr.operand)
    {
      success = compile_error(arena, src_loc, "expression was expected at `%s`", token->get_printstr());
    }
  }
  return success;
}

bool Parser::parse_rest_of_array(AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(token->kind)
  {
    case eToken::open_bracket:
      success = parse_array(node);
    break;

    case eToken::id:
      success = compile_error(arena, src_loc, "unknown type id `%s`", token->get_printstr());
    break;

    case eToken::void_:
    case eToken::int_:
    case eToken::float_:
    case eToken::bool_:
    case eToken::char_:
    case eToken::auto_:
      success = parse_basic_type(node);
    break;

    case eToken::open_parens:
    {
      if(success = get_next_token() && parse_array(node) && parse_pointer(*node, node))
      {
        if(token->kind == eToken::close_parens)
        {
          success = get_next_token();
        }
        else
          success = compile_error(arena, src_loc, "`)` was expected at `%s`", token->get_printstr());
      }
    }
    break;
  }
  return success;
}

bool Parser::parse_array(AstNode** node)
{
  *node = 0;
  bool success = true;

  if(token->kind == eToken::open_bracket)
  {
    AstNode* array = *node = create_ast_node(eAstNode::array, clone_source_loc());
    if(success = get_next_token() && parse_expr(&array->array.size_expr))
    {
      if(token->kind == eToken::close_bracket)
      {
        if(array->array.size_expr)
        {
          if(success = get_next_token() && parse_rest_of_array(&array->array.elem_expr))
          {
            if(!array->array.elem_expr)
            {
              success = compile_error(arena, src_loc,  "expression was expected at `%s`", token->get_printstr());
            }
          }
        }
        else
          success = compile_error(arena, src_loc, "expression was expected at `%s`", token->get_printstr());
      }
      else
        success = compile_error(arena, src_loc,  "`]` was expected at `%s`", token->get_printstr());
    }
  }
  return success;
}

bool Parser::parse_cast(AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(token->kind)
  {
    case eToken::open_parens:
    {
      if(success = get_next_token() && parse_expr(node))
      {
        if(token->kind == eToken::close_parens)
        {
          if(*node)
          {
            success = get_next_token() && parse_rest_of_cast(*node, node);
          }
          else
            success = compile_error(arena, src_loc, "expression was expected at `%s`", token->get_printstr());
        }
        else
          success = compile_error(arena, src_loc, "`)` was expected at `%s`", token->get_printstr());
      }
    }
    break;

    case eToken::circumflex:
      success = parse_deref(node) && parse_index(*node, node);
    break;

    case eToken::open_bracket:
      success = parse_array(node) && parse_pointer(*node, node);
    break;

    case eToken::id:
      success = parse_id(node) && parse_pointer(*node, node) && parse_index(*node, node);
    break;

    case eToken::void_:
    case eToken::int_:
    case eToken::float_:
    case eToken::bool_:
    case eToken::char_:
    case eToken::auto_:
      success = parse_basic_type(node) && parse_pointer(*node, node) && parse_index(*node, node);
    break;
  }
  return success;
}

bool Parser::parse_pointer(AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(token->kind == eToken::circumflex)
  {
    AstNode* pointer = *node = create_ast_node(eAstNode::pointer, clone_source_loc());
    pointer->pointer.pointee = left_node;

    if((success = get_next_token()) && token->kind == eToken::circumflex)
    {
      success = parse_pointer(*node, node);
    }
  }
  return success;
}

bool Parser::parse_rest_of_selector(AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(token->kind)
  {
    case eToken::open_parens:
      success = parse_call(left_node, node);
    break;

    case eToken::open_bracket:
      success = parse_index(left_node, node);
    break;
  }
  return success;
}

bool Parser::parse_lit(AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* lit = *node = create_ast_node(eAstNode::lit, clone_source_loc());

  switch(token->kind)
  {
    case eToken::int_val:
    {
      lit->lit.kind = eLiteral::int_;
      lit->lit.int_val = *token->int_val;
    }
    break;

    case eToken::float_val:
    {
      lit->lit.kind = eLiteral::float_;
      lit->lit.float_val = *token->float_val;
    }
    break;

    case eToken::true_:
    case eToken::false_:
    {
      lit->lit.kind = eLiteral::bool_;
      lit->lit.bool_val = (token->kind == eToken::true_ ? 1 : 0);
    }
    break;

    case eToken::char_val:
    {
      lit->lit.kind = eLiteral::char_;
      lit->lit.char_val = token->char_val;
    }
    break;

    case eToken::str_val:
    {
      lit->lit.kind = eLiteral::str;
      lit->lit.str_val = token->str_val;
    }
    break;

    default: assert(0);
  }

  success = get_next_token();

  return success;
}

bool Parser::parse_selector(AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(token->kind)
  {
    case eToken::true_:
    case eToken::false_:
    case eToken::int_val:
    case eToken::float_val:
    case eToken::char_val:
    case eToken::str_val:
      success = parse_lit(node);
    break;

    case eToken::open_parens:
    case eToken::open_bracket:
    case eToken::circumflex:
    case eToken::id:
    case eToken::int_:
    case eToken::float_:
    case eToken::bool_:
    case eToken::char_:
    case eToken::void_:
    case eToken::auto_:
      success = parse_cast(node) && parse_rest_of_selector(*node, node);
    break;
  }
  return success;
}

bool Parser::parse_formal_arg(AstNode** node)
{
  *node = 0;
  bool success = true;

  if((success = parse_expr(node)) && *node)
  {
    if(token->kind == eToken::id)
    {
      AstNode* type = *node;
      AstNode* var = *node = create_ast_node(eAstNode::var, clone_source_loc());
      var->var.type = type;
      var->var.name = token->lexeme;

      success = get_next_token();
    }
    else
      success = compile_error(arena, src_loc, "identifier was expected at `%s`", token->get_printstr());
  }
  return success;
}

bool Parser::parse_unr_expr(AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(token->kind)
  {
    case eToken::exclam:
    case eToken::not_:
    case eToken::ampersand:
    case eToken::minus:
      {
        AstNode* unr_expr = *node = create_ast_node(eAstNode::unr_expr, clone_source_loc());

        switch(token->kind)
        {
          case eToken::exclam:
            unr_expr->unr_expr.op = eOperator::bit_not;
            break;
          case eToken::not_:
            unr_expr->unr_expr.op = eOperator::logic_not;
            break;
          case eToken::ampersand:
            unr_expr->unr_expr.op = eOperator::address_of;
            break;
          case eToken::minus:
            unr_expr->unr_expr.op = eOperator::neg;
            break;
          default:
            assert(0);
        }

        if(success && (success = get_next_token() && parse_factor(&unr_expr->unr_expr.operand)))
        {
          if(!unr_expr->unr_expr.operand)
          {
            success = compile_error(arena, src_loc, "operand was expected at `%s`", token->get_printstr());
          }
        }
      }
      break;

    default:
      success = parse_selector(node);
      break;
  }
  return success;
}

bool Parser::parse_expr(AstNode** node)
{
  *node = 0;
  bool success = true;
  if((success = parse_assignment(node)) && *node)
  {
    success = parse_rest_of_assignment(*node, node);
  }
  return success;
}

bool Parser::parse_modifier(eModifier* modifier)
{
  bool success = true;
  *modifier = eModifier::None;

  switch(token->kind)
  {
    case eToken::extern_:
    {
      *modifier = eModifier::extern_;
      success = get_next_token();
    }
    break;

    case eToken::const_:
    {
      *modifier = eModifier::const_;
      success = get_next_token();
    }
    break;
  }

  return success;
}

bool Parser::parse_rest_of_formal_args(AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  bool success = true;
  if((token->kind == eToken::comma) && (success = get_next_token()))
  {
    success = parse_formal_args(args);
  }
  return success;
}

bool Parser::parse_formal_args(AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  bool success = true;

  AstNode* arg = 0;
  if(success = parse_formal_arg(&arg))
  {
    if(arg)
    {
      args->args.node_list.append(arg, eList::ast_node);
      success = parse_rest_of_formal_args(args);
    }
  }
  return success;
}

bool Parser::parse_empty(AstNode** node)
{
  *node = 0;
  bool success = true;
  if(token->kind == eToken::semicolon)
  {
    *node = create_ast_node(eAstNode::empty, clone_source_loc());
    success = get_next_token();
  }
  return success;
}

bool Parser::parse_block(AstNode** node)
{
  *node = 0;
  bool success = true;
  if(token->kind == eToken::open_brace)
  {
    AstNode* block = *node = create_ast_node(eAstNode::block, clone_source_loc());
    block->block.nodes.init(arena, eList::ast_node);
    block->block.vars.init(arena, eList::ast_node);
    block->block.stmts.init(arena, eList::ast_node);

    if(success = (get_next_token() && parse_block_stmts(block)))
    {
      if(token->kind == eToken::close_brace)
      {
        success = get_next_token();
      }
      else
        success = compile_error(arena, src_loc, "`}` was expected at `%s`", token->get_printstr());
    }
  }
  return success;
}

bool Parser::parse_else(AstNode** node)
{
  *node = 0;
  bool success = true;
  if(token->kind == eToken::else_)
  {
    success = get_next_token() && parse_block_stmt(node);
  }
  return success;
}

bool Parser::parse_if(AstNode** node)
{
  *node = 0;
  bool success = true;
  if(token->kind == eToken::if_)
  {
    AstNode* if_ = *node = create_ast_node(eAstNode::if_, clone_source_loc());
    if(success = get_next_token())
    {
      if(token->kind == eToken::open_parens)
      {
        if(success = (get_next_token() && parse_expr(&if_->if_.cond_expr)))
        {
          if(if_->if_.cond_expr)
          {
            if(token->kind == eToken::close_parens)
            {
              success = get_next_token() && parse_block_stmt(&if_->if_.body) && parse_else(&if_->if_.else_body);
            }
            else
              success = compile_error(arena, src_loc, "`)` was expected at `%s`", token->get_printstr());
          }
          else
            success = compile_error(arena, src_loc, "expression was expected at `%s`", token->get_printstr());
        }
      }
      else
        success = compile_error(arena, src_loc, "`(` was expected at `%s`", token->get_printstr());
    }
  }
  return success;
}

bool Parser::parse_do_while(AstNode** node)
{
  *node = 0;
  bool success = true;
  if(token->kind == eToken::do_)
  {
    AstNode* do_while = *node = create_ast_node(eAstNode::do_while, clone_source_loc());
    if(success = get_next_token() && parse_block_stmt(&do_while->do_while.body))
    {
      if(token->kind == eToken::while_)
      {
        if(success = get_next_token())
        {
          if(token->kind == eToken::open_parens)
          {
            if(success = get_next_token() && parse_expr(&do_while->do_while.cond_expr))
            {
              if(do_while->do_while.cond_expr)
              {
                if(token->kind == eToken::close_parens)
                  success = get_next_token();
                else
                  success = compile_error(arena, src_loc, "`)` was expected at `%s`", token->get_printstr());
              }
              else
                success = compile_error(arena, src_loc, "expression was expected at `%s`", token->get_printstr());
            }
          }
          else
            success = compile_error(arena, src_loc, "`(` was expected at `%s`", token->get_printstr());
        }
      }
      else
        success = compile_error(arena, src_loc, "`while` was expected at `%s`", token->get_printstr());
    }
  }
  return success;
}

bool Parser::parse_while(AstNode** node)
{
  *node = 0;
  bool success = true;
  if(token->kind == eToken::while_)
  {
    AstNode* while_ = *node = create_ast_node(eAstNode::while_, clone_source_loc());
    if(success = get_next_token())
    {
      if(token->kind == eToken::open_parens)
      {
        if(success = get_next_token() && parse_expr(&while_->while_.cond_expr))
        {
          if(while_->while_.cond_expr)
          {
            if(token->kind == eToken::close_parens)
            {
              success = get_next_token() && parse_block_stmt(&while_->while_.body);
            }
            else
              success = compile_error(arena, src_loc, "`)` was expected at `%s`", token->get_printstr());
          }
          else
            success = compile_error(arena, src_loc, "expression was expected at `%s`", token->get_printstr());
        }
      }
      else
        success = compile_error(arena, src_loc, "`(` was expected at `%s`", token->get_printstr());
    }
  }
  return success;
}

bool Parser::parse_return(AstNode** node)
{
  *node = 0;
  bool success = true;

  if(token->kind == eToken::return_)
  {
    AstNode* ret = *node = create_ast_node(eAstNode::return_, clone_source_loc());
    AstNode* ret_expr = 0;
    if(success = get_next_token() && parse_expr(&ret_expr))
    {
      if(ret_expr)
      {
        if(ret_expr->is_valid_expr_operand())
          ret->ret.expr = ret_expr;
        else
          success = compile_error(arena, ret_expr->src_loc, "invalid return expression");
      }
    }
  }
  return success;
}

bool Parser::parse_continue(AstNode** node)
{
  *node = 0;
  bool success = true;

  if(token->kind == eToken::continue_)
  {
    AstNode* loop_ctrl = *node = create_ast_node(eAstNode::loop_ctrl, clone_source_loc());
    loop_ctrl->loop_ctrl.kind = eLoopCtrl::continue_;
    success = get_next_token();
  }
  return success;
}

bool Parser::parse_break(AstNode** node)
{
  *node = 0;
  bool success = true;

  if(token->kind == eToken::break_)
  {
    AstNode* loop_ctrl = *node = create_ast_node(eAstNode::loop_ctrl, clone_source_loc());
    loop_ctrl->loop_ctrl.kind = eLoopCtrl::break_;
    success = get_next_token();
  }
  return success;
}

bool Parser::parse_block_var(char* name, eModifier modifier, AstNode* var_type, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(token->kind == eToken::id)
  {
    AstNode* var = *node = create_ast_node(eAstNode::var, clone_source_loc());
    var->var.type = var_type;
    var->var.name = token->lexeme;

    if(success = get_next_token())
    {
      if(token->kind == eToken::eq)
      {
        success = get_next_token() && parse_expr(&var->var.init_expr);
      }
    }
  }

  return success;
}

bool Parser::parse_block_stmt(AstNode** node)
{
  *node = 0;
  bool success = true;
  switch(token->kind)
  {
    case eToken::open_brace:
      success = parse_block(node);
    break;

    case eToken::if_:
      success = parse_if(node);
    break;

    case eToken::do_:
      success = parse_do_while(node);
    break;

    case eToken::while_:
      success = parse_while(node);
    break;

    case eToken::semicolon:
      success = parse_empty(node);
    break;

    case eToken::return_:
      success = parse_return(node) && consume_semicolon();
    break;

    case eToken::break_:
      success = parse_break(node) && consume_semicolon();
    break;

    case eToken::continue_:
      success = parse_continue(node) && consume_semicolon();
    break;

    default:
    {
      eModifier modifier = eModifier::None;

      if((success = success = parse_modifier(&modifier) && parse_expr(node)) && *node)
      {
        if(token->kind == eToken::id)
        {
          char* var_name = token->lexeme;
          success = parse_block_var(var_name, modifier, *node, node) && consume_semicolon();
        }
        else
        {
          success = consume_semicolon();
        }
      }
    }
    break;
  }
  return success;
}

bool Parser::parse_block_stmts(AstNode* block)
{
  assert(KIND(block, eAstNode::block));

  bool success = true;
  AstNode* stmt = 0;
  if(success = parse_block_stmt(&stmt))
  {
    if(stmt)
    {
      block->block.nodes.append(stmt, eList::ast_node);
      switch(stmt->kind)
      {
        case eAstNode::var:
          block->block.vars.append(stmt, eList::ast_node);
        break;

        case eAstNode::empty:
        break;

        default:
          block->block.stmts.append(stmt, eList::ast_node);
        break;
      }
      success = parse_block_stmts(block);
    }
  }
  return success;
}

bool Parser::parse_proc_body(AstNode* proc)
{
  bool success = true;
  switch(token->kind)
  {
    case eToken::open_brace:
      success = parse_block(&proc->proc.body);
    break;

    case eToken::semicolon:
      success = parse_empty(&proc->proc.body);
    break;

    default:
      success = compile_error(arena, src_loc, "unexpected `%s`", token->get_printstr());
      break;
  }
  return success;
}

#if 0
bool parse_enum(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(token.kind == eToken::enum)
  {
    AstNode* enum_decl = *node = new_ast_node(eAstNode::enum_decl, clone_source_loc(&parser->src_loc));

    if(!get_next_token(parser))
      return success = false;

    if(parser->token.kind == eToken::id)
    {
      AstNode* id = enum_decl->enum_decl.id = new_ast_node(eAstNode::id, clone_source_loc(&parser->src_loc));
      id->id.name = parser->token.lexeme;

      if(!get_next_token(parser))
        return success = false;

      if(parser->token.kind == eToken::open_brace)
      {

        if(!get_next_token(parser))
          return success = false;

        enum_decl->enum_decl.members = new_list(arena, eList::ast_node);
        AstNode* member = 0;
        do
        {
          member = 0;
          if(parser->token.kind == eToken::id)
          {
            member = new_ast_node(eAstNode::id, clone_source_loc(&parser->src_loc));
            member->id.name = parser->token.lexeme;
            append_list_elem(enum_decl->enum_decl.members, member, eList::ast_node);

            if((success = get_next_token(parser)) && parser->token.kind == eToken::comma)
            {
              success = get_next_token(parser);
            }
            else if(parser->token.kind != eToken::close_brace)
              member = 0;
          }
        }
        while(member && success);

        if(parser->token.kind == eToken::close_brace)
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

  if(parser->token.kind == eToken::union)
  {
    AstNode* union_decl = *node = new_ast_node(eAstNode::union_decl, clone_source_loc(&parser->src_loc));

    if((success = get_next_token(parser)) && parser->token.kind == eToken::id)
    {
      AstNode* id = union_decl->union_decl.id = new_ast_node(eAstNode::id, clone_source_loc(&parser->src_loc));
      id->id.name = parser->token.lexeme;
      success = get_next_token(parser);
    }

    if(success)
    {
      union_decl->union_decl.members = new_list(arena, eList::ast_node);
      success = parse_struct_member_list(parser, union_decl->union_decl.members);
    }
  }
  return success;
}

bool parse_struct(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token.kind == eToken::struct)
  {
    AstNode* struct_decl = *node = new_ast_node(eAstNode::struct_decl, clone_source_loc(&parser->src_loc));

    if((success = get_next_token(parser)) && parser->token.kind == eToken::id)
    {
      AstNode* id = struct_decl->struct_decl.id = new_ast_node(eAstNode::id, clone_source_loc(&parser->src_loc));
      id->id.name = parser->token.lexeme;
      success = get_next_token(parser);
    }

    if(success)
    {
      struct_decl->struct_decl.members = new_list(arena, eList::ast_node);
      success = parse_struct_member_list(parser, struct_decl->struct_decl.members);
    }
  }
  return success;
}

bool parse_struct_member_list(Parser* parser, List* member_list)
{
  bool success = true;

  if(parser->token.kind == eToken::open_brace)
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
          if(parser->token.kind == eToken::union)
          {
            success = parse_union(parser, &type);
          }
          else if(parser->token.kind == eToken::struct)
          {
            success = parse_struct(parser, &type);
          }
        }

        if(success && type)
        {
          AstNode* var = member = new_ast_node(eAstNode::var, clone_source_loc(&parser->src_loc));
          var->var.type = type;

          if(parser->token.kind == eToken::id)
          {
            var->var.name = parser->token.lexeme;
            success = get_next_token(parser);
          }
          else if(type->kind == eAstNode::struct_decl || type->kind == eAstNode::union_decl)
          {
            /* anonymous struct/union */
          }
          else
            success = compile_error(parser->arena, &parser->src_loc, "identifier was expected at `%s`", get_token_printstr(parser->token));

          if(success)
          {
            append_list_elem(member_list, member, eList::ast_node);
            success = consume_semicolon(parser);
          }
        }
      }
    }
    while(member && success);
    if(!success)
      return success;

    if(parser->token.kind == eToken::close_brace)
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

  if(parser->token.kind == eToken::open_brace)
  {
    AstNode* asm_block = *node = new_ast_node(eAstNode::asm_block, clone_source_loc(&parser->src_loc));

    if(success = get_asm_text(parser))
    {
      asm_block->asm_block.asm_text = parser->token.lexeme;
      if(success = get_next_token(parser))
      {
        if(parser->token.kind == eToken::close_brace)
        {
          success = get_next_token(parser);
        }
        else
          success = compile_error(parser->arena, &parser->src_loc, "`}` was expected at `%s`", parser->token->get_printstr());
      }
    }
  }
  return success;
}
#endif

bool Parser::parse_module_proc(char* name, eModifier modifier, AstNode* ret_type, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(token->kind == eToken::open_parens)
  {
    AstNode* proc = *node = create_ast_node(eAstNode::proc, clone_source_loc());
    proc->proc.ret_type = ret_type;
    proc->proc.name = name;
    proc->proc.modifier = modifier;

    if(success = get_next_token())
    {
      AstNode* args = proc->proc.args = create_ast_node(eAstNode::node_list, clone_source_loc());
      args->args.node_list.init(arena, eList::ast_node);

      if(success = parse_formal_args(proc->proc.args))
      {
        if(token->kind == eToken::close_parens)
        {
          success = get_next_token() && parse_proc_body(proc);
        }
        else
          success = compile_error(arena, src_loc, "`)` was expected at `%s`", token->get_printstr());
      }
    }
  }

  return success;
}

bool Parser::parse_module_var(char* name, eModifier modifier, AstNode* var_type, AstNode** node)
{
  bool success = true;

  AstNode* var = *node = create_ast_node(eAstNode::var, clone_source_loc());
  var->var.type = var_type;
  var->var.name = name;
  var->var.modifier = modifier;

  return success;
}

AstNode* Parser::find_include(HFile* file)
{
  AstNode* include = 0;

  for(ListItem* li = includes->first;
      li && !include;
      li = li->next)
  {
    include = KIND(li, eList::ast_node)->ast_node;
    if(Platform::file_identity(file, include->include.file))
      break;
    include = 0;
  }

  return include;
}

void AstNode_Module::merge(AstNode_Module* merged_module)
{
  nodes.join(&merged_module->nodes);
  procs.join(&merged_module->procs);
  vars.join(&merged_module->vars);
}

bool Parser::parse_module_include(AstNode** node)
{
  *node = 0;
  bool success = true;

  if(token->kind == eToken::include)
  {
    if(success = get_next_token())
    {
      AstNode* include = *node = create_ast_node(eAstNode::include, clone_source_loc());
      if(token->kind == eToken::str_val)
      {
        /* Make the full path to the included file, relative to the location of the current file. */
        String str = {};
        str.init(arena);
        str.append(src_loc->file_path);
        Platform::path_make_dir(str.head);
        str.tidyup();
        str.append(token->str_val);

        include->include.file_path = str.cap();

        if(success = get_next_token() && consume_semicolon())
        {
          HFile* included_file = include->include.file = Platform::file_open(arena, include->include.file_path);
          if(included_file)
          {
            AstNode* previous_include = find_include(included_file);
            if(!previous_include)
            {
              includes->append(include, eList::ast_node);

              char* hoc_text = Platform::file_read_text(arena, included_file->path);
              if(hoc_text)
              {
                Parser* included_parser = create_included();
                included_parser->set_input(hoc_text, included_file);

                if(success = included_parser->parse_module())
                {
                  module->module.merge(&included_parser->module->module);
                }
              }
            }
            else
            {
              success = compile_error(arena, include->src_loc, "file `%s` has already been included", include->include.file_path);
              if(previous_include->src_loc) // main file does not have an inclusion point
              {
                compile_error(arena, previous_include->src_loc, "see the previous inclusion point");
              }
            }
          }
          else
          {
            putback_token();
            success = compile_error(arena, src_loc, "file `%s` could not be opened", include->include.file_path);
          }
        }
        else
          success = compile_error(arena, src_loc, "could not read file `%s`", include->include.file_path);
      }
      else
        success = compile_error(arena, src_loc, "string literal was expected at `%s`", token->get_printstr());
    }
  }

  return success;
}

bool Parser::parse_module_stmt(AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(token->kind)
  {
    case eToken::include:
    {
      success = parse_module_include(node);
    }
    break;

    default:
    {
      eModifier modifier = eModifier::None;

      if((success = parse_modifier(&modifier) && parse_expr(node)) && *node)
      {
        if(token->kind == eToken::id)
        {
          char* name = token->lexeme;
          if(success = get_next_token())
          {
            if(token->kind == eToken::open_parens)
            {
              success = parse_module_proc(name, modifier, *node, node);
            }
            else
            {
              success = parse_module_var(name, modifier, *node, node) && consume_semicolon();
            }
          }
        }
        else
          success = compile_error(arena, src_loc, "identifier was expected at `%s`", token->get_printstr());
      }
    }
    break;
  }
  return success;
}

bool Parser::parse_module_stmts(AstNode* module)
{
  assert(KIND(module, eAstNode::module));
  bool success = true;
  AstNode* stmt = 0;

  if(success = parse_module_stmt(&stmt))
  {
    if(stmt)
    {
      switch(stmt->kind)
      {
        case eAstNode::proc:
        {
          module->module.nodes.append(stmt, eList::ast_node);
          module->module.procs.append(stmt, eList::ast_node);
        }
        break;

        case eAstNode::var:
        {
          module->module.nodes.append(stmt, eList::ast_node);
          module->module.vars.append(stmt, eList::ast_node);
        }
        break;
          
        case eAstNode::include:
        case eAstNode::empty:
          break;

        default:
          assert(0);
      }

      success = parse_module_stmts(module);
    }
  }
  return success;
}

bool Parser::parse_module_body(AstNode* module)
{
  assert(KIND(module, eAstNode::module));
  bool success = true;

  if(success = parse_module_stmts(module))
  {
    if(token->kind == eToken::end_of_input)
    {
      //TODO: reset the parser
    }
    else
      success = compile_error(arena, src_loc, "`end-of-input` was expected at `%s`", token->get_printstr());
  }

  return success;
}

bool Parser::parse_module()
{
  bool success = true;

  if(success = get_next_token())
  {
    module = create_ast_node(eAstNode::module, clone_source_loc());
    module->module.file_path = file->path;
    module->module.nodes.init(arena, eList::ast_node);
    module->module.procs.init(arena, eList::ast_node);
    module->module.vars.init(arena, eList::ast_node);

    AstNode* include = create_ast_node(eAstNode::include, 0);
    include->include.file = file;
    include->include.file_path = file->path;
    includes->append(include, eList::ast_node);

    success = parse_module_body(module);;
  }

  return success;
}

