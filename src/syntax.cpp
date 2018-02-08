char* get_operator_printstr(eOperator op)
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
    
    case eOperator::not_eq:
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

AstNode* new_ast_node(MemoryArena* arena, eAstNode kind, SourceLoc* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode);
  node->src_loc = src_loc;
  node->kind = kind;
  return node;
}

SourceLoc* clone_source_loc(MemoryArena* arena, SourceLoc* src_loc)
{
  SourceLoc* clone = mem_push_struct(arena, SourceLoc);
  *clone = *src_loc;
  return clone;
}

Parser* parser_new(MemoryArena* arena)
{
  Parser* parser = mem_push_struct(arena, Parser);
  parser->arena = arena;
  parser->includes = List::create(arena, eList::ast_node);

  Lexer* lexer = parser->lexer = lexer_new(arena);
  parser->token = &lexer->token;
  parser->src_loc = &lexer->src_loc;

  return parser;
}

internal inline
void parser_set_input(Parser* parser, char* text, HFile* file)
{
  parser->file = file;
  lexer_set_input(parser->lexer, text, file->path);
}

internal inline
bool parser_get_next_token(Parser* parser)
{
  bool result = lexer_get_next_token(parser->lexer);
  return result;
}

internal inline
void parser_putback_token(Parser* parser)
{
  lexer_putback_token(parser->lexer);
}

internal inline
Parser* parser_included_new(Parser* parser)
{
  Parser* included_parser = parser_new(parser->arena);
  included_parser->includes = parser->includes;

  return included_parser;
}

//
//  Parsing procs
//-----------------

bool parse_actual_args(Parser* parser, AstNode* call);
bool parse_array(Parser* parser, AstNode** node);
bool parse_block_stmts(Parser* parser, AstNode* block);
bool parse_block_stmt(Parser* parser, AstNode** node);
bool parse_expr(Parser*, AstNode**);
bool parse_formal_args(Parser* parser, AstNode* args);
bool parse_cast(Parser* parser, AstNode** node);
bool parse_deref(Parser* parser, AstNode** node);
bool parse_module(Parser* parser);
bool parse_pointer(Parser* parser, AstNode* left_node, AstNode** node);
bool parse_rest_of_selector(Parser* parser, AstNode* left_node, AstNode** node);
bool parse_selector(Parser*, AstNode**);
bool parse_unr_expr(Parser*, AstNode**);

bool consume_semicolon(Parser* parser)
{
  bool success = true;
  if(parser->token->kind == eToken::semicolon)
  {
    success = parser_get_next_token(parser);
  }
  else
    success = compile_error(parser->arena, parser->src_loc, "`;` was expected at `%s`", get_token_printstr(parser->token));

  return success;
}

bool is_valid_expr_operand(AstNode* node)
{
  bool valid = false;
  switch(node->kind)
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

bool parse_rest_of_actual_args(Parser* parser, AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  bool success = true;
  if((parser->token->kind == eToken::comma) && (success = parser_get_next_token(parser)))
  {
    success = parse_actual_args(parser, args);
  }
  return success;
}

bool parse_actual_args(Parser* parser, AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  bool success = true;

  AstNode* expr = 0;
  if(success = parse_expr(parser, &expr))
  {
    if(expr)
    {
      AstNode* call_arg = new_ast_node(parser->arena, eAstNode::call_arg,
                                         clone_source_loc(parser->arena, parser->src_loc));
      call_arg->call_arg.expr = expr;

      args->node_list.append(call_arg, eList::ast_node);
      success = parse_rest_of_actual_args(parser, args);
    }
  }
  return success;
}

bool parse_call(Parser* parser, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(parser->token->kind == eToken::open_parens)
  {
    AstNode* call = *node = new_ast_node(parser->arena, eAstNode::call,
                                         clone_source_loc(parser->arena, parser->src_loc));
    call->call.expr = left_node;
    AstNode* args = call->call.args = new_ast_node(parser->arena, eAstNode::node_list,
                                                   clone_source_loc(parser->arena, parser->src_loc));
    args->node_list.init(parser->arena, eList::ast_node);

    if(success = parser_get_next_token(parser) && parse_actual_args(parser, call->call.args))
    {
      if(parser->token->kind == eToken::close_parens)
      {
        if(success = parser_get_next_token(parser))
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

  if(parser->token->kind == eToken::open_bracket)
  {
    AstNode* index = *node = new_ast_node(parser->arena, eAstNode::index,
                                          clone_source_loc(parser->arena, parser->src_loc));
    index->index.array_expr = left_node;
    index->index.ndim = *ndim;

    if(success = parser_get_next_token(parser) && parse_expr(parser, &index->index.i_expr))
    {
      if(parser->token->kind == eToken::close_bracket)
      {
        if(index->index.i_expr)
        {
          *ndim = *ndim + 1;
          if(success = parser_get_next_token(parser) && parse_index_recursive(parser, *node, node, ndim))
          {
            index->index.ndim = *ndim - index->index.ndim;
          }
        }
        else
          success = compile_error(parser->arena, parser->src_loc, "expression was expected at %s", get_token_printstr(parser->token));
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

bool parse_rest_of_unr_expr(Parser* parser, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken::dot:
    case eToken::arrow_right:
      {
        AstNode* bin_expr = *node = new_ast_node(parser->arena, eAstNode::bin_expr,
                                                 clone_source_loc(parser->arena, parser->src_loc));
        bin_expr->bin_expr.left_operand = left_node;

        switch(parser->token->kind)
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
        if(success = parser_get_next_token(parser) && parse_selector(parser, &right_operand))
        {
          if(right_operand)
          {
            switch(right_operand->kind)
            {
              case eAstNode::id:
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
    case eToken::star:
    case eToken::fwd_slash:
    case eToken::mod:
    case eToken::and:
    case eToken::ampersand:
    {
      AstNode* bin_expr = *node = new_ast_node(parser->arena, eAstNode::bin_expr,
                                                clone_source_loc(parser->arena, parser->src_loc));
      bin_expr->bin_expr.left_operand = left_node;

      switch(parser->token->kind)
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

        case eToken::and:
          bin_expr->bin_expr.op = eOperator::logic_and;
        break;

        case eToken::ampersand:
          bin_expr->bin_expr.op = eOperator::bit_and;
        break;

        default:
          assert(0);
      }

      AstNode* right_operand = 0;
      if(success = parser_get_next_token(parser) && parse_factor(parser, &right_operand))
      {
        if(right_operand)
        {
          if(is_valid_expr_operand(right_operand))
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
    case eToken::or:
      {
        AstNode* bin_expr = *node = new_ast_node(parser->arena, eAstNode::bin_expr,
                                                 clone_source_loc(parser->arena, parser->src_loc));
        bin_expr->bin_expr.left_operand = left_node;

        switch(parser->token->kind)
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
            bin_expr->bin_expr.op = eOperator::not_eq;
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

          case eToken::or:
            bin_expr->bin_expr.op = eOperator::logic_or;
          break;

          default:
            assert(0);
        }

        AstNode* right_operand = 0;
        if(success && (success = parser_get_next_token(parser) && parse_term(parser, &right_operand)))
        {
          if(right_operand)
          {
            if(is_valid_expr_operand(right_operand))
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
    case eToken::eq:
      {
        AstNode* assign = *node = new_ast_node(parser->arena, eAstNode::assign,
                                               clone_source_loc(parser->arena, parser->src_loc));
        assign->assign.dest_expr = left_node;

        AstNode* source_expr = 0;
        if(success = parser_get_next_token(parser) && parse_expr(parser, &source_expr))
        {
          if(source_expr)
          {
            if(is_valid_expr_operand(source_expr))
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

  if(parser->token->kind == eToken::id)
  {
    AstNode* id = *node = new_ast_node(parser->arena, eAstNode::id,
                                       clone_source_loc(parser->arena, parser->src_loc));
    id->id.name = parser->token->lexeme;
    success = parser_get_next_token(parser);
  }
  return success;
}

bool parse_basic_type(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken::int_:
    case eToken::float_:
    case eToken::bool_:
    case eToken::char_:
    case eToken::void_:
    case eToken::auto_:
    {
      AstNode* basic_type = *node = new_ast_node(parser->arena, eAstNode::basic_type,
                                                  clone_source_loc(parser->arena, parser->src_loc));
      switch(parser->token->kind)
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
      success = parser_get_next_token(parser);
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
    AstNode* cast = *node = new_ast_node(eAstNode::bin_expr, clone_source_loc(&parser->src_loc));
    cast->bin_expr.op = eOperator::cast;
    cast->bin_expr.left_operand = left_node;
    cast->bin_expr.right_operand = right_node;
  }
#else
  switch(parser->token->kind)
  {
    case eToken::colon:
    {
      AstNode* cast = *node = new_ast_node(parser->arena, eAstNode::cast,
                                            clone_source_loc(parser->arena, parser->src_loc));
      cast->cast.to_type = left_node;

      if(success = parser_get_next_token(parser) && parse_unr_expr(parser, &cast->cast.from_expr))
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

bool parse_rest_of_deref(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken::circumflex:
      success = parse_deref(parser, node);
    break;

    case eToken::id:
      success = parse_id(parser, node);
    break;

    case eToken::void_:
    case eToken::int_:
    case eToken::float_:
    case eToken::bool_:
    case eToken::char_:
    case eToken::auto_:
      success = parse_basic_type(parser, node);
    break;

    case eToken::open_parens:
    {
      if(success = parser_get_next_token(parser) && parse_expr(parser, node))
      {
        if(parser->token->kind == eToken::close_parens)
        {
          success = parser_get_next_token(parser) && parse_rest_of_cast(parser, *node, node);
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

  AstNode* deref = *node = new_ast_node(parser->arena, eAstNode::unr_expr,
                                        clone_source_loc(parser->arena, parser->src_loc));
  deref->unr_expr.op = eOperator::deref;
  if(success = parser_get_next_token(parser) && parse_rest_of_deref(parser, &deref->unr_expr.operand))
  {
    if(!deref->unr_expr.operand)
    {
      success = compile_error(parser->arena, parser->src_loc, "expression was expected at `%s`", get_token_printstr(parser->token));
    }
  }
  return success;
}

bool parse_rest_of_array(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken::open_bracket:
      success = parse_array(parser, node);
    break;

    case eToken::id:
      success = compile_error(parser->arena, parser->src_loc, "unknown type id `%s`", get_token_printstr(parser->token));
    break;

    case eToken::void_:
    case eToken::int_:
    case eToken::float_:
    case eToken::bool_:
    case eToken::char_:
    case eToken::auto_:
      success = parse_basic_type(parser, node);
    break;

    case eToken::open_parens:
    {
      if(success = parser_get_next_token(parser) && parse_array(parser, node) && parse_pointer(parser, *node, node))
      {
        if(parser->token->kind == eToken::close_parens)
        {
          success = parser_get_next_token(parser);
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

  if(parser->token->kind == eToken::open_bracket)
  {
    AstNode* array = *node = new_ast_node(parser->arena, eAstNode::array,
                                          clone_source_loc(parser->arena, parser->src_loc));
    if(success = parser_get_next_token(parser) && parse_expr(parser, &array->array.size_expr))
    {
      if(parser->token->kind == eToken::close_bracket)
      {
        if(array->array.size_expr)
        {
          if(success = parser_get_next_token(parser) && parse_rest_of_array(parser, &array->array.elem_expr))
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
    case eToken::open_parens:
    {
      if(success = parser_get_next_token(parser) && parse_expr(parser, node))
      {
        if(parser->token->kind == eToken::close_parens)
        {
          if(*node)
          {
            success = parser_get_next_token(parser) && parse_rest_of_cast(parser, *node, node);
          }
          else
            success = compile_error(parser->arena, parser->src_loc, "expression was expected at `%s`", get_token_printstr(parser->token));
        }
        else
          success = compile_error(parser->arena, parser->src_loc, "`)` was expected at `%s`", get_token_printstr(parser->token));
      }
    }
    break;

    case eToken::circumflex:
      success = parse_deref(parser, node) && parse_index(parser, *node, node);
    break;

    case eToken::open_bracket:
      success = parse_array(parser, node) && parse_pointer(parser, *node, node);
    break;

    case eToken::id:
      success = parse_id(parser, node) && parse_pointer(parser, *node, node) && parse_index(parser, *node, node);
    break;

    case eToken::void_:
    case eToken::int_:
    case eToken::float_:
    case eToken::bool_:
    case eToken::char_:
    case eToken::auto_:
      success = parse_basic_type(parser, node) && parse_pointer(parser, *node, node) && parse_index(parser, *node, node);
    break;
  }
  return success;
}

bool parse_pointer(Parser* parser, AstNode* left_node, AstNode** node)
{
  *node = left_node;
  bool success = true;

  if(parser->token->kind == eToken::circumflex)
  {
    AstNode* pointer = *node = new_ast_node(parser->arena, eAstNode::pointer,
                                            clone_source_loc(parser->arena, parser->src_loc));
    pointer->pointer.pointee = left_node;

    if((success = parser_get_next_token(parser)) && parser->token->kind == eToken::circumflex)
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
    case eToken::open_parens:
      success = parse_call(parser, left_node, node);
    break;

    case eToken::open_bracket:
      success = parse_index(parser, left_node, node);
    break;
  }
  return success;
}

bool parse_lit(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  AstNode* lit = *node = new_ast_node(parser->arena, eAstNode::lit,
                                      clone_source_loc(parser->arena, parser->src_loc));

  switch(parser->token->kind)
  {
    case eToken::int_val:
    {
      lit->lit.kind = eLiteral::int_;
      lit->lit.int_val = *parser->token->int_val;
    }
    break;

    case eToken::float_val:
    {
      lit->lit.kind = eLiteral::float_;
      lit->lit.float_val = *parser->token->float_val;
    }
    break;

    case eToken::true_:
    case eToken::false_:
    {
      lit->lit.kind = eLiteral::bool_;
      lit->lit.bool_val = (parser->token->kind == eToken::true_ ? 1 : 0);
    }
    break;

    case eToken::char_val:
    {
      lit->lit.kind = eLiteral::char_;
      lit->lit.char_val = parser->token->char_val;
    }
    break;

    case eToken::str_val:
    {
      lit->lit.kind = eLiteral::str;
      lit->lit.str_val = parser->token->str_val;
    }
    break;

    default: assert(0);
  }

  success = parser_get_next_token(parser);

  return success;
}

bool parse_selector(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  switch(parser->token->kind)
  {
    case eToken::true_:
    case eToken::false_:
    case eToken::int_val:
    case eToken::float_val:
    case eToken::char_val:
    case eToken::str_val:
      success = parse_lit(parser, node);
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
    if(parser->token->kind == eToken::id)
    {
      AstNode* type = *node;
      AstNode* var = *node = new_ast_node(parser->arena, eAstNode::var,
                                          clone_source_loc(parser->arena, parser->src_loc));
      var->var.type = type;
      var->var.name = parser->token->lexeme;

      success = parser_get_next_token(parser);
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
    case eToken::exclam:
    case eToken::not:
    case eToken::ampersand:
    case eToken::minus:
      {
        AstNode* unr_expr = *node = new_ast_node(parser->arena, eAstNode::unr_expr,
                                                 clone_source_loc(parser->arena, parser->src_loc));

        switch(parser->token->kind)
        {
          case eToken::exclam:
            unr_expr->unr_expr.op = eOperator::bit_not;
            break;
          case eToken::not:
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

        if(success && (success = parser_get_next_token(parser) && parse_factor(parser, &unr_expr->unr_expr.operand)))
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
  *modifier = eModifier::None;

  switch(parser->token->kind)
  {
    case eToken::extern_:
    {
      *modifier = eModifier::extern_;
      success = parser_get_next_token(parser);
    }
    break;

    case eToken::const_:
    {
      *modifier = eModifier::const_;
      success = parser_get_next_token(parser);
    }
    break;
  }

  return success;
}

bool parse_rest_of_formal_args(Parser* parser, AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  bool success = true;
  if((parser->token->kind == eToken::comma) && (success = parser_get_next_token(parser)))
  {
    success = parse_formal_args(parser, args);
  }
  return success;
}

bool parse_formal_args(Parser* parser, AstNode* args)
{
  assert(KIND(args, eAstNode::node_list));
  bool success = true;

  AstNode* arg = 0;
  if(success = parse_formal_arg(parser, &arg))
  {
    if(arg)
    {
      args->node_list.append(arg, eList::ast_node);
      success = parse_rest_of_formal_args(parser, args);
    }
  }
  return success;
}

bool parse_empty(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(parser->token->kind == eToken::semicolon)
  {
    *node = new_ast_node(parser->arena, eAstNode::empty,
                         clone_source_loc(parser->arena, parser->src_loc));
    success = parser_get_next_token(parser);
  }
  return success;
}

bool parse_block(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(parser->token->kind == eToken::open_brace)
  {
    AstNode* block = *node = new_ast_node(parser->arena, eAstNode::block,
                                          clone_source_loc(parser->arena, parser->src_loc));
    block->block.nodes.init(parser->arena, eList::ast_node);
    block->block.vars.init(parser->arena, eList::ast_node);
    block->block.stmts.init(parser->arena, eList::ast_node);

    if(success = (parser_get_next_token(parser) && parse_block_stmts(parser, block)))
    {
      if(parser->token->kind == eToken::close_brace)
      {
        success = parser_get_next_token(parser);
      }
      else
        success = compile_error(parser->arena, parser->src_loc, "`}` was expected at `%s`", get_token_printstr(parser->token));
    }
  }
  return success;
}

bool parse_else(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(parser->token->kind == eToken::else_)
  {
    success = parser_get_next_token(parser) && parse_block_stmt(parser, node);
  }
  return success;
}

bool parse_if(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;
  if(parser->token->kind == eToken::if_)
  {
    AstNode* if_ = *node = new_ast_node(parser->arena, eAstNode::if_,
                                        clone_source_loc(parser->arena, parser->src_loc));
    if(success = parser_get_next_token(parser))
    {
      if(parser->token->kind == eToken::open_parens)
      {
        if(success = (parser_get_next_token(parser) && parse_expr(parser, &if_->if_.cond_expr)))
        {
          if(if_->if_.cond_expr)
          {
            if(parser->token->kind == eToken::close_parens)
            {
              success = parser_get_next_token(parser) && parse_block_stmt(parser, &if_->if_.body) && parse_else(parser, &if_->if_.else_body);
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
  if(parser->token->kind == eToken::do_)
  {
    AstNode* do_while = *node = new_ast_node(parser->arena, eAstNode::do_while,
                                             clone_source_loc(parser->arena, parser->src_loc));
    if(success = parser_get_next_token(parser) && parse_block_stmt(parser, &do_while->do_while.body))
    {
      if(parser->token->kind == eToken::while_)
      {
        if(success = parser_get_next_token(parser))
        {
          if(parser->token->kind == eToken::open_parens)
          {
            if(success = parser_get_next_token(parser) && parse_expr(parser, &do_while->do_while.cond_expr))
            {
              if(do_while->do_while.cond_expr)
              {
                if(parser->token->kind == eToken::close_parens)
                  success = parser_get_next_token(parser);
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
  if(parser->token->kind == eToken::while_)
  {
    AstNode* while_ = *node = new_ast_node(parser->arena, eAstNode::while_,
                                           clone_source_loc(parser->arena, parser->src_loc));
    if(success = parser_get_next_token(parser))
    {
      if(parser->token->kind == eToken::open_parens)
      {
        if(success = parser_get_next_token(parser) && parse_expr(parser, &while_->while_.cond_expr))
        {
          if(while_->while_.cond_expr)
          {
            if(parser->token->kind == eToken::close_parens)
            {
              success = parser_get_next_token(parser) && parse_block_stmt(parser, &while_->while_.body);
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

  if(parser->token->kind == eToken::return_)
  {
    AstNode* ret = *node = new_ast_node(parser->arena, eAstNode::return_,
                                        clone_source_loc(parser->arena, parser->src_loc));
    AstNode* ret_expr = 0;
    if(success = parser_get_next_token(parser) && parse_expr(parser, &ret_expr))
    {
      if(ret_expr)
      {
        if(is_valid_expr_operand(ret_expr))
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

  if(parser->token->kind == eToken::continue_)
  {
    AstNode* loop_ctrl = *node = new_ast_node(parser->arena, eAstNode::loop_ctrl,
                                              clone_source_loc(parser->arena, parser->src_loc));
    loop_ctrl->loop_ctrl.kind = eLoopCtrl::continue_;
    success = parser_get_next_token(parser);
  }
  return success;
}

bool parse_break(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token->kind == eToken::break_)
  {
    AstNode* loop_ctrl = *node = new_ast_node(parser->arena, eAstNode::loop_ctrl,
                                              clone_source_loc(parser->arena, parser->src_loc));
    loop_ctrl->loop_ctrl.kind = eLoopCtrl::break_;
    success = parser_get_next_token(parser);
  }
  return success;
}

bool parse_block_var(Parser* parser, char* name, eModifier modifier, AstNode* var_type, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token->kind == eToken::id)
  {
    AstNode* var = *node = new_ast_node(parser->arena, eAstNode::var,
                                        clone_source_loc(parser->arena, parser->src_loc));
    var->var.type = var_type;
    var->var.name = parser->token->lexeme;

    if(success = parser_get_next_token(parser))
    {
      if(parser->token->kind == eToken::eq)
      {
        success = parser_get_next_token(parser) && parse_expr(parser, &var->var.init_expr);
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
    case eToken::open_brace:
      success = parse_block(parser, node);
    break;

    case eToken::if_:
      success = parse_if(parser, node);
    break;

    case eToken::do_:
      success = parse_do_while(parser, node);
    break;

    case eToken::while_:
      success = parse_while(parser, node);
    break;

    case eToken::semicolon:
      success = parse_empty(parser, node);
    break;

    case eToken::return_:
      success = parse_return(parser, node) && consume_semicolon(parser);
    break;

    case eToken::break_:
      success = parse_break(parser, node) && consume_semicolon(parser);
    break;

    case eToken::continue_:
      success = parse_continue(parser, node) && consume_semicolon(parser);
    break;

    default:
    {
      eModifier modifier = eModifier::None;

      if((success = success = parse_modifier(parser, &modifier) && parse_expr(parser, node)) && *node)
      {
        if(parser->token->kind == eToken::id)
        {
          char* var_name = parser->token->lexeme;
          success = parse_block_var(parser, var_name, modifier, *node, node) && consume_semicolon(parser);
        }
        else
        {
          success = consume_semicolon(parser);
        }
      }
    }
    break;
  }
  return success;
}

bool parse_block_stmts(Parser* parser, AstNode* block)
{
  assert(KIND(block, eAstNode::block));

  bool success = true;
  AstNode* stmt = 0;
  if(success = parse_block_stmt(parser, &stmt))
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
    case eToken::open_brace:
      success = parse_block(parser, &proc->proc.body);
    break;

    case eToken::semicolon:
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

  if(parser->token.kind == eToken::enum)
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
          success = compile_error(parser->arena, &parser->src_loc, "`}` was expected at `%s`", get_token_printstr(parser->token));
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

  if(parser->token->kind == eToken::open_parens)
  {
    AstNode* proc = *node = new_ast_node(parser->arena, eAstNode::proc,
                                         clone_source_loc(parser->arena, parser->src_loc));
    proc->proc.ret_type = ret_type;
    proc->proc.name = name;
    proc->proc.modifier = modifier;

    if(success = parser_get_next_token(parser))
    {
      AstNode* args = proc->proc.args = new_ast_node(parser->arena, eAstNode::node_list,
                                                     clone_source_loc(parser->arena, parser->src_loc));
      args->node_list.init(parser->arena, eList::ast_node);

      if(success = parse_formal_args(parser, proc->proc.args))
      {
        if(parser->token->kind == eToken::close_parens)
        {
          success = parser_get_next_token(parser) && parse_proc_body(parser, proc);
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

  AstNode* var = *node = new_ast_node(parser->arena, eAstNode::var,
                                      clone_source_loc(parser->arena, parser->src_loc));
  var->var.type = var_type;
  var->var.name = name;
  var->var.modifier = modifier;

  return success;
}

AstNode* parser_find_include(Parser* parser, HFile* file)
{
  AstNode* include = 0;

  for(ListItem* li = parser->includes->first;
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

void merge_modules(AstNode* main_module, AstNode* merged_module)
{
  main_module->module.nodes.join(&merged_module->module.nodes);
  main_module->module.procs.join(&merged_module->module.procs);
  main_module->module.vars.join(&merged_module->module.vars);
}

bool parse_module_include(Parser* parser, AstNode** node)
{
  *node = 0;
  bool success = true;

  if(parser->token->kind == eToken::include)
  {
    if(success = parser_get_next_token(parser))
    {
      AstNode* include = *node = new_ast_node(parser->arena, eAstNode::include,
                                              clone_source_loc(parser->arena, parser->src_loc));
      if(parser->token->kind == eToken::str_val)
      {
        /* Make the full path to the included file, relative to the location of the current file. */
        String str = {};
        str.init(parser->arena);
        str.append(parser->src_loc->file_path);
        Platform::path_make_dir(str.head);
        str.tidyup();
        str.append(parser->token->str_val);

        include->include.file_path = str.cap();

        if(success = parser_get_next_token(parser) && consume_semicolon(parser))
        {
          HFile* included_file = include->include.file = Platform::file_open(parser->arena, include->include.file_path);
          if(included_file)
          {
            AstNode* previous_include = parser_find_include(parser, included_file);
            if(!previous_include)
            {
              parser->includes->append(include, eList::ast_node);

              char* hoc_text = Platform::file_read_text(parser->arena, included_file->path);
              if(hoc_text)
              {
                Parser* included_parser = parser_included_new(parser);
                parser_set_input(included_parser, hoc_text, included_file);

                if(success = parse_module(included_parser))
                {
                  merge_modules(parser->module, included_parser->module);
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
            parser_putback_token(parser);
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
    case eToken::include:
    {
      success = parse_module_include(parser, node);
    }
    break;

    default:
    {
      eModifier modifier = eModifier::None;

      if((success = parse_modifier(parser, &modifier) && parse_expr(parser, node)) && *node)
      {
        if(parser->token->kind == eToken::id)
        {
          char* name = parser->token->lexeme;
          if(success = parser_get_next_token(parser))
          {
            if(parser->token->kind == eToken::open_parens)
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
    }
    break;
  }
  return success;
}

bool parse_module_stmts(Parser* parser, AstNode* module)
{
  assert(KIND(module, eAstNode::module));
  bool success = true;
  AstNode* stmt = 0;

  if(success = parse_module_stmt(parser, &stmt))
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

      success = parse_module_stmts(parser, module);
    }
  }
  return success;
}

bool parse_module_body(Parser* parser, AstNode* module)
{
  assert(KIND(module, eAstNode::module));
  bool success = true;

  if(success = parse_module_stmts(parser, module))
  {
    if(parser->token->kind == eToken::end_of_input)
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

  if(success = parser_get_next_token(parser))
  {
    AstNode* module = parser->module = module = new_ast_node(parser->arena, eAstNode::module,
                                           clone_source_loc(parser->arena, parser->src_loc));
    module->module.file_path = parser->file->path;
    module->module.nodes.init(parser->arena, eList::ast_node);
    module->module.procs.init(parser->arena, eList::ast_node);
    module->module.vars.init(parser->arena, eList::ast_node);

    AstNode* include = new_ast_node(parser->arena, eAstNode::include, 0);
    include->include.file = parser->file;
    include->include.file_path = parser->file->path;
    parser->includes->append(include, eList::ast_node);

    success = parse_module_body(parser, module);;
  }

  return success;
}

