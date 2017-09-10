bool parse_initializer_list(TokenStream*, CstNode**);
bool parse_expression(TokenStream*, CstNode**);
bool parse_statement(TokenStream*, CstNode**);
bool parse_selector(TokenStream*, CstNode**);
bool parse_un_expr(TokenStream*, CstNode**);
bool parse_struct_member_list(TokenStream*, List*);
void DEBUG_print_cst_node(String* str, int indent_level, CstNode* node, char* tag);

#define CST(VAR, KIND)\
  (((VAR)->kind == CstKind##_##KIND) ? &(VAR)->KIND : 0)

#define CST_ITEM(VAR)\
  (((VAR)->kind == ListKind_cst_node) ? (VAR)->cst_node : 0)

CstNode*
new_cst_node(SourceLoc* src_loc, CstKind kind)
{
  CstNode* node = mem_push_struct(arena, CstNode);
  node->kind = kind;

  node->src_loc = mem_push_struct(arena, SourceLoc);
  *node->src_loc = *src_loc;
  return node;
}

CstNode*
new_cst_statement(SourceLoc* src_loc, CstNode* stmt)
{
  CstNode* node = new_cst_node(src_loc, CstKind_stmt);
  node->stmt.stmt = stmt;
  return node;
}

CstNode*
new_cst_block(SourceLoc* src_loc)
{
  CstNode* node = new_cst_node(src_loc, CstKind_block);
  node->block.nodes = new_list(arena, ListKind_cst_node);
  return node;
}

CstNode*
new_cst_module(SourceLoc* src_loc, char* file_path)
{
  CstNode* node = new_cst_node(src_loc, CstKind_module);
  node->module.file_path = file_path;
  node->module.body = new_cst_block(src_loc);
  return node;
}

CstNode*
new_cst_include(SourceLoc* src_loc, char* file_path)
{
  CstNode* node = new_cst_node(src_loc, CstKind_include);
  node->module.file_path = file_path;
  node->module.body = new_cst_block(src_loc);
  return node;
}

CstNode*
new_cst_id(SourceLoc* src_loc, char* name)
{
  CstNode* node = new_cst_node(src_loc, CstKind_id);
  node->id.name = name;
  return node;
}

CstNode*
new_cst_proc(SourceLoc* src_loc)
{
  CstNode* node = new_cst_node(src_loc, CstKind_proc);
  node->proc.args = new_list(arena, ListKind_cst_node);
  return node;
}

CstNode*
new_cst_call(SourceLoc* src_loc)
{
  CstNode* node = new_cst_node(src_loc, CstKind_call);
  node->call.args = new_list(arena, ListKind_cst_node);
  return node;
}

CstNode*
new_cst_enum(SourceLoc* src_loc)
{
  CstNode* node = new_cst_node(src_loc, CstKind_enum_decl);
  node->enum_decl.members = new_list(arena, ListKind_cst_node);
  return node;
}

CstNode*
new_cst_struct(SourceLoc* src_loc)
{
  CstNode* node = new_cst_node(src_loc, CstKind_struct_decl);
  node->struct_decl.members = new_list(arena, ListKind_cst_node);
  return node;
}

CstNode*
new_cst_union(SourceLoc* src_loc)
{
  CstNode* node = new_cst_node(src_loc, CstKind_union_decl);
  node->union_decl.members = new_list(arena, ListKind_cst_node);
  return node;
}

CstNode*
new_cst_initializer_list(SourceLoc* src_loc)
{
  CstNode* node = new_cst_node(src_loc, CstKind_init_list);
  node->init_list.members = new_list(arena, ListKind_cst_node);
  return node;
}

CstNode*
new_cst_bin_expr(SourceLoc* src_loc, OpKind op)
{
  CstNode* node = new_cst_node(src_loc, CstKind_bin_expr);
  node->bin_expr.op = op;
  return node;
}

CstNode*
new_cst_unr_expr(SourceLoc* src_loc, OpKind op)
{
  CstNode* node = new_cst_node(src_loc, CstKind_un_expr);
  node->un_expr.op = op;
  return node;
}

char*
get_cst_op_printstr(OpKind op)
{
  char* result = "???";

  if(op == OpKind__None)
    result = stringify(OpKind__None);
  else if(op == OpKind_Add)
    result = stringify(OpKind_Add);
  else if(op == OpKind_Sub)
    result = stringify(OpKind_Sub);
  else if(op == OpKind_Div)
    result = stringify(OpKind_Div);
  else if(op == OpKind_Mul)
    result = stringify(OpKind_Mul);
  else if(op == OpKind_Mod)
    result = stringify(OpKind_Mod);
  else if(op == OpKind_Neg)
    result = stringify(OpKind_Neg);
  else if(op == OpKind_Assign)
    result = stringify(OpKind_Assign);
  else if(op == OpKind_PointerDeref)
    result = stringify(OpKind_PointerDeref);
  else if(op == OpKind_AddressOf)
    result = stringify(OpKind_AddressOf);
  else if(op == OpKind_MemberSelect)
    result = stringify(OpKind_MemberSelect);
  else if(op == OpKind_PtrMemberSelect)
    result = stringify(OpKind_PtrMemberSelect);
  else if(op == OpKind_ArrayIndex)
    result = stringify(OpKind_ArrayIndex);
  else if(op == OpKind_PreDecrement)
    result = stringify(OpKind_PreDecrement);
  else if(op == OpKind_PostDecrement)
    result = stringify(OpKind_PostDecrement);
  else if(op == OpKind_PreIncrement)
    result = stringify(OpKind_PreIncrement);
  else if(op == OpKind_PostIncrement)
    result = stringify(OpKind_PostIncrement);
  else if(op == OpKind_Equals)
    result = stringify(OpKind_Equals);
  else if(op == OpKind_NotEquals)
    result = stringify(OpKind_NotEquals);
  else if(op == OpKind_Less)
    result = stringify(OpKind_Less);
  else if(op == OpKind_LessEquals)
    result = stringify(OpKind_LessEquals);
  else if(op == OpKind_Greater)
    result = stringify(OpKind_Greater);
  else if(op == OpKind_GreaterEquals)
    result = stringify(OpKind_GreaterEquals);
  else if(op == OpKind_LogicAnd)
    result = stringify(OpKind_LogicAnd);
  else if(op == OpKind_LogicOr)
    result = stringify(OpKind_LogicOr);
  else if(op == OpKind_LogicNot)
    result = stringify(OpKind_LogicNot);
  else if(op == OpKind_BitwiseAnd)
    result = stringify(OpKind_BitwiseAnd);
  else if(op == OpKind_BitwiseOr)
    result = stringify(OpKind_BitwiseOr);

  return result;
}

#if 1
char*
get_cst_kind_printstr(CstKind kind)
{
  char* result = "???";

  if(kind == CstKind__None)
    result = stringify(CstKind__None);
  else if(kind == CstKind_bin_expr)
    result = stringify(CstKind_bin_expr);
  else if(kind == CstKind_un_expr)
    result = stringify(CstKind_un_expr);
  else if(kind == CstKind_lit)
    result = stringify(CstKind_lit);
  else if(kind == CstKind_var_decl)
    result = stringify(CstKind_var_decl);
  else if(kind == CstKind_stmt)
    result = stringify(CstKind_stmt);
  else if(kind == CstKind_block)
    result = stringify(CstKind_block);
  else if(kind == CstKind_proc)
    result = stringify(CstKind_proc);
  else if(kind == CstKind_id)
    result = stringify(CstKind_id);
  else if(kind == CstKind_while_stmt)
    result = stringify(CstKind_while_stmt);
  else if(kind == CstKind_do_while_stmt)
    result = stringify(CstKind_do_while_stmt);
  else if(kind == CstKind_for_stmt)
    result = stringify(CstKind_for_stmt);
  else if(kind == CstKind_if_stmt)
    result = stringify(CstKind_if_stmt);
  else if(kind == CstKind_return_stmt)
    result = stringify(CstKind_return_stmt);
  else if(kind == CstKind_break_stmt)
    result = stringify(CstKind_break_stmt);
  else if(kind == CstKind_continue_stmt)
    result = stringify(CstKind_continue_stmt);
  else if(kind == CstKind_goto_stmt)
    result = stringify(CstKind_goto_stmt);
  else if(kind == CstKind_label)
    result = stringify(CstKind_label);
  else if(kind == CstKind_include)
    result = stringify(CstKind_include);
  else if(kind == CstKind_module)
    result = stringify(CstKind_module);
  else if(kind == CstKind_cast)
    result = stringify(CstKind_cast);
  else if(kind == CstKind_call)
    result = stringify(CstKind_call);
  else if(kind == CstKind_array)
    result = stringify(CstKind_array);
  else if(kind == CstKind_pointer)
    result = stringify(CstKind_pointer);
  else if(kind == CstKind_struct_decl)
    result = stringify(CstKind_struct_decl);
  else if(kind == CstKind_union_decl)
    result = stringify(CstKind_union_decl);
  else if(kind == CstKind_enum_decl)
    result = stringify(CstKind_enum_decl);
  else if(kind == CstKind_init_list)
    result = stringify(CstKind_init_list);
  else if(kind == CstKind_hoc_new)
    result = stringify(CstKind_hoc_new);
  else if(kind == CstKind_hoc_putc)
    result = stringify(CstKind_hoc_putc);

  return result;
}
#endif

char*
get_cst_literal_printstr(CstLiteralKind kind)
{
  char* result = "???";

  if(kind == CstLiteralKind__None)
    result = stringify(CstLiteralKind__None);
  else if(kind == CstLiteralKind_int_val)
    result = stringify(CstLiteralKind_int_val);
  else if(kind == CstLiteralKind_float_val)
    result = stringify(CstLiteralKind_float_val);
  else if(kind == CstLiteralKind_bool_val)
    result = stringify(CstLiteralKind_bool_val);
  else if(kind == CstLiteralKind_str)
    result = stringify(CstLiteralKind_str);
  else if(kind == CstLiteralKind_char_val)
    result = stringify(CstLiteralKind_char_val);

  return result;
}

bool
parse_semicolon(TokenStream* input)
{
  bool success = true;
  if(input->token.kind == TokenKind_Semicolon)
    success = get_next_token(input);
  else
    success = compile_error(&input->src_loc, "expected `;`, actual `%s`", get_token_printstr(&input->token));
  return success;
}

bool
parse_initializer_member_list(TokenStream* input, List* member_list)
{
  bool success = true;

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
        append_list_elem(arena, member_list, member, ListKind_cst_node);
        if(input->token.kind == TokenKind_Comma)
          success = get_next_token(input);
      }
    }
  }
  while(success && member);
  return success;
}

bool
parse_initializer_list(TokenStream* input, CstNode** node)
{
  node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    auto* init_list = CST(*node = new_cst_initializer_list(&input->src_loc), init_list);

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

bool
parse_actual_arg_list(TokenStream* input, List* args)
{
  bool success = true;

  CstNode* arg = 0;
  do
  {
    arg = 0;
    success = parse_expression(input, &arg);
    if(success && arg)
    {
      append_list_elem(arena, args, arg, ListKind_cst_node);
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

bool
parse_statement_list(TokenStream* input, List* stmt_list)
{
  bool success = true;

  CstNode* stmt = 0;
  do
  {
    stmt = 0;
    while(input->token.kind == TokenKind_Semicolon && (success = get_next_token(input)))
      ;

    if((success = parse_statement(input, &stmt)) && stmt)
    {
      append_list_elem(arena, stmt_list, stmt, ListKind_cst_node);
    }
  }
  while(success && stmt);

  return success;
}

bool
parse_block(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    auto* block = CST(*node = new_cst_block(&input->src_loc), block);

    if(success = get_next_token(input) && parse_statement_list(input, block->nodes))
    {
      if(input->token.kind == TokenKind_CloseBrace)
        success = get_next_token(input);
      else
        success = compile_error(&input->src_loc, "expected `}`, actual `%s`", get_token_printstr(&input->token));
    }
  }
  return success;
}

bool
parse_rest_of_id(TokenStream* input, CstNode* left_node, CstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    // procedure call
    if(left_node->kind == CstKind_id)
    {
      auto* call = CST(*node = new_cst_call(&input->src_loc), call);
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
    auto* index = CST(*node = new_cst_bin_expr(&input->src_loc, OpKind_ArrayIndex), bin_expr);

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

bool
parse_rest_of_selector(TokenStream* input, CstNode* left_node, CstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == TokenKind_Dot ||
     input->token.kind == TokenKind_ArrowRight)
  {
    auto* bin_expr = CST(*node = new_cst_node(&input->src_loc, CstKind_bin_expr), bin_expr);

    bin_expr->left_operand = left_node;

    if(input->token.kind == TokenKind_Dot)
      bin_expr->op = OpKind_MemberSelect;
    else if(input->token.kind == TokenKind_ArrowRight)
      bin_expr->op = OpKind_PtrMemberSelect;

    if(success = get_next_token(input) && parse_selector(input, &bin_expr->right_operand))
    {
      if(bin_expr->right_operand)
        success = parse_rest_of_selector(input, *node, node);
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
    auto* un_expr = CST(*node = new_cst_node(&input->src_loc, CstKind_un_expr), un_expr);

    un_expr->operand = left_node;

    if(input->token.kind == TokenKind_MinusMinus)
    {
#if 0
      un_expr->op = OpKind_PostDecrement;
#else
      success = compile_error(&input->src_loc, "`--` not supported");
#endif
    }
    else if(input->token.kind == TokenKind_PlusPlus)
    {
#if 0
      un_expr->op = OpKind_PostIncrement;
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
parse_factor(TokenStream* input, CstNode** node)
{
  bool success = true;

  if((success = parse_un_expr(input, node)) && *node)
    success = parse_rest_of_selector(input, *node, node);
  return success;
}

bool
parse_rest_of_factor(TokenStream* input, CstNode* left_node, CstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == TokenKind_Star ||
     input->token.kind == TokenKind_FwdSlash ||
     input->token.kind == TokenKind_Percent)
  {
    auto* bin_expr = CST(*node = new_cst_node(&input->src_loc, CstKind_bin_expr), bin_expr);

    bin_expr->left_operand = left_node;

    if(input->token.kind == TokenKind_Star)
      bin_expr->op = OpKind_Mul;
    else if(input->token.kind == TokenKind_FwdSlash)
      bin_expr->op = OpKind_Div;
    else if(input->token.kind == TokenKind_Percent)
      bin_expr->op = OpKind_Mod;
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

bool
parse_term(TokenStream* input, CstNode** node)
{
  bool success = true;

  if((success = parse_factor(input, node)) && *node)
    success = parse_rest_of_factor(input, *node, node);
  return success;
}

bool
parse_rest_of_term(TokenStream* input, CstNode* left_node, CstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == TokenKind_Plus ||
     input->token.kind == TokenKind_Minus ||
     input->token.kind == TokenKind_Pipe ||
     input->token.kind == TokenKind_PipePipe ||
     input->token.kind == TokenKind_Ampersand ||
     input->token.kind == TokenKind_AmpersandAmpersand)
  {
    auto* bin_expr = CST(*node = new_cst_node(&input->src_loc, CstKind_bin_expr), bin_expr);

    bin_expr->left_operand = left_node;

    if(input->token.kind == TokenKind_Plus)
      bin_expr->op = OpKind_Add;
    else if(input->token.kind == TokenKind_Minus)
      bin_expr->op = OpKind_Sub;
    else if(input->token.kind == TokenKind_Pipe)
      bin_expr->op = OpKind_BitwiseOr;
    else if(input->token.kind == TokenKind_PipePipe)
      bin_expr->op = OpKind_LogicOr;
    else if(input->token.kind == TokenKind_Ampersand)
      bin_expr->op = OpKind_BitwiseAnd;
    else if(input->token.kind == TokenKind_AmpersandAmpersand)
      bin_expr->op = OpKind_LogicAnd;
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

bool
parse_assignment(TokenStream* input, CstNode** node)
{
  bool success = true;

  if((success = parse_term(input, node)) && *node)
    success = parse_rest_of_term(input, *node, node);
  return success;
}

bool
parse_rest_of_assignment(TokenStream* input, CstNode* left_node, CstNode** node)
{
  *node = left_node;
  bool success = true;

  if(input->token.kind == TokenKind_Equals ||
     input->token.kind == TokenKind_EqualsEquals ||
     input->token.kind == TokenKind_ExclamEquals ||
     input->token.kind == TokenKind_AngleLeft ||
     input->token.kind == TokenKind_AngleLeftEquals ||
     input->token.kind == TokenKind_AngleRight ||
     input->token.kind == TokenKind_AngleRightEquals)
  {
    auto* bin_expr = CST(*node = new_cst_node(&input->src_loc, CstKind_bin_expr), bin_expr);

    bin_expr->left_operand = left_node;

    if(input->token.kind == TokenKind_Equals)
      bin_expr->op = OpKind_Assign;
    else if(input->token.kind == TokenKind_EqualsEquals)
      bin_expr->op = OpKind_Equals;
    else if(input->token.kind == TokenKind_ExclamEquals)
      bin_expr->op = OpKind_NotEquals;
    else if(input->token.kind == TokenKind_AngleLeft)
      bin_expr->op = OpKind_Less;
    else if(input->token.kind == TokenKind_AngleLeftEquals)
      bin_expr->op = OpKind_LessEquals;
    else if(input->token.kind == TokenKind_AngleRight)
      bin_expr->op = OpKind_Greater;
    else if(input->token.kind == TokenKind_AngleRightEquals)
      bin_expr->op = OpKind_GreaterEquals;

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

bool
parse_type_expr_pointer(TokenStream* input, CstNode* expr, CstNode** node)
{
  *node = expr;
  bool success = true;
  
  if(input->token.kind == TokenKind_Star)
  {
    auto* ptr = CST(*node = new_cst_node(&input->src_loc, CstKind_pointer), pointer);

    ptr->type_expr = expr;

    success = get_next_token(input) && parse_type_expr_pointer(input, *node, node);
  }
  return true;
}

bool
parse_type_expr_id(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Id)
  {
    *node = new_cst_id(&input->src_loc, input->token.lexeme);

    success = get_next_token(input) && parse_type_expr_pointer(input, *node, node);
  }
  return success;
}

bool
parse_type_expr(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_OpenBracket)
  {
    auto* array = CST(*node = new_cst_node(&input->src_loc, CstKind_array), array);

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

bool
parse_new(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = false;

  if(input->token.kind == TokenKind_New && (success = get_next_token(input)))
  {
    if(input->token.kind == TokenKind_OpenParens)
    {
      auto* hoc_new = CST(*node = new_cst_node(&input->src_loc, CstKind_hoc_new), hoc_new);

      if(success = get_next_token(input) && parse_type_expr(input, &hoc_new->type_expr))
      {
#if 1
        if(input->token.kind == TokenKind_Comma)
        {
          if(success = get_next_token(input) && parse_expression(input, &hoc_new->count_expr))
          {
            if(input->token.kind == TokenKind_CloseParens)
            {
              if(hoc_new->count_expr)
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

bool
parse_putc(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = false;

  if(input->token.kind == TokenKind_Putc && (success = get_next_token(input)))
  {
    if(input->token.kind == TokenKind_OpenParens)
    {
      auto* hoc_putc = CST(*node = new_cst_node(&input->src_loc, CstKind_hoc_putc), hoc_putc);

      if(success = get_next_token(input) && parse_expression(input, &hoc_putc->expr))
      {
        if(input->token.kind == TokenKind_CloseParens)
        {
          if(hoc_putc->expr)
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

bool
parse_selector(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

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
    auto* lit = CST(*node = new_cst_node(&input->src_loc, CstKind_lit), lit);

    if(input->token.kind == TokenKind_IntNum)
    {
      lit->kind = CstLiteralKind_int_val;
      lit->int_val = *input->token.int_val;
    }
    else if(input->token.kind == TokenKind_FloatNum)
    {
      lit->kind = CstLiteralKind_float_val;
      lit->float_val = *input->token.float_val;
    }
    else if(input->token.kind == TokenKind_True ||
            input->token.kind == TokenKind_False)
    {
      lit->kind = CstLiteralKind_bool_val;
      lit->bool_val = (input->token.kind == TokenKind_True ? 1 : 0);
    }
    else if(input->token.kind == TokenKind_Char)
    {
      lit->kind = CstLiteralKind_char_val;
      lit->char_val = input->token.char_val;
    }
    else if(input->token.kind == TokenKind_String)
    {
      lit->kind = CstLiteralKind_str;
      lit->str = input->token.str;
    }
    else
      assert(0);

    success = get_next_token(input);
  }
  else if(input->token.kind == TokenKind_Id)
  {
    *node = new_cst_id(&input->src_loc, input->token.lexeme);
    success = get_next_token(input) && parse_rest_of_id(input, *node, node);
  }
  else if(input->token.kind == TokenKind_New)
    success = parse_new(input, node);
  else if(input->token.kind == TokenKind_Putc)
    success = parse_putc(input, node);

  return success;
}

bool
parse_formal_arg(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  CstNode* type = 0;
  if((success = parse_type_expr(input, &type)) && type)
  {
    auto* var_decl = CST(*node = new_cst_node(&input->src_loc, CstKind_var_decl), var_decl);

    var_decl->type_expr = type;

    if(input->token.kind == TokenKind_Id)
    {
      var_decl->id = new_cst_id(&input->src_loc, input->token.lexeme);
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

  CstNode* arg = 0;
  do
  {
    arg = 0;
    if((success = parse_formal_arg(input, &arg)) && arg)
    {
      append_list_elem(arena, arg_list, arg, ListKind_cst_node);
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

bool
parse_un_expr(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Exclam ||
     input->token.kind == TokenKind_Star ||
     input->token.kind == TokenKind_Ampersand ||
     input->token.kind == TokenKind_Minus ||
     input->token.kind == TokenKind_MinusMinus ||
     input->token.kind == TokenKind_PlusPlus)
  {
    auto* un_expr = CST(*node = new_cst_node(&input->src_loc, CstKind_un_expr), un_expr);

    if(input->token.kind == TokenKind_Exclam)
      un_expr->op = OpKind_LogicNot;
    else if(input->token.kind == TokenKind_Star)
      un_expr->op = OpKind_PointerDeref;
    else if(input->token.kind == TokenKind_Ampersand)
      un_expr->op = OpKind_AddressOf;
    else if(input->token.kind == TokenKind_Minus)
      un_expr->op = OpKind_Neg;
    else if(input->token.kind == TokenKind_MinusMinus)
    {
#if 0
      un_expr->op = OpKind_PreDecrement;
#else
      success = compile_error(&input->src_loc, "`--` not supported");
#endif
    }
    else if(input->token.kind == TokenKind_PlusPlus)
    {
#if 0
      un_expr->op = OpKind_PreIncrement;
#else
      success = compile_error(&input->src_loc, "`++` not supported");
#endif
    }
    else
      assert(0);

    if(success && (success = get_next_token(input)) && parse_factor(input, &un_expr->operand))
    {
      if(!un_expr->operand)
      {
        putback_token(input);
        success = compile_error(&input->src_loc, "operand expected, at `%s`", get_token_printstr(&input->token));
      }
    }
  }
  else if(input->token.kind == TokenKind_Cast)
  {
    // cast
    auto* cast = CST(*node = new_cst_node(&input->src_loc, CstKind_cast), cast);

    if(success = get_next_token(input))
    {
      if(input->token.kind == TokenKind_OpenParens)
      {
        if(success = get_next_token(input) && parse_type_expr(input, &cast->type_expr))
        {
          if(input->token.kind == TokenKind_CloseParens)
          {
            if(cast->type_expr)
              success = get_next_token(input) && parse_un_expr(input, &cast->expr);
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
parse_expression(TokenStream* input, CstNode** node)
{
  bool success = true;

  if((success = parse_assignment(input, node)) && *node)
    success = parse_rest_of_assignment(input, *node, node);
  return success;
}

bool
parse_var_decl(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  CstNode* type = 0;
  if((success = parse_type_expr(input, &type)) && type)
  {
    if(input->token.kind == TokenKind_Id)
    {
      auto* var_decl = CST(*node = new_cst_node(&input->src_loc, CstKind_var_decl), var_decl);

      var_decl->type_expr = type;
      var_decl->id = new_cst_id(&input->src_loc, input->token.lexeme);

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

bool
parse_for_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_For)
  {
    auto* for_stmt = CST(*node = new_cst_node(&input->src_loc, CstKind_for_stmt), for_stmt);

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
                auto* block = CST(for_stmt->body = new_cst_block(&input->src_loc), block);
                append_list_elem(arena, block->nodes, single_stmt, ListKind_cst_node);
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

bool
parse_while_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_While)
  {
    auto* while_stmt = CST(*node = new_cst_node(&input->src_loc, CstKind_while_stmt), while_stmt);

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

bool
parse_else_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

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

bool
parse_if_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_If)
  {
    auto* if_stmt = CST(*node = new_cst_node(&input->src_loc, CstKind_if_stmt), if_stmt);

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

bool
parse_proc_decl(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Proc)
  {
    auto* proc = CST(*node = new_cst_proc(&input->src_loc), proc);

    if(success = get_next_token(input) && parse_type_expr(input, &proc->ret_type_expr))
    {
      if(proc->ret_type_expr)
      {
        if(input->token.kind == TokenKind_Id)
        {
          proc->id = new_cst_id(&input->src_loc, input->token.lexeme);

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
#if 0
                if(success = get_next_token(input) && parse_block(input, &proc->body))
                {
                  if(!proc->body && (proc->is_decl = success = parse_semicolon(input)))
                    proc->body = (CstNode*)new_cst_block(&(*node)->src_loc);
                }
#else
                if(success = get_next_token(input) && parse_block(input, &proc->body))
                {
                  if(!proc->body)
                    success = parse_semicolon(input);
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
parse_include_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Include)
  {
    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == TokenKind_String)
    {
      auto* include = CST(*node = new_cst_include(&input->src_loc, input->src_loc.file_path), include);

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
          success = parse_statement_list(incl_input, CST(include->body, block)->nodes);
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

bool
parse_enum_decl(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Enum)
  {
    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == TokenKind_Id)
    {
      auto* enum_decl = CST(*node = new_cst_enum(&input->src_loc), enum_decl);

      enum_decl->id = new_cst_id(&input->src_loc, input->token.lexeme);

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
            member = new_cst_id(&input->src_loc, input->token.lexeme);
            append_list_elem(arena, enum_decl->members, member, ListKind_cst_node);

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

bool
parse_union_decl(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Union)
  {
    auto* union_decl = CST(*node = new_cst_union(&input->src_loc), union_decl);

    if((success = get_next_token(input)) && input->token.kind == TokenKind_Id)
    {
      union_decl->id = new_cst_id(&input->src_loc, input->token.lexeme);
      success = get_next_token(input);
    }

    if(success)
      success = parse_struct_member_list(input, union_decl->members);
  }
  return success;
}

bool
parse_struct_decl(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Struct)
  {
    auto* struct_decl = CST(*node = new_cst_struct(&input->src_loc), struct_decl);

    if((success = get_next_token(input)) && input->token.kind == TokenKind_Id)
    {
      struct_decl->id = new_cst_id(&input->src_loc, input->token.lexeme);
      success = get_next_token(input);
    }

    if(success)
      success = parse_struct_member_list(input, struct_decl->members);
  }
  return success;
}

bool
parse_struct_member_list(TokenStream* input, List* member_list)
{
  bool success = true;

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
          auto* var_decl = CST(member = new_cst_node(&input->src_loc, CstKind_var_decl), var_decl);

          var_decl->type_expr = type;

          if(input->token.kind == TokenKind_Id)
          {
            var_decl->id = new_cst_id(&input->src_loc, input->token.lexeme);
            success = get_next_token(input);
          }
          else if(type->kind == CstKind_struct_decl
                  || type->kind == CstKind_union_decl)
          {
            /* anonymous struct/union */
          }
          else
            success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));

          if(success)
          {
            append_list_elem(arena, member_list, member, ListKind_cst_node);
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

bool
parse_var_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = false;

  if(input->token.kind == TokenKind_Var)
    success = get_next_token(input) && parse_var_decl(input, node);
  return success;
}

bool
parse_return_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Return)
  {
    auto* return_stmt = CST(*node = new_cst_node(&input->src_loc, CstKind_return_stmt), return_stmt);
    success = get_next_token(input) && parse_expression(input, &return_stmt->expr);
  }

  return success;
}

bool
parse_label(TokenStream* input, CstNode* id, CstNode** node)
{
  *node = id;
  bool success = true;

  if(input->token.kind == TokenKind_Colon && (success = get_next_token(input)))
  {
    if(id->kind == CstKind_id)
    {
      auto* label = CST(*node = new_cst_node(&input->src_loc, CstKind_label), label);
      label->id = id;
    }
    else
      success = compile_error(&input->src_loc, "label identifier expected");
  }
  return success;
}

bool
parse_goto_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Goto)
  {
    auto* goto_stmt = CST(*node = new_cst_node(&input->src_loc, CstKind_goto_stmt), goto_stmt);

    if(!(success = get_next_token(input)))
      return success;

    if(input->token.kind == TokenKind_Id)
    {
      goto_stmt->id = new_cst_id(&input->src_loc, input->token.lexeme);
      success = get_next_token(input);
    }
    else
      success = compile_error(&input->src_loc, "identifier expected, actual `%s`", get_token_printstr(&input->token));
  }
  return success;
}

bool
parse_continue_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Continue)
  {
    *node = new_cst_node(&input->src_loc, CstKind_continue_stmt);
    success = get_next_token(input);
  }
  return success;
}

bool
parse_break_stmt(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

  if(input->token.kind == TokenKind_Break)
  {
    *node = new_cst_node(&input->src_loc, CstKind_break_stmt);
    success = get_next_token(input);
  }
  return success;
}

bool
parse_statement(TokenStream* input, CstNode** node)
{
  *node = 0;
  bool success = true;

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
    *node = new_cst_statement(&input->src_loc, stmt);
  }
  return success;
}

bool
parse(TokenStream* input, CstNode** node)
{
  bool success = true;

  auto* module = CST(*node = new_cst_module(&input->src_loc, input->src_loc.file_path), module);

  if((success = parse_statement_list(input, CST(module->body, block)->nodes))
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
                     get_cst_kind_printstr(node->kind), node->src_loc->file_path, node->src_loc->line_nr);
#else
    DEBUG_print_line(str, indent_level, "%s", get_cst_kind_printstr(node->kind));
#endif
    ++indent_level;

    if(node->kind == CstKind_module)
    {
      auto* module = CST(node, module);
      DEBUG_print_line(str, indent_level, "file_path: \"%s\"", module->file_path);
      DEBUG_print_cst_node(str, indent_level, module->body, "body");
    }
    else if(node->kind == CstKind_include)
    {
      auto* include = CST(node, include);
      DEBUG_print_line(str, indent_level, "file_path: \"%s\"", include->file_path);
      DEBUG_print_cst_node(str, indent_level, include->body, "body");
    }
    else if(node->kind == CstKind_proc)
    {
      auto* proc = CST(node, proc);
      DEBUG_print_cst_node(str, indent_level, proc->ret_type_expr, "ret_type");
      DEBUG_print_cst_node(str, indent_level, proc->id, "id");
      DEBUG_print_cst_node_list(str, indent_level, proc->args, "args");
      DEBUG_print_cst_node(str, indent_level, proc->body, "body");
    }
    else if(node->kind == CstKind_var_decl)
    {
      auto* var_decl = CST(node, var_decl);
      DEBUG_print_cst_node(str, indent_level, var_decl->type_expr, "type_id");
      DEBUG_print_cst_node(str, indent_level, (CstNode*)var_decl->id, "id");
      DEBUG_print_cst_node(str, indent_level, var_decl->init_expr, "init_expr");
    }
#if 0
    else if(node->kind == CstKind_VarOccur)
    {
      CstVarOccur* var_occur = (CstVarOccur*)node;
      DEBUG_print_cst_node(str, indent_level, var_occur->id, "id");
      DEBUG_print_line(str, indent_level, "decl_block_offset: %d", var_occur->decl_block_offset);
    }
#endif
    else if(node->kind == CstKind_id)
    {
      auto* id = CST(node, id);
      DEBUG_print_line(str, indent_level, "name: %s", id->name);
    }
    else if(node->kind == CstKind_block)
    {
      auto* block = CST(node, block);
      DEBUG_print_cst_node_list(str, indent_level, block->nodes, "nodes");
    }
    else if(node->kind == CstKind_bin_expr)
    {
      auto* bin_expr = CST(node, bin_expr);
      DEBUG_print_line(str, indent_level, "op: %s", get_cst_op_printstr(bin_expr->op));
      DEBUG_print_cst_node(str, indent_level, bin_expr->left_operand, "left_operand");
      DEBUG_print_cst_node(str, indent_level, bin_expr->right_operand, "right_operand");
    }
    else if(node->kind == CstKind_un_expr)
    {
      auto* un_expr = CST(node, un_expr);
      DEBUG_print_line(str, indent_level, "op: %s", get_cst_op_printstr(un_expr->op));
      DEBUG_print_cst_node(str, indent_level, un_expr->operand, "operand");
    }
    else if(node->kind == CstKind_stmt)
    {
      auto* stmt = CST(node, stmt);
      DEBUG_print_cst_node(str, indent_level, stmt->stmt, "stmt");
    }
    else if(node->kind == CstKind_if_stmt)
    {
      auto* if_stmt = CST(node, if_stmt);
      DEBUG_print_cst_node(str, indent_level, if_stmt->cond_expr, "cond_expr");
      DEBUG_print_cst_node(str, indent_level, if_stmt->body, "body");
      DEBUG_print_cst_node(str, indent_level, if_stmt->else_body, "else_body");
    }
    else if(node->kind == CstKind_return_stmt)
    {
      auto* return_stmt = CST(node, return_stmt);
      DEBUG_print_cst_node(str, indent_level, return_stmt->expr, "expr");
    }
    else if(node->kind == CstKind_lit)
    {
      auto* lit = CST(node, lit);
      DEBUG_print_line(str, indent_level, get_cst_literal_printstr(lit->kind));
      if(lit->kind == CstLiteralKind_int_val)
        DEBUG_print_line(str, indent_level, "int_val: %d", lit->int_val);
      else if(lit->kind == CstLiteralKind_float_val)
        DEBUG_print_line(str, indent_level, "float_val: %f", lit->float_val);
      else if(lit->kind == CstLiteralKind_bool_val)
        DEBUG_print_line(str, indent_level, "bool_val: %d", lit->bool_val);
      else if(lit->kind == CstLiteralKind_char_val)
      {
        char buf[3] = {0};
        print_char(buf, lit->char_val);
        DEBUG_print_line(str, indent_level, "char_val: '%s'", buf);
      }
      else if(lit->kind == CstLiteralKind_str)
        DEBUG_print_line(str, indent_level, "str: \"%s\"", lit->str);
      else
        assert(0);
    }
    else if(node->kind == CstKind_while_stmt)
    {
      auto* while_stmt = CST(node, while_stmt);
      DEBUG_print_cst_node(str, indent_level, while_stmt->cond_expr, "cond_expr");
      DEBUG_print_cst_node(str, indent_level, while_stmt->body, "body");
    }
    else if(node->kind == CstKind_for_stmt)
    {
      auto* for_stmt = CST(node, for_stmt);
      DEBUG_print_cst_node(str, indent_level, for_stmt->decl_expr, "decl_expr");
      DEBUG_print_cst_node(str, indent_level, for_stmt->cond_expr, "cond_expr");
      DEBUG_print_cst_node(str, indent_level, for_stmt->loop_expr, "loop_expr");
      DEBUG_print_cst_node(str, indent_level, for_stmt->body, "body");
    }
    else if(node->kind == CstKind_cast)
    {
      auto* cast = CST(node, cast);
      DEBUG_print_cst_node(str, indent_level, cast->type_expr, "type_expr");
      DEBUG_print_cst_node(str, indent_level, cast->expr, "expr");
    }
    else if(node->kind == CstKind_array)
    {
      auto* array = CST(node, array);
      DEBUG_print_cst_node(str, indent_level, array->type_expr, "type_expr");
      DEBUG_print_cst_node(str, indent_level, array->size_expr, "size_expr");
    }
    else if(node->kind == CstKind_pointer)
    {
      auto* ptr = CST(node, pointer);
      DEBUG_print_cst_node(str, indent_level, ptr->type_expr, "type_expr");
    }
    else if(node->kind == CstKind_call)
    {
      auto* call = CST(node, call);
      DEBUG_print_cst_node(str, indent_level, call->id, "id");
      DEBUG_print_cst_node_list(str, indent_level, call->args, "args");
    }
    else if(node->kind == CstKind_break_stmt
            || node->kind == CstKind_continue_stmt)
    {
      /* no extra info to print */
    }
    else if(node->kind == CstKind_struct_decl)
    {
      auto* struct_decl = CST(node, struct_decl);
      DEBUG_print_cst_node(str, indent_level, struct_decl->id, "id");
      DEBUG_print_cst_node_list(str, indent_level, struct_decl->members, "members");
    }
    else if(node->kind == CstKind_union_decl)
    {
      auto* union_decl = CST(node, union_decl);
      DEBUG_print_cst_node(str, indent_level, union_decl->id, "id");
      DEBUG_print_cst_node_list(str, indent_level, union_decl->members, "members");
    }
    else if(node->kind == CstKind_enum_decl)
    {
      auto* enum_decl = CST(node, enum_decl);
      DEBUG_print_cst_node(str, indent_level, enum_decl->id, "id");
      DEBUG_print_cst_node_list(str, indent_level, enum_decl->members, "members");
    }
    else if(node->kind == CstKind_init_list)
    {
      auto* init_list = CST(node, init_list);
      DEBUG_print_cst_node_list(str, indent_level, init_list->members, "members");
    }
    else if(node->kind == CstKind_goto_stmt)
    {
      auto* goto_stmt = CST(node, goto_stmt);
      DEBUG_print_cst_node(str, indent_level, goto_stmt->id, "id");
    }
    else if(node->kind == CstKind_label)
    {
      auto* label = CST(node, label);
      DEBUG_print_cst_node(str, indent_level, label->id, "id");
    }
    else if(node->kind == CstKind_hoc_new)
    {
      auto* hoc_new = CST(node, hoc_new);
      DEBUG_print_cst_node(str, indent_level, hoc_new->type_expr, "type_expr");
      DEBUG_print_cst_node(str, indent_level, hoc_new->count_expr, "count_expr");
    }
    else if(node->kind == CstKind_hoc_putc)
    {
      auto* hoc_putc = CST(node, hoc_putc);
      DEBUG_print_cst_node(str, indent_level, hoc_putc->expr, "expr");
    }
    else
      assert(0);
  }
}
