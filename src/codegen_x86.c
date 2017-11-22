bool gen_code(String* text, AstNode* node);
void gen_load_rvalue(String* text, AstNode* node);

int printf_line(String* text, char* fline, ...)
{
  int text_len = 0;
  static char strbuf[128] = {0};
  va_list args;

  va_start(args, fline);
  text_len = h_vsprintf(strbuf, fline, args);
  va_end(args);

  str_append(text, strbuf);
  str_append(text, "\n");
  text_len++;

  return text_len;
}

void emit_enter(String* text, int locals_alloc)
{
  printf_line(text, "push ebp");
  printf_line(text, "mov ebp, esp");
  printf_line(text, "sub esp, %d", locals_alloc);
}

void emit_leave(String* text)
{
  printf_line(text, "mov esp, ebp");
  printf_line(text, "pop ebp");
}

void emit_halt(String* text)
{
  printf_line(text, "INVOKE ExitProcess,0");
}

void gen_load_lvalue(String* text, AstNode* node)
{
  assert(node->gen == eAstGen_gen1);

  if(node->kind == eAstNode_var_occur)
  {
    Symbol* occur_sym = ATTR(node, symbol, occur_sym);
    Symbol* decl_sym = occur_sym->decl;
    Scope* scope = occur_sym->scope;

    int decl_scope_offset = occur_sym->nesting_depth - decl_sym->nesting_depth;
    if(decl_scope_offset > 0)
    {
      // Non-local
      assert(scope->link_area.loc < 0);
      printf_line(text, "push ebp");
      printf_line(text, "add esp, %d", scope->link_area.loc);

      for(int i = decl_scope_offset; i > 0; i--)
      {
        //emit_instr_int32(instr_list, eOpcode_LOAD, link->size);
      }

      // The FP is offset relative to the Access Link
      printf_line(text, "add esp, %d", scope->ctrl_area.size + scope->link_area.size);
    }
    else
    {
      printf_line(text, "push ebp");
    }

    printf_line(text, "add esp, %d", decl_sym->data_area->loc);
  }
}

// REP STOSB
// REP MOVSB
// REP SCASB
void gen_load_rvalue(String* text, AstNode* node)
{
  assert(node->gen == eAstGen_gen1);

  if(node->kind == eAstNode_var_occur)
  {
    Type* type = ATTR(node, type, eval_type);
    gen_load_lvalue(text, node);

    // LOAD
    printf_line(text, "mov ecx, %d", type->width);
    printf_line(text, "mov esi, [esp]");
    printf_line(text, "mov edi, esp");
    printf_line(text, "rep movsb");
  }
  else if(node->kind == eAstNode_lit)
  {
    eLiteral kind = ATTR(node, lit_kind, lit_kind);
    if(kind == eLiteral_int_val)
    {
      printf_line(text, "push %d", ATTR(node, int_val, int_val));
    }
    else if(kind == eLiteral_bool_val)
    {
      printf_line(text, "push %d", ATTR(node, bool_val, bool_val));
    }
    else if(kind == eLiteral_float_val)
    {
      //FIXME
      printf_line(text, "push %d", ATTR(node, float_val, float_val));
    }
    else if(kind == eLiteral_char_val)
    {
      //FIXME
      printf_line(text, "push %d", ATTR(node, char_val, char_val));
    }
    else
      assert(0);
  }
}

bool gen_code(String* text, AstNode* node)
{
  assert(node->gen == eAstGen_gen1);
  bool success = true;

  if(node->kind == eAstNode_module)
  {
    AstNode* body = ATTR(node, ast_node, body);
    Scope* scope = ATTR(body, scope, scope);

    emit_enter(text, scope->local_area.size);

    for(ListItem* list_item = ATTR(body, list, stmts)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* stmt = ITEM(list_item, ast_node); assert(stmt->kind == eAstNode_stmt);
      gen_code(text, stmt);
    }

    emit_leave(text);
    emit_halt(text);

    for(ListItem* list_item = ATTR(body, list, procs)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* proc = ITEM(list_item, ast_node);
      assert(proc->kind == eAstNode_proc_decl);
      gen_code(text, proc);
    }
  }
  else if(node->kind == eAstNode_proc_decl)
  {
    Scope* scope = ATTR(node, scope, scope);

    printf_line(text, "%s:", ATTR(node, str_val, name));
    printf_line(text, "sub esp, %d", scope->local_area.size);

    AstNode* body = ATTR(node, ast_node, body);
    for(ListItem* list_item = ATTR(body, list, stmts)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* stmt = ITEM(list_item, ast_node);
      gen_code(text, stmt);
    }

    printf_line(text, "ret");

    for(ListItem* list_item = ATTR(body, list, procs)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* proc = ITEM(list_item, ast_node); assert(proc->kind == eAstNode_proc_decl);
      gen_code(text, proc);
    }
  }
  else if(node->kind == eAstNode_block)
  {
    Scope* scope = ATTR(node, scope, scope);

    printf_line(text, "push ebp");
    printf_line(text, "add esp, %d", scope->link_area.loc);
    emit_enter(text, scope->local_area.size);

    for(ListItem* list_item = ATTR(node, list, stmts)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* stmt = ITEM(list_item, ast_node);
      gen_code(text, stmt);
    }

    printf_line(text, "add esp, %d", scope->link_area.loc);

    for(ListItem* list_item = ATTR(node, list, procs)->first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* proc = ITEM(list_item, ast_node); assert(proc->kind == eAstNode_proc_decl);
      gen_code(text, proc);
    }
  }
  else if(node->kind == eAstNode_stmt)
  {
    AstNode* actual_stmt = ATTR(node, ast_node, stmt);
    Type* type = ATTR(actual_stmt, type, eval_type);
    gen_code(text, actual_stmt);
    printf_line(text, "add esp, %d", type->width);
  }
  else if(node->kind == eAstNode_bin_expr)
  {
    AstNode* left_operand = ATTR(node, ast_node, left_operand);
    AstNode* right_operand = ATTR(node, ast_node, right_operand);
    Type* type = ATTR(node, type, eval_type);
    Type* left_ty = ATTR(left_operand, type, eval_type);
    Type* right_ty = ATTR(right_operand, type, eval_type);
    eOperator bin_op = ATTR(node, op_kind, op_kind);

    if(bin_op == eOperator_assign)
    {
      gen_load_rvalue(text, right_operand);
      gen_load_lvalue(text, left_operand);

      // STORE
      printf_line(text, "mov ecx, %d", type->width);
      printf_line(text, "mov esi, esp");
      printf_line(text, "add esi, 4");
      printf_line(text, "mov edi, [esp]");
      printf_line(text, "rep movsb");
    }
  }
  return success;
}

