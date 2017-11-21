bool gen_code(String* text, AstNode* node);
void gen_load_rvalue(String* text, AstNode* node);

void gen_labels(AstNode* node)
{
  if(node->kind == eAstNode_module)
  {
    gen_labels(ATTR(node, ast_node, body));
  }
  else if(node->kind == eAstNode_block)
  {
    for(ListItem* list_item = ATTR(node, list, nodes)->first;
        list_item;
        list_item = list_item->next)
    {
      gen_labels(ITEM(list_item, ast_node));
    }
  }
  else if(node->kind == eAstNode_proc_decl)
  {
    Scope* proc_scope = ATTR(node, scope, scope);
    Scope* encl_proc_scope = find_scope(proc_scope->encl_scope, eScope_proc);
    if(encl_proc_scope)
    {
      AstNode* encl_proc = encl_proc_scope->ast_node; assert(encl_proc->kind == eAstNode_proc_decl);
      String qual_name; str_init(&qual_name, arena);
      str_append(&qual_name, ATTR(encl_proc, str_val, name));
      str_append(&qual_name, "$");
      str_append(&qual_name, ATTR(node, str_val, name));
      ATTR(node, str_val, name) = str_cap(&qual_name);
    }
    gen_labels(ATTR(node, ast_node, body));
  }
  else if(node->kind == eAstNode_proc_occur)
  {
    for(ListItem* list_item = ATTR(node, list, actual_args)->first;
        list_item;
        list_item = list_item->next)
    {
      gen_labels(ITEM(list_item, ast_node));
    }

    AstNode* proc_decl = ATTR(node, ast_node, proc_decl);
    ATTR(node, str_val, name) = ATTR(proc_decl, str_val, name);
  }
  else if(node->kind == eAstNode_stmt)
  {
    gen_labels(ATTR(node, ast_node, stmt));
  }
  else if(node->kind == eAstNode_bin_expr)
  {
    char* label_id = make_unique_label();

    String label; str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$logic_end");
    ATTR(node, str_val, label_end) = str_cap(&label);

    gen_labels(ATTR(node, ast_node, left_operand));
    gen_labels(ATTR(node, ast_node, right_operand));
  }
  else if(node->kind == eAstNode_un_expr)
  {
    gen_labels(ATTR(node, ast_node, operand));
  }
  else if(node->kind == eAstNode_if_stmt)
  {
    gen_labels(ATTR(node, ast_node, cond_expr));

    char* label_id = make_unique_label();

    String label; str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$if_else");
    ATTR(node, str_val, label_else) = str_cap(&label);

    str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$if_end");
    ATTR(node, str_val, label_end) = str_cap(&label);

    gen_labels(ATTR(node, ast_node, body));

    AstNode* else_body = ATTR(node, ast_node, else_body);
    if(else_body)
    {
      gen_labels(else_body);
    }
  }
  else if(node->kind == eAstNode_while_stmt)
  {
    gen_labels(ATTR(node, ast_node, cond_expr));

    char* label_id = make_unique_label();

    String label; str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$while_eval");
    ATTR(node, str_val, label_eval) = str_cap(&label);

    str_init(&label, arena);
    str_append(&label, label_id);
    str_append(&label, "$while_break");
    ATTR(node, str_val, label_break) = str_cap(&label);

    AstNode* body = ATTR(node, ast_node, body);
    if(body)
    {
      gen_labels(body);
    }
  }
  else if(node->kind == eAstNode_ret_stmt)
  {
    AstNode* ret_expr = ATTR(node, ast_node, ret_expr);
    if(ret_expr)
    {
      gen_labels(ret_expr);
    }
  }
  else
    ;//skip
}

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

void gen_load_rvalue(String* text, AstNode* node)
{
  assert(node->gen == eAstGen_gen1);

  if(node->kind == eAstNode_var_occur)
  {
    Type* type = ATTR(node, type, eval_type);
    gen_load_lvalue(text, node);
    //emit_instr_int32(instr_list, eOpcode_LOAD, type->width);
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

      //emit_instr_int32(instr_list, eOpcode_STORE, type->width);
    }
  }
  return success;
}

