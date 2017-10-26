#if 0
void rt_statement(AstNode* ast);
void rt_block_stmts(List* stmt_list);

void make_unique_label(String* label)
{
  sprintf(label->head, "L%d", last_label_id++);
  int len = cstr_len(label->head);
  label->end = label->head + len;
  MemoryArena* arena = label->arena;
  arena->free = (uint8*)label->end + 1;
}
#endif

int compute_data_loc(int sp, List* areas)
{
  for(ListItem* list_item = areas->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* area = ITEM(list_item, data_area);
    area->loc = sp;
    assert(area->size >= 0);
    sp += area->size;
  }
  return sp;
}

void fixup_data_loc(int fp, List* areas)
{
  for(ListItem* list_item = areas->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* area = ITEM(list_item, data_area);
    area->loc = area->loc - fp;
  }
}

void compute_area_locations(List* pre_fp_data, List* post_fp_data)
{
  int fp = compute_data_loc(0, pre_fp_data);
  compute_data_loc(fp, post_fp_data);
  fixup_data_loc(fp, pre_fp_data);
  fixup_data_loc(fp, post_fp_data);
}

#if 0
void rt_call(AstCall* call)
{
  List* args_list = &call->args->list;
  for(ListItem* list_item = args_list->first;
      list_item;
      list_item = list_item->next)
  {
    rt_statement((AstNode*)list_item->elem);
  }
}

void compute_locals_data_size(AstBlock* block, List* area_list)
{
  for(ListItem* list_item = block->local_decls.first;
      list_item;
      list_item = list_item->next)
  {
    AstVarDecl* var_decl = (AstVarDecl*)list_item->elem;
    var_decl->data.size = compute_type_width(var_decl->type);
    block->locals_size += var_decl->data.size;
    list_append(arena, area_list, &var_decl->data);
  }
}

void rt_block(AstBlock* block)
{
  List pre_fp_data = {0};
  list_init(&pre_fp_data);
  List post_fp_data = {0};
  list_init(&post_fp_data);

  /* access links */
  for(ListItem* list_item = block->nonlocal_occurs.first;
      list_item;
      list_item = list_item->next)
  {
    AstVarOccur* var_occur = (AstVarOccur*)list_item->elem;

    AccessLink* link = 0;
    for(ListItem* list_item = block->access_links.first;
        list_item;
        list_item = list_item->next)
    {
      link = (AccessLink*)list_item->elem;
      if(link->actv_rec_offset == var_occur->decl_block_offset)
        break;
      link = 0;
    }
    if(!link)
    {
      link = mem_push_struct(arena, AccessLink);
      link->actv_rec_offset = var_occur->decl_block_offset;
      link->data.size = 4;
      list_append(arena, &block->access_links, link);
      list_append(arena, &pre_fp_data, &link->data);
      block->links_size += link->data.size;
    }
    var_occur->link = link;
  }

  DataArea* old_fp = mem_push_struct(arena, DataArea);
  old_fp->size = 4;
  list_append(arena, &pre_fp_data, old_fp);

  compute_locals_data_size(block, &post_fp_data);
  compute_activation_record_locations(&pre_fp_data, &post_fp_data);
  rt_block_stmts(&block->node_list);
}

void rt_while_stmt(AstWhileStmt* while_stmt)
{
  rt_statement(while_stmt->cond_expr);

  {
    String* label_id = str_new(arena);
    make_unique_label(label_id);

    String* label = str_new(arena);
    str_append(label, label_id->head);
    str_append(label, ".while-expr");
    while_stmt->label_eval = str_cap(label);

    label = str_new(arena);
    str_append(label, label_id->head);
    str_append(label, ".while-break");
    while_stmt->label_break = str_cap(label);
  }

  if(while_stmt->body->kind == AstNodeKind_AstBlock)
    rt_block((AstBlock*)while_stmt->body);
  else
    rt_statement(while_stmt->body);
}

void rt_if_stmt(AstIfStmt* if_stmt)
{
  rt_statement(if_stmt->cond_expr);

  {
    String* label_id = str_new(arena);
    make_unique_label(label_id);

    String* label = str_new(arena);
    str_append(label, label_id->head);
    str_append(label, ".if-else");
    if_stmt->label_else = str_cap(label);

    str_init(label, arena);
    str_append(label, label_id->head);
    str_append(label, ".if-end");
    if_stmt->label_end = str_cap(label);
  }

  if(if_stmt->body->kind == AstNodeKind_AstBlock)
    rt_block((AstBlock*)if_stmt->body);
  else
    rt_statement(if_stmt->body);

  if(if_stmt->else_body)
  {
    AstNode* else_node = if_stmt->else_body;
    if(else_node->kind == AstNodeKind_AstBlock)
      rt_block((AstBlock*)else_node);
    else if(else_node->kind == AstNodeKind_AstIfStmt)
      rt_if_stmt((AstIfStmt*)else_node);
    else
      rt_statement(else_node);
  }
}

void rt_bin_expr(AstBinExpr* bin_expr)
{
  String* label_id = str_new(arena);
  make_unique_label(label_id);

  String* label = str_new(arena);
  str_append(label, label_id->head);
  str_append(label, ".logic-end");
  bin_expr->label_end = str_cap(label);

  rt_statement(bin_expr->left_operand);
  rt_statement(bin_expr->right_operand);
}

void rt_unr_expr(AstUnaryExpr* unr_expr)
{
  rt_statement(unr_expr->operand);
}

void rt_statement(AstNode* ast)
{
  if(ast->kind == AstNodeKind_AstBinExpr)
    rt_bin_expr((AstBinExpr*)ast);
  else if(ast->kind == AstNodeKind_AstUnaryExpr)
    rt_unr_expr((AstUnaryExpr*)ast);
  else if(ast->kind == AstNodeKind_AstCall)
    rt_call((AstCall*)ast);
  else if(ast->kind == AstNodeKind_AstIfStmt)
    rt_if_stmt((AstIfStmt*)ast);
  else if(ast->kind == AstNodeKind_AstWhileStmt)
    rt_while_stmt((AstWhileStmt*)ast);
  else if(ast->kind == AstNodeKind_AstReturnStmt)
  {
    AstReturnStmt* ret_stmt = (AstReturnStmt*)ast;
    if(ret_stmt->assign_expr)
      rt_statement((AstNode*)ret_stmt->assign_expr);
  }
  else if(ast->kind == AstNodeKind_AstCast)
  {
    AstCast* cast = (AstCast*)ast;
    rt_statement(cast->expr);
  }
  else if(ast->kind == AstNodeKind_AstVarDecl)
  {
    AstVarDecl* var_decl = (AstVarDecl*)ast;
    if(var_decl->assign_expr)
      rt_statement((AstNode*)var_decl->assign_expr);
  }
  else if(ast->kind == AstNodeKind_AstPutc)
  {
    AstPutc* putc_ast = (AstPutc*)ast;
    rt_statement(putc_ast->expr);
  }
  else if(ast->kind == AstNodeKind_AstBlock)
  {
    rt_block((AstBlock*)ast);
  }
  else if(ast->kind == AstNodeKind_AstVarOccur
          || ast->kind == AstNodeKind_AstBreakStmt
          || ast->kind == AstNodeKind_AstContinueStmt
          || ast->kind == AstNodeKind_AstLiteral
          || ast->kind == AstNodeKind_AstString
          || ast->kind == AstNodeKind_AstNew
          || ast->kind == AstNodeKind_AstEmptyStmt)
    ;//OK
  else
    assert(0);
}

void rt_block_stmts(List* stmt_list)
{
  for(ListItem* list_item = stmt_list->first;
      list_item;
      list_item = list_item->next)
  {
    rt_statement((AstNode*)list_item->elem);
  }
}

void rt_proc(AstProc* proc)
{
  proc->label = proc->id->name;

  String* label = str_new(arena);
  str_append(label, proc->id->name);
  str_append(label, ".proc-end");
  proc->label_end = str_cap(label);

  List pre_fp_data = {0};
  list_init(&pre_fp_data);
  List post_fp_data = {0};
  list_init(&post_fp_data);

  AstVarDecl* ret_var = (AstVarDecl*)proc->ret_var;
  ret_var->data.size = compute_type_width(ret_var->type);
  proc->ret_size = ret_var->data.size;
  if(proc->ret_size > 0)
    list_append(arena, &pre_fp_data, &ret_var->data);
  else
  {
    assert(proc->ret_size == 0);
    assert(proc->ret_var->type == basic_type_void);
  }

  /* formals */
  List* args_list = &proc->args->list;
  for(ListItem* list_item = args_list->first;
      list_item;
      list_item = list_item->next)
  {
    AstVarDecl* var_decl = (AstVarDecl*)list_item->elem;
    var_decl->data.size = compute_type_width(var_decl->type);
    proc->args_size += var_decl->data.size;
    list_append(arena, &pre_fp_data, &var_decl->data);
  }

  AstBlock* block = (AstBlock*)proc->body;

  DataArea* ctrl_links = mem_push_struct(arena, DataArea);
  ctrl_links->size = 3*4; // fp,sp,ip
  list_append(arena, &pre_fp_data, ctrl_links);

  compute_locals_data_size(block, &post_fp_data);
  compute_activation_record_locations(&pre_fp_data, &post_fp_data);
  rt_block_stmts(&block->node_list);
}
 
void build_runtime(AstNode* node)
{
  for(ListItem* list_item = module->proc_defs.first;
      list_item;
      list_item = list_item->next)
  {
    AstProc* proc = (AstProc*)list_item->elem;
    assert(proc->kind == AstNodeKind_AstProc);
    rt_proc(proc);
  }

  List pre_fp_data = {0};
  list_init(&pre_fp_data);
  List post_fp_data = {0};
  list_init(&post_fp_data);

  DataArea* base_offset = mem_push_struct(arena, DataArea);
  base_offset->size = 1; // null ptr
  list_append(arena, &post_fp_data, base_offset);

  compute_locals_data_size(module_block, &post_fp_data);
  compute_activation_record_locations(&pre_fp_data, &post_fp_data);
  rt_block_stmts(&module_block->node_list);
}
#endif

void compute_locals_area_size(Scope* scope, List* local_data_areas)
{
  for(Symbol* symbol = scope->last_symbol;
      symbol;
      symbol = symbol->prev_symbol)
  {
    if(symbol->kind == Symbol_var_decl)
    {
      AstNode* var_decl = symbol->ast_node;
      DataArea* data_area = ATTR(var_decl, data_area, data_area) = mem_push_struct(arena, DataArea);
      data_area->size = ATTR(var_decl, type, type)->width;
      scope->data_area_size += data_area->size;
      append_list_elem(local_data_areas, data_area, List_data_area);
    }
  }
}

void build_runtime()
{
  for(ListItem* list_item = symbol_table->scopes->first;
      list_item;
      list_item = list_item->next)
  {
    Scope* scope = ITEM(list_item, scope);
    if(scope->kind == Scope_global)
    {
      List* pre_fp_areas = new_list(arena, List_data_area);
      List* post_fp_areas = new_list(arena, List_data_area);

      DataArea* base_offset = mem_push_struct(arena, DataArea);
      base_offset->size = 1; // null ptr
      scope->data_area_size = base_offset->size;
      append_list_elem(post_fp_areas, base_offset, List_data_area);

      compute_area_locations(pre_fp_areas, post_fp_areas);
    }
    else if(scope->kind == Scope_module)
    {
      List* pre_fp_areas = new_list(arena, List_data_area);
      List* post_fp_areas = new_list(arena, List_data_area);

      compute_locals_area_size(scope, post_fp_areas);
      compute_area_locations(pre_fp_areas, post_fp_areas);
    }
    else if(scope->kind == Scope_proc)
    {
      List* pre_fp_areas = new_list(arena, List_data_area);
      List* post_fp_areas = new_list(arena, List_data_area);

      AstNode* proc = scope->ast_node;
      AstNode* ret_var = ATTR(proc, ast_node, ret_var);
      DataArea* ret_area = ATTR(ret_var, data_area, data_area) = mem_push_struct(arena, DataArea);
      ret_area->kind = DataArea_object;
      ret_area->size = ATTR(ret_var, type, type)->width;
      scope->data_area_size += ret_area->size;
      append_list_elem(pre_fp_areas, ret_area, List_data_area);

      for(ListItem* list_item = ATTR(proc, list, formal_args)->first;
          list_item;
          list_item = list_item->next)
      {
        AstNode* arg = ITEM(list_item, ast_node);
        DataArea* arg_area = ATTR(arg, data_area, data_area) = mem_push_struct(arena, DataArea);
        arg_area->kind = DataArea_object;
        arg_area->size = ATTR(arg, type, type)->width;
        scope->data_area_size += arg_area->size;
        append_list_elem(pre_fp_areas, arg_area, List_data_area);
      }

      DataArea* ctrl_links = mem_push_struct(arena, DataArea);
      ctrl_links->kind = DataArea_object;
      ctrl_links->size = 3*4; // fp, sp, ip
      append_list_elem(pre_fp_areas, ctrl_links, List_data_area);

      compute_locals_area_size(scope, post_fp_areas);
      compute_area_locations(pre_fp_areas, post_fp_areas);
    }
    else if(scope->kind == Scope_block)
    {
      List* pre_fp_areas = new_list(arena, List_data_area);
      List* post_fp_areas = new_list(arena, List_data_area);
      List* access_links = new_list(arena, List_data_area);
      int access_links_size = 0;

      for(Symbol* symbol = scope->last_symbol;
          symbol;
          symbol = symbol->prev_symbol)
      {
        if(symbol->kind == Symbol_var_occur)
        {
          AstNode* var_occur = symbol->ast_node;
          
          int decl_scope_depth = symbol->var_occur.decl_scope_depth;
          if(decl_scope_depth > 0)
          {
            // non-local
            DataArea* link = 0;
            for(ListItem* list_item = access_links->first;
                list_item;
                list_item = list_item->next)
            {
              link = ITEM(list_item, data_area);
              if(link->decl_scope_depth == symbol->var_occur.decl_scope_depth)
              {
                break;
              }
              link = 0;
            }
            if(!link)
            {
              link = mem_push_struct(arena, DataArea);
              link->kind = DataArea_link;
              link->decl_scope_depth = symbol->var_occur.decl_scope_depth;
              link->size = 4; // size of an int
              append_list_elem(access_links, link, List_data_area);
              append_list_elem(pre_fp_areas, link, List_data_area);
              access_links_size += link->size;
            }
            ATTR(var_occur, data_area, data_area) = link;
          }
          else if(decl_scope_depth == 0)
          {
            // local
            AstNode* var_decl = ATTR(var_occur, ast_node, var_decl);
            ATTR(var_occur, data_area, data_area) = ATTR(var_decl, data_area, data_area);
          }
          else
            assert(0);
        }
      }

      DataArea* old_fp = mem_push_struct(arena, DataArea);
      old_fp->size = 4; // size of an int
      append_list_elem(pre_fp_areas, old_fp, List_data_area);

      compute_locals_area_size(scope, post_fp_areas);
      compute_area_locations(pre_fp_areas, post_fp_areas);
    }
    int x = 0; x++;
  }
  int x = 0; x++;
}


