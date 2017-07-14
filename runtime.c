void
make_unique_label(String* label)
{
  sprintf(label->head, "L%d", last_label_id++);
  int len = cstr_len(label->head);
  label->end = label->head + len;
  MemoryArena* arena = label->arena;
  arena->free = (uint8*)label->end + 1;
}

int
compute_data_loc(int sp, List* areas)
{
  for(ListItem* list_item = list_first_item(areas);
      list_item;
      list_item = list_item->next)
  {
    DataArea* data = list_item->elem;
    data->loc = sp;
    assert(data->size > 0);
    sp += data->size;
  }
  return sp;
}

void
fixup_data_loc(int fp, List* areas)
{
  for(ListItem* list_item = list_first_item(areas);
      list_item;
      list_item = list_item->next)
  {
    DataArea* data = list_item->elem;
    data->loc = data->loc - fp;
  }
}

void
compute_activation_record_locations(List* pre_fp_data, List* post_fp_data)
{
  int fp = compute_data_loc(0, pre_fp_data);
  compute_data_loc(fp, post_fp_data);
  fixup_data_loc(fp, pre_fp_data);
  fixup_data_loc(fp, post_fp_data);
}

void
do_call(MemoryArena* arena, AstCall* call)
{
  for(ListItem* list_item = list_first_item(&call->actual_args);
      list_item;
      list_item = list_item->next)
  {
    do_stmt(arena, list_item->elem);
  }
}

void
do_print_stmt(MemoryArena* arena, AstPrintStmt* print_stmt)
{
  if(print_stmt->expr)
    do_stmt(arena, print_stmt->expr);
}

void
do_block(MemoryArena* arena, AstBlock* block)
{
  List pre_fp_data = {0};
  list_init(&pre_fp_data);
  List post_fp_data = {0};
  list_init(&post_fp_data);

  /* access links */
  for(ListItem* list_item = list_first_item(&block->nonlocal_occurs);
      list_item;
      list_item = list_item->next)
  {
    AstNode* occur_node = list_item->elem;
    assert(occur_node->kind == AstNodeKind_VarOccur);
    AstVarOccur* var_occur = &occur_node->var_occur;

    List* links_list = &block->access_links;

    AccessLink* link = 0;
    for(ListItem* list_item = list_first_item(links_list);
        list_item;
        list_item = list_item->next)
    {
      link = list_item->elem;
      if(link->actv_rec_offset == var_occur->decl_block_offset)
        break;
      link = 0;
    }
    if(!link)
    {
      link = mem_push_struct(arena, AccessLink, 1);
      link->actv_rec_offset = var_occur->decl_block_offset;
      link->data.size = 1;
      list_append(arena, links_list, link);
      list_append(arena, &pre_fp_data, &link->data);
      block->links_size += link->data.size;
    }
    var_occur->link = link;
  }

  DataArea* old_fp = mem_push_struct(arena, DataArea, 1);
  old_fp->size = 1;
  list_append(arena, &pre_fp_data, old_fp);

  /* locals*/
  for(ListItem* list_item = list_first_item(&block->decl_vars);
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = list_item->elem;
    assert(node->kind == AstNodeKind_VarDecl);
    AstVarDecl* local = &node->var_decl;

    list_append(arena, &post_fp_data, &local->data);
    block->locals_size += local->data.size;
  }
  compute_activation_record_locations(&pre_fp_data, &post_fp_data);
  do_block(arena, &block->stmt_list);
}

void
do_while_stmt(MemoryArena* arena, AstWhileStmt* while_stmt)
{
  do_stmt(arena, while_stmt->cond_expr);

  {
    /* labels */
    String label_id = {0};
    str_init(&label_id, arena);
    make_unique_label(&label_id);

    String label = {0};
    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".while-expr");
    while_stmt->label_eval = label.head;

    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".while-break");
    while_stmt->label_break = label.head;
  }

  if(while_stmt->body->kind == AstNodeKind_Block)
    do_block(arena, &while_stmt->body->block);
  else
    do_stmt(arena, while_stmt->body);
}

void
do_if_stmt(MemoryArena* arena, AstIfStmt* if_stmt)
{
  do_stmt(arena, if_stmt->cond_expr);

  {
    /* labels */
    String label_id = {0};
    str_init(&label_id, arena);
    make_unique_label(&label_id);

    String label = {0};
    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".if-else");
    if_stmt->label_else = label.head;

    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".if-end");
    if_stmt->label_end = label.head;
  }

  if(if_stmt->body->kind == AstNodeKind_Block)
    do_block(arena, &if_stmt->body->block);
  else
    do_stmt(arena, if_stmt->body);

  if(if_stmt->else_body)
  {
    AstNode* else_node = if_stmt->else_body;
    if(else_node->kind == AstNodeKind_Block)
      do_block(arena, &else_node->block);
    else if(else_node->kind == AstNodeKind_IfStmt)
      do_if_stmt(arena, &else_node->if_stmt);
    else
      do_stmt(arena, else_node);
  }
}

void
do_bin_expr(MemoryArena* arena, AstBinExpr* bin_expr)
{
  String label_id = {0};
  str_init(&label_id, arena);
  make_unique_label(&label_id);

  String label = {0};
  str_init(&label, arena);
  str_append(&label, label_id.head);
  str_append(&label, ".logic-end");
  bin_expr->label_end = label.head;

  do_stmt(arena, bin_expr->left_operand);
  do_stmt(arena, bin_expr->right_operand);
}

void
do_unr_expr(MemoryArena* arena, AstUnrExpr* unr_expr)
{
  do_stmt(arena, unr_expr->operand);
}

void
do_stmt(MemoryArena* arena, AstNode* node)
{
  if(node->kind == AstNodeKind_BinExpr)
    do_bin_expr(arena, &node->bin_expr);
  else if(node->kind == AstNodeKind_UnrExpr)
    do_unr_expr(arena, &node->unr_expr);
  else if(node->kind == AstNodeKind_Call)
    do_call(arena, &node->call);
  else if(node->kind == AstNodeKind_IfStmt)
    do_if_stmt(arena, &node->if_stmt);
  else if(node->kind == AstNodeKind_WhileStmt)
    do_while_stmt(arena, &node->while_stmt);
  else if(node->kind == AstNodeKind_PrintStmt)
    do_print_stmt(arena, &node->print_stmt);
  else if(node->kind == AstNodeKind_ReturnStmt)
  {
    if(node->ret_stmt.ret_expr)
      do_stmt(arena, node->ret_stmt.ret_expr);
  }
  else if(node->kind == AstNodeKind_Cast)
    do_stmt(arena, node->cast.expr);
  else if(node->kind == AstNodeKind_VarOccur ||
          node->kind == AstNodeKind_BreakStmt ||
          node->kind == AstNodeKind_EmptyStmt ||
          node->kind == AstNodeKind_Literal)
    ;
  else
    assert(false);
}

void
do_block(MemoryArena* arena, List* stmt_list)
{
  for(ListItem* list_item = list_first_item(stmt_list);
      list_item;
      list_item = list_item->next)
  {
    do_stmt(arena, list_item->elem);
  }
}

void
do_proc(MemoryArena* arena, AstProc* proc)
{
  proc->label = proc->name;

  String label = {0};
  str_init(&label, arena);
  str_append(&label, proc->name);
  str_append(&label, ".proc-end");
  proc->label_end = label.head;

  List pre_fp_data = {0};
  list_init(&pre_fp_data);
  List post_fp_data = {0};
  list_init(&post_fp_data);

  AstVarDecl* ret_var = &proc->ret_var;
  list_append(arena, &pre_fp_data, &ret_var->data);
  proc->ret_size = ret_var->data.size;

  /* formals */
  for(ListItem* list_item = list_first_item(&proc->formal_args);
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = list_item->elem;
    assert(node->kind == AstNodeKind_VarDecl);
    AstVarDecl* formal = &node->var_decl;

    list_append(arena, &pre_fp_data, &formal->data);
    proc->args_size += formal->data.size;
  }

  DataArea* ctrl_links = mem_push_struct(arena, DataArea, 1);
  ctrl_links->size = 3; // fp,sp,ip
  list_append(arena, &pre_fp_data, ctrl_links);

  AstBlock* body_block = &proc->body->block;
  /* locals */
  for(ListItem* list_item = list_first_item(&body_block->decl_vars);
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = list_item->elem;
    assert(node->kind == AstNodeKind_VarDecl);
    AstVarDecl* local = &node->var_decl;

    list_append(arena, &post_fp_data, &local->data);
    proc->locals_size += local->data.size;
  }
  compute_activation_record_locations(&pre_fp_data, &post_fp_data);
  do_block(arena, &body_block->stmt_list);
}
 
void
build_runtime(MemoryArena* arena, SymbolTable* symbol_table, AstNode* module_node)
{
  assert(module_node->kind == AstNodeKind_Module);
  AstModule* module = &module_node->module;

  for(ListItem* list_item = list_first_item(&module->proc_list);
      list_item;
      list_item = list_item->next)
  {
    AstNode* proc_node = list_item->elem;
    assert(proc_node->kind == AstNodeKind_Proc);
    AstProc* proc = &proc_node->proc;

    do_proc(arena, proc);
    if(cstr_match(proc->name, "main"))
    {
      module->main_proc = proc_node;
    }
  }

  if(module->main_proc)
  {
    AstNode* call_node = ast_new_call(arena, &module_node->src_loc);
    AstCall* call = &call_node->call;
    call->proc = module->main_proc;
    do_call(arena, call);
    module->main_call = call_node;
  }
}

