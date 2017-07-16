#include "hocc.h"

extern MemoryArena* arena;

local int last_label_id;

local void do_stmt(MemoryArena* arena, AstNode* ast);
local void do_block_stmts(MemoryArena* arena, List* stmt_list);

local void
make_unique_label(String* label)
{
  sprintf(label->head, "L%d", last_label_id++);
  int len = cstr_len(label->head);
  label->end = label->head + len;
  MemoryArena* arena = label->arena;
  arena->free = (uint8*)label->end + 1;
}

local int
compute_data_loc(int sp, List* areas)
{
  for(ListItem* list_item = areas->first;
      list_item;
      list_item = list_item->next)
  {
    auto data = (DataArea*)list_item->elem;
    data->loc = sp;
    assert(data->size > 0);
    sp += data->size;
  }
  return sp;
}

local void
fixup_data_loc(int fp, List* areas)
{
  for(ListItem* list_item = areas->first;
      list_item;
      list_item = list_item->next)
  {
    auto data = (DataArea*)list_item->elem;
    data->loc = data->loc - fp;
  }
}

local void
compute_activation_record_locations(List* pre_fp_data, List* post_fp_data)
{
  int fp = compute_data_loc(0, pre_fp_data);
  compute_data_loc(fp, post_fp_data);
  fixup_data_loc(fp, pre_fp_data);
  fixup_data_loc(fp, post_fp_data);
}

local void
do_call(MemoryArena* arena, AstNode* call_ast)
{
  assert(call_ast->kind == AstNodeKind_Call);
  use(call_ast, call);
  for(ListItem* list_item = call->args.first;
      list_item;
      list_item = list_item->next)
  {
    do_stmt(arena, (AstNode*)list_item->elem);
  }
}

local void
do_block(MemoryArena* arena, AstNode* block_ast)
{
  assert(block_ast->kind == AstNodeKind_Block);

  use(block_ast, block);
  List pre_fp_data = {0};
  list_init(&pre_fp_data);
  List post_fp_data = {0};
  list_init(&post_fp_data);

  /* access links */
  for(ListItem* list_item = block->nonlocals.first;
      list_item;
      list_item = list_item->next)
  {
    auto occur_ast = (AstNode*)list_item->elem;
    assert(occur_ast->kind == AstNodeKind_VarOccur);
    use(occur_ast, var_occur);

    List* links_list = &block->access_links;

    AccessLink* link = 0;
    for(ListItem* list_item = links_list->first;
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
      link->data.size = 1;
      list_append(arena, links_list, link);
      list_append(arena, &pre_fp_data, &link->data);
      block->links_size += link->data.size;
    }
    var_occur->link = link;
  }

  DataArea* old_fp = mem_push_struct(arena, DataArea);
  old_fp->size = 1;
  list_append(arena, &pre_fp_data, old_fp);

  /* locals*/
  for(ListItem* list_item = block->decl_vars.first;
      list_item;
      list_item = list_item->next)
  {
    auto ast = (AstNode*)list_item->elem;
    assert(ast->kind == AstNodeKind_VarDecl);
    use(ast, var_decl);

    list_append(arena, &post_fp_data, &var_decl->data);
    block->locals_size += var_decl->data.size;
  }
  compute_activation_record_locations(&pre_fp_data, &post_fp_data);
  do_block_stmts(arena, &block->node_list);
}

local void
do_while_stmt(MemoryArena* arena, AstNode* while_ast)
{
  assert(while_ast->kind == AstNodeKind_WhileStmt);
  use(while_ast, while_stmt);
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
    do_block(arena, while_stmt->body);
  else
    do_stmt(arena, while_stmt->body);
}

local void
do_if_stmt(MemoryArena* arena, AstNode* if_ast)
{
  assert(if_ast->kind == AstNodeKind_IfStmt);
  use(if_ast, if_stmt);
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
    do_block(arena, if_stmt->body);
  else
    do_stmt(arena, if_stmt->body);

  if(if_stmt->else_body)
  {
    AstNode* else_node = if_stmt->else_body;
    if(else_node->kind == AstNodeKind_Block)
      do_block(arena, else_node);
    else if(else_node->kind == AstNodeKind_IfStmt)
      do_if_stmt(arena, else_node);
    else
      do_stmt(arena, else_node);
  }
}

local void
do_bin_expr(MemoryArena* arena, AstNode* expr_ast)
{
  assert(expr_ast->kind == AstNodeKind_BinExpr);
  use(expr_ast, bin_expr);

  String label_id = {0};
  str_init(&label_id, arena);
  make_unique_label(&label_id);

  String label = {0};
  str_init(&label, arena);
  str_append(&label, label_id.head);
  str_append(&label, ".logic-end");
  bin_expr->label_end = label.head;

  do_stmt(arena, bin_expr->lhs);
  do_stmt(arena, bin_expr->rhs);
}

local void
do_unr_expr(MemoryArena* arena, AstNode* expr_ast)
{
  assert(expr_ast->kind == AstNodeKind_UnrExpr);
  do_stmt(arena, expr_ast->unr_expr.operand);
}

local void
do_stmt(MemoryArena* arena, AstNode* ast)
{
  if(ast->kind == AstNodeKind_BinExpr)
    do_bin_expr(arena, ast);
  else if(ast->kind == AstNodeKind_UnrExpr)
    do_unr_expr(arena, ast);
  else if(ast->kind == AstNodeKind_Call)
    do_call(arena, ast);
  else if(ast->kind == AstNodeKind_IfStmt)
    do_if_stmt(arena, ast);
  else if(ast->kind == AstNodeKind_WhileStmt)
    do_while_stmt(arena, ast);
  else if(ast->kind == AstNodeKind_ReturnStmt)
  {
    if(ast->ret_stmt.expr)
      do_stmt(arena, ast->ret_stmt.expr);
    /* else - it's OK */
  }
  else if(ast->kind == AstNodeKind_Cast)
    do_stmt(arena, ast->cast.expr);
  else if(ast->kind == AstNodeKind_VarOccur ||
          ast->kind == AstNodeKind_BreakStmt ||
          ast->kind == AstNodeKind_EmptyStmt ||
          ast->kind == AstNodeKind_Literal)
    ;
  else
    assert(false);
}

local void
do_block_stmts(MemoryArena* arena, List* stmt_list)
{
  for(ListItem* list_item = stmt_list->first;
      list_item;
      list_item = list_item->next)
  {
    do_stmt(arena, (AstNode*)list_item->elem);
  }
}

local void
do_proc(MemoryArena* arena, AstNode* proc_ast)
{
  assert(proc_ast->kind == AstNodeKind_Proc);
  use(proc_ast, proc);

  proc->label = proc->id->id.name;

  String label = {0};
  str_init(&label, arena);
  str_append(&label, proc->id->id.name);
  str_append(&label, ".proc-end");
  proc->label_end = label.head;

  List pre_fp_data = {0};
  list_init(&pre_fp_data);
  List post_fp_data = {0};
  list_init(&post_fp_data);

  auto ret_var = &proc->ret_var->var_decl;
  list_append(arena, &pre_fp_data, &ret_var->data);
  proc->ret_size = ret_var->data.size;

  /* formals */
  for(ListItem* list_item = proc->args.first;
      list_item;
      list_item = list_item->next)
  {
    auto ast = (AstNode*)list_item->elem;
    assert(ast->kind == AstNodeKind_VarDecl);
    use(ast, var_decl);

    list_append(arena, &pre_fp_data, &var_decl->data);
    proc->args_size += var_decl->data.size;
  }

  DataArea* ctrl_links = mem_push_struct(arena, DataArea);
  ctrl_links->size = 3; // fp,sp,ip
  list_append(arena, &pre_fp_data, ctrl_links);

  auto body_block = &proc->body->block;
  /* locals */
  for(ListItem* list_item = body_block->decl_vars.first;
      list_item;
      list_item = list_item->next)
  {
    auto ast = (AstNode*)list_item->elem;
    assert(ast->kind == AstNodeKind_VarDecl);
    use(ast, var_decl);

    list_append(arena, &post_fp_data, &var_decl->data);
    proc->locals_size += var_decl->data.size;
  }

  compute_activation_record_locations(&pre_fp_data, &post_fp_data);
  do_block_stmts(arena, &body_block->node_list);
}
 
bool32
build_runtime(AstNode* module_ast)
{
  assert(module_ast->kind == AstNodeKind_Module);
  bool32 success = true;
  use(module_ast, module);

  auto body_block = &module->body->block;
  for(ListItem* list_item = body_block->node_list.first;
      list_item;
      list_item = list_item->next)
  {
    auto stmt_ast = (AstNode*)list_item->elem;
    if(stmt_ast->kind == AstNodeKind_Proc)
      do_proc(arena, stmt_ast);
    else
      fail("not implemented");
  }

  Symbol* main_proc_sym = lookup_symbol("main", SymbolKind_Proc);
  if(main_proc_sym)
  {
    AstNode* call_ast = new_call(&module_ast->src_loc);
    use(call_ast, call);
    call->proc = module->main_proc;
    do_call(arena, call_ast);
    module->main_call = call_ast;
  }
  else
    success = error("main() not found");
  return success;
}

