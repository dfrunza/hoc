#include "hocc.h"

extern MemoryArena* arena;

extern Type* basic_type_void;

local int last_label_id;

local void do_stmt(AstNode* ast);
local void do_block_stmts(List* stmt_list);

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
get_type_size(Type* type)
{
  int size = 0;
  if(type->kind == TypeKind_Basic)
    size = type->basic.size;
  else if(type->kind == TypeKind_Pointer)
    size = 1;
  else
    fail("not implemented");
  return size;
}

local int
compute_data_loc(int sp, List* areas)
{
  for(ListItem* list_item = areas->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* data = (DataArea*)list_item->elem;
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
    DataArea* data = (DataArea*)list_item->elem;
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
do_call(AstCall* call)
{
  for(ListItem* list_item = call->args.first;
      list_item;
      list_item = list_item->next)
  {
    do_stmt((AstNode*)list_item->elem);
  }
}

local void
do_block(AstBlock* block)
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

  /* local decls */
  for(ListItem* list_item = block->decl_vars.first;
      list_item;
      list_item = list_item->next)
  {
    AstVarDecl* var_decl = (AstVarDecl*)list_item->elem;
    var_decl->data.size = get_type_size(var_decl->type);
    block->locals_size += var_decl->data.size;
    list_append(arena, &post_fp_data, &var_decl->data);
  }

  compute_activation_record_locations(&pre_fp_data, &post_fp_data);
  do_block_stmts(&block->node_list);
}

local void
do_while_stmt(AstWhileStmt* while_stmt)
{
  do_stmt(while_stmt->cond_expr);

  {
    /* labels */
    String label_id = {0};
    str_init(&label_id, arena);
    make_unique_label(&label_id);

    String label = {0};
    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".while-expr");
    while_stmt->label_eval = str_cap(&label);

    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".while-break");
    while_stmt->label_break = str_cap(&label);
  }

  if(while_stmt->body->kind == AstNodeKind_Block)
    do_block((AstBlock*)while_stmt->body);
  else
    do_stmt(while_stmt->body);
}

local void
do_if_stmt(AstIfStmt* if_stmt)
{
  do_stmt(if_stmt->cond_expr);

  {
    /* labels */
    String label_id = {0};
    str_init(&label_id, arena);
    make_unique_label(&label_id);

    String label = {0};
    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".if-else");
    if_stmt->label_else = str_cap(&label);

    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".if-end");
    if_stmt->label_end = str_cap(&label);
  }

  if(if_stmt->body->kind == AstNodeKind_Block)
    do_block((AstBlock*)if_stmt->body);
  else
    do_stmt(if_stmt->body);

  if(if_stmt->else_body)
  {
    AstNode* else_node = if_stmt->else_body;
    if(else_node->kind == AstNodeKind_Block)
      do_block((AstBlock*)else_node);
    else if(else_node->kind == AstNodeKind_IfStmt)
      do_if_stmt((AstIfStmt*)else_node);
    else
      do_stmt(else_node);
  }
}

local void
do_bin_expr(AstBinExpr* bin_expr)
{
  String label_id = {0};
  str_init(&label_id, arena);
  make_unique_label(&label_id);

  String label = {0};
  str_init(&label, arena);
  str_append(&label, label_id.head);
  str_append(&label, ".logic-end");
  bin_expr->label_end = str_cap(&label);

  do_stmt(bin_expr->left_operand);
  do_stmt(bin_expr->right_operand);
}

local void
do_unr_expr(AstUnrExpr* unr_expr)
{
  do_stmt(unr_expr->operand);
}

local void
do_stmt(AstNode* ast)
{
  if(ast->kind == AstNodeKind_BinExpr)
    do_bin_expr((AstBinExpr*)ast);
  else if(ast->kind == AstNodeKind_UnrExpr)
    do_unr_expr((AstUnrExpr*)ast);
  else if(ast->kind == AstNodeKind_Call)
    do_call((AstCall*)ast);
  else if(ast->kind == AstNodeKind_IfStmt)
    do_if_stmt((AstIfStmt*)ast);
  else if(ast->kind == AstNodeKind_WhileStmt)
    do_while_stmt((AstWhileStmt*)ast);
  else if(ast->kind == AstNodeKind_ReturnStmt)
  {
    AstReturnStmt* ret_stmt = (AstReturnStmt*)ast;
    if(ret_stmt->assign_expr)
      do_stmt((AstNode*)ret_stmt->assign_expr);
  }
  else if(ast->kind == AstNodeKind_Cast)
  {
    AstCast* cast = (AstCast*)ast;
    do_stmt(cast->expr);
  }
  else if(ast->kind == AstNodeKind_VarOccur ||
          ast->kind == AstNodeKind_BreakStmt ||
          ast->kind == AstNodeKind_EmptyStmt ||
          ast->kind == AstNodeKind_Literal)
    ; /* no-op */
  else if(ast->kind == AstNodeKind_VarDecl)
  {
    AstVarDecl* var_decl = (AstVarDecl*)ast;
    if(var_decl->assign_expr)
      do_stmt((AstNode*)var_decl->assign_expr);
  }
  else
    assert(false);
}

local void
do_block_stmts(List* stmt_list)
{
  for(ListItem* list_item = stmt_list->first;
      list_item;
      list_item = list_item->next)
  {
    do_stmt((AstNode*)list_item->elem);
  }
}

local void
do_proc(AstProc* proc)
{
  proc->label = proc->id->name;

  String label = {0};
  str_init(&label, arena);
  str_append(&label, proc->id->name);
  str_append(&label, ".proc-end");
  proc->label_end = str_cap(&label);

  List pre_fp_data = {0};
  list_init(&pre_fp_data);
  List post_fp_data = {0};
  list_init(&post_fp_data);

  AstVarDecl* ret_var = (AstVarDecl*)proc->ret_var;
  ret_var->data.size = get_type_size(ret_var->type);
  proc->ret_size = ret_var->data.size;
  if(proc->ret_size > 0)
    list_append(arena, &pre_fp_data, &ret_var->data);
  else
  {
    assert(proc->ret_size == 0);
    assert(proc->ret_var->type == basic_type_void);
  }

  /* formals */
  for(ListItem* list_item = proc->args.first;
      list_item;
      list_item = list_item->next)
  {
    AstVarDecl* var_decl = (AstVarDecl*)list_item->elem;
    var_decl->data.size = get_type_size(var_decl->type);
    proc->args_size += var_decl->data.size;
    list_append(arena, &pre_fp_data, &var_decl->data);
  }

  DataArea* ctrl_links = mem_push_struct(arena, DataArea);
  ctrl_links->size = 3; // fp,sp,ip
  list_append(arena, &pre_fp_data, ctrl_links);

  AstBlock* body_block = (AstBlock*)proc->body;
  /* local decls */
  for(ListItem* list_item = body_block->decl_vars.first;
      list_item;
      list_item = list_item->next)
  {
    AstVarDecl* var_decl = (AstVarDecl*)list_item->elem;
    var_decl->data.size = get_type_size(var_decl->type);
    proc->locals_size += var_decl->data.size;
    list_append(arena, &post_fp_data, &var_decl->data);
  }

  compute_activation_record_locations(&pre_fp_data, &post_fp_data);
  do_block_stmts(&body_block->node_list);
}
 
bool32
build_runtime(AstModule* module)
{
  bool32 success = true;

  AstBlock* body_block = (AstBlock*)module->body;
  for(ListItem* list_item = body_block->node_list.first;
      list_item;
      list_item = list_item->next)
  {
    AstNode* ast = (AstNode*)list_item->elem;
    if(ast->kind == AstNodeKind_Proc)
    {
      AstProc* proc = (AstProc*)ast;
      //FIXME: Remove proc decls from the AST
      if(!proc->is_decl)
        do_proc(proc);
    }
    else
      fail("not implemented");
  }

  do_stmt((AstNode*)module->main_stmt);

  return success;
}

