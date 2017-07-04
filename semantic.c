#include "hocc.h"

extern MemoryArena* arena;

extern Type* basic_type_bool;
extern Type* basic_type_int;
extern Type* basic_type_char;
extern Type* basic_type_float;
extern Type* basic_type_void;

internal SymbolTable* symtab = 0;

internal AstNode*
new_var_occur(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_VarOccur;
  node->src_loc = *src_loc;
  return node;
}

#if 0
internal int
block_find_owner(AstBlock* block, AstNodeKind kind, AstNode** result)
{
  int depth = 0;
  AstNode* owner = 0;
  while(block)
  {
    owner = block->owner;
    if(owner->kind == kind)
      break;
    depth++;
    block = block->enclosing_block;
  }
  *result = owner;
  return depth;
}
#endif

internal Symbol*
lookup_symbol(char* name, SymbolKind kind)
{
  Symbol* result = 0;

  Symbol* symbol = symtab->symbol;
  while(symbol)
  {
    if(symbol->kind == kind && cstr_match(symbol->name, name))
    {
      result = symbol;
      break;
    }
    symbol = symbol->next_symbol;
  }
  return result;
}

internal Symbol*
add_symbol(char* name, SymbolKind kind)
{
  Symbol* symbol = mem_push_struct(arena, Symbol, 1);
  symbol->kind = kind;
  symbol->name = name;
  symbol->block_id = symtab->scope_id;
  symbol->nesting_depth = symtab->nesting_depth;
  symbol->next_symbol = symtab->symbol;
  symtab->symbol = symbol;
  return symbol;
}

internal Symbol*
add_builtin_type(char* name, Type* type)
{
  assert(type->kind == TypeKind_Basic);
  Symbol* symbol = add_symbol(name, SymbolKind_Type);
  symbol->type = type;
  return symbol;
}

internal void
register_builtin_ids()
{
  add_builtin_type("bool", basic_type_bool);
  add_builtin_type("int", basic_type_int);
  add_builtin_type("char", basic_type_char);
  add_builtin_type("float", basic_type_float);
  add_builtin_type("void", basic_type_void);
}

internal bool
register_new_id(char* name, AstNode* node, SymbolKind symkind)
{
  bool success = true;

  Symbol* id_sym = lookup_symbol(name, symkind);
  if(id_sym)
    success = compile_error(&node->src_loc, __FILE__, __LINE__, "Symbol re-declaration `%s`", name);
  else
  {
    id_sym = add_symbol(name, symkind);
    id_sym->node = node;
  }
  return success;
}

internal bool
scope_begin(AstNode* node)
{
  assert(node->kind == AstNodeKind_Block);
  AstBlock* block = &node->block;

  int scope_id = ++symtab->last_scope_id;
  symtab->scope_id = scope_id;
  block->block_id = scope_id;
  block->encl_block = symtab->active_blocks[symtab->nesting_depth];

  int nesting_depth = ++symtab->nesting_depth;
  if(nesting_depth < sizeof_array(symtab->active_scopes))
  {
    symtab->active_scopes[nesting_depth] = scope_id;
    symtab->active_blocks[nesting_depth] = node;
    block->nesting_depth = nesting_depth;
  }
  else
  {
    compile_error(&node->src_loc, __FILE__, __LINE__,
                  "Maximum scope nesting depth has been reached: %d", sizeof_array(symtab->active_scopes));
    return false;
  }

  return true;
}

internal void
scope_end()
{
  int nesting_depth = --symtab->nesting_depth;
  int scope_id = symtab->active_scopes[nesting_depth];
  assert(scope_id >= 0);
  symtab->scope_id = scope_id;

  Symbol* symbol = symtab->symbol;
  while(symbol && symbol->block_id > symtab->scope_id)
    symbol = symbol->next_symbol;
  symtab->symbol = symbol;
}

internal bool do_block(AstNode*, AstNode*);
internal bool do_bin_expr(AstNode*, AstNode*);
internal bool do_operand(AstNode*, AstNode*);

internal void
do_include_stmt(List* include_list, List* module_list, ListItem* module_list_item)
{
  for(ListItem* list_item = include_list->first;
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = list_item->elem;
    if(node->kind == AstNodeKind_IncludeStmt)
    {
      AstIncludeStmt* include_node = &node->include_stmt;
      AstBlock* incl_block = &include_node->body->block;
      do_include_stmt(&incl_block->node_list, include_list, list_item);
    }
  }
  list_replace_at(include_list, module_list, module_list_item);

  mem_zero_struct(include_list, List);
  list_init(include_list);
}

internal bool
do_var_decl(AstNode* block_node, AstNode* var_node)
{
  assert(block_node->kind == AstNodeKind_Block);
  assert(var_node->kind == AstNodeKind_VarDecl);
  bool success = true;

  AstVarDecl* var_decl = &var_node->var_decl;
  AstId* id = &var_decl->id->id;
  if(success = register_new_id(id->name, var_node, SymbolKind_Var))
  {
    var_decl->decl_block = block_node;
    AstBlock* block = &block_node->block;
    list_append(arena, &block->decl_vars, var_node);
    if(var_decl->init_expr)
    {
      AstNode* assign_node = new_bin_expr(&var_node->src_loc);
      AstBinExpr* bin_expr = &assign_node->bin_expr;
      bin_expr->op = AstOpKind_Assign;
      bin_expr->lhs = new_id(&var_node->src_loc, id->name);;
      bin_expr->rhs = var_decl->init_expr;

      if(success = do_bin_expr(block_node, assign_node))
        list_append(arena, &block->stmts, assign_node);
    }
  }
  return success;
}

internal bool
do_bin_expr(AstNode* block_node, AstNode* expr_node)
{
  assert(block_node->kind == AstNodeKind_Block);
  assert(expr_node->kind == AstNodeKind_BinExpr);
  bool success = true;

  AstBinExpr* bin_expr = &expr_node->bin_expr;
  success = do_operand(block_node, bin_expr->lhs) && do_operand(block_node, bin_expr->rhs);
  return success;
}

internal bool
do_unr_expr(AstNode* block_node, AstNode* expr_node)
{
  assert(block_node->kind == AstNodeKind_Block);
  assert(expr_node->kind == AstNodeKind_UnrExpr);
  bool success = true;

  AstUnrExpr* unr_expr = &expr_node->unr_expr;
  success = do_operand(block_node, unr_expr->operand);
  return success;
}

internal bool
do_operand(AstNode* block_node, AstNode* operand)
{
  assert(block_node->kind == AstNodeKind_Block);
  bool success = true;

  if(operand->kind == AstNodeKind_Id)
  {
    AstId* id = &operand->id;
    Symbol* id_sym = lookup_symbol(id->name, SymbolKind_Var);
    if(id_sym)
    {
      AstNode* decl_node = id_sym->node;
      assert(decl_node->kind == AstNodeKind_VarDecl); // ...I think
      // transmute operand to VarOccur
      operand->kind = AstNodeKind_VarOccur;
      AstVarOccur* var_occur = &operand->var_occur;
      var_occur->name = id_sym->name;
      var_occur->var_decl = decl_node;

      AstVarDecl* var_decl = &decl_node->var_decl;
      AstNode* decl_block = var_decl->decl_block;
      var_occur->decl_block = decl_block;
      var_occur->occur_block = block_node;
      var_occur->decl_block_offset = block_node->block.nesting_depth - decl_block->block.nesting_depth;

      AstBlock* block = &block_node->block;
      if(var_occur->decl_block_offset > 0)
        list_append(arena, &block->nonlocals, operand);
      else if(var_occur->decl_block_offset == 0)
        list_append(arena, &block->locals, operand);
      else
        assert(false);
    }
    else
      success = compile_error(&operand->src_loc, __FILE__, __LINE__, "Unknown identifier `%s`", id->name);
  }
  else if(operand->kind == AstNodeKind_BinExpr)
    success = do_bin_expr(block_node, operand);
  else if(operand->kind == AstNodeKind_UnrExpr)
    success = do_unr_expr(block_node, operand);
  else if(operand->kind == AstNodeKind_Literal)
  { /* do nothing */ }
  else
    printf("TODO: %s\n", get_ast_kind_printstr(operand->kind));
  return success;
}

internal bool
do_proc_formal_args(AstNode* block_node, List* formal_args)
{
  assert(block_node->kind == AstNodeKind_Block);
  bool success = true;

  for(ListItem* list_item = formal_args->first;
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = list_item->elem;
    assert(node->kind == AstNodeKind_VarDecl);
    AstVarDecl* var_decl = &node->var_decl;
    if(var_decl->id)
      do_var_decl(block_node, node);
    else
      success = compile_error(&node->src_loc, __FILE__, __LINE__,
                              "Missing identifier: %s", get_ast_kind_printstr(node->kind));
    assert(!var_decl->init_expr); /* enforced by parser */
  }
  return success;
}

internal bool
do_procedure(AstNode* node)
{
  assert(node->kind == AstNodeKind_Proc);
  bool success = true;

  AstProc* proc = &node->proc;
  if(success = register_new_id(proc->id->id.name, node, SymbolKind_Proc))
  {
    if(proc->body)
    {
      success = scope_begin(proc->body);
      if(!success) goto end;

      success = do_proc_formal_args(proc->body, &proc->formal_args);
      if(!success) goto end;

      success = do_block(node, proc->body);
      if(!success) goto end;

      scope_end();
    }
  }

end:
  return success;
}

internal bool
do_block(AstNode* owner_node, AstNode* block_node)
{
  assert(block_node->kind == AstNodeKind_Block);
  bool success = true;

  AstBlock* block = &block_node->block;
  block->owner = owner_node;
  scope_begin(block_node);

  if(owner_node->kind == AstNodeKind_Module)
  {
    for(ListItem* list_item = block->node_list.first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* stmt_node = list_item->elem;
      if(stmt_node->kind == AstNodeKind_VarDecl)
        do_var_decl(block_node, stmt_node);
      else if(stmt_node->kind == AstNodeKind_Proc)
        do_procedure(stmt_node);
      else if(stmt_node->kind == AstNodeKind_Label ||
              stmt_node->kind == AstNodeKind_Call ||
              stmt_node->kind == AstNodeKind_Id ||
              stmt_node->kind == AstNodeKind_Literal ||
              stmt_node->kind == AstNodeKind_BinExpr ||
              stmt_node->kind == AstNodeKind_UnrExpr)
      {
        success = compile_error(&stmt_node->src_loc, __FILE__, __LINE__,
                                "Statement not allowed here: %s", get_ast_kind_printstr(stmt_node->kind));
        if(!success) goto end;
      }
      else if(stmt_node->kind == AstNodeKind_Struct ||
              stmt_node->kind == AstNodeKind_Union ||
              stmt_node->kind == AstNodeKind_Enum)
      {
        printf("TODO\n");
      }
      else
        assert(false);
    }
  }
  else if(owner_node->kind == AstNodeKind_Proc)
  {
    for(ListItem* list_item = block->node_list.first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* stmt_node = list_item->elem;
      if(stmt_node->kind == AstNodeKind_VarDecl)
        do_var_decl(block_node, stmt_node);
      else if(stmt_node->kind == AstNodeKind_BinExpr)
        do_bin_expr(block_node, stmt_node);
      else if(stmt_node->kind == AstNodeKind_WhileStmt ||
              stmt_node->kind == AstNodeKind_ForStmt ||
              stmt_node->kind == AstNodeKind_IfStmt ||
              stmt_node->kind == AstNodeKind_ReturnStmt ||
              stmt_node->kind == AstNodeKind_BreakStmt ||
              stmt_node->kind == AstNodeKind_ContinueStmt ||
              stmt_node->kind == AstNodeKind_GotoStmt ||
              stmt_node->kind == AstNodeKind_Label ||
              stmt_node->kind == AstNodeKind_Call ||
              stmt_node->kind == AstNodeKind_UnrExpr)
      {
        printf("TODO: %s\n", get_ast_kind_printstr(stmt_node->kind));
      }
      else
        assert(false);
    }
  }
  scope_end();
end:
  return success;
}

internal bool
do_module(AstNode* node)
{
  assert(node->kind == AstNodeKind_Module);
  bool success = true;

  AstModule* module = &node->module;
  AstBlock* block = &module->body->block;

  // process the includes
  for(ListItem* list_item = block->node_list.first;
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = list_item->elem;
    if(node->kind == AstNodeKind_IncludeStmt)
    {
      AstIncludeStmt* include_node = &node->include_stmt;
      AstBlock* incl_block = &include_node->body->block;
      do_include_stmt(&incl_block->node_list, &block->node_list, list_item);
    }
  }

  success = do_block(node, module->body);
  return success;
}

bool
semantic_analysis(AstNode* node)
{
  init_types();

  symtab = mem_push_struct(arena, SymbolTable, 1);
  register_builtin_ids(symtab);

  bool success = true;
  assert(node->kind == AstNodeKind_Module);
  success = do_module(node);

  return success;
}


