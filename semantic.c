#include "hocc.h"

extern MemoryArena* arena;
extern MemoryArena* sym_arena;

extern Type* basic_type_bool;
extern Type* basic_type_int;
extern Type* basic_type_char;
extern Type* basic_type_float;
extern Type* basic_type_void;

internal SymbolTable* symtab = 0;
internal int last_block_id = 0;
internal int tempvar_id = 0;

internal bool do_block(AstNode*, AstNode*, AstNode*, AstNode*);
internal bool do_expression(AstNode*, AstNode*);

internal AstNode*
make_tempvar_id(SourceLocation* src_loc, char* label)
{
  String str = {0};
  str_init(&str, arena);
  str_printf(&str, "$%s%d", label, tempvar_id++);
  return new_id(src_loc, str.head);
}

internal AstNode*
new_var_occur(SourceLocation* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode);
  node->kind = AstNodeKind_VarOccur;
  node->src_loc = *src_loc;
  return node;
}

internal Symbol*
lookup_symbol(char* name, SymbolKind kind)
{
  Symbol* result = 0;

  Symbol* symbol = symtab->curr_symbol;
  while(symbol)
  {
    if(symbol->kind == kind && cstr_match(symbol->name, name))
    {
      result = symbol;
      break;
    }
    symbol = symbol->prev_symbol;
  }
  return result;
}

internal AstNode*
get_module_block()
{
  return symtab->active_blocks[1];
}

internal Symbol*
find_last_symbol_in_block(AstNode* block)
{
  Symbol* symbol = symtab->curr_symbol;
  while(symbol && (symbol->block_id > block->block.block_id))
    symbol = symbol->prev_symbol;
  return symbol;
}

internal Symbol*
add_symbol(char* name, SymbolKind kind)
{
  Symbol* symbol = mem_push_struct(sym_arena, Symbol);
  symbol->kind = kind;
  symbol->name = name;
  symbol->block_id = symtab->block_id;
  symbol->nesting_depth = symtab->nesting_depth;
  symbol->prev_symbol = symtab->curr_symbol;
  symtab->curr_symbol = symbol;
  symtab->sym_count++;
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
register_builtin_types()
{
  add_builtin_type("bool", basic_type_bool);
  add_builtin_type("int", basic_type_int);
  add_builtin_type("char", basic_type_char);
  add_builtin_type("float", basic_type_float);
  add_builtin_type("void", basic_type_void);
}

internal Symbol*
register_type_sym(AstNode* id)
{
  assert(id->kind == AstNodeKind_Id);
  Symbol* sym = lookup_symbol(id->id.name, SymbolKind_Type);
  if(!sym)
  {
    sym = add_symbol(id->id.name, SymbolKind_Type);
    sym->type = new_typevar();
  }
  id->type = sym->type;
  return sym;
}

internal Symbol*
register_new_id(AstNode* id, SymbolKind symkind)
{
  assert(id->kind == AstNodeKind_Id);
  Symbol* sym = lookup_symbol(id->id.name, symkind);
  if(sym && (sym->block_id == symtab->block_id))
  {
    compile_error(&id->src_loc, "Symbol redeclaration `%s`...", id->id.name);
    compile_error(&id->src_loc, "...see previous declaration of `%s`", id->id.name);
    return 0;
  }
  else
    sym = add_symbol(id->id.name, symkind);
  return sym;
}

internal bool
scope_begin(AstNode* block)
{
  assert(block->kind == AstNodeKind_Block);
  symtab->block_id =++last_block_id; 
  block->block.block_id = symtab->block_id;
  block->block.encl_block = symtab->curr_block;

  ++symtab->nesting_depth;
  if(symtab->nesting_depth < sizeof_array(symtab->active_blocks))
  {
    block->block.nesting_depth = symtab->nesting_depth;
    symtab->active_blocks[symtab->nesting_depth] = block;
    symtab->curr_block = block;
  }
  else
  {
    compile_error(&block->src_loc, "Maximum scope nesting depth has been reached: %d", sizeof_array(symtab->active_blocks));
    return false;
  }

  return true;
}

internal void
scope_end()
{
  assert(symtab->curr_block == symtab->active_blocks[symtab->nesting_depth]);

  --symtab->nesting_depth;
  if(symtab->nesting_depth > 0)
  {
    AstNode* block = symtab->active_blocks[symtab->nesting_depth];
    assert(block->kind == AstNodeKind_Block);
    assert(block->block.block_id > 0);
    symtab->curr_block = block;

#if 1
    Symbol* symbol = symtab->curr_symbol;
    while(symbol && (symbol->block_id > symtab->block_id))
      symbol = symbol->prev_symbol;
#else
    symtab->curr_symbol = find_last_symbol_in_block(symtab->curr_block);
#endif
  }
  else
    assert(symtab->nesting_depth == 0);
}

internal void
do_include_stmt(List* include_list, List* module_list, ListItem* module_list_item)
{
  for(ListItem* list_item = include_list->first;
      list_item;
      list_item = list_item->next)
  {
    AstNode* stmt = list_item->elem;
    if(stmt->kind == AstNodeKind_IncludeStmt)
    {
      AstNode* incl_block = stmt->incl_stmt.body;
      do_include_stmt(&incl_block->block.node_list, include_list, list_item);
    }
  }
  list_replace_at(include_list, module_list, module_list_item);

  mem_zero_struct(include_list, List);
  list_init(include_list);
}

internal bool
do_var_decl(AstNode* block, AstNode* var)
{
  assert(block->kind == AstNodeKind_Block);
  assert(var->kind == AstNodeKind_VarDecl);
  bool success = true;

  var->type = register_type_sym(var->var_decl.type)->type;
  AstNode* id = var->var_decl.id;
  Symbol* sym = register_new_id(id, SymbolKind_Var);
  if(success = to_bool(sym))
  {
    sym->node = var;
    var->var_decl.decl_block = block;
    list_append(arena, &block->block.decl_vars, var);
    if(var->var_decl.init_expr)
    {
      AstNode* assign_node = new_bin_expr(&var->src_loc);
      AstBinExpr* bin_expr = &assign_node->bin_expr;
      bin_expr->op = AstOpKind_Assign;
      bin_expr->lhs = new_id(&var->src_loc, id->id.name);;
      bin_expr->rhs = var->var_decl.init_expr;

      if(success = do_expression(block, assign_node))
        list_append(arena, &block->block.stmts, assign_node);
    }
  }
  return success;
}

internal bool
do_expression(AstNode* block, AstNode* expr_node)
{
  assert(block->kind == AstNodeKind_Block);
  bool success = true;

  if(expr_node->kind == AstNodeKind_BinExpr)
    success = do_expression(block, expr_node->bin_expr.lhs)
      && do_expression(block, expr_node->bin_expr.rhs);
  else if(expr_node->kind == AstNodeKind_UnrExpr)
    success = do_expression(block, expr_node->unr_expr.operand);
  else if(expr_node->kind == AstNodeKind_Id)
  {
    Symbol* sym = lookup_symbol(expr_node->id.name, SymbolKind_Var);
    if(sym)
    {
      AstNode* decl_node = sym->node;
      assert(decl_node->kind == AstNodeKind_VarDecl); // ...I think
      // transmute operand to VarOccur
      expr_node->kind = AstNodeKind_VarOccur;
      expr_node->var_occur.name = sym->name;
      expr_node->var_occur.var_decl = decl_node;

      AstVarDecl* var_decl = &decl_node->var_decl;
      AstNode* decl_block = var_decl->decl_block;
      expr_node->var_occur.decl_block = decl_block;
      expr_node->var_occur.occur_block = block;
      expr_node->var_occur.decl_block_offset = block->block.nesting_depth - decl_block->block.nesting_depth;

      if(expr_node->var_occur.decl_block_offset > 0)
        list_append(arena, &block->block.nonlocals, expr_node);
      else if(expr_node->var_occur.decl_block_offset == 0)
        list_append(arena, &block->block.locals, expr_node);
      else
        assert(false);
    }
    else
      success = compile_error(&expr_node->src_loc, "Unknown identifier `%s`", expr_node->id.name);
  }
  else if(expr_node->kind == AstNodeKind_Literal)
  { /* nothing to do */ }
  else if(expr_node->kind == AstNodeKind_Call)
  {
#if 1
    printf("TODO: %s():%d\n", expr_node->call.id->id.name, expr_node->src_loc.line_nr);
#else
    assert(!"Not implemented");
#endif
  }
  else
  {
#if 1
    printf("TODO: %s\n", get_ast_kind_printstr(expr_node->kind));
#else
    assert(!"Not implemented");
#endif
  }

  return success;
}

internal bool
do_proc_formal_args(AstNode* block, List* formal_args)
{
  assert(block->kind == AstNodeKind_Block);
  bool success = true;

  for(ListItem* list_item = formal_args->first;
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = list_item->elem;
    assert(node->kind == AstNodeKind_VarDecl);
    AstVarDecl* var_decl = &node->var_decl;
    if(var_decl->id)
      success = do_var_decl(block, node);
    else
      success = compile_error(&node->src_loc, "Missing identifier: %s", get_ast_kind_printstr(node->kind));
    assert(!var_decl->init_expr); /* enforced by parser */
  }
  return success;
}

internal bool
do_proc_ret_var(AstNode* block, AstNode* proc)
{
  assert(proc->kind == AstNodeKind_Proc);
  bool success = true;
  proc->proc.ret_var = new_var_decl(&proc->src_loc);
  AstNode* var_decl = proc->proc.ret_var;
  var_decl->var_decl.id = make_tempvar_id(&proc->src_loc, "ret");
  var_decl->var_decl.type = proc->proc.ret_type;
  success = do_var_decl(block, proc->proc.ret_var);
  return success;
}

internal Type*
make_type_of_node_list(List* node_list)
{
  Type* type = basic_type_void;
  ListItem* first_item = node_list->first;
  if(first_item)
  {
    type = ((AstNode*)first_item->elem)->type;
    for(ListItem* list_item = first_item->next;
        list_item;
        list_item = list_item->next)
    {
      AstNode* node = list_item->elem;
      type = new_product_type(type, node->type);
    }
  }
  return type;
}

internal bool
do_proc_decl(AstNode* proc)
{
  assert(proc->kind == AstNodeKind_Proc);
  bool success = true;

  if(success = scope_begin(proc->proc.body))
  {
    AstNode* ret_type = proc->proc.ret_type;
    Symbol* type_sym = lookup_symbol(ret_type->id.name, SymbolKind_Type);
    if(!type_sym)
    {
      type_sym = add_symbol(ret_type->id.name, SymbolKind_Type);
      type_sym->type = new_typevar();
    }
    ret_type->type = type_sym->type;

    if(success = do_proc_formal_args(proc->proc.body, &proc->proc.formal_args)
       && do_proc_ret_var(proc->proc.body, proc))
    {
      proc->type = new_proc_type(make_type_of_node_list(&proc->proc.formal_args), proc->proc.ret_var->type);
      success = do_block(proc, 0, proc, proc->proc.body);
    }
    scope_end();
  }

  if(success)
  {
    Symbol* proc_sym = lookup_symbol(proc->proc.id->id.name, SymbolKind_Proc);
    if(proc_sym)
    {
      AstNode* registered_proc = proc_sym->node;
      assert(registered_proc && registered_proc->kind == AstNodeKind_Proc);

      if(registered_proc->proc.is_decl && proc->proc.is_decl)
      {
        if(!type_unification(registered_proc->type, proc->type))
        {
          success = compile_error(&proc->src_loc, "Inconsistent proc type...");
          compile_error(&registered_proc->src_loc, "...see previous decl");
        }
      }
      else if(!registered_proc->proc.is_decl && !proc->proc.is_decl)
      {
        success = compile_error(&proc->src_loc, "Proc redefinition...");
        compile_error(&registered_proc->src_loc, "...see previous def");
      }
      else if(registered_proc->proc.is_decl && !proc->proc.is_decl)
      {
        if(type_unification(registered_proc->type, proc->type))
        {
          proc_sym->node = proc;
          proc_sym->type = proc->type;
        }
        else
        {
          success = compile_error(&proc->src_loc, "Inconsistent proc type...");
          compile_error(&registered_proc->src_loc, "...see decl");
        }
      }
      /* else fall-thru */
    }
    else if(success = to_bool(proc_sym = register_new_id(proc->proc.id, SymbolKind_Proc)))
    {
      proc_sym->node = proc;
      proc_sym->type = proc->type;
    }
  }

  return success;
}

internal bool
do_statement(AstNode* proc, AstNode* block, AstNode* loop, AstNode* stmt)
{
  assert(block->kind == AstNodeKind_Block);
  bool success = true;

  if(stmt->kind == AstNodeKind_VarDecl)
    success = do_var_decl(block, stmt);
  else if(stmt->kind == AstNodeKind_BinExpr)
    success = do_expression(block, stmt);
  else if(stmt->kind == AstNodeKind_UnrExpr)
  {
    if(stmt->unr_expr.op == AstOpKind_PostDecrement
       || stmt->unr_expr.op == AstOpKind_PreDecrement
       || stmt->unr_expr.op == AstOpKind_PostIncrement
       || stmt->unr_expr.op == AstOpKind_PreIncrement)
      success = do_expression(block, stmt);
    else
      success = compile_error(&stmt->src_loc, "Unexpected statement %s", get_ast_kind_printstr(stmt->kind));
  }
  else if(stmt->kind == AstNodeKind_WhileStmt)
  {
    if(success = do_expression(block, stmt->while_stmt.cond_expr))
    {
      if(stmt->while_stmt.body->kind == AstNodeKind_Block)
      {
        if(success = scope_begin(stmt->while_stmt.body))
        {
          success = do_block(proc, stmt, stmt, stmt->while_stmt.body);
          scope_end();
        }
      }
      else
        success = do_statement(proc, block, stmt, stmt->while_stmt.body);
    }
  }
  else if(stmt->kind == AstNodeKind_ForStmt)
  {
    if(success = scope_begin(stmt->for_stmt.body))
    {
      success = (stmt->for_stmt.decl_expr ? do_var_decl(stmt->for_stmt.body, stmt->for_stmt.decl_expr) : 1);
      if(success)
      {
        success = do_block(proc, stmt, stmt, stmt->for_stmt.body) // will set the block->owner field, so do it first
          && (stmt->for_stmt.cond_expr ? do_expression(stmt->for_stmt.body, stmt->for_stmt.cond_expr) : 1)
          && (stmt->for_stmt.loop_expr ? do_expression(stmt->for_stmt.body, stmt->for_stmt.loop_expr) : 1);
      }
      scope_end();
    }
  }
  else if(stmt->kind == AstNodeKind_IfStmt)
  {
    if(success = do_expression(block, stmt->if_stmt.cond_expr))
    {
      if(stmt->if_stmt.body->kind == AstNodeKind_Block)
      {
        if(success = scope_begin(stmt->if_stmt.body))
        {
          success = do_block(proc, loop, stmt, stmt->if_stmt.body);
          scope_end();
        }
      }
      else
        success = do_statement(proc, block, loop, stmt->if_stmt.body);

      if(stmt->if_stmt.else_body)
      {
        if(stmt->if_stmt.else_body->kind == AstNodeKind_Block)
        {
          if(success = scope_begin(stmt->if_stmt.else_body))
          {
            success = do_block(proc, stmt, loop, stmt->if_stmt.else_body);
            scope_end();
          }
        }
        else
          success = do_statement(proc, block, loop, stmt->if_stmt.else_body);
      }
    }
  }
  else if(stmt->kind == AstNodeKind_ReturnStmt)
  {
    if(proc)
    {
      stmt->ret_stmt.proc = proc;
      stmt->ret_stmt.nesting_depth = block->block.nesting_depth - proc->proc.body->block.nesting_depth;

      AstNode* ret_expr = stmt->ret_stmt.expr;
      if(ret_expr)
      {
        AstNode* ret_var = proc->proc.ret_var;
        AstNode* assign = new_bin_expr(&stmt->src_loc);
        assign->bin_expr.op = AstOpKind_Assign;
        assign->bin_expr.lhs = ret_var->var_decl.id;
        assign->bin_expr.rhs = ret_expr;
        stmt->ret_stmt.expr = assign;
        success = do_expression(proc->proc.body, stmt->ret_stmt.expr);
      }
    }
    else
      success = compile_error(&stmt->src_loc, "Unexpected `return` at this location");
  }
  else if(stmt->kind == AstNodeKind_Id ||
          stmt->kind == AstNodeKind_Literal)
  {
    success = compile_error(&stmt->src_loc, "Unexpected statement `%s` at this location", get_ast_kind_printstr(stmt->kind));
  }
  else if(stmt->kind == AstNodeKind_BreakStmt || stmt->kind == AstNodeKind_ContinueStmt)
  {
    if(loop)
    {
      stmt->loop_ctrl.loop = loop;

      AstNode* loop_body = 0;
      if(loop->kind == AstNodeKind_WhileStmt)
        loop_body = loop->while_stmt.body;
      else if(loop->kind = AstNodeKind_ForStmt)
        loop_body = loop->for_stmt.body;
      assert(loop_body);

      stmt->loop_ctrl.nesting_depth = 0;
      if(loop_body->kind == AstNodeKind_Block)
        stmt->loop_ctrl.nesting_depth = block->block.nesting_depth - loop_body->block.nesting_depth;
      else
        assert(loop_body == stmt);
    }
    else
      success = compile_error(&stmt->src_loc, "Unexpected `%s` at this location", get_ast_kind_printstr(stmt->kind));
  }
  else if(stmt->kind == AstNodeKind_Call)
  {
    AstNode* id = stmt->call.id;
    Symbol* proc_sym = lookup_symbol(id->id.name, SymbolKind_Proc);
    if(proc_sym)
    {
      assert(!"do_call_args()");
      stmt->type = new_proc_type(make_type_of_node_list(&stmt->call.args), new_typevar());
      if(!type_unification(proc_sym->type, stmt->type))
      {
        success = compile_error(&stmt->src_loc, "Incompatible call and proc decl types...");
        compile_error(&proc_sym->node->src_loc, "...see proc decl");
      }
    }
    else
      success = compile_error(&stmt->src_loc, "Unknown procedure `%s`", id->id.name);
  }
  else if(stmt->kind == AstNodeKind_GotoStmt
          || stmt->kind == AstNodeKind_Label)
  {
    assert(!"Not implemented");
  }
  else if(stmt->kind == AstNodeKind_EmptyStmt)
  { /* nothing to do */ }
  else
    assert(false);
  return success;
}

internal bool
do_block(AstNode* proc, AstNode* loop, AstNode* owner, AstNode* block)
{
  assert(block->kind == AstNodeKind_Block);
  bool success = true;

  //block->block.owner = owner;

  if(owner && (owner->kind == AstNodeKind_Module))
  {
    for(ListItem* list_item = block->block.node_list.first;
        list_item && success;
        list_item = list_item->next)
    {
      AstNode* stmt = list_item->elem;
      if(stmt->kind == AstNodeKind_VarDecl)
        success = do_var_decl(block, stmt);
      else if(stmt->kind == AstNodeKind_Proc)
        success = do_proc_decl(stmt);
      else if(stmt->kind == AstNodeKind_Label
              || stmt->kind == AstNodeKind_Call
              || stmt->kind == AstNodeKind_Id
              || stmt->kind == AstNodeKind_Literal
              || stmt->kind == AstNodeKind_BinExpr
              || stmt->kind == AstNodeKind_UnrExpr)
        success = compile_error(&stmt->src_loc, "Unexpected statement %s", get_ast_kind_printstr(stmt->kind));
      else if(stmt->kind == AstNodeKind_Struct
              || stmt->kind == AstNodeKind_Union
              || stmt->kind == AstNodeKind_Enum)
      {
        assert(!"Not implemented");
      }
      else
        assert(false);
    }
  }
  else if((owner && (owner->kind == AstNodeKind_Proc
                     || owner->kind == AstNodeKind_WhileStmt
                     || owner->kind == AstNodeKind_ForStmt
                     || owner->kind == AstNodeKind_IfStmt))
          || !owner)
  {
    for(ListItem* list_item = block->block.node_list.first;
        list_item && success;
        list_item = list_item->next)
      success = do_statement(proc, block, loop, (AstNode*)list_item->elem);
  }
  else
    assert(false);
  return success;
}

internal bool
do_module(AstNode* module)
{
  assert(module->kind == AstNodeKind_Module);
  bool success = true;

  AstNode* mod_block = module->module.body;

  // process the includes
  for(ListItem* list_item = mod_block->block.node_list.first;
      list_item;
      list_item = list_item->next)
  {
    AstNode* stmt = list_item->elem;
    if(stmt->kind == AstNodeKind_IncludeStmt)
    {
      AstNode* incl_block = stmt->incl_stmt.body;
      do_include_stmt(&incl_block->block.node_list, &mod_block->block.node_list, list_item);
    }
  }

  if(success = scope_begin(module->module.body))
  {
    register_builtin_types();
    success = do_block(0, 0, module, module->module.body);
    scope_end();
  }
  return success;
}

bool
semantic_analysis(AstNode* node)
{
  init_types();

  symtab = mem_push_struct(arena, SymbolTable);

  bool success = true;
  assert(node->kind == AstNodeKind_Module);
  success = do_module(node);

  return success;
}


