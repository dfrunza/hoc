#include "hocc.h"

extern MemoryArena* arena;
extern MemoryArena* sym_arena;

extern Type* basic_type_bool;
extern Type* basic_type_int;
extern Type* basic_type_char;
extern Type* basic_type_float;
extern Type* basic_type_void;

SymbolTable* symtab = 0;
local int last_block_id = 0;
local int tempvar_id = 0;

local bool32 do_block(AstNode*, AstNode*, AstNode*, AstNode*);
local bool32 do_expression(AstNode*, AstNode*, AstNode**);

local char*
get_type_printstr(Type* type)
{
  char* result = "???";
  if(type->kind == TypeKind_Basic)
  {
    if(type->basic.kind == BasicTypeKind_Bool)
      result = "bool";
    else if(type->basic.kind == BasicTypeKind_Int)
      result = "int";
    else if(type->basic.kind == BasicTypeKind_Float)
      result = "float";
    else if(type->basic.kind == BasicTypeKind_Char)
      result = "char";
    else if(type->basic.kind == BasicTypeKind_Void)
      result = "void";
  }
  return result;
}

local bool32
is_arithmetic_op(AstOpKind op)
{
  return op == AstOpKind_Add || op == AstOpKind_Sub
    || op == AstOpKind_Mul || op == AstOpKind_Div
    || op == AstOpKind_Mod;
}

local bool32
is_logic_op(AstOpKind op)
{
  return op == AstOpKind_LogicOr || op == AstOpKind_LogicAnd || op == AstOpKind_LogicNot;
}

local bool32
is_relation_op(AstOpKind op)
{
  return op == AstOpKind_LogicEquals || op == AstOpKind_LogicNotEquals
      || op == AstOpKind_LogicLess || op == AstOpKind_LogicLessEquals
      || op == AstOpKind_LogicGreater || op == AstOpKind_LogicGreaterEquals;
}

local AstNode*
make_tempvar_id(SourceLocation* src_loc, char* label)
{
  String str = {0};
  str_init(&str, arena);
  str_printf(&str, "$%s%d", label, tempvar_id++);
  return new_id(src_loc, str.head);
}

Symbol*
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

#if 0
local Symbol*
find_last_symbol_in_block(AstNode* block)
{
  Symbol* symbol = symtab->curr_symbol;
  while(symbol && (symbol->block_id > block->block.block_id))
    symbol = symbol->prev_symbol;
  return symbol;
}
#endif

local Symbol*
register_id(AstNode* id, SymbolKind kind)
{
  assert(id->kind == AstNodeKind_Id);
  Symbol* sym = mem_push_struct(sym_arena, Symbol);
  sym->kind = kind;
  sym->name = id->id.name;
  sym->block_id = symtab->block_id;
  sym->nesting_depth = symtab->nesting_depth;
  sym->prev_symbol = symtab->curr_symbol;
  sym->id = id;
  id->id.sym = sym;

  symtab->curr_symbol = sym;
  symtab->sym_count++;
  return sym;
}

local Symbol*
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
    sym = register_id(id, symkind);
  return sym;
}

local Symbol*
add_builtin_type(char* name, Type* type)
{
  assert(type->kind == TypeKind_Basic);
  AstNode* type_id = new_id(mem_push_struct(arena, SourceLocation), name);
  Symbol* sym = register_new_id(type_id, SymbolKind_TypeDecl);
  sym->type = type;
  return sym;
}

local void
add_builtin_types()
{
  add_builtin_type("bool", basic_type_bool);
  add_builtin_type("int", basic_type_int);
  add_builtin_type("char", basic_type_char);
  add_builtin_type("float", basic_type_float);
  add_builtin_type("void", basic_type_void);
}

local bool32
scope_begin(AstNode* ast)
{
  assert(ast->kind == AstNodeKind_Block);

  use(ast, block);
  symtab->block_id =++last_block_id; 
  block->block_id = symtab->block_id;
  block->encl_block = symtab->curr_block;

  ++symtab->nesting_depth;
  if(symtab->nesting_depth < sizeof_array(symtab->active_blocks))
  {
    block->nesting_depth = symtab->nesting_depth;
    symtab->active_blocks[symtab->nesting_depth] = ast;
    symtab->curr_block = ast;
  }
  else
  {
    compile_error(&ast->src_loc, "Exceeded max scope nesting depth : %d", sizeof_array(symtab->active_blocks));
    return false;
  }

  return true;
}

local void
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

local void
do_include_stmt(List* include_list, List* module_list, ListItem* module_list_item)
{
  for(ListItem* list_item = include_list->first;
      list_item;
      list_item = list_item->next)
  {
    auto stmt = (AstNode*)list_item->elem;
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

local bool32
do_var_decl(AstNode* block, AstNode* var_decl_ast)
{
  assert(block->kind == AstNodeKind_Block);
  assert(var_decl_ast->kind == AstNodeKind_VarDecl);
  bool32 success = true;

  AstNode* type_id = var_decl_ast->var_decl.type;
  if(type_id->kind == AstNodeKind_Id)
  {
    Symbol* type_decl_sym = lookup_symbol(type_id->id.name, SymbolKind_TypeDecl);
    if(type_decl_sym)
    {
      assert(type_decl_sym->type);
      Symbol* type_occur_sym = register_id(type_id, SymbolKind_TypeOccur);
      type_occur_sym->type = type_decl_sym->type;
      type_id->type = type_decl_sym->type;

      AstNode* var_id = var_decl_ast->var_decl.id;
      var_id->type = type_decl_sym->type;
      Symbol* var_decl_sym = register_new_id(var_id, SymbolKind_VarDecl);
      if(success = to_bool(var_decl_sym))
      {
        var_decl_ast->type = type_decl_sym->type;
        var_decl_sym->ast = var_decl_ast;
        list_append(arena, &block->block.decl_vars, var_decl_sym);
        if(var_decl_ast->var_decl.init_expr)
        {
          AstNode* assign_node = new_bin_expr(&var_decl_ast->src_loc);
          use(assign_node, bin_expr);
          bin_expr->op = AstOpKind_Assign;
          bin_expr->lhs = new_id(&var_decl_ast->src_loc, var_id->id.name);;
          bin_expr->rhs = var_decl_ast->var_decl.init_expr;

          if(success = do_expression(block, assign_node, &assign_node))
            list_append(arena, &block->block.stmts, assign_node);
        }
      }
    }
    else
      success = compile_error(&type_id->src_loc, "Unknown type `%s`", type_id->id.name);
  }
  else
    fail("only simple types are supported");
  return success;
}

local bool32
do_call_args(AstNode* block, AstNode* call)
{
  assert(call->kind == AstNodeKind_Call);
  bool32 success = true;

  for(ListItem* list_item = call->call.args.first;
      list_item && success;
      list_item = list_item->next)
  {
    auto arg = (AstNode*)list_item->elem;
    success = do_expression(block, arg, &arg);
  }
  return success;
}

local bool32
do_call(AstNode* block, AstNode* call_ast)
{
  assert(call_ast->kind == AstNodeKind_Call);
  bool32 success = true;

  AstNode* call_id = call_ast->call.id;
  Symbol* proc_sym = lookup_symbol(call_id->id.name, SymbolKind_Proc);
  if(proc_sym)
  {
    Symbol* call_sym = register_id(call_id, SymbolKind_Call);
    call_sym->ast = call_ast;

    if(success = do_call_args(block, call_ast))
    {
      AstNode* registered_proc = proc_sym->ast;
      Type* ret_type = registered_proc->proc.ret_type->type;
      call_ast->type = new_proc_type(make_type_of_node_list(&call_ast->call.args), ret_type);
      if(type_unif(registered_proc->type, call_ast->type))
      {
        call_id->type = ret_type;
        call_ast->call.proc = registered_proc;
      }
      else
      {
        success = compile_error(&call_ast->src_loc, "Missmatch between call and proc signature");
        compile_error(&registered_proc->src_loc, "...see proc decl");
      }
    }
  }
  else
    success = compile_error(&call_ast->src_loc, "Unknown procedure `%s`", call_id->id.name);
  return success;
}

local bool32
do_expression(AstNode* block, AstNode* expr_ast, AstNode** out_ast)
{
  assert(block->kind == AstNodeKind_Block);
  bool32 success = true;
  *out_ast = expr_ast;

  if(expr_ast->kind == AstNodeKind_BinExpr)
  {
    use(expr_ast, bin_expr);
    if(success = do_expression(block, bin_expr->lhs, &bin_expr->lhs)
      && do_expression(block, bin_expr->rhs, &bin_expr->rhs))
    {
      if(type_unif(bin_expr->lhs->type, bin_expr->rhs->type))
      {
        expr_ast->type = bin_expr->lhs->type;
        if(is_arithmetic_op(bin_expr->op) || is_relation_op(bin_expr->op))
        {
          if(type_unif(expr_ast->type, basic_type_int) || type_unif(expr_ast->type, basic_type_float))
          {
            if(is_relation_op(bin_expr->op))
              expr_ast->type = basic_type_bool;
          }
          else
            success = compile_error(&expr_ast->src_loc,
                                    "int/float operands are expected, actual `%s`", get_type_printstr(expr_ast->type));
        }
        else if(is_logic_op(bin_expr->op) && !type_unif(expr_ast->type, basic_type_bool))
          success = compile_error(&expr_ast->src_loc,
                                  "bool operands are expected, actual `%s`", get_type_printstr(expr_ast->type));
      }
      else
        success = compile_error(&expr_ast->src_loc, "Expected operands of same type");
    }
  }
  else if(expr_ast->kind == AstNodeKind_UnrExpr)
  {
    use(expr_ast, unr_expr);
    if(success = do_expression(block, unr_expr->operand, &unr_expr->operand))
    {
      if(unr_expr->op == AstOpKind_AddressOf)
        expr_ast->type = new_pointer_type(unr_expr->operand->type);
      else if(unr_expr->op == AstOpKind_Neg)
      {
        if(type_unif(unr_expr->operand->type, basic_type_int)
           || type_unif(unr_expr->operand->type, basic_type_float))
          expr_ast->type = unr_expr->operand->type;
        else
          success = compile_error(&expr_ast->src_loc,
                                  "int/float operands are expected, actual `%s`", get_type_printstr(unr_expr->operand->type));
      }
      else if(is_logic_op(unr_expr->op))
      {
        if(type_unif(unr_expr->operand->type, basic_type_bool))
          expr_ast->type = unr_expr->operand->type;
        else
          success = compile_error(&expr_ast->src_loc,
                                  "bool operand is expected, actual `%s`", get_type_printstr(unr_expr->operand->type));
      }
      else
        fail("not implemented");
    }
  }
  else if(expr_ast->kind == AstNodeKind_Id)
  {
    Symbol* var_decl_sym = lookup_symbol(expr_ast->id.name, SymbolKind_VarDecl);
    if(var_decl_sym)
    {
      assert(var_decl_sym->ast->kind == AstNodeKind_VarDecl);

      AstNode* var_occur_ast = new_var_occur(&expr_ast->src_loc);
      use(var_occur_ast, var_occur);
      Symbol* var_occur_sym = register_id(expr_ast, SymbolKind_VarOccur);
      var_occur_sym->ast = *out_ast;
      var_occur->var_decl = var_decl_sym->ast;
      var_occur->id = expr_ast;
      expr_ast->type = var_decl_sym->ast->type;
      var_occur_ast->type = var_decl_sym->ast->type;

      var_occur->decl_block_offset = block->block.nesting_depth - var_decl_sym->nesting_depth;
      var_occur->occur_block = block;

      if(var_occur->decl_block_offset > 0)
        list_append(arena, &block->block.nonlocals, var_occur_ast);
      else if(var_occur->decl_block_offset == 0)
        list_append(arena, &block->block.locals, var_occur_ast);
      else
        assert(false);

      *out_ast = var_occur_ast;
    }
    else
      success = compile_error(&expr_ast->src_loc, "Unknown identifier `%s`", expr_ast->id.name);
  }
  else if(expr_ast->kind == AstNodeKind_Literal)
  {
    use(expr_ast, literal);
    if(literal->kind == AstLiteralKind_Int)
      expr_ast->type = basic_type_int;
    else if(literal->kind == AstLiteralKind_Float)
      expr_ast->type = basic_type_float;
    else if(literal->kind == AstLiteralKind_Bool)
      expr_ast->type = basic_type_bool;
    else if(literal->kind == AstLiteralKind_Char)
      expr_ast->type = basic_type_char;
    else if(literal->kind == AstLiteralKind_String)
      expr_ast->type = new_array_type(-1, basic_type_char);
    else
      assert(false);
  }
  else if(expr_ast->kind == AstNodeKind_Call)
  {
    if(success = do_call(block, expr_ast))
      expr_ast->type = expr_ast->type->proc.ret;
  }
  else if(expr_ast->kind == AstNodeKind_Cast)
  {
    use(expr_ast, cast);
    if(cast->type->kind == AstNodeKind_Id)
    {
      Symbol* type_sym = lookup_symbol(cast->type->id.name, SymbolKind_TypeDecl);
      if(type_sym)
        expr_ast->type = type_sym->type;
      else
        compile_error(&expr_ast->src_loc, "Unknown type `%s`", cast->type->id.name);
    }
    else
      fail("only simple types are supported");
  }
  else
    fail("not implemented : %s", get_ast_kind_printstr(expr_ast->kind));
  return success;
}

local bool32
do_proc_formal_args(AstNode* block, List* args)
{
  assert(block->kind == AstNodeKind_Block);
  bool32 success = true;

  for(ListItem* list_item = args->first;
      list_item && success;
      list_item = list_item->next)
  {
    auto ast = (AstNode*)list_item->elem;
    assert(ast->kind == AstNodeKind_VarDecl);
    use(ast, var_decl);
    if(var_decl->id)
      success = do_var_decl(block, ast);
    else
      success = compile_error(&ast->src_loc, "Missing identifier : %s", get_ast_kind_printstr(ast->kind));
    assert(!var_decl->init_expr); /* enforced by parser */
  }
  return success;
}

local bool32
do_proc_ret_var(AstNode* block, AstNode* proc)
{
  assert(proc->kind == AstNodeKind_Proc);
  bool32 success = true;
  use(proc->proc.ret_var = new_var_decl(&proc->src_loc), var_decl);
  var_decl->id = make_tempvar_id(&proc->src_loc, "ret");
  var_decl->type = proc->proc.ret_type;
  success = do_var_decl(block, proc->proc.ret_var);
  return success;
}

local bool32
do_proc_decl(AstNode* proc_ast)
{
  assert(proc_ast->kind == AstNodeKind_Proc);
  bool32 success = true;
  use(proc_ast, proc);

  if(success = scope_begin(proc->body))
  {
    if(success = do_proc_formal_args(proc->body, &proc->args)
       && do_proc_ret_var(proc->body, proc_ast))
    {
      proc_ast->type = new_proc_type(make_type_of_node_list(&proc->args), proc->ret_var->type);

      Symbol* proc_sym = lookup_symbol(proc->id->id.name, SymbolKind_Proc);
      if(proc_sym)
      {
        AstNode* registered_proc = proc_sym->ast;
        assert(registered_proc && registered_proc->kind == AstNodeKind_Proc);

        if(registered_proc->proc.is_decl && proc->is_decl)
        {
          if(!type_unif(registered_proc->type, proc_ast->type))
          {
            success = compile_error(&proc_ast->src_loc, "Inconsistent proc signature...");
            compile_error(&registered_proc->src_loc, "...see previous decl");
          }
        }
        else if(!registered_proc->proc.is_decl && !proc->is_decl)
        {
          success = compile_error(&proc_ast->src_loc, "Proc redefinition...");
          compile_error(&registered_proc->src_loc, "...see previous def");
        }
        else if(registered_proc->proc.is_decl && !proc->is_decl)
        {
          if(type_unif(registered_proc->type, proc_ast->type))
          {
            proc_sym->ast = proc_ast;
            proc->id->id.sym = proc_sym;
          }
          else
          {
            success = compile_error(&proc_ast->src_loc, "Inconsistent proc signature...");
            compile_error(&registered_proc->src_loc, "...see decl");
          }
        }
        /* else fall-thru */
      }
      else if(success = to_bool(proc_sym = register_new_id(proc->id, SymbolKind_Proc)))
      {
        proc_sym->ast = proc_ast;
        proc->id->id.sym = proc_sym;
      }

      if(success)
        success = do_block(proc_ast, 0, proc_ast, proc->body);
    }
    scope_end();
  }
  return success;
}

local bool32
do_statement(AstNode* proc, AstNode* block, AstNode* loop, AstNode* stmt)
{
  assert(block->kind == AstNodeKind_Block);
  bool32 success = true;

  if(stmt->kind == AstNodeKind_VarDecl)
    success = do_var_decl(block, stmt);
  else if(stmt->kind == AstNodeKind_BinExpr || stmt->kind == AstNodeKind_Call)
   success = do_expression(block, stmt, &stmt);
  else if(stmt->kind == AstNodeKind_UnrExpr)
  {
    if(stmt->unr_expr.op == AstOpKind_PostDecrement
       || stmt->unr_expr.op == AstOpKind_PreDecrement
       || stmt->unr_expr.op == AstOpKind_PostIncrement
       || stmt->unr_expr.op == AstOpKind_PreIncrement)
      success = do_expression(block, stmt, &stmt);
    else
      success = compile_error(&stmt->src_loc, "Unexpected statement %s", get_ast_kind_printstr(stmt->kind));
  }
  else if(stmt->kind == AstNodeKind_WhileStmt)
  {
    use(stmt, while_stmt);
    if(success = do_expression(block, while_stmt->cond_expr, &while_stmt->cond_expr))
    {
      if(type_unif(while_stmt->cond_expr->type, basic_type_bool))
      {
        if(while_stmt->body->kind == AstNodeKind_Block)
        {
          if(success = scope_begin(while_stmt->body))
          {
            success = do_block(proc, stmt, stmt, while_stmt->body);
            scope_end();
          }
        }
        else
          success = do_statement(proc, block, stmt, while_stmt->body);
      }
      else
        success = compile_error(&stmt->src_loc, "Boolean expression is expected");
    }
  }
  else if(stmt->kind == AstNodeKind_ForStmt)
  {
    use(stmt, for_stmt);
    if(success = scope_begin(for_stmt->body))
    {
      success = (for_stmt->decl_expr ? do_var_decl(for_stmt->body, for_stmt->decl_expr) : 1);
      if(success)
      {
        success = do_block(proc, stmt, stmt, for_stmt->body) // will set the block->owner field, so do it first
          && (for_stmt->cond_expr ? do_expression(for_stmt->body, for_stmt->cond_expr, &for_stmt->cond_expr) : 1)
          && (for_stmt->loop_expr ? do_expression(for_stmt->body, for_stmt->loop_expr, &for_stmt->loop_expr) : 1);
        if(success)
        {
          if(!type_unif(for_stmt->cond_expr->type, basic_type_bool))
            success = compile_error(&stmt->src_loc, "Boolean expression is expected");
        }
      }
      scope_end();
    }
  }
  else if(stmt->kind == AstNodeKind_IfStmt)
  {
    use(stmt, if_stmt);
    if(success = do_expression(block, if_stmt->cond_expr, &if_stmt->cond_expr))
    {
      if(type_unif(if_stmt->cond_expr->type, basic_type_bool))
      {
        if(if_stmt->body->kind == AstNodeKind_Block)
        {
          if(success = scope_begin(if_stmt->body))
          {
            success = do_block(proc, loop, stmt, if_stmt->body);
            scope_end();
          }
        }
        else
          success = do_statement(proc, block, loop, stmt->if_stmt.body);

        if(if_stmt->else_body)
        {
          if(if_stmt->else_body->kind == AstNodeKind_Block)
          {
            if(success = scope_begin(if_stmt->else_body))
            {
              success = do_block(proc, stmt, loop, if_stmt->else_body);
              scope_end();
            }
          }
          else
            success = do_statement(proc, block, loop, if_stmt->else_body);
        }
      }
      else
        success = compile_error(&stmt->src_loc, "Boolean expression is expected");
    }
  }
  else if(stmt->kind == AstNodeKind_ReturnStmt)
  {
    if(proc)
    {
      use(stmt, ret_stmt);
      ret_stmt->proc = proc;
      ret_stmt->nesting_depth = block->block.nesting_depth - proc->proc.body->block.nesting_depth;
      stmt->type = basic_type_void;

      AstNode* ret_expr = ret_stmt->expr;
      AstNode* ret_var = proc->proc.ret_var;
      assert(ret_var->kind == AstNodeKind_VarDecl);
      if(ret_expr)
      {
        AstNode* lhs_expr = new_id(&ret_expr->src_loc, ret_var->var_decl.id->id.name);

        if(success = do_expression(block, ret_expr, &ret_expr) &&
           do_expression(block, lhs_expr, &lhs_expr))
        {
          if(type_unif(ret_expr->type, ret_var->type))
          {
            AstNode* assign_expr = new_bin_expr(&stmt->src_loc);
            use(assign_expr, bin_expr);
            bin_expr->op = AstOpKind_Assign;
            bin_expr->lhs = lhs_expr;
            bin_expr->rhs = ret_expr;
            assign_expr->type = ret_expr->type;

            ret_stmt->expr = assign_expr;
            stmt->type = ret_expr->type;
          }
          else
            success = compile_error(&stmt->src_loc,
                                    "return type : expected `%s`, actual `%s`", get_type_printstr(ret_var->type), get_type_printstr(ret_expr->type));
        }
      }
      else
        if(!(success = type_unif(ret_var->type, stmt->type)))
          success = compile_error(&stmt->src_loc,
                                  "return type : expected `%s`, actual `%s`", get_type_printstr(ret_var->type), get_type_printstr(stmt->type));
      if(success)
        assert(type_unif(stmt->type, proc->proc.ret_var->type));
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
      use(stmt, loop_ctrl);
      loop_ctrl->loop = loop;

      AstNode* loop_body = 0;
      if(loop->kind == AstNodeKind_WhileStmt)
        loop_body = loop->while_stmt.body;
      else if(loop->kind = AstNodeKind_ForStmt)
        loop_body = loop->for_stmt.body;
      assert(loop_body);

      loop_ctrl->nesting_depth = 0;
      if(loop_body->kind == AstNodeKind_Block)
        loop_ctrl->nesting_depth = block->block.nesting_depth - loop_body->block.nesting_depth;
      else
        assert(loop_body == stmt);
    }
    else
      success = compile_error(&stmt->src_loc, "Unexpected `%s` at this location", get_ast_kind_printstr(stmt->kind));
  }
  else if(stmt->kind == AstNodeKind_GotoStmt
          || stmt->kind == AstNodeKind_Label)
  {
    fail("not implemented : %s\n", get_ast_kind_printstr(stmt->kind));
  }
  else if(stmt->kind == AstNodeKind_EmptyStmt)
  { /* nothing to do */ }
  else
    assert(false);
  return success;
}

local bool32
do_block(AstNode* proc, AstNode* loop, AstNode* owner, AstNode* block)
{
  assert(block->kind == AstNodeKind_Block);
  bool32 success = true;

  //block->block.owner = owner;

  if(owner && (owner->kind == AstNodeKind_Module))
  {
    for(ListItem* list_item = block->block.node_list.first;
        list_item && success;
        list_item = list_item->next)
    {
      auto stmt = (AstNode*)list_item->elem;
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
        fail("Not implemented");
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

local bool32
do_module(AstNode* mod_ast)
{
  assert(mod_ast->kind == AstNodeKind_Module);
  bool32 success = true;
  use(mod_ast, module);

  auto mod_block = &module->body->block;

  // process the includes
  for(ListItem* list_item = mod_block->node_list.first;
      list_item;
      list_item = list_item->next)
  {
    auto stmt = (AstNode*)list_item->elem;
    if(stmt->kind == AstNodeKind_IncludeStmt)
    {
      AstNode* incl_block = stmt->incl_stmt.body;
      do_include_stmt(&incl_block->block.node_list, &mod_block->node_list, list_item);
    }
  }

  if(success = scope_begin(module->body))
  {
    add_builtin_types();
    success = do_block(0, 0, mod_ast, module->body);
    scope_end();
  }
  return success;
}

bool32
semantic_analysis(AstNode* ast)
{
  init_types();

  symtab = mem_push_struct(arena, SymbolTable);

  bool32 success = true;
  assert(ast->kind == AstNodeKind_Module);
  success = do_module(ast);

  return success;
}


