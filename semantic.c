#include "hocc.h"

extern MemoryArena* arena;
extern MemoryArena* sym_arena;

extern Type* basic_type_bool;
extern Type* basic_type_int;
extern Type* basic_type_char;
extern Type* basic_type_float;
extern Type* basic_type_void;

extern SourceLocation* deflt_src_loc;

SymbolTable* symtab = 0;
local int last_block_id = 0;
local int tempvar_id = 0;

local bool32 do_block(AstProc*, AstNode*, AstNode*, AstBlock*);
local bool32 do_expression(AstBlock*, AstNode*, AstNode**);

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
  else if(type->kind == TypeKind_Pointer)
    result = "pointer";
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
  return op == AstOpKind_Equals || op == AstOpKind_NotEquals
      || op == AstOpKind_Less || op == AstOpKind_LessEquals
      || op == AstOpKind_Greater || op == AstOpKind_GreaterEquals;
}

local AstId*
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
register_id(AstId* id, SymbolKind kind)
{
  assert(id->kind == AstNodeKind_Id);
  Symbol* sym = mem_push_struct(sym_arena, Symbol);
  sym->kind = kind;
  sym->name = id->name;
  sym->block_id = symtab->block_id;
  sym->nesting_depth = symtab->nesting_depth;
  sym->prev_symbol = symtab->curr_symbol;
  sym->id = (AstNode*)id;
  id->sym = sym;

  symtab->curr_symbol = sym;
  symtab->sym_count++;
  return sym;
}

local bool32
register_type_occur(AstNode* type_expr)
{
  bool32 success = true;

  if(type_expr->kind == AstNodeKind_Pointer)
  {
    AstPointer* ptr_ast = (AstPointer*)type_expr;
    register_type_occur(ptr_ast->expr);
    ptr_ast->type = new_pointer_type(ptr_ast->expr->type);
  }
  else if(type_expr->kind == AstNodeKind_Id)
  {
    AstId* type_id = (AstId*)type_expr;
    Symbol* decl_sym = lookup_symbol(type_id->name, SymbolKind_TypeDecl);
    if(decl_sym)
    {
      assert(decl_sym->type);
      Type* type = decl_sym->type;
      Symbol* occur_sym = register_id(type_id, SymbolKind_TypeOccur);
      occur_sym->type = type;
      type_id->type = type;
    }
    else
      success = compile_error(&type_id->src_loc, "Unknown type `%s`", type_id->name);
  }
  else
    assert(false);

  return success;
}

local bool32
register_var_occur(AstVarOccur* var_occur)
{
  bool32 success = true;

  AstId* id = (AstId*)var_occur->id;
  Symbol* decl_sym = lookup_symbol(id->name, SymbolKind_VarDecl);
  if(decl_sym)
  {
    assert(decl_sym->ast->kind == AstNodeKind_VarDecl);
    AstVarDecl* var_decl = (AstVarDecl*)decl_sym->ast;

    Symbol* occur_sym = register_id(id, SymbolKind_VarOccur);
    occur_sym->ast = (AstNode*)var_occur;
    var_occur->var_decl = var_decl;
    var_occur->type = var_decl->type;
    var_occur->id->type = var_decl->type;
  }
  else
    success = compile_error(&var_occur->src_loc, "Unknown var `%s`", id->name);
  return success;
}

local bool32
register_new_id(AstId* id, SymbolKind symkind)
{
  assert(id->kind == AstNodeKind_Id);
  bool32 success = true;

  Symbol* sym = lookup_symbol(id->name, symkind);
  if(sym && (sym->block_id == symtab->block_id))
  {
    success = compile_error(&id->src_loc, "Symbol redeclaration `%s`...", id->name);
    compile_error(&id->src_loc, "...see previous declaration of `%s`", id->name);
  }
  else
    register_id(id, symkind);
  return success;
}

local bool32
register_var_decl(AstVarDecl* var_decl)
{
  bool32 success = true;

  AstNode* type_expr = var_decl->type_expr;
  if(register_type_occur(type_expr))
  {
    AstId* var_id = var_decl->id;
    var_id->type = type_expr->type;
    if(success = register_new_id(var_id, SymbolKind_VarDecl))
    {
      var_decl->type = type_expr->type;
      var_id->sym->ast = (AstNode*)var_decl;
    }
  }
  return success;
}

local bool32
register_proc_decl(AstProc* proc)
{
  bool32 success = true;

  AstId* id = (AstId*)proc->id;
  Symbol* proc_sym = lookup_symbol(id->name, SymbolKind_Proc);
  if(proc_sym)
  {
    AstProc* registered_proc = (AstProc*)proc_sym->ast;
    assert(registered_proc && registered_proc->kind == AstNodeKind_Proc);

    if(registered_proc->is_decl && proc->is_decl)
    {
      if(!type_unif(registered_proc->type, proc->type))
      {
        success = compile_error(&proc->src_loc, "Inconsistent proc signature...");
        compile_error(&registered_proc->src_loc, "...see previous decl");
      }
    }
    else if(!registered_proc->is_decl && !proc->is_decl)
    {
      success = compile_error(&proc->src_loc, "Proc redefinition...");
      compile_error(&registered_proc->src_loc, "...see previous def");
    }
    else if(registered_proc->is_decl && !proc->is_decl)
    {
      if(type_unif(registered_proc->type, proc->type))
      {
        proc_sym->ast = (AstNode*)proc;
        id->sym = proc_sym;
      }
      else
      {
        success = compile_error(&proc->src_loc, "Inconsistent proc signature...");
        compile_error(&registered_proc->src_loc, "...see decl");
      }
    }
    /* else fall-thru */
  }
  else if(success = register_new_id(id, SymbolKind_Proc))
    id->sym->ast = (AstNode*)proc;

  return success;
}

local bool32
register_call(AstCall* call)
{
  bool32 success = true;

  AstId* id = (AstId*)call->id;
  Symbol* proc_sym = lookup_symbol(id->name, SymbolKind_Proc);
  if(proc_sym)
  {
    Symbol* call_sym = register_id(id, SymbolKind_Call);
    call_sym->ast = (AstNode*)call;
    call->proc_sym = proc_sym;
    Type* proc_type = proc_sym->ast->type;
    call->type = proc_type->proc.ret;
  }
  else
    success = compile_error(&call->src_loc, "Unknown proc `%s`", id->name);
  return success;
}

local void
add_builtin_type(char* name, Type* type)
{
  assert(type->kind == TypeKind_Basic);
  bool32 success = true;
  AstId* type_id = new_id(deflt_src_loc, name);
  success = register_new_id(type_id, SymbolKind_TypeDecl);
  assert(success);
  type_id->sym->type = type;
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
scope_begin(AstBlock* block)
{
  symtab->block_id =++last_block_id; 
  block->block_id = symtab->block_id;
  block->encl_block = symtab->curr_block;

  ++symtab->nesting_depth;
  if(symtab->nesting_depth < sizeof_array(symtab->active_blocks))
  {
    block->nesting_depth = symtab->nesting_depth;
    symtab->active_blocks[symtab->nesting_depth] = block;
    symtab->curr_block = block;
  }
  else
  {
    compile_error(&block->src_loc, "Exceeded max scope nesting depth : %d", sizeof_array(symtab->active_blocks));
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
    AstBlock* block = symtab->active_blocks[symtab->nesting_depth];
    assert(block->block_id > 0);
    symtab->curr_block = block;
    symtab->block_id = block->block_id;

#if 1
    Symbol* symbol = symtab->curr_symbol;
    while(symbol && (symbol->block_id > symtab->block_id))
      symbol = symbol->prev_symbol;
    symtab->curr_symbol = symbol;
#else
    symtab->curr_symbol = find_last_symbol_in_block(symtab->curr_block);
#endif
  }
  else
  {
    assert(symtab->nesting_depth == 0);
    symtab->block_id = 0;
    symtab->curr_block = 0;
  }
}

local void
do_include_stmt(List* include_list, List* module_list, ListItem* module_list_item)
{
  for(ListItem* list_item = include_list->first;
      list_item;
      list_item = list_item->next)
  {
    AstNode* stmt = (AstNode*)list_item->elem;
    if(stmt->kind == AstNodeKind_IncludeStmt)
    {
      AstIncludeStmt* incl_stmt = (AstIncludeStmt*)stmt;
      AstBlock* incl_block = (AstBlock*)incl_stmt->body;
      do_include_stmt(&incl_block->node_list, include_list, list_item);
    }
  }
  list_replace_at(include_list, module_list, module_list_item);

  mem_zero_struct(include_list, List);
  list_init(include_list);
}

local bool32
do_var_decl(AstBlock* block, AstVarDecl* var_decl)
{
  bool32 success = true;

  if(success = register_var_decl(var_decl))
  {
    var_decl->decl_block = block;
    list_append(arena, &block->decl_vars, var_decl);

    if(var_decl->init_expr)
    {
      AstBinExpr* bin_expr = new_bin_expr(&var_decl->src_loc); 
      var_decl->assign_expr = bin_expr;
      bin_expr->op = AstOpKind_Assign;
      bin_expr->left_operand = (AstNode*)new_id(&var_decl->src_loc, var_decl->id->name);;
      bin_expr->right_operand = var_decl->init_expr;

      success = do_expression(block, (AstNode*)bin_expr, &(AstNode*)bin_expr);
    }
  }
  return success;
}

local bool32
do_call_args(AstBlock* block, AstCall* call)
{
  bool32 success = true;

  for(ListItem* list_item = call->args.first;
      list_item && success;
      list_item = list_item->next)
  {
    AstNode* arg = (AstNode*)list_item->elem;
    if(success = do_expression(block, arg, &arg))
      list_item->elem = arg;
  }
  return success;
}

local bool32
do_call(AstBlock* block, AstCall* call)
{
  bool32 success = true;

  if(success = do_call_args(block, call) && register_call(call))
  {
    assert(call->type);
    Type* call_type = new_proc_type(make_type_of_node_list(&call->args), call->type);
    AstProc* proc = (AstProc*)call->proc_sym->ast;
    if(!type_unif(proc->type, call_type))
    {
      success = compile_error(&call->src_loc, "`%s(..)` call does not match proc signature", call->id->name);
      compile_error(&proc->src_loc, "...see proc decl");
    }
  }
  return success;
}

local bool32
do_expression(AstBlock* encl_block, AstNode* expr, AstNode** out_expr)
{
  bool32 success = true;
  *out_expr = expr;

  if(expr->kind == AstNodeKind_BinExpr)
  {
    AstBinExpr* bin_expr = (AstBinExpr*)expr;
    if(success = do_expression(encl_block, bin_expr->left_operand, &bin_expr->left_operand)
      && do_expression(encl_block, bin_expr->right_operand, &bin_expr->right_operand))
    {
      Type* left_type = bin_expr->left_operand->type;
      Type* right_type = bin_expr->right_operand->type;
      expr->type = left_type;

      if(type_unif(left_type, right_type))
      {
        if(types_are_equal(left_type, basic_type_float))
        {
          if(is_arithmetic_op(bin_expr->op))
          {
            if(bin_expr->op == AstOpKind_Add)
              bin_expr->op = AstOpKind_AddFloat;
            else if(bin_expr->op == AstOpKind_Sub)
              bin_expr->op = AstOpKind_SubFloat;
            else if(bin_expr->op == AstOpKind_Div)
              bin_expr->op = AstOpKind_DivFloat;
            else if(bin_expr->op == AstOpKind_Mul)
              bin_expr->op = AstOpKind_MulFloat;
            else if(bin_expr->op == AstOpKind_Mod)
              success = compile_error(&expr->src_loc, "Modulo operator cannot be applied to float operands");
            else
              assert(false);
          }
        }

        if(success)
        {
          if(is_arithmetic_op(bin_expr->op) || is_relation_op(bin_expr->op))
          {
            if(types_are_equal(left_type, basic_type_int)
               || types_are_equal(left_type, basic_type_float)
               || types_are_equal(left_type, basic_type_char)
               || left_type->kind == TypeKind_Pointer)
            {
              if(is_relation_op(bin_expr->op))
                expr->type = basic_type_bool;
            }
            else
              success = compile_error(&expr->src_loc,
                                      "int, float or char operands are expected, actual `%s`", get_type_printstr(left_type));
          }
          else if(is_logic_op(bin_expr->op) && !types_are_equal(left_type, basic_type_bool))
            success = compile_error(&expr->src_loc,
                                    "bool operands are expected, actual `%s`", get_type_printstr(left_type));
        }
      }
      else if(types_are_equal(right_type, basic_type_int))
      {
        if(types_are_equal(left_type, basic_type_float))
        {
          AstUnrExpr* int_to_float = new_unr_expr(&bin_expr->right_operand->src_loc);
          int_to_float->op = AstOpKind_IntToFloat;
          int_to_float->type = basic_type_float;
          int_to_float->operand = bin_expr->right_operand;
          bin_expr->right_operand = (AstNode*)int_to_float;

          if(is_arithmetic_op(bin_expr->op))
          {
            if(bin_expr->op == AstOpKind_Add)
              bin_expr->op = AstOpKind_AddFloat;
            else if(bin_expr->op == AstOpKind_Sub)
              bin_expr->op = AstOpKind_SubFloat;
            else if(bin_expr->op == AstOpKind_Div)
              bin_expr->op = AstOpKind_DivFloat;
            else if(bin_expr->op == AstOpKind_Mul)
              bin_expr->op = AstOpKind_MulFloat;
            else if(bin_expr->op == AstOpKind_Mod)
              success = compile_error(&expr->src_loc, "Modulo operator cannot be applied to float operands");
            else
              assert(false);
          }
        }
        else if(types_are_equal(left_type, basic_type_bool)
                || left_type->kind == TypeKind_Pointer)
          ;/*noop*/
        else
          success = compile_error(&expr->src_loc, "Cannot convert int to `%s`", get_type_printstr(left_type));
      }
      else if(types_are_equal(right_type, basic_type_char))
      {
        if(types_are_equal(left_type, basic_type_int))
          bin_expr->right_operand->type = basic_type_int;
        else
          success = compile_error(&expr->src_loc, "Cannot convert char to `%s`", get_type_printstr(left_type));
      }
      else
        success = compile_error(&expr->src_loc, "Expression operands must be of same type");
    }
  }
  else if(expr->kind == AstNodeKind_UnrExpr)
  {
    AstUnrExpr* unr_expr = (AstUnrExpr*)expr;
    if(unr_expr->op == AstOpKind_PostIncrement)
    {
#if 0
      AstBinExpr* assign = new_bin_expr(&unr_expr->src_loc);
      assign->op = AstOpKind_Assign;
      assign->left_operand = unr_expr->operand;

      AstBinExpr* incr_one = new_bin_expr(&unr_expr->src_loc);
      incr_one->op = AstOpKind_Add;
      incr_one->left_operand = unr_expr->operand;
      AstLiteral* one = new_literal(&unr_expr->src_loc);
      one->lit_kind = AstLiteralKind_Int;
      one->int_val = 1;
      incr_one->right_operand = (AstNode*)one;

      assign->right_operand = (AstNode*)incr_one;
      if(success = do_expression(encl_block, (AstNode*)assign, &(AstNode*)assign))
        *out_expr = (AstNode*)assign;
#else
      fail("not implemented");
#endif
    }
    else 
    {
      if(success = do_expression(encl_block, unr_expr->operand, &unr_expr->operand))
      {
        if(unr_expr->op == AstOpKind_AddressOf)
        {
          if(unr_expr->operand->kind == AstNodeKind_VarOccur)
            expr->type = new_pointer_type(unr_expr->operand->type);
          else
            success = compile_error(&expr->src_loc, "`&` operator can only be applied to variable occurrences");
        }
        else if(unr_expr->op == AstOpKind_Neg)
        {
          if(type_unif(unr_expr->operand->type, basic_type_int)
             || type_unif(unr_expr->operand->type, basic_type_float))
          {
            expr->type = unr_expr->operand->type;
            if(types_are_equal(unr_expr->operand->type, basic_type_float))
              unr_expr->op = AstOpKind_NegFloat;
          }
          else
            success = compile_error(&expr->src_loc,
                                    "int or float operands are expected, actual `%s`", get_type_printstr(unr_expr->operand->type));
        }
        else if(is_logic_op(unr_expr->op))
        {
          if(type_unif(unr_expr->operand->type, basic_type_bool))
            expr->type = unr_expr->operand->type;
          else
            success = compile_error(&expr->src_loc,
                                    "bool operand is expected, actual `%s`", get_type_printstr(unr_expr->operand->type));
        }
        else if(unr_expr->op == AstOpKind_PtrDeref)
        {
          Type* operand_type = unr_expr->operand->type;
          expr->type = operand_type->ptr.pointee;
        }
        else
          fail("not implemented");
      }
    }
  }
  else if(expr->kind == AstNodeKind_Id)
  {
    AstVarOccur* var_occur = new_var_occur(&expr->src_loc);
    var_occur->id = expr;

    if(success = register_var_occur(var_occur))
    {
      AstVarDecl* var_decl = (AstVarDecl*)var_occur->var_decl;
      AstBlock* decl_block = var_decl->decl_block;

      var_occur->decl_block_offset = encl_block->nesting_depth - decl_block->nesting_depth;
      var_occur->occur_block = encl_block;
      var_occur->data = &var_decl->data;

      if(var_occur->decl_block_offset > 0)
        list_append(arena, &encl_block->nonlocal_occurs, var_occur);
      else if(var_occur->decl_block_offset < 0)
        assert(false);

      *out_expr = (AstNode*)var_occur;
    }
  }
  else if(expr->kind == AstNodeKind_Literal)
  {
    AstLiteral* literal = (AstLiteral*)expr;
    if(literal->lit_kind == AstLiteralKind_Int)
      expr->type = basic_type_int;
    else if(literal->lit_kind == AstLiteralKind_Float)
      expr->type = basic_type_float;
    else if(literal->lit_kind == AstLiteralKind_Bool)
      expr->type = basic_type_bool;
    else if(literal->lit_kind == AstLiteralKind_Char)
      expr->type = basic_type_char;
    else if(literal->lit_kind == AstLiteralKind_String)
      expr->type = new_array_type(-1, basic_type_char);
    else
      assert(false);
  }
  else if(expr->kind == AstNodeKind_Call)
    success = do_call(encl_block, (AstCall*)expr);
  else if(expr->kind == AstNodeKind_Cast)
  {
    AstCast* cast = (AstCast*)expr;
    if(success = register_type_occur(cast->type_to))
    {
      cast->type = cast->type_to->type;
      if(success = do_expression(encl_block, cast->expr, &cast->expr))
      {
        if(types_are_equal(cast->type, cast->expr->type))
          *out_expr = cast->expr; /* cast is redundant; remove it */
        else
        {
          if(types_are_equal(cast->type, basic_type_int))
          {
            if(types_are_equal(cast->expr->type, basic_type_float))
            {
              AstUnrExpr* float_to_int = new_unr_expr(&cast->src_loc);
              float_to_int->op = AstOpKind_FloatToInt;
              float_to_int->type = basic_type_int;
              float_to_int->operand = cast->expr;
              *out_expr = (AstNode*)float_to_int;
            }
            else if(types_are_equal(cast->expr->type, basic_type_bool)
                    || types_are_equal(cast->expr->type, basic_type_char)
                    || cast->expr->type->kind == TypeKind_Pointer)
            {
              cast->expr->type = cast->type;
              *out_expr = cast->expr;
            }
            else
              success = compile_error(&expr->src_loc, "Conversion to int not possible");
          }
          else if(cast->type->kind == TypeKind_Pointer
                  && types_are_equal(cast->expr->type, basic_type_int))
          {
            cast->expr->type = cast->type;
            *out_expr = cast->expr;
          }
          else if(types_are_equal(cast->type, basic_type_bool)
                  && (types_are_equal(cast->expr->type, basic_type_int)
                      || types_are_equal(cast->expr->type, basic_type_char)
                      || cast->expr->type->kind == TypeKind_Pointer))
          {
            cast->expr->type = cast->type;
            *out_expr = cast->expr;
          }
          else
            success = compile_error(&expr->src_loc, "Invalid cast");
        }
      }
    }
    //FIXME: Check the type conversions
  }
  else
    fail("not implemented : %s", get_ast_kind_printstr(expr->kind));
  return success;
}

local bool32
do_proc_formal_args(AstBlock* block, List* args)
{
  bool32 success = true;

  for(ListItem* list_item = args->first;
      list_item && success;
      list_item = list_item->next)
  {
    AstVarDecl* var_decl = (AstVarDecl*)list_item->elem;
    assert(var_decl->kind == AstNodeKind_VarDecl);
    assert(var_decl->id);
    assert(!var_decl->init_expr);
    if(success = register_var_decl(var_decl))
      var_decl->decl_block = block;
  }
  return success;
}

local bool32
do_proc_ret_var(AstBlock* block, AstProc* proc)
{
  bool32 success = true;

  AstVarDecl* var_decl = new_var_decl(&proc->src_loc);
  proc->ret_var = var_decl;
  var_decl->id = make_tempvar_id(&proc->src_loc, "ret");
  var_decl->type_expr = proc->ret_type_expr;
  if(success = register_var_decl(var_decl))
    var_decl->decl_block = block;
  return success;
}

local bool32
do_proc_decl(AstProc* proc)
{
  bool32 success = true;

  if(success = scope_begin((AstBlock*)proc->body))
  {
    if(success = do_proc_formal_args(proc->body, &proc->args)
       && do_proc_ret_var(proc->body, proc)
       && do_block(proc, 0, (AstNode*)proc, proc->body))
    {
      scope_end();

      proc->type = new_proc_type(make_type_of_node_list(&proc->args), proc->ret_var->type);
      //FIXME: HACK ALERT!
      if((proc->ret_var->type != basic_type_void) && !proc->body->type && !proc->is_decl)
        success = compile_error(&proc->src_loc, "Proc must return a `%s`", get_type_printstr(proc->ret_var->type));
      else
      {
        if(proc->body->type)
          assert(type_unif(proc->ret_var->type, proc->body->type));
        proc->body->type = proc->ret_var->type;
        success = register_proc_decl(proc);
      }
      /////
    }
  }
  return success;
}

local bool32
do_statement(AstProc* encl_proc, AstBlock* block, AstNode* encl_loop, AstNode* stmt)
{
  assert(block->kind == AstNodeKind_Block);
  bool32 success = true;

  if(stmt->kind == AstNodeKind_VarDecl)
    success = do_var_decl((AstBlock*)block, (AstVarDecl*)stmt);
  else if(stmt->kind == AstNodeKind_BinExpr || stmt->kind == AstNodeKind_Call)
   success = do_expression(block, stmt, &stmt);
  else if(stmt->kind == AstNodeKind_UnrExpr)
  {
    AstUnrExpr* unr_expr = (AstUnrExpr*)stmt;
    if(unr_expr->op == AstOpKind_PostDecrement
       || unr_expr->op == AstOpKind_PreDecrement
       || unr_expr->op == AstOpKind_PostIncrement
       || unr_expr->op == AstOpKind_PreIncrement)
      success = do_expression(block, stmt, &stmt);
    else
      success = compile_error(&stmt->src_loc, "Unexpected statement %s", get_ast_kind_printstr(stmt->kind));
  }
  else if(stmt->kind == AstNodeKind_WhileStmt)
  {
    AstWhileStmt* while_stmt = (AstWhileStmt*)stmt;
    if(success = do_expression(block, while_stmt->cond_expr, &while_stmt->cond_expr))
    {
      if(type_unif(while_stmt->cond_expr->type, basic_type_bool))
      {
        if(while_stmt->body->kind == AstNodeKind_Block)
        {
          if(success = scope_begin((AstBlock*)while_stmt->body))
          {
            success = do_block(encl_proc, stmt, stmt, (AstBlock*)while_stmt->body);
            scope_end();
          }
        }
        else
          success = do_statement(encl_proc, block, stmt, while_stmt->body);
      }
      else
        success = compile_error(&stmt->src_loc, "Boolean expression is expected");
    }
  }
  else if(stmt->kind == AstNodeKind_ForStmt)
  {
    AstForStmt* for_stmt = (AstForStmt*)stmt;
    if(success = scope_begin(for_stmt->body))
    {
      success = (for_stmt->decl_expr ? do_var_decl(for_stmt->body, for_stmt->decl_expr) : 1);
      if(success)
      {
        success = do_block(encl_proc, stmt, stmt, for_stmt->body) // will set the block->owner field, so do it first
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
    AstIfStmt* if_stmt = (AstIfStmt*)stmt;
    if(success = do_expression(block, if_stmt->cond_expr, &if_stmt->cond_expr))
    {
      if(type_unif(if_stmt->cond_expr->type, basic_type_bool))
      {
        if(if_stmt->body->kind == AstNodeKind_Block)
        {
          if(success = scope_begin((AstBlock*)if_stmt->body))
          {
            success = do_block(encl_proc, encl_loop, stmt, (AstBlock*)if_stmt->body);
            scope_end();
          }
        }
        else
          success = do_statement(encl_proc, block, encl_loop, if_stmt->body);

        if(if_stmt->else_body)
        {
          if(if_stmt->else_body->kind == AstNodeKind_Block)
          {
            if(success = scope_begin((AstBlock*)if_stmt->else_body))
            {
              success = do_block(encl_proc, stmt, encl_loop, (AstBlock*)if_stmt->else_body);
              scope_end();
            }
          }
          else
            success = do_statement(encl_proc, block, encl_loop, if_stmt->else_body);
        }
      }
      else
        success = compile_error(&stmt->src_loc, "Boolean expression is expected");
    }
  }
  else if(stmt->kind == AstNodeKind_ReturnStmt)
  {
    if(encl_proc)
    {
      AstReturnStmt* ret_stmt = (AstReturnStmt*)stmt;
      ret_stmt->proc = encl_proc;
      AstBlock* encl_proc_block = (AstBlock*)encl_proc->body;
      ret_stmt->nesting_depth = ((AstBlock*)block)->nesting_depth - encl_proc_block->nesting_depth;
      assert(!stmt->type);
      stmt->type = basic_type_void;

      AstNode* ret_expr = ret_stmt->expr;
      AstVarDecl* ret_var = encl_proc->ret_var;

      if(ret_expr)
      {
        AstBinExpr* bin_expr = new_bin_expr(&stmt->src_loc);
        ret_stmt->assign_expr = bin_expr;
        bin_expr->op = AstOpKind_Assign;
        bin_expr->left_operand = (AstNode*)new_id(&ret_expr->src_loc, ret_var->id->name);
        bin_expr->right_operand = ret_expr;

        if(success = do_expression(block, (AstNode*)bin_expr, &(AstNode*)bin_expr))
          stmt->type = bin_expr->type;
      }
      else if(!(success = type_unif(ret_var->type, stmt->type)))
      {
        success = compile_error(&stmt->src_loc,
                                "return type : expected `%s`, actual `%s`", get_type_printstr(ret_var->type), get_type_printstr(stmt->type));
      }

      if(success)
      {
        //FIXME: HACK ALERT!!
        assert(type_unif(stmt->type, encl_proc->ret_var->type));
        if(encl_proc->body->type)
          success = type_unif(encl_proc->body->type, stmt->type);
        else
          encl_proc->body->type = stmt->type;
        /////
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
    if(encl_loop)
    {
      AstNode* loop_body = 0;
      if(encl_loop->kind == AstNodeKind_WhileStmt)
        loop_body = ((AstWhileStmt*)encl_loop)->body;
      else if(encl_loop->kind = AstNodeKind_ForStmt)
        loop_body = (AstNode*)((AstForStmt*)encl_loop)->body;
      assert(loop_body);

      AstLoopCtrl* loop_ctrl = (AstLoopCtrl*)encl_loop;
      loop_ctrl->nesting_depth = 0;
      if(loop_body->kind == AstNodeKind_Block)
        loop_ctrl->nesting_depth = ((AstBlock*)block)->nesting_depth - ((AstBlock*)loop_body)->nesting_depth;
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
do_block(AstProc* encl_proc, AstNode* encl_loop, AstNode* owner, AstBlock* block)
{
  bool32 success = true;

  //block->block.owner = owner;

  if(owner && (owner->kind == AstNodeKind_Module))
  {
    for(ListItem* list_item = block->node_list.first;
        list_item && success;
        list_item = list_item->next)
    {
      AstNode* stmt = (AstNode*)list_item->elem;
      if(stmt->kind == AstNodeKind_VarDecl)
        success = do_var_decl(block, (AstVarDecl*)stmt);
      else if(stmt->kind == AstNodeKind_Proc)
        success = do_proc_decl((AstProc*)stmt);
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
    for(ListItem* list_item = block->node_list.first;
        list_item && success;
        list_item = list_item->next)
    {
      success = do_statement(encl_proc, block, encl_loop, (AstNode*)list_item->elem);
    }
  }
  else
    assert(false);
  return success;
}

local bool32
do_module(AstModule* module)
{
  bool32 success = true;
  AstBlock* module_block = module->body;

  {
    // process the includes
    for(ListItem* list_item = module_block->node_list.first;
        list_item;
        list_item = list_item->next)
    {
      AstNode* stmt = (AstNode*)list_item->elem;
      if(stmt->kind == AstNodeKind_IncludeStmt)
      {
        AstBlock* incl_block = (AstBlock*)((AstIncludeStmt*)stmt)->body;
        do_include_stmt(&incl_block->node_list, &module_block->node_list, list_item);
      }
    }
  }

  if(success = scope_begin(module_block))
  {
    add_builtin_types();

    if(success = do_block(0, 0, (AstNode*)module, module->body))
    {
      AstCall* main_call = new_call(deflt_src_loc);
      main_call->id = new_id(deflt_src_loc, "main");
      if(success = do_call(module_block, main_call))
      {
        if(type_unif(main_call->type, basic_type_int))
          module->main_stmt = main_call;
        else
        {
          AstProc* proc = (AstProc*)main_call->proc_sym->ast;
          success = compile_error(&proc->src_loc, "main() must return int");
        }
      }
    }
    scope_end();
  }
  return success;
}

bool32
semantic_analysis(AstModule* module)
{
  init_types();

  symtab = mem_push_struct(arena, SymbolTable);

  bool32 success = true;
  success = do_module(module);

  return success;
}


