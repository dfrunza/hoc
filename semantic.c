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

local bool32 do_stmt_block(AstBlock*, AstProc*, AstNode*, AstBlock*);
local bool32 do_expression(AstBlock*, AstBlock*, AstNode*, AstNode**);
local bool32 do_type_expr(AstNode* expr);

local void
make_type_printstr(String* str, Type* type)
{
  if(type->kind == TypeKind_Basic)
  {
    if(type->basic.kind == BasicTypeKind_Bool)
      str_append(str, "bool");
    else if(type->basic.kind == BasicTypeKind_Int)
      str_append(str, "int");
    else if(type->basic.kind == BasicTypeKind_Float)
      str_append(str, "float");
    else if(type->basic.kind == BasicTypeKind_Char)
      str_append(str, "char");
    else if(type->basic.kind == BasicTypeKind_Void)
      str_append(str, "void");
  }
  else if(type->kind == TypeKind_Pointer)
  {
    make_type_printstr(str, type->pointer.pointee);
    str_append(str, "*");
  }
  else if(type->kind == TypeKind_Array)
  {
    str_append(str, "[]");
    make_type_printstr(str, type->array.elem);
  }
  else if(type->kind == TypeKind_Product)
  {
    make_type_printstr(str, type->product.left);
    str_append(str, ", ");
    make_type_printstr(str, type->product.right);
  }
  else if(type->kind == TypeKind_Proc)
  {
    make_type_printstr(str, type->proc.ret);
    str_append(str, " (");
    make_type_printstr(str, type->proc.args);
    str_append(str, ")");
  }
  else
    fail("not implemented");
}

local char*
get_type_printstr(Type* type)
{
  String* str = str_new(arena);
  make_type_printstr(str, type);
  return str_cap(str);
}

bool32
is_arithmetic_op(AstOpKind op)
{
  return op == AstOpKind_Add || op == AstOpKind_Sub
    || op == AstOpKind_Mul || op == AstOpKind_Div
    || op == AstOpKind_Mod;
}

bool32
is_logic_op(AstOpKind op)
{
  return op == AstOpKind_LogicOr || op == AstOpKind_LogicAnd || op == AstOpKind_LogicNot;
}

bool32
is_relation_op(AstOpKind op)
{
  return op == AstOpKind_Equals || op == AstOpKind_NotEquals
      || op == AstOpKind_Less || op == AstOpKind_LessEquals
      || op == AstOpKind_Greater || op == AstOpKind_GreaterEquals;
}

local AstId*
make_tempvar_id(SourceLocation* src_loc, char* label)
{
  String* str = str_new(arena);
  str_printf(str, "$%s%d", label, tempvar_id++);
  return new_id(src_loc, str->head);
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
register_type_occur(AstId* type_id)
{
  bool32 success = true;

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
    success = compile_error(&type_id->src_loc, "unknown type `%s`", type_id->name);

  return success;
}

//TODO: Move code into `do_var_occur()` and leave here only the id registration parts
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
    success = compile_error(&var_occur->src_loc, "unknown var `%s`", id->name);
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
    success = compile_error(&id->src_loc, "identifier `%s` of same kind already declared...", id->name);
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
  if(success = do_type_expr(type_expr))
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
        success = compile_error(&proc->src_loc, "inconsistent proc signature...");
        compile_error(&registered_proc->src_loc, "...see previous decl");
      }
    }
    else if(!registered_proc->is_decl && !proc->is_decl)
    {
      success = compile_error(&proc->src_loc, "proc redefinition...");
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
        success = compile_error(&proc->src_loc, "inconsistent proc type...");
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
    assert(proc_sym->ast->kind == AstNodeKind_Proc);
    call->proc = (AstProc*)proc_sym->ast;

    Symbol* call_sym = register_id(id, SymbolKind_Call);
    call_sym->ast = (AstNode*)call;
  }
  else
    success = compile_error(&call->src_loc, "unknown proc `%s`", id->name);
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
  symtab->block_id = ++last_block_id; 
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
    compile_error(&block->src_loc, "exceeded max scope nesting depth : %d", sizeof_array(symtab->active_blocks));
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
  list_replace_item_at(include_list, module_list, module_list_item);

  mem_zero_struct(include_list, List);
  list_init(include_list);
}

local bool32
do_var_decl(AstBlock* module_block, AstBlock* block, AstVarDecl* var_decl)
{
  bool32 success = true;

  if(success = register_var_decl(var_decl))
  {
    var_decl->decl_block = block;
    list_append(arena, &block->local_decls, var_decl);

    if(var_decl->init_expr)
    {
      AstBinExpr* bin_expr = new_bin_expr(&var_decl->src_loc); 
      var_decl->assign_expr = bin_expr;
      bin_expr->op = AstOpKind_Assign;
      bin_expr->left_operand = (AstNode*)new_id(&var_decl->src_loc, var_decl->id->name);;
      bin_expr->right_operand = var_decl->init_expr;

      success = do_expression(module_block, block, (AstNode*)bin_expr, &(AstNode*)bin_expr);
    }
  }
  return success;
}

local bool32
do_call_args(AstBlock* module_block, AstBlock* block, AstCall* call)
{
  bool32 success = true;

  List* arg_list = &call->args.list;
  ListItem* first_item = arg_list->first;
  if(first_item)
  {
    AstNode* arg = (AstNode*)first_item->elem;
    if(success = do_expression(module_block, block, arg, &arg))
    {
      first_item->elem = arg;

      for(ListItem* list_item = first_item->next;
          list_item && success;
          list_item = list_item->next)
      {
        arg = (AstNode*)list_item->elem;
        if(success = do_expression(module_block, block, arg, &arg))
        {
          list_item->elem = arg;
        }
      }
    }
  }
  return success;
}

local bool32
type_convert_call_arg(AstVarDecl* formal_arg,
                      AstVarOccur* actual_arg, AstNode** node_out,
                      Type* type_from, Type* type_to)
{
  bool32 success = true;
  *node_out = (AstNode*)actual_arg;

  if(types_are_equal(type_from, type_to))
    return success;

  if(type_from->kind == TypeKind_Array && type_to->kind == TypeKind_Pointer)
  {
    if(type_unif(type_from->array.elem, type_to->pointer.pointee))
    {
      AstUnrExpr* address_of = new_unr_expr(&actual_arg->src_loc);
      address_of->op = AstOpKind_AddressOf;
      //address_of->type = new_pointer_type(type_from->array.elem);
      address_of->type = new_pointer_type(type_from); // pointer(array)
      address_of->operand = (AstNode*)actual_arg;
      *node_out = (AstNode*)address_of;
    }
    else
    {
      success = compile_error(&actual_arg->src_loc,
          "array element `%s` and pointee `%s` types are different",
          get_type_printstr(type_from->array.elem), get_type_printstr(type_to->pointer.pointee));
    }
  }
  else if(type_from->kind == TypeKind_Pointer && type_to->kind == TypeKind_Array)
  {
#if 1
    AstUnrExpr* deref_ptr = new_unr_expr(&actual_arg->src_loc);
    deref_ptr->op = AstOpKind_PointerDeref;
    deref_ptr->type = new_array_type(type_to->array.size, type_from->pointer.pointee);
    deref_ptr->operand = (AstNode*)actual_arg;
    *node_out = (AstNode*)deref_ptr;
#else
    AstVarDecl* var_decl = (AstVarDecl*)clone_ast_node((AstNode*)formal_arg);
    var_decl->type = new_array_type(type_to->array.size, type_from->pointer.pointee);
    if(actual_arg->kind == AstNodeKind_VarOccur)
    {
      AstVarOccur* var_occur = (AstVarOccur*)clone_ast_node((AstNode*)actual_arg);
      var_occur->type = new_array_type(type_to->array.size, type_from->pointer.pointee);
      *node_out = (AstNode*)var_occur;
    }
    else
      fail("not idea what to do here");
#endif
  }
  else
    fail("no idea what to do here");
  return success;
}

local bool32
do_call(AstBlock* module_block, AstBlock* block, AstCall* call)
{
  bool32 success = true;

  if(success = do_call_args(module_block, block, call) && register_call(call))
  {
    AstProc* proc = call->proc;
    Type* proc_type = proc->type;
    assert(proc_type->kind == TypeKind_Proc);

    if(call->args.count == proc->args.count)
    {
      if(call->args.count >= 1)
      {
        ListItem* actual_arg_li = call->args.list.first;
        ListItem* formal_arg_li = proc->args.list.first;
        AstNode* actual_arg = (AstNode*)actual_arg_li->elem;
        AstNode* formal_arg = (AstNode*)formal_arg_li->elem;

        call->args.type = actual_arg->type;

        while(success && actual_arg_li && formal_arg_li)
        {
          actual_arg = (AstNode*)actual_arg_li->elem;
          formal_arg = (AstNode*)formal_arg_li->elem;

          if(type_unif(actual_arg->type, formal_arg->type))
          {
            call->args.type = new_product_type(call->args.type, actual_arg->type);
          }
          else
          {
            if(type_convert_call_arg((AstVarDecl*)formal_arg, (AstVarOccur*)actual_arg, &actual_arg, actual_arg->type, formal_arg->type))
            {
              call->args.type = new_product_type(call->args.type, actual_arg->type);
              actual_arg_li->elem = (AstNode*)actual_arg;
            }
            else
            {
              success = compile_error(&actual_arg->src_loc,
                  "no implicit conversion from `%s` to `%s`",
                  get_type_printstr(actual_arg->type), get_type_printstr(formal_arg->type));
            }
          }

          actual_arg_li = actual_arg_li->next;
          formal_arg_li = formal_arg_li->next;
        }
      }
      else if(call->args.count == 0)
      {
        call->args.type = basic_type_void;
      }
      else
        assert(0);
    }
    else
    {
      success = compile_error(&call->src_loc,
          "incorrect number of arguments in call `%s %s(%s)`",
          get_type_printstr(proc_type->proc.ret), call->id->name, get_type_printstr(call->args.type));
    }

    if(success)
      call->type = proc_type->proc.ret;
#if 0
    Type* call_type = new_proc_type(call->args.type, proc_type->proc.ret);
    if(type_unif(proc_type, call_type))
    {
      call->type = proc_type->proc.ret;
    }
    else
    {
      success = compile_error(&call->src_loc,
          "call `%s %s(%s)` does not match proc type...",
          get_type_printstr(call_type->proc.ret), call->id->name, get_type_printstr(call_type->proc.args));
      compile_error(&proc->src_loc, "...see proc decl");
    }
#endif
  }
  return success;
}

local bool32
do_type_expr(AstNode* expr)
{
  bool32 success = false;

  if(expr->kind == AstNodeKind_Id)
  {
    AstId* type_id = (AstId*)expr;
    if(success = register_type_occur(type_id))
      expr->type = type_id->type;
  }
  else if(expr->kind == AstNodeKind_Array)
  {
    AstArray* array = (AstArray*)expr;
    if(success = do_type_expr(array->expr))
    {
      array->size = -1;
      if(array->index)
      {
        if(array->index->kind == AstNodeKind_Literal)
        {
          AstLiteral* size = (AstLiteral*)array->index;
          if(size->lit_kind == AstLiteralKind_Int)
            array->size = size->int_val;
          else
            success = compile_error(&expr->src_loc, "int literal expected");
        }
        else
          success = compile_error(&expr->src_loc, "int literal expected");
      }

      array->type = new_array_type(array->size, array->expr->type);
    }
  }
  else if(expr->kind == AstNodeKind_Pointer)
  {
    AstPointer* pointer = (AstPointer*)expr;
    if(success = do_type_expr(pointer->expr))
      expr->type = new_pointer_type(pointer->expr->type);
  }
  else
    assert(0);

  return success;
}

local bool32
do_expression(AstBlock* module_block,
              AstBlock* block,
              AstNode* expr,
              AstNode** out_expr)
{
  bool32 success = true;
  *out_expr = expr;

  if(expr->kind == AstNodeKind_BinExpr)
  {
    AstBinExpr* bin_expr = (AstBinExpr*)expr;
    if(success = do_expression(module_block, block, bin_expr->left_operand, &bin_expr->left_operand)
      && do_expression(module_block, block, bin_expr->right_operand, &bin_expr->right_operand))
    {
      Type* left_type = bin_expr->left_operand->type;
      Type* right_type = bin_expr->right_operand->type;
      bin_expr->type = left_type;

      if(type_unif(left_type, right_type))
      {
        if(is_arithmetic_op(bin_expr->op) || is_relation_op(bin_expr->op))
        {
          if(types_are_equal(bin_expr->type, basic_type_int)
              || types_are_equal(bin_expr->type, basic_type_float)
              || types_are_equal(bin_expr->type, basic_type_char)
              || bin_expr->type->kind == TypeKind_Pointer)
          {
            if(is_relation_op(bin_expr->op))
              expr->type = basic_type_bool;

            if(bin_expr->op == AstOpKind_Mod && types_are_equal(bin_expr->type, basic_type_float))
              success = compile_error(&expr->src_loc, "modulo operator cannot be applied to float operands");
          }
          else
            success = compile_error(&expr->src_loc,
                                    "int, float or char operands are expected, actual `%s`", get_type_printstr(bin_expr->type));
        }
        else if(is_logic_op(bin_expr->op) && !types_are_equal(bin_expr->type, basic_type_bool))
          success = compile_error(&expr->src_loc,
                                  "bool operands are expected, actual `%s`", get_type_printstr(bin_expr->type));
      }
      else if(types_are_equal(right_type, basic_type_int))
      {
        if(types_are_equal(left_type, basic_type_float))
        {
          if(bin_expr->op != AstOpKind_Mod)
          {
            AstUnrExpr* int_to_float = new_unr_expr(&bin_expr->right_operand->src_loc);
            int_to_float->op = AstOpKind_IntToFloat;
            int_to_float->type = basic_type_float;
            int_to_float->operand = bin_expr->right_operand;
            bin_expr->right_operand = (AstNode*)int_to_float;
          }
          else
            success = compile_error(&expr->src_loc, "modulo operator cannot be applied to float operands");
        }
        else if(types_are_equal(left_type, basic_type_bool))
        {
          bin_expr->left_operand->type = basic_type_bool;
        }
        else if(left_type->kind == TypeKind_Array)
        {
          if(bin_expr->op == AstOpKind_Add || bin_expr->op == AstOpKind_Sub)
          {
            AstUnrExpr* address_of = new_unr_expr(&bin_expr->src_loc);
            address_of->op = AstOpKind_AddressOf;
            //address_of->type = new_pointer_type(left_type->array.elem);
            address_of->type = new_pointer_type(left_type); // pointer(array)
            address_of->operand = bin_expr->left_operand;
            bin_expr->left_operand = (AstNode*)address_of;
            bin_expr->type = address_of->type;
          }
        }
        else if(left_type->kind == TypeKind_Pointer)
          ;/*no-op*/
        else
          success = compile_error(&expr->src_loc,
              "no implicit conversion from `%s` to `%s`", get_type_printstr(right_type), get_type_printstr(left_type));
      }
      else if(types_are_equal(right_type, basic_type_char))
      {
        if(types_are_equal(left_type, basic_type_int))
          bin_expr->right_operand->type = basic_type_int;
        else
          success = compile_error(&expr->src_loc,
              "no implicit conversion from `%s` to `%s`", get_type_printstr(right_type), get_type_printstr(left_type));
      }
      else if(left_type->kind == TypeKind_Pointer)
      {
        if(right_type->kind == TypeKind_Pointer
           && (types_are_equal(right_type->pointer.pointee, basic_type_void)))
        {
          bin_expr->right_operand->type = bin_expr->left_operand->type;
        }
        else if(right_type->kind == TypeKind_Array)
        {
          if(bin_expr->op == AstOpKind_Assign)
          {
            if(type_unif(left_type->pointer.pointee, right_type->array.elem))
            {
              AstUnrExpr* address_of = new_unr_expr(&bin_expr->src_loc);
              address_of->op = AstOpKind_AddressOf;
              //address_of->type = new_pointer_type(right_type->array.elem);
              address_of->type = new_pointer_type(right_type); // pointer(array)
              address_of->operand = bin_expr->right_operand;
              bin_expr->right_operand = (AstNode*)address_of;
            }
            else
              success = compile_error(&expr->src_loc,
                    "no implicit conversion from `%s` to `%s`", get_type_printstr(left_type), get_type_printstr(right_type));
          }
          else
            success = compile_error(&expr->src_loc,
                "no implicit conversion from `%s` to `%s`", get_type_printstr(left_type), get_type_printstr(right_type));
        }
        else if(types_are_equal(right_type, basic_type_int))
        {
          if(bin_expr->right_operand->kind == AstNodeKind_Literal)
          {
            AstLiteral* lit = (AstLiteral*)bin_expr;
            assert(lit->lit_kind == AstLiteralKind_Int);
            if(lit->int_val != 0)
              success = compile_error(&expr->src_loc,
                  "no implicit conversion from `%s` to `%s`", get_type_printstr(left_type), get_type_printstr(right_type));
          }
        }
        else
          success = compile_error(&expr->src_loc,
                "no implicit conversion from `%s` to `%s`", get_type_printstr(left_type), get_type_printstr(right_type));
      }
      else if(left_type->kind == TypeKind_Array)
      {
        if(right_type->kind == TypeKind_Pointer || right_type->kind == TypeKind_Array)
          bin_expr->right_operand->type = bin_expr->left_operand->type;
      }
      else
        success = compile_error(&expr->src_loc,
              "incompatible types in expression, `%s` and `%s`", get_type_printstr(left_type), get_type_printstr(right_type));
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
      if(success = do_expression(block, (AstNode*)assign, &(AstNode*)assign))
        *out_expr = (AstNode*)assign;
#else
      fail("not implemented");
#endif
    }
    else 
    {
      if(success = do_expression(module_block, block, unr_expr->operand, &unr_expr->operand))
      {
        if(unr_expr->op == AstOpKind_AddressOf)
        {
          if(unr_expr->operand->kind == AstNodeKind_VarOccur)
          {
            Type* operand_type = unr_expr->operand->type;
            if(operand_type->kind == TypeKind_Array)
              //unr_expr->type = new_pointer_type(operand_type->array.elem); // ptr to element of array
              unr_expr->type = new_pointer_type(operand_type); // pointer(array)
            else
              unr_expr->type = new_pointer_type(operand_type);
          }
          else
            success = compile_error(&expr->src_loc, "`&` operator can only be applied to variable occurrences");
        }
        else if(unr_expr->op == AstOpKind_Neg)
        {
          if(type_unif(unr_expr->operand->type, basic_type_int)
             || type_unif(unr_expr->operand->type, basic_type_float))
          {
            expr->type = unr_expr->operand->type;
#if 0
            if(types_are_equal(unr_expr->operand->type, basic_type_float))
              unr_expr->op = AstOpKind_NegFloat;
#endif
          }
          else
            success = compile_error(&expr->src_loc,
                                    "int or float operands are expected, actual `%s`", get_type_printstr(unr_expr->operand->type));
        }
        else if(is_logic_op(unr_expr->op))
        {
          if(type_unif(unr_expr->operand->type, basic_type_bool))
            unr_expr->type = unr_expr->operand->type;
          else
            success = compile_error(&expr->src_loc,
                                    "bool operand is expected, actual `%s`", get_type_printstr(unr_expr->operand->type));
        }
        else if(unr_expr->op == AstOpKind_PointerDeref)
        {
          Type* operand_type = unr_expr->operand->type;
          if(operand_type->kind == TypeKind_Pointer)
          {
            operand_type = operand_type->pointer.pointee;
            unr_expr->type = operand_type;
          }

          if(operand_type->kind == TypeKind_Array)
          {
            AstUnrExpr* address_of = new_unr_expr(&unr_expr->src_loc);
            address_of->op = AstOpKind_AddressOf;
            address_of->type = new_pointer_type(operand_type->array.elem);

            address_of->operand = unr_expr->operand;
            unr_expr->operand = (AstNode*)address_of;
            unr_expr->type = operand_type->array.elem;
          }
          else
            success = compile_error(&expr->src_loc,
                "pointer or array type expected, actual `%s`", get_type_printstr(unr_expr->operand->type));
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

      var_occur->decl_block_offset = block->nesting_depth - decl_block->nesting_depth;
      if(var_occur->decl_block_offset > 0 && decl_block == module_block)
        var_occur->decl_block_offset = -1; // global

      var_occur->occur_block = block;
      var_occur->data = &var_decl->data;

      if(var_occur->decl_block_offset > 0)
        list_append(arena, &block->nonlocal_occurs, var_occur);

      *out_expr = (AstNode*)var_occur;
    }
  }
  else if(expr->kind == AstNodeKind_Literal)
  {
    AstLiteral* lit = (AstLiteral*)expr;
    if(lit->lit_kind == AstLiteralKind_Int)
      expr->type = basic_type_int;
    else if(lit->lit_kind == AstLiteralKind_Float)
      expr->type = basic_type_float;
    else if(lit->lit_kind == AstLiteralKind_Bool)
      expr->type = basic_type_bool;
    else if(lit->lit_kind == AstLiteralKind_Char)
      expr->type = basic_type_char;
    else if(lit->lit_kind == AstLiteralKind_String)
    {
#if 1
      AstString* str = new_string(&lit->src_loc, lit->str);
      str->len = cstr_len(str->str);
      str->type = new_array_type(str->len+1/*NULL*/, basic_type_char);
#else
      int len = cstr_len(lit->str);
      lit->type = new_array_type(len+1/*NULL*/, basic_type_char);
#endif

      AstVarDecl* var_decl = new_var_decl(&expr->src_loc);
      var_decl->type = str->type;
      var_decl->init_expr = (AstNode*)str;
      var_decl->decl_block = module_block;
      list_append(arena, &module_block->local_decls, var_decl);

      AstVarOccur* var_occur = new_var_occur(&expr->src_loc);
      var_occur->var_decl = var_decl;
      var_occur->type = var_decl->type;

      var_occur->decl_block_offset = -1; // global
      var_occur->occur_block = block;
      var_occur->data = &var_decl->data;

      if(var_occur->decl_block_offset > 0)
        list_append(arena, &block->nonlocal_occurs, var_occur);

      *out_expr = (AstNode*)var_occur;
    }
    else
      assert(0);
  }
  else if(expr->kind == AstNodeKind_Call)
    success = do_call(module_block, block, (AstCall*)expr);
  else if(expr->kind == AstNodeKind_Cast)
  {
    AstCast* cast = (AstCast*)expr;
    if(success = do_type_expr(cast->type_expr))
    {
      if(success = do_expression(module_block, block, cast->expr, &cast->expr))
      {
        Type* from_type = cast->expr->type;
        Type* to_type = cast->type_expr->type;

        if(types_are_equal(from_type, to_type))
          *out_expr = cast->expr; /* cast is redundant; remove it */
        else
        {
          if(types_are_equal(to_type, basic_type_int))
          {
            if(types_are_equal(from_type, basic_type_float))
            {
              AstUnrExpr* float_to_int = new_unr_expr(&cast->src_loc);
              float_to_int->op = AstOpKind_FloatToInt;
              float_to_int->type = basic_type_int;
              float_to_int->operand = cast->expr;
              *out_expr = (AstNode*)float_to_int;
            }
            else if(types_are_equal(from_type, basic_type_bool)
                    || types_are_equal(from_type, basic_type_char)
                    || to_type->kind == TypeKind_Pointer)
            {
              cast->expr->type = to_type;
              *out_expr = cast->expr;
            }
            else
              success = compile_error(&expr->src_loc, "conversion to int not possible");
          }
          else if(to_type->kind == TypeKind_Pointer
                  && (types_are_equal(from_type, basic_type_int)
                      || from_type->kind == TypeKind_Pointer))
          {
            cast->expr->type = to_type;
            *out_expr = cast->expr;
          }
          else if(types_are_equal(to_type, basic_type_bool)
                  && (types_are_equal(from_type, basic_type_int)
                      || types_are_equal(from_type, basic_type_char)
                      || from_type->kind == TypeKind_Pointer))
          {
            cast->expr->type = to_type;
            *out_expr = cast->expr;
          }
          else
            success = compile_error(&expr->src_loc, "invalid cast : `%s` to `%s`",
                                    get_type_printstr(from_type), get_type_printstr(to_type));
        }
      }
    }
  }
  else if(expr->kind == AstNodeKind_New)
  {
    AstNew* new_ast = (AstNew*)expr;
    if(success = do_type_expr(new_ast->type_expr))
    {
      Type* expr_type = new_ast->type_expr->type;
      if(expr_type->kind == TypeKind_Array)
        //new_ast->type = new_pointer_type(expr_type->array.elem);
        new_ast->type = new_pointer_type(expr_type); // pointer(array)
      else
        new_ast->type = new_pointer_type(new_ast->type_expr->type);
    }
  }
  else
    fail("not implemented : %s", get_ast_kind_printstr(expr->kind));
  return success;
}

local bool32
do_proc_formal_args(AstBlock* block, AstProc* proc)
{
  bool32 success = true;

  Type* args_type = basic_type_void;
  List* args_list = &proc->args.list;
  ListItem* first_item = args_list->first;
  if(first_item)
  {
    AstVarDecl* var_decl = (AstVarDecl*)first_item->elem;
    if(success = register_var_decl(var_decl))
    {
      var_decl->decl_block = block;
      args_type = var_decl->type;

      for(ListItem* list_item = first_item->next;
          list_item && success;
          list_item = list_item->next)
      {
        var_decl = (AstVarDecl*)list_item->elem;
        assert(var_decl->kind == AstNodeKind_VarDecl);
        assert(var_decl->id);
        assert(!var_decl->init_expr);
        if(success = register_var_decl(var_decl))
        {
          var_decl->decl_block = block;
          args_type = new_product_type(args_type, var_decl->type);
        }
      }
    }
  }
  proc->args.type = args_type;
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
do_proc_decl(AstBlock* module_block, AstProc* proc)
{
  bool32 success = true;

  if(success = scope_begin((AstBlock*)proc->body))
  {
    if(success = do_proc_formal_args(proc->body, proc)
       && do_proc_ret_var(proc->body, proc)
       && do_stmt_block(module_block, proc, 0, proc->body))
    {
      scope_end();

      proc->type = new_proc_type(proc->args.type, proc->ret_var->type);
      //FIXME: HACK ALERT!
      if((proc->ret_var->type != basic_type_void) && !proc->body->type && !proc->is_decl)
        success = compile_error(&proc->src_loc, "proc must return a `%s`", get_type_printstr(proc->ret_var->type));
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
do_statement(AstBlock* module_block,
             AstProc* proc,
             AstBlock* block,
             AstNode* loop,
             AstNode* stmt)
{
  bool32 success = true;

  if(stmt->kind == AstNodeKind_VarDecl)
    success = do_var_decl(module_block, block, (AstVarDecl*)stmt);
  else if(stmt->kind == AstNodeKind_BinExpr || stmt->kind == AstNodeKind_Call)
    success = do_expression(module_block, block, stmt, &stmt);
  else if(stmt->kind == AstNodeKind_UnrExpr)
  {
    AstUnrExpr* unr_expr = (AstUnrExpr*)stmt;
    if(unr_expr->op == AstOpKind_PostDecrement
       || unr_expr->op == AstOpKind_PreDecrement
       || unr_expr->op == AstOpKind_PostIncrement
       || unr_expr->op == AstOpKind_PreIncrement)
      success = do_expression(module_block, block, stmt, &stmt);
    else
      success = compile_error(&stmt->src_loc, "unexpected statement %s", get_ast_kind_printstr(stmt->kind));
  }
  else if(stmt->kind == AstNodeKind_WhileStmt)
  {
    AstWhileStmt* while_stmt = (AstWhileStmt*)stmt;
    if(success = do_expression(module_block, block, while_stmt->cond_expr, &while_stmt->cond_expr))
    {
      if(type_unif(while_stmt->cond_expr->type, basic_type_bool))
      {
        if(while_stmt->body->kind == AstNodeKind_Block)
        {
          if(success = scope_begin((AstBlock*)while_stmt->body))
          {
            success = do_stmt_block(module_block, proc, stmt, (AstBlock*)while_stmt->body);
            scope_end();
          }
        }
        else
          success = do_statement(module_block, proc, block, stmt, while_stmt->body);
      }
      else
        success = compile_error(&stmt->src_loc, "boolean expression is expected");
    }
  }
  else if(stmt->kind == AstNodeKind_ForStmt)
  {
    AstForStmt* for_stmt = (AstForStmt*)stmt;
    if(success = scope_begin(for_stmt->body))
    {
      success = (for_stmt->decl_expr ? do_var_decl(module_block, for_stmt->body, for_stmt->decl_expr) : 1);
      if(success)
      {
        success = do_stmt_block(module_block, proc, stmt, for_stmt->body)
          && (for_stmt->cond_expr ? do_expression(module_block, for_stmt->body, for_stmt->cond_expr, &for_stmt->cond_expr) : 1)
          && (for_stmt->loop_expr ? do_expression(module_block, for_stmt->body, for_stmt->loop_expr, &for_stmt->loop_expr) : 1);

        if(success)
        {
          if(!type_unif(for_stmt->cond_expr->type, basic_type_bool))
            success = compile_error(&stmt->src_loc, "boolean expression is expected");
        }
      }
      scope_end();
    }
  }
  else if(stmt->kind == AstNodeKind_IfStmt)
  {
    AstIfStmt* if_stmt = (AstIfStmt*)stmt;
    if(success = do_expression(module_block, block, if_stmt->cond_expr, &if_stmt->cond_expr))
    {
      if(type_unif(if_stmt->cond_expr->type, basic_type_bool))
      {
        if(if_stmt->body->kind == AstNodeKind_Block)
        {
          if(success = scope_begin((AstBlock*)if_stmt->body))
          {
            success = do_stmt_block(module_block, proc, loop, (AstBlock*)if_stmt->body);
            scope_end();
          }
        }
        else
          success = do_statement(module_block, proc, block, loop, if_stmt->body);

        if(success)
        {
          if(if_stmt->else_body)
          {
            if(if_stmt->else_body->kind == AstNodeKind_Block)
            {
              if(success = scope_begin((AstBlock*)if_stmt->else_body))
              {
                success = do_stmt_block(module_block, proc, stmt, (AstBlock*)if_stmt->else_body);
                scope_end();
              }
            }
            else
              success = do_statement(module_block, proc, block, loop, if_stmt->else_body);
          }
        }
      }
      else
        success = compile_error(&stmt->src_loc, "boolean expression is expected");
    }
  }
  else if(stmt->kind == AstNodeKind_ReturnStmt)
  {
    if(proc)
    {
      AstReturnStmt* ret_stmt = (AstReturnStmt*)stmt;
      ret_stmt->proc = proc;
      AstBlock* encl_proc_block = (AstBlock*)proc->body;
      ret_stmt->nesting_depth = ((AstBlock*)block)->nesting_depth - encl_proc_block->nesting_depth;
      assert(!stmt->type);
      stmt->type = basic_type_void;

      AstNode* ret_expr = ret_stmt->expr;
      AstVarDecl* ret_var = proc->ret_var;

      if(ret_expr)
      {
        AstBinExpr* bin_expr = new_bin_expr(&stmt->src_loc);
        ret_stmt->assign_expr = bin_expr;
        bin_expr->op = AstOpKind_Assign;
        bin_expr->left_operand = (AstNode*)new_id(&ret_expr->src_loc, ret_var->id->name);
        bin_expr->right_operand = ret_expr;

        if(success = do_expression(module_block, block, (AstNode*)bin_expr, &(AstNode*)bin_expr))
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
        assert(type_unif(stmt->type, proc->ret_var->type));
        if(proc->body->type)
          success = type_unif(proc->body->type, stmt->type);
        else
          proc->body->type = stmt->type;
        /////
      }
    }
    else
      success = compile_error(&stmt->src_loc, "unexpected `return` at this location");
  }
  else if(stmt->kind == AstNodeKind_Id ||
          stmt->kind == AstNodeKind_Literal)
  {
    success = compile_error(&stmt->src_loc, "unexpected statement `%s` at this location", get_ast_kind_printstr(stmt->kind));
  }
  else if(stmt->kind == AstNodeKind_BreakStmt || stmt->kind == AstNodeKind_ContinueStmt)
  {
    if(loop)
    {
      AstNode* loop_body = 0;
      if(loop->kind == AstNodeKind_WhileStmt)
        loop_body = ((AstWhileStmt*)loop)->body;
      else if(loop->kind = AstNodeKind_ForStmt)
        loop_body = (AstNode*)((AstForStmt*)loop)->body;
      assert(loop_body);

      AstLoopCtrl* loop_ctrl = (AstLoopCtrl*)stmt;
      loop_ctrl->loop = loop;
      loop_ctrl->nesting_depth = 0;
      if(loop_body->kind == AstNodeKind_Block)
        loop_ctrl->nesting_depth = block->nesting_depth - ((AstBlock*)loop_body)->nesting_depth;
      else
        assert(loop_body == stmt);
    }
    else
      success = compile_error(&stmt->src_loc, "unexpected `%s` at this location", get_ast_kind_printstr(stmt->kind));
  }
  else if(stmt->kind == AstNodeKind_GotoStmt
          || stmt->kind == AstNodeKind_Label
          || stmt->kind == AstNodeKind_Cast)
  {
    fail("not implemented : %s\n", get_ast_kind_printstr(stmt->kind));
  }
  else if(stmt->kind == AstNodeKind_EmptyStmt)
  {
    ;/*ok*/
  }
  else if(stmt->kind == AstNodeKind_New)
  {
    fail("");
  }
  else if(stmt->kind == AstNodeKind_Putc)
  {
    AstPutc* putc_ast = (AstPutc*)stmt;
    if(putc_ast->expr)
    {
      if(success = do_expression(module_block, block, putc_ast->expr, &putc_ast->expr))
      {
        if(types_are_equal(putc_ast->expr->type, basic_type_char))
          putc_ast->type = putc_ast->expr->type;
        else
          success = compile_error(&stmt->src_loc,
                                  "putc : `char` type required, actual `%s`", get_type_printstr(putc_ast->expr->type));
      }
    }
    else
      success = compile_error(&stmt->src_loc, "putc : argument expression required");
  }
  else
    assert(0);
  return success;
}

local bool32
do_stmt_block(AstBlock* module_block,
              AstProc* proc,
              AstNode* loop,
              AstBlock* block)
{
  bool32 success = true;

  for(ListItem* list_item = block->node_list.first;
      list_item && success;
      list_item = list_item->next)
  {
    success = do_statement(module_block, proc, block, loop, (AstNode*)list_item->elem);
  }
  return success;
}

local bool32
do_module_block(AstProc* proc,
                AstNode* loop,
                AstBlock* block)
{
  bool32 success = true;

  for(ListItem* list_item = block->node_list.first;
      list_item && success;
      list_item = list_item->next)
  {
    AstNode* stmt = (AstNode*)list_item->elem;
    if(stmt->kind == AstNodeKind_VarDecl)
      success = do_var_decl(block, block, (AstVarDecl*)stmt);
    else if(stmt->kind == AstNodeKind_Proc)
      success = do_proc_decl(block, (AstProc*)stmt);
    else if(stmt->kind == AstNodeKind_Label
            || stmt->kind == AstNodeKind_Call
            || stmt->kind == AstNodeKind_Id
            || stmt->kind == AstNodeKind_Literal
            || stmt->kind == AstNodeKind_BinExpr
            || stmt->kind == AstNodeKind_UnrExpr)
    {
      success = compile_error(&stmt->src_loc, "unexpected statement %s", get_ast_kind_printstr(stmt->kind));
    }
    else if(stmt->kind == AstNodeKind_Struct
            || stmt->kind == AstNodeKind_Union
            || stmt->kind == AstNodeKind_Enum)
    {
      fail("Not implemented");
    }
    else
      assert(0);
  }
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

    if(success = do_module_block(0, 0, module_block))
    {
      for(ListItem* list_item = module_block->node_list.first;
          list_item;
          list_item = list_item->next)
      {
        AstNode* ast = (AstNode*)list_item->elem;
        if(ast->kind == AstNodeKind_Proc)
        {
          AstProc* proc = (AstProc*)ast;
          list_remove_item(&module_block->node_list, list_item);
          if(!proc->is_decl)
            list_append(arena, &module->proc_defs, list_item->elem);
        }
      }

      AstCall* main_call = new_call(deflt_src_loc);
      main_call->id = new_id(deflt_src_loc, "main");
      if(success = do_call(module_block, module_block, main_call))
      {
        if(type_unif(main_call->type, basic_type_int))
        {
          list_append(arena, &module_block->node_list, main_call);
        }
        else
        {
          success = compile_error(&main_call->proc->src_loc, "main() must return a `int`");
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


