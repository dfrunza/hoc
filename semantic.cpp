#define AST2(VAR, KIND)\
  (((VAR)->kind == AstKind2##_##KIND) ? &(VAR)->KIND : 0)

#define SYM(VAR, KIND)\
  (((VAR)->kind == SymbolKind##_##KIND) ? (VAR)->KIND : 0)

#if 0
char* AstBuiltinProc_names[] = 
{
#define ENUM_MEMBER(PREFIX, NAME) #NAME
  AstBuiltinProc_MEMBER_LIST()
#undef ENUM_MEMBER
};
#endif

#if 0
AstNode2*
new_ast2_node(SourceLoc* src_loc, AstKind2 kind)
{
  AstNode2* node = mem_push_struct(arena, AstNode2);
  node->kind = kind;
  node->src_loc = src_loc;
  return node;
}

AstNode2*
new_ast2_module(SourceLoc* src_loc)
{
  AstNode2* node = new_ast2_node(src_loc, AstKind2_module);
  node->module.procs = new_list(arena, List_ast2_node);
  return node;
}

AstNode2*
new_ast2_block(SourceLoc* src_loc)
{
  AstNode2* node = new_ast2_node(src_loc, AstKind2_block);
  node->block.stmts = new_list(arena, List_ast2_node);
  return node;
}

AstNode2*
new_ast2_type_decl(SourceLoc* src_loc, char* type_name)
{
  AstNode2* node = new_ast2_node(src_loc, AstKind2_type_decl);
  node->type_decl.type_name = type_name;
  return node;
}

AstNode2*
new_ast2_var_decl(SourceLoc* src_loc, char* var_name)
{
  AstNode2* node = new_ast2_node(src_loc, AstKind2_var_decl);
  node->var_decl.var_name = var_name;
  return node;
}

AstNode2*
new_ast2_var_occur(SourceLoc* src_loc, char* var_name)
{
  AstNode2* node = new_ast2_node(src_loc, AstKind2_var_occur);
  node->var_occur.var_name = var_name;
  return node;
}

AstNode2*
new_ast2_proc_decl(SourceLoc* src_loc, char* proc_name)
{
  AstNode2* node = new_ast2_node(src_loc, AstKind2_proc_decl);
  //node->proc_decl.kind = AstProcKind_User;
  node->proc_decl.proc_name = proc_name;
  node->proc_decl.formal_args = new_list(arena, List_ast2_node);
  node->proc_decl.ret_var = new_ast2_var_decl(src_loc, 0);
  return node;
}

AstNode2*
new_ast2_proc_occur(SourceLoc* src_loc, char* proc_name)
{
  AstNode2* node = new_ast2_node(src_loc, AstKind2_proc_occur);
  node->proc_occur.proc_name = proc_name;
  node->proc_occur.actual_args = new_list(arena, List_ast2_node);
  return node;
}

#if 0
AstNode2*
new_ast2_builtin_proc_occur(SourceLoc* src_loc, AstBuiltinProc proc_id)
{
  return new_ast2_proc_occur(src_loc, AstBuiltinProc_names[proc_id]);
}
#endif

AstNode2*
new_ast2_stmt(SourceLoc* src_loc, AstNode2* stmt)
{
  AstNode2* node = new_ast2_node(src_loc, AstKind2_stmt);
  node->stmt.stmt = stmt;
  return node;
}
#endif

Scope*
new_scope()
{
  Scope* scope = mem_push_struct(arena, Scope);
  scope->local_decls = new_list(arena, List_ast_node);
  scope->nonlocal_occurs = new_list(arena, List_ast_node);
  return scope;
}

const char* AstKind2_strings[] =
{
#define ENUM_MEMBER(NAME) #NAME
  AstKind2_MEMBER_LIST()
#undef ENUM_MEMBER
};

const char*
get_ast2_kind_printstr(AstKind2 kind)
{
  return AstKind2_strings[kind];
}

void
make_type_printstr(String* str, Type* type)
{
  if(type->kind == TypeKind_basic)
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
  else if(type->kind == TypeKind_pointer)
  {
    make_type_printstr(str, type->pointer.pointee);
    str_append(str, "*");
  }
  else if(type->kind == TypeKind_array)
  {
    str_append(str, "(");
    str_printf(str, "[%d]", type->array.size);
    make_type_printstr(str, type->array.elem);
    str_append(str, ")");
  }
  else if(type->kind == TypeKind_product)
  {
    make_type_printstr(str, type->product.left);
    str_append(str, ", ");
    make_type_printstr(str, type->product.right);
  }
  else if(type->kind == TypeKind_proc)
  {
    make_type_printstr(str, type->proc.ret);
    str_append(str, " (");
    make_type_printstr(str, type->proc.args);
    str_append(str, ")");
  }
  else
    assert(0);
}

char*
get_type_printstr(Type* type)
{
  String* str = str_new(arena);
  make_type_printstr(str, type);
  return str_cap(str);
}

#if 0
bool
is_arithmetic_op(AstCstOperator op)
{
  return op == AstCstOperator_Add || op == AstCstOperator_Sub
    || op == AstCstOperator_Mul || op == AstCstOperator_Div
    || op == AstCstOperator_Mod;
}

bool
is_logic_op(AstCstOperator op)
{
  return op == AstCstOperator_LogicOr || op == AstCstOperator_LogicAnd || op == AstCstOperator_LogicNot;
}

bool
is_relation_op(AstCstOperator op)
{
  return op == AstCstOperator_Equals || op == AstCstOperator_NotEquals
      || op == AstCstOperator_Less || op == AstCstOperator_LessEquals
      || op == AstCstOperator_Greater || op == AstCstOperator_GreaterEquals;
}
#endif

#if 0
AstId*
make_tempvar_id(SourceLoc* src_loc, char* label)
{
  String* str = str_new(arena);
  str_printf(str, "$%s%d", label, tempvar_id++);
  return new_id(src_loc, str->head);
}
#endif

typedef enum SymbolLookup
{
  SymbolLookup__None,
  SymbolLookup_StartLocal,
  SymbolLookup_StartGlobal,
};

Symbol*
lookup_symbol(char* name, SymbolKind kind, SymbolLookup start)
{
  Symbol* result = 0, *symbol = 0;

  if(start == SymbolLookup_StartLocal)
  {
    symbol = symbol_table->local_scope->last_symbol;
  }
  else if(start == SymbolLookup_StartGlobal)
  {
    symbol = symbol_table->global_scope->last_symbol;
  }

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

Symbol*
register_name(char* name, SourceLoc* src_loc, SymbolKind kind)
{
  Symbol* sym = mem_push_struct(symbol_table_arena, Symbol);
  sym->kind = kind;
  sym->name = name;
  sym->scope_id = symbol_table->scope_id;
  sym->nesting_depth = symbol_table->nesting_depth;
  sym->prev_symbol = symbol_table->local_scope->last_symbol;
  symbol_table->local_scope->last_symbol = sym;
  symbol_table->sym_count++;
  return sym;
}

bool
register_var_decl(AstNode2* decl_node)
{
  bool success = true;

  auto* var_decl = AST2(decl_node, var_decl);
  Symbol* decl_sym = lookup_symbol(var_decl->var_name, SymbolKind_var_decl, SymbolLookup_StartLocal);
  if(decl_sym && (decl_sym->scope_id == symbol_table->scope_id))
  {
    success = compile_error(decl_node->src_loc, "variable `%s` already declared", var_decl->var_name);
    compile_error(SYM(decl_sym, var_decl)->src_loc, "see previous declaration of `%s`", var_decl->var_name);
  }
  else
  {
    decl_sym = register_name(var_decl->var_name, decl_node->src_loc, SymbolKind_var_decl);
    decl_sym->var_decl = decl_node;
  }
  return success;
}

bool
register_var_occur(AstNode2* occur_node)
{
  bool success = true;

  auto* var_occur = AST2(occur_node, var_occur);
  Symbol* decl_sym = lookup_symbol(var_occur->var_name, SymbolKind_var_decl, SymbolLookup_StartLocal);
  if(decl_sym)
  {
    var_occur->var_decl = SYM(decl_sym, var_decl);

    Symbol* occur_sym = register_name(var_occur->var_name, occur_node->src_loc, SymbolKind_var_occur);
    occur_sym->var_occur = occur_node;
  }
  else
    success = compile_error(occur_node->src_loc, "unknown var `%s`", var_occur->var_name);
  return success;
}

bool
register_type_decl(AstNode2* decl_node)
{
  bool success = true;

  auto* type_decl = AST2(decl_node, type_decl);
  Symbol* decl_sym = lookup_symbol(type_decl->type_name, SymbolKind_type_decl, SymbolLookup_StartLocal);
  if(decl_sym && (decl_sym->scope_id == symbol_table->scope_id))
  {
    success = compile_error(decl_node->src_loc, "type `%s` already declared", type_decl->type_name);
    compile_error(SYM(decl_sym, type_decl)->src_loc, "see previous declaration of `%s`", type_decl->type_name);
  }
  else
  {
    decl_sym = register_name(type_decl->type_name, decl_node->src_loc, SymbolKind_type_decl);
    decl_sym->type_decl = decl_node;
  }
  return success;
}

bool
register_type_occur(AstNode2* occur_node)
{
  bool success = true;

  auto* type_occur = AST2(occur_node, type_occur);
  Symbol* decl_sym = lookup_symbol(type_occur->type_name, SymbolKind_type_decl, SymbolLookup_StartLocal);
  if(decl_sym)
  {
    type_occur->type_decl = SYM(decl_sym, type_decl);

    Symbol* occur_sym = register_name(type_occur->type_name, occur_node->src_loc, SymbolKind_type_occur);
    occur_sym->type_occur = occur_node;
  }
  else
    success = compile_error(occur_node->src_loc, "unknown type `%s`", type_occur->type_name);

  return success;
}

bool
register_proc_decl(AstNode2* decl_node)
{
  bool success = true;

  auto* proc_decl = AST2(decl_node, proc_decl);
  Symbol* decl_sym = lookup_symbol(proc_decl->proc_name, SymbolKind_proc_decl, SymbolLookup_StartLocal);
  if(decl_sym && (decl_sym->scope_id == symbol_table->scope_id))
  {
    success = compile_error(decl_node->src_loc, "proc `%s` already declared", proc_decl->proc_name);
    compile_error(SYM(decl_sym, proc_decl)->src_loc, "see previous declaration of `%s`", proc_decl->proc_name);
  }
  else
  {
    decl_sym = register_name(proc_decl->proc_name, decl_node->src_loc, SymbolKind_proc_decl);
    decl_sym->proc_decl = decl_node;
  }
  return success;
}

#if 0
bool
register_builtin_proc_occur(AstNode2* occur_node)
{
  bool success = true;

  auto* proc_occur = AST2(occur_node, proc_occur);
  Symbol* decl_sym = lookup_symbol(proc_occur->proc_name, SymbolKind_proc_decl, SymbolLookup_StartGlobal);
  if(decl_sym)
  {
    proc_occur->proc_decl = SYM(decl_sym, proc_decl);

    Symbol* occur_sym = register_name(proc_occur->proc_name, occur_node->src_loc, SymbolKind_proc_occur);
    occur_sym->proc_occur = occur_node;
  }
  else
    success = compile_error(occur_node->src_loc, "unknown proc `%s`", proc_occur->proc_name);

  return success;
}
#endif

#if 0
bool
register_proc_decl(AstProc* proc)
{
  bool success = true;

  AstId* id = (AstId*)proc->id;
  Symbol* proc_sym = lookup_symbol(id->name, SymbolKind_Proc);
  if(proc_sym)
  {
    AstProc* registered_proc = (AstProc*)proc_sym->ast;
    assert(registered_proc && registered_proc->kind == AstKind2_AstProc);

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
        proc_sym->ast = (AstNode2*)proc;
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
    id->sym->ast = (AstNode2*)proc;

  return success;
}
#endif

#if 0
bool
register_call(AstCall* call)
{
  bool success = true;

  AstId* id = (AstId*)call->id;
  Symbol* proc_sym = lookup_symbol(id->name, SymbolKind_Proc);
  if(proc_sym)
  {
    assert(proc_sym->ast->kind == AstKind2_AstProc);
    call->proc_sym = proc_sym;

    Symbol* call_sym = register_id(id, SymbolKind_Call);
    call_sym->ast = (AstNode2*)call;
  }
  else
    success = compile_error(&call->src_loc, "unknown proc `%s`", id->name);
  return success;
}
#endif

#if 0
void
add_builtin_type(char* name, Type* type)
{
  assert(type->kind == TypeKind_basic);
  register_type_decl(new_ast2_type_decl(0, name));
}

void
add_builtin_types()
{
  add_builtin_type("bool", basic_type_bool);
  add_builtin_type("int", basic_type_int);
  add_builtin_type("char", basic_type_char);
  add_builtin_type("float", basic_type_float);
  add_builtin_type("void", basic_type_void);
}
#endif

#if 0
const char*
get_builtin_proc_name(AstBuiltinProc proc)
{
  return AstBuiltinProc_names[proc];
}

void
add_builtin_proc(AstBuiltinProc proc_id, AstProcKind kind, Type* type)
{
  assert(type->kind == TypeKind_proc);
  AstNode2* proc_decl = new_ast2_proc_decl(0, AstBuiltinProc_names[proc_id]);
  if(register_proc_decl(proc_decl))
  {
    AST2(proc_decl, proc_decl)->kind = kind;
    AST2(proc_decl, proc_decl)->builtin_id = proc_id;
  }
  else
    assert(0);
}

void
add_builtin_procs()
{
  {
    Type* type = basic_type_int;
    add_builtin_proc(AstBuiltinProc_add_int, AstProcKind_BinOp,
        new_proc_type(new_product_type(type, type), type));
    add_builtin_proc(AstBuiltinProc_sub_int, AstProcKind_BinOp,
        new_proc_type(new_product_type(type, type), type));
    add_builtin_proc(AstBuiltinProc_mul_int, AstProcKind_BinOp,
        new_proc_type(new_product_type(type, type), type));
    add_builtin_proc(AstBuiltinProc_div_int, AstProcKind_BinOp,
        new_proc_type(new_product_type(type, type), type));
    add_builtin_proc(AstBuiltinProc_neg_int, AstProcKind_UnOp,
        new_proc_type(type, type));
  }
  {
    Type* type = basic_type_float;
    add_builtin_proc(AstBuiltinProc_add_float, AstProcKind_BinOp,
        new_proc_type(new_product_type(type, type), type));
    add_builtin_proc(AstBuiltinProc_sub_float, AstProcKind_BinOp,
        new_proc_type(new_product_type(type, type), type));
    add_builtin_proc(AstBuiltinProc_mul_float, AstProcKind_BinOp,
        new_proc_type(new_product_type(type, type), type));
    add_builtin_proc(AstBuiltinProc_div_float, AstProcKind_BinOp,
        new_proc_type(new_product_type(type, type), type));
    add_builtin_proc(AstBuiltinProc_neg_float, AstProcKind_UnOp,
        new_proc_type(type, type));
  }
  {
    add_builtin_proc(AstBuiltinProc_float_to_int, AstProcKind_BinOp,
        new_proc_type(new_product_type(basic_type_int, basic_type_float), basic_type_int));
    add_builtin_proc(AstBuiltinProc_int_to_float, AstProcKind_BinOp,
        new_proc_type(new_product_type(basic_type_float, basic_type_int), basic_type_float));
  }
  {
    Type* type = new_typevar();
    add_builtin_proc(AstBuiltinProc_assign, AstProcKind_BinOp,
        new_proc_type(new_product_type(type, type), type));
  }
}
#endif

bool
begin_scope(SourceLoc* src_loc, Scope** scope)
{
  *scope = new_scope();

  symbol_table->scope_id = ++last_scope_id; 
  (*scope)->scope_id = symbol_table->scope_id;
  (*scope)->encl_scope = symbol_table->local_scope;

  ++symbol_table->nesting_depth;
  if(symbol_table->nesting_depth < sizeof_array(symbol_table->active_scopes))
  {
    (*scope)->nesting_depth = symbol_table->nesting_depth;
    symbol_table->active_scopes[symbol_table->nesting_depth] = *scope;
    symbol_table->local_scope = *scope;
  }
  else
  {
    compile_error(src_loc, "exceeded max scope nesting depth : %d", sizeof_array(symbol_table->active_scopes));
    return false;
  }

  return true;
}

void
end_scope()
{
  assert(symbol_table->local_scope == symbol_table->active_scopes[symbol_table->nesting_depth]);

  --symbol_table->nesting_depth;
  if(symbol_table->nesting_depth > 0)
  {
    Scope* local_scope = symbol_table->active_scopes[symbol_table->nesting_depth];
    assert(local_scope->scope_id >= 0);
    symbol_table->local_scope = local_scope;
    symbol_table->scope_id = local_scope->scope_id;

    Symbol* symbol = local_scope->last_symbol;
    while(symbol && (symbol->scope_id > symbol_table->scope_id))
    {
      symbol = symbol->prev_symbol;
    }
    local_scope->last_symbol = symbol;
  }
  else
  {
    assert(symbol_table->nesting_depth == 0);
    symbol_table->scope_id = 0;
    symbol_table->local_scope = 0;
  }
}

void
process_includes(List* include_list, List* module_list, ListItem* module_list_item)
{
  for(ListItem* list_item = include_list->first;
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = ITEM(list_item, ast_node);

    if(node->kind == AstNode_include)
    {
      AstNode* block = ATTR(node, body, ast_node);
      process_includes(ATTR(block, nodes, list), include_list, list_item);
    }
  }
  replace_list_item_at(include_list, module_list, module_list_item);

  mem_zero_struct(include_list, List);
}

#if 0
bool
build_ast2_var_decl(AstNode1* cst_var, AstNode2** var_decl)
{
  bool success = true;

  *var_decl = new_ast2_var_decl(cst_var->src_loc, AST1(AST1(cst_var, var)->id, id)->name);

  if(success = register_var_decl(*var_decl))
  {
    Scope* decl_scope = AST2(*var_decl, var_decl)->decl_scope = symbol_table->local_scope;
    append_list_elem(arena, decl_scope->local_decls, *var_decl, List_ast2_node);
  }
  return success;
}
#endif

#if 0
bool
sem_call_args(AstBlock* module_block, AstBlock* block, AstCall* call)
{
  bool success = true;

  List* arg_list = &call->args->list;
  ListItem* first_item = arg_list->first;
  if(first_item)
  {
    AstNode2* arg = (AstNode2*)first_item->elem;
    if(success = sem_expression(module_block, block, arg, &arg))
    {
      first_item->elem = arg;

      for(ListItem* list_item = first_item->next;
          list_item && success;
          list_item = list_item->next)
      {
        arg = (AstNode2*)list_item->elem;
        if(success = sem_expression(module_block, block, arg, &arg))
        {
          list_item->elem = arg;
        }
      }
    }
  }
  return success;
}
#endif

#if 0
bool
type_convert_call_arg(AstVarDecl* formal_arg,
                      AstVarOccur* actual_arg, AstNode2** node_out,
                      Type* type_from, Type* type_to)
{
  bool success = true;
  *node_out = (AstNode2*)actual_arg;

  if(types_are_equal(type_from, type_to))
    return success;

  if(type_from->kind == TypeKind_Array && type_to->kind == TypeKind_Pointer)
  {
    if(type_unif(type_from->array.elem, type_to->pointer.pointee))
    {
      AstUnaryExpr* address_of = new_unr_expr(&actual_arg->src_loc);
      address_of->op = AstCstOperator_AddressOf;
      address_of->type = new_pointer_type(type_from); // pointer(array)
      address_of->operand = (AstNode2*)actual_arg;
      *node_out = (AstNode2*)address_of;
    }
    else
    {
      success = compile_error(&actual_arg->src_loc,
          "array element `%s` and pointer type `%s` are different",
          get_type_printstr(type_from->array.elem), get_type_printstr(type_to->pointer.pointee));
    }
  }
  else if(type_from->kind == TypeKind_Pointer && type_to->kind == TypeKind_Array)
  {
    AstUnaryExpr* deref_ptr = new_unr_expr(&actual_arg->src_loc);
    deref_ptr->op = AstCstOperator_PointerDeref;
    deref_ptr->type = new_array_type(type_to->array.size, type_from->pointer.pointee);
    deref_ptr->operand = (AstNode2*)actual_arg;
    *node_out = (AstNode2*)deref_ptr;
  }
  else if(type_from->kind == TypeKind_Array && type_to->kind == TypeKind_Array)
  {
    actual_arg->type = formal_arg->type;
  }
  else if(types_are_equal(type_from, basic_type_int) && types_are_equal(type_to, basic_type_float))
  {
    AstUnaryExpr* int_to_float = new_unr_expr(&actual_arg->src_loc);
    int_to_float->op = AstCstOperator_IntToFloat;
    int_to_float->type = basic_type_float;
    int_to_float->operand = (AstNode2*)actual_arg;
    *node_out = (AstNode2*)int_to_float;
  }
  else if(types_are_equal(type_from, basic_type_float) && types_are_equal(type_to, basic_type_int))
  {
    AstUnaryExpr* float_to_int = new_unr_expr(&actual_arg->src_loc);
    float_to_int->op = AstCstOperator_FloatToInt;
    float_to_int->type = basic_type_int;
    float_to_int->operand = (AstNode2*)actual_arg;
    *node_out = (AstNode2*)float_to_int;
  }
  else
    success = compile_error(&actual_arg->src_loc,
                            "no implicit conversion from `%s` to `%s`",
                            get_type_printstr(type_from), get_type_printstr(type_to));
  return success;
}
#endif

#if 0
bool
sem_call(AstBlock* module_block, AstBlock* block, AstCall* call)
{
  bool success = true;

  if(success = sem_call_args(module_block, block, call) && register_call(call))
  {
    AstProc* proc = (AstProc*)call->proc_sym->ast;
    assert(proc->kind == AstKind2_AstProc);

    Type* proc_type = proc->type;
    assert(proc_type->kind == TypeKind_Proc);

    if(call->args->count == proc->args->count)
    {
      if(call->args->count >= 1)
      {
        ListItem* actual_arg_li = call->args->list.first;
        ListItem* formal_arg_li = proc->args->list.first;
        AstNode2* actual_arg = (AstNode2*)actual_arg_li->elem;
        AstNode2* formal_arg = (AstNode2*)formal_arg_li->elem;

        call->args->type = actual_arg->type;

        while(success && actual_arg_li && formal_arg_li)
        {
          actual_arg = (AstNode2*)actual_arg_li->elem;
          formal_arg = (AstNode2*)formal_arg_li->elem;

          if(type_unif(actual_arg->type, formal_arg->type))
          {
            call->args->type = new_product_type(call->args->type, actual_arg->type);
          }
          else
          {
            if(type_convert_call_arg((AstVarDecl*)formal_arg, (AstVarOccur*)actual_arg, &actual_arg, actual_arg->type, formal_arg->type))
            {
              call->args->type = new_product_type(call->args->type, actual_arg->type);
              actual_arg_li->elem = (AstNode2*)actual_arg;
            }
          }

          actual_arg_li = actual_arg_li->next;
          formal_arg_li = formal_arg_li->next;
        }
      }
      else if(call->args->count == 0)
      {
        call->args->type = basic_type_void;
      }
      else
        assert(0);
    }
    else
    {
      Type* call_args_type = make_type_of_node_list(call->args);
      success = compile_error(&call->src_loc,
          "%s %s(%s) : incorrect number of arguments...",
          get_type_printstr(proc_type->proc.ret), call->id->name, get_type_printstr(call_args_type));
      compile_error(&proc->src_loc, "...see proc decl");
    }

    if(success)
      call->type = proc_type->proc.ret;
  }
  return success;
}
#endif

#if 0
bool
sem_type_expr(AstNode2* expr)
{
  bool success = false;

  if(expr->kind == AstKind2_AstId)
  {
    AstId* type_id = (AstId*)expr;
    if(success = register_type_occur(type_id))
      expr->type = type_id->type;
  }
  else if(expr->kind == AstKind2_AstArray)
  {
    AstArray* array = (AstArray*)expr;
    if(success = sem_type_expr(array->type_expr))
    {
      array->size = 0;
      if(array->size_expr->kind == AstKind2_AstLiteral)
      {
        AstLiteral* size = (AstLiteral*)array->size_expr;
        if(size->lit_kind == AstLiteralKind_Int)
        {
          array->size = size->int_val;
          array->type = new_array_type(array->size, array->type_expr->type);
        }
        else
          success = compile_error(&expr->src_loc, "integer constant expected");
      }
      else
        success = compile_error(&expr->src_loc, "integer constant expected");
    }
  }
  else if(expr->kind == AstKind2_AstPointer)
  {
    AstPointer* pointer = (AstPointer*)expr;
    if(success = sem_type_expr(pointer->type_expr))
      expr->type = new_pointer_type(pointer->type_expr->type);
  }
  else
    assert(0);

  return success;
}
#endif

#if 0
bool
sem_expression(AstBlock* module_block, AstBlock* block, AstNode2* expr, AstNode2** out_expr)
{
  bool success = true;

  if(expr->kind == AstKind2_AstBinExpr)
  {
    AstBinExpr* bin_expr = (AstBinExpr*)expr;
    if(success = sem_expression(module_block, block, bin_expr->left_operand, &bin_expr->left_operand)
      && sem_expression(module_block, block, bin_expr->right_operand, &bin_expr->right_operand))
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

            if(bin_expr->op == AstCstOperator_Mod && types_are_equal(bin_expr->type, basic_type_float))
              success = compile_error(&expr->src_loc, "modulo operator cannot be applied to float operands");
          }
          else
          {
            success = compile_error(&expr->src_loc,
                                    "int, float or char operands are expected, actual `%s`", get_type_printstr(bin_expr->type));
          }
        }
        else if(is_logic_op(bin_expr->op) && !types_are_equal(bin_expr->type, basic_type_bool))
        {
          success = compile_error(&expr->src_loc,
                                  "bool operands are expected, actual `%s`", get_type_printstr(bin_expr->type));
        }
      }
      else if(types_are_equal(right_type, basic_type_int))
      {
        if(types_are_equal(left_type, basic_type_float))
        {
          if(bin_expr->op != AstCstOperator_Mod)
          {
            AstUnaryExpr* int_to_float = new_unr_expr(&bin_expr->right_operand->src_loc);
            int_to_float->op = AstCstOperator_IntToFloat;
            int_to_float->type = basic_type_float;
            int_to_float->operand = bin_expr->right_operand;
            bin_expr->right_operand = (AstNode2*)int_to_float;
          }
          else
            success = compile_error(&expr->src_loc, "modulo operator cannot be applied to float operands");
        }
        else if(types_are_equal(left_type, basic_type_bool))
        {
          bin_expr->left_operand->type = basic_type_bool;
        }
        else if(left_type->kind == TypeKind_Array || left_type->kind == TypeKind_Pointer)
        {
          if(left_type->kind == TypeKind_Array)
          {
            if(bin_expr->op == AstCstOperator_Add || bin_expr->op == AstCstOperator_Sub
               || bin_expr->op == AstCstOperator_ArrayIndex)
            {
              AstUnaryExpr* address_of = new_unr_expr(&bin_expr->left_operand->src_loc);
              address_of->op = AstCstOperator_AddressOf;
              address_of->type = new_pointer_type(left_type); // pointer(array)
              address_of->operand = bin_expr->left_operand;
              bin_expr->left_operand = (AstNode2*)address_of;
              bin_expr->type = address_of->type;
            }
            else
              success = compile_error(&expr->src_loc, "only addition and subtraction are allowed in pointer arithmetic");
          }
          else if(left_type->kind == TypeKind_Pointer)
          {
            if(bin_expr->op == AstCstOperator_Add || bin_expr->op == AstCstOperator_Sub
               || bin_expr->op == AstCstOperator_ArrayIndex)
              ;/*OK*/
            else if(bin_expr->op == AstCstOperator_Assign)
            {
              if(bin_expr->right_operand->kind == AstKind2_AstLiteral)
              {
                AstLiteral* lit = (AstLiteral*)bin_expr->right_operand;
                assert(lit->lit_kind == AstLiteralKind_Int);
                if(lit->int_val != 0)
                {
                  success = compile_error(&bin_expr->right_operand->src_loc,
                                          "%d is not a valid pointer constant", lit->int_val);
                }
              }
              else
              {
                success = compile_error(&expr->src_loc,
                                        "no implicit conversion from `%s` to `%s`",
                                        get_type_printstr(right_type), get_type_printstr(left_type));
              }
            }
          }

          if(success &&
             (bin_expr->op == AstCstOperator_Add || bin_expr->op == AstCstOperator_Sub
              || bin_expr->op == AstCstOperator_ArrayIndex))
          {
            Type* elem_type = 0;
            if(left_type->kind == TypeKind_Array)
              elem_type = left_type->array.elem;
            else if(left_type->kind == TypeKind_Pointer)
              elem_type = left_type->pointer.pointee;
            else
              assert(0);

            AstLiteral* type_width = new_literal(&bin_expr->right_operand->src_loc);
            type_width->lit_kind = AstLiteralKind_Int;
            type_width->int_val = compute_type_width(elem_type);
            type_width->type = basic_type_int;

            AstBinExpr* mul_op = new_bin_expr(&bin_expr->right_operand->src_loc);
            mul_op->op = AstCstOperator_Mul;
            mul_op->left_operand = bin_expr->right_operand;
            mul_op->right_operand = (AstNode2*)type_width;
            mul_op->type = basic_type_int;

            bin_expr->right_operand = (AstNode2*)mul_op;

            if(bin_expr->op == AstCstOperator_ArrayIndex)
            {
              bin_expr->op = AstCstOperator_Add;

              AstUnaryExpr* deref_ptr = new_unr_expr(&bin_expr->src_loc);
              deref_ptr->op = AstCstOperator_PointerDeref;
              deref_ptr->type = elem_type;
              deref_ptr->operand = (AstNode2*)bin_expr;
              *out_expr = (AstNode2*)deref_ptr;
            }
          }
        }
        else
          success = compile_error(&expr->src_loc,
                                  "no implicit conversion from `%s` to `%s`",
                                  get_type_printstr(right_type), get_type_printstr(left_type));
      }
      else if(types_are_equal(right_type, basic_type_char))
      {
        if(types_are_equal(left_type, basic_type_int))
          bin_expr->right_operand->type = basic_type_int;
        else
          success = compile_error(&expr->src_loc,
                                  "no implicit conversion from `%s` to `%s`",
                                  get_type_printstr(right_type), get_type_printstr(left_type));
      }
      else if(left_type->kind == TypeKind_Pointer)
      {
        if(right_type->kind == TypeKind_Pointer)
        {
          Type* pointee_type = right_type->pointer.pointee;
          if(types_are_equal(pointee_type, basic_type_void))
            bin_expr->right_operand->type = bin_expr->left_operand->type;
          else if(pointee_type->kind == TypeKind_Array)
          {
            pointee_type = pointee_type->array.elem;
            if(type_unif(left_type->pointer.pointee, pointee_type))
              ;/*OK*/
            else
              success = compile_error(&expr->src_loc,
                                      "no implicit conversion from `%s` to `%s`",
                                      get_type_printstr(right_type->pointer.pointee), get_type_printstr(left_type));
          }
          else
            success = compile_error(&expr->src_loc,
                                    "no implicit conversion from `%s` to `%s`",
                                    get_type_printstr(right_type), get_type_printstr(left_type));
        }
        else if(right_type->kind == TypeKind_Array)
        {
          if(bin_expr->op == AstCstOperator_Assign)
          {
            if(type_unif(left_type->pointer.pointee, right_type->array.elem))
            {
              AstUnaryExpr* address_of = new_unr_expr(&bin_expr->src_loc);
              address_of->op = AstCstOperator_AddressOf;
              address_of->type = new_pointer_type(right_type); // pointer(array)
              address_of->operand = bin_expr->right_operand;
              bin_expr->right_operand = (AstNode2*)address_of;
            }
            else
              success = compile_error(&expr->src_loc,
                                      "no implicit conversion from `%s` to `%s`",
                                      get_type_printstr(right_type), get_type_printstr(left_type));
          }
          else
            success = compile_error(&expr->src_loc,
                                    "no implicit conversion from `%s` to `%s`",
                                    get_type_printstr(right_type), get_type_printstr(left_type));
        }
        else if(types_are_equal(right_type, basic_type_int))
        {
          if(bin_expr->right_operand->kind == AstKind2_AstLiteral)
          {
            AstLiteral* lit = (AstLiteral*)bin_expr;
            assert(lit->lit_kind == AstLiteralKind_Int);
            if(lit->int_val != 0)
              success = compile_error(&bin_expr->right_operand->src_loc,
                                      "%d is not a valid pointer constant", lit->int_val);
          }
          else
            success = compile_error(&expr->src_loc,
                                    "no implicit conversion from `%s` to `%s`",
                                    get_type_printstr(right_type), get_type_printstr(left_type));
        }
        else
          success = compile_error(&expr->src_loc,
                                  "no implicit conversion from `%s` to `%s`",
                                  get_type_printstr(right_type), get_type_printstr(left_type));
      }
      else if(left_type->kind == TypeKind_Array)
      {
        if(right_type->kind == TypeKind_Pointer || right_type->kind == TypeKind_Array)
          bin_expr->right_operand->type = bin_expr->left_operand->type;
      }
      else
      {
        success = compile_error(&expr->src_loc,
                                "no implicit conversion from `%s` to `%s`",
                                get_type_printstr(right_type), get_type_printstr(left_type));
      }

#if 0
      if(success && bin_expr->op == AstCstOperator_Assign)
      {
        // Check that the left operand is an l-value
        AstNode2* left_operand = bin_expr->left_operand;
        if(left_operand->kind == AstKind2_VarOccur)
          ;/*OK*/
        else if(left_operand->kind == AstKind2_UnrExpr)
        {
          AstUnaryExpr* unr_expr = (AstUnaryExpr*)left_operand;
          if(unr_expr->op == AstCstOperator_PointerDeref)
            ;/*OK*/
          else
            success = compile_error(&left_operand->src_loc, "left side of assignment is not an l-value");
        }
        else
          success = compile_error(&left_operand->src_loc, "left side of assignment is not an l-value");
      }
#endif
    }
  }
  else if(expr->kind == AstKind2_AstUnaryExpr)
  {
    AstUnaryExpr* unr_expr = (AstUnaryExpr*)expr;
    if(unr_expr->op == AstCstOperator_PostIncrement)
    {
#if 0
      AstBinExpr* assign = new_bin_expr(&unr_expr->src_loc);
      assign->op = AstCstOperator_Assign;
      assign->left_operand = unr_expr->operand;

      AstBinExpr* incr_one = new_bin_expr(&unr_expr->src_loc);
      incr_one->op = AstCstOperator_Add;
      incr_one->left_operand = unr_expr->operand;
      AstLiteral* one = new_literal(&unr_expr->src_loc);
      one->lit_kind = AstLiteralKind_Int;
      one->int_val = 1;
      incr_one->right_operand = (AstNode2*)one;

      assign->right_operand = (AstNode2*)incr_one;
      if(success = sem_expression(block, (AstNode2*)assign, &(AstNode2*)assign))
        *out_expr = (AstNode2*)assign;
#else
      fail("not implemented");
#endif
    }
    else 
    {
      if(success = sem_expression(module_block, block, unr_expr->operand, &unr_expr->operand))
      {
        if(unr_expr->op == AstCstOperator_AddressOf)
        {
          Type* operand_type = unr_expr->operand->type;
          if(unr_expr->operand->kind == AstKind2_AstVarOccur)
          {
            if(operand_type->kind == TypeKind_Array)
              unr_expr->type = new_pointer_type(operand_type); // pointer(array)
            else
              unr_expr->type = new_pointer_type(operand_type);
          }
          else if(unr_expr->operand->kind == AstKind2_AstUnaryExpr)
          {
            // &data[0]
            AstUnaryExpr* operand = (AstUnaryExpr*)unr_expr->operand;
            if(operand->op == AstCstOperator_PointerDeref)
            {
              assert(operand->operand->type->kind == TypeKind_Pointer);
              *out_expr = operand->operand; // address_of(ptr_deref(x)) = x
            }
            else
              success = compile_error(&unr_expr->src_loc, "invalid application of `&` operator");
          }
          else
            success = compile_error(&unr_expr->src_loc, "invalid application of `&` operator");
        }
        else if(unr_expr->op == AstCstOperator_Neg)
        {
          if(type_unif(unr_expr->operand->type, basic_type_int)
             || type_unif(unr_expr->operand->type, basic_type_float))
          {
            expr->type = unr_expr->operand->type;
          }
          else
            success = compile_error(&expr->src_loc,
                                    "integer or float operands are expected, actual `%s`", get_type_printstr(unr_expr->operand->type));
        }
        else if(is_logic_op(unr_expr->op))
        {
          if(type_unif(unr_expr->operand->type, basic_type_bool))
            unr_expr->type = unr_expr->operand->type;
          else
            success = compile_error(&expr->src_loc,
                                    "bool operand is expected, actual `%s`", get_type_printstr(unr_expr->operand->type));
        }
        else if(unr_expr->op == AstCstOperator_PointerDeref)
        {
          Type* operand_type = unr_expr->operand->type;
          switch(operand_type->kind)
          {
            case TypeKind_Pointer:
            case TypeKind_Array:
            {
              if(operand_type->kind == TypeKind_Pointer)
              {
                operand_type = operand_type->pointer.pointee;
                unr_expr->type = operand_type;
              }

              if(operand_type->kind == TypeKind_Array)
              {
                AstUnaryExpr* address_of = new_unr_expr(&unr_expr->src_loc);
                address_of->op = AstCstOperator_AddressOf;
                address_of->type = new_pointer_type(operand_type->array.elem);

                address_of->operand = unr_expr->operand;
                unr_expr->operand = (AstNode2*)address_of;
                unr_expr->type = operand_type->array.elem;
              }
            } break;

            default:
            {
              success = compile_error(&expr->src_loc,
                                      "pointer or array type expected, actual `%s`",
                                      get_type_printstr(unr_expr->operand->type));
            }
          }
        }
        else
          fail("not implemented");
      }
    }
  }
  else if(expr->kind == AstKind2_AstId)
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

      expr = (AstNode2*)var_occur;
    }
  }
  else if(expr->kind == AstKind2_AstLiteral)
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
      var_decl->init_expr = (AstNode2*)str;
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

      *out_expr = (AstNode2*)var_occur;
    }
    else
      assert(0);
  }
  else if(expr->kind == AstKind2_AstCall)
    success = sem_call(module_block, block, (AstCall*)expr);
  else if(expr->kind == AstKind2_AstCast)
  {
    AstCast* cast = (AstCast*)expr;
    if(success = sem_type_expr(cast->type_expr))
    {
      if(success = sem_expression(module_block, block, cast->expr, &cast->expr))
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
              AstUnaryExpr* float_to_int = new_unr_expr(&cast->src_loc);
              float_to_int->op = AstCstOperator_FloatToInt;
              float_to_int->type = basic_type_int;
              float_to_int->operand = cast->expr;
              *out_expr = (AstNode2*)float_to_int;
            }
            else if(types_are_equal(from_type, basic_type_bool)
                    || types_are_equal(from_type, basic_type_char)
                    || from_type->kind == TypeKind_Pointer)
            {
              cast->expr->type = to_type;
              *out_expr = cast->expr;
            }
            else
              success = compile_error(&expr->src_loc, "invalid cast : `%s` to `%s`",
                                      get_type_printstr(from_type), get_type_printstr(to_type));
          }
          else if(types_are_equal(to_type, basic_type_float))
          {
            if(types_are_equal(from_type, basic_type_int))
            {
              AstUnaryExpr* int_to_float = new_unr_expr(&cast->src_loc);
              int_to_float->op = AstCstOperator_IntToFloat;
              int_to_float->type = basic_type_float;
              int_to_float->operand = cast->expr;
              *out_expr = (AstNode2*)int_to_float;
            }
            else
              success = compile_error(&expr->src_loc, "invalid cast : `%s` to `%s`",
                                      get_type_printstr(from_type), get_type_printstr(to_type));
          }
          else if(to_type->kind == TypeKind_Pointer
                  && (types_are_equal(from_type, basic_type_int)
                      || from_type->kind == TypeKind_Pointer))
          {
            cast->expr->type = to_type;
            *out_expr = cast->expr;
          }
          else if(to_type->kind == TypeKind_Array && from_type->kind == TypeKind_Pointer)
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
  else if(expr->kind == AstKind2_AstNew)
  {
    AstNew* new_ast = (AstNew*)expr;
    if(success = sem_type_expr(new_ast->type_expr))
    {
      Type* type_expr = new_ast->type_expr->type;

      if(!types_are_equal(type_expr, basic_type_void))
      {
        if(type_expr->kind == TypeKind_Array)
          new_ast->type = new_pointer_type(type_expr->array.elem);
        else
          new_ast->type = new_pointer_type(new_ast->type_expr->type);

        new_ast->size_expr = (AstNode2*)new_bin_expr(&new_ast->src_loc);
        AstBinExpr* size_expr = (AstBinExpr*)new_ast->size_expr;

        size_expr->op = AstCstOperator_Mul;
        size_expr->left_operand = (AstNode2*)new_int_literal(&new_ast->src_loc, compute_type_width(type_expr));
        size_expr->right_operand = new_ast->count_expr;

        success = sem_expression(module_block, block, new_ast->size_expr, &new_ast->size_expr);
      }
      else
        success = compile_error(&new_ast->type_expr->src_loc, "new() : `%s` type not allowed", get_type_printstr(type_expr));
    }
  }
  else if(expr->kind == AstKind2_AstPutc)
  {
    success = compile_error(&expr->src_loc, "putc() used in expression");
  }
  else
    fail("not implemented : %s", get_ast2_kind_printstr(expr->kind));
  return success;
}
#endif

#if 0
bool
sem_proc_formal_args(AstBlock* block, AstProc* proc)
{
  bool success = true;

  Type* args_type = basic_type_void;
  List* args_list = &proc->args->list;
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
        assert(var_decl->kind == AstKind2_AstVarDecl);
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
  proc->args->type = args_type;
  return success;
}
#endif

#if 0
bool
sem_proc_ret_var(AstBlock* block, AstProc* proc)
{
  bool success = true;

  AstVarDecl* var_decl = new_var_decl(&proc->src_loc);
  proc->ret_var = (AstNode2*)var_decl;
  var_decl->id = make_tempvar_id(&proc->src_loc, "ret");
  var_decl->type_expr = proc->ret_type_expr;
  if(success = register_var_decl(var_decl))
    var_decl->decl_block = block;
  return success;
}
#endif

#if 0
bool
sem_proc_decl(AstBlock* module_block, AstProc* proc)
{
  bool success = true;

  if(success = scope_begin((AstBlock*)proc->body))
  {
    if(success = sem_proc_formal_args((AstBlock*)proc->body, proc)
       && sem_proc_ret_var((AstBlock*)proc->body, proc)
       && sem_stmt_block(module_block, proc, 0, (AstBlock*)proc->body))
    {
      scope_end();

      proc->type = new_proc_type(proc->args->type, proc->ret_var->type);
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
#endif

#if 0
bool
sem_statement(AstBlock* module_block,
             AstProc* proc,
             AstBlock* block,
             AstNode2* loop,
             AstNode2* stmt)
{
  bool success = true;

  if(stmt->kind == AstKind2_AstVarDecl)
    success = sem_var_decl(module_block, block, (AstVarDecl*)stmt);
  else if(stmt->kind == AstKind2_AstBinExpr || stmt->kind == AstKind2_AstCall)
    success = sem_expression(module_block, block, stmt, &stmt);
  else if(stmt->kind == AstKind2_AstUnaryExpr)
  {
    AstUnaryExpr* unr_expr = (AstUnaryExpr*)stmt;
    if(unr_expr->op == AstCstOperator_PostDecrement
       || unr_expr->op == AstCstOperator_PreDecrement
       || unr_expr->op == AstCstOperator_PostIncrement
       || unr_expr->op == AstCstOperator_PreIncrement)
      success = sem_expression(module_block, block, stmt, &stmt);
    else
      success = compile_error(&stmt->src_loc, "unexpected statement %s", get_ast2_kind_printstr(stmt->kind));
  }
  else if(stmt->kind == AstKind2_AstWhileStmt)
  {
    AstWhileStmt* while_stmt = (AstWhileStmt*)stmt;
    if(success = sem_expression(module_block, block, while_stmt->cond_expr, &while_stmt->cond_expr))
    {
      if(type_unif(while_stmt->cond_expr->type, basic_type_bool))
      {
        if(while_stmt->body->kind == AstKind2_AstBlock)
        {
          if(success = scope_begin((AstBlock*)while_stmt->body))
          {
            success = sem_stmt_block(module_block, proc, stmt, (AstBlock*)while_stmt->body);
            scope_end();
          }
        }
        else
          success = sem_statement(module_block, proc, block, stmt, while_stmt->body);
      }
      else
        success = compile_error(&stmt->src_loc, "boolan expression is expected");
    }
  }
  else if(stmt->kind == AstKind2_AstForStmt)
  {
    AstForStmt* for_stmt = (AstForStmt*)stmt;
    if(for_stmt->body->kind == AstKind2_AstBlock)
    {
      if(success = scope_begin((AstBlock*)for_stmt->body))
      {
        success = (for_stmt->decl_expr ? sem_var_decl(module_block, (AstBlock*)for_stmt->body, (AstVarDecl*)for_stmt->decl_expr) : 1);
        if(success)
        {
          success = sem_stmt_block(module_block, proc, stmt, (AstBlock*)for_stmt->body)
            && (for_stmt->cond_expr ? sem_expression(module_block, (AstBlock*)for_stmt->body, for_stmt->cond_expr, &for_stmt->cond_expr) : 1)
            && (for_stmt->loop_expr ? sem_expression(module_block, (AstBlock*)for_stmt->body, for_stmt->loop_expr, &for_stmt->loop_expr) : 1);

          if(success)
          {
            if(!type_unif(for_stmt->cond_expr->type, basic_type_bool))
              success = compile_error(&stmt->src_loc, "boolan expression is expected");
          }
        }
        scope_end();
      }
    }
    else
      assert(0);
  }
  else if(stmt->kind == AstKind2_AstIfStmt)
  {
    AstIfStmt* if_stmt = (AstIfStmt*)stmt;
    if(success = sem_expression(module_block, block, if_stmt->cond_expr, &if_stmt->cond_expr))
    {
      if(type_unif(if_stmt->cond_expr->type, basic_type_bool))
      {
        if(if_stmt->body->kind == AstKind2_AstBlock)
        {
          if(success = scope_begin((AstBlock*)if_stmt->body))
          {
            success = sem_stmt_block(module_block, proc, loop, (AstBlock*)if_stmt->body);
            scope_end();
          }
        }
        else
          success = sem_statement(module_block, proc, block, loop, if_stmt->body);

        if(success)
        {
          if(if_stmt->else_body)
          {
            if(if_stmt->else_body->kind == AstKind2_AstBlock)
            {
              if(success = scope_begin((AstBlock*)if_stmt->else_body))
              {
                success = sem_stmt_block(module_block, proc, stmt, (AstBlock*)if_stmt->else_body);
                scope_end();
              }
            }
            else
              success = sem_statement(module_block, proc, block, loop, if_stmt->else_body);
          }
        }
      }
      else
        success = compile_error(&stmt->src_loc, "boolan expression is expected");
    }
  }
  else if(stmt->kind == AstKind2_AstReturnStmt)
  {
    if(proc)
    {
      AstReturnStmt* ret_stmt = (AstReturnStmt*)stmt;
      ret_stmt->proc = proc;
      AstBlock* encl_proc_block = (AstBlock*)proc->body;
      ret_stmt->nesting_depth = ((AstBlock*)block)->nesting_depth - encl_proc_block->nesting_depth;
      assert(!stmt->type);
      stmt->type = basic_type_void;

      AstNode2* ret_expr = ret_stmt->expr;
      AstVarDecl* ret_var = (AstVarDecl*)proc->ret_var;

      if(ret_expr)
      {
        AstBinExpr* bin_expr = new_bin_expr(&stmt->src_loc);
        ret_stmt->assign_expr = bin_expr;
        bin_expr->op = AstCstOperator_Assign;
        bin_expr->left_operand = (AstNode2*)new_id(&ret_expr->src_loc, ret_var->id->name);
        bin_expr->right_operand = ret_expr;

        if(success = sem_expression(module_block, block, (AstNode2*)bin_expr, &(AstNode2*)bin_expr))
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
  else if(stmt->kind == AstKind2_AstId ||
          stmt->kind == AstKind2_AstLiteral)
  {
    success = compile_error(&stmt->src_loc, "unexpected statement `%s` at this location", get_ast2_kind_printstr(stmt->kind));
  }
  else if(stmt->kind == AstKind2_AstBreakStmt || stmt->kind == AstKind2_AstContinueStmt)
  {
    if(loop)
    {
      AstNode2* loop_body = 0;
      if(loop->kind == AstKind2_AstWhileStmt)
        loop_body = ((AstWhileStmt*)loop)->body;
      else if(loop->kind = AstKind2_AstForStmt)
        loop_body = (AstNode2*)((AstForStmt*)loop)->body;
      assert(loop_body);

      AstLoopCtrl* loop_ctrl = (AstLoopCtrl*)stmt;
      loop_ctrl->loop = loop;
      loop_ctrl->nesting_depth = 0;
      if(loop_body->kind == AstKind2_AstBlock)
      {
        AstBlock* loop_encl_block = ((AstBlock*)loop_body)->encl_block;
        loop_ctrl->nesting_depth = block->nesting_depth - loop_encl_block->nesting_depth;
      }
      else
        assert(loop_body == stmt);
    }
    else
      success = compile_error(&stmt->src_loc, "unexpected `%s` at this location", get_ast2_kind_printstr(stmt->kind));
  }
  else if(stmt->kind == AstKind2_AstEmptyStmt)
  {
    ;//OK
  }
  else if(stmt->kind == AstKind2_AstNew)
  {
    success = compile_error(&stmt->src_loc, "new() used as a statement");
  }
  else if(stmt->kind == AstKind2_AstPutc)
  {
    AstPutc* putc_ast = (AstPutc*)stmt;
    if(putc_ast->expr)
    {
      if(success = sem_expression(module_block, block, putc_ast->expr, &putc_ast->expr))
      {
        if(types_are_equal(putc_ast->expr->type, basic_type_char))
          putc_ast->type = putc_ast->expr->type;
        else
          success = compile_error(&stmt->src_loc,
                                  "putc : expected argument of type `char`, actual `%s`", get_type_printstr(putc_ast->expr->type));
      }
    }
    else
      success = compile_error(&stmt->src_loc, "putc : argument required");
  }
  else if(stmt->kind == AstKind2_AstBlock)
  {
    // anonymours block
    if(success = scope_begin((AstBlock*)stmt))
    {
      success = sem_stmt_block(module_block, proc, 0, (AstBlock*)stmt);
      scope_end();
    }
  }
  else
    assert(0);
  return success;
}
#endif

#if 0
bool
sem_stmt_block(AstBlock* module_block,
              AstProc* proc,
              AstNode2* loop,
              AstBlock* block)
{
  bool success = true;

  for(ListItem* list_item = block->node_list.first;
      list_item && success;
      list_item = list_item->next)
  {
    success = sem_statement(module_block, proc, block, loop, (AstNode2*)list_item->elem);
  }
  return success;
}
#endif

#if 0
AstNode2*
make_ast2_var_occur(AstNode2* var_decl)
{
  AstNode2* var_occur = new_ast2_var_occur(var_decl->src_loc, AST2(var_decl, var_decl)->var_name);
  AST2(var_occur, var_occur)->occur_scope = symbol_table->local_scope;
  AST2(var_occur, var_occur)->decl_scope_offset =
    AST2(var_decl, var_decl)->decl_scope->scope_id - AST2(var_occur, var_occur)->occur_scope->scope_id;
  AST2(var_occur, var_occur)->var_decl = var_decl;
  return var_occur;
}

AstNode2*
make_ast2_proc_occur(SourceLoc* src_loc, char* proc_name)
{
  AstNode2* proc_occur = 0;
  Symbol* sym = lookup_symbol(proc_name, SymbolKind_proc_decl);
  if(sym)
  {
    proc_occur = new_ast2_proc_occur(src_loc, proc_name);
    AST2(proc_occur, proc_occur)->proc_decl = SYM(sym, proc_decl);
  }
  return proc_occur;
}
#endif

#if 0
bool
build_ast2_module(AstNode1* module1, AstNode2** module2)
{
  bool success = true;

  *module2 = new_ast2_module(module1->src_loc);
  AstNode2* body = AST2(*module2, module)->body = new_ast2_block(AST1(module1, module)->body->src_loc);

  if(success = begin_scope(module1->src_loc, &AST2(body, block)->scope))
  {
    //symbol_table->module_scope = AST2(body, block)->scope;

    for(ListItem* list_item = AST1(AST1(module1, module)->body, block)->nodes->first;
        list_item && success;
        list_item = list_item->next)
    {
      AstNode1* ast1_node = AST1_ITEM(list_item);
      AstNode2* module_block = AST2(*module2, module)->body;
      if(ast1_node->kind == AstKind1_var)
      {
        AstNode2* ast2_var_decl = 0;
        if(build_ast2_var_decl(ast1_node, &ast2_var_decl))
        {
          AstNode1* ast1_init_expr = AST1(ast1_node, var)->init_expr;
          if(ast1_init_expr)
          {
            AstNode2* init_expr = new_ast2_node(ast1_init_expr->src_loc, AstKind2_binop_occur);
            AST2(init_expr, binop_occur)->op_kind = OperatorKind_Assign;
            AST2(init_expr, binop_occur)->left_operand = make_ast2_var_occur(ast2_var_decl);
            //TODO: Set the 'right_operand' !

            append_list_elem(arena, AST2(module_block, block)->stmts, init_expr, List_ast2_node);
#if 0
            AstNode2* init_expr = new_ast2_builtin_proc_occur(ast1_init_expr->src_loc, AstBuiltinProc_assign);
            if(success = register_builtin_proc_occur(init_expr))
            {
              append_list_elem(arena, AST2(init_expr, proc_occur)->actual_args, make_ast2_var_occur(ast2_var_decl), List_ast2_node);
              //TODO: Add the second argument!!
              append_list_elem(arena, AST2(module_block, block)->stmts, init_expr, List_ast2_node);
            }
#endif
          }
        }
        else if(ast1_node->kind == AstKind1_proc)
        {
#if 1
          AstNode2* proc = new_ast2_proc_decl(ast1_node->src_loc, AST1(AST1(ast1_node, proc)->id, id)->name);
          AstNode2* body = AST2(proc, proc_decl)->body;
          if(success = begin_scope(body->src_loc, &AST2(body, block)->scope))
          {
            end_scope();
          }
#endif
        }
      }
#if 0
      else
        success = compile_error(ast1_node->src_loc, "unexpected statement `%s`", get_ast2_kind_printstr(ast1_node->kind));
#endif
    }

    end_scope();
  }

  return success;
}
#endif

#if 0
bool
sem_module_block(AstProc* proc,
                AstNode2* loop,
                AstBlock* block)
{
  bool success = true;

  for(ListItem* list_item = block->node_list.first;
      list_item && success;
      list_item = list_item->next)
  {
    AstNode2* stmt = (AstNode2*)list_item->elem;
    if(stmt->kind == AstKind2_AstVarDecl)
      success = sem_var_decl(block, block, (AstVarDecl*)stmt);
    else if(stmt->kind == AstKind2_AstProc)
      success = sem_proc_decl(block, (AstProc*)stmt);
    else if(stmt->kind == AstKind2_AstLabel
            || stmt->kind == AstKind2_AstCall
            || stmt->kind == AstKind2_AstId
            || stmt->kind == AstKind2_AstLiteral
            || stmt->kind == AstKind2_AstBinExpr
            || stmt->kind == AstKind2_AstUnaryExpr)
    {
      success = compile_error(&stmt->src_loc, "unexpected statement %s", get_ast2_kind_printstr(stmt->kind));
    }
    else if(stmt->kind == AstKind2_AstStruct
            || stmt->kind == AstKind2_AstUnion
            || stmt->kind == AstKind2_AstEnum)
    {
      fail("Not implemented");
    }
    else
      assert(0);
  }
  return success;
}
#endif

void
init_symbol_table()
{
  symbol_table = mem_push_struct(arena, SymbolTable);
  symbol_table->scope_id = last_scope_id = -1;
}

#if 0
void
init_operator_table()
{
  operator_table = mem_push_array(arena, AstNode2, OperatorKind__Count);
  AstNode2* op;
  OperatorKind kind;

  kind = OperatorKind_Assign;
  op = &operator_table[kind];
  op->src_loc = 0;
  op->kind = AstKind2_binop_decl;
  AST2(op, binop_decl)->kind = kind;

  kind = OperatorKind_Add;
  op = &operator_table[kind];
  op->src_loc = 0;
  op->kind = AstKind2_binop_decl;
  AST2(op, binop_decl)->kind = kind;
}
#endif

#if 0
bool
build_ast2(AstNode1* module1, AstNode2** module2)
{
  bool success = true;

  init_types();
  init_symbol_table();
  init_operator_table();

  if(success = begin_scope(0, &symbol_table->global_scope))
  {
    add_builtin_types();
    //add_builtin_procs();

    success = build_ast2_module(module1, module2);
    end_scope();
#if 0
    AstCall* main_call = new_call(deflt_src_loc);
    main_call->id = new_id(deflt_src_loc, "main");
    if(success = sem_call(module_block, module_block, main_call))
    {
      if(type_unif(main_call->type, basic_type_int))
      {
        list_append(arena, &module_block->node_list, main_call);
      }
      else
      {
        AstProc* main_proc = (AstProc*)main_call->proc_sym->ast;
        assert(main_proc->kind == AstKind2_AstProc);
        success = compile_error(&main_proc->src_loc, "main() must return a `int`");
      }
    }
#endif
  }

  return success;
}
#endif

#if 0
void
DEBUG_print_scope(String* str, int indent_level, Scope* scope, char* tag)
{
  if(scope)
  {
    ++indent_level;
    if(tag)
    {
      DEBUG_print_line(str, indent_level, tag);
      ++indent_level;
    }

    DEBUG_print_line(str, indent_level, "scope_id: %d", scope->scope_id);
    DEBUG_print_line(str, indent_level, "nesting_depth: %d", scope->nesting_depth);
    DEBUG_print_scope(str, indent_level, scope->encl_scope, "encl_scope");
    DEBUG_print_ast_node_list(str, indent_level, "local_decls", scope->local_decls);
    DEBUG_print_ast_node_list(str, indent_level, "nonlocal_occurs", scope->nonlocal_occurs);
  }
}

void
DEBUG_print_ast2_node(String* str, int indent_level, char* tag, AstNode2* node)
{
  if(node)
  {
    if(tag)
    {
      DEBUG_print_line(str, indent_level, tag);
      ++indent_level;
    }
#if 1
    if(node->src_loc)
    {
      DEBUG_print_line(str, indent_level, "%s src_line=\"%s:%d\"",
          get_ast2_kind_printstr(node->kind), node->src_loc->file_path, node->src_loc->line_nr);
    }
    else
    {
      DEBUG_print_line(str, indent_level, "%s", get_ast2_kind_printstr(node->kind));
    }
#else
    if(node->src_loc)
    {
      DEBUG_print_line(str, indent_level, "src_line=\"%s:%d\"", node->src_loc->file_path, node->src_loc->line_nr);
    }
#endif
    ++indent_level;

    if(node->kind == AstKind2_module)
    {
      auto* module2 = AST2(node, module);
      DEBUG_print_ast2_node(str, indent_level, "body", module2->body);
      DEBUG_print_ast_node_list(str, indent_level, "procs", module2->procs);
    }
    else if(node->kind == AstKind2_block)
    {
      auto* block = AST2(node, block);
      DEBUG_print_ast_node_list(str, indent_level, "stmts", block->stmts);
      DEBUG_print_scope(str, indent_level, block->scope, "scope");
    }
    else if(node->kind == AstKind2_stmt)
    {
      auto* stmt = AST2(node, stmt);
      DEBUG_print_ast2_node(str, indent_level, "stmt", stmt->stmt);
    }
    else if(node->kind == AstKind2_var_decl)
    {
      auto* var_decl = AST2(node, var_decl);
      DEBUG_print_line(str, indent_level, "var_name: %s", var_decl->var_name);
    }
    else if(node->kind == AstKind2_proc_occur)
    {
      auto* proc_occur = AST2(node, proc_occur);
      DEBUG_print_line(str, indent_level, "proc_name: %s", proc_occur->proc_name);
      DEBUG_print_ast_node_list(str, indent_level, "actual_args", proc_occur->actual_args);
    }
    else if(node->kind == AstKind2_var_occur)
    {
      auto* var_occur = AST2(node, var_occur);
      DEBUG_print_line(str, indent_level, "var_name: %s", var_occur->var_name);
      DEBUG_print_line(str, indent_level, "decl_scope_offset: %d", var_occur->decl_scope_offset);
    }
    else if(node->kind == AstKind2_binop_decl)
    {
      auto* binop_decl = AST2(node, binop_decl);
      DEBUG_print_line(str, indent_level, "kind: %s", get_op_kind_printstr(binop_decl->kind));
    }
    else if(node->kind == AstKind2_binop_occur)
    {
      auto* binop_occur = AST2(node, binop_occur);
      DEBUG_print_line(str, indent_level, "op_kind", get_op_kind_printstr(binop_occur->op_kind));
      DEBUG_print_ast2_node(str, indent_level, "left_operand", binop_occur->left_operand);
      DEBUG_print_ast2_node(str, indent_level, "right_operand", binop_occur->right_operand);
    }
    else
      assert(0);
  }
}
#endif

