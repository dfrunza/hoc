SymbolTable* symbol_table = 0;
int tempvar_id = 0;

Type* basic_type_bool;
Type* basic_type_int;
Type* basic_type_char;
Type* basic_type_float;
Type* basic_type_void;
List* subst_list;
int typevar_id = 1;

int last_label_id;

bool symtab(AstNode* node);
void gen_x86_load_rvalue(String* code, AstNode* node);
bool gen_x86(String* code, AstNode* node);

Label make_unique_label()
{
  Label label = {0};
  h_sprintf(label.name, "L%d", last_label_id++);
  return label;
}

char* make_temp_name(char* label)
{
  String str; str_init(&str, arena);
  str_printf(&str, "$%s%d", label, tempvar_id++);
  return str_cap(&str);
}

AstNode* new_ast_node(eAstNode kind, SourceLoc* src_loc)
{
  AstNode* node = mem_push_struct(arena, AstNode);
  node->src_loc = src_loc;
  node->kind = kind;
  return node;
}

void DEBUG_print_arena_usage(MemoryArena* arena, char* tag)
{
  ArenaUsage usage = arena_usage(arena);
  h_printf("in_use(`%s`) : %.2f%%\n", tag, usage.in_use*100);
}

void make_type_printstr(String* str, Type* type)
{
  if(type->kind == eType_basic)
  {
    if(type->basic.kind == eBasicType_bool)
      str_append(str, "bool");
    else if(type->basic.kind == eBasicType_int)
      str_append(str, "int");
    else if(type->basic.kind == eBasicType_float)
      str_append(str, "float");
    else if(type->basic.kind == eBasicType_char)
      str_append(str, "char");
    else if(type->basic.kind == eBasicType_void)
      str_append(str, "void");
    else if(type->basic.kind == eBasicType_auto)
      str_append(str, "auto");
    else
      assert(0);
  }
  else if(type->kind == eType_pointer)
  {
    make_type_printstr(str, type->pointer.pointee);
    str_append(str, "*");
  }
  else if(type->kind == eType_array)
  {
    str_append(str, "(");
    if(type->array.size >= 0)
      str_printf(str, "[%d]", type->array.size);
    else
      str_append(str, "[]");
    make_type_printstr(str, type->array.elem);
    str_append(str, ")");
  }
  else if(type->kind == eType_product)
  {
    make_type_printstr(str, type->product.left);
    str_append(str, ", ");
    make_type_printstr(str, type->product.right);
  }
  else if(type->kind == eType_proc)
  {
    make_type_printstr(str, type->proc.ret);
    str_append(str, " (");
    make_type_printstr(str, type->proc.args);
    str_append(str, ")");
  }
  else if(type->kind == eType_var)
  {
    make_type_printstr(str, type->var.type);
  }
  else if(type->kind == eType_typevar)
  {
    str_printf(str, "$type%d", type->typevar.id);
  }
  else
    assert(0);
}

#if 0
void DEBUG_print_line(String* str, int indent_level, char* message, ...)
{/*>>>*/
  for(int i = 0; i < indent_level; i++)
  {
    str_append(str, "  ");
  }

  va_list varargs;
  va_start(varargs, message);
  str_printf_va(str, message, varargs);
  va_end(varargs);

  str_append(str, "\n");
}/*<<<*/

void DEBUG_print_type(String* str, int indent_level, char* tag, Type* type)
{/*>>>*/
  for(int i = 0; i < indent_level; i++)
  {
    str_append(str, "  ");
  }

  if(tag)
  {
    str_append(str, tag);
    str_append(str, ": ");
  }

  make_type_printstr(str, type);
  str_append(str, "\n");
}/*<<<*/

void DEBUG_print_ast_node(String* str, int indent_level, char* tag, AstNode* node)
{/*>>>*/
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
          get_ast_printstr(node->kind), node->src_loc->file_path, node->src_loc->line_nr);
    }
    else
    {
      DEBUG_print_line(str, indent_level, "%s", get_ast_printstr(node->kind));
    }
#else
    if(node->src_loc)
    {
      DEBUG_print_line(str, indent_level, "src_line=\"%s:%d\"", node->src_loc->file_path, node->src_loc->line_nr);
    }
#endif
    ++indent_level;

    /* Example usage
       DEBUG_print_line(str, indent_level, "name: `%s`", node->proc.name);
       DEBUG_print_ast_node(str, indent_level, "id", node);
       DEBUG_print_ast_nodes(str, indent_level, "formal_args", &node->proc.formal_args);
    */
  }
}/*<<<*/

void DEBUG_print_ast_nodes(String* str, int indent_level, char* tag, List* nodes)
{/*>>>*/
  if(nodes->first)
  {
    if(tag)
    {
      DEBUG_print_line(str, indent_level, tag);
      ++indent_level;
    }

    for(ListItem* li = nodes->first;
        li;
        li = li->next)
    {
      AstNode* node = KIND(li, eList_ast_node)->ast_node;
      DEBUG_print_ast_node(str, indent_level, 0, node);
    }
  }
}/*<<<*/
#endif

#include "lex.c"
#include "syntax.c"

Type* new_var_type(Type* var_type)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_var;
  type->var.type = var_type;
  return type;
}

Type* new_basic_type(eBasicType kind)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_basic;
  type->basic.kind = kind;
  return type;
}

Type* new_proc_type(Type* args, Type* ret)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_proc;
  type->proc.args = args;
  type->proc.ret = ret;
  return type;
}

Type* new_typevar()
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_typevar;
  type->typevar.id = typevar_id++;
  return type;
}

Type* new_product_type(Type* left, Type* right)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_product;
  type->product.left = left;
  type->product.right = right;
  return type;
}

Type* new_array_type(int size, Type* elem)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_array;
  type->array.size = size;
  type->array.elem = elem;
  return type;
}

Type* new_pointer_type(Type* pointee)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_pointer;
  type->pointer.pointee = pointee;
  return type;
}

bool types_are_equal(Type* type_a, Type* type_b)
{
  bool are_equal = false;

  if((type_a->kind != eType_typevar) && (type_b->kind == type_a->kind))
  {
    switch(type_a->kind)
    {
      case eType_basic:
        are_equal = (type_a->basic.kind == type_b->basic.kind);
        break;

      case eType_proc:
        are_equal = types_are_equal(type_a->proc.args, type_b->proc.args)
          && types_are_equal(type_a->proc.ret, type_b->proc.ret);
        break;

      case eType_pointer:
        are_equal = types_are_equal(type_a->pointer.pointee, type_b->pointer.pointee);
        break;

      case eType_product:
        are_equal = types_are_equal(type_a->product.left, type_b->product.right);
        break;

      case eType_array:
        are_equal = types_are_equal(type_a->array.elem, type_b->array.elem);
        break;

      case eType_var:
        are_equal = types_are_equal(type_a->var.type, type_b->var.type);
        break;

      default:
        assert(0);
    }
  }
  return are_equal;
}

int compute_type_width(Type* type)
{
  switch(type->kind)
  {
    case eType_array:
      type->width = type->array.size * compute_type_width(type->array.elem);
      break;

    case eType_product:
      type->width = compute_type_width(type->product.left) + compute_type_width(type->product.right);
      break;

    case eType_proc:
      type->width = compute_type_width(type->proc.ret) + compute_type_width(type->proc.args);
      break;

    case eType_basic:
      {
        switch(type->basic.kind)
        {
          case eBasicType_int:
          case eBasicType_float:
          case eBasicType_bool:
            type->width = 4;
            break;
          case eBasicType_char:
            type->width = 1;
            break;
          case eBasicType_void:
            type->width = 0;
            break;
          default:
            assert(0);
        }
      }
      break;

    case eType_pointer:
      type->width = 4;
      break;

    case eType_var:
      type->width = compute_type_width(type->var.type);
      break;

    default:
      assert(0);
  }
  return type->width;
}

Type* copy_type(Type* type)
{
  Type* copy = mem_push_struct(arena, Type);
  *copy = *type;
  return copy;
}

Type* get_type_repr(Type* type)
{
  Type* result = type;
  while(type->repr_type)
  {
    type = type->repr_type;
    result = type;
  }
  return result;
}

void set_union(Type* type_a, Type* type_b)
{
  if(type_a->kind == eType_typevar)
  {
    type_a->repr_type = type_b;
  }
  else
  {
    type_b->repr_type = type_a;
  }
}

bool type_unif(Type* type_a, Type* type_b)
{
  bool success = false;
  Type* repr_type_a = get_type_repr(type_a);
  Type* repr_type_b = get_type_repr(type_b);

  if(repr_type_a == repr_type_b)
  {
    success = true;
  }
  else
  {
    if(repr_type_a->kind == eType_typevar || repr_type_b->kind == eType_typevar)
    {
      set_union(repr_type_a, repr_type_b);
      success = true;
    }
    else if(repr_type_a->kind == repr_type_b->kind)
    {
      if(repr_type_a == repr_type_b)
      {
        success = true;
      }
      else if(repr_type_a->kind == eType_basic)
      {
        success = (repr_type_a->basic.kind == repr_type_b->basic.kind);
      }
      else
      {
        set_union(repr_type_a, repr_type_b);
        assert(repr_type_a->kind == repr_type_b->kind);

        switch(repr_type_a->kind)
        {
          case eType_proc:
            success = type_unif(repr_type_a->proc.args, repr_type_b->proc.args)
              && type_unif(repr_type_a->proc.ret, repr_type_b->proc.ret);
            break;

          case eType_product:
            success = type_unif(repr_type_a->product.left, repr_type_b->product.left)
              && type_unif(repr_type_a->product.right, repr_type_b->product.right);
            break;

          case eType_pointer:
            success = type_unif(repr_type_a->pointer.pointee, repr_type_b->pointer.pointee);
            break;

          case eType_array:
            success = type_unif(repr_type_a->array.elem, repr_type_b->array.elem);
            break;

          case eType_var:
            success = type_unif(repr_type_a->var.type, repr_type_b->var.type);
            break;

          default:
            assert(0);
        }
      }
    }
  }

  return success;
}

TypePair* new_type_pair(Type* key, Type* value)
{
  TypePair* pair = mem_push_struct(arena, TypePair);
  pair->key = key;
  pair->value = value;
  return pair;
}

TypePair* find_pair(List* subst_list, Type* type)
{
  TypePair* result = 0;
  for(ListItem* li = subst_list->first;
      li;
      li = li->next)
  {
    TypePair* pair = (TypePair*)li->elem;
    if(pair->key == type)
    {
      result = pair;
      break;
    }
  }
  return result;
}

Type* type_subst(List* subst_list, Type* type)
{
  type = get_type_repr(type);
  Type* subst = 0;

  TypePair* pair = find_pair(subst_list, type);
  if(pair)
  {
    subst = pair->value;
  }
  else
  {
    subst = copy_type(type);

    pair = new_type_pair(type, subst);
    append_list_elem(subst_list, pair, eList_type_pair);

    switch(subst->kind)
    {
      case eType_typevar:
        subst->typevar.id = typevar_id++;
        break;

      case eType_proc:
        subst->proc.args = type_subst(subst_list, subst->proc.args);
        subst->proc.ret = type_subst(subst_list, subst->proc.ret);
        break;

      case eType_product:
        subst->product.left = type_subst(subst_list, subst->product.left);
        subst->product.right = type_subst(subst_list, subst->product.right);
        break;

      case eType_pointer:
        subst->pointer.pointee = type_subst(subst_list, subst->pointer.pointee);
        break;

      case eType_array:
        subst->array.elem = type_subst(subst_list, subst->array.elem);
        break;

      case eType_var:
        subst->var.type = type_subst(subst_list, subst->var.type);
        break;

      default:
        assert(0);
    }
  }
  return subst;
}

bool resolve_type(Type* type, Type** resolved_type)
{
  bool success = true;

  switch(type->kind)
  {
    case eType_typevar:
      type = get_type_repr(type);
      if(type->kind == eType_typevar)
      {
        success = false;
      }
      break;

    case eType_basic:
      break; // ok

    case eType_proc:
      success = resolve_type(type->proc.args, &type->proc.args)
        && resolve_type(type->proc.ret, &type->proc.ret);
      break;

    case eType_product:
      success = resolve_type(type->product.left, &type->product.left)
        && resolve_type(type->product.right, &type->product.right);
      break;

    case eType_pointer:
      success = resolve_type(type->pointer.pointee, &type->pointer.pointee);
      break;
      
    case eType_array:
      success = resolve_type(type->array.elem, &type->array.elem);
      break;

    case eType_var:
      success = resolve_type(type->var.type, &type->var.type);
      break;

    default:
      assert(0);
  }

  if(success)
  {
    *resolved_type = type;
  }
  return success;
}

char* get_type_printstr(Type* type)
{
  String str; str_init(&str, arena);
  make_type_printstr(&str, type);
  return str_cap(&str);
}

SymbolTable* new_symbol_table(MemoryArena** arena, int size)
{
  MemoryArena* symbol_arena = push_arena(arena, size);
  SymbolTable* symtab = mem_push_struct(symbol_arena, SymbolTable);
  symtab->arena = symbol_arena;
  symtab->nesting_depth = -1;
  symtab->scopes = new_list(symbol_arena, eList_scope);
  return symtab;
}

Scope* find_scope(Scope* active_scope, eScope kind)
{
  Scope* scope = active_scope;
  while(scope)
  {
    if((scope->kind & kind) != 0)
      break;
    scope = scope->encl_scope;
  }
  return scope;
}

Symbol* lookup_sym(char* name, List* symbols)
{
  Symbol* result = 0;
  ListItem* li = symbols->last;
  while(li)
  {
    Symbol* symbol = KIND(li, eList_symbol)->symbol;
    if(cstr_match(symbol->name, name))
    {
      result = symbol;
      break;
    }
    li = li->prev;
  }
  return result;
}

Symbol* lookup_decl_sym(char* name, Scope* scope, eSymbol kind)
{
  Symbol* result = 0;
  while(!result && scope)
  {
    eSymbol k = kind;
    int p = bitpos(k);
    while(!result && p)
    {
      result = lookup_sym(name, &scope->decls[p]);
      k = k ^ (1 << (p-1)); // clear the bit
      p = bitpos(k);
    }
    scope = scope->encl_scope;
  }
  return result;
}

Symbol* lookup_all_decl_sym(char* name, Scope* scope)
{
  Symbol* result = 0;
  while(!result && scope)
  {
    for(int p = eSymbol_None;
        p < eSymbol_Count && !result;
        p++)
    {
      result = lookup_sym(name, &scope->decls[p]);
    }
    scope = scope->encl_scope;
  }
  return result;
}

Symbol* lookup_occur_sym(char* name, Scope* scope)
{
  Symbol* result = 0;
  while(!result && scope)
  {
    result = lookup_sym(name, &scope->occurs);
    scope = scope->encl_scope;
  }
  return result;
}

Symbol* add_decl_sym(SymbolTable* symtab, char* name, Scope* scope, eSymbol kind, AstNode* ast_node)
{
  Symbol* sym = mem_push_struct(symtab->arena, Symbol);
  sym->kind = kind;
  sym->name = name;
  sym->src_loc = ast_node->src_loc;
  sym->scope = scope;
  sym->ast_node = ast_node;
  sym->order_nr = scope->sym_order_nr++;
  ast_node->decl_sym = sym;
  append_list_elem(&scope->decls[bitpos(kind)], sym, eList_symbol);
  return sym;
}

Symbol* add_occur_sym(SymbolTable* symtab, char* name, Scope* scope, AstNode* ast_node)
{
  Symbol* sym = mem_push_struct(symtab->arena, Symbol);
  sym->name = name;
  sym->src_loc = ast_node->src_loc;
  sym->scope = scope;
  sym->ast_node = ast_node;
  sym->order_nr = scope->sym_order_nr++;
  ast_node->occur_sym = sym;
  append_list_elem(&scope->occurs, sym, eList_symbol);
  return sym;
}

Scope* begin_scope(SymbolTable* symtab, eScope kind, AstNode* ast_node)
{
  Scope* scope = mem_push_struct(symtab->arena, Scope);
  scope->kind = kind;
  scope->nesting_depth = symtab->nesting_depth;
  scope->sym_order_nr = 0;
  scope->encl_scope = symtab->active_scope;
  scope->ast_node = ast_node;
  for(int k = 0; k < eSymbol_Count; k++)
  {
    init_list(&scope->decls[k], arena, eList_symbol);
  }
  init_list(&scope->occurs, arena, eList_symbol);
  symtab->active_scope = scope;
  append_list_elem(symtab->scopes, scope, eList_scope);
  return scope;
}

void end_scope(SymbolTable* symtab)
{
  Scope* scope = symtab->active_scope;
  symtab->active_scope = scope->encl_scope;
}

Scope* begin_nested_scope(SymbolTable* symtab, eScope kind, AstNode* ast_node)
{
  symtab->nesting_depth++;
  return begin_scope(symtab, kind, ast_node);
}

void end_nested_scope(SymbolTable* symtab)
{
  end_scope(symtab);
  symtab->nesting_depth--;
}


#if 0
void process_includes(List* include_list, List* module_list, ListItem* module_li)
{
  for(ListItem* li = include_list->first;
      li;
      li = li->next)
  {
    AstNode* node = KIND(li, eList_ast_node)->ast_node;

    if(node->kind == eAstNode_include)
    {
      AstNode* block = node->include.body;
      process_includes(block->block.nodes, include_list, li);
    }
  }
  replace_li_at(include_list, module_list, module_li);

  mem_zero_struct(include_list, List);
}
#endif

bool symtab_expr(SymbolTable* symtab, AstNode* block, AstNode* expr);

bool symtab_formal_arg(SymbolTable* symtab, Scope* proc_scope, AstNode* arg)
{
  assert(KIND(arg, eAstNode_var));
  bool success = true;

  Symbol* decl_sym = lookup_decl_sym(arg->var.name, proc_scope, eSymbol_var);
  if(decl_sym && (decl_sym->scope == proc_scope))
  {
    success = compile_error(arg->src_loc, "formal arg `%s` has already been declared", arg->var.name);
    compile_error(decl_sym->src_loc, "see the declaration of `%s`", arg->var.name);
  }
  else
  {
    add_decl_sym(symtab, arg->var.name, proc_scope, eSymbol_var, arg);
  }
  return success;
}

bool symtab_var(SymbolTable* symtab, AstNode* block, AstNode* var)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(var, eAstNode_var));
  bool success = true;

  Symbol* decl_sym = lookup_all_decl_sym(var->var.name, symtab->active_scope);
  if(decl_sym && (decl_sym->scope == symtab->active_scope || decl_sym->scope == symtab->proc_scope))
  {
    success = compile_error(var->src_loc, "name `%s` already declared", var->var.name);
    compile_error(decl_sym->src_loc, "see declaration of `%s`", var->var.name);
  }
  else
  {
    add_decl_sym(symtab, var->var.name, symtab->active_scope, eSymbol_var, var);
  }
  return success;
}

bool symtab_id(SymbolTable* symtab, AstNode* block, AstNode* id)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(id, eAstNode_id));
  bool success = true;
  add_occur_sym(symtab, id->id.name, symtab->active_scope, id);
  return success;
}

bool symtab_bin_expr(SymbolTable* symtab, AstNode* block, AstNode* bin_expr)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;

  success = symtab_expr(symtab, block, bin_expr->bin_expr.left_operand)
    && symtab_expr(symtab, block, bin_expr->bin_expr.right_operand);
  return success;
}

bool symtab_unr_expr(SymbolTable* symtab, AstNode* block, AstNode* unr_expr)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  success = symtab_expr(symtab, block, unr_expr->unr_expr.operand);
  return success;
}

bool symtab_actual_args(SymbolTable* symtab, AstNode* block, AstNode* arg_list)
{
  assert(KIND(arg_list, eAstNode_arg_list));
  bool success = true;

  for(ListItem* li = arg_list->arg_list.args.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = symtab_expr(symtab, block, arg);
  }
  return success;
}

bool symtab_call(SymbolTable* symtab, AstNode* block, AstNode* call)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(call, eAstNode_call));
  bool success = true;

  AstNode* call_expr = call->call.expr;
  AstNode* arg_list = call->call.arg_list;
  if(call_expr->kind == eAstNode_id)
  {
    AstNode* id = call->call.expr;
    add_occur_sym(symtab, id->id.name, symtab->active_scope, id);
    success = symtab_expr(symtab, block, call_expr) && symtab_actual_args(symtab, block, arg_list);
  }
  else
    success = compile_error(call->src_loc, "unsupported call expr");
  return success;
}

bool symtab_expr(SymbolTable* symtab, AstNode* block, AstNode* expr)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode_bin_expr:
      success = symtab_bin_expr(symtab, block, expr);
      break;
    case eAstNode_unr_expr:
      success = symtab_unr_expr(symtab, block, expr);
      break;
    case eAstNode_id:
      success = symtab_id(symtab, block, expr);
      break;
    case eAstNode_call:
      success = symtab_call(symtab, block, expr);
      break;
    case eAstNode_basic_type:
    case eAstNode_lit:
      break;
    default:
      assert(0);
  }
  return success;
}

bool symtab_block(SymbolTable* symtab, AstNode* block);
bool symtab_block_stmt(SymbolTable* symtab, AstNode* block, AstNode* stmt);

bool symtab_if(SymbolTable* symtab, AstNode* block, AstNode* if_)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(if_, eAstNode_if));
  bool success = true;

  if(success = symtab_expr(symtab, block, if_->if_.cond_expr))
  {
    if((success = symtab_block_stmt(symtab, block, if_->if_.body)) && if_->if_.else_body)
    {
      success = symtab_block_stmt(symtab, block, if_->if_.else_body);
    }
  }
  return success;
}

bool symtab_while(SymbolTable* symtab, AstNode* block, AstNode* while_)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(while_, eAstNode_while));
  bool success = true;

  if(success = symtab_expr(symtab, block, while_->while_.cond_expr))
  {
    while_->while_.scope = begin_nested_scope(symtab, eScope_while, while_);
    success = symtab_block_stmt(symtab, block, while_->while_.body);
    end_nested_scope(symtab);
  }
  return success;
}

bool symtab_loop_ctrl(SymbolTable* symtab, AstNode* block, AstNode* stmt)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;

  Scope* loop_scope = find_scope(symtab->active_scope, eScope_while);
  if(loop_scope)
  {
    stmt->loop_ctrl.loop = loop_scope->ast_node;
  }
  else
  {
    char* keyword = "???";
    if(stmt->loop_ctrl.kind == eLoopCtrl_break)
      keyword = "break";
    else if(stmt->loop_ctrl.kind == eLoopCtrl_continue)
      keyword = "continue";
    else
      assert(0);
    success = compile_error(stmt->src_loc, "unexpected `%s`", keyword);
  }
  return success;
}

bool symtab_return(SymbolTable* symtab, AstNode* block, AstNode* ret)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(ret, eAstNode_return));
  bool success = true;

  if(symtab->proc_scope)
  {
    ret->ret.proc = symtab->proc_scope->ast_node;
    success = symtab_expr(symtab, block, ret->ret.expr);
  }
  else
    success = compile_error(ret->src_loc, "unexpected `return`");
  return success;
}

bool symtab_block_stmt(SymbolTable* symtab, AstNode* block, AstNode* stmt)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode_var:
      success = symtab_var(symtab, block, stmt);
      break;
    case eAstNode_if:
      success = symtab_if(symtab, block, stmt);
      break;
    case eAstNode_while:
      success = symtab_while(symtab, block, stmt);
      break;
    case eAstNode_block:
      {
        stmt->block.scope = begin_nested_scope(symtab, eScope_block, stmt);
        success = symtab_block(symtab, stmt);
        end_nested_scope(symtab);
      }
      break;
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
      success = symtab_expr(symtab, block, stmt);
      break;
    case eAstNode_loop_ctrl:
      success = symtab_loop_ctrl(symtab, block, stmt);
      break;
    case eAstNode_return:
      success = symtab_return(symtab, block, stmt);
      break;
    case eAstNode_empty:
      break;
    default:
      assert(0);
  }
  return success;
}

bool symtab_block(SymbolTable* symtab, AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;

  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = symtab_block_stmt(symtab, block, stmt);
  }
  return success;
}

bool symtab_proc_body(SymbolTable* symtab, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  AstNode* body = proc->proc.body;
  if(proc->modifier == eModifier_extern)
  {
    if(body->kind != eAstNode_empty)
    {
      success = compile_error(proc->src_loc, "`extern` proc `%s` must not define a body", proc->proc.name);
    }
  }
  else
  {
    if(body->kind == eAstNode_block)
    {
      begin_scope(symtab, eScope_block, body);
      success = symtab_block(symtab, body);
      end_scope(symtab);
    }
    else if(body->kind == eAstNode_empty)
      success = compile_error(proc->src_loc, "proc `%s` must define a body", proc->proc.name);
    else
      assert(0);
  }
  return success;
}

bool symtab_formal_args(SymbolTable* symtab, AstNode* arg_list)
{
  assert(KIND(arg_list, eAstNode_arg_list));
  bool success = true;

  for(ListItem* li = arg_list->arg_list.args.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = symtab_formal_arg(symtab, symtab->proc_scope, arg);
  }
  return success;
}

bool symtab_module_proc(SymbolTable* symtab, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  Symbol* decl_sym = lookup_all_decl_sym(proc->proc.name, symtab->active_scope);
  if(decl_sym && (decl_sym->scope == symtab->active_scope))
  {
    success = compile_error(proc->src_loc, "name `%s` has already been declared", proc->proc.name);
    compile_error(decl_sym->src_loc, "see the declaration of `%s`", proc->proc.name);
  }
  else
  {
    add_decl_sym(symtab, proc->proc.name, symtab->active_scope, eSymbol_proc, proc);
    symtab->proc_scope = proc->proc.scope = begin_nested_scope(symtab, eScope_proc, proc);
    success = symtab_formal_args(symtab, proc->proc.arg_list) && symtab_proc_body(symtab, proc);
    end_nested_scope(symtab);
    symtab->proc_scope = 0;
  }
  return success;
}

bool symtab_module_var(SymbolTable* symtab, AstNode* module, AstNode* var)
{
  assert(KIND(module, eAstNode_module));
  assert(KIND(var, eAstNode_var));
  bool success = true;

  Symbol* decl_sym = lookup_all_decl_sym(var->var.name, symtab->active_scope);
  if(decl_sym && (decl_sym->scope == symtab->active_scope))
  {
    success = compile_error(var->src_loc, "name `%s` already declared", var->var.name);
    compile_error(decl_sym->src_loc, "see declaration of `%s`", var->var.name);
  }
  else
  {
    add_decl_sym(symtab, var->var.name, symtab->active_scope, eSymbol_var, var);
  }
  return success;
}

bool symtab_module(SymbolTable* symtab, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;

  module->module.scope = begin_nested_scope(symtab, eScope_module, module);

  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    switch(stmt->kind)
    {
      case eAstNode_var:
        success = symtab_module_var(symtab, module, stmt);
        break;
      case eAstNode_proc:
        success = symtab_module_proc(symtab, stmt);
        break;
      case eAstNode_include:
        break;
      default:
        assert(0);
    }
  }
  end_nested_scope(symtab);
  assert(symtab->active_scope == 0);
  assert(symtab->nesting_depth == -1);
  return success;
}

#if 0
bool node_resolve_type(AstNode* node)
{
  bool success = true;

  if(success = resolve_type(node->ty, &node->ty))
  {
    compute_type_width(node->ty);
    if(success = resolve_type(node->eval_ty, &node->eval_ty))
    {
      compute_type_width(node->eval_ty);
    }
    else
      assert(0);
  }
  else
    assert(0);

  return success;
}

bool resolve_types(AstNode* node)
{
  bool success = true;

  switch(node->kind)
  {
    case eAstNode_module:
      success = resolve_types(node->module.body);
      break;

    case eAstNode_block:
      {
        for(ListItem* li = node->block.nodes.first;
            li && success;
            li = li->next)
        {
          success = resolve_types(KIND(li, eList_ast_node)->ast_node);
        }
      }
      break;

    case eAstNode_stmt:
      if(node->stmt.stmt)
      {
        success = resolve_types(node->stmt.stmt);
      }
      break;

    case eAstNode_var:
      if(success = node_resolve_type(node))
      {
        node->decl_sym->ty = node->ty;
      }
      break;

#if 0
    case eAstNode_var_occur:
      {
        if(success = node_resolve_type(node))
        {
          ATTR(node, symbol, occur_sym)->ty = ATTR(node, type, type);
        }
      }
      break;
#endif

    case eAstNode_bin_expr:
      success = resolve_types(node->bin_expr.left_operand)
        && resolve_types(node->bin_expr.right_operand)
        && node_resolve_type(node);
      break;

    case eAstNode_unr_expr:
      success = resolve_types(node->unr_expr.operand) && node_resolve_type(node);
      break;

    case eAstNode_proc:
      {
        for(ListItem* li = node->proc.formal_args.first;
            li && success;
            li = li->next)
        {
          success = resolve_types(KIND(li, eList_ast_node)->ast_node);
        }
        if(!success)
          break;

        if(success = node_resolve_type(node))
        {
          node->proc.decl_sym->ty = node->ty;

#if 0
          AstNode* ret_var = node->proc.ret_var;
          if(success = node_resolve_type(ret_var))
          {
            node->id.decl_sym->ty = ret_var->ty;

            if(!node->proc.is_extern)
            {
              AstNode* body = node->proc.body;
              success = resolve_types(body) && node_resolve_type(body);
            }
          }
#endif
        }
      }
      break;

    case eAstNode_call:
      {
        for(ListItem* li = node->call.actual_args.first;
            li && success;
            li = li->next)
        {
          success = resolve_types(KIND(li, eList_ast_node)->ast_node);
        }
        if(success && (success = node_resolve_type(node)))
        {
          node->occur_sym->ty = node->ty;
        }
      }
      break;

    case eAstNode_return:
      if(success = node_resolve_type(node))
      {
        if(node->ret.expr)
        {
          success = resolve_types(node->ret.expr);
        }
      }
      break;

    case eAstNode_while:
      success = resolve_types(node->while_.cond_expr) && resolve_types(node->while_.body);
      break;

    case eAstNode_if:
      {
        success = resolve_types(node->if_.cond_expr) && resolve_types(node->if_.body);
        if(node->if_.else_body)
        {
          success = resolve_types(node->if_.else_body);
        }
      }
      break;

    case eAstNode_lit:
      success = node_resolve_type(node);
      break;

    case eAstNode_type:
      if(success = node_resolve_type(node))
      {
#if 0
        node->type.decl_sym->ty = node->ty;
#endif
      }
      break;

    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    case eAstNode_asm_block:
      break;

    default:
      assert(0);
  }
  return success;
}

bool check_types(AstNode* node)
{
  bool success = true;

  switch(node->kind)
  {
    case eAstNode_module:
      success = check_types(node->module.body);
      break;

    case eAstNode_block:
      for(ListItem* li = node->block.nodes.first;
          li && success;
          li = li->next)
      {
        success = check_types(KIND(li, eList_ast_node)->ast_node);
      }
      break;

    case eAstNode_stmt:
      if(node->stmt.stmt)
      {
        success = check_types(node->stmt.stmt);
      }
      break;

    case eAstNode_var:
      if(node->ty == basic_type_void)
      {
        success = compile_error(node->src_loc, "variable type cannot be `void`");
      }
      break;

    case eAstNode_bin_expr:
      {
        AstNode* right_operand = node->bin_expr.right_operand;
        AstNode* left_operand = node->bin_expr.left_operand;

        if(success = check_types(right_operand) && check_types(left_operand))
        {
          assert(node->ty->kind == eType_proc);
          Type* args_ty = node->ty->proc.arg_list; assert(args_ty->kind == eType_product);
          Type* ret_ty = node->ty->proc.ret;

          switch(node->bin_expr.op)
          {
            case eOperator_add:
            case eOperator_sub:
            case eOperator_mul:
            case eOperator_div:
              {
                if(types_are_equal(ret_ty, basic_type_int)
                    || types_are_equal(ret_ty, basic_type_float)
                    || (types_are_equal(ret_ty, basic_type_char))
                    || (ret_ty->kind == eType_pointer))
                {
                  ;//ok
                  assert(types_are_equal(ret_ty, args_ty->product.left) && types_are_equal(args_ty->product.left, args_ty->product.right));
                }
                else
                {
                  success = compile_error(node->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                      get_operator_printstr(node->bin_expr.op), get_type_printstr(args_ty));
                }
              }
              break;

            case eOperator_mod:
              {
                if(types_are_equal(ret_ty, basic_type_int))
                {
                  ;//ok
                  assert(types_are_equal(ret_ty, args_ty->product.left) && types_are_equal(args_ty->product.left, args_ty->product.right));
                }
                else
                {
                  success = compile_error(node->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                      get_operator_printstr(node->bin_expr.op), get_type_printstr(args_ty));
                }
              }
              break;

            case eOperator_logic_and:
            case eOperator_logic_or:
            case eOperator_logic_not:
              {
                if(types_are_equal(args_ty->product.left, basic_type_bool) && types_are_equal(args_ty->product.left, args_ty->product.right))
                {
                  ;//ok
                  assert(ret_ty == basic_type_bool);
                }
                else
                  success = compile_error(node->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                      get_operator_printstr(node->bin_expr.op), get_type_printstr(args_ty));
              }
              break;

            case eOperator_bit_and:
            case eOperator_bit_or:
            case eOperator_bit_xor:
              if(types_are_equal(args_ty->product.left, basic_type_int) && types_are_equal(args_ty->product.right, basic_type_int))
              {
                ;//ok
              }
              else
                success = compile_error(node->src_loc, "type error (bitwise op)");
              break;

            case eOperator_bit_shift_left:
            case eOperator_bit_shift_right:
              if(types_are_equal(args_ty->product.left, basic_type_int) && types_are_equal(args_ty->product.right, basic_type_char))
              {
                ;//ok
              }
              else
                success = compile_error(node->src_loc, "type error (bitwise op)");
              break;

            case eOperator_less:
            case eOperator_less_eq:
            case eOperator_greater:
            case eOperator_greater_eq:
            case eOperator_eq:
            case eOperator_not_eq:
              {
                if(types_are_equal(args_ty->product.left, basic_type_int)
                    || types_are_equal(args_ty->product.left, basic_type_char)
                    || types_are_equal(args_ty->product.left, basic_type_float)
                    && types_are_equal(args_ty->product.left, args_ty->product.right))
                {
                  ;//ok
                  assert(types_are_equal(ret_ty, basic_type_bool));
                }
                else
                {
                  success = compile_error(node->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                      get_operator_printstr(node->bin_expr.op), get_type_printstr(args_ty));
                }
              }
              break;

            case eOperator_assign:
              {
                ;//ok
                assert(types_are_equal(args_ty->product.left, args_ty->product.right) && types_are_equal(ret_ty, args_ty->product.left));
              }
              break;

            case eOperator_indexer:
              {
                if(ret_ty->width > 0)
                {
                  ;//ok
                }
                else
                  success = compile_error(node->src_loc, "type error (array index): type size = 0");
              }
              break;

            case eOperator_cast:
              {
                if(!types_are_equal(args_ty->product.left, args_ty->product.right))
                {
                  success = false;

                  if(types_are_equal(args_ty->product.left, basic_type_int))
                  {
                    // int <- float | bool | pointer | char
                    success = types_are_equal(args_ty->product.right, basic_type_float)
                      || types_are_equal(args_ty->product.right, basic_type_bool)
                      || types_are_equal(args_ty->product.right, basic_type_char)
                      || (args_ty->product.right->kind == eType_pointer);
                  }
                  else if(types_are_equal(args_ty->product.left, basic_type_char))
                  {
                    // char <- int
                    success = types_are_equal(args_ty->product.right, basic_type_int);
                  }
                  else if(types_are_equal(args_ty->product.left, basic_type_float))
                  {
                    // float <- int
                    success = types_are_equal(args_ty->product.right, basic_type_int);
                  }
                  else if(args_ty->product.left->kind == eType_pointer)
                  {
                    // pointer <- pointer | array | int
                    success = (args_ty->product.right->kind == eType_pointer)
                      || (args_ty->product.right->kind == eType_array)
                      || types_are_equal(args_ty->product.right, basic_type_int);
                  }
                  else if(args_ty->product.left->kind == eType_array)
                  {
                    // array <- pointer | array
                    success = (args_ty->product.right->kind == eType_pointer) || (args_ty->product.right->kind == eType_array);
                  }

                  if(!success)
                  {
                    compile_error(node->src_loc, "invalid cast `%s` <- `%s`",
                        get_type_printstr(args_ty->product.left), get_type_printstr(args_ty->product.right));
                  }
                }
              }
              break;

            default:
              assert(0);
          }
        }
      }
      break;

    case eAstNode_unr_expr:
      success = check_types(node->unr_expr.operand);
      break;

    case eAstNode_proc:
      {
        for(ListItem* li = node->proc.formal_args.first;
            li && success;
            li = li->next)
        {
          success = check_types(KIND(li, eList_ast_node)->ast_node);
        }
        if(!success)
          break;

        success = check_types(node->proc.body);
      }
      break;

    case eAstNode_call:
      {
        for(ListItem* li = node->call.actual_args.first;
            li && success;
            li = li->next)
        {
          success = check_types(KIND(li, eList_ast_node)->ast_node);
        }
      }
      break;

    case eAstNode_return:
      {
        if(node->ret.expr)
        {
          success = check_types(node->ret.expr);
        }
      }
      break;

    case eAstNode_while:
      {
        success = check_types(node->while_.cond_expr) && check_types(node->while_.body);
      }
      break;

    case eAstNode_if:
      {
        success = check_types(node->if_.cond_expr) && check_types(node->if_.body);
        if(node->if_.else_body)
        {
          success = check_types(node->if_.else_body);
        }
      }
      break;

    //case eAstNode_var_occur:
    case eAstNode_lit:
    case eAstNode_type:
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    case eAstNode_asm_block:
      break;

    default:
      assert(0);
  }
  return success;
}

void gen_x86_leave_frame(String* code, int depth)
{
  if(depth > 0)
  {
    str_printfln(code, "push %d", depth);
    str_printfln(code, "call _rt_leave_frame");
  }
  else if(depth == 0)
  {
    str_printfln(code, "mov esp, ebp");
    str_printfln(code, "pop ebp");
  }
  else
    assert(0);
}

void gen_x86_load_lvalue(String* code, AstNode* node)
{
  switch(node->kind)
  {
#if 0
    case eAstNode_var_occur:
      {
        Symbol* occur_sym = node->var_occur.occur_sym;
        Symbol* decl_sym = occur_sym->decl;

        if(decl_sym->is_static_alloc)
        {
          assert(decl_sym->data_loc >=0 );
          str_printfln(code, "push OFFSET(static_area) + %d", decl_sym->data_loc);
        }
        else
        {
          str_printfln(code, "push ebp");

          int decl_scope_offset = occur_sym->scope->nesting_depth - decl_sym->scope->nesting_depth;
          if(decl_scope_offset > 0)
          {
            // Non-local
            str_printfln(code, "add dword ptr [esp], %d", 2*MACHINE_WORD_SIZE);
            str_printfln(code, "push %d", decl_scope_offset);
            str_printfln(code, "call _rt_load_access_link");

            // Load the FP by taking the offset relative to the Access Link
            str_printfln(code, "sub dword ptr [esp], %d", 2*MACHINE_WORD_SIZE);
          }
          else if(decl_scope_offset < 0)
            assert(0);

          if(decl_sym->data_loc >= 0)
          {
            str_printfln(code, "add dword ptr [esp], %d", decl_sym->data_loc);
          }
          else
          {
            str_printfln(code, "sub dword ptr [esp], %d", -decl_sym->data_loc);
          }
        }
      }
      break;
#endif

    case eAstNode_unr_expr:
      {
        AstNode* operand = node->unr_expr.operand;
        if(node->unr_expr.op == eOperator_deref)
        {
          assert(operand->ty->kind == eType_pointer);
          gen_x86_load_rvalue(code, operand);
        }
        else
          assert(0);
      }
      break;

    case eAstNode_bin_expr:
      {
        AstNode* left_operand = node->bin_expr.left_operand;
        AstNode* right_operand = node->bin_expr.right_operand;

        if(node->bin_expr.op == eOperator_indexer)
        {
          gen_x86_load_lvalue(code, left_operand);
          gen_x86_load_rvalue(code, right_operand);

          str_printfln(code, "pop eax");
          str_printfln(code, "mov ebx, %d", node->eval_ty->width);
          str_printfln(code, "imul ebx");

          str_printfln(code, "add dword ptr [esp], eax");
        }
        else
          assert(0);
      }
      break;

    default:
      assert(0);
  }
}

void gen_x86_load_rvalue(String* code, AstNode* node)
{
  switch(node->kind)
  {
#if 0
    case eAstNode_var_occur:
      {
        Type* type = ATTR(node, type, eval_type);
        gen_x86_load_lvalue(code, node);
        str_printfln(code, "push %d", type->width);
        str_printfln(code, "call _rt_load");
      }
      break;
#endif

    case eAstNode_call:
      gen_x86(code, node);
      break;

    case eAstNode_lit:
      {
        switch(node->lit.kind)
        {
          case eLiteral_int:
            str_printfln(code, "push %d", node->lit.int_val);
            break;
          case eLiteral_bool:
            str_printfln(code, "push %d", node->lit.bool_val);
            break;
          case eLiteral_float:
            {
              union BitcastF32ToI32
              {
                float32 float32_val;
                int32 int32_val;
              };

              union BitcastF32ToI32 val = {0};
              val.float32_val = node->lit.float_val;
              str_printfln(code, "push %xh ; %f", val.int32_val, val.float32_val);
            }
            break;
          case eLiteral_char:
            str_printfln(code, "push %d", node->lit.char_val);
            break;

          default:
            assert(0);
        }
      }
      break;

    case eAstNode_bin_expr:
      {
        if(node->bin_expr.op == eOperator_indexer)
        {
          gen_x86_load_lvalue(code, node);
          str_printfln(code, "push %d", node->eval_ty->width);
          str_printfln(code, "call _rt_load");
        }
        else
        {
          gen_x86(code, node);
        }
      }
      break;

    case eAstNode_unr_expr:
      {
        AstNode* operand = node->unr_expr.operand;

        if(node->unr_expr.op == eOperator_address_of)
        {
#if 0
          if(operand->kind == eAstNode_var_occur)
          {
            gen_x86_load_lvalue(code, operand);
          }
          else
#endif
          {
            gen_x86_load_rvalue(code, operand);
          }
        }
        else if(node->unr_expr.op == eOperator_deref)
        {
          gen_x86_load_lvalue(code, node);
          str_printfln(code, "push %d", node->eval_ty->width);
          str_printfln(code, "call _rt_load");
        }
        else
        {
          gen_x86(code, node);
        }
      }
      break;
  }
}

bool gen_x86(String* code, AstNode* node)
{
  bool success = true;

  switch(node->kind)
  {
    case eAstNode_module:
      {
        Scope* module_scope = node->scope;
#if 0
        for(ListItem* li = module_scope->decls[eSymbol_extern_proc]->first;
            li;
            li = li->next)
        {
          Symbol* decl_sym = KIND(li, eList_symbol)->symbol;
          gen_x86(code, decl_sym->ast_node);
        }
#endif
        for(ListItem* li = module_scope->decls[eSymbol_proc].first;
            li;
            li = li->next)
        {
          Symbol* decl_sym = KIND(li, eList_symbol)->symbol;
          gen_x86(code, decl_sym->ast_node);
        }

        AstNode* body = node->module.body;
        Scope* body_scope = body->block.scope;

        str_printfln(code, "startup PROC");
        str_printfln(code, "call _rt_module_prologue");
        str_printfln(code, "sub esp, %d ;alloc locals", body_scope->locals_area_size);
        for(ListItem* li = body->block.stmts.first;
            li;
            li = li->next)
        {
          AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
          gen_x86(code, stmt);
        }
        gen_x86_leave_frame(code, 0);
        str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE); // access link + dummy IP
        str_printfln(code, "ret");
        str_printfln(code, "startup ENDP");
      }
      break;

    case eAstNode_proc:
      {
        Scope* proc_scope = node->scope;

#if 0
        char* label = node->proc.label;
        if(node->proc.is_extern)
        {
          str_printfln(code, "EXTERN %s:PROC", label);
        }
        else
#endif
        {
#if 0
          for(ListItem* li = proc_scope->decls[eSymbol_extern_proc]->first;
              li;
              li = li->next)
          {
            Symbol* decl_sym = KIND(li, eList_symbol)->symbol;
            gen_x86(code, decl_sym->ast_node);
          }
#endif
          for(ListItem* li = proc_scope->decls[eSymbol_proc].first;
              li;
              li = li->next)
          {
            Symbol* decl_sym = KIND(li, eList_symbol)->symbol;
            gen_x86(code, decl_sym->ast_node);
          }

          AstNode* body = node->proc.body;
          Scope* body_scope = body->block.scope;
          str_printfln(code, "%s PROC", node->proc.name);
          str_printfln(code, "push ebp");
          str_printfln(code, "mov ebp, esp");
          str_printfln(code, "sub esp, %d ;alloc locals", body_scope->locals_area_size);
          for(ListItem* li = body->block.stmts.first;
              li;
              li = li->next)
          {
            AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
            gen_x86(code, stmt);
          }
          gen_x86_leave_frame(code, 0);
          str_printfln(code, "ret");
          str_printfln(code, "%s ENDP", node->proc.name);
        }
      }
      break;

    case eAstNode_block:
      {
        Scope* scope = node->block.scope;

        str_printfln(code, "call _rt_block_prologue");
        str_printfln(code, "sub esp, %d", scope->locals_area_size);
        for(ListItem* li = node->block.stmts.first;
            li;
            li = li->next)
        {
          AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
          gen_x86(code, stmt);
        }
        gen_x86_leave_frame(code, 0);
        str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE); // access link + dummy IP
      }
      break;

    case eAstNode_call:
      {
        AstNode* proc = node->call.proc;
        Scope* callee_scope = proc->scope;
//        char* label = proc->proc.label;

        str_printfln(code, ";%s()", node->proc.name);
        str_printfln(code, "sub esp, %d ;alloc ret", callee_scope->ret_area_size);
        for(ListItem* li = node->call.actual_args.last;
            li;
            li = li->prev)
        {
          gen_x86_load_rvalue(code, KIND(li, eList_ast_node)->ast_node);
        }

#if 0
        if(proc->proc.is_extern)
        {
          str_printfln(code, "call %s", node->proc.name);

          if(callee_scope->ret_area_size == MACHINE_WORD_SIZE)
          {
            str_printfln(code, "mov dword ptr [esp], eax ; save the return value");
          }
          else if(callee_scope->ret_area_size != 0)
            assert(0); //FIXME: don't know what to do
        }
        else
#endif
        {
          Symbol* occur_sym = node->occur_sym;
          Scope* caller_scope = occur_sym->scope;

          str_printfln(code, "push ebp");
          str_printfln(code, "add dword ptr [esp], %d", 2*MACHINE_WORD_SIZE);

          int callee_depth_offset = caller_scope->nesting_depth - callee_scope->nesting_depth;
          if(callee_depth_offset >= 0)
          {
            str_printfln(code, "push %d", callee_depth_offset + 1);
            str_printfln(code, "call _rt_load_access_link");
          }

          str_printfln(code, "call %s", node->proc.name);
          str_printfln(code, "add esp, %d ;dealloc args", MACHINE_WORD_SIZE + callee_scope->args_area_size);
        }
      }
      break;

    case eAstNode_stmt:
      {
        AstNode* actual_stmt = node->stmt.stmt;
        gen_x86(code, actual_stmt);
        str_printfln(code, "add esp, %d", actual_stmt->eval_ty->width);
      }
      break;

    case eAstNode_bin_expr:
      {
        AstNode* left_operand = node->bin_expr.left_operand;
        AstNode* right_operand = node->bin_expr.right_operand;

        switch(node->bin_expr.op)
        {
          case eOperator_assign:
            {
              gen_x86_load_rvalue(code, right_operand);
              gen_x86_load_lvalue(code, left_operand);
              str_printfln(code, "push %d", node->eval_ty->width);
              str_printfln(code, "call _rt_store");
            }
            break;

          case eOperator_add:
            {
              gen_x86_load_rvalue(code, left_operand);
              gen_x86_load_rvalue(code, right_operand);

              if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
              {
                str_printfln(code, "pop eax");
                str_printfln(code, "add dword ptr [esp], eax");
              }
              else if(types_are_equal(node->eval_ty, basic_type_float))
              {
                str_printfln(code, "movss xmm0, dword ptr [esp]");
                str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                str_printfln(code, "addss xmm0, dword ptr [esp]");
                str_printfln(code, "movss dword ptr [esp], xmm0");
              }
              else
                assert(0);
            }
            break;

          case eOperator_sub:
            {
              if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
              {
                gen_x86_load_rvalue(code, left_operand);
                gen_x86_load_rvalue(code, right_operand);

                str_printfln(code, "pop eax");
                str_printfln(code, "sub dword ptr [esp], eax");
              }
              else if(types_are_equal(node->eval_ty, basic_type_float))
              {
                gen_x86_load_rvalue(code, right_operand);
                gen_x86_load_rvalue(code, left_operand);

                str_printfln(code, "movss xmm0, dword ptr [esp]");
                str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                str_printfln(code, "subss xmm0, dword ptr [esp]");
                str_printfln(code, "movss dword ptr [esp], xmm0");
              }
              else
                assert(0);
            }
            break;

          case eOperator_mul:
            {
              gen_x86_load_rvalue(code, right_operand);
              gen_x86_load_rvalue(code, left_operand);

              if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
              {
                str_printfln(code, "pop eax");
                str_printfln(code, "imul dword ptr [esp]");
                str_printfln(code, "mov dword ptr [esp], eax");
              }
              else if(types_are_equal(node->eval_ty, basic_type_float))
              {
                str_printfln(code, "movss xmm0, dword ptr [esp]");
                str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                str_printfln(code, "mulss xmm0, dword ptr [esp]");
                str_printfln(code, "movss dword ptr [esp], xmm0");
              }
              else
                assert(0);
            }
            break;

          case eOperator_div:
            {
              gen_x86_load_rvalue(code, right_operand);
              gen_x86_load_rvalue(code, left_operand);

              if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
              {
                str_printfln(code, "pop eax");
                str_printfln(code, "cdq"); // extend EAX into EDX
                str_printfln(code, "idiv dword ptr [esp]");
                str_printfln(code, "mov dword ptr [esp], eax");
              }
              else if(types_are_equal(node->eval_ty, basic_type_float))
              {
                str_printfln(code, "movss xmm0, dword ptr [esp]");
                str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                str_printfln(code, "divss xmm0, dword ptr [esp]");
                str_printfln(code, "movss dword ptr [esp], xmm0");
              }
              else
                assert(0);
            }
            break;

          case eOperator_mod:
            {
              gen_x86_load_rvalue(code, right_operand);
              gen_x86_load_rvalue(code, left_operand);

              if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
              {
                str_printfln(code, "pop eax");
                str_printfln(code, "cdq"); // extend EAX into EDX
                str_printfln(code, "idiv dword ptr [esp]");
                str_printfln(code, "mov dword ptr [esp], edx");
              }
              else
                assert(0);
            }
            break;

          case eOperator_eq:
          case eOperator_not_eq:
            {
              gen_x86_load_rvalue(code, left_operand);
              gen_x86_load_rvalue(code, right_operand);

              if(types_are_equal(left_operand->eval_ty, basic_type_char) || types_are_equal(left_operand->eval_ty, basic_type_int))
              {
                if(types_are_equal(left_operand->eval_ty, basic_type_int))
                {
                  str_printfln(code, "pop eax");
                  str_printfln(code, "pop ebx");
                }
                else if(types_are_equal(left_operand->eval_ty, basic_type_char))
                {
                  str_printfln(code, "movzx eax, byte ptr [esp]");
                  str_printfln(code, "movzx ebx, byte ptr [esp+%d]", MACHINE_WORD_SIZE);
                  str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE);
                }

                str_printfln(code, "cmp ebx, eax");
              }
              else if(types_are_equal(left_operand->eval_ty, basic_type_float))
              {
                str_printfln(code, "movss xmm0, dword ptr [esp]");
                str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                str_printfln(code, "comiss xmm0, dword ptr [esp]");
              }
              else
                assert(0);

              Label label = make_unique_label();
              str_printfln(code, "push 1");
              if(node->bin_expr.op == eOperator_eq)
              {
                str_printfln(code, "jz %s$cmp_eq", label.name);
                str_printfln(code, "xor dword ptr [esp], 1");
                str_printfln(code, "%s$cmp_eq:", label.name);
              }
              else if(node->bin_expr.op == eOperator_not_eq)
              {
                str_printfln(code, "jnz %s$cmp_not_eq", label.name);
                str_printfln(code, "xor dword ptr [esp], 1");
                str_printfln(code, "%s$cmp_not_eq:", label.name);
              }
              else
                assert(0);
              //TODO: Do we need to explicitly handle the case when at least one of floating point operands is NaN,
              //or is there going to be some kind of CPU exception that will halt the program?
            }
            break;

          case eOperator_less:
          case eOperator_greater:
          case eOperator_less_eq:
          case eOperator_greater_eq:
            {
              if(types_are_equal(left_operand->eval_ty, basic_type_char) || types_are_equal(left_operand->eval_ty, basic_type_int))
              {
                gen_x86_load_rvalue(code, left_operand);
                gen_x86_load_rvalue(code, right_operand);

                if(types_are_equal(left_operand->eval_ty, basic_type_int))
                {
                  str_printfln(code, "pop eax");
                  str_printfln(code, "pop ebx");
                }
                else if(types_are_equal(left_operand->eval_ty, basic_type_char))
                {
                  str_printfln(code, "movzx eax, byte ptr [esp]");
                  str_printfln(code, "movzx ebx, byte ptr [esp+%d]", MACHINE_WORD_SIZE);
                  str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE);
                }

                str_printfln(code, "cmp ebx, eax");

                Label label = make_unique_label();
                str_printfln(code, "push 1");
                if(node->bin_expr.op == eOperator_less)
                {
                  str_printfln(code, "jl %s$cmp_less", label.name);
                  str_printfln(code, "xor dword ptr [esp], 1");
                  str_printfln(code, "%s$cmp_less:", label.name);
                }
                else if(node->bin_expr.op == eOperator_less_eq)
                {
                  str_printfln(code, "jle %s$cmp_less_eq", label.name);
                  str_printfln(code, "xor dword ptr [esp], 1");
                  str_printfln(code, "%s$cmp_less_eq:", label.name);
                }
                else if(node->bin_expr.op == eOperator_greater)
                {
                  str_printfln(code, "jg %s$cmp_greater", label.name);
                  str_printfln(code, "xor dword ptr [esp], 1");
                  str_printfln(code, "%s$cmp_greater:", label.name);
                }
                else if(node->bin_expr.op == eOperator_greater_eq)
                {
                  str_printfln(code, "jge %s$cmp_greater_eq", label.name);
                  str_printfln(code, "xor dword ptr [esp], 1");
                  str_printfln(code, "%s$cmp_greater_eq:", label.name);
                }
                else
                  assert(0);
              }
              else if(types_are_equal(left_operand->eval_ty, basic_type_float))
              {
                if(node->bin_expr.op == eOperator_less)
                {
                  gen_x86_load_rvalue(code, right_operand);
                  gen_x86_load_rvalue(code, left_operand);
                  str_printfln(code, "movss xmm0, dword ptr [esp]");
                  str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                  str_printfln(code, "cmpss xmm0, dword ptr [esp], 1");
                }
                else if(node->bin_expr.op == eOperator_less_eq)
                {
                  gen_x86_load_rvalue(code, right_operand);
                  gen_x86_load_rvalue(code, left_operand);
                  str_printfln(code, "movss xmm0, dword ptr [esp]");
                  str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                  str_printfln(code, "cmpss xmm0, dword ptr [esp], 2");
                }
                else if(node->bin_expr.op == eOperator_greater)
                {
                  gen_x86_load_rvalue(code, left_operand);
                  gen_x86_load_rvalue(code, right_operand);
                  str_printfln(code, "movss xmm0, dword ptr [esp]");
                  str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                  str_printfln(code, "cmpss xmm0, dword ptr [esp], 1");
                }
                else if(node->bin_expr.op == eOperator_greater_eq)
                {
                  gen_x86_load_rvalue(code, left_operand);
                  gen_x86_load_rvalue(code, right_operand);
                  str_printfln(code, "movss xmm0, dword ptr [esp]");
                  str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
                  str_printfln(code, "cmpss xmm0, dword ptr [esp], 2");
                }
                else
                  assert(0);
                //TODO: Do we need to explicitly handle the case when at least one of floating point operands is NaN,
                //or is there going to be some kind of CPU exception that will halt the program?

                str_printfln(code, "movss dword ptr [esp], xmm0");
              }
              else
                assert(0);
            }
            break;

          case eOperator_logic_and:
          case eOperator_logic_or:
            {
              gen_x86_load_rvalue(code, left_operand);

              Label label = make_unique_label();
              str_printfln(code, "pop eax");
              if(node->bin_expr.op == eOperator_logic_and)
              {
                str_printfln(code, "and eax, 1");
                str_printfln(code, "jz %s$logic_end", label.name);
              }
              else if(node->bin_expr.op == eOperator_logic_or)
              {
                str_printfln(code, "or eax, 0");
                str_printfln(code, "jnz %s$logic_end", label.name);
              }
              else
                assert(0);

              gen_x86_load_rvalue(code, right_operand);

              str_printfln(code, "pop eax");
              if(node->bin_expr.op == eOperator_logic_and)
              {
                str_printfln(code, "and eax, 1");
                str_printfln(code, "jz %s$logic_end", label.name);
              }
              else if(node->bin_expr.op == eOperator_logic_or)
              {
                str_printfln(code, "or eax, 0");
                str_printfln(code, "jnz %s$logic_end", label.name);
              }
              else
                assert(0);

              str_printfln(code, "%s$logic_end:", label.name);
              str_printfln(code, "push eax");
            }
            break;

          case eOperator_bit_and:
          case eOperator_bit_or:
          case eOperator_bit_xor:
            {
              gen_x86_load_rvalue(code, left_operand);
              gen_x86_load_rvalue(code, right_operand);

              str_printfln(code, "pop eax");
              if(node->bin_expr.op == eOperator_bit_and)
              {
                str_printfln(code, "and dword ptr [esp], eax");
              }
              else if(node->bin_expr.op == eOperator_bit_or)
              {
                str_printfln(code, "or dword ptr [esp], eax");
              }
              else if(node->bin_expr.op == eOperator_bit_xor)
              {
                str_printfln(code, "xor dword ptr [esp], eax");
              }
              else
                assert(0);
            }
            break;

          case eOperator_bit_shift_left:
          case eOperator_bit_shift_right:
            {
              gen_x86_load_rvalue(code, left_operand);
              gen_x86_load_rvalue(code, right_operand);

              str_printfln(code, "mov cl, byte ptr [esp]");
              str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);

              if(node->bin_expr.op == eOperator_bit_shift_left)
              {
                str_printfln(code, "shl dword ptr [esp], cl");
              }
              else if(node->bin_expr.op == eOperator_bit_shift_right)
              {
                str_printfln(code, "shr dword ptr [esp], cl");
              }
              else
                assert(0);
            }
            break;

          case eOperator_cast:
            {
              gen_x86_load_rvalue(code, right_operand);

              if(types_are_equal(left_operand->eval_ty, basic_type_int)
                  && types_are_equal(right_operand->eval_ty, basic_type_float))
              {
                // int <- float
                str_printfln(code, "cvttss2si eax, dword ptr [esp]");
                str_printfln(code, "mov dword ptr [esp], eax");
              }
              if(types_are_equal(left_operand->eval_ty, basic_type_float)
                  && types_are_equal(right_operand->eval_ty, basic_type_int))
              {
                // float <- int
                str_printfln(code, "cvtsi2ss xmm0, dword ptr [esp]");
                str_printfln(code, "movss dword ptr [esp], xmm0");
              }
            }
            break;

          case eOperator_indexer:
            {
              gen_x86_load_rvalue(code, node);
            }
            break;

          default:
            assert(0);
        }
      }
      break;

    case eAstNode_unr_expr:
      {
        AstNode* operand = node->unr_expr.operand;

        if(node->unr_expr.op == eOperator_address_of)
        {
          gen_x86_load_rvalue(code, node);
        }
        else if(node->unr_expr.op == eOperator_neg)
        {
          gen_x86_load_rvalue(code, operand);

          if(types_are_equal(operand->eval_ty, basic_type_int))
          {
            str_printfln(code, "neg dword ptr [esp]");
          }
          else if(types_are_equal(operand->eval_ty, basic_type_float))
          {
            str_printfln(code, "xor dword ptr [esp], %xh", 0x80000000);
          }
          else
            assert(0);
        }
        else if(node->unr_expr.op == eOperator_logic_not)
        {
          assert(0);
        }
        else if(node->unr_expr.op == eOperator_deref)
        {
          gen_x86_load_rvalue(code, node);
        }
        else
          assert(0);
      }
      break;

    case eAstNode_while:
      {
        Label label = node->while_.label = make_unique_label();
        str_printfln(code, "%s$while_eval:", label.name);
        gen_x86_load_rvalue(code, node->while_.cond_expr);

        str_printfln(code, "pop eax");
        str_printfln(code, "and eax, 1");
        str_printfln(code, "jz %s$while_break", label.name);

        if(node->while_.body)
        {
          gen_x86(code, node->while_.body);
        }

        str_printfln(code, "jmp %s$while_eval", label.name);
        str_printfln(code, "%s$while_break:", label.name);
      }
      break;

    case eAstNode_if:
      {
        gen_x86_load_rvalue(code, node->if_.cond_expr);

        str_printfln(code, "pop eax");
        str_printfln(code, "and eax, 1");

        Label label = make_unique_label();
        if(node->if_.else_body)
        {
          str_printfln(code, "jz %s$if_else", label.name);
        }
        else
        {
          str_printfln(code, "jz %s$if_end", label.name);
        }

        gen_x86(code, node->if_.body);
        str_printfln(code, "jmp %s$if_end", label.name);

        if(node->if_.else_body)
        {
          str_printfln(code, "%s$if_else:", label.name);
          gen_x86(code, node->if_.else_body);
        }

        str_printfln(code, "%s$if_end:", label.name);
      }
      break;

    case eAstNode_return:
      {
        if(node->ret.expr)
        {
          gen_x86(code, node->ret.expr);
        }
        gen_x86_leave_frame(code, node->ret.nesting_depth);
        str_printfln(code, "ret");
      }
      break;

    case eAstNode_loop_ctrl:
      {
        AstNode* loop = node->loop_ctrl.loop;
        gen_x86_leave_frame(code, node->loop_ctrl.nesting_depth);
        Label* label = 0;
        if(loop->kind == eAstNode_while)
        {
          label = &loop->while_.label;
          if(node->loop_ctrl.kind == eLoopCtrl_break)
            str_printfln(code, "jmp %s$while_break", label->name);
          else if(node->loop_ctrl.kind == eLoopCtrl_continue)
            str_printfln(code, "jmp %s$while_eval", label->name);
          else
            assert(0);
        }
        else
          assert(0);
      }
      break;

    case eAstNode_empty:
      break; // skip

    case eAstNode_asm_block:
      str_append(code, node->asm_block.asm_text);
      break;

    default:
      assert(0);
  }
  return success;
}
#endif

bool set_types_expr(AstNode* expr);

bool set_types_type(AstNode* type)
{
  bool success = true;

  switch(type->kind)
  {
    case eAstNode_unr_expr:
      if(type->unr_expr.op == eOperator_pointer)
      {
        AstNode* pointee = type->unr_expr.operand;
        if(success = set_types_type(pointee))
        {
          type->ty = type->eval_ty = new_pointer_type(pointee->ty);
        }
      }
      else
        success = compile_error(type->src_loc, "invalid type expression");
      break;

    case eAstNode_bin_expr:
      if(type->bin_expr.op == eOperator_array)
      {
        AstNode* size = type->bin_expr.left_operand;
        AstNode* elem = type->bin_expr.right_operand;
        if(success = set_types_expr(size) && set_types_type(elem))
        {
          type->ty = type->eval_ty = new_array_type(0/*TODO*/, elem->ty);
        }
      }
      else
        success = compile_error(type->src_loc, "invalid type expression");
      break;

    case eAstNode_basic_type:
      switch(type->basic_type.kind)
      {
        case eBasicType_int:
          type->ty = type->eval_ty = basic_type_int;
          break;
        case eBasicType_float:
          type->ty = type->eval_ty = basic_type_float;
          break;
        case eBasicType_bool:
          type->ty = type->eval_ty = basic_type_bool;
          break;
        case eBasicType_char:
          type->ty = type->eval_ty = basic_type_char;
          break;
        case eBasicType_void:
          type->ty = type->eval_ty = basic_type_void;
          break;
        case eBasicType_auto:
          type->ty = type->eval_ty = new_typevar();
          break;
        default:
          assert(0);
      }
      break;

    default:
      success = compile_error(type->src_loc, "invalid type expression");
  }
  return success;
}

bool set_types_var(AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;

  AstNode* type = var->var.type;
  if(success = set_types_type(type))
  {
    var->ty = new_var_type(type->ty);
    var->eval_ty = type->ty;
  }
  return success;
}

bool set_types_bin_expr(AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;

  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  eOperator op = bin_expr->bin_expr.op;

  if(op == eOperator_cast)
  {
    success = set_types_type(left_operand) && set_types_expr(right_operand);
  }
  else if(op == eOperator_array)
  {
    success = set_types_expr(left_operand) && set_types_type(right_operand);
  }
  else
    success = set_types_expr(left_operand) && set_types_expr(right_operand);

  if(success)
  {
    bin_expr->eval_ty = new_typevar();
    bin_expr->ty = new_proc_type(new_product_type(left_operand->ty, right_operand->ty), bin_expr->eval_ty);
  }
  return success;
}

bool set_types_unr_expr(AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;

  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;

  if(op == eOperator_pointer || op == eOperator_array)
  {
    success = compile_error(unr_expr->src_loc, "invalid application of operator");
  }
  else if(success = set_types_expr(operand))
  {
    unr_expr->eval_ty = new_typevar();
    unr_expr->ty = new_proc_type(operand->eval_ty, unr_expr->eval_ty);
  }
  return success;
}

bool set_types_id(AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;
  id->ty = new_typevar();
  id->eval_ty = new_typevar();
  return success;
}

Type* make_type_of_arg_list(AstNode* arg_list);

bool set_types_actual_args(AstNode* arg_list)
{
  assert(KIND(arg_list, eAstNode_arg_list));
  bool success = true;

  for(ListItem* li = arg_list->arg_list.args.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = set_types_expr(arg);
  }
  if(success)
  {
    arg_list->ty = arg_list->eval_ty = make_type_of_arg_list(arg_list);
  }
  return success;
}

bool set_types_call(AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  bool success = true;

  AstNode* call_expr = call->call.expr;
  AstNode* arg_list = call->call.arg_list;
  if(success = set_types_expr(call_expr) && set_types_actual_args(arg_list))
  {
    call->eval_ty = new_typevar();
    call->ty = new_proc_type(arg_list->ty, call->eval_ty);
  }
  return success;
}

bool set_types_lit(AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));
  bool success = true;

  Type* ty = 0;
  switch(lit->lit.kind)
  {
    case eLiteral_int:
      ty = basic_type_int;
      break;
    case eLiteral_float:
      ty = basic_type_float;
      break;
    case eLiteral_char:
      ty = basic_type_char;
      break;
    case eLiteral_bool:
      ty = basic_type_bool;
      break;
    case eLiteral_string:
      ty = new_array_type(0, basic_type_char);
      break;
    default:
      assert(0);
  }
  lit->ty = lit->eval_ty = ty;
  return success;
}

bool set_types_expr(AstNode* expr)
{
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode_bin_expr:
      success = set_types_bin_expr(expr);
      break;
    case eAstNode_unr_expr:
      success = set_types_unr_expr(expr);
      break;
    case eAstNode_id:
      success = set_types_id(expr);
      break;
    case eAstNode_call:
      success = set_types_call(expr);
      break;
    case eAstNode_lit:
      success = set_types_lit(expr);
      break;
    case eAstNode_basic_type:
      success = set_types_type(expr);
      break;
    default:
      assert(0);
  }
  return success;
}

bool set_types_return(AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;

  if(ret->ret.expr)
  {
    AstNode* ret_expr = ret->ret.expr;
    if(success = set_types_expr(ret_expr))
    {
      ret->ty = ret_expr->ty;
      ret->eval_ty = ret_expr->eval_ty;
    }
  }
  else
  {
    ret->ty = ret->eval_ty = basic_type_void;
  }
  return success;
}

bool set_types_block_stmt(AstNode* stmt);

bool set_types_if(AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;

  if(success = set_types_expr(if_->if_.cond_expr))
  {
    AstNode* body = if_->if_.body;
    if(success = set_types_block_stmt(body))
    {
      if_->ty = body->ty;
      if_->eval_ty = body->eval_ty;

      AstNode* else_body = if_->if_.else_body;
      if(else_body)
      {
        success = set_types_block_stmt(else_body);
      }
    }
  }
  return success;
}

bool set_types_while(AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;

  if(success = set_types_expr(while_->while_.cond_expr))
  {
    AstNode* body = while_->while_.body;
    if(success = set_types_block_stmt(body))
    {
      while_->ty = body->ty;
      while_->eval_ty = body->eval_ty;
    }
  }
  return success;
}

bool set_types_block(AstNode* block);

bool set_types_block_stmt(AstNode* stmt)
{
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode_var:
      success = set_types_var(stmt);
      break;
    case eAstNode_block:
      success = set_types_block(stmt);
      break;
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
      success = set_types_expr(stmt);
      break;
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
      stmt->ty = stmt->eval_ty = basic_type_void;
      break;
    case eAstNode_return:
      success = set_types_return(stmt);
      break;
    case eAstNode_if:
      success = set_types_if(stmt);
      break;
    case eAstNode_while:
      success = set_types_while(stmt);
      break;
    default:
      assert(0);
  }
  return success;
}

bool set_types_block(AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;

  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = set_types_block_stmt(stmt);
  }
  if(success)
  {
    block->ty = block->eval_ty = basic_type_void;
  }
  return success;
}

Type* make_type_of_arg_list(AstNode* arg_list)
{
  Type* result = basic_type_void;
  ListItem* li = arg_list->arg_list.args.first;
  if(li)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    result = arg->eval_ty;
    for(li = li->next; li; li = li->next)
    {
      AstNode* next_arg = KIND(li, eList_ast_node)->ast_node;
      result = new_product_type(result, next_arg->eval_ty);
    }
  }
  return result;
}

bool set_types_formal_args(AstNode* arg_list)
{
  assert(KIND(arg_list, eAstNode_arg_list));
  bool success = true;

  for(ListItem* li = arg_list->arg_list.args.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = set_types_var(arg);
  }
  if(success)
  {
    arg_list->ty = arg_list->eval_ty = make_type_of_arg_list(arg_list);
  }
  return success;
}

bool set_types_proc(AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  AstNode* ret_type = proc->proc.ret_type;
  AstNode* arg_list = proc->proc.arg_list;
  if(success = set_types_formal_args(arg_list) && set_types_type(ret_type))
  {
    proc->ty = new_proc_type(arg_list->ty, ret_type->ty);
    proc->eval_ty = basic_type_void;

    if(proc->modifier != eModifier_extern)
    {
      success = set_types_block(proc->proc.body);
    }
  }
  return success;
}

bool set_types_module_stmt(AstNode* stmt)
{
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode_proc:
      success = set_types_proc(stmt);
      break;
    case eAstNode_var:
      success = set_types_var(stmt);
      break;
    case eAstNode_include:
      stmt->ty = stmt->eval_ty = basic_type_void;
      break;
    default:
      assert(0);
  }
  return success;
}

bool set_types_module(AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;

  module->ty = module->eval_ty = basic_type_void;

  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = set_types_module_stmt(stmt);
  }
  return success;
}

bool eval_types_type(AstNode* type)
{
  bool success = true;

  switch(type->kind)
  {
    case eAstNode_unr_expr:
      assert(type->unr_expr.op == eOperator_pointer)
      break;
    case eAstNode_bin_expr:
      assert(type->bin_expr.op == eOperator_array)
      break;
    case eAstNode_basic_type:
      break;
  }
  return success;
}

bool eval_types_expr(AstNode* expr);

bool eval_types_bin_expr(AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;

  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  eOperator op = bin_expr->bin_expr.op;

  if(op == eOperator_cast)
  {
    success = eval_types_type(left_operand) && eval_types_expr(right_operand);
  }
  else if(op == eOperator_array)
  {
    success = eval_types_expr(left_operand) && eval_types_type(right_operand);
  }
  else
    success = eval_types_expr(left_operand) && eval_types_expr(right_operand);

  if(success)
  {
    switch(op)
    {
      case eOperator_cast:
        if(!type_unif(bin_expr->eval_ty, left_operand->eval_ty))
        {
          success = compile_error(bin_expr->src_loc, "type error (cast op)");
        }
        break;

      case eOperator_index:
        if(type_unif(right_operand->eval_ty, basic_type_int))
        {
          if(left_operand->eval_ty->kind == eType_array)
          {
            success = type_unif(left_operand->eval_ty->array.elem, bin_expr->eval_ty);
          }
          else if(left_operand->eval_ty->kind == eType_typevar)
          {
            success = type_unif(left_operand->eval_ty, new_array_type(0, bin_expr->eval_ty));
          }
          else
            success = compile_error(bin_expr->src_loc, "type error (index op)");
        }
        break;

      case eOperator_bit_and:
      case eOperator_bit_or:
      case eOperator_bit_xor:
        if(type_unif(left_operand->eval_ty, right_operand->eval_ty) && type_unif(left_operand->eval_ty, basic_type_int))
        {
          ;//ok
        }
        else
          success = compile_error(bin_expr->src_loc, "type error (bitwise op)");
        break;

      case eOperator_bit_shift_left:
      case eOperator_bit_shift_right:
        if(type_unif(left_operand->eval_ty, basic_type_int) && type_unif(right_operand->eval_ty, basic_type_char))
        {
          ;//ok
        }
        else
          success = compile_error(bin_expr->src_loc, "type error (bitwise op)");
        break;

      default:
        if(type_unif(left_operand->eval_ty, right_operand->eval_ty))
        {
          switch(bin_expr->bin_expr.op)
          {
            case eOperator_less:
            case eOperator_less_eq:
            case eOperator_greater:
            case eOperator_greater_eq:
            case eOperator_eq:
            case eOperator_not_eq:
            case eOperator_logic_and:
            case eOperator_logic_or:
            case eOperator_logic_not:
              if(!type_unif(bin_expr->eval_ty, basic_type_bool))
              {
                success = compile_error(bin_expr->src_loc, "type error (bin expr)");
              }
              break;

            default:
              if(!type_unif(bin_expr->eval_ty, left_operand->eval_ty))
              {
                success = compile_error(bin_expr->src_loc, "type error (bin expr)");
              }
              break;
          }
        }
        else
          success = compile_error(bin_expr->src_loc, "type error (bin expr)");
        break;
    }
    if(success)
    {
      Type* bin_expr_ty = KIND(bin_expr->ty, eType_proc);
      if(!type_unif(bin_expr_ty->proc.ret, bin_expr->eval_ty))
      {
        success = compile_error(bin_expr->src_loc, "type error (bin expr)");
      }
    }
  }
  return success;
}

bool eval_types_id(AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;

  if(!id->decl_sym)
  {
    assert(!id->id.decl_ast);
    id->decl_sym = lookup_all_decl_sym(id->id.name, id->occur_sym->scope);
    if(id->decl_sym)
    {
      id->id.decl_ast = id->decl_sym->ast_node;
    }
    else
      success = compile_error(id->src_loc, "unknown id `%s`", id->id.name);
  }
  if(success)
  {
    AstNode* decl_ast = id->id.decl_ast;
    if(type_unif(decl_ast->ty, id->ty))
    {
      switch(decl_ast->ty->kind)
      {
        case eType_var:
          if((id->decl_sym->scope == id->occur_sym->scope) && (id->decl_sym->order_nr > id->occur_sym->order_nr))
          {
            success = compile_error(id->src_loc, "var `%s` must be declared before its use", id->id.name);
          }
          else
          {
            if(!type_unif(decl_ast->ty->var.type, id->eval_ty))
            {
              success = compile_error(id->src_loc, "type error (var id)");
            }
          }
          break;
        case eType_proc:
          if(!type_unif(decl_ast->ty->proc.ret, id->eval_ty))
          {
            success = compile_error(id->src_loc, "type error (proc id)");
          }
          break;
        default:
          assert(0);
      }
    }
    else
      success = compile_error(id->src_loc, "type error (id)");
  }
  return success;
}

bool eval_types_unr_expr(AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;

  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;

  success = (op == eOperator_pointer ? eval_types_type(operand) : eval_types_expr(operand));

  if(success)
  {
    switch(unr_expr->unr_expr.op)
    {
      case eOperator_neg:
      case eOperator_logic_not:
        if(!type_unif(unr_expr->eval_ty, operand->eval_ty))
        {
          success = compile_error(unr_expr->src_loc, "type error (unr expr)");
        }
        break;

      case eOperator_deref:
        {
          Type* pointee_ty = new_typevar();
          if(type_unif(operand->eval_ty, new_pointer_type(pointee_ty)))
          {
            if(!type_unif(unr_expr->eval_ty, pointee_ty))
            {
              success = compile_error(unr_expr->src_loc, "type error (unr expr)");
            }
          }
          else
            success = compile_error(operand->src_loc, "pointer type expected");
        }
        break;

      case eOperator_address_of:
        if(operand->eval_ty->kind == eType_array)
        {
          // ptr(T) == ptr(array(T))
          success = type_unif(unr_expr->eval_ty, new_pointer_type(operand->eval_ty->array.elem));
        }
        else
        {
          success = type_unif(unr_expr->eval_ty, new_pointer_type(operand->eval_ty));
        }
        if(!success)
        {
          compile_error(unr_expr->src_loc, "type error (unr expr)");
        }
        break;

      case eOperator_pointer:
      case eOperator_array:
        success = compile_error(unr_expr->src_loc, "invalid application of operator");
        break;

      default:
        assert(0);
    }
    Type* unr_expr_ty = KIND(unr_expr->ty, eType_proc);
    if(!type_unif(unr_expr_ty->proc.ret, unr_expr->eval_ty))
    {
      success = compile_error(unr_expr->src_loc, "type error (unr expr)");
    }
  }
  return success;
}

bool eval_types_var(AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;

  if(!type_unif(var->ty->var.type, var->eval_ty))
  {
    success = compile_error(var->src_loc, "type error (var)");
  }
  return success;
}

bool eval_types_formal_args(AstNode* arg_list)
{
  assert(KIND(arg_list, eAstNode_arg_list));
  bool success = true;

  for(ListItem* li = arg_list->arg_list.args.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = eval_types_var(arg);
  }
  return success;
}

bool eval_types_actual_args(AstNode* arg_list)
{
  assert(KIND(arg_list, eAstNode_arg_list));
  bool success = true;

  for(ListItem* li = arg_list->arg_list.args.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = eval_types_expr(arg);
  }
  return success;
}

bool eval_types_call(AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  bool success = true;

  AstNode* call_expr = call->call.expr;
  AstNode* arg_list = call->call.arg_list;
  if(call_expr->kind == eAstNode_id)
  {
    if(success = eval_types_id(call_expr) && eval_types_actual_args(arg_list))
    {
      AstNode* proc = call->call.proc = call_expr->id.decl_ast;
      Type* proc_ty = KIND(proc->ty, eType_proc);
      Type* call_ty = KIND(call->ty, eType_proc);
      if(!type_unif(proc_ty, call_ty))
      {
        success = compile_error(call->src_loc, "call signature mismatch");
      }
    }
  }
  else
    success = compile_error(call->src_loc, "unsupported call expr");
  return success;
}

bool eval_types_expr(AstNode* expr)
{
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode_bin_expr:
      success = eval_types_bin_expr(expr);
      break;
    case eAstNode_unr_expr:
      success = eval_types_unr_expr(expr);
      break;
    case eAstNode_id:
      success = eval_types_id(expr);
      break;
    case eAstNode_call:
      success = eval_types_call(expr);
      break;
    case eAstNode_lit:
    case eAstNode_basic_type:
      break;
    default:
      assert(0);
  }
  return success;
}

bool eval_types_block_stmt(AstNode* stmt);

bool eval_types_if(AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;

  AstNode* body = if_->if_.body;
  AstNode* else_body = if_->if_.else_body;
  if(success = eval_types_block_stmt(body) && (else_body ? eval_types_block_stmt(else_body) : true))
  {
    AstNode* cond_expr = if_->if_.cond_expr;
    if(success = eval_types_expr(cond_expr))
    {
      if(!type_unif(cond_expr->eval_ty, basic_type_bool))
      {
        success = compile_error(cond_expr->src_loc, "bool type was expected in `if` expression");
      }
    }
  }
  return success;
}

bool eval_types_block(AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;

  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = eval_types_block_stmt(stmt);
  }
  return success;
}

bool eval_types_while(AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;

  AstNode* cond_expr = while_->while_.cond_expr;
  AstNode* body = while_->while_.body;
  if(success = eval_types_expr(cond_expr) && eval_types_block_stmt(body))
  {
    if(!type_unif(cond_expr->eval_ty, basic_type_bool))
    {
      success = compile_error(cond_expr->src_loc, "bool type was expected in `while` expression");
    }
  }
  return success;
}

bool eval_types_return(AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;

  AstNode* ret_expr = ret->ret.expr;
  if(success = eval_types_expr(ret_expr))
  {
    AstNode* proc = ret->ret.proc;
    Type* proc_ty = KIND(proc->ty, eType_proc);
    if(!type_unif(ret_expr->eval_ty, proc_ty->proc.ret))
    {
      success = compile_error(ret->src_loc, "type error (return)");
    }
  }
  return success;
}
bool eval_types_block_stmt(AstNode* stmt)
{
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
      success = eval_types_expr(stmt);
      break;
    case eAstNode_if:
      success = eval_types_if(stmt);
      break;
    case eAstNode_while:
      success = eval_types_while(stmt);
      break;
    case eAstNode_block:
      success = eval_types_block(stmt);
      break;
    case eAstNode_return:
      success = eval_types_return(stmt);
      break;
    case eAstNode_var:
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
      break;
    default:
      assert(0);
  }
  return success;
}

bool eval_types_proc(AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  success = eval_types_formal_args(proc->proc.arg_list) && eval_types_block_stmt(proc->proc.body);
  return success;
}

bool eval_types_module_stmt(AstNode* stmt)
{
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode_proc:
      success = eval_types_proc(stmt);
      break;
    case eAstNode_var:
    case eAstNode_include:
      break;
    default:
      assert(0);
  }
  return success;
}

bool eval_types_module(AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;

  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = eval_types_module_stmt(stmt);
  }
  return success;
}

bool translate(char* title, char* file_path, char* hoc_text, String* x86_text)
{
  TokenStream token_stream = {0};
  init_token_stream(&token_stream, hoc_text, file_path);
  get_next_token(&token_stream);

  AstNode* module = 0;
  if(!parse_module(&token_stream, &module))
  {
    return false;
  }
#if 0
  if(DEBUG_enabled)/*>>>*/
  {
    h_printf("--- Parse ---\n");
    DEBUG_print_arena_usage(arena, "arena");

    begin_temp_memory(&arena);
    String str; str_init(&str, arena);
    DEBUG_print_ast_node(&str, 0, "module", module);
    str_dump_to_file(&str, "debug_parse.txt");
    str_cap(&str);
    end_temp_memory(&arena);
  }/*<<<*/
#endif

#if 0
  AstNode* module_body = module->module.body;
  List* module_nodes_list = module_body->block.nodes;
  for(ListItem* li = module_nodes_list->first;
      li;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;

    if(stmt->kind == eAstNode_include)
    {
      AstNode* incl_body = stmt->include.body;
      process_includes(incl_body->block.nodes, module_nodes_list, li);
    }
  }
#endif

  basic_type_bool = new_basic_type(eBasicType_bool);
  basic_type_int = new_basic_type(eBasicType_int);
  basic_type_char = new_basic_type(eBasicType_char);
  basic_type_float = new_basic_type(eBasicType_float);
  basic_type_void = new_basic_type(eBasicType_void);
  subst_list = new_list(arena, eList_type_pair);

  symbol_table = new_symbol_table(&arena, SYMBOL_ARENA_SIZE);
  if(!symtab_module(symbol_table, module))
  {
    return false;
  }

  if(!(set_types_module(module) && eval_types_module(module)))
  {
    return false;
  }

#if 0
  if(!(build_types(module) && eval_types(module)
      && resolve_types(module) && check_types(module)))
  {
    return false;
  }

  for(ListItem* li = symbol_table->scopes->first;
      li;
      li = li->next)
  {
    Scope* scope = KIND(li, eList_scope)->scope;
    int offset = 0;
    for(ListItem* li = scope->decls[eSymbol_var]->first;
        li;
        li = li->next)
    {
      Symbol* symbol = KIND(li, eList_symbol)->symbol;
      if(symbol->is_static_alloc)
      {
        symbol->data_loc = offset;
        offset += symbol->ty->width;
        scope->static_area_size += symbol->ty->width;
      }
      else
      {
        offset += symbol->ty->width;
        symbol->data_loc = -offset;
        scope->locals_area_size += symbol->ty->width;
      }
    }

    offset = 3*MACHINE_WORD_SIZE;
    for(ListItem* li = scope->decls[eSymbol_formal_arg]->first;
        li;
        li = li->next)
    {
      Symbol* symbol = KIND(li, eList_symbol)->symbol;
      symbol->data_loc = offset;
      offset += symbol->ty->width;
      scope->args_area_size += symbol->ty->width;
    }

    for(ListItem* li = scope->decls[eSymbol_ret_var]->first;
        li;
        li = li->next)
    {
      Symbol* symbol = KIND(li, eList_symbol)->symbol;
      symbol->data_loc = offset;
      offset += symbol->ty->width;
      scope->ret_area_size += symbol->ty->width;
    }

    //FIXME: This piece of code feels awfully out of place.
    for(ListItem* li = scope->decls[eSymbol_extern_proc]->first;
        li;
        li = li->next)
    {
      Symbol* decl_sym = KIND(li, eList_symbol)->symbol;
      AstNode* proc = decl_sym->ast_node;
      assert(proc->kind == eAstNode_proc);
      assert(proc->proc.is_extern);

      int args_size = compute_type_width(decl_sym->ty->proc.args);
      char* label = proc->proc.label;
      String decorated_label; str_init(&decorated_label, arena);
      str_printf(&decorated_label, "%s@%d", label, args_size);
      proc->proc.label = str_cap(&decorated_label);
    }
  }

  str_init(x86_text, push_arena(&arena, X86_CODE_ARENA_SIZE));
  str_printfln(x86_text, ".686");
  str_printfln(x86_text, ".XMM");
  str_printfln(x86_text, ".MODEL flat, C");
  str_printfln(x86_text, ".STACK 4096");

  str_printfln(x86_text, ".DATA");
  str_printfln(x86_text, "static_area LABEL BYTE");
  Scope* module_scope = module->module.scope;
  for(ListItem* li = module_scope->decls[eSymbol_var]->first;
      li;
      li = li->next)
  {
    Symbol* symbol = KIND(li, eList_symbol)->symbol;
    int data_size = symbol->ty->width;
    if(symbol->data)
    {
      str_printf(x86_text, "BYTE ");
      uint8* p_data = (uint8*)symbol->data;
      int i;
      for(i = 0; i < data_size - 1; i++)
      {
        str_printf(x86_text, "0%xh,", p_data[i]);
      }
      if(i < data_size)
      {
        str_printf(x86_text, "0%xh", p_data[i]);
      }
      str_printfln(x86_text, "");
    }
    else
    {
      str_printfln(x86_text, "BYTE %d DUP(?) ; %s", data_size, symbol->name);
    }
  }

  str_printfln(x86_text, ".CODE");

  Label label;
  str_printfln(x86_text, "_rt_module_prologue PROC");
  str_printfln(x86_text, "pop ebx ;return address");
  str_printfln(x86_text, "push esp ;access link");
  str_printfln(x86_text, "sub esp, %d ;dummy IP", MACHINE_WORD_SIZE);
  str_printfln(x86_text, "push ebp");
  str_printfln(x86_text, "mov ebp, esp");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_module_prologue ENDP");
  
  str_printfln(x86_text, "_rt_block_prologue PROC");
  str_printfln(x86_text, "pop ebx ;return address");
  str_printfln(x86_text, "push ebp");
  str_printfln(x86_text, "add dword ptr [esp], %d ;access link", 2*MACHINE_WORD_SIZE);
  str_printfln(x86_text, "sub esp, %d ;dummy IP", MACHINE_WORD_SIZE);
  str_printfln(x86_text, "push ebp");
  str_printfln(x86_text, "mov ebp, esp");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_block_prologue ENDP");

  str_printfln(x86_text, "_rt_load PROC");
  str_printfln(x86_text, "pop ebx ;return address");
  str_printfln(x86_text, "pop ecx ;byte count");
  str_printfln(x86_text, "mov esi, dword ptr [esp]");
  str_printfln(x86_text, "mov edi, esp");
  str_printfln(x86_text, "rep movs byte ptr [edi], byte ptr [esi]");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_load ENDP");

  str_printfln(x86_text, "_rt_store PROC");
  str_printfln(x86_text, "pop ebx ;return address;");
  str_printfln(x86_text, "pop ecx");
  str_printfln(x86_text, "pop edi");
  str_printfln(x86_text, "mov esi, esp");
  str_printfln(x86_text, "rep movs byte ptr [edi], byte ptr [esi]");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_store ENDP");

  str_printfln(x86_text, "_rt_leave_frame PROC");
  str_printfln(x86_text, "pop ebx ;return address;");
  label = make_unique_label();
  str_printfln(x86_text, "pop ecx ;depth");
  str_printfln(x86_text, "%s$loop:", label.name);
  str_printfln(x86_text, "mov esp, ebp");
  str_printfln(x86_text, "pop ebp");
  str_printfln(x86_text, "loop %s$loop", label.name);
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_leave_frame ENDP");

  str_printfln(x86_text, "_rt_load_access_link PROC");
  str_printfln(x86_text, "pop ebx ;return address;");
  str_printfln(x86_text, "pop ecx ;declaration scope offset");
  str_printfln(x86_text, "mov esi, dword ptr [esp]");
  label = make_unique_label();
  str_printfln(x86_text, "%s$loop:", label.name);
  str_printfln(x86_text, "mov esi, dword ptr[esi]");
  str_printfln(x86_text, "loop %s$loop", label.name);
  str_printfln(x86_text, "mov dword ptr [esp], esi");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_load_access_link ENDP");

  if(!gen_x86(x86_text, module))
  {
    return false;
  }
  str_printfln(x86_text, "END");
#endif

  return true;
}

