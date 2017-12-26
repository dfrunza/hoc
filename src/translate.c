static int tempvar_id = 0;

static Type* basic_type_bool;
static Type* basic_type_int;
static Type* basic_type_char;
static Type* basic_type_float;
static Type* basic_type_void;
static List* subst_list;
static int typevar_id = 1;

static int last_label_id;

static IrArg ir_arg_bool_true;
static IrArg ir_arg_bool_false;

Label* new_symbolic_label(MemoryArena* arena)
{
  Label* label = mem_push_struct(arena, Label);
  label->kind = eLabel_symbolic;
  label->name = mem_push_array(arena, char, 12);
  h_sprintf(label->name, "L_%d", last_label_id++);
  return label;
}

Label* make_symbolic_label(MemoryArena* arena, char* name)
{
  Label* label = mem_push_struct(arena, Label);
  label->kind = eLabel_symbolic;
  label->name = name;
  return label;
}

Label* make_numeric_label(MemoryArena* arena, int num)
{
  Label* label = mem_push_struct(arena, Label);
  label->kind = eLabel_numeric;
  label->num = num;
  return label;
}

char* new_tempvar_name(char* label)
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

char* get_operator_printstr(eOperator op)
{
  char* str = "???";
  switch(op)
  {
    case eOperator_add:
      str = "+";
      break;
    case eOperator_sub:
      str = "-";
      break;
    case eOperator_mul:
      str = "*";
      break;
    case eOperator_div:
      str = "/";
      break;
    case eOperator_mod:
      str = "mod";
      break;
    case eOperator_neg:
      str = "-";
      break;
    case eOperator_deref:
#if 0
    case eOperator_pointer:
#endif
      str = "^";
      break;
    case eOperator_address_of:
      str = "&";
      break;
    case eOperator_selector:
      str = ".";
      break;
    case eOperator_indirect_selector:
      str = "->";
      break;
#if 0
    case eOperator_pre_decr:
    case eOperator_post_decr:
      str = "--";
      break;
    case eOperator_pre_incr:
    case eOperator_post_incr:
      str = "++";
      break;
#endif
    case eOperator_eq:
      str = "==";
      break;
    case eOperator_not_eq:
      str = "<>";
      break;
    case eOperator_less:
      str = "<";
      break;
    case eOperator_less_eq:
      str = "<=";
      break;
    case eOperator_greater:
      str = ">";
      break;
    case eOperator_greater_eq:
      str = ">=";
      break;
    case eOperator_logic_and:
      str = "and";
      break;
    case eOperator_logic_or:
      str = "or";
      break;
    case eOperator_logic_not:
      str = "not";
      break;
    case eOperator_bit_and:
      str = "&";
      break;
    case eOperator_bit_or:
      str = "|";
      break;
    case eOperator_bit_xor:
      str = "~";
      break;
    case eOperator_bit_not:
      str = "!";
      break;
  }
  return str;
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
    str_append(str, "^");
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

#if 0/*>>>*/
void DEBUG_print_line(String* str, int indent_level, char* message, ...)
{
  for(int i = 0; i < indent_level; i++)
  {
    str_append(str, "  ");
  }

  va_list varargs;
  va_start(varargs, message);
  str_printf_va(str, message, varargs);
  va_end(varargs);

  str_append(str, "\n");
}

void DEBUG_print_type(String* str, int indent_level, char* tag, Type* type)
{
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
}

void DEBUG_print_ast_node(String* str, int indent_level, char* tag, AstNode* node)
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
}

void DEBUG_print_ast_nodes(String* str, int indent_level, char* tag, List* nodes)
{
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
}
#endif/*<<<*/

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

Type* new_array_type(int size, int ndim, Type* elem)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_array;
  type->array.size = size;
  type->array.ndim = ndim;
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

int size_of_array_dim(Type* array_ty, int dim)
{
  assert(dim > 0);
  assert(KIND(array_ty, eType_array));
  assert(dim <= array_ty->array.ndim);

  Type* ty = array_ty;
  int size = array_ty->array.size;
  for(int d = 2; d <= dim; d++)
  {
    ty = ty->array.elem;
    assert(KIND(ty, eType_array));
    size = ty->array.size;
  }
  return size;
}

int array_elem_width(Type* array_ty)
{
  assert(KIND(array_ty, eType_array));

  Type* ty = array_ty->array.elem;
  for(int d = 2; d <= array_ty->array.ndim; d++)
  {
    ty = ty->array.elem;
  }
  return ty->width;
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

Symbol* lookup_decl_sym(char* name, Scope* scope)
{
  Symbol* result = 0;
  while(!result && scope)
  {
    result = lookup_sym(name, &scope->decl_syms);
    scope = scope->encl_scope;
  }
  return result;
}

void alloc_data_area(Symbol* sym, Scope* scope)
{
  sym->data_loc = scope->data_offset;
  int area_size = sym->ty->width;
  if((area_size & (MACHINE_WORD_SIZE-1)) != 0)
    area_size = (area_size + MACHINE_WORD_SIZE) & ~(MACHINE_WORD_SIZE-1);
  scope->data_offset += area_size;
}

Symbol* new_tempvar(MemoryArena* arena, Scope* scope, Type* ty)
{
  Symbol* sym = mem_push_struct(arena, Symbol);
  sym->name = new_tempvar_name("t_");
  sym->ty = ty;
  sym->scope = scope;
  sym->order_nr = scope->sym_count++;
  alloc_data_area(sym, scope);
  append_list_elem(&scope->decl_syms, sym, eList_symbol);
  return sym;
}

Symbol* add_decl_sym(MemoryArena* arena, char* name, Scope* scope, AstNode* ast_node)
{
  Symbol* sym = mem_push_struct(arena, Symbol);
  sym->name = name;
  sym->src_loc = ast_node->src_loc;
  sym->scope = scope;
  sym->ast_node = ast_node;
  sym->order_nr = scope->sym_count++;
  append_list_elem(&scope->decl_syms, sym, eList_symbol);
  return sym;
}

Scope* begin_scope(SymbolContext* sym_context, eScope kind, AstNode* ast_node)
{
  Scope* scope = mem_push_struct(sym_context->arena, Scope);
  scope->kind = kind;
  scope->nesting_depth = sym_context->nesting_depth;
  scope->sym_count = 0;
  scope->data_offset = 0;
  scope->encl_scope = sym_context->active_scope;
  scope->ast_node = ast_node;
  init_list(&scope->decl_syms, arena, eList_symbol);
  sym_context->active_scope = scope;
  append_list_elem(&sym_context->scopes, scope, eList_scope);
  return scope;
}

void end_scope(SymbolContext* sym_context)
{
  Scope* scope = sym_context->active_scope;
  sym_context->active_scope = scope->encl_scope;
}

Scope* begin_nested_scope(SymbolContext* sym_context, eScope kind, AstNode* ast_node)
{
  sym_context->nesting_depth++;
  return begin_scope(sym_context, kind, ast_node);
}

void end_nested_scope(SymbolContext* sym_context)
{
  end_scope(sym_context);
  sym_context->nesting_depth--;
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

bool sym_expr(SymbolContext* sym_context, AstNode* block, AstNode* expr);

bool sym_formal_arg(SymbolContext* sym_context, Scope* proc_scope, AstNode* arg)
{
  assert(KIND(arg, eAstNode_var));
  bool success = true;

  Symbol* decl_sym = lookup_decl_sym(arg->var.name, proc_scope);
  if(decl_sym && (decl_sym->scope == proc_scope))
  {
    success = compile_error(arg->src_loc, "formal arg `%s` has already been declared", arg->var.name);
    compile_error(decl_sym->src_loc, "see the declaration of `%s`", arg->var.name);
  }
  else
  {
    arg->var.decl_sym = add_decl_sym(sym_context->arena, arg->var.name, proc_scope, arg);
  }
  return success;
}

bool sym_var(SymbolContext* sym_context, AstNode* block, AstNode* var)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(var, eAstNode_var));
  bool success = true;

  Symbol* decl_sym = lookup_decl_sym(var->var.name, sym_context->active_scope);
  Scope* proc_scope = find_scope(sym_context->active_scope, eScope_proc);
  if(decl_sym && (decl_sym->scope == sym_context->active_scope || decl_sym->scope == proc_scope))
  {
    success = compile_error(var->src_loc, "name `%s` already declared", var->var.name);
    compile_error(decl_sym->src_loc, "see declaration of `%s`", var->var.name);
  }
  else
  {
    var->var.decl_sym = add_decl_sym(sym_context->arena, var->var.name, sym_context->active_scope, var);
  }
  return success;
}

bool sym_id(SymbolContext* sym_context, AstNode* block, AstNode* id)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(id, eAstNode_id));
  bool success = true;
  Scope* scope = sym_context->active_scope;
  id->id.scope = scope;
  id->id.order_nr = scope->sym_count++;
  return success;
}

bool sym_bin_expr(SymbolContext* sym_context, AstNode* block, AstNode* bin_expr)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;

  success = sym_expr(sym_context, block, bin_expr->bin_expr.left_operand)
    && sym_expr(sym_context, block, bin_expr->bin_expr.right_operand);
  return success;
}

bool sym_unr_expr(SymbolContext* sym_context, AstNode* block, AstNode* unr_expr)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  success = sym_expr(sym_context, block, unr_expr->unr_expr.operand);
  return success;
}

bool sym_actual_args(SymbolContext* sym_context, AstNode* block, AstNode* arg_list)
{
  assert(KIND(arg_list, eAstNode_arg_list));
  bool success = true;

  for(ListItem* li = arg_list->arg_list.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = sym_expr(sym_context, block, arg);
  }
  return success;
}

bool sym_call(SymbolContext* sym_context, AstNode* block, AstNode* call)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(call, eAstNode_call));
  bool success = true;

  AstNode* call_expr = call->call.expr;
  AstNode* arg_list = call->call.arg_list;
  if(call_expr->kind == eAstNode_id)
  {
    success = sym_id(sym_context, block, call_expr) && sym_actual_args(sym_context, block, arg_list);
  }
  else
    success = compile_error(call_expr->src_loc, "unsupported call expr");
  return success;
}

bool sym_index(SymbolContext* sym_context, AstNode* block, AstNode* index)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(index, eAstNode_index));
  bool success = true;

  AstNode* array_expr = index->index.array_expr;
  if(array_expr->kind == eAstNode_id || array_expr->kind == eAstNode_index)
  {
    success = sym_expr(sym_context, block, array_expr) && sym_expr(sym_context, block, index->index.i_expr);
  }
  else
    success = compile_error(array_expr->src_loc, "unsupported index expr");
  return success;
}

bool sym_cast(SymbolContext* sym_context, AstNode* block, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;
  success = sym_expr(sym_context, block, cast->cast.to_type) && sym_expr(sym_context, block, cast->cast.from_expr);
  return success;
}

bool sym_expr(SymbolContext* sym_context, AstNode* block, AstNode* expr)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode_cast:
      success = sym_cast(sym_context, block, expr);
      break;
    case eAstNode_bin_expr:
      success = sym_bin_expr(sym_context, block, expr);
      break;
    case eAstNode_unr_expr:
      success = sym_unr_expr(sym_context, block, expr);
      break;
    case eAstNode_id:
      success = sym_id(sym_context, block, expr);
      break;
    case eAstNode_call:
      success = sym_call(sym_context, block, expr);
      break;
    case eAstNode_pointer:
    case eAstNode_array:
    case eAstNode_basic_type:
    case eAstNode_lit:
    case eAstNode_str:
      break;
    case eAstNode_index:
      success = sym_index(sym_context, block, expr);
      break;
    default:
      assert(0);
  }
  return success;
}

bool sym_block(SymbolContext* sym_context, AstNode* block);
bool sym_block_stmt(SymbolContext* sym_context, AstNode* block, AstNode* stmt);

bool sym_if(SymbolContext* sym_context, AstNode* block, AstNode* if_)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(if_, eAstNode_if));
  bool success = true;

  if(success = sym_expr(sym_context, block, if_->if_.cond_expr) && sym_block_stmt(sym_context, block, if_->if_.body))
  {
    if(success && if_->if_.else_body)
    {
      success = sym_block_stmt(sym_context, block, if_->if_.else_body);
    }
  }
  return success;
}

bool sym_while(SymbolContext* sym_context, AstNode* block, AstNode* while_)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(while_, eAstNode_while));
  bool success = true;

  if(success = sym_expr(sym_context, block, while_->while_.cond_expr))
  {
    while_->while_.scope = begin_nested_scope(sym_context, eScope_while, while_);
    success = sym_block_stmt(sym_context, block, while_->while_.body);
    end_nested_scope(sym_context);
  }
  return success;
}

bool sym_loop_ctrl(SymbolContext* sym_context, AstNode* block, AstNode* stmt)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;

  Scope* loop_scope = find_scope(sym_context->active_scope, eScope_while);
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

bool sym_return(SymbolContext* sym_context, AstNode* block, AstNode* ret)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(ret, eAstNode_return));
  bool success = true;

  Scope* proc_scope = find_scope(sym_context->active_scope, eScope_proc);
  if(proc_scope)
  {
    ret->ret.proc = proc_scope->ast_node;
    if(ret->ret.expr)
      success = sym_expr(sym_context, block, ret->ret.expr);
  }
  else
    success = compile_error(ret->src_loc, "unexpected `return`");
  return success;
}

bool sym_assign(SymbolContext* sym_context, AstNode* block, AstNode* assign)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(assign, eAstNode_assign));
  bool success = true;

  success = sym_expr(sym_context, block, assign->assign.dest_expr) && sym_expr(sym_context, block, assign->assign.source_expr);
  return success;
}

bool sym_block_stmt(SymbolContext* sym_context, AstNode* block, AstNode* stmt)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode_var:
      success = sym_var(sym_context, block, stmt);
      break;
    case eAstNode_if:
      success = sym_if(sym_context, block, stmt);
      break;
    case eAstNode_while:
      success = sym_while(sym_context, block, stmt);
      break;
    case eAstNode_block:
      {
        stmt->block.scope = begin_nested_scope(sym_context, eScope_block, stmt);
        success = sym_block(sym_context, stmt);
        end_nested_scope(sym_context);
      }
      break;
    case eAstNode_assign:
      success = sym_assign(sym_context, block, stmt);
      break;
    case eAstNode_cast:
      success = sym_cast(sym_context, block, stmt);
      break;
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    case eAstNode_str:
      success = sym_expr(sym_context, block, stmt);
      break;
    case eAstNode_loop_ctrl:
      success = sym_loop_ctrl(sym_context, block, stmt);
      break;
    case eAstNode_return:
      success = sym_return(sym_context, block, stmt);
      break;
    case eAstNode_basic_type:
    case eAstNode_empty:
      break;
    case eAstNode_index:
      success = sym_index(sym_context, block, stmt);
      break;
    default:
      assert(0);
  }
  return success;
}

bool sym_block(SymbolContext* sym_context, AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;

  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = sym_block_stmt(sym_context, block, stmt);
  }
  return success;
}

bool sym_proc_body(SymbolContext* sym_context, AstNode* proc)
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
      body->block.scope = begin_scope(sym_context, eScope_block, body);
      success = sym_block(sym_context, body);
      end_scope(sym_context);
    }
    else if(body->kind == eAstNode_empty)
      success = compile_error(proc->src_loc, "proc `%s` must define a body", proc->proc.name);
    else
      assert(0);
  }
  return success;
}

bool sym_formal_args(SymbolContext* sym_context, AstNode* arg_list)
{
  assert(KIND(arg_list, eAstNode_arg_list));
  bool success = true;

  Scope* proc_scope = find_scope(sym_context->active_scope, eScope_proc);
  for(ListItem* li = arg_list->arg_list.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = sym_formal_arg(sym_context, proc_scope, arg);
  }
  return success;
}

bool sym_module_proc(SymbolContext* sym_context, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  Symbol* decl_sym = lookup_decl_sym(proc->proc.name, sym_context->active_scope);
  if(decl_sym && (decl_sym->scope == sym_context->active_scope))
  {
    success = compile_error(proc->src_loc, "name `%s` has already been declared", proc->proc.name);
    compile_error(decl_sym->src_loc, "see the declaration of `%s`", proc->proc.name);
  }
  else
  {
    proc->proc.decl_sym = add_decl_sym(sym_context->arena, proc->proc.name, sym_context->active_scope, proc);
    proc->proc.scope = begin_nested_scope(sym_context, eScope_proc, proc);
    proc->proc.retvar = add_decl_sym(sym_context->arena, new_tempvar_name("r_"), proc->proc.scope, proc->proc.ret_type);
    success = sym_formal_args(sym_context, proc->proc.arg_list) && sym_proc_body(sym_context, proc);
    end_nested_scope(sym_context);
  }
  return success;
}

bool sym_module_var(SymbolContext* sym_context, AstNode* module, AstNode* var)
{
  assert(KIND(module, eAstNode_module));
  assert(KIND(var, eAstNode_var));
  bool success = true;

  Symbol* decl_sym = lookup_decl_sym(var->var.name, sym_context->active_scope);
  if(decl_sym && (decl_sym->scope == sym_context->active_scope))
  {
    success = compile_error(var->src_loc, "name `%s` already declared", var->var.name);
    compile_error(decl_sym->src_loc, "see declaration of `%s`", var->var.name);
  }
  else
  {
    var->var.decl_sym = add_decl_sym(sym_context->arena, var->var.name, sym_context->active_scope, var);
  }
  return success;
}

bool sym_module(SymbolContext* sym_context, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;

  module->module.scope = begin_nested_scope(sym_context, eScope_module, module);

  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    switch(stmt->kind)
    {
      case eAstNode_var:
        success = sym_module_var(sym_context, module, stmt);
        break;
      case eAstNode_proc:
        success = sym_module_proc(sym_context, stmt);
        break;
      case eAstNode_include:
        break;
      default:
        assert(0);
    }
  }
  end_nested_scope(sym_context);
  assert(sym_context->active_scope == 0);
  assert(sym_context->nesting_depth == -1);
  return success;
}

#if 0
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

              Label label = new_symbolic_label();
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

                Label label = new_symbolic_label();
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

              Label label = new_symbolic_label();
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
        Label label = node->while_.label = new_symbolic_label();
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

        Label label = new_symbolic_label();
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

/*------------------       SET TYPES       -------------------- */

bool set_types_expr(AstNode* expr);
bool set_types_type(AstNode* type);
bool set_types_block_stmt(AstNode* stmt);

bool set_types_array(AstNode* array)
{
  assert(KIND(array, eAstNode_array));
  bool success = true;

  if(success = (array->array.size_expr ? set_types_expr(array->array.size_expr) : true) &&
     set_types_type(array->array.elem_expr))
  {
    int size = 0;
    AstNode* size_expr = array->array.size_expr;
    if(size_expr)
    {
      if(size_expr->kind == eAstNode_lit && size_expr->lit.kind == eLiteral_int)
      {
        size = size_expr->lit.int_val;
        if(size < 0)
          success = compile_error(size_expr->src_loc, "array size must be greater than 0");
      }
      else
        success = compile_error(size_expr->src_loc, "array size must be an int literal");
    }
    if(success)
    {
      array->array.ndim = 1;
      array->array.size = size;
      AstNode* elem_expr = array->array.elem_expr;
      if(elem_expr->kind == eAstNode_array)
        array->array.ndim += elem_expr->array.ndim;

      array->ty = array->eval_ty = new_array_type(array->array.size, array->array.ndim, array->array.elem_expr->ty);
    }
  }
  return success;
}

bool set_types_pointer(AstNode* pointer)
{
  assert(KIND(pointer, eAstNode_pointer));
  bool success = true;

  AstNode* pointee = pointer->pointer.pointee;
  if(success = set_types_type(pointee))
  {
    pointer->ty = pointer->eval_ty = new_pointer_type(pointee->ty);
  }
  return success;
}

bool set_types_type(AstNode* type)
{
  bool success = true;

  switch(type->kind)
  {
    case eAstNode_pointer:
      success = set_types_pointer(type);
      break;

    case eAstNode_array:
      success = set_types_array(type);
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

  if(success = set_types_expr(left_operand) && set_types_expr(right_operand))
  {
    bin_expr->eval_ty = new_typevar();
    bin_expr->ty = new_proc_type(new_product_type(left_operand->eval_ty, right_operand->eval_ty), bin_expr->eval_ty);
  }
  return success;
}

bool set_types_unr_expr(AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;

  AstNode* operand = unr_expr->unr_expr.operand;
  if(success = set_types_expr(operand))
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

  for(ListItem* li = arg_list->arg_list.nodes.first;
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
  if(call_expr->kind == eAstNode_id)
  {
    if(success = set_types_id(call_expr) && set_types_actual_args(arg_list))
    {
      call->eval_ty = new_typevar();
      call->ty = new_proc_type(arg_list->ty, call->eval_ty);
    }
  }
  else
    success = compile_error(call->src_loc, "unsupported call expr");
  return success;
}

bool set_types_str(AstNode* str)
{
  assert(KIND(str, eAstNode_str));
  bool success = true;
  str->ty = str->eval_ty = new_array_type(0/*TODO*/, 1, basic_type_char);
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
    default:
      assert(0);
  }
  lit->ty = lit->eval_ty = ty;
  return success;
}

bool set_types_index(AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;

  if(success = set_types_expr(index->index.array_expr) && set_types_expr(index->index.i_expr))
  {
    index->ty = index->index.array_expr->eval_ty;
    index->eval_ty = new_typevar();
  }
  return success;
}

bool set_types_cast(AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;
  AstNode* to_type = cast->cast.to_type;
  AstNode* from_expr = cast->cast.from_expr;
  if(success = set_types_type(to_type) && set_types_expr(from_expr))
  {
    cast->eval_ty = to_type->eval_ty;
    cast->ty = new_product_type(from_expr->eval_ty, cast->eval_ty);
  }
  return success;
}

bool set_types_expr(AstNode* expr)
{
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode_pointer:
      success = set_types_pointer(expr);
      break;
    case eAstNode_array:
      success = set_types_array(expr);
      break;
    case eAstNode_cast:
      success = set_types_cast(expr);
      break;
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
    case eAstNode_str:
      success = set_types_str(expr);
      break;
    case eAstNode_index:
      success = set_types_index(expr);
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

bool set_types_assign(AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;

  AstNode* dest_expr = assign->assign.dest_expr;
  AstNode* source_expr =assign->assign.source_expr;
  if(success = set_types_expr(dest_expr) && set_types_expr(source_expr))
  {
    assign->ty = assign->eval_ty = dest_expr->eval_ty;
  }
  return success;
}

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
    case eAstNode_assign:
      success = set_types_assign(stmt);
      break;
    case eAstNode_cast:
      success = set_types_cast(stmt);
      break;
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    case eAstNode_str:
      success = set_types_expr(stmt);
      break;
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
      stmt->ty = stmt->eval_ty = basic_type_void;
      break;
    case eAstNode_basic_type:
      success = set_types_type(stmt);
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
    case eAstNode_index:
      success = set_types_index(stmt);
      break;
    default:
      assert(0);
  }
  return success;
}

Type* make_type_of_arg_list(AstNode* arg_list)
{
  Type* result = basic_type_void;
  ListItem* li = arg_list->arg_list.nodes.first;
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

  for(ListItem* li = arg_list->arg_list.nodes.first;
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
    proc->ty = new_proc_type(arg_list->eval_ty, ret_type->eval_ty);
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

/*------------------       EVAL TYPES       -------------------- */

bool eval_types_expr(AstNode* expr);
bool eval_types_type(AstNode* type);
bool eval_types_block_stmt(AstNode* stmt);

bool eval_types_array(AstNode* array)
{
  assert(KIND(array, eAstNode_array));
  bool success = true;

  success = eval_types_expr(array->array.size_expr) && eval_types_type(array->array.elem_expr);
  return success;
}

bool eval_types_pointer(AstNode* pointer)
{
  assert(KIND(pointer, eAstNode_pointer));
  bool success = true;
  success = eval_types_expr(pointer->pointer.pointee);
  return success;
}

bool eval_types_type(AstNode* type)
{
  bool success = true;

  switch(type->kind)
  {
    case eAstNode_pointer:
      success = eval_types_pointer(type);
      break;
    case eAstNode_array:
      success = eval_types_array(type);
      break;
    case eAstNode_basic_type:
      break;
  }
  return success;
}

bool eval_types_cast(AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;
  success = eval_types_type(cast->cast.to_type) && eval_types_expr(cast->cast.from_expr);
  return success;
}

bool eval_types_bin_expr(AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;

  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  eOperator op = bin_expr->bin_expr.op;

  if(success = eval_types_expr(left_operand) && eval_types_expr(right_operand))
  {
    switch(op)
    {
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

  if(!id->id.decl_sym)
  {
    assert(!id->id.decl_ast);
    id->id.decl_sym = lookup_decl_sym(id->id.name, id->id.scope);
    if(id->id.decl_sym)
    {
      id->id.decl_ast = id->id.decl_sym->ast_node;
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
          if((id->id.decl_sym->scope == id->id.scope) && (id->id.decl_sym->order_nr > id->id.order_nr))
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

  if(success = eval_types_expr(operand))
  {
    switch(op)
    {
      case eOperator_neg:
      case eOperator_logic_not:
      case eOperator_bit_not:
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

      default:
        assert(0);
    }
    if(success)
    {
      Type* unr_expr_ty = KIND(unr_expr->ty, eType_proc);
      if(!type_unif(unr_expr_ty->proc.ret, unr_expr->eval_ty))
      {
        success = compile_error(unr_expr->src_loc, "type error (unr expr)");
      }
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

  for(ListItem* li = arg_list->arg_list.nodes.first;
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

  for(ListItem* li = arg_list->arg_list.nodes.first;
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
  if(success = eval_types_id(call->call.expr) && eval_types_actual_args(call->call.arg_list))
  {
    AstNode* proc = call->call.proc = KIND(call->call.expr, eAstNode_id)->id.decl_ast;
    if(proc->ty->kind == eType_proc)
    {
      if(!type_unif(proc->ty, call->ty))
        success = compile_error(call->src_loc, "type error (call argument types)");
    }
    else
      success = compile_error(call->src_loc, "type error (call)");
  }
  return success;
}

bool eval_types_index(AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;

  AstNode* array_expr = index->index.array_expr;
  AstNode* i_expr = index->index.i_expr;

  if(success = eval_types_expr(array_expr) && eval_types_expr(i_expr))
  {
    if(type_unif(i_expr->eval_ty, basic_type_int))
    {
      if(array_expr->eval_ty->kind == eType_array)
      {
        if(!type_unif(array_expr->eval_ty->array.elem, index->eval_ty))
        {
          success = compile_error(index->src_loc, "type error (index)");
        }
      }
      else if(array_expr->eval_ty->kind == eType_typevar)
      {
        if(!type_unif(array_expr->eval_ty, new_array_type(0, 1, index->eval_ty)))
        {
          success = compile_error(index->src_loc, "type error (index)");
        }
      }
      else
        success = compile_error(index->src_loc, "type error (index)");
    }
    else
      success = compile_error(index->src_loc, "type error (index)");
  }
  return success;
}

bool eval_types_expr(AstNode* expr)
{
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode_cast:
      success = eval_types_cast(expr);
      break;
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
    case eAstNode_index:
      success = eval_types_index(expr);
      break;
    case eAstNode_lit:
    case eAstNode_str:
    case eAstNode_basic_type:
      break;
    default:
      assert(0);
  }
  return success;
}

bool eval_types_if(AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;

  AstNode* cond_expr = if_->if_.cond_expr;
  AstNode* body = if_->if_.body;
  AstNode* else_body = if_->if_.else_body;

  if(success = eval_types_expr(cond_expr) &&
     eval_types_block_stmt(body) &&
     (else_body ? eval_types_block_stmt(else_body) : true))
  {
    if(!type_unif(cond_expr->eval_ty, basic_type_bool))
    {
      success = compile_error(cond_expr->src_loc, "bool expression was expected");
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
      success = compile_error(cond_expr->src_loc, "bool expression was expected");
    }
  }
  return success;
}

bool eval_types_return(AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;

  AstNode* ret_expr = ret->ret.expr;
  if(ret_expr && (success = eval_types_expr(ret_expr)))
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

bool eval_types_assign(AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;
  success = eval_types_expr(assign->assign.dest_expr) && eval_types_expr(assign->assign.source_expr);
  return success;
}

bool eval_types_block_stmt(AstNode* stmt)
{
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode_assign:
      success = eval_types_assign(stmt);
      break;
    case eAstNode_cast:
      success = eval_types_cast(stmt);
      break;
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    case eAstNode_str:
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
    case eAstNode_basic_type:
      success = eval_types_type(stmt);
      break;
    case eAstNode_index:
      success = eval_types_index(stmt);
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
  success = eval_types_formal_args(proc->proc.arg_list) && eval_types_block_stmt(proc->proc.body)
    && eval_types_type(proc->proc.ret_type);
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

/*------------------       RESOLVE TYPES       -------------------- */

bool resolve_types_expr(AstNode* expr);
bool resolve_types_type(AstNode* type);
bool resolve_types_block_stmt(AstNode* stmt);

bool resolve_types_of_node(AstNode* node)
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
      success = compile_error(node->src_loc, "type error (unresolved type)");
  }
  else
    success = compile_error(node->src_loc, "type error (unresolved type)");

  return success;
}

bool resolve_types_var(AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;

  if(success = resolve_types_of_node(var))
  {
    var->var.decl_sym->ty = var->eval_ty;
  }
  return success;
}

bool resolve_types_formal_args(AstNode* arg_list)
{
  assert(KIND(arg_list, eAstNode_arg_list));
  bool success = true;

  for(ListItem* li = arg_list->arg_list.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = resolve_types_var(arg);
  }
  if(success)
  {
    success = resolve_types_of_node(arg_list);
  }
  return success;
}

bool resolve_types_bin_expr(AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;
  success = resolve_types_expr(bin_expr->bin_expr.left_operand) &&
    resolve_types_expr(bin_expr->bin_expr.right_operand) && resolve_types_of_node(bin_expr);
  return success;
}

bool resolve_types_unr_expr(AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  success = resolve_types_expr(unr_expr->unr_expr.operand) && resolve_types_of_node(unr_expr);
  return success;
}

bool resolve_types_id(AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;
  success = resolve_types_of_node(id);
  return success;
}

bool resolve_types_actual_args(AstNode* arg_list)
{
  assert(KIND(arg_list, eAstNode_arg_list));
  bool success = true;

  for(ListItem* li = arg_list->arg_list.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = resolve_types_expr(arg);
  }
  if(success)
  {
    success = resolve_types_of_node(arg_list);
  }
  return success;
}

bool resolve_types_call(AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  bool success = true;

  assert(call->call.expr->kind == eAstNode_id);
  success = resolve_types_id(call->call.expr) && resolve_types_actual_args(call->call.arg_list)
    && resolve_types_of_node(call);
  return success;
}

bool resolve_types_index(AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;
  success = resolve_types_expr(index->index.array_expr) && resolve_types_expr(index->index.i_expr) &&
    resolve_types_of_node(index);
  return success;
}

bool resolve_types_cast(AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;
  success = resolve_types_type(cast->cast.to_type) && resolve_types_expr(cast->cast.from_expr) &&
    resolve_types_of_node(cast);
  return success;
}

bool resolve_types_expr(AstNode* expr)
{
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode_cast:
      success = resolve_types_cast(expr);
      break;
    case eAstNode_bin_expr:
      success = resolve_types_bin_expr(expr);
      break;
    case eAstNode_unr_expr:
      success = resolve_types_unr_expr(expr);
      break;
    case eAstNode_id:
      success = resolve_types_id(expr);
      break;
    case eAstNode_call:
      success = resolve_types_call(expr);
      break;
    case eAstNode_lit:
    case eAstNode_str:
    case eAstNode_basic_type:
      break;
    case eAstNode_index:
      success = resolve_types_index(expr);
      break;
    default:
      assert(0);
  }
  return success;
}

bool resolve_types_block(AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;

  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = resolve_types_block_stmt(stmt);
  }
  if(success)
  {
    success = resolve_types_of_node(block);
  }
  return success;
}

bool resolve_types_return(AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;
  if(ret->ret.expr)
  {
    success = resolve_types_expr(ret->ret.expr);
  }
  if(success)
  {
    resolve_types_of_node(ret);
  }
  return success;
}

bool resolve_types_if(AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  if(success = resolve_types_expr(if_->if_.cond_expr) && resolve_types_block_stmt(if_->if_.body))
  {
    if(if_->if_.else_body)
    {
      success = resolve_types_block_stmt(if_->if_.else_body);
    }
    if(success)
    {
      success = resolve_types_of_node(if_);
    }
  }
  return success;
}

bool resolve_types_while(AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  success = resolve_types_expr(while_->while_.cond_expr) && resolve_types_block_stmt(while_->while_.body)
    && resolve_types_of_node(while_);
  return success;
}

bool resolve_types_array(AstNode* array)
{
  assert(KIND(array, eAstNode_array));
  bool success = true;

  success = resolve_types_expr(array->array.size_expr) &&
    resolve_types_type(array->array.elem_expr) && resolve_types_of_node(array);
  return success;
}

bool resolve_types_pointer(AstNode* pointer)
{
  assert(KIND(pointer, eAstNode_pointer));
  bool success = true;

  success = resolve_types_expr(pointer->pointer.pointee) && resolve_types_of_node(pointer);
  return success;
}

bool resolve_types_type(AstNode* type)
{
  bool success = true;

  switch(type->kind)
  {
    case eAstNode_pointer:
      success = resolve_types_pointer(type);
      break;
    case eAstNode_array:
      success = resolve_types_array(type);
      break;
    case eAstNode_basic_type:
      break;
  }
  return success;
}

bool resolve_types_assign(AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;
  success = resolve_types_expr(assign->assign.dest_expr) && resolve_types_expr(assign->assign.source_expr) &&
    resolve_types_of_node(assign);
  return success;
}

bool resolve_types_block_stmt(AstNode* stmt)
{
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode_assign:
      success = resolve_types_assign(stmt);
      break;
    case eAstNode_cast:
      success = resolve_types_cast(stmt);
      break;
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    case eAstNode_str:
      success = resolve_types_expr(stmt);
      break;
    case eAstNode_block:
      success = resolve_types_block(stmt);
      break;
    case eAstNode_var:
      success = resolve_types_var(stmt);
      break;
    case eAstNode_return:
      success = resolve_types_return(stmt);
      break;
    case eAstNode_if:
      success = resolve_types_if(stmt);
      break;
    case eAstNode_while:
      success = resolve_types_while(stmt);
      break;
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
      break;
    case eAstNode_basic_type:
      success = resolve_types_type(stmt);
      break;
    case eAstNode_index:
      success = resolve_types_index(stmt);
      break;
    default:
      assert(0);
  }
  return success;
}

bool resolve_types_proc(AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  if(success = resolve_types_formal_args(proc->proc.arg_list) && resolve_types_type(proc->proc.ret_type)
    && resolve_types_block_stmt(proc->proc.body) && resolve_types_of_node(proc))
  {
    proc->proc.decl_sym->ty = proc->eval_ty;
    proc->proc.retvar->ty = proc->proc.ret_type->eval_ty;
  }
  return success;
}

bool resolve_types_module_stmt(AstNode* stmt)
{
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode_proc:
      success = resolve_types_proc(stmt);
      break;
    case eAstNode_var:
      success = resolve_types_var(stmt);
    case eAstNode_include:
      break;
    default:
      assert(0);
  }
  return success;
}

bool resolve_types_module(AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;

  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = resolve_types_module_stmt(stmt);
  }
  return success;
}

/*------------------       CHECK TYPES       -------------------- */

bool check_types_expr(AstNode* expr);
bool check_types_block_stmt(AstNode* stmt);

bool check_types_var(AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;

  if(types_are_equal(var->eval_ty, basic_type_void))
  {
    success = compile_error(var->src_loc, "type of var cannot be `void`");
  }
  return success;
}

bool check_types_formal_args(AstNode* arg_list)
{
  assert(KIND(arg_list, eAstNode_arg_list));
  bool success = true;

  for(ListItem* li = arg_list->arg_list.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = check_types_var(arg);
  }
  return success;
}

bool check_types_cast(AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;

  AstNode* from_expr = cast->cast.from_expr;
  AstNode* to_type = cast->cast.to_type;

  if(success = check_types_expr(from_expr))
  {
    Type* from_ty = from_expr->eval_ty;
    Type* to_ty = to_type->eval_ty;

    if(!types_are_equal(from_ty, to_ty))
    {
      success = false;

      if(types_are_equal(to_ty, basic_type_int))
      {
        // int <- float | bool | pointer(T) | char
        success = types_are_equal(from_ty, basic_type_float) ||
          types_are_equal(from_ty, basic_type_bool) ||
          types_are_equal(from_ty, basic_type_char) ||
          (from_ty->kind == eType_pointer);
      }
      else if(types_are_equal(to_ty, basic_type_char))
      {
        // char <- int
        success = types_are_equal(from_ty, basic_type_int);
      }
      else if(types_are_equal(to_ty, basic_type_float))
      {
        // float <- int
        success = types_are_equal(from_ty, basic_type_int);
      }
      else if(types_are_equal(to_ty, basic_type_bool))
      {
        // bool <- int | pointer(T)
        success = types_are_equal(from_ty, basic_type_int) ||
          (from_ty->kind == eType_pointer);
      }
      else if(to_ty->kind == eType_pointer)
      {
        // pointer(T) <- pointer(P) | int
        success = (from_ty->kind == eType_pointer) ||
          types_are_equal(from_ty, basic_type_int);
      }
      if(!success)
      {
        compile_error(cast->src_loc, "invalid cast `%s` <- `%s`", get_type_printstr(to_ty), get_type_printstr(from_ty));
      }
    }
  }
  return success;
}

bool check_types_bin_expr(AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;

  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  eOperator op = bin_expr->bin_expr.op;

  if(success = check_types_expr(left_operand) && check_types_expr(right_operand))
  {
    Type* expr_ty = KIND(bin_expr->ty, eType_proc);
    Type* operands_ty = expr_ty->proc.args;
    assert(KIND(operands_ty, eType_product));
    Type* left_ty = operands_ty->product.left;
    Type* right_ty = operands_ty->product.right;
    Type* ret_ty = expr_ty->proc.ret;

    switch(op)
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
            assert(types_are_equal(ret_ty, left_ty) && types_are_equal(left_ty, right_ty));
          }
          else
          {
            success = compile_error(bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                    get_operator_printstr(op), get_type_printstr(operands_ty));
          }
        }
        break;

      case eOperator_mod:
        {
          if(types_are_equal(ret_ty, basic_type_int))
          {
            ;//ok
            assert(types_are_equal(ret_ty, left_ty) && types_are_equal(left_ty, right_ty));
          }
          else
          {
            success = compile_error(bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                    get_operator_printstr(op), get_type_printstr(operands_ty));
          }
        }
        break;

      case eOperator_logic_and:
      case eOperator_logic_or:
        {
          if(types_are_equal(left_ty, basic_type_bool) && types_are_equal(left_ty, right_ty))
          {
            ;//ok
            assert(ret_ty == basic_type_bool);
          }
          else
            success = compile_error(bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                    get_operator_printstr(op), get_type_printstr(operands_ty));
        }
        break;

      case eOperator_bit_and:
      case eOperator_bit_or:
      case eOperator_bit_xor:
        if(types_are_equal(left_ty, basic_type_int) && types_are_equal(right_ty, basic_type_int))
        {
          ;//ok
        }
        else
          success = compile_error(bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                  get_operator_printstr(op), get_type_printstr(operands_ty));
        break;

      case eOperator_bit_shift_left:
      case eOperator_bit_shift_right:
        if(types_are_equal(left_ty, basic_type_int) && types_are_equal(right_ty, basic_type_char))
        {
          ;//ok
        }
        else
          success = compile_error(bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                  get_operator_printstr(op), get_type_printstr(operands_ty));
        break;

      case eOperator_less:
      case eOperator_less_eq:
      case eOperator_greater:
      case eOperator_greater_eq:
      case eOperator_eq:
      case eOperator_not_eq:
        {
          if(types_are_equal(left_ty, basic_type_int) ||
             types_are_equal(left_ty, basic_type_char) ||
             types_are_equal(left_ty, basic_type_float) ||
             left_ty->kind == eType_pointer &&
             types_are_equal(left_ty, right_ty))
          {
            ;//ok
            assert(types_are_equal(ret_ty, basic_type_bool));
          }
          else
          {
            success = compile_error(bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                    get_operator_printstr(op), get_type_printstr(operands_ty));
          }
        }
        break;

      default:
        assert(0);
    }
  }
  return success;
}

bool check_types_unr_expr(AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;

  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;

  if(success = check_types_expr(operand))
  {
    Type* expr_ty = KIND(unr_expr->ty, eType_proc);
    Type* operand_ty = expr_ty->proc.args;
    Type* ret_ty = expr_ty->proc.ret;

    switch(op)
    {
      case eOperator_logic_not:
        if(types_are_equal(operand_ty, basic_type_bool))
        {
          ;//ok
          assert(ret_ty == basic_type_bool);
        }
        else
          success = compile_error(unr_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                  get_operator_printstr(op), get_type_printstr(operand_ty));
        break;

      case eOperator_bit_not:
        if(types_are_equal(operand_ty, basic_type_int))
        {
          ;//ok
        }
        else
          success = compile_error(unr_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                  get_operator_printstr(op), get_type_printstr(operand_ty));
        break;

      case eOperator_neg:
        if(types_are_equal(operand_ty, basic_type_int) ||
           types_are_equal(operand_ty, basic_type_float))
        {
          ;//ok
        }
        else
          success = compile_error(unr_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                  get_operator_printstr(op), get_type_printstr(operand_ty));
        break;
    }
  }
  return success;
}

bool check_types_actual_args(AstNode* arg_list)
{
  assert(KIND(arg_list, eAstNode_arg_list));
  bool success = true;

  for(ListItem* li = arg_list->arg_list.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = check_types_expr(arg);
  }
  return success;
}

bool check_types_call(AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  bool success = true;
  success = check_types_actual_args(call->call.arg_list);
  return success;
}

bool check_types_index(AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;

  if(index->eval_ty->width > 0)
  {
    ;//ok
  }
  else
    success = compile_error(index->src_loc, "type error (array index): size of type = 0");

  return success;
}

bool check_types_assign(AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;

  AstNode* dest_expr = assign->assign.dest_expr;
  AstNode* source_expr = assign->assign.source_expr;
  if(success = check_types_expr(dest_expr) && check_types_expr(source_expr))
  {
    if(!type_unif(dest_expr->eval_ty, source_expr->eval_ty))
    {
      success = compile_error(assign->src_loc, "type error (assignment)");
    }
  }
  return success;
}

bool check_types_expr(AstNode* expr)
{
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode_assign:
      success = check_types_assign(expr);
      break;
    case eAstNode_cast:
      success = check_types_cast(expr);
      break;
    case eAstNode_bin_expr:
      success = check_types_bin_expr(expr);
      break;
    case eAstNode_unr_expr:
      success = check_types_unr_expr(expr);
      break;
    case eAstNode_call:
      success = check_types_call(expr);
      break;
    case eAstNode_id:
    case eAstNode_lit:
    case eAstNode_str:
    case eAstNode_basic_type:
      break;
    case eAstNode_index:
      success = check_types_index(expr);
      break;
    default:
      assert(0);
  }
  return success;
}

bool check_types_return(AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;
  if(ret->ret.expr)
  {
    success = check_types_expr(ret->ret.expr);
  }
  return success;
}

bool check_types_while(AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  success = check_types_expr(while_->while_.cond_expr) && check_types_block_stmt(while_->while_.body);
  return success;
}

bool check_types_if(AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  success = check_types_expr(if_->if_.cond_expr) && check_types_block_stmt(if_->if_.body) &&
    (if_->if_.else_body ? check_types_block_stmt(if_->if_.else_body) : true);
  return success;
}

bool check_types_block(AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;

  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = check_types_block_stmt(stmt);
  }
  return success;
}

bool check_types_block_stmt(AstNode* stmt)
{
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode_assign:
      success = check_types_assign(stmt);
      break;
    case eAstNode_cast:
      success = check_types_cast(stmt);
      break;
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    case eAstNode_str:
      success = check_types_expr(stmt);
      break;
    case eAstNode_return:
      success = check_types_return(stmt);
      break;
    case eAstNode_if:
      success = check_types_if(stmt);
      break;
    case eAstNode_while:
      success = check_types_while(stmt);
      break;
    case eAstNode_block:
      success = check_types_block(stmt);
      break;
    case eAstNode_var:
      success = check_types_var(stmt);
      break;
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
      break;
    case eAstNode_index:
      success = check_types_index(stmt);
      break;
    default:
      assert(0);
  }
  return success;
}

bool check_types_proc(AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  success = check_types_formal_args(proc->proc.arg_list) && check_types_block_stmt(proc->proc.body);
  return success;
}

bool check_types_module_stmt(AstNode* stmt)
{
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode_proc:
      success = check_types_proc(stmt);
      break;
    case eAstNode_var:
      success = check_types_var(stmt);
      break;
    default:
      assert(0);
  }
  return success;
}

bool check_types_module(AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;

  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = check_types_module_stmt(stmt);
  }
  return success;
}

/*------------------       IR       -------------------- */

eIrOp conv_operator_to_ir_op(eOperator op)
{
  eIrOp ir_op = eIrOp_None;
  switch(op)
  {
    case eOperator_add:
      ir_op = eIrOp_add;
      break;
    case eOperator_sub:
      ir_op = eIrOp_sub;
      break;
    case eOperator_mul:
      ir_op = eIrOp_mul;
      break;
    case eOperator_div:
      ir_op = eIrOp_div;
      break;
    case eOperator_mod:
      ir_op = eIrOp_mod;
      break;
    case eOperator_neg:
      ir_op = eIrOp_neg;
      break;
    case eOperator_bit_and:
      ir_op = eIrOp_bit_and;
      break;
    case eOperator_bit_or:
      ir_op = eIrOp_bit_or;
      break;
    case eOperator_bit_xor:
      ir_op = eIrOp_bit_xor;
      break;
    case eOperator_bit_shift_left:
      ir_op = eIrOp_bit_shift_left;
      break;
    case eOperator_bit_shift_right:
      ir_op = eIrOp_bit_shift_right;
      break;
    case eOperator_less:
      ir_op = eIrOp_less;
      break;
    case eOperator_less_eq:
      ir_op = eIrOp_less_eq;
      break;
    case eOperator_greater:
      ir_op = eIrOp_greater;
      break;
    case eOperator_greater_eq:
      ir_op = eIrOp_greater_eq;
      break;
    case eOperator_eq:
      ir_op = eIrOp_eq;
      break;
    case eOperator_not_eq:
      ir_op = eIrOp_not_eq;
      break;
    default:
      assert(0);
  }
  return ir_op;
}

void ir_emit_assign(IrContext* ir_context, eIrOp op, IrArg* arg1, IrArg* arg2, IrArg* result)
{
  assert(result->kind == eIrArg_data_obj);
  IrStmt* stmt = mem_push_struct(ir_context->ir_arena, IrStmt);
  ir_context->stmt_count++;
  stmt->kind = eIrStmt_assign;
  stmt->nr = ir_context->next_stmt_nr++;
  stmt->assign.op = op;
  stmt->assign.arg1 = arg1;
  stmt->assign.arg2 = arg2;
  stmt->assign.result = result;
}

void ir_emit_label(IrContext* ir_context, Label* label)
{
  IrStmt* stmt = mem_push_struct(ir_context->ir_arena, IrStmt);
  ir_context->stmt_count++;
  stmt->kind = eIrStmt_label;
  stmt->nr = ir_context->next_stmt_nr; // do not increment the 'next_stmt_nr' for label stmts
  stmt->label = label;
}

void ir_emit_cond_goto(IrContext* ir_context, eIrOp relop, IrArg* arg1, IrArg* arg2, Label* label)
{
  IrStmt* stmt = mem_push_struct(ir_context->ir_arena, IrStmt);
  ir_context->stmt_count++;
  stmt->kind = eIrStmt_cond_goto;
  stmt->nr = ir_context->next_stmt_nr++;
  stmt->cond_goto.relop = relop;
  stmt->cond_goto.arg1 = arg1;
  stmt->cond_goto.arg2 = arg2;
  stmt->cond_goto.label = label;
}

void ir_emit_goto(IrContext* ir_context, Label* label)
{
  if(label->kind == eLabel_symbolic)
    assert(label->name);
  else if(label->kind == eLabel_numeric)
    assert(label->num > 0);
  else
    assert(0);

  IrStmt* stmt = mem_push_struct(ir_context->ir_arena, IrStmt);
  ir_context->stmt_count++;
  stmt->nr = ir_context->next_stmt_nr++;
  stmt->kind = eIrStmt_goto;
  stmt->label = label;
}

void ir_emit_call_param(IrContext* ir_context, IrArg* param)
{
  IrStmt* stmt = mem_push_struct(ir_context->ir_arena, IrStmt);
  ir_context->stmt_count++;
  stmt->kind = eIrStmt_param;
  stmt->nr = ir_context->next_stmt_nr++;
  stmt->param = param;
}

void ir_emit_call(IrContext* ir_context, char* proc_name, int param_count)
{
  IrStmt* stmt = mem_push_struct(ir_context->ir_arena, IrStmt);
  ir_context->stmt_count++;
  stmt->nr = ir_context->next_stmt_nr++;
  stmt->kind = eIrStmt_call;
  stmt->call.name = proc_name;
  stmt->call.param_count = param_count;
}

void ir_emit_return(IrContext* ir_context, IrArg* ret)
{
  IrStmt* stmt = mem_push_struct(ir_context->ir_arena, IrStmt);
  ir_context->stmt_count++;
  stmt->nr = ir_context->next_stmt_nr++;
  stmt->kind = eIrStmt_return;
  stmt->ret = ret;
}

bool gen_ir_expr(IrContext* ir_context, Scope* scope, AstNode* expr);
bool gen_ir_bool_expr(IrContext* ir_context, Scope* scope, AstNode* expr);
bool gen_ir_block_stmt(IrContext* ir_context, Scope* scope, AstNode* stmt);

bool gen_ir_bin_expr(IrContext* ir_context, Scope* scope, AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;

  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  eOperator op = bin_expr->bin_expr.op;

  switch(op)
  {
    case eOperator_add:
    case eOperator_sub:
    case eOperator_mul:
    case eOperator_div:
    case eOperator_mod:
    case eOperator_bit_and:
    case eOperator_bit_or:
    case eOperator_bit_xor:
    case eOperator_bit_shift_left:
    case eOperator_bit_shift_right:
    case eOperator_less:
    case eOperator_less_eq:
    case eOperator_greater:
    case eOperator_greater_eq:
    case eOperator_eq:
    case eOperator_not_eq:
      if(success = gen_ir_expr(ir_context, scope, left_operand) && gen_ir_expr(ir_context, scope, right_operand))
      {
        IrArg* place = bin_expr->place = mem_push_struct(arena, IrArg);
        place->kind = eIrArg_data_obj;
        place->data_obj = new_tempvar(ir_context->sym_arena, scope, bin_expr->eval_ty);
        ir_emit_assign(ir_context, conv_operator_to_ir_op(op), left_operand->place, right_operand->place, bin_expr->place);
      }
      break;

    default:
      assert(0);
  }
  return success;
}

void gen_ir_id(IrContext* ir_context, Scope* scope, AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  IrArg* place = id->place = mem_push_struct(arena, IrArg);
  place->kind = eIrArg_data_obj;
  place->data_obj = id->id.decl_sym;
}

bool gen_ir_unr_expr(IrContext* ir_context, Scope* scope, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;

  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;

  switch(op)
  {
    case eOperator_neg:
      if(success = gen_ir_expr(ir_context, scope, operand))
      {
        IrArg* place = unr_expr->place = mem_push_struct(arena, IrArg);
        place->kind = eIrArg_data_obj;
        place->data_obj = new_tempvar(ir_context->sym_arena, scope, unr_expr->eval_ty);
        ir_emit_assign(ir_context, conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
      }
      break;

    case eOperator_logic_not:
      fail("todo");
      break;

    default:
      assert(0);
  }
  return success;
}

void gen_ir_lit(IrContext* ir_context, Scope* scope, AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));

  IrArg* place = lit->place = mem_push_struct(arena, IrArg);
  place->kind = eIrArg_const;
  IrConstant* const_ = &place->const_;
  switch(lit->lit.kind)
  {
    case eLiteral_int:
      const_->kind = eIrConstant_int;
      const_->int_val = lit->lit.int_val;
      break;

    case eLiteral_float:
      const_->kind = eIrConstant_float;
      const_->float_val = lit->lit.float_val;
      break;

    case eLiteral_bool:
      const_->kind = eIrConstant_int;
      const_->int_val = (int)lit->lit.bool_val;
      break;

    case eLiteral_char:
      const_->kind = eIrConstant_char;
      const_->char_val = lit->lit.char_val;
      break;

    default:
      assert(0);
  }
}

bool gen_ir_bool_unr_expr(IrContext* ir_context, Scope* scope, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;

  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;

  switch(op)
  {
    case eOperator_logic_not:
      operand->label_true = unr_expr->label_false;
      operand->label_false = unr_expr->label_true;
      success = gen_ir_bool_expr(ir_context, scope, operand);
      break;

    default:
      assert(0);
  }
  return success;
}

bool gen_ir_actual_args(IrContext* ir_context, Scope* scope, AstNode* arg_list)
{
  assert(KIND(arg_list, eAstNode_arg_list));
  bool success = true;
  for(ListItem* li = arg_list->arg_list.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = gen_ir_expr(ir_context, scope, arg);
  }
  return success;
}

void gen_ir_call(IrContext* ir_context, Scope* scope, AstNode* call)
{
  assert(KIND(call, eAstNode_call));

  IrArg* place = call->place = mem_push_struct(arena, IrArg);
  place->kind = eIrArg_data_obj;
  place->data_obj = new_tempvar(ir_context->sym_arena, scope, call->eval_ty);

  AstNode* arg_list = call->call.arg_list;
  gen_ir_actual_args(ir_context, scope, arg_list);
  int param_count = 0;
  for(ListItem* li = arg_list->arg_list.nodes.first;
      li;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    ir_emit_call_param(ir_context, arg->place);
    param_count++;
  }

  assert(KIND(call->call.expr, eAstNode_id));
  ir_emit_call(ir_context, call->call.expr->id.name, param_count);
}

bool gen_ir_index(IrContext* ir_context, Scope* scope, AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;
  AstNode* array_expr = index->index.array_expr;
  AstNode* i_expr = index->index.i_expr;

  if(array_expr->kind == eAstNode_id)
  {
    gen_ir_id(ir_context, scope, array_expr);
    index->index.place = array_expr->place;
    index->index.array_ty = array_expr->eval_ty;
    if(success = gen_ir_expr(ir_context, scope, i_expr))
      index->index.i_place = i_expr->place;
  }
  else if(array_expr->kind == eAstNode_index)
  {
    if(success = gen_ir_index(ir_context, scope, array_expr) && gen_ir_expr(ir_context, scope, i_expr))
    {
      index->index.place = array_expr->index.place;
      index->index.array_ty = array_expr->index.array_ty;

      IrArg* t = index->index.i_place = mem_push_struct(arena, IrArg);
      t->kind = eIrArg_data_obj;
      t->data_obj = new_tempvar(arena, scope, basic_type_int);
      IrArg* dim_size = mem_push_struct(arena, IrArg);
      dim_size->kind = eIrArg_const;
      if(dim_size->const_.int_val = size_of_array_dim(index->index.array_ty, index->index.ndim) > 0)
      {
        ir_emit_assign(ir_context, eIrOp_mul, array_expr->index.i_place, dim_size, t);
        ir_emit_assign(ir_context, eIrOp_add, t, i_expr->place, t);
      }
      else
        success = compile_error(i_expr->src_loc, "array dim size = 0");
    }
  }
  else
    assert(0);
  return success;
}

bool gen_ir_index_with_offset(IrContext* ir_context, Scope* scope, AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;
  if(success = gen_ir_index(ir_context, scope, index))
  {
    IrArg* offset = index->index.offset = mem_push_struct(arena, IrArg);
    offset->kind = eIrArg_data_obj;
    offset->data_obj = new_tempvar(arena, scope, basic_type_int);
    IrArg* width = mem_push_struct(arena, IrArg);
    width->kind = eIrArg_const;
    width->const_.int_val = array_elem_width(index->index.array_ty);
    ir_emit_assign(ir_context, eIrOp_mul, index->index.i_place, width, offset);
  }
  return success;
}

bool gen_ir_assign(IrContext* ir_context, Scope* scope, AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;

  AstNode* dest_expr = assign->assign.dest_expr;
  AstNode* source_expr = assign->assign.source_expr;

  if(dest_expr->kind == eAstNode_id)
  {
    if(success = gen_ir_expr(ir_context, scope, dest_expr) && gen_ir_expr(ir_context, scope, source_expr))
      ir_emit_assign(ir_context, eIrOp_None, source_expr->place, 0, dest_expr->place);
  }
  else if(dest_expr->kind == eAstNode_index)
  {
    if(success = gen_ir_index_with_offset(ir_context, scope, dest_expr) && gen_ir_expr(ir_context, scope, source_expr))
    {
      dest_expr->place = dest_expr->index.place;
      ir_emit_assign(ir_context, eIrOp_index_dest, source_expr->place, dest_expr->index.offset, dest_expr->index.place);
    }
  }
  else
    success = compile_error(dest_expr->src_loc, "unsupported expression on the left-side of assignment");
  return success;
}

bool gen_ir_cast(IrContext* ir_context, Scope* scope, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;
  AstNode* to_type = cast->cast.to_type;
  AstNode* from_expr = cast->cast.from_expr;

  if(success = gen_ir_expr(ir_context, scope, from_expr))
  {
    cast->place = from_expr->place;

    if(types_are_equal(to_type->eval_ty, from_expr->eval_ty) ||
       ((to_type->eval_ty->kind == from_expr->eval_ty->kind) && (to_type->eval_ty->kind == eType_pointer)))
    {
      return success;
    }
    bool require_conv = true;
    if(types_are_equal(to_type->eval_ty, basic_type_int))
    {
      // int <- pointer
      require_conv = from_expr->eval_ty->kind != eType_pointer;
    }
    else if(to_type->eval_ty->kind == eType_pointer)
    {
      // pointer <- int
      require_conv = !types_are_equal(from_expr->eval_ty, basic_type_int);
    }
    if(require_conv)
    {
      IrArg* place = cast->place = mem_push_struct(arena, IrArg);
      place->kind = eIrArg_data_obj;
      place->data_obj = new_tempvar(arena, scope, cast->eval_ty);
      eIrOp cast_op = eIrOp_None;
      if(types_are_equal(to_type->eval_ty, basic_type_int))
      {
        if(types_are_equal(from_expr->eval_ty, basic_type_float))
          cast_op = eIrOp_ftoi; // int <- float
        else if(types_are_equal(from_expr->eval_ty, basic_type_bool))
          cast_op = eIrOp_btoi; // int <- bool
        else if(types_are_equal(from_expr->eval_ty, basic_type_char))
          cast_op = eIrOp_ctoi; // int <- char
        else
          assert(0);
      }
      else if(types_are_equal(to_type->eval_ty, basic_type_float))
      {
        if(types_are_equal(from_expr->eval_ty, basic_type_int))
          cast_op = eIrOp_itof; // float <- int
        else
          assert(0);
      }
      else if(types_are_equal(to_type->eval_ty, basic_type_char))
      {
        if(types_are_equal(from_expr->eval_ty, basic_type_int))
          cast_op = eIrOp_itoc; // char <- int
        else
          assert(0);
      }
      else if(types_are_equal(to_type->eval_ty, basic_type_bool))
      {
        if(types_are_equal(from_expr->eval_ty, basic_type_int))
          cast_op = eIrOp_itob; // bool <- int
        else if(from_expr->eval_ty->kind == eType_pointer)
          cast_op = eIrOp_itob; // bool <- pointer(T)
        else
          assert(0);
      }
      ir_emit_assign(ir_context, cast_op, from_expr->place, 0, cast->place);
    }
  }
  return success;
}

bool gen_ir_expr(IrContext* ir_context, Scope* scope, AstNode* expr)
{
  bool success = true;
  switch(expr->kind)
  {
    case eAstNode_bin_expr:
      {
        eOperator op = expr->bin_expr.op;
        if(op == eOperator_logic_and || op == eOperator_logic_or)
        {
          expr->label_true = new_symbolic_label(arena);
          expr->label_false = new_symbolic_label(arena);
          IrArg* place = expr->place = mem_push_struct(arena, IrArg);
          place->kind = eIrArg_data_obj;
          place->data_obj = new_tempvar(ir_context->sym_arena, scope, expr->eval_ty);
          gen_ir_bool_expr(ir_context, scope, expr);
          ir_emit_label(ir_context, expr->label_true);
          ir_emit_assign(ir_context, eIrOp_None, &ir_arg_bool_true, 0, expr->place);
          ir_emit_goto(ir_context, make_numeric_label(arena, ir_context->next_stmt_nr + 2));
          ir_emit_label(ir_context, expr->label_false);
          ir_emit_assign(ir_context, eIrOp_None, &ir_arg_bool_false, 0, expr->place);
        }
        else
          gen_ir_bin_expr(ir_context, scope, expr);
      }
      break;

    case eAstNode_unr_expr:
      {
        eOperator op = expr->unr_expr.op;
        if(op == eOperator_logic_not)
        {
          expr->label_true = new_symbolic_label(arena);
          expr->label_false = new_symbolic_label(arena);
          IrArg* place = expr->place = mem_push_struct(arena, IrArg);
          place->data_obj = new_tempvar(ir_context->sym_arena, scope, expr->eval_ty);
          gen_ir_bool_unr_expr(ir_context, scope, expr);
          ir_emit_label(ir_context, expr->label_true);
          ir_emit_assign(ir_context, eIrOp_None, &ir_arg_bool_true, 0, expr->place);
          ir_emit_goto(ir_context, make_numeric_label(arena, ir_context->next_stmt_nr + 2));
          ir_emit_label(ir_context, expr->label_false);
          ir_emit_assign(ir_context, eIrOp_None, &ir_arg_bool_false, 0, expr->place);
        }
        else
          gen_ir_unr_expr(ir_context, scope, expr);
      }
      break;

    case eAstNode_id:
      gen_ir_id(ir_context, scope, expr);
      break;

    case eAstNode_lit:
      gen_ir_lit(ir_context, scope, expr);
      break;

    case eAstNode_call:
      gen_ir_call(ir_context, scope, expr);
      break;

    case eAstNode_index:
      {
        if(success = gen_ir_index_with_offset(ir_context, scope, expr))
        {
          IrArg* place = expr->place = mem_push_struct(arena, IrArg);
          place->kind = eIrArg_data_obj;
          place->data_obj = new_tempvar(arena, scope, expr->eval_ty);
          ir_emit_assign(ir_context, eIrOp_index_source, expr->index.place, expr->index.offset, expr->place);
        }
      }
      break;

    case eAstNode_cast:
      gen_ir_cast(ir_context, scope, expr);
      break;

    default:
      assert(0);
  }
  return success;
}

bool gen_ir_block(IrContext* ir_context, Scope* scope, AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;

  for(ListItem* li = block->block.nodes.first;
      li;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    gen_ir_block_stmt(ir_context, scope, stmt);
  }
  return success;
}

eOperator negate_relop(eOperator op)
{
  eOperator result = eOperator_None;
  switch(op)
  {
    case eOperator_eq:
      result = eOperator_not_eq;
      break;
    case eOperator_not_eq:
      result = eOperator_eq;
      break;
    case eOperator_less:
      result = eOperator_greater_eq;
      break;
    case eOperator_less_eq:
      result = eOperator_greater;
      break;
    case eOperator_greater:
      result = eOperator_less_eq;
      break;
    case eOperator_greater_eq:
      result = eOperator_less;
      break;
    default:
      assert(0);
  }
  return result;
}

bool gen_ir_bool_bin_expr(IrContext* ir_context, Scope* scope, AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;

  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  eOperator op = bin_expr->bin_expr.op;

  switch(op)
  {
    case eOperator_eq:
    case eOperator_not_eq:
    case eOperator_less:
    case eOperator_less_eq:
    case eOperator_greater:
    case eOperator_greater_eq:
      {
        if(success = gen_ir_expr(ir_context, scope, left_operand) && gen_ir_expr(ir_context, scope, right_operand))
          ir_emit_cond_goto(ir_context, negate_relop(conv_operator_to_ir_op(op)),
                            left_operand->place, right_operand->place, bin_expr->label_false);
      }
      break;

    case eOperator_logic_or:
      assert(bin_expr->label_true);
      assert(bin_expr->label_false);

      left_operand->label_true = bin_expr->label_true;
      left_operand->label_false = new_symbolic_label(arena);
      right_operand->label_true = bin_expr->label_true;
      right_operand->label_false = bin_expr->label_false;
      if(success = gen_ir_bool_expr(ir_context, scope, left_operand))
      {
        ir_emit_label(ir_context, left_operand->label_false);
        success = gen_ir_bool_expr(ir_context, scope, right_operand);
      }
      break;

    case eOperator_logic_and:
      assert(bin_expr->label_true);
      assert(bin_expr->label_false);

      left_operand->label_true = new_symbolic_label(arena);
      left_operand->label_false = bin_expr->label_false;
      right_operand->label_true = bin_expr->label_true;
      right_operand->label_false = bin_expr->label_false;
      if(success = gen_ir_bool_expr(ir_context, scope, left_operand))
      {
        ir_emit_label(ir_context, left_operand->label_true);
        success = gen_ir_bool_expr(ir_context, scope, right_operand);
      }
      break;

    default:
      assert(0);
  }
  return success;
}

bool gen_ir_bool_id(IrContext* ir_context, Scope* scope, AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;
  if(success = gen_ir_expr(ir_context, scope, id))
    ir_emit_cond_goto(ir_context, eIrOp_eq, id->place, &ir_arg_bool_false, id->label_false);
  return success;
}

bool gen_ir_bool_cast(IrContext* ir_context, Scope* scope, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;
  if(success = gen_ir_cast(ir_context, scope, cast))
    ir_emit_cond_goto(ir_context, eIrOp_eq, cast->place, &ir_arg_bool_false, cast->label_false);
  return success;
}

void gen_ir_bool_lit(IrContext* ir_context, Scope* scope, AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));
  if(lit->lit.bool_val != 0)
  {
    ir_emit_goto(ir_context, lit->label_true);
  }
  else
  {
    ir_emit_goto(ir_context, lit->label_false);
  }
}

bool gen_ir_bool_expr(IrContext* ir_context, Scope* scope, AstNode* expr)
{
  bool success = true;
  switch(expr->kind)
  {
    case eAstNode_id:
      success = gen_ir_bool_id(ir_context, scope, expr);
      break;
    case eAstNode_lit:
      gen_ir_bool_lit(ir_context, scope, expr);
      break;
    case eAstNode_bin_expr:
      success = gen_ir_bool_bin_expr(ir_context, scope, expr);
      break;
    case eAstNode_unr_expr:
      success = gen_ir_bool_unr_expr(ir_context, scope, expr);
      break;
    case eAstNode_cast:
      success = gen_ir_bool_cast(ir_context, scope, expr);
      break;
    default:
      assert(0);
  }
  return success;
}

bool gen_ir_while(IrContext* ir_context, Scope* scope, AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;

  AstNode* cond_expr = while_->while_.cond_expr;
  AstNode* body = while_->while_.body;

  while_->label_begin = new_symbolic_label(arena);
  while_->label_next = new_symbolic_label(arena);
  cond_expr->label_true = new_symbolic_label(arena);
  cond_expr->label_false = while_->label_next;
  body->label_next = while_->label_begin;

  ir_emit_label(ir_context, while_->label_begin);
  if(success = gen_ir_bool_expr(ir_context, scope, cond_expr))
  {
    ir_emit_label(ir_context, cond_expr->label_true);
    gen_ir_block_stmt(ir_context, scope, body);
    ir_emit_goto(ir_context, while_->label_begin);
    ir_emit_label(ir_context, while_->label_next);
  }
  return success;
}

bool gen_ir_if(IrContext* ir_context, Scope* scope, AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;

  AstNode* cond_expr = if_->if_.cond_expr;
  AstNode* body = if_->if_.body;
  AstNode* else_body = if_->if_.else_body;

  if_->label_next = new_symbolic_label(arena);
  if(else_body)
  {
    cond_expr->label_true = new_symbolic_label(arena);
    cond_expr->label_false = new_symbolic_label(arena);
    body->label_next = if_->label_next;
    else_body->label_next = if_->label_next;

    if(success = gen_ir_bool_expr(ir_context, scope, cond_expr))
    {
      ir_emit_label(ir_context, cond_expr->label_true);
      gen_ir_block_stmt(ir_context, scope, body);
      ir_emit_goto(ir_context, if_->label_next);
      ir_emit_label(ir_context, cond_expr->label_false);
      gen_ir_block_stmt(ir_context, scope, else_body);
    }
  }
  else
  {
    cond_expr->label_true = new_symbolic_label(arena);
    cond_expr->label_false = if_->label_next;
    body->label_next = if_->label_next;

    if(success = gen_ir_bool_expr(ir_context, scope, cond_expr))
    {
      ir_emit_label(ir_context, cond_expr->label_true);
      gen_ir_block_stmt(ir_context, scope, body);
    }
  }
  if(success)
    ir_emit_label(ir_context, if_->label_next);
  return success;
}

bool gen_ir_return(IrContext* ir_context, Scope* scope, AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;
  if(ret->ret.expr)
  {
    if(success = gen_ir_expr(ir_context, scope, ret->ret.expr))
      ir_emit_return(ir_context, ret->ret.expr->place);
  }
  else
    ir_emit_return(ir_context, 0);
  return success;
}

void gen_ir_var(IrContext* ir_context, Scope* scope, AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  alloc_data_area(var->var.decl_sym, scope);
}

bool gen_ir_block_stmt(IrContext* ir_context, Scope* scope, AstNode* stmt)
{
  bool success = true;

  switch(stmt->kind)
  {
    case eAstNode_assign:
      success = gen_ir_assign(ir_context, scope, stmt);
      break;
    case eAstNode_cast:
      success = gen_ir_cast(ir_context, scope, stmt);
      break;
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_index:
    case eAstNode_lit:
      success = gen_ir_expr(ir_context, scope, stmt);
      break;
    case eAstNode_block:
      success = gen_ir_block(ir_context, stmt->block.scope, stmt);
      break;
    case eAstNode_if:
      success = gen_ir_if(ir_context, scope, stmt);
      break;
    case eAstNode_while:
      success = gen_ir_while(ir_context, scope, stmt);
      break;
    case eAstNode_var:
      gen_ir_var(ir_context, scope, stmt);
      break;
    case eAstNode_str:
      fail("TODO");
      break;
    case eAstNode_return:
      success = gen_ir_return(ir_context, scope, stmt);
      break;
    default:
      assert(0);
  }
  return success;
}

void gen_ir_formal_args(IrContext* ir_context, Scope* scope, AstNode* arg_list)
{
  assert(KIND(arg_list, eAstNode_arg_list));
  for(ListItem* li = arg_list->arg_list.nodes.first;
      li;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    gen_ir_var(ir_context, scope, arg);
  }
}

bool gen_ir_proc(IrContext* ir_context, Scope* scope, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  IrArg* place = proc->place = mem_push_struct(arena, IrArg);
  place->kind = eIrArg_data_obj;
  place->data_obj = proc->proc.retvar;

  if((proc->modifier & eModifier_extern) != 0)
  {
    fail("TODO");
  }
  else
  {
    AstNode* body = proc->proc.body;
    assert(KIND(body, eAstNode_block));
    alloc_data_area(proc->proc.retvar, proc->proc.scope);
    gen_ir_formal_args(ir_context, proc->proc.scope, proc->proc.arg_list);
    if(success = gen_ir_block_stmt(ir_context, body->block.scope, body))
    {
      ir_emit_return(ir_context, 0);
    }
  }
  return success;
}

bool gen_ir_module_stmt(IrContext* ir_context, Scope* scope, AstNode* stmt)
{
  bool success = true;
  switch(stmt->kind)
  {
    case eAstNode_proc:
      ir_emit_label(ir_context, make_symbolic_label(arena, stmt->proc.name));
      success = gen_ir_proc(ir_context, scope, stmt);
      break;

    default:
      assert(0);
  }
  return success;
}

bool gen_ir_module(IrContext* ir_context, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;

  for(ListItem* li = module->module.nodes.first;
      li;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = gen_ir_module_stmt(ir_context, module->module.scope, stmt);
  }
  return success;
}

void DEBUG_print_ir_op(String* text, eIrOp op)
{
  switch(op)
  {
    case eIrOp_add:
      str_printf(text, "+");
      break;
    case eIrOp_sub:
      str_printf(text, "-");
      break;
    case eIrOp_mul:
      str_printf(text, "*");
      break;
    case eIrOp_div:
      str_printf(text, "/");
      break;
    case eIrOp_mod:
      str_printf(text, "mod");
      break;
    case eIrOp_neg:
      str_printf(text, "-");
      break;
    case eIrOp_eq:
      str_printf(text, "==");
      break;
    case eIrOp_not_eq:
      str_printf(text, "<>");
      break;
    case eIrOp_less:
      str_printf(text, "<");
      break;
    case eIrOp_less_eq:
      str_printf(text, "<=");
      break;
    case eIrOp_greater:
      str_printf(text, ">");
      break;
    case eIrOp_greater_eq:
      str_printf(text, ">=");
      break;
    case eIrOp_logic_and:
      str_printf(text, "and");
      break;
    case eIrOp_logic_or:
      str_printf(text, "or");
      break;
    case eIrOp_logic_not:
      str_printf(text, "not");
      break;
    case eIrOp_bit_and:
      str_printf(text, "&");
      break;
    case eIrOp_bit_or:
      str_printf(text, "|");
      break;
    case eIrOp_bit_xor:
      str_printf(text, "~");
      break;
    case eIrOp_bit_not:
      str_printf(text, "!");
      break;
    case eIrOp_bit_shift_left:
      str_printf(text, "<<");
      break;
    case eIrOp_bit_shift_right:
      str_printf(text, ">>");
      break;
    case eIrOp_itof:
      str_printf(text, "itof");
      break;
    case eIrOp_itoc:
      str_printf(text, "itoc");
      break;
    case eIrOp_itob:
      str_printf(text, "itob");
      break;
    case eIrOp_ftoi:
      str_printf(text, "ftoi");
      break;
    case eIrOp_ctoi:
      str_printf(text, "ctoi");
      break;
    case eIrOp_btoi:
      str_printf(text, "btoi");
      break;

    default:
      assert(0);
  }
}

void DEBUG_print_ir_arg(String* text, IrArg* arg)
{
  switch(arg->kind)
  {
    case eIrArg_data_obj:
      str_printf(text, "%s", arg->data_obj->name);
      break;

    case eIrArg_const:
      switch(arg->const_.kind)
      {
        case eIrConstant_int:
          str_printf(text, "%d", arg->const_.int_val);
          break;
        case eIrConstant_float:
          str_printf(text, "%f", arg->const_.float_val);
          break;
        case eIrConstant_char:
          str_printf(text, "%c", arg->const_.char_val);
          break;
        default:
          assert(0);
      }
      break;

    default:
      assert(0);
  }
}

void DEBUG_print_ir_stmt(String* text, IrStmt* stmt)
{
  switch(stmt->kind)
  {
    case eIrStmt_assign:
      {
        struct IrStmt_assign* assign = &stmt->assign;
        switch(assign->op)
        {
          case eIrOp_None:
            DEBUG_print_ir_arg(text, assign->result);
            str_printf(text, " = ");
            DEBUG_print_ir_arg(text, assign->arg1);
            break;

          /* bin_ops */
          case eIrOp_add:
          case eIrOp_sub:
          case eIrOp_mul:
          case eIrOp_div:
          case eIrOp_mod:
          case eIrOp_eq:
          case eIrOp_not_eq:
          case eIrOp_less:
          case eIrOp_less_eq:
          case eIrOp_greater:
          case eIrOp_greater_eq:
          case eIrOp_logic_and:
          case eIrOp_logic_or:
          case eIrOp_logic_not:
          case eIrOp_bit_and:
          case eIrOp_bit_or:
          case eIrOp_bit_xor:
          case eIrOp_bit_not:
          case eIrOp_bit_shift_left:
          case eIrOp_bit_shift_right:
            DEBUG_print_ir_arg(text, assign->result);
            str_printf(text, " = ");
            DEBUG_print_ir_arg(text, assign->arg1);
            str_printf(text, " ");
            DEBUG_print_ir_op(text, assign->op);
            str_printf(text, " ");
            DEBUG_print_ir_arg(text, assign->arg2);
            break;

          /* unr_ops */
          case eIrOp_neg:
          case eIrOp_itof:
          case eIrOp_itoc:
          case eIrOp_itob:
          case eIrOp_ftoi:
          case eIrOp_ctoi:
          case eIrOp_btoi:
            DEBUG_print_ir_arg(text, assign->result);
            str_printf(text, " = ");
            DEBUG_print_ir_op(text, assign->op);
            str_printf(text, " ");
            DEBUG_print_ir_arg(text, assign->arg1);
            break;

          case eIrOp_index_dest:
            DEBUG_print_ir_arg(text, assign->result);
            str_printf(text, "[");
            DEBUG_print_ir_arg(text, assign->arg2);
            str_printf(text, "] = ");
            DEBUG_print_ir_arg(text, assign->arg1);
            break;

          case eIrOp_index_source:
            DEBUG_print_ir_arg(text, assign->result);
            str_printf(text, " = ");
            DEBUG_print_ir_arg(text, assign->arg1);
            str_printf(text, "[");
            DEBUG_print_ir_arg(text, assign->arg2);
            str_printf(text, "]");
            break;

          default:
            assert(0);
        }
      }
      break;

    case eIrStmt_cond_goto:
      {
        struct IrStmt_cond_goto* cond_goto = &stmt->cond_goto;
        str_printf(text, "if ");
        DEBUG_print_ir_arg(text, cond_goto->arg1);
        str_printf(text, " ");
        DEBUG_print_ir_op(text, cond_goto->relop);
        str_printf(text, " ");
        DEBUG_print_ir_arg(text, cond_goto->arg2);
        str_printf(text, " goto %s", cond_goto->label->name);
      }
      break;

    case eIrStmt_goto:
      {
        Label* label = stmt->label;
        if(label->kind == eLabel_symbolic)
          str_printf(text, "goto %s", label->name);
        else if(label->kind == eLabel_numeric)
          str_printf(text, "goto %d", label->num);
        else
          assert(0);
      }
      break;

    case eIrStmt_param:
      {
        str_printf(text, "param ");
        DEBUG_print_ir_arg(text, stmt->param);
      }
      break;

    case eIrStmt_call:
      {
        struct IrStmt_call* call = &stmt->call;
        str_printf(text, "call %s, %d", call->name, call->param_count);
      }
      break;

    case eIrStmt_return:
      {
        str_printf(text, "return ");
        if(stmt->ret)
          DEBUG_print_ir_arg(text, stmt->ret);
      }
      break;

    default:
      str_printf(text, "???");
  }
}

void DEBUG_print_ir_code(IrContext* ir_context, char* file_path)
{
  begin_temp_memory(&arena);
  String text = {0};
  str_init(&text, arena);

  for(int i = 0; i < ir_context->stmt_count; i++)
  {
    IrStmt* stmt = (IrStmt*)(ir_context->ir_arena->base) + i;
    if(stmt->kind == eIrStmt_label)
    {
      str_printf(&text, "%6s:", stmt->label->name);
    }
    else
    {
      str_printf(&text, "%6d: ", stmt->nr);
      DEBUG_print_ir_stmt(&text, stmt);
    }
    str_printfln(&text, "");
  }

#if 0
  int text_len = str_len(&text);
  file_write_bytes(file_path, (uint8*)text.head, text_len);
#endif
  str_dump_to_file(&text, file_path);
  end_temp_memory(&arena);
}

bool translate(char* title, char* file_path, char* hoc_text, String* x86_text)
{
  basic_type_bool = new_basic_type(eBasicType_bool);
  basic_type_int = new_basic_type(eBasicType_int);
  basic_type_char = new_basic_type(eBasicType_char);
  basic_type_float = new_basic_type(eBasicType_float);
  basic_type_void = new_basic_type(eBasicType_void);
  subst_list = new_list(arena, eList_type_pair);

  ir_arg_bool_true.kind = eIrArg_const;
  ir_arg_bool_true.const_.kind = eIrConstant_int;
  ir_arg_bool_true.const_.int_val = 1;
  ir_arg_bool_false.kind = eIrArg_const;
  ir_arg_bool_false.const_.kind = eIrConstant_int;
  ir_arg_bool_false.const_.int_val = 0;

  SymbolContext sym_context = {0};
  sym_context.arena = push_arena(&arena, SYMBOL_ARENA_SIZE);
  sym_context.nesting_depth = -1;
  init_list(&sym_context.scopes, sym_context.arena, eList_scope);

  IrContext ir_context = {0};
  ir_context.ir_arena = push_arena(&arena, IR_CODE_ARENA_SIZE);
  ir_context.sym_arena = sym_context.arena;
  ir_context.stmt_count = 0;

  TokenStream token_stream = {0};
  init_token_stream(&token_stream, hoc_text, file_path);
  get_next_token(&token_stream);

  AstNode* module = 0;
  if(!(parse_module(&token_stream, &module) &&
       sym_module(&sym_context, module) &&
       set_types_module(module) &&
       eval_types_module(module) &&
       resolve_types_module(module) &&
       check_types_module(module)))
  {
    return false;
  }
  if(!gen_ir_module(&ir_context, module))
  {
    return false;
  }
  DEBUG_print_ir_code(&ir_context, "./out.ir");

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

#if 0
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
  label = new_symbolic_label();
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
  label = new_symbolic_label();
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

