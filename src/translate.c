global_var int tempvar_id;

global_var Type* basic_type_bool;
global_var Type* basic_type_int;
global_var Type* basic_type_char;
global_var Type* basic_type_float;
global_var Type* basic_type_void;
global_var List* subst_list;
global_var int typevar_id = 1;

global_var int last_label_id;

global_var Symbol* bool_true;
global_var Symbol* bool_false;

void gen_label_name(MemoryArena* arena, IrLabel* label)
{
  label->name = mem_push_array(arena, char, 12);
  h_sprintf(label->name, "L_%d", last_label_id++);
}

IrLabel* new_gen_label(MemoryArena* arena)
{
  IrLabel* label = mem_push_struct(arena, IrLabel);
  gen_label_name(arena, label);
  return label;
}

IrLabel* new_label_by_name(MemoryArena* arena, char* name)
{
  IrLabel* label = mem_push_struct(arena, IrLabel);
  label->name = name;
  return label;
}

char* new_tempvar_name(char* label)
{
  String str; str_init(&str, arena);
  str_printf(&str, "%s%d", label, tempvar_id++);
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
    {
      str = "+";
    }
    break;
    
    case eOperator_sub:
    {
      str = "-";
    }
    break;
    
    case eOperator_mul:
    {
      str = "*";
    }
    break;
    
    case eOperator_div:
    {
      str = "/";
    }
    break;
    
    case eOperator_mod:
    {
      str = "mod";
    }
    break;
    
    case eOperator_neg:
    {
      str = "-";
    }
    break;
    
    case eOperator_deref:
#if 0
    case eOperator_pointer:
#endif
    str = "^";
    break;
    
    case eOperator_address_of:
    {
      str = "&";
    }
    break;
    
    case eOperator_selector:
    {
      str = ".";
    }
    break;
    
    case eOperator_indirect_selector:
    {
      str = "->";
    }
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
    {
      str = "==";
    }
    break;
    
    case eOperator_not_eq:
    {
      str = "<>";
    }
    break;
    
    case eOperator_less:
    {
      str = "<";
    }
    break;
    
    case eOperator_less_eq:
    {
      str = "<=";
    }
    break;
    
    case eOperator_greater:
    {
      str = ">";
    }
    break;
    
    case eOperator_greater_eq:
    {
      str = ">=";
    }
    break;
    
    case eOperator_logic_and:
    {
      str = "and";
    }
    break;
    
    case eOperator_logic_or:
    {
      str = "or";
    }
    break;
    
    case eOperator_logic_not:
    {
      str = "not";
    }
    break;
    
    case eOperator_bit_and:
    {
      str = "&";
    }
    break;
    
    case eOperator_bit_or:
    {
      str = "|";
    }
    break;
    
    case eOperator_bit_xor:
    {
      str = "~";
    }
    break;
    
    case eOperator_bit_not:
    {
      str = "!";
    }
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
    str_printf(str, "type_%d", type->typevar.id);
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
      {
        are_equal = (type_a->basic.kind == type_b->basic.kind);
      }
      break;
      
      case eType_proc:
      {
        are_equal = types_are_equal(type_a->proc.args, type_b->proc.args)
          && types_are_equal(type_a->proc.ret, type_b->proc.ret);
      }
      break;
      
      case eType_pointer:
      {
        are_equal = types_are_equal(type_a->pointer.pointee, type_b->pointer.pointee);
      }
      break;
      
      case eType_product:
      {
        are_equal = types_are_equal(type_a->product.left, type_b->product.right);
      }
      break;
      
      case eType_array:
      {
        are_equal = types_are_equal(type_a->array.elem, type_b->array.elem);
      }
      break;
      
      case eType_var:
      {
        are_equal = types_are_equal(type_a->var.type, type_b->var.type);
      }
      break;
      
      default: assert(0);
    }
  }
  return are_equal;
}

int compute_type_width(Type* type)
{
  switch(type->kind)
  {
    case eType_array:
    {
      type->width = type->array.size * compute_type_width(type->array.elem);
    }
    break;
    
    case eType_product:
    {
      type->width = compute_type_width(type->product.left) + compute_type_width(type->product.right);
    }
    break;
    
    case eType_proc:
    {
      type->width = compute_type_width(type->proc.ret) + compute_type_width(type->proc.args);
    }
    break;
    
    case eType_basic:
    {
      switch(type->basic.kind)
      {
        case eBasicType_int:
        case eBasicType_float:
        case eBasicType_bool:
        {
          type->width = 4;
        }
        break;
        
        case eBasicType_char:
        {
          type->width = 1;
        }
        break;
        
        case eBasicType_void:
        {
          type->width = 0;
        }
        break;
        
        default: assert(0);
      }
    }
    break;
    
    case eType_pointer:
    {
      type->width = 4;
    }
    break;
    
    case eType_var:
    {
      type->width = compute_type_width(type->var.type);
    }
    break;
    
    default: assert(0);
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
          {
            success = type_unif(repr_type_a->proc.args, repr_type_b->proc.args)
              && type_unif(repr_type_a->proc.ret, repr_type_b->proc.ret);
          }
          break;
          
          case eType_product:
          {
            success = type_unif(repr_type_a->product.left, repr_type_b->product.left)
              && type_unif(repr_type_a->product.right, repr_type_b->product.right);
          }
          break;
          
          case eType_pointer:
          {
            success = type_unif(repr_type_a->pointer.pointee, repr_type_b->pointer.pointee);
          }
          break;
          
          case eType_array:
          {
            success = type_unif(repr_type_a->array.elem, repr_type_b->array.elem);
          }
          break;
          
          case eType_var:
          {
            success = type_unif(repr_type_a->var.type, repr_type_b->var.type);
          }
          break;
          
          default: assert(0);
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
      {
        subst->typevar.id = typevar_id++;
      }
      break;
      
      case eType_proc:
      {
        subst->proc.args = type_subst(subst_list, subst->proc.args);
        subst->proc.ret = type_subst(subst_list, subst->proc.ret);
      }
      break;
      
      case eType_product:
      {
        subst->product.left = type_subst(subst_list, subst->product.left);
        subst->product.right = type_subst(subst_list, subst->product.right);
      }
      break;
      
      case eType_pointer:
      {
        subst->pointer.pointee = type_subst(subst_list, subst->pointer.pointee);
      }
      break;
      
      case eType_array:
      {
        subst->array.elem = type_subst(subst_list, subst->array.elem);
      }
      break;
      
      case eType_var:
      {
        subst->var.type = type_subst(subst_list, subst->var.type);
      }
      break;
      
      default: assert(0);
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
    {
      type = get_type_repr(type);
      if(type->kind == eType_typevar)
      {
        success = false;
      }
      else
      {
        success = resolve_type(type, &type);
      }
    }
    break;
    
    case eType_basic:
    break; // ok
    
    case eType_proc:
    {
      success = resolve_type(type->proc.args, &type->proc.args)
        && resolve_type(type->proc.ret, &type->proc.ret);
    }
    break;
    
    case eType_product:
    {
      success = resolve_type(type->product.left, &type->product.left)
        && resolve_type(type->product.right, &type->product.right);
    }
    break;
    
    case eType_pointer:
    {
      success = resolve_type(type->pointer.pointee, &type->pointer.pointee);
    }
    break;
    
    case eType_array:
    {
      success = resolve_type(type->array.elem, &type->array.elem);
    }
    break;
    
    case eType_var:
    {
      success = resolve_type(type->var.type, &type->var.type);
    }
    break;
    
    default: assert(0);
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
    if(scope->kind == kind)
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

void alloc_data_object(Symbol* sym, Scope* scope, int alignment)
{
  sym->data_loc = scope->data_size;
  sym->data_size = sym->ty->width;
  if((sym->data_size & (alignment-1)) != 0)
  {
    sym->data_size = (sym->data_size + alignment) & ~(alignment-1);
  }
  scope->data_size += sym->data_size;
}

Symbol* new_temp_object(MemoryArena* arena, Scope* scope, Type* ty, SourceLoc* src_loc, int alignment)
{
  Symbol* sym = mem_push_struct(arena, Symbol);

  sym->name = new_tempvar_name("temp_");
  sym->src_loc = src_loc;
  sym->ty = ty;
  sym->scope = scope;
  sym->order_nr = scope->sym_count++;
  sym->storage_space = eStorageSpace_local;
  sym->next_use = NextUse_None;
  sym->is_temp = true;
  sym->is_live = false;

  alloc_data_object(sym, scope, alignment);
  append_list_elem(&scope->decl_syms, sym, eList_symbol);

  return sym;
}

Symbol* new_const_object(MemoryArena* arena, Type* ty, SourceLoc* src_loc)
{
  Symbol* sym = mem_push_struct(arena, Symbol);

  sym->kind = eSymbol_constant;
  sym->name = new_tempvar_name("const_");
  sym->src_loc = src_loc;
  sym->ty = ty;
  sym->scope = 0;
  sym->order_nr = 0;
  sym->storage_space = eStorageSpace_constant;
  sym->next_use = NextUse_None;
  sym->is_temp = false;
  sym->is_live = false;

  return sym;
}

Symbol* add_decl_sym(MemoryArena* arena, char* name, eStorageSpace storage_space,
                     Scope* scope, AstNode* ast_node)
{
  Symbol* sym = mem_push_struct(arena, Symbol);

  sym->name = name;
  sym->src_loc = ast_node->src_loc;
  sym->scope = scope;
  sym->ast_node = ast_node;
  sym->order_nr = scope->sym_count++;
  sym->storage_space = storage_space;
  sym->next_use = NextUse_None;
  sym->is_temp = false;
  sym->is_live = true;

  append_list_elem(&scope->decl_syms, sym, eList_symbol);

  return sym;
}

Scope* begin_scope(SymbolContext* sym_context, eScope kind, AstNode* ast_node)
{
  Scope* scope = mem_push_struct(sym_context->arena, Scope);

  scope->kind = kind;
  scope->nesting_depth = sym_context->nesting_depth;
  scope->sym_count = 0;
  scope->data_size = 0;
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
    arg->var.decl_sym = add_decl_sym(sym_context->arena, arg->var.name,
                                     eStorageSpace_arg, proc_scope, arg);
  }
  return success;
}

bool sym_var(SymbolContext* sym_context, AstNode* block, AstNode* var)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  Symbol* decl_sym = lookup_decl_sym(var->var.name, sym_context->active_scope);
  Scope* arg_scope = find_scope(sym_context->active_scope, eScope_args);
  if(decl_sym && (decl_sym->scope == sym_context->active_scope || decl_sym->scope == arg_scope))
  {
    success = compile_error(var->src_loc, "name `%s` already declared", var->var.name);
    compile_error(decl_sym->src_loc, "see declaration of `%s`", var->var.name);
  }
  else
  {
    var->var.decl_sym = add_decl_sym(sym_context->arena, var->var.name,
                                     eStorageSpace_local, sym_context->active_scope, var);
  }
  return success;
}

bool sym_lit(SymbolContext* sym_context, AstNode* block, AstNode* lit)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(lit, eAstNode_lit));
  bool success = true;

  Symbol* constant = lit->lit.constant = new_const_object(sym_context->arena, lit->eval_ty, lit->src_loc);
  constant->kind = eSymbol_constant;

  switch(lit->lit.kind)
  {
    case eLiteral_int:
      constant->int_val = lit->lit.int_val;
    break;
    
    case eLiteral_float:
      constant->float_val = lit->lit.float_val;
    break;
    
    case eLiteral_bool:
      constant->int_val = (int)lit->lit.bool_val;
    break;
    
    case eLiteral_char:
      constant->char_val = lit->lit.char_val;
    break;
    
    default: assert(0);
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

bool sym_actual_args(SymbolContext* sym_context, AstNode* block, AstNode* args)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = sym_expr(sym_context, block, arg->actual_arg.expr);
  }
  return success;
}

bool sym_call(SymbolContext* sym_context, AstNode* block, AstNode* call)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(call, eAstNode_call));
  bool success = true;
  
  AstNode* call_expr = call->call.expr;
  AstNode* args = call->call.args;

  if(call_expr->kind == eAstNode_id)
  {
    if(success = sym_id(sym_context, block, call_expr) && sym_actual_args(sym_context, block, args))
    {
      call->call.param_scope = begin_scope(sym_context, eScope_params, call);
      call->call.retvar = add_decl_sym(sym_context->arena, new_tempvar_name("ret_"),
                                       eStorageSpace_param, call->call.param_scope, call);

      for(ListItem* li = args->node_list.first;
          li;
          li = li->next)
      {
        AstNode* arg = KIND(li, eList_ast_node)->ast_node;
        arg->actual_arg.param = add_decl_sym(sym_context->arena, new_tempvar_name("param_"),
                                             eStorageSpace_param, call->call.param_scope, arg);
      }

      end_scope(sym_context);
    }
  }
  else
  {
    success = compile_error(call_expr->src_loc, "unsupported call expression");
  }
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
  assert(KIND(block, eAstNode_block));
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
    {
      success = sym_cast(sym_context, block, expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = sym_bin_expr(sym_context, block, expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = sym_unr_expr(sym_context, block, expr);
    }
    break;
    
    case eAstNode_id:
    {
      success = sym_id(sym_context, block, expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = sym_call(sym_context, block, expr);
    }
    break;
    
    case eAstNode_pointer:
    case eAstNode_array:
    case eAstNode_basic_type:
    case eAstNode_str:
    break;

    case eAstNode_lit:
    {
      success = sym_lit(sym_context, block, expr);
    }
    break;
    
    case eAstNode_index:
    {
      success = sym_index(sym_context, block, expr);
    }
    break;

    default: assert(0);
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

bool sym_do_while(SymbolContext* sym_context, AstNode* block, AstNode* do_while)
{
  assert(KIND(block, eAstNode_block));
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  do_while->do_while.scope = begin_nested_scope(sym_context, eScope_while, do_while);
  success = sym_block_stmt(sym_context, block, do_while->do_while.body) &&
    sym_expr(sym_context, block, do_while->do_while.cond_expr);
  end_nested_scope(sym_context);

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
    assert(KIND(proc_scope->ast_node, eAstNode_proc));

    ret->ret.proc = proc_scope->ast_node;
    if(ret->ret.expr)
    {
      success = sym_expr(sym_context, block, ret->ret.expr);
    }
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
    {
      success = sym_var(sym_context, block, stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = sym_if(sym_context, block, stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = sym_do_while(sym_context, block, stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = sym_while(sym_context, block, stmt);
    }
    break;
    
    case eAstNode_block:
    {
      stmt->block.scope = begin_nested_scope(sym_context, eScope_block, stmt);
      success = sym_block(sym_context, stmt);
      end_nested_scope(sym_context);
    }
    break;
    
    case eAstNode_assign:
    {
      success = sym_assign(sym_context, block, stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = sym_cast(sym_context, block, stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    case eAstNode_str:
    {
      success = sym_expr(sym_context, block, stmt);
    }
    break;
    
    case eAstNode_loop_ctrl:
    {
      success = sym_loop_ctrl(sym_context, block, stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = sym_return(sym_context, block, stmt);
    }
    break;
    
    case eAstNode_basic_type:
    case eAstNode_empty:
    break;
    
    case eAstNode_index:
    {
      success = sym_index(sym_context, block, stmt);
    }
    break;
    
    default: assert(0);
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
    {
      success = compile_error(proc->src_loc, "proc `%s` must define a body", proc->proc.name);
    }
    else assert(0);
  }
  return success;
}

bool sym_formal_args(SymbolContext* sym_context, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  Scope* args_scope = find_scope(sym_context->active_scope, eScope_args);
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = sym_formal_arg(sym_context, args_scope, arg);
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
    proc->proc.decl_sym = add_decl_sym(sym_context->arena, proc->proc.name,
                                       eStorageSpace_None, sym_context->active_scope, proc);
    proc->proc.arg_scope = begin_nested_scope(sym_context, eScope_args, proc);
    proc->proc.retvar = add_decl_sym(sym_context->arena, new_tempvar_name("r_"),
                                     eStorageSpace_arg, proc->proc.arg_scope, proc->proc.ret_type);

    proc->proc.scope = begin_scope(sym_context, eScope_proc, proc);

    if(success = sym_formal_args(sym_context, proc->proc.args) && sym_proc_body(sym_context, proc))
    {
      AstNode* body = proc->proc.body;
      proc->proc.body_scope = body->block.scope;
    }

    end_scope(sym_context);
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
    var->var.decl_sym = add_decl_sym(sym_context->arena, var->var.name,
                                     eStorageSpace_static, sym_context->active_scope, var);
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
      {
        success = sym_module_var(sym_context, module, stmt);
      }
      break;
      
      case eAstNode_proc:
      {
        success = sym_module_proc(sym_context, stmt);
      }
      break;
      
      case eAstNode_include:
      break;
      
      default: assert(0);
    }
  }
  end_nested_scope(sym_context);
  assert(sym_context->active_scope == 0);
  assert(sym_context->nesting_depth == -1);
  return success;
}

//     SET TYPES
//-----------------------------------------------------

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
      {
        type->ty = type->eval_ty = basic_type_int;
      }
      break;
      
      case eBasicType_float:
      {
        type->ty = type->eval_ty = basic_type_float;
      }
      break;
      
      case eBasicType_bool:
      {
        type->ty = type->eval_ty = basic_type_bool;
      }
      break;
      
      case eBasicType_char:
      {
        type->ty = type->eval_ty = basic_type_char;
      }
      break;
      
      case eBasicType_void:
      {
        type->ty = type->eval_ty = basic_type_void;
      }
      break;
      
      case eBasicType_auto:
      {
        type->ty = type->eval_ty = new_typevar();
      }
      break;
      
      default: assert(0);
    }
    break;
    
    default:
    {
      success = compile_error(type->src_loc, "invalid type expression");
    }
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

bool set_types_actual_arg(AstNode* actual_arg)
{
  assert(KIND(actual_arg, eAstNode_actual_arg));
  bool success = true;

  AstNode* expr = actual_arg->actual_arg.expr;
  if(success = set_types_expr(expr))
  {
    actual_arg->eval_ty = expr->eval_ty;
    actual_arg->ty = expr->ty;
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

Type* make_type_of_args(AstNode* args);

bool set_types_actual_args(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = set_types_expr(arg);
  }
  if(success)
  {
    args->ty = args->eval_ty = make_type_of_args(args);
  }
  return success;
}

bool set_types_call(AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  bool success = true;
  
  AstNode* call_expr = call->call.expr;
  AstNode* args = call->call.args;
  if(call_expr->kind == eAstNode_id)
  {
    if(success = set_types_id(call_expr) && set_types_actual_args(args))
    {
      call->eval_ty = new_typevar();
      call->ty = new_proc_type(args->ty, call->eval_ty);
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
    {
      ty = basic_type_int;
    }
    break;
    
    case eLiteral_float:
    {
      ty = basic_type_float;
    }
    break;
    
    case eLiteral_char:
    {
      ty = basic_type_char;
    }
    break;
    
    case eLiteral_bool:
    {
      ty = basic_type_bool;
    }
    break;
    
    default: assert(0);
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
    {
      success = set_types_pointer(expr);
    }
    break;
    
    case eAstNode_array:
    {
      success = set_types_array(expr);
    }
    break;
    
    case eAstNode_cast:
    {
      success = set_types_cast(expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = set_types_bin_expr(expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = set_types_unr_expr(expr);
    }
    break;
    
    case eAstNode_id:
    {
      success = set_types_id(expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = set_types_call(expr);
    }
    break;
    
    case eAstNode_lit:
    {
      success = set_types_lit(expr);
    }
    break;
    
    case eAstNode_basic_type:
    {
      success = set_types_type(expr);
    }
    break;
    
    case eAstNode_str:
    {
      success = set_types_str(expr);
    }
    break;
    
    case eAstNode_index:
    {
      success = set_types_index(expr);
    }
    break;

    case eAstNode_actual_arg:
    {
      success = set_types_actual_arg(expr);
    }
    break;
    
    default: assert(0);
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

bool set_types_do_while(AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;
  AstNode* body = do_while->do_while.body;
  if(success = set_types_block_stmt(body) && set_types_expr(do_while->do_while.cond_expr))
  {
    do_while->ty = body->ty;
    do_while->eval_ty = body->eval_ty;
  }
  return success;
}

bool set_types_while(AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  
  AstNode* body = while_->while_.body;
  if(success = set_types_expr(while_->while_.cond_expr) && set_types_block_stmt(body))
  {
    while_->ty = body->ty;
    while_->eval_ty = body->eval_ty;
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
    {
      success = set_types_var(stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = set_types_block(stmt);
    }
    break;
    
    case eAstNode_assign:
    {
      success = set_types_assign(stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = set_types_cast(stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    case eAstNode_str:
    {
      success = set_types_expr(stmt);
    }
    break;
    
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    {
      stmt->ty = stmt->eval_ty = basic_type_void;
    }
    break;
    
    case eAstNode_basic_type:
    {
      success = set_types_type(stmt);
    }
    break;
    case eAstNode_return:
    {
      success = set_types_return(stmt);
    }
    break;

    case eAstNode_if:
    {
      success = set_types_if(stmt);
    }
    break;

    case eAstNode_do_while:
    {
      success = set_types_do_while(stmt);
    }
    break;

    case eAstNode_while:
    {
      success = set_types_while(stmt);
    }
    break;

    case eAstNode_index:
    {
      success = set_types_index(stmt);
    }
    break;

    default: assert(0);
  }
  return success;
}

Type* make_type_of_args(AstNode* args)
{
  Type* result = basic_type_void;

  ListItem* li = args->node_list.first;
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

bool set_types_formal_args(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = set_types_var(arg);
  }
  if(success)
  {
    args->ty = args->eval_ty = make_type_of_args(args);
  }
  return success;
}

bool set_types_proc(AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  
  AstNode* ret_type = proc->proc.ret_type;
  AstNode* args = proc->proc.args;
  if(success = set_types_formal_args(args) && set_types_type(ret_type))
  {
    proc->ty = new_proc_type(args->eval_ty, ret_type->eval_ty);
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
    {
      success = set_types_proc(stmt);
    }
    break;

    case eAstNode_var:
    {
      success = set_types_var(stmt);
    }
    break;

    case eAstNode_include:
    {
      stmt->ty = stmt->eval_ty = basic_type_void;
    }
    break;

    default: assert(0);
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

//       EVAL TYPES
//-----------------------------------------------------

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
    {
      success = eval_types_pointer(type);
    }
    break;

    case eAstNode_array:
    {
      success = eval_types_array(type);
    }
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
      {
        if(type_unif(left_operand->eval_ty, right_operand->eval_ty) && type_unif(left_operand->eval_ty, basic_type_int))
        {
          ;//ok
        }
        else
          success = compile_error(bin_expr->src_loc, "type error (bitwise op)");
      }
      break;
      
      case eOperator_bit_shift_left:
      case eOperator_bit_shift_right:
      {
        if(type_unif(left_operand->eval_ty, basic_type_int) && type_unif(right_operand->eval_ty, basic_type_char))
        {
          ;//ok
        }
        else
          success = compile_error(bin_expr->src_loc, "type error (bitwise op)");
      }
      break;
      
      default:
      {
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
            {
              if(!type_unif(bin_expr->eval_ty, basic_type_bool))
              {
                success = compile_error(bin_expr->src_loc, "type error (bin expr)");
              }
            }
            break;

            default:
            {
              if(!type_unif(bin_expr->eval_ty, left_operand->eval_ty))
              {
                success = compile_error(bin_expr->src_loc, "type error (bin expr)");
              }
            }
            break;
          }
        }
        else
          success = compile_error(bin_expr->src_loc, "type error (bin expr)");
      }
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
          {
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
          }
        break;
        
        case eType_proc:
        {
          if(!type_unif(decl_ast->ty->proc.ret, id->eval_ty))
          {
            success = compile_error(id->src_loc, "type error (proc id)");
          }
        }
        break;

        default: assert(0);
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
      
      default: assert(0);
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

bool eval_types_formal_args(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = eval_types_var(arg);
  }
  return success;
}

bool eval_types_actual_args(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = eval_types_expr(arg->actual_arg.expr);
  }
  return success;
}

bool eval_types_call(AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  bool success = true;
  if(success = eval_types_id(call->call.expr) && eval_types_actual_args(call->call.args))
  {
    AstNode* proc = call->call.proc = KIND(call->call.expr, eAstNode_id)->id.decl_ast;
    if(proc->ty->kind == eType_proc)
    {
      if(!type_unif(proc->ty, call->ty))
      {
        success = compile_error(call->src_loc, "type error (call argument types)");
      }
    }
    else
    {
      success = compile_error(call->src_loc, "type error (call)");
    }
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
    {
      success = eval_types_cast(expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = eval_types_bin_expr(expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = eval_types_unr_expr(expr);
    }
    break;
    
    case eAstNode_id:
    {
      success = eval_types_id(expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = eval_types_call(expr);
    }
    break;
    
    case eAstNode_index:
    {
      success = eval_types_index(expr);
    }
    break;
    
    case eAstNode_lit:
    case eAstNode_str:
    case eAstNode_basic_type:
    break;
    
    default: assert(0);
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

bool eval_types_do_while(AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;
  AstNode* cond_expr = do_while->do_while.cond_expr;
  if(success = eval_types_block_stmt(do_while->do_while.body) && eval_types_expr(cond_expr))
  {
    if(!type_unif(cond_expr->eval_ty, basic_type_bool))
      success = compile_error(cond_expr->src_loc, "bool expression was expected");
  }
  return success;
}

bool eval_types_while(AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  
  AstNode* cond_expr = while_->while_.cond_expr;
  if(success = eval_types_expr(cond_expr) && eval_types_block_stmt(while_->while_.body))
  {
    if(!type_unif(cond_expr->eval_ty, basic_type_bool))
      success = compile_error(cond_expr->src_loc, "bool expression was expected");
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
    {
      success = eval_types_assign(stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = eval_types_cast(stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    case eAstNode_str:
    {
      success = eval_types_expr(stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = eval_types_if(stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = eval_types_do_while(stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = eval_types_while(stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = eval_types_block(stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = eval_types_return(stmt);
    }
    break;
    
    case eAstNode_var:
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    break;
    
    case eAstNode_basic_type:
    {
      success = eval_types_type(stmt);
    }
    break;
    
    case eAstNode_index:
    {
      success = eval_types_index(stmt);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool eval_types_proc(AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  success = eval_types_formal_args(proc->proc.args) && eval_types_block_stmt(proc->proc.body)
    && eval_types_type(proc->proc.ret_type);
  return success;
}

bool eval_types_module_stmt(AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_proc:
    {
      success = eval_types_proc(stmt);
    }
    break;

    case eAstNode_var:
    case eAstNode_include:
    break;

    default: assert(0);
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

//       RESOLVE TYPES
//-----------------------------------------------------

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

bool resolve_types_lit(AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));
  bool success = true;

  if(success = resolve_types_of_node(lit))
  {
    lit->lit.constant->ty = lit->eval_ty;
  }

  return success;
}

bool resolve_types_formal_args(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = resolve_types_var(arg);
  }
  if(success)
  {
    success = resolve_types_of_node(args);
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

bool resolve_types_actual_args(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    if(success = resolve_types_expr(arg->actual_arg.expr))
    {
      arg->eval_ty = arg->actual_arg.expr->eval_ty;
    }
  }
  if(success)
  {
    success = resolve_types_of_node(args);
  }
  return success;
}

bool resolve_types_call(AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  assert(call->call.expr->kind == eAstNode_id);

  bool success = true;

  if(success = resolve_types_id(call->call.expr) && resolve_types_actual_args(call->call.args)
    && resolve_types_of_node(call))
  {
    call->call.retvar->ty = call->eval_ty;

    AstNode* args = call->call.args;
    for(ListItem* li = args->node_list.first;
        li;
        li = li->next)
    {
      AstNode* arg = KIND(li, eList_ast_node)->ast_node;
      arg->actual_arg.param->ty = arg->eval_ty;
    }
  }
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
    {
      success = resolve_types_cast(expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = resolve_types_bin_expr(expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = resolve_types_unr_expr(expr);
    }
    break;
    
    case eAstNode_id:
    {
      success = resolve_types_id(expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = resolve_types_call(expr);
    }
    break;
    
    case eAstNode_lit:
    {
      success = resolve_types_lit(expr);
    }
    break;

    case eAstNode_str:
    case eAstNode_basic_type:
    break;
    
    case eAstNode_index:
    {
      success = resolve_types_index(expr);
    }
    break;
    
    default: assert(0);
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

bool resolve_types_do_while(AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;
  success = resolve_types_block_stmt(do_while->do_while.body) && resolve_types_expr(do_while->do_while.cond_expr) &&
    resolve_types_of_node(do_while);
  return success;
}

bool resolve_types_while(AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  success = resolve_types_expr(while_->while_.cond_expr) && resolve_types_block_stmt(while_->while_.body) &&
    resolve_types_of_node(while_);
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
    {
      success = resolve_types_pointer(type);
    }
    break;

    case eAstNode_array:
    {
      success = resolve_types_array(type);
    }
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
    {
      success = resolve_types_assign(stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = resolve_types_cast(stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    case eAstNode_str:
    {
      success = resolve_types_expr(stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = resolve_types_block(stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = resolve_types_var(stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = resolve_types_return(stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = resolve_types_if(stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = resolve_types_do_while(stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = resolve_types_while(stmt);
    }
    break;
    
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    break;
    
    case eAstNode_basic_type:
    {
      success = resolve_types_type(stmt);
    }
    break;
    
    case eAstNode_index:
    {
      success = resolve_types_index(stmt);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool resolve_types_proc(AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  if(success = resolve_types_formal_args(proc->proc.args) && resolve_types_type(proc->proc.ret_type)
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
    {
      success = resolve_types_proc(stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = resolve_types_var(stmt);
    }
    case eAstNode_include:
    break;
    
    default: assert(0);
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

//          CHECK TYPES
//-----------------------------------------------------

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

bool check_types_formal_args(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
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
      
      default: assert(0);
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

bool check_types_actual_args(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = check_types_expr(arg->actual_arg.expr);
  }
  return success;
}

bool check_types_call(AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  bool success = true;

  success = check_types_actual_args(call->call.args);

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
    {
      success = check_types_assign(expr);
    }
    break;
    
    case eAstNode_cast:
    {
      success = check_types_cast(expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = check_types_bin_expr(expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = check_types_unr_expr(expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = check_types_call(expr);
    }
    break;
    
    case eAstNode_id:
    case eAstNode_lit:
    case eAstNode_str:
    case eAstNode_basic_type:
    break;
    
    case eAstNode_index:
    {
      success = check_types_index(expr);
    }
    break;
    
    default: assert(0);
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

bool check_types_do_while(AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;
  success = check_types_block_stmt(do_while->do_while.body) && check_types_expr(do_while->do_while.cond_expr);
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
    {
      success = check_types_assign(stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = check_types_cast(stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    case eAstNode_str:
    {
      success = check_types_expr(stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = check_types_return(stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = check_types_if(stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = check_types_do_while(stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = check_types_while(stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = check_types_block(stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = check_types_var(stmt);
    }
    break;
    
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    break;
    
    case eAstNode_index:
    {
      success = check_types_index(stmt);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool check_types_proc(AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  success = check_types_formal_args(proc->proc.args) && check_types_block_stmt(proc->proc.body);
  return success;
}

bool check_types_module_stmt(AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_proc:
    {
      success = check_types_proc(stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = check_types_var(stmt);
    }
    break;
    
    default: assert(0);
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

//       IR
//-----------------------------------------------------

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
    
    case eOperator_address_of:
      ir_op = eIrOp_address_of;
    break;

    case eOperator_deref:
      ir_op = eIrOp_deref_source;
    break;

    default: assert(0);
  }
  return ir_op;
}

IrLabel* get_label_at(List* label_list, int stmt_nr)
{
  IrLabel* label = 0;
  for(ListItem* li = label_list->first;
      li;
      li = li->next)
  {
    label = KIND(li, eList_ir_label)->ir_label;
    if(label->stmt_nr == stmt_nr)
      break;
    label = 0;
  }
  return label;
}

eX86Type conv_object_type_to_x86_type(Type* type)
{
  eX86Type x86_type = eX86Type_None;

  if(type->kind == eType_basic)
  {
    eBasicType basic_type = type->basic.kind;

    switch(basic_type)
    {
      case eBasicType_int:
        x86_type = eX86Type_int32;
        break;

      case eBasicType_char:
        x86_type = eX86Type_int8;
        break;

      case eBasicType_float:
        x86_type = eX86Type_float32;
        break;

      default: assert(0);
    }
  }
  else if(type->kind == eType_pointer)
  {
    Type* pointee = type->pointer.pointee;
    assert(pointee->kind == eType_basic);

    x86_type = eX86Type_int32;
  }
  else assert(0);

  return x86_type;
}

eX86Type x86_get_stmt_type(IrStmt* ir_stmt)
{
  eX86Type x86_type = eX86Type_None;

  IrArg* result = ir_stmt->assign.result;
  IrArg* arg1 = ir_stmt->assign.arg1;
  IrArg* arg2 = ir_stmt->assign.arg2;

  switch(ir_stmt->kind)
  {
    case eIrStmt_assign:
    {
      if(ir_stmt->assign.op)
      {
        switch(ir_stmt->assign.op)
        {
          case eIrOp_index_source:
            // result = arg1[arg2]
            x86_type = conv_object_type_to_x86_type(result->object->ty);
            break;

          case eIrOp_index_dest:
            // result[arg2] = arg1
            x86_type = conv_object_type_to_x86_type(arg1->object->ty);
            break;

          case eIrOp_deref_source:
            // result = ^arg1
            x86_type = conv_object_type_to_x86_type(result->object->ty);
            break;

          case eIrOp_deref_dest:
            // ^result = arg1
            x86_type = conv_object_type_to_x86_type(arg1->object->ty);
            break;

          case eIrOp_address_of:
            // result = &arg1
            x86_type = conv_object_type_to_x86_type(result->object->ty);
            break;

          default:
            {
              if(arg2)
              {
                // result = arg1 op arg2
                x86_type = conv_object_type_to_x86_type(result->object->ty);
              }
              else
              {
                // result = op arg1
                fail("TODO");
              }
            }
        }
      }
      else
      {
        // result = arg1
        x86_type = conv_object_type_to_x86_type(result->object->ty);
      }
    }
    break;

    case eIrStmt_cond_goto:
    {
      x86_type = conv_object_type_to_x86_type(arg1->object->ty);
    }
    break;

    case eIrStmt_goto:
    case eIrStmt_call:
    case eIrStmt_return:
    break;

    default: assert(0);
  }

  return x86_type;
}

void ir_emit_assign(IrContext* ir_context, eIrOp op, IrArg* arg1, IrArg* arg2, IrArg* result)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_assign;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);

  stmt->assign.op = op;
  stmt->assign.arg1 = mem_push_struct(arena, IrArg);
  *stmt->assign.arg1 = *arg1;
  if(arg2)
  {
    stmt->assign.arg2 = mem_push_struct(arena, IrArg);
    *stmt->assign.arg2 = *arg2;
  }
  stmt->assign.result = mem_push_struct(arena, IrArg);
  *stmt->assign.result = *result;

  stmt->type = x86_get_stmt_type(stmt);

  ir_context->stmt_count++;
}

void ir_emit_label(IrContext* ir_context, IrLabel* label)
{
  label->kind = eIrLabelTarget_stmt_nr;
  label->stmt_nr = ir_context->stmt_count;
  
  IrLabel* prim_label = 0;
  for(ListItem* li = ir_context->label_list->last;
      li;
      li = li->prev)
  {
    prim_label = KIND(li, eList_ir_label)->ir_label;
    if(prim_label->stmt_nr == label->stmt_nr)
      break;
    prim_label = 0;
  }

  if(prim_label)
  {
    label->primary = prim_label;
  }
  else
  {
    append_list_elem(ir_context->label_list, label, eList_ir_label);
  }
}

void ir_emit_nop(IrContext* ir_context)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);

  *stmt = (IrStmt){0};
  stmt->kind = eIrStmt_nop;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);

  ir_context->stmt_count++;
}

void ir_emit_cond_goto(IrContext* ir_context, eIrOp relop, IrArg* arg1, IrArg* arg2, IrLabel* label)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_cond_goto;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);

  stmt->cond_goto.relop = relop;
  stmt->cond_goto.arg1 = arg1;
  stmt->cond_goto.arg2 = arg2;
  stmt->cond_goto.label = label;

  ir_context->stmt_count++;
}

void ir_emit_goto(IrContext* ir_context, IrLabel* goto_label)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_goto;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);
  stmt->goto_label = goto_label;

  ir_context->stmt_count++;
}

void ir_emit_call(IrContext* ir_context, AstNode* proc)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_call;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);
  stmt->call.proc = proc;

  ir_context->stmt_count++;
}

void ir_emit_return(IrContext* ir_context)
{
  IrStmt* stmt = mem_push_struct(ir_context->stmt_arena, IrStmt);
  stmt->kind = eIrStmt_return;
  stmt->label = get_label_at(ir_context->label_list, ir_context->stmt_count);

  ir_context->stmt_count++;
}

void reset_ir_context(IrContext* ir_context)
{
  ir_context->stmt_array = &ir_context->stmt_array[ir_context->stmt_count];
  ir_context->total_stmt_count += ir_context->stmt_count;
  ir_context->stmt_count = 0;

  /*XXX: 'label_list' storage is a good candidate for begin_temp_memory()/end_temp_memory() pattern of allocation. */
  clear_list(ir_context->label_list);
}

IrArg* ir_new_arg_temp_object(IrContext* context, Scope* scope, Type* ty, SourceLoc* src_loc)
{
  IrArg* arg = mem_push_struct(arena, IrArg);
  arg->object = new_temp_object(context->sym_arena, scope, ty, src_loc, context->data_alignment);

  return arg;
}

IrArg* ir_new_arg_existing_object(IrContext* context, Symbol* object)
{
  IrArg* arg = mem_push_struct(arena, IrArg);
  arg->object = object;

  return arg;
}

bool ir_gen_expr(IrContext* ir_context, Scope* scope, AstNode* expr);
bool ir_gen_bool_expr(IrContext* ir_context, Scope* scope, AstNode* expr);
bool ir_gen_block_stmt(IrContext* ir_context, Scope* scope, AstNode* stmt);

bool ir_gen_bin_expr(IrContext* ir_context, Scope* scope, AstNode* bin_expr)
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
    {
      if(success = ir_gen_expr(ir_context, scope, left_operand) && ir_gen_expr(ir_context, scope, right_operand))
      {
        bin_expr->place = ir_new_arg_temp_object(ir_context, scope, bin_expr->eval_ty, bin_expr->src_loc);

        assert(bin_expr->eval_ty->kind == eType_basic);
        ir_emit_assign(ir_context, conv_operator_to_ir_op(op), left_operand->place, right_operand->place, bin_expr->place);
      }
    }
    break;
    
    default: assert(0);
  }
  return success;
}

void ir_gen_id(IrContext* ir_context, AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  id->place = ir_new_arg_existing_object(ir_context, id->id.decl_sym);
}

bool ir_gen_unr_expr(IrContext* ir_context, Scope* scope, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;
  
  switch(op)
  {
    case eOperator_neg:
    {
      if(success = ir_gen_expr(ir_context, scope, operand))
      {
        unr_expr->place = ir_new_arg_temp_object(ir_context, scope, unr_expr->eval_ty, unr_expr->src_loc);
        ir_emit_assign(ir_context, conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
      }
    }
    break;
    
    case eOperator_logic_not:
    {
      fail("todo");
    }
    break;

    case eOperator_address_of:
    {
      if(success = ir_gen_expr(ir_context, scope, operand))
      {
        unr_expr->place = ir_new_arg_temp_object(ir_context, scope, unr_expr->eval_ty, unr_expr->src_loc);
        ir_emit_assign(ir_context, conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
      }
    }
    break;

    case eOperator_deref:
    {
      if(success = ir_gen_expr(ir_context, scope, operand))
      {
        unr_expr->place = ir_new_arg_temp_object(ir_context, scope, unr_expr->eval_ty, unr_expr->src_loc);
        ir_emit_assign(ir_context, conv_operator_to_ir_op(op), operand->place, 0, unr_expr->place);
      }
    }
    break;
    
    default: assert(0);
  }
  return success;
}

void ir_gen_lit(IrContext* ir_context, Scope* scope, AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));
  
  lit->place = ir_new_arg_existing_object(ir_context, lit->lit.constant);
}

bool ir_gen_bool_unr_expr(IrContext* ir_context, Scope* scope, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;
  
  switch(op)
  {
    case eOperator_logic_not:
    {
      operand->label_true = unr_expr->label_false;
      operand->label_false = unr_expr->label_true;
      success = ir_gen_bool_expr(ir_context, scope, operand);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool ir_gen_actual_args(IrContext* ir_context, Scope* scope, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;

  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    AstNode* expr = arg->actual_arg.expr;
    if(success = ir_gen_expr(ir_context, scope, expr))
    {
      IrArg* place = ir_new_arg_existing_object(ir_context, arg->actual_arg.param);

      ir_emit_assign(ir_context, eIrOp_None, expr->place, 0, place);
    }
  }
  return success;
}

void ir_gen_call(IrContext* ir_context, Scope* scope, AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  
  call->place = ir_new_arg_existing_object(ir_context, call->call.retvar);

  AstNode* args = call->call.args;
  ir_gen_actual_args(ir_context, scope, args);

  for(ListItem* li = args->node_list.first;
      li;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    alloc_data_object(arg->actual_arg.param, call->call.param_scope, ir_context->data_alignment);
  }
  alloc_data_object(call->call.retvar, call->call.param_scope, ir_context->data_alignment);

  ir_emit_call(ir_context, call->call.proc);
}

bool ir_gen_index(IrContext* ir_context, Scope* scope, AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;
  AstNode* array_expr = index->index.array_expr;
  AstNode* i_expr = index->index.i_expr;
  
  if(array_expr->kind == eAstNode_id)
  {
    ir_gen_id(ir_context, array_expr);
    index->index.place = array_expr->place;
    index->index.array_ty = array_expr->eval_ty;
    if(success = ir_gen_expr(ir_context, scope, i_expr))
    {
      index->index.i_place = i_expr->place;
    }
  }
  else if(array_expr->kind == eAstNode_index)
  {
    if(success = ir_gen_index(ir_context, scope, array_expr) && ir_gen_expr(ir_context, scope, i_expr))
    {
      index->index.place = array_expr->index.place;
      index->index.array_ty = array_expr->index.array_ty;
      
      IrArg* offset = index->index.i_place = ir_new_arg_temp_object(ir_context, scope, basic_type_int, index->src_loc);

      Symbol* size_constant = new_const_object(ir_context->sym_arena, basic_type_int, index->src_loc);
      int size_val = size_constant->int_val = size_of_array_dim(index->index.array_ty, index->index.ndim);
      IrArg* dim_size = ir_new_arg_existing_object(ir_context, size_constant);

      if(size_val > 0)
      {
        ir_emit_assign(ir_context, eIrOp_mul, array_expr->index.i_place, dim_size, offset);
        ir_emit_assign(ir_context, eIrOp_add, offset, i_expr->place, offset);
      }
      else
        success = compile_error(i_expr->src_loc, "array dim size = 0");
    }
  }
  else assert(0);

  return success;
}

bool ir_gen_index_with_offset(IrContext* ir_context, Scope* scope, AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;
  if(success = ir_gen_index(ir_context, scope, index))
  {
    IrArg* offset = index->index.offset = ir_new_arg_temp_object(ir_context, scope, basic_type_int, index->src_loc);

    Symbol* width_constant = new_const_object(ir_context->sym_arena, basic_type_int, index->src_loc);
    width_constant->int_val = array_elem_width(index->index.array_ty);
    IrArg* width = ir_new_arg_existing_object(ir_context, width_constant);

    ir_emit_assign(ir_context, eIrOp_mul, index->index.i_place, width, offset);
  }
  return success;
}

bool ir_gen_assign(IrContext* ir_context, Scope* scope, AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;
  
  AstNode* dest_expr = assign->assign.dest_expr;
  AstNode* source_expr = assign->assign.source_expr;
  
  if(dest_expr->kind == eAstNode_id)
  {
    if(success = ir_gen_expr(ir_context, scope, dest_expr) && ir_gen_expr(ir_context, scope, source_expr))
    {
      ir_emit_assign(ir_context, eIrOp_None, source_expr->place, 0, dest_expr->place);
    }
  }
  else if(dest_expr->kind == eAstNode_index)
  {
    if(success = ir_gen_index_with_offset(ir_context, scope, dest_expr) && ir_gen_expr(ir_context, scope, source_expr))
    {
      dest_expr->place = dest_expr->index.place;
      ir_emit_assign(ir_context, eIrOp_index_dest, source_expr->place, dest_expr->index.offset, dest_expr->index.place);
    }
  }
  else if(dest_expr->kind == eAstNode_unr_expr && dest_expr->unr_expr.op == eOperator_deref)
  {
    AstNode* operand = dest_expr->unr_expr.operand;
    if(success = ir_gen_expr(ir_context, scope, operand) && ir_gen_expr(ir_context, scope, source_expr))
    {
      dest_expr->place = operand->place;
      ir_emit_assign(ir_context, eIrOp_deref_dest, source_expr->place, 0, dest_expr->place);
    }
  }
  else
    success = compile_error(dest_expr->src_loc, "unsupported expression on the left-side of assignment");

  return success;
}

bool ir_gen_cast(IrContext* ir_context, Scope* scope, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;
  AstNode* to_type = cast->cast.to_type;
  AstNode* from_expr = cast->cast.from_expr;
  
  if(success = ir_gen_expr(ir_context, scope, from_expr))
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
      cast->place = ir_new_arg_temp_object(ir_context, scope, cast->eval_ty, cast->src_loc);

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

bool ir_gen_expr(IrContext* ir_context, Scope* scope, AstNode* expr)
{
  bool success = true;

  switch(expr->kind)
  {
    case eAstNode_bin_expr:
    {
      eOperator op = expr->bin_expr.op;
      if(op == eOperator_logic_and || op == eOperator_logic_or)
      {
        expr->label_true = new_gen_label(arena);
        expr->label_false = new_gen_label(arena);
        expr->label_next = new_gen_label(arena);

        expr->place = ir_new_arg_temp_object(ir_context, scope, expr->eval_ty, expr->src_loc);

        ir_gen_bool_expr(ir_context, scope, expr);
        
        ir_emit_label(ir_context, expr->label_true);
        ir_emit_assign(ir_context, eIrOp_None, ir_new_arg_existing_object(ir_context, bool_true), 0, expr->place);
        ir_emit_goto(ir_context, expr->label_next);
        ir_emit_label(ir_context, expr->label_false);
        ir_emit_assign(ir_context, eIrOp_None, ir_new_arg_existing_object(ir_context, bool_false), 0, expr->place);
        ir_emit_label(ir_context, expr->label_next);
      }
      else
        ir_gen_bin_expr(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      eOperator op = expr->unr_expr.op;
      if(op == eOperator_logic_not)
      {
        expr->label_true = new_gen_label(arena);
        expr->label_false = new_gen_label(arena);
        expr->label_next = new_gen_label(arena);

        expr->place = ir_new_arg_temp_object(ir_context, scope, expr->eval_ty, expr->src_loc);

        ir_gen_bool_unr_expr(ir_context, scope, expr);

        ir_emit_label(ir_context, expr->label_true);
        ir_emit_assign(ir_context, eIrOp_None, ir_new_arg_existing_object(ir_context, bool_true), 0, expr->place);
        ir_emit_goto(ir_context, expr->label_next);
        ir_emit_label(ir_context, expr->label_false);
        ir_emit_assign(ir_context, eIrOp_None, ir_new_arg_existing_object(ir_context, bool_false), 0, expr->place);
        ir_emit_label(ir_context, expr->label_next);
      }
      else
        ir_gen_unr_expr(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_id:
    {
      ir_gen_id(ir_context, expr);
    }
    break;
    
    case eAstNode_lit:
    {
      ir_gen_lit(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_call:
    {
      ir_gen_call(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_index:
    {
      if(success = ir_gen_index_with_offset(ir_context, scope, expr))
      {
        expr->place = ir_new_arg_temp_object(ir_context, scope, expr->eval_ty, expr->src_loc);

        ir_emit_assign(ir_context, eIrOp_index_source, expr->index.place, expr->index.offset, expr->place);
      }
    }
    break;
    
    case eAstNode_cast:
    {
      ir_gen_cast(ir_context, scope, expr);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool ir_gen_block(IrContext* ir_context, Scope* scope, AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    ir_gen_block_stmt(ir_context, scope, stmt);
  }
  return success;
}

eOperator negate_relop(eOperator op)
{
  eOperator result = eOperator_None;
  switch(op)
  {
    case eOperator_eq:
    {
      result = eOperator_not_eq;
    }
    break;
    
    case eOperator_not_eq:
    {
      result = eOperator_eq;
    }
    break;
    
    case eOperator_less:
    {
      result = eOperator_greater_eq;
    }
    break;
    
    case eOperator_less_eq:
    {
      result = eOperator_greater;
    }
    break;
    
    case eOperator_greater:
    {
      result = eOperator_less_eq;
    }
    break;
    
    case eOperator_greater_eq:
    {
      result = eOperator_less;
    }
    break;
    
    default: assert(0);
  }
  return result;
}

bool ir_gen_bool_bin_expr(IrContext* ir_context, Scope* scope, AstNode* bin_expr)
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
      if(success = ir_gen_expr(ir_context, scope, left_operand) && ir_gen_expr(ir_context, scope, right_operand))
      {
        ir_emit_cond_goto(ir_context, conv_operator_to_ir_op(op), left_operand->place, right_operand->place, bin_expr->label_true);
        ir_emit_goto(ir_context, bin_expr->label_false);
      }
    }
    break;
    
    case eOperator_logic_or:
    {
      assert(bin_expr->label_true);
      assert(bin_expr->label_false);

      left_operand->label_true = bin_expr->label_true;
      left_operand->label_false = new_gen_label(arena);
      right_operand->label_true = bin_expr->label_true;
      right_operand->label_false = bin_expr->label_false;
      if(success = ir_gen_bool_expr(ir_context, scope, left_operand))
      {
        ir_emit_label(ir_context, left_operand->label_false);
        success = ir_gen_bool_expr(ir_context, scope, right_operand);
      }
    }
    break;
    
    case eOperator_logic_and:
    {
      assert(bin_expr->label_true);
      assert(bin_expr->label_false);

      left_operand->label_true = new_gen_label(arena);
      left_operand->label_false = bin_expr->label_false;
      right_operand->label_true = bin_expr->label_true;
      right_operand->label_false = bin_expr->label_false;
      if(success = ir_gen_bool_expr(ir_context, scope, left_operand))
      {
        ir_emit_label(ir_context, left_operand->label_true);
        success = ir_gen_bool_expr(ir_context, scope, right_operand);
      }
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool ir_gen_bool_id(IrContext* ir_context, Scope* scope, AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;
  if(success = ir_gen_expr(ir_context, scope, id))
  {
    ir_emit_cond_goto(ir_context, eIrOp_eq, id->place, ir_new_arg_existing_object(ir_context, bool_true), id->label_true);
    ir_emit_goto(ir_context, id->label_false);
  }
  return success;
}

bool ir_gen_bool_cast(IrContext* ir_context, Scope* scope, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;
  if(success = ir_gen_cast(ir_context, scope, cast))
  {
    ir_emit_cond_goto(ir_context, eIrOp_eq, cast->place, ir_new_arg_existing_object(ir_context, bool_true), cast->label_true);
    ir_emit_goto(ir_context, cast->label_false);
  }
  return success;
}

void ir_gen_bool_lit(IrContext* ir_context, Scope* scope, AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));

  if(lit->lit.bool_val != bool_false->int_val)
  {
    ir_emit_goto(ir_context, lit->label_true);
  }
  else
  {
    ir_emit_goto(ir_context, lit->label_false);
  }
}

bool ir_gen_bool_expr(IrContext* ir_context, Scope* scope, AstNode* expr)
{
  bool success = true;
  switch(expr->kind)
  {
    case eAstNode_id:
    {
      success = ir_gen_bool_id(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_lit:
    {
      ir_gen_bool_lit(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = ir_gen_bool_bin_expr(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = ir_gen_bool_unr_expr(ir_context, scope, expr);
    }
    break;
    
    case eAstNode_cast:
    {
      success = ir_gen_bool_cast(ir_context, scope, expr);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool ir_gen_do_while(IrContext* ir_context, Scope* scope, AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;
  AstNode* cond_expr = do_while->do_while.cond_expr;
  AstNode* body = do_while->do_while.body;
  
  do_while->label_begin = new_gen_label(arena);
  do_while->label_next = new_gen_label(arena);
  do_while->label_true = cond_expr->label_true = do_while->label_begin;
  do_while->label_false = cond_expr->label_false = new_gen_label(arena);
  body->label_next = do_while->label_next;
  
  ir_emit_label(ir_context, do_while->label_begin);
  ir_gen_block_stmt(ir_context, scope, body);
  ir_emit_label(ir_context, do_while->label_next);
  if(success = ir_gen_bool_expr(ir_context, scope, cond_expr))
  {
    ir_emit_label(ir_context, cond_expr->label_false);
  }
  return success;
}

bool ir_gen_while(IrContext* ir_context, Scope* scope, AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  
  AstNode* cond_expr = while_->while_.cond_expr;
  AstNode* body = while_->while_.body;
  
  while_->label_begin = new_gen_label(arena);
  while_->label_next = new_gen_label(arena);
  cond_expr->label_true = new_gen_label(arena);
  cond_expr->label_false = while_->label_next;
  body->label_next = while_->label_begin;
  
  ir_emit_label(ir_context, while_->label_begin);
  if(success = ir_gen_bool_expr(ir_context, scope, cond_expr))
  {
    ir_emit_label(ir_context, cond_expr->label_true);
    ir_gen_block_stmt(ir_context, scope, body);
    ir_emit_goto(ir_context, while_->label_begin);
    ir_emit_label(ir_context, while_->label_next);
  }
  return success;
}

bool ir_gen_if(IrContext* ir_context, Scope* scope, AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  
  AstNode* cond_expr = if_->if_.cond_expr;
  AstNode* body = if_->if_.body;
  AstNode* else_body = if_->if_.else_body;
  
  if_->label_next = new_gen_label(arena);
  if(else_body)
  {
    cond_expr->label_true = new_gen_label(arena);
    cond_expr->label_false = new_gen_label(arena);
    body->label_next = if_->label_next;
    else_body->label_next = if_->label_next;
    
    if(success = ir_gen_bool_expr(ir_context, scope, cond_expr))
    {
      ir_emit_label(ir_context, cond_expr->label_true);
      ir_gen_block_stmt(ir_context, scope, body);
      ir_emit_goto(ir_context, if_->label_next);
      ir_emit_label(ir_context, cond_expr->label_false);
      ir_gen_block_stmt(ir_context, scope, else_body);
    }
  }
  else
  {
    cond_expr->label_true = new_gen_label(arena);
    cond_expr->label_false = if_->label_next;
    body->label_next = if_->label_next;
    
    if(success = ir_gen_bool_expr(ir_context, scope, cond_expr))
    {
      ir_emit_label(ir_context, cond_expr->label_true);
      ir_gen_block_stmt(ir_context, scope, body);
    }
  }
  if(success)
  {
    ir_emit_label(ir_context, if_->label_next);
  }

  return success;
}

bool ir_gen_return(IrContext* ir_context, Scope* scope, AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));

  bool success = true;

  AstNode* ret_expr = ret->ret.expr;
  AstNode* proc = ret->ret.proc;

  if(ret_expr)
  {
    if(success = ir_gen_expr(ir_context, scope, ret_expr))
    {
      IrArg* retvar = ir_new_arg_existing_object(ir_context, proc->proc.retvar);

      ir_emit_assign(ir_context, eIrOp_None, ret_expr->place, 0, retvar);
    }
  }

  ir_emit_goto(ir_context, proc->label_next);

  return success;
}

bool ir_gen_loop_ctrl(IrContext* ir_context, Scope* scope, AstNode* loop_ctrl)
{
  assert(KIND(loop_ctrl, eAstNode_loop_ctrl));
  bool success = true;

  AstNode* loop = loop_ctrl->loop_ctrl.loop;
  if(loop_ctrl->loop_ctrl.kind == eLoopCtrl_break)
  {
    ir_emit_goto(ir_context, loop->label_false);
  }
  else if(loop_ctrl->loop_ctrl.kind == eLoopCtrl_continue)
  {
    ir_emit_goto(ir_context, loop->label_next);
  }
  else assert(0);

  return success;
}

void ir_gen_var(IrContext* ir_context, Scope* scope, AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  alloc_data_object(var->var.decl_sym, scope, ir_context->data_alignment);
}

bool ir_gen_block_stmt(IrContext* ir_context, Scope* scope, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_assign:
    {
      success = ir_gen_assign(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = ir_gen_cast(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_index:
    case eAstNode_lit:
    {
      success = ir_gen_expr(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = ir_gen_block(ir_context, stmt->block.scope, stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = ir_gen_if(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = ir_gen_do_while(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = ir_gen_while(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_var:
    {
      ir_gen_var(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_str:
    {
      fail("TODO");
    }
    break;
    
    case eAstNode_return:
    {
      success = ir_gen_return(ir_context, scope, stmt);
    }
    break;
    
    case eAstNode_empty:
    break;

    case eAstNode_loop_ctrl:
    {
      success = ir_gen_loop_ctrl(ir_context, scope, stmt);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

void ir_gen_formal_args(IrContext* ir_context, Scope* scope, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  for(ListItem* li = args->node_list.first;
      li;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    ir_gen_var(ir_context, scope, arg);
  }
}

bool ir_gen_proc(IrContext* ir_context, Scope* scope, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  
  proc->place = ir_new_arg_existing_object(ir_context, proc->proc.retvar);
  
  if((proc->modifier & eModifier_extern) != 0)
  {
    fail("TODO");
  }
  else
  {
    AstNode* body = proc->proc.body;
    assert(KIND(body, eAstNode_block));

    ir_gen_formal_args(ir_context, proc->proc.arg_scope, proc->proc.args);
    alloc_data_object(proc->proc.retvar, proc->proc.arg_scope, ir_context->data_alignment);

    IrLabel* label_start = &proc->proc.label_start;
    label_start->name = proc->proc.name;
    proc->label_begin = label_start;

    IrLabel* label_return = &proc->proc.label_return;
    gen_label_name(arena, label_return);
    proc->label_next = label_return;

    ir_emit_label(ir_context, label_start);

    if(success = ir_gen_block_stmt(ir_context, body->block.scope, body))
    {
      ir_emit_label(ir_context, label_return);
      ir_emit_return(ir_context);
    }
  }

  return success;
}

bool ir_gen_module_stmt(IrContext* ir_context, Scope* scope, AstNode* stmt)
{
  bool success = true;
  switch(stmt->kind)
  {
    case eAstNode_proc:
    {
      if(success = ir_gen_proc(ir_context, scope, stmt))
      {
        stmt->proc.ir_stmt_array = ir_context->stmt_array;
        stmt->proc.ir_stmt_count = ir_context->stmt_count;

        reset_ir_context(ir_context);
      }
    }
    break;
    
    case eAstNode_var:
    {
      alloc_data_object(stmt->var.decl_sym, scope, ir_context->data_alignment);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool ir_gen_module(IrContext* ir_context, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  for(ListItem* li = module->module.nodes.first;
      li;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = ir_gen_module_stmt(ir_context, module->module.scope, stmt);
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
    
    default: assert(0);
  }
}

void DEBUG_print_ir_arg(String* text, IrArg* arg)
{
  Symbol* object = arg->object;

  switch(object->kind)
  {
    case eSymbol_None:
    {
      str_printf(text, "%s", arg->object->name);
    }
    break;
    
    case eSymbol_constant:
    {
      if(types_are_equal(object->ty, basic_type_int))
      {
        str_printf(text, "%d", object->int_val);
      }
      else if(types_are_equal(object->ty, basic_type_float))
      {
        str_printf(text, "%f", object->float_val);
      }
      else if(types_are_equal(object->ty, basic_type_char))
      {
        char buf[3] = {0};
        print_char(buf, object->char_val);
        str_printf(text, "'%s'", buf);
      }
      else assert(0);
    }
    break;
    
    default: assert(0);
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
        {
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, " = ");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
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
        {
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, " = ");
          DEBUG_print_ir_arg(text, assign->arg1);
          str_printf(text, " ");
          DEBUG_print_ir_op(text, assign->op);
          str_printf(text, " ");
          DEBUG_print_ir_arg(text, assign->arg2);
        }
        break;
        
        /* unr_ops */
        case eIrOp_neg:
        case eIrOp_itof:
        case eIrOp_itoc:
        case eIrOp_itob:
        case eIrOp_ftoi:
        case eIrOp_ctoi:
        case eIrOp_btoi:
        {
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, " = ");
          DEBUG_print_ir_op(text, assign->op);
          str_printf(text, " ");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;
        
        case eIrOp_index_dest:
        {
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, "[");
          DEBUG_print_ir_arg(text, assign->arg2);
          str_printf(text, "] = ");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;
        
        case eIrOp_index_source:
        {
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, " = ");
          DEBUG_print_ir_arg(text, assign->arg1);
          str_printf(text, "[");
          DEBUG_print_ir_arg(text, assign->arg2);
          str_printf(text, "]");
        }
        break;

        case eIrOp_address_of:
        {
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, " = &");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;

        case eIrOp_deref_dest:
        {
          str_printf(text, "^");
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, " = ");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;

        case eIrOp_deref_source:
        {
          DEBUG_print_ir_arg(text, assign->result);
          str_printf(text, " = ^");
          DEBUG_print_ir_arg(text, assign->arg1);
        }
        break;
        
        default: assert(0);
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
      str_printf(text, "goto %s", stmt->goto_label->name);
    }
    break;
    
    case eIrStmt_call:
    {
      struct IrStmt_call* call = &stmt->call;
      AstNode* proc = call->proc;

      str_printf(text, "call %s", proc->label_begin->name);
    }
    break;
    
    case eIrStmt_return:
    {
      str_printf(text, "return");
    }
    break;
    
    case eIrStmt_nop:
    {
      str_printf(text, "nop");
    }
    break;
    
    default:
    {
      str_printf(text, "???");
    }
  }
}

void DEBUG_print_basic_block(MemoryArena* arena, String* text, BasicBlock* bb)
{
  IrStmt** stmt_array = bb->stmt_array;
  for(int i = 0; i < bb->stmt_count; i++)
  {
    IrStmt* stmt = stmt_array[i];
    if(stmt->label)
    {
      str_printfln(text, "%5s:", stmt->label->name);
    }
    str_printf(text, "%5d: ", i);
    DEBUG_print_ir_stmt(text, stmt);
    str_printfln(text, "");
  }
}

void DEBUG_print_ir_code(MemoryArena* arena, List* procs, char* file_path)
{
  begin_temp_memory(&arena);
  String text = {0};
  str_init(&text, arena);

  for(ListItem* li = procs->first;
      li;
      li = li->next)
  {
    AstNode* proc = KIND(li, eList_ast_node)->ast_node;

    List* basic_blocks = proc->proc.basic_blocks;
    for(ListItem* li = basic_blocks->first;
        li;
        li = li->next)
    {
      BasicBlock* bb = KIND(li, eList_basic_block)->basic_block;
      DEBUG_print_basic_block(arena, &text, bb);
    }
  }

  str_dump_to_file(&text, file_path);
  end_temp_memory(&arena);
}

IrLeaderStmt* get_leader_stmt(List* leaders, int stmt_nr)
{
  ListItem* li = leaders->first;
  assert(li);
  IrLeaderStmt* leader = KIND(li, eList_ir_leader_stmt)->ir_leader_stmt;
  assert(leader->stmt_nr == 0);
  for(;
      li && (stmt_nr != leader->stmt_nr);
      li = li->next, leader = KIND(li, eList_ir_leader_stmt)->ir_leader_stmt)
  { }
  return leader->stmt_nr == stmt_nr ? leader : 0;
}

IrLeaderStmt* new_leader_stmt(MemoryArena* arena, int stmt_nr, IrStmt* stmt)
{
  IrLeaderStmt* new_elem = mem_push_struct(arena, IrLeaderStmt);
  new_elem->stmt_nr = stmt_nr;
  new_elem->stmt = stmt;
  IrLabel* label = new_elem->label = stmt->label;
  if(label && label->primary)
  {
    new_elem->label = label->primary;
  }
  return new_elem;
}

void insert_leader_stmt(List* leaders, int stmt_nr, IrStmt* stmt)
{
  assert(stmt);

  ListItem* li = leaders->first;
  assert(li);

  IrLeaderStmt* leader = KIND(li, eList_ir_leader_stmt)->ir_leader_stmt;
  assert(leader->stmt_nr == 0);

  for(;
      li && (leader->stmt_nr < stmt_nr);
      li = li->next, leader = (li ? KIND(li, eList_ir_leader_stmt)->ir_leader_stmt : 0))
  { }

  if(leader)
  {
    if(leader->stmt_nr > stmt_nr)
    {
      insert_elem_before(leaders, li, new_leader_stmt(leaders->arena, stmt_nr, stmt), eList_ir_leader_stmt);
    }
  }
  else
  {
    append_list_elem(leaders, new_leader_stmt(leaders->arena, stmt_nr, stmt), eList_ir_leader_stmt);
  }
}

void start_new_basic_block(List* leaders, int at_stmt_nr, IrStmt* stmt_array, int stmt_count)
{
  if(at_stmt_nr < stmt_count)
  {
    insert_leader_stmt(leaders, at_stmt_nr, &stmt_array[at_stmt_nr]);
  }
}

void x86_print_register(String* text, X86Location* reg)
{
  switch(reg->kind)
  {
    /* 8-bit */
    case eX86Location_al:
    {
      str_printf(text, "al");
    }
    break;

    case eX86Location_ah:
    {
      str_printf(text, "ah");
    }
    break;

    case eX86Location_bl:
    {
      str_printf(text, "bl");
    }
    break;

    case eX86Location_bh:
    {
      str_printf(text, "bh");
    }
    break;

    case eX86Location_cl:
    {
      str_printf(text, "ch");
    }
    break;

    case eX86Location_dl:
    {
      str_printf(text, "dh");
    }
    break;

    /* 32-bit */
    case eX86Location_eax:
    {
      str_printf(text, "eax");
    }
    break;
    
    case eX86Location_ebx:
    {
      str_printf(text, "ebx");
    }
    break;
    
    case eX86Location_ecx:
    {
      str_printf(text, "ecx");
    }
    break;
    
    case eX86Location_edx:
    {
      str_printf(text, "edx");
    }
    break;
    
    case eX86Location_ebp:
    {
      str_printf(text, "ebp");
    }
    break;
    
    case eX86Location_esp:
    {
      str_printf(text, "esp");
    }
    break;
    
    case eX86Location_esi:
    {
      str_printf(text, "esi");
    }
    break;
    
    case eX86Location_edi:
    {
      str_printf(text, "edi");
    }
    break;
    
    default: assert(0);
  }
}

char* x86_get_size_directive(eX86Type x86_type)
{
  char* directv = "";

  switch(x86_type)
  {
    case eX86Type_None:
    break;

    case eX86Type_int8:
      directv = "byte";
    break;

    case eX86Type_int32:
    case eX86Type_float32:
      directv = "dword";
    break;

    default: assert(0);
  }

  return directv;
}

void x86_print_operand(String* text, eX86Type x86_type, X86Operand* operand)
{
  switch(operand->kind)
  {
    case eX86Operand_indexed:
    {
      str_printf(text, "%s ptr [", x86_get_size_directive(x86_type));
      x86_print_operand(text, x86_type, operand->indexed.base);
      if(operand->indexed.offset)
      {
        str_printf(text, "+");
        x86_print_operand(text, x86_type, operand->indexed.offset);
      }
      str_printf(text, "]");
    }
    break;

    case eX86Operand_memory:
    {
      str_printf(text, "%s ptr [", x86_get_size_directive(x86_type));
      x86_print_operand(text, x86_type, operand->memory.base);
      str_printf(text, "%+d]", operand->memory.offset);
    }
    break;

    case eX86Operand_address:
    {
      str_printf(text, "[");
      x86_print_operand(text, x86_type, operand->memory.base);
      str_printf(text, "%+d]", operand->memory.offset);
    }
    break;
    
    case eX86Operand_register:
    {
      x86_print_register(text, operand->reg);
    }
    break;
    
    case eX86Operand_constant:
    {
      str_printf(text, "0%xh", operand->constant);
    }
    break;
    
    case eX86Operand_id:
    {
      str_printf(text, "%s", operand->id);
    }
    break;
    
    default: assert(0);
  }
}

void x86_print_opcode(String* text, eX86StmtOpcode opcode)
{
  switch(opcode)
  {
    case eX86StmtOpcode_call:
    {
      str_printf(text, "call ");
    }
    break;

    case eX86StmtOpcode_pop:
    {
      str_printf(text, "pop ");
    }
    break;

    case eX86StmtOpcode_push:
    {
      str_printf(text, "push ");
    }
    break;

    case eX86StmtOpcode_lea:
    {
      str_printf(text, "lea ");
    }
    break;

    case eX86StmtOpcode_mov:
    {
      str_printf(text, "mov ");
    }
    break;
    
    case eX86StmtOpcode_add:
    {
      str_printf(text, "add ");
    }
    break;
    
    case eX86StmtOpcode_sub:
    {
      str_printf(text, "sub ");
    }
    break;
    
    case eX86StmtOpcode_imul:
    {
      str_printf(text, "imul ");
    }
    break;
    
    case eX86StmtOpcode_idiv:
    {
      str_printf(text, "idiv ");
    }
    break;

    case eX86StmtOpcode_cmp:
    {
      str_printf(text, "cmp ");
    }
    break;

    case eX86StmtOpcode_cmpss:
    {
      str_printf(text, "cmpss ");
    }
    break;
    
    case eX86StmtOpcode_jz:
    {
      str_printf(text, "jz ");
    }
    break;
    
    case eX86StmtOpcode_jnz:
    {
      str_printf(text, "jnz ");
    }
    break;
    
    case eX86StmtOpcode_jl:
    {
      str_printf(text, "jl ");
    }
    break;
    
    case eX86StmtOpcode_jle:
    {
      str_printf(text, "jle ");
    }
    break;
    
    case eX86StmtOpcode_jg:
    {
      str_printf(text, "jg ");
    }
    break;
    
    case eX86StmtOpcode_jge:
    {
      str_printf(text, "jge ");
    }
    break;
    
    case eX86StmtOpcode_jmp:
    {
      str_printf(text, "jmp ");
    }
    break;
    
    case eX86StmtOpcode_label:
    break;
    
    case eX86StmtOpcode_nop:
    {
      //      str_printfln(text, "nop");
    }
    break;
    
    case eX86StmtOpcode_ret:
    {
      str_printf(text, "ret ");
    }
    break;
    
    default: assert(0);
  }
}

void x86_print_stmt(String* text, X86Stmt* stmt)
{
  x86_print_opcode(text, stmt->opcode);
  if(stmt->operand1)
  {
    x86_print_operand(text, stmt->type, stmt->operand1);
    if(stmt->opcode == eX86StmtOpcode_label)
    {
      str_printf(text, ":");
    }
  }
  if(stmt->operand2)
  {
    str_printf(text, ", ");
    x86_print_operand(text, stmt->type, stmt->operand2);
  }
  str_printfln(text, "");
}

X86Stmt* x86_new_stmt(X86Context* context, eX86Type type, eX86StmtOpcode opcode)
{
  X86Stmt* stmt = mem_push_struct(context->stmt_arena, X86Stmt);
  context->stmt_count++;
  stmt->opcode = opcode;
  stmt->type = type;
  stmt->operand1 = stmt->operand2 = 0;

  return stmt;
}

internal inline
bool is_object_in_memory(Symbol* object)
{
  return object->locations._[eX86Location_memory];
}

internal inline
bool is_object_in_location(Symbol* object, X86Location* loc)
{
  return object->locations._[loc->kind];
}

internal inline
void set_object_in_location(Symbol* object, X86Location* loc, bool in_loc)
{
  object->locations._[loc->kind] = in_loc;
}

X86Location* x86_get_location(X86Context* context, eX86Location loc_kind)
{
  X86Location* loc = 0;

  if(loc_kind == eX86Location_memory)
  {
    loc = &context->memory;
  }
  else
  {
    for(int i = 0; i < sizeof_array(context->registers); i++)
    {
      loc = context->registers[i];
      if(loc->kind == loc_kind)
        break;
      loc = 0;
    }
  }

  return loc;
}

void new_object_location_entry(X86Context* context, Symbol* object, X86Location* loc)
{
  set_object_in_location(object, loc, true);

  List* occupants = &loc->occupants;
  for(ListItem* li = occupants->first;
      li;
      li = li->next)
  {
    assert(object != KIND(li, eList_symbol)->symbol);
  }
  append_list_elem(occupants, object, eList_symbol);
}

void delete_object_from_location(X86Context* context, Symbol* object, X86Location* loc)
{
  if(is_object_in_location(object, loc))
  {
    set_object_in_location(object, loc, false);
    
    List* occupants = &loc->occupants;
    ListItem* li = occupants->first;

    for(; li; li = li->next)
    {
      if(object == KIND(li, eList_symbol)->symbol)
        break;
    }
    if(li)
    {
      remove_list_item(occupants, li);
    }
  }
}

void add_object_to_location(X86Context* context, Symbol* object, X86Location* loc)
{
  if(is_object_in_location(object, loc))
  {
    List* occupants = &loc->occupants;
    ListItem* li = occupants->first;
    for(; li; li = li->next)
    {
      if(object == KIND(li, eList_symbol)->symbol)
        break;
    }
    assert(li);
  }
  else
  {
    new_object_location_entry(context, object, loc);
  }
}

void set_exclusive_object_location(X86Context* context, Symbol* object, X86Location* loc)
{
  for(int i = 0; i < sizeof_array(context->registers); i++)
  {
    delete_object_from_location(context, object, context->registers[i]);
  }
  delete_object_from_location(context, object, &context->memory);

  add_object_to_location(context, object, loc);
}

bool register_occupants_all_in_memory(X86Context* context, X86Location* reg)
{
  bool result = true;

  List* occupants = &reg->occupants;

  for(ListItem* li = occupants->first;
      li && result;
      li = li->next)
  {
    Symbol* object = KIND(li, eList_symbol)->symbol;
    result &= is_object_in_memory(object);
  }

  return result;
}

internal inline
bool is_register_location(X86Location* loc)
{
  return loc && ((loc->kind > eX86Location_None && loc->kind < eX86Location_memory)
    || loc->kind == eX86Location_ebp || loc->kind == eX86Location_esp);
}

internal inline
bool is_memory_location(eX86Location loc)
{
  return loc == eX86Location_memory;
}

int x86_get_type_width(eX86Type type)
{
  int width = 0;

  switch(type)
  {
    case eX86Type_int8:
      width = 1;
    break;

    case eX86Type_int32:
    case eX86Type_float32:
      width = 4;
    break;

    default: assert(0);
  }

  return width;
}

int x86_get_register_width(X86Location* reg)
{
  assert(is_register_location(reg));

  int width = x86_get_type_width(reg->type);

  return width;
}

X86Location* find_free_register(X86Context* context, Symbol* object)
{
  X86Location* reg = 0;
  eX86Type x86_type = conv_object_type_to_x86_type(object->ty);

  for(int i = 0; i < sizeof_array(context->registers); i++)
  {
    reg = context->registers[i];
    if(reg->type == x86_type)
    {
      List* occupants = &reg->occupants;

      if(occupants->count == 0)
        break;
    }
    reg = 0;
  }
  
  return reg;
}

X86Location* lookup_object_location(X86Context* context, Symbol* object)
{
  X86Location* loc = 0;

  for(int i = 0; i < sizeof_array(context->registers); i++)
  {
    loc = context->registers[i];
    if(is_object_in_location(object, loc))
      break;
    loc = 0;
  }

  if(!loc)
  {
    if(is_object_in_location(object, &context->memory))
    {
      loc = &context->memory;
    }
  }

  return loc;
}

bool is_single_occupant_register(X86Context* context, X86Location* reg, Symbol* object)
{
  assert(is_register_location(reg));
  
  bool result = false;
  List* occupants = &reg->occupants;
  if(occupants->count == 1)
  {
    Symbol* single_object = KIND(occupants->first, eList_symbol)->symbol;
    result = (single_object == object);
  }

  return result;
}

X86Operand* x86_make_register_operand(MemoryArena* arena, X86Location* reg)
{
  assert(is_register_location(reg));
  
  X86Operand* operand = mem_push_struct(arena, X86Operand);
  operand->kind = eX86Operand_register;
  operand->reg = reg;
  return operand;
}

X86Operand* x86_make_constant_operand(MemoryArena* arena, int constant)
{
  X86Operand* operand = mem_push_struct(arena, X86Operand);
  operand->kind = eX86Operand_constant;
  operand->constant = constant;
  return operand;
}

int x86_conv_constant_to_int(Symbol* constant)
{
  assert(constant->kind == eSymbol_constant);

  int x86_const = 0;

  if(types_are_equal(constant->ty, basic_type_int))
  {
    x86_const = constant->int_val;
  }
  else if(types_are_equal(constant->ty, basic_type_float))
  {
    union BitcastF32ToI32
    {
      float32 float32_val;
      int32 int32_val;
    };

    union BitcastF32ToI32 val = {0};
    val.float32_val = constant->float_val;

    x86_const = val.int32_val;
  }
  else if(types_are_equal(constant->ty, basic_type_char))
  {
    x86_const = constant->char_val;
  }
  else assert(0);

  return x86_const;
}

// X86 stack grows downwards
void x86_set_memory_base_and_offset(X86Context* context, struct X86Operand_memory* memory, Symbol* object)
{
  memory->base = mem_push_struct(arena, X86Operand);

  switch(object->storage_space)
  {
    case eStorageSpace_local:
    {
      memory->base->kind = eX86Operand_register;
      memory->base->reg = &context->ebp;
      memory->offset = -(object->data_loc + object->data_size); 
    }
    break;

    case eStorageSpace_static:
    {
      memory->base->kind = eX86Operand_id;
      memory->base->id = "static_area";
      memory->offset = object->data_loc;
    }
    break;

    case eStorageSpace_param:
    {
      memory->base->kind = eX86Operand_register;
      memory->base->reg = &context->esp;
      memory->offset = -(object->data_loc + object->data_size);
    }
    break;

    case eStorageSpace_arg:
    {
      memory->base->kind = eX86Operand_register;
      memory->base->reg = &context->ebp;

      int scope_data_size = object->scope->data_size;
      int machine_area_size = 2*context->machine_word_size; // instruction_pointer + frame_pointer
      memory->offset = (scope_data_size + machine_area_size) - (object->data_loc + object->data_size);
    }
    break;

    default: assert(0);
  }
}

X86Operand* x86_make_memory_operand(X86Context* context, Symbol* object)
{
  X86Operand* operand = mem_push_struct(arena, X86Operand);
  operand->kind = eX86Operand_memory;

  x86_set_memory_base_and_offset(context, &operand->memory, object);

  return operand;
}

X86Operand* x86_make_address_operand(X86Context* context, Symbol* object)
{
  X86Operand* operand = mem_push_struct(arena, X86Operand);
  operand->kind = eX86Operand_address;

  x86_set_memory_base_and_offset(context, &operand->memory, object);

  return operand;
}

X86Operand* x86_make_indexed_operand(MemoryArena* arena, X86Operand* base, X86Operand* offset)
{
  X86Operand* operand = mem_push_struct(arena, X86Operand);
  operand->kind = eX86Operand_indexed;

  operand->indexed.base = base;
  operand->indexed.offset = offset;

  return operand;
}

X86Operand* x86_make_object_operand(X86Context* context, Symbol* object, X86Location* loc)
{
  if(is_register_location(loc))
  {
    return x86_make_register_operand(arena, loc);
  }
  else
  {
    if(object->kind == eSymbol_constant)
    {
      return x86_make_constant_operand(arena, x86_conv_constant_to_int(object));
    }
    else
    {
      return x86_make_memory_operand(context, object);
    }
  }
}

X86Operand* x86_make_id_operand(MemoryArena* arena, char* id)
{
  X86Operand* operand = mem_push_struct(arena, X86Operand);
  operand->kind = eX86Operand_id;
  operand->id = id;
  return operand;
}

void x86_load_object_into_register(X86Context* context, X86Location* dest_reg, X86Location* source_loc, Symbol* object)
{
  assert(is_register_location(dest_reg));
  
  if(!is_object_in_location(object, dest_reg))
  {
    X86Stmt* stmt = x86_new_stmt(context, conv_object_type_to_x86_type(object->ty), eX86StmtOpcode_mov);
    
    stmt->operand1 = x86_make_register_operand(arena, dest_reg);
    stmt->operand2 = x86_make_object_operand(context, object, source_loc);
  }
}

#if 0
void x86_load_constant_into_register(X86Context* context, eX86Type x86_type, eX86Location dest_reg, Symbol* constant)
{
  assert(is_register_location(dest_reg));
  
  X86Stmt* stmt = x86_new_stmt(context, x86_type, eX86StmtOpcode_mov);
  
  stmt->operand1 = x86_make_register_operand(arena, dest_reg);
  stmt->operand2 = x86_make_constant_operand(arena, x86_conv_constant_to_int(constant));
}

void x86_store_constant_to_memory(X86Context* context, eX86Type x86_type, Symbol* constant, Symbol* object)
{
  X86Stmt* stmt = x86_new_stmt(context, x86_type, eX86StmtOpcode_mov);
  
  stmt->operand1 = x86_make_memory_operand(context, object);
  stmt->operand2 = x86_make_constant_operand(arena, x86_conv_constant_to_int(constant));
}
#endif

void x86_store_object_to_memory(X86Context* context, X86Location* source_reg, Symbol* object)
{
  assert(is_register_location(source_reg));
  
  X86Stmt* stmt = x86_new_stmt(context, conv_object_type_to_x86_type(object->ty), eX86StmtOpcode_mov);
  
  stmt->operand1 = x86_make_memory_operand(context, object);
  stmt->operand2 = x86_make_register_operand(arena, source_reg);
}

void x86_load_memory_address(X86Context* context, X86Location* dest_reg, Symbol* object)
{
  assert(is_register_location(dest_reg));

  X86Stmt* stmt = x86_new_stmt(context, eX86Type_int32, eX86StmtOpcode_lea);

  stmt->operand1 = x86_make_register_operand(arena, dest_reg);
  stmt->operand2 = x86_make_address_operand(context, object);
}

eX86StmtOpcode conv_ir_op_to_x86_opcode(eIrOp ir_op, eX86Type x86_type)
{
  eX86StmtOpcode x86_opcode = eX86StmtOpcode_None;

  switch(ir_op)
  {
    case eIrOp_add:
    {
      if(x86_type == eX86Type_int32)
      {
        x86_opcode = eX86StmtOpcode_add;
      }
      else if(x86_type == eX86Type_float32)
      {
        x86_opcode = eX86StmtOpcode_addss;
      }
      else
        assert(0);
    }
    break;
    
    case eIrOp_sub:
    {
      if(x86_type == eX86Type_int32)
      {
        x86_opcode = eX86StmtOpcode_sub;
      }
      else if(x86_type == eX86Type_float32)
      {
        x86_opcode = eX86StmtOpcode_subss;
      }
      else
        assert(0);
    }
    break;
    
    case eIrOp_mul:
    {
      if(x86_type == eX86Type_int32)
      {
        x86_opcode = eX86StmtOpcode_imul;
      }
      else if(x86_type == eX86Type_float32)
      {
        x86_opcode = eX86StmtOpcode_mulss;
      }
      else
        assert(0);
    }
    break;
    
    case eIrOp_div:
    {
      if(x86_type == eX86Type_int32)
      {
        x86_opcode = eX86StmtOpcode_idiv;
      }
      else if(x86_type == eX86Type_float32)
      {
        x86_opcode = eX86StmtOpcode_divss;
      }
      else
        assert(0);
    }
    break;

    case eIrOp_less:
      {
        if(x86_type == eX86Type_int32)
        {
          x86_opcode = eX86StmtOpcode_jl;
        }
        else
          assert(0);
      }
      break;

    case eIrOp_less_eq:
      {
        if(x86_type == eX86Type_int32)
        {
          x86_opcode = eX86StmtOpcode_jle;
        }
        else
          assert(0);
      }
      break;

    case eIrOp_greater:
      {
        if(x86_type == eX86Type_int32)
        {
          x86_opcode = eX86StmtOpcode_jg;
        }
        else
          assert(0);
      }
      break;

    case eIrOp_greater_eq:
      {
        if(x86_type == eX86Type_int32)
        {
          x86_opcode = eX86StmtOpcode_jge;
        }
        else
          assert(0);
      }
      break;

    case eIrOp_eq:
      {
        if(x86_type == eX86Type_int32)
        {
          x86_opcode = eX86StmtOpcode_jz;
        }
        else
          assert(0);
      }
      break;

    case eIrOp_not_eq:
      {
        if(x86_type == eX86Type_int32)
        {
          x86_opcode = eX86StmtOpcode_jnz;
        }
        else
          assert(0);
      }
      break;
    
    default: assert(0);
  }
  return x86_opcode;
}

X86Location* find_least_used_register(X86Context* context, Symbol* exclude_object)
{
  X86Location* min_reg = 0;

  int min_count = max_int();
  for(int i = 0; i < sizeof_array(context->registers); i++)
  {
    X86Location* reg = context->registers[i];
    List* occupants = &reg->occupants;

    if(occupants->count < min_count)
    {
      if(exclude_object && is_object_in_location(exclude_object, reg))
      {
        ;//continue
      }
      else
      {
        min_reg = reg;
        min_count = occupants->count;
      }
    }
  }

  return min_reg;
}

void save_object_to_memory(X86Context* context, X86Location* reg, Symbol* object)
{
  assert(is_register_location(reg));

  if(!is_object_in_memory(object))
  {
    x86_store_object_to_memory(context, reg, object);
    add_object_to_location(context, object, &context->memory);
  }
}

void save_register_to_memory(X86Context* context, X86Location* reg, bool free_reg)
{
  List* occupants = &reg->occupants;

  for(ListItem* li = occupants->first; li; )
  {
    ListItem* li_next = li->next;

    Symbol* object = KIND(li, eList_symbol)->symbol;
    if(object->is_live)
    {
      save_object_to_memory(context, reg, object);
    }

    if(free_reg)
    {
      delete_object_from_location(context, object, reg);
    }

    li = li_next;
  }
}

X86Location* get_best_available_register(X86Context* context, Symbol* object, IrStmt* stmt)
{
  X86Location* best_reg = 0;

  X86Location* free_reg = find_free_register(context, object);
  if(free_reg)
  {
    best_reg = free_reg;
  }
  else
  {
    for(int i = 0; i < sizeof_array(context->registers); i++)
    {
      best_reg = context->registers[i];
      if(register_occupants_all_in_memory(context, best_reg))
        break;
      best_reg = 0;
    }

    if(best_reg)
    {
      save_register_to_memory(context, best_reg, true);
    }
    else
    {
      if(stmt->kind == eIrStmt_assign)
      {
        struct IrStmt_assign* assign = &stmt->assign;

        X86Location* result_loc = lookup_object_location(context, assign->result->object);
        if(is_register_location(result_loc)
           && is_single_occupant_register(context, result_loc, assign->result->object))
        {
          best_reg = result_loc;
        }
        else
        {
          Symbol* exclude_object = 0;
          if(assign->arg2)
          {
            exclude_object = assign->arg2->object;
          }

          best_reg = find_least_used_register(context, exclude_object);
          save_register_to_memory(context, best_reg, true);
        }
      }
      else if(stmt->kind == eIrStmt_cond_goto)
      {
        best_reg = find_least_used_register(context, 0);
        save_register_to_memory(context, best_reg, true);
      }
      else assert(0);
    }
  }

  return best_reg;
}

void discard_assign_arg(X86Context* context, IrArg* arg, X86Location* arg_loc)
{
  if(is_register_location(arg_loc)
     && (arg->next_use == NextUse_None && !arg->is_live))
  {
    delete_object_from_location(context, arg->object, arg_loc);
  }
}

void discard_all_assign_args(X86Context* context, struct IrStmt_assign* assign, X86Location* store_reg)
{
  X86Location* arg1_loc = lookup_object_location(context, assign->arg1->object);
  discard_assign_arg(context, assign->arg1, arg1_loc);

  if(assign->arg2)
  {
    X86Location* arg2_loc = lookup_object_location(context, assign->arg2->object);
    discard_assign_arg(context, assign->arg2, arg2_loc);
  }
}

void save_all_registers_to_memory(X86Context* context, bool free_reg)
{
  for(int i = 0; i < sizeof_array(context->registers); i++)
  {
    X86Location* reg = context->registers[i];
    save_register_to_memory(context, reg, free_reg);
  }
}

// getreg() pag. 538, Sec. 9.6
X86Location* select_result_register(X86Context* context, IrStmt* ir_stmt)
{
  X86Location* dest_loc = 0;

  if(ir_stmt->kind == eIrStmt_assign)
  {
    struct IrStmt_assign* assign = &ir_stmt->assign;
    IrArg* result = assign->result;
    IrArg* arg1 = assign->arg1;

    if(assign->op)
    {
      X86Location* arg1_loc = lookup_object_location(context, arg1->object);

      switch(assign->op)
      {
        case eIrOp_address_of:
        case eIrOp_deref_source:
        case eIrOp_deref_dest:
        case eIrOp_index_source:
        case eIrOp_index_dest:
          assert(0);
        break;

        // result = arg1 op arg2
        default:
        {
          if(is_register_location(arg1_loc)
             && is_single_occupant_register(context, arg1_loc, arg1->object)
             && (arg1->next_use == NextUse_None && !arg1->is_live))
          {
            delete_object_from_location(context, arg1->object, arg1_loc);
            dest_loc = arg1_loc;
          }
          else
          {
            dest_loc = get_best_available_register(context, arg1->object, ir_stmt);
          }
        }
      }
    }
    else // result = arg1
    {
      X86Location* result_loc = lookup_object_location(context, result->object);

      if(is_register_location(result_loc)
         && is_single_occupant_register(context, result_loc, result->object))
      {
        dest_loc = result_loc;
      }
      else
      {
        dest_loc = get_best_available_register(context, result->object, ir_stmt);
      }
    }
  }
  else if(ir_stmt->kind == eIrStmt_cond_goto)
  {
    struct IrStmt_cond_goto* cond_goto = &ir_stmt->cond_goto;
    IrArg* arg1 = cond_goto->arg1;

    X86Location* arg1_loc = lookup_object_location(context, arg1->object);

    if(is_register_location(arg1_loc))
    {
      dest_loc = arg1_loc;
    }
    else
    {
      dest_loc = get_best_available_register(context, arg1->object, ir_stmt);
      x86_load_object_into_register(context, dest_loc, arg1_loc, arg1->object);
    }
  }
  else assert(0);

  return dest_loc;
}

void update_object_live_info(IrArg* result, IrArg* arg1, IrArg* arg2)
{
  result->object->is_live = result->is_live;
  result->object->next_use = result->next_use;

  arg1->object->is_live = arg1->is_live;
  arg1->object->next_use = arg1->next_use;

  if(arg2)
  {
    arg2->object->is_live = arg2->is_live;
    arg2->object->next_use = arg2->next_use;
  }
}

IrLabel* normalize_jump_target_labels(IrStmt* stmt)
{
  IrLabel* target_label = 0;
  if(stmt->kind == eIrStmt_cond_goto)
  {
    target_label = stmt->cond_goto.label;
    if(target_label->primary)
    {
      stmt->cond_goto.label = target_label->primary;
      target_label = stmt->cond_goto.label;
    }
  }
  else if(stmt->kind == eIrStmt_goto)
  {
    target_label = stmt->goto_label;
    if(target_label->primary)
    {
      stmt->goto_label = target_label->primary;
      target_label = stmt->goto_label;
    }
  }
  return target_label;
}

void partition_to_basic_blocks(MemoryArena* stmt_arena, AstNode* proc)
{
  if(proc->proc.ir_stmt_count > 0)
  {
    List* leaders = new_list(arena, eList_ir_leader_stmt);

    IrStmt* stmt_array = proc->proc.ir_stmt_array;
    int stmt_count = proc->proc.ir_stmt_count;

    IrStmt* stmt = &stmt_array[0];
    normalize_jump_target_labels(stmt);
    append_list_elem(leaders, new_leader_stmt(leaders->arena, 0, stmt), eList_ir_leader_stmt);
    
    for(int i = 1; i < proc->proc.ir_stmt_count; i++)
    {
      stmt = &stmt_array[i];
      if(stmt->kind == eIrStmt_cond_goto || stmt->kind == eIrStmt_goto)
      {
        start_new_basic_block(leaders, i+1, stmt_array, stmt_count);
        IrLabel* target_label = normalize_jump_target_labels(stmt);
        start_new_basic_block(leaders, target_label->stmt_nr, stmt_array, stmt_count);
      }
      else if(stmt->kind == eIrStmt_call || stmt->kind == eIrStmt_return)
      {
        start_new_basic_block(leaders, i+1, stmt_array, stmt_count);
      }
    }

    //------
    
    List* basic_blocks = proc->proc.basic_blocks = new_list(stmt_arena, eList_basic_block);

    for(ListItem* li = leaders->first;
        li;
        li = li->next)
    {
      int next_stmt_nr = proc->proc.ir_stmt_count;
      if(li->next)
      {
        IrLeaderStmt* leader_next = KIND(li->next, eList_ir_leader_stmt)->ir_leader_stmt;
        next_stmt_nr = leader_next->stmt_nr;
      }

      IrLeaderStmt* leader = KIND(li, eList_ir_leader_stmt)->ir_leader_stmt;
      BasicBlock* block = mem_push_struct(stmt_arena, BasicBlock);
      append_list_elem(basic_blocks, block, eList_basic_block);
      init_list(&block->pred_list, stmt_arena, eList_basic_block);
      init_list(&block->succ_list, stmt_arena, eList_basic_block);
      leader->block = block;
      block->stmt_array = mem_push_array(stmt_arena, IrStmt*, next_stmt_nr - leader->stmt_nr);
      block->stmt_count = 0;
      block->label = leader->label;

      for(int i = leader->stmt_nr;
          i < next_stmt_nr;
          i++)
      {
        block->stmt_array[block->stmt_count++] = &proc->proc.ir_stmt_array[i];
      }
      assert(block->stmt_count > 0);
    }

    for(ListItem* li = basic_blocks->first;
        li;
        li = li->next)
    {
      BasicBlock* bb_next = 0;
      if(li->next)
      {
        bb_next = KIND(li->next, eList_basic_block)->basic_block;
      }

      BasicBlock* bb = KIND(li, eList_basic_block)->basic_block;
      IrStmt* last_stmt = bb->stmt_array[bb->stmt_count - 1];

      if(last_stmt->kind == eIrStmt_goto || last_stmt->kind == eIrStmt_cond_goto)
      {
        IrLabel* goto_label = 0;
        if(last_stmt->kind == eIrStmt_cond_goto)
        {
          goto_label = last_stmt->cond_goto.label;
        }
        else if(last_stmt->kind == eIrStmt_goto)
        {
          goto_label = last_stmt->goto_label;
        }
        else assert(0);
        
        int stmt_nr = goto_label->stmt_nr;
        IrLeaderStmt* leader = get_leader_stmt(leaders, stmt_nr);
        append_list_elem(&bb->succ_list, leader->block, eList_basic_block);
        append_list_elem(&leader->block->pred_list, bb, eList_basic_block);
        
        if(last_stmt->kind != eIrStmt_goto)
        {
          append_list_elem(&bb->succ_list, bb_next, eList_basic_block);
          append_list_elem(&bb_next->pred_list, bb, eList_basic_block);
        }
      }
      else if(bb_next)
      {
        append_list_elem(&bb->succ_list, bb_next, eList_basic_block);
        append_list_elem(&bb_next->pred_list, bb, eList_basic_block);
      }
      
      // next-use information
      for(int i = bb->stmt_count - 1; i >=0 ; i--)
      {
        IrStmt* stmt = bb->stmt_array[i];

        if(stmt->kind == eIrStmt_assign)
        {
          IrArg* result = stmt->assign.result;
          IrArg* arg1 = stmt->assign.arg1;
          IrArg* arg2 = stmt->assign.arg2;

          result->is_live = result->object->is_live;
          result->next_use = result->object->next_use;

          arg1->is_live = arg1->object->is_live;
          arg1->next_use = arg1->object->next_use;

          if(arg2)
          {
            arg2->is_live = arg2->object->is_live;
            arg2->next_use = arg2->object->next_use;
          }

          //-----

          result->object->is_live = result->object->is_temp ? false : true;
          result->object->next_use = NextUse_None;

          arg1->object->is_live = true;
          arg1->object->next_use = i;

          if(arg2)
          {
            arg2->object->is_live = true;
            arg2->object->next_use = i;
          }
        }
      }
    }
  }
}

// result = arg1[arg2]
void x86_gen_assign_index_source_stmt(X86Context* context, IrStmt* ir_stmt)
{
  IrArg* result = ir_stmt->assign.result;
  IrArg* arg1 = ir_stmt->assign.arg1;
  IrArg* arg2 = ir_stmt->assign.arg2;

  X86Location* address_reg = get_best_available_register(context, arg1->object, ir_stmt);
  x86_load_memory_address(context, address_reg, arg1->object);

  X86Location* arg2_loc = lookup_object_location(context, arg2->object);

  if(is_register_location(arg2_loc))
  {
    X86Stmt* mov_stmt = x86_new_stmt(context, ir_stmt->type, eX86StmtOpcode_mov);
    mov_stmt->operand1 = x86_make_register_operand(arena, address_reg);
    mov_stmt->operand2 = x86_make_indexed_operand(arena,
                                                  x86_make_register_operand(arena, address_reg),
                                                  x86_make_register_operand(arena, arg2_loc));

    set_exclusive_object_location(context, result->object, address_reg);
  }
  else // !is_register_location(arg2_loc)
  {
    X86Stmt* add_offset_stmt = x86_new_stmt(context, ir_stmt->type, eX86StmtOpcode_add);
    add_offset_stmt->operand1 = x86_make_register_operand(arena, address_reg);
    add_offset_stmt->operand2 = x86_make_object_operand(context, arg2->object, arg2_loc);

    X86Stmt* mov_stmt = x86_new_stmt(context, ir_stmt->type, eX86StmtOpcode_mov);
    mov_stmt->operand1 = x86_make_register_operand(arena, address_reg);
    mov_stmt->operand2 = x86_make_indexed_operand(arena,
                                                  x86_make_register_operand(arena, address_reg),
                                                  x86_make_object_operand(context, arg2->object, &context->memory));

    set_exclusive_object_location(context, result->object, address_reg);
  }
}

// result[arg2] = arg1
void x86_gen_assign_index_dest_stmt(X86Context* context, IrStmt* ir_stmt)
{
  IrArg* result = ir_stmt->assign.result;
  IrArg* arg1 = ir_stmt->assign.arg1;
  IrArg* arg2 = ir_stmt->assign.arg2;

  X86Location* address_reg = get_best_available_register(context, result->object, ir_stmt);
  x86_load_memory_address(context, address_reg, result->object);

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);
  X86Location* arg2_loc = lookup_object_location(context, arg2->object);

  if(is_register_location(arg2_loc) && is_register_location(arg1_loc))
  {
    X86Stmt* mov_stmt = x86_new_stmt(context, ir_stmt->type, eX86StmtOpcode_mov);
    mov_stmt->operand1 = x86_make_indexed_operand(arena,
                                                  x86_make_register_operand(arena, address_reg),
                                                  x86_make_register_operand(arena, arg2_loc));
    mov_stmt->operand2 = x86_make_register_operand(arena, arg1_loc);
  }
  else if(is_register_location(arg2_loc) && !is_register_location(arg1_loc))
  {
    if(arg2->next_use != NextUse_None || arg2->is_live)
    {
      save_object_to_memory(context, arg2_loc, arg2->object);
    }
    delete_object_from_location(context, arg2->object, arg2_loc);

    X86Stmt* add_offset_stmt = x86_new_stmt(context, eX86Type_int32, eX86StmtOpcode_add);
    add_offset_stmt->operand1 = x86_make_register_operand(arena, arg2_loc);
    add_offset_stmt->operand2 = x86_make_register_operand(arena, address_reg);

    x86_load_object_into_register(context, address_reg, arg1_loc, arg1->object);
    add_object_to_location(context, arg1->object, address_reg);

    X86Stmt* mov_stmt = x86_new_stmt(context, ir_stmt->type, eX86StmtOpcode_mov);
    mov_stmt->operand1 = x86_make_indexed_operand(arena,
                                                  x86_make_register_operand(arena, arg2_loc),
                                                  0);
    mov_stmt->operand2 = x86_make_register_operand(arena, address_reg);

    if(arg2->next_use != NextUse_None)
    {
      x86_load_object_into_register(context, arg2_loc, &context->memory, arg2->object);
      add_object_to_location(context, arg2->object, arg2_loc);
    }
  }
  else if(!is_register_location(arg2_loc))
  {
    X86Stmt* add_offset_stmt = x86_new_stmt(context, ir_stmt->type, eX86StmtOpcode_add);
    add_offset_stmt->operand1 = x86_make_register_operand(arena, address_reg);
    add_offset_stmt->operand2 = x86_make_object_operand(context, arg2->object, arg2_loc);

    X86Location* arg1_loc = lookup_object_location(context, arg1->object);

    if(!is_register_location(arg1_loc))
    {
      arg1_loc = get_best_available_register(context, arg1->object, ir_stmt);
      x86_load_object_into_register(context, arg1_loc, &context->memory, arg1->object);
      add_object_to_location(context, arg1->object, arg1_loc);
    }

    X86Stmt* mov_stmt = x86_new_stmt(context, ir_stmt->type, eX86StmtOpcode_mov);
    mov_stmt->operand1 = x86_make_indexed_operand(arena,
                                                  x86_make_register_operand(arena, address_reg),
                                                  0);
    mov_stmt->operand2 = x86_make_register_operand(arena, arg1_loc);
  }
}

// result = ^arg1
void x86_gen_assign_deref_source_stmt(X86Context* context, IrStmt* ir_stmt)
{
  IrArg* result = ir_stmt->assign.result;
  IrArg* arg1 = ir_stmt->assign.arg1;

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);

  if(!is_register_location(arg1_loc))
  {
    arg1_loc = get_best_available_register(context, arg1->object, ir_stmt);
    x86_load_memory_address(context, arg1_loc, arg1->object);
    add_object_to_location(context, arg1->object, arg1_loc);
  }

  X86Location* result_loc = lookup_object_location(context, result->object);
  
  if(!is_register_location(result_loc))
  {
    result_loc = get_best_available_register(context, result->object, ir_stmt);
    add_object_to_location(context, result->object, result_loc);
  }

  X86Stmt* mov_stmt = x86_new_stmt(context, ir_stmt->type, eX86StmtOpcode_mov);
  mov_stmt->operand1 = x86_make_register_operand(arena, result_loc);
  mov_stmt->operand2 = x86_make_indexed_operand(arena,
                                                x86_make_register_operand(arena, arg1_loc),
                                                0);

  set_exclusive_object_location(context, result->object, result_loc);
}

// ^result = arg1
void x86_gen_assign_deref_dest_stmt(X86Context* context, IrStmt* ir_stmt)
{
  IrArg* result = ir_stmt->assign.result;
  IrArg* arg1 = ir_stmt->assign.arg1;

  X86Location* result_loc = lookup_object_location(context, result->object);

  if(!is_register_location(result_loc))
  {
    result_loc = get_best_available_register(context, result->object, ir_stmt);
    x86_load_memory_address(context, result_loc, result->object);
    add_object_to_location(context, result->object, result_loc);
  }

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);

  if(!is_register_location(arg1_loc))
  {
    arg1_loc = get_best_available_register(context, arg1->object, ir_stmt);
    x86_load_object_into_register(context, arg1_loc, &context->memory, arg1->object);
    add_object_to_location(context, arg1->object, arg1_loc);
  }

  X86Stmt* mov_stmt = x86_new_stmt(context, ir_stmt->type, eX86StmtOpcode_mov);
  mov_stmt->operand1 = x86_make_indexed_operand(arena,
                                                x86_make_register_operand(arena, result_loc),
                                                0);
  mov_stmt->operand2 = x86_make_register_operand(arena, arg1_loc);
}

// result = arg1
void x86_gen_assign_simple_stmt(X86Context* context, IrStmt* ir_stmt)
{
  struct IrStmt_assign* assign = &ir_stmt->assign;
  IrArg* result = ir_stmt->assign.result;
  IrArg* arg1 = ir_stmt->assign.arg1;

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);

  if(is_register_location(arg1_loc))
  {
    set_exclusive_object_location(context, result->object, arg1_loc);
    discard_all_assign_args(context, assign, arg1_loc);
  }
  else
  {
    if(result->next_use == NextUse_None)
    {
      X86Location* result_loc = lookup_object_location(context, result->object);
      if(is_register_location(result_loc))
      {
        if(is_single_occupant_register(context, result_loc, result->object))
        {
          x86_load_object_into_register(context, result_loc, &context->memory, arg1->object);
          x86_store_object_to_memory(context, result_loc, result->object);
        }
      }
      else
      {
        X86Location* store_reg = select_result_register(context, ir_stmt);
        x86_load_object_into_register(context, store_reg, &context->memory, arg1->object);
        x86_store_object_to_memory(context, store_reg, result->object);
      }
      set_exclusive_object_location(context, result->object, &context->memory);
    }
    else
    {
      X86Location* store_reg = select_result_register(context, ir_stmt);
      x86_load_object_into_register(context, store_reg, &context->memory, arg1->object);
      set_exclusive_object_location(context, result->object, store_reg);
    }
  }
}

// result = &arg1
void x86_gen_assign_address_of_stmt(X86Context* context, IrStmt* ir_stmt)
{
  IrArg* result = ir_stmt->assign.result;
  IrArg* arg1 = ir_stmt->assign.arg1;

  X86Location* result_loc = get_best_available_register(context, result->object, ir_stmt);
  x86_load_memory_address(context, result_loc, arg1->object);
  set_exclusive_object_location(context, result->object, result_loc);
}

// result = arg1 op arg2
void x86_gen_assign_binexpr_stmt(X86Context* context, IrStmt* ir_stmt)
{
  struct IrStmt_assign* assign = &ir_stmt->assign;
  IrArg* result = ir_stmt->assign.result;
  IrArg* arg1 = ir_stmt->assign.arg1;
  IrArg* arg2 = ir_stmt->assign.arg2;

  X86Location* store_reg = select_result_register(context, ir_stmt);
  X86Location* arg1_loc = lookup_object_location(context, arg1->object);
  x86_load_object_into_register(context, store_reg, arg1_loc, arg1->object);

  X86Stmt* x86_stmt = x86_new_stmt(context, ir_stmt->type, conv_ir_op_to_x86_opcode(assign->op, ir_stmt->type));
  x86_stmt->operand1 = x86_make_object_operand(context, arg1->object, store_reg);
  x86_stmt->operand2 = x86_make_object_operand(context, arg2->object, lookup_object_location(context, arg2->object));

  set_exclusive_object_location(context, result->object, store_reg);
  discard_all_assign_args(context, assign, store_reg);
}

void x86_gen_assign_stmt(X86Context* context, IrStmt* ir_stmt)
{
  struct IrStmt_assign* assign = &ir_stmt->assign;
  IrArg* result = ir_stmt->assign.result;
  IrArg* arg1 = ir_stmt->assign.arg1;
  IrArg* arg2 = ir_stmt->assign.arg2;

  update_object_live_info(result, arg1, arg2);

  if(assign->op)
  {
    if(assign->op == eIrOp_index_source)
    {
      // result = arg1[arg2]
      x86_gen_assign_index_source_stmt(context, ir_stmt);
    }
    else if(assign->op == eIrOp_index_dest)
    {
      // result[arg2] = arg1
      x86_gen_assign_index_dest_stmt(context, ir_stmt);
    }
    else if(assign->op == eIrOp_deref_source)
    {
      // result = ^arg1
      x86_gen_assign_deref_source_stmt(context, ir_stmt);
    }
    else if(assign->op == eIrOp_deref_dest)
    {
      // ^result = arg1
      x86_gen_assign_deref_dest_stmt(context, ir_stmt);
    }
    else if(assign->op == eIrOp_address_of)
    {
      // result = &arg1
      x86_gen_assign_address_of_stmt(context, ir_stmt);
    }
    else
    {
      if(arg2)
      {
        // result = arg1 op arg2
        x86_gen_assign_binexpr_stmt(context, ir_stmt);
      }
      else
      {
        // result = op arg1
        fail("TODO");
      }
    }
  }
  else
  {
    // result = arg1
    x86_gen_assign_simple_stmt(context, ir_stmt);
  }
}

// goto L
void x86_gen_goto_stmt(X86Context* context, IrStmt* ir_stmt)
{
  save_all_registers_to_memory(context, false);

  X86Stmt* jump_stmt = x86_new_stmt(context, eX86Type_None, eX86StmtOpcode_jmp);
  jump_stmt->operand1 = x86_make_id_operand(arena, ir_stmt->goto_label->name);
}

// if arg1 relop arg2 goto L
void x86_gen_cond_goto_stmt(X86Context* context, IrStmt* ir_stmt)
{
  struct IrStmt_cond_goto* cond_goto = &ir_stmt->cond_goto;
  IrArg* arg1 = ir_stmt->cond_goto.arg1;
  IrArg* arg2 = ir_stmt->cond_goto.arg2;

  save_all_registers_to_memory(context, false);

  if(ir_stmt->type == eX86Type_int32)
  {
    X86Location* dest_loc = select_result_register(context, ir_stmt);

    X86Stmt* cmp_stmt = x86_new_stmt(context, ir_stmt->type, eX86StmtOpcode_cmp);
    cmp_stmt->operand1 = x86_make_object_operand(context, arg1->object, dest_loc);
    cmp_stmt->operand2 = x86_make_object_operand(context, arg2->object, lookup_object_location(context, arg2->object));

    X86Stmt* jump_stmt = x86_new_stmt(context, ir_stmt->type, conv_ir_op_to_x86_opcode(cond_goto->relop, ir_stmt->type));
    jump_stmt->operand1 = x86_make_id_operand(arena, cond_goto->label->name);
  }
  else if(ir_stmt->type == eX86Type_float32)
  {
    fail("TODO");
  }
  else assert(0);
}

void x86_gen_call_stmt(X86Context* context, IrStmt* ir_stmt)
{
  AstNode* proc = ir_stmt->call.proc;
  Scope* arg_scope = proc->proc.arg_scope;
  
  save_all_registers_to_memory(context, true);

  /* sub esp, #param_size */
  X86Stmt* stmt = x86_new_stmt(context, eX86Type_int32, eX86StmtOpcode_sub);
  stmt->operand1 = x86_make_register_operand(arena, &context->esp);
  stmt->operand2 = x86_make_constant_operand(arena, arg_scope->data_size);

  /* call #proc_name */
  stmt = x86_new_stmt(context, eX86Type_None, eX86StmtOpcode_call);
  stmt->operand1 = x86_make_id_operand(arena, proc->label_begin->name);

  /* add esp, #param_size */
  stmt = x86_new_stmt(context, eX86Type_int32, eX86StmtOpcode_add);
  stmt->operand1 = x86_make_register_operand(arena, &context->esp);
  stmt->operand2 = x86_make_constant_operand(arena, arg_scope->data_size);
}

void x86_gen_basic_block(X86Context* context, BasicBlock* bb)
{
  for(int i = 0; i < bb->stmt_count; i++)
  {
    IrStmt* ir_stmt = bb->stmt_array[i];

    switch(ir_stmt->kind)
    {
      case eIrStmt_assign:
      {
        x86_gen_assign_stmt(context, ir_stmt);
      }
      break;

      case eIrStmt_goto:
      {
        // goto L
        x86_gen_goto_stmt(context, ir_stmt);
      }
      break;

      case eIrStmt_cond_goto: 
      {
        // if arg1 relop arg2 goto L
        x86_gen_cond_goto_stmt(context, ir_stmt);
      }
      break;

      case eIrStmt_call:
      {
        x86_gen_call_stmt(context, ir_stmt);
      }
      break;

      case eIrStmt_return:
      case eIrStmt_nop:
      break;

      default: assert(0);
    }
  }
}

void x86_gen_proc(X86Context* context, AstNode* proc)
{
  List* basic_blocks = proc->proc.basic_blocks;
  ListItem* li = basic_blocks->first;
  BasicBlock* first_bb = KIND(li, eList_basic_block)->basic_block;

  /* #proc_name: */
  X86Stmt* stmt = x86_new_stmt(context, eX86Type_None, eX86StmtOpcode_label);
  stmt->operand1 = x86_make_id_operand(arena, first_bb->label->name);

  /* push ebp */
  stmt = x86_new_stmt(context, eX86Type_int32, eX86StmtOpcode_push);
  stmt->operand1 = x86_make_register_operand(arena, &context->ebp);

  /* mov ebp, esp */
  stmt = x86_new_stmt(context, eX86Type_int32, eX86StmtOpcode_mov);
  stmt->operand1 = x86_make_register_operand(arena, &context->ebp);
  stmt->operand2 = x86_make_register_operand(arena, &context->esp);

  /* sub esp, #frame_size */
  Scope* body_scope = proc->proc.body_scope;
  stmt = x86_new_stmt(context, eX86Type_int32, eX86StmtOpcode_sub);
  stmt->operand1 = x86_make_register_operand(arena, &context->esp);
  stmt->operand2 = x86_make_constant_operand(arena, body_scope->data_size);

  x86_gen_basic_block(context, first_bb);
  save_all_registers_to_memory(context, true);

  for(li = li->next;
      li;
      li = li->next)
  {
    BasicBlock* bb = KIND(li, eList_basic_block)->basic_block;

    if(bb->label)
    {
      X86Stmt* label_stmt = x86_new_stmt(context, eX86Type_None, eX86StmtOpcode_label);
      label_stmt->operand1 = x86_make_id_operand(arena, bb->label->name);
    }

    x86_gen_basic_block(context, bb);
    save_all_registers_to_memory(context, true);
  }

  /* mov esp, ebp */
  stmt = x86_new_stmt(context, eX86Type_int32, eX86StmtOpcode_mov);
  stmt->operand1 = x86_make_register_operand(arena, &context->esp);
  stmt->operand2 = x86_make_register_operand(arena, &context->ebp);

  /* pop ebp */
  stmt = x86_new_stmt(context, eX86Type_int32, eX86StmtOpcode_pop);
  stmt->operand1 = x86_make_register_operand(arena, &context->ebp);

  /* ret */
  x86_new_stmt(context, eX86Type_None, eX86StmtOpcode_ret);
}

void x86_init_registers(X86Context* context)
{
  int register_count = 0;

  /* eax */
  X86Location* loc = &context->eax;
  loc->kind = eX86Location_eax;
  loc->type = eX86Type_int32;
  context->registers[register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* al */
  X86Location* sub_loc = &context->al;
  loc->sub_loc[0] = sub_loc;
  sub_loc->parent_loc = loc;
  sub_loc->kind = eX86Location_al;
  sub_loc->type = eX86Type_int8;
  context->registers[register_count++] = sub_loc;
  init_list(&sub_loc->occupants, arena, eList_symbol);

  /* ah */
  sub_loc = &context->ah;
  loc->sub_loc[1] = sub_loc;
  sub_loc->parent_loc = loc;
  sub_loc->kind = eX86Location_ah;
  sub_loc->type = eX86Type_int8;
  context->registers[register_count++] = sub_loc;
  init_list(&sub_loc->occupants, arena, eList_symbol);

  /* ebx */
  loc = &context->ebx;
  loc->kind = eX86Location_ebx;
  loc->type = eX86Type_int32;
  context->registers[register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* bl */
  sub_loc = &context->bl;
  loc->sub_loc[0] = sub_loc;
  sub_loc->parent_loc = loc;
  sub_loc->kind = eX86Location_bl;
  sub_loc->type = eX86Type_int8;
  context->registers[register_count++] = sub_loc;
  init_list(&sub_loc->occupants, arena, eList_symbol);

  /* bh */
  sub_loc = &context->bh;
  loc->sub_loc[1] = sub_loc;
  sub_loc->parent_loc = loc;
  sub_loc->kind = eX86Location_bh;
  sub_loc->type = eX86Type_int8;
  context->registers[register_count++] = sub_loc;
  init_list(&sub_loc->occupants, arena, eList_symbol);

  /* ecx */
  loc = &context->ecx;
  loc->kind = eX86Location_ecx;
  loc->type = eX86Type_int32;
  context->registers[register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* cl */
  sub_loc = &context->cl;
  loc->sub_loc[0] = sub_loc;
  sub_loc->parent_loc = loc;
  sub_loc->kind = eX86Location_cl;
  sub_loc->type = eX86Type_int8;
  context->registers[register_count++] = sub_loc;
  init_list(&sub_loc->occupants, arena, eList_symbol);

  /* ch */
  sub_loc = &context->ch;
  loc->sub_loc[1] = sub_loc;
  sub_loc->parent_loc = loc;
  sub_loc->kind = eX86Location_ch;
  sub_loc->type = eX86Type_int8;
  context->registers[register_count++] = sub_loc;
  init_list(&sub_loc->occupants, arena, eList_symbol);

  /* edx */
  loc = &context->edx;
  loc->kind = eX86Location_edx;
  loc->type = eX86Type_int32;
  context->registers[register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* dl */
  sub_loc = &context->dl;
  loc->sub_loc[0] = sub_loc;
  sub_loc->parent_loc = loc;
  sub_loc->kind = eX86Location_dl;
  sub_loc->type = eX86Type_int8;
  context->registers[register_count++] = sub_loc;
  init_list(&sub_loc->occupants, arena, eList_symbol);

  /* dh */
  sub_loc = &context->dh;
  loc->sub_loc[1] = sub_loc;
  sub_loc->parent_loc = loc;
  sub_loc->kind = eX86Location_dh;
  sub_loc->type = eX86Type_int8;
  context->registers[register_count++] = sub_loc;
  init_list(&sub_loc->occupants, arena, eList_symbol);

  /* esi */
  loc = &context->esi;
  loc->kind = eX86Location_esi;
  loc->type = eX86Type_int32;
  context->registers[register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* edi */
  loc = &context->edi;
  loc->kind = eX86Location_edi;
  loc->type = eX86Type_int32;
  context->registers[register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* memory */
  loc = &context->memory;
  *loc = (X86Location){0};
  loc->kind = eX86Location_memory;
  loc->type = eX86Type_None;
  init_list(&loc->occupants, arena, eList_symbol);

  /* ebp */
  loc = &context->ebp;
  loc->kind = eX86Location_ebp;
  loc->type = eX86Type_int32;
  init_list(&loc->occupants, arena, eList_symbol);

  /* esp */
  loc = &context->esp;
  loc->kind = eX86Location_esp;
  loc->type = eX86Type_int32;
  init_list(&loc->occupants, arena, eList_symbol);
}

bool translate(char* title, char* file_path, char* hoc_text, String* x86_text)
{
  basic_type_bool = new_basic_type(eBasicType_bool);
  basic_type_int = new_basic_type(eBasicType_int);
  basic_type_char = new_basic_type(eBasicType_char);
  basic_type_float = new_basic_type(eBasicType_float);
  basic_type_void = new_basic_type(eBasicType_void);
  subst_list = new_list(arena, eList_type_pair);
  
  SymbolContext sym_context = {0};
  sym_context.arena = push_arena(&arena, 1*MEGABYTE);
  sym_context.nesting_depth = -1;
  init_list(&sym_context.scopes, sym_context.arena, eList_scope);

  bool_true = new_const_object(sym_context.arena, basic_type_int, 0);
  bool_true->int_val = 1;

  bool_false = new_const_object(sym_context.arena, basic_type_int, 0);
  bool_false->int_val = 0;
  
  IrContext ir_context = {0};
  ir_context.stmt_arena = push_arena(&arena, 1*MEGABYTE);
  ir_context.stmt_array = (IrStmt*)ir_context.stmt_arena->base;
  ir_context.stmt_count = 0;
  ir_context.sym_arena = sym_context.arena;
  ir_context.label_list = new_list(arena, eList_ir_label);
  ir_context.data_alignment = 4;
  
  TokenStream token_stream = {0};
  init_token_stream(&token_stream, hoc_text, file_path);
  get_next_token(&token_stream);
  
  AstNode* module = 0;
  if(!(parse_module(&token_stream, &module) &&
       sym_module(&sym_context, module) &&
       set_types_module(module) &&
       eval_types_module(module) &&
       resolve_types_module(module) &&
       check_types_module(module) &&
       ir_gen_module(&ir_context, module)))
  {
    return false;
  }

  // X86 Codegen
  {
    X86Context x86_context = {0};
    x86_context.stmt_arena = push_arena(&arena, 1*MEGABYTE);
    x86_context.stmt_array = (X86Stmt*)x86_context.stmt_arena->base;
    x86_context.machine_word_size = 4;
    x86_init_registers(&x86_context);
    
    List* procs = &module->module.procs;
    for(ListItem* li = procs->first;
        li;
        li = li->next)
    {
      AstNode* proc = KIND(li, eList_ast_node)->ast_node;

      partition_to_basic_blocks(ir_context.stmt_arena, proc);
      x86_gen_proc(&x86_context, proc);
    }

    DEBUG_print_ir_code(arena, &module->module.procs, "./out.ir");

    str_init(x86_text, push_arena(&arena, 1*MEGABYTE));
    str_printfln(x86_text, ".686");
    str_printfln(x86_text, ".xmm");
    str_printfln(x86_text, ".model flat, C");
    str_printfln(x86_text, ".stack 4096");
    str_printfln(x86_text, ".data");
    str_printfln(x86_text, "static_area label byte");
    str_printfln(x86_text, "align %d", ir_context.data_alignment);
    
    Scope* module_scope = module->module.scope;
    for(ListItem* li = module_scope->decl_syms.first;
        li;
        li = li->next)
    {
      Symbol* object = KIND(li, eList_symbol)->symbol;
      if(object->data_size > 0)
      {
        str_printfln(x86_text, "byte %d dup(?)", object->data_size);
      }
    }
    
    str_printfln(x86_text, ".code");
    str_printfln(x86_text, "public startup");
    
    for(int i = 0; i < x86_context.stmt_count; i++)
    {
      x86_print_stmt(x86_text, &x86_context.stmt_array[i]);
    }
    
    str_printfln(x86_text, "END");
  }
  
  
  //           Old X86 codegen
  //---------------------------------------------------
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
  
  IrLabel label;
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
  label = new_gen_label();
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
  label = new_gen_label();
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

//           Old X86 codegen
//---------------------------------------------------
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
        
        default: assert(0);
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
          
          IrLabel label = new_gen_label();
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
            
            IrLabel label = new_gen_label();
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
          
          IrLabel label = new_gen_label();
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
        
        default: assert(0);
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
      IrLabel label = node->while_.label = new_gen_label();
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
      
      IrLabel label = new_gen_label();
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
      IrLabel* label = 0;
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
    
    default: assert(0);
  }
  return success;
}
#endif

