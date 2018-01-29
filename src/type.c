int get_type_width(Type* type)
{
  switch(type->kind)
  {
    case eType_array:
    {
      type->width = type->array.size * get_type_width(type->array.elem);
    }
    break;
    
    case eType_product:
    {
      type->width = get_type_width(type->product.left) + get_type_width(type->product.right);
    }
    break;
    
    case eType_proc:
    {
      type->width = get_type_width(type->proc.ret) + get_type_width(type->proc.args);
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
      type->width = get_type_width(type->var.type);
    }
    break;
    
    default: assert(0);
  }
  return type->width;
}

Type* new_var_type(MemoryArena* arena, Type* var_type)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_var;
  type->var.type = var_type;
  type->width = get_type_width(type);

  return type;
}

Type* new_basic_type(MemoryArena* arena, eBasicType kind)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_basic;
  type->basic.kind = kind;
  type->width = get_type_width(type);

  return type;
}

Type* new_proc_type(MemoryArena* arena, Type* args, Type* ret)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_proc;
  type->proc.args = args;
  type->proc.ret = ret;
  type->width = 0;

  return type;
}

Type* new_typevar(MemoryArena* arena)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_typevar;
  type->typevar.id = typevar_id++;
  type->width = 0;

  return type;
}

Type* new_product_type(MemoryArena* arena, Type* left, Type* right)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_product;
  type->product.left = left;
  type->product.right = right;
  type->width = 0;

  return type;
}

Type* new_array_type(MemoryArena* arena, int size, int ndim, Type* elem)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_array;
  type->array.size = size;
  type->array.ndim = ndim;
  type->array.elem = elem;
  type->width = 0;

  return type;
}

Type* new_pointer_type(MemoryArena* arena, Type* pointee)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = eType_pointer;
  type->pointer.pointee = pointee;
  type->width = 0;

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

Type* copy_type(MemoryArena* arena, Type* type)
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

TypePair* new_type_pair(MemoryArena* arena, Type* key, Type* value)
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

Type* type_subst(MemoryArena* arena, List* subst_list, Type* type)
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
    subst = copy_type(arena, type);
    
    pair = new_type_pair(arena, type, subst);
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
        subst->proc.args = type_subst(arena, subst_list, subst->proc.args);
        subst->proc.ret = type_subst(arena, subst_list, subst->proc.ret);
      }
      break;
      
      case eType_product:
      {
        subst->product.left = type_subst(arena, subst_list, subst->product.left);
        subst->product.right = type_subst(arena, subst_list, subst->product.right);
      }
      break;
      
      case eType_pointer:
      {
        subst->pointer.pointee = type_subst(arena, subst_list, subst->pointer.pointee);
      }
      break;
      
      case eType_array:
      {
        subst->array.elem = type_subst(arena, subst_list, subst->array.elem);
      }
      break;
      
      case eType_var:
      {
        subst->var.type = type_subst(arena, subst_list, subst->var.type);
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

void make_type_printstr(String* str, Type* type)
{
  switch(type->kind)
  {
    case eType_basic:
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
    break;

    case eType_pointer:
    {
      make_type_printstr(str, type->pointer.pointee);
      str_append(str, "^");
    }
    break;

    case eType_array:
    {
      str_append(str, "(");
      if(type->array.size >= 0)
        str_printf(str, "[%d]", type->array.size);
      else
        str_append(str, "[]");
      make_type_printstr(str, type->array.elem);
      str_append(str, ")");
    }
    break;

    case eType_product:
    {
      make_type_printstr(str, type->product.left);
      str_append(str, ", ");
      make_type_printstr(str, type->product.right);
    }
    break;

    case eType_proc:
    {
      make_type_printstr(str, type->proc.ret);
      str_append(str, " (");
      make_type_printstr(str, type->proc.args);
      str_append(str, ")");
    }
    break;

    case eType_var:
    {
      make_type_printstr(str, type->var.type);
    }
    break;

    case eType_typevar:
    {
      str_printf(str, "type_%d", type->typevar.id);
    }
    break;

    default: assert(0);
  }
}

char* get_type_printstr(MemoryArena* arena, Type* type)
{
  String str; str_init(arena, &str);
  make_type_printstr(&str, type);

  return str_cap(&str);
}

//     SET TYPES
//-----------------------------------------------------

bool set_types_expr(MemoryArena* arena, AstNode* expr);
bool set_types_type(MemoryArena* arena, AstNode* type);
bool set_types_block_stmt(MemoryArena* arena, AstNode* stmt);

bool set_types_array(MemoryArena* arena, AstNode* array)
{
  assert(KIND(array, eAstNode_array));
  bool success = true;
  
  if(success = set_types_expr(arena, array->array.size_expr) && set_types_type(arena, array->array.elem_expr))
  {
    int size = 0;
    AstNode* size_expr = array->array.size_expr;
    if(size_expr->kind == eAstNode_lit && size_expr->lit.kind == eLiteral_int)
    {
      size = size_expr->lit.int_val;
      if(size < 0)
        success = compile_error(arena, size_expr->src_loc, "array size must be greater than 0");
    }
    else
      success = compile_error(arena, size_expr->src_loc, "array size must be an int literal");

    if(success)
    {
      array->array.ndim = 1;
      array->array.size = size;

      AstNode* elem_expr = array->array.elem_expr;
      if(elem_expr->kind == eAstNode_array)
      {
        array->array.ndim += elem_expr->array.ndim;
      }
      
      array->ty = array->eval_ty = new_array_type(arena, array->array.size, array->array.ndim, elem_expr->ty);

      if(size == 0)
      {
        array->ty = array->eval_ty = new_pointer_type(arena, elem_expr->ty);
      }
    }
  }

  return success;
}

bool set_types_pointer(MemoryArena* arena, AstNode* pointer)
{
  assert(KIND(pointer, eAstNode_pointer));
  bool success = true;
  
  AstNode* pointee = pointer->pointer.pointee;
  if(success = set_types_type(arena, pointee))
  {
    pointer->ty = pointer->eval_ty = new_pointer_type(arena, pointee->ty);
  }
  return success;
}

bool set_types_type(MemoryArena* arena, AstNode* type)
{
  bool success = true;
  
  switch(type->kind)
  {
    case eAstNode_pointer:
    success = set_types_pointer(arena, type);
    break;
    
    case eAstNode_array:
    success = set_types_array(arena, type);
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
        type->ty = type->eval_ty = new_typevar(arena);
      }
      break;
      
      default: assert(0);
    }
    break;
    
    default:
    {
      success = compile_error(arena, type->src_loc, "invalid type expression");
    }
  }

  return success;
}

bool set_types_var(MemoryArena* arena, AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  AstNode* type = var->var.type;
  if(success = set_types_type(arena, type))
  {
    var->ty = new_var_type(arena, type->ty);
    var->eval_ty = type->ty;

    if(var->var.init_expr)
    {
      success = set_types_expr(arena, var->var.init_expr);
    }
  }

  return success;
}

bool set_types_bin_expr(MemoryArena* arena, AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;
  
  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  
  if(success = set_types_expr(arena, left_operand) && set_types_expr(arena, right_operand))
  {
    bin_expr->eval_ty = new_typevar(arena);
    bin_expr->ty = new_proc_type(arena, new_product_type(arena, left_operand->eval_ty, right_operand->eval_ty), bin_expr->eval_ty);
  }

  return success;
}

bool set_types_unr_expr(MemoryArena* arena, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  if(success = set_types_expr(arena, operand))
  {
    unr_expr->eval_ty = new_typevar(arena);
    unr_expr->ty = new_proc_type(arena, operand->eval_ty, unr_expr->eval_ty);
  }

  return success;
}

bool set_types_actual_arg(MemoryArena* arena, AstNode* call_arg)
{
  assert(KIND(call_arg, eAstNode_call_arg));
  bool success = true;

  AstNode* expr = call_arg->call_arg.expr;
  if(success = set_types_expr(arena, expr))
  {
    call_arg->eval_ty = expr->eval_ty;
    call_arg->ty = expr->ty;
  }

  return success;
}

bool set_types_id(MemoryArena* arena, AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;

  id->ty = new_typevar(arena);
  id->eval_ty = new_typevar(arena);

  return success;
}

Type* make_type_of_args(MemoryArena* arena, AstNode* args);

bool set_types_actual_args(MemoryArena* arena, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = set_types_expr(arena, arg);
  }

  if(success)
  {
    args->ty = args->eval_ty = make_type_of_args(arena, args);
  }

  return success;
}

bool set_types_call(MemoryArena* arena, AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  bool success = true;
  
  AstNode* call_expr = call->call.expr;
  AstNode* args = call->call.args;
  if(call_expr->kind == eAstNode_id)
  {
    if(success = set_types_id(arena, call_expr) && set_types_actual_args(arena, args))
    {
      call->eval_ty = new_typevar(arena);
      call->ty = new_proc_type(arena, args->ty, call->eval_ty);
    }
  }
  else
    success = compile_error(arena, call->src_loc, "unsupported call expr");

  return success;
}

bool set_types_lit(MemoryArena* arena, AstNode* lit)
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

    case eLiteral_str:
    {
      ty = new_array_type(arena, cstr_len(lit->lit.str_val)+1, 1, basic_type_char);
    }
    break;
    
    default: assert(0);
  }
  lit->ty = lit->eval_ty = ty;

  return success;
}

bool set_types_index(MemoryArena* arena, AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;
  
  if(success = set_types_expr(arena, index->index.array_expr) && set_types_expr(arena, index->index.i_expr))
  {
    index->ty = index->index.array_expr->eval_ty;
    index->eval_ty = new_typevar(arena);
  }

  return success;
}

bool set_types_cast(MemoryArena* arena, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;

  AstNode* to_type = cast->cast.to_type;
  AstNode* from_expr = cast->cast.from_expr;
  if(success = set_types_type(arena, to_type) && set_types_expr(arena, from_expr))
  {
    cast->eval_ty = to_type->eval_ty;
    cast->ty = new_product_type(arena, from_expr->eval_ty, cast->eval_ty);
  }

  return success;
}

bool set_types_assign(MemoryArena* arena, AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;
  
  AstNode* dest_expr = assign->assign.dest_expr;
  AstNode* source_expr =assign->assign.source_expr;
  if(success = set_types_expr(arena, dest_expr) && set_types_expr(arena, source_expr))
  {
    assign->ty = assign->eval_ty = dest_expr->eval_ty;
  }

  return success;
}

bool set_types_expr(MemoryArena* arena, AstNode* expr)
{
  bool success = true;
  
  switch(expr->kind)
  {
    case eAstNode_pointer:
    {
      success = set_types_pointer(arena, expr);
    }
    break;
    
    case eAstNode_array:
    {
      success = set_types_array(arena, expr);
    }
    break;
    
    case eAstNode_cast:
    {
      success = set_types_cast(arena, expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = set_types_bin_expr(arena, expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = set_types_unr_expr(arena, expr);
    }
    break;
    
    case eAstNode_id:
    {
      success = set_types_id(arena, expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = set_types_call(arena, expr);
    }
    break;
    
    case eAstNode_lit:
    {
      success = set_types_lit(arena, expr);
    }
    break;
    
    case eAstNode_basic_type:
    {
      success = set_types_type(arena, expr);
    }
    break;
    
    case eAstNode_index:
    {
      success = set_types_index(arena, expr);
    }
    break;

    case eAstNode_call_arg:
    {
      success = set_types_actual_arg(arena, expr);
    }
    break;

    case eAstNode_assign:
    {
      success = set_types_assign(arena, expr);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool set_types_return(MemoryArena* arena, AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;
  
  if(ret->ret.expr)
  {
    AstNode* ret_expr = ret->ret.expr;
    if(success = set_types_expr(arena, ret_expr))
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

bool set_types_if(MemoryArena* arena, AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  
  if(success = set_types_expr(arena, if_->if_.cond_expr))
  {
    AstNode* body = if_->if_.body;
    if(success = set_types_block_stmt(arena, body))
    {
      if_->ty = body->ty;
      if_->eval_ty = body->eval_ty;
      
      AstNode* else_body = if_->if_.else_body;
      if(else_body)
      {
        success = set_types_block_stmt(arena, else_body);
      }
    }
  }

  return success;
}

bool set_types_do_while(MemoryArena* arena, AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  AstNode* body = do_while->do_while.body;
  if(success = set_types_block_stmt(arena, body) && set_types_expr(arena, do_while->do_while.cond_expr))
  {
    do_while->ty = body->ty;
    do_while->eval_ty = body->eval_ty;
  }

  return success;
}

bool set_types_while(MemoryArena* arena, AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  
  AstNode* body = while_->while_.body;
  if(success = set_types_expr(arena, while_->while_.cond_expr) && set_types_block_stmt(arena, body))
  {
    while_->ty = body->ty;
    while_->eval_ty = body->eval_ty;
  }

  return success;
}

bool set_types_block(MemoryArena* arena, AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = set_types_block_stmt(arena, stmt);
  }

  if(success)
  {
    block->ty = block->eval_ty = basic_type_void;
  }

  return success;
}

bool set_types_block_stmt(MemoryArena* arena, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_var:
    {
      success = set_types_var(arena, stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = set_types_block(arena, stmt);
    }
    break;
    
    case eAstNode_assign:
    {
      success = set_types_assign(arena,stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = set_types_cast(arena, stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    {
      success = set_types_expr(arena, stmt);
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
      success = set_types_type(arena, stmt);
    }
    break;
    case eAstNode_return:
    {
      success = set_types_return(arena, stmt);
    }
    break;

    case eAstNode_if:
    {
      success = set_types_if(arena, stmt);
    }
    break;

    case eAstNode_do_while:
    {
      success = set_types_do_while(arena, stmt);
    }
    break;

    case eAstNode_while:
    {
      success = set_types_while(arena, stmt);
    }
    break;

    case eAstNode_index:
    {
      success = set_types_index(arena, stmt);
    }
    break;

    default: assert(0);
  }

  return success;
}

Type* make_type_of_args(MemoryArena* arena, AstNode* args)
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
      result = new_product_type(arena, result, next_arg->eval_ty);
    }
  }

  return result;
}

bool set_types_formal_args(MemoryArena* arena, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = set_types_var(arena, arg);
  }
  if(success)
  {
    args->ty = args->eval_ty = make_type_of_args(arena, args);
  }

  return success;
}

bool set_types_proc(MemoryArena* arena, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  
  AstNode* ret_type = proc->proc.ret_type;
  AstNode* args = proc->proc.args;
  if(success = set_types_formal_args(arena, args) && set_types_type(arena, ret_type))
  {
    proc->ty = new_proc_type(arena, args->eval_ty, ret_type->eval_ty);
    proc->eval_ty = basic_type_void;
    
    if(!is_extern_proc(proc))
    {
      success = set_types_block(arena, proc->proc.body);
    }
  }

  return success;
}

bool set_types_module_stmt(MemoryArena* arena, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_proc:
    {
      success = set_types_proc(arena, stmt);
    }
    break;

    case eAstNode_var:
    {
      success = set_types_var(arena, stmt);
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

bool set_types_module(MemoryArena* arena, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  module->ty = module->eval_ty = basic_type_void;
  
  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = set_types_module_stmt(arena, stmt);
  }

  return success;
}

//       EVAL TYPES
//-----------------------------------------------------

bool eval_types_expr(MemoryArena* arena, AstNode* expr);
bool eval_types_type(MemoryArena* arena, AstNode* type);
bool eval_types_block_stmt(MemoryArena* arena, AstNode* stmt);

bool eval_types_array(MemoryArena* arena, AstNode* array)
{
  assert(KIND(array, eAstNode_array));
  bool success = true;
  
  success = eval_types_expr(arena, array->array.size_expr) && eval_types_type(arena, array->array.elem_expr);
  return success;
}

bool eval_types_pointer(MemoryArena* arena, AstNode* pointer)
{
  assert(KIND(pointer, eAstNode_pointer));

  bool success = true;
  success = eval_types_expr(arena, pointer->pointer.pointee);

  return success;
}

bool eval_types_type(MemoryArena* arena, AstNode* type)
{
  bool success = true;
  
  switch(type->kind)
  {
    case eAstNode_pointer:
    {
      success = eval_types_pointer(arena, type);
    }
    break;

    case eAstNode_array:
    {
      success = eval_types_array(arena, type);
    }
    break;

    case eAstNode_basic_type:
    break;
  }

  return success;
}

bool eval_types_cast(MemoryArena* arena, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));

  bool success = true;
  success = eval_types_type(arena, cast->cast.to_type) && eval_types_expr(arena, cast->cast.from_expr);

  return success;
}

bool eval_types_bin_expr(MemoryArena* arena, AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;
  
  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  eOperator op = bin_expr->bin_expr.op;
  
  if(success = eval_types_expr(arena, left_operand) && eval_types_expr(arena, right_operand))
  {
    switch(op)
    {
      case eOperator_bit_and:
      case eOperator_bit_or:
      case eOperator_bit_xor:
      {
        if(type_unif(left_operand->eval_ty, basic_type_int)
           && type_unif(right_operand->eval_ty, basic_type_int)
           && type_unif(bin_expr->eval_ty, basic_type_int))
        {
          ;//ok
        }
        else
          success = compile_error(arena, bin_expr->src_loc, "type error (bitwise op)");
      }
      break;
      
      case eOperator_bit_shift_left:
      case eOperator_bit_shift_right:
      {
        if(type_unif(left_operand->eval_ty, basic_type_int)
           && type_unif(right_operand->eval_ty, basic_type_char)
           && type_unif(bin_expr->eval_ty, basic_type_int))
        {
          ;//ok
        }
        else
          success = compile_error(arena, bin_expr->src_loc, "type error (bitwise op)");
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
                success = compile_error(arena, bin_expr->src_loc, "type error (bin expr)");
              }
            }
            break;

            default:
            {
              if(!type_unif(bin_expr->eval_ty, left_operand->eval_ty))
              {
                success = compile_error(arena, bin_expr->src_loc, "type error (bin expr)");
              }
            }
            break;
          }
        }
        else
          success = compile_error(arena, bin_expr->src_loc, "type error (bin expr)");
      }
      break;
    }

    if(success)
    {
      Type* bin_expr_ty = KIND(bin_expr->ty, eType_proc);
      if(!type_unif(bin_expr_ty->proc.ret, bin_expr->eval_ty))
      {
        success = compile_error(arena, bin_expr->src_loc, "type error (bin expr)");
      }
    }
  }

  return success;
}

bool eval_types_id(MemoryArena* arena, AstNode* id)
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
      success = compile_error(arena, id->src_loc, "unknown id `%s`", id->id.name);
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
              success = compile_error(arena, id->src_loc, "var `%s` must be declared before its use", id->id.name);
            }
            else
            {
              if(!type_unif(decl_ast->ty->var.type, id->eval_ty))
              {
                success = compile_error(arena, id->src_loc, "type error (var id)");
              }
            }
          }
        break;
        
        case eType_proc:
        {
          if(!type_unif(decl_ast->ty->proc.ret, id->eval_ty))
          {
            success = compile_error(arena, id->src_loc, "type error (proc id)");
          }
        }
        break;

        default: assert(0);
      }
    }
    else
      success = compile_error(arena, id->src_loc, "type error (id)");
  }

  return success;
}

bool eval_types_unr_expr(MemoryArena* arena, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;
  
  if(success = eval_types_expr(arena, operand))
  {
    switch(op)
    {
      case eOperator_neg:
      case eOperator_logic_not:
      case eOperator_bit_not:
      if(!type_unif(unr_expr->eval_ty, operand->eval_ty))
      {
        success = compile_error(arena, unr_expr->src_loc, "type error (unr expr)");
      }
      break;
      
      case eOperator_deref:
      {
        Type* pointee_ty = new_typevar(arena);
        if(type_unif(operand->eval_ty, new_pointer_type(arena, pointee_ty)))
        {
          if(!type_unif(unr_expr->eval_ty, pointee_ty))
          {
            success = compile_error(arena, unr_expr->src_loc, "type error (unr expr)");
          }
        }
        else
          success = compile_error(arena, operand->src_loc, "pointer type expected");
      }
      break;
      
      case eOperator_address_of:
      {
        ; // skip
      }
      break;
      
      default: assert(0);
    }

    if(success)
    {
      Type* unr_expr_ty = KIND(unr_expr->ty, eType_proc);
      if(!type_unif(unr_expr_ty->proc.ret, unr_expr->eval_ty))
      {
        success = compile_error(arena, unr_expr->src_loc, "type error (unr expr)");
      }
    }
  }

  return success;
}

bool eval_types_var(MemoryArena* arena, AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  if(type_unif(var->ty->var.type, var->eval_ty))
  {
    AstNode* init_expr = var->var.init_expr;
    if(init_expr)
    {
      if(success = eval_types_expr(arena, init_expr))
      {
        if(!type_unif(var->eval_ty, init_expr->eval_ty))
        {
          success = compile_error(arena, var->src_loc, "type error (var init expr)");
        }
      }
    }
  }
  else
  {
    success = compile_error(arena, var->src_loc, "type error (var)");
  }

  return success;
}

bool eval_types_formal_args(MemoryArena* arena, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = eval_types_var(arena, arg);
  }

  return success;
}

bool eval_types_actual_args(MemoryArena* arena, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = eval_types_expr(arena, arg->call_arg.expr);
  }

  return success;
}

bool eval_types_call(MemoryArena* arena, AstNode* call)
{
  assert(KIND(call, eAstNode_call));

  bool success = true;
  success = eval_types_id(arena, call->call.expr) && eval_types_actual_args(arena, call->call.args);

  return success;
}

bool eval_types_index(MemoryArena* arena, AstNode* index)
{
  assert(KIND(index, eAstNode_index));

  bool success = true;
  success = eval_types_expr(arena, index->index.array_expr) && eval_types_expr(arena, index->index.i_expr);

  return success;
}

bool eval_types_assign(MemoryArena* arena, AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));

  bool success = true;
  success = eval_types_expr(arena, assign->assign.dest_expr) && eval_types_expr(arena, assign->assign.source_expr);

  return success;
}

bool eval_types_expr(MemoryArena* arena, AstNode* expr)
{
  bool success = true;
  
  switch(expr->kind)
  {
    case eAstNode_cast:
    {
      success = eval_types_cast(arena, expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = eval_types_bin_expr(arena, expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = eval_types_unr_expr(arena, expr);
    }
    break;
    
    case eAstNode_id:
    {
      success = eval_types_id(arena, expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = eval_types_call(arena, expr);
    }
    break;
    
    case eAstNode_index:
    {
      success = eval_types_index(arena, expr);
    }
    break;
    
    case eAstNode_lit:
    case eAstNode_basic_type:
    break;

    case eAstNode_assign:
    {
      success = eval_types_assign(arena, expr);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool eval_types_if(MemoryArena* arena, AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  
  AstNode* cond_expr = if_->if_.cond_expr;
  AstNode* body = if_->if_.body;
  AstNode* else_body = if_->if_.else_body;
  
  if(success = eval_types_expr(arena, cond_expr) &&
     eval_types_block_stmt(arena, body) &&
     (else_body ? eval_types_block_stmt(arena, else_body) : true))
  {
    if(!type_unif(cond_expr->eval_ty, basic_type_bool))
    {
      success = compile_error(arena, cond_expr->src_loc, "bool expression was expected");
    }
  }

  return success;
}

bool eval_types_block(MemoryArena* arena, AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = eval_types_block_stmt(arena, stmt);
  }

  return success;
}

bool eval_types_do_while(MemoryArena* arena, AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  AstNode* cond_expr = do_while->do_while.cond_expr;
  if(success = eval_types_block_stmt(arena, do_while->do_while.body) && eval_types_expr(arena, cond_expr))
  {
    if(!type_unif(cond_expr->eval_ty, basic_type_bool))
    {
      success = compile_error(arena, cond_expr->src_loc, "bool expression was expected");
    }
  }
  return success;
}

bool eval_types_while(MemoryArena* arena, AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  
  AstNode* cond_expr = while_->while_.cond_expr;
  if(success = eval_types_expr(arena, cond_expr) && eval_types_block_stmt(arena, while_->while_.body))
  {
    if(!type_unif(cond_expr->eval_ty, basic_type_bool))
    {
      success = compile_error(arena, cond_expr->src_loc, "bool expression was expected");
    }
  }

  return success;
}

bool eval_types_return(MemoryArena* arena, AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;
  
  AstNode* ret_expr = ret->ret.expr;
  if(ret_expr && (success = eval_types_expr(arena, ret_expr)))
  {
    AstNode* proc = ret->ret.proc;
    Type* proc_ty = KIND(proc->ty, eType_proc);
    if(!type_unif(ret_expr->eval_ty, proc_ty->proc.ret))
    {
      success = compile_error(arena, ret->src_loc, "type error (return)");
    }
  }

  return success;
}

bool eval_types_block_stmt(MemoryArena* arena, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_assign:
    {
      success = eval_types_assign(arena, stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = eval_types_cast(arena, stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    {
      success = eval_types_expr(arena, stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = eval_types_if(arena, stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = eval_types_do_while(arena, stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = eval_types_while(arena, stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = eval_types_block(arena, stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = eval_types_return(arena, stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = eval_types_var(arena, stmt);
    }
    break;

    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    break;
    
    case eAstNode_basic_type:
    {
      success = eval_types_type(arena, stmt);
    }
    break;
    
    case eAstNode_index:
    {
      success = eval_types_index(arena, stmt);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool eval_types_proc(MemoryArena* arena, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  success = eval_types_formal_args(arena, proc->proc.args) && eval_types_block_stmt(arena, proc->proc.body)
    && eval_types_type(arena, proc->proc.ret_type);

  return success;
}

bool eval_types_module_stmt(MemoryArena* arena, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_proc:
    {
      success = eval_types_proc(arena, stmt);
    }
    break;

    case eAstNode_var:
    case eAstNode_include:
    break;

    default: assert(0);
  }
  return success;
}

bool eval_types_module(MemoryArena* arena, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = eval_types_module_stmt(arena, stmt);
  }

  return success;
}

//       RESOLVE TYPES
//-----------------------------------------------------

bool resolve_types_expr(MemoryArena* arena, AstNode* expr);
bool resolve_types_type(MemoryArena* arena, AstNode* type);
bool resolve_types_block_stmt(MemoryArena* arena, AstNode* stmt);

bool resolve_types_of_node(MemoryArena* arena, AstNode* node)
{
  bool success = true;
  
  if(success = resolve_type(node->ty, &node->ty))
  {
    get_type_width(node->ty);
    if(success = resolve_type(node->eval_ty, &node->eval_ty))
    {
      get_type_width(node->eval_ty);
    }
    else
      success = compile_error(arena, node->src_loc, "type error (unresolved type)");
  }
  else
    success = compile_error(arena, node->src_loc, "type error (unresolved type)");
  
  return success;
}

bool resolve_types_var(MemoryArena* arena, AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  if(success = resolve_types_of_node(arena, var))
  {
    var->var.decl_sym->ty = var->eval_ty;

    if(var->var.init_expr)
    {
      success = resolve_types_expr(arena, var->var.init_expr);
    }
  }

  return success;
}

bool resolve_types_lit(MemoryArena* arena, AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));
  bool success = true;

  if(success = resolve_types_of_node(arena, lit))
  {
    lit->lit.constant->ty = lit->eval_ty;
  }

  return success;
}

bool resolve_types_formal_args(MemoryArena* arena, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = resolve_types_var(arena, arg);
  }
  if(success)
  {
    success = resolve_types_of_node(arena, args);
  }
  return success;
}

bool resolve_types_bin_expr(MemoryArena* arena, AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;

  success = resolve_types_expr(arena, bin_expr->bin_expr.left_operand) &&
    resolve_types_expr(arena, bin_expr->bin_expr.right_operand) && resolve_types_of_node(arena, bin_expr);

  return success;
}

bool resolve_types_unr_expr(MemoryArena* arena, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;

  if(success = resolve_types_expr(arena, unr_expr->unr_expr.operand))
  {
    AstNode* operand = unr_expr->unr_expr.operand;
    eOperator op = unr_expr->unr_expr.op;

    if(op == eOperator_address_of)
    {
      if(operand->eval_ty->kind == eType_array)
      {
        // ptr(array(T)) = ptr(T)
        Type* operand_ty = operand->eval_ty;
        success = type_unif(unr_expr->eval_ty, new_pointer_type(arena, operand_ty->array.elem));
      }
      else
      {
        success = type_unif(unr_expr->eval_ty, new_pointer_type(arena, operand->eval_ty));
      }

      if(!success)
      {
        compile_error(arena, unr_expr->src_loc, "type error (unr expr)");
      }
    }
  }
  
  if(success)
  {
    success = resolve_types_of_node(arena, unr_expr);
  }

  return success;
}

bool resolve_types_id(MemoryArena* arena, AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;

  success = resolve_types_of_node(arena, id);
  return success;
}

bool resolve_types_actual_args(MemoryArena* arena, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    if(success = resolve_types_expr(arena, arg->call_arg.expr))
    {
      arg->eval_ty = arg->call_arg.expr->eval_ty;
    }
  }

  if(success)
  {
    success = resolve_types_of_node(arena, args);
  }

  return success;
}

bool resolve_types_call(MemoryArena* arena, AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  assert(call->call.expr->kind == eAstNode_id);

  bool success = true;

  if(success = resolve_types_id(arena, call->call.expr) && resolve_types_actual_args(arena, call->call.args))
  {
    AstNode* args = call->call.args;
    for(ListItem* li = args->node_list.first;
        li;
        li = li->next)
    {
      AstNode* arg = KIND(li, eList_ast_node)->ast_node;
      arg->call_arg.param->ty = arg->eval_ty;
    }

    AstNode* proc = call->call.proc = KIND(call->call.expr, eAstNode_id)->id.decl_ast;
    if(proc->ty->kind == eType_proc)
    {
      if(!type_unif(proc->ty, call->ty))
      {
        success = compile_error(arena, call->src_loc, "type error (call argument types)");
      }
    }
    else
    {
      success = compile_error(arena, call->src_loc, "type error (call)");
    }
  }

  if(success && (success = resolve_types_of_node(arena, call)))
  {
    call->call.retvar->ty = call->eval_ty;
  }

  return success;
}

bool resolve_types_index(MemoryArena* arena, AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;

  AstNode* array_expr = index->index.array_expr;
  AstNode* i_expr = index->index.i_expr;

  if(success = resolve_types_expr(arena, array_expr) && resolve_types_expr(arena, i_expr))
  {
    if(type_unif(i_expr->eval_ty, basic_type_int))
    {
      Type* array_ty = array_expr->eval_ty;

      if(array_ty->kind == eType_array)
      {
        if(!type_unif(array_ty->array.elem, index->eval_ty))
        {
          success = compile_error(arena, index->src_loc, "type error (index)");
        }
      }
      else if(array_ty->kind == eType_pointer)
      {
        if(!type_unif(array_ty->pointer.pointee, index->eval_ty))
        {
          success = compile_error(arena, index->src_loc, "type error (index)");
        }
      }
      else
        success = compile_error(arena, index->src_loc, "type error (index)");
    }
    else
      success = compile_error(arena, index->src_loc, "type error (index)");
  }

  if(success)
  {
    success = resolve_types_of_node(arena, index);
  }

  return success;
}

bool resolve_types_cast(MemoryArena* arena, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;

  success = resolve_types_type(arena, cast->cast.to_type) && resolve_types_expr(arena, cast->cast.from_expr) &&
    resolve_types_of_node(arena, cast);

  return success;
}

bool resolve_types_assign(MemoryArena* arena, AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;

  success = resolve_types_expr(arena, assign->assign.dest_expr) && resolve_types_expr(arena, assign->assign.source_expr) &&
    resolve_types_of_node(arena, assign);

  return success;
}

bool resolve_types_expr(MemoryArena* arena, AstNode* expr)
{
  bool success = true;
  
  switch(expr->kind)
  {
    case eAstNode_cast:
    {
      success = resolve_types_cast(arena, expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = resolve_types_bin_expr(arena, expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = resolve_types_unr_expr(arena, expr);
    }
    break;
    
    case eAstNode_id:
    {
      success = resolve_types_id(arena, expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = resolve_types_call(arena, expr);
    }
    break;
    
    case eAstNode_lit:
    {
      success = resolve_types_lit(arena, expr);
    }
    break;

    case eAstNode_basic_type:
    break;
    
    case eAstNode_index:
    {
      success = resolve_types_index(arena, expr);
    }
    break;

    case eAstNode_assign:
    {
      success = resolve_types_assign(arena, expr);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool resolve_types_block(MemoryArena* arena, AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = resolve_types_block_stmt(arena, stmt);
  }

  if(success)
  {
    success = resolve_types_of_node(arena, block);
  }

  return success;
}

bool resolve_types_return(MemoryArena* arena, AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;

  if(ret->ret.expr)
  {
    success = resolve_types_expr(arena, ret->ret.expr);
  }

  if(success)
  {
    success = resolve_types_of_node(arena, ret);
  }

  return success;
}

bool resolve_types_if(MemoryArena* arena, AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;

  if(success = resolve_types_expr(arena, if_->if_.cond_expr) && resolve_types_block_stmt(arena, if_->if_.body))
  {
    if(if_->if_.else_body)
    {
      success = resolve_types_block_stmt(arena, if_->if_.else_body);
    }

    if(success)
    {
      success = resolve_types_of_node(arena, if_);
    }
  }

  return success;
}

bool resolve_types_do_while(MemoryArena* arena, AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  success = resolve_types_block_stmt(arena, do_while->do_while.body) && resolve_types_expr(arena, do_while->do_while.cond_expr) &&
    resolve_types_of_node(arena, do_while);

  return success;
}

bool resolve_types_while(MemoryArena* arena, AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;

  success = resolve_types_expr(arena, while_->while_.cond_expr) && resolve_types_block_stmt(arena, while_->while_.body) &&
    resolve_types_of_node(arena, while_);

  return success;
}

bool resolve_types_array(MemoryArena* arena, AstNode* array)
{
  assert(KIND(array, eAstNode_array));
  bool success = true;
  
  success = resolve_types_expr(arena, array->array.size_expr) &&
    resolve_types_type(arena, array->array.elem_expr) && resolve_types_of_node(arena, array);

  return success;
}

bool resolve_types_pointer(MemoryArena* arena, AstNode* pointer)
{
  assert(KIND(pointer, eAstNode_pointer));
  bool success = true;
  
  success = resolve_types_expr(arena, pointer->pointer.pointee) && resolve_types_of_node(arena, pointer);

  return success;
}

bool resolve_types_type(MemoryArena* arena, AstNode* type)
{
  bool success = true;
  
  switch(type->kind)
  {
    case eAstNode_pointer:
    {
      success = resolve_types_pointer(arena, type);
    }
    break;

    case eAstNode_array:
    {
      success = resolve_types_array(arena, type);
    }
    break;

    case eAstNode_basic_type:
    break;
  }

  return success;
}

bool resolve_types_block_stmt(MemoryArena* arena, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_assign:
    {
      success = resolve_types_assign(arena, stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = resolve_types_cast(arena, stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    {
      success = resolve_types_expr(arena, stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = resolve_types_block(arena, stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = resolve_types_var(arena, stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = resolve_types_return(arena, stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = resolve_types_if(arena, stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = resolve_types_do_while(arena, stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = resolve_types_while(arena, stmt);
    }
    break;
    
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    break;
    
    case eAstNode_basic_type:
    {
      success = resolve_types_type(arena, stmt);
    }
    break;
    
    case eAstNode_index:
    {
      success = resolve_types_index(arena, stmt);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool resolve_types_proc(MemoryArena* arena, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  if(success = resolve_types_formal_args(arena, proc->proc.args) && resolve_types_type(arena, proc->proc.ret_type)
     && resolve_types_block_stmt(arena, proc->proc.body) && resolve_types_of_node(arena, proc))
  {
    proc->proc.decl_sym->ty = proc->eval_ty;
    proc->proc.retvar->ty = proc->proc.ret_type->eval_ty;
  }

  return success;
}

bool resolve_types_module_stmt(MemoryArena* arena, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_proc:
    {
      success = resolve_types_proc(arena, stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = resolve_types_var(arena, stmt);
    }
    case eAstNode_include:
    break;
    
    default: assert(0);
  }

  return success;
}

bool resolve_types_module(MemoryArena* arena, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = resolve_types_module_stmt(arena, stmt);
  }

  return success;
}

//          CHECK TYPES
//-----------------------------------------------------

bool check_types_expr(MemoryArena* arena, AstNode* expr);
bool check_types_block_stmt(MemoryArena* arena, AstNode* stmt);

bool check_types_var(MemoryArena* arena, AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  if(types_are_equal(var->eval_ty, basic_type_void))
  {
    success = compile_error(arena, var->src_loc, "type of var cannot be `void`");
  }
  else
  {
    if(var->var.init_expr)
    {
      success = check_types_expr(arena, var->var.init_expr);
    }
  }

  return success;
}

bool check_types_formal_args(MemoryArena* arena, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = check_types_var(arena, arg);
  }

  return success;
}

bool check_types_cast(MemoryArena* arena, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;
  
  AstNode* from_expr = cast->cast.from_expr;
  AstNode* to_type = cast->cast.to_type;
  
  if(success = check_types_expr(arena, from_expr))
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
        compile_error(arena, cast->src_loc, "invalid cast `%s` <- `%s`",
                      get_type_printstr(arena, to_ty), get_type_printstr(arena, from_ty));
      }
    }
  }

  return success;
}

bool check_types_bin_expr(MemoryArena* arena, AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;
  
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  eOperator op = bin_expr->bin_expr.op;
  
  if(success = check_types_expr(arena, left_operand) && check_types_expr(arena, right_operand))
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
          success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                  get_operator_printstr(op), get_type_printstr(arena, operands_ty));
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
          success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                  get_operator_printstr(op), get_type_printstr(arena, operands_ty));
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
          success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                  get_operator_printstr(op), get_type_printstr(arena, operands_ty));
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
        success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), get_type_printstr(arena, operands_ty));
      break;
      
      case eOperator_bit_shift_left:
      case eOperator_bit_shift_right:
      if(types_are_equal(left_ty, basic_type_int) && types_are_equal(right_ty, basic_type_char))
      {
        ;//ok
      }
      else
        success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), get_type_printstr(arena, operands_ty));
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
          success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                  get_operator_printstr(op), get_type_printstr(arena, operands_ty));
        }
      }
      break;
      
      default: assert(0);
    }
  }

  return success;
}

bool check_types_unr_expr(MemoryArena* arena, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;
  
  if(success = check_types_expr(arena, operand))
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
        success = compile_error(arena, unr_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), get_type_printstr(arena, operand_ty));
      break;
      
      case eOperator_bit_not:
      if(types_are_equal(operand_ty, basic_type_int))
      {
        ;//ok
      }
      else
        success = compile_error(arena, unr_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), get_type_printstr(arena, operand_ty));
      break;
      
      case eOperator_neg:
      if(types_are_equal(operand_ty, basic_type_int) ||
         types_are_equal(operand_ty, basic_type_float))
      {
        ;//ok
      }
      else
        success = compile_error(arena, unr_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), get_type_printstr(arena, operand_ty));
      break;
    }
  }

  return success;
}

bool check_types_actual_args(MemoryArena* arena, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = check_types_expr(arena, arg->call_arg.expr);
  }

  return success;
}

bool check_types_call(MemoryArena* arena, AstNode* call)
{
  assert(KIND(call, eAstNode_call));

  bool success = true;
  success = check_types_actual_args(arena, call->call.args);

  return success;
}

bool check_types_index(MemoryArena* arena, AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;
  
  if(index->eval_ty->width > 0)
  {
    ;//ok
  }
  else
    success = compile_error(arena, index->src_loc, "type error (array index): size of type = 0");
  
  return success;
}

bool check_types_assign(MemoryArena* arena, AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;
  
  AstNode* dest_expr = assign->assign.dest_expr;
  AstNode* source_expr = assign->assign.source_expr;

  if(success = check_types_expr(arena, dest_expr) && check_types_expr(arena, source_expr))
  {
    if(!type_unif(dest_expr->eval_ty, source_expr->eval_ty))
    {
      success = compile_error(arena, assign->src_loc, "type error (assignment)");
    }
  }

  return success;
}

bool check_types_expr(MemoryArena* arena, AstNode* expr)
{
  bool success = true;
  
  switch(expr->kind)
  {
    case eAstNode_assign:
    {
      success = check_types_assign(arena, expr);
    }
    break;
    
    case eAstNode_cast:
    {
      success = check_types_cast(arena, expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = check_types_bin_expr(arena, expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = check_types_unr_expr(arena, expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = check_types_call(arena, expr);
    }
    break;
    
    case eAstNode_id:
    case eAstNode_lit:
    case eAstNode_basic_type:
    break;
    
    case eAstNode_index:
    {
      success = check_types_index(arena, expr);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool check_types_return(MemoryArena* arena, AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;

  if(ret->ret.expr)
  {
    success = check_types_expr(arena, ret->ret.expr);
  }

  return success;
}

bool check_types_do_while(MemoryArena* arena, AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  success = check_types_block_stmt(arena, do_while->do_while.body)
    && check_types_expr(arena, do_while->do_while.cond_expr);

  return success;
}

bool check_types_while(MemoryArena* arena, AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;

  success = check_types_expr(arena, while_->while_.cond_expr)
    && check_types_block_stmt(arena, while_->while_.body);
  return success;
}

bool check_types_if(MemoryArena* arena, AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  
  success = check_types_expr(arena, if_->if_.cond_expr) && check_types_block_stmt(arena, if_->if_.body) &&
    (if_->if_.else_body ? check_types_block_stmt(arena, if_->if_.else_body) : true);

  return success;
}

bool check_types_block(MemoryArena* arena, AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = check_types_block_stmt(arena, stmt);
  }

  return success;
}

bool check_types_block_stmt(MemoryArena* arena, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_assign:
    {
      success = check_types_assign(arena, stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = check_types_cast(arena, stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    {
      success = check_types_expr(arena, stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = check_types_return(arena, stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = check_types_if(arena, stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = check_types_do_while(arena, stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = check_types_while(arena, stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = check_types_block(arena, stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = check_types_var(arena, stmt);
    }
    break;
    
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    break;
    
    case eAstNode_index:
    {
      success = check_types_index(arena, stmt);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool check_types_proc(MemoryArena* arena, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  success = check_types_formal_args(arena, proc->proc.args) && check_types_block_stmt(arena, proc->proc.body);

  return success;
}

bool check_types_module_stmt(MemoryArena* arena, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_proc:
    {
      success = check_types_proc(arena, stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = check_types_var(arena, stmt);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool check_types_module(MemoryArena* arena, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = check_types_module_stmt(arena, stmt);
  }

  return success;
}

