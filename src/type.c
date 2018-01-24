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

char* get_type_printstr(Type* type)
{
  String str; str_init(&str, arena);
  make_type_printstr(&str, type);

  return str_cap(&str);
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
  
  if(success = set_types_expr(array->array.size_expr) && set_types_type(array->array.elem_expr))
  {
    int size = 0;
    AstNode* size_expr = array->array.size_expr;
    if(size_expr->kind == eAstNode_lit && size_expr->lit.kind == eLiteral_int)
    {
      size = size_expr->lit.int_val;
      if(size < 0)
        success = compile_error(size_expr->src_loc, "array size must be greater than 0");
    }
    else
      success = compile_error(size_expr->src_loc, "array size must be an int literal");

    if(success)
    {
      array->array.ndim = 1;
      array->array.size = size;

      AstNode* elem_expr = array->array.elem_expr;
      if(elem_expr->kind == eAstNode_array)
      {
        array->array.ndim += elem_expr->array.ndim;
      }
      
      array->ty = array->eval_ty = new_array_type(array->array.size, array->array.ndim, elem_expr->ty);

      if(size == 0)
      {
        array->ty = array->eval_ty = new_pointer_type(elem_expr->ty);
      }
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

    case eLiteral_str:
    {
      ty = new_array_type(cstr_len(lit->lit.str_val)+1, 1, basic_type_char);
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
  success = eval_types_id(call->call.expr) && eval_types_actual_args(call->call.args);

  return success;
}

bool eval_types_index(AstNode* index)
{
  assert(KIND(index, eAstNode_index));

  bool success = true;
  success = eval_types_expr(index->index.array_expr) && eval_types_expr(index->index.i_expr);

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
    {
      success = compile_error(cond_expr->src_loc, "bool expression was expected");
    }
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
    get_type_width(node->ty);
    if(success = resolve_type(node->eval_ty, &node->eval_ty))
    {
      get_type_width(node->eval_ty);
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

  if(success = resolve_types_expr(unr_expr->unr_expr.operand))
  {
    AstNode* operand = unr_expr->unr_expr.operand;
    eOperator op = unr_expr->unr_expr.op;

    if(op == eOperator_address_of)
    {
      if(operand->eval_ty->kind == eType_array)
      {
        // ptr(array(T)) = ptr(T)
        Type* operand_ty = operand->eval_ty;
        success = type_unif(unr_expr->eval_ty, new_pointer_type(operand_ty->array.elem));
      }
      else
      {
        success = type_unif(unr_expr->eval_ty, new_pointer_type(operand->eval_ty));
      }

      if(!success)
      {
        compile_error(unr_expr->src_loc, "type error (unr expr)");
      }
    }
  }
  
  if(success)
  {
    success = resolve_types_of_node(unr_expr);
  }

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

  if(success = resolve_types_id(call->call.expr) && resolve_types_actual_args(call->call.args))
  {
    AstNode* args = call->call.args;
    for(ListItem* li = args->node_list.first;
        li;
        li = li->next)
    {
      AstNode* arg = KIND(li, eList_ast_node)->ast_node;
      arg->actual_arg.param->ty = arg->eval_ty;
    }

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

  if(success && (success = resolve_types_of_node(call)))
  {
    call->call.retvar->ty = call->eval_ty;
  }

  return success;
}

bool resolve_types_index(AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;

  AstNode* array_expr = index->index.array_expr;
  AstNode* i_expr = index->index.i_expr;

  if(success = resolve_types_expr(array_expr) && resolve_types_expr(i_expr))
  {
    if(type_unif(i_expr->eval_ty, basic_type_int))
    {
      Type* array_ty = array_expr->eval_ty;

      if(array_ty->kind == eType_array)
      {
        if(!type_unif(array_ty->array.elem, index->eval_ty))
        {
          success = compile_error(index->src_loc, "type error (index)");
        }
      }
      else if(array_ty->kind == eType_pointer)
      {
        if(!type_unif(array_ty->pointer.pointee, index->eval_ty))
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

  if(success)
  {
    success = resolve_types_of_node(index);
  }

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
    success = resolve_types_of_node(ret);
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

