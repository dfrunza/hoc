int set_type_width(Type* type)
{
  switch(type->kind)
  {
    case eType_array:
    {
      type->width = type->array.size * set_type_width(type->array.elem);
    }
    break;
    
    case eType_product:
    {
      type->width = set_type_width(type->product.left) + set_type_width(type->product.right);
    }
    break;
    
    case eType_proc:
    {
      type->width = set_type_width(type->proc.ret) + set_type_width(type->proc.args);
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
      type->width = set_type_width(type->var.type);
    }
    break;
    
    default: assert(0);
  }

  return type->width;
}

int maximum_of_int(int N, ...)
{
  assert(N > 0);

  va_list args;
  va_start(args, N);

  int result = va_arg(args, int);
  for(int i = 1; i < N; i++)
  {
    int next_int = va_arg(args, int);
    if(next_int > result)
    {
      result = next_int;
    }
  }

  va_end(args);
  return result;
}

Type* new_var_type(TypePass* pass, Type* var_type)
{
  Type* type = push_struct(pass->arena, Type);
  type->kind = eType_var;
  type->var.type = var_type;
  set_type_width(type);

  return type;
}

Type* new_basic_type(TypePass* pass, eBasicType kind)
{
  Type* type = push_struct(pass->arena, Type);
  type->kind = eType_basic;
  type->basic.kind = kind;
  set_type_width(type);

  return type;
}

Type* new_proc_type(TypePass* pass, Type* args, Type* ret)
{
  Type* type = push_struct(pass->arena, Type);
  type->kind = eType_proc;
  type->proc.args = args;
  type->proc.ret = ret;
  type->width = 0;

  return type;
}

Type* new_typevar(TypePass* pass)
{
  Type* type = push_struct(pass->arena, Type);
  type->kind = eType_typevar;
  type->typevar.id = pass->typevar_id++;
  type->width = 0;

  return type;
}

Type* new_product_type(TypePass* pass, Type* left, Type* right)
{
  Type* type = push_struct(pass->arena, Type);
  type->kind = eType_product;
  type->product.left = left;
  type->product.right = right;
  type->width = 0;

  return type;
}

Type* new_array_type(TypePass* pass, int size, Type* elem)
{
  Type* type = push_struct(pass->arena, Type);
  type->kind = eType_array;
  type->array.size = size;
  type->array.elem = elem;
  type->width = 0;

  return type;
}

Type* new_pointer_type(TypePass* pass, Type* pointee)
{
  Type* type = push_struct(pass->arena, Type);
  type->kind = eType_pointer;
  type->pointer.pointee = pointee;
  type->width = 0;

  return type;
}

TypePass* new_type_pass(MemoryArena* arena)
{
  int struct_size = maximum_of_int(2,
                                   sizeof(TypePass_Resolve),
                                   sizeof(TypePass_Check));

  TypePass* pass = (TypePass*)push_struct_(arena, struct_size, 1);

  pass->arena = arena;
  pass->basic_type_bool  = new_basic_type(pass, eBasicType_bool);
  pass->basic_type_int   = new_basic_type(pass, eBasicType_int);
  pass->basic_type_char  = new_basic_type(pass, eBasicType_char);
  pass->basic_type_float = new_basic_type(pass, eBasicType_float);
  pass->basic_type_void  = new_basic_type(pass, eBasicType_void);
  pass->basic_type_str   = new_array_type(pass, 0, pass->basic_type_char);
  pass->subst_list = list_new(arena, eList_type_pair);

  return pass;
}

bool type_eq(Type* type_A, Type* type_B)
{
  bool are_equal = false;
  
  if((type_A->kind != eType_typevar) && (type_B->kind == type_A->kind))
  {
    switch(type_A->kind)
    {
      case eType_basic:
      {
        are_equal = (type_A->basic.kind == type_B->basic.kind);
      }
      break;
      
      case eType_proc:
      {
        are_equal = type_eq(type_A->proc.args, type_B->proc.args)
          && type_eq(type_A->proc.ret, type_B->proc.ret);
      }
      break;
      
      case eType_pointer:
      {
        are_equal = type_eq(type_A->pointer.pointee, type_B->pointer.pointee);
      }
      break;
      
      case eType_product:
      {
        are_equal = type_eq(type_A->product.left, type_B->product.right);
      }
      break;
      
      case eType_array:
      {
        are_equal = type_eq(type_A->array.elem, type_B->array.elem);
      }
      break;
      
      case eType_var:
      {
        are_equal = type_eq(type_A->var.type, type_B->var.type);
      }
      break;
      
      default: assert(0);
    }
  }
  return are_equal;
}

Type* clone_type(Type* type, MemoryArena* arena)
{
  Type* clone = push_struct(arena, Type);
  *clone = *type;

  return clone;
}

Type* get_repr_type(Type* type)
{
  Type* result = type;
  while(result->repr_type)
  {
    result = result->repr_type;
  }

  return result;
}

void type_union(Type* type_A, Type* type_B)
{
  if(type_A->kind == eType_typevar)
  {
    type_A->repr_type = type_B;
  }
  else
  {
    type_B->repr_type = type_A;
  }
}

bool type_unif(Type* type_A, Type* type_B)
{
  bool success = false;
  Type* repr_type_A = get_repr_type(type_A);
  Type* repr_type_B = get_repr_type(type_B);
  
  if(repr_type_A == repr_type_B)
  {
    success = true;
  }
  else
  {
    if(repr_type_A->kind == eType_typevar || repr_type_B->kind == eType_typevar)
    {
      type_union(repr_type_A, repr_type_B);
      success = true;
    }
    else if(repr_type_A->kind == repr_type_B->kind)
    {
      if(repr_type_A == repr_type_B)
      {
        success = true;
      }
      else if(repr_type_A->kind == eType_basic)
      {
        success = (repr_type_A->basic.kind == repr_type_B->basic.kind);
      }
      else
      {
        type_union(repr_type_A, repr_type_B);
        assert(repr_type_A->kind == repr_type_B->kind);
        
        switch(repr_type_A->kind)
        {
          case eType_proc:
          {
            success = type_unif(repr_type_A->proc.args, repr_type_B->proc.args)
              && type_unif(repr_type_A->proc.ret, repr_type_B->proc.ret);
          }
          break;
          
          case eType_product:
          {
            success = type_unif(repr_type_A->product.left, repr_type_B->product.left)
              && type_unif(repr_type_A->product.right, repr_type_B->product.right);
          }
          break;
          
          case eType_pointer:
          {
            success = type_unif(repr_type_A->pointer.pointee, repr_type_B->pointer.pointee);
          }
          break;
          
          case eType_array:
          {
            success = type_unif(repr_type_A->array.elem, repr_type_B->array.elem);
          }
          break;
          
          case eType_var:
          {
            success = type_unif(repr_type_A->var.type, repr_type_B->var.type);
          }
          break;
          
          default: assert(0);
        }
      }
    }
  }
  
  return success;
}

struct TypePair
{
  Type* key;
  Type* value;
};

TypePair* new_type_pair(TypePass* pass, Type* key, Type* value)
{
  TypePair* pair = push_struct(pass->arena, TypePair);
  pair->key = key;
  pair->value = value;

  return pair;
}

TypePair* find_type_pair(TypePass* pass, Type* type)
{
  TypePair* result = 0;
  for(ListItem* li = pass->subst_list->first;
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

Type* type_subst(TypePass* pass, Type* type)
{
  type = get_repr_type(type);
  Type* subst = 0;
  
  TypePair* pair = find_type_pair(pass, type);
  if(pair)
  {
    subst = pair->value;
  }
  else
  {
    subst = clone_type(type, pass->arena);
    
    pair = new_type_pair(pass, type, subst);
    list_append(pass->subst_list, pair, eList_type_pair);
    
    switch(subst->kind)
    {
      case eType_typevar:
      {
        subst->typevar.id = pass->typevar_id++;
      }
      break;
      
      case eType_proc:
      {
        subst->proc.args = type_subst(pass, subst->proc.args);
        subst->proc.ret = type_subst(pass, subst->proc.ret);
      }
      break;
      
      case eType_product:
      {
        subst->product.left = type_subst(pass, subst->product.left);
        subst->product.right = type_subst(pass, subst->product.right);
      }
      break;
      
      case eType_pointer:
      {
        subst->pointer.pointee = type_subst(pass, subst->pointer.pointee);
      }
      break;
      
      case eType_array:
      {
        subst->array.elem = type_subst(pass, subst->array.elem);
      }
      break;
      
      case eType_var:
      {
        subst->var.type = type_subst(pass, subst->var.type);
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
      type = get_repr_type(type);
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
      success = resolve_type(type->proc.args, &type->proc.args) && resolve_type(type->proc.ret, &type->proc.ret);
    }
    break;
    
    case eType_product:
    {
      success = resolve_type(type->product.left, &type->product.left) && resolve_type(type->product.right, &type->product.right);
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

bool AstNode_resolve_types(AstNode* node, MemoryArena* arena)
{
  bool success = true;
  
  if(success = resolve_type(node->ty, &node->ty))
  {
    set_type_width(node->ty);
    if(success = resolve_type(node->eval_ty, &node->eval_ty))
    {
      set_type_width(node->eval_ty);
    }
    else
      success = compile_error(arena, node->src_loc, "type error (unresolved type)");
  }
  else
    success = compile_error(arena, node->src_loc, "type error (unresolved type)");
  
  return success;
}

void make_type_printstr(Type* type, String* str)
{
  switch(type->kind)
  {
    case eType_basic:
    {
      switch(type->basic.kind)
      {
        case eBasicType_bool:
          str_append(str, "bool");
        break;

        case eBasicType_int:
          str_append(str, "int");
        break;

        case eBasicType_float:
          str_append(str, "float");
        break;

        case eBasicType_char:
          str_append(str, "char");
        break;

        case eBasicType_void:
          str_append(str, "void");
        break;

        case eBasicType_auto:
          str_append(str, "auto");
        break;

        default: assert(0);
      }
    }
    break;

    case eType_pointer:
    {
      make_type_printstr(type->pointer.pointee, str);
      str_append(str, "^");
    }
    break;

    case eType_array:
    {
      str_append(str, "(");
      if(type->array.size >= 0)
        str_format(str, "[%d]", type->array.size);
      else
        str_append(str, "[]");
      make_type_printstr(type->array.elem, str);
      str_append(str, ")");
    }
    break;

    case eType_product:
    {
      make_type_printstr(type->product.left, str);
      str_append(str, ", ");
      make_type_printstr(type->product.right, str);
    }
    break;

    case eType_proc:
    {
      make_type_printstr(type->proc.ret, str);
      str_append(str, " (");
      make_type_printstr(type->proc.args, str);
      str_append(str, ")");
    }
    break;

    case eType_var:
    {
      make_type_printstr(type->var.type, str);
    }
    break;

    case eType_typevar:
    {
      str_format(str, "type_%d", type->typevar.id);
    }
    break;

    default: assert(0);
  }
}

char* get_type_printstr(Type* type, MemoryArena* arena)
{
  String str = {};
  str_init(&str, arena);
  make_type_printstr(type, &str);

  return str_cap(&str);
}

//     SET TYPES
//-----------------------------------------------------

bool SetTypePass_visit_expr(TypePass* pass, AstNode* expr);
bool SetTypePass_visit_type(TypePass* pass, AstNode* type);

bool SetTypePass_visit_array(TypePass* pass, AstNode* array)
{
  assert(KIND(array, eAstNode_array));
  bool success = true;
  
  if(success = SetTypePass_visit_expr(pass, array->array.size_expr) && SetTypePass_visit_type(pass, array->array.elem_expr))
  {
    int size = 0;
    AstNode* size_expr = array->array.size_expr;
    if(size_expr->kind == eAstNode_lit && size_expr->lit.kind == eLiteral_int)
    {
      size = size_expr->lit.int_val;
      if(size <= 0)
        success = compile_error(pass->arena, size_expr->src_loc, "array size must be greater than 0");
    }
    else
      success = compile_error(pass->arena, size_expr->src_loc, "array size must be an int literal");

    if(success)
    {
      array->array.ndim = 1;
      array->array.size = size;

      AstNode* elem_expr = array->array.elem_expr;
      if(elem_expr->kind == eAstNode_array)
      {
        array->array.ndim += elem_expr->array.ndim;
      }
      
      array->ty = array->eval_ty = new_array_type(pass, array->array.size, elem_expr->ty);
    }
  }

  return success;
}

bool SetTypePass_visit_pointer(TypePass* pass, AstNode* pointer)
{
  assert(KIND(pointer, eAstNode_pointer));
  bool success = true;
  
  AstNode* pointee = pointer->pointer.pointee;
  if(success = SetTypePass_visit_type(pass, pointee))
  {
    pointer->ty = pointer->eval_ty = new_pointer_type(pass, pointee->ty);
  }
  return success;
}

bool SetTypePass_visit_type(TypePass* pass, AstNode* type)
{
  bool success = true;
  
  switch(type->kind)
  {
    case eAstNode_pointer:
      success = SetTypePass_visit_pointer(pass, type);
    break;
    
    case eAstNode_array:
      success = SetTypePass_visit_array(pass, type);
    break;
    
    case eAstNode_basic_type:
    switch(type->basic_type.kind)
    {
      case eBasicType_int:
      {
        type->ty = type->eval_ty = pass->basic_type_int;
      }
      break;
      
      case eBasicType_float:
      {
        type->ty = type->eval_ty = pass->basic_type_float;
      }
      break;
      
      case eBasicType_bool:
      {
        type->ty = type->eval_ty = pass->basic_type_bool;
      }
      break;
      
      case eBasicType_char:
      {
        type->ty = type->eval_ty = pass->basic_type_char;
      }
      break;
      
      case eBasicType_void:
      {
        type->ty = type->eval_ty = pass->basic_type_void;
      }
      break;
      
      case eBasicType_auto:
      {
        type->ty = type->eval_ty = new_typevar(pass);
      }
      break;
      
      default: assert(0);
    }
    break;
    
    default:
    {
      success = compile_error(pass->arena, type->src_loc, "invalid type expression");
    }
  }

  return success;
}

bool SetTypePass_visit_var(TypePass* pass, AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  AstNode* type = var->var.type;
  if(success = SetTypePass_visit_type(pass, type))
  {
    var->ty = new_var_type(pass, type->ty);
    var->eval_ty = type->ty;

    if(var->var.init_expr)
    {
      success = SetTypePass_visit_expr(pass, var->var.init_expr);
    }
  }

  return success;
}

bool SetTypePass_visit_bin_expr(TypePass* pass, AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;
  
  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  
  if(success = SetTypePass_visit_expr(pass, left_operand) && SetTypePass_visit_expr(pass, right_operand))
  {
    bin_expr->eval_ty = new_typevar(pass);
    bin_expr->ty = new_proc_type(pass, new_product_type(pass, left_operand->eval_ty, right_operand->eval_ty), bin_expr->eval_ty);
  }

  return success;
}

bool SetTypePass_visit_unr_expr(TypePass* pass, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  if(success = SetTypePass_visit_expr(pass, operand))
  {
    unr_expr->eval_ty = new_typevar(pass);
    unr_expr->ty = new_proc_type(pass, operand->eval_ty, unr_expr->eval_ty);
  }

  return success;
}

bool SetTypePass_visit_actual_arg(TypePass* pass, AstNode* call_arg)
{
  assert(KIND(call_arg, eAstNode_call_arg));
  bool success = true;

  AstNode* expr = call_arg->call_arg.expr;
  if(success = SetTypePass_visit_expr(pass, expr))
  {
    call_arg->eval_ty = expr->eval_ty;
    call_arg->ty = expr->ty;
  }

  return success;
}

bool SetTypePass_visit_id(TypePass* pass, AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;

  id->ty = new_typevar(pass);
  id->eval_ty = new_typevar(pass);

  return success;
}

Type* make_args_product_type(TypePass* pass, AstNode* args)
{
  Type* result = pass->basic_type_void;

  ListItem* li = args->args.node_list.first;
  if(li)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    result = arg->eval_ty;
    for(li = li->next; li; li = li->next)
    {
      AstNode* next_arg = KIND(li, eList_ast_node)->ast_node;
      result = new_product_type(pass, result, next_arg->eval_ty);
    }
  }

  return result;
}

bool SetTypePass_visit_actual_args(TypePass* pass, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = SetTypePass_visit_expr(pass, arg);
  }

  if(success)
  {
    args->ty = args->eval_ty = make_args_product_type(pass, args);
  }

  return success;
}

bool SetTypePass_visit_call(TypePass* pass, AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  bool success = true;
  
  AstNode* call_expr = call->call.expr;
  AstNode* args = call->call.args;
  if(call_expr->kind == eAstNode_id)
  {
    if(success = SetTypePass_visit_id(pass, call_expr) && SetTypePass_visit_actual_args(pass, args))
    {
      call->eval_ty = new_typevar(pass);
      call->ty = new_proc_type(pass, args->ty, call->eval_ty);
    }
  }
  else
    success = compile_error(pass->arena, call->src_loc, "unsupported call expr");

  return success;
}

bool SetTypePass_visit_lit(TypePass* pass, AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));
  bool success = true;
  
  Type* ty = 0;
  switch(lit->lit.kind)
  {
    case eLiteral_int:
    {
      ty = pass->basic_type_int;
    }
    break;
    
    case eLiteral_float:
    {
      ty = pass->basic_type_float;
    }
    break;
    
    case eLiteral_char:
    {
      ty = pass->basic_type_char;
    }
    break;
    
    case eLiteral_bool:
    {
      ty = pass->basic_type_bool;
    }
    break;

    case eLiteral_str:
    {
      ty = new_array_type(pass, cstr_len(lit->lit.str_val)+1, pass->basic_type_char);
    }
    break;
    
    default: assert(0);
  }
  lit->ty = lit->eval_ty = ty;

  return success;
}

bool SetTypePass_visit_index(TypePass* pass, AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;
  
  if(success = SetTypePass_visit_expr(pass, index->index.array_expr) && SetTypePass_visit_expr(pass, index->index.i_expr))
  {
    index->ty = index->index.array_expr->eval_ty;
    index->eval_ty = new_typevar(pass);
  }

  return success;
}

bool SetTypePass_visit_cast(TypePass* pass, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;

  AstNode* to_type = cast->cast.to_type;
  AstNode* from_expr = cast->cast.from_expr;
  if(success = SetTypePass_visit_type(pass, to_type) && SetTypePass_visit_expr(pass, from_expr))
  {
    cast->eval_ty = to_type->eval_ty;
    cast->ty = new_product_type(pass, from_expr->eval_ty, cast->eval_ty);
  }

  return success;
}

bool SetTypePass_visit_assign(TypePass* pass, AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;
  
  AstNode* dest_expr = assign->assign.dest_expr;
  AstNode* source_expr =assign->assign.source_expr;
  if(success = SetTypePass_visit_expr(pass, dest_expr) && SetTypePass_visit_expr(pass, source_expr))
  {
    assign->ty = assign->eval_ty = dest_expr->eval_ty;
  }

  return success;
}

bool SetTypePass_visit_expr(TypePass* pass, AstNode* expr)
{
  bool success = true;
  
  switch(expr->kind)
  {
    case eAstNode_pointer:
    {
      success = SetTypePass_visit_pointer(pass, expr);
    }
    break;
    
    case eAstNode_array:
    {
      success = SetTypePass_visit_array(pass, expr);
    }
    break;
    
    case eAstNode_cast:
    {
      success = SetTypePass_visit_cast(pass, expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = SetTypePass_visit_bin_expr(pass, expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = SetTypePass_visit_unr_expr(pass, expr);
    }
    break;
    
    case eAstNode_id:
    {
      success = SetTypePass_visit_id(pass, expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = SetTypePass_visit_call(pass, expr);
    }
    break;
    
    case eAstNode_lit:
    {
      success = SetTypePass_visit_lit(pass, expr);
    }
    break;
    
    case eAstNode_basic_type:
    {
      success = SetTypePass_visit_type(pass, expr);
    }
    break;
    
    case eAstNode_index:
    {
      success = SetTypePass_visit_index(pass, expr);
    }
    break;

    case eAstNode_call_arg:
    {
      success = SetTypePass_visit_actual_arg(pass, expr);
    }
    break;

    case eAstNode_assign:
    {
      success = SetTypePass_visit_assign(pass, expr);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool SetTypePass_visit_return(TypePass* pass, AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;
  
  if(ret->ret.expr)
  {
    AstNode* ret_expr = ret->ret.expr;
    if(success = SetTypePass_visit_expr(pass, ret_expr))
    {
      ret->ty = ret_expr->ty;
      ret->eval_ty = ret_expr->eval_ty;
    }
  }
  else
  {
    ret->ty = ret->eval_ty = pass->basic_type_void;
  }

  return success;
}

bool SetTypePass_visit_block_stmt(TypePass* pass, AstNode* stmt);

bool SetTypePass_visit_if(TypePass* pass, AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  
  if(success = SetTypePass_visit_expr(pass, if_->if_.cond_expr))
  {
    AstNode* body = if_->if_.body;
    if(success = SetTypePass_visit_block_stmt(pass, body))
    {
      if_->ty = body->ty;
      if_->eval_ty = body->eval_ty;
      
      AstNode* else_body = if_->if_.else_body;
      if(else_body)
      {
        success = SetTypePass_visit_block_stmt(pass, else_body);
      }
    }
  }

  return success;
}

bool SetTypePass_visit_do_while(TypePass* pass, AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  AstNode* body = do_while->do_while.body;
  if(success = SetTypePass_visit_block_stmt(pass, body) && SetTypePass_visit_expr(pass, do_while->do_while.cond_expr))
  {
    do_while->ty = body->ty;
    do_while->eval_ty = body->eval_ty;
  }

  return success;
}

bool SetTypePass_visit_while(TypePass* pass, AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  
  AstNode* body = while_->while_.body;
  if(success = SetTypePass_visit_expr(pass, while_->while_.cond_expr) && SetTypePass_visit_block_stmt(pass, body))
  {
    while_->ty = body->ty;
    while_->eval_ty = body->eval_ty;
  }

  return success;
}

bool SetTypePass_visit_block(TypePass* pass, AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = SetTypePass_visit_block_stmt(pass, stmt);
  }

  if(success)
  {
    block->ty = block->eval_ty = pass->basic_type_void;
  }

  return success;
}

bool SetTypePass_visit_block_stmt(TypePass* pass, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_var:
    {
      success = SetTypePass_visit_var(pass, stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = SetTypePass_visit_block(pass, stmt);
    }
    break;
    
    case eAstNode_assign:
    {
      success = SetTypePass_visit_assign(pass, stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = SetTypePass_visit_cast(pass, stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    {
      success = SetTypePass_visit_expr(pass, stmt);
    }
    break;
    
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    {
      stmt->ty = stmt->eval_ty = pass->basic_type_void;
    }
    break;
    
    case eAstNode_basic_type:
    {
      success = SetTypePass_visit_type(pass, stmt);
    }
    break;
    case eAstNode_return:
    {
      success = SetTypePass_visit_return(pass, stmt);
    }
    break;

    case eAstNode_if:
    {
      success = SetTypePass_visit_if(pass, stmt);
    }
    break;

    case eAstNode_do_while:
    {
      success = SetTypePass_visit_do_while(pass, stmt);
    }
    break;

    case eAstNode_while:
    {
      success = SetTypePass_visit_while(pass, stmt);
    }
    break;

    case eAstNode_index:
    {
      success = SetTypePass_visit_index(pass, stmt);
    }
    break;

    default: assert(0);
  }

  return success;
}

bool SetTypePass_visit_formal_args(TypePass* pass, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = SetTypePass_visit_var(pass, arg);
  }

  if(success)
  {
    args->ty = args->eval_ty = make_args_product_type(pass, args);
  }

  return success;
}

bool SetTypePass_visit_proc(TypePass* pass, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  
  AstNode* ret_type = proc->proc.ret_type;
  AstNode* args = proc->proc.args;
  if(success = SetTypePass_visit_formal_args(pass, args) && SetTypePass_visit_type(pass, ret_type))
  {
    proc->ty = new_proc_type(pass, args->eval_ty, ret_type->eval_ty);
    proc->eval_ty = pass->basic_type_void;
    
    if(!is_extern_proc(proc))
    {
      success = SetTypePass_visit_block(pass, proc->proc.body);
    }
  }

  return success;
}

bool SetTypePass_visit_module_stmt(TypePass* pass, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_proc:
    {
      success = SetTypePass_visit_proc(pass, stmt);
    }
    break;

    case eAstNode_var:
    {
      success = SetTypePass_visit_var(pass, stmt);
    }
    break;

    case eAstNode_include:
    {
      stmt->ty = stmt->eval_ty = pass->basic_type_void;
    }
    break;

    default: assert(0);
  }

  return success;
}

bool SetTypePass_visit_module(TypePass* pass, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  module->ty = module->eval_ty = pass->basic_type_void;
  
  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = SetTypePass_visit_module_stmt(pass, stmt);
  }

  return success;
}

//       EVAL TYPES
//-----------------------------------------------------

bool EvalTypePass_visit_expr(TypePass* pass, AstNode* expr);
bool EvalTypePass_visit_type(TypePass* pass, AstNode* type);

bool EvalTypePass_visit_array(TypePass* pass, AstNode* array)
{
  assert(KIND(array, eAstNode_array));
  bool success = true;
  
  success = EvalTypePass_visit_expr(pass, array->array.size_expr) && EvalTypePass_visit_type(pass, array->array.elem_expr);
  return success;
}

bool EvalTypePass_visit_pointer(TypePass* pass, AstNode* pointer)
{
  assert(KIND(pointer, eAstNode_pointer));

  bool success = true;
  success = EvalTypePass_visit_expr(pass, pointer->pointer.pointee);

  return success;
}

bool EvalTypePass_visit_type(TypePass* pass, AstNode* type)
{
  bool success = true;
  
  switch(type->kind)
  {
    case eAstNode_pointer:
    {
      success = EvalTypePass_visit_pointer(pass, type);
    }
    break;

    case eAstNode_array:
    {
      success = EvalTypePass_visit_array(pass, type);
    }
    break;

    case eAstNode_basic_type:
    break;
  }

  return success;
}

bool EvalTypePass_visit_cast(TypePass* pass, AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));

  bool success = true;
  success = EvalTypePass_visit_type(pass, cast->cast.to_type) && EvalTypePass_visit_expr(pass, cast->cast.from_expr);

  return success;
}

bool EvalTypePass_visit_bin_expr(TypePass* pass, AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;
  
  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  eOperator op = bin_expr->bin_expr.op;
  
  if(success = EvalTypePass_visit_expr(pass, left_operand) && EvalTypePass_visit_expr(pass, right_operand))
  {
    switch(op)
    {
      case eOperator_bit_and:
      case eOperator_bit_or:
      case eOperator_bit_xor:
      {
        if(type_unif(left_operand->eval_ty, pass->basic_type_int)
           && type_unif(right_operand->eval_ty, pass->basic_type_int)
           && type_unif(bin_expr->eval_ty, pass->basic_type_int))
        {
          ;//ok
        }
        else
          success = compile_error(pass->arena, bin_expr->src_loc, "type error (bitwise op)");
      }
      break;
      
      case eOperator_bit_shift_left:
      case eOperator_bit_shift_right:
      {
        if(type_unif(left_operand->eval_ty, pass->basic_type_int)
           && type_unif(right_operand->eval_ty, pass->basic_type_char)
           && type_unif(bin_expr->eval_ty, pass->basic_type_int))
        {
          ;//ok
        }
        else
          success = compile_error(pass->arena, bin_expr->src_loc, "type error (bitwise op)");
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
              if(!type_unif(bin_expr->eval_ty, pass->basic_type_bool))
              {
                success = compile_error(pass->arena, bin_expr->src_loc, "type error (bin expr)");
              }
            }
            break;

            default:
            {
              if(!type_unif(bin_expr->eval_ty, left_operand->eval_ty))
              {
                success = compile_error(pass->arena, bin_expr->src_loc, "type error (bin expr)");
              }
            }
            break;
          }
        }
        else
          success = compile_error(pass->arena, bin_expr->src_loc, "type error (bin expr)");
      }
      break;
    }

    if(success)
    {
      Type* bin_expr_ty = KIND(bin_expr->ty, eType_proc);
      if(!type_unif(bin_expr_ty->proc.ret, bin_expr->eval_ty))
      {
        success = compile_error(pass->arena, bin_expr->src_loc, "type error (bin expr)");
      }
    }
  }

  return success;
}

bool EvalTypePass_visit_id(TypePass* pass, AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;
  
  if(!id->id.decl_sym)
  {
    assert(!id->id.decl_ast);
    id->id.decl_sym = lookup_decl_symbol(id->id.scope, id->id.name);
    if(id->id.decl_sym)
    {
      id->id.decl_ast = id->id.decl_sym->ast_node;
    }
    else
      success = compile_error(pass->arena, id->src_loc, "unknown id `%s`", id->id.name);
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
              success = compile_error(pass->arena, id->src_loc, "var `%s` must be declared before its use", id->id.name);
            }
            else
            {
              if(!type_unif(decl_ast->ty->var.type, id->eval_ty))
              {
                success = compile_error(pass->arena, id->src_loc, "type error (var id)");
              }
            }
          }
        break;
        
        case eType_proc:
        {
          if(!type_unif(decl_ast->ty->proc.ret, id->eval_ty))
          {
            success = compile_error(pass->arena, id->src_loc, "type error (proc id)");
          }
        }
        break;

        default: assert(0);
      }
    }
    else
      success = compile_error(pass->arena, id->src_loc, "type error (id)");
  }

  return success;
}

bool EvalTypePass_visit_unr_expr(TypePass* pass, AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;
  
  if(success = EvalTypePass_visit_expr(pass, operand))
  {
    switch(op)
    {
      case eOperator_neg:
      case eOperator_logic_not:
      case eOperator_bit_not:
      if(!type_unif(unr_expr->eval_ty, operand->eval_ty))
      {
        success = compile_error(pass->arena, unr_expr->src_loc, "type error (unr expr)");
      }
      break;
      
      case eOperator_deref:
      {
        Type* pointee_ty = new_typevar(pass);
        if(type_unif(operand->eval_ty, new_pointer_type(pass, pointee_ty)))
        {
          if(!type_unif(unr_expr->eval_ty, pointee_ty))
          {
            success = compile_error(pass->arena, unr_expr->src_loc, "type error (unr expr)");
          }
        }
        else
          success = compile_error(pass->arena, operand->src_loc, "pointer type expected");
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
        success = compile_error(pass->arena, unr_expr->src_loc, "type error (unr expr)");
      }
    }
  }

  return success;
}

bool EvalTypePass_visit_var(TypePass* pass, AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  if(type_unif(var->ty->var.type, var->eval_ty))
  {
    AstNode* init_expr = var->var.init_expr;
    if(init_expr)
    {
      if(success = EvalTypePass_visit_expr(pass, init_expr))
      {
        if(!type_unif(var->eval_ty, init_expr->eval_ty))
        {
          success = compile_error(pass->arena, var->src_loc, "type error (var init expr)");
        }
      }
    }
  }
  else
  {
    success = compile_error(pass->arena, var->src_loc, "type error (var)");
  }

  return success;
}

bool EvalTypePass_visit_formal_args(TypePass* pass, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = EvalTypePass_visit_var(pass, arg);
  }

  return success;
}

bool EvalTypePass_visit_actual_args(TypePass* pass, AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = EvalTypePass_visit_expr(pass, arg->call_arg.expr);
  }

  return success;
}

bool EvalTypePass_visit_call(TypePass* pass, AstNode* call)
{
  assert(KIND(call, eAstNode_call));

  bool success = true;
  success = EvalTypePass_visit_id(pass, call->call.expr) && EvalTypePass_visit_actual_args(pass, call->call.args);

  return success;
}

bool EvalTypePass_visit_index(TypePass* pass, AstNode* index)
{
  assert(KIND(index, eAstNode_index));

  bool success = true;
  success = EvalTypePass_visit_expr(pass, index->index.array_expr) && EvalTypePass_visit_expr(pass, index->index.i_expr);

  return success;
}

bool EvalTypePass_visit_assign(TypePass* pass, AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));

  bool success = true;
  success = EvalTypePass_visit_expr(pass, assign->assign.dest_expr) && EvalTypePass_visit_expr(pass, assign->assign.source_expr);

  return success;
}

bool EvalTypePass_visit_expr(TypePass* pass, AstNode* expr)
{
  bool success = true;
  
  switch(expr->kind)
  {
    case eAstNode_cast:
    {
      success = EvalTypePass_visit_cast(pass, expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = EvalTypePass_visit_bin_expr(pass, expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = EvalTypePass_visit_unr_expr(pass, expr);
    }
    break;
    
    case eAstNode_id:
    {
      success = EvalTypePass_visit_id(pass, expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = EvalTypePass_visit_call(pass, expr);
    }
    break;
    
    case eAstNode_index:
    {
      success = EvalTypePass_visit_index(pass, expr);
    }
    break;
    
    case eAstNode_lit:
    break;

    case eAstNode_basic_type:
    case eAstNode_pointer:
    case eAstNode_array:
    {
      success = EvalTypePass_visit_type(pass, expr);
    }
    break;

    case eAstNode_assign:
    {
      success = EvalTypePass_visit_assign(pass, expr);
    }
    break;
    
    default: assert(0);
  }
  return success;
}

bool EvalTypePass_visit_block_stmt(TypePass* pass, AstNode* stmt);

bool EvalTypePass_visit_if(TypePass* pass, AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  
  AstNode* cond_expr = if_->if_.cond_expr;
  AstNode* body = if_->if_.body;
  AstNode* else_body = if_->if_.else_body;
  
  if(success = EvalTypePass_visit_expr(pass, cond_expr) &&
     EvalTypePass_visit_block_stmt(pass, body) &&
     (else_body ? EvalTypePass_visit_block_stmt(pass, else_body) : true))
  {
    if(!type_unif(cond_expr->eval_ty, pass->basic_type_bool))
    {
      success = compile_error(pass->arena, cond_expr->src_loc, "bool expression was expected");
    }
  }

  return success;
}

bool EvalTypePass_visit_block(TypePass* pass, AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = EvalTypePass_visit_block_stmt(pass, stmt);
  }

  return success;
}

bool EvalTypePass_visit_do_while(TypePass* pass, AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  AstNode* cond_expr = do_while->do_while.cond_expr;
  if(success = EvalTypePass_visit_block_stmt(pass, do_while->do_while.body) && EvalTypePass_visit_expr(pass, cond_expr))
  {
    if(!type_unif(cond_expr->eval_ty, pass->basic_type_bool))
    {
      success = compile_error(pass->arena, cond_expr->src_loc, "bool expression was expected");
    }
  }
  return success;
}

bool EvalTypePass_visit_while(TypePass* pass, AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  
  AstNode* cond_expr = while_->while_.cond_expr;
  if(success = EvalTypePass_visit_expr(pass, cond_expr) && EvalTypePass_visit_block_stmt(pass, while_->while_.body))
  {
    if(!type_unif(cond_expr->eval_ty, pass->basic_type_bool))
    {
      success = compile_error(pass->arena, cond_expr->src_loc, "bool expression was expected");
    }
  }

  return success;
}

bool EvalTypePass_visit_return(TypePass* pass, AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;
  
  AstNode* ret_expr = ret->ret.expr;
  if(ret_expr && (success = EvalTypePass_visit_expr(pass, ret_expr)))
  {
    AstNode* proc = ret->ret.proc;
    Type* proc_ty = KIND(proc->ty, eType_proc);
    if(!type_unif(ret_expr->eval_ty, proc_ty->proc.ret))
    {
      success = compile_error(pass->arena, ret->src_loc, "type error (return)");
    }
  }

  return success;
}

bool EvalTypePass_visit_block_stmt(TypePass* pass, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_assign:
    {
      success = EvalTypePass_visit_assign(pass, stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = EvalTypePass_visit_cast(pass, stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    {
      success = EvalTypePass_visit_expr(pass, stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = EvalTypePass_visit_if(pass, stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = EvalTypePass_visit_do_while(pass, stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = EvalTypePass_visit_while(pass, stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = EvalTypePass_visit_block(pass, stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = EvalTypePass_visit_return(pass, stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = EvalTypePass_visit_var(pass, stmt);
    }
    break;

    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    break;
    
    case eAstNode_basic_type:
    {
      success = EvalTypePass_visit_type(pass, stmt);
    }
    break;
    
    case eAstNode_index:
    {
      success = EvalTypePass_visit_index(pass, stmt);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool EvalTypePass_visit_proc(TypePass* pass, AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  success = EvalTypePass_visit_formal_args(pass, proc->proc.args) && EvalTypePass_visit_block_stmt(pass, proc->proc.body)
    && EvalTypePass_visit_type(pass, proc->proc.ret_type);

  return success;
}

bool EvalTypePass_visit_module_stmt(TypePass* pass, AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_proc:
    {
      success = EvalTypePass_visit_proc(pass, stmt);
    }
    break;

    case eAstNode_var:
    case eAstNode_include:
    break;

    default: assert(0);
  }
  return success;
}

bool EvalTypePass_visit_module(TypePass* pass, AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = EvalTypePass_visit_module_stmt(pass, stmt);
  }

  return success;
}

//       RESOLVE TYPES
//-----------------------------------------------------

bool TypePass_Resolve::visit_var(AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  if(success = AstNode_resolve_types(var, arena))
  {
    var->var.decl_sym->ty = var->eval_ty;

    if(var->var.init_expr)
    {
      success = visit_expr(var->var.init_expr);
    }

    Symbol* object = var->var.decl_sym;
    assert(object->ty->width >= 0);

    if(object->ty->width == 0)
    {
      success = compile_error(arena, var->src_loc, "variable of 0 size");
    }
  }

  return success;
}

bool TypePass_Resolve::visit_lit(AstNode* lit)
{
  assert(KIND(lit, eAstNode_lit));
  bool success = true;

  if(success = AstNode_resolve_types(lit, arena))
  {
    lit->lit.constant->ty = lit->eval_ty;
  }

  return success;
}

bool TypePass_Resolve::visit_formal_args(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = visit_var(arg);
  }
  if(success)
  {
    success = AstNode_resolve_types(args, arena);
  }
  return success;
}

bool TypePass_Resolve::visit_bin_expr(AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;

  success = visit_expr(bin_expr->bin_expr.left_operand) &&
    visit_expr(bin_expr->bin_expr.right_operand) && AstNode_resolve_types(bin_expr, arena);

  return success;
}

bool TypePass_Resolve::visit_unr_expr(AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;

  if(success = visit_expr(unr_expr->unr_expr.operand))
  {
    AstNode* operand = unr_expr->unr_expr.operand;
    eOperator op = unr_expr->unr_expr.op;

    if(op == eOperator_address_of)
    {
      if(operand->eval_ty->kind == eType_array)
      {
        // ptr(array(T)) = ptr(T)
        Type* operand_ty = operand->eval_ty;
        success = type_unif(unr_expr->eval_ty, new_pointer_type(this, operand_ty->array.elem));
      }
      else
      {
        success = type_unif(unr_expr->eval_ty, new_pointer_type(this, operand->eval_ty));
      }

      if(!success)
      {
        compile_error(arena, unr_expr->src_loc, "type error (unr expr)");
      }
    }
  }
  
  if(success)
  {
    success = AstNode_resolve_types(unr_expr, arena);
  }

  return success;
}

bool TypePass_Resolve::visit_id(AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;

  success = AstNode_resolve_types(id, arena);
  return success;
}

bool TypePass_Resolve::visit_actual_args(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    if(success = visit_expr(arg->call_arg.expr))
    {
      arg->eval_ty = arg->call_arg.expr->eval_ty;
    }
  }

  if(success)
  {
    success = AstNode_resolve_types(args, arena);
  }

  return success;
}

bool TypePass_Resolve::visit_call(AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  assert(call->call.expr->kind == eAstNode_id);

  bool success = true;

  if(success = visit_id(call->call.expr) && visit_actual_args(call->call.args))
  {
    AstNode* args = call->call.args;
    assert(KIND(args, eAstNode_node_list));
    for(ListItem* li = args->args.node_list.first;
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

  if(success && (success = AstNode_resolve_types(call, arena)))
  {
    call->call.retvar->ty = call->eval_ty;
  }

  return success;
}

bool TypePass_Resolve::visit_index(AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;

  AstNode* array_expr = index->index.array_expr;
  AstNode* i_expr = index->index.i_expr;

  if(success = visit_expr(array_expr) && visit_expr(i_expr))
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
    success = AstNode_resolve_types(index, arena);
  }

  return success;
}

bool TypePass_Resolve::visit_cast(AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;

  success = visit_type(cast->cast.to_type) && visit_expr(cast->cast.from_expr) &&
    AstNode_resolve_types(cast, arena);

  return success;
}

bool TypePass_Resolve::visit_assign(AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;

  success = visit_expr(assign->assign.dest_expr) && visit_expr(assign->assign.source_expr) &&
    AstNode_resolve_types(assign, arena);

  return success;
}

bool TypePass_Resolve::visit_expr(AstNode* expr)
{
  bool success = true;
  
  switch(expr->kind)
  {
    case eAstNode_cast:
    {
      success = visit_cast(expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = visit_bin_expr(expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = visit_unr_expr(expr);
    }
    break;
    
    case eAstNode_id:
    {
      success = visit_id(expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = visit_call(expr);
    }
    break;
    
    case eAstNode_lit:
    {
      success = visit_lit(expr);
    }
    break;

    case eAstNode_basic_type:
    case eAstNode_pointer:
    case eAstNode_array:
    {
      success = visit_type(expr);
    }
    break;
    
    case eAstNode_index:
    {
      success = visit_index(expr);
    }
    break;

    case eAstNode_assign:
    {
      success = visit_assign(expr);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool TypePass_Resolve::visit_block(AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = visit_block_stmt(stmt);
  }

  if(success)
  {
    success = AstNode_resolve_types(block, arena);
  }

  return success;
}

bool TypePass_Resolve::visit_return(AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;

  if(ret->ret.expr)
  {
    success = visit_expr(ret->ret.expr);
  }

  if(success)
  {
    success = AstNode_resolve_types(ret, arena);
  }

  return success;
}

bool TypePass_Resolve::visit_if(AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;

  if(success = visit_expr(if_->if_.cond_expr) && visit_block_stmt(if_->if_.body))
  {
    if(if_->if_.else_body)
    {
      success = visit_block_stmt(if_->if_.else_body);
    }

    if(success)
    {
      success = AstNode_resolve_types(if_, arena);
    }
  }

  return success;
}

bool TypePass_Resolve::visit_do_while(AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  success = visit_block_stmt(do_while->do_while.body) && visit_expr(do_while->do_while.cond_expr) &&
    AstNode_resolve_types(do_while, arena);

  return success;
}

bool TypePass_Resolve::visit_while(AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;

  success = visit_expr(while_->while_.cond_expr) && visit_block_stmt(while_->while_.body) &&
    AstNode_resolve_types(while_, arena);

  return success;
}

bool TypePass_Resolve::visit_array(AstNode* array)
{
  assert(KIND(array, eAstNode_array));
  bool success = true;
  
  success = visit_expr(array->array.size_expr) &&
    visit_type(array->array.elem_expr) && AstNode_resolve_types(array, arena);

  return success;
}

bool TypePass_Resolve::visit_pointer(AstNode* pointer)
{
  assert(KIND(pointer, eAstNode_pointer));
  bool success = true;
  
  success = visit_expr(pointer->pointer.pointee) && AstNode_resolve_types(pointer, arena);

  return success;
}

bool TypePass_Resolve::visit_type(AstNode* type)
{
  bool success = true;
  
  switch(type->kind)
  {
    case eAstNode_pointer:
    {
      success = visit_pointer(type);
    }
    break;

    case eAstNode_array:
    {
      success = visit_array(type);
    }
    break;

    case eAstNode_basic_type:
    break;
  }

  return success;
}

bool TypePass_Resolve::visit_block_stmt(AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_assign:
    {
      success = visit_assign(stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = visit_cast(stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    {
      success = visit_expr(stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = visit_block(stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = visit_var(stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = visit_return(stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = visit_if(stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = visit_do_while(stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = visit_while(stmt);
    }
    break;
    
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    break;
    
    case eAstNode_basic_type:
    {
      success = visit_type(stmt);
    }
    break;
    
    case eAstNode_index:
    {
      success = visit_index(stmt);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool TypePass_Resolve::visit_proc(AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  if(success = visit_formal_args(proc->proc.args) && visit_type(proc->proc.ret_type)
     && visit_block_stmt(proc->proc.body) && AstNode_resolve_types(proc, arena))
  {
    proc->proc.decl_sym->ty = proc->eval_ty;
    proc->proc.retvar->ty = proc->proc.ret_type->eval_ty;
  }

  return success;
}

bool TypePass_Resolve::visit_module_stmt(AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_proc:
    {
      success = visit_proc(stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = visit_var(stmt);
    }
    case eAstNode_include:
    break;
    
    default: assert(0);
  }

  return success;
}

bool TypePass_Resolve::visit_module(AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = visit_module_stmt(stmt);
  }

  return success;
}

//          CHECK TYPES
//-----------------------------------------------------

bool TypePass_Check::visit_var(AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  if(type_eq(var->eval_ty, basic_type_void))
  {
    success = compile_error(arena, var->src_loc, "type of var cannot be `void`");
  }
  else
  {
    if(var->var.init_expr)
    {
      success = visit_expr(var->var.init_expr);
    }
  }

  return success;
}

bool TypePass_Check::visit_formal_args(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = visit_var(arg);
  }

  return success;
}

bool TypePass_Check::visit_cast(AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;
  
  AstNode* from_expr = cast->cast.from_expr;
  AstNode* to_type = cast->cast.to_type;
  
  if(success = visit_expr(from_expr))
  {
    Type* from_ty = from_expr->eval_ty;
    Type* to_ty = to_type->eval_ty;
    
    if(!type_eq(from_ty, to_ty))
    {
      success = false;
      
      if(type_eq(to_ty, basic_type_int))
      {
        // float | bool | pointer(T) | char -> int 
        success = type_eq(from_ty, basic_type_float) ||
          type_eq(from_ty, basic_type_bool) ||
          type_eq(from_ty, basic_type_char) ||
          (from_ty->kind == eType_pointer);
      }
      else if(type_eq(to_ty, basic_type_char))
      {
        // int -> char 
        success = type_eq(from_ty, basic_type_int);
      }
      else if(type_eq(to_ty, basic_type_float))
      {
        // int -> float 
        success = type_eq(from_ty, basic_type_int);
      }
      else if(type_eq(to_ty, basic_type_bool))
      {
        // int | pointer(T) -> bool
        success = type_eq(from_ty, basic_type_int) ||
          (from_ty->kind == eType_pointer);
      }
      else if(to_ty->kind == eType_pointer)
      {
        // pointer(P) | int -> pointer(T)
        success = (from_ty->kind == eType_pointer) ||
          type_eq(from_ty, basic_type_int);
      }
      if(!success)
      {
        compile_error(arena, cast->src_loc, "invalid cast `%s` -> `%s`",
                      get_type_printstr(from_ty, arena), get_type_printstr(to_ty, arena));
      }
    }
  }

  return success;
}

bool TypePass_Check::visit_bin_expr(AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;
  
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  eOperator op = bin_expr->bin_expr.op;
  
  if(success = visit_expr(left_operand) && visit_expr(right_operand))
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
        if(type_eq(ret_ty, basic_type_int)
           || type_eq(ret_ty, basic_type_float)
           || type_eq(ret_ty, basic_type_char)
           || (ret_ty->kind == eType_pointer))
        {
          ;//ok
          assert(type_eq(ret_ty, left_ty) && type_eq(left_ty, right_ty));
        }
        else
        {
          success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                  get_operator_printstr(op), get_type_printstr(operands_ty, arena));
        }
      }
      break;
      
      case eOperator_mod:
      {
        if(type_eq(ret_ty, basic_type_int))
        {
          ;//ok
          assert(type_eq(ret_ty, left_ty) && type_eq(left_ty, right_ty));
        }
        else
        {
          success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                  get_operator_printstr(op), get_type_printstr(operands_ty, arena));
        }
      }
      break;
      
      case eOperator_logic_and:
      case eOperator_logic_or:
      {
        if(type_eq(left_ty, basic_type_bool) && type_eq(left_ty, right_ty))
        {
          ;//ok
          assert(ret_ty == basic_type_bool);
        }
        else
          success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                  get_operator_printstr(op), get_type_printstr(operands_ty, arena));
      }
      break;
      
      case eOperator_bit_and:
      case eOperator_bit_or:
      case eOperator_bit_xor:
      if(type_eq(left_ty, basic_type_int) && type_eq(right_ty, basic_type_int))
      {
        ;//ok
      }
      else
        success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), get_type_printstr(operands_ty, arena));
      break;
      
      case eOperator_bit_shift_left:
      case eOperator_bit_shift_right:
      if(type_eq(left_ty, basic_type_int) && type_eq(right_ty, basic_type_char))
      {
        ;//ok
      }
      else
        success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), get_type_printstr(operands_ty, arena));
      break;
      
      case eOperator_less:
      case eOperator_less_eq:
      case eOperator_greater:
      case eOperator_greater_eq:
      case eOperator_eq:
      case eOperator_not_eq:
      {
        if((type_eq(left_ty, basic_type_int) ||
            type_eq(left_ty, basic_type_char) ||
            type_eq(left_ty, basic_type_float) ||
            (left_ty->kind == eType_pointer)) &&
           type_eq(left_ty, right_ty))
        {
          ;//ok
          assert(type_eq(ret_ty, basic_type_bool));
        }
        else
        {
          success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                  get_operator_printstr(op), get_type_printstr(operands_ty, arena));
        }
      }
      break;
      
      default: assert(0);
    }
  }

  return success;
}

bool TypePass_Check::visit_unr_expr(AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;
  
  if(success = visit_expr(operand))
  {
    Type* expr_ty = KIND(unr_expr->ty, eType_proc);
    Type* operand_ty = expr_ty->proc.args;
    Type* ret_ty = expr_ty->proc.ret;
    
    switch(op)
    {
      case eOperator_logic_not:
      if(type_eq(operand_ty, basic_type_bool))
      {
        ;//ok
        assert(ret_ty == basic_type_bool);
      }
      else
        success = compile_error(arena, unr_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), get_type_printstr(operand_ty, arena));
      break;
      
      case eOperator_bit_not:
      if(type_eq(operand_ty, basic_type_int))
      {
        ;//ok
      }
      else
        success = compile_error(arena, unr_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), get_type_printstr(operand_ty, arena));
      break;
      
      case eOperator_neg:
      if(type_eq(operand_ty, basic_type_int) || type_eq(operand_ty, basic_type_float))
      {
        ;//ok
      }
      else
        success = compile_error(arena, unr_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), get_type_printstr(operand_ty, arena));
      break;
    }
  }

  return success;
}

bool TypePass_Check::visit_actual_args(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = visit_expr(arg->call_arg.expr);
  }

  return success;
}

bool TypePass_Check::visit_call(AstNode* call)
{
  assert(KIND(call, eAstNode_call));

  bool success = true;
  success = visit_actual_args(call->call.args);

  return success;
}

bool TypePass_Check::visit_index(AstNode* index)
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

bool TypePass_Check::visit_assign(AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;
  
  AstNode* dest_expr = assign->assign.dest_expr;
  AstNode* source_expr = assign->assign.source_expr;

  if(success = visit_expr(dest_expr) && visit_expr(source_expr))
  {
    if(!type_unif(dest_expr->eval_ty, source_expr->eval_ty))
    {
      success = compile_error(arena, assign->src_loc, "type error (assignment)");
    }
  }

  return success;
}

bool TypePass_Check::visit_expr(AstNode* expr)
{
  bool success = true;
  
  switch(expr->kind)
  {
    case eAstNode_assign:
    {
      success = visit_assign(expr);
    }
    break;
    
    case eAstNode_cast:
    {
      success = visit_cast(expr);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      success = visit_bin_expr(expr);
    }
    break;
    
    case eAstNode_unr_expr:
    {
      success = visit_unr_expr(expr);
    }
    break;
    
    case eAstNode_call:
    {
      success = visit_call(expr);
    }
    break;
    
    case eAstNode_id:
    case eAstNode_lit:
    case eAstNode_basic_type:
    break;
    
    case eAstNode_index:
    {
      success = visit_index(expr);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool TypePass_Check::visit_return(AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;

  if(ret->ret.expr)
  {
    success = visit_expr(ret->ret.expr);
  }

  return success;
}

bool TypePass_Check::visit_do_while(AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  success = visit_block_stmt(do_while->do_while.body)
    && visit_expr(do_while->do_while.cond_expr);

  return success;
}

bool TypePass_Check::visit_while(AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;

  success = visit_expr(while_->while_.cond_expr)
    && visit_block_stmt(while_->while_.body);
  return success;
}

bool TypePass_Check::visit_if(AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  
  success = visit_expr(if_->if_.cond_expr) && visit_block_stmt(if_->if_.body) &&
    (if_->if_.else_body ? visit_block_stmt(if_->if_.else_body) : true);

  return success;
}

bool TypePass_Check::visit_block(AstNode* block)
{
  assert(KIND(block, eAstNode_block));
  bool success = true;
  
  for(ListItem* li = block->block.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = visit_block_stmt(stmt);
  }

  return success;
}

bool TypePass_Check::visit_block_stmt(AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_assign:
    {
      success = visit_assign(stmt);
    }
    break;
    
    case eAstNode_cast:
    {
      success = visit_cast(stmt);
    }
    break;
    
    case eAstNode_bin_expr:
    case eAstNode_unr_expr:
    case eAstNode_id:
    case eAstNode_call:
    case eAstNode_lit:
    {
      success = visit_expr(stmt);
    }
    break;
    
    case eAstNode_return:
    {
      success = visit_return(stmt);
    }
    break;
    
    case eAstNode_if:
    {
      success = visit_if(stmt);
    }
    break;
    
    case eAstNode_do_while:
    {
      success = visit_do_while(stmt);
    }
    break;
    
    case eAstNode_while:
    {
      success = visit_while(stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = visit_block(stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = visit_var(stmt);
    }
    break;
    
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    break;
    
    case eAstNode_index:
    {
      success = visit_index(stmt);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool TypePass_Check::visit_proc(AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  success = visit_formal_args(proc->proc.args) && visit_block_stmt(proc->proc.body);

  return success;
}

bool TypePass_Check::visit_module_stmt(AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_proc:
    {
      success = visit_proc(stmt);
    }
    break;
    
    case eAstNode_var:
    {
      success = visit_var(stmt);
    }
    break;
    
    default: assert(0);
  }

  return success;
}

bool TypePass_Check::visit_module(AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = visit_module_stmt(stmt);
  }

  return success;
}

bool run_type_pass(TypePass* pass, AstNode* module)
{
  bool success = true;

  TypePass_Resolve* resolve = (TypePass_Resolve*)pass;
  TypePass_Check* check = (TypePass_Check*)pass;

  success = SetTypePass_visit_module(pass, module) && EvalTypePass_visit_module(pass, module)
    && resolve->visit_module(module) && check->visit_module(module);

  return success;
}

