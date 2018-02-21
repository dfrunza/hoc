int Type::set_width()
{
  switch(kind)
  {
    case eType_array:
    {
      width = array.size * array.elem->set_width();
    }
    break;
    
    case eType_product:
    {
      width = product.left->set_width() + product.right->set_width();
    }
    break;
    
    case eType_proc:
    {
      width = proc.ret->set_width() + proc.args->set_width();
    }
    break;
    
    case eType_basic:
    {
      switch(basic.kind)
      {
        case eBasicType_int:
        case eBasicType_float:
        case eBasicType_bool:
        {
          width = 4;
        }
        break;
        
        case eBasicType_char:
        {
          width = 1;
        }
        break;
        
        case eBasicType_void:
        {
          width = 0;
        }
        break;
        
        default: assert(0);
      }
    }
    break;
    
    case eType_pointer:
    {
      width = 4;
    }
    break;
    
    case eType_var:
    {
      width = var.type->set_width();
    }
    break;
    
    default: assert(0);
  }

  return width;
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

TypePass* TypePass::create(MemoryArena* arena)
{
  int struct_size = maximum_of_int(4,
                                   sizeof(TypePass_Set),
                                   sizeof(TypePass_Eval),
                                   sizeof(TypePass_Resolve),
                                   sizeof(TypePass_Check));

  TypePass* context = (TypePass*)push_struct_(arena, struct_size, 1);
  context->init(arena);

  return context;
}

void TypePass::init(MemoryArena* arena)
{
  this->arena = arena;
  basic_type_bool  = create_basic_type(eBasicType_bool);
  basic_type_int   = create_basic_type(eBasicType_int);
  basic_type_char  = create_basic_type(eBasicType_char);
  basic_type_float = create_basic_type(eBasicType_float);
  basic_type_void  = create_basic_type(eBasicType_void);
  basic_type_str   = create_array_type(0, basic_type_char);
  subst_list = list_new(arena, eList_type_pair);
}

Type* TypePass::create_var_type(Type* var_type)
{
  Type* type = push_struct(arena, Type);
  type->kind = eType_var;
  type->var.type = var_type;
  type->set_width();

  return type;
}

Type* TypePass::create_basic_type(eBasicType kind)
{
  Type* type = push_struct(arena, Type);
  type->kind = eType_basic;
  type->basic.kind = kind;
  type->set_width();

  return type;
}

Type* TypePass::create_proc_type(Type* args, Type* ret)
{
  Type* type = push_struct(arena, Type);
  type->kind = eType_proc;
  type->proc.args = args;
  type->proc.ret = ret;
  type->width = 0;

  return type;
}

Type* TypePass::create_typevar()
{
  Type* type = push_struct(arena, Type);
  type->kind = eType_typevar;
  type->typevar.id = typevar_id++;
  type->width = 0;

  return type;
}

Type* TypePass::create_product_type(Type* left, Type* right)
{
  Type* type = push_struct(arena, Type);
  type->kind = eType_product;
  type->product.left = left;
  type->product.right = right;
  type->width = 0;

  return type;
}

Type* TypePass::create_array_type(int size, Type* elem)
{
  Type* type = push_struct(arena, Type);
  type->kind = eType_array;
  type->array.size = size;
  type->array.elem = elem;
  type->width = 0;

  return type;
}

Type* TypePass::create_pointer_type(Type* pointee)
{
  Type* type = push_struct(arena, Type);
  type->kind = eType_pointer;
  type->pointer.pointee = pointee;
  type->width = 0;

  return type;
}

bool Type::equal(Type* type_b)
{
  bool are_equal = false;
  
  if((kind != eType_typevar) && (type_b->kind == kind))
  {
    switch(kind)
    {
      case eType_basic:
      {
        are_equal = (basic.kind == type_b->basic.kind);
      }
      break;
      
      case eType_proc:
      {
        are_equal = proc.args->equal(type_b->proc.args)
          && proc.ret->equal(type_b->proc.ret);
      }
      break;
      
      case eType_pointer:
      {
        are_equal = pointer.pointee->equal(type_b->pointer.pointee);
      }
      break;
      
      case eType_product:
      {
        are_equal = product.left->equal(type_b->product.right);
      }
      break;
      
      case eType_array:
      {
        are_equal = array.elem->equal(type_b->array.elem);
      }
      break;
      
      case eType_var:
      {
        are_equal = var.type->equal(type_b->var.type);
      }
      break;
      
      default: assert(0);
    }
  }
  return are_equal;
}

Type* Type::copy(MemoryArena* arena)
{
  Type* copy = push_struct(arena, Type);
  *copy = *this;

  return copy;
}

Type* Type::get_repr_type()
{
  Type* result = this;
  while(result->repr_type)
  {
    result = result->repr_type;
  }

  return result;
}

void Type::set_union(Type* type_b)
{
  if(kind == eType_typevar)
  {
    repr_type = type_b;
  }
  else
  {
    type_b->repr_type = this;
  }
}

bool Type::unif(Type* type_b)
{
  bool success = false;
  Type* repr_type_a = get_repr_type();
  Type* repr_type_b = type_b->get_repr_type();
  
  if(repr_type_a == repr_type_b)
  {
    success = true;
  }
  else
  {
    if(repr_type_a->kind == eType_typevar || repr_type_b->kind == eType_typevar)
    {
      repr_type_a->set_union(repr_type_b);
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
        repr_type_a->set_union(repr_type_b);
        assert(repr_type_a->kind == repr_type_b->kind);
        
        switch(repr_type_a->kind)
        {
          case eType_proc:
          {
            success = repr_type_a->proc.args->unif(repr_type_b->proc.args)
              && repr_type_a->proc.ret->unif(repr_type_b->proc.ret);
          }
          break;
          
          case eType_product:
          {
            success = repr_type_a->product.left->unif(repr_type_b->product.left)
              && repr_type_a->product.right->unif(repr_type_b->product.right);
          }
          break;
          
          case eType_pointer:
          {
            success = repr_type_a->pointer.pointee->unif(repr_type_b->pointer.pointee);
          }
          break;
          
          case eType_array:
          {
            success = repr_type_a->array.elem->unif(repr_type_b->array.elem);
          }
          break;
          
          case eType_var:
          {
            success = repr_type_a->var.type->unif(repr_type_b->var.type);
          }
          break;
          
          default: assert(0);
        }
      }
    }
  }
  
  return success;
}

bool AstNode_resolve_types(AstNode* node, MemoryArena* arena)
{
  bool success = true;
  
  if(success = node->ty->resolve(&node->ty))
  {
    node->ty->set_width();
    if(success = node->eval_ty->resolve(&node->eval_ty))
    {
      node->eval_ty->set_width();
    }
    else
      success = compile_error(arena, node->src_loc, "type error (unresolved type)");
  }
  else
    success = compile_error(arena, node->src_loc, "type error (unresolved type)");
  
  return success;
}

TypePass::TypePair* TypePass::create_type_pair(Type* key, Type* value)
{
  TypePair* pair = push_struct(arena, TypePair);
  pair->key = key;
  pair->value = value;

  return pair;
}

TypePass::TypePair* TypePass::find_pair(Type* type)
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

Type* TypePass::type_subst(Type* type)
{
  type = type->get_repr_type();
  Type* subst = 0;
  
  TypePair* pair = find_pair(type);
  if(pair)
  {
    subst = pair->value;
  }
  else
  {
    subst = type->copy(arena);
    
    pair = create_type_pair(type, subst);
    list_append(subst_list, pair, eList_type_pair);
    
    switch(subst->kind)
    {
      case eType_typevar:
      {
        subst->typevar.id = typevar_id++;
      }
      break;
      
      case eType_proc:
      {
        subst->proc.args = type_subst(subst->proc.args);
        subst->proc.ret = type_subst(subst->proc.ret);
      }
      break;
      
      case eType_product:
      {
        subst->product.left = type_subst(subst->product.left);
        subst->product.right = type_subst(subst->product.right);
      }
      break;
      
      case eType_pointer:
      {
        subst->pointer.pointee = type_subst(subst->pointer.pointee);
      }
      break;
      
      case eType_array:
      {
        subst->array.elem = type_subst(subst->array.elem);
      }
      break;
      
      case eType_var:
      {
        subst->var.type = type_subst(subst->var.type);
      }
      break;
      
      default: assert(0);
    }
  }

  return subst;
}

bool Type::resolve(Type** resolved_type)
{
  bool success = true;
  
  Type* type = this;

  switch(kind)
  {
    case eType_typevar:
    {
      type = get_repr_type();
      if(type->kind == eType_typevar)
      {
        success = false;
      }
      else
      {
        success = type->resolve(&type);
      }
    }
    break;
    
    case eType_basic:
    break; // ok
    
    case eType_proc:
    {
      success = proc.args->resolve(&proc.args) && proc.ret->resolve(&proc.ret);
    }
    break;
    
    case eType_product:
    {
      success = product.left->resolve(&product.left) && product.right->resolve(&product.right);
    }
    break;
    
    case eType_pointer:
    {
      success = pointer.pointee->resolve(&pointer.pointee);
    }
    break;
    
    case eType_array:
    {
      success = array.elem->resolve(&array.elem);
    }
    break;
    
    case eType_var:
    {
      success = var.type->resolve(&var.type);
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

void Type::append_printstr(String* str)
{
  switch(kind)
  {
    case eType_basic:
    {
      if(basic.kind == eBasicType_bool)
        str_append(str, "bool");
      else if(basic.kind == eBasicType_int)
        str_append(str, "int");
      else if(basic.kind == eBasicType_float)
        str_append(str, "float");
      else if(basic.kind == eBasicType_char)
        str_append(str, "char");
      else if(basic.kind == eBasicType_void)
        str_append(str, "void");
      else if(basic.kind == eBasicType_auto)
        str_append(str, "auto");
      else
        assert(0);
    }
    break;

    case eType_pointer:
    {
      pointer.pointee->append_printstr(str);
      str_append(str, "^");
    }
    break;

    case eType_array:
    {
      str_append(str, "(");
      if(array.size >= 0)
        str_format(str, "[%d]", array.size);
      else
        str_append(str, "[]");
      array.elem->append_printstr(str);
      str_append(str, ")");
    }
    break;

    case eType_product:
    {
      product.left->append_printstr(str);
      str_append(str, ", ");
      product.right->append_printstr(str);
    }
    break;

    case eType_proc:
    {
      proc.ret->append_printstr(str);
      str_append(str, " (");
      proc.args->append_printstr(str);
      str_append(str, ")");
    }
    break;

    case eType_var:
    {
      var.type->append_printstr(str);
    }
    break;

    case eType_typevar:
    {
      str_format(str, "type_%d", typevar.id);
    }
    break;

    default: assert(0);
  }
}

char* Type::get_printstr(MemoryArena* arena)
{
  String str = {};
  str_init(&str, arena);
  append_printstr(&str);

  return str_cap(&str);
}

//     SET TYPES
//-----------------------------------------------------

bool TypePass_Set::visit_array(AstNode* array)
{
  assert(KIND(array, eAstNode_array));
  bool success = true;
  
  if(success = visit_expr(array->array.size_expr) && visit_type(array->array.elem_expr))
  {
    int size = 0;
    AstNode* size_expr = array->array.size_expr;
    if(size_expr->kind == eAstNode_lit && size_expr->lit.kind == eLiteral_int)
    {
      size = size_expr->lit.int_val;
      if(size <= 0)
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
      
      array->ty = array->eval_ty = create_array_type(array->array.size, elem_expr->ty);
    }
  }

  return success;
}

bool TypePass_Set::visit_pointer(AstNode* pointer)
{
  assert(KIND(pointer, eAstNode_pointer));
  bool success = true;
  
  AstNode* pointee = pointer->pointer.pointee;
  if(success = visit_type(pointee))
  {
    pointer->ty = pointer->eval_ty = create_pointer_type(pointee->ty);
  }
  return success;
}

bool TypePass_Set::visit_type(AstNode* type)
{
  bool success = true;
  
  switch(type->kind)
  {
    case eAstNode_pointer:
      success = visit_pointer(type);
    break;
    
    case eAstNode_array:
      success = visit_array(type);
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
        type->ty = type->eval_ty = create_typevar();
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

bool TypePass_Set::visit_var(AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  AstNode* type = var->var.type;
  if(success = visit_type(type))
  {
    var->ty = create_var_type(type->ty);
    var->eval_ty = type->ty;

    if(var->var.init_expr)
    {
      success = visit_expr(var->var.init_expr);
    }
  }

  return success;
}

bool TypePass_Set::visit_bin_expr(AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;
  
  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  
  if(success = visit_expr(left_operand) && visit_expr(right_operand))
  {
    bin_expr->eval_ty = create_typevar();
    bin_expr->ty = create_proc_type(create_product_type(left_operand->eval_ty, right_operand->eval_ty), bin_expr->eval_ty);
  }

  return success;
}

bool TypePass_Set::visit_unr_expr(AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  if(success = visit_expr(operand))
  {
    unr_expr->eval_ty = create_typevar();
    unr_expr->ty = create_proc_type(operand->eval_ty, unr_expr->eval_ty);
  }

  return success;
}

bool TypePass_Set::visit_actual_arg(AstNode* call_arg)
{
  assert(KIND(call_arg, eAstNode_call_arg));
  bool success = true;

  AstNode* expr = call_arg->call_arg.expr;
  if(success = visit_expr(expr))
  {
    call_arg->eval_ty = expr->eval_ty;
    call_arg->ty = expr->ty;
  }

  return success;
}

bool TypePass_Set::visit_id(AstNode* id)
{
  assert(KIND(id, eAstNode_id));
  bool success = true;

  id->ty = create_typevar();
  id->eval_ty = create_typevar();

  return success;
}

Type* make_args_product_type(TypePass* context, AstNode* args)
{
  Type* result = context->basic_type_void;

  ListItem* li = args->args.node_list.first;
  if(li)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    result = arg->eval_ty;
    for(li = li->next; li; li = li->next)
    {
      AstNode* next_arg = KIND(li, eList_ast_node)->ast_node;
      result = context->create_product_type(result, next_arg->eval_ty);
    }
  }

  return result;
}

bool TypePass_Set::visit_actual_args(AstNode* args)
{
  assert(KIND(args, eAstNode_node_list));
  bool success = true;
  
  for(ListItem* li = args->args.node_list.first;
      li && success;
      li = li->next)
  {
    AstNode* arg = KIND(li, eList_ast_node)->ast_node;
    success = visit_expr(arg);
  }

  if(success)
  {
    args->ty = args->eval_ty = make_args_product_type(this, args);
  }

  return success;
}

bool TypePass_Set::visit_call(AstNode* call)
{
  assert(KIND(call, eAstNode_call));
  bool success = true;
  
  AstNode* call_expr = call->call.expr;
  AstNode* args = call->call.args;
  if(call_expr->kind == eAstNode_id)
  {
    if(success = visit_id(call_expr) && visit_actual_args(args))
    {
      call->eval_ty = create_typevar();
      call->ty = create_proc_type(args->ty, call->eval_ty);
    }
  }
  else
    success = compile_error(arena, call->src_loc, "unsupported call expr");

  return success;
}

bool TypePass_Set::visit_lit(AstNode* lit)
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
      ty = create_array_type(cstr_len(lit->lit.str_val)+1, basic_type_char);
    }
    break;
    
    default: assert(0);
  }
  lit->ty = lit->eval_ty = ty;

  return success;
}

bool TypePass_Set::visit_index(AstNode* index)
{
  assert(KIND(index, eAstNode_index));
  bool success = true;
  
  if(success = visit_expr(index->index.array_expr) && visit_expr(index->index.i_expr))
  {
    index->ty = index->index.array_expr->eval_ty;
    index->eval_ty = create_typevar();
  }

  return success;
}

bool TypePass_Set::visit_cast(AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));
  bool success = true;

  AstNode* to_type = cast->cast.to_type;
  AstNode* from_expr = cast->cast.from_expr;
  if(success = visit_type(to_type) && visit_expr(from_expr))
  {
    cast->eval_ty = to_type->eval_ty;
    cast->ty = create_product_type(from_expr->eval_ty, cast->eval_ty);
  }

  return success;
}

bool TypePass_Set::visit_assign(AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));
  bool success = true;
  
  AstNode* dest_expr = assign->assign.dest_expr;
  AstNode* source_expr =assign->assign.source_expr;
  if(success = visit_expr(dest_expr) && visit_expr(source_expr))
  {
    assign->ty = assign->eval_ty = dest_expr->eval_ty;
  }

  return success;
}

bool TypePass_Set::visit_expr(AstNode* expr)
{
  bool success = true;
  
  switch(expr->kind)
  {
    case eAstNode_pointer:
    {
      success = visit_pointer(expr);
    }
    break;
    
    case eAstNode_array:
    {
      success = visit_array(expr);
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
    {
      success = visit_type(expr);
    }
    break;
    
    case eAstNode_index:
    {
      success = visit_index(expr);
    }
    break;

    case eAstNode_call_arg:
    {
      success = visit_actual_arg(expr);
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

bool TypePass_Set::visit_return(AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;
  
  if(ret->ret.expr)
  {
    AstNode* ret_expr = ret->ret.expr;
    if(success = visit_expr(ret_expr))
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

bool TypePass_Set::visit_if(AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  
  if(success = visit_expr(if_->if_.cond_expr))
  {
    AstNode* body = if_->if_.body;
    if(success = visit_block_stmt(body))
    {
      if_->ty = body->ty;
      if_->eval_ty = body->eval_ty;
      
      AstNode* else_body = if_->if_.else_body;
      if(else_body)
      {
        success = visit_block_stmt(else_body);
      }
    }
  }

  return success;
}

bool TypePass_Set::visit_do_while(AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  AstNode* body = do_while->do_while.body;
  if(success = visit_block_stmt(body) && visit_expr(do_while->do_while.cond_expr))
  {
    do_while->ty = body->ty;
    do_while->eval_ty = body->eval_ty;
  }

  return success;
}

bool TypePass_Set::visit_while(AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  
  AstNode* body = while_->while_.body;
  if(success = visit_expr(while_->while_.cond_expr) && visit_block_stmt(body))
  {
    while_->ty = body->ty;
    while_->eval_ty = body->eval_ty;
  }

  return success;
}

bool TypePass_Set::visit_block(AstNode* block)
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
    block->ty = block->eval_ty = basic_type_void;
  }

  return success;
}

bool TypePass_Set::visit_block_stmt(AstNode* stmt)
{
  bool success = true;
  
  switch(stmt->kind)
  {
    case eAstNode_var:
    {
      success = visit_var(stmt);
    }
    break;
    
    case eAstNode_block:
    {
      success = visit_block(stmt);
    }
    break;
    
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
    
    case eAstNode_loop_ctrl:
    case eAstNode_empty:
    {
      stmt->ty = stmt->eval_ty = basic_type_void;
    }
    break;
    
    case eAstNode_basic_type:
    {
      success = visit_type(stmt);
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

    case eAstNode_index:
    {
      success = visit_index(stmt);
    }
    break;

    default: assert(0);
  }

  return success;
}

bool TypePass_Set::visit_formal_args(AstNode* args)
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
    args->ty = args->eval_ty = make_args_product_type(this, args);
  }

  return success;
}

bool TypePass_Set::visit_proc(AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;
  
  AstNode* ret_type = proc->proc.ret_type;
  AstNode* args = proc->proc.args;
  if(success = visit_formal_args(args) && visit_type(ret_type))
  {
    proc->ty = create_proc_type(args->eval_ty, ret_type->eval_ty);
    proc->eval_ty = basic_type_void;
    
    if(!is_extern_proc(proc))
    {
      success = visit_block(proc->proc.body);
    }
  }

  return success;
}

bool TypePass_Set::visit_module_stmt(AstNode* stmt)
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

    case eAstNode_include:
    {
      stmt->ty = stmt->eval_ty = basic_type_void;
    }
    break;

    default: assert(0);
  }

  return success;
}

bool TypePass_Set::visit_module(AstNode* module)
{
  assert(KIND(module, eAstNode_module));
  bool success = true;
  
  module->ty = module->eval_ty = basic_type_void;
  
  for(ListItem* li = module->module.nodes.first;
      li && success;
      li = li->next)
  {
    AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
    success = visit_module_stmt(stmt);
  }

  return success;
}

//       EVAL TYPES
//-----------------------------------------------------

bool TypePass_Eval::visit_array(AstNode* array)
{
  assert(KIND(array, eAstNode_array));
  bool success = true;
  
  success = visit_expr(array->array.size_expr) && visit_type(array->array.elem_expr);
  return success;
}

bool TypePass_Eval::visit_pointer(AstNode* pointer)
{
  assert(KIND(pointer, eAstNode_pointer));

  bool success = true;
  success = visit_expr(pointer->pointer.pointee);

  return success;
}

bool TypePass_Eval::visit_type(AstNode* type)
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
    {
      breakpoint();
    }
    break;
  }

  return success;
}

bool TypePass_Eval::visit_cast(AstNode* cast)
{
  assert(KIND(cast, eAstNode_cast));

  bool success = true;
  success = visit_type(cast->cast.to_type) && visit_expr(cast->cast.from_expr);

  return success;
}

bool TypePass_Eval::visit_bin_expr(AstNode* bin_expr)
{
  assert(KIND(bin_expr, eAstNode_bin_expr));
  bool success = true;
  
  AstNode* left_operand = bin_expr->bin_expr.left_operand;
  AstNode* right_operand = bin_expr->bin_expr.right_operand;
  eOperator op = bin_expr->bin_expr.op;
  
  if(success = visit_expr(left_operand) && visit_expr(right_operand))
  {
    switch(op)
    {
      case eOperator_bit_and:
      case eOperator_bit_or:
      case eOperator_bit_xor:
      {
        if(left_operand->eval_ty->unif(basic_type_int)
           && right_operand->eval_ty->unif(basic_type_int)
           && bin_expr->eval_ty->unif(basic_type_int))
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
        if(left_operand->eval_ty->unif(basic_type_int)
           && right_operand->eval_ty->unif(basic_type_char)
           && bin_expr->eval_ty->unif(basic_type_int))
        {
          ;//ok
        }
        else
          success = compile_error(arena, bin_expr->src_loc, "type error (bitwise op)");
      }
      break;
      
      default:
      {
        if(left_operand->eval_ty->unif(right_operand->eval_ty))
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
              if(!bin_expr->eval_ty->unif(basic_type_bool))
              {
                success = compile_error(arena, bin_expr->src_loc, "type error (bin expr)");
              }
            }
            break;

            default:
            {
              if(!bin_expr->eval_ty->unif(left_operand->eval_ty))
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
      if(!bin_expr_ty->proc.ret->unif(bin_expr->eval_ty))
      {
        success = compile_error(arena, bin_expr->src_loc, "type error (bin expr)");
      }
    }
  }

  return success;
}

bool TypePass_Eval::visit_id(AstNode* id)
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
      success = compile_error(arena, id->src_loc, "unknown id `%s`", id->id.name);
  }

  if(success)
  {
    AstNode* decl_ast = id->id.decl_ast;
    if(decl_ast->ty->unif(id->ty))
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
              if(!decl_ast->ty->var.type->unif(id->eval_ty))
              {
                success = compile_error(arena, id->src_loc, "type error (var id)");
              }
            }
          }
        break;
        
        case eType_proc:
        {
          if(!decl_ast->ty->proc.ret->unif(id->eval_ty))
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

bool TypePass_Eval::visit_unr_expr(AstNode* unr_expr)
{
  assert(KIND(unr_expr, eAstNode_unr_expr));
  bool success = true;
  
  AstNode* operand = unr_expr->unr_expr.operand;
  eOperator op = unr_expr->unr_expr.op;
  
  if(success = visit_expr(operand))
  {
    switch(op)
    {
      case eOperator_neg:
      case eOperator_logic_not:
      case eOperator_bit_not:
      if(!unr_expr->eval_ty->unif(operand->eval_ty))
      {
        success = compile_error(arena, unr_expr->src_loc, "type error (unr expr)");
      }
      break;
      
      case eOperator_deref:
      {
        Type* pointee_ty = create_typevar();
        if(operand->eval_ty->unif(create_pointer_type(pointee_ty)))
        {
          if(!unr_expr->eval_ty->unif(pointee_ty))
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
      if(!unr_expr_ty->proc.ret->unif(unr_expr->eval_ty))
      {
        success = compile_error(arena, unr_expr->src_loc, "type error (unr expr)");
      }
    }
  }

  return success;
}

bool TypePass_Eval::visit_var(AstNode* var)
{
  assert(KIND(var, eAstNode_var));
  bool success = true;
  
  if(var->ty->var.type->unif(var->eval_ty))
  {
    AstNode* init_expr = var->var.init_expr;
    if(init_expr)
    {
      if(success = visit_expr(init_expr))
      {
        if(!var->eval_ty->unif(init_expr->eval_ty))
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

bool TypePass_Eval::visit_formal_args(AstNode* args)
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

bool TypePass_Eval::visit_actual_args(AstNode* args)
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

bool TypePass_Eval::visit_call(AstNode* call)
{
  assert(KIND(call, eAstNode_call));

  bool success = true;
  success = visit_id(call->call.expr) && visit_actual_args(call->call.args);

  return success;
}

bool TypePass_Eval::visit_index(AstNode* index)
{
  assert(KIND(index, eAstNode_index));

  bool success = true;
  success = visit_expr(index->index.array_expr) && visit_expr(index->index.i_expr);

  return success;
}

bool TypePass_Eval::visit_assign(AstNode* assign)
{
  assert(KIND(assign, eAstNode_assign));

  bool success = true;
  success = visit_expr(assign->assign.dest_expr) && visit_expr(assign->assign.source_expr);

  return success;
}

bool TypePass_Eval::visit_expr(AstNode* expr)
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
    
    case eAstNode_index:
    {
      success = visit_index(expr);
    }
    break;
    
    case eAstNode_lit:
    break;

    case eAstNode_basic_type:
    case eAstNode_pointer:
    case eAstNode_array:
    {
      success = visit_type(expr);
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

bool TypePass_Eval::visit_if(AstNode* if_)
{
  assert(KIND(if_, eAstNode_if));
  bool success = true;
  
  AstNode* cond_expr = if_->if_.cond_expr;
  AstNode* body = if_->if_.body;
  AstNode* else_body = if_->if_.else_body;
  
  if(success = visit_expr(cond_expr) &&
     visit_block_stmt(body) &&
     (else_body ? visit_block_stmt(else_body) : true))
  {
    if(!cond_expr->eval_ty->unif(basic_type_bool))
    {
      success = compile_error(arena, cond_expr->src_loc, "bool expression was expected");
    }
  }

  return success;
}

bool TypePass_Eval::visit_block(AstNode* block)
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

bool TypePass_Eval::visit_do_while(AstNode* do_while)
{
  assert(KIND(do_while, eAstNode_do_while));
  bool success = true;

  AstNode* cond_expr = do_while->do_while.cond_expr;
  if(success = visit_block_stmt(do_while->do_while.body) && visit_expr(cond_expr))
  {
    if(!cond_expr->eval_ty->unif(basic_type_bool))
    {
      success = compile_error(arena, cond_expr->src_loc, "bool expression was expected");
    }
  }
  return success;
}

bool TypePass_Eval::visit_while(AstNode* while_)
{
  assert(KIND(while_, eAstNode_while));
  bool success = true;
  
  AstNode* cond_expr = while_->while_.cond_expr;
  if(success = visit_expr(cond_expr) && visit_block_stmt(while_->while_.body))
  {
    if(!cond_expr->eval_ty->unif(basic_type_bool))
    {
      success = compile_error(arena, cond_expr->src_loc, "bool expression was expected");
    }
  }

  return success;
}

bool TypePass_Eval::visit_return(AstNode* ret)
{
  assert(KIND(ret, eAstNode_return));
  bool success = true;
  
  AstNode* ret_expr = ret->ret.expr;
  if(ret_expr && (success = visit_expr(ret_expr)))
  {
    AstNode* proc = ret->ret.proc;
    Type* proc_ty = KIND(proc->ty, eType_proc);
    if(!ret_expr->eval_ty->unif(proc_ty->proc.ret))
    {
      success = compile_error(arena, ret->src_loc, "type error (return)");
    }
  }

  return success;
}

bool TypePass_Eval::visit_block_stmt(AstNode* stmt)
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
    
    case eAstNode_return:
    {
      success = visit_return(stmt);
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

bool TypePass_Eval::visit_proc(AstNode* proc)
{
  assert(KIND(proc, eAstNode_proc));
  bool success = true;

  success = visit_formal_args(proc->proc.args) && visit_block_stmt(proc->proc.body)
    && visit_type(proc->proc.ret_type);

  return success;
}

bool TypePass_Eval::visit_module_stmt(AstNode* stmt)
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
    case eAstNode_include:
    break;

    default: assert(0);
  }
  return success;
}

bool TypePass_Eval::visit_module(AstNode* module)
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
        success = unr_expr->eval_ty->unif(create_pointer_type(operand_ty->array.elem));
      }
      else
      {
        success = unr_expr->eval_ty->unif(create_pointer_type(operand->eval_ty));
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
      if(!proc->ty->unif(call->ty))
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
    if(i_expr->eval_ty->unif(basic_type_int))
    {
      Type* array_ty = array_expr->eval_ty;

      if(array_ty->kind == eType_array)
      {
        if(!array_ty->array.elem->unif(index->eval_ty))
        {
          success = compile_error(arena, index->src_loc, "type error (index)");
        }
      }
      else if(array_ty->kind == eType_pointer)
      {
        if(!array_ty->pointer.pointee->unif(index->eval_ty))
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
  
  if(var->eval_ty->equal(basic_type_void))
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
    
    if(!from_ty->equal(to_ty))
    {
      success = false;
      
      if(to_ty->equal(basic_type_int))
      {
        // int <- float | bool | pointer(T) | char
        success = from_ty->equal(basic_type_float) ||
          from_ty->equal(basic_type_bool) ||
          from_ty->equal(basic_type_char) ||
          (from_ty->kind == eType_pointer);
      }
      else if(to_ty->equal(basic_type_char))
      {
        // char <- int
        success = from_ty->equal(basic_type_int);
      }
      else if(to_ty->equal(basic_type_float))
      {
        // float <- int
        success = from_ty->equal(basic_type_int);
      }
      else if(to_ty->equal(basic_type_bool))
      {
        // bool <- int | pointer(T)
        success = from_ty->equal(basic_type_int) ||
          (from_ty->kind == eType_pointer);
      }
      else if(to_ty->kind == eType_pointer)
      {
        // pointer(T) <- pointer(P) | int
        success = (from_ty->kind == eType_pointer) ||
          from_ty->equal(basic_type_int);
      }
      if(!success)
      {
        compile_error(arena, cast->src_loc, "invalid cast `%s` -> `%s`",
                      from_ty->get_printstr(arena), to_ty->get_printstr(arena));
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
        if(ret_ty->equal(basic_type_int)
           || ret_ty->equal(basic_type_float)
           || ret_ty->equal(basic_type_char)
           || (ret_ty->kind == eType_pointer))
        {
          ;//ok
          assert(ret_ty->equal(left_ty) && left_ty->equal(right_ty));
        }
        else
        {
          success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                  get_operator_printstr(op), operands_ty->get_printstr(arena));
        }
      }
      break;
      
      case eOperator_mod:
      {
        if(ret_ty->equal(basic_type_int))
        {
          ;//ok
          assert(ret_ty->equal(left_ty) && left_ty->equal(right_ty));
        }
        else
        {
          success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                  get_operator_printstr(op), operands_ty->get_printstr(arena));
        }
      }
      break;
      
      case eOperator_logic_and:
      case eOperator_logic_or:
      {
        if(left_ty->equal(basic_type_bool) && left_ty->equal(right_ty))
        {
          ;//ok
          assert(ret_ty == basic_type_bool);
        }
        else
          success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                  get_operator_printstr(op), operands_ty->get_printstr(arena));
      }
      break;
      
      case eOperator_bit_and:
      case eOperator_bit_or:
      case eOperator_bit_xor:
      if(left_ty->equal(basic_type_int) && right_ty->equal(basic_type_int))
      {
        ;//ok
      }
      else
        success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), operands_ty->get_printstr(arena));
      break;
      
      case eOperator_bit_shift_left:
      case eOperator_bit_shift_right:
      if(left_ty->equal(basic_type_int) && right_ty->equal(basic_type_char))
      {
        ;//ok
      }
      else
        success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), operands_ty->get_printstr(arena));
      break;
      
      case eOperator_less:
      case eOperator_less_eq:
      case eOperator_greater:
      case eOperator_greater_eq:
      case eOperator_eq:
      case eOperator_not_eq:
      {
        if((left_ty->equal(basic_type_int) ||
            left_ty->equal(basic_type_char) ||
            left_ty->equal(basic_type_float) ||
            (left_ty->kind == eType_pointer)) &&
           left_ty->equal(right_ty))
        {
          ;//ok
          assert(ret_ty->equal(basic_type_bool));
        }
        else
        {
          success = compile_error(arena, bin_expr->src_loc, "type error: `%s` cannot be applied to `%s` operands",
                                  get_operator_printstr(op), operands_ty->get_printstr(arena));
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
      if(operand_ty->equal(basic_type_bool))
      {
        ;//ok
        assert(ret_ty == basic_type_bool);
      }
      else
        success = compile_error(arena, unr_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), operand_ty->get_printstr(arena));
      break;
      
      case eOperator_bit_not:
      if(operand_ty->equal(basic_type_int))
      {
        ;//ok
      }
      else
        success = compile_error(arena, unr_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), operand_ty->get_printstr(arena));
      break;
      
      case eOperator_neg:
      if(operand_ty->equal(basic_type_int) || operand_ty->equal(basic_type_float))
      {
        ;//ok
      }
      else
        success = compile_error(arena, unr_expr->src_loc, "type error: `%s` cannot be applied to `%s` operand",
                                get_operator_printstr(op), operand_ty->get_printstr(arena));
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
    if(!dest_expr->eval_ty->unif(source_expr->eval_ty))
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

bool TypePass::process(AstNode* module)
{
  bool success = true;

  TypePass_Set* set = (TypePass_Set*)this;
  TypePass_Eval* eval = (TypePass_Eval*)this;
  TypePass_Resolve* resolve = (TypePass_Resolve*)this;
  TypePass_Check* check = (TypePass_Check*)this;

  success = set->visit_module(module) && eval->visit_module(module)
    && resolve->visit_module(module) && check->visit_module(module);

  return success;
}

