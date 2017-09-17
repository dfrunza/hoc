Type*
new_basic_type(BasicTypeKind kind, int size)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = TypeKind_basic;
  type->basic.kind = kind;
  type->size = size;
  return type;
}

Type*
new_proc_type(Type* args, Type* ret)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = TypeKind_proc;
  type->proc.args = args;
  type->proc.ret = ret;
  return type;
}

Type*
new_typevar()
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = TypeKind_typevar;
  type->typevar.id = typevar_id++;
  return type;
}

Type*
new_product_type(Type* left, Type* right)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = TypeKind_product;
  type->product.left = left;
  type->product.right = right;
  return type;
}

Type*
new_array_type(int size, Type* elem)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = TypeKind_array;
  type->array.size = size;
  type->array.elem = elem;
  return type;
}

Type*
new_pointer_type(Type* pointee)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = TypeKind_pointer;
  type->pointer.pointee = pointee;
  return type;
}

void
init_types()
{
  basic_type_bool = new_basic_type(BasicTypeKind_Bool, 1);
  basic_type_int = new_basic_type(BasicTypeKind_Int, 1);
  basic_type_char = new_basic_type(BasicTypeKind_Char, 1);
  basic_type_float = new_basic_type(BasicTypeKind_Float, 1);
  basic_type_void = new_basic_type(BasicTypeKind_Void, 0);

  subst_list = new_list(arena, ListKind_TypePair);
}

bool
types_are_equal(Type* type_a, Type* type_b)
{
  bool are_equal = false;

  if((type_a->kind != TypeKind_typevar) && (type_b->kind == type_a->kind))
  {
    if(type_a->kind == TypeKind_basic)
      are_equal = (type_a->basic.kind == type_b->basic.kind);
    else if(type_a->kind == TypeKind_proc)
      are_equal = types_are_equal(type_a->proc.args, type_b->proc.args)
        && types_are_equal(type_a->proc.ret, type_b->proc.ret);
    else if(type_a->kind == TypeKind_pointer)
      are_equal = types_are_equal(type_a->pointer.pointee, type_b->pointer.pointee);
    else if(type_a->kind == TypeKind_product)
      are_equal = types_are_equal(type_a->product.left, type_b->product.right);
    else if(type_a->kind == TypeKind_array)
      are_equal = types_are_equal(type_a->array.elem, type_b->array.elem)
        && type_a->array.size == type_b->array.size;
    else
      assert(false);
  }
  return are_equal;
}

int
compute_type_width(Type* type)
{
  if(type->kind == TypeKind_array)
    type->size = type->array.size * compute_type_width(type->array.elem);
  else if(type->kind == TypeKind_product)
    type->size = compute_type_width(type->product.left) + compute_type_width(type->product.right);
  else if(type->kind == TypeKind_basic)
  {
    if(type->basic.kind == BasicTypeKind_Int
       || type->basic.kind == BasicTypeKind_Float
       || type->basic.kind == BasicTypeKind_Bool)
    {
      type->size = 4;
    }
    else if(type->basic.kind == BasicTypeKind_Char)
      type->size = 1;
    else if(type->basic.kind == BasicTypeKind_Void)
      type->size = 0;
    else
      assert(0);
  }
  else if(type->kind == TypeKind_pointer)
    type->size = 4;
  else
    assert(0);
  return type->size;
}

Type*
copy_type(Type* type)
{
  Type* copy = mem_push_struct(arena, Type);
  *copy = *type;
  return copy;
}

Type*
get_type_repr(Type* type)
{
  Type* result = type;
  while(type->repr_type)
  {
    type = type->repr_type;
    result = type;
  }
  return result;
}

void
set_union(Type* type_a, Type* type_b)
{
  if(type_a->kind == TypeKind_typevar)
    type_a->repr_type = type_b;
  else
    type_b->repr_type = type_a;
}

bool
type_unif(Type* type_a, Type* type_b)
{
  bool success = false;
  Type* repr_type_a = get_type_repr(type_a);
  Type* repr_type_b = get_type_repr(type_b);

  if(repr_type_a->kind == TypeKind_typevar || repr_type_b->kind == TypeKind_typevar)
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
    else if(repr_type_a->kind == TypeKind_basic)
    {
      success = (repr_type_a->basic.kind == repr_type_b->basic.kind);
    }
    else
    {
      set_union(repr_type_a, repr_type_b);
      assert(repr_type_a->kind == repr_type_b->kind);

      if(repr_type_a->kind == TypeKind_proc)
      {
        success = type_unif(repr_type_a->proc.args, repr_type_b->proc.args)
          && type_unif(repr_type_a->proc.ret, repr_type_b->proc.ret);
      }
      else if(repr_type_a->kind == TypeKind_product)
      {
        success = type_unif(repr_type_a->product.left, repr_type_b->product.left)
          && type_unif(repr_type_a->product.right, repr_type_b->product.right);
      }
      else if(repr_type_a->kind == TypeKind_pointer)
      {
        success = type_unif(repr_type_a->pointer.pointee, repr_type_b->pointer.pointee);
      }
      else if(repr_type_a->kind == TypeKind_array)
      {
        success = (repr_type_a->array.size == repr_type_b->array.size)
          && type_unif(repr_type_a->array.elem, repr_type_b->array.elem);
      }
      else
        assert(false);
    }
  }

  return success;
}

TypePair*
new_type_pair(Type* key, Type* value)
{
  TypePair* pair = mem_push_struct(arena, TypePair);
  pair->key = key;
  pair->value = value;
  return pair;
}

TypePair*
find_pair(List* subst_list, Type* type)
{
  TypePair* result = 0;
  for(ListItem* list_item = subst_list->first;
      list_item;
      list_item = list_item->next)
  {
    TypePair* pair = (TypePair*)list_item->elem;
    if(pair->key == type)
    {
      result = pair;
      break;
    }
  }
  return result;
}

Type*
type_subst(List* subst_list, Type* type)
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
    append_list_elem(arena, subst_list, pair, ListKind_ast1_node);

    if(subst->kind == TypeKind_typevar)
    {
      subst->typevar.id = typevar_id++;
    }
    else if(subst->kind == TypeKind_proc)
    {
      subst->proc.args = type_subst(subst_list, subst->proc.args);
      subst->proc.ret = type_subst(subst_list, subst->proc.ret);
    }
    else if(subst->kind == TypeKind_product)
    {
      subst->product.left = type_subst(subst_list, subst->product.left);
      subst->product.right = type_subst(subst_list, subst->product.right);
    }
    else if(subst->kind == TypeKind_pointer)
    {
      subst->pointer.pointee = type_subst(subst_list, subst->pointer.pointee);
    }
    else if(subst->kind == TypeKind_array)
    {
      subst->array.elem = type_subst(subst_list, subst->array.elem);
    }
    else
      assert(false);
  }
  return subst;
}


