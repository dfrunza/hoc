Type* new_basic_type(BasicTypeKind kind)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = Type_basic;
  type->basic.kind = kind;
  return type;
}

Type* new_proc_type(Type* args, Type* ret)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = Type_proc;
  type->proc.args = args;
  type->proc.ret = ret;
  return type;
}

Type* new_typevar()
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = Type_typevar;
  type->typevar.id = typevar_id++;
  return type;
}

Type* new_product_type(Type* left, Type* right)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = Type_product;
  type->product.left = left;
  type->product.right = right;
  return type;
}

Type* new_array_type(int size, Type* elem)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = Type_array;
  type->array.size = size;
  type->array.elem = elem;
  return type;
}

Type* new_pointer_type(Type* pointee)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = Type_pointer;
  type->pointer.pointee = pointee;
  return type;
}

void init_types()
{
  basic_type_bool = new_basic_type(BasicType_bool);
  basic_type_int = new_basic_type(BasicType_int);
  basic_type_char = new_basic_type(BasicType_char);
  basic_type_float = new_basic_type(BasicType_float);
  basic_type_void = new_basic_type(BasicType_void);
  basic_type_type = new_basic_type(BasicType_type);

  subst_list = new_list(arena, List_type_pair);
}

bool types_are_equal(Type* type_a, Type* type_b)
{
  bool are_equal = false;

  if((type_a->kind != Type_typevar) && (type_b->kind == type_a->kind))
  {
    if(type_a->kind == Type_basic)
    {
      are_equal = (type_a->basic.kind == type_b->basic.kind);
    }
    else if(type_a->kind == Type_proc)
    {
      are_equal = types_are_equal(type_a->proc.args, type_b->proc.args)
        && types_are_equal(type_a->proc.ret, type_b->proc.ret);
    }
    else if(type_a->kind == Type_pointer)
    {
      are_equal = types_are_equal(type_a->pointer.pointee, type_b->pointer.pointee);
    }
    else if(type_a->kind == Type_product)
    {
      are_equal = types_are_equal(type_a->product.left, type_b->product.right);
    }
    else if(type_a->kind == Type_array)
    {
      are_equal = types_are_equal(type_a->array.elem, type_b->array.elem);
    }
    else
      assert(0);
  }
  return are_equal;
}

int compute_type_width(Type* type)
{
  if(type->kind == Type_array)
  {
    type->width = type->array.size * compute_type_width(type->array.elem);
  }
  else if(type->kind == Type_product)
  {
    type->width = compute_type_width(type->product.left) + compute_type_width(type->product.right);
  }
  else if(type->kind == Type_proc)
  {
    type->width = compute_type_width(type->proc.ret) + compute_type_width(type->proc.args);
  }
  else if(type->kind == Type_basic)
  {
    if(type->basic.kind == BasicType_int
       || type->basic.kind == BasicType_float
       || type->basic.kind == BasicType_bool)
    {
      type->width = 4;
    }
    else if(type->basic.kind == BasicType_char)
      type->width = 1;
    else if(type->basic.kind == BasicType_void)
      type->width = 0;
    else
      assert(0);
  }
  else if(type->kind == Type_pointer)
  {
    type->width = 4;
  }
  else
    assert(0);
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
  if(type_a->kind == Type_typevar)
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
    if(repr_type_a->kind == Type_typevar || repr_type_b->kind == Type_typevar)
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
      else if(repr_type_a->kind == Type_basic)
      {
        success = (repr_type_a->basic.kind == repr_type_b->basic.kind);
      }
      else
      {
        set_union(repr_type_a, repr_type_b);
        assert(repr_type_a->kind == repr_type_b->kind);

        if(repr_type_a->kind == Type_proc)
        {
          success = type_unif(repr_type_a->proc.args, repr_type_b->proc.args)
            && type_unif(repr_type_a->proc.ret, repr_type_b->proc.ret);
        }
        else if(repr_type_a->kind == Type_product)
        {
          success = type_unif(repr_type_a->product.left, repr_type_b->product.left)
            && type_unif(repr_type_a->product.right, repr_type_b->product.right);
        }
        else if(repr_type_a->kind == Type_pointer)
        {
          success = type_unif(repr_type_a->pointer.pointee, repr_type_b->pointer.pointee);
        }
        else if(repr_type_a->kind == Type_array)
        {
          success = (repr_type_a->array.size == repr_type_b->array.size)
            && type_unif(repr_type_a->array.elem, repr_type_b->array.elem);
        }
        else
          assert(0);
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
    append_list_elem(subst_list, pair, List_type_pair);

    if(subst->kind == Type_typevar)
    {
      subst->typevar.id = typevar_id++;
    }
    else if(subst->kind == Type_proc)
    {
      subst->proc.args = type_subst(subst_list, subst->proc.args);
      subst->proc.ret = type_subst(subst_list, subst->proc.ret);
    }
    else if(subst->kind == Type_product)
    {
      subst->product.left = type_subst(subst_list, subst->product.left);
      subst->product.right = type_subst(subst_list, subst->product.right);
    }
    else if(subst->kind == Type_pointer)
    {
      subst->pointer.pointee = type_subst(subst_list, subst->pointer.pointee);
    }
    else if(subst->kind == Type_array)
    {
      subst->array.elem = type_subst(subst_list, subst->array.elem);
    }
    else
      assert(0);
  }
  return subst;
}

bool resolve_type(Type* type, Type** resolved_type)
{
  bool success = true;

  if(type->kind == Type_typevar)
  {
    type = get_type_repr(type);
    if(type->kind == Type_typevar)
    {
      success = false;
    }
  }
  else if(type->kind == Type_basic)
  {
    ; // ok
  }
  else if(type->kind == Type_proc)
  {
    success = resolve_type(type->proc.args, &type->proc.args)
      && resolve_type(type->proc.ret, &type->proc.ret);
  }
  else if(type->kind == Type_product)
  {
    success = resolve_type(type->product.left, &type->product.left)
      && resolve_type(type->product.right, &type->product.right);
  }
  else if(type->kind == Type_pointer)
  {
    success = resolve_type(type->pointer.pointee, &type->pointer.pointee);
  }
  else if(type->kind == Type_array)
  {
    success = resolve_type(type->array.elem, &type->array.elem);
  }
  else
    assert(0);

  if(success)
  {
    *resolved_type = type;
  }

  return success;
}

