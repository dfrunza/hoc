#include "hocc.h"

typedef struct
{
  Type* key;
  Type* value;
}
TypePair;

extern MemoryArena* arena;

local List subst_list;
local int typevar_id = 1;

Type* basic_type_bool;
Type* basic_type_int;
Type* basic_type_char;
Type* basic_type_float;
Type* basic_type_void;

local Type*
new_basic_type(BasicTypeKind kind, int size)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = TypeKind_Basic;
  type->basic.kind = kind;
  type->size = size;
  return type;
}

Type*
new_proc_type(Type* args, Type* ret)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = TypeKind_Proc;
  type->proc.args = args;
  type->proc.ret = ret;
  return type;
}

Type*
new_typevar()
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = TypeKind_TypeVar;
  type->typevar.id = typevar_id++;
  return type;
}

Type*
new_product_type(Type* left, Type* right)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = TypeKind_Product;
  type->product.left = left;
  type->product.right = right;
  return type;
}

Type*
new_array_type(int size, Type* elem)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = TypeKind_Array;
  type->array.size = size;
  type->array.elem = elem;
  return type;
}

Type*
new_pointer_type(Type* pointee)
{
  Type* type = mem_push_struct(arena, Type);
  type->kind = TypeKind_Pointer;
  type->ptr.pointee = pointee;
  return type;
}

Type*
make_type_of_node_list(List* node_list)
{
  Type* type = basic_type_void;
  ListItem* first_item = node_list->first;
  if(first_item)
  {
    type = ((AstNode*)first_item->elem)->type;
    for(ListItem* list_item = first_item->next;
        list_item;
        list_item = list_item->next)
    {
      AstNode* node = (AstNode*)list_item->elem;
      type = new_product_type(type, node->type);
    }
  }
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

  list_init(&subst_list);
}

bool32
types_are_equal(Type* type_a, Type* type_b)
{
  bool32 are_equal = false;

  if((type_a->kind != TypeKind_TypeVar) && (type_b->kind == type_a->kind))
  {
    if(type_a->kind == TypeKind_Basic)
      are_equal = (type_a->basic.kind == type_b->basic.kind);
    else if(type_a->kind == TypeKind_Proc)
      are_equal = types_are_equal(type_a->proc.args, type_b->proc.args)
        && types_are_equal(type_a->proc.ret, type_b->proc.ret);
    else if(type_a->kind == TypeKind_Pointer)
      are_equal = types_are_equal(type_a->ptr.pointee, type_b->ptr.pointee);
    else if(type_a->kind == TypeKind_Product)
      are_equal = types_are_equal(type_a->product.left, type_b->product.right);
    else if(type_a->kind == TypeKind_Array)
      are_equal = types_are_equal(type_a->array.elem, type_b->array.elem)
        && type_a->array.size == type_b->array.size;
    else
      assert(false);
  }
  return are_equal;
}

local Type*
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

local void
set_union(Type* type_a, Type* type_b)
{
  if(type_a->kind == TypeKind_TypeVar)
    type_a->repr_type = type_b;
  else
    type_b->repr_type = type_a;
}

bool32
type_unif(Type* type_a, Type* type_b)
{
  bool32 success = false;
  Type* repr_type_a = get_type_repr(type_a);
  Type* repr_type_b = get_type_repr(type_b);

  if(repr_type_a->kind == TypeKind_TypeVar || repr_type_b->kind == TypeKind_TypeVar)
  {
    set_union(repr_type_a, repr_type_b);
    success = true;
  }
  else if(repr_type_a->kind == repr_type_b->kind)
  {
    if(repr_type_a == repr_type_b)
      success = true;
    else if(repr_type_a->kind == TypeKind_Basic)
      success = (repr_type_a->basic.kind == repr_type_b->basic.kind);
    else
    {
      set_union(repr_type_a, repr_type_b);
      assert(repr_type_a->kind == repr_type_b->kind);

      if(repr_type_a->kind == TypeKind_Proc)
        success = type_unif(repr_type_a->proc.args, repr_type_b->proc.args)
          && type_unif(repr_type_a->proc.ret, repr_type_b->proc.ret);
      else if(repr_type_a->kind == TypeKind_Product)
        success = type_unif(repr_type_a->product.left, repr_type_b->product.left)
          && type_unif(repr_type_a->product.right, repr_type_b->product.right);
      else if(repr_type_a->kind == TypeKind_Pointer)
        success = type_unif(repr_type_a->ptr.pointee, repr_type_b->ptr.pointee);
      else if(repr_type_a->kind == TypeKind_Array)
        success = (repr_type_a->array.size == repr_type_b->array.size)
          && type_unif(repr_type_a->array.elem, repr_type_b->array.elem);
      else
        assert(false);
    }
  }

  return success;
}

local TypePair*
new_type_pair(Type* key, Type* value)
{
  TypePair* pair = mem_push_struct(arena, TypePair);
  pair->key = key;
  pair->value = value;
  return pair;
}

local TypePair*
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

local Type*
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
    list_append(arena, subst_list, pair);

    if(subst->kind == TypeKind_TypeVar)
      subst->typevar.id = typevar_id++;
    else if(subst->kind == TypeKind_Proc)
    {
      subst->proc.args = type_subst(subst_list, subst->proc.args);
      subst->proc.ret = type_subst(subst_list, subst->proc.ret);
    }
    else if(subst->kind == TypeKind_Product)
    {
      subst->product.left = type_subst(subst_list, subst->product.left);
      subst->product.right = type_subst(subst_list, subst->product.right);
    }
    else if(subst->kind == TypeKind_Pointer)
      subst->ptr.pointee = type_subst(subst_list, subst->ptr.pointee);
    else if(subst->kind == TypeKind_Array)
      subst->array.elem = type_subst(subst_list, subst->array.elem);
    else
      assert(false);
  }
  return subst;
}


