#pragma once

typedef enum
{
  TypeKind__Null,
  TypeKind_TypeVar,
  TypeKind_Unary,
  TypeKind_Basic,
  TypeKind_Type,
  TypeKind_Proc,
  TypeKind_Product,
  TypeKind_Pointer,
  TypeKind_Array,
}
TypeKind;

typedef enum
{
  UnaryCtorKind__Null,
  UnaryCtorKind_Pointer,
  UnaryCtorKind_Array,
}
UnaryCtorKind;

typedef struct
{
  int id;
}
TypeVar;

typedef enum
{
  BasicTypeKind__Null,
  BasicTypeKind_Void,
  BasicTypeKind_Int,
  BasicTypeKind_Float,
  BasicTypeKind_Char,
  BasicTypeKind_Bool,
}
BasicTypeKind;

typedef struct
{
  BasicTypeKind kind;
}
BasicType;

typedef struct
{
  Type* pointee;
}
PointerType;

typedef struct
{
  Type* args;
  Type* ret;
}
ProcType;

typedef struct
{
  Type* left;
  Type* right;
}
ProductType;

typedef struct Type
{
  TypeKind kind;
  Type* repr_type; // representative of the set of equivalent types

  union
  {
    BasicType basic;
    PointerType ptr;
    ProcType proc;
    ProductType product;
    TypeVar typevar;
  };
}
Type;

typedef struct
{
  Type* key;
  Type* value;
}
TypeTuple;

Type* make_product_type(MemoryArena*, Type*, ListItem*);
bool32 typecheck_block(MemoryArena*, List*, AstNode*);

////////////////////////////////////////////////////////////////////////////////

static List type_tuples;
static int typevar_id = 1;

static Type* basic_type_bool;
static Type* basic_type_int;
static Type* basic_type_char;
static Type* basic_type_float;
static Type* basic_type_void;

