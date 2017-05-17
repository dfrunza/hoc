#pragma once

typedef enum
{
  EntityKind_Ast,
  EntityKind_RuntimeObj,
  EntityKind_IntermRepr,
} EntityKind;

typedef struct
{
  int i;
}
RuntimeObj;

struct Entity
{
  EntityKind kind;
  void* e;

  union
  {
    AstNode n;
    Type t;
    RuntimeObj r;
  };
};

void build_stmt(MemoryArena*, AstNode*);
void build_block_stmts(MemoryArena*, List*);

static int g_last_label_id;

