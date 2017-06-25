#pragma once
#include "lib.h"
#include "lex.h"

typedef struct AstNode AstNode;

typedef enum
{
  AstOpKind__Null,

  AstOpKind_Add,
  AstOpKind_Sub,
  AstOpKind_Div,
  AstOpKind_Mul,
  AstOpKind_Mod,
  AstOpKind_Neg,

  AstOpKind_Assign,
  AstOpKind_Deref,
  AstOpKind_AddressOf,
  AstOpKind_MemberAccess,
  AstOpKind_PreDecrement,
  AstOpKind_PostDecrement,
  AstOpKind_PreIncrement,
  AstOpKind_PostIncrement,
  
  AstOpKind_LogicEquals,
  AstOpKind_LogicNotEquals,
  AstOpKind_LogicLess,
  AstOpKind_LogicLessEquals,
  AstOpKind_LogicGreater,
  AstOpKind_LogicGreaterEquals,
  AstOpKind_LogicAnd,
  AstOpKind_LogicOr,
  AstOpKind_LogicNot,

  AstOpKind_BitwiseAnd,
  AstOpKind_BitwiseOr,

  AstOpKind__Count,
}
AstOpKind;

internal char*
DEBUG_AstOpKind_tags[] =
{
  "AstOpKind__Null",
  "AstOpKind_Add",
  "AstOpKind_Sub",
  "AstOpKind_Div",
  "AstOpKind_Mul",
  "AstOpKind_Mod",
  "AstOpKind_Neg",
  "AstOpKind_Assign",
  "AstOpKind_Deref",
  "AstOpKind_AddressOf",
  "AstOpKind_MemberAccess",
  "AstOpKind_PreDecrement",
  "AstOpKind_PostDecrement",
  "AstOpKind_PreIncrement",
  "AstOpKind_PostIncrement",
  "AstOpKind_LogicEquals",
  "AstOpKind_LogicNotEquals",
  "AstOpKind_LogicLess",
  "AstOpKind_LogicLessEquals",
  "AstOpKind_LogicGreater",
  "AstOpKind_LogicGreaterEquals",
  "AstOpKind_LogicAnd",
  "AstOpKind_LogicOr",
  "AstOpKind_LogicNot",
  "AstOpKind_BitwiseAnd",
  "AstOpKind_BitwiseOr",
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 // guards
};

typedef enum
{
  AstNodeKind__Null,
  AstNodeKind_BinExpr,
  AstNodeKind_UnrExpr,
  AstNodeKind_Literal,
  AstNodeKind_VarDecl,
  AstNodeKind_Block,
  AstNodeKind_Proc,
  AstNodeKind_Id,
  AstNodeKind_WhileStmt,
  AstNodeKind_ForStmt,
  AstNodeKind_IfStmt,
  AstNodeKind_ReturnStmt,
  AstNodeKind_BreakStmt,
  AstNodeKind_IncludeStmt,
  AstNodeKind_EmptyStmt,
  AstNodeKind_Module,
  AstNodeKind_Noop,
  AstNodeKind_Cast,
  AstNodeKind_Call,
  AstNodeKind_Array,
  AstNodeKind_Pointer,
  AstNodeKind_Struct,
  AstNodeKind_Enum,
  AstNodeKind_Initializer,

  AstNodeKind__Count,
}
AstNodeKind;

internal char*
DEBUG_AstNodeKind_tags[] = 
{
  "AstNodeKind__Null",
  "AstNodeKind_BinExpr",
  "AstNodeKind_UnrExpr",
  "AstNodeKind_Literal",
  "AstNodeKind_VarDecl",
  "AstNodeKind_Block",
  "AstNodeKind_Proc",
  "AstNodeKind_Id",
  "AstNodeKind_WhileStmt",
  "AstNodeKind_ForStmt",
  "AstNodeKind_IfStmt",
  "AstNodeKind_ReturnStmt",
  "AstNodeKind_BreakStmt",
  "AstNodeKind_IncludeStmt",
  "AstNodeKind_EmptyStmt",
  "AstNodeKind_Module",
  "AstNodeKind_Noop",
  "AstNodeKind_Cast",
  "AstNodeKind_Call",
  "AstNodeKind_Array",
  "AstNodeKind_Pointer",
  "AstNodeKind_Struct",
  "AstNodeKind_Enum",
  "AstNodeKind_Initializer",
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 // guards
};

typedef struct
{
  char* name;
}
AstId;

typedef struct
{
  int loc;
  int size;
}
DataArea;

typedef struct
{
  int actv_rec_offset;
  DataArea data;
}
AccessLink;

typedef struct
{
  AstNode* expr;
}
AstPointer;

typedef struct
{
  AstNode* expr;
  AstNode* index;
}
AstArray;

typedef struct
{
  AstNode* lhs;
  AstNode* rhs;
}
AstAccessor;

typedef struct
{
  AstNode* type;
  AstNode* id;
  AstNode* init_expr;
  /*
  DataArea data;
  Type* var_type;
  */
}
AstVarDecl;

/*
typedef struct
{
  char* name;
  AstNode* var_decl;
  int decl_block_offset;
  AccessLink* link;
  DataArea* data;
}
AstVarOccur;
*/

typedef struct
{
  AstOpKind op;
  AstNode* lhs;
  AstNode* rhs;

  /*
  char* label_end; // for boolean expressions
  */
}
AstBinExpr;

typedef struct
{
  AstOpKind op;
  AstNode* operand;
}
AstUnrExpr;

typedef enum
{
  AstLiteralKind__Null,
  AstLiteralKind_Int,
  AstLiteralKind_Float,
  AstLiteralKind_Bool,
  AstLiteralKind_String,
  AstLiteralKind_Char,
  
  AstLiteralKind__Count,
}
AstLiteralKind;

internal char*
DEBUG_AstLiteralKind_tags[] =
{
  "AstLiteralKind__Null",
  "AstLiteralKind_Int",
  "AstLiteralKind_Float",
  "AstLiteralKind_Bool",
  "AstLiteralKind_String",
  "AstLiteralKind_Char",
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 // guards
};

typedef struct
{
  AstLiteralKind kind;

  union
  {
    int32 int_val;
    float32 float_val;
    int32 bool_val;
    char char_val;
    char* str;
  };
}
AstLiteral;

typedef struct AstBlock
{
  List stmt_list;

/*
  AstNode* owner;
  int block_id;
  int nesting_depth;
  struct AstBlock* enclosing_block;
  List decl_vars; // <AstNode>
  List local_occurs; // <AstNode>
  List nonlocal_occurs; // <AstNode>
  List access_links; // <AccessLink>

  int links_size;
  int locals_size;
*/
}
AstBlock;

typedef struct
{
  AstNode* ret_type;
  AstNode* id;
  List formal_args;
  AstNode* body;

  /*
  Type* ret_type;
  AstVarDecl ret_var;

  char* label;
  char* label_end;
  int ret_size;
  int args_size;
  int locals_size;
  */
}
AstProc;

typedef struct
{
  AstNode* expr;
/*
  AstNode* assgn_expr;
  int depth;
  AstProc* proc;
*/
}
AstReturnStmt;

typedef struct
{
  AstNode* id;
  List actual_args;
/*
  AstNode* proc;
*/
}
AstCall;

typedef struct
{
  List node_list;
/*
  List proc_list; // <AstProc>
  AstNode* body;

  AstNode* main_proc;
  AstNode* main_call;
*/
}
AstModule;

typedef struct
{
  AstNode* id;
  List member_list;
}
AstStruct;

typedef struct
{
  AstNode* id;
  List member_list;
}
AstEnum;

typedef struct
{
  AstNode* cond_expr;
  AstNode* body;
  AstNode* else_body;

  /*
  char* label_else;
  char* label_end;
  */
}
AstIfStmt;

typedef struct
{
  AstNode* cond_expr;
  AstNode* body;

  /*
  char* label_eval;
  char* label_break;
  */
}
AstWhileStmt;

typedef struct
{
  AstNode* decl;
  AstNode* cond_expr;
  AstNode* loop_expr;
  AstNode* body;
}
AstForStmt;

/*
typedef struct
{
  AstWhileStmt* while_stmt;
  int depth;
}
AstBreakStmt;
*/

/*
typedef struct
{
  AstNode* expr;
  bool new_line;
}
AstPrintStmt;
*/

typedef struct
{
  char* file_path;
  List node_list;
}
AstIncludeStmt;

typedef struct
{
  AstNode* type;
  AstNode* expr;
}
AstCast;

typedef struct
{
  List member_list;
}
AstInitializer;

typedef struct AstNode
{
  AstNodeKind kind;
  //Type* type;
  SourceLocation src_loc;

  union
  {
    AstModule module;
    AstVarDecl var_decl;
    AstBinExpr bin_expr;
    AstUnrExpr unr_expr;
    AstLiteral literal;
    AstProc proc;
    AstReturnStmt ret_stmt;
    AstIfStmt if_stmt;
    AstWhileStmt while_stmt;
    AstForStmt for_stmt;
    AstIncludeStmt include;
    AstBlock block;
    AstCast cast;
    AstId id;
    AstCall call;
    AstArray array;
    AstPointer pointer;
    AstStruct struct_;
    AstAccessor accessor;
    AstEnum enum_;
    AstInitializer initer;
  };
}
AstNode;

bool parse(MemoryArena*, TokenStream*, AstNode**);
void DEBUG_print_ast_node(String*, int, AstNode*, char*);

