#pragma once

////////////////////////////////////////////////////////////////////////////////
//  Abstract syntax tree
//

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
  AstOpKind_PtrDeref,
  AstOpKind_AddressOf,
  
  AstOpKind_LogicEquals,
  AstOpKind_LogicNotEquals,
  AstOpKind_LogicLess,
  AstOpKind_LogicLessEquals,
  AstOpKind_LogicGreater,
  AstOpKind_LogicGreaterEquals,
  AstOpKind_LogicAnd,
  AstOpKind_LogicOr,
  AstOpKind_LogicNot,

  AstOpKind__Count,
}
AstOpKind;

internal char*
DEBUG_AstOpKind_tags[AstOpKind__Count] =
{
  "AstOpKind__Null",
  "AstOpKind_Add",
  "AstOpKind_Sub",
  "AstOpKind_Div",
  "AstOpKind_Mul",
  "AstOpKind_Mod",
  "AstOpKind_Neg",
  "AstOpKind_Assign",
  "AstOpKind_PtrDeref",
  "AstOpKind_AddressOf",
  "AstOpKind_LogicEquals",
  "AstOpKind_LogicNotEquals",
  "AstOpKind_LogicLess",
  "AstOpKind_LogicLessEquals",
  "AstOpKind_LogicGreater",
  "AstOpKind_LogicGreaterEquals",
  "AstOpKind_LogicAnd",
  "AstOpKind_LogicOr",
  "AstOpKind_LogicNot",
};

typedef enum
{
  AstNodeKind__Null,
  AstNodeKind_BinExpr,
  AstNodeKind_UnrExpr,
  AstNodeKind_Literal,
  AstNodeKind_VarDecl,
  AstNodeKind_Block,
  AstNodeKind_ProcDecl,
  AstNodeKind_Id,
  AstNodeKind_WhileStmt,
  AstNodeKind_IfStmt,
  AstNodeKind_ReturnStmt,
  AstNodeKind_BreakStmt,
  AstNodeKind_IncludeStmt,
  AstNodeKind_EmptyStmt,
  AstNodeKind_Module,
  AstNodeKind_Noop,
  AstNodeKind_Cast,
  AstNodeKind_ProcCall,

  AstNodeKind__Count,
}
AstNodeKind;

internal char*
DEBUG_AstNodeKind_tags[AstNodeKind__Count] = 
{
  "AstNodeKind__Null",
  "AstNodeKind_BinExpr",
  "AstNodeKind_UnrExpr",
  "AstNodeKind_Literal",
  "AstNodeKind_VarDecl",
  "AstNodeKind_Block",
  "AstNodeKind_ProcDecl",
  "AstNodeKind_Id",
  "AstNodeKind_WhileStmt",
  "AstNodeKind_IfStmt",
  "AstNodeKind_ReturnStmt",
  "AstNodeKind_BreakStmt",
  "AstNodeKind_IncludeStmt",
  "AstNodeKind_EmptyStmt",
  "AstNodeKind_Module",
  "AstNodeKind_Noop",
  "AstNodeKind_Cast",
  "AstNodeKind_ProcCall",
};

typedef enum
{
  AstIdKind__Null,
  AstIdKind_Plain,
  AstIdKind_Type,
  AstIdKind_ProcCall,
  AstIdKind_ProcSignature,
  AstIdKind_ArrayIndexer,

  AstIdKind__Count,
}
AstIdKind;

internal char*
DEBUG_AstIdKind_tags[AstIdKind__Count] =
{
  "AstIdKind__Null",
  "AstIdKind_Plain",
  "AstIdKind_Type",
  "AstIdKind_ProcCall",
  "AstIdKind_ProcSignature",
  "AstIdKind_ArrayIndexer",
};

typedef struct
{
  AstIdKind kind;
  char* name;

  union
  {
    List call_args;
    List formal_args;
    List indexer_list;
    int ptr_indir_level;
  };
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
  AstNode* id;
  List args;
}
AstCall;

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
  AstNode* left_operand;
  AstNode* right_operand;

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
  
  AstLiteralKind__Count,
}
AstLiteralKind;

internal char*
DEBUG_AstLiteralKind_tags[AstLiteralKind__Count] =
{
  "AstLiteralKind__Null",
  "AstLiteralKind_Int",
  "AstLiteralKind_Float",
  "AstLiteralKind_Bool",
};

typedef struct
{
  AstLiteralKind kind;

  union
  {
    int32 int_val;
    float32 float_val;
    int32 bool_val;
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
  AstNode* signature;
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

/*
typedef struct
{
  char* name;
  List actual_args; // <AstNode>
  AstNode* proc;
}
AstCall;
*/

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
  bool32 new_line;
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
    AstIncludeStmt inc_stmt;
    AstBlock block;
    AstCast cast;
    AstId id;
    AstCall call;
  };
}
AstNode;

////////////////////////////////////////////////////////////////////////////////
//  Symbol table
//

typedef struct
{
  Symbol* symbol;
  int scope_id;
  int last_scope_id;
  int nesting_depth;
  int active_scopes[32];
  MemoryArena* arena;
}
SymbolTable;

typedef enum
{
  SymbolKind__Null,
  SymbolKind_Keyword,
  SymbolKind_Proc,
  SymbolKind_Type,
  SymbolKind_Var,
}
SymbolKind;

typedef struct Symbol
{
  SymbolKind kind;
  Symbol* next_symbol;

  char* name;
  int block_id;
  int nesting_depth;

  union {
    TokenKind keyword;
    AstNode* node;
    Type* type;
  };
}
Symbol;

bool32 parse_expression(MemoryArena*, TokenStream*, AstNode**);
bool32 parse_statement(MemoryArena*, TokenStream*, AstNode**);
bool32 parse_module(MemoryArena*, TokenStream*, List*);
void DEBUG_print_ast_node(String*, int, AstNode*, char*);

