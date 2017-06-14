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

  AstOpKind_Call,
  AstOpKind_Assign,
  
  AstOpKind_LogicEquals,
  AstOpKind_LogicNotEquals,
  AstOpKind_LogicLess,
  AstOpKind_LogicLessEquals,
  AstOpKind_LogicGreater,
  AstOpKind_LogicGreaterEquals,
  AstOpKind_LogicAnd,
  AstOpKind_LogicOr,
  AstOpKind_LogicNot,
}
AstOpKind;

typedef enum
{
  AstNodeKind__Null,
  AstNodeKind_BinExpr,
  AstNodeKind_UnrExpr,
  AstNodeKind_Literal,
  AstNodeKind_VarDecl,
  AstNodeKind_VarOccur,
  AstNodeKind_Call,
  AstNodeKind_Block,
  AstNodeKind_Proc,
  AstNodeKind_Id,
  AstNodeKind_WhileStmt,
  AstNodeKind_IfStmt,
  AstNodeKind_ReturnStmt,
  AstNodeKind_BreakStmt,
  AstNodeKind_PrintStmt,
  AstNodeKind_IncludeStmt,
  AstNodeKind_EmptyStmt,
  AstNodeKind_Module,
  AstNodeKind_Noop,
  AstNodeKind_Cast,
}
AstNodeKind;

typedef enum
{
  AstIdKind__Null,
  AstIdKind_Plain,
  AstIdKind_ProcCall,
  AstIdKind_ArrayIndexer,
}
AstIdKind;

typedef struct
{
  AstIdKind kind;
  char* name;

  union
  {
    List call_args;
    AstNode* indexer_expr;
  };
}
AstId;

typedef struct
{
  int loc;
  int size;
}
DataArea;

typedef struct AccessLink
{
  int actv_rec_offset;
  DataArea data;
}
AccessLink;

typedef struct
{
  AstNode* type;
  AstNode* decl;
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
}
AstLiteralKind;

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

/*
typedef struct AstBlock
{
  List stmt_list;

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
}
AstBlock;
*/

typedef struct
{
  char* name;
  char* ret_type;
  List formal_args; // <AstNode>
  AstNode* body; // <AstBlock>

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

/*
typedef struct
{
  AstNode* ret_expr;
  AstNode* assgn_expr;
  int depth;
  AstProc* proc;
}
AstReturnStmt;
*/

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
  Type* type;
  SourceLocation src_loc;

  union
  {
    AstModule module;
    AstVarDecl var_decl;
    //AstVarOccur var_occur;
    AstBinExpr bin_expr;
    AstUnrExpr unr_expr;
    AstLiteral literal;
    AstProc proc;
    //AstReturnStmt ret_stmt;
    AstNode* ret_expr;
    //AstCall call;
    AstIfStmt if_stmt;
    AstWhileStmt while_stmt;
    //AstPrintStmt print_stmt;
    AstIncludeStmt inc_stmt;
    //AstBreakStmt break_stmt;
    //AstBlock block;
    AstCast cast;
    AstId id;
    //List arg_list;
    List stmt_list;
    //List node_list;
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

bool32 is_logical_operator(AstOpKind);
bool32 parse_expression(MemoryArena*, TokenStream*, AstNode**);
bool32 parse_term(MemoryArena*, TokenStream*, AstNode**);
bool32 parse_statement(MemoryArena*, TokenStream*, AstNode**);
bool32 parse_if_stmt(MemoryArena*, TokenStream*, AstNode**);
bool32 parse(MemoryArena*, TokenStream*, AstNode**);
bool32 parse_factor(MemoryArena*, TokenStream*, AstNode**);
bool32 parse_module(MemoryArena*, TokenStream*, List*);

