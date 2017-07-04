#pragma once
#include "lib.h"

typedef struct
{
  char* file_path;
  int line_nr;
  char* src_line;
}
SourceLocation;

bool compile_error(SourceLocation* src_loc, char* file, int line, char* message, ...);

typedef enum TokenKind
{
  TokenKind__Null,
  /* 'Simple' tokens must be listed at the beginning of the enum */
  TokenKind_Dot,
  TokenKind_ArrowRight,
  TokenKind_OpenBracket,
  TokenKind_CloseBracket,
  TokenKind_OpenParens,
  TokenKind_CloseParens,
  TokenKind_OpenBrace,
  TokenKind_CloseBrace,
  TokenKind_Semicolon,
  TokenKind_Colon,
  TokenKind_Comma,
  TokenKind_Percent,
  TokenKind_Star,
  TokenKind_FwdSlash,
  TokenKind_BackSlash,
  TokenKind_Plus,
  TokenKind_PlusPlus,
  TokenKind_Minus,
  TokenKind_MinusMinus,
  TokenKind_Exclam,
  TokenKind_ExclamEquals,
  TokenKind_Equals,
  TokenKind_EqualsEquals,
  TokenKind_AngleRight,
  TokenKind_AngleRightEquals,
  TokenKind_AngleLeft,
  TokenKind_AngleLeftEquals,
  TokenKind_Ampersand,
  TokenKind_AmpersandAmpersand,
  TokenKind_Pipe,
  TokenKind_PipePipe,
  TokenKind_Unknown,
  TokenKind_EndOfInput,

  TokenKind_Var,
  TokenKind_If,
  TokenKind_Else,
  TokenKind_While,
  TokenKind_For,
  TokenKind_Proc,
  TokenKind_Struct,
  TokenKind_Union,
  TokenKind_Return,
  TokenKind_Break,
  TokenKind_Continue,
  TokenKind_Goto,
  TokenKind_Include,
  TokenKind_Enum,
  TokenKind_True,
  TokenKind_False,

  TokenKind_Id,
  TokenKind_IntNum,
  TokenKind_FloatNum,
  TokenKind_String,
  TokenKind_Char,
}
TokenKind;

typedef struct
{
  TokenKind kind;
  char* lexeme;

  union
  {
    int* int_val;
    float* float_val;
    char char_val;
    char* str;
  };
}
Token;

typedef struct TokenStream
{
  struct TokenStream* prev_state;
  Token token;
  char* text;
  char* cursor;

  SourceLocation src_loc;
}
TokenStream;

Token* get_next_token(TokenStream* input);
void init_token_stream(TokenStream* token_stream, char* text, char* file_path);

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
  AstOpKind_PtrDeref,
  AstOpKind_AddressOf,
  AstOpKind_MemberAccess,
  AstOpKind_PtrMemberAccess,
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
  AstNodeKind_ContinueStmt,
  AstNodeKind_GotoStmt,
  AstNodeKind_Label,
  AstNodeKind_IncludeStmt,
  AstNodeKind_Module,
  AstNodeKind_Cast,
  AstNodeKind_Call,
  AstNodeKind_Array,
  AstNodeKind_Pointer,
  AstNodeKind_Struct,
  AstNodeKind_Union,
  AstNodeKind_Enum,
  AstNodeKind_Initializer,

  AstNodeKind__Count,
}
AstNodeKind;

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

  AstNode* owner;
  int block_id;
  int nesting_depth;
  struct AstNode* encl_block;
/*
  List decl_vars;
  List local_occurs;
  List nonlocal_occurs;
  List access_links;

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
}
AstGotoStmt, AstLabel;

typedef struct
{
  AstNode* id;
  List actual_args;
/*
  AstNode* proc;
*/
}
AstCall;

#if 0
typedef struct
{
  //List node_list;

  AstNode* body; /* AstBlock */
}
AstModule;
#endif

typedef struct
{
  char* file_path;
  AstNode* body;
/*
  AstNode* main_proc;
  AstNode* main_call;
*/
}
AstModule, AstIncludeStmt;

typedef struct
{
  AstNode* id;
  List member_list;
}
AstStruct, AstUnion, AstEnum;

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
    AstGotoStmt goto_stmt;
    AstLabel label;
    AstIfStmt if_stmt;
    AstWhileStmt while_stmt;
    AstForStmt for_stmt;
    AstIncludeStmt include_stmt;
    AstBlock block;
    AstCast cast;
    AstId id;
    AstCall call;
    AstArray array;
    AstPointer pointer;
    AstStruct struct_decl;
    AstUnion union_decl;
    AstAccessor accessor;
    AstEnum enum_decl;
    AstInitializer initer;
  };
}
AstNode;

bool parse(TokenStream*, AstNode**);
void DEBUG_print_ast_node(String*, int, AstNode*, char*);

typedef struct Type Type;
typedef struct Symbol Symbol;

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
  Type* repr_type; /* representative member of the set of equivalent types */

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
TypePair;

typedef enum
{
  SymbolKind__Null,
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
    AstNode* node;
    Type* type;
  };
}
Symbol;

#define MAX_SCOPE_NESTING_DEPTH 32
typedef struct
{
  Symbol* symbol;
  int scope_id;
  int last_scope_id;
  int nesting_depth;
  int active_scopes[MAX_SCOPE_NESTING_DEPTH];
  AstNode* active_blocks[MAX_SCOPE_NESTING_DEPTH];
}
SymbolTable;

void init_types();
bool semantic_analysis(AstNode* ast);
void print_char(char buf[3], char raw_char);
char* get_token_printstr(Token* token);
char* get_ast_kind_printstr(AstNodeKind);
bool is_literal_token(TokenKind);
void putback_token(TokenStream*);


