#pragma once
#include "lib.h"

typedef struct AstNode AstNode;
typedef struct Type Type;
typedef struct Symbol Symbol;

typedef struct
{
  char* file_path;
  int line_nr;
  char* src_line;
}
SourceLocation;

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
  AstNodeKind_VarOccur,
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
  AstNodeKind_EmptyStmt,

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

  AstNode* decl_block;
  /*
  DataArea data;
  Type* var_type;
  */
}
AstVarDecl;

typedef struct
{
  char* name;
  AstNode* var_decl;
  AstNode* decl_block;
  AstNode* occur_block;
  int decl_block_offset;
/*
  AccessLink* link;
  DataArea* data;
*/
}
AstVarOccur;

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
  AstLiteralKind_Char,
  AstLiteralKind_String,
  
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
  List node_list;

  //AstNode* owner;
  int block_id;
  int nesting_depth;
  struct AstNode* encl_block;
  List decl_vars;
  List stmts;
  List locals;
  List nonlocals;
/*
  List access_links;

  int links_size;
  int locals_size;
*/
}
AstBlock;

typedef struct
{
  AstNode* ret_type; // syntactic node
  AstNode* id;
  List formal_args;
  AstNode* body;

  AstNode* ret_var; // semantic node
  bool is_decl;
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

  AstNode* proc;
  int nesting_depth;
}
AstReturnStmt;

typedef struct
{
  AstNode* loop;
  int nesting_depth;
}
AstLoopCtrl;

typedef struct
{
  AstNode* id;
}
AstGotoStmt, AstLabel;

typedef struct
{
  AstNode* id;
  List args;
  AstNode* proc;
}
AstCall;

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
  AstNode* decl_expr;
  AstNode* cond_expr;
  AstNode* loop_expr;
  AstNode* body;
}
AstForStmt;

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

/*
union U
{
  AstModule module;
  AstVarDecl var_decl;
  ...
}

struct AstNode
{
  AstNodeKind kind;
  Type* type;
  union U;
}
*/

typedef struct AstNode
{
  AstNodeKind kind;
  Type* type;
  SourceLocation src_loc;

  union
  {
    AstModule module;
    AstVarDecl var_decl;
    AstVarOccur var_occur;
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
    AstLoopCtrl loop_ctrl;
    AstIncludeStmt incl_stmt;
    AstBlock block;
    AstCast cast;
    AstId id;
    AstCall call;
    AstArray array;
    AstPointer ptr;
    AstStruct struct_decl;
    AstUnion union_decl;
    AstAccessor accessor;
    AstEnum enum_decl;
    AstInitializer initer;
  };
}
AstNode;

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

typedef struct
{
  int dim;
  Type* elem;
}
ArrayType;

typedef struct Type
{
  TypeKind kind;
  Type* repr_type; /* representative member of the set of equivalent types */
  AstNode* node;

  union
  {
    BasicType basic;
    PointerType ptr;
    ProcType proc;
    ProductType product;
    ArrayType array;
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

  Symbol* prev_symbol;
  char* name;
  int block_id;
  int nesting_depth;

  AstNode* node;
}
Symbol;

#define MAX_SCOPE_NESTING_DEPTH 100
typedef struct
{
  Symbol* curr_symbol;
  AstNode* curr_block;
  int block_id;
  int nesting_depth;
  AstNode* active_blocks[MAX_SCOPE_NESTING_DEPTH];
  int sym_count;
}
SymbolTable;

#define compile_error(SRC, MESSAGE, ...)\
  compile_error_f((SRC), __FILE__, __LINE__,(MESSAGE), __VA_ARGS__)

bool compile_error_f(SourceLocation* src_loc, char* file, int line, char* message, ...);
bool get_next_token(TokenStream* input);
void putback_token(TokenStream* input);
void init_token_stream(TokenStream* token_stream, char* text, char* file_path);
void print_char(char buf[3], char raw_char);
bool is_literal_token(TokenKind kind);
char* get_token_printstr(Token* token);
char* get_ast_kind_printstr(AstNodeKind kind);
void DEBUG_print_ast_node(String* str, int indent_level, AstNode* node, char* tag);
void DEBUG_print_arena_usage(char* tag);
bool parse(TokenStream* input, AstNode** node);
void init_types();
bool semantic_analysis(AstNode* ast);
AstNode* new_bin_expr(SourceLocation* src_loc);
AstNode* new_id(SourceLocation* src_loc, char* name);
AstNode* new_var_decl(SourceLocation* src_loc);
AstNode* new_block(SourceLocation* src_loc);
Type* new_proc_type(Type* args, Type* ret);
Type* new_typevar();
Type* new_pointer_type(Type* pointee);
Type* new_product_type(Type* left, Type* right);
Type* new_array_type(int dim, Type* elem_type);
Type* make_type_of_node_list(List* node_list);
bool type_unif(Type* type_a, Type* type_b);
void build_runtime(AstNode* ast);


