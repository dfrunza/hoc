#pragma once
#include "lib.h"

#define ARENA_SIZE (3*MEGABYTE)
#define DEBUG_ARENA_SIZE (ARENA_SIZE/3)
#define SYM_ARENA_SIZE (ARENA_SIZE/10)
#define MAX_SCOPE_NESTING_DEPTH 100

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
  TokenKind_Cast,
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

typedef struct AstNode
{
  AstNodeKind kind;
  Type* type;
  SourceLocation src_loc;

  union {
    struct {
      char* file_path;
      AstNode* body;

      /* runtime */
      AstNode* main_call;
    }
    module,
    incl_stmt;

    struct {
      AstNode* type;
      AstNode* id;
      AstNode* init_expr;

      /* semantic */
      AstNode* decl_block;
      AstNode* assign_expr;

      /* runtime */
      DataArea data;
    }
    var_decl;

    struct {
      AstNode* id;

      /* semantic */
      AstNode* var_decl;
      AstNode* occur_block;
      int decl_block_offset;

      /* runtime */
      AccessLink* link;
      DataArea* data;
    }
    var_occur;

    struct {
      AstNode* ret_type;
      AstNode* id;
      List args;
      AstNode* body;

      /* semantic */
      AstNode* ret_var;
      bool32 is_decl;

      /* runtime */
      char* label;
      char* label_end;
      int ret_size;
      int args_size;
      int locals_size;
    }
    proc;

    struct {
      AstNode* id;
      List args;

      /* semantic */
      AstNode* proc;
    }
    call;

    struct {
      AstOpKind op;
      AstNode* lhs;
      AstNode* rhs;

      /* runtime */
      char* label_end; // for boolean expressions
    }
    bin_expr;

    struct {
      AstOpKind op;
      AstNode* operand;
    }
    unr_expr;

    struct {
      AstLiteralKind kind;

      union {
        int32 int_val;
        float32 float_val;
        int32 bool_val;
        char char_val;
        char* str;
      };
    }
    literal;

    struct {
      AstNode* expr;

      /* semantic */
      AstNode* proc;
      int nesting_depth;
      AstNode* assign_expr;
    }
    ret_stmt;

    struct {
      AstNode* id;
    }
    goto_stmt,
    label;

    struct {
      AstNode* cond_expr;
      AstNode* body;
      AstNode* else_body;

      /* runtime */
      char* label_else;
      char* label_end;
    }
    if_stmt;

    struct {
      AstNode* cond_expr;
      AstNode* body;

      /* runtime */
      char* label_eval;
      char* label_break;
    }
    while_stmt;

    struct {
      AstNode* decl_expr;
      AstNode* cond_expr;
      AstNode* loop_expr;
      AstNode* body;

      /* runtime */
      char* label_eval;
      char* label_break;
    }
    for_stmt;

    struct {
      AstNode* loop;
      int nesting_depth;
    }
    loop_ctrl;

    struct {
      List node_list;

      /* semantic */
      //AstNode* owner;
      int block_id;
      int nesting_depth;
      struct AstNode* encl_block;
      List decl_vars;
      List locals;
      List nonlocals;

      /* runtime */
      List access_links;
      int links_size;
      int locals_size;
    }
    block;

    struct {
      AstNode* to_type;
      AstNode* expr;
    }
    cast;

    struct {
      char* name;
      Symbol* sym;
    }
    id;

    struct {
      AstNode* expr;
      AstNode* index;
    }
    array;

    struct {
      AstNode* expr;
    }
    ptr;

    struct {
      AstNode* id;
      List member_list;
    }
    struct_decl,
    union_decl,
    enum_decl;

    struct {
      AstNode* lhs;
      AstNode* rhs;
    }
    accessor;

    struct {
      List member_list;
    }
    initer;
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

typedef struct Type
{
  TypeKind kind;
  Type* repr_type; /* representative member of the set of equivalent types */
  AstNode* node;

  union {
    struct {
      BasicTypeKind kind;
      int size; // in VM-words
    }
    basic;

    struct {
      Type* pointee;
    }
    ptr;

    struct {
      Type* args;
      Type* ret;
    }
    proc;

    struct {
      Type* left;
      Type* right;
    }
    product;

    struct {
      int dim;
      Type* elem;
    }
    array;

    struct {
      int id;
    }
    typevar;
  };
}
Type;

typedef enum
{
  SymbolKind__Null,
  SymbolKind_VarDecl,
  SymbolKind_VarOccur,
  SymbolKind_TypeDecl,
  SymbolKind_TypeOccur,
  SymbolKind_Proc,
  SymbolKind_Call,
}
SymbolKind;

typedef struct Symbol
{
  SymbolKind kind;

  Symbol* prev_symbol;
  char* name;
  int block_id;
  int nesting_depth;
  AstNode* id;

  union {
    AstNode* ast;
    Type* type;
  };
}
Symbol;

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

typedef enum
{
  Opcode__Null,
  Opcode_PUSH,
  Opcode_PUSHF,
  Opcode_POP,
  Opcode_DUP,
  Opcode_LOAD,
  Opcode_LOAD8,
  Opcode_STORE,
  Opcode_STORE8,
  Opcode_ALLOC,

  Opcode_ADD,
  Opcode_SUB,
  Opcode_MUL,
  Opcode_DIV,
  Opcode_MOD,
  Opcode_NEG,
  Opcode_INCR,
  Opcode_DECR,

  Opcode_ADDF,
  Opcode_SUBF,
  Opcode_MULF,
  Opcode_DIVF,
  Opcode_NEGF,

  Opcode_CMPEQ,
  Opcode_CMPNEQ,
  Opcode_CMPLSS,
  Opcode_CMPGRT,
  Opcode_AND,
  Opcode_OR,
  Opcode_NOT,

  Opcode_LABEL,
  Opcode_JUMPZ,
  Opcode_JUMPNZ,
  Opcode_GOTO,
  Opcode_CALL,
  Opcode_RETURN,
  Opcode_ENTER,
  Opcode_LEAVE,
  Opcode_HALT,

  Opcode_NOOP,
  Opcode_PRINT,
  Opcode_PRINTNL,

  Opcode_FLOAT_TO_INT,
  Opcode_INT_TO_FLOAT,
}
Opcode;

typedef struct
{
  int source_line_nr;
  char* string;
}
InstructionLine;

typedef enum
{
  ParamType__Null,
  ParamType_Int32,
  ParamType_Float32,
  ParamType_String,
  ParamType_Reg,
}
ParamType;

typedef enum
{
  RegName__Null,
  RegName_IP,
  RegName_SP,
  RegName_FP
}
RegName;

typedef struct
{
  Opcode opcode;
  ParamType param_type;

  union {
    int32 int_val;
    float32 float_val;
    RegName reg;
    char* str;
  } param;

  int source_line_nr;
}
Instruction;

typedef struct
{
  char groove[4];
  int instr_count;
  uint8* code_start;
  uint code_size;
  Instruction* instr_array;
}
HasmCode;

typedef struct
{
  String text;
  int text_len;
  List instr_list;
  bool32 success;
}
VmProgram;

#define compile_error(SRC, MESSAGE, ...)\
  compile_error_f(__FILE__, __LINE__, (SRC), (MESSAGE), __VA_ARGS__)

bool32 compile_error_f(char* file, int line, SourceLocation* src_loc, char* message, ...);
bool32 get_next_token(TokenStream* input);
void putback_token(TokenStream* input);
void init_token_stream(TokenStream* token_stream, char* text, char* file_path);
void print_char(char buf[3], char raw_char);
bool32 is_literal_token(TokenKind kind);
char* get_token_printstr(Token* token);
char* get_ast_kind_printstr(AstNodeKind kind);
void DEBUG_print_ast_node(String* str, int indent_level, AstNode* node, char* tag);
void DEBUG_print_arena_usage(char* tag);
bool32 parse(TokenStream* input, AstNode** node);
void init_types();
bool32 semantic_analysis(AstNode* ast);
Symbol* lookup_symbol(char* name, SymbolKind kind);
AstNode* new_bin_expr(SourceLocation* src_loc);
AstNode* new_id(SourceLocation* src_loc, char* name);
AstNode* new_var_decl(SourceLocation* src_loc);
AstNode* new_var_occur(SourceLocation* src_loc);
AstNode* new_call(SourceLocation* src_loc);
Type* new_proc_type(Type* args, Type* ret);
Type* new_typevar();
Type* new_pointer_type(Type* pointee);
Type* new_product_type(Type* left, Type* right);
Type* new_array_type(int dim, Type* elem_type);
Type* make_type_of_node_list(List* node_list);
bool32 type_unif(Type* type_a, Type* type_b);
bool32 build_runtime(AstNode* ast);
void codegen(List* code, AstNode* module_ast);
void print_code(VmProgram* vm_program);
bool32 convert_hasm_to_bincode(char* hasm_text, HasmCode** code);


