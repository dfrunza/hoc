#pragma once
#include "lib.h"

#define ARENA_SIZE (3*MEGABYTE)
#define DEBUG_ARENA_SIZE (ARENA_SIZE/3)
#define SYM_ARENA_SIZE (ARENA_SIZE/10)
#define MAX_SCOPE_NESTING_DEPTH 100
#define BINCODE_SIGNATURE "HC"

typedef struct AstNode AstNode;
typedef struct AstBlock AstBlock;
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
  TokenKind_New,
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

  AstOpKind_AddFloat,
  AstOpKind_SubFloat,
  AstOpKind_DivFloat,
  AstOpKind_MulFloat,
  AstOpKind_ModFloat,
  AstOpKind_NegFloat,

  AstOpKind_Assign,
  AstOpKind_PtrDeref,
  AstOpKind_AddressOf,
  AstOpKind_MemberAccess,
  AstOpKind_PtrMemberAccess,
  AstOpKind_PreDecrement,
  AstOpKind_PostDecrement,
  AstOpKind_PreIncrement,
  AstOpKind_PostIncrement,
  
  AstOpKind_Equals,
  AstOpKind_NotEquals,
  AstOpKind_Less,
  AstOpKind_LessEquals,
  AstOpKind_Greater,
  AstOpKind_GreaterEquals,
  AstOpKind_LogicAnd,
  AstOpKind_LogicOr,
  AstOpKind_LogicNot,

  AstOpKind_BitwiseAnd,
  AstOpKind_BitwiseOr,

  AstOpKind_IntToFloat,
  AstOpKind_FloatToInt,

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
  AstNodeKind_New,
  AstNodeKind_Call,
  AstNodeKind_Array,
  AstNodeKind_Pointer,
  AstNodeKind_Struct,
  AstNodeKind_Union,
  AstNodeKind_Enum,
  AstNodeKind_Initializer,
  AstNodeKind_String,
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
}
AstNode,
AstEmptyStmt;

typedef struct
{
  AstNode;

  char* name;

  Symbol* sym;
}
AstId;

typedef struct AstBlock
{
  AstNode;

  List node_list;

  int block_id;
  int nesting_depth;
  AstBlock* encl_block;
  List local_decls;
  List nonlocal_occurs;

  List access_links;
  int links_size;
  int locals_size;
}
AstBlock;

typedef struct
{
  AstNode;

  AstOpKind op;
  AstNode* left_operand;
  AstNode* right_operand;

  char* label_end; // for boolean expressions
}
AstBinExpr;

typedef struct
{
  AstNode;

  AstId* id;
  List args;

  Symbol* proc_sym;
}
AstCall;

typedef struct
{
  AstNode;

  char* file_path;
  AstBlock* body;

  List proc_defs;
  //AstCall* main_call;
}
AstModule,
AstIncludeStmt;

typedef struct
{
  AstNode;

  AstNode* type_expr;
  AstId* id;
  AstNode* init_expr;

  AstBlock* decl_block;
  AstBinExpr* assign_expr;

  DataArea data;
}
AstVarDecl;

typedef struct
{
  AstNode;

  AstNode* id;

  AstVarDecl* var_decl;
  AstBlock* occur_block;
  int decl_block_offset;

  AccessLink* link;
  DataArea* data;
}
AstVarOccur;

typedef struct
{
  AstNode;

  AstNode* ret_type_expr;
  AstId* id;
  List args;
  AstBlock* body;

  AstVarDecl* ret_var;
  bool32 is_decl;

  char* label;
  char* label_end;
  int ret_size;
  int args_size;
}
AstProc;

typedef struct
{
  AstNode;

  AstOpKind op;
  AstNode* operand;
}
AstUnrExpr;

typedef struct
{
  AstNode;

  AstLiteralKind lit_kind;
  union {
    int32 int_val;
    float32 float_val;
    int32 bool_val;
    char char_val;
    char* str;
  };
}
AstLiteral;

typedef struct
{
  AstNode;

  int len;
  char* str;
  DataArea data;
}
AstString;

typedef struct
{
  AstNode;

  AstNode* expr;

  AstProc* proc;
  int nesting_depth;
  AstBinExpr* assign_expr;
}
AstReturnStmt;

typedef struct
{
  AstNode;

  AstId* id;
}
AstGotoStmt,
AstLabel;

typedef struct
{
  AstNode;

  AstNode* cond_expr;
  AstNode* body;
  AstNode* else_body;

  char* label_else;
  char* label_end;
}
AstIfStmt;

typedef struct
{
  AstNode;

  AstNode* cond_expr;
  AstNode* body;

  char* label_eval;
  char* label_break;
}
AstWhileStmt;

typedef struct
{
  AstNode;

  AstVarDecl* decl_expr;
  AstNode* cond_expr;
  AstNode* loop_expr;
  AstBlock* body;

  char* label_eval;
  char* label_break;
}
AstForStmt;

typedef struct
{
  AstNode;

  AstNode* loop;

  int nesting_depth;
}
AstContinueStmt,
AstBreakStmt,
AstLoopCtrl;

typedef struct
{
  AstNode;

  AstNode* type_expr;
  AstNode* expr;
}
AstCast;

typedef struct
{
  AstNode;

  AstNode* type_expr;

  int storage_size;
}
AstNew;

typedef struct
{
  AstNode;

  AstNode* expr;
  AstNode* index;

  int size;
}
AstArray;

typedef struct
{
  AstNode;

  AstNode* expr;
}
AstPointer;

typedef struct
{
  AstNode;

  AstId* id;
  List member_list;
}
AstStruct,
AstUnion,
AstEnum;

typedef struct
{
  AstNode;

  AstNode* left_operand;
  AstNode* right_operand;
}
AstAccessor;

typedef struct
{
  AstNode;

  List member_list;
}
AstInitializer;

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
  AstNode* ast;
  int size; // in VM-words

  union {
    struct {
      BasicTypeKind kind;
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
      int size;
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
  AstBlock* curr_block;
  int block_id;
  int nesting_depth;
  AstBlock* active_blocks[MAX_SCOPE_NESTING_DEPTH];
  int sym_count;
}
SymbolTable;

typedef enum
{
  Opcode__Null,
  Opcode_PUSH_I8,
  Opcode_PUSH_I32,
  Opcode_PUSH_F32,
  Opcode_PUSH_R,
  Opcode_POP,
  Opcode_DUP,
  Opcode_LOAD,
  Opcode_LOAD8,
  Opcode_STORE,
  Opcode_STORE8,
  Opcode_ALLOC,
  Opcode_NEW,

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
  char sig[4];

  int data_offset;
  int data_size;

  int code_offset;
  int code_size;
}
BinCode;

typedef struct
{
  bool32 success;

  String text;
  int text_len;

  List instr_list;
  Instruction* instructions;
  int instr_count;

  uint8* data;
  int data_size;
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
bool32 semantic_analysis(AstModule* ast);
AstString* new_string(SourceLocation* src_loc, char* str);
AstBinExpr* new_bin_expr(SourceLocation* src_loc);
AstCast* new_cast(SourceLocation* src_loc);
AstId* new_id(SourceLocation* src_loc, char* name);
AstNode* clone_ast_node(AstNode* node);
AstVarDecl* new_var_decl(SourceLocation* src_loc);
AstVarOccur* new_var_occur(SourceLocation* src_loc);
AstCall* new_call(SourceLocation* src_loc);
AstUnrExpr* new_unr_expr(SourceLocation* src_loc);
AstLiteral* new_literal(SourceLocation* src_loc);
Type* new_typevar();
Type* new_proc_type(Type* args, Type* ret);
Type* new_pointer_type(Type* pointee);
Type* new_product_type(Type* left, Type* right);
Type* new_array_type(int size, Type* elem_type);
Type* new_cast_type(Type* from_type, Type* to_type);
Type* make_type_of_node_list(List* node_list);
Type* get_type_repr(Type* type);
bool32 type_unif(Type* type_a, Type* type_b);
bool32 types_are_equal(Type* type_a, Type* type_b);
int compute_type_width(Type* type);
void build_runtime(AstModule* ast);
void codegen(List* code, uint8** data, int* data_size, AstModule* module);
void print_code(VmProgram* vm_program);
bool32 convert_hasm_to_instructions(char* hasm_text, VmProgram* code);


