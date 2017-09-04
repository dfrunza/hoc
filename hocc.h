#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

#define ARENA_SIZE (3*MEGABYTE)
#define SYM_ARENA_SIZE (ARENA_SIZE/10)
#define MAX_SCOPE_NESTING_DEPTH 100
#define BINCODE_SIGNATURE "HC"

typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef int boole;

typedef char int8;
typedef short int16;
typedef int int32;
typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned int uint32;
typedef long long int64;
typedef unsigned long long uint64;
typedef float float32;
typedef double float64;

#define KILOBYTE (1024ll)
#define MEGABYTE (1024*KILOBYTE)
#define false 0
#define true  1
#define inline __inline
#define internal static

typedef struct MemoryArena
{
  uint8* base;
  uint8* free;
  uint8* cap;
  struct MemoryArena* prev_arena;
  char* label;
}
MemoryArena;

typedef struct
{
  size_t total_avail;
  double in_use;
}
ArenaUsage;

typedef struct
{
  char* head;
  char* end;
  MemoryArena* arena;
}
String;

#define assert(EXPR)\
  if(!(EXPR)) assert_f(#EXPR, __FILE__, __LINE__)
#define fail(MESSAGE, ...)\
  fail_f(__FILE__, __LINE__, (MESSAGE), __VA_ARGS__)
#define sizeof_array(ARRAY)\
  (sizeof(ARRAY)/sizeof(ARRAY[0]))
#define to_bool(EXPR)\
  ((EXPR) ? true : false)
#define error(MESSAGE, ...)\
  error_f(__FILE__, __LINE__, (MESSAGE), __VA_ARGS__)
#define compile_error(SRC, MESSAGE, ...)\
  compile_error_f(__FILE__, __LINE__, (SRC), (MESSAGE), __VA_ARGS__)

#define mem_push_struct(ARENA, TYPE)\
  ((TYPE*)mem_push_struct_f(ARENA, sizeof(TYPE), 1, true))
#define mem_push_count(ARENA, TYPE, COUNT)\
  ((TYPE*)mem_push_struct_f(ARENA, sizeof(TYPE), COUNT, true))
#define mem_push_count_nz(ARENA, TYPE, COUNT)\
  ((TYPE*)mem_push_struct_f(ARENA, sizeof(TYPE), COUNT, false))
#define mem_zero_struct(VAR, TYPE)\
  (mem_zero_f(VAR, sizeof(TYPE)))

typedef struct
{
  char* file_path;
  int line_nr;
  char* src_line;
}
SourceLocation;

typedef enum
{
  TokenKind__None,
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
  TokenKind_Putc,
  TokenKind_True,
  TokenKind_False,

  TokenKind_Id,
  TokenKind_IntNum,
  TokenKind_FloatNum,
  TokenKind_String,
  TokenKind_Char,

  TokenKind__Count,
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
  CstOpKind__None,

  CstOpKind_Add,
  CstOpKind_Sub,
  CstOpKind_Div,
  CstOpKind_Mul,
  CstOpKind_Mod,
  CstOpKind_Neg,

  CstOpKind_Assign,
  CstOpKind_PointerDeref,
  CstOpKind_AddressOf,
  CstOpKind_MemberAccess,
  CstOpKind_PtrMemberAccess,
  CstOpKind_PreDecrement,
  CstOpKind_PostDecrement,
  CstOpKind_PreIncrement,
  CstOpKind_PostIncrement,
  CstOpKind_ArrayIndex,

  CstOpKind_Equals,
  CstOpKind_NotEquals,
  CstOpKind_Less,
  CstOpKind_LessEquals,
  CstOpKind_Greater,
  CstOpKind_GreaterEquals,
  CstOpKind_LogicAnd,
  CstOpKind_LogicOr,
  CstOpKind_LogicNot,

  CstOpKind_BitwiseAnd,
  CstOpKind_BitwiseOr,

  CstOpKind_IntToFloat,
  CstOpKind_FloatToInt,

  CstOpKind__Count,
}
CstOpKind;

typedef enum
{
  CstNodeKind__None,
  CstNodeKind_CstBinExpr,
  CstNodeKind_CstUnaryExpr,
  CstNodeKind_CstLiteral,
  CstNodeKind_CstVarDecl,
  CstNodeKind_CstVarOccur,
  CstNodeKind_CstBlock,
  CstNodeKind_CstProc,
  CstNodeKind_CstId,
  CstNodeKind_CstStatement,
  CstNodeKind_CstWhileStmt,
  CstNodeKind_CstDoWhileStmt,
  CstNodeKind_CstForStmt,
  CstNodeKind_CstIfStmt,
  CstNodeKind_CstReturnStmt,
  CstNodeKind_CstBreakStmt,
  CstNodeKind_CstContinueStmt,
  CstNodeKind_CstGotoStmt,
  CstNodeKind_CstLabel,
  CstNodeKind_CstInclude,
  CstNodeKind_CstModule,
  CstNodeKind_CstCast,
  CstNodeKind_CstNew,
  CstNodeKind_CstCall,
  CstNodeKind_CstArray,
  CstNodeKind_CstPointer,
  CstNodeKind_CstStruct,
  CstNodeKind_CstUnion,
  CstNodeKind_CstEnum,
  CstNodeKind_CstInitList,
  CstNodeKind_CstString,
  CstNodeKind_CstPutc,
  CstNodeKind_CstEmptyStmt,

  CstNodeKind__Count,
}
CstNodeKind;

typedef enum
{
  CstLiteralKind__None,
  CstLiteralKind_Int,
  CstLiteralKind_Float,
  CstLiteralKind_Bool,
  CstLiteralKind_Char,
  CstLiteralKind_String,
  
  CstLiteralKind__Count,
}
CstLiteralKind;

typedef struct CstNode CstNode; // Concrete Syntax Tree
typedef struct Type Type;
typedef struct Symbol Symbol;

typedef enum
{
  ListKind__None,
  ListKind_Cst,
  ListKind__Count,
}
ListKind;

typedef struct ListItem
{
  ListKind kind;
  union
  {
    void* elem;
    CstNode* cst;
  };
  struct ListItem* next;
  struct ListItem* prev;
}
ListItem;

typedef struct
{
  ListKind kind;
  ListItem* first;
  ListItem* last;
}
List;

typedef struct
{
  char* name;
}
CstId;

typedef struct
{
  CstNode* id;
}
CstGotoStmt,
CstLabel;

typedef struct
{
  List* stmts;
}
CstBlock;

typedef struct
{
  CstOpKind op;
  CstNode* left_operand;
  CstNode* right_operand;
}
CstBinExpr;

typedef struct
{
  CstOpKind op;
  CstNode* operand;
}
CstUnaryExpr;

typedef struct
{
  char* file_path;
  CstNode* body;
}
CstModule,
CstInclude;

typedef struct
{
  CstNode* stmt;
}
CstStatement;

typedef struct
{
  CstNode* type_expr;
  CstNode* id;
  CstNode* init_expr;
}
CstVarDecl;

typedef struct
{
  CstNode* ret_type_expr;
  CstNode* id;
  List* args;
  CstNode* body;
  boole is_decl;
}
CstProc;

typedef struct
{
  CstNode* id;
  List* args;
}
CstCall;

typedef struct
{
  CstLiteralKind kind;

  union
  {
    int32 int_val;
    float32 float_val;
    int32 bool_val;
    char char_val;
    char* str;
  };
}
CstLiteral;

typedef struct
{
  CstNode* expr;
}
CstReturnStmt;

typedef struct
{
  CstNode* cond_expr;
  CstNode* body;
  CstNode* else_body;
}
CstIfStmt;

typedef struct
{
  CstNode* cond_expr;
  CstNode* body;
}
CstWhileStmt,
CstDoWhileStmt;

typedef struct
{
  CstNode* decl_expr;
  CstNode* cond_expr;
  CstNode* loop_expr;
  CstNode* body;
}
CstForStmt;

typedef struct
{
  CstNode* type_expr;
  CstNode* expr;
}
CstCast;

typedef struct
{
  CstNode* type_expr;
  CstNode* size_expr;
}
CstArray;

typedef struct
{
  CstNode* type_expr;
}
CstPointer;

typedef struct
{
  CstNode* id;
  List* members;
}
CstStruct,
CstUnion,
CstEnum,
CstRecord;

typedef struct
{
  List* members;
}
CstInitList;

typedef struct
{
  CstNode* type_expr;
  CstNode* count_expr;
}
CstNew;

typedef struct
{
  CstNode* expr;
}
CstPutc;

typedef struct CstNode
{
  CstNodeKind kind;
  Type* type;
  SourceLocation src_loc;

  union
  {
    CstId id;
    CstBlock block;
    CstBinExpr bin_expr;
    CstUnaryExpr unary_expr;
    CstModule module;
    CstInclude include;
    CstStatement stmt;
    CstVarDecl var_decl;
    CstProc proc;
    CstCall call;
    CstLiteral lit;
    CstReturnStmt ret_stmt;
    CstGotoStmt goto_stmt;
    CstLabel label;
    CstIfStmt if_stmt;
    CstWhileStmt while_stmt;
    CstForStmt for_stmt;
    CstCast cast;
    CstArray array;
    CstPointer pointer;
    CstEnum enum_decl;
    CstStruct struct_decl;
    CstUnion union_decl;
    CstBinExpr member_access;
    CstInitList init_list;
    CstNew new;
    CstPutc putc;
  };
}
CstNode;

typedef enum
{
  TypeKind__None,
  TypeKind_TypeVar,
  TypeKind_Unary,
  TypeKind_Basic,
  TypeKind_Type,
  TypeKind_Proc,
  TypeKind_Product,
  TypeKind_Pointer,
  TypeKind_Array,

  TypeKind__Count,
}
TypeKind;

typedef enum
{
  UnaryCtorKind__None,
  UnaryCtorKind_Pointer,
  UnaryCtorKind_Array,
}
UnaryCtorKind;

typedef enum
{
  BasicTypeKind__None,
  BasicTypeKind_Void,
  BasicTypeKind_Int,
  BasicTypeKind_Float,
  BasicTypeKind_Char,
  BasicTypeKind_Bool,

  BasicTypeKind__Count,
}
BasicTypeKind;

typedef struct Type
{
  TypeKind kind;
  Type* repr_type; // representative member of the set of equivalent types
  CstNode* ast;
  int size;

  union
  {
    struct
    {
      BasicTypeKind kind;
    }
    basic;

    struct
    {
      Type* pointee;
    }
    pointer;

    struct
    {
      Type* args;
      Type* ret;
    }
    proc;

    struct
    {
      Type* left;
      Type* right;
    }
    product;

    struct
    {
      int size;
      Type* elem;
    }
    array;

    struct
    {
      int id;
    }
    typevar;
  };
}
Type;

typedef enum
{
  SymbolKind__None,
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
  CstNode* id;

  union
  {
    CstNode* ast;
    Type* type;
  };
}
Symbol;

typedef struct
{
  Symbol* curr_symbol;
  CstBlock* curr_block;
  int block_id;
  int nesting_depth;
  CstBlock* active_blocks[MAX_SCOPE_NESTING_DEPTH];
  int sym_count;
}
SymbolTable;

typedef enum
{
  Opcode__None,
  Opcode_PUSH_CHAR,
  Opcode_PUSH_INT,
  Opcode_PUSH_FLOAT,
  Opcode_PUSH_REG,
  Opcode_POP_REG,
  Opcode_DUP,
  Opcode_LOAD,
  Opcode_STORE,
  Opcode_GROW,
  Opcode_NEW,

  Opcode_ADD_INT,
  Opcode_SUB_INT,
  Opcode_MUL_INT,
  Opcode_DIV_INT,
  Opcode_MOD_INT,
  Opcode_NEG_INT,
  Opcode_INCR_INT,
  Opcode_DECR_INT,

  Opcode_ADD_FLOAT,
  Opcode_SUB_FLOAT,
  Opcode_MUL_FLOAT,
  Opcode_DIV_FLOAT,
  Opcode_NEG_FLOAT,

  Opcode_CMPEQ_CHAR,
  Opcode_CMPNEQ_CHAR,
  Opcode_CMPLSS_CHAR,
  Opcode_CMPGRT_CHAR,
  Opcode_CMPEQ_INT,
  Opcode_CMPNEQ_INT,
  Opcode_CMPLSS_INT,
  Opcode_CMPGRT_INT,
  Opcode_CMPEQ_FLOAT,
  Opcode_CMPNEQ_FLOAT,
  Opcode_CMPLSS_FLOAT,
  Opcode_CMPGRT_FLOAT,
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
  Opcode_PUTC,

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
  ParamType__None,
  ParamType_Int32,
  ParamType_Float32,
  ParamType_String,
  ParamType_Reg,
}
ParamType;

typedef enum
{
  RegName__None,
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
  boole success;

  String text;
  int text_len;

  List instr_list;
  Instruction* instructions;
  int instr_count;

  uint8* data;
  int data_size;
}
VmProgram;

boole DEBUG_enabled = true;
boole DEBUG_zero_arena = true;
boole DEBUG_check_arena_bounds = true;

MemoryArena* arena = 0;
MemoryArena* sym_arena = 0;

SourceLocation* deflt_src_loc = 0;
SymbolTable* symtab = 0;

Type* basic_type_bool;
Type* basic_type_int;
Type* basic_type_char;
Type* basic_type_float;
Type* basic_type_void;

