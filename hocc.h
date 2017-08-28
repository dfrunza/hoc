#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

#define ARENA_SIZE (3*MEGABYTE)
#define DEBUG_ARENA_SIZE (ARENA_SIZE/3)
#define SYM_ARENA_SIZE (ARENA_SIZE/10)
#define MAX_SCOPE_NESTING_DEPTH 100
#define BINCODE_SIGNATURE "HC"

typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef int bool32;

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
  uint8* alloc;
  uint8* free;
  uint8* cap;
  struct MemoryArena* host;
  int sub_arena_count;
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

typedef struct ListItem
{
  void* elem;
  struct ListItem* next;
  struct ListItem* prev;
}
ListItem;

typedef struct
{
  ListItem* first;
  ListItem* last;
}
List;

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
  TokenKind_Putc,
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
  AstOpKind_PointerDeref,
  AstOpKind_AddressOf,
  AstOpKind_MemberAccess,
  AstOpKind_PtrMemberAccess,
  AstOpKind_PreDecrement,
  AstOpKind_PostDecrement,
  AstOpKind_PreIncrement,
  AstOpKind_PostIncrement,
  AstOpKind_ArrayIndex,
  
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
  AstNodeKind_Putc,
  AstNodeKind_NodeList,
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

  List list;
  int count;
}
AstNodeList;

typedef struct
{
  AstNode;

  char* file_path;
  AstBlock* body;

  List proc_defs;
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
  AstNodeList args;
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

  AstId* id;
  AstNodeList args;

  Symbol* proc_sym;
}
AstCall;

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
  AstNode* count_expr;

  AstBinExpr* size_expr;
}
AstNew;

typedef struct
{
  AstNode;

  AstNode* expr;
}
AstPutc;

typedef struct
{
  AstNode;

  AstNode* type_expr;
  AstNode* size_expr;

  int size;
}
AstArray;

typedef struct
{
  AstNode;

  AstNode* type_expr;
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
  int size;

  union {
    struct {
      BasicTypeKind kind;
    }
    basic;

    struct {
      Type* pointee;
    }
    pointer;

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

bool32 DEBUG_enabled = true;
bool32 DEBUG_zero_arena = false;
bool32 DEBUG_check_arena_bounds = true;
MemoryArena* DEBUG_arena = 0;

MemoryArena* arena = 0;
MemoryArena* sym_arena = 0;

SourceLocation* deflt_src_loc;
SymbolTable* symtab = 0;

Type* basic_type_bool;
Type* basic_type_int;
Type* basic_type_char;
Type* basic_type_float;
Type* basic_type_void;

