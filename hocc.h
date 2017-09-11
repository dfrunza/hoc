#include <stdio.h>
#include <windows.h>

#define ARENA_SIZE (3*MEGABYTE)
#define SYMBOL_TABLE_ARENA_SIZE (ARENA_SIZE/10)
#define MAX_SCOPE_NESTING_DEPTH 100
#define BINCODE_SIGNATURE "HC"

typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
//typedef int boole;

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

#define stringify(S) #S

#define OBJ(VAR, KIND, NAME)\
  (((VAR)->kind == KIND##_##NAME) ? &(VAR)->NAME : 0)

typedef struct List List;

typedef struct
{
  char* file_path;
  int line_nr;
  char* src_line;
}
SourceLoc;

enum TokenKind
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
};

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

  SourceLoc src_loc;
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

typedef struct CstNode CstNode; // Concrete Syntax Tree

enum CstOperator
{
  CstOperator__None,

  CstOperator_Add,
  CstOperator_Sub,
  CstOperator_Div,
  CstOperator_Mul,
  CstOperator_Mod,
  CstOperator_Neg,

  CstOperator_Assign,
  CstOperator_PointerDeref,
  CstOperator_AddressOf,
  CstOperator_MemberSelect,
  CstOperator_PtrMemberSelect,
  CstOperator_PreDecrement,
  CstOperator_PostDecrement,
  CstOperator_PreIncrement,
  CstOperator_PostIncrement,
  CstOperator_ArrayIndex,

  CstOperator_Equals,
  CstOperator_NotEquals,
  CstOperator_Less,
  CstOperator_LessEquals,
  CstOperator_Greater,
  CstOperator_GreaterEquals,
  CstOperator_LogicAnd,
  CstOperator_LogicOr,
  CstOperator_LogicNot,

  CstOperator_BitwiseAnd,
  CstOperator_BitwiseOr,
};

enum BuiltinProc
{
  BuiltinProc__None,
  BuiltinProc_Assign,
  BuiltinProc_Add,
  BuiltinProc_Sub,
  BuiltinProc_Mul,
  BuiltinProc_Div,
};

enum CstLiteralKind
{
  CstLiteralKind__None,
  CstLiteralKind_int_val,
  CstLiteralKind_float_val,
  CstLiteralKind_bool_val,
  CstLiteralKind_char_val,
  CstLiteralKind_str,
};

enum CstKind
{
  CstKind__None,
  CstKind_bin_expr,
  CstKind_un_expr,
  CstKind_lit,
  CstKind_var_decl,
  CstKind_var_occur,
  CstKind_block,
  CstKind_proc,
  CstKind_id,
  CstKind_stmt,
  CstKind_while_stmt,
  CstKind_do_while_stmt,
  CstKind_for_stmt,
  CstKind_if_stmt,
  CstKind_return_stmt,
  CstKind_break_stmt,
  CstKind_continue_stmt,
  CstKind_goto_stmt,
  CstKind_label,
  CstKind_include,
  CstKind_module,
  CstKind_cast,
  CstKind_call,
  CstKind_array,
  CstKind_pointer,
  CstKind_struct_decl,
  CstKind_union_decl,
  CstKind_enum_decl,
  CstKind_init_list,
  CstKind_string,
  CstKind_hoc_new,
  CstKind_hoc_putc,

  CstKind__Count,
};

typedef struct CstNode
{
  enum CstKind kind;
  SourceLoc* src_loc;

  union
  {
    struct
    {
      char* name;
    }
    id;

    struct
    {
      List* nodes;
    }
    block;

    struct
    {
      enum CstOperator op;
      CstNode* left_operand;
      CstNode* right_operand;
    }
    bin_expr;

    struct
    {
      enum CstOperator op;
      CstNode* operand;
    }
    un_expr;

    struct
    {
      char* file_path;
      CstNode* body;
    }
    module,
    include;

    struct
    {
      CstNode* stmt;
    }
    stmt;

    struct
    {
      CstNode* type_expr;
      CstNode* id;
      CstNode* init_expr;
    }
    var_decl;

    struct
    {
      CstNode* ret_type_expr;
      CstNode* id;
      List* args;
      CstNode* body;
      bool is_decl;
    }
    proc;

    struct
    {
      CstNode* id;
      List* args;
    }
    call;

    struct
    {
      enum CstLiteralKind kind;

      union
      {
        int32 int_val;
        float32 float_val;
        int32 bool_val;
        char char_val;
        char* str;
      };
    }
    lit;

    struct
    {
      CstNode* expr;
    }
    return_stmt;

    struct
    {
      CstNode* id;
    }
    goto_stmt,
    label;

    struct
    {
      CstNode* cond_expr;
      CstNode* body;
      CstNode* else_body;
    }
    if_stmt;

    struct
    {
      CstNode* cond_expr;
      CstNode* body;
    }
    while_stmt,
    do_while_stmt;

    struct
    {
      CstNode* decl_expr;
      CstNode* cond_expr;
      CstNode* loop_expr;
      CstNode* body;
    }
    for_stmt;

    struct
    {
      CstNode* type_expr;
      CstNode* expr;
    }
    cast,
    pointer;

    struct
    {
      CstNode* type_expr;
      CstNode* size_expr;
    }
    array;

    struct
    {
      CstNode* id;
      List* members;
    }
    enum_decl,
    struct_decl,
    union_decl;

    struct
    {
      List* members;
    }
    init_list;

    struct
    {
      CstNode* type_expr;
      CstNode* count_expr;
    }
    hoc_new;

    struct
    {
      CstNode* expr;
    }
    hoc_putc;
  };
}
CstNode;

typedef struct AstNode AstNode; // Abstract Syntax Tree
typedef struct Type Type;
typedef struct Symbol Symbol;

#if 0
typedef enum ScopeKind
{
  ScopeKind__None,
  ScopeKind_module,
  ScopeKind_block,
  ScopeKind_proc,
}
ScopeKind;
#endif

#if 1
typedef struct Scope
{
  //ScopeKind kind;

  Symbol* last_symbol;
  int scope_id;
  int nesting_depth;
  struct Scope* encl_scope;
  List* local_decls;
  List* nonlocal_occurs;
}
Scope;
#else
typedef struct AstBlock
{
  int block_id;
  int nesting_depth;
  struct AstBlock* encl_block;
  List* local_decls;
  List* nonlocal_occurs;
}
AstBlock;
#endif

enum AstKind
{
  AstKind__None,
  AstKind_block,
  AstKind_module,
  AstKind_stmt,
  AstKind_var_decl,
  AstKind_var_occur,
  AstKind_type_decl,
  AstKind_type_occur,
  AstKind_proc_decl,
  AstKind_proc_occur,
  AstKind_op_occur,
  AstKind_if_stmt,
  AstKind_while_stmt,
  AstKind_do_while_stmt,
  AstKind_return_stmt,
  AstKind_continue_stmt,
  AstKind_break_stmt,
};

typedef struct AstNode
{
  enum AstKind kind;
  Type* type;
  SourceLoc* src_loc;

  union
  {
    struct
    {
      List* stmts;
      Scope* scope;
    }
    block;

    struct
    {
      AstNode* body;
      List* procs;
    }
    module;

    struct
    {
      AstNode* stmt;
    }
    stmt;

    struct
    {
      char* var_name;
      Scope* decl_scope;
    }
    var_decl;

    struct
    {
      char* var_name;
      AstNode* var_decl;
      Scope* occur_scope;
      int decl_scope_offset;
    }
    var_occur;

    struct
    {
      char* type_name;
      Scope* decl_scope;
      AstNode* type;
    }
    type_decl;

    struct
    {
      char* type_name;
      AstNode* type_decl;
    }
    type_occur;

    struct
    {
      char* proc_name;
      AstNode* body;
      List* formal_args;
      AstNode* ret_var;
    }
    proc_decl;

    struct
    {
      enum BuiltinProc proc;
    }
    builtin_proc_decl;

    struct
    {
      char* proc_name;
      List* actual_args;
      AstNode* proc_decl;
    }
    proc_occur;

    struct
    {
      AstNode* assign_stmt;
      AstNode* proc;
      int nesting_depth;
    }
    return_stmt;

    struct
    {
      AstNode* cond_expr;
      AstNode* body;
      AstNode* else_body;
    }
    if_stmt;

    struct
    {
      AstNode* cond_expr;
      AstNode* body;
    }
    while_stmt,
    do_while_stmt;

    struct
    {
      AstNode* loop;
      int nesting_depth;
    }
    continue_stmt,
    break_stmt;
  };
}
AstNode;

enum TypeKind
{
  TypeKind__None,
  TypeKind_typevar,
  TypeKind_unary,
  TypeKind_basic,
  TypeKind_type,
  TypeKind_proc,
  TypeKind_product,
  TypeKind_pointer,
  TypeKind_array,
};

enum UnaryCtorKind
{
  UnaryCtorKind__None,
  UnaryCtorKind_pointer,
  UnaryCtorKind_array,
};

enum BasicTypeKind
{
  BasicTypeKind__None,
  BasicTypeKind_Void,
  BasicTypeKind_Int,
  BasicTypeKind_Float,
  BasicTypeKind_Char,
  BasicTypeKind_Bool,
};

typedef struct Type
{
  TypeKind kind;
  Type* repr_type; // representative member of the set of equivalent types
  AstNode* ast;
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

typedef struct
{
  Type* key;
  Type* value;
}
TypePair;

typedef enum
{
  SymbolKind__None,
  SymbolKind_var_decl,
  SymbolKind_var_occur,
  SymbolKind_type_decl,
  SymbolKind_type_occur,
  SymbolKind_proc_decl,
  SymbolKind_proc_occur,
}
SymbolKind;

typedef struct Symbol
{
  SymbolKind kind;

  Symbol* prev_symbol;
  char* name;
  int scope_id;
  int nesting_depth;

  union
  {
    AstNode* var_decl;
    AstNode* var_occur;
    AstNode* type_decl;
    AstNode* type_occur;
    AstNode* proc_decl;
    AstNode* proc_occur;
  };
}
Symbol;

typedef struct
{
  Scope* global_scope;
  //Scope* module_scope;
  Scope* local_scope;
  int scope_id;
  int nesting_depth;
  Scope* active_scopes[MAX_SCOPE_NESTING_DEPTH];
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
  bool success;

  String text;
  int text_len;

  List* instr_list;
  Instruction* instructions;
  int instr_count;

  uint8* data;
  int data_size;
}
VmProgram;

typedef enum
{
  ListKind__None,
  ListKind_cst_node,
  ListKind_ast_node,
  ListKind_VmInstr,
  ListKind_TypePair,
}
ListKind;

typedef struct ListItem
{
  ListKind kind;
  union
  {
    void* elem;
    CstNode* cst_node;
    AstNode* ast_node;
    Instruction* instr;
    TypePair* type_pair;
  };
  struct ListItem* next;
  struct ListItem* prev;
}
ListItem;

typedef struct List
{
  ListKind kind;
  ListItem* first;
  ListItem* last;
}
List;

void DEBUG_print_line(String* str, int indent_level, char* message, ...);
void DEBUG_print_xst_node_list(String* str, int indent_level, char* tag, List* node_list);

