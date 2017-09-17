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

#define mem_push_array(ARENA, TYPE, COUNT)\
  ((TYPE*)mem_push_struct_f(ARENA, sizeof(TYPE), COUNT, true))

#define mem_push_array_nz(ARENA, TYPE, COUNT)\
  ((TYPE*)mem_push_struct_f(ARENA, sizeof(TYPE), COUNT, false))

#define mem_zero_struct(VAR, TYPE)\
  (mem_zero_f(VAR, sizeof(TYPE)))

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

#ifndef OperatorKind_MEMBER_LIST
#define OperatorKind_MEMBER_LIST()\
  ENUM_MEMBER(OperatorKind__None),\
  ENUM_MEMBER(OperatorKind_Add),\
  ENUM_MEMBER(OperatorKind_Sub),\
  ENUM_MEMBER(OperatorKind_Mul),\
  ENUM_MEMBER(OperatorKind_Div),\
  ENUM_MEMBER(OperatorKind_Mod),\
  ENUM_MEMBER(OperatorKind_Neg),\
  ENUM_MEMBER(OperatorKind_Assign),\
  ENUM_MEMBER(OperatorKind_PointerDeref),\
  ENUM_MEMBER(OperatorKind_AddressOf),\
  ENUM_MEMBER(OperatorKind_MemberSelect),\
  ENUM_MEMBER(OperatorKind_PtrMemberSelect),\
  ENUM_MEMBER(OperatorKind_PreDecrement),\
  ENUM_MEMBER(OperatorKind_PostDecrement),\
  ENUM_MEMBER(OperatorKind_PreIncrement),\
  ENUM_MEMBER(OperatorKind_PostIncrement),\
  ENUM_MEMBER(OperatorKind_ArrayIndex),\
  ENUM_MEMBER(OperatorKind_Equals),\
  ENUM_MEMBER(OperatorKind_NotEquals),\
  ENUM_MEMBER(OperatorKind_Less),\
  ENUM_MEMBER(OperatorKind_LessEquals),\
  ENUM_MEMBER(OperatorKind_Greater),\
  ENUM_MEMBER(OperatorKind_GreaterEquals),\
  ENUM_MEMBER(OperatorKind_LogicAnd),\
  ENUM_MEMBER(OperatorKind_LogicOr),\
  ENUM_MEMBER(OperatorKind_LogicNot),\
  ENUM_MEMBER(OperatorKind_BitwiseAnd),\
  ENUM_MEMBER(OperatorKind_BitwiseOr),\
  ENUM_MEMBER(OperatorKind__Count),
#endif

enum OperatorKind
{
#define ENUM_MEMBER(NAME) NAME
  OperatorKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

#ifndef LiteralKind_MEMBER_LIST
#define LiteralKind_MEMBER_LIST()\
  ENUM_MEMBER(LiteralKind__None),\
  ENUM_MEMBER(LiteralKind_int_val),\
  ENUM_MEMBER(LiteralKind_float_val),\
  ENUM_MEMBER(LiteralKind_bool_val),\
  ENUM_MEMBER(LiteralKind_char_val),\
  ENUM_MEMBER(LiteralKind_str),
#endif

enum LiteralKind
{
#define ENUM_MEMBER(NAME) NAME
  LiteralKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

#ifndef AstKind1_MEMBER_LIST
#define AstKind1_MEMBER_LIST()\
  ENUM_MEMBER(AstKind1__None),\
  ENUM_MEMBER(AstKind1_bin_expr),\
  ENUM_MEMBER(AstKind1_un_expr),\
  ENUM_MEMBER(AstKind1_lit),\
  ENUM_MEMBER(AstKind1_var),\
  ENUM_MEMBER(AstKind1_block),\
  ENUM_MEMBER(AstKind1_proc),\
  ENUM_MEMBER(AstKind1_id),\
  ENUM_MEMBER(AstKind1_stmt),\
  ENUM_MEMBER(AstKind1_while_stmt),\
  ENUM_MEMBER(AstKind1_do_while_stmt),\
  ENUM_MEMBER(AstKind1_for_stmt),\
  ENUM_MEMBER(AstKind1_if_stmt),\
  ENUM_MEMBER(AstKind1_return_stmt),\
  ENUM_MEMBER(AstKind1_break_stmt),\
  ENUM_MEMBER(AstKind1_continue_stmt),\
  ENUM_MEMBER(AstKind1_goto_stmt),\
  ENUM_MEMBER(AstKind1_label),\
  ENUM_MEMBER(AstKind1_include),\
  ENUM_MEMBER(AstKind1_module),\
  ENUM_MEMBER(AstKind1_cast),\
  ENUM_MEMBER(AstKind1_call),\
  ENUM_MEMBER(AstKind1_array),\
  ENUM_MEMBER(AstKind1_pointer),\
  ENUM_MEMBER(AstKind1_struct_decl),\
  ENUM_MEMBER(AstKind1_union_decl),\
  ENUM_MEMBER(AstKind1_enum_decl),\
  ENUM_MEMBER(AstKind1_init_list),\
  ENUM_MEMBER(AstKind1_string),\
  ENUM_MEMBER(AstKind1_hoc_new),\
  ENUM_MEMBER(AstKind1_hoc_putc),\
  ENUM_MEMBER(AstKind1__Count),
#endif

enum AstKind1
{
#define ENUM_MEMBER(NAME) NAME
  AstKind1_MEMBER_LIST()
#undef ENUM_MEMBER
};

typedef struct AstNode1
{
  enum AstKind1 kind;
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
      enum OperatorKind op;
      AstNode1* left_operand;
      AstNode1* right_operand;
    }
    bin_expr;

    struct
    {
      enum OperatorKind op;
      AstNode1* operand;
    }
    un_expr;

    struct
    {
      char* file_path;
      AstNode1* body;
    }
    module,
    include;

    struct
    {
      AstNode1* stmt;
    }
    stmt;

    struct
    {
      AstNode1* type_expr;
      AstNode1* id;
      AstNode1* init_expr;
    }
    var;

    struct
    {
      AstNode1* ret_type_expr;
      AstNode1* id;
      List* args;
      AstNode1* body;
    }
    proc;

    struct
    {
      AstNode1* id;
      List* args;
    }
    call;

    struct
    {
      enum LiteralKind kind;

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
      AstNode1* expr;
    }
    return_stmt;

    struct
    {
      AstNode1* id;
    }
    goto_stmt,
    label;

    struct
    {
      AstNode1* cond_expr;
      AstNode1* body;
      AstNode1* else_body;
    }
    if_stmt;

    struct
    {
      AstNode1* cond_expr;
      AstNode1* body;
    }
    while_stmt,
    do_while_stmt;

    struct
    {
      AstNode1* decl_expr;
      AstNode1* cond_expr;
      AstNode1* loop_expr;
      AstNode1* body;
    }
    for_stmt;

    struct
    {
      AstNode1* type_expr;
      AstNode1* expr;
    }
    cast,
    pointer;

    struct
    {
      AstNode1* type_expr;
      AstNode1* size_expr;
    }
    array;

    struct
    {
      AstNode1* id;
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
      AstNode1* type_expr;
      AstNode1* count_expr;
    }
    hoc_new;

    struct
    {
      AstNode1* expr;
    }
    hoc_putc;
  };
}
AstNode1;

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

#if 0
#ifndef AstBuiltinProc_MEMBER_LIST
#define AstBuiltinProc_MEMBER_LIST()\
  ENUM_MEMBER(AstBuiltinProc_, _None),\
  ENUM_MEMBER(AstBuiltinProc_, assign),\
  ENUM_MEMBER(AstBuiltinProc_, add_int),\
  ENUM_MEMBER(AstBuiltinProc_, sub_int),\
  ENUM_MEMBER(AstBuiltinProc_, div_int),\
  ENUM_MEMBER(AstBuiltinProc_, mul_int),\
  ENUM_MEMBER(AstBuiltinProc_, neg_int),\
  ENUM_MEMBER(AstBuiltinProc_, add_float),\
  ENUM_MEMBER(AstBuiltinProc_, sub_float),\
  ENUM_MEMBER(AstBuiltinProc_, div_float),\
  ENUM_MEMBER(AstBuiltinProc_, mul_float),\
  ENUM_MEMBER(AstBuiltinProc_, neg_float),\
  ENUM_MEMBER(AstBuiltinProc_, float_to_int),\
  ENUM_MEMBER(AstBuiltinProc_, int_to_float),
#endif

enum AstBuiltinProc
{
#define ENUM_MEMBER(PREFIX, NAME) PREFIX##NAME
  AstBuiltinProc_MEMBER_LIST()
#undef ENUM_MEMBER
};
#endif

#if 0
#ifndef AstOperatorKind_MEMBER_LIST
#define AstOperatorKind_MEMBER_LIST()\
  ENUM_MEMBER(AstOperatorKind_Assign),\
  ENUM_MEMBER(AstOperatorKind_Add),\
  ENUM_MEMBER(AstOperatorKind_Sub),\
  ENUM_MEMBER(AstOperatorKind_Mul),\
  ENUM_MEMBER(AstOperatorKind_Div),\
  ENUM_MEMBER(AstOperatorKind_Neg),
#endif

enum AstOperatorKind
{
#define ENUM_MEMBER(NAME) NAME
  AstOperatorKind_MEMBER_LIST()
#undef ENUM_MEMBER
};
#endif

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

#ifndef AstKind2_MEMBER_LIST
#define AstKind2_MEMBER_LIST()\
  ENUM_MEMBER(AstKind2__None),\
  ENUM_MEMBER(AstKind2_block),\
  ENUM_MEMBER(AstKind2_module),\
  ENUM_MEMBER(AstKind2_stmt),\
  ENUM_MEMBER(AstKind2_binop_decl),\
  ENUM_MEMBER(AstKind2_unop_decl),\
  ENUM_MEMBER(AstKind2_binop_occur),\
  ENUM_MEMBER(AstKind2_unop_occur),\
  ENUM_MEMBER(AstKind2_var_decl),\
  ENUM_MEMBER(AstKind2_var_occur),\
  ENUM_MEMBER(AstKind2_type_decl),\
  ENUM_MEMBER(AstKind2_type_occur),\
  ENUM_MEMBER(AstKind2_proc_decl),\
  ENUM_MEMBER(AstKind2_proc_occur),\
  ENUM_MEMBER(AstKind2_if_stmt),\
  ENUM_MEMBER(AstKind2_while_stmt),\
  ENUM_MEMBER(AstKind2_do_while_stmt),\
  ENUM_MEMBER(AstKind2_return_stmt),\
  ENUM_MEMBER(AstKind2_continue_stmt),\
  ENUM_MEMBER(AstKind2_break_stmt),\
  ENUM_MEMBER(AstKind2__Count),
#endif

enum AstKind2
{
#define ENUM_MEMBER(NAME) NAME
  AstKind2_MEMBER_LIST()
#undef ENUM_MEMBER
};

enum AstProcKind
{
  AstProcKind__None,
  AstProcKind_UnOp,
  AstProcKind_BinOp,
  AstProcKind_User,
};

// Abstract Syntax Tree
typedef struct AstNode2
{
  enum AstKind2 kind;
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
      AstNode2* body;
      List* procs;
    }
    module;

    struct
    {
      AstNode2* stmt;
    }
    stmt;

    struct
    {
      enum OperatorKind kind;
    }
    binop_decl,
    unop_decl;

    struct
    {
      //enum OperatorKind kind;
      AstNode2* left_operand;
      AstNode2* right_operand;
      AstNode2* op_decl;
    }
    binop_occur;

    struct
    {
      //enum OperatorKind kind;
      AstNode2* operand;
      AstNode2* op_decl;
    }
    unop_occur;

    struct
    {
      char* var_name;
      Scope* decl_scope;
    }
    var_decl;

    struct
    {
      char* var_name;
      AstNode2* var_decl;
      Scope* occur_scope;
      int decl_scope_offset;
    }
    var_occur;

    struct
    {
      char* type_name;
      Scope* decl_scope;
      AstNode2* type;
    }
    type_decl;

    struct
    {
      char* type_name;
      AstNode2* type_decl;
    }
    type_occur;

    struct
    {
#if 0
      AstProcKind kind;
      enum AstBuiltinProc builtin_id;
#endif
      char* proc_name;
      AstNode2* body;
      List* formal_args;
      AstNode2* ret_var;
    }
    proc_decl;

    struct
    {
      char* proc_name;
      List* actual_args;
      AstNode2* proc_decl;
    }
    proc_occur;

    struct
    {
      AstNode2* assign_stmt;
      AstNode2* proc;
      int nesting_depth;
    }
    return_stmt;

    struct
    {
      AstNode2* cond_expr;
      AstNode2* body;
      AstNode2* else_body;
    }
    if_stmt;

    struct
    {
      AstNode2* cond_expr;
      AstNode2* body;
    }
    while_stmt,
    do_while_stmt;

    struct
    {
      AstNode2* loop;
      int nesting_depth;
    }
    continue_stmt,
    break_stmt;
  };
}
AstNode2;

enum NodeKind
{
  NodeKind__None,
  NodeKind_ast,
  NodeKind_cst,
};

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
  AstNode2* ast;
  int size; // -> width

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
  SymbolKind_unresolved,
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
    AstNode2* var_decl;
    AstNode2* var_occur;
    AstNode2* type_decl;
    AstNode2* type_occur;
    AstNode2* proc_decl;
    AstNode2* proc_occur;
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
  ListKind_ast1_node,
  ListKind_ast2_node,
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
    AstNode1* ast1_node;
    AstNode2* ast2_node;
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
void DEBUG_print_ast_node_list(String* str, int indent_level, char* tag, List* node_list);

