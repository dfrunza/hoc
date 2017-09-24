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
  while(!(EXPR)) { assert_f(#EXPR, __FILE__, __LINE__); }

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

typedef enum TokenKind
{
  Token__None,
  /* 'Simple' tokens must be listed at the beginning of the enum */
  Token_Dot,
  Token_ArrowRight,
  Token_OpenBracket,
  Token_CloseBracket,
  Token_OpenParens,
  Token_CloseParens,
  Token_OpenBrace,
  Token_CloseBrace,
  Token_Semicolon,
  Token_Colon,
  Token_Comma,
  Token_Percent,
  Token_Star,
  Token_FwdSlash,
  Token_BackSlash,
  Token_Plus,
  Token_PlusPlus,
  Token_Minus,
  Token_MinusMinus,
  Token_Exclam,
  Token_ExclamEquals,
  Token_Equals,
  Token_EqualsEquals,
  Token_AngleRight,
  Token_AngleRightEquals,
  Token_AngleLeft,
  Token_AngleLeftEquals,
  Token_Ampersand,
  Token_AmpersandAmpersand,
  Token_Pipe,
  Token_PipePipe,
  Token_Unknown,
  Token_EndOfInput,

  Token_Var,
  Token_If,
  Token_Else,
  Token_While,
  Token_For,
  Token_Proc,
  Token_Struct,
  Token_Union,
  Token_Return,
  Token_Break,
  Token_Continue,
  Token_Goto,
  Token_Include,
  Token_Enum,
  Token_Cast,
  Token_New,
  Token_Putc,
  Token_True,
  Token_False,

  Token_Id,
  Token_IntNum,
  Token_FloatNum,
  Token_String,
  Token_Char,
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
  ENUM_MEMBER(OperatorKind_BitwiseOr),
#endif

typedef enum OperatorKind
{
#define ENUM_MEMBER(NAME) NAME
  OperatorKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

char* OperatorKind_strings[] =
{
#define ENUM_MEMBER(NAME) #NAME
  OperatorKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

char*
get_operator_kind_printstr(OperatorKind op)
{
  return OperatorKind_strings[op];
}

#ifndef LiteralKind_MEMBER_LIST
#define LiteralKind_MEMBER_LIST()\
  ENUM_MEMBER(Literal__None),\
  ENUM_MEMBER(Literal_int_val),\
  ENUM_MEMBER(Literal_float_val),\
  ENUM_MEMBER(Literal_bool_val),\
  ENUM_MEMBER(Literal_char_val),\
  ENUM_MEMBER(Literal_str),
#endif

typedef enum LiteralKind
{
#define ENUM_MEMBER(NAME) NAME
  LiteralKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

char* LiteralKind_strings[] =
{
#define ENUM_MEMBER(NAME) #NAME
  LiteralKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

char*
get_literal_kind_printstr(LiteralKind kind)
{
  return LiteralKind_strings[kind];
}

typedef struct Type Type;
typedef struct AstNode AstNode;
typedef struct Symbol Symbol;

#ifndef ScopeKind_MEMBER_LIST
#define ScopeKind_MEMBER_LIST()\
  ENUM_MEMBER(ScopeKind__None),\
  ENUM_MEMBER(ScopeKind_Global),\
  ENUM_MEMBER(ScopeKind_Module),\
  ENUM_MEMBER(ScopeKind_Proc),\
  ENUM_MEMBER(ScopeKind_Loop),\
  ENUM_MEMBER(ScopeKind_Block),
#endif

typedef enum ScopeKind
{
#define ENUM_MEMBER(NAME) NAME
  ScopeKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

char* ScopeKind_strings[] =
{
#define ENUM_MEMBER(NAME) #NAME
  ScopeKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

char*
get_scope_kind_printstr(ScopeKind kind)
{
  return ScopeKind_strings[kind];
}

typedef struct Scope
{
  ScopeKind kind;

  Symbol* last_symbol;
  int scope_id;
  int nesting_depth;
  struct Scope* encl_scope;
}
Scope;

#define ATTR(NODE, KIND, NAME)\
  (get_ast_attribute_safe((NODE), AstAttribute_##KIND, AstAttributeName_##NAME)->KIND)

typedef enum AstAttributeKind
{
  AstAttribute__None,
  AstAttribute_ast_node,
  AstAttribute_int_val,
  AstAttribute_float_val,
  AstAttribute_char_val,
  AstAttribute_bool_val,
  AstAttribute_op_kind,
  AstAttribute_str,
  AstAttribute_list,
  AstAttribute_lit_kind,
  AstAttribute_scope,
};

typedef enum AstAttributeName
{
  AstAttributeName__None,
  AstAttributeName_procs,
  AstAttributeName_stmts,
  AstAttributeName_scope,
  AstAttributeName_decl_scope,
  AstAttributeName_occur_scope,
  AstAttributeName_decl_scope_offset,
  AstAttributeName_proc_decl,
  AstAttributeName_type_decl,
  AstAttributeName_var_decl,
  AstAttributeName_operand,
  AstAttributeName_left_operand,
  AstAttributeName_right_operand,
  AstAttributeName_op_kind,
  AstAttributeName_name,
  AstAttributeName_nodes,
  AstAttributeName_file_path,
  AstAttributeName_body,
  AstAttributeName_stmt,
  AstAttributeName_type_expr,
  AstAttributeName_id,
  AstAttributeName_init_expr,
  AstAttributeName_ret_type_expr,
  AstAttributeName_args,
  AstAttributeName_int_val,
  AstAttributeName_float_val,
  AstAttributeName_bool_val,
  AstAttributeName_char_val,
  AstAttributeName_str,
  AstAttributeName_expr,
  AstAttributeName_cond_expr,
  AstAttributeName_loop_expr,
  AstAttributeName_else_body,
  AstAttributeName_decl_expr,
  AstAttributeName_size_expr,
  AstAttributeName_members,
  AstAttributeName_count_expr,
  AstAttributeName_lit_kind,
  AstAttributeName_local_decls,
  AstAttributeName_non_local_occurs,
};

typedef struct AstAttribute
{
  AstAttributeKind kind;
  AstAttributeName name;

  union
  {
    AstNode* ast_node;
    bool bool_val;
    int int_val;
    float float_val;
    char char_val;
    char* str;
    List* list;
    OperatorKind op_kind;
    LiteralKind lit_kind;
    Scope* scope;
  };
}
AstAttribute;

typedef struct AstAttributeMetaInfo
{
  AstAttributeKind kind;
  AstAttributeName name;
}
AstAttributeMetaInfo;

#ifndef AstKind_MEMBER_LIST
#define AstKind_MEMBER_LIST()\
  ENUM_MEMBER(AstNode__None),\
  ENUM_MEMBER(AstNode_string),\
  ENUM_MEMBER(AstNode_id),\
  ENUM_MEMBER(AstNode_array),\
  ENUM_MEMBER(AstNode_bin_expr),\
  ENUM_MEMBER(AstNode_un_expr),\
  ENUM_MEMBER(AstNode_module),\
  ENUM_MEMBER(AstNode_include),\
  ENUM_MEMBER(AstNode_block),\
  ENUM_MEMBER(AstNode_stmt),\
  ENUM_MEMBER(AstNode_var),\
  ENUM_MEMBER(AstNode_var_decl),\
  ENUM_MEMBER(AstNode_var_occur),\
  ENUM_MEMBER(AstNode_proc),\
  ENUM_MEMBER(AstNode_proc_decl),\
  ENUM_MEMBER(AstNode_proc_occur),\
  ENUM_MEMBER(AstNode_type),\
  ENUM_MEMBER(AstNode_type_decl),\
  ENUM_MEMBER(AstNode_type_occur),\
  ENUM_MEMBER(AstNode_call),\
  ENUM_MEMBER(AstNode_lit),\
  ENUM_MEMBER(AstNode_return_stmt),\
  ENUM_MEMBER(AstNode_goto_stmt),\
  ENUM_MEMBER(AstNode_label),\
  ENUM_MEMBER(AstNode_if_stmt),\
  ENUM_MEMBER(AstNode_while_stmt),\
  ENUM_MEMBER(AstNode_do_while_stmt),\
  ENUM_MEMBER(AstNode_for_stmt),\
  ENUM_MEMBER(AstNode_break_stmt),\
  ENUM_MEMBER(AstNode_continue_stmt),\
  ENUM_MEMBER(AstNode_cast),\
  ENUM_MEMBER(AstNode_pointer),\
  ENUM_MEMBER(AstNode_enum_decl),\
  ENUM_MEMBER(AstNode_struct_decl),\
  ENUM_MEMBER(AstNode_union_decl),\
  ENUM_MEMBER(AstNode_init_list),\
  ENUM_MEMBER(AstNode_hoc_new),\
  ENUM_MEMBER(AstNode_hoc_putc),
#endif

typedef enum AstKind
{
#define ENUM_MEMBER(NAME) NAME
  AstKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

char* AstKind_strings[] =
{
#define ENUM_MEMBER(NAME) #NAME
  AstKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

char*
get_ast_kind_printstr(AstKind kind)
{
  return AstKind_strings[kind];
}

typedef struct AstKindMetaInfo
{
  AstKind kind;
  int attrib_count;
  AstAttributeMetaInfo* attribs;
}
AstKindMetaInfo;

typedef struct AstMetaInfo
{
  int kind_count;
  AstKindMetaInfo* kinds;
}
AstMetaInfo;

typedef struct AstNode
{
  int gen_id;
  AstKind kind;
  SourceLoc* src_loc;

  AstAttribute attribs[10];
}
AstNode;

AstMetaInfo ast_meta_infos[2];

#if 0
typedef struct AstNode2
{
  AstKind2 kind;
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
      OperatorKind kind;
    }
    binop_decl,
    unop_decl;

    struct
    {
      OperatorKind op_kind;
      AstNode2* left_operand;
      AstNode2* right_operand;
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
      ProcKind kind;
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
#endif

typedef enum TypeKind
{
  Type__None,
  Type_typevar,
  Type_unary,
  Type_basic,
  Type_type,
  Type_proc,
  Type_product,
  Type_pointer,
  Type_array,
};

typedef enum UnaryTypeCtorKind
{
  UnaryTypeCtor__None,
  UnaryTypeCtor_pointer,
  UnaryTypeCtor_array,
};

typedef enum BasicTypeKind
{
  BasicType__None,
  BasicType_Void,
  BasicType_Int,
  BasicType_Float,
  BasicType_Char,
  BasicType_Bool,
};

typedef struct Type
{
  TypeKind kind;
  Type* repr_type; // representative member of the set of equivalent types
  AstNode* ast_node;
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
  Symbol__None,
  Symbol_unresolved,
  Symbol_var_decl,
  Symbol_var_occur,
  Symbol_type_decl,
  Symbol_type_occur,
  Symbol_proc_decl,
  Symbol_proc_occur,
}
SymbolKind;

typedef struct Symbol
{
  SymbolKind kind;

  Symbol* prev_symbol;
  char* name;
  Scope* scope;
  int nesting_depth;
  AstNode* ast_node;
}
Symbol;

typedef struct
{
#if 0
  Scope* global_scope;
  Scope* module_scope;
#endif
  Scope* active_scope;
  int nesting_depth;
  Scope* scopes[MAX_SCOPE_NESTING_DEPTH];
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
  List__None,
  List_ast_node,
  List_VmInstr,
  List_TypePair,
}
ListKind;

typedef struct ListItem
{
  ListKind kind;
  union
  {
    void* elem;
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

void DEBUG_print_ast_node_list(String* str, int indent_level, char* tag, List* node_list);

