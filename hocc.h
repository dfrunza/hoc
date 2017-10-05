#include <stdio.h>
#include <windows.h>

#define ARENA_SIZE (3*MEGABYTE)
#define SYMBOL_ARENA_SIZE (ARENA_SIZE/10)
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
#define false (bool)0
#define true  (bool)1
#define inline __inline
#define internal static

typedef struct String String;

typedef struct MemoryArena
{
  uint8* base;
  uint8* free;
  uint8* cap;
  struct MemoryArena* prev_arena;
  char* label;
  struct String* str;
}
MemoryArena;

typedef struct
{
  size_t total_avail;
  double in_use;
}
ArenaUsage;

typedef struct String
{
  char* head;
  char* end;
  MemoryArena* arena;
}
String;

#define assert(EXPR)\
  do { if(!(EXPR)) assert_f(#EXPR, __FILE__, __LINE__); } while(0);

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
  Token_None,
  /* 'Simple' tokens must be listed at the beginning of the enum */
  Token_dot,
  Token_arrow_right,
  Token_open_bracket,
  Token_close_bracket,
  Token_open_parens,
  Token_close_parens,
  Token_open_brace,
  Token_close_brace,
  Token_semicolon,
  Token_colon,
  Token_comma,
  Token_percent,
  Token_star,
  Token_fwd_slash,
  Token_back_slash,
  Token_plus,
  Token_plus_plus,
  Token_minus,
  Token_minus_minus,
  Token_exclam,
  Token_exclam_eq,
  Token_eq,
  Token_eq_eq,
  Token_angle_right,
  Token_angle_right_eq,
  Token_angle_left,
  Token_angle_left_eq,
  Token_ampersand,
  Token_ampersand_ampersand,
  Token_pipe,
  Token_pipe_pipe,
  Token_unknown_char,
  Token_end_of_input,

  Token_var,
  Token_type,
  Token_if,
  Token_else,
  Token_while,
  Token_for,
  Token_proc,
  Token_struct,
  Token_union,
  Token_return,
  Token_break,
  Token_continue,
  Token_goto,
  Token_include,
  Token_enum,
//  Token_new,
//  Token_putc,
  Token_true,
  Token_false,

  Token_id,
  Token_int_num,
  Token_float_num,
  Token_string,
  Token_char,
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
  ENUM_MEMBER(OperatorKind_None),\
  ENUM_MEMBER(OperatorKind_add),\
  ENUM_MEMBER(OperatorKind_sub),\
  ENUM_MEMBER(OperatorKind_mul),\
  ENUM_MEMBER(OperatorKind_div),\
  ENUM_MEMBER(OperatorKind_mod),\
  ENUM_MEMBER(OperatorKind_neg),\
  ENUM_MEMBER(OperatorKind_assign),\
  ENUM_MEMBER(OperatorKind_ptr_deref),\
  ENUM_MEMBER(OperatorKind_address_of),\
  ENUM_MEMBER(OperatorKind_member_select),\
  ENUM_MEMBER(OperatorKind_ptr_member_select),\
  ENUM_MEMBER(OperatorKind_pre_decr),\
  ENUM_MEMBER(OperatorKind_post_decr),\
  ENUM_MEMBER(OperatorKind_pre_incr),\
  ENUM_MEMBER(OperatorKind_post_incr),\
  ENUM_MEMBER(OperatorKind_array_index),\
  ENUM_MEMBER(OperatorKind_eq),\
  ENUM_MEMBER(OperatorKind_not_eq),\
  ENUM_MEMBER(OperatorKind_less),\
  ENUM_MEMBER(OperatorKind_less_eq),\
  ENUM_MEMBER(OperatorKind_greater),\
  ENUM_MEMBER(OperatorKind_greater_eq),\
  ENUM_MEMBER(OperatorKind_logic_and),\
  ENUM_MEMBER(OperatorKind_logic_or),\
  ENUM_MEMBER(OperatorKind_logic_not),\
  ENUM_MEMBER(OperatorKind_bit_and),\
  ENUM_MEMBER(OperatorKind_bit_or),\
  ENUM_MEMBER(OperatorKind_cast),
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
  ENUM_MEMBER(Literal_None),\
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
  ENUM_MEMBER(ScopeKind_None),\
  ENUM_MEMBER(ScopeKind_global),\
  ENUM_MEMBER(ScopeKind_module),\
  ENUM_MEMBER(ScopeKind_proc),\
  ENUM_MEMBER(ScopeKind_loop),\
  ENUM_MEMBER(ScopeKind_block),
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
  int nesting_depth;
  struct Scope* encl_scope;
  AstNode* ast_node;
}
Scope;

typedef enum 
{
  AstAttribute_None,
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
  AstAttribute_type,
  AstAttribute_symbol,
}
AstAttributeKind;

typedef enum AstAttributeName
{
  AstAttributeName_None,
  AstAttributeName_symbol,
  AstAttributeName_decl_sym,
  AstAttributeName_occur_sym,
  AstAttributeName_type,
  AstAttributeName_type_expr,
  AstAttributeName_proc,
  AstAttributeName_nesting_depth,
  AstAttributeName_loop,
  AstAttributeName_formal_args,
  AstAttributeName_ret_var,
  AstAttributeName_scope,
  AstAttributeName_decl_scope,
  AstAttributeName_occur_scope,
  AstAttributeName_decl_scope_depth,
  AstAttributeName_proc_decl,
  AstAttributeName_type_decl,
  AstAttributeName_operand,
  AstAttributeName_left_operand,
  AstAttributeName_right_operand,
  AstAttributeName_op_kind,
  AstAttributeName_name,
  AstAttributeName_nodes,
  AstAttributeName_file_path,
  AstAttributeName_body,
  AstAttributeName_stmt,
  AstAttributeName_id,
  AstAttributeName_init_expr,
  AstAttributeName_ret_type,
  AstAttributeName_actual_args,
  AstAttributeName_int_val,
  AstAttributeName_float_val,
  AstAttributeName_bool_val,
  AstAttributeName_char_val,
  AstAttributeName_str,
  AstAttributeName_ret_expr,
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

typedef struct
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
    Type* type;
    Symbol* symbol;
  };
}
AstAttribute;

typedef struct
{
  AstAttributeKind kind;
  AstAttributeName name;
}
AstAttributeMetaInfo;

#ifndef AstKind_MEMBER_LIST
#define AstKind_MEMBER_LIST()\
  ENUM_MEMBER(AstNode_None),\
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
  ENUM_MEMBER(AstNode_type),\
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
  ENUM_MEMBER(AstNode_pointer),\
  ENUM_MEMBER(AstNode_enum_decl),\
  ENUM_MEMBER(AstNode_struct_decl),\
  ENUM_MEMBER(AstNode_union_decl),\
  ENUM_MEMBER(AstNode_init_list),
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

typedef struct
{
  AstKind kind;
  int attr_count;
  AstAttributeMetaInfo* attrs;
}
AstKindMetaInfo;

typedef struct
{
  int kind_count;
  AstKindMetaInfo* kinds;
}
AstMetaInfo;

typedef enum
{
  Ast_gen0,
  Ast_gen1,
  Ast_gen_Count,
}
Ast_Gen;

#define ATTR(NODE, KIND, NAME)\
  (get_ast_attribute_safe((NODE), AstAttribute_##KIND, AstAttributeName_##NAME)->KIND)

typedef struct AstNode
{
  Ast_Gen gen;
  AstKind kind;
  SourceLoc* src_loc;

  AstAttribute attrs[10];
}
AstNode;

AstMetaInfo ast_meta_infos[2];

typedef enum TypeKind
{
  Type_None,
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
  UnaryTypeCtor_None,
  UnaryTypeCtor_pointer,
  UnaryTypeCtor_array,
};

typedef enum BasicTypeKind
{
  BasicType_None,
  BasicType_void,
  BasicType_int,
  BasicType_float,
  BasicType_char,
  BasicType_bool,
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
  Symbol_None,
  Symbol_unresolved,
  Symbol_var_decl,
  Symbol_var_occur,
  Symbol_type_decl,
  Symbol_type_occur,
  Symbol_proc_decl,
  Symbol_proc_occur,
}
SymbolKind;

#define SYM(VAR, NAME)\
  (((VAR)->kind == Symbol_##NAME) ? &(VAR)->NAME : 0)

typedef struct Symbol
{
  SymbolKind kind;

  Symbol* prev_symbol;
  char* name;
  SourceLoc* src_loc;
  Scope* scope;
  int nesting_depth;
  Type* type;

  union
  {
    struct
    {
      void* data;
    }
    var_decl;

    struct
    {
      Symbol* var_decl;
      int decl_scope_depth;
    }
    var_occur;

    struct
    {
      Symbol* type_decl;
    }
    type_occur;

    struct
    {
      Symbol* ret_var;
    }
    proc_decl;

    struct
    {
      Symbol* proc_decl;
    }
    proc_occur;
  };
}
Symbol;

typedef struct
{
  Scope* active_scope;
  int nesting_depth;
  int sym_count;
  int scope_count;
  MemoryArena* arena;
}
SymbolTable;

typedef enum
{
  Opcode_None,
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
  ParamType_None,
  ParamType_Int32,
  ParamType_Float32,
  ParamType_String,
  ParamType_Reg,
}
ParamType;

typedef enum
{
  RegName_None,
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
  List_None,
  List_ast_node,
  List_vm_instr,
  List_type_pair,
}
ListKind;

#define ITEM(VAR, NAME)\
  (((VAR)->kind == List_##NAME) ? (VAR)->NAME : 0)

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
  MemoryArena* arena;
}
List;

void DEBUG_print_ast_node_list(String* str, int indent_level, char* tag, List* node_list);

