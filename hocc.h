#define ARENA_SIZE (3*MEGABYTE)
#define BINIMAGE_SIGNATURE "HC"

typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned int bool;

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
  int total_avail;
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

#define sizeof_array(ARRAY)\
  (sizeof(ARRAY)/sizeof(ARRAY[0]))

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

typedef enum
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
  Token_true,
  Token_false,

  Token_id,
  Token_int_num,
  Token_float_num,
  Token_string,
  Token_char,
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

  SourceLoc src_loc;
}
TokenStream;

typedef enum
{
  DataArea_None,
  DataArea_data,
  DataArea_link,
}
DataAreaKind;

typedef struct
{
  DataAreaKind kind;

  int loc;
  int size;
  List* subareas;

  int decl_scope_offset;
  void* data;
}
DataArea;

#ifndef OperatorKind_MEMBER_LIST
#define OperatorKind_MEMBER_LIST()\
  ENUM_MEMBER(Operator_None),\
  ENUM_MEMBER(Operator_add),\
  ENUM_MEMBER(Operator_sub),\
  ENUM_MEMBER(Operator_mul),\
  ENUM_MEMBER(Operator_div),\
  ENUM_MEMBER(Operator_mod),\
  ENUM_MEMBER(Operator_neg),\
  ENUM_MEMBER(Operator_assign),\
  ENUM_MEMBER(Operator_deref),\
  ENUM_MEMBER(Operator_address_of),\
  ENUM_MEMBER(Operator_member_select),\
  ENUM_MEMBER(Operator_ptr_member_select),\
  ENUM_MEMBER(Operator_pre_decr),\
  ENUM_MEMBER(Operator_post_decr),\
  ENUM_MEMBER(Operator_pre_incr),\
  ENUM_MEMBER(Operator_post_incr),\
  ENUM_MEMBER(Operator_array_index),\
  ENUM_MEMBER(Operator_eq),\
  ENUM_MEMBER(Operator_not_eq),\
  ENUM_MEMBER(Operator_less),\
  ENUM_MEMBER(Operator_less_eq),\
  ENUM_MEMBER(Operator_greater),\
  ENUM_MEMBER(Operator_greater_eq),\
  ENUM_MEMBER(Operator_logic_and),\
  ENUM_MEMBER(Operator_logic_or),\
  ENUM_MEMBER(Operator_logic_not),\
  ENUM_MEMBER(Operator_bit_and),\
  ENUM_MEMBER(Operator_bit_or),\
  ENUM_MEMBER(Operator_cast),
#endif

typedef enum
{
#define ENUM_MEMBER(NAME) NAME
  OperatorKind_MEMBER_LIST()
#undef ENUM_MEMBER
}
OperatorKind;

char* get_operator_kind_printstr(OperatorKind op)
{
  char* str = "???";
  if(op == Operator_add)
    str = "+";
  else if(op == Operator_sub)
    str = "-";
  else if(op == Operator_mul)
    str = "*";
  else if(op == Operator_div)
    str = "/";
  else if(op == Operator_mod)
    str = "%";
  else if(op == Operator_neg)
    str = "-";
  else if (op == Operator_assign)
    str = "=";
  else if(op == Operator_deref)
    str = "*";
  else if(op == Operator_address_of)
    str = "&";
  else if(op == Operator_member_select)
    str = ".";
  else if(op == Operator_ptr_member_select)
    str = "->";
  else if(op == Operator_pre_decr || op == Operator_post_decr)
    str = "--";
  else if(op == Operator_pre_incr || op == Operator_post_incr)
    str = "++";
  else if(op == Operator_array_index)
    str = "[]";
  else if(op == Operator_eq)
    str = "==";
  else if(op == Operator_not_eq)
    str = "!=";
  else if(op == Operator_less)
    str = "<";
  else if(op == Operator_less_eq)
    str = "<=";
  else if(op == Operator_greater)
    str = ">";
  else if(op == Operator_greater_eq)
    str = ">=";
  else if(op == Operator_logic_and)
    str = "&&";
  else if(op == Operator_logic_or)
    str = "||";
  else if(op == Operator_logic_not)
    str = "!";
  else if(op == Operator_bit_and)
    str = "&";
  else if(op == Operator_bit_or)
    str = "|";
  else if(op == Operator_cast)
    str = "(cast)";
  return str;
}

#ifndef LiteralKind_MEMBER_LIST
#define LiteralKind_MEMBER_LIST()\
  ENUM_MEMBER(Literal_None),\
  ENUM_MEMBER(Literal_int_val),\
  ENUM_MEMBER(Literal_float_val),\
  ENUM_MEMBER(Literal_bool_val),\
  ENUM_MEMBER(Literal_char_val),\
  ENUM_MEMBER(Literal_str_val),
#endif

typedef enum
{
#define ENUM_MEMBER(NAME) NAME
  LiteralKind_MEMBER_LIST()
#undef ENUM_MEMBER
}
LiteralKind;

char* LiteralKind_strings[] =
{
#define ENUM_MEMBER(NAME) #NAME
  LiteralKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

char* get_literal_kind_printstr(LiteralKind kind)
{
  return LiteralKind_strings[kind];
}

typedef struct Type Type;
typedef struct AstNode AstNode;
typedef struct Symbol Symbol;

#ifndef ScopeKind_MEMBER_LIST
#define ScopeKind_MEMBER_LIST()\
  ENUM_MEMBER(Scope_None),\
  ENUM_MEMBER(Scope_global),\
  ENUM_MEMBER(Scope_module),\
  ENUM_MEMBER(Scope_proc),\
  ENUM_MEMBER(Scope_loop),\
  ENUM_MEMBER(Scope_block),
#endif

typedef enum
{
#define ENUM_MEMBER(NAME) NAME
  ScopeKind_MEMBER_LIST()
#undef ENUM_MEMBER
}
ScopeKind;

char* ScopeKind_strings[] =
{
#define ENUM_MEMBER(NAME) #NAME
  ScopeKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

char* get_scope_kind_printstr(ScopeKind kind)
{
  return ScopeKind_strings[kind];
}

typedef enum
{
  Symbol_None,
  Symbol_var,
  Symbol_return_var,
  Symbol_formal_arg,
  Symbol_str,
  Symbol_type,
  Symbol_proc,
  Symbol_Count,
}
SymbolKind;

typedef struct Scope
{
  ScopeKind kind;

  int nesting_depth;
  struct Scope* encl_scope;
  AstNode* ast_node;

  List* decls[Symbol_Count];
  List* occurs[Symbol_Count];

  List* pre_fp_areas;
  List* post_fp_areas;

  DataArea* ret_area;
  DataArea* args_area;
  DataArea* link_area;
  DataArea* ctrl_area;
  DataArea* local_area;
#if 0
  DataArea pre_fp_area;
  DataArea post_fp_area;
  List* access_links;
#endif
#if 0
  int local_area_size;
  int link_area_size;
#endif
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
  AstAttribute_str_val,
  AstAttribute_op_kind,
  AstAttribute_list,
  AstAttribute_lit_kind,
  AstAttribute_scope,
  AstAttribute_type,
  AstAttribute_symbol,
}
AstAttributeKind;

typedef enum
{
  AstAttributeName_None,
  AstAttributeName_symbol,
  AstAttributeName_decl_sym,
  AstAttributeName_occur_sym,
  AstAttributeName_type,
  AstAttributeName_eval_type,
  AstAttributeName_type_expr,
  AstAttributeName_pointee_expr,
  AstAttributeName_elem_expr,
  AstAttributeName_proc,
  AstAttributeName_nesting_depth,
  AstAttributeName_loop,
  AstAttributeName_formal_args,
  AstAttributeName_return_type,
  AstAttributeName_return_var,
  AstAttributeName_scope,
  AstAttributeName_decl_scope,
  AstAttributeName_occur_scope,
  AstAttributeName_var_decl,
  AstAttributeName_proc_decl,
  AstAttributeName_type_decl,
  AstAttributeName_str_lit,
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
  AstAttributeName_actual_args,
  AstAttributeName_int_val,
  AstAttributeName_float_val,
  AstAttributeName_bool_val,
  AstAttributeName_char_val,
  AstAttributeName_str_val,
  AstAttributeName_return_expr,
  AstAttributeName_expr,
  AstAttributeName_cond_expr,
  AstAttributeName_loop_expr,
  AstAttributeName_else_body,
  AstAttributeName_decl_expr,
  AstAttributeName_size_expr,
  AstAttributeName_members,
  AstAttributeName_count_expr,
  AstAttributeName_lit_kind,
  AstAttributeName_data_area,
  AstAttributeName_label_end,
  AstAttributeName_label_else,
  AstAttributeName_label_eval,
  AstAttributeName_label_break,
}
AstAttributeName;

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
    char* str_val;
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
  ENUM_MEMBER(AstNode_id),\
  ENUM_MEMBER(AstNode_array),\
  ENUM_MEMBER(AstNode_bin_expr),\
  ENUM_MEMBER(AstNode_un_expr),\
  ENUM_MEMBER(AstNode_module),\
  ENUM_MEMBER(AstNode_include),\
  ENUM_MEMBER(AstNode_block),\
  ENUM_MEMBER(AstNode_stmt),\
  ENUM_MEMBER(AstNode_var_decl),\
  ENUM_MEMBER(AstNode_var_occur),\
  ENUM_MEMBER(AstNode_proc_decl),\
  ENUM_MEMBER(AstNode_proc_occur),\
  ENUM_MEMBER(AstNode_type_decl),\
  ENUM_MEMBER(AstNode_type_occur),\
  ENUM_MEMBER(AstNode_str),\
  ENUM_MEMBER(AstNode_lit),\
  ENUM_MEMBER(AstNode_return_stmt),\
  ENUM_MEMBER(AstNode_return_var),\
  ENUM_MEMBER(AstNode_assign),\
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

typedef enum 
{
#define ENUM_MEMBER(NAME) NAME
  AstKind_MEMBER_LIST()
#undef ENUM_MEMBER
}
AstKind;

char* AstKind_strings[] =
{
#define ENUM_MEMBER(NAME) #NAME
  AstKind_MEMBER_LIST()
#undef ENUM_MEMBER
};

char* get_ast_kind_printstr(AstKind kind)
{
  return AstKind_strings[kind];
}

#define MAX_ATTRIBUTE_COUNT 12

typedef struct
{
  AstKind kind;
  int attr_count;
  AstAttributeMetaInfo attrs[MAX_ATTRIBUTE_COUNT];
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

typedef struct AstNode
{
  Ast_Gen gen;
  AstKind kind;
  SourceLoc* src_loc;

  AstAttribute attrs[MAX_ATTRIBUTE_COUNT];
}
AstNode;

AstMetaInfo ast_meta_infos[2];

typedef enum
{
  Type_None,
  Type_typevar,
  Type_basic,
  Type_proc,
  Type_product,
  Type_pointer,
  Type_array,
}
TypeKind;

typedef enum
{
  BasicType_None,
  BasicType_void,
  BasicType_int,
  BasicType_float,
  BasicType_char,
  BasicType_bool,
  BasicType_type,
}
BasicTypeKind;

typedef struct Type
{
  TypeKind kind;
  Type* repr_type; // representative member of the set of equivalent types
  int width;

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

#if 0
#define SYM(VAR, NAME)\
  (((VAR)->kind == Symbol_##NAME) ? &(VAR)->NAME : 0)
#endif

typedef struct Symbol
{
  SymbolKind kind;

  char* name;
  SourceLoc* src_loc;
  Scope* scope;
  int nesting_depth;
  AstNode* ast_node;
  Type* type;
  DataArea* data_area;
  void* data;

  Symbol* decl;
  int decl_scope_offset;
}
Symbol;

typedef struct
{
  List* scopes;
  Scope* global_scope;
  Scope* module_scope;
  Scope* active_scope;
  int nesting_depth;
  MemoryArena* arena;
}
SymbolTable;

typedef enum
{
  Opcode_None,
  Opcode_PUSH_INT8,
  Opcode_PUSH_INT32,
  Opcode_PUSH_FLOAT32,
  Opcode_PUSH_REG,
  Opcode_POP_REG,
  Opcode_DUP,
  Opcode_LOAD,
  Opcode_STORE,
  Opcode_GROW,
  //Opcode_NEW,

  Opcode_ADD_INT32,
  Opcode_SUB_INT32,
  Opcode_MUL_INT32,
  Opcode_DIV_INT32,
  Opcode_MOD_INT32,
  Opcode_NEG_INT32,
  Opcode_INCR_INT32,
  Opcode_DECR_INT32,

  Opcode_ADD_FLOAT32,
  Opcode_SUB_FLOAT32,
  Opcode_MUL_FLOAT32,
  Opcode_DIV_FLOAT32,
  Opcode_NEG_FLOAT32,

  Opcode_CMPEQ_INT8,
  Opcode_CMPNEQ_INT8,
  Opcode_CMPLSS_INT8,
  Opcode_CMPGRT_INT8,
  Opcode_CMPEQ_INT32,
  Opcode_CMPNEQ_INT32,
  Opcode_CMPLSS_INT32,
  Opcode_CMPGRT_INT32,
  Opcode_CMPEQ_FLOAT32,
  Opcode_CMPNEQ_FLOAT32,
  Opcode_CMPLSS_FLOAT32,
  Opcode_CMPGRT_FLOAT32,
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
  //Opcode_PUTC,

  Opcode_FLOAT32_TO_INT32,
  Opcode_INT32_TO_FLOAT32,
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
  ParamType_int32,
  ParamType_int8,
  ParamType_float32,
  ParamType_str,
  ParamType_reg,
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
    int8 char_val; //todo
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
BinImage;

typedef struct
{
  char* text;
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
  List_symbol,
  List_scope,
  List_vm_instr,
  List_type_pair,
  List_data_area,
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
    Symbol* symbol;
    Scope* scope;
    Instruction* instr;
    TypePair* type_pair;
    DataArea* data_area;
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

