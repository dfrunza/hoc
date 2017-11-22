#define ARENA_SIZE (3*MEGABYTE)
#define SYMBOL_ARENA_SIZE (ARENA_SIZE / 8)
#define TARGET_CODE_ARENA_SIZE (ARENA_SIZE / 4)
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
  eToken_None,
  /* 'Simple' tokens must be listed at the beginning of the enum */
  eToken_dot,
  eToken_arrow_right,
  eToken_open_bracket,
  eToken_close_bracket,
  eToken_open_parens,
  eToken_close_parens,
  eToken_open_brace,
  eToken_close_brace,
  eToken_semicolon,
  eToken_colon,
  eToken_comma,
  eToken_percent,
  eToken_star,
  eToken_fwd_slash,
  eToken_back_slash,
  eToken_plus,
  eToken_plus_plus,
  eToken_minus,
  eToken_minus_minus,
  eToken_exclam,
  eToken_exclam_eq,
  eToken_eq,
  eToken_eq_eq,
  eToken_angle_right,
  eToken_angle_right_eq,
  eToken_angle_left,
  eToken_angle_left_eq,
  eToken_ampersand,
  eToken_ampersand_ampersand,
  eToken_pipe,
  eToken_pipe_pipe,
  eToken_unknown_char,
  eToken_end_of_input,

  eToken_var,
  eToken_asm,
  eToken_type,
  eToken_if,
  eToken_else,
  eToken_while,
  eToken_for,
  eToken_proc,
  eToken_struct,
  eToken_union,
  eToken_return,
  eToken_break,
  eToken_continue,
  eToken_goto,
  eToken_include,
  eToken_enum,
  eToken_true,
  eToken_false,

  eToken_id,
  eToken_int_num,
  eToken_float_num,
  eToken_string,
  eToken_char,
  eToken_asm_text,
}
eToken;

typedef struct
{
  eToken kind;
  char* lexeme;

  union
  {
    int* int_val;
    float* float_val;
    char char_val;
    char* str_val;
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
  eDataArea_None,
  eDataArea_var,
  eDataArea_link,
}
eDataArea;

typedef struct
{
  eDataArea kind;

  int loc;
  int size;
  List* subareas;

  void* data;
}
DataArea;

#ifndef eOperator_MEMBER_LIST
#define eOperator_MEMBER_LIST()\
  ENUM_MEMBER(eOperator_None),\
  ENUM_MEMBER(eOperator_add),\
  ENUM_MEMBER(eOperator_sub),\
  ENUM_MEMBER(eOperator_mul),\
  ENUM_MEMBER(eOperator_div),\
  ENUM_MEMBER(eOperator_mod),\
  ENUM_MEMBER(eOperator_neg),\
  ENUM_MEMBER(eOperator_assign),\
  ENUM_MEMBER(eOperator_deref),\
  ENUM_MEMBER(eOperator_address_of),\
  ENUM_MEMBER(eOperator_member_select),\
  ENUM_MEMBER(eOperator_ptr_member_select),\
  ENUM_MEMBER(eOperator_pre_decr),\
  ENUM_MEMBER(eOperator_post_decr),\
  ENUM_MEMBER(eOperator_pre_incr),\
  ENUM_MEMBER(eOperator_post_incr),\
  ENUM_MEMBER(eOperator_index),\
  ENUM_MEMBER(eOperator_eq),\
  ENUM_MEMBER(eOperator_not_eq),\
  ENUM_MEMBER(eOperator_less),\
  ENUM_MEMBER(eOperator_less_eq),\
  ENUM_MEMBER(eOperator_greater),\
  ENUM_MEMBER(eOperator_greater_eq),\
  ENUM_MEMBER(eOperator_logic_and),\
  ENUM_MEMBER(eOperator_logic_or),\
  ENUM_MEMBER(eOperator_logic_not),\
  ENUM_MEMBER(eOperator_bit_and),\
  ENUM_MEMBER(eOperator_bit_or),\
  ENUM_MEMBER(eOperator_cast),\
  ENUM_MEMBER(eOperator_Count),
#endif

typedef enum
{
#define ENUM_MEMBER(NAME) NAME
  eOperator_MEMBER_LIST()
#undef ENUM_MEMBER
}
eOperator;

char* get_operator_kind_printstr(eOperator op)
{
  char* str = "???";
  if(op == eOperator_add)
    str = "+";
  else if(op == eOperator_sub)
    str = "-";
  else if(op == eOperator_mul)
    str = "*";
  else if(op == eOperator_div)
    str = "/";
  else if(op == eOperator_mod)
    str = "%";
  else if(op == eOperator_neg)
    str = "-";
  else if (op == eOperator_assign)
    str = "=";
  else if(op == eOperator_deref)
    str = "*";
  else if(op == eOperator_address_of)
    str = "&";
  else if(op == eOperator_member_select)
    str = ".";
  else if(op == eOperator_ptr_member_select)
    str = "->";
  else if(op == eOperator_pre_decr || op == eOperator_post_decr)
    str = "--";
  else if(op == eOperator_pre_incr || op == eOperator_post_incr)
    str = "++";
  else if(op == eOperator_index)
    str = "[]";
  else if(op == eOperator_eq)
    str = "==";
  else if(op == eOperator_not_eq)
    str = "!=";
  else if(op == eOperator_less)
    str = "<";
  else if(op == eOperator_less_eq)
    str = "<=";
  else if(op == eOperator_greater)
    str = ">";
  else if(op == eOperator_greater_eq)
    str = ">=";
  else if(op == eOperator_logic_and)
    str = "&&";
  else if(op == eOperator_logic_or)
    str = "||";
  else if(op == eOperator_logic_not)
    str = "!";
  else if(op == eOperator_bit_and)
    str = "&";
  else if(op == eOperator_bit_or)
    str = "|";
  else if(op == eOperator_cast)
    str = "(cast)";
  return str;
}

#ifndef eLiteral_MEMBER_LIST
#define eLiteral_MEMBER_LIST()\
  ENUM_MEMBER(eLiteral_None),\
  ENUM_MEMBER(eLiteral_int_val),\
  ENUM_MEMBER(eLiteral_float_val),\
  ENUM_MEMBER(eLiteral_bool_val),\
  ENUM_MEMBER(eLiteral_char_val),\
  ENUM_MEMBER(eLiteral_str_val),
#endif

typedef enum
{
#define ENUM_MEMBER(NAME) NAME
  eLiteral_MEMBER_LIST()
#undef ENUM_MEMBER
}
eLiteral;

char* eLiteral_strings[] =
{
#define ENUM_MEMBER(NAME) #NAME
  eLiteral_MEMBER_LIST()
#undef ENUM_MEMBER
};

char* get_literal_kind_printstr(eLiteral kind)
{
  return eLiteral_strings[kind];
}

typedef struct Type Type;
typedef struct AstNode AstNode;
typedef struct Symbol Symbol;

#ifndef eScope_MEMBER_LIST
#define eScope_MEMBER_LIST()\
  ENUM_MEMBER(eScope_None),\
  ENUM_MEMBER(eScope_global),\
  ENUM_MEMBER(eScope_module),\
  ENUM_MEMBER(eScope_proc),\
  ENUM_MEMBER(eScope_loop),\
  ENUM_MEMBER(eScope_block),
#endif

typedef enum
{
#define ENUM_MEMBER(NAME) NAME
  eScope_MEMBER_LIST()
#undef ENUM_MEMBER
}
eScope;

char* eScope_strings[] =
{
#define ENUM_MEMBER(NAME) #NAME
  eScope_MEMBER_LIST()
#undef ENUM_MEMBER
};

char* get_scope_kind_printstr(eScope kind)
{
  return eScope_strings[kind];
}

typedef enum
{
  eSymbol_None,
  eSymbol_var,
  eSymbol_ret_var,
  eSymbol_formal_arg,
  eSymbol_type,
  eSymbol_proc,
  eSymbol_Count,
}
eSymbol;

typedef struct Scope
{
  eScope kind;

  int nesting_depth;
  struct Scope* encl_scope;
  AstNode* ast_node;

  List* decls[eSymbol_Count];
  List* occurs[eSymbol_Count];

  List* pre_fp_areas;
  List* post_fp_areas;

  DataArea ret_area;
  DataArea args_area;
  DataArea link_area;
  DataArea ctrl_area;
  DataArea local_area;
}
Scope;

typedef enum 
{
  eAstAttribute_None,
  eAstAttribute_ast_node,
  eAstAttribute_int_val,
  eAstAttribute_float_val,
  eAstAttribute_char_val,
  eAstAttribute_bool_val,
  eAstAttribute_str_val,
  eAstAttribute_op_kind,
  eAstAttribute_list,
  eAstAttribute_lit_kind,
  eAstAttribute_scope,
  eAstAttribute_type,
  eAstAttribute_symbol,
}
eAstAttribute;

typedef enum
{
  eAstAttributeName_None,
  eAstAttributeName_symbol,
  eAstAttributeName_decl_sym,
  eAstAttributeName_occur_sym,
  eAstAttributeName_type,
  eAstAttributeName_eval_type,
  eAstAttributeName_type_expr,
  eAstAttributeName_pointee_expr,
  eAstAttributeName_elem_expr,
  eAstAttributeName_nesting_depth,
  eAstAttributeName_loop,
  eAstAttributeName_formal_args,
  eAstAttributeName_ret_type,
  eAstAttributeName_ret_var,
  eAstAttributeName_scope,
  eAstAttributeName_decl_scope,
  eAstAttributeName_occur_scope,
  eAstAttributeName_var_decl,
  eAstAttributeName_proc_decl,
  eAstAttributeName_type_decl,
  eAstAttributeName_str_lit,
  eAstAttributeName_operand,
  eAstAttributeName_left_operand,
  eAstAttributeName_right_operand,
  eAstAttributeName_op_kind,
  eAstAttributeName_name,
  eAstAttributeName_nodes,
  eAstAttributeName_procs,
  eAstAttributeName_vars,
  eAstAttributeName_stmts,
  eAstAttributeName_file_path,
  eAstAttributeName_body,
  eAstAttributeName_stmt,
  eAstAttributeName_id,
  eAstAttributeName_init_expr,
  eAstAttributeName_actual_args,
  eAstAttributeName_int_val,
  eAstAttributeName_float_val,
  eAstAttributeName_bool_val,
  eAstAttributeName_char_val,
  eAstAttributeName_str_val,
  eAstAttributeName_ret_expr,
  eAstAttributeName_expr,
  eAstAttributeName_cond_expr,
  eAstAttributeName_loop_expr,
  eAstAttributeName_else_body,
  eAstAttributeName_decl_expr,
  eAstAttributeName_size_expr,
  eAstAttributeName_members,
  eAstAttributeName_count_expr,
  eAstAttributeName_lit_kind,
  eAstAttributeName_data_area,
  eAstAttributeName_label_end,
  eAstAttributeName_label_else,
  eAstAttributeName_label_eval,
  eAstAttributeName_label_break,
  eAstAttributeName_asm_text,
}
eAstAttributeName;

typedef struct
{
  eAstAttribute kind;
  eAstAttributeName name;

  union
  {
    AstNode* ast_node;
    bool bool_val;
    int int_val;
    float float_val;
    char char_val;
    char* str_val;
    List* list;
    eOperator op_kind;
    eLiteral lit_kind;
    Scope* scope;
    Type* type;
    Symbol* symbol;
  };
}
AstAttribute;

typedef struct
{
  eAstAttribute kind;
  eAstAttributeName name;
}
AstAttributeMetaInfo;

#ifndef eAstNode_MEMBER_LIST
#define eAstNode_MEMBER_LIST()\
  ENUM_MEMBER(eAstNode_None),\
  ENUM_MEMBER(eAstNode_id),\
  ENUM_MEMBER(eAstNode_asm_block),\
  ENUM_MEMBER(eAstNode_array),\
  ENUM_MEMBER(eAstNode_bin_expr),\
  ENUM_MEMBER(eAstNode_un_expr),\
  ENUM_MEMBER(eAstNode_module),\
  ENUM_MEMBER(eAstNode_include),\
  ENUM_MEMBER(eAstNode_block),\
  ENUM_MEMBER(eAstNode_stmt),\
  ENUM_MEMBER(eAstNode_var_decl),\
  ENUM_MEMBER(eAstNode_var_occur),\
  ENUM_MEMBER(eAstNode_proc_decl),\
  ENUM_MEMBER(eAstNode_proc_occur),\
  ENUM_MEMBER(eAstNode_type_decl),\
  ENUM_MEMBER(eAstNode_type_occur),\
  ENUM_MEMBER(eAstNode_lit),\
  ENUM_MEMBER(eAstNode_ret_stmt),\
  ENUM_MEMBER(eAstNode_goto_stmt),\
  ENUM_MEMBER(eAstNode_label),\
  ENUM_MEMBER(eAstNode_if_stmt),\
  ENUM_MEMBER(eAstNode_while_stmt),\
  ENUM_MEMBER(eAstNode_do_while_stmt),\
  ENUM_MEMBER(eAstNode_for_stmt),\
  ENUM_MEMBER(eAstNode_break_stmt),\
  ENUM_MEMBER(eAstNode_continue_stmt),\
  ENUM_MEMBER(eAstNode_pointer),\
  ENUM_MEMBER(eAstNode_enum_decl),\
  ENUM_MEMBER(eAstNode_struct_decl),\
  ENUM_MEMBER(eAstNode_union_decl),\
  ENUM_MEMBER(eAstNode_init_list),\
  ENUM_MEMBER(eAstNode_empty),
#endif

typedef enum 
{
#define ENUM_MEMBER(NAME) NAME
  eAstNode_MEMBER_LIST()
#undef ENUM_MEMBER
}
eAstNode;

char* eAstNode_strings[] =
{
#define ENUM_MEMBER(NAME) #NAME
  eAstNode_MEMBER_LIST()
#undef ENUM_MEMBER
};

char* get_ast_kind_printstr(eAstNode kind)
{
  return eAstNode_strings[kind];
}

#define MAX_ATTRIBUTE_COUNT 12

typedef struct
{
  eAstNode kind;
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
  eAstGen_gen0,
  eAstGen_gen1,
  eAstGen_Count,
}
eAstGen;

typedef struct AstNode
{
  eAstGen gen;
  eAstNode kind;
  SourceLoc* src_loc;

  AstAttribute attrs[MAX_ATTRIBUTE_COUNT];
}
AstNode;

AstMetaInfo ast_meta_infos[2];

typedef enum
{
  eType_None,
  eType_typevar,
  eType_basic,
  eType_proc,
  eType_product,
  eType_pointer,
  eType_array,
}
eType;

typedef enum
{
  eBasicType_None,
  eBasicType_void,
  eBasicType_int,
  eBasicType_float,
  eBasicType_char,
  eBasicType_bool,
  eBasicType_type,
}
eBasicType;

typedef struct Type
{
  eType kind;
  Type* repr_type; // representative member of the set of equivalent types
  int width;

  union
  {
    struct
    {
      eBasicType kind;
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
  eSymbol kind;

  char* name;
  SourceLoc* src_loc;
  Scope* scope;
  int nesting_depth;
  AstNode* ast_node;
  Type* type;
  DataArea* data_area;
  void* data;
  Symbol* decl;
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
  eOpcode_None,
  eOpcode_PUSH_INT8,
  eOpcode_PUSH_INT32,
  eOpcode_PUSH_FLOAT32,
  eOpcode_PUSH_REG,
  eOpcode_POP_REG,
  eOpcode_DUP,
  eOpcode_LOAD,
  eOpcode_STORE,
  eOpcode_GROW,
  eOpcode_GROWNZ,
  eOpcode_NEW,

  eOpcode_ADD_INT32,
  eOpcode_SUB_INT32,
  eOpcode_MUL_INT32,
  eOpcode_DIV_INT32,
  eOpcode_MOD_INT32,
  eOpcode_NEG_INT32,
  eOpcode_INCR_INT32,
  eOpcode_DECR_INT32,

  eOpcode_ADD_FLOAT32,
  eOpcode_SUB_FLOAT32,
  eOpcode_MUL_FLOAT32,
  eOpcode_DIV_FLOAT32,
  eOpcode_NEG_FLOAT32,

  eOpcode_CMPEQ_INT8,
  eOpcode_CMPNEQ_INT8,
  eOpcode_CMPLSS_INT8,
  eOpcode_CMPGRT_INT8,
  eOpcode_CMPEQ_INT32,
  eOpcode_CMPNEQ_INT32,
  eOpcode_CMPLSS_INT32,
  eOpcode_CMPGRT_INT32,
  eOpcode_CMPEQ_FLOAT32,
  eOpcode_CMPNEQ_FLOAT32,
  eOpcode_CMPLSS_FLOAT32,
  eOpcode_CMPGRT_FLOAT32,
  eOpcode_AND,
  eOpcode_OR,
  eOpcode_NOT,

  eOpcode_LABEL,
  eOpcode_JUMPZ,
  eOpcode_JUMPNZ,
  eOpcode_GOTO,
  eOpcode_CALL,
  eOpcode_RETURN,
  eOpcode_ENTER,
  eOpcode_LEAVE,
  eOpcode_HALT,

  eOpcode_NOOP,
  eOpcode_PUTC,

  eOpcode_FLOAT32_TO_INT32,
  eOpcode_INT32_TO_FLOAT32,
}
eOpcode;

#if 0
typedef struct
{
  int source_line_nr;
  char* string;
}
IrInstructionLine;
#endif

typedef enum
{
  eRegName_None,
  eRegName_IP,
  eRegName_SP,
  eRegName_FP
}
eRegName;

typedef enum
{
  eInstrParam_None,
  eInstrParam_int8,
  eInstrParam_int32,
  eInstrParam_float32,
  eInstrParam_id,
  eInstrParam_reg,
}
eIrInstrParam;

typedef struct
{
  eIrInstrParam kind;

  union
  {
    int8 int8_val;
    int32 int32_val;
    float32 float32_val;
    eRegName reg;
    char* id;
  };
}
IrInstrParam;

typedef struct
{
  eOpcode opcode;
  IrInstrParam param;
  int source_line_nr;
}
IrInstruction;

typedef struct
{
  char sig[4];

  int code_offset;
  int code_size;

  int data_offset;
  int data_size;
  int sp;
}
BinImage;

typedef struct
{
//  String text;

  List* instr_list;
//  IrInstruction* instructions;
  int instr_count;

  uint8* data;
  int data_size;
  int32 sp;
}
IrProgram;

typedef enum
{
  eList_None,
  eList_ast_node,
  eList_symbol,
  eList_scope,
  eList_ir_instr,
  eList_type_pair,
  eList_data_area,
}
eList;

#define ITEM(VAR, NAME)\
  (((VAR)->kind == eList_##NAME) ? (VAR)->NAME : 0)

typedef struct ListItem
{
  eList kind;
  union
  {
    void* elem;
    AstNode* ast_node;
    Symbol* symbol;
    Scope* scope;
    IrInstruction* instr;
    TypePair* type_pair;
    DataArea* data_area;
  };
  struct ListItem* next;
  struct ListItem* prev;
}
ListItem;

typedef struct List
{
  eList kind;
  ListItem* first;
  ListItem* last;
  MemoryArena* arena;
}
List;

void DEBUG_print_ast_node_list(String* str, int indent_level, char* tag, List* node_list);
