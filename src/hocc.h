#define ARENA_SIZE (3*MEGABYTE)
#define SYMBOL_ARENA_SIZE (ARENA_SIZE / 4)
#define IR_CODE_ARENA_SIZE (ARENA_SIZE / 4)
#define BINIMAGE_SIGNATURE "HC"
#define MACHINE_WORD_SIZE 4

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

#define countof(ARRAY)\
  (sizeof(ARRAY)/sizeof(ARRAY[0]))

#define KIND(VAR, KIND)\
  (((VAR)->kind == KIND) ? (VAR) : 0)

typedef struct List List;
typedef struct Type Type;
typedef struct AstNode AstNode;
typedef struct Symbol Symbol;
typedef struct Scope Scope;
typedef struct TypePair TypePair;

void DEBUG_print_ast_nodes(String* str, int indent_level, char* tag, List* nodes);

typedef struct
{
  char* file_path;
  int line_nr;
  char* src_line;
}
SourceLoc;

typedef enum
{
  eLabel_None,
  eLabel_symbolic,
  eLabel_numeric,
}
eLabel;

typedef struct
{
  eLabel kind;
  union
  {
    char* name;
    int num;
  };
}
Label;

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
  eToken_star,
  eToken_fwd_slash,
  eToken_back_slash,
  eToken_plus,
  eToken_plus_plus,
  eToken_minus,
  eToken_minus_minus,
  eToken_mul,
  eToken_exclam,
//  eToken_exclam_eq,
  eToken_logic_not,
  eToken_eq,
  eToken_eq_eq,
  eToken_angle_right,
  eToken_angle_right_eq,
  eToken_angle_right_right,
  eToken_angle_left,
  eToken_angle_left_eq,
  eToken_angle_left_left,
  eToken_angle_left_right,
  eToken_ampersand,
  eToken_pipe,
  eToken_tilde,
  eToken_circumflex,
  eToken_and,
  eToken_or,
  eToken_xor,
  eToken_not,
  eToken_mod,
  eToken_unknown_char,
  eToken_end_of_input,

  eToken_asm,
  eToken_var,
  eToken_if,
  eToken_else,
  eToken_while,
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
  eToken_extern,
  eToken_int,
  eToken_float,
  eToken_bool,
  eToken_char,
  eToken_void,
  eToken_auto,

  eToken_id,
  eToken_int_val,
  eToken_float_val,
  eToken_str_val,
  eToken_char_val,
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
  struct TokenStream* last_state;
  Token token;
  char* text;
  char* cursor;

  SourceLoc src_loc;
}
TokenStream;

typedef enum
{
  eOperator_None,

  /* arithmetic ops */
  eOperator_add,
  eOperator_sub,
  eOperator_mul,
  eOperator_div,
  eOperator_mod,
  eOperator_neg,

  /* relational ops */
  eOperator_eq,
  eOperator_not_eq,
  eOperator_less,
  eOperator_less_eq,
  eOperator_greater,
  eOperator_greater_eq,

  /* logical ops */
  eOperator_logic_and,
  eOperator_logic_or,
  eOperator_logic_not,

  /* bit ops */
  eOperator_bit_and,
  eOperator_bit_or,
  eOperator_bit_xor,
  eOperator_bit_not,
  eOperator_bit_shift_left,
  eOperator_bit_shift_right,

  eOperator_deref,
  eOperator_address_of,
  eOperator_selector,
  eOperator_indirect_selector,
  eOperator_cast,
  eOperator_Count,
}
eOperator;

char* get_operator_printstr(eOperator op)
{
  char* str = "???";
  switch(op)
  {
    case eOperator_add:
      str = "+";
      break;
    case eOperator_sub:
      str = "-";
      break;
    case eOperator_mul:
      str = "*";
      break;
    case eOperator_div:
      str = "/";
      break;
    case eOperator_mod:
      str = "mod";
      break;
    case eOperator_neg:
      str = "-";
      break;
    case eOperator_deref:
#if 0
    case eOperator_pointer:
#endif
      str = "^";
      break;
    case eOperator_address_of:
      str = "&";
      break;
    case eOperator_selector:
      str = ".";
      break;
    case eOperator_indirect_selector:
      str = "->";
      break;
#if 0
    case eOperator_pre_decr:
    case eOperator_post_decr:
      str = "--";
      break;
    case eOperator_pre_incr:
    case eOperator_post_incr:
      str = "++";
      break;
#endif
    case eOperator_eq:
      str = "==";
      break;
    case eOperator_not_eq:
      str = "<>";
      break;
    case eOperator_less:
      str = "<";
      break;
    case eOperator_less_eq:
      str = "<=";
      break;
    case eOperator_greater:
      str = ">";
      break;
    case eOperator_greater_eq:
      str = ">=";
      break;
    case eOperator_logic_and:
      str = "and";
      break;
    case eOperator_logic_or:
      str = "or";
      break;
    case eOperator_logic_not:
      str = "not";
      break;
    case eOperator_bit_and:
      str = "&";
      break;
    case eOperator_bit_or:
      str = "|";
      break;
    case eOperator_bit_xor:
      str = "~";
      break;
    case eOperator_bit_not:
      str = "!";
      break;
    case eOperator_cast:
      str = ":";
      break;
  }
  return str;
}

typedef enum
{
  eLiteral_None,
  eLiteral_int,
  eLiteral_float,
  eLiteral_bool,
  eLiteral_char,
}
eLiteral;

typedef enum
{
  eScope_None = 0,
  eScope_module = 1 << 0,
  eScope_proc = 1 << 1,
  eScope_while = 1 << 2,
  eScope_block = 1 << 3,
  eScope_struct = 1 << 4,
}
eScope;

typedef enum
{
  eList_None,
  eList_ast_node,
  eList_symbol,
  eList_scope,
  eList_type_pair,
  eList_data_area,
}
eList;

typedef struct ListItem
{
  eList kind;
  union
  {
    void* elem;
    AstNode* ast_node;
    Symbol* symbol;
    Scope* scope;
    TypePair* type_pair;
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

typedef struct Scope
{
  eScope kind;

  int nesting_depth;
  int sym_order_nr;
  struct Scope* encl_scope;
  AstNode* ast_node;

  List decl_syms;

  int static_area_size;
  int locals_area_size;
  int ret_area_size;
  int args_area_size;
}
Scope;

typedef enum
{
  eLoopCtrl_None,
  eLoopCtrl_break,
  eLoopCtrl_continue,
}
eLoopCtrl;

typedef enum
{
  eModifier_None,
  eModifier_extern,
}
eModifier;

typedef enum
{
  eType_None,
  eType_typevar,
  eType_basic,
  eType_proc,
  eType_var,
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
  eBasicType_auto,
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
      Type* type;
    }
    var;

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
      int ndim;
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

typedef struct TypePair
{
  Type* key;
  Type* value;
}
TypePair;

typedef enum
{
  eIrOp_None,
  eIrOp_add,
  eIrOp_sub,
  eIrOp_mul,
  eIrOp_div,
  eIrOp_mod,
  eIrOp_neg,
  eIrOp_eq,
  eIrOp_not_eq,
  eIrOp_less,
  eIrOp_less_eq,
  eIrOp_greater,
  eIrOp_greater_eq,
  eIrOp_logic_and,
  eIrOp_logic_or,
  eIrOp_logic_not,
  eIrOp_bit_and,
  eIrOp_bit_or,
  eIrOp_bit_xor,
  eIrOp_bit_not,
  eIrOp_bit_shift_left,
  eIrOp_bit_shift_right,
  eIrOp_itof,
  eIrOp_ftoi,

  // op | arg1 | arg2 | dest 
  eIrOp_index_source,  // dest = arg1[arg2]
  eIrOp_index_dest,    // dest[arg2] = arg1
  eIrOp_deref_source,  // dest = *arg1
  eIrOp_deref_dest,    // *dest = arg1
  eIrOp_address_of,    // dest = &arg1
}
eIrOp;

typedef struct
{
  MemoryArena* ir_arena;
  MemoryArena* sym_arena;
  MemoryArena* arena;
  int stmt_count;
  int next_stmt_nr;
}
IrContext;

typedef enum
{
  eIrConstant_int,
  eIrConstant_float,
  eIrConstant_char,
}
eIrConstant;

typedef struct
{
  eIrConstant kind;
  union
  {
    int int_val;
    float float_val;
    char char_val;
  };
}
IrConstant;

typedef enum
{
  eIrArg_None,
  eIrArg_data_obj,
  eIrArg_const,
}
eIrArg;

typedef struct
{
  eIrArg kind;
  union
  {
    Symbol* data_obj;
    IrConstant const_;
  };
}
IrArg;

typedef enum
{
  eIrStmt_None,
  eIrStmt_assign,
  eIrStmt_goto,
  eIrStmt_cond_goto,
  eIrStmt_call,
  eIrStmt_param,
  eIrStmt_label,
  eIrStmt_return,
}
eIrStmt;

typedef struct
{
  eIrStmt kind;
  int nr;

  union
  {
    IrArg* param, *ret;
    Label* label;
    struct IrStmt_assign
    {
      eIrOp op;
      IrArg* arg1;
      IrArg* arg2;
      IrArg* result;
    }
    assign;

    struct IrStmt_cond_goto
    {
      eIrOp relop;
      IrArg* arg1;
      IrArg* arg2;
      Label* label;
    } 
    cond_goto;

    struct IrStmt_call
    {
      char* name;
      int param_count;
    }
    call;
  };
}
IrStmt;

typedef enum 
{
  eAstNode_None,
  eAstNode_id,
  eAstNode_asm_block,
  eAstNode_bin_expr,
  eAstNode_unr_expr,
  eAstNode_module,
  eAstNode_include,
  eAstNode_block,
  eAstNode_stmt,
  eAstNode_var,
  eAstNode_proc,
  eAstNode_call,
  eAstNode_basic_type,
  eAstNode_lit,
  eAstNode_str,
  eAstNode_return,
  eAstNode_if,
  eAstNode_while,
  eAstNode_loop_ctrl,
  eAstNode_enum_decl,
  eAstNode_struct_decl,
  eAstNode_union_decl,
  eAstNode_empty,
  eAstNode_arg_list,
  eAstNode_array,
  eAstNode_index,
  eAstNode_pointer,
  eAstNode_assign,
}
eAstNode;

typedef struct AstNode
{
  eAstNode kind;
  SourceLoc* src_loc;
  Type* ty;
  Type* eval_ty;
  eModifier modifier;
  IrArg* place;

  Label* label_true;
  Label* label_false;
  Label* label_next;
  Label* label_begin;

  union
  {
    struct
    {
      AstNode* dest_expr;
      AstNode* source_expr;
    }
    assign;

    struct
    {
      AstNode* pointee;
    }
    pointer;

    struct
    {
      AstNode* size_expr;
      AstNode* elem_expr;
      int size;
      int ndim;
    }
    array;

    struct
    {
      AstNode* array_expr;
      AstNode* i_expr;
      IrArg* place;   // L.place
      IrArg* offset;  // L.offset
      IrArg* i_place; // Elist.place
      Type* array_ty; // Elist.array
      int ndim;
    }
    index;

    struct
    {
      eBasicType kind;
    }
    basic_type;

    struct
    {
      List nodes;
    }
    arg_list;

    struct
    {
      char* name;
      AstNode* decl_ast;
      int order_nr;
      Scope* scope;
      Symbol* decl_sym;
    }
    id;

    struct
    {
      AstNode* left_operand;
      AstNode* right_operand;
      eOperator op;
    }
    bin_expr;

    struct
    {
      AstNode* operand;
      eOperator op;
    }
    unr_expr;

    struct
    {
      AstNode* expr;
      AstNode* arg_list;
      AstNode* proc;
    }
    call;

    struct
    {
      AstNode* expr;
      AstNode* proc;
      int nesting_depth;
    }
    ret;

    struct
    {
      eLoopCtrl kind;
      AstNode* loop;
      int nesting_depth;
    }
    loop_ctrl;

    struct
    {
      AstNode* cond_expr;
      AstNode* body;
      AstNode* else_body;
    }
    if_;

    struct
    {
      AstNode* cond_expr;
      AstNode* body;
      Scope* scope;
      Label label;
    }
    while_;
    
    struct
    {
      char* name;
      AstNode* arg_list;
      AstNode* ret_type;
      AstNode* body;
      Scope* scope;
      Symbol* decl_sym;
    }
    proc;

    struct
    {
      char* str_val;
    }
    str;

    struct
    {
      eLiteral kind;
      union
      {
        int int_val;
        float float_val;
        bool bool_val;
        char char_val;
      };
    }
    lit;

    struct
    {
      char* name;
      AstNode* type;
      Symbol* decl_sym;
    }
    var;

    struct
    {
      char* file_path;
      AstNode* body;
      List nodes;
      List procs;
      List vars;
      List includes;
      Scope* scope;
    }
    module;

    struct
    {
      char* file_path;
      AstNode* body;
    }
    include;

    struct
    {
      Scope* scope;
      List nodes;
      List vars;
      List stmts;
    }
    block;

    struct
    {
      char* name;
      AstNode* id;
      List* members;
    }
    enum_decl,
    union_decl, struct_decl; //record;

    struct
    {
      char* asm_text;
    }
    asm_block;
  };
}
AstNode;

typedef struct Symbol
{
  char* name;
  int order_nr;
  SourceLoc* src_loc;
  Scope* scope;
  AstNode* ast_node;
  Type* ty;
  Symbol* decl_sym;
  int data_loc;
  void* data;
  bool is_static_alloc;
}
Symbol;

typedef struct
{
  List scopes;
  Scope* active_scope;
  int nesting_depth;
  MemoryArena* arena;
}
SymbolContext;

