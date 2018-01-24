#define BINIMAGE_SIGNATURE "HC"

typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef int bool;

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
#define local_persist static
#define global_var static
#define internal static

typedef struct String String;
typedef struct List List;
typedef struct Type Type;
typedef struct AstNode AstNode;
typedef struct Symbol Symbol;
typedef struct Scope Scope;
typedef struct TypePair TypePair;
typedef struct IrStmt IrStmt;
typedef struct IrArg IrArg;
typedef struct IrLeaderStmt IrLeaderStmt;
typedef struct IrLabel IrLabel;
typedef struct BasicBlock BasicBlock;

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

#define countof(ARRAY) (sizeof(ARRAY)/sizeof(ARRAY[0]))
#define KIND(VAR, KIND) (((VAR)->kind == KIND) ? (VAR) : 0)

void DEBUG_print_ast_nodes(String* str, int indent_level, char* tag, List* nodes);

typedef struct SourceLoc
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
  eToken_do,
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

typedef struct Token
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

  /* misc */
  eOperator_deref,
  eOperator_address_of,
  eOperator_selector,
  eOperator_indirect_selector,
  eOperator_Count,
}
eOperator;

typedef enum
{
  eLiteral_None,
  eLiteral_int,
  eLiteral_float,
  eLiteral_bool,
  eLiteral_char,
  eLiteral_str,
}
eLiteral;

typedef enum
{
  eScope_None,
  eScope_module,
  eScope_proc,
  eScope_args,
  eScope_params,
  eScope_while,
  eScope_block,
  eScope_struct,
}
eScope;

typedef enum
{
  eList_None,
  eList_ast_node,
  eList_symbol,
  eList_scope,
  eList_type_pair,
  eList_ir_stmt,
  eList_ir_leader_stmt,
  eList_ir_label,
  eList_basic_block,
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
    IrStmt* ir_stmt;
    IrLeaderStmt* ir_leader_stmt;
    IrLabel* ir_label;
    BasicBlock* basic_block;
  };
  struct ListItem* next;
  struct ListItem* prev;
}
ListItem;

typedef struct List
{
  eList kind;
  int count;
  ListItem* first;
  ListItem* last;
  MemoryArena* arena;
}
List;

typedef struct Scope
{
  eScope kind;

  int nesting_depth;
  struct Scope* encl_scope;
  AstNode* ast_node;
  int sym_count;
  int allocd_size; // aligned

  List decl_syms;
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

  /* arith ops */
  eIrOp_add,
  eIrOp_sub,
  eIrOp_mul,
  eIrOp_div,
  eIrOp_mod,
  eIrOp_neg,

  /* bit ops */
  eIrOp_bit_and,
  eIrOp_bit_or,
  eIrOp_bit_xor,
  eIrOp_bit_not,
  eIrOp_bit_shift_left,
  eIrOp_bit_shift_right,

  /* relational ops */
  eIrOp_eq,
  eIrOp_not_eq,
  eIrOp_less,
  eIrOp_less_eq,
  eIrOp_greater,
  eIrOp_greater_eq,
  eIrOp_logic_and,
  eIrOp_logic_or,
  eIrOp_logic_not,
  
  /* conv funcs */
  eIrOp_itof,
  eIrOp_itoc,
  eIrOp_itob,
  eIrOp_ftoi,
  eIrOp_ctoi,
  eIrOp_btoi,

  // op | arg1 | arg2 | result
  eIrOp_index_source,  // result = arg1[arg2]
  eIrOp_index_dest,    // result[arg2] = arg1
  eIrOp_deref_source,  // result = *arg1
  eIrOp_deref_dest,    // *result = arg1
  eIrOp_address_of,    // result = &arg1

  eIrOp_param,
}
eIrOp;

internal inline
bool is_ir_op_arithmetic(eIrOp op)
{
  return op >= eIrOp_add && op <= eIrOp_neg;
}

internal inline
bool is_ir_op_relational(eIrOp op)
{
  return op >= eIrOp_eq && op <= eIrOp_logic_not;
}

internal inline
bool is_ir_op_conv_func(eIrOp op)
{
  return op >= eIrOp_itof && op <= eIrOp_btoi;
}

internal inline
bool is_ir_op_bitwise_op(eIrOp op)
{
  return op >= eIrOp_bit_and && op <= eIrOp_bit_shift_right;
}

typedef struct
{
  MemoryArena* stmt_arena;
  MemoryArena* sym_arena;

  IrStmt* stmt_array;
  int stmt_count;
  List* label_list;

  int data_alignment;
  int total_stmt_count;
}
IrContext;

typedef enum
{
  eIrLabelTarget_None,
  eIrLabelTarget_stmt_nr,
//  eIrLabelTarget_basic_block,
}
eIrLabelTarget;

typedef struct IrLabel
{
  eIrLabelTarget kind;
  struct IrLabel* primary;
  union
  {
    int stmt_nr;
//    BasicBlock* basic_block;
  };
  char* name;
}
IrLabel;

typedef int NextUse;

typedef enum
{
  eIrArg_None,
  eIrArg_object,
  eIrArg_constant,
}
eIrArg;

typedef enum
{
  eX86Location_None,

  eX86Location_eax,
  eX86Location_ebx,
  eX86Location_ecx,
  eX86Location_edx,
  eX86Location_esi,
  eX86Location_edi,

  eX86Location_al,
  eX86Location_ah,
  eX86Location_bl,
  eX86Location_bh,
  eX86Location_cl,
  eX86Location_ch,
  eX86Location_dl,
  eX86Location_dh,

  eX86Location_memory,
  eX86Location_ebp,
  eX86Location_esp,

  eX86Location_Count,
}
eX86Location;

typedef struct X86Location
{
  eX86Location kind;
  Type* type;

  struct X86Location* parent;
  struct X86Location* sub[2];

  List occupants;
}
X86Location;

typedef struct IrArg
{
  bool is_live;
  NextUse next_use;
  Symbol* object;
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
  eIrStmt_return,
  eIrStmt_nop,
}
eIrStmt;

typedef struct IrStmt
{
  eIrStmt kind;
  IrLabel* label;

  union
  {
    IrLabel* goto_label;

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
      IrLabel* label;
    }
    cond_goto;

    struct IrStmt_call
    {
      AstNode* proc;
    }
    call;
  };
}
IrStmt;

typedef struct BasicBlock
{
  IrLabel* label;
  IrStmt** stmt_array;
  int stmt_count;
  List pred_list;
  List succ_list;
}
BasicBlock;

typedef struct IrLeaderStmt
{
  int stmt_nr;
  BasicBlock* block;
  IrStmt* stmt;
  IrLabel* label;
}
IrLeaderStmt;

typedef enum
{
  eX86Operand_None,
  eX86Operand_id,
  eX86Operand_constant,
  eX86Operand_register,
  eX86Operand_memory,
  eX86Operand_address,
}
eX86Operand;

typedef enum
{
  eX86Constant_None,
  eX86Constant_int,
  eX86Constant_float,
  eX86Constant_char,
}
eX86Constant;

typedef struct X86Operand
{
  eX86Operand kind;

  union
  {
    char* id;
    X86Location* reg;

    struct X86Operand_index
    {
      Type* type;
      struct X86Operand* base;
      struct X86Operand* offset;
    }
    index;

    struct X86Operand_constant
    {
      eX86Constant kind;

      union
      {
        int int_val;
        float float_val;
        char char_val;
      };
    }
    constant;
  };
}
X86Operand;

typedef enum
{
  eX86Stmt_None,
  eX86Stmt_call,
  eX86Stmt_pop,
  eX86Stmt_push,
  eX86Stmt_lea,
  eX86Stmt_mov,
  eX86Stmt_movss,
  eX86Stmt_add,
  eX86Stmt_addss,
  eX86Stmt_sub,
  eX86Stmt_subss,
  eX86Stmt_imul,
  eX86Stmt_mulss,
  eX86Stmt_idiv,
  eX86Stmt_cdq,
  eX86Stmt_divss,
  eX86Stmt_neg,
  eX86Stmt_cmp,
  eX86Stmt_cmpss,
  eX86Stmt_jz,
  eX86Stmt_jnz,
  eX86Stmt_jl,
  eX86Stmt_jle,
  eX86Stmt_jg,
  eX86Stmt_jge,
  eX86Stmt_jmp,
  eX86Stmt_nop,
  eX86Stmt_label,
  eX86Stmt_extern_proc,
  eX86Stmt_ret,
}
eX86Stmt;

typedef struct X86Stmt
{
  eX86Stmt opcode;
  X86Operand* operand1;
  X86Operand* operand2;
}
X86Stmt;

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
  eAstNode_return,
  eAstNode_if,
  eAstNode_while,
  eAstNode_do_while,
  eAstNode_loop_ctrl,
  eAstNode_enum_decl,
  eAstNode_struct_decl,
  eAstNode_union_decl,
  eAstNode_empty,
  eAstNode_node_list,
  eAstNode_array,
  eAstNode_index,
  eAstNode_pointer,
  eAstNode_assign,
  eAstNode_cast,
  eAstNode_actual_arg,
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

  IrLabel* label_true;
  IrLabel* label_false;
  IrLabel* label_next;
  IrLabel* label_begin;

  union
  {
    List node_list;

    struct
    {
      AstNode* expr;
      Symbol* param;
    }
    actual_arg;
    
    struct
    {
      AstNode* from_expr;
      AstNode* to_type;
    }
    cast;

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
      AstNode* args;
      AstNode* proc;

      Scope* param_scope;
      Symbol* retvar;
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
    }
    while_, do_while;

    struct
    {
      char* name;
      char* decorated_name;
      AstNode* args;
      AstNode* ret_type;
      AstNode* body;

      Scope* scope;
      Scope* preamble_scope;
      Scope* body_scope;
      Symbol* decl_sym;
      Symbol* retvar;

      IrLabel label_start;
      IrLabel label_return;
      IrStmt* ir_stmt_array;
      int ir_stmt_count;
      List* basic_blocks;
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
        char* str_val;
      };
      Symbol* constant;
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

typedef enum
{
  eStorageSpace_None,
  eStorageSpace_constant, // immediate
  eStorageSpace_local, // vars and temps
  eStorageSpace_param, // param at the call site
  eStorageSpace_arg, // formal arg to proc
  eStorageSpace_static, // module-level vars
}
eStorageSpace;

typedef enum
{
  eSymbol_None,
  eSymbol_constant,
}
eSymbol;

//FIXME: Discriminate symbols by kind : var, proc, etc.
//The proc symbol needs to have a ref to the retvar symbol and to the args.
typedef struct Symbol
{
  eSymbol kind;

  char* name;
  int order_nr;
  SourceLoc* src_loc;
  Scope* scope;
  AstNode* ast_node;
  Type* ty;

  eStorageSpace storage_space;
  int data_loc;
  int allocd_size; // aligned
  void* data;

  union
  {
    int int_val;
    float float_val;
    char char_val;
    char* str_val;
  };

  bool is_live;
  bool is_temp;
  NextUse next_use;

  // for natvis
  struct Symbol_locations
  {
    X86Location* _[eX86Location_Count];
  }
  locations;
}
Symbol;

typedef struct
{
  List scopes;
  Scope* active_scope;
  int nesting_depth;
  MemoryArena* sym_arena;
}
SymbolContext;

typedef struct X86Context
{
  MemoryArena* stmt_arena;
  X86Stmt* stmt_array;
  int stmt_count;

  int machine_word_size; // = stack width

  struct
  {
    X86Location eax;
    X86Location ebx;
    X86Location ecx;
    X86Location edx;
    X86Location esi;
    X86Location edi;

    X86Location ebp;
    X86Location esp;

    X86Location al;
    X86Location ah;
    X86Location bl;
    X86Location bh;
    X86Location cl;
    X86Location ch;
    X86Location dl;
    X86Location dh;
  };

  X86Location memory;

  // for natvis
  struct X86Context_registers
  {
    X86Location* _[14];
  }
  registers;
}
X86Context;

