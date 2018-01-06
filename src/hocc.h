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
#define local_persist static
#define global_var static

typedef struct String String;
typedef struct List List;
typedef struct Type Type;
typedef struct AstNode AstNode;
typedef struct Symbol Symbol;
typedef struct Scope Scope;
typedef struct TypePair TypePair;
typedef struct IrStmt IrStmt;
typedef struct IrLeaderStmt IrLeaderStmt;
typedef struct IrLabel IrLabel;
typedef struct BasicBlock BasicBlock;
typedef struct StorageLocation StorageLocation;

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
  eList_ir_stmt,
  eList_ir_leader_stmt,
  eList_ir_label,
  eList_basic_block,
  eList_storage_location,
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
    StorageLocation* storage_location;
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
  struct Scope* encl_scope;
  AstNode* ast_node;
  int sym_count;
  int data_size;

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
}
eIrOp;

typedef struct
{
  MemoryArena* ir_arena;
  MemoryArena* sym_arena;
  IrStmt* stmt_array;
  int stmt_count;
  List* label_list;
}
IrContext;

typedef enum
{
  eIrConstant_int,
  eIrConstant_float,
  eIrConstant_char,
}
eIrConstant;

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
  eIrArg_object,
  eIrArg_constant,
}
eIrArg;

typedef struct
{
  eIrArg kind;
  union
  {
    Symbol* object;
    IrConstant constant;
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
    IrArg* param, *ret;
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
      char* name;
      int param_count;
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
  eX86Register_None,
  eX86Register_eax,
  eX86Register_ebx,
  eX86Register_ecx,
  eX86Register_edx,
  eX86Register_ebp,
  eX86Register_esp,
  eX86Register_esi,
  eX86Register_edi,
  eX86Register_Count,
}
eX86Register;

typedef enum
{
  eX86Operand_None,
  eX86Operand_id,
  eX86Operand_constant,
  eX86Operand_register,
  eX86Operand_indexed,
}
eX86Operand;

typedef struct X86Operand
{
  eX86Operand kind;
  union
  {
    char* id;
    int constant;
    eX86Register reg;

    struct X86Operand_indexed
    {
      int i;
      struct X86Operand* base;
    }
    indexed;
  };
}
X86Operand;

typedef enum
{
  eX86Stmt_None,
  eX86Stmt_mov,
  eX86Stmt_imul,
  eX86Stmt_label,
  eX86Stmt_jmp,
  eX86Stmt_je,
  eX86Stmt_nop,
}
eX86Stmt;

typedef struct X86Stmt
{
  eX86Stmt kind;
  union
  {
    struct
    {
      X86Operand* operand1;
      X86Operand* operand2;
    };
  };
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
  eAstNode_str,
  eAstNode_return,
  eAstNode_if,
  eAstNode_while,
  eAstNode_do_while,
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
  eAstNode_cast,
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

    struct {
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
      IrLabel label;
    }
    while_, do_while;

    struct
    {
      char* name;
      AstNode* arg_list;
      AstNode* ret_type;
      AstNode* body;
      Scope* scope;
      Symbol* decl_sym;
      Symbol* retvar;
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
  int data_loc;
  int data_size;
  void* data;
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

typedef enum
{
  eStorageLocation_None,
  eStorageLocation_memory,
  eStorageLocation_reg,
}
eStorageLocation;

typedef enum
{
  eMemoryStorage_None,
  eMemoryStorage_stack,
  eMemoryStorage_static,
}
eMemoryStorage;

typedef struct StorageLocation
{
  eStorageLocation kind;
  Symbol* object;

  union
  {
    struct StorageLocation_reg
    {
      eX86Register reg;
      List objects;
    }
    reg;

    struct StorageLocation_memory
    {
      eMemoryStorage kind;
      int loc;
      union
      {
        eX86Register stack_base;
        char* static_area;
      };
    }
    memory;
  };
}
StorageLocation;

