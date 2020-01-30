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
#define local_persist static
#define global_var static
#define internal static
#define true 1
#define false 0

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
typedef struct Label Label;
typedef struct BasicBlock BasicBlock;
typedef struct X86Context X86Context;

typedef struct
{
  char* path;
}
PlatformFile;

typedef struct 
{
  int total_avail;
  double in_use;
}
MemoryArenaUsage;

typedef struct MemoryArena
{
  uint8* base;
  uint8* free;
  uint8* cap;
  struct MemoryArena* prev_arena;
  struct String* str;
}
MemoryArena;

typedef struct String
{
  char* head;
  char* end;
  MemoryArena* arena;
}
String;

void add_object_to_memory(X86Context* x86_ctx, Symbol* object);

int platform_printf(char* format, ...);
void* platform_alloc_memory(int size);
int platform_sprintf_va(char* buffer, char* format, va_list args);
int platform_printf_va(char* format, va_list args);
int platform_file_write_bytes(char* file_path, uint8* bytes, int count);
int platform_sprintf(char* buffer, char* format, ...);
int platform_sscanf(char* buffer, char* format, ...);
bool platform_file_identity(PlatformFile* file_A_arg, PlatformFile* file_B_arg);
PlatformFile* platform_file_open(MemoryArena* arena, char* file_path);
char* platform_file_read_text(MemoryArena* arena, char* file_path);

#define KIND(VAR, KIND) (((VAR)->kind == KIND) ? (VAR) : 0)

typedef struct
{
  char* title;
  char* working_dir;
  char* asm_file;
  char* obj_file;
  char* source_file;
}
OutFileNames;

typedef struct 
{
  char* file_path;
  int   line_nr;
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
  eToken_not,
  eToken_percent,
  eToken_unknown_char,
  eToken_end_of_input,

  eToken_asm,
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
  eToken_const,
  eToken_int,
  eToken_float,
  eToken_bool,
  eToken_char,
  eToken_void,
  eToken_auto,
  eToken_cast,

  eToken_id,
  eToken_int_val,
  eToken_float_val,
  eToken_str_val,
  eToken_char_val,
  eToken_asm_text,

  eToken_Count,
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

typedef struct
{
  char quote;
  int len;
  char* begin;
  char* end;
}
EscapedStr;

typedef struct 
{
  MemoryArena* arena;
  struct Lexer* last_state;
  Token token;
  char* text;
  char* cursor;
  SourceLoc src_loc;
}
Lexer;

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
  eList_file,
}
eList;

typedef struct ListItem
{
  eList kind;
  struct ListItem* next;
  struct ListItem* prev;

  union
  {
    void*       elem;
    AstNode*    ast_node;
    Symbol*     symbol;
    Scope*      scope;
    TypePair*   type_pair;
    IrStmt*     ir_stmt;
    IrLeaderStmt* ir_leader_stmt;
    Label*      ir_label;
    BasicBlock* basic_block;
    PlatformFile* file;
  };
}
ListItem;

typedef struct List
{
  eList kind;
  MemoryArena* arena;
  ListItem* first;
  ListItem* last;
  int count;
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
  eModifier_const,
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
      Type* elem;
    }
    array;

    struct Type_Typevar
    {
      int id;
    }
    typevar;
  };
}
Type;

typedef struct 
{
  MemoryArena* arena;
  List* subst_list;
  int typevar_id;

  Type* basic_type_bool;
  Type* basic_type_int;
  Type* basic_type_char;
  Type* basic_type_float;
  Type* basic_type_void;
  Type* basic_type_str;
}
TypePass;

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

  /* conv ops */
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

typedef struct Label
{
  struct Label* primary;
  int stmt_nr;
  char* name;
}
Label;

typedef int NextUse;

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

  eX86Location_xmm0,
  eX86Location_xmm1,
  eX86Location_xmm2,
  eX86Location_xmm3,
  eX86Location_xmm4,
  eX86Location_xmm5,
  eX86Location_xmm6,
  eX86Location_xmm7,

  eX86Location_Count,
}
eX86Location;

typedef struct X86Location
{
  eX86Location kind;
  Type* type;

  struct X86Location* parent;
  struct X86Location* subloc[2];

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
  eIrStmt_return,
  eIrStmt_nop,
}
eIrStmt;

typedef struct 
{
  eIrOp op;
  IrArg* arg1;
  IrArg* arg2;
  IrArg* result;
}
IrStmt_Assign;

typedef struct 
{
  eIrOp relop;
  IrArg* arg1;
  IrArg* arg2;
  Label* goto_label;
}
IrStmt_CondGoto;

typedef struct 
{
  Label* goto_label;
}
IrStmt_Goto;

typedef struct 
{
  Label* name;
  Scope* param_scope;
  Symbol* retvar;
  int arg_count;
  bool is_extern;
}
IrStmt_Call;

typedef struct IrStmt
{
  eIrStmt kind;
  Label* label;

  union
  {
    IrStmt_Assign   assign;
    IrStmt_Goto     goto_;
    IrStmt_CondGoto cond_goto;
    IrStmt_Call     call;
  };
}
IrStmt;

typedef struct BasicBlock
{
  Label* label;
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
  Label* label;
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

typedef struct 
{
  Type* type;
  struct X86Operand* base;
  struct X86Operand* offset;
}
X86Operand_Index;

typedef struct X86Operand_Constant
{
  eX86Constant kind;

  union
  {
    int int_val;
    float float_val;
    char char_val;
  };
}
X86Operand_Constant;

typedef struct X86Operand
{
  eX86Operand kind;

  union
  {
    char* id;
    X86Location* reg;
    X86Operand_Index index;
    X86Operand_Constant constant;
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
  eX86Stmt_cdq,

  eX86Stmt_mov,
  eX86Stmt_add,
  eX86Stmt_sub,
  eX86Stmt_imul,
  eX86Stmt_idiv,
  eX86Stmt_neg,
  eX86Stmt_or,
  eX86Stmt_and,
  eX86Stmt_not,

  eX86Stmt_jz,
  eX86Stmt_jnz,
  eX86Stmt_jl,
  eX86Stmt_jle,
  eX86Stmt_jg,
  eX86Stmt_jge,
  eX86Stmt_jmp,

  eX86Stmt_cmp,
  eX86Stmt_ucomiss,  // floating point cmp

  eX86Stmt_movss,
  eX86Stmt_addss,
  eX86Stmt_subss,
  eX86Stmt_mulss,
  eX86Stmt_divss,

  eX86Stmt_jb,
  eX86Stmt_jbe,
  eX86Stmt_ja,
  eX86Stmt_jae,
  eX86Stmt_je,
  eX86Stmt_jne,

  eX86Stmt_cvtsi2ss,
  eX86Stmt_cvttss2si,

  eX86Stmt_nop,
  eX86Stmt_label,
  eX86Stmt_extern_proc,
  eX86Stmt_ret,
}
eX86Stmt;

typedef struct 
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
  eAstNode_call_arg,
}
eAstNode;

typedef struct AstNode
{
  eAstNode   kind;
  SourceLoc* src_loc;
  Type*      ty;
  Type*      eval_ty;
  IrArg*     place;

  Label* label_true;
  Label* label_false;
  Label* label_next;
  Label* label_begin;

  union
  {
    struct
    {
      List node_list;
    }
    args;

    struct
    {
      AstNode* expr;
      Symbol* param;
    }
    call_arg;

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
    while_,
    do_while;

    struct
    {
      char* name;
      AstNode* args;
      AstNode* ret_type;
      AstNode* body;
      eModifier modifier;

      Scope* scope;
      Scope* param_scope;
      Symbol* decl_sym;
      Symbol* retvar;

      // If the proc is external, then this is the decorated name.
      Label label_name;

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
      AstNode* init_expr;
      Symbol* decl_sym;
      eModifier modifier;
    }
    var;

    struct
    {
      char* file_path;
      List nodes;
      List procs;
      List vars;
      Scope* scope;
    }
    module;

    struct
    {
      char* file_path;
      PlatformFile* file;
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
    union_decl,
    struct_decl;

    struct
    {
      char* asm_text;
    }
    asm_block;
  };
}
AstNode;

typedef struct Parser
{
  MemoryArena* arena;
  Token token;
  SourceLoc src_loc;
  char* text;
  char* cursor;
  struct Parser* last_state;
  
  AstNode* module;
  PlatformFile* file;
  List* includes;
}
Parser;

typedef enum 
{
  eStorageSpace_None,
  eStorageSpace_constant,
  eStorageSpace_local, // vars and temps
  eStorageSpace_actual_param, // params (actual args and retvar) at the call site
  eStorageSpace_formal_param, // params to proc 
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
  void* data;
  int allocd_size; // aligned

  union
  {
    int int_val;
    float float_val;
    char char_val;
    char* str_val;
  };

  bool is_live;
  bool is_live_on_exit;
  NextUse next_use;

  // natvis
  struct Symbol_Locations
  {
    X86Location* _[eX86Location_Count];
  }
  locations;
}
Symbol;

typedef struct 
{
  Type* basic_type_bool;
  Type* basic_type_int;
  Type* basic_type_char;
  Type* basic_type_float;
  Type* basic_type_void;
  Type* basic_type_str;

  MemoryArena* gp_arena;
  MemoryArena* sym_arena;
  List scopes;
  Scope* module_scope;
  Scope* active_scope;
  int nesting_depth;
  int data_alignment;
  X86Context* x86_context;
}
SymbolPass;

typedef struct 
{
  Type* basic_type_bool;
  Type* basic_type_int;
  Type* basic_type_char;
  Type* basic_type_float;
  Type* basic_type_void;
  Type* basic_type_str;

  MemoryArena* gp_arena;
  SymbolPass* sym_pass;
  X86Context* x86_context;

  MemoryArena* stmt_arena;
  IrStmt* stmt_array;
  int stmt_count;
  List* label_list;

  int data_alignment;
  int total_stmt_count;

  Symbol* bool_true;
  Symbol* bool_false;

  int current_alloc_offset;
}
IrPass;

typedef struct X86Context
{
  Type* basic_type_bool;
  Type* basic_type_int;
  Type* basic_type_char;
  Type* basic_type_float;
  Type* basic_type_void;
  Type* basic_type_str;

  MemoryArena* gp_arena;
  MemoryArena* stmt_arena;
  X86Stmt* stmt_array;
  int stmt_count;

  int machine_word_size; // = stack width
  int data_alignment;

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

    X86Location xmm0;
    X86Location xmm1;
    X86Location xmm2;
    X86Location xmm3;
    X86Location xmm4;
    X86Location xmm5;
    X86Location xmm6;
    X86Location xmm7;
  };

  X86Location memory;

  // natvis
  struct X86Context_Registers
  {
    X86Location* _[22];
  }
  registers;
  int register_count;

  Symbol* float_minus_one;
  String* text;
}
X86Context;

