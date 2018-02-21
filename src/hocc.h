typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;

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

struct String;
struct List;
struct Type;
struct AstNode;
struct Symbol;
struct Scope;
struct TypePair;
struct IrStmt;
struct IrArg;
struct IrLeaderStmt;
struct Label;
struct BasicBlock;
struct X86Context;

struct PlatformFile
{
  char* path;
};

struct MemoryArenaUsage
{
  int total_avail;
  double in_use;
};

struct MemoryArena
{
  uint8* base;
  uint8* free;
  uint8* cap;
  struct MemoryArena* prev_arena;
  struct String* str;
};

struct String
{
  char* head;
  char* end;
  MemoryArena* arena;
};

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
  char* source_file;
}
OutFileNames;

struct SourceLoc
{
  char* file_path;
  int   line_nr;
  char* src_line;
};

enum eToken
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
};

struct Token
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
};

typedef struct
{
  char quote;
  int len;
  char* begin;
  char* end;
}
EscapedStr;

struct Lexer
{
  MemoryArena* arena;
  struct Lexer* last_state;
  Token token;
  char* text;
  char* cursor;
  SourceLoc src_loc;
};

enum eOperator
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
};

enum eLiteral
{
  eLiteral_None,
  eLiteral_int,
  eLiteral_float,
  eLiteral_bool,
  eLiteral_char,
  eLiteral_str,
};

enum eScope
{
  eScope_None,
  eScope_module,
  eScope_proc,
  eScope_args,
  eScope_params,
  eScope_while,
  eScope_block,
  eScope_struct,
};

enum eList
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
};

struct ListItem
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
};

struct List
{
  eList kind;
  MemoryArena* arena;
  ListItem* first;
  ListItem* last;
  int count;
};

struct Scope
{
  eScope kind;

  int nesting_depth;
  struct Scope* encl_scope;
  AstNode* ast_node;
  int sym_count;
  int allocd_size; // aligned
  List decl_syms;
};

enum eLoopCtrl
{
  eLoopCtrl_None,
  eLoopCtrl_break,
  eLoopCtrl_continue,
};

enum eModifier
{
  eModifier_None,
  eModifier_extern,
  eModifier_const,
};

enum eType
{
  eType_None,
  eType_typevar,
  eType_basic,
  eType_proc,
  eType_var,
  eType_product,
  eType_pointer,
  eType_array,
};

enum eBasicType
{
  eBasicType_None,
  eBasicType_void,
  eBasicType_int,
  eBasicType_float,
  eBasicType_char,
  eBasicType_bool,
  eBasicType_auto,
};

struct Type
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

  bool  equal(Type* type_b);
  int   set_width();
  Type* copy(MemoryArena* arena);
  Type* get_repr_type();
  void  set_union(Type* type_b);
  bool  unif(Type* type_b);
  void  append_printstr(String* str);
  char* get_printstr(MemoryArena* arena);
  bool  resolve(Type** resolved_type);
};

struct TypePass
{
  struct TypePair
  {
    Type* key;
    Type* value;
  };

  MemoryArena* arena;
  List* subst_list;
  int typevar_id = 1;

  Type* basic_type_bool;
  Type* basic_type_int;
  Type* basic_type_char;
  Type* basic_type_float;
  Type* basic_type_void;
  Type* basic_type_str;

  Type* create_var_type(Type* var_type);
  Type* create_basic_type(eBasicType kind);
  Type* create_proc_type(Type* args, Type* ret);
  Type* create_typevar();
  Type* create_product_type(Type* left, Type* right);
  Type* create_array_type(int size, Type* elem);
  Type* create_pointer_type(Type* pointee);

  static TypePass* create(MemoryArena* arena);
  void      init(MemoryArena* arena);
  Type*     type_subst(Type* type);
  TypePair* create_type_pair(Type* key, Type* value);
  TypePair* find_pair(Type* type);
  bool      process(AstNode* module);
};

struct TypePass_Set : TypePass
{
  bool visit_array(AstNode* array);
  bool visit_pointer(AstNode* pointer);
  bool visit_type(AstNode* type);
  bool visit_var(AstNode* var);
  bool visit_bin_expr(AstNode* bin_expr);
  bool visit_unr_expr(AstNode* unr_expr);
  bool visit_actual_arg(AstNode* call_arg);
  bool visit_id(AstNode* id);
  bool visit_actual_args(AstNode* args);
  bool visit_call(AstNode* call);
  bool visit_lit(AstNode* lit);
  bool visit_index(AstNode* index);
  bool visit_cast(AstNode* cast);
  bool visit_assign(AstNode* assign);
  bool visit_expr(AstNode* expr);
  bool visit_return(AstNode* ret);
  bool visit_if(AstNode* if_);
  bool visit_do_while(AstNode* do_while);
  bool visit_while(AstNode* while_);
  bool visit_block(AstNode* block);
  bool visit_block_stmt(AstNode* stmt);
  bool visit_formal_args(AstNode* args);
  bool visit_proc(AstNode* proc);
  bool visit_module_stmt(AstNode* stmt);
  bool visit_module(AstNode* module);
};

struct TypePass_Eval : TypePass
{
  bool visit_array(AstNode* array);
  bool visit_pointer(AstNode* pointer);
  bool visit_type(AstNode* type);
  bool visit_cast(AstNode* cast);
  bool visit_bin_expr(AstNode* bin_expr);
  bool visit_id(AstNode* id);
  bool visit_unr_expr(AstNode* unr_expr);
  bool visit_var(AstNode* var);
  bool visit_formal_args(AstNode* args);
  bool visit_actual_args(AstNode* args);
  bool visit_call(AstNode* call);
  bool visit_index(AstNode* index);
  bool visit_assign(AstNode* assign);
  bool visit_expr(AstNode* expr);
  bool visit_if(AstNode* if_);
  bool visit_block(AstNode* block);
  bool visit_do_while(AstNode* do_while);
  bool visit_while(AstNode* while_);
  bool visit_return(AstNode* ret);
  bool visit_block_stmt(AstNode* stmt);
  bool visit_proc(AstNode* proc);
  bool visit_module_stmt(AstNode* stmt);
  bool visit_module(AstNode* module);
};

struct TypePass_Resolve : TypePass
{
  bool visit_var(AstNode* var);
  bool visit_lit(AstNode* lit);
  bool visit_formal_args(AstNode* args);
  bool visit_bin_expr(AstNode* bin_expr);
  bool visit_unr_expr(AstNode* unr_expr);
  bool visit_id(AstNode* id);
  bool visit_actual_args(AstNode* args);
  bool visit_call(AstNode* call);
  bool visit_index(AstNode* index);
  bool visit_cast(AstNode* cast);
  bool visit_assign(AstNode* assign);
  bool visit_expr(AstNode* expr);
  bool visit_block(AstNode* block);
  bool visit_return(AstNode* ret);
  bool visit_if(AstNode* if_);
  bool visit_do_while(AstNode* do_while);
  bool visit_while(AstNode* while_);
  bool visit_array(AstNode* array);
  bool visit_pointer(AstNode* pointer);
  bool visit_type(AstNode* type);
  bool visit_block_stmt(AstNode* stmt);
  bool visit_proc(AstNode* proc);
  bool visit_module_stmt(AstNode* stmt);
  bool visit_module(AstNode* module);
};

struct TypePass_Check : TypePass
{
  bool visit_var(AstNode* var);
  bool visit_formal_args(AstNode* args);
  bool visit_cast(AstNode* cast);
  bool visit_bin_expr(AstNode* bin_expr);
  bool visit_unr_expr(AstNode* unr_expr);
  bool visit_actual_args(AstNode* args);
  bool visit_call(AstNode* call);
  bool visit_index(AstNode* index);
  bool visit_assign(AstNode* assign);
  bool visit_expr(AstNode* expr);
  bool visit_return(AstNode* ret);
  bool visit_do_while(AstNode* do_while);
  bool visit_while(AstNode* while_);
  bool visit_if(AstNode* if_);
  bool visit_block(AstNode* block);
  bool visit_block_stmt(AstNode* stmt);
  bool visit_proc(AstNode* proc);
  bool visit_module_stmt(AstNode* stmt);
  bool visit_module(AstNode* module);
};

enum eIrOp
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
};

struct Label
{
  struct Label* primary;
  int stmt_nr;
  char* name;

  static Label* create(MemoryArena* arena);
  static Label* create_by_name(MemoryArena* arena, char* name);
};

typedef int NextUse;

enum eX86Location
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
};

struct X86Location
{
  eX86Location kind;
  Type* type;

  struct X86Location* parent;
  struct X86Location* subloc[2];

  List occupants;

  X86Location* get_top();
  bool is_free();
  bool is_parent_free();
  bool is_subloc_free();
};

struct IrArg
{
  bool is_live;
  NextUse next_use;
  Symbol* object;
};

enum eIrStmt
{
  eIrStmt_None,
  eIrStmt_assign,
  eIrStmt_goto,
  eIrStmt_cond_goto,
  eIrStmt_call,
  eIrStmt_return,
  eIrStmt_nop,
};

struct IrStmt_Assign
{
  eIrOp op;
  IrArg* arg1;
  IrArg* arg2;
  IrArg* result;

  void update_object_live_info();
};

struct IrStmt_CondGoto
{
  eIrOp relop;
  IrArg* arg1;
  IrArg* arg2;
  Label* goto_label;
};

struct IrStmt_Goto
{
  Label* goto_label;
};

struct IrStmt_Call
{
  Label* name;
  Scope* param_scope;
  Symbol* retvar;
  int arg_count;
  bool is_extern;
};

struct IrStmt
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
};

struct BasicBlock
{
  Label* label;
  IrStmt** stmt_array;
  int stmt_count;
  List pred_list;
  List succ_list;
};

struct IrLeaderStmt
{
  int stmt_nr;
  BasicBlock* block;
  IrStmt* stmt;
  Label* label;
};

enum eX86Operand
{
  eX86Operand_None,
  eX86Operand_id,
  eX86Operand_constant,
  eX86Operand_register,
  eX86Operand_memory,
  eX86Operand_address,
};

enum eX86Constant
{
  eX86Constant_None,
  eX86Constant_int,
  eX86Constant_float,
  eX86Constant_char,
};

struct X86Operand_Index
{
  Type* type;
  struct X86Operand* base;
  struct X86Operand* offset;
};

struct X86Operand_Constant
{
  eX86Constant kind;

  union
  {
    int int_val;
    float float_val;
    char char_val;
  };
};

struct X86Operand
{
  eX86Operand kind;

  union
  {
    char* id;
    X86Location* reg;
    X86Operand_Index index;
    X86Operand_Constant constant;
  };
};

enum eX86Stmt
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
};

struct X86Stmt
{
  eX86Stmt opcode;
  X86Operand* operand1;
  X86Operand* operand2;
};

enum eAstNode
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
};

struct AstNode
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
};

struct Parser
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
};

enum eStorageSpace
{
  eStorageSpace_None,
  eStorageSpace_constant,
  eStorageSpace_local, // vars and temps
  eStorageSpace_actual_param, // params (actual args and retvar) at the call site
  eStorageSpace_formal_param, // params to proc 
  eStorageSpace_static, // module-level vars
};

enum eSymbol
{
  eSymbol_None,
  eSymbol_constant,
};

//FIXME: Discriminate symbols by kind : var, proc, etc.
//The proc symbol needs to have a ref to the retvar symbol and to the args.
struct Symbol
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

  void init_locations();
  bool is_in_location(X86Location* loc);
};

struct SymbolPass
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

  Symbol* create_const(Type* ty, SourceLoc* src_loc);
  Symbol* create_const_int(SourceLoc* src_loc, int int_val);
  Symbol* create_const_char(SourceLoc* src_loc, char char_val);
  Symbol* create_const_str(SourceLoc* src_loc, char* str_val);
  Symbol* create_const_float(SourceLoc* src_loc, float float_val);

  Scope* begin_scope(eScope kind, AstNode* ast_node);
  void   end_scope();
  Scope* begin_nested_scope(eScope kind, AstNode* ast_node);
  void   end_nested_scope();
  Symbol* add_decl(char* name, eStorageSpace storage_space, Scope* scope, AstNode* ast_node);
  void init(MemoryArena* gp_arena, MemoryArena* sym_arena, TypePass* type_pass);

  bool visit_lit(AstNode* lit);
  bool visit_array(AstNode* array);
  bool visit_expr(AstNode* expr);
  bool visit_cast(AstNode* cast);
  bool visit_index(AstNode* index);
  bool visit_call(AstNode* call);
  bool visit_actual_args(AstNode* args);
  bool visit_unr_expr(AstNode* unr_expr);
  bool visit_bin_expr(AstNode* bin_expr);
  bool visit_id(AstNode* id);
  bool visit_if(AstNode* if_);
  bool visit_do_while(AstNode* do_while);
  bool visit_while(AstNode* while_);
  bool visit_loop_ctrl(AstNode* stmt);
  bool visit_return(AstNode* ret);
  bool visit_block_stmt(AstNode* stmt);
  bool visit_block(AstNode* block);
  bool visit_proc_body(AstNode* proc);
  bool visit_formal_args(Scope* param_scope, AstNode* args);
  bool visit_module_proc(AstNode* proc);
  bool visit_module_var(AstNode* module, AstNode* var);
  bool visit_module(AstNode* module);
  bool visit_pointer(AstNode* pointer);
  bool visit_assign(AstNode* assign);
  bool visit_var(AstNode* var);
  bool visit_formal_arg(Scope* proc_scope, AstNode* arg);

  bool process(AstNode* module);
};

struct IrContext
{
#define IrContext_DEBUG_line_nr_gutter_width 10

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

  void emit_assign(eIrOp op, IrArg* arg1, IrArg* arg2, IrArg* result);
  void emit_label(Label* label);
  void emit_nop();
  void emit_cond_goto(eIrOp relop, IrArg* arg1, IrArg* arg2, Label* label);
  void emit_goto(Label* goto_label);
  void emit_call(Label* name, Scope* param_scope, Symbol* retvar, bool is_extern);
  void emit_return();

  bool visit_bin_expr(Scope* scope, AstNode* bin_expr);
  void visit_id(AstNode* id);
  bool visit_unr_expr(Scope* scope, AstNode* unr_expr);
  void visit_lit(Scope* scope, AstNode* lit);
  bool visit_bool_unr_expr(Scope* scope, AstNode* unr_expr);
  bool visit_actual_args(Scope* scope, AstNode* args);
  void visit_call(Scope* scope, AstNode* call);
  bool visit_index(Scope* scope, AstNode* index);
  bool visit_index_with_offset(Scope* scope, AstNode* index);
  bool visit_assign(Scope* scope, AstNode* assign);
  bool visit_cast(Scope* scope, AstNode* cast);
  bool visit_expr(Scope* scope, AstNode* expr);
  bool visit_block(Scope* scope, AstNode* block);
  bool visit_bool_bin_expr(Scope* scope, AstNode* bin_expr);
  bool visit_bool_id(Scope* scope, AstNode* id);
  bool visit_bool_call(Scope* scope, AstNode* call);
  bool visit_bool_cast(Scope* scope, AstNode* cast);
  void visit_bool_lit(Scope* scope, AstNode* lit);
  bool visit_bool_expr(Scope* scope, AstNode* expr);
  bool visit_do_while(Scope* scope, AstNode* do_while);
  bool visit_while(Scope* scope, AstNode* while_);
  bool visit_if(Scope* scope, AstNode* if_);
  bool visit_return(Scope* scope, AstNode* ret);
  bool visit_loop_ctrl(Scope* scope, AstNode* loop_ctrl);
  bool visit_var(Scope* scope, AstNode* var);
  bool visit_block_stmt(Scope* scope, AstNode* stmt);
  void visit_formal_args(Scope* scope, AstNode* args);
  bool visit_proc(Scope* scope, AstNode* proc);
  void visit_module_var(Scope* scope, AstNode* var);
  bool visit_module_stmt(Scope* scope, AstNode* stmt);
  bool visit_module(AstNode* module);

  void DEBUG_print_ir_op(String* text, eIrOp op);
  void DEBUG_print_ir_arg(String* text, IrArg* arg);
  void DEBUG_print_ir_stmt(String* text, IrStmt* stmt);
  void DEBUG_print_basic_block(String* text, BasicBlock* bb);
  void DEBUG_print_ir_code(List* procs, char* file_path);

  void init(MemoryArena* gp_arena, MemoryArena* stmt_arena, TypePass* type_pass, SymbolPass* sym_pass);
  void reset();
  IrArg* create_arg_temp_object(Scope* scope, Type* ty, SourceLoc* src_loc);
  IrArg* create_arg_existing_object(Symbol* object);
  void partition_basic_blocks_proc(AstNode* proc);
  void partition_basic_blocks_module(AstNode* module);
  Symbol* create_temp_object(Scope* scope, Type* ty, SourceLoc* src_loc);
  void alloc_data_object_incremental(Symbol* sym, Scope* scope);
  void alloc_scope_data_objects(Scope* scope);
  void alloc_data_object(Symbol* sym, Scope* scope);
  void start_basic_block(List* leaders, int at_stmt_nr, IrStmt* stmt_array, int stmt_count);
  static eIrOp conv_operator_to_ir_op(eOperator op);
  Label* get_label_at(int stmt_nr);
  IrLeaderStmt* get_leader_stmt(List* leaders, int stmt_nr);
  IrLeaderStmt* create_leader_stmt(MemoryArena* arena, int stmt_nr, IrStmt* stmt);
  void insert_leader_stmt(List* leaders, int stmt_nr, IrStmt* stmt);
  Label* normalize_jump_target_labels(IrStmt* stmt);
  int get_proc_arg_size(AstNode* args);
  static void gen_label_name(MemoryArena* arena, Label* label);
  static char* new_tempvar_name(MemoryArena* arena, char* label);
  static bool is_cast_op(eIrOp ir_op);
};

struct X86Context
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

  static void print_register(String* text, X86Location* reg);
  char* make_type_directive(Type* type);
  void print_operand(String* text, X86Operand* operand);
  void print_opcode(String* text, eX86Stmt opcode);
  void print_stmt(String* text, X86Stmt* stmt);
  X86Stmt* create_stmt(eX86Stmt opcode);
  void new_object_location_entry(Symbol* object, X86Location* loc);
  void remove_object_from_location(Symbol* object, X86Location* loc);
  void add_object_to_location(Symbol* object, X86Location* loc);
  void clean_register(X86Location* loc);
  void clean_register_all_sizes(X86Location* loc);
  void set_exclusive_object_location(Symbol* object, X86Location* loc);
  bool is_register_location(X86Location* loc);
  bool is_memory_location(X86Location* loc);
  bool type_fits_into_register(Type* type, X86Location* reg);
  X86Location* find_free_register(Type* type);
  X86Location* lookup_object_location(Symbol* object);
  bool is_object_in_register(Symbol* object);
  bool is_single_occupant_register(X86Location* reg, Symbol* object);
  X86Operand* make_index_operand(eX86Operand kind, Symbol* object);
  X86Operand* make_register_operand(X86Location* reg);
  X86Operand* make_int_constant_operand(int int_val);
  X86Operand* make_memory_operand(Type* type, X86Operand* base, X86Operand* offset);
  X86Operand* make_object_address_operand(Symbol* object);
  X86Operand* make_object_memory_operand(Symbol* object);
  X86Operand* make_object_operand(Symbol* object);
  X86Operand* make_id_operand(char* id);
  void emit_mov(Type* type, X86Operand* dest_operand, X86Operand* source_operand);
  void load_object_value(Symbol* object, X86Location* dest_loc);
  void load_object_address(Symbol* object, X86Location* dest_loc);
  void load_object(Symbol* object, X86Location* dest_loc);
  void store_object(Symbol* object);
  eX86Stmt conv_ir_op_to_x86_opcode(eIrOp ir_op, Type* type);
  X86Location* get_first_fit_register(Type* type);
  X86Location* find_least_used_register(Type* type);
  void add_object_to_memory(Symbol* object);
  void save_object_to_memory(Symbol* object);
  void save_register(X86Location* reg, bool free_reg);
  void save_register_all_sizes(X86Location* loc, bool free_reg);
  X86Location* get_best_available_register(Type* type);
  void discard_unused_arg(IrArg* arg, X86Location* arg_loc);
  void discard_all_unused_args(IrStmt_Assign* assign);
  void save_all_registers(bool free_reg);
  void write_data_bytes(String* text, uint8* p_data, int data_size);
  void write_static_data_text(String* text, Scope* scope);
  void init_registers();
  void init(MemoryArena* gp_arena, MemoryArena* stmt_arena, MemoryArena* text_arena,
            TypePass* type_pass, IrContext* ir_context, SymbolPass* sym_pass);

  void gen_divmod_op(IrStmt_Assign* assign);
  void gen_index_source(IrStmt_Assign* assign);
  void gen_index_dest(IrStmt_Assign* assign);
  void gen_deref_source(IrStmt_Assign* assign);
  void gen_deref_dest(IrStmt_Assign *assign);
  void gen_equal(IrStmt_Assign* assign);
  void gen_address_of(IrStmt_Assign* assign);
  void gen_bin_expr(IrStmt_Assign* assign);
  void gen_unr_expr(IrStmt_Assign* assign);
  void gen_assign(IrStmt_Assign* assign);
  void gen_goto(IrStmt_Goto* goto_);
  void gen_cond_goto(IrStmt_CondGoto* cond_goto);
  void gen_call(IrStmt_Call* call);
  void gen_basic_block(BasicBlock* bb);
  void gen_extern_proc(AstNode* proc);
  void gen_proc(AstNode* proc);
  void gen_module(AstNode* module);
  void gen(AstNode* module, char* title);
};

