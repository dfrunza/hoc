#define BINIMAGE_SIGNATURE "HC"

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

struct HFile
{
  struct Impl;

  char* path;

  Impl* impl;
};

struct MemoryArena
{
  struct Usage
  {
    int total_avail;
    double in_use;
  };

  uint8* base;
  uint8* free;
  uint8* cap;
  struct MemoryArena* prev_arena;
  char* label;
  struct String* str;

  static MemoryArena* create(int size);
  static void         pop(MemoryArena** arena);
  static MemoryArena* push(MemoryArena** arena, int size);
  static void         begin_temp_memory(MemoryArena** arena);
  static void         end_temp_memory(MemoryArena** arena);
  void  dealloc();
  void* push_struct(int elem_size, int count);
  Usage usage();
  void  check_bounds(int elem_size, void* ptr);
};

struct String
{
  char* head;
  char* end;
  MemoryArena* arena;

  static String* create(MemoryArena* arena);
  void  init(MemoryArena* arena);
  int   len();
  void  append(char* cstr);
  int   printf_va(char* fmessage, va_list args);
  int   printf(char* ftext, ...);
  int   printfln(char* fline, ...);
  void  println();
  void  tidyup();
  void  free();
  char* cap();
  bool  dump_to_file(char* file_path);
};

namespace Platform
{
  void*  alloc_memory(int size);
  int    printf(char* format, ...);
  int    printf_va(char* format, va_list args);
  int    sprintf(char* buffer, char* format, ...);
  int    sprintf_va(char* buffer, char* format, va_list args);
  int    sscanf(char* buffer, char* format, ...);
  int    stdin_read(char* buf, int buf_size);
  int    file_read_bytes(MemoryArena* arena, uint8** bytes, char* file_path, int alloc_extra);
  int    file_write_bytes(char* file_path, uint8* bytes, int count);
  char*  file_read_text(MemoryArena* arena, char* file_path);
  HFile* file_open(MemoryArena* arena, char* filename);
  bool   file_identity(HFile* file_A, HFile* file_B);
  char*  path_find_leaf(char* file_path);
  char*  path_make_leaf(char* file_path, bool with_extension);
  char*  path_make_dir(char* file_path);
};

#define KIND(VAR, KIND) (((VAR)->kind == KIND) ? (VAR) : 0)

struct SourceLoc
{
  char* file_path;
  int line_nr;
  char* src_line;
};

enum struct eToken
{
  None,
  /* 'Simple' tokens must be listed at the beginning of the enum */
  dot,
  arrow_right,
  open_bracket,
  close_bracket,
  open_parens,
  close_parens,
  open_brace,
  close_brace,
  semicolon,
  colon,
  comma,
  star,
  fwd_slash,
  back_slash,
  plus,
  plus_plus,
  minus,
  minus_minus,
  mul,
  exclam,
//  eToken_exclam_eq,
  logic_not,
  eq,
  eq_eq,
  angle_right,
  angle_right_eq,
  angle_right_right,
  angle_left,
  angle_left_eq,
  angle_left_left,
  angle_left_right,
  ampersand,
  pipe,
  tilde,
  circumflex,
  and_,
  or_,
  not_,
  mod,
  unknown_char,
  end_of_input,

  asm_,
  if_,
  else_,
  do_,
  while_,
  proc_,
  struct_,
  union_,
  return_,
  break_,
  continue_,
  goto_,
  include,
  enum_,
  true_,
  false_,
  extern_,
  const_,
  int_,
  float_,
  bool_,
  char_,
  void_,
  auto_,

  id,
  int_val,
  float_val,
  str_val,
  char_val,
  asm_text,
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

struct Lexer
{
  MemoryArena* arena;
  struct Lexer* last_state;
  Token token;
  char* text;
  char* cursor;

  SourceLoc src_loc;
};

enum struct eOperator
{
  None,

  /* arithmetic ops */
  add,
  sub,
  mul,
  div,
  mod,
  neg,

  /* relational ops */
  eq,
  not_eq_,
  less,
  less_eq,
  greater,
  greater_eq,

  /* logical ops */
  logic_and,
  logic_or,
  logic_not,

  /* bit ops */
  bit_and,
  bit_or,
  bit_xor,
  bit_not,
  bit_shift_left,
  bit_shift_right,

  /* misc */
  deref,
  address_of,
  selector,
  indirect_selector,

  Count,
};

enum struct eLiteral
{
  None,
  int_,
  float_,
  bool_,
  char_,
  str,
};

enum struct eScope
{
  None,
  module,
  proc,
  args,
  params,
  while_,
  block,
  struct_,
};

enum struct eList
{
  None,
  ast_node,
  symbol,
  scope,
  type_pair,
  ir_stmt,
  ir_leader_stmt,
  ir_label,
  basic_block,
  file,
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
    HFile*      file;
  };
};

struct List
{
  eList kind;
  MemoryArena* arena;
  ListItem* first;
  ListItem* last;
  int count;

  static List* create(MemoryArena* arena, eList kind);
  void init(MemoryArena* arena, eList kind);
  void append(void* elem, eList kind);
  void append_item(ListItem* item);
  void remove_item(ListItem* item);
  void prepend_item(ListItem* item);
  void prepend(void* elem, eList kind);
  void replace_item_at(List* list_b, ListItem* at_b_item);
  void join(List* list_b);
  void insert_item_before(ListItem* at_li, ListItem* new_li);
  void insert_before(ListItem* at_li, void* elem, eList kind);
  void insert_item_after(ListItem* at_li, ListItem* new_li);
  void insert_after(ListItem* at_li, void* elem, eList kind);
  void clear();
  ListItem* remove_first_item();
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

enum struct eLoopCtrl
{
  None,
  break_,
  continue_,
};

enum struct eModifier
{
  None,
  extern_,
  const_,
};

enum struct eType
{
  None,
  typevar,
  basic,
  proc,
  var,
  product,
  pointer,
  array,
};

enum struct eBasicType
{
  None,
  void_,
  int_,
  float_,
  char_,
  bool_,
  auto_,
};

struct Type_Var
{
  Type* type;
};

struct Type_Basic
{
  eBasicType kind;
};

struct Type_Pointer
{
  Type* pointee;
};

struct Type_Proc
{
  Type* args;
  Type* ret;
};

struct Type_Product
{
  Type* left;
  Type* right;
};

struct Type_Array
{
  int size;
  Type* elem;
};

struct Type_Typevar
{
  int id;
};

struct Type
{
  eType kind;
  Type* repr_type; // representative member of the set of equivalent types
  int width;

  union
  {
    Type_Var     var;
    Type_Basic   basic;
    Type_Pointer pointer;
    Type_Proc    proc;
    Type_Product product;
    Type_Array   array;
    Type_Typevar typevar;
  };

  bool  equal(Type* type_b);
  int   set_width();
  Type* copy(MemoryArena* arena);
  Type* get_repr_type();
  void  set_union(Type* type_b);
  bool  unif(Type* type_b);
  void  append_printstr(String* str);
  char* get_printstr(MemoryArena* arena);
};

struct TypePair
{
  Type* key;
  Type* value;
};

enum struct eIrOp
{
  None,

  /* arith ops */
  add,
  sub,
  mul,
  div,
  mod,
  neg,

  /* bit ops */
  bit_and,
  bit_or,
  bit_xor,
  bit_not,
  bit_shift_left,
  bit_shift_right,

  /* relational ops */
  eq,
  not_eq_,
  less,
  less_eq,
  greater,
  greater_eq,
  logic_and,
  logic_or,
  logic_not,

  /* conv ops */
  itof,
  itoc,
  itob,
  ftoi,
  ctoi,
  btoi,

  // op | arg1 | arg2 | result
  index_source,  // result = arg1[arg2]
  index_dest,    // result[arg2] = arg1
  deref_source,  // result = *arg1
  deref_dest,    // *result = arg1
  address_of,    // result = &arg1

  param,
};

struct Label
{
  struct Label* primary;
  int stmt_nr;
  char* name;
};

typedef int NextUse;

enum struct eX86Location
{
  None,

  eax,
  ebx,
  ecx,
  edx,
  esi,
  edi,

  al,
  ah,
  bl,
  bh,
  cl,
  ch,
  dl,
  dh,

  memory,
  ebp,
  esp,

  xmm0,
  xmm1,
  xmm2,
  xmm3,
  xmm4,
  xmm5,
  xmm6,
  xmm7,

  Count,
};

struct X86Location
{
  eX86Location kind;
  Type* type;

  struct X86Location* parent;
  struct X86Location* subloc[2];

  List occupants;
};

struct IrArg
{
  bool is_live;
  NextUse next_use;
  Symbol* object;
};

enum struct eIrStmt
{
  None,
  assign,
  goto_,
  cond_goto,
  call,
  return_,
  nop,
};

struct IrStmt_Assign
{
  eIrOp op;
  IrArg* arg1;
  IrArg* arg2;
  IrArg* result;
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

enum struct eX86Operand
{
  None,
  id,
  constant,
  register_,
  memory,
  address,
};

enum struct eX86Constant
{
  None,
  int_,
  float_,
  char_,
};

struct X86Operand_index
{
  Type* type;
  struct X86Operand* base;
  struct X86Operand* offset;
};

struct X86Operand_constant
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
    X86Operand_index index;
    X86Operand_constant constant;
  };
};

enum struct eX86Stmt
{
  None,
  call,
  pop,
  push,
  lea,
  cdq,

  mov,
  add,
  sub,
  imul,
  idiv,
  neg,
  or_,
  and_,
  not_,

  jz,
  jnz,
  jl,
  jle,
  jg,
  jge,
  jmp,

  cmp,
  ucomiss,  // floating point cmp

  movss,
  addss,
  subss,
  mulss,
  divss,

  jb,
  jbe,
  ja,
  jae,
  je,
  jne,

  cvtsi2ss,
  cvttss2si,

  nop,
  label,
  extern_proc,
  ret,
};

struct X86Stmt
{
  eX86Stmt opcode;
  X86Operand* operand1;
  X86Operand* operand2;
};

enum struct eAstNode
{
  None,
  id,
  asm_block,
  bin_expr,
  unr_expr,
  module,
  include,
  block,
  stmt,
  var,
  proc,
  call,
  basic_type,
  lit,
  return_,
  if_,
  while_,
  do_while,
  loop_ctrl,
  enum_decl,
  struct_decl,
  union_decl,
  empty,
  node_list,
  array,
  index,
  pointer,
  assign,
  cast,
  call_arg,
};

struct AstNode_NodeList
{
  List node_list;
};

struct AstNode_CallArg
{
  AstNode* expr;
  Symbol* param;
};

struct AstNode_Cast
{
  AstNode* from_expr;
  AstNode* to_type;
};

struct AstNode_Assign
{
  AstNode* dest_expr;
  AstNode* source_expr;
};

struct AstNode_Pointer
{
  AstNode* pointee;
};

struct AstNode_Array
{
  AstNode* size_expr;
  AstNode* elem_expr;
  int size;
  int ndim;
};

struct AstNode_Index
{
  AstNode* array_expr;
  AstNode* i_expr;
  IrArg* place;   // L.place
  IrArg* offset;  // L.offset
  IrArg* i_place; // Elist.place
  int ndim;
};

struct AstNode_BasicType
{
  eBasicType kind;
};

struct AstNode_Id
{
  char* name;
  AstNode* decl_ast;
  int order_nr;
  Scope* scope;
  Symbol* decl_sym;
};

struct AstNode_BinExpr
{
  AstNode* left_operand;
  AstNode* right_operand;
  eOperator op;
};

struct AstNode_UnrExpr
{
  AstNode* operand;
  eOperator op;
};

struct AstNode_Call
{
  AstNode* expr;
  AstNode* args;
  AstNode* proc;

  Scope* param_scope;
  Symbol* retvar;
};

struct AstNode_Return
{
  AstNode* expr;
  AstNode* proc;
  int nesting_depth;
};

struct AstNode_LoopCtrl
{
  eLoopCtrl kind;
  AstNode* loop;
  int nesting_depth;
};

struct AstNode_If
{
  AstNode* cond_expr;
  AstNode* body;
  AstNode* else_body;
};

struct AstNode_While
{
  AstNode* cond_expr;
  AstNode* body;
  Scope* scope;
};

struct AstNode_Proc
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
};

struct AstNode_Str
{
  char* str_val;
};

struct AstNode_Lit
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
};

struct AstNode_Var
{
  char* name;
  AstNode* type;
  AstNode* init_expr;
  Symbol* decl_sym;
  eModifier modifier;
};

struct AstNode_Module
{
  char* file_path;
  List nodes;
  List procs;
  List vars;
  Scope* scope;
};

struct AstNode_Include
{
  char* file_path;
  HFile* file;
};

struct AstNode_Block
{
  Scope* scope;
  List nodes;
  List vars;
  List stmts;
};

struct AstNode_Struct
{
  char* name;
  AstNode* id;
  List* members;
};

struct AstNode_Asm
{
  char* asm_text;
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
    AstNode_NodeList  args;
    AstNode_CallArg   call_arg;
    AstNode_Cast      cast;
    AstNode_Assign    assign;
    AstNode_Pointer   pointer;
    AstNode_Array     array;
    AstNode_Index     index;
    AstNode_BasicType basic_type;
    AstNode_Id        id;
    AstNode_BinExpr   bin_expr;
    AstNode_UnrExpr   unr_expr;
    AstNode_Call      call;
    AstNode_Return    ret;
    AstNode_LoopCtrl  loop_ctrl;
    AstNode_If        if_;
    AstNode_While     while_;
    AstNode_While     do_while;
    AstNode_Proc      proc;
    AstNode_Str       str;
    AstNode_Lit       lit;
    AstNode_Var       var;
    AstNode_Module    module;
    AstNode_Include   include;
    AstNode_Block     block;
    AstNode_Struct    enum_decl;
    AstNode_Struct    union_decl;
    AstNode_Struct    struct_decl;
    AstNode_Asm       asm_block;
  };
};

struct Parser
{
  MemoryArena* arena;
  Lexer* lexer;
  Token* token;
  SourceLoc* src_loc;

  AstNode* module;
  HFile* file;
  List* includes;
};

enum struct eStorageSpace
{
  None,
  constant,
  local, // vars and temps
  actual_param, // params (actual args and retvar) at the call site
  formal_param, // params to proc 
  static_, // module-level vars
};

enum struct eSymbol
{
  None,
  constant,
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
  bool is_live_on_exit;
  NextUse next_use;

  // natvis
  struct Symbol_Locations
  {
    X86Location* _[(int)eX86Location::Count];
  }
  locations;
};

struct SymbolContext
{
  MemoryArena* gp_arena;
  MemoryArena* sym_arena;
  List scopes;
  Scope* module_scope;
  Scope* active_scope;
  int nesting_depth;
  int data_alignment;
  X86Context* x86_context;
};

struct IrContext
{
  MemoryArena* gp_arena;
  SymbolContext* sym_context;
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
};

struct X86Context
{
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
};

