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
  struct String* str;

  static MemoryArena* create(int size);
  static MemoryArena* push(MemoryArena** arena, int size);
  static void         pop(MemoryArena** arena);
  static void         begin_temp_memory(MemoryArena** arena);
  static void         end_temp_memory(MemoryArena** arena);
  void  dealloc();
  void* push_struct(int elem_size, int count);
  void  check_bounds(int elem_size, void* ptr);
  Usage get_usage();
  void DEBUG_print_usage(char* tag);
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
  int   line_nr;
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

  Count,
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

  char* get_printstr();
};

struct Lexer
{
  typedef struct
  {
    char quote;
    int len;
    char* begin;
    char* end;
  }
  EscapedStr;

  MemoryArena* arena;
  struct Lexer* last_state;
  Token token;
  char* text;
  char* cursor;
  SourceLoc src_loc;

  static Lexer* create(MemoryArena* arena);
  Token* lookup_keyword(char* lexeme);
  bool   get_next_token();
  Token* get_prev_token();
  void   putback_token();
  char   skip_whitespace(char* whitechars);
  bool   get_asm_text();
  void   set_input(char* text, char* file_path);
  char*  install_lexeme(char* begin_char, char* end_char);
  static bool is_valid_escape_char(char c);
  bool   escaped_string(char* file, int line, EscapedStr* estr);
  char*  install_escaped_str(EscapedStr* estr);
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

  Scope* find(eScope kind);
  Symbol* lookup(char* name);
  Symbol* lookup_decl(char* name);
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
  bool  resolve(Type** resolved_type);
};

struct TypeContext_Set;
struct TypeContext_Eval;
struct TypeContext_Resolve;
struct TypeContext_Check;

struct TypeContext
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

  static TypeContext* create(MemoryArena* arena);
  void      init(MemoryArena* arena);
  Type*     type_subst(Type* type);
  TypePair* create_type_pair(Type* key, Type* value);
  TypePair* find_pair(Type* type);
  bool      process(AstNode* module);
};

struct TypeContext_Set : TypeContext
{
  int f;

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

struct TypeContext_Eval : TypeContext
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

struct TypeContext_Resolve : TypeContext
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

struct TypeContext_Check : TypeContext
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

  static Label* create(MemoryArena* arena);
  static Label* create_by_name(MemoryArena* arena, char* name);
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

  Type* make_product_type(TypeContext* typesys);
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

  bool is_extern();
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

  void merge(AstNode_Module* merged_module);
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

  bool resolve_types(MemoryArena* arena);
  bool is_valid_expr_operand();
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

  static char* get_operator_printstr(eOperator op);
  static Parser* create(MemoryArena* arena);
  Parser*    create_included();
  void       set_input(char* text, HFile* file);
  bool       get_next_token();
  void       putback_token();
  SourceLoc* clone_source_loc();
  AstNode*   create_ast_node(eAstNode kind, SourceLoc* src_loc);
  bool       consume_semicolon();
  AstNode*   find_include(HFile* file);

  bool parse_actual_args(AstNode* args);
  bool parse_rest_of_actual_args(AstNode* args);
  bool parse_call(AstNode* left_node, AstNode** node);
  bool parse_index_recursive(AstNode* left_node, AstNode** node, int* ndim);
  bool parse_index(AstNode* left_node, AstNode** node);
  bool parse_rest_of_unr_expr(AstNode* left_node, AstNode** node);
  bool parse_factor(AstNode** node);
  bool parse_rest_of_factor(AstNode* left_node, AstNode** node);
  bool parse_term(AstNode** node);
  bool parse_rest_of_term(AstNode* left_node, AstNode** node);
  bool parse_assignment(AstNode** node);
  bool parse_rest_of_assignment(AstNode* left_node, AstNode** node);
  bool parse_id(AstNode** node);
  bool parse_basic_type(AstNode** node);
  bool parse_rest_of_cast(AstNode* left_node, AstNode** node);
  bool parse_rest_of_deref(AstNode** node);
  bool parse_deref(AstNode** node);
  bool parse_rest_of_array(AstNode** node);
  bool parse_array(AstNode** node);
  bool parse_cast(AstNode** node);
  bool parse_pointer(AstNode* left_node, AstNode** node);
  bool parse_rest_of_selector(AstNode* left_node, AstNode** node);
  bool parse_lit(AstNode** node);
  bool parse_selector(AstNode** node);
  bool parse_formal_arg(AstNode** node);
  bool parse_unr_expr(AstNode** node);
  bool parse_expr(AstNode** node);
  bool parse_modifier(eModifier* modifier);
  bool parse_rest_of_formal_args(AstNode* args);
  bool parse_formal_args(AstNode* args);
  bool parse_empty(AstNode** node);
  bool parse_block(AstNode** node);
  bool parse_else(AstNode** node);
  bool parse_if(AstNode** node);
  bool parse_do_while(AstNode** node);
  bool parse_while(AstNode** node);
  bool parse_return(AstNode** node);
  bool parse_continue(AstNode** node);
  bool parse_break(AstNode** node);
  bool parse_block_var(char* name, eModifier modifier, AstNode* var_type, AstNode** node);
  bool parse_block_stmt(AstNode** node);
  bool parse_block_stmts(AstNode* block);
  bool parse_proc_body(AstNode* proc);
  bool parse_module_proc(char* name, eModifier modifier, AstNode* ret_type, AstNode** node);
  bool parse_module_var(char* name, eModifier modifier, AstNode* var_type, AstNode** node);
  bool parse_module_include(AstNode** node);
  bool parse_module_stmt(AstNode** node);
  bool parse_module_stmts(AstNode* module);
  bool parse_module_body(AstNode* module);
  bool parse_module();
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
    X86Location* _[(int)eX86Location::Count];
  }
  locations;

  void init_locations();
  bool is_in_location(X86Location* loc);
};

struct SymbolContext
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
  void init(MemoryArena* gp_arena, MemoryArena* sym_arena, TypeContext* type_context);

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
  Type* basic_type_bool;
  Type* basic_type_int;
  Type* basic_type_char;
  Type* basic_type_float;
  Type* basic_type_void;
  Type* basic_type_str;

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

  void init(MemoryArena* gp_arena, MemoryArena* stmt_arena, TypeContext* type_context, SymbolContext* sym_context);
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
  void delete_object_from_location(Symbol* object, X86Location* loc);
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
            TypeContext* type_context, IrContext* ir_context, SymbolContext* sym_context);

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
  void gen(AstNode* module);
};

