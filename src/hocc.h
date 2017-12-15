#define ARENA_SIZE (3*MEGABYTE)
#define SYMBOL_ARENA_SIZE (ARENA_SIZE / 8)
#define X86_CODE_ARENA_SIZE (ARENA_SIZE / 4)
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

typedef struct
{
  char* file_path;
  int line_nr;
  char* src_line;
}
SourceLoc;

typedef struct
{
  char name[12];
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
  eToken_string_val,
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
  eOperator_add,
  eOperator_sub,
  eOperator_mul,
  eOperator_div,
  eOperator_mod,
  eOperator_neg,
  eOperator_assign,
  eOperator_deref,
  eOperator_address_of,
  eOperator_selector,
  eOperator_indirect_selector,
  eOperator_pre_decr,
  eOperator_post_decr,
  eOperator_pre_incr,
  eOperator_post_incr,
  eOperator_index,
  eOperator_eq,
  eOperator_not_eq,
  eOperator_less,
  eOperator_less_eq,
  eOperator_greater,
  eOperator_greater_eq,
  eOperator_logic_and,
  eOperator_logic_or,
  eOperator_logic_not,
  eOperator_bit_and,
  eOperator_bit_or,
  eOperator_bit_xor,
  eOperator_bit_not,
  eOperator_bit_shift_left,
  eOperator_bit_shift_right,
  eOperator_cast,
  eOperator_array,
  eOperator_pointer,
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
    case eOperator_assign:
      str = "=";
      break;
    case eOperator_deref:
      str = "*";
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
    case eOperator_pre_decr:
    case eOperator_post_decr:
      str = "--";
      break;
    case eOperator_pre_incr:
    case eOperator_post_incr:
      str = "++";
      break;
    case eOperator_index:
      str = "[]";
      break;
    case eOperator_eq:
      str = "==";
      break;
    case eOperator_not_eq:
      str = "!=";
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
      str = "&";
      break;
    case eOperator_logic_or:
      str = "|";
      break;
    case eOperator_logic_not:
      str = "!";
      break;
    case eOperator_bit_and:
      str = "and";
      break;
    case eOperator_bit_or:
      str = "or";
      break;
    case eOperator_bit_xor:
      str = "xor";
      break;
    case eOperator_bit_not:
      str = "not";
      break;
    case eOperator_cast:
      str = "cast";
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
  eLiteral_string,
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

typedef struct Type Type;
typedef struct AstNode AstNode;
typedef struct Symbol Symbol;
typedef struct Scope Scope;
typedef struct TypePair TypePair;

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

typedef enum
{
  eSymbol_None,
  eSymbol_var = 1 << 0,
  eSymbol_proc = 1 << 1,
  eSymbol_type = 1 << 2,
  eSymbol_Count = 4,
}
eSymbol;

typedef struct Scope
{
  eScope kind;

  int nesting_depth;
  struct Scope* encl_scope;
  AstNode* ast_node;

  List decls[eSymbol_Count];
  List occurs;

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
  eProcModifier_None,
  eProcModifier_extern,
}
eProcModifier;

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

typedef struct TypePair
{
  Type* key;
  Type* value;
}
TypePair;

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
  eAstNode_type,
  eAstNode_basic_type,
  eAstNode_lit,
  eAstNode_return,
  eAstNode_if,
  eAstNode_while,
  eAstNode_loop_ctrl,
  eAstNode_enum_decl,
  eAstNode_struct_decl,
  eAstNode_union_decl,
  eAstNode_empty,
  eAstNode_args,
}
eAstNode;

typedef struct AstNode
{
  eAstNode kind;
  SourceLoc* src_loc;
  Type* ty;
  Type* eval_ty;
  Symbol* occur_sym;
  Symbol* decl_sym;

  union
  {
    struct
    {
      List nodes;
    }
    args;

    struct
    {
      eSymbol sym_kind;
      char* name;
      AstNode* decl_ast;
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
      eType kind;
      union
      {
        char* name;
        AstNode* pointee;
        struct
        {
          AstNode* elem;
          AstNode* size;
        };
      };
    }
    type;

    struct
    {
      eBasicType kind;
    }
    basic_type;

    struct
    {
      AstNode* expr;
      List actual_args;
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
      AstNode* stmt;
    }
    stmt;

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
      eProcModifier modifier;
      AstNode* args;
      AstNode* ret_type;
      AstNode* body;
      Scope* scope;
    }
    proc;

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
    }
    lit;

    struct
    {
      char* name;
      AstNode* type;
      AstNode* init_expr;
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
      Scope* encl_proc_scope;
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
    //record;
    enum_decl, union_decl, struct_decl;

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
  eSymbol kind;

  char* name;
  SourceLoc* src_loc;
  Scope* scope;
  AstNode* ast_node;
  Type* ty;
  Symbol* decl;
  int data_loc;
  void* data;
  bool is_static_alloc;
}
Symbol;

typedef struct
{
  List* scopes;
  Scope* active_scope;
  int nesting_depth;
  MemoryArena* arena;
}
SymbolTable;

void DEBUG_print_ast_nodes(String* str, int indent_level, char* tag, List* nodes);

