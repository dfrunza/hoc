#include "lib.c"

typedef struct Symbol Symbol;
typedef struct AstNode AstNode;
typedef struct Type Type;

#include "lex.h"

///////////////////////////////////////////////////////////////////////////////
//    Type system
//

typedef enum
{
  TypeKind__Null,
  TypeKind_TypeVar,
  TypeKind_Unary,
  TypeKind_Basic,
  TypeKind_Type,
  TypeKind_Proc,
  TypeKind_Product,
  TypeKind_Pointer,
  TypeKind_Array,
} TypeKind;

typedef enum
{
  UnaryCtorKind__Null,
  UnaryCtorKind_Pointer,
  UnaryCtorKind_Array,
} UnaryCtorKind;

typedef struct
{
  int id;
} TypeVar;

typedef enum
{
  BasicTypeKind__Null,
  BasicTypeKind_Void,
  BasicTypeKind_Int,
  BasicTypeKind_Float,
  BasicTypeKind_Char,
  BasicTypeKind_Bool,
} BasicTypeKind;

typedef struct
{
  BasicTypeKind kind;
} BasicType;

typedef struct
{
  Type* pointee;
} PointerType;

typedef struct
{
  Type* args;
  Type* ret;
} ProcType;

typedef struct
{
  Type* left;
  Type* right;
} ProductType;

typedef struct Type
{
  TypeKind kind;
  Type* equiv_set;

  union
  {
    BasicType basic;
    PointerType ptr;
    ProcType proc;
    ProductType product;
    TypeVar typevar;
  };
} Type;

typedef struct
{
  Type* key;
  Type* value;
} TypeTuple;

///////////////////////////////////////////////////////////////////////////////
//  AST
//

typedef enum
{
  AstOpKind__Null,

  AstOpKind_Add,
  AstOpKind_Sub,
  AstOpKind_Div,
  AstOpKind_Mul,
  AstOpKind_Mod,
  AstOpKind_Neg,

  AstOpKind_Call,
  AstOpKind_Assign,
  
  AstOpKind_LogicEquals,
  AstOpKind_LogicNotEquals,
  AstOpKind_LogicLess,
  AstOpKind_LogicLessEquals,
  AstOpKind_LogicGreater,
  AstOpKind_LogicGreaterEquals,
  AstOpKind_LogicAnd,
  AstOpKind_LogicOr,
  AstOpKind_LogicNot,
}
AstOpKind;

typedef enum
{
  AstNodeKind__Null,
  AstNodeKind_BinExpr,
  AstNodeKind_UnrExpr,
  AstNodeKind_IntNum,
  AstNodeKind_VarDecl,
  AstNodeKind_VarOccur,
  AstNodeKind_Call,
  AstNodeKind_Block,
  AstNodeKind_Proc,
  AstNodeKind_Id,
  AstNodeKind_WhileStmt,
  AstNodeKind_IfStmt,
  AstNodeKind_ReturnStmt,
  AstNodeKind_BreakStmt,
  AstNodeKind_PrintStmt,
  AstNodeKind_IncludeStmt,
  AstNodeKind_EmptyStmt,
  AstNodeKind_Module,
  AstNodeKind_Noop,
}
AstNodeKind;

typedef struct
{
  int loc;
  int size;
}
DataArea;

typedef struct AccessLink
{
  int actv_rec_offset;
  DataArea data;
}
AccessLink;

typedef struct
{
  char* name;
  Symbol* symbol;
  DataArea data;
  AstNode* init_expr;
  Type* var_type;
}
AstVarDecl;

typedef struct
{
  char* name;
  Symbol* symbol;
  AstNode* var_decl;
  int decl_block_offset;
  AccessLink* link;
  DataArea* data;
}
AstVarOccur;

typedef struct
{
  AstOpKind op;
  AstNode* left_operand;
  AstNode* right_operand;

  char* label_end; // for boolean expressions
}
AstBinExpr;

typedef struct
{
  AstOpKind op;
  AstNode* operand;
}
AstUnrExpr;

typedef struct
{
  int32 value;
}
AstIntNum;

typedef struct AstBlock
{
  AstNode* owner;
  int block_id;
  int nesting_depth;
  struct AstBlock* enclosing_block;
  List decl_vars; // <AstNode>
  List local_occurs; // <AstNode>
  List nonlocal_occurs; // <AstNode>
  List stmt_list; // <AstNode>
  List access_links; // <AccessLink>

  int links_size;
  int locals_size;
}
AstBlock;

typedef struct
{
  char* name;
  List formal_args; // <AstNode>
  AstNode* body; // <AstBlock>

  Type* ret_type;
  AstVarDecl ret_var;

  char* label;
  char* label_end;
  int ret_size;
  int args_size;
  int locals_size;
}
AstProc;

typedef struct
{
  AstNode* ret_expr;
  AstBinExpr* assgn_expr;
  int depth;
  AstProc* proc;
}
AstReturnStmt;

typedef struct
{
  Symbol* symbol;
  char* name;
  List actual_args; // <AstNode>
  AstNode* proc;
}
AstCall;

typedef struct
{
  List proc_list; // <AstProc>
  AstNode* body;

  AstNode* main_proc;
  AstNode* main_call;
}
AstModule;

typedef struct
{
  AstNode* cond_expr;
  AstNode* body;
  AstNode* else_body;

  char* label_else;
  char* label_end;
}
AstIfStmt;

typedef struct
{
  AstNode* cond_expr;
  AstNode* body;

  char* label_eval;
  char* label_break;
}
AstWhileStmt;

typedef struct
{
  AstWhileStmt* while_stmt;
  int depth;
}
AstBreakStmt;

typedef struct
{
  AstNode* expr;
  bool32 new_line;
}
AstPrintStmt;

typedef struct
{
  char* file_path;
}
AstIncludeStmt;

typedef AstNode AstEmptyStmt;

typedef struct AstNode
{
  AstNodeKind kind;
  Type* type;

  union
  {
    AstModule module;
    AstVarDecl var_decl;
    AstVarOccur var_occur;
    AstBinExpr bin_expr;
    AstUnrExpr unr_expr;
    AstIntNum int_val;
    AstProc proc;
    AstReturnStmt ret_stmt;
    AstCall call;
    AstIfStmt if_stmt;
    AstWhileStmt while_stmt;
    AstPrintStmt print_stmt;
    AstIncludeStmt inc_stmt;
    AstBreakStmt break_stmt;
    AstBlock block;
  };
}
AstNode;

typedef enum
{
  EntityKind_Ast,
  EntityKind_RuntimeObj,
  EntityKind_IntermRepr,
} EntityKind;

typedef struct
{
  int i;
}
RuntimeObj;

struct Entity
{
  EntityKind kind;
  void* e;

  union
  {
    AstNode n;
    Type t;
    RuntimeObj r;
  };
};

////////////////////////////////////////////////////////////////////////////////
//  Symbol table
//

typedef struct
{
  Symbol* symbol;
  int scope_id;
  int last_scope_id;
  int nesting_depth;
  int active_scopes[32];
  MemoryArena* arena;
}
SymbolTable;

typedef enum
{
  SymbolKind__Null,
  SymbolKind_Keyword,
  SymbolKind_Proc,
  SymbolKind_Type,
  SymbolKind_Var,
}
SymbolKind;

typedef struct Symbol
{
  SymbolKind kind;
  Symbol* next_symbol;

  char* name;
  int block_id;
  int nesting_depth;

  union {
    TokenKind keyword;
    AstNode* node;
    Type* type;
  };
}
Symbol;

typedef struct
{
  String text;
  int text_len;
  List instr_list; // <Instruction>
}
VmProgram;

////////////////////////////////////////////////////////////////////////////////
//  Globals
//

static List g_type_tuples;
static int g_typevar_id = 1;
static int g_last_label_id;

static Type* g_basic_type_bool;
static Type* g_basic_type_int;
static Type* g_basic_type_char;
static Type* g_basic_type_float;
static Type* g_basic_type_void;

//
//  End of structs
////////////////////////////////////////////////////////////////////////////////

void
syntax_error(TokenStream* input, char* message, ...)
{
  va_list args;

  fprintf(stderr, "%s(%d) : ", input->file_path, input->line_nr);

  va_start(args, message);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
}

////////////////////////////////////////////////////////////////////////////////
//  Types
//

Type*
new_basic_type(MemoryArena* arena, BasicTypeKind kind)
{
  Type* type = mem_push_struct(arena, Type, 1);
  type->kind = TypeKind_Basic;
  type->basic.kind = kind;
  return type;
}

Type*
new_proc_type(MemoryArena* arena, Type* args, Type* ret)
{
  Type* type = mem_push_struct(arena, Type, 1);
  type->kind = TypeKind_Proc;
  type->proc.args = args;
  type->proc.ret = ret;
  return type;
}

Type*
new_typevar(MemoryArena* arena)
{
  Type* type = mem_push_struct(arena, Type, 1);
  type->kind = TypeKind_TypeVar;
  type->typevar.id = g_typevar_id++;
  return type;
}

Type*
new_product_type(MemoryArena* arena, Type* left, Type* right)
{
  Type* type = mem_push_struct(arena, Type, 1);
  type->kind = TypeKind_Product;
  type->product.left = left;
  type->product.right = right;
  return type;
}

Type*
copy_type(MemoryArena* arena, Type* type)
{
  Type* copy = mem_push_struct(arena, Type, 1);
  *copy = *type;
  return copy;
}

////////////////////////////////////////////////////////////////////////////////
//  Symbol table
//

Symbol*
lookup_symbol(SymbolTable* symbol_table, char* name, SymbolKind kind)
{
  Symbol* result = 0;

  Symbol* symbol = symbol_table->symbol;
  while(symbol)
  {
    if(symbol->kind == kind && cstr_match(symbol->name, name))
    {
      result = symbol;
      break;
    }
    symbol = symbol->next_symbol;
  }
  return result;
}

Symbol*
add_symbol(MemoryArena* arena, SymbolTable* symbol_table, char* name, SymbolKind kind)
{
  Symbol* symbol = mem_push_struct(arena, Symbol, 1);
  symbol->kind = kind;
  symbol->name = name;
  symbol->block_id = symbol_table->scope_id;
  symbol->nesting_depth = symbol_table->nesting_depth;
  symbol->next_symbol = symbol_table->symbol;
  symbol_table->symbol = symbol;
  return symbol;
}

#include "lex.c"

bool32
is_logical_operator(AstOpKind op)
{
  return op >= AstOpKind_LogicEquals && op <= AstOpKind_LogicNot;
}

Symbol*
add_builtin_type(MemoryArena* arena, SymbolTable* symbol_table, char* name, Type* type)
{
  assert(type->kind == TypeKind_Basic);
  Symbol* symbol = add_symbol(arena, symbol_table, name, SymbolKind_Type);
  symbol->type = type;
  return symbol;
}

Symbol*
add_keyword(MemoryArena* arena, SymbolTable* symbol_table, char* name, TokenKind token_kind)
{
  Symbol* symbol = add_symbol(arena, symbol_table, name, SymbolKind_Keyword);
  symbol->keyword = token_kind;
  return symbol;
}

void
add_keyword_list(MemoryArena* arena, SymbolTable* symbol_table)
{
  add_builtin_type(arena, symbol_table, "bool", g_basic_type_bool);
  add_builtin_type(arena, symbol_table, "int", g_basic_type_int);
  add_builtin_type(arena, symbol_table, "char", g_basic_type_char);
  add_builtin_type(arena, symbol_table, "float", g_basic_type_float);
  add_builtin_type(arena, symbol_table, "void", g_basic_type_void);

  add_keyword(arena, symbol_table, "var", TokenKind_Var);
  add_keyword(arena, symbol_table, "proc", TokenKind_Proc);
  add_keyword(arena, symbol_table, "type", TokenKind_Type);
  add_keyword(arena, symbol_table, "struct", TokenKind_Type);
  add_keyword(arena, symbol_table, "array", TokenKind_Array);
  add_keyword(arena, symbol_table, "of", TokenKind_Of);
  add_keyword(arena, symbol_table, "if", TokenKind_If);
  add_keyword(arena, symbol_table, "else", TokenKind_Else);
  add_keyword(arena, symbol_table, "while", TokenKind_While);
  add_keyword(arena, symbol_table, "return", TokenKind_Return);
  add_keyword(arena, symbol_table, "break", TokenKind_Break);
  add_keyword(arena, symbol_table, "include", TokenKind_Include);
  add_keyword(arena, symbol_table, "true", TokenKind_True);
  add_keyword(arena, symbol_table, "false", TokenKind_False);
  add_keyword(arena, symbol_table, "print", TokenKind_Print);
}

void
init_global_basic_types(MemoryArena* arena)
{
  g_basic_type_bool = new_basic_type(arena, BasicTypeKind_Bool);
  g_basic_type_int = new_basic_type(arena, BasicTypeKind_Int);
  g_basic_type_char = new_basic_type(arena, BasicTypeKind_Char);
  g_basic_type_float = new_basic_type(arena, BasicTypeKind_Float);
  g_basic_type_void = new_basic_type(arena, BasicTypeKind_Void);
}

bool32
scope_begin(SymbolTable* symbol_table)
{
  int scope_id = ++symbol_table->last_scope_id;
  symbol_table->scope_id = scope_id;

  int nesting_depth = ++symbol_table->nesting_depth;
  if(nesting_depth < sizeof_array(symbol_table->active_scopes))
  {
    symbol_table->active_scopes[nesting_depth] = scope_id;
  } else {
    error("Reached the maximum scope nesting depth");
    return false;
  }

  return true;
}

void
scope_end(SymbolTable* symbol_table)
{
  int nesting_depth = --symbol_table->nesting_depth;
  int scope_id = symbol_table->active_scopes[nesting_depth];
  assert(scope_id >= 0);
  symbol_table->scope_id = scope_id;

  Symbol* symbol = symbol_table->symbol;
  while(symbol && symbol->block_id > symbol_table->scope_id)
    symbol = symbol->next_symbol;
  symbol_table->symbol = symbol;
}

void
make_unique_label(String* label)
{
  sprintf(label->head, "L%d", g_last_label_id++);
  int len = cstr_len(label->head);
  label->end = label->head + len;
  MemoryArena* arena = label->arena;
  arena->free = label->end + 1;
}

/* Parse */

bool32 parse_expression(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstNode**);
bool32 parse_statement_list(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*);
bool32 parse_actual_argument_list(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstCall*);
bool32 parse_term(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstNode**);
bool32 parse_statement(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstNode**);
bool32 parse_if_stmt(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstNode**);
bool32 parse_block(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstNode*, AstNode**);
bool32 parse(MemoryArena*, TokenStream*, SymbolTable*, AstNode**);
void build_node(MemoryArena*, AstNode*);
void build_block_stmts(MemoryArena*, List*);
Type* make_product_type(MemoryArena*, Type*, ListItem*);
bool32 typecheck_block(MemoryArena*, List*, AstNode*);

inline AstNode*
ast_new_module(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  list_init(&node->module.proc_list);
  node->kind = AstNodeKind_Module;
  node->type = g_basic_type_void;
  return node;
}

inline AstNode*
ast_new_block(MemoryArena* arena, SymbolTable* symbol_table)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Block;
  node->type = g_basic_type_void;
  AstBlock* block = &node->block;
  list_init(&block->decl_vars);
  list_init(&block->local_occurs);
  list_init(&block->nonlocal_occurs);
  list_init(&block->stmt_list);
  list_init(&block->access_links);
  block->block_id = symbol_table->scope_id;
  block->nesting_depth = symbol_table->nesting_depth;
  return node;
}

inline AstNode*
ast_new_call(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Call;
  node->type = new_typevar(arena);
  list_init(&node->call.actual_args);
  return node;
}

inline AstNode*
ast_new_proc(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_Proc;
  node->type = new_typevar(arena);
  list_init(&node->proc.formal_args);
  return node;
}

inline AstNode*
ast_new_bin_expr(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_BinExpr;
  node->type = new_typevar(arena);
  return node;
}

inline AstNode*
ast_new_unr_expr(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_UnrExpr;
  node->type = new_typevar(arena);
  return node;
}

inline AstNode*
ast_new_int_val(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_IntNum;
  node->type = new_basic_type(arena, BasicTypeKind_Int);
  return node;
}

inline AstNode*
ast_new_var_decl(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_VarDecl;
  node->type = new_typevar(arena);
  return node;
}

inline AstNode*
ast_new_var_occur(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_VarOccur;
  node->type = new_typevar(arena);
  return node;
}

inline AstNode*
ast_new_while_stmt(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_WhileStmt;
  node->type = g_basic_type_void;
  return node;
}

inline AstNode*
ast_new_if_stmt(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_IfStmt;
  node->type = g_basic_type_void;
  return node;
}

inline AstNode*
ast_new_return_stmt(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_ReturnStmt;
  node->type = new_typevar(arena);
  return node;
}

inline AstNode*
ast_new_break_stmt(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_BreakStmt;
  node->type = g_basic_type_void;
  return node;
}

inline AstNode*
ast_new_print_stmt(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_PrintStmt;
  node->type = g_basic_type_void;
  return node;
}

inline AstNode*
ast_new_include_stmt(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_IncludeStmt;
  node->type = g_basic_type_void;
  return node;
}

inline AstNode*
ast_new_empty_stmt(MemoryArena* arena)
{
  AstNode* node = mem_push_struct(arena, AstNode, 1);
  node->kind = AstNodeKind_EmptyStmt;
  node->type = g_basic_type_void;
  return node;
}

bool32
parse_factor(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
             AstBlock* enclosing_block, AstNode** node)
{
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenParens)
  {
    consume_token(arena, input, symbol_table);
    success = parse_expression(arena, input, symbol_table, enclosing_block, node);
    if(success)
    {
      if(input->token.kind == TokenKind_CloseParens)
        consume_token(arena, input, symbol_table);
      else {
        syntax_error(input, "Missing ')'");
        success = false;
      }
    }
  }
  else if(input->token.kind == TokenKind_UnaryMinus)
  {
    consume_token(arena, input, symbol_table);

    AstNode* operand = 0;
    success = parse_term(arena, input, symbol_table, enclosing_block, &operand);
    if(success)
    {
      if(operand)
      {
        *node = ast_new_unr_expr(arena);
        AstUnrExpr* expr = &(*node)->unr_expr;
        expr->op = AstOpKind_Neg;
        expr->operand = operand;
      } else {
        syntax_error(input, "Expression expected after '-'");
        success = false;
      }
    }
  }
  else if(input->token.kind == TokenKind_Bang)
  {
    consume_token(arena, input, symbol_table);

    AstNode* operand = 0;
    success = parse_term(arena, input, symbol_table, enclosing_block, &operand);
    if(success)
    {
      if(operand)
      {
        *node = ast_new_unr_expr(arena);
        AstUnrExpr* expr = &(*node)->unr_expr;
        expr->op = AstOpKind_LogicNot;
        expr->operand = operand;
      } else {
        syntax_error(input, "Expression expected after '!'");
        success = false;
      }
    }
  }
  else if(input->token.kind == TokenKind_IntNum)
  {
    *node = ast_new_int_val(arena);
    AstIntNum* int_val = &(*node)->int_val;
    int_val->value = *(int32*)input->token.int_val;

    consume_token(arena, input, symbol_table);
  }
  else if(input->token.kind == TokenKind_Id)
  {
    char* id_name = input->token.lexeme;
    consume_token(arena, input, symbol_table);

    if(input->token.kind == TokenKind_OpenParens)
    {
      *node = ast_new_call(arena);
      AstCall* call = &(*node)->call;
      Symbol* symbol = lookup_symbol(symbol_table, id_name, SymbolKind_Proc);
      if(symbol)
      {
        AstNode* proc_node = symbol->node;
        call->symbol = symbol;
        call->name = symbol->name;
        call->proc = proc_node;

        consume_token(arena, input, symbol_table);
        success = parse_actual_argument_list(arena, input, symbol_table, enclosing_block, call);
        if(success)
        {
          if(input->token.kind == TokenKind_CloseParens)
          {
            consume_token(arena, input, symbol_table);

            List* formal_args = &proc_node->proc.formal_args;
            List* actual_args = &call->actual_args;
            if(formal_args->count != actual_args->count)
            {
              syntax_error(input, "Expected %d arguments in the call: %s(..)", formal_args->count, proc_node->proc.name);
              success = false;
            }
          }
          else {
            syntax_error(input, "Missing ')' in procedure call");
            success = false;
          }
        }
        else {
          syntax_error(input, "Missing '(' in procedure call");
          success = false;
        }
      }
      else {
        syntax_error(input, "Unknown procedure: %s", input->token.lexeme);
        success = false;
      }
    }
    else
    {
      *node = ast_new_var_occur(arena);
      AstVarOccur* var_occur = &(*node)->var_occur;
      Symbol* symbol = lookup_symbol(symbol_table, id_name, SymbolKind_Var);
      if(symbol)
      {
        var_occur->symbol = symbol;
        var_occur->name = symbol->name;

        AstVarDecl* var_decl = &symbol->node->var_decl;
        var_occur->data = &var_decl->data;
        var_occur->decl_block_offset = (symbol_table->nesting_depth - symbol->nesting_depth);
        var_occur->var_decl = symbol->node;

        if(var_occur->decl_block_offset > 0)
        {
          list_append(arena, &enclosing_block->nonlocal_occurs, *node);
        }
        else if(var_occur->decl_block_offset == 0)
        {
          list_append(arena, &enclosing_block->local_occurs, *node);
        }
        else
          assert(false);
      }
      else {
        syntax_error(input, "Unknown identifier: %s", id_name);
        success = false;
      }
    }
  }
  else if(input->token.kind == TokenKind_True || input->token.kind == TokenKind_False)
  {
    *node = ast_new_int_val(arena);
    AstIntNum* int_val = &(*node)->int_val;
    int_val->value = (input->token.kind == TokenKind_True ? 1 : 0);

    consume_token(arena, input, symbol_table);
  }

  return success;
}

bool32
parse_rest_of_factors(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                      AstBlock* enclosing_block, AstNode* left_node, AstNode** node)
{
  bool32 success = true;

  if(input->token.kind == TokenKind_Star ||
     input->token.kind == TokenKind_FwdSlash ||
     input->token.kind == TokenKind_Percent ||
     input->token.kind == TokenKind_EqualsEquals ||
     input->token.kind == TokenKind_BangEquals ||
     input->token.kind == TokenKind_AmprsndAmprsnd ||
     input->token.kind == TokenKind_PipePipe ||
     input->token.kind == TokenKind_AngleLeft ||
     input->token.kind == TokenKind_AngleLeftEquals ||
     input->token.kind == TokenKind_AngleRight ||
     input->token.kind == TokenKind_AngleRightEquals)
  {
    *node = ast_new_bin_expr(arena);
    AstBinExpr* expr = &(*node)->bin_expr;

    if(input->token.kind == TokenKind_Star)
    {
      expr->op = AstOpKind_Mul;
    }
    else if(input->token.kind == TokenKind_FwdSlash)
    {
      expr->op = AstOpKind_Div;
    }
    else if(input->token.kind == TokenKind_Percent)
    {
      expr->op = AstOpKind_Mod;
    }
    else if(input->token.kind == TokenKind_EqualsEquals)
    {
      expr->op = AstOpKind_LogicEquals;
    }
    else if(input->token.kind == TokenKind_BangEquals)
    {
      expr->op = AstOpKind_LogicNotEquals;
    }
    else if(input->token.kind == TokenKind_AngleLeft)
    {
      expr->op = AstOpKind_LogicLess;
    }
    else if(input->token.kind == TokenKind_AngleLeftEquals)
    {
      expr->op = AstOpKind_LogicLessEquals;
    }
    else if(input->token.kind == TokenKind_AngleRight)
    {
      expr->op = AstOpKind_LogicGreater;
    }
    else if(input->token.kind == TokenKind_AngleRightEquals)
    {
      expr->op = AstOpKind_LogicGreaterEquals;
    }
    else if(input->token.kind == TokenKind_AmprsndAmprsnd)
    {
      expr->op = AstOpKind_LogicAnd;
    }
    else if(input->token.kind == TokenKind_PipePipe)
    {
      expr->op = AstOpKind_LogicOr;
    }
    else
      assert(false);

    consume_token(arena, input, symbol_table);
    AstNode* factor_node = 0;
    success = parse_factor(arena, input, symbol_table, enclosing_block, &factor_node);

    if(success)
    {
      if(factor_node)
      {
        expr->right_operand = factor_node;
        expr->left_operand = left_node;
        success = parse_rest_of_factors(arena, input, symbol_table, enclosing_block, *node, node);
      }
      else {
        syntax_error(input, "Factor expected");
        success = false;
      }
    }
  }
  else
    *node = left_node;

  return success;
}

bool32
parse_term(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
           AstBlock* enclosing_block, AstNode** node)
{
  AstNode* factor_node = 0;
  AstNode* expr_node = 0;

  bool32 success = parse_factor(arena, input, symbol_table, enclosing_block, &factor_node);
  if(success && factor_node)
    success = parse_rest_of_factors(arena, input, symbol_table,
                                    enclosing_block, factor_node, &expr_node);

  *node = expr_node;
  return success;
}

bool32
parse_rest_of_terms(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                    AstBlock* enclosing_block, AstNode* left_node, AstNode** node)
{
  bool32 success = true;

  if(input->token.kind == TokenKind_Plus || input->token.kind == TokenKind_Minus)
  {
    *node = ast_new_bin_expr(arena);
    AstBinExpr* expr = &(*node)->bin_expr;

    if(input->token.kind == TokenKind_Plus)
      expr->op = AstOpKind_Add;
    else if(input->token.kind == TokenKind_Minus)
      expr->op = AstOpKind_Sub;
    else
      assert(false);

    consume_token(arena, input, symbol_table);
    AstNode* term_node = 0;
    success = parse_term(arena, input, symbol_table, enclosing_block, &term_node);

    if(success && term_node)
    {
      expr->right_operand = term_node;
      expr->left_operand = left_node;
      success = parse_rest_of_terms(arena, input, symbol_table, enclosing_block, *node, node);
    } else {
      syntax_error(input, "Expression term expected");
      success = false;
    }
  }
  else
    *node = left_node;

  return success;
}

bool32
parse_assignment_term(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                      AstBlock* enclosing_block, AstNode** node)
{
  AstNode* term_node = 0;
  AstNode* expr_node = 0;

  bool32 success = parse_term(arena, input, symbol_table, enclosing_block, &term_node);
  if(success && term_node)
    success = parse_rest_of_terms(arena, input, symbol_table, enclosing_block, term_node, &expr_node);

  *node = expr_node;
  return success;
}

bool32
parse_rest_of_assignment_terms(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                               AstBlock* enclosing_block, AstNode* left_node, AstNode** node)
{
  bool32 success = true;

  if(input->token.kind == TokenKind_Equals)
  {
    consume_token(arena, input, symbol_table);
    AstNode* right_side = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &right_side);
    if(success)
    {
      if(right_side)
      {
        if(left_node->kind == AstNodeKind_VarOccur)
        {
          *node = ast_new_bin_expr(arena);
          AstBinExpr* expr = &(*node)->bin_expr;
          expr->op = AstOpKind_Assign;
          expr->left_operand = left_node;
          expr->right_operand = right_side;
        } else {
          syntax_error(input, "Variable required on the left side of assignment");
          success = false;
        }
      } else {
        syntax_error(input, "Missing right side of assignment");
        success = false;
      }
    }
  } else
    *node = left_node;

  return success;
}

bool32
parse_expression(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                 AstBlock* enclosing_block, AstNode** node)
{
  AstNode* assgn_node = 0;
  AstNode* expr_node = 0;

  bool32 success = parse_assignment_term(arena, input, symbol_table, enclosing_block, &assgn_node);
  if(success && assgn_node)
  {
    success = parse_rest_of_assignment_terms(arena, input, symbol_table,
                                             enclosing_block, assgn_node, &expr_node);
  }

  *node = expr_node;
  return success;
}

bool32
parse_var_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                    AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Var)
  {
    *node = ast_new_var_decl(arena);
    AstVarDecl* var_decl = &(*node)->var_decl;
    var_decl->data.size = 1;

    consume_token(arena, input, symbol_table);

    if(input->token.kind == TokenKind_Id)
    {
      Symbol* type_symbol = lookup_symbol(symbol_table, input->token.lexeme, SymbolKind_Type);
      if(type_symbol)
      {
        var_decl->var_type = type_symbol->type;

        consume_token(arena, input, symbol_table);
        if(input->token.kind == TokenKind_Id)
        {
          Symbol* var_symbol = add_symbol(arena, symbol_table, input->token.lexeme, SymbolKind_Var);
          var_symbol->node = *node;
          var_decl->symbol = var_symbol;
          var_decl->name = var_symbol->name;

          consume_token(arena, input, symbol_table);
          if(input->token.kind == TokenKind_Equals)
          {
            AstNode* var_node = ast_new_var_occur(arena);
            AstVarOccur* var_occur = &var_node->var_occur;
            var_occur->symbol = var_decl->symbol;
            var_occur->name = var_decl->name;
            var_occur->data = &var_decl->data;
            var_occur->decl_block_offset = 0;
            var_occur->var_decl = *node;
            var_node->type = (*node)->type;

            AstNode* init_expr = 0;
            success = parse_rest_of_assignment_terms(arena, input, symbol_table,
                                                     enclosing_block, var_node, &init_expr);
            if(success)
            {
              var_decl->init_expr = init_expr;
            }
          }
        }
        else {
          syntax_error(input, "Identifier expected");
          success = false;
        }
      }
      else {
        syntax_error(input, "Unknown type: %s", input->token.lexeme);
        success = false;
      }
    }
    else {
      syntax_error(input, "Identifier expected");
      success = false;
    }
  }

  return success;
}

bool32
parse_formal_argument_list(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                           AstBlock* enclosing_block, AstProc* proc)
{
  bool32 success = true;

  AstNode* arg_node = 0;
  success = parse_var_statement(arena, input, symbol_table, enclosing_block, &arg_node);
  if(success && arg_node)
  {
    assert(arg_node->kind == AstNodeKind_VarDecl);
    if(!arg_node->var_decl.init_expr)
    {
      list_append(arena, &proc->formal_args, arg_node);

      if(input->token.kind == TokenKind_Comma)
      {
        consume_token(arena, input, symbol_table);
        success = parse_formal_argument_list(arena, input, symbol_table, enclosing_block, proc);
      }
    }
    else {
      syntax_error(input, "Variable initializer not allowed here");
      success = false;
    }
  }

  return success;
}

bool32
parse_actual_argument_list(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                           AstBlock* enclosing_block, AstCall* call)
{
  bool32 success = true;

  AstNode* arg_node = 0;
  success = parse_expression(arena, input, symbol_table, enclosing_block, &arg_node);
  if(success && arg_node)
  {
    list_append(arena, &call->actual_args, arg_node);

    if(input->token.kind == TokenKind_Comma)
    {
      consume_token(arena, input, symbol_table);
      success = parse_actual_argument_list(arena, input, symbol_table, enclosing_block, call);
    }
  }

  return success;
}

bool32
parse_while_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                 AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_While)
  {
    consume_token(arena, input, symbol_table);

    if(input->token.kind == TokenKind_OpenParens)
    {
      consume_token(arena, input, symbol_table);

      AstNode* expr_node = 0;
      if(success = parse_expression(arena, input, symbol_table, enclosing_block, &expr_node))
      {
        if(input->token.kind == TokenKind_CloseParens)
        {
          consume_token(arena, input, symbol_table);

          if(expr_node)
          {
            *node = ast_new_while_stmt(arena);
            AstWhileStmt* while_stmt = &(*node)->while_stmt;
            while_stmt->cond_expr = expr_node;

            AstNode* body_node = 0;
            success = parse_block(arena, input, symbol_table, enclosing_block, *node, &body_node);
            if(success)
            {
              if(body_node)
              {
                while_stmt->body = body_node;
              }
              else
              {
                success = parse_statement(arena, input, symbol_table, enclosing_block, &body_node);
                if(success)
                {
                  if(body_node)
                  {
                    while_stmt->body = body_node;
                  }
                  else {
                    syntax_error(input, "Statement(s) required");
                    success = false;
                  }
                }
              }
            }
          }
          else {
            syntax_error(input, "Expression required");
            success = false;
          }
        }
        else {
          syntax_error(input, "Missing ')'");
          success = false;
        }
      }
    }
    else {
      syntax_error(input, "Missing '('");
      success = false;
    }
  }

  return success;
}

bool32
parse_block(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
            AstBlock* enclosing_block, AstNode* owner, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_OpenBrace)
  {
    consume_token(arena, input, symbol_table);

    success = scope_begin(symbol_table);
    if(success)
    {
      *node = ast_new_block(arena, symbol_table);
      AstBlock* block = &(*node)->block;
      block->owner = owner;
      block->enclosing_block = enclosing_block;

      success = parse_statement_list(arena, input, symbol_table, block);
      if(success)
      {
        if(input->token.kind == TokenKind_CloseBrace)
        {
          consume_token(arena, input, symbol_table);
          scope_end(symbol_table);
        } else {
          syntax_error(input, "Missing '}'");
          success = false;
        }
      }
    }
  }

  return success;
}

bool32
parse_else_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                     AstBlock* enclosing_block, AstNode* owner, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Else)
  {
    consume_token(arena, input, symbol_table);

    AstNode* else_node = 0;
    success = parse_if_stmt(arena, input, symbol_table, enclosing_block, &else_node);
    if(success)
    {
      if(else_node)
      {
        *node = else_node;
      } else
      {
        success = parse_block(arena, input, symbol_table, enclosing_block, owner, &else_node);
        if(success)
        {
          if(else_node)
          {
            *node = else_node;
          } else
          {
            success = parse_statement(arena, input, symbol_table, enclosing_block, &else_node);
            if(else_node)
            {
              *node = else_node;
            } else {
              syntax_error(input, "Statement(s) required");
              success = false;
            }
          }
        }
      }
    }
  }
  return success;
}

bool32
parse_if_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
              AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_If)
  {
    consume_token(arena, input, symbol_table);

    if(input->token.kind == TokenKind_OpenParens)
    {
      consume_token(arena, input, symbol_table);

      AstNode* expr_node = 0;
      if(success = parse_expression(arena, input, symbol_table, enclosing_block, &expr_node))
      {
        if(input->token.kind == TokenKind_CloseParens)
        {
          consume_token(arena, input, symbol_table);

          if(expr_node)
          {
            *node = ast_new_if_stmt(arena);
            AstIfStmt* if_stmt = &(*node)->if_stmt;
            if_stmt->cond_expr = expr_node;

            AstNode* body_node = 0;
            success = parse_block(arena, input, symbol_table, enclosing_block, *node, &body_node);
            if(success)
            {
              if(body_node)
              {
                if_stmt->body = body_node;

                AstNode* else_node = 0;
                success = parse_else_statement(arena, input, symbol_table, enclosing_block, *node, &else_node);
                if(success)
                {
                  if_stmt->else_body = else_node;
                }
              }
              else
              {
                success = parse_statement(arena, input, symbol_table, enclosing_block, &body_node);
                if(success)
                {
                  if(body_node)
                  {
                    if(body_node->kind != AstNodeKind_VarDecl)
                    {
                      if_stmt->body = body_node;

                      AstNode* else_node = 0;
                      success = parse_else_statement(arena, input, symbol_table, enclosing_block, *node, &else_node);
                      if(success)
                      {
                        if_stmt->else_body = else_node;
                      }
                    }
                    else {
                      syntax_error(input, "Var statement not allowed here");
                      success = false;
                    }
                  }
                  else {
                    syntax_error(input, "Statement(s) required");
                    success = false;
                  }
                }
              }
            }
          }
          else {
            syntax_error(input, "Expression required");
            success = false;
          }
        }
        else {
          syntax_error(input, "Missing ')'");
          success = false;
        }
      }
    }
    else {
      syntax_error(input, "Missing '('");
      success = false;
    }
  }
  return success;
}

bool32
parse_procedure(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Proc)
  {
    *node = ast_new_proc(arena);
    AstProc* proc = &(*node)->proc;

    consume_token(arena, input, symbol_table);
    if(input->token.kind == TokenKind_Id)
    {
      Symbol* type_symbol = lookup_symbol(symbol_table, input->token.lexeme, SymbolKind_Type);
      if(type_symbol)
      {
        proc->ret_type = type_symbol->type;

        consume_token(arena, input, symbol_table);
        if(input->token.kind == TokenKind_Id)
        {
          Symbol* symbol = lookup_symbol(symbol_table, input->token.lexeme, SymbolKind_Proc);
          if(!symbol)
          {
            symbol = add_symbol(arena, symbol_table, input->token.lexeme, SymbolKind_Proc);
            symbol->node = *node;

            proc->name = symbol->name;
            proc->ret_var.data.size = 1;

            consume_token(arena, input, symbol_table);
            if(input->token.kind == TokenKind_OpenParens)
            {
              consume_token(arena, input, symbol_table);

              // arguments
              success = scope_begin(symbol_table);
              if(success)
              {
                AstNode* block_node = ast_new_block(arena, symbol_table);
                AstBlock* block = &block_node->block;
                proc->body = block_node;
                block->owner = *node;

                success = parse_formal_argument_list(arena, input, symbol_table, block, proc);
                if(success)
                {
                  for(ListItem* list_item = list_first_item(&proc->formal_args);
                      list_item;
                      list_item = list_item->next)
                  {
                    AstNode* arg_node = list_item->elem;
                    assert(arg_node->kind == AstNodeKind_VarDecl);
                    AstVarDecl* arg = &arg_node->var_decl;
                    arg->data.size = 1;
                  }

                  if(input->token.kind == TokenKind_CloseParens)
                  {
                    consume_token(arena, input, symbol_table);

                    if(input->token.kind == TokenKind_OpenBrace)
                    {
                      // body
                      consume_token(arena, input, symbol_table);

                      success = parse_statement_list(arena, input, symbol_table, block);
                      if(success)
                      {
                        if(input->token.kind == TokenKind_CloseBrace)
                        {
                          consume_token(arena, input, symbol_table);
                          scope_end(symbol_table); // body
                        }
                        else {
                          syntax_error(input, "Missing '}'");
                          success = false;
                        }
                      }
                    }
                    else {
                      syntax_error(input, "Missing '{'");
                      success = false;
                    }
                  }
                  else {
                    if(input->token.kind == TokenKind_Id)
                    {
                      syntax_error(input, "Missing 'var' keyword", input->token.lexeme);
                    }
                    else
                    {
                      syntax_error(input, "Missing ')'");
                    }
                    success = false;
                  }
                }
              }
            }
            else {
              syntax_error(input, "Missing '('");
              success = false;
            }
          }
          else {
            syntax_error(input, "Redeclaration of procedure: %s", input->token.lexeme);
            success = false;
          }
        }
        else {
          syntax_error(input, "Identifier expected");
          success = false;
        }
      }
      else {
        syntax_error(input, "Unknown type: %s", input->token.lexeme);
        success = false;
      }
    }
    else {
      syntax_error(input, "Identifier expected");
      success = false;
    }
  }

  return success;
}

bool32
parse_include_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                   AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Include)
  {
    consume_token(arena, input, symbol_table);

    if(input->token.kind == TokenKind_String)
    {
      *node = ast_new_include_stmt(arena);
      AstIncludeStmt* inc_stmt = &(*node)->inc_stmt;

      String str = {0};
      str_init(&str, arena);
      str_append(&str, input->file_path);
      path_make_dir(str.head);
      str_tidyup(&str);
      str_append(&str, input->token.str);
      inc_stmt->file_path = str.head;

      consume_token(arena, input, symbol_table);
    } else {
      syntax_error(input, "String required after 'include'\n");
      success = false;
    }
  }
  return success;
}

bool32
parse_module(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
             AstBlock* enclosing_block, AstModule* module)
{
  bool32 success = true;

  AstNode* node = 0;
  success = parse_procedure(arena, input, symbol_table, enclosing_block, &node);
  if(success)
  {
    if(node)
    {
      list_append(arena, &module->proc_list, node);
      success = parse_module(arena, input, symbol_table, enclosing_block, module);
    }
    else
    {
      success = parse_include_stmt(arena, input, symbol_table, enclosing_block, &node);
      if(success)
      {
        if(node)
        {
          AstIncludeStmt* inc_stmt = &node->inc_stmt;

          char* hoc_text = file_read_text(arena, inc_stmt->file_path);
          if(hoc_text)
          {
            TokenStream* inc_input = mem_push_struct(arena, TokenStream, 1);
            token_stream_init(inc_input, hoc_text, inc_stmt->file_path);

            consume_token(arena, inc_input, symbol_table);

            success = parse_module(arena, inc_input, symbol_table, enclosing_block, module);
            if(success)
            {
              if(inc_input->token.kind == TokenKind_EndOfInput)
              {
                success = parse_module(arena, input, symbol_table, enclosing_block, module);
              }
              else {
                syntax_error(inc_input, "Unexpected token");
                success = false;
              }
            }
          }
          else {
            syntax_error(input, "File could not be read: %s", inc_stmt->file_path);
            success = false;
          }
        }
      }
    }
  }

  return success;
}

bool32
parse_print_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                 AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Print)
  {
    consume_token(arena, input, symbol_table);

    AstNode* expr_node = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &expr_node);
    if(success)
    {
      *node = ast_new_print_stmt(arena);
      AstPrintStmt* print_stmt = &(*node)->print_stmt;

      if(expr_node)
      {
        print_stmt->expr = expr_node;
      }/* else {
        syntax_error(input, "Expression required after 'print'");
        success = false;
      }*/

      if(input->token.kind == TokenKind_BackSlash)
      {
        consume_token(arena, input, symbol_table);

        if(input->token.kind == TokenKind_Id)
        {
          if(cstr_match("n", input->token.lexeme))
          {
            print_stmt->new_line = true;
            consume_token(arena, input, symbol_table);
          } else {
            syntax_error(input, "Expected new line char '\n'");
            success = false;
          }
        }
      }
    }
  }
  return success;
}

int
block_find_owner(AstBlock* block, AstNodeKind kind, AstNode** result)
{
  int depth = 0;
  AstNode* owner = 0;
  while(block)
  {
    owner = block->owner;
    if(owner->kind == kind)
      break;
    depth++;
    block = block->enclosing_block;
  }
  *result = owner;
  return depth;
}

bool32
parse_return_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                  AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Return)
  {
    consume_token(arena, input, symbol_table);

    AstNode* ret_expr = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &ret_expr);
    if(success)
    {
      *node = ast_new_return_stmt(arena);
      AstReturnStmt* ret_stmt = &(*node)->ret_stmt;
      ret_stmt->ret_expr = ret_expr;

      AstNode* owner = 0;
      int depth = block_find_owner(enclosing_block, AstNodeKind_Proc, &owner);
      if(owner)
      {
        AstProc* ret_proc = &owner->proc;
        ret_stmt->proc = ret_proc;
        ret_stmt->depth = depth;

        if(ret_expr)
        {
          AstNode* var_node = ast_new_var_occur(arena);
          AstVarOccur* var_occur = &var_node->var_occur;
          var_occur->data = &ret_proc->ret_var.data;
          var_occur->decl_block_offset = depth;
          var_node->type = ret_proc->ret_type;

          if(depth > 0)
          {
            list_append(arena, &enclosing_block->nonlocal_occurs, var_node);
          }
          else
          {
            list_append(arena, &enclosing_block->local_occurs, var_node);
          }

          AstNode* assgn_node = ast_new_bin_expr(arena);
          AstBinExpr* assgn_expr = &assgn_node->bin_expr;
          assgn_expr->op = AstOpKind_Assign;
          assgn_expr->left_operand = var_node;
          assgn_expr->right_operand = ret_expr;

          ret_stmt->assgn_expr = assgn_expr;
        }
      }
      else {
        syntax_error(input, "'return' : enclosing procedure not found");
        success = false;
      }
    }
  }

  return success;
}

bool32
parse_break_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                 AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  if(input->token.kind == TokenKind_Break)
  {
    consume_token(arena, input, symbol_table);

    *node = ast_new_break_stmt(arena);
    AstBreakStmt* break_stmt = &(*node)->break_stmt;

    AstNode* owner = 0;
    int depth = block_find_owner(enclosing_block, AstNodeKind_WhileStmt, &owner);
    if(owner)
    {
      break_stmt->while_stmt = &owner->while_stmt;
      break_stmt->depth = depth + 1;
    } else {
      syntax_error(input, "'break': enclosing 'while' statement not found");
      success = false;
    }
  }
  return success;
}

bool32
parse_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                AstBlock* enclosing_block, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  typedef enum
  {
    Alt__Null,
    Alt_Var,
    Alt_Expr,
    Alt_If,
    Alt_While,
    Alt_Return,
    Alt_Break,
    Alt_Print,
    Alt_EmptyStmt,
  } Alternative;

  Alternative alt = (Alternative)1;
  AstNode* stmt_node = 0;

  while(alt) {
    switch(alt) {
      case Alt_Expr:
      {
        success = parse_expression(arena, input, symbol_table, enclosing_block, &stmt_node);
        if(success)
        {
          if(stmt_node)
          {
            alt = Alt__Null;
            if(input->token.kind == TokenKind_Semicolon)
            {
              consume_token(arena, input, symbol_table);

              if(stmt_node->kind == AstNodeKind_BinExpr)
              {
                AstBinExpr* expr = &stmt_node->bin_expr;
                if(expr->op != AstOpKind_Assign)
                {
                  syntax_error(input, "Assignment expression required");
                  success = false;
                }
              } else if(stmt_node->kind != AstNodeKind_Call)
              {
                syntax_error(input, "Expression is not a statement");
                success = false;
              }
            } else {
              syntax_error(input, "Missing ';'");
              success = false;
            }
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_If:
      {
        success = parse_if_stmt(arena, input, symbol_table, enclosing_block, &stmt_node);
        if(success)
        {
          if(stmt_node)
          {
            alt = Alt__Null;
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_While:
      {
        success = parse_while_stmt(arena, input, symbol_table, enclosing_block, &stmt_node);
        if(success)
        {
          if(stmt_node)
          {
            alt = Alt__Null;
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_Return:
      {
        success = parse_return_stmt(arena, input, symbol_table, enclosing_block, &stmt_node);
        if(success)
        {
          if(stmt_node)
          {
            alt = Alt__Null;
            if(input->token.kind == TokenKind_Semicolon)
              consume_token(arena, input, symbol_table);
            else {
              syntax_error(input, "Missing ';'");
              success = false;
            }
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_Break:
      {
        success = parse_break_stmt(arena, input, symbol_table, enclosing_block, &stmt_node);
        if(success)
        {
          if(stmt_node)
          {
            alt = Alt__Null;
            if(input->token.kind == TokenKind_Semicolon)
              consume_token(arena, input, symbol_table);
            else {
              syntax_error(input, "Missing ';'");
              success = false;
            }
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_Var:
      {
        success = parse_var_statement(arena, input, symbol_table, enclosing_block, &stmt_node);
        if(success)
        {
          if(stmt_node)
          {
            alt = Alt__Null;
            if(input->token.kind == TokenKind_Semicolon)
              consume_token(arena, input, symbol_table);
            else {
              syntax_error(input, "Missing ';'");
              success = false;
            }
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_Print:
      {
        success = parse_print_stmt(arena, input, symbol_table, enclosing_block, &stmt_node);
        if(success)
        {
          if(stmt_node)
          {
            alt = Alt__Null;
            if(input->token.kind == TokenKind_Semicolon)
              consume_token(arena, input, symbol_table);
            else {
              syntax_error(input, "Missing ';'");
              success = false;
            }
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_EmptyStmt:
      {
        if(input->token.kind == TokenKind_Semicolon)
        {
          consume_token(arena, input, symbol_table);
          stmt_node = ast_new_empty_stmt(arena);
        } else
          alt = (Alternative)((int)alt+1);
      } break;

      default:
        alt = Alt__Null;
        break;
    }
  }

  *node = stmt_node;
  return success;
}

bool32
parse_statement_list(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                     AstBlock* block)
{
  bool32 success = true;

  while(input->token.kind == TokenKind_Semicolon)
    consume_token(arena, input, symbol_table);

  AstNode* stmt_node = 0;
  success = parse_statement(arena, input, symbol_table, block, &stmt_node);
  if(success && stmt_node)
  {
    if(stmt_node->kind == AstNodeKind_VarDecl)
    {
      list_append(arena, &block->decl_vars, stmt_node);
      AstVarDecl* var_decl = &stmt_node->var_decl;
      if(var_decl->init_expr)
      {
        list_append(arena, &block->stmt_list, var_decl->init_expr);
      }
    }
    else
      list_append(arena, &block->stmt_list, stmt_node);

    success = parse_statement_list(arena, input, symbol_table, block);
  }
  return success;
}

bool32
parse(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table, AstNode** node)
{
  *node = 0;
  bool32 success = true;

  success = scope_begin(symbol_table);
  if(success)
  {
    *node = ast_new_module(arena);
    AstModule* module = &(*node)->module;

    AstNode* block_node = ast_new_block(arena, symbol_table);
    AstBlock* block = &block_node->block;
    block->owner = *node;
    module->body = block_node;

    success = parse_module(arena, input, symbol_table, block, module);
    if(success)
    {
      scope_end(symbol_table);

      if(input->token.kind != TokenKind_EndOfInput)
      {
        syntax_error(input, "Unexpected token");
        success = false;
      }
    }
  }

  return success;
}

int
compute_data_loc(int sp, List* areas)
{
  for(ListItem* list_item = list_first_item(areas);
      list_item;
      list_item = list_item->next)
  {
    DataArea* data = list_item->elem;
    data->loc = sp;
    assert(data->size > 0);
    sp += data->size;
  }
  return sp;
}

void
fixup_data_loc(int fp, List* areas)
{
  for(ListItem* list_item = list_first_item(areas);
      list_item;
      list_item = list_item->next)
  {
    DataArea* data = list_item->elem;
    data->loc = data->loc - fp;
  }
}

void
compute_activation_record_locations(List* pre_fp_data, List* post_fp_data)
{
  int fp = compute_data_loc(0, pre_fp_data);
  compute_data_loc(fp, post_fp_data);
  fixup_data_loc(fp, pre_fp_data);
  fixup_data_loc(fp, post_fp_data);
}

void
build_call(MemoryArena* arena, AstCall* call)
{
  for(ListItem* list_item = list_first_item(&call->actual_args);
      list_item;
      list_item = list_item->next)
  {
    build_node(arena, list_item->elem);
  }
}

void
build_print_stmt(MemoryArena* arena, AstPrintStmt* print_stmt)
{
  if(print_stmt->expr)
    build_node(arena, print_stmt->expr);
}

void
build_block(MemoryArena* arena, AstBlock* block)
{
  List pre_fp_data = {0};
  list_init(&pre_fp_data);
  List post_fp_data = {0};
  list_init(&post_fp_data);

  /* access links */
  for(ListItem* list_item = list_first_item(&block->nonlocal_occurs);
      list_item;
      list_item = list_item->next)
  {
    AstNode* occur_node = list_item->elem;
    assert(occur_node->kind == AstNodeKind_VarOccur);
    AstVarOccur* var_occur = &occur_node->var_occur;

    List* links_list = &block->access_links;

    AccessLink* link = 0;
    for(ListItem* list_item = list_first_item(links_list);
        list_item;
        list_item = list_item->next)
    {
      link = list_item->elem;
      if(link->actv_rec_offset == var_occur->decl_block_offset)
        break;
      link = 0;
    }
    if(!link)
    {
      link = mem_push_struct(arena, AccessLink, 1);
      link->actv_rec_offset = var_occur->decl_block_offset;
      link->data.size = 1;
      list_append(arena, links_list, link);
      list_append(arena, &pre_fp_data, &link->data);
      block->links_size += link->data.size;
    }
    var_occur->link = link;
  }

  DataArea* old_fp = mem_push_struct(arena, DataArea, 1);
  old_fp->size = 1;
  list_append(arena, &pre_fp_data, old_fp);

  /* locals*/
  for(ListItem* list_item = list_first_item(&block->decl_vars);
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = list_item->elem;
    assert(node->kind == AstNodeKind_VarDecl);
    AstVarDecl* local = &node->var_decl;

    list_append(arena, &post_fp_data, &local->data);
    block->locals_size += local->data.size;
  }
  compute_activation_record_locations(&pre_fp_data, &post_fp_data);
  build_block_stmts(arena, &block->stmt_list);
}

void
build_while_stmt(MemoryArena* arena, AstWhileStmt* while_stmt)
{
  build_node(arena, while_stmt->cond_expr);

  {
    /* labels */
    String label_id = {0};
    str_init(&label_id, arena);
    make_unique_label(&label_id);

    String label = {0};
    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".while-expr");
    while_stmt->label_eval = label.head;

    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".while-break");
    while_stmt->label_break = label.head;
  }

  if(while_stmt->body->kind == AstNodeKind_Block)
    build_block(arena, &while_stmt->body->block);
  else
    build_node(arena, while_stmt->body);
}

void
build_if_stmt(MemoryArena* arena, AstIfStmt* if_stmt)
{
  build_node(arena, if_stmt->cond_expr);

  {
    /* labels */
    String label_id = {0};
    str_init(&label_id, arena);
    make_unique_label(&label_id);

    String label = {0};
    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".if-else");
    if_stmt->label_else = label.head;

    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".if-end");
    if_stmt->label_end = label.head;
  }

  if(if_stmt->body->kind == AstNodeKind_Block)
    build_block(arena, &if_stmt->body->block);
  else
    build_node(arena, if_stmt->body);

  if(if_stmt->else_body)
  {
    AstNode* else_node = if_stmt->else_body;
    if(else_node->kind == AstNodeKind_Block)
    {
      build_block(arena, &else_node->block);
    }
    else if(else_node->kind == AstNodeKind_IfStmt)
    {
      build_if_stmt(arena, &else_node->if_stmt);
    }
    else
    {
      build_node(arena, else_node);
    }
  }
}

void
build_bin_expr(MemoryArena* arena, AstBinExpr* bin_expr)
{
  String label_id = {0};
  str_init(&label_id, arena);
  make_unique_label(&label_id);

  String label = {0};
  str_init(&label, arena);
  str_append(&label, label_id.head);
  str_append(&label, ".logic-end");
  bin_expr->label_end = label.head;

  build_node(arena, bin_expr->left_operand);
  build_node(arena, bin_expr->right_operand);
}

void
build_unr_expr(MemoryArena* arena, AstUnrExpr* unr_expr)
{
  build_node(arena, unr_expr->operand);
}

void
build_node(MemoryArena* arena, AstNode* node)
{
  if(node->kind == AstNodeKind_BinExpr)
  {
    build_bin_expr(arena, &node->bin_expr);
  }
  else if(node->kind == AstNodeKind_UnrExpr)
  {
    build_unr_expr(arena, &node->unr_expr);
  }
  else if(node->kind == AstNodeKind_Call)
  {
    build_call(arena, &node->call);
  }
  else if(node->kind == AstNodeKind_IfStmt)
  {
    build_if_stmt(arena, &node->if_stmt);
  }
  else if(node->kind == AstNodeKind_WhileStmt)
  {
    build_while_stmt(arena, &node->while_stmt);
  }
  else if(node->kind == AstNodeKind_PrintStmt)
  {
    build_print_stmt(arena, &node->print_stmt);
  }
  else if(node->kind == AstNodeKind_VarOccur ||
          node->kind == AstNodeKind_BreakStmt ||
          node->kind == AstNodeKind_ReturnStmt ||
          node->kind == AstNodeKind_EmptyStmt ||
          node->kind == AstNodeKind_IntNum)
    ;
  else
    assert(false);
}

void
build_block_stmts(MemoryArena* arena, List* stmt_list)
{
  for(ListItem* list_item = list_first_item(stmt_list);
      list_item;
      list_item = list_item->next)
  {
    build_node(arena, list_item->elem);
  }
}

void
build_proc(MemoryArena* arena, AstProc* proc)
{
  proc->label = proc->name;

  String label = {0};
  str_init(&label, arena);
  str_append(&label, proc->name);
  str_append(&label, ".proc-end");
  proc->label_end = label.head;

  List pre_fp_data = {0};
  list_init(&pre_fp_data);
  List post_fp_data = {0};
  list_init(&post_fp_data);

  AstVarDecl* ret_var = &proc->ret_var;
  list_append(arena, &pre_fp_data, &ret_var->data);
  proc->ret_size = ret_var->data.size;

  /* formals */
  for(ListItem* list_item = list_first_item(&proc->formal_args);
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = list_item->elem;
    assert(node->kind == AstNodeKind_VarDecl);
    AstVarDecl* formal = &node->var_decl;

    list_append(arena, &pre_fp_data, &formal->data);
    proc->args_size += formal->data.size;
  }

  DataArea* ctrl_links = mem_push_struct(arena, DataArea, 1);
  ctrl_links->size = 3; // fp,sp,ip
  list_append(arena, &pre_fp_data, ctrl_links);

  AstBlock* body_block = &proc->body->block;
  /* locals */
  for(ListItem* list_item = list_first_item(&body_block->decl_vars);
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = list_item->elem;
    assert(node->kind == AstNodeKind_VarDecl);
    AstVarDecl* local = &node->var_decl;

    list_append(arena, &post_fp_data, &local->data);
    proc->locals_size += local->data.size;
  }
  compute_activation_record_locations(&pre_fp_data, &post_fp_data);
  build_block_stmts(arena, &body_block->stmt_list);
}
 
void
build_module(MemoryArena* arena, SymbolTable* symbol_table, AstModule* module)
{
  for(ListItem* list_item = list_first_item(&module->proc_list);
      list_item;
      list_item = list_item->next)
  {
    AstNode* proc_node = list_item->elem;
    assert(proc_node->kind == AstNodeKind_Proc);
    AstProc* proc = &proc_node->proc;

    build_proc(arena, proc);
    if(cstr_match(proc->name, "main"))
    {
      module->main_proc = proc_node;
    }
  }

  if(module->main_proc)
  {
    AstNode* call_node = ast_new_call(arena);
    AstCall* call = &call_node->call;
    call->proc = module->main_proc;
    build_call(arena, call);
    module->main_call = call_node;
  }
}

/* Code gen */

void
print_instruction(VmProgram* vm_program, char* code, ...)
{
  static char strbuf[128] = {0};
  va_list args;

  va_start(args, code);
  vm_program->text_len += vsprintf(strbuf, code, args);
  va_end(args);

  str_append(&vm_program->text, strbuf);
  str_append(&vm_program->text, "\n");
  vm_program->text_len++;
}

void
emit_instr_reg(MemoryArena* arena, List* instr_list, Opcode opcode, RegName reg)
{
  Instruction* instr = mem_push_struct(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType_Reg;
  instr->param.reg = reg;
  list_append(arena, instr_list, instr);
}

void
emit_instr_int(MemoryArena* arena, List* instr_list, Opcode opcode, int32 int_val)
{
  Instruction* instr = mem_push_struct(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType_Int32;
  instr->param.int_val = int_val;
  list_append(arena, instr_list, instr);
}

void
emit_instr(MemoryArena* arena, List* instr_list, Opcode opcode)
{
  Instruction* instr = mem_push_struct(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType__Null;
  list_append(arena, instr_list, instr);
}

void
emit_instr_str(MemoryArena* arena, List* instr_list, Opcode opcode, char* str)
{
  assert(str);
  Instruction* instr = mem_push_struct(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType_String;
  instr->param.str = str;
  list_append(arena, instr_list, instr);
}

void gen_load_rvalue(MemoryArena*, List*, AstNode*);
void gen_load_lvalue(MemoryArena*, List*, AstVarOccur*);
void gen_statement(MemoryArena*, List*, AstNode*);

void
gen_bin_expr(MemoryArena* arena, List* code, AstBinExpr* bin_expr)
{
  if(bin_expr->op == AstOpKind_Assign)
  {
    gen_load_rvalue(arena, code, bin_expr->right_operand);

    assert(bin_expr->left_operand->kind = AstNodeKind_VarOccur);
    gen_load_lvalue(arena, code, &bin_expr->left_operand->var_occur);

    emit_instr(arena, code, Opcode_STORE);
  }
  else
  {
    switch(bin_expr->op)
    {
      case AstOpKind_Add:
      case AstOpKind_Sub:
      case AstOpKind_Mul:
      case AstOpKind_Div:
      case AstOpKind_Mod:
      case AstOpKind_LogicEquals:
      case AstOpKind_LogicNotEquals:
      case AstOpKind_LogicLess:
      case AstOpKind_LogicGreater:
      {
        gen_load_rvalue(arena, code, bin_expr->left_operand);
        gen_load_rvalue(arena, code, bin_expr->right_operand);

        if(bin_expr->op == AstOpKind_Add)
        {
          emit_instr(arena, code, Opcode_ADD);
        }
        else if(bin_expr->op == AstOpKind_Sub)
        {
          emit_instr(arena, code, Opcode_SUB);
        }
        else if(bin_expr->op == AstOpKind_Mul)
        {
          emit_instr(arena, code, Opcode_MUL);
        }
        else if(bin_expr->op == AstOpKind_Div)
        {
          emit_instr(arena, code, Opcode_DIV);
        }
        else if(bin_expr->op == AstOpKind_Mod)
        {
          emit_instr(arena, code, Opcode_MOD);
        }
        else if(bin_expr->op == AstOpKind_LogicEquals)
        {
          emit_instr(arena, code, Opcode_CMPEQ);
        }
        else if(bin_expr->op == AstOpKind_LogicNotEquals)
        {
          emit_instr(arena, code, Opcode_CMPNEQ);
        }
        else if(bin_expr->op == AstOpKind_LogicLess)
        {
          emit_instr(arena, code, Opcode_CMPLSS);
        }
        else if(bin_expr->op == AstOpKind_LogicGreater)
        {
          emit_instr(arena, code, Opcode_CMPGRT);
        }
        else
          assert(false);
      } break;

      case AstOpKind_LogicAnd:
      case AstOpKind_LogicOr:
      {
        gen_load_rvalue(arena, code, bin_expr->left_operand);
        emit_instr(arena, code, Opcode_DUP);

        if(bin_expr->op == AstOpKind_LogicAnd)
        {
          emit_instr_str(arena, code, Opcode_JUMPZ, bin_expr->label_end);
        }
        else if(bin_expr->op == AstOpKind_LogicOr)
        {
          emit_instr_str(arena, code, Opcode_JUMPNZ, bin_expr->label_end);
        }
        else
          assert(false);

        emit_instr(arena, code, Opcode_POP);
        gen_load_rvalue(arena, code, bin_expr->right_operand);
        emit_instr_str(arena, code, Opcode_LABEL, bin_expr->label_end);
      } break;

      case AstOpKind_LogicLessEquals:
      case AstOpKind_LogicGreaterEquals:
      {
        gen_load_rvalue(arena, code, bin_expr->left_operand);
        gen_load_rvalue(arena, code, bin_expr->right_operand);
        if(bin_expr->op == AstOpKind_LogicLessEquals)
        {
          emit_instr(arena, code, Opcode_CMPLSS);
        }
        else if(bin_expr->op == AstOpKind_LogicGreaterEquals)
        {
          emit_instr(arena, code, Opcode_CMPGRT);
        }
        else
          assert(false);
        emit_instr(arena, code, Opcode_DUP);

        emit_instr_str(arena, code, Opcode_JUMPNZ, bin_expr->label_end);
        emit_instr(arena, code, Opcode_POP);
        gen_load_rvalue(arena, code, bin_expr->left_operand);
        gen_load_rvalue(arena, code, bin_expr->right_operand);
        emit_instr(arena, code, Opcode_CMPEQ);
        emit_instr_str(arena, code, Opcode_LABEL, bin_expr->label_end);
      } break;

      default:
        assert(false);
    }
  }
}

void gen_unr_expr(MemoryArena* arena, List* code, AstUnrExpr* unr_expr)
{
  gen_load_rvalue(arena, code, unr_expr->operand);
  if(unr_expr->op == AstOpKind_Neg)
  {
    emit_instr(arena, code, Opcode_NEG);
  }
  else if(unr_expr->op == AstOpKind_LogicNot)
  {
    emit_instr_int(arena, code, Opcode_PUSH, 0);
    emit_instr(arena, code, Opcode_CMPEQ);
  }
  else
    assert(false);
}

void
gen_call(MemoryArena* arena, List* code, AstCall* call)
{
  AstProc* proc = &call->proc->proc;
  emit_instr_int(arena, code, Opcode_ALLOC, proc->ret_size);

  for(ListItem* list_item = list_first_item(&call->actual_args);
      list_item;
      list_item = list_item->next)
  {
    AstNode* arg = list_item->elem;
    gen_load_rvalue(arena, code, arg);
  }

  emit_instr_str(arena, code, Opcode_CALL, proc->label);
  emit_instr_int(arena, code, Opcode_POP, proc->args_size); // discard args
}

void
gen_load_lvalue(MemoryArena* arena, List* code, AstVarOccur* var_occur)
{
  DataArea* data = var_occur->data;
  AccessLink* link = var_occur->link;

  emit_instr_reg(arena, code, Opcode_PUSH, RegName_FP);
  if(link) 
  {
    // this is a non-local
    assert(link->data.loc < 0); // relative to FP
    emit_instr_int(arena, code, Opcode_PUSH, link->data.loc);
    emit_instr(arena, code, Opcode_ADD);
    emit_instr(arena, code, Opcode_LOAD); // access link is on the stack now
  }
  emit_instr_int(arena, code, Opcode_PUSH, data->loc);
  emit_instr(arena, code, Opcode_ADD);
}

void
gen_load_rvalue(MemoryArena* arena, List* code, AstNode* node)
{
  if(node->kind == AstNodeKind_VarOccur)
  {
    gen_load_lvalue(arena, code, &node->var_occur);
    emit_instr(arena, code, Opcode_LOAD);
  }
  else if(node->kind == AstNodeKind_Call)
  {
    gen_call(arena, code, &node->call);
  }
  else if(node->kind == AstNodeKind_IntNum)
  {
    emit_instr_int(arena, code, Opcode_PUSH, node->int_val.value);
  }
  else if(node->kind == AstNodeKind_BinExpr)
  {
    gen_bin_expr(arena, code, &node->bin_expr);
  }
  else if(node->kind == AstNodeKind_UnrExpr)
  {
    gen_unr_expr(arena, code, &node->unr_expr);
  }
  else
    assert(false);
}

void
gen_return_stmt(MemoryArena* arena, List* code, AstReturnStmt* ret_stmt)
{
  if(ret_stmt->assgn_expr)
  {
    gen_bin_expr(arena, code, ret_stmt->assgn_expr);
    emit_instr(arena, code, Opcode_POP);
  }

  AstProc* proc = ret_stmt->proc;
  int depth = ret_stmt->depth;
  while(depth--)
    emit_instr(arena, code, Opcode_LEAVE);
  emit_instr_str(arena, code, Opcode_GOTO, proc->label_end);
}

void
gen_break_stmt(MemoryArena* arena, List* code, AstBreakStmt* break_stmt)
{
  AstWhileStmt* while_stmt = break_stmt->while_stmt;
  int depth = break_stmt->depth;
  while(depth--)
    emit_instr(arena, code, Opcode_LEAVE);
  emit_instr_str(arena, code, Opcode_GOTO, while_stmt->label_break);
}

void
gen_block(MemoryArena* arena, List* code, AstBlock* block)
{
  for(ListItem* list_item = list_first_item(&block->access_links);
      list_item;
      list_item = list_item->next)
  {
    AccessLink* link = list_item->elem;
    emit_instr_reg(arena, code, Opcode_PUSH, RegName_FP);
    assert(link->actv_rec_offset > 0);
    int offset = link->actv_rec_offset - 1;
    while(offset--)
    {
      emit_instr(arena, code, Opcode_DECR); // TODO: explain why
      emit_instr(arena, code, Opcode_LOAD);
    }
  }

  emit_instr(arena, code, Opcode_ENTER);

  emit_instr_int(arena, code, Opcode_ALLOC, block->locals_size);

  for(ListItem* list_item = list_first_item(&block->stmt_list);
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = list_item->elem;
    gen_statement(arena, code, node);
  }

  emit_instr(arena, code, Opcode_LEAVE);
}

void
gen_proc(MemoryArena* arena, List* code, AstProc* proc)
{
  emit_instr_str(arena, code, Opcode_LABEL, proc->label);
  emit_instr_int(arena, code, Opcode_ALLOC, proc->locals_size);

  AstBlock* body_block = &proc->body->block;
  for(ListItem* list_item = list_first_item(&body_block->stmt_list);
      list_item;
      list_item = list_item->next)
  {
    AstNode* node = list_item->elem;
    gen_statement(arena, code, node);
  }

  emit_instr_str(arena, code, Opcode_LABEL, proc->label_end);
  emit_instr(arena, code, Opcode_RETURN);
}

void
gen_if_stmt(MemoryArena* arena, List* code, AstIfStmt* if_stmt)
{
  gen_load_rvalue(arena, code, if_stmt->cond_expr);

  if(if_stmt->else_body)
    emit_instr_str(arena, code, Opcode_JUMPZ, if_stmt->label_else);
  else
    emit_instr_str(arena, code, Opcode_JUMPZ, if_stmt->label_end);

  if(if_stmt->body->kind == AstNodeKind_Block)
    gen_block(arena, code, &if_stmt->body->block);
  else
    gen_statement(arena, code, if_stmt->body);

  emit_instr_str(arena, code, Opcode_GOTO, if_stmt->label_end);

  if(if_stmt->else_body)
  {
    emit_instr_str(arena, code, Opcode_LABEL, if_stmt->label_else);
    AstNode* else_body = if_stmt->else_body;
    if(else_body->kind == AstNodeKind_Block)
      gen_block(arena, code, &else_body->block);
    else
      gen_statement(arena, code, else_body);
  }

  emit_instr_str(arena, code, Opcode_LABEL, if_stmt->label_end);
}

void
gen_while_stmt(MemoryArena* arena, List* code, AstWhileStmt* while_stmt)
{
  emit_instr_str(arena, code, Opcode_LABEL, while_stmt->label_eval);
  gen_load_rvalue(arena, code, while_stmt->cond_expr);
  emit_instr_str(arena, code, Opcode_JUMPZ, while_stmt->label_break);
  if(while_stmt->body->kind == AstNodeKind_Block)
    gen_block(arena, code, &while_stmt->body->block);
  else
    gen_statement(arena, code, while_stmt->body);
  emit_instr_str(arena, code, Opcode_GOTO, while_stmt->label_eval);

  emit_instr_str(arena, code, Opcode_LABEL, while_stmt->label_break);
}

void
gen_print_stmt(MemoryArena* arena, List* code, AstPrintStmt* print_stmt)
{
  if(print_stmt->expr)
  {
    gen_load_rvalue(arena, code, print_stmt->expr);
    emit_instr(arena, code, Opcode_PRINT);
  }
  if(print_stmt->new_line)
    emit_instr(arena, code, Opcode_PRINTNL);
}

void
gen_empty_stmt(MemoryArena* arena, List* code)
{
  emit_instr(arena, code, Opcode_NOOP);
}

void
gen_statement(MemoryArena* arena, List* code, AstNode* stmt_node)
{
  if(stmt_node->kind == AstNodeKind_BinExpr)
  {
    AstBinExpr* bin_expr = &stmt_node->bin_expr;
    assert(bin_expr->op == AstOpKind_Assign);
    gen_bin_expr(arena, code, bin_expr);
    emit_instr(arena, code, Opcode_POP);
  }
  else if(stmt_node->kind == AstNodeKind_Call)
  {
    gen_call(arena, code, &stmt_node->call);
    emit_instr(arena, code, Opcode_POP);
  }
  else if(stmt_node->kind == AstNodeKind_ReturnStmt)
  {
    gen_return_stmt(arena, code, &stmt_node->ret_stmt);
  }
  else if(stmt_node->kind == AstNodeKind_BreakStmt)
  {
    gen_break_stmt(arena, code, &stmt_node->break_stmt);
  }
  else if(stmt_node->kind == AstNodeKind_Noop)
  {
    emit_instr(arena, code, Opcode_NOOP);
  }
  else if(stmt_node->kind == AstNodeKind_IfStmt)
  {
    gen_if_stmt(arena, code, &stmt_node->if_stmt);
  }
  else if(stmt_node->kind == AstNodeKind_WhileStmt)
  {
    gen_while_stmt(arena, code, &stmt_node->while_stmt);
  }
  else if(stmt_node->kind == AstNodeKind_PrintStmt)
  {
    gen_print_stmt(arena, code, &stmt_node->print_stmt);
  }
  else if(stmt_node->kind == AstNodeKind_EmptyStmt)
  {
    gen_empty_stmt(arena, code);
  }
  else
    assert(false);
}

void
gen_module(MemoryArena* arena, List* code, AstModule* module)
{
  gen_call(arena, code, &module->main_call->call);
  emit_instr(arena, code, Opcode_HALT);

  for(ListItem* list_item = list_first_item(&module->proc_list);
      list_item;
      list_item = list_item->next)
  {
    AstNode* proc_node = list_item->elem;
    assert(proc_node->kind == AstNodeKind_Proc);
    AstProc* proc = &proc_node->proc;

    gen_proc(arena, code, proc);
  }
}

char*
get_regname_str(RegName reg)
{
  static char* reg_fp = "fp";
  static char* reg_sp = "sp";
  static char* reg_ip = "ip";
  char* regname = 0;

  if(reg == RegName_FP)
    regname = reg_fp;
  else if(reg == RegName_SP)
    regname = reg_sp;
  else if(reg == RegName_IP)
    regname = reg_ip;
  else
    assert(false);
  return regname;
}

void
print_code(VmProgram* vm_program)
{
  for(ListItem* list_item = list_first_item(&vm_program->instr_list);
      list_item;
      list_item = list_item->next)
  {
    Instruction* instr = list_item->elem;
    switch(instr->opcode)
    {
      case Opcode_PUSH:
      {
        if(instr->param_type == ParamType_Reg)
          print_instruction(vm_program, "push %s", get_regname_str(instr->param.reg));
        else if(instr->param_type == ParamType_Int32)
          print_instruction(vm_program, "push %d", instr->param.int_val);
        else
          assert(false);
      } break;

      case Opcode_POP:
      {
        if(instr->param_type == ParamType_Reg)
          print_instruction(vm_program, "pop %s", get_regname_str(instr->param.reg));
        else if(instr->param_type == ParamType__Null)
          print_instruction(vm_program, "pop");
        else if(instr->param_type == ParamType_Int32)
          print_instruction(vm_program, "pop %d", instr->param.int_val);
        else
          assert(false);
      } break;

      case Opcode_DUP:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "dup");
      } break;

      case Opcode_ADD:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "add");
      } break;

      case Opcode_SUB:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "sub");
      } break;

      case Opcode_MUL:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "mul");
      } break;

      case Opcode_DIV:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "div");
      } break;

      case Opcode_MOD:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "mod");
      } break;

      case Opcode_NEG:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "neg");
      } break;

      case Opcode_LOAD:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "load");
      } break;

      case Opcode_STORE:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "store");
      } break;

      case Opcode_LABEL:
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "label %s", instr->param.str);
      } break;

      case Opcode_RETURN:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "return");
      } break;

      case Opcode_ALLOC:
      {
        assert(instr->param_type == ParamType_Int32);
        print_instruction(vm_program, "alloc %d", instr->param.int_val);
      } break;

      case Opcode_CALL:
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "call %s", instr->param.str);
      } break;

      case Opcode_HALT:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "halt");
      } break;

      case Opcode_GOTO:
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "goto %s", instr->param.str);
      } break;

      case Opcode_JUMPZ:
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "jumpz %s", instr->param.str);
      } break;

      case Opcode_JUMPNZ:
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "jumpnz %s", instr->param.str);
      } break;

      case Opcode_DECR:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "decr");
      } break;

      case Opcode_ENTER:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "enter");
      } break;

      case Opcode_LEAVE:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "leave");
      } break;

      case Opcode_NOOP:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "noop");
      } break;

      case Opcode_CMPEQ:
      case Opcode_CMPNEQ:
      case Opcode_CMPLSS:
      case Opcode_CMPGRT:
      {
        assert(instr->param_type == ParamType__Null);
        if(instr->opcode == Opcode_CMPEQ)
          print_instruction(vm_program, "cmpeq");
        else if(instr->opcode == Opcode_CMPNEQ)
          print_instruction(vm_program, "cmpneq");
        else if(instr->opcode == Opcode_CMPLSS)
          print_instruction(vm_program, "cmplss");
        else if(instr->opcode == Opcode_CMPGRT)
          print_instruction(vm_program, "cmpgrt");
        else
          assert(false);
      } break;

      case Opcode_AND:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "and");
      } break;

      case Opcode_OR:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "or");
      } break;

      case Opcode_NOT:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "not");
      } break;

      case Opcode_PRINT:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "print");
      } break;

      case Opcode_PRINTNL:
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "printnl");
      } break;

      default:
        assert(false);
    }
  }
}

Type*
type_find_equiv_set(Type* type)
{
  Type* result = type;
  while(type->equiv_set)
  {
    type = type->equiv_set;
    result = type;
  }
  return result;
}

void
type_set_union(Type* type_a, Type* type_b)
{
  if(type_a->kind == TypeKind_TypeVar)
  {
    type_a->equiv_set = type_b;
  }
  else
  {
    type_b->equiv_set = type_a;
  }
}

bool32
type_unification(Type* type_a, Type* type_b)
{
  bool32 success = false;
  Type* equiv_set_a = type_find_equiv_set(type_a);
  Type* equiv_set_b = type_find_equiv_set(type_b);

  if(equiv_set_a->kind == TypeKind_TypeVar ||
     equiv_set_b->kind == TypeKind_TypeVar)
  {
    type_set_union(equiv_set_a, equiv_set_b);
    success = true;
  }
  else if(equiv_set_a->kind == equiv_set_b->kind)
  {
    if(equiv_set_a == equiv_set_b)
    {
      success = true;
    }
    else if(equiv_set_a->kind == TypeKind_Basic)
    {
      success = equiv_set_a->basic.kind == equiv_set_b->basic.kind;
    }
    else
    {
      type_set_union(equiv_set_a, equiv_set_b);

      if(equiv_set_a->kind == TypeKind_Proc)
      {
        success = type_unification(equiv_set_a->proc.args, equiv_set_b->proc.args) &&
          type_unification(equiv_set_a->proc.ret, equiv_set_b->proc.ret);
      }
      else if(equiv_set_a->kind == TypeKind_Product)
      {
        success = type_unification(equiv_set_a->product.left, equiv_set_b->product.left) &&
          type_unification(equiv_set_a->product.right, equiv_set_b->product.right);
      }
      else if(equiv_set_a->kind == TypeKind_Pointer)
      {
        success = type_unification(equiv_set_a->ptr.pointee, equiv_set_b->ptr.pointee);
      }
    }
  }

  if(!success)
    error("Type error");

  return success;
}

Type*
make_product_type(MemoryArena* arena, Type* type_in, ListItem* list_item)
{
  Type* type_out = type_in;

  if(list_item)
  {
    AstNode* node = list_item->elem;
    Type* right_type = make_product_type(arena, node->type, list_item->next);
    type_out = new_product_type(arena, type_in, right_type);
  }
  return type_out;
}

TypeTuple*
new_type_tuple(MemoryArena* arena, Type* key, Type* value)
{
  TypeTuple* tuple = mem_push_struct(arena, TypeTuple, 1);
  tuple->key = key;
  tuple->value = value;
  return tuple;
}

TypeTuple*
type_find_tuple(List* tuple_list, Type* type)
{
  TypeTuple* result = 0;
  for(ListItem* list_item = list_first_item(tuple_list);
      list_item;
      list_item = list_item->next)
  {
    TypeTuple* tuple = list_item->elem;
    if(tuple->key == type)
    {
      result = tuple;
      break;
    }
  }
  return result;
}

Type*
type_substitution(MemoryArena* arena, List* tuple_list, Type* type)
{
  type = type_find_equiv_set(type);
  Type* subst = 0;

  TypeTuple* tuple = type_find_tuple(tuple_list, type);
  if(tuple)
  {
    subst = tuple->value;
  }
  else
  {
    subst = copy_type(arena, type);

    tuple = new_type_tuple(arena, type, subst);
    list_append(arena, tuple_list, tuple);

    if(subst->kind == TypeKind_TypeVar)
    {
      subst->typevar.id = g_typevar_id++;
    }
    else if(subst->kind == TypeKind_Proc)
    {
      subst->proc.args = type_substitution(arena, tuple_list, subst->proc.args);
      subst->proc.ret = type_substitution(arena, tuple_list, subst->proc.ret);
    }
    else if(subst->kind == TypeKind_Product)
    {
      subst->product.left = type_substitution(arena, tuple_list, subst->product.left);
      subst->product.right = type_substitution(arena, tuple_list, subst->product.right);
    }
    else if(subst->kind == TypeKind_Pointer)
    {
      subst->ptr.pointee = type_substitution(arena, tuple_list, subst->ptr.pointee);
    }
    // else fall-thru
  }
  return subst;
}

bool32
typecheck_expr(MemoryArena* arena, List* type_tuples, AstNode* expr_node, Type** type)
{
  Type* result = 0;
  bool32 success = true;

  if(expr_node->kind == AstNodeKind_IntNum)
  {
    result = type_substitution(arena, type_tuples, expr_node->type);
  }
  else if(expr_node->kind == AstNodeKind_VarOccur)
  {
    AstNode* var_decl_node = expr_node->var_occur.var_decl;
    success = type_unification(expr_node->type, var_decl_node->type);
    if(success)
    {
      result = type_substitution(arena, type_tuples, expr_node->type);
    }
  }
  else if(expr_node->kind == AstNodeKind_BinExpr)
  {
    AstBinExpr* bin_expr = &expr_node->bin_expr;
    AstNode* left_operand = bin_expr->left_operand;
    AstNode* right_operand = bin_expr->right_operand;
    Type* left_type = 0;
    Type* right_type = 0;

    if(success = typecheck_expr(arena, type_tuples, left_operand, &left_type) &&
       typecheck_expr(arena, type_tuples, right_operand, &right_type) &&
       type_unification(left_type, right_type))
    {
      if(!is_logical_operator(bin_expr->op))
      {
        result = type_substitution(arena, type_tuples, left_operand->type);
      }
      else
      {
        result = g_basic_type_bool;
      }
    }
  }
  else if(expr_node->kind == AstNodeKind_UnrExpr)
  {
    AstUnrExpr* unr_expr = &expr_node->unr_expr;
    AstNode* operand = unr_expr->operand;
    Type* operand_type = 0;

    if(success = typecheck_expr(arena, type_tuples, operand, &operand_type))
    {
      if(unr_expr->op == AstOpKind_LogicNot)
        success = type_unification(expr_node->type, g_basic_type_bool);

      if(success)
        result = type_substitution(arena, type_tuples, expr_node->type);
    }
  }
  else if(expr_node->kind == AstNodeKind_Call)
  {
    AstCall* call = &expr_node->call;
    AstProc* proc = &call->proc->proc;
    assert(call->actual_args.count == proc->formal_args.count);

    // actual args
    {
      Type* arg_type = 0;
      for(ListItem* list_item = list_first_item(&call->actual_args);
          success && list_item;
          list_item = list_item->next)
      {
        AstNode* arg_node = list_item->elem;
        success = typecheck_expr(arena, type_tuples, arg_node, &arg_type) &&
          type_unification(arg_node->type, arg_type);
      }
    }

    // call type
    {
      Type* args_type = g_basic_type_void;
      int arg_count = call->actual_args.count;

      if(arg_count > 0)
      {
        ListItem* list_item = list_first_item(&call->actual_args);
        AstNode* arg_node = list_item->elem;
        args_type = make_product_type(arena, arg_node->type, list_item->next);
      }
      else if(arg_count < 0)
        assert(false);

      Type* proc_type = new_proc_type(arena, args_type, proc->ret_type);
      if(success = type_unification(proc_type, call->proc->type) &&
         type_unification(expr_node->type, proc->ret_type))
      {
        result = type_substitution(arena, type_tuples, expr_node->type);
      }
    }
  }
  else
    assert(false);

  *type = result;
  return success;
}

bool32
typecheck_stmt(MemoryArena* arena, List* type_tuples, AstNode* stmt_node)
{
  bool32 success = true;

  if(stmt_node->kind == AstNodeKind_ReturnStmt)
  {
    AstReturnStmt* ret_stmt = &stmt_node->ret_stmt;
    Type* ret_type = g_basic_type_void;
    if(ret_stmt->ret_expr)
    {
      success = typecheck_expr(arena, type_tuples, ret_stmt->ret_expr, &ret_type) &&
        type_unification(ret_type, ret_stmt->proc->ret_type);
    }
  }
  else if(stmt_node->kind == AstNodeKind_VarDecl)
  {
    success = type_unification(stmt_node->type, stmt_node->var_decl.var_type);
  }
  else if(stmt_node->kind == AstNodeKind_VarOccur)
  {
    AstNode* var_decl_node = stmt_node->var_occur.var_decl;
    success = type_unification(stmt_node->type, var_decl_node->var_decl.var_type);
  }
  else if(stmt_node->kind == AstNodeKind_BinExpr)
  {
    Type* expr_type = 0;
    success = typecheck_expr(arena, type_tuples, stmt_node, &expr_type);
  }
  else if(stmt_node->kind == AstNodeKind_IfStmt)
  {
    AstIfStmt* if_stmt = &stmt_node->if_stmt;
    Type* cond_type = 0;
    if(success = typecheck_expr(arena, type_tuples, if_stmt->cond_expr, &cond_type) &&
       type_unification(cond_type, g_basic_type_bool))
    {
      AstNode* body_node = if_stmt->body;
      if(body_node->kind == AstNodeKind_Block)
        success = typecheck_block(arena, type_tuples, body_node);
      else
        success = typecheck_stmt(arena, type_tuples, body_node);

      AstNode* else_node = if_stmt->else_body;
      if(else_node)
      {
        if(else_node->kind == AstNodeKind_Block)
          success = typecheck_block(arena, type_tuples, else_node);
        else
          success = typecheck_stmt(arena, type_tuples, else_node);
      }
    }
  }
  else if(stmt_node->kind == AstNodeKind_WhileStmt)
  {
    AstWhileStmt* while_stmt = &stmt_node->while_stmt;
    Type* cond_type = 0;
    if(success = typecheck_expr(arena, type_tuples, while_stmt->cond_expr, &cond_type) &&
       type_unification(cond_type, g_basic_type_bool))
    {
      AstNode* body_node = while_stmt->body;
      if(body_node->kind == AstNodeKind_Block)
        success = typecheck_block(arena, type_tuples, body_node);
      else
        success = typecheck_stmt(arena, type_tuples, body_node);
    }
  }
  else if(stmt_node->kind == AstNodeKind_EmptyStmt)
  {
    // ok
  }
  else
    assert(false);

  return success;
}

bool32
typecheck_block(MemoryArena* arena, List* type_tuples, AstNode* block_node)
{
  bool32 success = true;
  assert(block_node->kind == AstNodeKind_Block);
  AstBlock* block = &block_node->block;

  // decl vars
  for(ListItem* list_item = list_first_item(&block->decl_vars);
      success && list_item;
      list_item = list_item->next)
  {
    AstNode* var_decl_node = list_item->elem;
    assert(var_decl_node->kind == AstNodeKind_VarDecl);
    AstVarDecl* var_decl = &var_decl_node->var_decl;

    success = type_unification(var_decl_node->type, var_decl->var_type);
  }

  // statements
  for(ListItem* list_item = list_first_item(&block->stmt_list);
      success && list_item;
      list_item = list_item->next)
  {
    AstNode* stmt_node = list_item->elem;
    typecheck_stmt(arena, type_tuples, stmt_node);
  }

  return success;
}

bool32
typecheck_proc(MemoryArena* arena, List* type_tuples, AstNode* proc_node)
{
  bool32 success = true;
  assert(proc_node->kind == AstNodeKind_Proc);
  AstProc* proc = &proc_node->proc;

  // formal args
  for(ListItem* list_item = list_first_item(&proc->formal_args);
      success && list_item;
      list_item = list_item->next)
  {
    AstNode* var_decl_node = list_item->elem;
    assert(var_decl_node->kind == AstNodeKind_VarDecl);
    success = type_unification(var_decl_node->type, var_decl_node->var_decl.var_type);
  }

  if(success)
  {
    Type* args_type = g_basic_type_void;
    int arg_count = proc->formal_args.count;

    if(arg_count > 0)
    {
      ListItem* list_item = list_first_item(&proc->formal_args);
      AstNode* arg_node = list_item->elem;
      args_type = make_product_type(arena, arg_node->type, list_item->next);
    }
    else if(arg_count < 0)
      assert(false);

    Type* proc_type = new_proc_type(arena, args_type, proc->ret_type);
    success = type_unification(proc_type, proc_node->type) &&
      typecheck_block(arena, type_tuples, proc->body);
  }

  return success;
}

bool32
typecheck_module(MemoryArena* arena, List* type_tuples, AstModule* module)
{
  bool32 success = true;
  for(ListItem* list_item = list_first_item(&module->proc_list);
      success && list_item;
      list_item = list_item->next)
  {
    AstNode* proc = list_item->elem;
    assert(proc->kind == AstNodeKind_Proc);
    success = typecheck_proc(arena, type_tuples, proc);
  }
  return success;
}

VmProgram* translate_hoc(MemoryArena* arena, char* file_path, char* hoc_text)
{
  bool32 success = false;

  init_global_basic_types(arena);
  list_init(&g_type_tuples);

  SymbolTable symbol_table = {0};
  symbol_table.arena = arena;
  add_keyword_list(arena, &symbol_table);

  TokenStream token_stream = {0};
  token_stream_init(&token_stream, hoc_text, file_path);

  consume_token(arena, &token_stream, &symbol_table);

  AstNode* node = 0;
  success = parse(arena, &token_stream, &symbol_table, &node);
  DEBUG_arena_print_occupancy("Parse", arena);

  VmProgram* vm_program = 0;
  if(success)
  {
    assert(symbol_table.scope_id == 0);
    assert(symbol_table.nesting_depth == 0);

    AstModule* module = &node->module;
    if(typecheck_module(arena, &g_type_tuples, module))
    {
      DEBUG_arena_print_occupancy("Typecheck", arena);

      build_module(arena, &symbol_table, module);
      DEBUG_arena_print_occupancy("Runtime objects", arena);

      if(module->main_call)
      {
        vm_program = mem_push_struct(arena, VmProgram, 1);
        list_init(&vm_program->instr_list);
        gen_module(arena, &vm_program->instr_list, module);
        DEBUG_arena_print_occupancy("Gen code", arena);

        str_init(&vm_program->text, arena);
        print_code(vm_program);
        DEBUG_arena_print_occupancy("Print code", arena);
      }
      else
      {
        error("Missing main() procedure");
      }
    }
  }
  return vm_program;
}

