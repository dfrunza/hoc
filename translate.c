#include "lib.c"

#include "lex.h"

///////////////////////////////////////////////////////////////////////////////
//  AST
//

typedef struct Symbol_ Symbol;
typedef struct AstNode_ AstNode;
typedef struct AstBlock_ AstBlock;
typedef struct AstProc_ AstProc;
typedef struct AstCall_ AstCall;
typedef struct Type_ Type;

///////////////////////////////////////////////////////////////////////////////
//    Type system
//

typedef enum
{
  TypeKind__Null,
  TypeKind_TypeVar,
  TypeKind_Unary,
  TypeKind_Basic,
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

static int type_var_id = 1; // FIXME: Find this guy a home.

typedef struct
{
  int id;
} TypeVar;

typedef enum
{
  BasicTypeKind__Null,
  BasicTypeKind_Void,
  BasicTypeKind_Integer,
  BasicTypeKind_Float,
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

typedef struct Type_
{
  TypeKind kind;
  struct Type_* equiv_set;

  union
  {
    BasicType basic;
    PointerType ptr;
    ProcType proc;
    ProductType product;
    TypeVar type_var;
  };
} Type;

typedef struct
{
  Type* key;
  Type* value;
} TypeTuple;

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
  AstNodeKind_AstBinExpr,
  AstNodeKind_AstUnrExpr,
  AstNodeKind_AstIntNum,
  AstNodeKind_AstVarDecl,
  AstNodeKind_AstVarOccur,
  AstNodeKind_AstCall,
  AstNodeKind_AstBlock,
  AstNodeKind_AstProc,
  AstNodeKind_AstWhileStmt,
  AstNodeKind_AstIfStmt,
  AstNodeKind_AstReturnStmt,
  AstNodeKind_AstBreakStmt,
  AstNodeKind_AstPrintStmt,
  AstNodeKind_AstIncludeStmt,
  AstNodeKind_AstEmptyStmt,
  AstNodeKind_AstModule,
  AstNodeKind_AstNoop,
}
AstNodeKind;

typedef struct AstNode_
{
  AstNodeKind kind;
}
AstNode;

#define ast_node_as(NODE, TYPE) (TYPE*)ast_node_as_((NODE), AstNodeKind_##TYPE)

inline AstNode*
ast_node_as_(AstNode* node, AstNodeKind kind)
{
  assert(node->kind == kind);
  return node;
}

typedef struct
{
  AstNode node;

  List proc_list; // <AstNode>
  AstNode* body;

  AstProc* main_proc;
  AstCall* main_call;
}
AstModule;

typedef struct
{
  int loc;
  int size;
}
DataArea;

typedef struct AccessLink_
{
  int actv_rec_offset;
  DataArea data;
}
AccessLink;

typedef struct
{
  AstNode node;

  Symbol* symbol;
  char* name;
  DataArea data;
  AstNode* init_expr;
}
AstVarDecl;

typedef struct
{
  AstNode node;

  Symbol* symbol;
  char* name;
  int decl_block_offset;
  AccessLink* link;
  DataArea* data;
}
AstVarOccur;

typedef struct
{
  AstNode node;

  AstOpKind op;
  AstNode* left_operand;
  AstNode* right_operand;

  char* label_end; // for boolean expressions
}
AstBinExpr;

typedef struct
{
  AstNode node;

  AstOpKind op;
  AstNode* operand;
}
AstUnrExpr;

typedef struct
{
  AstNode node;

  int32 value;
}
AstIntNum;

typedef struct AstProc_
{
  AstNode node;

  Symbol* symbol;
  char* name;
  List formal_args; // <AstVarDecl>
  AstBlock* body;
  AstVarDecl ret_var;
  struct Type_ ret_type;

  char* label;
  char* label_end;
  int ret_size;
  int args_size;
  int locals_size;
}
AstProc;

typedef struct
{
  AstNode node;

  AstNode* ret_expr;
  AstBinExpr* assgn_expr;
  int depth;
  AstProc* proc;
}
AstReturnStmt;

typedef struct AstCall_
{
  AstNode node;

  Symbol* symbol;
  char* name;
  List actual_args; // <AstNode>
  AstProc* proc;
}
AstCall;

typedef struct
{
  AstNode node;

  AstNode* expr;
  AstNode* body;
  AstNode* else_body;

  char* label_else;
  char* label_end;
}
AstIfStmt;

typedef struct
{
  AstNode node;

  AstNode* expr;
  AstNode* body;

  char* label_eval;
  char* label_break;
}
AstWhileStmt;

typedef struct
{
  AstNode node;

  AstWhileStmt* while_stmt;
  int depth;
}
AstBreakStmt;

typedef struct
{
  AstNode node;

  AstNode* expr;
  bool32 new_line;
}
AstPrintStmt;

typedef struct
{
  AstNode node;

  char* file_path;
}
AstIncludeStmt;

typedef AstNode AstEmptyStmt;

typedef struct AstBlock_
{
  AstNode node;

  AstNode* owner;
  int block_id;
  int nesting_depth;
  struct AstBlock_* enclosing_block;
  List decl_vars; // <AstVarDecl>
  List local_occurs; // <AstVarOccur>
  List nonlocal_occurs; // <AstVarOccur>
  List stmt_list; // <AstNode>
  List access_links; // <AccessLink>

  int links_size;
  int locals_size;
}
AstBlock;

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
  char label[64];
  int last_label_id;
  List type_tuples;
  MemoryArena* arena;
}
SymbolTable;

typedef enum
{
  SymbolKind__Null,
  SymbolKind_Keyword,
  SymbolKind_Proc,
  SymbolKind_Var,
}
SymbolKind;


typedef struct Symbol_
{
  SymbolKind kind;
  Symbol* next_symbol;

  char* name;
  int block_id;
  int nesting_depth;
  Type type;

  union {
    AstVarDecl* var_decl;
    AstProc* proc;
    Token kw_token;
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

//
//  End of structs
////////////////////////////////////////////////////////////////////////////

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

void
make_type_var(Type* type)
{
  type->kind = TypeKind_TypeVar;
  type->type_var.id = type_var_id++;
}

void
make_basic_type(Type* type, Token type_token)
{
  type->kind = TypeKind_Basic;
  if(type_token == Token_Int)
  {
    type->basic.kind = BasicTypeKind_Integer;
  }
  else if(type_token == Token_Void)
  {
    type->basic.kind = BasicTypeKind_Void;
  }
  else if(type_token == Token_Float)
  {
    type->basic.kind = BasicTypeKind_Float;
  }
  else
    assert(false);
}

Type*
new_type_var(MemoryArena* arena)
{
  Type* type = mem_push_struct(arena, Type, 1);
  type->kind = TypeKind_TypeVar;
  type->type_var.id = type_var_id++;
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
symbol_lookup(SymbolTable* symbol_table, char* name)
{
  Symbol* result = 0;

  Symbol* symbol = symbol_table->symbol;
  while(symbol)
  {
    if(cstr_match(symbol->name, name))
    {
      result = symbol;
      break;
    }
    symbol = symbol->next_symbol;
  }
  return result;
}

Symbol*
symbol_add(SymbolTable* symbol_table, char* name, SymbolKind kind)
{
  Symbol* symbol = mem_push_struct(symbol_table->arena, Symbol, 1);
  symbol->name = name;
  symbol->kind = kind;
  symbol->block_id = symbol_table->scope_id;
  symbol->nesting_depth = symbol_table->nesting_depth;
  symbol->next_symbol = symbol_table->symbol;
  symbol_table->symbol = symbol;
  return symbol;
}

bool32
symbol_register_new(SymbolTable* symbol_table, TokenStream* input, Symbol** new_symbol, SymbolKind kind)
{
  assert(input->token == Token_Id);
  bool32 success = true;

  Symbol* symbol = symbol_lookup(symbol_table, input->lexeme.str);
  if(!symbol)
  {
    symbol = symbol_add(symbol_table, input->lexeme.str, kind);
  } else
  {
    if(symbol->kind != SymbolKind_Keyword)
    {
      if(symbol->block_id != symbol_table->scope_id || symbol->kind != kind)
      {
        assert(symbol->nesting_depth <= symbol_table->nesting_depth);

        symbol = symbol_add(symbol_table, input->lexeme.str, kind);
      } else {
        syntax_error(input, "Re-declaraion of identifier: %s", symbol->name);
        success = false;
      }
    } else {
      syntax_error(input, "Keyword used as identifier: %s", symbol->name);
      success = false;
    }
  }
  *new_symbol = symbol;
  return success;
}

Symbol*
add_keyword(SymbolTable* symbol_table, char* name, Token token)
{
  Symbol* symbol = symbol_add(symbol_table, name, SymbolKind_Keyword);
  symbol->kw_token = token;
  return symbol;
}

void
register_keywords(SymbolTable* symbol_table)
{
  add_keyword(symbol_table, "int", Token_Int);
  add_keyword(symbol_table, "float", Token_Float);
  add_keyword(symbol_table, "void", Token_Void);
  add_keyword(symbol_table, "char", Token_Char);
  add_keyword(symbol_table, "var", Token_Var);
  add_keyword(symbol_table, "proc", Token_Proc);
  add_keyword(symbol_table, "type", Token_Type);
  add_keyword(symbol_table, "struct", Token_Type);
  add_keyword(symbol_table, "array", Token_Array);
  add_keyword(symbol_table, "of", Token_Of);
  add_keyword(symbol_table, "if", Token_If);
  add_keyword(symbol_table, "else", Token_Else);
  add_keyword(symbol_table, "while", Token_While);
  add_keyword(symbol_table, "return", Token_Return);
  add_keyword(symbol_table, "break", Token_Break);
  add_keyword(symbol_table, "include", Token_Include);
  add_keyword(symbol_table, "true", Token_True);
  add_keyword(symbol_table, "false", Token_False);
  add_keyword(symbol_table, "print", Token_Print);
}

#include "lex.c"

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

static int last_label_id; // FIXME: got no idea where to stick this

void
make_unique_label(String* label)
{
  sprintf(label->head, "L%d", last_label_id++);
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
bool32 parse_module(MemoryArena*, TokenStream*, SymbolTable*, AstNode**);
void build_node(MemoryArena*, AstNode*);
void build_block_stmts(MemoryArena*, List*);

inline AstModule*
ast_new_module(MemoryArena* arena)
{
  AstModule* module = mem_push_struct(arena, AstModule, 1);
  list_init(&module->proc_list);
  module->node.kind = AstNodeKind_AstModule;
  return module;
}

inline AstBlock*
ast_new_block(MemoryArena* arena, SymbolTable* symbol_table)
{
  AstBlock* block = mem_push_struct(arena, AstBlock, 1);
  list_init(&block->decl_vars);
  list_init(&block->local_occurs);
  list_init(&block->nonlocal_occurs);
  list_init(&block->stmt_list);
  list_init(&block->access_links);
  block->block_id = symbol_table->scope_id;
  block->nesting_depth = symbol_table->nesting_depth;
  block->node.kind = AstNodeKind_AstBlock;
  return block;
}

inline AstCall*
ast_new_call(MemoryArena* arena)
{
  AstCall* call = mem_push_struct(arena, AstCall, 1);
  list_init(&call->actual_args);
  call->node.kind = AstNodeKind_AstCall;
  return call;
}

inline AstProc*
ast_new_proc(MemoryArena* arena)
{
  AstProc* proc = mem_push_struct(arena, AstProc, 1);
  list_init(&proc->formal_args);
  proc->node.kind = AstNodeKind_AstProc;
  return proc;
}

inline AstBinExpr*
ast_new_bin_expr(MemoryArena* arena)
{
  AstBinExpr* bin_expr = mem_push_struct(arena, AstBinExpr, 1);
  bin_expr->node.kind = AstNodeKind_AstBinExpr;
  return bin_expr;
}

inline AstUnrExpr*
ast_new_unr_expr(MemoryArena* arena)
{
  AstUnrExpr* unr_expr = mem_push_struct(arena, AstUnrExpr, 1);
  unr_expr->node.kind = AstNodeKind_AstUnrExpr;
  return unr_expr;
}

inline AstIntNum*
ast_new_int_num(MemoryArena* arena)
{
  AstIntNum* int_num = mem_push_struct(arena, AstIntNum, 1);
  int_num->node.kind = AstNodeKind_AstIntNum;
  return int_num;
}

inline AstVarDecl*
ast_new_var_decl(MemoryArena* arena)
{
  AstVarDecl* var_decl = mem_push_struct(arena, AstVarDecl, 1);
  var_decl->node.kind = AstNodeKind_AstVarDecl;
  return var_decl;
}

inline AstVarOccur*
ast_new_var_occur(MemoryArena* arena)
{
  AstVarOccur* var_occur = mem_push_struct(arena, AstVarOccur, 1);
  var_occur->node.kind = AstNodeKind_AstVarOccur;
  return var_occur;
}

inline AstWhileStmt*
ast_new_while_stmt(MemoryArena* arena)
{
  AstWhileStmt* while_stmt = mem_push_struct(arena, AstWhileStmt, 1);
  while_stmt->node.kind = AstNodeKind_AstWhileStmt;
  return while_stmt;
}

inline AstIfStmt*
ast_new_if_stmt(MemoryArena* arena)
{
  AstIfStmt* if_stmt = mem_push_struct(arena, AstIfStmt, 1);
  if_stmt->node.kind = AstNodeKind_AstIfStmt;
  return if_stmt;
}

inline AstReturnStmt*
ast_new_return_stmt(MemoryArena* arena)
{
  AstReturnStmt* ret_stmt = mem_push_struct(arena, AstReturnStmt, 1);
  ret_stmt->node.kind = AstNodeKind_AstReturnStmt;
  return ret_stmt;
}

inline AstBreakStmt*
ast_new_break_stmt(MemoryArena* arena)
{
  AstBreakStmt* break_stmt = mem_push_struct(arena, AstBreakStmt, 1);
  break_stmt->node.kind = AstNodeKind_AstBreakStmt;
  return break_stmt;
}

inline AstPrintStmt*
ast_new_print_stmt(MemoryArena* arena)
{
  AstPrintStmt* print_stmt = mem_push_struct(arena, AstPrintStmt, 1);
  print_stmt->node.kind = AstNodeKind_AstPrintStmt;
  return print_stmt;
}

inline AstIncludeStmt*
ast_new_include_stmt(MemoryArena* arena)
{
  AstIncludeStmt* inc_stmt = mem_push_struct(arena, AstIncludeStmt, 1);
  inc_stmt->node.kind = AstNodeKind_AstIncludeStmt;
  return inc_stmt;
}

inline AstEmptyStmt*
ast_new_empty_stmt(MemoryArena* arena)
{
  AstNode* empty = mem_push_struct(arena, AstNode, 1);
  empty->kind = AstNodeKind_AstEmptyStmt;
  return empty;
}

bool32
parse_factor(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
             AstBlock* enclosing_block, AstNode** node)
{
  bool32 success = true;

  if(input->token == Token_OpenParens)
  {
    consume_token(input, symbol_table);
    success = parse_expression(arena, input, symbol_table, enclosing_block, node);
    if(success)
    {
      if(input->token == Token_CloseParens)
        consume_token(input, symbol_table);
      else {
        syntax_error(input, "Missing ')'");
        success = false;
      }
    }
  }
  else if(input->token == Token_UnaryMinus)
  {
    consume_token(input, symbol_table);

    AstNode* operand = 0;
    success = parse_term(arena, input, symbol_table, enclosing_block, &operand);
    if(success)
    {
      if(operand)
      {
        AstUnrExpr* expr = ast_new_unr_expr(arena);
        expr->op = AstOpKind_Neg;
        expr->operand = operand;
        *node = &expr->node;
      } else {
        syntax_error(input, "Expression expected after '-'");
        success = false;
      }
    }
  }
  else if(input->token == Token_Bang)
  {
    consume_token(input, symbol_table);

    AstNode* operand = 0;
    success = parse_term(arena, input, symbol_table, enclosing_block, &operand);
    if(success)
    {
      if(operand)
      {
        AstUnrExpr* expr = ast_new_unr_expr(arena);
        expr->op = AstOpKind_LogicNot;
        expr->operand = operand;
        *node = &expr->node;
      } else {
        syntax_error(input, "Expression expected after '!'");
        success = false;
      }
    }
  }
  else if(input->token == Token_IntNum)
  {
    AstIntNum* int_num = ast_new_int_num(arena);
    int_num->value = *(int32*)input->lexeme.int_num;
    *node = &int_num->node;

    consume_token(input, symbol_table);
  }
  else if(input->token == Token_Id)
  {
    Symbol* symbol = symbol_lookup(symbol_table, input->lexeme.str);
    if(symbol)
    {
      consume_token(input, symbol_table);

      if(symbol->kind == SymbolKind_Var)
      {
        AstVarOccur* var_occur = ast_new_var_occur(arena);
        var_occur->symbol = symbol;
        var_occur->name = symbol->name;
        var_occur->data = &symbol->var_decl->data;
        var_occur->decl_block_offset = (symbol_table->nesting_depth - symbol->nesting_depth);
        *node = &var_occur->node;

        if(var_occur->decl_block_offset > 0)
        {
          list_append(arena, &enclosing_block->nonlocal_occurs, var_occur);
        }
        else if(var_occur->decl_block_offset == 0)
        {
          list_append(arena, &enclosing_block->local_occurs, var_occur);
        }
        else
          assert(false);
      }
      else if(symbol->kind == SymbolKind_Proc)
      {
        AstCall* call = ast_new_call(arena);
        call->symbol = symbol;
        call->name = symbol->name;
        call->proc = symbol->proc;
        *node = &call->node;

        if(input->token == Token_OpenParens)
        {
          consume_token(input, symbol_table);
          success = parse_actual_argument_list(arena, input, symbol_table, enclosing_block, call);
          if(success)
          {
            if(input->token == Token_CloseParens)
            {
              consume_token(input, symbol_table);

              List* formal_args = &symbol->proc->formal_args;
              List* actual_args = &call->actual_args;
              if(formal_args->count != actual_args->count)
              {
                syntax_error(input, "Expected %d arguments in the call: %s(..)", formal_args->count, symbol->name);
                success = false;
              }
            } else {
              syntax_error(input, "Missing ')' in procedure call");
              success = false;
            }
          }
        } else {
          syntax_error(input, "Missing '(' in procedure call");
          success = false;
        }
      } else if(symbol->kind == SymbolKind_Keyword)
      {
        syntax_error(input, "Keyword used as identifier: %s", input->lexeme.str);
        success = false;
      }
      else
        assert(false);
    } else {
      syntax_error(input, "Unknown identifier: %s", input->lexeme.str);
      success = false;
    }
  }
  else if(input->token == Token_True || input->token == Token_False)
  {
    AstIntNum* int_num = ast_new_int_num(arena);
    int_num->value = (input->token == Token_True ? 1 : 0);
    *node = &int_num->node;

    consume_token(input, symbol_table);
  }

  return success;
}

bool32
parse_rest_of_factors(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                      AstBlock* enclosing_block, AstNode* left_node, AstNode** node)
{
  bool32 success = true;

  if(input->token == Token_Star ||
     input->token == Token_FwdSlash ||
     input->token == Token_Percent ||
     input->token == Token_EqualsEquals ||
     input->token == Token_BangEquals ||
     input->token == Token_AmprsndAmprsnd ||
     input->token == Token_PipePipe ||
     input->token == Token_AngleLeft ||
     input->token == Token_AngleLeftEquals ||
     input->token == Token_AngleRight ||
     input->token == Token_AngleRightEquals)
  {
    AstBinExpr* expr = ast_new_bin_expr(arena);

    if(input->token == Token_Star)
    {
      expr->op = AstOpKind_Mul;
    }
    else if(input->token == Token_FwdSlash)
    {
      expr->op = AstOpKind_Div;
    }
    else if(input->token == Token_Percent)
    {
      expr->op = AstOpKind_Mod;
    }
    else if(input->token == Token_EqualsEquals)
    {
      expr->op = AstOpKind_LogicEquals;
    }
    else if(input->token == Token_BangEquals)
    {
      expr->op = AstOpKind_LogicNotEquals;
    }
    else if(input->token == Token_AngleLeft)
    {
      expr->op = AstOpKind_LogicLess;
    }
    else if(input->token == Token_AngleLeftEquals)
    {
      expr->op = AstOpKind_LogicLessEquals;
    }
    else if(input->token == Token_AngleRight)
    {
      expr->op = AstOpKind_LogicGreater;
    }
    else if(input->token == Token_AngleRightEquals)
    {
      expr->op = AstOpKind_LogicGreaterEquals;
    }
    else if(input->token == Token_AmprsndAmprsnd)
    {
      expr->op = AstOpKind_LogicAnd;
    }
    else if(input->token == Token_PipePipe)
    {
      expr->op = AstOpKind_LogicOr;
    }
    else
      assert(false);

    consume_token(input, symbol_table);
    AstNode* factor_node = 0;
    success = parse_factor(arena, input, symbol_table, enclosing_block, &factor_node);

    if(success && factor_node)
    {
      expr->right_operand = factor_node;
      expr->left_operand = left_node;
      success = parse_rest_of_factors(arena, input, symbol_table,
                                      enclosing_block, &expr->node, node);
    } else {
      syntax_error(input, "Factor expected");
      success = false;
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

  if(input->token == Token_Plus || input->token == Token_Minus)
  {
    AstBinExpr* expr = ast_new_bin_expr(arena);

    if(input->token == Token_Plus)
      expr->op = AstOpKind_Add;
    else if(input->token == Token_Minus)
      expr->op = AstOpKind_Sub;
    else
      assert(false);

    consume_token(input, symbol_table);
    AstNode* term_node = 0;
    success = parse_term(arena, input, symbol_table, enclosing_block, &term_node);

    if(success && term_node)
    {
      expr->right_operand = term_node;
      expr->left_operand = left_node;
      success = parse_rest_of_terms(arena, input, symbol_table, enclosing_block, &expr->node, node);
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

  if(input->token == Token_Equals)
  {
    consume_token(input, symbol_table);
    AstNode* right_side = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &right_side);
    if(success)
    {
      if(right_side)
      {
        if(left_node->kind == AstNodeKind_AstVarOccur)
        {
          AstBinExpr* expr = ast_new_bin_expr(arena);
          expr->op = AstOpKind_Assign;
          expr->left_operand = left_node;
          expr->right_operand = right_side;
          *node = &expr->node;
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
  bool32 success = true;

  if(input->token == Token_Var)
  {
    consume_token(input, symbol_table);

    if(input->token == Token_Int ||
       input->token == Token_Float ||
       input->token == Token_Void)
    {
      Token type_token = input->token;

      consume_token(input, symbol_table);
      if(input->token == Token_Id)
      {
        Symbol* symbol = 0;
        success = symbol_register_new(symbol_table, input, &symbol, SymbolKind_Var);

        if(success && symbol)
        {
          make_basic_type(&symbol->type, type_token);
          AstVarDecl* var_decl = ast_new_var_decl(arena);
          var_decl->symbol = symbol;
          var_decl->name = symbol->name;
          var_decl->data.size = 1;
          symbol->var_decl = var_decl;
          *node = &var_decl->node;

          consume_token(input, symbol_table);
          if(input->token == Token_Equals)
          {
            AstVarOccur* var_occur = ast_new_var_occur(arena);
            var_occur->symbol = symbol;
            var_occur->name = symbol->name;
            var_occur->data = &var_decl->data;
            var_occur->decl_block_offset = 0;

            AstNode* init_expr = 0;
            success = parse_rest_of_assignment_terms(arena, input, symbol_table,
                                                     enclosing_block, &var_occur->node, &init_expr);
            if(success)
              var_decl->init_expr = init_expr;
          }
        }
      } else {
        syntax_error(input, "Missing identifier");
        success = false;
      }
    } else {
      syntax_error(input, "Type expected");
      success = false;
    }
  }

  return success;
}

bool32
parse_formal_argument(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                      AstBlock* enclosing_block, AstNode** node)
{
  bool32 success = true;

  if(input->token == Token_Var)
  {
    consume_token(input, symbol_table);

    if(input->token == Token_Int ||
       input->token == Token_Float)
    {
      consume_token(input, symbol_table);
      Token type_token = input->token;
        
      if(input->token == Token_Id)
      {
        Symbol* symbol = 0;
        success = symbol_register_new(symbol_table, input, &symbol, SymbolKind_Var);
        if(success && symbol)
        {
          make_basic_type(&symbol->type, type_token);
          AstVarDecl* var_decl = ast_new_var_decl(arena);
          var_decl->symbol = symbol;
          var_decl->name = symbol->name;
          symbol->var_decl = var_decl;
          *node = &var_decl->node;

          consume_token(input, symbol_table);
        }
      } else {
        syntax_error(input, "Expecting an identifier token");
        success = false;
      }
    } else {
      syntax_error(input, "Type expected");
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
  success = parse_formal_argument(arena, input, symbol_table, enclosing_block, &arg_node);
  if(success && arg_node)
  {
    list_append(arena, &proc->formal_args, ast_node_as(arg_node, AstVarDecl));

    if(input->token == Token_Comma)
    {
      consume_token(input, symbol_table);
      success = parse_formal_argument_list(arena, input, symbol_table, enclosing_block, proc);
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

    if(input->token == Token_Comma)
    {
      consume_token(input, symbol_table);
      success = parse_actual_argument_list(arena, input, symbol_table, enclosing_block, call);
    }
  }

  return success;
}

bool32
parse_while_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                 AstBlock* enclosing_block, AstNode** node)
{
  bool32 success = true;

  if(input->token == Token_While)
  {
    consume_token(input, symbol_table);

    AstNode* expr_node = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &expr_node);
    if(success)
    {
      AstWhileStmt* while_stmt = ast_new_while_stmt(arena);
      while_stmt->expr = expr_node;
      *node = &while_stmt->node;

      AstNode* body_node = 0;
      success = parse_block(arena, input, symbol_table, enclosing_block, &while_stmt->node, &body_node);
      if(success)
      {
        if(body_node)
        {
          while_stmt->body = body_node;
        } else
        {
          success = parse_statement(arena, input, symbol_table, enclosing_block, &body_node);
          if(success)
          {
            if(body_node)
            {
              while_stmt->body = body_node;
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
parse_block(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
            AstBlock* enclosing_block, AstNode* owner, AstNode** node)
{
  bool32 success = true;

  if(input->token == Token_OpenBrace)
  {
    consume_token(input, symbol_table);

    success = scope_begin(symbol_table);
    if(success)
    {
      AstBlock* block = ast_new_block(arena, symbol_table);
      block->owner = owner;
      block->enclosing_block = enclosing_block;
      *node = &block->node;

      success = parse_statement_list(arena, input, symbol_table, block);
      if(success)
      {
        if(input->token == Token_CloseBrace)
        {
          consume_token(input, symbol_table);
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
  bool32 success = true;

  if(input->token == Token_Else)
  {
    consume_token(input, symbol_table);

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
  bool32 success = true;

  if(input->token == Token_If)
  {
    consume_token(input, symbol_table);
    AstNode* expr_node = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &expr_node);
    if(success)
    {
      AstIfStmt* if_stmt = ast_new_if_stmt(arena);
      if_stmt->expr = expr_node;
      *node = &if_stmt->node;

      AstNode* body_node = 0;
      success = parse_block(arena, input, symbol_table, enclosing_block, &if_stmt->node, &body_node);
      if(success)
      {
        if(body_node)
        {
          if_stmt->body = body_node;

          AstNode* else_node = 0;
          success = parse_else_statement(arena, input, symbol_table, enclosing_block, &if_stmt->node, &else_node);
          if(success)
          {
            if_stmt->else_body = else_node;
          }
        } else
        {
          success = parse_statement(arena, input, symbol_table, enclosing_block, &body_node);
          if(success)
          {
            if(body_node)
            {
              if(body_node->kind != AstNodeKind_AstVarDecl)
              {
                if_stmt->body = body_node;

                AstNode* else_node = 0;
                success = parse_else_statement(arena, input, symbol_table, enclosing_block, &if_stmt->node, &else_node);
                if(success)
                {
                  if_stmt->else_body = else_node;
                }
              } else {
                syntax_error(input, "Var statement not allowed");
                success = false;
              }
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
parse_procedure(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                AstBlock* enclosing_block, AstNode** node)
{
  bool32 success = true;

  if(input->token == Token_Proc)
  {
    AstProc* proc = ast_new_proc(arena);
    *node = &proc->node;

    consume_token(input, symbol_table);
    if(input->token == Token_Int ||
       input->token == Token_Void ||
       input->token == Token_Float)
    {
      make_basic_type(&proc->ret_type, input->token);

      consume_token(input, symbol_table);
      if(input->token == Token_Id)
      {
        Symbol* symbol = 0;
        success = symbol_register_new(symbol_table, input, &symbol, SymbolKind_Proc);

        if(success && symbol)
        {
          make_type_var(&symbol->type);
          proc->symbol = symbol;
          symbol->proc = proc;
          proc->name = symbol->name;
          proc->ret_var.data.size = 1;

          consume_token(input, symbol_table);
          if(input->token == Token_OpenParens)
          {
            consume_token(input, symbol_table);

            // arguments
            success = scope_begin(symbol_table);
            if(success)
            {
              AstBlock* block = ast_new_block(arena, symbol_table);
              proc->body = block;
              block->owner = &proc->node;

              success = parse_formal_argument_list(arena, input, symbol_table, block, proc);
              if(success)
              {
                ListItem* arg_item = list_first_item(&proc->formal_args);
                while(arg_item)
                {
                  AstVarDecl* arg = arg_item->elem;
                  Symbol* symbol = arg->symbol;
                  symbol->var_decl = arg;
                  arg->data.size = 1;

                  arg_item = arg_item->next;
                }

                if(input->token == Token_CloseParens)
                {
                  consume_token(input, symbol_table);

                  if(input->token == Token_OpenBrace)
                  {
                    // body
                    consume_token(input, symbol_table);

                    success = parse_statement_list(arena, input, symbol_table, block);
                    if(success)
                    {
                      if(input->token == Token_CloseBrace)
                      {
                        consume_token(input, symbol_table);
                        scope_end(symbol_table); // body
                      } else {
                        syntax_error(input, "Missing '}'");
                        success = false;
                      }
                    }
                  } else {
                    syntax_error(input, "Missing '{'");
                    success = false;
                  }
                } else {
                  if(input->token == Token_Id)
                    syntax_error(input, "Missing 'var' keyword", input->lexeme.str);
                  else
                    syntax_error(input, "Missing ')'");
                  success = false;
                }
              }
            }
          } else {
            syntax_error(input, "Missing '('");
            success = false;
          }
        }
      } else {
        syntax_error(input, "Missing identifier");
        success = false;
      }
    } else {
      syntax_error(input, "Type expected");
      success = false;
    }
  }

  return success;
}

bool32
parse_include_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                   AstBlock* enclosing_block, AstNode** node)
{
  bool32 success = true;

  if(input->token == Token_Include)
  {
    consume_token(input, symbol_table);

    if(input->token == Token_String)
    {
      AstIncludeStmt* inc_stmt = ast_new_include_stmt(arena);
      *node = &inc_stmt->node;

      String str = {0};
      str_init(&str, arena);
      str_append(&str, input->file_path);
      path_make_dir(str.head);
      str_tidyup(&str);
      str_append(&str, input->lexeme.str);
      inc_stmt->file_path = str.head;

      consume_token(input, symbol_table);
    } else {
      syntax_error(input, "String required after 'include'\n");
      success = false;
    }
  }
  return success;
}

bool32
parse_module_definition(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
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
      success = parse_module_definition(arena, input, symbol_table, enclosing_block, module);
    } else
    {
      success = parse_include_stmt(arena, input, symbol_table, enclosing_block, &node);
      if(success)
      {
        if(node)
        {
          AstIncludeStmt* inc_stmt = ast_node_as(node, AstIncludeStmt);

          char* hoc_text = file_read_text(arena, inc_stmt->file_path);
          if(hoc_text)
          {
            TokenStream* inc_input = mem_push_struct(arena, TokenStream, 1);
            token_stream_init(inc_input, arena, hoc_text, inc_stmt->file_path);

            consume_token(inc_input, symbol_table);

            success = parse_module_definition(arena, inc_input, symbol_table, enclosing_block, module);
            if(success)
              success = parse_module_definition(arena, input, symbol_table, enclosing_block, module);
          } else
          {
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
  bool32 success = true;

  if(input->token == Token_Print)
  {
    consume_token(input, symbol_table);

    AstNode* expr_node = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &expr_node);
    if(success)
    {
      AstPrintStmt* print_stmt = ast_new_print_stmt(arena);
      *node = &print_stmt->node;

      if(expr_node)
      {
        print_stmt->expr = expr_node;
      }/* else {
        syntax_error(input, "Expression required after 'print'");
        success = false;
      }*/

      if(input->token == Token_BackSlash)
      {
        consume_token(input, symbol_table);

        if(input->token == Token_Id)
        {
          if(cstr_match("n", input->lexeme.str))
          {
            print_stmt->new_line = true;
            consume_token(input, symbol_table);
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

bool32
parse_return_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                  AstBlock* enclosing_block, AstNode** node)
{
  bool32 success = true;

  if(input->token == Token_Return)
  {
    consume_token(input, symbol_table);

    AstNode* ret_expr = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &ret_expr);
    if(success)
    {
      AstReturnStmt* ret_stmt = ast_new_return_stmt(arena);
      ret_stmt->ret_expr = ret_expr;
      *node = &ret_stmt->node;

      int depth = 0;
      AstBlock* block = enclosing_block;
      while(block)
      {
        AstNode* owner = block->owner;
        if(owner->kind == AstNodeKind_AstProc)
          break;
        depth++;
        block = block->enclosing_block;
      }
      if(block)
      {
        AstProc* ret_proc = ast_node_as(block->owner, AstProc);
        ret_stmt->proc = ret_proc;
        ret_stmt->depth = depth;

        if(ret_expr)
        {
          AstVarOccur* var_occur = ast_new_var_occur(arena);
          var_occur->data = &ret_proc->ret_var.data;
          var_occur->decl_block_offset = depth;

          if(depth > 0)
            list_append(arena, &enclosing_block->nonlocal_occurs, &var_occur->node);
          else
            list_append(arena, &enclosing_block->local_occurs, &var_occur->node);

          AstBinExpr* assgn_expr = ast_new_bin_expr(arena);
          assgn_expr->op = AstOpKind_Assign;
          assgn_expr->left_operand = &var_occur->node;
          assgn_expr->right_operand = ret_expr;

          ret_stmt->assgn_expr = assgn_expr;
        }
      } else {
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
  bool32 success = true;

  if(input->token == Token_Break)
  {
    consume_token(input, symbol_table);

    AstBreakStmt* break_stmt = ast_new_break_stmt(arena);
    *node = &break_stmt->node;

    int depth = 0;
    AstBlock* block = enclosing_block;
    while(block)
    {
      AstNode* owner = block->owner;
      if(owner->kind == AstNodeKind_AstWhileStmt)
        break;
      depth++;
      block = block->enclosing_block;
    }
    if(block)
    {
      break_stmt->while_stmt = ast_node_as(block->owner, AstWhileStmt);
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
            if(input->token == Token_Semicolon)
            {
              consume_token(input, symbol_table);

              if(stmt_node->kind == AstNodeKind_AstBinExpr)
              {
                AstBinExpr* expr = (AstBinExpr*)stmt_node;
                if(expr->op != AstOpKind_Assign)
                {
                  syntax_error(input, "Assignment expression required");
                  success = false;
                }
              } else if(stmt_node->kind != AstNodeKind_AstCall)
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
            if(input->token == Token_Semicolon)
              consume_token(input, symbol_table);
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
            if(input->token == Token_Semicolon)
              consume_token(input, symbol_table);
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
            if(input->token == Token_Semicolon)
              consume_token(input, symbol_table);
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
            if(input->token == Token_Semicolon)
              consume_token(input, symbol_table);
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
        if(input->token == Token_Semicolon)
        {
          consume_token(input, symbol_table);
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

  while(input->token == Token_Semicolon)
    consume_token(input, symbol_table);

  AstNode* stmt_node = 0;
  success = parse_statement(arena, input, symbol_table, block, &stmt_node);
  if(success && stmt_node)
  {
    if(stmt_node->kind == AstNodeKind_AstVarDecl)
    {
      AstVarDecl* var_decl = ast_node_as(stmt_node, AstVarDecl); 
      list_append(arena, &block->decl_vars, var_decl);
      if(var_decl->init_expr)
        list_append(arena, &block->stmt_list, var_decl->init_expr);
    }
    else
      list_append(arena, &block->stmt_list, stmt_node);

    success = parse_statement_list(arena, input, symbol_table, block);
  }
  return success;
}

bool32
parse_module(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table, AstNode** node)
{
  bool32 success = true;

  success = scope_begin(symbol_table);
  if(success)
  {
    AstModule* module = ast_new_module(arena);
    *node = &module->node;

    AstBlock* block = ast_new_block(arena, symbol_table);
    block->owner = &module->node;
    module->body = &block->node;

    success = parse_module_definition(arena, input, symbol_table, block, module);
    if(success)
    {
      scope_end(symbol_table);

      if(input->token != Token_EndOfInput)
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
  ListItem* list_item = list_first_item(areas);
  while(list_item)
  {
    DataArea* data = list_item->elem;
    data->loc = sp;
    assert(data->size > 0);
    sp += data->size;
    list_item = list_item->next;
  }
  return sp;
}

void
fixup_data_loc(int fp, List* areas)
{
  ListItem* list_item = list_first_item(areas);
  while(list_item)
  {
    DataArea* data = list_item->elem;
    data->loc = data->loc - fp;
    list_item = list_item->next;
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
  ListItem* arg_item = list_first_item(&call->actual_args);
  while(arg_item)
  {
    build_node(arena, (AstNode*)arg_item->elem);
    arg_item = arg_item->next;
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
  {
    /* activation record */
    List pre_fp_data = {0};
    list_init(&pre_fp_data);
    List post_fp_data = {0};
    list_init(&post_fp_data);

    {
      /* access links */
      ListItem* occur_item = list_first_item(&block->nonlocal_occurs);
      while(occur_item)
      {
        AstVarOccur* var_occur = occur_item->elem;
        List* links_list = &block->access_links;

        ListItem* link_item = list_first_item(links_list);
        AccessLink* link = 0;
        while(link_item)
        {
          link = link_item->elem;
          if(link->actv_rec_offset == var_occur->decl_block_offset)
            break;
          link_item = link_item->next;
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

        occur_item = occur_item->next;
      }
    }

    DataArea* old_fp = mem_push_struct(arena, DataArea, 1);
    old_fp->size = 1;
    list_append(arena, &pre_fp_data, old_fp);

    {
      /* locals*/
      ListItem* node_item = list_first_item(&block->decl_vars);
      while(node_item)
      {
        AstVarDecl* local = node_item->elem;

        list_append(arena, &post_fp_data, &local->data);
        block->locals_size += local->data.size;

        node_item = node_item->next;
      }
    }
    compute_activation_record_locations(&pre_fp_data, &post_fp_data);
  }
  build_block_stmts(arena, &block->stmt_list);
}

void
build_while_stmt(MemoryArena* arena, AstWhileStmt* while_stmt)
{
  build_node(arena, while_stmt->expr);

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

  if(while_stmt->body->kind == AstNodeKind_AstBlock)
    build_block(arena, (AstBlock*)while_stmt->body);
  else
    build_node(arena, while_stmt->body);
}

void
build_if_stmt(MemoryArena* arena, AstIfStmt* if_stmt)
{
  build_node(arena, if_stmt->expr);

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

  if(if_stmt->body->kind == AstNodeKind_AstBlock)
  {
    build_block(arena, (AstBlock*)if_stmt->body);
  }
  else
    build_node(arena, if_stmt->body);

  if(if_stmt->else_body)
  {
    AstNode* else_node = if_stmt->else_body;
    if(else_node->kind == AstNodeKind_AstBlock)
    {
      build_block(arena, (AstBlock*)else_node);
    }
    else if(else_node->kind == AstNodeKind_AstIfStmt)
    {
      build_if_stmt(arena, (AstIfStmt*)else_node);
    }
    else
      build_node(arena, else_node);
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
  if(node->kind == AstNodeKind_AstBinExpr)
  {
    build_bin_expr(arena, (AstBinExpr*)node);
  }
  else if(node->kind == AstNodeKind_AstUnrExpr)
  {
    build_unr_expr(arena, (AstUnrExpr*)node);
  }
  else if(node->kind == AstNodeKind_AstCall)
  {
    build_call(arena, (AstCall*)node);
  }
  else if(node->kind == AstNodeKind_AstIfStmt)
  {
    build_if_stmt(arena, (AstIfStmt*)node);
  }
  else if(node->kind == AstNodeKind_AstWhileStmt)
  {
    build_while_stmt(arena, (AstWhileStmt*)node);
  }
  else if(node->kind == AstNodeKind_AstPrintStmt)
  {
    build_print_stmt(arena, (AstPrintStmt*)node);
  }
  else if(node->kind == AstNodeKind_AstVarOccur ||
          node->kind == AstNodeKind_AstBreakStmt ||
          node->kind == AstNodeKind_AstReturnStmt ||
          node->kind == AstNodeKind_AstEmptyStmt ||
          node->kind == AstNodeKind_AstIntNum)
    ;
  else
    assert(false);
}

void
build_block_stmts(MemoryArena* arena, List* stmt_list)
{
  ListItem* node_item = list_first_item(stmt_list);
  while(node_item)
  {
    build_node(arena, (AstNode*)node_item->elem);
    node_item = node_item->next;
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

  {
    /* activation record */
    List pre_fp_data = {0};
    list_init(&pre_fp_data);
    List post_fp_data = {0};
    list_init(&post_fp_data);

    AstVarDecl* ret_var = &proc->ret_var;
    list_append(arena, &pre_fp_data, &ret_var->data);
    proc->ret_size = ret_var->data.size;

    {
      /* formals */
      ListItem* node_item = list_first_item(&proc->formal_args);
      while(node_item)
      {
        AstVarDecl* formal = node_item->elem;
        list_append(arena, &pre_fp_data, &formal->data);
        proc->args_size += formal->data.size;

        node_item = node_item->next;
      }
    }

    DataArea* ctrl_links = mem_push_struct(arena, DataArea, 1);
    ctrl_links->size = 3; // fp,sp,ip
    list_append(arena, &pre_fp_data, ctrl_links);

    {
      /* locals */
      ListItem* node_item = list_first_item(&proc->body->decl_vars);
      while(node_item)
      {
        AstVarDecl* local = node_item->elem;

        list_append(arena, &post_fp_data, &local->data);
        proc->locals_size += local->data.size;

        node_item = node_item->next;
      }
    }
    compute_activation_record_locations(&pre_fp_data, &post_fp_data);
  }
  build_block_stmts(arena, &proc->body->stmt_list);
}
 
void
build_module(MemoryArena* arena, AstModule* module)
{
  ListItem* proc_item = list_first_item(&module->proc_list);
  while(proc_item)
  {
    AstProc* proc = ast_node_as(proc_item->elem, AstProc);
    build_proc(arena, proc);
    if(cstr_match(proc->name, "main"))
      module->main_proc = proc;

    proc_item = proc_item->next;
  }

  if(module->main_proc)
  {
    AstCall* call = ast_new_call(arena);
    call->proc = module->main_proc;
    build_call(arena, call);
    module->main_call = call;
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
emit_instr_int(MemoryArena* arena, List* instr_list, Opcode opcode, int32 int_num)
{
  Instruction* instr = mem_push_struct(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType_Int32;
  instr->param.int_num = int_num;
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

    assert(bin_expr->left_operand->kind = AstNodeKind_AstVarOccur);
    gen_load_lvalue(arena, code, (AstVarOccur*)bin_expr->left_operand);

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
  AstProc* proc = call->proc;
  emit_instr_int(arena, code, Opcode_ALLOC, proc->ret_size);

  ListItem* arg_item = list_first_item(&call->actual_args);
  while(arg_item)
  {
    AstNode* arg = arg_item->elem;
    gen_load_rvalue(arena, code, arg);
    arg_item = arg_item->next;
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
  if(node->kind == AstNodeKind_AstVarOccur)
  {
    gen_load_lvalue(arena, code, (AstVarOccur*)node);
    emit_instr(arena, code, Opcode_LOAD);
  }
  else if(node->kind == AstNodeKind_AstCall)
  {
    gen_call(arena, code, (AstCall*)node);
  }
  else if(node->kind == AstNodeKind_AstIntNum)
  {
    emit_instr_int(arena, code, Opcode_PUSH, ((AstIntNum*)node)->value);
  }
  else if(node->kind == AstNodeKind_AstBinExpr)
  {
    gen_bin_expr(arena, code, (AstBinExpr*)node);
  }
  else if(node->kind == AstNodeKind_AstUnrExpr)
  {
    gen_unr_expr(arena, code, (AstUnrExpr*)node);
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
  ListItem* link_item = list_first_item(&block->access_links);
  while(link_item)
  {
    AccessLink* link = link_item->elem;
    emit_instr_reg(arena, code, Opcode_PUSH, RegName_FP);
    int offset = link->actv_rec_offset - 1;
    while(offset--)
    {
      emit_instr(arena, code, Opcode_DECR); // TODO: explain why
      emit_instr(arena, code, Opcode_LOAD);
    }
    link_item = link_item->next;
  }

  emit_instr(arena, code, Opcode_ENTER);

  emit_instr_int(arena, code, Opcode_ALLOC, block->locals_size);

  ListItem* item = list_first_item(&block->stmt_list);
  while(item)
  {
    AstNode* node = item->elem;
    gen_statement(arena, code, node);
    item = item->next;
  }

  emit_instr(arena, code, Opcode_LEAVE);
}

void
gen_proc(MemoryArena* arena, List* code, AstProc* proc)
{
  emit_instr_str(arena, code, Opcode_LABEL, proc->label);
  emit_instr_int(arena, code, Opcode_ALLOC, proc->locals_size);

  ListItem* item = list_first_item(&proc->body->stmt_list);
  while(item)
  {
    AstNode* node = item->elem;
    gen_statement(arena, code, node);
    item = item->next;
  }

  emit_instr_str(arena, code, Opcode_LABEL, proc->label_end);
  emit_instr(arena, code, Opcode_RETURN);
}

void
gen_if_stmt(MemoryArena* arena, List* code, AstIfStmt* if_stmt)
{
  gen_load_rvalue(arena, code, if_stmt->expr);

  if(if_stmt->else_body)
    emit_instr_str(arena, code, Opcode_JUMPZ, if_stmt->label_else);
  else
    emit_instr_str(arena, code, Opcode_JUMPZ, if_stmt->label_end);

  if(if_stmt->body->kind == AstNodeKind_AstBlock)
    gen_block(arena, code, (AstBlock*)if_stmt->body);
  else
    gen_statement(arena, code, if_stmt->body);

  emit_instr_str(arena, code, Opcode_GOTO, if_stmt->label_end);

  if(if_stmt->else_body)
  {
    emit_instr_str(arena, code, Opcode_LABEL, if_stmt->label_else);
    AstNode* else_body = if_stmt->else_body;
    if(else_body->kind == AstNodeKind_AstBlock)
      gen_block(arena, code, (AstBlock*)else_body);
    else
      gen_statement(arena, code, else_body);
  }

  emit_instr_str(arena, code, Opcode_LABEL, if_stmt->label_end);
}

void
gen_while_stmt(MemoryArena* arena, List* code, AstWhileStmt* while_stmt)
{
  emit_instr_str(arena, code, Opcode_LABEL, while_stmt->label_eval);
  gen_load_rvalue(arena, code, while_stmt->expr);
  emit_instr_str(arena, code, Opcode_JUMPZ, while_stmt->label_break);
  if(while_stmt->body->kind == AstNodeKind_AstBlock)
    gen_block(arena, code, (AstBlock*)while_stmt->body);
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
gen_statement(MemoryArena* arena, List* code, AstNode* stmt_node)
{
  if(stmt_node->kind == AstNodeKind_AstBinExpr)
  {
    AstBinExpr* bin_expr = (AstBinExpr*)stmt_node;
    assert(bin_expr->op == AstOpKind_Assign);
    gen_bin_expr(arena, code, bin_expr);
    emit_instr(arena, code, Opcode_POP);
  }
  else if(stmt_node->kind == AstNodeKind_AstCall)
  {
    gen_call(arena, code, (AstCall*)stmt_node);
    emit_instr(arena, code, Opcode_POP);
  }
  else if(stmt_node->kind == AstNodeKind_AstReturnStmt)
  {
    gen_return_stmt(arena, code, (AstReturnStmt*)stmt_node);
  }
  else if(stmt_node->kind == AstNodeKind_AstBreakStmt)
  {
    gen_break_stmt(arena, code, (AstBreakStmt*)stmt_node);
  }
  else if(stmt_node->kind == AstNodeKind_AstNoop)
  {
    emit_instr(arena, code, Opcode_NOOP);
  }
  else if(stmt_node->kind == AstNodeKind_AstIfStmt)
  {
    gen_if_stmt(arena, code, (AstIfStmt*)stmt_node);
  }
  else if(stmt_node->kind == AstNodeKind_AstWhileStmt)
  {
    gen_while_stmt(arena, code, (AstWhileStmt*)stmt_node);
  }
  else if(stmt_node->kind == AstNodeKind_AstPrintStmt)
  {
    gen_print_stmt(arena, code, (AstPrintStmt*)stmt_node);
  }
  else
    assert(false);
}

void
gen_module(MemoryArena* arena, List* code, AstModule* module)
{
  gen_call(arena, code, module->main_call);
  emit_instr(arena, code, Opcode_HALT);

  ListItem* item = list_first_item(&module->proc_list);
  while(item)
  {
    AstNode* node = item->elem;
    assert(node->kind == AstNodeKind_AstProc);
    gen_proc(arena, code, (AstProc*)node);
    item = item->next;
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
  ListItem* item = list_first_item(&vm_program->instr_list);
  while(item)
  {
    Instruction* instr = item->elem;
    switch(instr->opcode)
    {
      case Opcode_PUSH:
      {
        if(instr->param_type == ParamType_Reg)
          print_instruction(vm_program, "push %s", get_regname_str(instr->param.reg));
        else if(instr->param_type == ParamType_Int32)
          print_instruction(vm_program, "push %d", instr->param.int_num);
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
          print_instruction(vm_program, "pop %d", instr->param.int_num);
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
        print_instruction(vm_program, "alloc %d", instr->param.int_num);
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
    item = item->next;
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
  Type* set_type_a = type_find_equiv_set(type_a);
  Type* set_type_b = type_find_equiv_set(type_b);

  if(set_type_a->kind == TypeKind_TypeVar ||
     set_type_b->kind == TypeKind_TypeVar)
  {
    type_set_union(set_type_a, set_type_b);
    success = true;
  }
  else if(set_type_a->kind == set_type_b->kind)
  {
    if(set_type_a == set_type_b)
    {
      success = true;
    }
    else if(set_type_a->kind == TypeKind_Basic)
    {
      success = type_a->basic.kind == type_b->basic.kind;
    }
    else
    {
      type_set_union(set_type_a, set_type_b);

      if(set_type_a->kind == TypeKind_Proc)
      {
        success = type_unification(type_a->proc.args, type_b->proc.args) &&
          type_unification(type_a->proc.ret, type_b->proc.ret);
      }
      else if(set_type_a->kind == TypeKind_Product)
      {
        success = type_unification(type_a->product.left, type_b->product.left) &&
          type_unification(type_a->product.right, type_b->product.right);
      }
      else if(set_type_a->kind == TypeKind_Pointer)
      {
        success = type_unification(type_a->ptr.pointee, type_b->ptr.pointee);
      }
    }
  }

  if(!success)
    error("Type error\n");

  return success;
}

Type*
make_product_type(MemoryArena* arena, SymbolTable* symbol_table,
                  Type* type_in, ListItem* list_item)
{
  Type* type_out = type_in;

  if(list_item)
  {
    AstVarDecl* var_decl = ast_node_as(list_item->elem, AstVarDecl);
    Type* right_type = make_product_type(arena, symbol_table,
                                         &var_decl->symbol->type,
                                         list_item->next);
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
  ListItem* list_item = list_first_item(tuple_list);
  while(list_item)
  {
    TypeTuple* tuple = (TypeTuple*)list_item->elem;
    if(tuple->key == type)
    {
      result = tuple;
      break;
    }
    list_item = list_item->next;
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
      subst->type_var.id = type_var_id++;
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

Type*
typecheck_expr(MemoryArena* arena, SymbolTable* symbol_table, AstNode* expr)
{
  Type* result = 0;

  if(expr->kind == AstNodeKind_AstVarOccur)
  {
    AstVarOccur* var_occur = (AstVarOccur*)expr;
    result = type_substitution(arena, &symbol_table->type_tuples, &var_occur->symbol->type);
  }
  else
    assert(false);

  return result;
}

bool32
typecheck_proc(MemoryArena* arena, SymbolTable* symbol_table, AstProc* proc)
{
  bool32 success = true;
  Type* args_type = 0;
  int arg_count = proc->formal_args.count;

  if(arg_count > 0)
  {
    ListItem* list_item = list_first_item(&proc->formal_args);
    AstVarDecl* var_decl = ast_node_as(list_item->elem, AstVarDecl);
    args_type = make_product_type(arena, symbol_table,
                                  &var_decl->symbol->type,
                                  list_item->next);
  }
  else if(arg_count == 0)
  {
    args_type = new_basic_type(arena, BasicTypeKind_Void);
  }
  else
    assert(false);

  Type* proc_type = new_proc_type(arena, args_type, &proc->ret_type);
  success = type_unification(proc_type, &proc->symbol->type);

  if(success)
  {
    Type* ret_type = 0;

    ListItem* list_item = list_first_item(&proc->body->stmt_list);
    while(success && list_item)
    {
      AstNode* ast_node = (AstNode*)list_item->elem;
      if(ast_node->kind == AstNodeKind_AstReturnStmt)
      {
        AstReturnStmt* ret_stmt = (AstReturnStmt*)ast_node;
        if(ret_stmt->ret_expr)
        {
          ret_type = typecheck_expr(arena, symbol_table, ret_stmt->ret_expr);
          success = type_unification(ret_type, proc_type->proc.ret);
        }
      }
      list_item = list_item->next;
    }

    if(success && !ret_type)
    {
      ret_type = new_basic_type(arena, BasicTypeKind_Void);
      success = type_unification(ret_type, proc_type->proc.ret);
    }
  }

  return success;
}

bool32
typecheck_module(MemoryArena* arena, SymbolTable* symbol_table, AstModule* module)
{
  bool32 success = true;
  ListItem* proc_item = list_first_item(&module->proc_list);
  while(success && proc_item)
  {
    AstProc* proc = ast_node_as(proc_item->elem, AstProc);
    success = typecheck_proc(arena, symbol_table, proc);
    proc_item = proc_item->next;
  }
  return success;
}

VmProgram* translate_hoc(MemoryArena* arena, char* file_path, char* hoc_text)
{
  bool32 success = false;

  SymbolTable symbol_table = {0};
  symbol_table.arena = arena;
  list_init(&symbol_table.type_tuples);

  TokenStream token_stream = {0};
  token_stream_init(&token_stream, arena, hoc_text, file_path);

  register_keywords(&symbol_table);
  consume_token(&token_stream, &symbol_table);

  AstNode* node = 0;
  success = parse_module(arena, &token_stream, &symbol_table, &node);

  VmProgram* vm_program = 0;
  if(success)
  {
    assert(symbol_table.scope_id == 0);
    assert(symbol_table.nesting_depth == 0);

    AstModule* module = ast_node_as(node, AstModule);
    if(typecheck_module(arena, &symbol_table, module))
    {
      build_module(arena, module);
      if(module->main_call)
      {
        vm_program = mem_push_struct(arena, VmProgram, 1);
        list_init(&vm_program->instr_list);
        gen_module(arena, &vm_program->instr_list, module);

        str_init(&vm_program->text, arena);
        print_code(vm_program);
      } else
        error("Missing main() procedure");
    }
  }
  return vm_program;
}

