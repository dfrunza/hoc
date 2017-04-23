#include "lib.c"

/* Lex structs */

typedef enum
{
  Token__Null,
  Token_EndOfInput,

  Token__KeywordBegin,
  Token_If,
  Token_Else,
  Token_While,
  Token_Type,
  Token_Of,
  Token_Array,
  Token_Char,
  Token_Float,
  Token_Void,
  Token_Int,
  Token_Var,
  Token_Proc,
  Token_Struct,
  Token_Return,
  Token_Break,
  Token_Include,
  Token_True,
  Token_False,
  Token_Print,
  Token__KeywordEnd,

  Token_Id,
  Token_Dot,
  Token_IntNum,
  Token_UpArrow,
  Token_RightArrow,
  Token_Literal,
  Token_OpenBracket,
  Token_CloseBracket,
  Token_Semicolon,
  Token_Colon,
  Token_Comma,
  Token_Percent,
  Token_Star,
  Token_FwdSlash,
  Token_BackSlash,
  Token_Plus,
  Token_Minus,
  Token_UnaryMinus,
  Token_Bang,
  Token_Equals,
  Token_EqualsEquals,
  Token_BangEquals,
  Token_AngleRight,
  Token_AngleRightEquals,
  Token_AngleLeft,
  Token_AngleLeftEquals,
  Token_Amprsnd,
  Token_AmprsndAmprsnd,
  Token_Pipe,
  Token_PipePipe,
  Token_OpenParens,
  Token_CloseParens,
  Token_OpenBrace,
  Token_CloseBrace,
  Token_String,
}
Token;

typedef struct
{
  Token prev_token;
  Token token;
  char* text;
  char* cursor;
  MemoryArena* arena;

  char* file_path;
  int line_nr;
  char* src_line;

  union {
    int* int_num;
    char* str;
  } lexeme;
}
TokenStream;

/* AST structs */

typedef struct Symbol_ Symbol;
typedef struct AstNode_ AstNode;
typedef struct AstBlock_ AstBlock;
typedef struct AstProc_ AstProc;
typedef struct AstCall_ AstCall;
typedef struct AstVar_ AstVar;

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

typedef struct AstNode_
{
  AstNodeKind kind;
}
AstNode;

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

typedef struct AstVar_
{
  DataArea* data;
  AccessLink* link;
}
AstVar;

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

  AstNode* expr;
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

/* Symbol table structs */

typedef struct
{
  Symbol* symbol;
  int scope_id;
  int last_scope_id;
  int nesting_depth;
  int active_scopes[32];
  char label[64];
  int last_label_id;
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
  char* name;
  int block_id;
  int nesting_depth;
  Symbol* next_symbol;

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

/* Symbol table */

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
  add_keyword(symbol_table, "int", Token_If);
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

static int last_label_id; // TODO: got no idea where to stick this

void
make_unique_label(String* label)
{
  sprintf(label->head, "L%d", last_label_id++);
  int len = cstr_len(label->head);
  label->end = label->head + len;
  MemoryArena* arena = label->arena;
  arena->free = label->end + 1;
}

/* Lex */

bool32
token_is_keyword(Token token)
{
  return token > Token__KeywordBegin && token < Token__KeywordEnd;
}

char*
lexeme_install_id(TokenStream* input, char* begin_char, char* end_char)
{
  assert(end_char >= begin_char);

  //TODO: Search the lexeme, and if found, then return it.
  size_t len = end_char - begin_char + 1;
  char* lexeme = mem_push_struct(input->arena, char, len + 1);
  cstr_copy_substr(lexeme, begin_char, end_char);
  return lexeme;
}

char*
lexeme_install_dquot_str(TokenStream* input, char* begin_char, char* end_char)
{
  assert(end_char >= begin_char);
  assert(*begin_char == '"' && *end_char == '"');

  size_t len = (end_char - begin_char + 1) - 2; // minus the quotes
  char* lexeme = mem_push_struct(input->arena, char, len + 1); // +NULL

  char* dest_str = lexeme;
  char* src_str = begin_char+1;
  for(uint i = 0; i < len; i++)
  {
    *dest_str++ = *src_str++;
  }
  cstr_copy_substr(lexeme, begin_char+1, end_char-1);
  return lexeme;
}

void
token_stream_init(TokenStream* token_stream, MemoryArena* arena, char* text, char* file_path)
{
  token_stream->arena = arena;
  token_stream->text = text;
  token_stream->cursor = token_stream->text;
  token_stream->line_nr = 1;
  //TODO: Compute the absolute path to the file, so that Vim could properly
  // jump from the QuickFix window to the error line in the file.
  token_stream->file_path = file_path;
}

void
consume_token(TokenStream* input, SymbolTable* symbol_table)
{
  input->prev_token = input->token;
  input->token = Token__Null;
  mem_zero(&input->lexeme);

  input->src_line = input->cursor;
  char c;

loop:
  c = *input->cursor;
  while(c == ' ' || c == '\t' ||
        c == '\r' || c == '\n')
  {
    if(c == '\n')
    {
      input->line_nr++;
      input->src_line = input->cursor;
    }
    c = *(++input->cursor);
  }

  if(char_is_letter(c) || c == '_')
  {
    char* begin_char = input->cursor;
    c = *(++input->cursor);

    while(char_is_letter(c) || char_is_numeric(c) || c == '_')
      c = *(++input->cursor);

    char* end_char = input->cursor - 1;
    char* lexeme = lexeme_install_id(input, begin_char, end_char);
    input->lexeme.str = lexeme;

    Symbol* symbol = symbol_lookup(symbol_table, lexeme);
    if(symbol && symbol->kind == SymbolKind_Keyword)
      input->token = symbol->kw_token;
    else
      input->token = Token_Id;
  }
  else if(char_is_numeric(c))
  {
    int num = c - '0';
    c = *(++input->cursor);

    while(char_is_numeric(c))
    {
      num = (10 * num) + (c - '0');
      c = *(++input->cursor);
    }

    int* value = mem_push_struct(input->arena, int, 1);
    *value = num;
    input->token = Token_IntNum;
    input->lexeme.int_num = value;
  }
  else if(c == '-')
  {
    c = *(++input->cursor);
    if(c == '>')
    {
      input->token = Token_RightArrow;
      ++input->cursor;
    }
    else if(input->prev_token == Token_Equals ||
            input->prev_token == Token_OpenParens ||
            input->prev_token == Token_Star ||
            input->prev_token == Token_Plus ||
            input->prev_token == Token_Comma ||
            input->prev_token == Token_FwdSlash ||
            input->prev_token == Token_Return)
      input->token = Token_UnaryMinus;
    else
      input->token = Token_Minus;
  }
  else if(c == '/')
  {
    char* fwd_cursor = input->cursor;

    c = *(++fwd_cursor);
    if(c == '*')
    {
      c = *(++fwd_cursor);

      while(true)
      {
        while(c != '*' && c != '\0')
          c = *(++fwd_cursor);
        if(c == '*')
        {
          c = *(++fwd_cursor);
          if(c == '/')
            break;
        } else if(c == '\0')
          break;
      }
      input->cursor = ++fwd_cursor;
      goto loop;
    } else
    {
      input->token = Token_FwdSlash;
      ++input->cursor;
    }
  }
  else if(c == '"')
  {
    char* fwd_cursor = input->cursor;

    c = *(++fwd_cursor);
    while(c != '"' && c != '\0')
      c = *(++fwd_cursor);

    if(c == '"')
    {
      char* lexeme = lexeme_install_dquot_str(input, input->cursor, fwd_cursor);
      input->lexeme.str = lexeme;
      input->token = Token_String;
      input->cursor = ++fwd_cursor;
    } else
      syntax_error(input, "Missing closing '\"'\n");
  }
  else if(c == '=')
  {
    char* fwd_cursor = input->cursor;
    c = *(++fwd_cursor);
    if(c == '=')
    {
      input->token = Token_EqualsEquals;
      input->cursor = ++fwd_cursor;
    } else
    {
      input->token = Token_Equals;
      ++input->cursor;
    }
  }
  else if(c == '<')
  {
    char* fwd_cursor = input->cursor;
    c = *(++fwd_cursor);
    if(c == '=')
    {
      input->token = Token_AngleLeftEquals;
      input->cursor = ++fwd_cursor;
    } else
    {
      input->token = Token_AngleLeft;
      ++input->cursor;
    }
  }
  else if(c == '>')
  {
    char* fwd_cursor = input->cursor;
    c = *(++fwd_cursor);
    if(c == '=')
    {
      input->token = Token_AngleRightEquals;
      input->cursor = ++fwd_cursor;
    } else
    {
      input->token = Token_AngleRight;
      ++input->cursor;
    }
  }
  else if(c == '&')
  {
    char* fwd_cursor = input->cursor;
    c = *(++fwd_cursor);
    if(c == '&')
    {
      input->token = Token_AmprsndAmprsnd;
      input->cursor = ++fwd_cursor;
    }/* else
    {
      input->token = Token_Amprsnd;
      ++input->cursor;
    }*/
  }
  else if(c == '|')
  {
    char* fwd_cursor = input->cursor;
    c = *(++fwd_cursor);
    if(c == '|')
    {
      input->token = Token_PipePipe;
      input->cursor = ++fwd_cursor;
    }/* else
    {
      input->token = Token_Pipe;
      ++input->cursor;
    }*/
  }
  else if(c == '!')
  {
    char* fwd_cursor = input->cursor;
    c = *(++fwd_cursor);
    if(c == '=')
    {
      input->token = Token_BangEquals;
      input->cursor = ++fwd_cursor;
    } else
    {
      input->token = Token_Bang;
      ++input->cursor;
    }
  }
  else if(c == '%')
  {
    input->token = Token_Percent;
    ++input->cursor;
  }
  else if(c == '\\')
  {
    input->token = Token_BackSlash;
    ++input->cursor;
  }
  else if(c == '*')
  {
    input->token = Token_Star;
    ++input->cursor;
  }
  else if(c == '.')
  {
    input->token = Token_Dot;
    ++input->cursor;
  }
  else if(c == '}')
  {
    input->token = Token_CloseBrace;
    ++input->cursor;
  }
  else if(c == '{')
  {
    input->token = Token_OpenBrace;
    ++input->cursor;
  }
  else if(c == '+')
  {
    input->token = Token_Plus;
    ++input->cursor;
  }
  else if(c == '(')
  {
    input->token = Token_OpenParens;
    ++input->cursor;
  }
  else if(c == ')')
  {
    input->token = Token_CloseParens;
    ++input->cursor;
  }
  else if(c == ';')
  {
    input->token = Token_Semicolon;
    ++input->cursor;
  }
  else if(c == ',')
  {
    input->token = Token_Comma;
    ++input->cursor;
  }
  else if(c == ':')
  {
    input->token = Token_Colon;
    ++input->cursor;
  }
  else if(c == '[')
  {
    input->token = Token_OpenParens;
    ++input->cursor;
  }
  else if(c == ']')
  {
    input->token = Token_CloseBracket;
    ++input->cursor;
  }
  else if(c == '^')
  {
    input->token = Token_UpArrow;
    ++input->cursor;
  }
  else if(c == '\0')
    input->token = Token_EndOfInput;
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
void block_build_nodes(MemoryArena*, List*);

inline AstModule*
ast_new_module(MemoryArena* arena)
{
  AstModule* module = mem_push_struct(arena, AstModule, 1);
  list_init(&module->proc_list);
  module->node.kind = AstNodeKind_Module;
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
  block->node.kind = AstNodeKind_Block;
  return block;
}

inline AstCall*
ast_new_call(MemoryArena* arena)
{
  AstCall* call = mem_push_struct(arena, AstCall, 1);
  list_init(&call->actual_args);
  call->node.kind = AstNodeKind_Call;
  return call;
}

inline AstProc*
ast_new_proc(MemoryArena* arena)
{
  AstProc* proc = mem_push_struct(arena, AstProc, 1);
  list_init(&proc->formal_args);
  proc->node.kind = AstNodeKind_Proc;
  return proc;
}

inline AstBinExpr*
ast_new_bin_expr(MemoryArena* arena)
{
  AstBinExpr* bin_expr = mem_push_struct(arena, AstBinExpr, 1);
  bin_expr->node.kind = AstNodeKind_BinExpr;
  return bin_expr;
}

inline AstUnrExpr*
ast_new_unr_expr(MemoryArena* arena)
{
  AstUnrExpr* unr_expr = mem_push_struct(arena, AstUnrExpr, 1);
  unr_expr->node.kind = AstNodeKind_UnrExpr;
  return unr_expr;
}

inline AstIntNum*
ast_new_int_num(MemoryArena* arena)
{
  AstIntNum* int_num = mem_push_struct(arena, AstIntNum, 1);
  int_num->node.kind = AstNodeKind_IntNum;
  return int_num;
}

inline AstVarDecl*
ast_new_var_decl(MemoryArena* arena)
{
  AstVarDecl* var_decl = mem_push_struct(arena, AstVarDecl, 1);
  var_decl->node.kind = AstNodeKind_VarDecl;
  return var_decl;
}

inline AstVarOccur*
ast_new_var_occur(MemoryArena* arena)
{
  AstVarOccur* var_occur = mem_push_struct(arena, AstVarOccur, 1);
  var_occur->node.kind = AstNodeKind_VarOccur;
  return var_occur;
}

inline AstWhileStmt*
ast_new_while_stmt(MemoryArena* arena)
{
  AstWhileStmt* while_stmt = mem_push_struct(arena, AstWhileStmt, 1);
  while_stmt->node.kind = AstNodeKind_WhileStmt;
  return while_stmt;
}

inline AstIfStmt*
ast_new_if_stmt(MemoryArena* arena)
{
  AstIfStmt* if_stmt = mem_push_struct(arena, AstIfStmt, 1);
  if_stmt->node.kind = AstNodeKind_IfStmt;
  return if_stmt;
}

inline AstReturnStmt*
ast_new_return_stmt(MemoryArena* arena)
{
  AstReturnStmt* ret_stmt = mem_push_struct(arena, AstReturnStmt, 1);
  ret_stmt->node.kind = AstNodeKind_ReturnStmt;
  return ret_stmt;
}

inline AstBreakStmt*
ast_new_break_stmt(MemoryArena* arena)
{
  AstBreakStmt* break_stmt = mem_push_struct(arena, AstBreakStmt, 1);
  break_stmt->node.kind = AstNodeKind_BreakStmt;
  return break_stmt;
}

inline AstPrintStmt*
ast_new_print_stmt(MemoryArena* arena)
{
  AstPrintStmt* print_stmt = mem_push_struct(arena, AstPrintStmt, 1);
  print_stmt->node.kind = AstNodeKind_PrintStmt;
  return print_stmt;
}

inline AstIncludeStmt*
ast_new_include_stmt(MemoryArena* arena)
{
  AstIncludeStmt* inc_stmt = mem_push_struct(arena, AstIncludeStmt, 1);
  inc_stmt->node.kind = AstNodeKind_IncludeStmt;
  return inc_stmt;
}

inline AstEmptyStmt*
ast_new_empty_stmt(MemoryArena* arena)
{
  AstNode* empty = mem_push_struct(arena, AstNode, 1);
  empty->kind = AstNodeKind_EmptyStmt;
  return empty;
}

void
block_init(AstBlock* block, SymbolTable* symbol_table)
{
  block->block_id = symbol_table->scope_id;
  block->nesting_depth = symbol_table->nesting_depth;
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
        if(left_node->kind == AstNodeKind_VarOccur)
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
    if(input->token == Token_Id)
    {
      Symbol* symbol = 0;
      success = symbol_register_new(symbol_table, input, &symbol, SymbolKind_Var);
      if(success && symbol)
      {
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
    if(input->token == Token_Id)
    {
      Symbol* symbol = 0;
      success = symbol_register_new(symbol_table, input, &symbol, SymbolKind_Var);
      if(success && symbol)
      {
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
    assert(arg_node->kind == AstNodeKind_VarDecl);
    list_append(arena, &proc->formal_args, (AstVarDecl*)arg_node);

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
              if(body_node->kind != AstNodeKind_VarDecl)
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
    if(input->token == Token_Id)
    {
      Symbol* symbol = 0;
      success = symbol_register_new(symbol_table, input, &symbol, SymbolKind_Proc);
      if(success && symbol)
      {
        //AstProc* proc = &proc_node->proc;
        proc->symbol = symbol;
        proc->name = symbol->name;
        proc->ret_var.data.size = 1;
        symbol->proc = proc;

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
          assert(node->kind == AstNodeKind_IncludeStmt);
          AstIncludeStmt* inc_stmt = (AstIncludeStmt*)node;

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
      if(ret_expr)
      {
        AstReturnStmt* ret_stmt = ast_new_return_stmt(arena);
        *node = &ret_stmt->node;

        int depth = 0;
        AstBlock* block = enclosing_block;
        while(block)
        {
          AstNode* owner = block->owner;
          if(owner->kind == AstNodeKind_Proc)
            break;
          depth++;
          block = block->enclosing_block;
        }
        if(block)
        {
          AstNode* owner_node = block->owner;
          assert(owner_node->kind == AstNodeKind_Proc);
          AstProc* ret_proc = (AstProc*)owner_node;
          ret_stmt->proc = ret_proc;
          ret_stmt->depth = depth;

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

          ret_stmt->expr = &assgn_expr->node;
        } else {
          syntax_error(input, "'return' : enclosing procedure not found");
          success = false;
        }
      } else {
        syntax_error(input, "Expression required after 'return'");
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
      if(owner->kind == AstNodeKind_WhileStmt)
        break;
      depth++;
      block = block->enclosing_block;
    }
    if(block)
    {
      AstNode* owner_node = block->owner;
      assert(owner_node->kind == AstNodeKind_WhileStmt);
      AstWhileStmt* while_stmt = (AstWhileStmt*)owner_node;
      break_stmt->while_stmt = while_stmt;
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

              if(stmt_node->kind == AstNodeKind_BinExpr)
              {
                AstBinExpr* expr = (AstBinExpr*)stmt_node;
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
    if(stmt_node->kind == AstNodeKind_VarDecl)
    {
      AstVarDecl* var_decl = (AstVarDecl*)stmt_node;
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
compute_activation_record_loc(List* pre_fp_data, List* post_fp_data)
{
  int fp = compute_data_loc(0, pre_fp_data);
  compute_data_loc(fp, post_fp_data);
  fixup_data_loc(fp, pre_fp_data);
  fixup_data_loc(fp, post_fp_data);
}

void
block_compute_activation_record(MemoryArena* arena, AstBlock* block)
{
  List pre_fp_data = {0};
  list_init(&pre_fp_data);
  List post_fp_data = {0};
  list_init(&post_fp_data);

  {/* compute the access links */
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

  {/* locals*/
    ListItem* node_item = list_first_item(&block->decl_vars);
    while(node_item)
    {
      AstVarDecl* local = node_item->elem;

      list_append(arena, &post_fp_data, &local->data);
      block->locals_size += local->data.size;

      node_item = node_item->next;
    }
  }

  compute_activation_record_loc(&pre_fp_data, &post_fp_data);
}

void
proc_compute_activation_record(MemoryArena* arena, AstProc* proc)
{
  List pre_fp_data = {0};
  list_init(&pre_fp_data);
  List post_fp_data = {0};
  list_init(&post_fp_data);

  AstVarDecl* ret_var = &proc->ret_var;
  list_append(arena, &pre_fp_data, &ret_var->data);
  proc->ret_size = ret_var->data.size;

  {/* formals */
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

  {/* locals */
    ListItem* node_item = list_first_item(&proc->body->decl_vars);
    while(node_item)
    {
      AstVarDecl* local = node_item->elem;

      list_append(arena, &post_fp_data, &local->data);
      proc->locals_size += local->data.size;

      node_item = node_item->next;
    }
  }

  compute_activation_record_loc(&pre_fp_data, &post_fp_data);
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
  block_compute_activation_record(arena, block);
  block_build_nodes(arena, &block->stmt_list);
}

void
build_while_stmt(MemoryArena* arena, AstWhileStmt* while_stmt)
{
  build_node(arena, while_stmt->expr);

  {/* labels */
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
    build_block(arena, (AstBlock*)while_stmt->body);
  else
    build_node(arena, while_stmt->body);
}

void
build_if_stmt(MemoryArena* arena, AstIfStmt* if_stmt)
{
  build_node(arena, if_stmt->expr);

  {/* labels */
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
  {
    build_block(arena, (AstBlock*)if_stmt->body);
  }
  else
    build_node(arena, if_stmt->body);

  if(if_stmt->else_body)
  {
    AstNode* else_node = if_stmt->else_body;
    if(else_node->kind == AstNodeKind_Block)
      build_block(arena, (AstBlock*)else_node);
    else if(else_node->kind == AstNodeKind_IfStmt)
      build_if_stmt(arena, (AstIfStmt*)else_node);
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
  if(node->kind == AstNodeKind_BinExpr)
  {
    build_bin_expr(arena, (AstBinExpr*)node);
  }
  else if(node->kind == AstNodeKind_UnrExpr)
  {
    build_unr_expr(arena, (AstUnrExpr*)node);
  }
  else if(node->kind == AstNodeKind_Call)
  {
    build_call(arena, (AstCall*)node);
  }
  else if(node->kind == AstNodeKind_IfStmt)
  {
    build_if_stmt(arena, (AstIfStmt*)node);
  }
  else if(node->kind == AstNodeKind_WhileStmt)
  {
    build_while_stmt(arena, (AstWhileStmt*)node);
  }
  else if(node->kind == AstNodeKind_PrintStmt)
  {
    build_print_stmt(arena, (AstPrintStmt*)node);
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
block_build_nodes(MemoryArena* arena, List* stmt_list)
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

  proc_compute_activation_record(arena, proc);
  block_build_nodes(arena, &proc->body->stmt_list);
}
 
void
build_module(MemoryArena* arena, AstModule* module)
{
  ListItem* proc_item = list_first_item(&module->proc_list);
  while(proc_item)
  {
    AstNode* proc_node = proc_item->elem;
    assert(proc_node->kind == AstNodeKind_Proc);
    AstProc* proc = (AstProc*)proc_node;
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

    assert(bin_expr->left_operand->kind = AstNodeKind_VarOccur);
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
  if(node->kind == AstNodeKind_VarOccur)
  {
    gen_load_lvalue(arena, code, (AstVarOccur*)node);
    emit_instr(arena, code, Opcode_LOAD);
  }
  else if(node->kind == AstNodeKind_Call)
  {
    gen_call(arena, code, (AstCall*)node);
  }
  else if(node->kind == AstNodeKind_IntNum)
  {
    emit_instr_int(arena, code, Opcode_PUSH, ((AstIntNum*)node)->value);
  }
  else if(node->kind == AstNodeKind_BinExpr)
  {
    gen_bin_expr(arena, code, (AstBinExpr*)node);
  }
  else if(node->kind == AstNodeKind_UnrExpr)
  {
    gen_unr_expr(arena, code, (AstUnrExpr*)node);
  }
  else
    assert(false);
}

void
gen_return_stmt(MemoryArena* arena, List* code, AstReturnStmt* ret_stmt)
{
  assert(ret_stmt->expr->kind == AstNodeKind_BinExpr);

  gen_bin_expr(arena, code, (AstBinExpr*)ret_stmt->expr);
  emit_instr(arena, code, Opcode_POP);

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

  if(if_stmt->body->kind == AstNodeKind_Block)
    gen_block(arena, code, (AstBlock*)if_stmt->body);
  else
    gen_statement(arena, code, if_stmt->body);

  emit_instr_str(arena, code, Opcode_GOTO, if_stmt->label_end);

  if(if_stmt->else_body)
  {
    emit_instr_str(arena, code, Opcode_LABEL, if_stmt->label_else);
    AstNode* else_body = if_stmt->else_body;
    if(else_body->kind == AstNodeKind_Block)
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
  if(while_stmt->body->kind == AstNodeKind_Block)
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
  if(stmt_node->kind == AstNodeKind_BinExpr)
  {
    AstBinExpr* bin_expr = (AstBinExpr*)stmt_node;
    assert(bin_expr->op == AstOpKind_Assign);
    gen_bin_expr(arena, code, bin_expr);
    emit_instr(arena, code, Opcode_POP);
  }
  else if(stmt_node->kind == AstNodeKind_Call)
  {
    gen_call(arena, code, (AstCall*)stmt_node);
    emit_instr(arena, code, Opcode_POP);
  }
  else if(stmt_node->kind == AstNodeKind_ReturnStmt)
  {
    gen_return_stmt(arena, code, (AstReturnStmt*)stmt_node);
  }
  else if(stmt_node->kind == AstNodeKind_BreakStmt)
  {
    gen_break_stmt(arena, code, (AstBreakStmt*)stmt_node);
  }
  else if(stmt_node->kind == AstNodeKind_Noop)
  {
    emit_instr(arena, code, Opcode_NOOP);
  }
  else if(stmt_node->kind == AstNodeKind_IfStmt)
  {
    gen_if_stmt(arena, code, (AstIfStmt*)stmt_node);
  }
  else if(stmt_node->kind == AstNodeKind_WhileStmt)
  {
    gen_while_stmt(arena, code, (AstWhileStmt*)stmt_node);
  }
  else if(stmt_node->kind == AstNodeKind_PrintStmt)
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
    assert(node->kind == AstNodeKind_Proc);
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

VmProgram* translate_hoc(MemoryArena* arena, char* file_path, char* hoc_text)
{
  bool32 success = false;

  SymbolTable symbol_table = {0};
  symbol_table.arena = arena;

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

    assert(node->kind == AstNodeKind_Module);
    AstModule* module = (AstModule*)node;
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
  return vm_program;
}

