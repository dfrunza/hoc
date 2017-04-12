#include "lib.c"

/* Lex structs */

typedef enum
{/*>>>*/
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
  Token_Import,
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
  Token_AngleLeft,
  Token_AngleRight,
  Token_AngleLeftEquals,
  Token_AngleRightEquals,
  Token_Amprsnd,
  Token_AmprsndAmprsnd,
  Token_Pipe,
  Token_PipePipe,
  Token_OpenParens,
  Token_CloseParens,
  Token_OpenBrace,
  Token_CloseBrace,
  Token_String,
}/*<<<*/
Token;

typedef struct
{/*>>>*/
  Token prev_token;
  Token token;
  char* text;
  char* cursor;
  MemoryArena* arena;

  char* file_path;
  int   line_nr;
  char* src_line;

  union {
    int*  int_num;
    char* str;
  } lexeme;
}/*<<<*/
TokenStream;

/* AST structs */

typedef struct Symbol_ Symbol;
typedef struct AstNode_ AstNode;
typedef struct Block_ AstBlock;
typedef struct AccessLink_ AccessLink;
typedef struct IrWhileStmt_ IrWhileStmt;
typedef struct IrProc_ IrProc;

typedef enum
{/*>>>*/
  AstOpKind__Null,

  AstOpKind_Add,
  AstOpKind_Sub,
  AstOpKind_Div,
  AstOpKind_Mul,
  AstOpKind_Mod,
  AstOpKind_Neg,

  AstOpKind_Call,
  AstOpKind_Assign,
  
  AstOpKind_Equals,
  AstOpKind_NotEquals,
  AstOpKind_Less,
  AstOpKind_Greatr,
  AstOpKind_LogicAnd,
  AstOpKind_LogicOr,
  AstOpKind_LogicNot,
}/*<<<*/
AstOpKind;

typedef enum
{/*>>>*/
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
  AstNodeKind_Import,
  AstNodeKind_EmptyStmt,
  AstNodeKind_Module,
}/*<<<*/
AstNodeKind;

typedef struct
{/*>>>*/
  List     proc_list; // of AstNode type
  AstNode* body;
}/*<<<*/
AstModule;

typedef struct
{/*>>>*/
  int loc;
  int size;
}/*<<<*/
DataArea;

typedef struct
{/*>>>*/
  Symbol*     symbol;
  char*       name;
  DataArea var_data;
  AstNode*    init_expr;
}/*<<<*/
AstVarDecl;

typedef struct
{/*>>>*/
  AstOpKind op;
  AstNode* left_operand;
  AstNode* right_operand;
}/*<<<*/
AstBinExpr;

typedef struct
{/*>>>*/
  AstOpKind op;
  AstNode* operand;
}/*<<<*/
AstUnrExpr;

typedef struct
{/*>>>*/
  Symbol*       symbol;
  char*         name;
  int           decl_block_offset;
  AstVarDecl*   var_decl;
  AccessLink* link;
}/*<<<*/
AstVarOccur;

typedef struct
{/*>>>*/
  int32 value;
}/*<<<*/
AstIntNum;

typedef struct
{/*>>>*/
  Symbol*   symbol;
  char*     name;
  List      formal_args;
  AstBlock* body;
  IrProc*   ir_proc;
  AstVarDecl ret_var;
}/*<<<*/
AstProc;

typedef struct
{/*>>>*/
  AstNode* expr;
  int      block_count;
  AstProc* proc;
  AstNode* ret_var;
}/*<<<*/
AstReturnStmt;

typedef struct
{/*>>>*/
  Symbol*  symbol;
  char*    name;
  List     actual_args;
  AstProc* proc;
}/*<<<*/
AstCall;

typedef struct
{/*>>>*/
  AstNode* expr;
  AstNode* body;
  AstNode* else_body;
}/*<<<*/
AstIfStmt;

typedef struct
{/*>>>*/
  AstNode* expr;
  AstNode* body;
  IrWhileStmt* ir_while;
}/*<<<*/
AstWhileStmt;

typedef struct
{/*>>>*/
  AstWhileStmt* while_stmt;
  int block_count;
}/*<<<*/
AstBreakStmt;

typedef struct
{
  AstNode* expr;
  bool32 new_line;
}
AstPrintStmt;

typedef struct
{/*>>>*/
  char* file_path;
}/*<<<*/
AstImport;

typedef struct Block_
{/*>>>*/
  AstNode* owner;
  int      block_id;
  int      nesting_depth;
  List     decl_vars;
  List     local_occurs;
  List     non_local_occurs;
  List     stmt_list;
  struct   Block_* enclosing_block;
}/*<<<*/
AstBlock;

typedef struct AstNode_
{/*>>>*/
  AstNodeKind  kind;

  union {
    AstBinExpr    bin_expr;
    AstUnrExpr    unr_expr;
    AstVarDecl    var_decl;
    AstVarOccur   var_occur;
    AstIntNum     int_num;
    AstCall       call;
    AstProc       proc;
    AstModule     module;
    AstBlock      block;
    AstImport     import;
    AstReturnStmt ret_stmt;
    AstWhileStmt  while_stmt;
    AstBreakStmt  break_stmt;
    AstPrintStmt  print_stmt;
    AstIfStmt     if_stmt;
  };
}/*<<<*/
AstNode;

/* IR structs */

typedef struct IrNode_ IrNode;
typedef struct IrValue_ IrValue;

typedef enum
{/*>>>*/
  IrNodeKind__Null,
  IrNodeKind_Value,
  IrNodeKind_BinExpr,
  IrNodeKind_UnrExpr,
  IrNodeKind_Proc,
  IrNodeKind_Block,
  IrNodeKind_Module,
  IrNodeKind_Call,
  IrNodeKind_ReturnStmt,
  IrNodeKind_BreakStmt,
  IrNodeKind_PrintStmt,
  IrNodeKind_IfStmt,
  IrNodeKind_WhileStmt,
  IrNodeKind_Noop,
}/*<<<*/
IrNodeKind;

typedef enum
{/*>>>*/
  IrOpKind__Null,

  IrOpKind_Add,
  IrOpKind_Mul,
  IrOpKind_Sub,
  IrOpKind_Div,
  IrOpKind_Mod,
  IrOpKind_Neg,

  IrOpKind_Store,

  IrOpKind_CmpEq,
  IrOpKind_CmpNEq,
  IrOpKind_CmpLss,
  IrOpKind_CmpGrt,
  IrOpKind_And,
  IrOpKind_Or,
  IrOpKind_Not,
}/*<<<*/
IrOpKind;

typedef struct AccessLink_
{/*>>>*/
  int actv_rec_offset;
  int loc; // within the current actv. rec.
}/*<<<*/
AccessLink;

typedef struct IrVar_
{/*>>>*/
  DataArea*  data;
  AccessLink* link;
}/*<<<*/
IrVar;

typedef enum
{/*>>>*/
  IrValueKind__Null,
  IrValueKind_Var,
  IrValueKind_IntNum,
  IrValueKind_Call,
  IrValueKind_BinExpr,
  IrValueKind_UnrExpr,
}/*<<<*/
IrValueKind;

typedef struct
{/*>>>*/
  IrProc* proc;
  List    actual_args;
}/*<<<*/
IrCall;

typedef struct
{/*>>>*/
  IrOpKind op;
  IrValue* left_operand;
  IrValue* right_operand;
}/*<<<*/
IrBinExpr;

typedef struct
{/*>>>*/
  IrOpKind op;
  IrValue* operand;
}/*<<<*/
IrUnrExpr;

typedef struct
{/*>>>*/
  IrBinExpr* assgn_expr;
  IrVar      ret_var;
  IrProc*    proc;
  int        depth;
}/*<<<*/
IrReturnStmt;

typedef struct
{/*>>>*/
  IrValue* value;
  bool32 new_line;
}/*<<<*/
IrPrintStmt;

typedef enum
{/*>>>*/
  IrAvRecord__Null,
  IrAvRecord_Proc,
  IrAvRecord_Block,
}/*<<<*/
IrAvRecordKind;

typedef struct
{/*>>>*/
  DataArea access_links;
  DataArea old_fp;
}/*<<<*/
IrBlockAvRecord;

typedef struct
{/*>>>*/
  DataArea ret;
  DataArea args;
  DataArea ctrl_links;
}/*<<<*/
IrProcAvRecord;

typedef struct
{/*>>>*/
  IrValue* expr;
  IrNode*  body;
  IrNode*  else_body;
  char*    label_else;
  char*    label_end;
}/*<<<*/
IrIfStmt;

typedef struct IrWhileStmt_
{/*>>>*/
  IrValue* expr;
  IrNode* body;
  char* label_eval;
  char* label_break;
}/*<<<*/
IrWhileStmt;

typedef struct
{/*>>>*/
  IrWhileStmt* while_stmt;
  int depth;
}/*<<<*/
IrBreakStmt;

typedef struct IrValue_
{/*>>>*/
  IrValueKind kind;

  union {
    int32      int_num;
    IrVar*     var;
    IrCall*    call;
    IrBinExpr* bin_expr;
    IrUnrExpr* unr_expr;
  };
}/*<<<*/
IrValue;

typedef struct
{/*>>>*/
  union {
    IrProcAvRecord  proc;
    IrBlockAvRecord block;
  };
  DataArea locals;
  IrAvRecordKind kind;
  int fp;
  int sp;
}/*<<<*/
IrActivationRecord;

typedef struct IrProc_
{/*>>>*/
  char* label;
  char* label_end;
  List  instr_list;
  IrActivationRecord* actv_rec;
}/*<<<*/
IrProc;

typedef struct IrBlock_
{/*>>>*/
  List access_links;
  List instr_list;
  IrActivationRecord* actv_rec;
}/*<<<*/
IrBlock;

typedef struct
{/*>>>*/
  IrProc* main_proc;
  IrCall* main_call;
  List    proc_list;
}/*<<<*/
IrModule;

typedef struct IrNode_
{/*>>>*/
  IrNodeKind kind;

  union {
    IrVar        var;
    IrValue      value;
    IrBinExpr    bin_expr;
    IrUnrExpr    unr_expr;
    IrProc       proc;
    IrBlock      block;
    IrModule     module;
    IrCall       call;
    IrReturnStmt ret;
    IrIfStmt     if_stmt;
    IrWhileStmt  while_stmt;
    IrBreakStmt  break_stmt;
    IrPrintStmt  print_stmt;
  };
}/*<<<*/
IrNode;

/* Symbol table structs */

typedef struct
{/*>>>*/
  Symbol* symbol;
  int     scope_id;
  int     last_scope_id;
  int     nesting_depth;
  int     active_scopes[32];
  char    label[64];
  int     last_label_id;
  MemoryArena* arena;
}/*<<<*/
SymbolTable;

typedef enum
{/*>>>*/
  SymbolKind__Null,
  SymbolKind_Keyword,
  SymbolKind_Proc,
  SymbolKind_Var,
}/*<<<*/
SymbolKind;

typedef struct Symbol_
{/*>>>*/
  SymbolKind kind;
  char*      name;
  int        block_id;
  int        nesting_depth;
  Symbol*    next_symbol;

  union {
    AstVarDecl* var;
    AstProc*    proc;
    Token       kw_token;
  };
}/*<<<*/
Symbol;

typedef struct
{/*>>>*/
  String       text;
  int          text_len;
  List         instr_list;
  MemoryArena* arena;
}/*<<<*/
VmProgram;

void
block_init(SymbolTable* symbol_table, AstBlock* block)
{/*>>>*/
  block->block_id = symbol_table->scope_id;
  block->nesting_depth = symbol_table->nesting_depth;

  list_init(&block->local_occurs);
  list_init(&block->non_local_occurs);
  list_init(&block->stmt_list);
  list_init(&block->decl_vars);
}/*<<<*/

void
syntax_error(TokenStream* input, char* message, ...)
{/*>>>*/
  va_list args;

  fprintf(stderr, "%s(%d) : ", input->file_path, input->line_nr);

  va_start(args, message);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
}/*<<<*/

/* Symbol table */

Symbol*
symbol_lookup(SymbolTable* symbol_table, char* name)
{/*>>>*/
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
}/*<<<*/

Symbol*
symbol_add(SymbolTable* symbol_table, char* name, SymbolKind kind)
{/*>>>*/
  Symbol* symbol = mem_push_struct(symbol_table->arena, Symbol, 1);
  symbol->name = name;
  symbol->kind = kind;
  symbol->block_id = symbol_table->scope_id;
  symbol->nesting_depth = symbol_table->nesting_depth;
  symbol->next_symbol = symbol_table->symbol;
  symbol_table->symbol = symbol;
  return symbol;
}/*<<<*/

bool32
symbol_register_new(SymbolTable* symbol_table, TokenStream* input, Symbol** new_symbol, SymbolKind kind)
{/*>>>*/
  assert(input->token == Token_Id);
  bool32 success = true;

  Symbol* symbol = symbol_lookup(symbol_table, input->lexeme.str);
  if(!symbol)
  {
    symbol = symbol_add(symbol_table, input->lexeme.str, kind);
  } else {
    if(symbol->kind != SymbolKind_Keyword)
    {
      if(symbol->block_id != symbol_table->scope_id ||
          symbol->kind != kind)
      {
        assert(symbol->nesting_depth <= symbol_table->nesting_depth);

        symbol = symbol_add(symbol_table, input->lexeme.str, kind);
      } else
      {
        syntax_error(input, "Re-declaraion of identifier: %s", symbol->name);
        success = false;
      }
    } else
    {
      syntax_error(input, "Keyword used as identifier: %s", symbol->name);
      success = false;
    }
  }
  *new_symbol = symbol;
  return success;
}/*<<<*/

Symbol*
add_keyword(SymbolTable* symbol_table, char* name, Token token)
{/*>>>*/
  Symbol* symbol = symbol_add(symbol_table, name, SymbolKind_Keyword);
  symbol->kw_token = token;
  return symbol;
}/*<<<*/

void
register_keywords(SymbolTable* symbol_table)
{/*>>>*/
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
  add_keyword(symbol_table, "import", Token_Import);
  add_keyword(symbol_table, "true", Token_True);
  add_keyword(symbol_table, "false", Token_False);
  add_keyword(symbol_table, "print", Token_Print);
}/*<<<*/

bool32
scope_begin(SymbolTable* symbol_table)
{/*>>>*/
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
}/*<<<*/

void
scope_end(SymbolTable* symbol_table)
{/*>>>*/
  int nesting_depth = --symbol_table->nesting_depth;
  int scope_id = symbol_table->active_scopes[nesting_depth];
  assert(scope_id >= 0);
  symbol_table->scope_id = scope_id;

  Symbol* symbol = symbol_table->symbol;
  while(symbol && symbol->block_id > symbol_table->scope_id)
    symbol = symbol->next_symbol;
  symbol_table->symbol = symbol;
}/*<<<*/

static int last_label_id; // TODO: got no idea where to stick this

void
make_unique_label(String* label)
{/*>>>*/
  sprintf(label->head, "L%d", last_label_id++);
  int len = cstr_len(label->head);
  label->end = label->head + len;
  MemoryArena* arena = label->arena;
  arena->free = label->end + 1;
}/*<<<*/

/* Lex */

bool32
token_is_keyword(Token token)
{/*>>>*/
  return token > Token__KeywordBegin && token < Token__KeywordEnd;
}/*<<<*/

char*
lexeme_install_id(TokenStream* input, char* begin_char, char* end_char)
{/*>>>*/
  assert(end_char >= begin_char);

  //TODO: Search the lexeme, and if found, then return it.
  size_t len = end_char - begin_char + 1;
  char* lexeme = mem_push_struct(input->arena, char, len + 1);
  cstr_copy_substr(lexeme, begin_char, end_char);
  return lexeme;
}/*<<<*/

char*
lexeme_install_dquot_str(TokenStream* input, char* begin_char, char* end_char)
{/*>>>*/
  assert(end_char >= begin_char);
  assert(*begin_char == '"' && *end_char == '"');

  size_t len = (end_char - begin_char + 1) - 2; // minus the quotes
  char* lexeme = mem_push_struct(input->arena, char, len + 1); // +NULL

  char* dest_str = lexeme;
  char* src_str = begin_char+1;
  for(int i = 0; i < len; i++)
  {
    *dest_str++ = *src_str++;
  }
  cstr_copy_substr(lexeme, begin_char+1, end_char-1);
  return lexeme;
}/*<<<*/

void
consume_token(TokenStream* input, SymbolTable* symbol_table)
{/*>>>*/
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
  {/*>>>*/
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
  }/*<<<*/
  else if(char_is_numeric(c))
  {/*>>>*/
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
  }/*<<<*/
  else if(c == '-')
  {/*>>>*/
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
  }/*<<<*/
  else if(c == '/')
  {/*>>>*/
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
  }/*<<<*/
  else if(c == '"')
  {/*>>>*/
    char* fwd_cursor = input->cursor;

    c = *(++fwd_cursor);
    while(c != '"' && c != '\0')
      c = *(++fwd_cursor);

    char* lexeme = lexeme_install_dquot_str(input, input->cursor, fwd_cursor);
    input->lexeme.str = lexeme;
    input->token = Token_String;
    input->cursor = ++fwd_cursor;
  }/*<<<*/
  else if(c == '=')
  {/*>>>*/
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
  }/*<<<*/
  else if(c == '<')
  {/*>>>*/
    /*
    char* fwd_cursor = input->cursor;
    c = *(++fwd_cursor);
    if(c == '=')
    {
      input->token = Token_AngleLeftEquals;
      input->cursor = ++fwd_cursor;
    } else*/
    {
      input->token = Token_AngleLeft;
      ++input->cursor;
    }
  }/*<<<*/
  else if(c == '>')
  {/*>>>*/
    /*
    char* fwd_cursor = input->cursor;
    c = *(++fwd_cursor);
    if(c == '=')
    {
      input->token = Token_AngleRightEquals;
      input->cursor = ++fwd_cursor;
    } else*/
    {
      input->token = Token_AngleRight;
      ++input->cursor;
    }
  }/*<<<*/
  else if(c == '&')
  {/*>>>*/
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
  }/*<<<*/
  else if(c == '|')
  {/*>>>*/
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
  }/*<<<*/
  else if(c == '!')
  {/*>>>*/
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
  }/*<<<*/
  else if(c == '%')
  {/*>>>*/
    input->token = Token_Percent;
    ++input->cursor;
  }/*<<<*/
  else if(c == '\\')
  {/*>>>*/
    input->token = Token_BackSlash;
    ++input->cursor;
  }/*<<<*/
  else if(c == '*')
  {/*>>>*/
    input->token = Token_Star;
    ++input->cursor;
  }/*<<<*/
  else if(c == '.')
  {/*>>>*/
    input->token = Token_Dot;
    ++input->cursor;
  }/*<<<*/
  else if(c == '}')
  {/*>>>*/
    input->token = Token_CloseBrace;
    ++input->cursor;
  }/*<<<*/
  else if(c == '{')
  {/*>>>*/
    input->token = Token_OpenBrace;
    ++input->cursor;
  }/*<<<*/
  else if(c == '+')
  {/*>>>*/
    input->token = Token_Plus;
    ++input->cursor;
  }/*<<<*/
  else if(c == '(')
  {/*>>>*/
    input->token = Token_OpenParens;
    ++input->cursor;
  }/*<<<*/
  else if(c == ')')
  {/*>>>*/
    input->token = Token_CloseParens;
    ++input->cursor;
  }/*<<<*/
  else if(c == ';')
  {/*>>>*/
    input->token = Token_Semicolon;
    ++input->cursor;
  }/*<<<*/
  else if(c == ',')
  {/*>>>*/
    input->token = Token_Comma;
    ++input->cursor;
  }/*<<<*/
  else if(c == ':')
  {/*>>>*/
    input->token = Token_Colon;
    ++input->cursor;
  }/*<<<*/
  else if(c == '[')
  {/*>>>*/
    input->token = Token_OpenParens;
    ++input->cursor;
  }/*<<<*/
  else if(c == ']')
  {/*>>>*/
    input->token = Token_CloseBracket;
    ++input->cursor;
  }/*<<<*/
  else if(c == '^')
  {/*>>>*/
    input->token = Token_UpArrow;
    ++input->cursor;
  }/*<<<*/
  else if(c == '\0')
    input->token = Token_EndOfInput;
}/*<<<*/

/* Parse */

bool32 parse_expression(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstNode**);
bool32 parse_statement_list(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*);
bool32 parse_actual_argument_list(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstCall*);
bool32 parse_term(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstNode**);
bool32 parse_statement(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstNode**);
bool32 parse_if_stmt(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstNode**);
bool32 parse_block(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstNode*, AstNode**);
bool32 parse_module(MemoryArena*, TokenStream*, SymbolTable*, AstNode**);

bool32
parse_factor(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
             AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
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
        AstNode* neg_node = mem_push_struct(arena, AstNode, 1);
        neg_node->kind = AstNodeKind_UnrExpr;
        AstUnrExpr* expr = &neg_node->unr_expr;
        expr->op = AstOpKind_Neg;
        expr->operand = operand;
        *node = neg_node;
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
        AstNode* not_node = mem_push_struct(arena, AstNode, 1);
        not_node->kind = AstNodeKind_UnrExpr;
        AstUnrExpr* expr = &not_node->unr_expr;
        expr->op = AstOpKind_LogicNot;
        expr->operand = operand;
        *node = not_node;
      } else {
        syntax_error(input, "Expression expected after '!'");
        success = false;
      }
    }
  }
  else if(input->token == Token_IntNum)
  {
    AstNode* num_node = mem_push_struct(arena, AstNode, 1);
    num_node->kind = AstNodeKind_IntNum;
    num_node->int_num.value = *(int32*)input->lexeme.int_num;
    *node = num_node;

    consume_token(input, symbol_table);
  }
  else if(input->token == Token_Id)
  {
    Symbol* symbol = symbol_lookup(symbol_table, input->lexeme.str);
    if(symbol)
    {
      AstNode* id_node = mem_push_struct(arena, AstNode, 1);
      *node = id_node;

      consume_token(input, symbol_table);

      if(symbol->kind == SymbolKind_Var)
      {
        id_node->kind = AstNodeKind_VarOccur;
        AstVarOccur* var_occur = &id_node->var_occur;
        var_occur->symbol = symbol;
        var_occur->name = symbol->name;
        var_occur->var_decl = symbol->var;
        var_occur->decl_block_offset = (symbol_table->nesting_depth - symbol->nesting_depth);

        if(var_occur->decl_block_offset > 0)
        {
          list_append(arena, &enclosing_block->non_local_occurs, id_node);
        }
        else
        {
          assert(var_occur->decl_block_offset == 0);
          list_append(arena, &enclosing_block->local_occurs, id_node);
        }
      }
      else if(symbol->kind == SymbolKind_Proc)
      {
        id_node->kind = AstNodeKind_Call;
        AstCall* call = &id_node->call;
        call->symbol = symbol;
        call->name = symbol->name;
        call->proc = symbol->proc;
        list_init(&call->actual_args);

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
    AstNode* num_node = mem_push_struct(arena, AstNode, 1);
    num_node->kind = AstNodeKind_IntNum;
    num_node->int_num.value = (input->token == Token_True ? 1 : 0);
    *node = num_node;

    consume_token(input, symbol_table);
  }

  return success;
}/*<<<*/

bool32
parse_rest_of_factors(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                      AstBlock* enclosing_block, AstNode* left_node, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Star ||
     input->token == Token_FwdSlash ||
     input->token == Token_Percent ||
     input->token == Token_EqualsEquals ||
     input->token == Token_BangEquals ||
     input->token == Token_AmprsndAmprsnd ||
     input->token == Token_PipePipe ||
     input->token == Token_AngleLeft ||
     input->token == Token_AngleRight)
  {
    AstNode* expr_node = mem_push_struct(arena, AstNode, 1);
    expr_node->kind = AstNodeKind_BinExpr;
    AstBinExpr* expr = &expr_node->bin_expr;
    if(input->token == Token_Star)
      expr->op = AstOpKind_Mul;
    else if(input->token == Token_FwdSlash)
      expr->op = AstOpKind_Div;
    else if(input->token == Token_Percent)
      expr->op = AstOpKind_Mod;
    else if(input->token == Token_EqualsEquals)
      expr->op = AstOpKind_Equals;
    else if(input->token == Token_BangEquals)
      expr->op = AstOpKind_NotEquals;
    else if(input->token == Token_AngleLeft)
      expr->op = AstOpKind_Less;
    else if(input->token == Token_AngleRight)
      expr->op = AstOpKind_Greatr;
    else if(input->token == Token_AmprsndAmprsnd)
      expr->op = AstOpKind_LogicAnd;
    else if(input->token == Token_PipePipe)
      expr->op = AstOpKind_LogicOr;
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
                                      enclosing_block, expr_node, node);
    } else {
      syntax_error(input, "Factor expected");
      success = false;
    }
  }
  else
    *node = left_node;

  return success;
}/*<<<*/

bool32
parse_term(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
           AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  AstNode* factor_node = 0;
  AstNode* expr_node = 0;

  bool32 success = parse_factor(arena, input, symbol_table, enclosing_block, &factor_node);
  if(success && factor_node)
    success = parse_rest_of_factors(arena, input, symbol_table,
                                    enclosing_block, factor_node, &expr_node);

  *node = expr_node;
  return success;
}/*<<<*/

bool32
parse_rest_of_terms(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                    AstBlock* enclosing_block, AstNode* left_node, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Plus ||
      input->token == Token_Minus)
  {
    AstNode* expr_node = mem_push_struct(arena, AstNode, 1);
    expr_node->kind = AstNodeKind_BinExpr;
    AstBinExpr* expr = &expr_node->bin_expr;
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
      success = parse_rest_of_terms(arena, input, symbol_table, enclosing_block, expr_node, node);
    } else {
      syntax_error(input, "Expression term expected");
      success = false;
    }
  }
  else
    *node = left_node;

  return success;
}/*<<<*/

bool32
parse_assignment_term(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                      AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  AstNode* term_node = 0;
  AstNode* expr_node = 0;

  bool32 success = parse_term(arena, input, symbol_table, enclosing_block, &term_node);
  if(success && term_node)
    success = parse_rest_of_terms(arena, input, symbol_table, enclosing_block, term_node, &expr_node);

  *node = expr_node;
  return success;
}/*<<<*/

bool32
parse_rest_of_assignment_terms(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                               AstBlock* enclosing_block, AstNode* left_node, AstNode** node)
{/*>>>*/
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
          AstNode* expr_node = mem_push_struct(arena, AstNode, 1);
          expr_node->kind = AstNodeKind_BinExpr;
          AstBinExpr* expr = &expr_node->bin_expr;
          expr->op = AstOpKind_Assign;
          expr->left_operand = left_node;
          expr->right_operand = right_side;

          *node = expr_node;
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
}/*<<<*/

bool32
parse_expression(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                 AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
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
}/*<<<*/

bool32
parse_var_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                    AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Var)
  {
    consume_token(input, symbol_table);
    if(input->token == Token_Id)
    {
      AstNode* var_node = mem_push_struct(arena, AstNode, 1);
      var_node->kind = AstNodeKind_VarDecl;
      *node = var_node;

      Symbol* symbol = 0;
      success = symbol_register_new(symbol_table, input, &symbol, SymbolKind_Var);
      if(success && symbol)
      {
        AstVarDecl* var_decl = &var_node->var_decl;
        var_decl->symbol = symbol;
        var_decl->name = symbol->name;
        var_decl->var_data.size = 1;
        symbol->var = var_decl;

        consume_token(input, symbol_table);
        if(input->token == Token_Equals)
        {
          AstNode* occur_node = mem_push_struct(arena, AstNode, 1);
          occur_node->kind = AstNodeKind_VarOccur;
          AstVarOccur* var_occur = &occur_node->var_occur;
          var_occur->symbol = symbol;
          var_occur->name = symbol->name;
          var_occur->var_decl = var_decl;
          var_occur->decl_block_offset = 0;

          AstNode* init_expr = 0;
          success = parse_rest_of_assignment_terms(arena, input, symbol_table,
              enclosing_block, occur_node, &init_expr);
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
}/*<<<*/

bool32
parse_formal_argument(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                      AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Var)
  {
    consume_token(input, symbol_table);
    if(input->token == Token_Id)
    {
      AstNode* var_node = mem_push_struct(arena, AstNode, 1);
      var_node->kind = AstNodeKind_VarDecl;
      *node = var_node;

      Symbol* symbol = 0;
      success = symbol_register_new(symbol_table, input, &symbol, SymbolKind_Var);
      if(success && symbol)
      {
        AstVarDecl* var_decl = &var_node->var_decl;
        var_decl->symbol = symbol;
        var_decl->name = symbol->name;
        symbol->var = var_decl;

        consume_token(input, symbol_table);
      }
    } else {
      syntax_error(input, "Expecting an identifier token");
      success = false;
    }
  }

  return success;
}/*<<<*/

bool32
parse_formal_argument_list(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                           AstBlock* enclosing_block, AstProc* proc)
{/*>>>*/
  bool32 success = true;

  AstNode* arg_node = 0;
  success = parse_formal_argument(arena, input, symbol_table, enclosing_block, &arg_node);
  if(success && arg_node)
  {
    list_append(arena, &proc->formal_args, arg_node);

    if(input->token == Token_Comma)
    {
      consume_token(input, symbol_table);
      success = parse_formal_argument_list(arena, input, symbol_table, enclosing_block, proc);
    }
  }

  return success;
}/*<<<*/

bool32
parse_actual_argument_list(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                           AstBlock* enclosing_block, AstCall* call)
{/*>>>*/
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
}/*<<<*/

bool32
parse_while_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                 AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_While)
  {
    consume_token(input, symbol_table);

    AstNode* expr_node = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &expr_node);
    if(success)
    {
      AstNode* while_node = mem_push_struct(arena, AstNode, 1);
      while_node->kind = AstNodeKind_WhileStmt;
      AstWhileStmt* while_stmt = &while_node->while_stmt;
      while_stmt->expr = expr_node;
      *node = while_node;

      AstNode* body_node = 0;
      success = parse_block(arena, input, symbol_table, enclosing_block, while_node, &body_node);
      if(success)
      {
        if(body_node)
        {
          while_stmt->body = body_node;
        } else
        {
          success = parse_statement(arena, input, symbol_table, enclosing_block, &body_node);
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

  return success;
}/*<<<*/

bool32
parse_block(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
            AstBlock* enclosing_block, AstNode* owner, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_OpenBrace)
  {
    consume_token(input, symbol_table);

    success = scope_begin(symbol_table);
    if(success)
    {
      AstNode* block_node = mem_push_struct(arena, AstNode, 1);
      block_node->kind = AstNodeKind_Block;
      AstBlock* block = &block_node->block;
      block->owner = owner;
      block->enclosing_block = enclosing_block;
      block_init(symbol_table, block);

      success = parse_statement_list(arena, input, symbol_table, block);
      if(success)
      {
        *node = block_node;

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
}/*<<<*/

bool32
parse_else_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                     AstBlock* enclosing_block, AstNode* owner, AstNode** node)
{/*>>>*/
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
}/*<<<*/

bool32
parse_if_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
              AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_If)
  {
    consume_token(input, symbol_table);
    AstNode* expr_node = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &expr_node);
    if(success)
    {
      AstNode* if_node = mem_push_struct(arena, AstNode, 1);
      if_node->kind = AstNodeKind_IfStmt;
      AstIfStmt* if_stmt = &if_node->if_stmt;
      if_stmt->expr = expr_node;
      *node = if_node;

      AstNode* body_node = 0;
      success = parse_block(arena, input, symbol_table, enclosing_block, if_node, &body_node);
      if(success)
      {
        if(body_node)
        {
          if_stmt->body = body_node;

          AstNode* else_node = 0;
          success = parse_else_statement(arena, input, symbol_table, enclosing_block, if_node, &else_node);
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
                success = parse_else_statement(arena, input, symbol_table, enclosing_block, if_node, &else_node);
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
}/*<<<*/

bool32
parse_procedure(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Proc)
  {
    AstNode* proc_node = mem_push_struct(arena, AstNode, 1);
    proc_node->kind = AstNodeKind_Proc;
    *node = proc_node;

    consume_token(input, symbol_table);
    if(input->token == Token_Id)
    {
      Symbol* symbol = 0;
      success = symbol_register_new(symbol_table, input, &symbol, SymbolKind_Proc);
      if(success && symbol)
      {
        AstProc* proc = &proc_node->proc;
        proc->symbol = symbol;
        proc->name = symbol->name;
        list_init(&proc->formal_args);
        symbol->proc = proc;

        consume_token(input, symbol_table);
        if(input->token == Token_OpenParens)
        {
          consume_token(input, symbol_table);

          // arguments
          success = scope_begin(symbol_table);
          if(success)
          {
            AstBlock* block = mem_push_struct(arena, AstBlock, 1);
            proc->body = block;
            block->owner = proc_node;
            block_init(symbol_table, block);

            success = parse_formal_argument_list(arena, input, symbol_table, block, proc);
            if(success)
            {
              ListItem* arg_item = list_first_item(&proc->formal_args);
              while(arg_item)
              {
                AstNode* node = arg_item->elem;
                assert(node->kind == AstNodeKind_VarDecl);
                AstVarDecl* arg = &node->var_decl;
                Symbol* symbol = node->var_decl.symbol;
                symbol->var = arg;
                arg->var_data.size = 1;

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
}/*<<<*/

bool32
parse_import(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
             AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Import)
  {
    consume_token(input, symbol_table);

    if(input->token == Token_String)
    {
      AstNode* imp_node = mem_push_struct(arena, AstNode, 1);
      imp_node->kind = AstNodeKind_Import;
      *node = imp_node;

      String str = {0};
      str_init(&str, arena);
      str_append(&str, input->file_path);
      path_make_dir(str.head);
      str_tidyup(&str);
      str_append(&str, input->lexeme.str);

      AstImport* ast_imp = &imp_node->import;
      ast_imp->file_path = str.head;

      consume_token(input, symbol_table);
    }
  }
  return success;
}/*<<<*/

bool32
parse_module_definition(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                        AstBlock* enclosing_block, AstModule* module)
{/*>>>*/
  bool32 success = true;

  AstNode* ast_node = 0;
  success = parse_procedure(arena, input, symbol_table, enclosing_block, &ast_node);
  if(success)
  {
    if(ast_node)
    {
      list_append(arena, &module->proc_list, ast_node);
      success = parse_module_definition(arena, input, symbol_table, enclosing_block, module);
    } else
    {
      success = parse_import(arena, input, symbol_table, enclosing_block, &ast_node);
      if(success)
      {
        if(ast_node)
        {
          AstImport* ast_imp = &ast_node->import;

          char* hoc_text = file_read_text(arena, ast_imp->file_path);
          if(hoc_text)
          {
            TokenStream* imp_input = mem_push_struct(arena, TokenStream, 1);
            imp_input->arena = arena;
            imp_input->text = hoc_text;
            imp_input->cursor = imp_input->text;
            imp_input->line_nr = 1;
            imp_input->file_path = ast_imp->file_path;

            consume_token(imp_input, symbol_table);

            success = parse_module_definition(arena, imp_input, symbol_table, enclosing_block, module);
            if(success)
              success = parse_module_definition(arena, input, symbol_table, enclosing_block, module);
          } else
          {
            syntax_error(input, "File could not be read: %s", ast_imp->file_path);
            success = false;
          }
        }
      }
    }
  }

  return success;
}/*<<<*/

bool32
parse_print_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                 AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Print)
  {
    consume_token(input, symbol_table);

    AstNode* expr_node = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &expr_node);
    if(success)
    {
      AstNode* print_node = mem_push_struct(arena, AstNode, 1);
      print_node->kind = AstNodeKind_PrintStmt;
      *node = print_node;
      AstPrintStmt* print_stmt = &print_node->print_stmt;

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
}/*<<<*/

bool32
parse_return_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                  AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Return)
  {
    consume_token(input, symbol_table);

    AstNode* expr_node = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &expr_node);
    if(success)
    {
      if(expr_node)
      {
        AstNode* ret_node = mem_push_struct(arena, AstNode, 1);
        ret_node->kind = AstNodeKind_ReturnStmt;
        AstReturnStmt* ret_stmt = &ret_node->ret_stmt;
        ret_stmt->expr = expr_node;
        *node = ret_node;

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
          AstProc* ret_proc = &block->owner->proc;
          ret_stmt->proc = ret_proc;
          ret_stmt->block_count = depth;

          AstNode* var_node = mem_push_struct(arena, AstNode, 1);
          var_node->kind = AstNodeKind_VarOccur;
          AstVarOccur* var_occur = &var_node->var_occur;
          var_occur->var_decl = &ret_proc->ret_var;
          var_occur->decl_block_offset = depth;
          if(depth > 0)
            list_append(arena, &enclosing_block->non_local_occurs, var_node);
          else
            list_append(arena, &enclosing_block->local_occurs, var_node);
          ret_stmt->ret_var = var_node;
        } else {
          syntax_error(input, "'return' : no enclosing procedure");
          success = false;
        }
      } else {
        syntax_error(input, "Expression required after 'return'");
        success = false;
      }
    }
  }

  return success;
}/*<<<*/

bool32
parse_break_stmt(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                 AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Break)
  {
    consume_token(input, symbol_table);

    AstNode* break_node = mem_push_struct(arena, AstNode, 1);
    break_node->kind = AstNodeKind_BreakStmt;
    AstBreakStmt* break_stmt = &break_node->break_stmt;
    *node = break_node;

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
      AstWhileStmt* while_stmt = &block->owner->while_stmt;
      break_stmt->while_stmt = while_stmt;
      break_stmt->block_count = depth + 1;
    } else {
      syntax_error(input, "'break': no enclosing while statement");
      success = false;
    }
  }
  return success;
}/*<<<*/

bool32
parse_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
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
          stmt_node = mem_push_struct(arena, AstNode, 1);
          stmt_node->kind = AstNodeKind_EmptyStmt;
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
}/*<<<*/

bool32
parse_statement_list(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                     AstBlock* block)
{/*>>>*/
  bool32 success = true;

  while(input->token == Token_Semicolon)
    consume_token(input, symbol_table);

  AstNode* stmt_node = 0;
  success = parse_statement(arena, input, symbol_table, block, &stmt_node);
  if(success && stmt_node)
  {
    if(stmt_node->kind == AstNodeKind_VarDecl)
    {
      list_append(arena, &block->decl_vars, stmt_node);
      AstVarDecl* var_decl = &stmt_node->var_decl;
      if(var_decl->init_expr)
        list_append(arena, &block->stmt_list, var_decl->init_expr);
    }
    else
      list_append(arena, &block->stmt_list, stmt_node);

    success = parse_statement_list(arena, input, symbol_table, block); //FIXME: Is this tail-recursion - can it be optimized?
  }
  return success;
}/*<<<*/

bool32
parse_module(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                    AstNode** node)
{/*>>>*/
  bool32 success = true;

  success = scope_begin(symbol_table);
  if(success)
  {
    AstNode* module_node = mem_push_struct(arena, AstNode, 1);
    module_node->kind = AstNodeKind_Module;

    AstNode* block_node = mem_push_struct(arena, AstNode, 1);
    block_node->kind = AstNodeKind_Block;
    AstBlock* block = &block_node->block;
    block->owner = module_node;
    block_init(symbol_table, block);

    AstModule* module = &module_node->module;
    module->body = block_node;
    list_init(&module->proc_list);

    success = parse_module_definition(arena, input, symbol_table, block, module);
    if(success)
    {
      scope_end(symbol_table);

      if(input->token == Token_EndOfInput)
        *node = module_node;
      else {
        syntax_error(input, "End of file expected");
        success = false;
      }
    }
  }

  return success;
}/*<<<*/

/* IR */

IrNode* ir_build_value(MemoryArena*, IrActivationRecord*, AstNode*);
IrNode* ir_build_statement(MemoryArena*, IrActivationRecord*, AstNode*);
void ir_block_build_statements(MemoryArena*, IrActivationRecord*, List*, List*);

IrNode*
ir_build_bin_expr(MemoryArena* arena, IrActivationRecord* actv_rec, AstBinExpr* bin_expr)
{/*>>>*/
  IrNode* ir_node = mem_push_struct(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_BinExpr;
  IrBinExpr* ir_op = &ir_node->bin_expr;

  AstNode* left_operand = bin_expr->left_operand;
  AstNode* right_operand = bin_expr->right_operand;
  IrNode* val_node = ir_build_value(arena, actv_rec, right_operand);
  ir_op->right_operand = &val_node->value;
  val_node = ir_build_value(arena, actv_rec, left_operand);
  ir_op->left_operand = &val_node->value;

  if(bin_expr->op == AstOpKind_Assign)
  {
    assert(left_operand->kind == AstNodeKind_VarOccur);
    ir_op->op = IrOpKind_Store;
  }
  else if(bin_expr->op == AstOpKind_Add)
    ir_op->op = IrOpKind_Add;
  else if(bin_expr->op == AstOpKind_Sub)
    ir_op->op = IrOpKind_Sub;
  else if(bin_expr->op == AstOpKind_Mul)
    ir_op->op = IrOpKind_Mul;
  else if(bin_expr->op == AstOpKind_Div)
    ir_op->op = IrOpKind_Div;
  else if(bin_expr->op == AstOpKind_Mod)
    ir_op->op = IrOpKind_Mod;
  else if(bin_expr->op == AstOpKind_Equals)
    ir_op->op = IrOpKind_CmpEq;
  else if(bin_expr->op == AstOpKind_NotEquals)
    ir_op->op = IrOpKind_CmpNEq;
  else if(bin_expr->op == AstOpKind_Less)
    ir_op->op = IrOpKind_CmpLss;
  else if(bin_expr->op == AstOpKind_Greatr)
    ir_op->op = IrOpKind_CmpGrt;
  else if(bin_expr->op == AstOpKind_LogicAnd)
    ir_op->op = IrOpKind_And;
  else if(bin_expr->op == AstOpKind_LogicOr)
    ir_op->op = IrOpKind_Or;
  else
    assert(false);

  return ir_node;
}/*<<<*/

IrNode*
ir_build_unr_expr(MemoryArena* arena, IrActivationRecord* actv_rec, AstUnrExpr* unr_expr)
{/*>>>*/
  IrNode* ir_node = mem_push_struct(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_UnrExpr;
  IrUnrExpr* ir_op = &ir_node->unr_expr;
  IrNode* val_node = ir_build_value(arena, actv_rec, unr_expr->operand);
  ir_op->operand = &val_node->value;
  if(unr_expr->op == AstOpKind_Neg)
    ir_op->op = IrOpKind_Neg;
  else if(unr_expr->op == AstOpKind_LogicNot)
    ir_op->op = IrOpKind_Not;
  else
    assert(false);
  return ir_node;
}/*<<<*/

IrNode*
ir_build_call(MemoryArena* arena, IrActivationRecord* actv_rec, AstCall* ast_call)
{/*>>>*/
  IrNode* ir_call_node = mem_push_struct(arena, IrNode, 1);
  ir_call_node->kind = IrNodeKind_Call;
  IrCall* ir_call = &ir_call_node->call;
  IrProc* ir_proc = ast_call->proc->ir_proc;
  ir_call->proc = ir_proc;

  list_init(&ir_call->actual_args);
  ListItem* arg_item = list_first_item(&ast_call->actual_args);
  while(arg_item)
  {
    IrNode* val_node = ir_build_value(arena, actv_rec, (AstNode*)arg_item->elem);
    list_append(arena, &ir_call->actual_args, &val_node->value);
    arg_item = arg_item->next;
  }

  return ir_call_node;
}/*<<<*/

IrNode*
ir_build_value(MemoryArena* arena, IrActivationRecord* actv_rec, AstNode* ast_node)
{/*>>>*/
  IrNode* ir_node = mem_push_struct(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_Value;
  if(ast_node->kind == AstNodeKind_VarOccur)
  {
    AstVarOccur* var_occur = &ast_node->var_occur;
    AstVarDecl* var_decl = var_occur->var_decl;
    IrVar* var = mem_push_struct(arena, IrVar, 1);
    var->data = &var_decl->var_data;
    var->link = var_occur->link;
    ir_node->value.kind = IrValueKind_Var;
    ir_node->value.var = var;
  }
  else if(ast_node->kind == AstNodeKind_IntNum)
  {
    ir_node->value.kind = IrValueKind_IntNum;
    ir_node->value.int_num = ast_node->int_num.value;
  }
  else if(ast_node->kind == AstNodeKind_BinExpr)
  {
    ir_node->value.kind = IrValueKind_BinExpr;
    IrNode* op_node = ir_build_bin_expr(arena, actv_rec, &ast_node->bin_expr);
    ir_node->value.bin_expr = &op_node->bin_expr;
  }
  else if(ast_node->kind == AstNodeKind_UnrExpr)
  {
    ir_node->value.kind = IrValueKind_UnrExpr;
    IrNode* op_node = ir_build_unr_expr(arena, actv_rec, &ast_node->unr_expr);
    ir_node->value.unr_expr = &op_node->unr_expr;
  }
  else if(ast_node->kind == AstNodeKind_Call)
  {
    ir_node->value.kind = IrValueKind_Call;
    IrNode* call_node = ir_build_call(arena, actv_rec, &ast_node->call);
    ir_node->value.call = &call_node->call;
  }
  else
    assert(false);
  return ir_node;
}/*<<<*/

IrNode*
ir_build_print_stmt(MemoryArena* arena, IrActivationRecord* actv_rec, AstPrintStmt* ast_print)
{/*>>>*/
  IrNode* ir_node = mem_push_struct(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_PrintStmt;
  IrPrintStmt* ir_print = &ir_node->print_stmt;
  if(ast_print->expr) {
    ir_print->value = &ir_build_value(arena, actv_rec, ast_print->expr)->value;
  }
  ir_print->new_line = ast_print->new_line;
  return ir_node;
}/*<<<*/

IrNode*
ir_build_return_stmt(MemoryArena* arena, IrActivationRecord* actv_rec, AstReturnStmt* ast_ret)
{/*>>>*/
  IrNode* ir_node = mem_push_struct(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_ReturnStmt;
  IrReturnStmt* ir_ret = &ir_node->ret;

  IrBinExpr* assgn_expr = mem_push_struct(arena, IrBinExpr, 1);
  assgn_expr->op = IrOpKind_Store;

  assert(ast_ret->ret_var->kind == AstNodeKind_VarOccur);
  AstVarOccur* var_occur = &ast_ret->ret_var->var_occur;
  AstVarDecl* var_decl = var_occur->var_decl;
  IrVar* ret_var = &ir_ret->ret_var;
  ret_var->data = &var_decl->var_data;
  ret_var->link = var_occur->link;
  IrValue* left_operand = mem_push_struct(arena, IrValue, 1);
  left_operand->kind = IrValueKind_Var;
  left_operand->var = &ir_ret->ret_var;
  assgn_expr->left_operand = left_operand;

  IrNode* ret_val = ir_build_value(arena, actv_rec, ast_ret->expr);
  assgn_expr->right_operand = &ret_val->value;
  ir_ret->assgn_expr = assgn_expr;

  ir_ret->proc = ast_ret->proc->ir_proc;
  ir_ret->depth = ast_ret->block_count;

  return ir_node;
}/*<<<*/

IrNode*
ir_build_break_stmt(MemoryArena* arena, IrActivationRecord* actv_rec, AstBreakStmt* ast_break)
{/*>>>*/
  IrNode* ir_node = mem_push_struct(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_BreakStmt;
  IrBreakStmt* ir_break = &ir_node->break_stmt;
  AstWhileStmt* ast_while = ast_break->while_stmt;
  ir_break->while_stmt = ast_while->ir_while;
  ir_break->depth = ast_break->block_count;
  return ir_node;
}/*<<<*/

void
ir_block_decl_vars_compute_address(IrActivationRecord* actv_rec, DataArea* locals_area, List* decl_vars)
{/*>>>*/
  locals_area->loc = actv_rec->sp;
  {/* locals*/
    ListItem* node_item = list_first_item(decl_vars);
    while(node_item)
    {
      AstNode* decl_node = node_item->elem;
      assert(decl_node->kind == AstNodeKind_VarDecl);
      AstVarDecl* var_decl = &decl_node->var_decl;
      DataArea* var_data = &var_decl->var_data;

      var_data->loc = actv_rec->sp - actv_rec->fp;
      actv_rec->sp += var_data->size;

      node_item = node_item->next;
    }
  }
  locals_area->size = actv_rec->sp - locals_area->loc;
}/*<<<*/

IrNode*
ir_build_block(MemoryArena* arena, AstBlock* ast_block)
{/*>>>*/
  IrNode* ir_node = mem_push_struct(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_Block;
  IrBlock* ir_block = &ir_node->block;
  IrActivationRecord* actv_rec = mem_push_struct(arena, IrActivationRecord, 1);
  actv_rec->kind = IrAvRecord_Block;
  ir_block->actv_rec = actv_rec;
  IrBlockAvRecord* block_av = &actv_rec->block;
  list_init(&ir_block->instr_list);
  list_init(&ir_block->access_links);

  DataArea* area = &block_av->access_links;
  area->loc = actv_rec->sp;
  {/*>>> non-locals */
    ListItem* occur_item = list_first_item(&ast_block->non_local_occurs);
    while(occur_item)
    {
      AstNode* node = occur_item->elem;
      AstVarOccur* var_occur = &node->var_occur;
      List* links_list = &ir_block->access_links;

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
        link->loc = actv_rec->sp++; // size of link = 1
        list_append(arena, links_list, link);
      }
      var_occur->link = link;

      occur_item = occur_item->next;
    }
  }/*<<<*/
  area->size = actv_rec->sp - area->loc;

  area = &block_av->old_fp;
  area->loc = actv_rec->sp;
  area->size = 1; // fp
  actv_rec->sp += area->size;

  actv_rec->fp = actv_rec->sp;

  {/*>>> correct the offsets of access links */
    if(ir_block->access_links.count > 0)
    {
      ListItem* link_item = list_first_item(&ir_block->access_links);
      while(link_item)
      {
        AccessLink* link = link_item->elem;
        link->loc = -(actv_rec->fp - link->loc);

        link_item = link_item->next;
      }
    }
  }/*<<<*/

  // correct the offset of old_fp
  block_av->old_fp.loc = actv_rec->fp - block_av->old_fp.loc;

  ir_block_decl_vars_compute_address(actv_rec, &actv_rec->locals, &ast_block->decl_vars);

  ir_block_build_statements(arena, actv_rec, &ir_block->instr_list, &ast_block->stmt_list);

  return ir_node;
}/*<<<*/

IrNode*
ir_build_while_stmt(MemoryArena* arena, IrActivationRecord* actv_rec, AstWhileStmt* ast_while)
{/*>>>*/
  IrNode* ir_node = mem_push_struct(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_WhileStmt;
  IrWhileStmt* ir_while = &ir_node->while_stmt;
  ast_while->ir_while = ir_while;
  ir_while->expr = &ir_build_value(arena, actv_rec, ast_while->expr)->value;

  {/*>>> labels */
    String label_id = {0};
    str_init(&label_id, arena);
    make_unique_label(&label_id);

    String label = {0};
    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".while-expr");
    ir_while->label_eval = label.head;

    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".while-break");
    ir_while->label_break = label.head;
  }/*<<<*/

  if(ast_while->body->kind == AstNodeKind_Block)
  {
    AstBlock* ast_block = &ast_while->body->block;
    IrNode* ir_block = ir_build_block(arena, ast_block);
    ir_while->body = ir_block;
  }
  else
    ir_while->body = ir_build_statement(arena, actv_rec, ast_while->body);

  return ir_node;
}/*<<<*/

IrNode*
ir_build_if_stmt(MemoryArena* arena, IrActivationRecord* actv_rec, AstIfStmt* ast_if)
{/*>>>*/
  IrNode* ir_node = mem_push_struct(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_IfStmt;
  IrIfStmt* ir_if = &ir_node->if_stmt;
  ir_if->expr = &ir_build_value(arena, actv_rec, ast_if->expr)->value;

  {/*>>> labels */
    String label_id = {0};
    str_init(&label_id, arena);
    make_unique_label(&label_id);

    String label = {0};
    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".if-else");
    ir_if->label_else = label.head;

    str_init(&label, arena);
    str_append(&label, label_id.head);
    str_append(&label, ".if-end");
    ir_if->label_end = label.head;
  }/*<<<*/

  if(ast_if->body->kind == AstNodeKind_Block)
  {
    AstBlock* ast_block = &ast_if->body->block;
    IrNode* ir_block = ir_build_block(arena, ast_block);
    ir_if->body = ir_block;
  }
  else
    ir_if->body = ir_build_statement(arena, actv_rec, ast_if->body);

  if(ast_if->else_body)
  {
    AstNode* else_node = ast_if->else_body;
    if(else_node->kind == AstNodeKind_Block)
    {
      IrNode* ir_block = ir_build_block(arena, &else_node->block);
      ir_if->else_body = ir_block;
    }
    else if(else_node->kind == AstNodeKind_IfStmt)
    {
      IrNode* ir_stmt = ir_build_if_stmt(arena, actv_rec, &else_node->if_stmt);
      ir_if->else_body = ir_stmt;
    }
    else
      ir_if->else_body = ir_build_statement(arena, actv_rec, else_node);
  }

  return ir_node;
}/*<<<*/

IrNode*
ir_build_noop(MemoryArena* arena)
{/*>>>*/
  IrNode* ir_node = mem_push_struct(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_Noop;
  return ir_node;
}/*<<<*/

IrNode*
ir_build_statement(MemoryArena* arena, IrActivationRecord* actv_rec, AstNode* ast_node)
{/*>>>*/
  IrNode* ir_node = 0;
  if(ast_node->kind == AstNodeKind_BinExpr)
  {
    AstBinExpr* bin_expr = &ast_node->bin_expr;
    assert(bin_expr->op == AstOpKind_Assign);
    ir_node = ir_build_bin_expr(arena, actv_rec, bin_expr);
  }
  else if(ast_node->kind == AstNodeKind_Call)
    ir_node = ir_build_call(arena, actv_rec, &ast_node->call);
  else if(ast_node->kind == AstNodeKind_ReturnStmt)
    ir_node = ir_build_return_stmt(arena, actv_rec, &ast_node->ret_stmt);
  else if(ast_node->kind == AstNodeKind_IfStmt)
    ir_node = ir_build_if_stmt(arena, actv_rec, &ast_node->if_stmt);
  else if(ast_node->kind == AstNodeKind_WhileStmt)
    ir_node = ir_build_while_stmt(arena, actv_rec, &ast_node->while_stmt);
  else if(ast_node->kind == AstNodeKind_BreakStmt)
    ir_node = ir_build_break_stmt(arena, actv_rec, &ast_node->break_stmt);
  else if(ast_node->kind == AstNodeKind_PrintStmt)
    ir_node = ir_build_print_stmt(arena, actv_rec, &ast_node->print_stmt);
  else if(ast_node->kind == AstNodeKind_EmptyStmt)
    ir_node = ir_build_noop(arena);
  else
    assert(false);

  return ir_node;
}/*<<<*/

void
ir_block_build_statements(MemoryArena* arena, IrActivationRecord* actv_rec,
                          List* instr_list, List* ast_stmt_list)
{/*>>>*/
  ListItem* ast_node_item = list_first_item(ast_stmt_list);
  while(ast_node_item)
  {
    IrNode* ir_node = ir_build_statement(arena, actv_rec, (AstNode*)ast_node_item->elem);
    list_append(arena, instr_list, ir_node);
    ast_node_item = ast_node_item->next;
  }
}/*<<<*/

IrNode*
ir_build_proc(MemoryArena* arena, AstProc* ast_proc)
{/*>>>*/
  AstBlock* ast_block = ast_proc->body;

  IrActivationRecord* actv_rec = mem_push_struct(arena, IrActivationRecord, 1);
  actv_rec->kind = IrAvRecord_Proc;
  IrProcAvRecord* proc_av = &actv_rec->proc;
  actv_rec->sp = 0;

  DataArea* area = &proc_av->ret;
  area->loc = actv_rec->sp;
  {/*>>> ret val*/
    AstVarDecl* var_decl = &ast_proc->ret_var;
    DataArea* var_data = &var_decl->var_data;
    var_data->loc = actv_rec->sp;
    var_data->size = 1;
    actv_rec->sp += var_data->size;
  }/*<<<*/
  area->size = actv_rec->sp - area->loc;

  area = &proc_av->args;
  area->loc = actv_rec->sp;
  {/*>>> formal args*/
    ListItem* node_item = list_first_item(&ast_proc->formal_args);
    while(node_item)
    {
      AstNode* decl_node = node_item->elem;
      assert(decl_node->kind == AstNodeKind_VarDecl);
      AstVarDecl* var_decl = &decl_node->var_decl;
      DataArea* arg_var_data = &var_decl->var_data;

      arg_var_data->loc = actv_rec->sp;
      actv_rec->sp += arg_var_data->size;

      node_item = node_item->next;
    }
  }/*<<<*/
  area->size = actv_rec->sp - area->loc;

  area = &proc_av->ctrl_links;
  area->loc = actv_rec->sp;
  area->size = 3; // ip+sp+fp
  actv_rec->sp += area->size;

  actv_rec->fp = actv_rec->sp;

  ir_block_decl_vars_compute_address(actv_rec, &actv_rec->locals, &ast_block->decl_vars);

  {/*>>> These vars have negative locations : retval and arguments */
    DataArea* var_data = &ast_proc->ret_var.var_data;
    var_data->loc = -(actv_rec->fp - var_data->loc);

    ListItem* node_item = list_first_item(&ast_proc->formal_args);
    while(node_item)
    {
      AstNode* decl_node = node_item->elem;
      assert(decl_node->kind == AstNodeKind_VarDecl);
      AstVarDecl* var_decl = &decl_node->var_decl;
      DataArea* var_data = &var_decl->var_data;

      var_data->loc = -(actv_rec->fp - var_data->loc);

      node_item = node_item->next;
    }
  }/*<<<*/

  IrNode* ir_node = mem_push_struct(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_Proc;
  IrProc* ir_proc = &ir_node->proc;
  ir_proc->actv_rec = actv_rec;
  ir_proc->label = ast_proc->name;
  list_init(&ir_proc->instr_list);

  String label = {0};
  str_init(&label, arena);
  str_append(&label, ast_proc->name);
  str_append(&label, ".proc-end");
  ir_proc->label_end = label.head;
  ast_proc->ir_proc = ir_proc; // must be set before building the statement list

  ir_block_build_statements(arena, actv_rec, &ir_proc->instr_list, &ast_block->stmt_list);

  return ir_node;
}/*<<<*/
 
IrNode*
ir_build_module(MemoryArena* arena, AstModule* module)
{/*>>>*/
  IrNode* ir_module_node = mem_push_struct(arena, IrNode, 1);
  ir_module_node->kind = IrNodeKind_Module;
  IrModule* ir_module = &ir_module_node->module;
  list_init(&ir_module->proc_list);

  ListItem* proc_item = list_first_item(&module->proc_list);
  while(proc_item)
  {
    AstNode* ast_proc_node = proc_item->elem;
    assert(ast_proc_node->kind == AstNodeKind_Proc);
    IrNode* ir_proc_node = ir_build_proc(arena, &ast_proc_node->proc);
    ast_proc_node->proc.ir_proc = &ir_proc_node->proc;
    list_append(arena, &ir_module->proc_list, ir_proc_node);
    if(cstr_match(ast_proc_node->proc.name, "main"))
      ir_module->main_proc = &ir_proc_node->proc;

    proc_item = proc_item->next;
  }

  if(ir_module->main_proc)
  {
    IrNode* ir_call_node = mem_push_struct(arena, IrNode, 1);
    ir_call_node->kind = IrNodeKind_Call;
    ir_call_node->call.proc = ir_module->main_proc;
    list_init(&ir_call_node->call.actual_args);
    ir_module->main_call = &ir_call_node->call;
  } else
  {
    error("Missing main() procedure");
    return 0;
  }

  return ir_module_node;
}/*<<<*/

/* Code gen */

void
print_instruction(VmProgram* vm_program, char* code, ...)
{/*>>>*/
  static char strbuf[128] = {0};
  va_list args;

  va_start(args, code);
  vm_program->text_len += vsprintf(strbuf, code, args);
  va_end(args);

  str_append(&vm_program->text, strbuf);
  str_append(&vm_program->text, "\n");
  vm_program->text_len++;
}/*<<<*/

void
emit_instr_reg(MemoryArena* arena, List* instr_list, Opcode opcode, RegName reg)
{/*>>>*/
  Instruction* instr = mem_push_struct(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType_Reg;
  instr->param.reg = reg;
  list_append(arena, instr_list, instr);
}/*<<<*/

void
emit_instr_int(MemoryArena* arena, List* instr_list, Opcode opcode, int32 int_num)
{/*>>>*/
  Instruction* instr = mem_push_struct(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType_Int32;
  instr->param.int_num = int_num;
  list_append(arena, instr_list, instr);
}/*<<<*/

void
emit_instr(MemoryArena* arena, List* instr_list, Opcode opcode)
{/*>>>*/
  Instruction* instr = mem_push_struct(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType__Null;
  list_append(arena, instr_list, instr);
}/*<<<*/

void
emit_instr_str(MemoryArena* arena, List* instr_list, Opcode opcode, char* str)
{/*>>>*/
  Instruction* instr = mem_push_struct(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType_String;
  instr->param.str = str;
  list_append(arena, instr_list, instr);
}/*<<<*/

void gen_load_rvalue(MemoryArena*, List*, IrValue*);
void gen_load_lvalue(MemoryArena*, List*, IrValue*);
void gen_statement(MemoryArena*, List*, IrNode*);

void
gen_bin_expr(MemoryArena* arena, List* code, IrBinExpr* bin_expr)
{/*>>>*/
  if(bin_expr->op == IrOpKind_Store)
  {
    gen_load_rvalue(arena, code, bin_expr->right_operand);
    gen_load_lvalue(arena, code, bin_expr->left_operand);

    emit_instr(arena, code, Opcode_STORE);
  }
  else
  {
    gen_load_rvalue(arena, code, bin_expr->left_operand);
    gen_load_rvalue(arena, code, bin_expr->right_operand);

    if(bin_expr->op == IrOpKind_Add)
      emit_instr(arena, code, Opcode_ADD);
    else if(bin_expr->op == IrOpKind_Sub)
      emit_instr(arena, code, Opcode_SUB);
    else if(bin_expr->op == IrOpKind_Mul)
      emit_instr(arena, code, Opcode_MUL);
    else if(bin_expr->op == IrOpKind_Div)
      emit_instr(arena, code, Opcode_DIV);
    else if(bin_expr->op == IrOpKind_Mod)
      emit_instr(arena, code, Opcode_MOD);
    else if(bin_expr->op == IrOpKind_CmpEq)
      emit_instr(arena, code, Opcode_CMPEQ);
    else if(bin_expr->op == IrOpKind_CmpNEq)
      emit_instr(arena, code, Opcode_CMPNEQ);
    else if(bin_expr->op == IrOpKind_CmpLss)
      emit_instr(arena, code, Opcode_CMPLSS);
    else if(bin_expr->op == IrOpKind_CmpGrt)
      emit_instr(arena, code, Opcode_CMPGRT);
    else if(bin_expr->op == IrOpKind_And)
      emit_instr(arena, code, Opcode_AND);
    else if(bin_expr->op == IrOpKind_Or)
      emit_instr(arena, code, Opcode_OR);
    else
      assert(false);
  }
}/*<<<*/

void gen_unr_expr(MemoryArena* arena, List* code, IrUnrExpr* unr_expr)
{/*>>>*/
  gen_load_rvalue(arena, code, unr_expr->operand);
  if(unr_expr->op == IrOpKind_Neg)
  {
    // emulated instruction
    emit_instr_int(arena, code, Opcode_PUSH, -1);
    emit_instr(arena, code, Opcode_MUL);
  }
  else if(unr_expr->op == IrOpKind_Not)
  {
    emit_instr(arena, code, Opcode_NOT);
  }
  else
    assert(false);
}/*<<<*/

void
gen_call(MemoryArena* arena, List* code, IrCall* call)
{/*>>>*/
  IrProc* proc = call->proc;
  IrActivationRecord* actv_rec = proc->actv_rec;
  assert(actv_rec->kind == IrAvRecord_Proc);
  IrProcAvRecord* proc_actv_rec = &actv_rec->proc;
  if(proc_actv_rec->ret.size > 0)
    emit_instr_int(arena, code, Opcode_ALLOC, proc_actv_rec->ret.size);

  ListItem* arg_item = list_first_item(&call->actual_args);
  while(arg_item)
  {
    IrValue* arg_val = arg_item->elem;
    gen_load_rvalue(arena, code, arg_val);
    arg_item = arg_item->next;
  }

  emit_instr_str(arena, code, Opcode_CALL, proc->label);
  if(proc_actv_rec->args.size > 0)
    emit_instr_int(arena, code, Opcode_POP, proc_actv_rec->args.size); // discard args
}/*<<<*/

void
gen_load_lvalue(MemoryArena* arena, List* code, IrValue* ir_value)
{/*>>>*/
  if(ir_value->kind == IrValueKind_Var)
  {
    DataArea* data = ir_value->var->data;
    AccessLink* link = ir_value->var->link;

    emit_instr_reg(arena, code, Opcode_PUSH, RegName_FP);
    if(link) 
    {
      // this is a non-local
      assert(link->loc < 0); // relative to FP
      emit_instr_int(arena, code, Opcode_PUSH, link->loc);
      emit_instr(arena, code, Opcode_ADD);
      emit_instr(arena, code, Opcode_LOAD); // access link is on the stack now
    }
    emit_instr_int(arena, code, Opcode_PUSH, data->loc);
    emit_instr(arena, code, Opcode_ADD);
  } else
    assert(false);
}/*<<<*/

void
gen_load_rvalue(MemoryArena* arena, List* code, IrValue* ir_value)
{/*>>>*/
  if(ir_value->kind == IrValueKind_Var)
  {
    gen_load_lvalue(arena, code, ir_value);
    emit_instr(arena, code, Opcode_LOAD);
  }
  else if(ir_value->kind == IrValueKind_Call)
  {
    gen_call(arena, code, ir_value->call);
  }
  else if(ir_value->kind == IrValueKind_IntNum)
  {
    emit_instr_int(arena, code, Opcode_PUSH, ir_value->int_num);
  }
  else if(ir_value->kind == IrValueKind_BinExpr)
  {
    gen_bin_expr(arena, code, ir_value->bin_expr);
  }
  else if(ir_value->kind == IrValueKind_UnrExpr)
  {
    gen_unr_expr(arena, code, ir_value->unr_expr);
  }
  else
    assert(false);
}/*<<<*/

void
gen_return_stmt(MemoryArena* arena, List* code, IrReturnStmt* ir_ret)
{/*>>>*/
  gen_bin_expr(arena, code, ir_ret->assgn_expr);
  emit_instr(arena, code, Opcode_POP);

  IrProc* proc = ir_ret->proc;
  int depth = ir_ret->depth;
  while(depth--)
    emit_instr(arena, code, Opcode_LEAVE);
  emit_instr_str(arena, code, Opcode_GOTO, proc->label_end);
}/*<<<*/

void
gen_break_stmt(MemoryArena* arena, List* code, IrBreakStmt* ir_break)
{/*>>>*/
  IrWhileStmt* ir_while = ir_break->while_stmt;
  int depth = ir_break->depth;
  while(depth--)
    emit_instr(arena, code, Opcode_LEAVE);
  emit_instr_str(arena, code, Opcode_GOTO, ir_while->label_break);
}/*<<<*/

void
gen_block(MemoryArena* arena, List* code, IrBlock* block)
{/*>>>*/
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

  IrActivationRecord* actv_rec = block->actv_rec;
  DataArea* locals_area = &actv_rec->locals;
  if(locals_area->size > 0)
    emit_instr_int(arena, code, Opcode_ALLOC, locals_area->size);

  ListItem* item = list_first_item(&block->instr_list);
  while(item)
  {
    IrNode* ir_node = item->elem;
    gen_statement(arena, code, ir_node);
    item = item->next;
  }

  emit_instr(arena, code, Opcode_LEAVE);
}/*<<<*/

void
gen_proc(MemoryArena* arena, List* code, IrProc* proc)
{/*>>>*/
  emit_instr_str(arena, code, Opcode_LABEL, proc->label);
  IrActivationRecord* actv_rec = proc->actv_rec;
  DataArea* locals_area = &actv_rec->locals;
  if(locals_area->size > 0)
    emit_instr_int(arena, code, Opcode_ALLOC, locals_area->size);

  ListItem* item = list_first_item(&proc->instr_list);
  while(item)
  {
    IrNode* ir_node = item->elem;
    gen_statement(arena, code, ir_node);
    item = item->next;
  }

  emit_instr_str(arena, code, Opcode_LABEL, proc->label_end);
  emit_instr(arena, code, Opcode_RETURN);
}/*<<<*/

void
gen_statement(MemoryArena* arena, List* code, IrNode* stmt_node)
{/*>>>*/
  if(stmt_node->kind == IrNodeKind_BinExpr)
  {
    assert(stmt_node->bin_expr.op == IrOpKind_Store);
    gen_bin_expr(arena, code, &stmt_node->bin_expr);
    emit_instr(arena, code, Opcode_POP);
  }
  else if(stmt_node->kind == IrNodeKind_Call)
  {
    gen_call(arena, code, &stmt_node->call);
    emit_instr(arena, code, Opcode_POP);
  }
  else if(stmt_node->kind == IrNodeKind_ReturnStmt)
  {
    gen_return_stmt(arena, code, &stmt_node->ret);
  }
  else if(stmt_node->kind == IrNodeKind_BreakStmt)
  {
    gen_break_stmt(arena, code, &stmt_node->break_stmt);
  }
  else if(stmt_node->kind == IrNodeKind_Noop)
  {
    emit_instr(arena, code, Opcode_NOOP);
  }
  else if(stmt_node->kind == IrNodeKind_IfStmt)
  {
    IrIfStmt* ir_if = &stmt_node->if_stmt;

    gen_load_rvalue(arena, code, ir_if->expr);
    if(ir_if->else_body)
      emit_instr_str(arena, code, Opcode_JUMPZ, ir_if->label_else);
    else
      emit_instr_str(arena, code, Opcode_JUMPZ, ir_if->label_end);

    if(ir_if->body->kind == IrNodeKind_Block)
      gen_block(arena, code, &ir_if->body->block);
    else
      gen_statement(arena, code, ir_if->body);

    emit_instr_str(arena, code, Opcode_GOTO, ir_if->label_end);

    if(ir_if->else_body)
    {
      emit_instr_str(arena, code, Opcode_LABEL, ir_if->label_else);
      IrNode* else_body = ir_if->else_body;
      if(else_body->kind == IrNodeKind_Block)
        gen_block(arena, code, &else_body->block);
      else
        gen_statement(arena, code, else_body);
    }
    emit_instr_str(arena, code, Opcode_LABEL, ir_if->label_end);
  }
  else if(stmt_node->kind == IrNodeKind_WhileStmt)
  {
    IrWhileStmt* ir_while = &stmt_node->while_stmt;

    emit_instr_str(arena, code, Opcode_LABEL, ir_while->label_eval);
    gen_load_rvalue(arena, code, ir_while->expr);
    emit_instr_str(arena, code, Opcode_JUMPZ, ir_while->label_break);
    if(ir_while->body->kind == IrNodeKind_Block)
      gen_block(arena, code, &ir_while->body->block);
    else
      gen_statement(arena, code, ir_while->body);
    emit_instr_str(arena, code, Opcode_GOTO, ir_while->label_eval);

    emit_instr_str(arena, code, Opcode_LABEL, ir_while->label_break);
  }
  else if(stmt_node->kind == IrNodeKind_PrintStmt)
  {
    IrPrintStmt* ir_print = &stmt_node->print_stmt;
    if(ir_print->value)
    {
      gen_load_rvalue(arena, code, ir_print->value);
      emit_instr(arena, code, Opcode_PRINT);
    }
    if(ir_print->new_line)
      emit_instr(arena, code, Opcode_PRINTNL);
  }
  else
    assert(false);
}/*<<<*/

void
gen_module(MemoryArena* arena, List* code, IrModule* module)
{/*>>>*/
  gen_call(arena, code, module->main_call);
  emit_instr(arena, code, Opcode_HALT);

  ListItem* item = list_first_item(&module->proc_list);
  while(item)
  {
    IrNode* ir_node = item->elem;
    gen_proc(arena, code, &ir_node->proc);
    item = item->next;
  }
}/*<<<*/

char*
get_regname_str(RegName reg)
{/*>>>*/
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
}/*<<<*/

void
print_code(VmProgram* vm_program)
{/*>>>*/
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
}/*<<<*/

bool32
translate_hoc(MemoryArena* arena, char* file_path, char* hoc_text, VmProgram* vm_program)
{/*>>>*/
  bool32 success = false;

  SymbolTable symbol_table = {0};
  symbol_table.arena = arena;

  TokenStream token_stream = {0};
  token_stream.arena = arena;
  token_stream.text = hoc_text;
  token_stream.cursor = token_stream.text;
  token_stream.line_nr = 1;
  //TODO: Compute the absolute path to the file, so that Vim could properly
  // jump from the QuickFix window to the error line in the file.
  token_stream.file_path = file_path;

  register_keywords(&symbol_table);
  consume_token(&token_stream, &symbol_table);

  AstNode* ast_node = 0;
  success = parse_module(arena, &token_stream, &symbol_table, &ast_node);

  if(success)
  {
    assert(ast_node->kind == AstNodeKind_Module);
    assert(symbol_table.scope_id == 0);
    assert(symbol_table.nesting_depth == 0);

    IrNode* ir_module_node = ir_build_module(arena, &ast_node->module);
    if(ir_module_node)
    {
      list_init(&vm_program->instr_list);
      gen_module(arena, &vm_program->instr_list, &ir_module_node->module);

      str_init(&vm_program->text, arena);
      print_code(vm_program);
    }
    else
      success = false;
  }

  return success;
}/*<<<*/

