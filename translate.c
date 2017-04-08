#include "lib.c"

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
  Token_True,
  Token_False,
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
  Token_Star,
  Token_FwdSlash,
  Token_Plus,
  Token_Minus,
  Token_UnaryMinus,
  Token_Equals,
  Token_OpenParens,
  Token_CloseParens,
  Token_OpenBrace,
  Token_CloseBrace,
}
Token;/*<<<*/

typedef struct
{
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
    char* id;
  } lexval;
}
TokenStream;

typedef struct Symbol_ Symbol;
typedef struct AstNode_ AstNode;
typedef struct Block_ AstBlock;

typedef struct
{
  Symbol* symbol;
  int     scope_id;
  int     last_scope_id;
  int     nesting_depth;
  int     active_scopes[32];
  char    label[64];
  int     last_label_id;
  MemoryArena* arena;
}
SymbolTable;

typedef enum
{
  AstOperatorKind__Null,
  AstOperatorKind_Add,
  AstOperatorKind_Sub,
  AstOperatorKind_Assign,
  AstOperatorKind_Div,
  AstOperatorKind_Mul,
  AstOperatorKind_Call,
  AstOperatorKind_Neg,
}
AstOperatorKind;

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
  AstNodeKind_Module,
}
AstNodeKind;

typedef struct
{
  List     proc_list;
  AstNode* body;
}
AstModule;

typedef struct
{
  int actv_rec_offset;
  int loc; // within the current actv. rec.
}
IrAccessLink;

typedef struct
{
  int loc;
  int size;
}
IrStackArea;

typedef struct IrVar_
{
  IrStackArea*  data;
  IrAccessLink* link;
}
IrVar;

typedef struct
{
  Symbol*     symbol;
  char*       name;
  IrStackArea var_data;
  AstNode*    init_expr;
}
AstVarDecl;

typedef struct
{
  AstOperatorKind op;
  AstNode* left_operand;
  AstNode* right_operand;
}
AstBinExpr;

typedef struct
{
  AstOperatorKind op;
  AstNode* operand;
}
AstUnrExpr;

typedef struct
{
  Symbol*       symbol;
  char*         name;
  int           decl_block_offset;
  AstVarDecl*   var_decl;
  IrAccessLink* link;
}
AstVarOccur;

typedef struct
{
  int32 value;
}
AstIntNum;

typedef struct IrProc_ IrProc;
typedef struct
{
  Symbol*   symbol;
  char*     name;
  List      formal_args;
  AstBlock* body;
  IrProc*   ir_proc;
  AstVarDecl ret_var;
}
AstProc;

typedef struct
{
  AstNode* expr;
  int      intervening_block_count;
  AstProc* proc;
  AstNode* ret_var;
}
AstReturnStmt;

typedef struct
{
  Symbol*  symbol;
  char*    name;
  List     actual_args;
  AstProc* proc;
}
AstCall;

typedef struct
{
  AstNode* expr;
  AstNode* body;
  AstNode* else_body;
}
AstIfStmt;

typedef struct
{
  AstNode* expr;
  AstNode* body;
}
AstWhileStmt;

typedef struct Block_
{
  AstNode* owner;
  int      block_id;
  int      nesting_depth;
  List     decl_vars;
  List     local_occurs;
  List     non_local_occurs;
  List     stmt_list;
  struct   Block_* enclosing_block;
}
AstBlock;

typedef struct AstNode_
{
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
    AstReturnStmt ret_stmt;
    AstWhileStmt  while_stmt;
    AstIfStmt     if_stmt;
  };
}
AstNode;

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
  char*      name;
  int        block_id;
  int        nesting_depth;
  Symbol*    next_symbol;

  union {
    AstVarDecl* var;
    AstProc*    proc;
    Token       kw_token;
  };
}
Symbol;

typedef struct
{
  String       text;
  int          text_len;
  List         instr_list;
  MemoryArena* arena;
}
VmProgram;

void block_init(SymbolTable* symbol_table, AstBlock* block)
{/*>>>*/
  block->block_id = symbol_table->scope_id;
  block->nesting_depth = symbol_table->nesting_depth;

  list_init(&block->local_occurs);
  list_init(&block->non_local_occurs);
  list_init(&block->stmt_list);
  list_init(&block->decl_vars);
}/*<<<*/

void syntax_error(TokenStream* input, char* message, ...)
{
  va_list args;

  fprintf(stderr, "%s(%d) : ", input->file_path, input->line_nr);

  va_start(args, message);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
}

/*>>> Symbol table */
Symbol* symbol_lookup(SymbolTable* symbol_table, char* name)
{/*>>>*/
  Symbol* result = 0;

  Symbol* symbol = symbol_table->symbol;
  while(symbol)
  {
    if(str_match(symbol->name, name))
    {
      result = symbol;
      break;
    }
    symbol = symbol->next_symbol;
  }
  return result;
}/*<<<*/

Symbol* symbol_add(SymbolTable* symbol_table, char* name, SymbolKind kind)
{/*>>>*/
  Symbol* symbol = push_element(symbol_table->arena, Symbol, 1);
  symbol->name = name;
  symbol->kind = kind;
  symbol->block_id = symbol_table->scope_id;
  symbol->nesting_depth = symbol_table->nesting_depth;
  symbol->next_symbol = symbol_table->symbol;
  symbol_table->symbol = symbol;
  return symbol;
}/*<<<*/

Symbol* symbol_register_new(SymbolTable* symbol_table, TokenStream* input, SymbolKind kind)
{/*>>>*/
  assert(input->token == Token_Id);

  Symbol* result = 0;

  Symbol* symbol = symbol_lookup(symbol_table, input->lexval.id);
  if(!symbol)
  {
    result = symbol_add(symbol_table, input->lexval.id, kind);
  } else {
    if(symbol->kind != SymbolKind_Keyword)
    {
      if(symbol->block_id != symbol_table->scope_id ||
          symbol->kind != kind)
      {
        assert(symbol->nesting_depth <= symbol_table->nesting_depth);

        result = symbol_add(symbol_table, input->lexval.id, kind);
      } else
        syntax_error(input, "Redeclaration of identifier: %s", symbol->name);
    } else
      syntax_error(input, "Keyword used as identifier: %s", symbol->name);
  }
  return result;
}/*<<<*/

Symbol* add_keyword(SymbolTable* symbol_table, char* name, Token token)
{/*>>>*/
  Symbol* symbol = symbol_add(symbol_table, name, SymbolKind_Keyword);
  symbol->kw_token = token;
  return symbol;
}/*<<<*/

void register_keywords(SymbolTable* symbol_table)
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
  add_keyword(symbol_table, "true", Token_True);
  add_keyword(symbol_table, "false", Token_False);
}/*<<<*/

bool32 scope_begin(SymbolTable* symbol_table)
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

void scope_end(SymbolTable* symbol_table)
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
/*<<<*/

static int last_label_id;

void gen_unique_label(String* label)
{
  sprintf(label->start, "L%d", last_label_id++);
  int len = str_len(label->start);
  label->end = label->start + len;
  MemoryArena* arena = label->arena;
  arena->free = label->end + 1;
}

/*>>> Lex */
bool32 token_is_keyword(Token token)
{/*>>>*/
  return token > Token__KeywordBegin && token < Token__KeywordEnd;
}/*<<<*/

char* install_lexeme(TokenStream* input, char* beginChar, char* endChar)
{/*>>>*/
  //FIXME: If the lexeme had been previously installed then return it.
  int len = (int)(endChar - beginChar + 1);
  char* lexeme = push_element(input->arena, char, len + 1);
  copy_substr(lexeme, beginChar, endChar);
  return lexeme;
}/*<<<*/

void consume_token(TokenStream* input, SymbolTable* symbol_table)
{/*>>>*/
  input->prev_token = input->token;
  input->token = Token__Null;
  mem_zero(&input->lexval);

  input->src_line = input->cursor;
  char c = *input->cursor;

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

  if(is_letter_char(c) || c == '_')
  {
    char* beginChar = input->cursor;
    c = *(++input->cursor);

    while(is_letter_char(c) || is_numeric_char(c) || c == '_')
      c = *(++input->cursor);

    char* endChar = input->cursor - 1;
    char* lexeme = install_lexeme(input, beginChar, endChar);
    input->lexval.id = lexeme;

    Symbol* symbol = symbol_lookup(symbol_table, lexeme);
    if(symbol && symbol->kind == SymbolKind_Keyword)
      input->token = symbol->kw_token;
    else
      input->token = Token_Id;
  }
  else if(is_numeric_char(c))
  {
    int num = c - '0';
    c = *(++input->cursor);

    while(is_numeric_char(c))
    {
      num = (10 * num) + (c - '0');
      c = *(++input->cursor);
    }

    int* value = push_element(input->arena, int, 1);
    *value = num;
    input->token = Token_IntNum;
    input->lexval.int_num = value;
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
  else if(c == '=')
  {
    input->token = Token_Equals;
    ++input->cursor;
  }
  else if(c == '+')
  {
    input->token = Token_Plus;
    ++input->cursor;
  }
  else if(c == '/')
  {
    input->token = Token_FwdSlash;
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
}/*<<<*/
/*<<<*/

/*>>> Parse */
bool32 parse_expression(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstNode**);
bool32 parse_statement_list(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*);
bool32 parse_actual_argument_list(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstCall*);
bool32 parse_term(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstNode**);
bool32 parse_statement(MemoryArena*, TokenStream*, SymbolTable*, AstBlock*, AstNode**);

bool32 parse_factor(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                    AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_OpenParens)
  {
    consume_token(input, symbol_table);
    success = parse_expression(arena, input, symbol_table,
                              enclosing_block, node);
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
        AstNode* negNode = push_element(arena, AstNode, 1);
        negNode->kind = AstNodeKind_UnrExpr;
        AstUnrExpr* expr = &negNode->unr_expr;
        expr->op = AstOperatorKind_Neg;
        expr->operand = operand;
        *node = negNode;
      } else {
        syntax_error(input, "Expression expected after '-'");
        success = false;
      }
    }
  }
  else if(input->token == Token_IntNum)
  {
    AstNode* numNode = push_element(arena, AstNode, 1);
    numNode->kind = AstNodeKind_IntNum;
    numNode->int_num.value = *(int32*)input->lexval.int_num;
    *node = numNode;

    consume_token(input, symbol_table);
  }
  else if(input->token == Token_Id)
  {
    Symbol* symbol = symbol_lookup(symbol_table, input->lexval.id);
    if(symbol)
    {
      AstNode* id_node = push_element(arena, AstNode, 1);
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

              List* procArgList = &symbol->proc->formal_args;
              List* callArgList = &call->actual_args;
              if(procArgList->count != callArgList->count)
              {
                syntax_error(input, "Incorrect number of arguments in the call: %s(..)", symbol->name);
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
      }
      else if(symbol->kind == SymbolKind_Keyword)
      {
        syntax_error(input, "Keyword used as identifier: %s", input->lexval.id);
        success = false;
      }
      else
        assert(false);
    } else {
      syntax_error(input, "Unknown identifier: %s", input->lexval.id);
      success = false;
    }
  }
  else if(input->token == Token_True || input->token == Token_False)
  {
    AstNode* numNode = push_element(arena, AstNode, 1);
    numNode->kind = AstNodeKind_IntNum;
    numNode->int_num.value = (input->token == Token_True ? 1 : 0);
    *node = numNode;

    consume_token(input, symbol_table);
  }

  return success;
}/*<<<*/

bool32 parse_rest_of_factors(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                             AstBlock* enclosing_block, AstNode* left_node, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Star ||
      input->token == Token_FwdSlash)
  {
    AstNode* expr_node = push_element(arena, AstNode, 1);
    expr_node->kind = AstNodeKind_BinExpr;
    AstBinExpr* expr = &expr_node->bin_expr;
    if(input->token == Token_Star)
      expr->op = AstOperatorKind_Mul;
    else if(input->token == Token_FwdSlash)
      expr->op = AstOperatorKind_Div;
    else
      assert(false);

    consume_token(input, symbol_table);
    AstNode* factorNode = 0;
    success = parse_factor(arena, input, symbol_table, enclosing_block, &factorNode);

    if(success && factorNode)
    {
      expr->right_operand = factorNode;
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

bool32 parse_term(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                  AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  AstNode* factorNode = 0;
  AstNode* expr_node = 0;

  bool32 success = parse_factor(arena, input, symbol_table, enclosing_block, &factorNode);
  if(success && factorNode)
    success = parse_rest_of_factors(arena, input, symbol_table,
                                 enclosing_block, factorNode, &expr_node);

  *node = expr_node;
  return success;
}/*<<<*/

bool32 parse_rest_of_terms(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                           AstBlock* enclosing_block, AstNode* left_node, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Plus ||
      input->token == Token_Minus)
  {
    AstNode* expr_node = push_element(arena, AstNode, 1);
    expr_node->kind = AstNodeKind_BinExpr;
    AstBinExpr* expr = &expr_node->bin_expr;
    if(input->token == Token_Plus)
      expr->op = AstOperatorKind_Add;
    else if(input->token == Token_Minus)
      expr->op = AstOperatorKind_Sub;
    else
      assert(false);

    consume_token(input, symbol_table);
    AstNode* termNode = 0;
    success = parse_term(arena, input, symbol_table, enclosing_block, &termNode);

    if(success && termNode)
    {
      expr->right_operand = termNode;
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

bool32 parse_assignment_term(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                             AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  AstNode* termNode = 0;
  AstNode* expr_node = 0;

  bool32 success = parse_term(arena, input, symbol_table, enclosing_block, &termNode);
  if(success && termNode)
    success = parse_rest_of_terms(arena, input, symbol_table, enclosing_block, termNode, &expr_node);

  *node = expr_node;
  return success;
}/*<<<*/

bool32 parse_rest_of_assignment_terms(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
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
          AstNode* expr_node = push_element(arena, AstNode, 1);
          expr_node->kind = AstNodeKind_BinExpr;
          AstBinExpr* expr = &expr_node->bin_expr;
          expr->op = AstOperatorKind_Assign;
          expr->left_operand = left_node;
          expr->right_operand = right_side;

          *node = expr_node;
        } else {
          syntax_error(input, "Variable is required on the left side of assignment");
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

bool32 parse_expression(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                        AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  AstNode* assgn_node = 0;
  AstNode* expr_node = 0;

  bool32 success = parse_assignment_term(arena, input, symbol_table, enclosing_block, &assgn_node);
  if(success && assgn_node)
    success = parse_rest_of_assignment_terms(arena, input, symbol_table,
                                             enclosing_block, assgn_node, &expr_node);

  *node = expr_node;
  return success;
}/*<<<*/

bool32 parse_var_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                           AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Var)
  {
    consume_token(input, symbol_table);
    if(input->token == Token_Id)
    {
      AstNode* var_node = push_element(arena, AstNode, 1);
      var_node->kind = AstNodeKind_VarDecl;
      *node = var_node;

      Symbol* symbol = symbol_register_new(symbol_table, input, SymbolKind_Var);
      if(symbol)
      {
        AstVarDecl* var_decl = &var_node->var_decl;
        var_decl->symbol = symbol;
        var_decl->name = symbol->name;
        var_decl->var_data.size = 1;
        symbol->var = var_decl;

        consume_token(input, symbol_table);
        if(input->token == Token_Equals)
        {
          AstNode* occur_node = push_element(arena, AstNode, 1);
          occur_node->kind = AstNodeKind_VarOccur;
          AstVarOccur* var_occur = &occur_node->var_occur;
          var_occur->symbol = symbol;
          var_occur->name = symbol->name;
          var_occur->var_decl = var_decl;
          var_occur->decl_block_offset = 0;
          //var_occur->is_non_local = false;

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

bool32 parse_formal_argument(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                             AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Var)
  {
    consume_token(input, symbol_table);
    if(input->token == Token_Id)
    {
      AstNode* var_node = push_element(arena, AstNode, 1);
      var_node->kind = AstNodeKind_VarDecl;
      *node = var_node;

      Symbol* symbol = symbol_register_new(symbol_table, input, SymbolKind_Var);
      if(symbol)
      {
        AstVarDecl* var_decl = &var_node->var_decl;
        var_decl->symbol = symbol;
        var_decl->name = symbol->name;
        symbol->var = var_decl;

        consume_token(input, symbol_table);
      } else {
        syntax_error(input, "Identifier re-declared : %s", input->lexval.id);
        success = false;
      }
    } else {
      syntax_error(input, "Expecting an identifier token");
      success = false;
    }
  }

  return success;
}/*<<<*/

bool32 parse_formal_argument_list(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                                 AstBlock* enclosing_block, AstProc* proc)
{/*>>>*/
  bool32 success = true;

  AstNode* argNode = 0;
  success = parse_formal_argument(arena, input, symbol_table, enclosing_block, &argNode);
  if(success && argNode)
  {
    list_append(arena, &proc->formal_args, argNode);

    if(input->token == Token_Comma)
    {
      consume_token(input, symbol_table);
      success = parse_formal_argument_list(arena, input, symbol_table, enclosing_block, proc);
    }
  }

  return success;
}/*<<<*/

bool32 parse_actual_argument_list(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                                  AstBlock* enclosing_block, AstCall* call)
{/*>>>*/
  bool32 success = true;

  AstNode* argNode = 0;
  success = parse_expression(arena, input, symbol_table, enclosing_block, &argNode);
  if(success && argNode)
  {
    list_append(arena, &call->actual_args, argNode);

    if(input->token == Token_Comma)
    {
      consume_token(input, symbol_table);
      success = parse_actual_argument_list(arena, input, symbol_table, enclosing_block, call);
    }
  }

  return success;
}/*<<<*/

bool32 parse_while_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
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
      AstNode* whileNode = push_element(arena, AstNode, 1);
      whileNode->kind = AstNodeKind_WhileStmt;
      AstWhileStmt* while_stmt = &whileNode->while_stmt;
      while_stmt->expr = expr_node;
      *node = whileNode;

      if(input->token == Token_OpenBrace)
      {
        consume_token(input, symbol_table);

        success = scope_begin(symbol_table);
        if(success)
        {
          AstNode* block_node = push_element(arena, AstNode, 1);
          block_node->kind = AstNodeKind_Block;
          AstBlock* block = &block_node->block;
          block->owner = whileNode;
          block_init(symbol_table, block);

          success = parse_statement_list(arena, input, symbol_table, block);
          if(success)
          {
            while_stmt->body = block_node;

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
      } else {
        syntax_error(input, "Missing '{'");
        success = false;
      }
    }
  }

  return success;
}/*<<<*/

bool32 parse_block(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                   AstBlock* enclosing_block, AstNode** node)
{
  bool32 success = true;

  if(input->token == Token_OpenBrace)
  {
    consume_token(input, symbol_table);

    success = scope_begin(symbol_table);
    if(success)
    {
      AstNode* block_node = push_element(arena, AstNode, 1);
      block_node->kind = AstNodeKind_Block;
      AstBlock* block = &block_node->block;
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
}

bool32 parse_if_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
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
      AstNode* if_node = push_element(arena, AstNode, 1);
      if_node->kind = AstNodeKind_IfStmt;
      AstIfStmt* if_stmt = &if_node->if_stmt;
      if_stmt->expr = expr_node;
      *node = if_node;

      if(input->token == Token_OpenBrace)
      {
        consume_token(input, symbol_table);

        success = scope_begin(symbol_table);
        if(success)
        {
          AstNode* block_node = push_element(arena, AstNode, 1);
          block_node->kind = AstNodeKind_Block;
          AstBlock* block = &block_node->block;
          block->enclosing_block = enclosing_block;
          block->owner = if_node;
          block_init(symbol_table, block);

          success = parse_statement_list(arena, input, symbol_table, block);
          if(success)
          {
            if_stmt->body = block_node;

            if(input->token == Token_CloseBrace)
            {
              consume_token(input, symbol_table);
              scope_end(symbol_table);

              if(input->token == Token_Else)
              {
                consume_token(input, symbol_table);
                AstNode* else_node = 0;
                success = parse_if_statement(arena, input, symbol_table, enclosing_block, &else_node);
                if(success)
                {
                  if(else_node)
                  {
                    if_stmt->else_body = else_node;
                  }
                  else if(input->token == Token_OpenBrace)
                  {
                    consume_token(input, symbol_table);

                    success = scope_begin(symbol_table);
                    if(success)
                    {
                      AstNode* block_node = push_element(arena, AstNode, 1);
                      block_node->kind = AstNodeKind_Block;
                      AstBlock* block = &block_node->block;
                      block->enclosing_block = enclosing_block;
                      block->owner = if_node;
                      block_init(symbol_table, block);

                      success = parse_statement_list(arena, input, symbol_table, block);
                      if(success)
                      {
                        if_stmt->else_body = block_node;

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
                  } else {
                    syntax_error(input, "Missing '{'");
                    success = false;
                  }
                }
              }
            } else {
              syntax_error(input, "Missing '}'");
              success = false;
            }
          }
        }
      } else // single-statement
      {
        AstNode* stmt_node = 0;
        success = parse_statement(arena, input, symbol_table, enclosing_block, &stmt_node);
        if(success)
        {
          while(input->token == Token_Semicolon)
            consume_token(input, symbol_table);

          if(stmt_node)
          {
            if(stmt_node->kind != AstNodeKind_VarDecl)
            {
              if_stmt->body = stmt_node;

              if(input->token == Token_Else)
              {
                consume_token(input, symbol_table);

                AstNode* else_node = 0;
                success = parse_if_statement(arena, input, symbol_table, enclosing_block, &else_node);
                if(success)
                {
                  if(else_node)
                  {
                    if_stmt->else_body = else_node;
                  } else
                  {
                    success = parse_block(arena, input, symbol_table, enclosing_block, &else_node);
                    if(success)
                    {
                      if(else_node)
                      {
                        if_stmt->else_body = else_node;

                        AstBlock* block = &else_node->block;
                        block->owner = if_node;
                      } else
                      {
                        success = parse_statement(arena, input, symbol_table, enclosing_block, &else_node);
                        if(success)
                        {
                          if(else_node)
                          {
                            if_stmt->else_body = else_node;
                          } else {
                            syntax_error(input, "Non-empty statement list required");
                            success = false;
                          }
                        }
                      }
                    }
                  }
                }
              }
            } else
            {
              syntax_error(input, "Var statement not allowed here");
              success = false;
            }
          } else
          {
            syntax_error(input, "Non-empty statement required");
            success = false;
          }
        }
      }
    }
  }
  return success;
}/*<<<*/

bool32 parse_procedure(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                       AstBlock* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Proc)
  {
    AstNode* procNode = push_element(arena, AstNode, 1);
    procNode->kind = AstNodeKind_Proc;
    *node = procNode;

    consume_token(input, symbol_table);
    if(input->token == Token_Id)
    {
      Symbol* symbol = symbol_register_new(symbol_table, input, SymbolKind_Proc);
      if(symbol)
      {
        AstProc* proc = &procNode->proc;
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
            AstBlock* block = push_element(arena, AstBlock, 1);
            proc->body = block;
            block->owner = procNode;
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
                  syntax_error(input, "Missing 'var' keyword", input->lexval.id);
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

bool32 parse_procedureList(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                           AstBlock* enclosing_block, AstModule* module)
{/*>>>*/
  bool32 success = true;

  AstNode* procNode = 0;
  success = parse_procedure(arena, input, symbol_table, enclosing_block, &procNode);
  if(success && procNode)
  {
    list_append(arena, &module->proc_list, procNode);

    success = parse_procedureList(arena, input, symbol_table, enclosing_block, module);
  }

  return success;
}/*<<<*/

bool32 parse_return_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
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
        AstNode* retNode = push_element(arena, AstNode, 1);
        retNode->kind = AstNodeKind_ReturnStmt;
        AstReturnStmt* ret_stmt = &retNode->ret_stmt;
        ret_stmt->expr = expr_node;
        *node = retNode;

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
        assert(block);
        AstProc* ret_proc = &block->owner->proc;
        ret_stmt->proc = ret_proc;
        ret_stmt->intervening_block_count = depth;

        AstNode* var_node = push_element(arena, AstNode, 1);
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
        syntax_error(input, "Expression required after the 'return' keyword");
        success = false;
      }
    }
  }

  return success;
}/*<<<*/

bool32 parse_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
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
                if(expr->op != AstOperatorKind_Assign)
                {
                  syntax_error(input, "Assignment expression required");
                  success = false;
                }
              }
              else if(stmt_node->kind != AstNodeKind_Call)
              {
                syntax_error(input, "Expression is not a statement");
                success = false;
              }
            }
            else {
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
        success = parse_if_statement(arena, input, symbol_table, enclosing_block, &stmt_node);
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
        success = parse_while_statement(arena, input, symbol_table, enclosing_block, &stmt_node);
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
        success = parse_return_statement(arena, input, symbol_table, enclosing_block, &stmt_node);
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

      default:
        alt = Alt__Null;
        break;
    }
  }

  *node = stmt_node;
  return success;
}/*<<<*/

bool32 parse_statement_list(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
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

bool32 parse_module(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                    AstNode** node)
{/*>>>*/
  bool32 success = true;

  success = scope_begin(symbol_table);
  if(success)
  {
    AstNode* moduleNode = push_element(arena, AstNode, 1);
    moduleNode->kind = AstNodeKind_Module;

    AstNode* block_node = push_element(arena, AstNode, 1);
    block_node->kind = AstNodeKind_Block;
    AstBlock* block = &block_node->block;
    block->owner = moduleNode;
    block_init(symbol_table, block);

    AstModule* module = &moduleNode->module;
    module->body = block_node;
    list_init(&module->proc_list);

    success = parse_procedureList(arena, input, symbol_table, block, module);
    if(success)
    {
      scope_end(symbol_table);

      if(input->token == Token_EndOfInput)
        *node = moduleNode;
      else {
        syntax_error(input, "End of file expected");
        success = false;
      }
    }
  }

  return success;
}/*<<<*/
/*<<<*/

/*>>> IR */
typedef enum
{
  IrNodeKind__Null,
  IrNodeKind_Value,
  IrNodeKind_BinExpr,
  IrNodeKind_UnrExpr,
  IrNodeKind_Proc,
  IrNodeKind_Block,
  IrNodeKind_Module,
  IrNodeKind_Call,
  IrNodeKind_Return,
  IrNodeKind_IfStmt,
}
IrNodeKind;

typedef enum
{
  IrOpKind__Null,
  IrOpKind_Add,
  IrOpKind_Mul,
  IrOpKind_Sub,
  IrOpKind_Div,
  IrOpKind_Store,
  IrOpKind_Neg,
}
IrOpKind;

typedef struct IrNode_ IrNode;
typedef struct IrValue_ IrValue;

typedef enum
{
  IrValueKind__Null,
  IrValueKind_Var,
  IrValueKind_IntNum,
  IrValueKind_Call,
  IrValueKind_BinExpr,
  IrValueKind_UnrExpr,
}
IrValueKind;

typedef struct
{
  IrProc* proc;
  List    actual_args;
}
IrCall;

typedef struct
{
  IrOpKind op;
  IrValue* left_operand;
  IrValue* right_operand;
}
IrBinExpr;

typedef struct
{
  IrOpKind op;
  IrValue* operand;
}
IrUnrExpr;

typedef struct
{
  IrBinExpr* assgn_expr;
  IrVar      ret_var;
  IrProc*    proc;
  int        depth;
}
IrReturn;

typedef enum
{
  IrAvRecord__Null,
  IrAvRecord_Proc,
  IrAvRecord_Block,
}
IrAvRecordKind;

typedef struct
{
  IrStackArea access_links;
  IrStackArea old_fp;
}
IrBlockAvRecord;

typedef struct
{
  IrStackArea ret;
  IrStackArea args;
  IrStackArea ctrl_links;
}
IrProcAvRecord;

typedef struct
{
  IrValue* expr;
  IrNode*  body;
  IrNode*  else_body;
  char*    label_else;
  char*    label_end;
}
IrIfStmt;

typedef struct IrValue_
{
  IrValueKind kind;

  union {
    int32      int_num;
    IrVar*     var;
    IrCall*    call;
    IrBinExpr* bin_expr;
    IrUnrExpr* unr_expr;
  };
}
IrValue;

typedef struct
{
  union {
    IrProcAvRecord  proc;
    IrBlockAvRecord block;
  };
  IrStackArea locals;
  IrAvRecordKind kind;
  int fp;
  int sp;
}
IrActivationRecord;

typedef struct IrProc_
{
  char* label;
  char* end_label;
  List  instr_list;
  IrActivationRecord* actv_rec;
}
IrProc;

typedef struct IrBlock_
{
  List access_links;
  List instr_list;
  IrActivationRecord* actv_rec;
}
IrBlock;

typedef struct
{
  IrProc* main_proc;
  IrCall* main_call;
  List    proc_list;
}
IrModule;

typedef struct IrNode_
{
  IrNodeKind kind;

  union {
    IrVar     var;
    IrValue   value;
    IrBinExpr bin_expr;
    IrUnrExpr unr_expr;
    IrProc    proc;
    IrModule  module;
    IrCall    call;
    IrReturn  ret;
    IrIfStmt  if_stmt;
    IrBlock   block;
  };
} IrNode;

IrNode* ir_build_value(MemoryArena*, IrActivationRecord*, AstNode*);
IrNode* ir_build_statement(MemoryArena*, IrActivationRecord*, AstNode*);
void ir_block_build_statements(MemoryArena*, IrActivationRecord*, List*, List*);

IrNode* ir_build_bin_expr(MemoryArena* arena, IrActivationRecord* actv_rec, AstBinExpr* bin_expr)
{/*>>>*/
  IrNode* ir_node = push_element(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_BinExpr;
  IrBinExpr* ir_op = &ir_node->bin_expr;

  AstNode* left_operand = bin_expr->left_operand;
  AstNode* right_operand = bin_expr->right_operand;
  IrNode* val_node = ir_build_value(arena, actv_rec, right_operand);
  ir_op->right_operand = &val_node->value;
  val_node = ir_build_value(arena, actv_rec, left_operand);
  ir_op->left_operand = &val_node->value;

  if(bin_expr->op == AstOperatorKind_Assign)
  {
    assert(left_operand->kind == AstNodeKind_VarOccur);
    ir_op->op = IrOpKind_Store;
  }
  else if(bin_expr->op == AstOperatorKind_Add)
    ir_op->op = IrOpKind_Add;
  else if(bin_expr->op == AstOperatorKind_Sub)
    ir_op->op = IrOpKind_Sub;
  else if(bin_expr->op == AstOperatorKind_Mul)
    ir_op->op = IrOpKind_Mul;
  else if(bin_expr->op == AstOperatorKind_Div)
    ir_op->op = IrOpKind_Div;
  else
    assert(false);

  return ir_node;
}/*<<<*/

IrNode* ir_build_unr_expr(MemoryArena* arena, IrActivationRecord* actv_rec, AstUnrExpr* unr_expr)
{/*>>>*/
  IrNode* ir_node = push_element(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_UnrExpr;
  IrUnrExpr* ir_op = &ir_node->unr_expr;
  IrNode* val_node = ir_build_value(arena, actv_rec, unr_expr->operand);
  ir_op->operand = &val_node->value;
  if(unr_expr->op == AstOperatorKind_Neg)
    ir_op->op = IrOpKind_Neg;
  return ir_node;
}/*<<<*/

IrNode* ir_build_call(MemoryArena* arena, IrActivationRecord* actv_rec, AstCall* ast_call)
{/*>>>*/
  IrNode* ir_call_node = push_element(arena, IrNode, 1);
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

IrNode* ir_build_value(MemoryArena* arena, IrActivationRecord* actv_rec, AstNode* ast_node)
{/*>>>*/
  IrNode* ir_node = push_element(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_Value;
  if(ast_node->kind == AstNodeKind_VarOccur)
  {
    AstVarOccur* var_occur = &ast_node->var_occur;
    AstVarDecl* var_decl = var_occur->var_decl;
    IrVar* var = push_element(arena, IrVar, 1);
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

IrNode* ir_build_return(MemoryArena* arena, IrActivationRecord* actv_rec, AstReturnStmt* ast_ret)
{/*>>>*/
  IrNode* ir_node = push_element(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_Return;
  IrReturn* ir_ret = &ir_node->ret;

  IrBinExpr* assgn_expr = push_element(arena, IrBinExpr, 1);
  assgn_expr->op = IrOpKind_Store;

  assert(ast_ret->ret_var->kind == AstNodeKind_VarOccur);
  AstVarOccur* var_occur = &ast_ret->ret_var->var_occur;
  AstVarDecl* var_decl = var_occur->var_decl;
  IrVar* ret_var = &ir_ret->ret_var;
  ret_var->data = &var_decl->var_data;
  ret_var->link = var_occur->link;
  IrValue* left_operand = push_element(arena, IrValue, 1);
  left_operand->kind = IrValueKind_Var;
  left_operand->var = &ir_ret->ret_var;
  assgn_expr->left_operand = left_operand;

  IrNode* ret_val = ir_build_value(arena, actv_rec, ast_ret->expr);
  assgn_expr->right_operand = &ret_val->value;
  ir_ret->assgn_expr = assgn_expr;

  ir_ret->proc = ast_ret->proc->ir_proc;
  ir_ret->depth = ast_ret->intervening_block_count;

  return ir_node;
}/*<<<*/

void ir_block_decl_vars_compute_address(IrActivationRecord* actv_rec, IrStackArea* locals_area, List* decl_vars)
{
  locals_area->loc = actv_rec->sp;
  {/*>>> locals*/
    ListItem* node_item = list_first_item(decl_vars);
    while(node_item)
    {
      AstNode* decl_node = node_item->elem;
      assert(decl_node->kind == AstNodeKind_VarDecl);
      AstVarDecl* var_decl = &decl_node->var_decl;
      IrStackArea* var_data = &var_decl->var_data;

      var_data->loc = actv_rec->sp - actv_rec->fp;
      actv_rec->sp += var_data->size;

      node_item = node_item->next;
    }
  }/*<<<*/
  locals_area->size = actv_rec->sp - locals_area->loc;
}

IrNode* ir_build_block(MemoryArena* arena, AstBlock* ast_block)
{/*>>>*/
  IrNode* ir_node = push_element(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_Block;
  IrBlock* ir_block = &ir_node->block;
  IrActivationRecord* actv_rec = push_element(arena, IrActivationRecord, 1);
  actv_rec->kind = IrAvRecord_Block;
  ir_block->actv_rec = actv_rec;
  IrBlockAvRecord* block_av = &actv_rec->block;
  list_init(&ir_block->instr_list);
  list_init(&ir_block->access_links);

  IrStackArea* area = &block_av->access_links;
  area->loc = actv_rec->sp;
  {/*>>> non-locals */
    ListItem* occur_item = list_first_item(&ast_block->non_local_occurs);
    while(occur_item)
    {
      AstNode* node = occur_item->elem;
      AstVarOccur* var_occur = &node->var_occur;
      List* links_list = &ir_block->access_links;

      ListItem* link_item = list_first_item(links_list);
      IrAccessLink* link = 0;
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
        link = push_element(arena, IrAccessLink, 1);
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
        IrAccessLink* link = link_item->elem;
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

IrNode* ir_build_if_stmt(MemoryArena* arena, IrActivationRecord* actv_rec, AstIfStmt* ast_if)
{/*>>>*/
  IrNode* ir_node = push_element(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_IfStmt;
  IrIfStmt* ir_if = &ir_node->if_stmt;
  ir_if->expr = &ir_build_value(arena, actv_rec, ast_if->expr)->value;

  {/*>>> label generation */
    String label_id = {0};
    string_init(&label_id, arena);
    gen_unique_label(&label_id);

    String label = {0};
    string_init(&label, arena);
    append_string(&label, label_id.start);
    append_string(&label, ".if-else");
    ir_if->label_else = label.start;

    string_init(&label, arena);
    append_string(&label, label_id.start);
    append_string(&label, ".if-end");
    ir_if->label_end = label.start;
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

IrNode* ir_build_statement(MemoryArena* arena, IrActivationRecord* actv_rec, AstNode* ast_node)
{/*>>>*/
  IrNode* ir_node = 0;
  if(ast_node->kind == AstNodeKind_BinExpr)
  {
    AstBinExpr* bin_expr = &ast_node->bin_expr;
    assert(bin_expr->op == AstOperatorKind_Assign);
    ir_node = ir_build_bin_expr(arena, actv_rec, bin_expr);
  }
  else if(ast_node->kind == AstNodeKind_Call)
    ir_node = ir_build_call(arena, actv_rec, &ast_node->call);
  else if(ast_node->kind == AstNodeKind_ReturnStmt)
    ir_node = ir_build_return(arena, actv_rec, &ast_node->ret_stmt);
  else if(ast_node->kind == AstNodeKind_IfStmt)
    ir_node = ir_build_if_stmt(arena, actv_rec, &ast_node->if_stmt);
  else
    assert(false);

  return ir_node;
}/*<<<*/

void ir_block_build_statements(MemoryArena* arena, IrActivationRecord* actv_rec,
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

IrNode* ir_build_proc(MemoryArena* arena, AstProc* ast_proc)
{/*>>>*/
  AstBlock* ast_block = ast_proc->body;

  IrActivationRecord* actv_rec = push_element(arena, IrActivationRecord, 1);
  actv_rec->kind = IrAvRecord_Proc;
  IrProcAvRecord* proc_av = &actv_rec->proc;
  actv_rec->sp = 0;

  IrStackArea* area = &proc_av->ret;
  area->loc = actv_rec->sp;
  {/*>>> ret val*/
    AstVarDecl* var_decl = &ast_proc->ret_var;
    IrStackArea* var_data = &var_decl->var_data;
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
      IrStackArea* arg_var_data = &var_decl->var_data;

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
    IrStackArea* var_data = &ast_proc->ret_var.var_data;
    var_data->loc = -(actv_rec->fp - var_data->loc);

    ListItem* node_item = list_first_item(&ast_proc->formal_args);
    while(node_item)
    {
      AstNode* decl_node = node_item->elem;
      assert(decl_node->kind == AstNodeKind_VarDecl);
      AstVarDecl* var_decl = &decl_node->var_decl;
      IrStackArea* var_data = &var_decl->var_data;

      var_data->loc = -(actv_rec->fp - var_data->loc);

      node_item = node_item->next;
    }
  }/*<<<*/

  IrNode* ir_node = push_element(arena, IrNode, 1);
  ir_node->kind = IrNodeKind_Proc;
  IrProc* ir_proc = &ir_node->proc;
  ir_proc->actv_rec = actv_rec;
  ir_proc->label = ast_proc->name;
  list_init(&ir_proc->instr_list);

  String end_label = {0};
  string_init(&end_label, arena);
  append_string(&end_label, ast_proc->name);
  append_string(&end_label, ".proc-end");
  ir_proc->end_label = end_label.start;
  ast_proc->ir_proc = ir_proc; // the pointer must be set before building the statement list

  ir_block_build_statements(arena, actv_rec, &ir_proc->instr_list, &ast_block->stmt_list);

  return ir_node;
}/*<<<*/
 
IrNode* ir_build_module(MemoryArena* arena, AstModule* module)
{/*>>>*/
  IrNode* ir_module_node = push_element(arena, IrNode, 1);
  ir_module_node->kind = IrNodeKind_Module;
  IrModule* ir_module = &ir_module_node->module;
  list_init(&ir_module->proc_list);

  ListItem* proc_item = list_first_item(&module->proc_list);
  while(proc_item)
  {
    AstNode* ast_proc_node = proc_item->elem;
    assert(ast_proc_node->kind == AstNodeKind_Proc);
    IrNode* ir_procNode = ir_build_proc(arena, &ast_proc_node->proc);
    ast_proc_node->proc.ir_proc = &ir_procNode->proc;
    list_append(arena, &ir_module->proc_list, ir_procNode);
    if(str_match(ast_proc_node->proc.name, "main"))
      ir_module->main_proc = &ir_procNode->proc;

    proc_item = proc_item->next;
  }

  if(ir_module->main_proc)
  {
    IrNode* ir_call_node = push_element(arena, IrNode, 1);
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
/*<<<*/

void print_instruction(VmProgram* vm_program, char* code, ...)
{/*>>>*/
  static char strbuf[128] = {0};
  va_list args;

  va_start(args, code);
  vm_program->text_len += vsprintf(strbuf, code, args);
  va_end(args);

  append_string(&vm_program->text, strbuf);
  append_string(&vm_program->text, "\n");
  vm_program->text_len++;
}/*<<<*/

void emit_instr_reg(MemoryArena* arena, List* instr_list, Opcode opcode, RegName reg)
{/*>>>*/
  Instruction* instr = push_element(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType_Reg;
  instr->param.reg = reg;
  list_append(arena, instr_list, instr);
}/*<<<*/

void emit_instr_int(MemoryArena* arena, List* instr_list, Opcode opcode, int32 int_num)
{/*>>>*/
  Instruction* instr = push_element(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType_Int32;
  instr->param.int_num = int_num;
  list_append(arena, instr_list, instr);
}/*<<<*/

void emit_instr(MemoryArena* arena, List* instr_list, Opcode opcode)
{/*>>>*/
  Instruction* instr = push_element(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType__Null;
  list_append(arena, instr_list, instr);
}/*<<<*/

void emit_instr_str(MemoryArena* arena, List* instr_list, Opcode opcode, char* str)
{/*>>>*/
  Instruction* instr = push_element(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->param_type = ParamType_String;
  instr->param.str = str;
  list_append(arena, instr_list, instr);
}/*<<<*/

void gen_load_rvalue(MemoryArena*, List*, IrValue*);
void gen_load_lvalue(MemoryArena*, List*, IrValue*);
void gen_statement(MemoryArena*, List*, IrNode*);

void gen_bin_expr(MemoryArena* arena, List* code, IrBinExpr* bin_expr)
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
  else
    assert(false);
}/*<<<*/

void gen_call(MemoryArena* arena, List* code, IrCall* call)
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

void gen_load_lvalue(MemoryArena* arena, List* code, IrValue* ir_value)
{/*>>>*/
  if(ir_value->kind == IrValueKind_Var)
  {
    IrStackArea* data = ir_value->var->data;
    IrAccessLink* link = ir_value->var->link;

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

void gen_load_rvalue(MemoryArena* arena, List* code, IrValue* ir_value)
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

void gen_return(MemoryArena* arena, List* code, IrReturn* ir_ret)
{/*>>>*/
  gen_bin_expr(arena, code, ir_ret->assgn_expr);
  emit_instr(arena, code, Opcode_POP);

  IrProc* proc = ir_ret->proc;
  int level = ir_ret->depth;
  while(level--)
    emit_instr(arena, code, Opcode_LEAVE);
  emit_instr_str(arena, code, Opcode_GOTO, proc->end_label);
}/*<<<*/

void gen_block(MemoryArena* arena, List* code, IrBlock* block)
{
  ListItem* link_item = list_first_item(&block->access_links);
  while(link_item)
  {
    IrAccessLink* link = link_item->elem;
    emit_instr_reg(arena, code, Opcode_PUSH, RegName_FP);
    int offset = link->actv_rec_offset - 1;
    while(offset--)
    {
      emit_instr(arena, code, Opcode_DECR); // todo: explain why
      emit_instr(arena, code, Opcode_LOAD);
    }
    link_item = link_item->next;
  }

  emit_instr(arena, code, Opcode_ENTER);

  IrActivationRecord* actv_rec = block->actv_rec;
  IrStackArea* locals_area = &actv_rec->locals;
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
}

void gen_proc(MemoryArena* arena, List* code, IrProc* proc)
{/*>>>*/
  emit_instr_str(arena, code, Opcode_LABEL, proc->label);
  IrActivationRecord* actv_rec = proc->actv_rec;
  IrStackArea* locals_area = &actv_rec->locals;
  if(locals_area->size > 0)
    emit_instr_int(arena, code, Opcode_ALLOC, locals_area->size);

  ListItem* item = list_first_item(&proc->instr_list);
  while(item)
  {
    IrNode* ir_node = item->elem;
    gen_statement(arena, code, ir_node);
    item = item->next;
  }

  emit_instr_str(arena, code, Opcode_LABEL, proc->end_label);
  emit_instr(arena, code, Opcode_RETURN);
}/*<<<*/

void gen_statement(MemoryArena* arena, List* code, IrNode* stmt_node)
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
  else if(stmt_node->kind == IrNodeKind_Return)
  {
    gen_return(arena, code, &stmt_node->ret);
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
      else if(else_body->kind == IrNodeKind_IfStmt)
        gen_statement(arena, code, else_body);
      else
        gen_statement(arena, code, else_body);
    }
    emit_instr_str(arena, code, Opcode_LABEL, ir_if->label_end);
  }
  else
    assert(false);
}/*<<<*/

void gen_module(MemoryArena* arena, List* code, IrModule* module)
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

char* get_regname_str(RegName reg)
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

void print_code(VmProgram* vm_program)
{/*>>>*/
  ListItem* item = list_first_item(&vm_program->instr_list);
  while(item)
  {
    Instruction* instr = item->elem;
    switch(instr->opcode)
    {
      case(Opcode_PUSH):
      {
        if(instr->param_type == ParamType_Reg)
          print_instruction(vm_program, "push %s", get_regname_str(instr->param.reg));
        else if(instr->param_type == ParamType_Int32)
          print_instruction(vm_program, "push %d", instr->param.int_num);
        else
          assert(false);
      } break;

      case(Opcode_POP):
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

      case(Opcode_ADD):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "add");
      } break;

      case(Opcode_SUB):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "sub");
      } break;

      case(Opcode_MUL):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "mul");
      } break;

      case(Opcode_DIV):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "div");
      } break;

      case(Opcode_LOAD):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "load");
      } break;

      case(Opcode_STORE):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "store");
      } break;

      case(Opcode_LABEL):
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "label %s", instr->param.str);
      } break;

      case(Opcode_RETURN):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "return");
      } break;

      case(Opcode_ALLOC):
      {
        assert(instr->param_type == ParamType_Int32);
        print_instruction(vm_program, "alloc %d", instr->param.int_num);
      } break;

      case(Opcode_CALL):
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "call %s", instr->param.str);
      } break;

      case(Opcode_HALT):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "halt");
      } break;

      case(Opcode_GOTO):
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "goto %s", instr->param.str);
      } break;

      case(Opcode_JUMPZ):
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vm_program, "jumpz %s", instr->param.str);
      } break;

      case(Opcode_DECR):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "decr");
      } break;

      case(Opcode_ENTER):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "enter");
      } break;

      case(Opcode_LEAVE):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vm_program, "leave");
      } break;

      default:
        assert(false);
    }
    item = item->next;
  }
}/*<<<*/

/*>>> Old code gen */
//bool32 BuildIr(MemoryArena* arena, SymbolTable* symbol_table, AstNode* ast)
//{/*>>>*/
//  bool32 success = true;
//
//  switch(ast->kind)
//  {
//    case Ast_Call:
//    {
//      int actualArgCount = ast->call.argCount;
//      Symbol* procSymbol = ast->call.symbol;
//      Block* procBlock = procSymbol->ast->block;
//      assert(procBlock->kind == Block_Proc);
//      assert(actualArgCount == procBlock->proc.argCount);
//    } break;
//
//    case Ast_Block:
//    {
//      Block* block = ast->block;
//      ActivationRecord* actv_rec = &block->actv_rec;
//      actv_rec->kind = ActvRecord_Block;
//
//      switch(block->kind)
//      {
//        case Block_Module:
//          {
//            Ast_ListItem* proc_list = block->module.proc_list;
//            while(proc_list && success)
//            {
//              AstNode* procAst = proc_list->ast;
//              success = BuildIr(arena, symbol_table, procAst);
//
//              proc_list = proc_list->nextItem;
//            }
//          } break;
//
//        case Block_Proc:
//          {
//            actv_rec->kind = ActvRecord_Proc;
//            actv_rec->proc.retAreaSize = 1;
//
//            Ast_ListItem* argList = block->proc.argList;
//            int args_areaSize = 0;
//            int argsCount = 0;
//            DataObjList* irArgs = 0;
//            while(argList)
//            {
//              AstNode* argAst = argList->ast;
//              Symbol* argSymbol = argAst->id.symbol;
//
//              assert(argAst->kind == Ast_Var);
//              assert(argSymbol->kind == SymbolKind_Var);
//
//              DataObj* irArg = &argSymbol->dataObj;
//              // Note that the storage locs for arguments are negative
//              irArg->loc = -(args_areaSize + CONTROL_LINKS_DATA_SIZE + 1);
//              irArg->size     = 1;
//
//              args_areaSize += irArg->size;
//
//              DataObjList* irArgItem = push_element(arena, DataObjList, 1);
//              irArgItem->dataObj = irArg;
//              irArgItem->nextItem = irArgs;
//              irArgs = irArgItem;
//
//              argList = argList->nextItem;
//              argsCount++;
//            }
//            actv_rec->proc.args = irArgs;
//            actv_rec->proc.args_areaSize = args_areaSize;
//            actv_rec->proc.argsCount = argsCount;
//
//            Symbol* procSymbol = block->proc.symbol;
//
//            //FIXME Looks like a hack
//            if(str_match(procSymbol->name, "main"))
//            {
//              if(block->proc.argList)
//              {
//                error("main() must not have arguments");
//                success = false;
//              }
//            }
//          } break;
//
//        case Block_WhileStmt:
//          {
//            success = BuildIr(arena, symbol_table, block->while_stmt.expr);
//          } break;
//
//        default:
//          assert(false && !"Not implemented");
//      }
//
//      // Process the declared vars
//      Ast_ListItem* declList = block->decl_vars;
//      DataObjList* irLocalsList = 0;
//      int localAreaSize = 0;
//      while(declList)
//      {
//        AstNode* varAst = declList->ast;
//        Symbol* varSymbol = varAst->var.symbol;
//
//        assert(varAst->kind == Ast_Var);
//        assert(varSymbol->kind == SymbolKind_Var);
//
//        DataObj* dataObj = &varSymbol->dataObj;
//        dataObj->loc = localAreaSize;
//        localAreaSize += dataObj->size;
//
//        DataObjList* irLocalItem = push_element(arena, DataObjList, 1);
//        irLocalItem->dataObj = dataObj;
//        irLocalItem->nextItem = irLocalsList;
//        irLocalsList = irLocalItem;
//
//        declList = declList->nextItem;
//      }
//      actv_rec->localObjects = irLocalsList;
//      actv_rec->localAreaSize = localAreaSize;
//
//      // Process the non-local refs
//      Ast_ListItem* nonLocalsList = block->nonLocalVars;
//      int access_linkCount = 0;
//      AccessLink* access_links = 0;
//      while(nonLocalsList)
//      {
//        AstNode* idAst = nonLocalsList->ast;
//        Symbol* idSymbol = idAst->id.symbol;
//
//        assert(idAst->kind == Ast_Id);
//        assert(idAst->id.decl_block_offset > 0);
//        assert(idSymbol->kind == SymbolKind_Var);
//
//        AccessLink* access_link = 0;
//        {
//          AccessLink* link = access_links;
//          while(link)
//          {
//            if(link->actv_rec_offset == idAst->id.decl_block_offset)
//            {
//              access_link = link;
//              break;
//            }
//            link = link->nextLink;
//          }
//          if(!access_link)
//          {
//            access_link = push_element(arena, AccessLink, 1);
//            access_link->actv_rec_offset = idAst->id.decl_block_offset;
//            access_link->index = access_linkCount++;
//
//            access_link->nextLink = access_links;
//            access_links = access_link;
//          }
//        }
//
//        idAst->id.access_link = access_link;
//
//        nonLocalsList = nonLocalsList->nextItem;
//      }
//      actv_rec->access_links = access_links;
//      actv_rec->access_linkCount = access_linkCount;
//
//      if(success)
//      {
//        Ast_ListItem* stmt_list = block->stmt_list;
//        while(stmt_list && success)
//        {
//          AstNode* stmtAst = stmt_list->ast;
//          success = BuildIr(arena, symbol_table, stmtAst);
//          stmt_list = stmt_list->nextItem;
//        }
//      }
//    } break;
//
//    case Ast_Return:
//    {
//      AstNode* exprAst = ast->ret.expr;
//      success = BuildIr(arena, symbol_table, exprAst);
//      if(success)
//        ast->ret.actv_rec = &ast->ret.block->actv_rec;
//    } break;
//
//    case Ast_Expr:
//    {
//      AstNode* left_operand = ast->expr.left_operand;
//      AstNode* right_operand = ast->expr.right_operand;
//      success = BuildIr(arena, symbol_table, left_operand);
//      if(right_operand)
//        success &= BuildIr(arena, symbol_table, right_operand);
//    } break;
//
//    case Ast_Id:
//    case Ast_Var:
//    case Ast_IntNum:
//    case Ast_Neg:
//    {} break;
//
//    default:
//      assert(false && !"Not implemented");
//  }
//
//  return success;
//}/*<<<*/
//
//void gen_codeLValue(VmProgram* vm_program, AstNode* ast)
//{/*>>>*/
//  switch(ast->kind)
//  {
//    case Ast_Id:
//      {
//        Symbol* symbol = ast->id.symbol;
//        DataObj* dataObj = &symbol->dataObj;
//
//        if(ast->id.is_non_local)
//        {
//          AccessLink* access_link = ast->id.access_link;
//
//          Emit(vm_program, ";begin load l-value of non-local '%s'", symbol->name);
//          Emit(vm_program, "push fp");
//          int access_link_loc = 2 + access_link->index;
//          Emit(vm_program, "push -%d", access_link_loc);
//          Emit(vm_program, "add");
//          Emit(vm_program, "load ;access link"); // access link is on the stack now
//        } else
//        {
//          Emit(vm_program, ";begin load l-value of local '%s'", symbol->name);
//          Emit(vm_program, "push fp");
//        }
//        Emit(vm_program, "push %d", dataObj->loc);
//        Emit(vm_program, "add");
//        Emit(vm_program, ";end load of l-value");
//      } break;
//
//    default:
//      assert(false);
//  }
//}/*<<<*/
//
//void gen_codeRValue(VmProgram* vm_program, AstNode* ast)
//{/*>>>*/
//  switch(ast->kind)
//  {
//    case Ast_Id:
//      {
//        gen_codeLValue(vm_program, ast);
//        Emit(vm_program, "load ;r-value");
//      } break;
//
//    case Ast_Expr:
//      {
//        switch(ast->expr.op)
//        {
//          case Operator_Call:
//            {
//              AstNode* callAst = ast->expr.left_operand;
//              Symbol* procSymbol = callAst->call.symbol;
//
//              assert(callAst->kind == Ast_Call);
//              assert(procSymbol->kind == SymbolKind_Proc);
//
//              Emit(vm_program, "push 0 ;retval of %s", callAst->call.name);
//
//              Emit(vm_program, ";begin arg-eval");
//              Ast_ListItem* argList = callAst->call.argList;
//              while(argList)
//              {
//                AstNode* argAst = argList->ast;
//                gen_codeRValue(vm_program, argAst);
//                argList = argList->nextItem;
//              }
//              Emit(vm_program, ";end arg-eval");
//
//              Emit(vm_program, "call %s", callAst->call.name);
//
//              ActivationRecord* actv_rec = callAst->call.actv_rec;
//              assert(actv_rec->kind == ActvRecord_Proc);
//              int restoreSp = actv_rec->proc.args_areaSize;
//
//              if(ast->expr.is_statement)
//                restoreSp += 1; // discard retval
//              if(restoreSp > 0)
//                Emit(vm_program, "pop %d ;restore callee sp", restoreSp);
//            } break;
//
//          case Operator_Assign:
//            {
//              AstNode* right_side = ast->expr.right_operand;
//              gen_codeRValue(vm_program, right_side);
//
//              AstNode* leftSide = ast->expr.left_operand;
//              assert(leftSide->kind == Ast_Id);
//              gen_codeLValue(vm_program, leftSide);
//              Emit(vm_program, "store ;'%s'", leftSide->id.name);
//
//              if(ast->expr.is_statement)
//                Emit(vm_program, "pop"); // discard the r-value
//            } break;
//
//          case Operator_Mul:
//          case Operator_Add:
//          case Operator_Div:
//          case Operator_Sub:
//            {
//              AstNode* left_operand = ast->expr.left_operand;
//              gen_codeRValue(vm_program, left_operand);
//              AstNode* right_operand = ast->expr.right_operand;
//              gen_codeRValue(vm_program, right_operand);
//
//              if(ast->expr.op == Operator_Mul)
//                Emit(vm_program, "mul");
//              else if(ast->expr.op == Operator_Add)
//                Emit(vm_program, "add");
//              else if(ast->expr.op == Operator_Div)
//                Emit(vm_program, "div");
//              else if(ast->expr.op == Operator_Sub)
//                Emit(vm_program, "sub");
//              else
//                assert(false);
//            } break;
//        }
//      } break;
//
//    case Ast_IntNum:
//      {
//        Emit(vm_program, "push %d", ast->literal.int_num);
//      } break;
//
//    case Ast_Neg:
//      {
//        AstNode* expr = ast->neg.expr;
//        if(expr->kind == Ast_IntNum)
//        {
//          Emit(vm_program, "push -%d", expr->literal.int_num);
//        } else {
//          gen_codeRValue(vm_program, expr);
//          Emit(vm_program, "push -1");
//          Emit(vm_program, "mul");
//        }
//      } break;
//
//    default:
//      assert(false);
//  }
//}/*<<<*/
//
//void gen_code(VmProgram* vm_program, SymbolTable* symbol_table,
//             Block* block, AstNode* ast)
//{/*>>>*/
//  switch(ast->kind)
//  {
//    case Ast_IntNum:
//    case Ast_Expr:
//    case Ast_Id:
//      {
//        gen_codeRValue(vm_program, ast);
//      } break;
//
//    case Ast_Var:
//      {} break;
//
//    case Ast_Block:
//      {
//        Block* block = ast->block;
//
//        switch(block->kind)
//        {
//          case Block_Module:
//            {
//              Emit(vm_program, "push 0 ;main retval");
//              Emit(vm_program, "call main");
//              Emit(vm_program, "halt");
//
//              Ast_ListItem* proc_list = block->module.proc_list;
//              while(proc_list)
//              {
//                AstNode* procAst = proc_list->ast;
//                gen_code(vm_program, symbol_table, block, procAst);
//
//                proc_list = proc_list->nextItem;
//              }
//            } break;
//
//          case Block_Proc:
//            {
//              Symbol* procSymbol = block->proc.symbol;
//              Emit(vm_program, "label %s", procSymbol->name); // entry point
//
//              ActivationRecord* actv_rec = &block->actv_rec;
//              int dataAreaSize = actv_rec->localAreaSize;
//              if(dataAreaSize > 0)
//                Emit(vm_program, "alloc %d ;local storage", dataAreaSize);
//
//              Ast_ListItem* stmt_list = block->stmt_list;
//              while(stmt_list)
//              {
//                AstNode* stmt = stmt_list->ast;
//                gen_code(vm_program, symbol_table, block, stmt);
//
//                stmt_list = stmt_list->nextItem;
//              }
//
//              Emit(vm_program, "label %s.end-proc", procSymbol->name);
//              Emit(vm_program, "return");
//            } break;
//
//          case Block_WhileStmt:
//            {
//              ActivationRecord* actv_rec = &block->actv_rec;
//
//              char label[32] = {};
//              gen_unique_label(symbol_table, label);
//              Emit(vm_program, "label %s.while-expr", label);
//
//              // conditional expr
//              gen_codeRValue(vm_program, block->while_stmt.expr);
//              Emit(vm_program, "jumpz %s.while-break", label);
//
//              if(actv_rec->access_linkCount > 0)
//              {
//                // Highest indexed access link is first from the top
//                Emit(vm_program, ";begin set-up of access links");
//
//                AccessLink* access_link = actv_rec->access_links;
//                while(access_link)
//                {
//                  Emit(vm_program, "push fp"); // first level up is the caller's activ. record
//
//                  assert(access_link->actv_rec_offset > 0);
//                  int offset = access_link->actv_rec_offset;
//                  offset--;
//                  while(offset--)
//                  {
//                    Emit(vm_program, "decr"); // offset to the fp of actv. record n-1
//                    Emit(vm_program, "load");
//                  }
//
//                  access_link = access_link->nextLink;
//                }
//                Emit(vm_program, ";end set-up of access links");
//              }
//
//              Emit(vm_program, "enter");
//
//              if(actv_rec->localAreaSize > 0)
//                Emit(vm_program, "alloc %d ;local storage", actv_rec->localAreaSize);
//
//              // body
//              Ast_ListItem* stmt_list = block->stmt_list;
//              while(stmt_list)
//              {
//                AstNode* stmt = stmt_list->ast;
//                gen_code(vm_program, symbol_table, block, stmt);
//
//                stmt_list = stmt_list->nextItem;
//              }
//
//              Emit(vm_program, "leave");
//              Emit(vm_program, "pop %d ;discard access links", actv_rec->access_linkCount);
//
//              Emit(vm_program, "goto %s.while-expr", label);
//              Emit(vm_program, "label %s.while-break", label);
//            } break;
//
//          default:
//            assert(false && !"Not implemented");
//        }
//      } break;
//
//    case Ast_Call:
//      {
//        Emit(vm_program, "call %s", ast->call.name);
//      } break;
//
//    case Ast_Return:
//      {
//        gen_codeRValue(vm_program, ast->ret.expr);
//
//        ActivationRecord* actv_rec = ast->ret.actv_rec;
//        assert(actv_rec->kind == ActvRecord_Proc);
//
//        //FIXME: 'retval' is a DataObj
//        int retValloc = CONTROL_LINKS_DATA_SIZE + actv_rec->proc.args_areaSize +
//          actv_rec->proc.retAreaSize;
//
//        // Load l-value of the 'ret' data object
//        Emit(vm_program, "push fp");
//        assert(ast->ret.intervening_block_count >= 0);
//        int level = ast->ret.intervening_block_count;
//        while(level--)
//        {
//          Emit(vm_program, "decr"); // offset to the fp of actv. record n-1
//          Emit(vm_program, "load");
//        }
//        //--
//
//        Emit(vm_program, "push %d ;loc of retval", -retValloc); // note the negative sign
//        Emit(vm_program, "add");
//        Emit(vm_program, "store ;retval");
//
//        // Exit from the enclosing procedure and any of the intervening blocks
//        Block* block = ast->ret.block; 
//        Symbol* procSymbol = block->proc.symbol;
//        int depth = ast->ret.intervening_block_count;
//        assert(depth >= 0);
//        while(depth--)
//          Emit(vm_program, "leave");
//        Emit(vm_program, "goto %s.end-proc", procSymbol->name);
//      } break;
//#if 0
//    case Ast_IfStmt:
//      {
//        Emit(vm_program, ";if-begin");
//
//        // conditional
//        gen_codeRValue(vm_program, ast->if_stmt.expr);
//
//        char* label = gen_unique_label(symbol_table);
//
//        if(ast->if_stmt.bodyElse)
//          Emit(vm_program, "jumpz %s.else", label);
//        else
//          Emit(vm_program, "jumpz %s.if-end", label);
//
//        gen_code(vm_program, symbol_table, ast->if_stmt.body);
//        if(ast->if_stmt.bodyElse)
//        {
//          Emit(vm_program, "goto %s.if-end", label);
//          Emit(vm_program, "label %s.else", label);
//          gen_code(vm_program, symbol_table, ast->if_stmt.bodyElse);
//        }
//
//        Emit(vm_program, "label %s.if-end", label);
//
//      } break;
//
//    case Ast_WhileStmt:
//      {
//      } break;
//#endif
//    default:
//      assert(false && !"Not implemented");
//  }
//}/*<<<*/
/*<<<*/

uint write_bytes_to_file(char* fileName, char* text, int count)
{/*>>>*/
  uint bytesWritten = 0;
  FILE* hFile = fopen(fileName, "wb");
  if(hFile)
  {
    bytesWritten = (uint)fwrite(text, 1, count, hFile);
    fclose(hFile);
  }
  return bytesWritten;
}/*<<<*/

bool32 translate_hoc(MemoryArena* arena, char* file_path, char* hocProgram, VmProgram* vm_program)
{/*>>>*/
  bool32 success = false;

  SymbolTable symbol_table = {0};
  symbol_table.arena = arena;

  TokenStream tokenStream = {0};
  tokenStream.arena = arena;
  tokenStream.text = hocProgram;
  tokenStream.cursor = tokenStream.text;
  tokenStream.line_nr = 1;
  //TODO: Compute the absolute path to the file, so that Vim could properly
  // jump from the QuickFix window to the error line in the file.
  tokenStream.file_path = file_path;

  register_keywords(&symbol_table);
  consume_token(&tokenStream, &symbol_table);

  AstNode* ast_node = 0;
  success = parse_module(arena, &tokenStream, &symbol_table, &ast_node);

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

      string_init(&vm_program->text, arena);
      print_code(vm_program);
    }
    else
      success = false;
  }

  return success;
}/*<<<*/

