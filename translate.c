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
  OperatorKind__Null,
  OperatorKind_Add,
  OperatorKind_Sub,
  OperatorKind_Assign,
  OperatorKind_Div,
  OperatorKind_Mul,
  OperatorKind_Call,
  OperatorKind_Neg,
}
OperatorKind;

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
  int actv_rec_offset; // must be greater than zero
  int index;
}
AccessLink;

typedef struct
{
  List     proc_list;
  AstNode* body;
}
Module;

typedef struct
{
  Symbol* symbol;
  char*   name;
  bool32  is_argument;
  int     location; // relative to fp
  int     data_size;
}
VarDecl;

typedef struct
{
  OperatorKind op;
  AstNode* left_operand;
  AstNode* right_operand;
  bool32   is_statement;
}
BinExpr;

typedef struct
{
  OperatorKind op;
  AstNode* operand;
}
UnrExpr;

typedef struct
{
  Symbol*     symbol;
  char*       name;
  int         decl_block_offset;
  bool32      is_non_local;
  bool32      is_argument;
  AccessLink* access_link; // if non-local
  VarDecl*    var_decl;
}
VarOccur;

typedef struct
{
  int32 value;
}
IntNum;

typedef struct
{
  AstNode* expr;
  int      intervening_block_count;
  AstNode* proc;
}
ReturnStmt;

typedef struct IrProc_ IrProc;
typedef struct
{
  Symbol*  symbol;
  char*    name;
  List     formal_args;
  AstNode* body;
  int      args_data_size;
  int      ret_data_size;
  IrProc   *ir_proc;
}
Proc;

typedef struct
{
  Symbol* symbol;
  char*   name;
  List    actual_args;
  Proc*   proc;
  bool32  is_statement;
}
Call;

typedef struct
{
  AstNode* expr;
  AstNode* body;
  AstNode* else_node;
}
IfStmt;

typedef struct
{
  AstNode* expr;
  AstNode* body;
}
WhileStmt;

typedef struct Block_
{
  AstNode* owner;
  int      block_id;
  int      nesting_depth;
  List     decl_vars;
  List     local_occurs;
  List     non_local_occurs;
  List     stmtList;
  struct   Block_* enclosing_block;
  int      locals_data_size;
  List     access_links;
  int      access_links_area_size;
}
Block;

typedef struct AstNode_
{
  AstNodeKind  kind;

  union {
    BinExpr    bin_expr;
    UnrExpr    unr_expr;
    VarDecl    var_decl;
    VarOccur   var_occur;
    IntNum     int_num;
    Call       call;
    Proc       proc;
    Module     module;
    Block      block;
    ReturnStmt ret_stmt;
    WhileStmt  while_stmt;
    IfStmt     if_stmt;
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
    VarDecl* var;
    Proc*    proc;
    Token    kw_token;
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

void block_init(SymbolTable* symbol_table, Block* block)
{/*>>>*/
  block->block_id = symbol_table->scope_id;
  block->nesting_depth = symbol_table->nesting_depth;

  list_init(&block->local_occurs);
  list_init(&block->non_local_occurs);
  list_init(&block->stmtList);
  list_init(&block->decl_vars);
  list_init(&block->access_links);
}/*<<<*/

void block_process_vars(MemoryArena* arena, Block* block)
{/*>>>*/
  /*>>> Process declared vars */
  ListItem* nodeItem = list_first_item(&block->decl_vars);
  while(nodeItem)
  {
    AstNode* node = nodeItem->elem;
    VarDecl* var_decl = &node->var_decl;

    var_decl->location = block->locals_data_size;
    block->locals_data_size += var_decl->data_size;

    nodeItem = nodeItem->next;
  }
  /*<<<*/
  /*>>> Process non-local var occurrences */
  nodeItem = list_first_item(&block->non_local_occurs);
  while(nodeItem)
  {
    AstNode* node = nodeItem->elem;
    VarOccur* var_occur = &node->var_occur;
    List* linksList = &block->access_links;

    ListItem* link_item = list_first_item(&block->access_links);
    AccessLink* link = 0;
    while(link_item)
    {
      link = link_item->elem;
      if(link->actv_rec_offset == var_occur->decl_block_offset)
        break;
      link_item = link_item->next;
    }
    if(!link)
    {
      link = push_element(arena, AccessLink, 1);
      link->actv_rec_offset = var_occur->decl_block_offset;
      link->index = linksList->count;
      list_append(arena, linksList, link);
    }
    block->access_links_area_size = linksList->count;
  }
  /*<<<*/
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
Symbol* SymbolLookup(SymbolTable* symbol_table, char* name)
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

Symbol* SymbolAdd(SymbolTable* symbol_table, char* name, SymbolKind kind)
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

Symbol* SymbolRegisterNew(SymbolTable* symbol_table, TokenStream* input, SymbolKind kind)
{/*>>>*/
  assert(input->token == Token_Id);

  Symbol* result = 0;

  Symbol* symbol = SymbolLookup(symbol_table, input->lexval.id);
  if(!symbol)
  {
    result = SymbolAdd(symbol_table, input->lexval.id, kind);
  } else {
    if(symbol->kind != SymbolKind_Keyword)
    {
      if(symbol->block_id != symbol_table->scope_id ||
          symbol->kind != kind)
      {
        assert(symbol->nesting_depth <= symbol_table->nesting_depth);

        result = SymbolAdd(symbol_table, input->lexval.id, kind);
      } else
        syntax_error(input, "Redeclaration of identifier: %s", symbol->name);
    } else
      syntax_error(input, "Keyword used as identifier: %s", symbol->name);
  }
  return result;
}/*<<<*/

Symbol* AddKeyword(SymbolTable* symbol_table, char* name, Token token)
{/*>>>*/
  Symbol* symbol = SymbolAdd(symbol_table, name, SymbolKind_Keyword);
  symbol->kw_token = token;
  return symbol;
}/*<<<*/

void RegisterKeywords(SymbolTable* symbol_table)
{/*>>>*/
  AddKeyword(symbol_table, "int", Token_If);
  AddKeyword(symbol_table, "float", Token_Float);
  AddKeyword(symbol_table, "void", Token_Void);
  AddKeyword(symbol_table, "char", Token_Char);
  AddKeyword(symbol_table, "var", Token_Var);
  AddKeyword(symbol_table, "proc", Token_Proc);
  AddKeyword(symbol_table, "type", Token_Type);
  AddKeyword(symbol_table, "struct", Token_Type);
  AddKeyword(symbol_table, "array", Token_Array);
  AddKeyword(symbol_table, "of", Token_Of);
  AddKeyword(symbol_table, "if", Token_If);
  AddKeyword(symbol_table, "else", Token_Else);
  AddKeyword(symbol_table, "while", Token_While);
  AddKeyword(symbol_table, "return", Token_Return);
  AddKeyword(symbol_table, "true", Token_True);
  AddKeyword(symbol_table, "false", Token_False);
}/*<<<*/

bool32 ScopeBegin(SymbolTable* symbol_table)
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

void ScopeEnd(SymbolTable* symbol_table)
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

void make_unique_label(SymbolTable* symbol_table, char* label)
{
  sprintf(label, "L%d", symbol_table->last_label_id++);
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
  memset(&input->lexval, 0, sizeof(input->lexval));

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

    Symbol* symbol = SymbolLookup(symbol_table, lexeme);
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
bool32 parse_expression(MemoryArena*, TokenStream*, SymbolTable*, Block*, AstNode**);
bool32 parse_statement_list(MemoryArena*, TokenStream*, SymbolTable*, Block*);
bool32 parse_actual_argument_list(MemoryArena*, TokenStream*, SymbolTable*, Block*, Call*);
bool32 parse_term(MemoryArena*, TokenStream*, SymbolTable*, Block*, AstNode**);

bool32 parse_factor(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                   Block* enclosing_block, AstNode** node)
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
        UnrExpr* expr = &negNode->unr_expr;
        expr->op = OperatorKind_Neg;
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
    Symbol* symbol = SymbolLookup(symbol_table, input->lexval.id);
    if(symbol)
    {
      AstNode* idNode = push_element(arena, AstNode, 1);
      *node = idNode;

      consume_token(input, symbol_table);

      if(symbol->kind == SymbolKind_Var)
      {
        idNode->kind = AstNodeKind_VarOccur;
        VarOccur* var_occur = &idNode->var_occur;
        var_occur->symbol = symbol;
        var_occur->name = symbol->name;
        var_occur->var_decl = symbol->var;
        var_occur->is_argument = symbol->var->is_argument;
        var_occur->decl_block_offset = (symbol_table->nesting_depth - symbol->nesting_depth);
        var_occur->is_non_local = (var_occur->decl_block_offset > 0);

        if(var_occur->is_non_local)
        {
          list_append(arena, &enclosing_block->non_local_occurs, idNode);
        }
        else
        {
          assert(var_occur->decl_block_offset == 0);
          list_append(arena, &enclosing_block->local_occurs, idNode);
        }
      }
      else if(symbol->kind == SymbolKind_Proc)
      {
        idNode->kind = AstNodeKind_Call;
        Call* call = &idNode->call;
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
                          Block* enclosing_block, AstNode* leftNode, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Star ||
      input->token == Token_FwdSlash)
  {
    AstNode* exprNode = push_element(arena, AstNode, 1);
    exprNode->kind = AstNodeKind_BinExpr;
    BinExpr* expr = &exprNode->bin_expr;
    if(input->token == Token_Star)
      expr->op = OperatorKind_Mul;
    else if(input->token == Token_FwdSlash)
      expr->op = OperatorKind_Div;
    else
      assert(false);

    consume_token(input, symbol_table);
    AstNode* factorNode = 0;
    success = parse_factor(arena, input, symbol_table, enclosing_block, &factorNode);

    if(success && factorNode)
    {
      expr->right_operand = factorNode;
      expr->left_operand = leftNode;
      success = parse_rest_of_factors(arena, input, symbol_table,
                                   enclosing_block, exprNode, node);
    } else {
      syntax_error(input, "Factor expected");
      success = false;
    }
  }
  else
    *node = leftNode;

  return success;
}/*<<<*/

bool32 parse_term(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                 Block* enclosing_block, AstNode** node)
{/*>>>*/
  AstNode* factorNode = 0;
  AstNode* exprNode = 0;

  bool32 success = parse_factor(arena, input, symbol_table, enclosing_block, &factorNode);
  if(success && factorNode)
    success = parse_rest_of_factors(arena, input, symbol_table,
                                 enclosing_block, factorNode, &exprNode);

  *node = exprNode;
  return success;
}/*<<<*/

bool32 parse_rest_of_terms(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                        Block* enclosing_block, AstNode* leftNode, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Plus ||
      input->token == Token_Minus)
  {
    AstNode* exprNode = push_element(arena, AstNode, 1);
    exprNode->kind = AstNodeKind_BinExpr;
    BinExpr* expr = &exprNode->bin_expr;
    if(input->token == Token_Plus)
      expr->op = OperatorKind_Add;
    else if(input->token == Token_Minus)
      expr->op = OperatorKind_Sub;
    else
      assert(false);

    consume_token(input, symbol_table);
    AstNode* termNode = 0;
    success = parse_term(arena, input, symbol_table, enclosing_block, &termNode);

    if(success && termNode)
    {
      expr->right_operand = termNode;
      expr->left_operand = leftNode;
      success = parse_rest_of_terms(arena, input, symbol_table, enclosing_block, exprNode, node);
    } else {
      syntax_error(input, "Expression term expected");
      success = false;
    }
  }
  else
    *node = leftNode;

  return success;
}/*<<<*/

bool32 parse_assignment_term(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                           Block* enclosing_block, AstNode** node)
{/*>>>*/
  AstNode* termNode = 0;
  AstNode* exprNode = 0;

  bool32 success = parse_term(arena, input, symbol_table, enclosing_block, &termNode);
  if(success && termNode)
    success = parse_rest_of_terms(arena, input, symbol_table, enclosing_block, termNode, &exprNode);

  *node = exprNode;
  return success;
}/*<<<*/

bool32 parse_rest_of_assignment_terms(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                                  Block* enclosing_block, AstNode* leftNode, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Equals)
  {
    consume_token(input, symbol_table);
    AstNode* rightSide = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &rightSide);
    if(success)
    {
      if(rightSide)
      {
        if(leftNode->kind == AstNodeKind_VarOccur)
        {
          AstNode* exprNode = push_element(arena, AstNode, 1);
          exprNode->kind = AstNodeKind_BinExpr;
          BinExpr* expr = &exprNode->bin_expr;
          expr->op = OperatorKind_Assign;
          expr->left_operand = leftNode;
          expr->right_operand = rightSide;

          *node = exprNode;
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
    *node = leftNode;

  return success;
}/*<<<*/

bool32 parse_expression(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                       Block* enclosing_block, AstNode** node)
{/*>>>*/
  AstNode* assgnNode = 0;
  AstNode* exprNode = 0;

  bool32 success = parse_assignment_term(arena, input, symbol_table, enclosing_block, &assgnNode);
  if(success && assgnNode)
    success = parse_rest_of_assignment_terms(arena, input, symbol_table,
                                         enclosing_block, assgnNode, &exprNode);

  *node = exprNode;
  return success;
}/*<<<*/

bool32 parse_var_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                         Block* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Var)
  {
    consume_token(input, symbol_table);
    if(input->token == Token_Id)
    {
      AstNode* varNode = push_element(arena, AstNode, 1);
      varNode->kind = AstNodeKind_VarDecl;
      *node = varNode;

      Symbol* symbol = SymbolRegisterNew(symbol_table, input, SymbolKind_Var);
      if(symbol)
      {
        VarDecl* var_decl = &varNode->var_decl;
        var_decl->symbol = symbol;
        var_decl->name = symbol->name;
        var_decl->data_size = 1;
        symbol->var = var_decl;

        consume_token(input, symbol_table);
      }
    } else {
      syntax_error(input, "Missing identifier");
      success = false;
    }
  }

  return success;
}/*<<<*/

bool32 parse_formal_argument(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                           Block* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Var)
  {
    consume_token(input, symbol_table);
    if(input->token == Token_Id)
    {
      AstNode* varNode = push_element(arena, AstNode, 1);
      varNode->kind = AstNodeKind_VarDecl;
      *node = varNode;

      Symbol* symbol = SymbolRegisterNew(symbol_table, input, SymbolKind_Var);
      if(symbol)
      {
        VarDecl* var_decl = &varNode->var_decl;
        var_decl->symbol = symbol;
        var_decl->name = symbol->name;
        var_decl->is_argument = true;
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

bool32 parse_formal_argumentList(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                               Block* enclosing_block, Proc* proc)
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
      success = parse_formal_argumentList(arena, input, symbol_table, enclosing_block, proc);
    }
  }

  return success;
}/*<<<*/

bool32 parse_actual_argument_list(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                               Block* enclosing_block, Call* call)
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
                           Block* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_While)
  {
    consume_token(input, symbol_table);
    AstNode* exprNode = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &exprNode);
    if(success)
    {
      AstNode* whileNode = push_element(arena, AstNode, 1);
      whileNode->kind = AstNodeKind_WhileStmt;
      WhileStmt* while_stmt = &whileNode->while_stmt;
      while_stmt->expr = exprNode;
      *node = whileNode;

      if(input->token == Token_OpenBrace)
      {
        consume_token(input, symbol_table);

        success = ScopeBegin(symbol_table);
        if(success)
        {
          AstNode* blockNode = push_element(arena, AstNode, 1);
          blockNode->kind = AstNodeKind_Block;
          Block* block = &blockNode->block;
          block->owner = whileNode;
          block_init(symbol_table, block);

          success = parse_statement_list(arena, input, symbol_table, block);
          if(success)
          {
            while_stmt->body = blockNode;

            if(input->token == Token_CloseBrace)
            {
              consume_token(input, symbol_table);
              ScopeEnd(symbol_table);
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

bool32 parse_if_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                        Block* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_If)
  {
    consume_token(input, symbol_table);
    AstNode* exprNode = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &exprNode);
    if(success)
    {
      AstNode* ifNode = push_element(arena, AstNode, 1);
      ifNode->kind = AstNodeKind_IfStmt;
      IfStmt* if_stmt = &ifNode->if_stmt;
      if_stmt->expr = exprNode;
      *node = ifNode;

      if(input->token == Token_OpenBrace)
      {
        consume_token(input, symbol_table);

        success = ScopeBegin(symbol_table);
        if(success)
        {
          AstNode* blockNode = push_element(arena, AstNode, 1);
          blockNode->kind = AstNodeKind_Block;
          Block* block = &blockNode->block;
          block->owner = ifNode;
          block_init(symbol_table, block);

          success = parse_statement_list(arena, input, symbol_table, block);
          if(success)
          {
            if_stmt->body = blockNode;

            if(input->token == Token_CloseBrace)
            {
              consume_token(input, symbol_table);
              ScopeEnd(symbol_table);

              if(input->token == Token_Else)
              {
                consume_token(input, symbol_table);
                AstNode* else_node = 0;
                success = parse_if_statement(arena, input, symbol_table, enclosing_block, &else_node);
                if(success)
                {
                  if(else_node)
                    if_stmt->else_node = else_node;
                  else if(input->token == Token_OpenBrace)
                  {
                    consume_token(input, symbol_table);

                    success = ScopeBegin(symbol_table);
                    if(success)
                    {
                      AstNode* blockNode = push_element(arena, AstNode, 1);
                      blockNode->kind = AstNodeKind_Block;
                      Block* block = &blockNode->block;
                      block->owner = ifNode;
                      block_init(symbol_table, block);

                      success = parse_statement_list(arena, input, symbol_table, block);
                      if(success)
                      {
                        if_stmt->else_node = blockNode;
                        if(input->token == Token_CloseBrace)
                        {
                          consume_token(input, symbol_table);
                          ScopeEnd(symbol_table);
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
      } else {
        syntax_error(input, "Missing '{'");
        success = false;
      }
    }
  }
  return success;
}/*<<<*/

bool32 parse_procedure(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                      Block* enclosing_block, AstNode** node)
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
      Symbol* symbol = SymbolRegisterNew(symbol_table, input, SymbolKind_Proc);
      if(symbol)
      {
        Proc* proc = &procNode->proc;
        proc->symbol = symbol;
        proc->name   = symbol->name;
        proc->ret_data_size = 1;
        list_init(&proc->formal_args);
        symbol->proc = proc;

        consume_token(input, symbol_table);
        if(input->token == Token_OpenParens)
        {
          consume_token(input, symbol_table);

          // arguments
          success = ScopeBegin(symbol_table);
          if(success)
          {
            AstNode* blockNode = push_element(arena, AstNode, 1);
            blockNode->kind = AstNodeKind_Block;
            proc->body = blockNode;
            Block* block = &blockNode->block;
            block->owner = procNode;
            block_init(symbol_table, block);

            success = parse_formal_argumentList(arena, input, symbol_table, block, proc);
            if(success)
            {
              ListItem* argItem = list_first_item(&proc->formal_args);
              while(argItem)
              {
                AstNode* node = argItem->elem;
                assert(node->kind == AstNodeKind_VarDecl);
                VarDecl* arg = &node->var_decl;
                Symbol* symbol = node->var_decl.symbol;
                symbol->var = arg;

                arg->location = proc->args_data_size;
                arg->data_size = 1;
                proc->args_data_size += arg->data_size;

                argItem = argItem->next;
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
                    block_process_vars(arena, block);

                    if(input->token == Token_CloseBrace)
                    {
                      consume_token(input, symbol_table);
                      ScopeEnd(symbol_table); // body
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
                          Block* enclosing_block, Module* module)
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
                            Block* enclosing_block, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Return)
  {
    consume_token(input, symbol_table);
    AstNode* exprNode = 0;
    success = parse_expression(arena, input, symbol_table, enclosing_block, &exprNode);
    if(success)
    {
      if(exprNode)
      {
        AstNode* retNode = push_element(arena, AstNode, 1);
        retNode->kind = AstNodeKind_ReturnStmt;
        ReturnStmt* ret_stmt = &retNode->ret_stmt;
        ret_stmt->expr = exprNode;
        *node = retNode;

        int depth = 0;
        Block* block = enclosing_block;
        while(block)
        {
          AstNode* owner = block->owner;
          if(owner->kind == AstNodeKind_Proc)
            break;
          depth++;
          block = block->enclosing_block;
        }
        assert(block);
        ret_stmt->proc = block->owner;
        ret_stmt->intervening_block_count = depth;
      } else {
        syntax_error(input, "Expression required after the 'return' keyword");
        success = false;
      }
    }
  }

  return success;
}/*<<<*/

bool32 parse_statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                      Block* enclosing_block, AstNode** node)
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
  AstNode* stmtNode = 0;

  while(alt) {
    switch(alt) {
      case Alt_Expr:
      {
        success = parse_expression(arena, input, symbol_table, enclosing_block, &stmtNode);
        if(success)
        {
          if(stmtNode)
          {
            alt = Alt__Null;
            if(input->token == Token_Semicolon)
            {
              consume_token(input, symbol_table);

              if(stmtNode->kind == AstNodeKind_BinExpr)
              {
                BinExpr* expr = &stmtNode->bin_expr;
                if(expr->op == OperatorKind_Assign)
                  expr->is_statement = true;
                else {
                  syntax_error(input, "Assignment expression required");
                  success = false;
                }
              }
              else if(stmtNode->kind == AstNodeKind_Call)
              {
                Call* call = &stmtNode->call;
                call->is_statement = true;
              }
              else {
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
        success = parse_if_statement(arena, input, symbol_table, enclosing_block, &stmtNode);
        if(success)
        {
          if(stmtNode)
          {
            alt = Alt__Null;
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_While:
      {
        success = parse_while_statement(arena, input, symbol_table, enclosing_block, &stmtNode);
        if(success)
        {
          if(stmtNode)
          {
            alt = Alt__Null;
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_Return:
      {
        success = parse_return_statement(arena, input, symbol_table, enclosing_block, &stmtNode);
        if(success)
        {
          if(stmtNode)
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
        success = parse_var_statement(arena, input, symbol_table, enclosing_block, &stmtNode);
        if(success)
        {
          if(stmtNode)
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

  *node = stmtNode;
  return success;
}/*<<<*/

bool32 parse_statement_list(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                          Block* block)
{/*>>>*/
  bool32 success = true;

  AstNode* stmtNode = 0;
  success = parse_statement(arena, input, symbol_table, block, &stmtNode);
  if(success && stmtNode)
  {
    while(input->token == Token_Semicolon)
      consume_token(input, symbol_table);

    if(stmtNode->kind == AstNodeKind_VarDecl)
      list_append(arena, &block->decl_vars, stmtNode);
    else
      list_append(arena, &block->stmtList, stmtNode);

    success = parse_statement_list(arena, input, symbol_table, block); //FIXME: Is this tail-recursion - can it be optimized?
  }
  return success;
}/*<<<*/

bool32 parse_module(MemoryArena* arena, TokenStream* input, SymbolTable* symbol_table,
                   AstNode** node)
{/*>>>*/
  bool32 success = true;

  success = ScopeBegin(symbol_table);
  if(success)
  {
    AstNode* moduleNode = push_element(arena, AstNode, 1);
    moduleNode->kind = AstNodeKind_Module;

    AstNode* blockNode = push_element(arena, AstNode, 1);
    blockNode->kind = AstNodeKind_Block;
    Block* block = &blockNode->block;
    block->owner = moduleNode;
    block_init(symbol_table, block);

    Module* module = &moduleNode->module;
    module->body = blockNode;
    list_init(&module->proc_list);

    success = parse_procedureList(arena, input, symbol_table, block, module);
    if(success)
    {
      ScopeEnd(symbol_table);

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
  IrNodeKind_LoadRValue,
  IrNodeKind_LoadLValue,
  IrNodeKind_BinOp,
  IrNodeKind_Proc,
  IrNodeKind_Module,
  IrNodeKind_Call,
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
}
IrOpKind;

typedef struct IrNode_ IrNode;

typedef enum
{
  IrValueKind__Null,
  IrValueKind_Var,
  IrValueKind_IntNum,
  IrValueKind_Call,
}
IrValueKind;

typedef struct
{
  bool32 is_non_local;
  int    access_linkLocation;
  int    data_location;
}
IrVar;

typedef struct
{
  IrProc* proc;
  List    actual_args;
}
IrCall;

typedef struct
{
  IrValueKind kind;

  union {
    int32   int_num;
    IrVar   var;
    IrCall* call;
  };
}
IrValue;

typedef struct
{
  int size;
  int loc;
}
IrStackArea;

typedef enum
{
  IrAvRecord__Null,
  IrAvRecord_Proc,
  IrAvRecord_Block,
}
IrAvRecordKind;

typedef struct
{
  IrStackArea ctrlLinks;
  IrStackArea access_links;
  IrStackArea locals;
  List        foreignAvRecords;
}
IrBlockAvRecord;

typedef struct
{
  IrStackArea ret;
  IrStackArea args;
  IrStackArea ctrlLinks;
  IrStackArea locals;
}
IrProcAvRecord;

typedef struct
{
  union {
    IrProcAvRecord  proc;
    IrBlockAvRecord block;
  };
  IrAvRecordKind kind;
  int fp;
  int sp;
}
IrActivationRecord;

typedef struct
{
  List actv_recStack;
  IrActivationRecord* actv_rec;
  int fp;
  int sp;
}
IrExecutionContext;

typedef struct
{
  IrOpKind op;
  IrNode*  left_operand;
  IrNode*  right_operand;
}
IrBinOp;

typedef struct IrProc_
{
  char* label;
  char* endLabel;
  List  instr_list;
  IrActivationRecord* actv_rec;
}
IrProc;

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
    IrValue  value;
    IrBinOp  op;
    IrProc   proc;
    IrModule module;
    IrCall   call;
  };
} IrNode;

void ir_set_var_value(IrValue* irValue, IrExecutionContext* execContext, VarOccur* var_occur)
{/*>>>*/
  Symbol* symbol = var_occur->symbol;
  IrVar* ir_var = &irValue->var;
  ir_var->is_non_local = var_occur->is_non_local;
  IrActivationRecord* actv_rec = execContext->actv_rec;

  if(var_occur->is_non_local)
  {
    AccessLink* link = var_occur->access_link;
    assert(actv_rec->kind == IrAvRecord_Block);
    IrBlockAvRecord* block_av = &actv_rec->block;
    IrStackArea* access_links_area = &block_av->access_links;
    ir_var->access_linkLocation = -(actv_rec->fp - access_links_area->loc) + link->index;
  } else 
  {
    if(var_occur->is_argument)
    {
      IrProcAvRecord* proc_av = &actv_rec->proc;
      IrStackArea* args_area = &proc_av->args;
      irValue->var.data_location = -(actv_rec->fp - args_area->loc);
    } else
    {
      ir_var->data_location = symbol->var->location;
    }
  }
}/*<<<*/

IrNode* ir_build_lvalue(MemoryArena* arena, IrExecutionContext* execContext, VarOccur* var_occur)
{/*>>>*/
  IrNode* is_node = push_element(arena, IrNode, 1);
  is_node->kind = IrNodeKind_LoadLValue;
  is_node->value.kind = IrValueKind_Var;
  ir_set_var_value(&is_node->value, execContext, var_occur);
  return is_node;
}/*<<<*/

IrNode* ir_build_rvalue(MemoryArena* arena, IrExecutionContext* execContext, AstNode* ast_node)
{/*>>>*/
  IrNode* is_node = push_element(arena, IrNode, 1);
  if(ast_node->kind == AstNodeKind_VarOccur)
  {
    VarOccur* var_occur = &ast_node->var_occur;

    is_node->kind = IrNodeKind_LoadRValue;
    is_node->value.kind = IrValueKind_Var;
    ir_set_var_value(&is_node->value, execContext, var_occur);
  }
  else if(ast_node->kind == AstNodeKind_IntNum)
  {
    is_node->kind = IrNodeKind_LoadRValue;
    is_node->value.kind = IrValueKind_IntNum;
    is_node->value.int_num = ast_node->int_num.value;
  }
  return is_node;
}/*<<<*/

IrNode* ir_build_bin_expr(MemoryArena* arena, IrExecutionContext* execContext, BinExpr* bin_expr)
{/*>>>*/
  IrNode* is_node = 0;
  AstNode* left_operand = bin_expr->left_operand;
  AstNode* right_operand = bin_expr->right_operand;
  if(bin_expr->op == OperatorKind_Assign)
  {
    assert(left_operand->kind == AstNodeKind_VarOccur);
    
    is_node = push_element(arena, IrNode, 1);
    is_node->kind = IrNodeKind_BinOp;
    IrBinOp* ir_op = &is_node->op;
    ir_op->op = IrOpKind_Store;
    ir_op->left_operand = ir_build_lvalue(arena, execContext, &left_operand->var_occur);
    ir_op->right_operand = ir_build_rvalue(arena, execContext, right_operand);
  }
  return is_node;
}/*<<<*/

IrNode* ir_build_call(MemoryArena* arena, IrExecutionContext* execContext, Call* astCall)
{/*>>>*/
  IrNode* ir_call_node = push_element(arena, IrNode, 1);
  ir_call_node->kind = IrNodeKind_Call;
  IrCall* ir_call = &ir_call_node->call;
  IrProc* ir_proc = astCall->proc->ir_proc;
  ir_call->proc = ir_proc;

  list_init(&ir_call->actual_args);
  ListItem* argItem = list_first_item(&astCall->actual_args);
  while(argItem)
  {
    AstNode* astArgNode = argItem->elem;
    IrNode* irArgValue = ir_build_rvalue(arena, execContext, astArgNode);
    list_append(arena, &ir_call->actual_args, irArgValue);
    argItem = argItem->next;
  }

  return ir_call_node;
}/*<<<*/

IrNode* ir_build_call_lvalue(MemoryArena* arena, IrExecutionContext* execContext, Call* astCall)
{/*>>>*/
  IrNode* ir_call_node = ir_build_call(arena, execContext, astCall);
  IrNode* irValueNode = push_element(arena, IrNode, 1);
  irValueNode->kind = IrNodeKind_LoadRValue;
  IrValue* irValue = &irValueNode->value;
  irValue->kind = IrValueKind_Call;
  irValue->call = &ir_call_node->call;
  return irValueNode;
}/*<<<*/

IrNode* ir_build_proc(MemoryArena* arena, IrExecutionContext* execContext, Proc* proc)
{/*>>>*/
  AstNode* bodyNode = proc->body;
  assert(bodyNode->kind == AstNodeKind_Block);
  Block* bodyBlock = &bodyNode->block;

  IrActivationRecord* actv_rec = push_element(arena, IrActivationRecord, 1);
  actv_rec->kind = IrAvRecord_Proc;
  IrProcAvRecord* proc_av = &actv_rec->proc;
  int offset = 0;

  IrStackArea* area = &proc_av->ret;
  area->loc = offset;
  area->size = proc->ret_data_size;
  offset += area->size;

  area = &proc_av->args;
  area->loc = offset;
  area->size = proc->args_data_size;
  offset += area->size;

  area = &proc_av->ctrlLinks;
  area->loc = offset;
  area->size = 3; // ip+sp+fp
  offset += area->size;

  actv_rec->fp = offset;

  area = &proc_av->locals;
  area->loc = offset;
  area->size = bodyBlock->locals_data_size;
  offset += area->size;

  actv_rec->sp = offset;

//  list_init(&actv_rec->foreignAvRecords);
//  {/*>>> access links are not for procs! */
//    ListItem* item = list_first_item(&bodyBlock->access_links);
//    while(item)
//    {
//      AccessLink* link = item->elem;
//      ListItem* avItem = list_first_item(&execContext->avRecordStack);
//      int offset = link->actv_rec_offset - 1;
//      while(offset-- > 0)
//        avItem = avItem->prev;
//      list_append(arena, &actv_rec->foreignAvRecords, avItem->elem);
//
//      item = item->next;
//    }
//  }/*<<<*/

  list_append(arena, &execContext->actv_recStack, actv_rec);
  execContext->actv_rec = actv_rec;

  IrNode* is_node = push_element(arena, IrNode, 1);
  is_node->kind = IrNodeKind_Proc;
  IrProc* ir_proc = &is_node->proc;
  ir_proc->actv_rec = actv_rec;
  ir_proc->label = proc->name;
  list_init(&ir_proc->instr_list);

  String endLabel = {0};
  string_init(&endLabel, arena);
  append_string(&endLabel, proc->name);
  append_string(&endLabel, ".proc-end");
  ir_proc->endLabel = endLabel.start;

  ListItem* nodeItem = list_first_item(&bodyBlock->stmtList);
  while(nodeItem)
  {
    AstNode* ast_node = nodeItem->elem;
    if(ast_node->kind == AstNodeKind_BinExpr)
    {
      BinExpr* bin_expr = &ast_node->bin_expr;
      assert(bin_expr->op == OperatorKind_Assign);
      IrNode* ir_expr = ir_build_bin_expr(arena, execContext, bin_expr);
      list_append(arena, &ir_proc->instr_list, ir_expr);
    }
    else if(ast_node->kind == AstNodeKind_Call)
    {
      Call* call = &ast_node->call;
      IrNode* ir_call = ir_build_call(arena, execContext, call);
      list_append(arena, &ir_proc->instr_list, ir_call);
    }
    nodeItem = nodeItem->next;
  }

  return is_node;
}/*<<<*/
 
IrNode* ir_build_module(MemoryArena* arena, IrExecutionContext* execContext, Module* module)
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
    IrNode* ir_procNode = ir_build_proc(arena, execContext, &ast_proc_node->proc);
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

void print_instruction(VmProgram* vmProgram, char* code, ...)
{/*>>>*/
  static char strbuf[128] = {0};
  va_list args;

  va_start(args, code);
  vmProgram->text_len += vsprintf(strbuf, code, args);
  va_end(args);

  append_string(&vmProgram->text, strbuf);
  append_string(&vmProgram->text, "\n");
  vmProgram->text_len++;
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

void gen_call(MemoryArena* arena, List* code, IrCall* call)
{/*>>>*/
  IrProc* proc = call->proc;
  IrActivationRecord* actv_rec = proc->actv_rec;
  assert(actv_rec->kind == IrAvRecord_Proc);
  if(actv_rec->proc.ret.size > 0)
    emit_instr_int(arena, code, Opcode_ALLOC, actv_rec->proc.ret.size);

  ListItem* argItem = list_first_item(&call->actual_args);
  while(argItem)
  {
    IrNode* argNode = argItem->elem;
    assert(argNode->kind == IrNodeKind_LoadRValue);
    gen_load_rvalue(arena, code, &argNode->value);
    argItem = argItem->next;
  }

  emit_instr_str(arena, code, Opcode_CALL, proc->label);
}/*<<<*/

void gen_load_lvalue(MemoryArena* arena, List* code, IrValue* value)
{/*>>>*/
  assert(value->kind == IrValueKind_Var);
  if(value->var.is_non_local)
  {
    emit_instr_reg(arena, code, Opcode_PUSH, RegName_FP);
    emit_instr_int(arena, code, Opcode_PUSH, value->var.access_linkLocation);
    emit_instr(arena, code, Opcode_ADD);
    emit_instr(arena, code, Opcode_LOAD);
  } else
  {
    emit_instr_reg(arena, code, Opcode_PUSH, RegName_FP);
  }
  emit_instr_int(arena, code, Opcode_PUSH, value->var.data_location);
  emit_instr(arena, code, Opcode_ADD);
}/*<<<*/

void gen_load_rvalue(MemoryArena* arena, List* code, IrValue* value)
{/*>>>*/
  if(value->kind == IrValueKind_Var)
  {
    gen_load_lvalue(arena, code, value);
    emit_instr(arena, code, Opcode_LOAD);
  }
  else if(value->kind == IrValueKind_Call)
  {
    gen_call(arena, code, value->call);
  }
  else if(value->kind == IrValueKind_IntNum)
  {
    emit_instr_int(arena, code, Opcode_PUSH, value->int_num);
  }
}/*<<<*/

void gen_code(MemoryArena* arena, List* code, IrNode* is_node)
{/*>>>*/
  switch(is_node->kind)
  {
    case(IrNodeKind_Module):
    {
      IrModule* module = &is_node->module;

      gen_call(arena, code, module->main_call);
      emit_instr(arena, code, Opcode_HALT);

      ListItem* item = list_first_item(&module->proc_list);
      while(item)
      {
        IrNode* is_node = item->elem;
        gen_code(arena, code, is_node);
        item = item->next;
      }
    } break;

    case(IrNodeKind_Proc):
    {
      IrProc* proc = &is_node->proc;
      emit_instr_str(arena, code, Opcode_LABEL, proc->label);

//      {/*>>> Access links - are not for procs! */
//        IrActivationRecord* actv_rec = proc->actv_rec;
//        ListItem* item = list_first_item(&actv_rec->foreignAvRecords);
//        while(item)
//        {
//          IrActivationRecord* foreignActvRecord = item->elem;
//          int offsetFp = foreignActvRecord->fp - actv_rec->fp;
//          emit_instr_reg(arena, code, Opcode_PUSH, RegName_FP);
//          emit_instr_int(arena, code, Opcode_PUSH, offsetFp);
//          emit_instr(arena, code, Opcode_ADD);
//        }
//      }/*<<<*/

      IrActivationRecord* actv_rec = proc->actv_rec;
      IrStackArea* localsArea = &actv_rec->proc.locals;
      if(localsArea->size > 0)
        emit_instr_int(arena, code, Opcode_ALLOC, localsArea->size);

      {
        ListItem* item = list_first_item(&proc->instr_list);
        while(item)
        {
          IrNode* is_node = item->elem;
          gen_code(arena, code, is_node);
          item = item->next;
        }
      }

      emit_instr_str(arena, code, Opcode_LABEL, proc->endLabel);
      emit_instr(arena, code, Opcode_RETURN);
    } break;

    case(IrNodeKind_LoadLValue):
    {
      IrValue* value = &is_node->value;
      gen_load_lvalue(arena, code, value);
    } break;

    case(IrNodeKind_LoadRValue):
    {
      IrValue* value = &is_node->value;
      gen_load_rvalue(arena, code, value);
    } break;

    case(IrNodeKind_BinOp):
    {
      IrBinOp* op = &is_node->op;
      gen_code(arena, code, op->left_operand);
      gen_code(arena, code, op->right_operand);
      if(op->op == IrOpKind_Store)
        emit_instr(arena, code, Opcode_STORE);
      else if(op->op == IrOpKind_Add)
        emit_instr(arena, code, Opcode_ADD);
      else if(op->op == IrOpKind_Sub)
        emit_instr(arena, code, Opcode_SUB);
      else if(op->op == IrOpKind_Mul)
        emit_instr(arena, code, Opcode_MUL);
      else if(op->op == IrOpKind_Div)
        emit_instr(arena, code, Opcode_DIV);
      else
        assert(false);
    } break;

    case(IrNodeKind_Call):
    {
      IrCall* call = &is_node->call;
      gen_call(arena, code, call);
      emit_instr(arena, code, Opcode_POP);
    } break;
  }
}/*<<<*/

char* get_regname_str(RegName reg)
{/*>>>*/
  static char* regFP = "fp";
  static char* regSP = "sp";
  static char* regIP = "ip";
  char* regStr = 0;

  if(reg == RegName_FP)
    regStr = regFP;
  else if(reg == RegName_SP)
    regStr = regSP;
  else if(reg == RegName_IP)
    regStr = regIP;
  else
    assert(false);
  return regStr;
}/*<<<*/

void print_code(VmProgram* vmProgram)
{/*>>>*/
  ListItem* item = list_first_item(&vmProgram->instr_list);
  while(item)
  {
    Instruction* instr = item->elem;
    switch(instr->opcode)
    {
      case(Opcode_PUSH):
      {
        if(instr->param_type == ParamType_Reg)
          print_instruction(vmProgram, "push %s", get_regname_str(instr->param.reg));
        else if(instr->param_type == ParamType_Int32)
          print_instruction(vmProgram, "push %d", instr->param.int_num);
        else
          assert(false);
      } break;

      case(Opcode_POP):
      {
        if(instr->param_type == ParamType_Reg)
          print_instruction(vmProgram, "pop %s", get_regname_str(instr->param.reg));
        else if(instr->param_type == ParamType__Null)
          print_instruction(vmProgram, "pop");
        else
          assert(false);
      } break;

      case(Opcode_ADD):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vmProgram, "add");
      } break;

      case(Opcode_LOAD):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vmProgram, "load");
      } break;

      case(Opcode_STORE):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vmProgram, "store");
      } break;

      case(Opcode_LABEL):
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vmProgram, "label %s", instr->param.str);
      } break;

      case(Opcode_RETURN):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vmProgram, "return");
      } break;

      case(Opcode_ALLOC):
      {
        assert(instr->param_type == ParamType_Int32);
        print_instruction(vmProgram, "alloc %d", instr->param.int_num);
      } break;

      case(Opcode_CALL):
      {
        assert(instr->param_type == ParamType_String);
        print_instruction(vmProgram, "call %s", instr->param.str);
      } break;

      case(Opcode_HALT):
      {
        assert(instr->param_type == ParamType__Null);
        print_instruction(vmProgram, "halt");
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
//              // Note that the storage locations for arguments are negative
//              irArg->location = -(args_areaSize + CONTROL_LINKS_DATA_SIZE + 1);
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
//        dataObj->location = localAreaSize;
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
//        Ast_ListItem* stmtList = block->stmtList;
//        while(stmtList && success)
//        {
//          AstNode* stmtAst = stmtList->ast;
//          success = BuildIr(arena, symbol_table, stmtAst);
//          stmtList = stmtList->nextItem;
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
//void gen_codeLValue(VmProgram* vmProgram, AstNode* ast)
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
//          Emit(vmProgram, ";begin load l-value of non-local '%s'", symbol->name);
//          Emit(vmProgram, "push fp");
//          int access_linkLocation = 2 + access_link->index;
//          Emit(vmProgram, "push -%d", access_linkLocation);
//          Emit(vmProgram, "add");
//          Emit(vmProgram, "load ;access link"); // access link is on the stack now
//        } else
//        {
//          Emit(vmProgram, ";begin load l-value of local '%s'", symbol->name);
//          Emit(vmProgram, "push fp");
//        }
//        Emit(vmProgram, "push %d", dataObj->location);
//        Emit(vmProgram, "add");
//        Emit(vmProgram, ";end load of l-value");
//      } break;
//
//    default:
//      assert(false);
//  }
//}/*<<<*/
//
//void gen_codeRValue(VmProgram* vmProgram, AstNode* ast)
//{/*>>>*/
//  switch(ast->kind)
//  {
//    case Ast_Id:
//      {
//        gen_codeLValue(vmProgram, ast);
//        Emit(vmProgram, "load ;r-value");
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
//              Emit(vmProgram, "push 0 ;retval of %s", callAst->call.name);
//
//              Emit(vmProgram, ";begin arg-eval");
//              Ast_ListItem* argList = callAst->call.argList;
//              while(argList)
//              {
//                AstNode* argAst = argList->ast;
//                gen_codeRValue(vmProgram, argAst);
//                argList = argList->nextItem;
//              }
//              Emit(vmProgram, ";end arg-eval");
//
//              Emit(vmProgram, "call %s", callAst->call.name);
//
//              ActivationRecord* actv_rec = callAst->call.actv_rec;
//              assert(actv_rec->kind == ActvRecord_Proc);
//              int restoreSp = actv_rec->proc.args_areaSize;
//
//              if(ast->expr.is_statement)
//                restoreSp += 1; // discard retval
//              if(restoreSp > 0)
//                Emit(vmProgram, "pop %d ;restore callee sp", restoreSp);
//            } break;
//
//          case Operator_Assign:
//            {
//              AstNode* rightSide = ast->expr.right_operand;
//              gen_codeRValue(vmProgram, rightSide);
//
//              AstNode* leftSide = ast->expr.left_operand;
//              assert(leftSide->kind == Ast_Id);
//              gen_codeLValue(vmProgram, leftSide);
//              Emit(vmProgram, "store ;'%s'", leftSide->id.name);
//
//              if(ast->expr.is_statement)
//                Emit(vmProgram, "pop"); // discard the r-value
//            } break;
//
//          case Operator_Mul:
//          case Operator_Add:
//          case Operator_Div:
//          case Operator_Sub:
//            {
//              AstNode* left_operand = ast->expr.left_operand;
//              gen_codeRValue(vmProgram, left_operand);
//              AstNode* right_operand = ast->expr.right_operand;
//              gen_codeRValue(vmProgram, right_operand);
//
//              if(ast->expr.op == Operator_Mul)
//                Emit(vmProgram, "mul");
//              else if(ast->expr.op == Operator_Add)
//                Emit(vmProgram, "add");
//              else if(ast->expr.op == Operator_Div)
//                Emit(vmProgram, "div");
//              else if(ast->expr.op == Operator_Sub)
//                Emit(vmProgram, "sub");
//              else
//                assert(false);
//            } break;
//        }
//      } break;
//
//    case Ast_IntNum:
//      {
//        Emit(vmProgram, "push %d", ast->literal.int_num);
//      } break;
//
//    case Ast_Neg:
//      {
//        AstNode* expr = ast->neg.expr;
//        if(expr->kind == Ast_IntNum)
//        {
//          Emit(vmProgram, "push -%d", expr->literal.int_num);
//        } else {
//          gen_codeRValue(vmProgram, expr);
//          Emit(vmProgram, "push -1");
//          Emit(vmProgram, "mul");
//        }
//      } break;
//
//    default:
//      assert(false);
//  }
//}/*<<<*/
//
//void gen_code(VmProgram* vmProgram, SymbolTable* symbol_table,
//             Block* block, AstNode* ast)
//{/*>>>*/
//  switch(ast->kind)
//  {
//    case Ast_IntNum:
//    case Ast_Expr:
//    case Ast_Id:
//      {
//        gen_codeRValue(vmProgram, ast);
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
//              Emit(vmProgram, "push 0 ;main retval");
//              Emit(vmProgram, "call main");
//              Emit(vmProgram, "halt");
//
//              Ast_ListItem* proc_list = block->module.proc_list;
//              while(proc_list)
//              {
//                AstNode* procAst = proc_list->ast;
//                gen_code(vmProgram, symbol_table, block, procAst);
//
//                proc_list = proc_list->nextItem;
//              }
//            } break;
//
//          case Block_Proc:
//            {
//              Symbol* procSymbol = block->proc.symbol;
//              Emit(vmProgram, "label %s", procSymbol->name); // entry point
//
//              ActivationRecord* actv_rec = &block->actv_rec;
//              int dataAreaSize = actv_rec->localAreaSize;
//              if(dataAreaSize > 0)
//                Emit(vmProgram, "alloc %d ;local storage", dataAreaSize);
//
//              Ast_ListItem* stmtList = block->stmtList;
//              while(stmtList)
//              {
//                AstNode* stmt = stmtList->ast;
//                gen_code(vmProgram, symbol_table, block, stmt);
//
//                stmtList = stmtList->nextItem;
//              }
//
//              Emit(vmProgram, "label %s.end-proc", procSymbol->name);
//              Emit(vmProgram, "return");
//            } break;
//
//          case Block_WhileStmt:
//            {
//              ActivationRecord* actv_rec = &block->actv_rec;
//
//              char label[32] = {};
//              make_unique_label(symbol_table, label);
//              Emit(vmProgram, "label %s.while-expr", label);
//
//              // conditional expr
//              gen_codeRValue(vmProgram, block->while_stmt.expr);
//              Emit(vmProgram, "jumpz %s.while-break", label);
//
//              if(actv_rec->access_linkCount > 0)
//              {
//                // Highest indexed access link is first from the top
//                Emit(vmProgram, ";begin set-up of access links");
//
//                AccessLink* access_link = actv_rec->access_links;
//                while(access_link)
//                {
//                  Emit(vmProgram, "push fp"); // first level up is the caller's activ. record
//
//                  assert(access_link->actv_rec_offset > 0);
//                  int offset = access_link->actv_rec_offset;
//                  offset--;
//                  while(offset--)
//                  {
//                    Emit(vmProgram, "decr"); // offset to the fp of actv. record n-1
//                    Emit(vmProgram, "load");
//                  }
//
//                  access_link = access_link->nextLink;
//                }
//                Emit(vmProgram, ";end set-up of access links");
//              }
//
//              Emit(vmProgram, "enter");
//
//              if(actv_rec->localAreaSize > 0)
//                Emit(vmProgram, "alloc %d ;local storage", actv_rec->localAreaSize);
//
//              // body
//              Ast_ListItem* stmtList = block->stmtList;
//              while(stmtList)
//              {
//                AstNode* stmt = stmtList->ast;
//                gen_code(vmProgram, symbol_table, block, stmt);
//
//                stmtList = stmtList->nextItem;
//              }
//
//              Emit(vmProgram, "leave");
//              Emit(vmProgram, "pop %d ;discard access links", actv_rec->access_linkCount);
//
//              Emit(vmProgram, "goto %s.while-expr", label);
//              Emit(vmProgram, "label %s.while-break", label);
//            } break;
//
//          default:
//            assert(false && !"Not implemented");
//        }
//      } break;
//
//    case Ast_Call:
//      {
//        Emit(vmProgram, "call %s", ast->call.name);
//      } break;
//
//    case Ast_Return:
//      {
//        gen_codeRValue(vmProgram, ast->ret.expr);
//
//        ActivationRecord* actv_rec = ast->ret.actv_rec;
//        assert(actv_rec->kind == ActvRecord_Proc);
//
//        //FIXME: 'retval' is a DataObj
//        int retValLocation = CONTROL_LINKS_DATA_SIZE + actv_rec->proc.args_areaSize +
//          actv_rec->proc.retAreaSize;
//
//        // Load l-value of the 'ret' data object
//        Emit(vmProgram, "push fp");
//        assert(ast->ret.intervening_block_count >= 0);
//        int level = ast->ret.intervening_block_count;
//        while(level--)
//        {
//          Emit(vmProgram, "decr"); // offset to the fp of actv. record n-1
//          Emit(vmProgram, "load");
//        }
//        //--
//
//        Emit(vmProgram, "push %d ;location of retval", -retValLocation); // note the negative sign
//        Emit(vmProgram, "add");
//        Emit(vmProgram, "store ;retval");
//
//        // Exit from the enclosing procedure and any of the intervening blocks
//        Block* block = ast->ret.block; 
//        Symbol* procSymbol = block->proc.symbol;
//        int depth = ast->ret.intervening_block_count;
//        assert(depth >= 0);
//        while(depth--)
//          Emit(vmProgram, "leave");
//        Emit(vmProgram, "goto %s.end-proc", procSymbol->name);
//      } break;
//#if 0
//    case Ast_IfStmt:
//      {
//        Emit(vmProgram, ";if-begin");
//
//        // conditional
//        gen_codeRValue(vmProgram, ast->if_stmt.expr);
//
//        char* label = make_unique_label(symbol_table);
//
//        if(ast->if_stmt.bodyElse)
//          Emit(vmProgram, "jumpz %s.else", label);
//        else
//          Emit(vmProgram, "jumpz %s.if-end", label);
//
//        gen_code(vmProgram, symbol_table, ast->if_stmt.body);
//        if(ast->if_stmt.bodyElse)
//        {
//          Emit(vmProgram, "goto %s.if-end", label);
//          Emit(vmProgram, "label %s.else", label);
//          gen_code(vmProgram, symbol_table, ast->if_stmt.bodyElse);
//        }
//
//        Emit(vmProgram, "label %s.if-end", label);
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

bool32 translate_hoc(MemoryArena* arena, char* file_path, char* hocProgram, VmProgram* vmProgram)
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

  RegisterKeywords(&symbol_table);
  consume_token(&tokenStream, &symbol_table);

  AstNode* ast_node = 0;
  success = parse_module(arena, &tokenStream, &symbol_table, &ast_node);

  if(success)
  {
    assert(ast_node->kind == AstNodeKind_Module);
    assert(symbol_table.scope_id == 0);
    assert(symbol_table.nesting_depth == 0);

    //IrActivationRecord topActvRecord = {0};
    IrExecutionContext execContext = {0};
    list_init(&execContext.actv_recStack);
    //list_append(arena, &execContext.avRecordStack, &topActvRecord);
    IrNode* ir_module_node = ir_build_module(arena, &execContext, &ast_node->module);
    if(ir_module_node)
    {
      list_init(&vmProgram->instr_list);
      gen_code(arena, &vmProgram->instr_list, ir_module_node);

      string_init(&vmProgram->text, arena);
      print_code(vmProgram);
    }
    else
      success = false;
  }

  return success;
}/*<<<*/

