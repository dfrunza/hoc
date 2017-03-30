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
  Token prevToken;
  Token token;
  char* text;
  char* cursor;
  MemoryArena* arena;

  char* filePath;
  int   lineNr;
  char* srcLine;

  union {
    int*  intNum;
    char* id;
  } lexval;
}
TokenStream;

typedef struct Symbol_ Symbol;
typedef struct AstNode_ AstNode;

typedef struct
{
  Symbol* symbol;
  int     scopeId;
  int     lastScopeId;
  int     nestingDepth;
  int     activeScopes[32];
  char    label[64];
  int     lastLabelId;
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
  int actvRecordOffset; // must be greater than zero
  int index;
}
AccessLink;

typedef struct
{
  List     procList;
  AstNode* body;
}
Module;

typedef struct
{
  Symbol* symbol;
  char*   name;
  int     location; // relative to fp
  int     dataSize;
}
VarDecl;

typedef struct
{
  OperatorKind op;
  AstNode* leftOperand;
  AstNode* rightOperand;
  bool32   isStatement;
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
  int         declBlockOffset;
  bool32      isNonLocal;
  AccessLink* accessLink; // if non-local
  VarDecl*    varDecl;
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
  int interveningBlockCount;
  AstNode* proc;
}
ReturnStmt;

typedef struct IrProc_ IrProc;
typedef struct
{
  Symbol*  symbol;
  char*    name;
  List     formalArgs;
  AstNode* body;
  int      argsDataSize;
  int      retDataSize;
  IrProc   *irProc;
}
Proc;

typedef struct
{
  Symbol* symbol;
  char*   name;
  List    actualArgs;
  Proc*   proc;
  bool32  isStatement;
}
Call;

typedef struct
{
  AstNode* expr;
  AstNode* body;
  AstNode* elseNode;
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
  int      blockId;
  int      nestingDepth;
  List     declVars;
  List     localOccurs;
  List     nonLocalOccurs;
  List     stmtList;
  struct   Block_* enclosingBlock;
  int      localsDataSize;
  List     accessLinks;
  int      accessLinksAreaSize;
}
Block;

typedef struct AstNode_
{
  AstNodeKind  kind;

  union {
    BinExpr    binExpr;
    UnrExpr    unrExpr;
    VarDecl    varDecl;
    VarOccur   varOccur;
    IntNum     intNum;
    Call       call;
    Proc       proc;
    Module     module;
    Block      block;
    ReturnStmt retStmt;
    WhileStmt  whileStmt;
    IfStmt     ifStmt;
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
  int        blockId;
  int        nestingDepth;
  Symbol*    nextSymbol;

  union {
    VarDecl* var;
    Proc*    proc;
    Token    kwToken;
  };
}
Symbol;

typedef struct
{
  String       text;
  int          textLen;
  List         instrList;
  MemoryArena* arena;
}
VmProgram;

void BlockInit(SymbolTable* symbolTable, Block* block)
{/*>>>*/
  block->blockId      = symbolTable->scopeId;
  block->nestingDepth = symbolTable->nestingDepth;

  ListInit(&block->localOccurs);
  ListInit(&block->nonLocalOccurs);
  ListInit(&block->stmtList);
  ListInit(&block->declVars);
  ListInit(&block->accessLinks);
}/*<<<*/

void BlockProcessVars(MemoryArena* arena, Block* block)
{/*>>>*/
  /*>>> Process declared vars */
  ListItem* nodeItem = ListFirstItem(&block->declVars);
  while(nodeItem)
  {
    AstNode* node = nodeItem->elem;
    VarDecl* varDecl = &node->varDecl;

    varDecl->location = block->localsDataSize;
    block->localsDataSize += varDecl->dataSize;

    nodeItem = nodeItem->next;
  }
  /*<<<*/
  /*>>> Process non-local var occurrences */
  nodeItem = ListFirstItem(&block->nonLocalOccurs);
  while(nodeItem)
  {
    AstNode* node = nodeItem->elem;
    VarOccur* varOccur = &node->varOccur;
    List* linksList = &block->accessLinks;

    ListItem* linkItem = ListFirstItem(&block->accessLinks);
    AccessLink* link = 0;
    while(linkItem)
    {
      link = linkItem->elem;
      if(link->actvRecordOffset == varOccur->declBlockOffset)
        break;
      linkItem = linkItem->next;
    }
    if(!link)
    {
      link = PushElement(arena, AccessLink, 1);
      link->actvRecordOffset = varOccur->declBlockOffset;
      link->index = linksList->count;
      ListAppend(arena, linksList, link);
    }
    block->accessLinksAreaSize = linksList->count;
  }
  /*<<<*/
}/*<<<*/

void SyntaxError(TokenStream* input, char* message, ...)
{
  va_list args;

  fprintf(stderr, "%s(%d) : ", input->filePath, input->lineNr);

  va_start(args, message);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
}

/*>>> Symbol table */
Symbol* SymbolLookup(SymbolTable* symbolTable, char* name)
{/*>>>*/
  Symbol* result = 0;

  Symbol* symbol = symbolTable->symbol;
  while(symbol)
  {
    if(StrMatch(symbol->name, name))
    {
      result = symbol;
      break;
    }
    symbol = symbol->nextSymbol;
  }
  return result;
}/*<<<*/

Symbol* SymbolAdd(SymbolTable* symbolTable, char* name, SymbolKind kind)
{/*>>>*/
  Symbol* symbol = PushElement(symbolTable->arena, Symbol, 1);
  symbol->name = name;
  symbol->kind = kind;
  symbol->blockId = symbolTable->scopeId;
  symbol->nestingDepth = symbolTable->nestingDepth;
  symbol->nextSymbol = symbolTable->symbol;
  symbolTable->symbol = symbol;
  return symbol;
}/*<<<*/

Symbol* SymbolRegisterNew(SymbolTable* symbolTable, TokenStream* input, SymbolKind kind)
{/*>>>*/
  assert(input->token == Token_Id);

  Symbol* result = 0;

  Symbol* symbol = SymbolLookup(symbolTable, input->lexval.id);
  if(!symbol)
  {
    result = SymbolAdd(symbolTable, input->lexval.id, kind);
  } else {
    if(symbol->kind != SymbolKind_Keyword)
    {
      if(symbol->blockId != symbolTable->scopeId ||
          symbol->kind != kind)
      {
        assert(symbol->nestingDepth <= symbolTable->nestingDepth);

        result = SymbolAdd(symbolTable, input->lexval.id, kind);
      } else
        SyntaxError(input, "Redeclaration of identifier: %s", symbol->name);
    } else
      SyntaxError(input, "Keyword used as identifier: %s", symbol->name);
  }
  return result;
}/*<<<*/

Symbol* AddKeyword(SymbolTable* symbolTable, char* name, Token token)
{/*>>>*/
  Symbol* symbol = SymbolAdd(symbolTable, name, SymbolKind_Keyword);
  symbol->kwToken = token;
  return symbol;
}/*<<<*/

void RegisterKeywords(SymbolTable* symbolTable)
{/*>>>*/
  AddKeyword(symbolTable, "int", Token_If);
  AddKeyword(symbolTable, "float", Token_Float);
  AddKeyword(symbolTable, "void", Token_Void);
  AddKeyword(symbolTable, "char", Token_Char);
  AddKeyword(symbolTable, "var", Token_Var);
  AddKeyword(symbolTable, "proc", Token_Proc);
  AddKeyword(symbolTable, "type", Token_Type);
  AddKeyword(symbolTable, "struct", Token_Type);
  AddKeyword(symbolTable, "array", Token_Array);
  AddKeyword(symbolTable, "of", Token_Of);
  AddKeyword(symbolTable, "if", Token_If);
  AddKeyword(symbolTable, "else", Token_Else);
  AddKeyword(symbolTable, "while", Token_While);
  AddKeyword(symbolTable, "return", Token_Return);
  AddKeyword(symbolTable, "true", Token_True);
  AddKeyword(symbolTable, "false", Token_False);
}/*<<<*/

bool32 ScopeBegin(SymbolTable* symbolTable)
{/*>>>*/
  int scopeId = ++symbolTable->lastScopeId;
  symbolTable->scopeId = scopeId;

  int nestingDepth = ++symbolTable->nestingDepth;
  if(nestingDepth < SizeofArray(symbolTable->activeScopes))
  {
    symbolTable->activeScopes[nestingDepth] = scopeId;
  } else {
    Error("Reached the maximum scope nesting depth");
    return false;
  }

  return true;
}/*<<<*/

void ScopeEnd(SymbolTable* symbolTable)
{/*>>>*/
  int nestingDepth = --symbolTable->nestingDepth;
  int scopeId = symbolTable->activeScopes[nestingDepth];
  assert(scopeId >= 0);
  symbolTable->scopeId = scopeId;

  Symbol* symbol = symbolTable->symbol;
  while(symbol && symbol->blockId > symbolTable->scopeId)
    symbol = symbol->nextSymbol;
  symbolTable->symbol = symbol;
}/*<<<*/
/*<<<*/

void MakeUniqueLabel(SymbolTable* symbolTable, char* label)
{
  sprintf(label, "L%d", symbolTable->lastLabelId++);
}

/*>>> Lex */
bool32 TokenIsKeyword(Token token)
{/*>>>*/
  return token > Token__KeywordBegin && token < Token__KeywordEnd;
}/*<<<*/

char* InstallLexeme(TokenStream* input, char* beginChar, char* endChar)
{/*>>>*/
  //FIXME: If the lexeme had been previously installed then return it.
  int len = (int)(endChar - beginChar + 1);
  char* lexeme = PushElement(input->arena, char, len + 1);
  CopySubstr(lexeme, beginChar, endChar);
  return lexeme;
}/*<<<*/

void ConsumeToken(TokenStream* input, SymbolTable* symbolTable)
{/*>>>*/
  input->prevToken = input->token;
  input->token = Token__Null;
  memset(&input->lexval, 0, sizeof(input->lexval));

  input->srcLine = input->cursor;
  char c = *input->cursor;

  while(c == ' ' || c == '\t' ||
        c == '\r' || c == '\n')
  {
    if(c == '\n')
    {
      input->lineNr++;
      input->srcLine = input->cursor;
    }
    c = *(++input->cursor);
  }

  if(IsLetterChar(c) || c == '_')
  {
    char* beginChar = input->cursor;
    c = *(++input->cursor);

    while(IsLetterChar(c) || IsNumericChar(c) || c == '_')
      c = *(++input->cursor);

    char* endChar = input->cursor - 1;
    char* lexeme = InstallLexeme(input, beginChar, endChar);
    input->lexval.id = lexeme;

    Symbol* symbol = SymbolLookup(symbolTable, lexeme);
    if(symbol && symbol->kind == SymbolKind_Keyword)
      input->token = symbol->kwToken;
    else
      input->token = Token_Id;
  }
  else if(IsNumericChar(c))
  {
    int num = c - '0';
    c = *(++input->cursor);

    while(IsNumericChar(c))
    {
      num = (10 * num) + (c - '0');
      c = *(++input->cursor);
    }

    int* value = PushElement(input->arena, int, 1);
    *value = num;
    input->token = Token_IntNum;
    input->lexval.intNum = value;
  }
  else if(c == '-')
  {
    c = *(++input->cursor);
    if(c == '>')
    {
      input->token = Token_RightArrow;
      ++input->cursor;
    }
    else if(input->prevToken == Token_Equals ||
            input->prevToken == Token_OpenParens ||
            input->prevToken == Token_Star ||
            input->prevToken == Token_Plus ||
            input->prevToken == Token_Comma ||
            input->prevToken == Token_FwdSlash ||
            input->prevToken == Token_Return)
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
bool32 ParseExpression(MemoryArena*, TokenStream*, SymbolTable*, Block*, AstNode**);
bool32 ParseStatementList(MemoryArena*, TokenStream*, SymbolTable*, Block*);
bool32 ParseActualArgumentList(MemoryArena*, TokenStream*, SymbolTable*, Block*, Call*);
bool32 ParseTerm(MemoryArena*, TokenStream*, SymbolTable*, Block*, AstNode**);

bool32 ParseFactor(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                   Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_OpenParens)
  {
    ConsumeToken(input, symbolTable);
    success = ParseExpression(arena, input, symbolTable,
                              enclosingBlock, node);
    if(success)
    {
      if(input->token == Token_CloseParens)
        ConsumeToken(input, symbolTable);
      else {
        SyntaxError(input, "Missing ')'");
        success = false;
      }
    }
  }
  else if(input->token == Token_UnaryMinus)
  {
    ConsumeToken(input, symbolTable);
    AstNode* operand = 0;
    success = ParseTerm(arena, input, symbolTable, enclosingBlock, &operand);
    if(success)
    {
      if(operand)
      {
        AstNode* negNode = PushElement(arena, AstNode, 1);
        negNode->kind = AstNodeKind_UnrExpr;
        UnrExpr* expr = &negNode->unrExpr;
        expr->op = OperatorKind_Neg;
        expr->operand = operand;
        *node = negNode;
      } else {
        SyntaxError(input, "Expression expected after '-'");
        success = false;
      }
    }
  }
  else if(input->token == Token_IntNum)
  {
    AstNode* numNode = PushElement(arena, AstNode, 1);
    numNode->kind = AstNodeKind_IntNum;
    numNode->intNum.value = *(int32*)input->lexval.intNum;
    *node = numNode;

    ConsumeToken(input, symbolTable);
  }
  else if(input->token == Token_Id)
  {
    Symbol* symbol = SymbolLookup(symbolTable, input->lexval.id);
    if(symbol)
    {
      AstNode* idNode = PushElement(arena, AstNode, 1);
      *node = idNode;

      ConsumeToken(input, symbolTable);

      if(symbol->kind == SymbolKind_Var)
      {
        idNode->kind = AstNodeKind_VarOccur;
        VarOccur* varOccur = &idNode->varOccur;
        varOccur->symbol = symbol;
        varOccur->name = symbol->name;
        varOccur->declBlockOffset = (symbolTable->nestingDepth - symbol->nestingDepth);
        varOccur->isNonLocal = (varOccur->declBlockOffset > 0);

        if(varOccur->isNonLocal)
        {
          ListAppend(arena, &enclosingBlock->nonLocalOccurs, idNode);
        }
        else
        {
          assert(varOccur->declBlockOffset == 0);
          ListAppend(arena, &enclosingBlock->localOccurs, idNode);
        }
      }
      else if(symbol->kind == SymbolKind_Proc)
      {
        idNode->kind = AstNodeKind_Call;
        Call* call = &idNode->call;
        call->symbol = symbol;
        call->name = symbol->name;
        call->proc = symbol->proc;
        ListInit(&call->actualArgs);

        if(input->token == Token_OpenParens)
        {
          ConsumeToken(input, symbolTable);
          success = ParseActualArgumentList(arena, input, symbolTable, enclosingBlock, call);
          if(success)
          {
            if(input->token == Token_CloseParens)
            {
              ConsumeToken(input, symbolTable);

              List* procArgList = &symbol->proc->formalArgs;
              List* callArgList = &call->actualArgs;
              if(procArgList->count != callArgList->count)
              {
                SyntaxError(input, "Incorrect number of arguments in the call: %s(..)", symbol->name);
                success = false;
              }
            } else {
              SyntaxError(input, "Missing ')' in procedure call");
              success = false;
            }
          }
        } else {
          SyntaxError(input, "Missing '(' in procedure call");
          success = false;
        }
      }
      else if(symbol->kind == SymbolKind_Keyword)
      {
        SyntaxError(input, "Keyword used as identifier: %s", input->lexval.id);
        success = false;
      }
      else
        assert(false);
    } else {
      SyntaxError(input, "Unknown identifier: %s", input->lexval.id);
      success = false;
    }
  }
  else if(input->token == Token_True || input->token == Token_False)
  {
    AstNode* numNode = PushElement(arena, AstNode, 1);
    numNode->kind = AstNodeKind_IntNum;
    numNode->intNum.value = (input->token == Token_True ? 1 : 0);
    *node = numNode;

    ConsumeToken(input, symbolTable);
  }

  return success;
}/*<<<*/

bool32 ParseRestOfFactors(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                          Block* enclosingBlock, AstNode* leftNode, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Star ||
      input->token == Token_FwdSlash)
  {
    AstNode* exprNode = PushElement(arena, AstNode, 1);
    exprNode->kind = AstNodeKind_BinExpr;
    BinExpr* expr = &exprNode->binExpr;
    if(input->token == Token_Star)
      expr->op = OperatorKind_Mul;
    else if(input->token == Token_FwdSlash)
      expr->op = OperatorKind_Div;
    else
      assert(false);

    ConsumeToken(input, symbolTable);
    AstNode* factorNode = 0;
    success = ParseFactor(arena, input, symbolTable, enclosingBlock, &factorNode);

    if(success && factorNode)
    {
      expr->rightOperand = factorNode;
      expr->leftOperand = leftNode;
      success = ParseRestOfFactors(arena, input, symbolTable,
                                   enclosingBlock, exprNode, node);
    } else {
      SyntaxError(input, "Factor expected");
      success = false;
    }
  }
  else
    *node = leftNode;

  return success;
}/*<<<*/

bool32 ParseTerm(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                 Block* enclosingBlock, AstNode** node)
{/*>>>*/
  AstNode* factorNode = 0;
  AstNode* exprNode = 0;

  bool32 success = ParseFactor(arena, input, symbolTable, enclosingBlock, &factorNode);
  if(success && factorNode)
    success = ParseRestOfFactors(arena, input, symbolTable,
                                 enclosingBlock, factorNode, &exprNode);

  *node = exprNode;
  return success;
}/*<<<*/

bool32 ParseRestOfTerms(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                        Block* enclosingBlock, AstNode* leftNode, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Plus ||
      input->token == Token_Minus)
  {
    AstNode* exprNode = PushElement(arena, AstNode, 1);
    exprNode->kind = AstNodeKind_BinExpr;
    BinExpr* expr = &exprNode->binExpr;
    if(input->token == Token_Plus)
      expr->op = OperatorKind_Add;
    else if(input->token == Token_Minus)
      expr->op = OperatorKind_Sub;
    else
      assert(false);

    ConsumeToken(input, symbolTable);
    AstNode* termNode = 0;
    success = ParseTerm(arena, input, symbolTable, enclosingBlock, &termNode);

    if(success && termNode)
    {
      expr->rightOperand = termNode;
      expr->leftOperand = leftNode;
      success = ParseRestOfTerms(arena, input, symbolTable, enclosingBlock, exprNode, node);
    } else {
      SyntaxError(input, "Expression term expected");
      success = false;
    }
  }
  else
    *node = leftNode;

  return success;
}/*<<<*/

bool32 ParseAssignmentTerm(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                           Block* enclosingBlock, AstNode** node)
{/*>>>*/
  AstNode* termNode = 0;
  AstNode* exprNode = 0;

  bool32 success = ParseTerm(arena, input, symbolTable, enclosingBlock, &termNode);
  if(success && termNode)
    success = ParseRestOfTerms(arena, input, symbolTable, enclosingBlock, termNode, &exprNode);

  *node = exprNode;
  return success;
}/*<<<*/

bool32 ParseRestOfAssignmentTerms(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                                  Block* enclosingBlock, AstNode* leftNode, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Equals)
  {
    ConsumeToken(input, symbolTable);
    AstNode* rightSide = 0;
    success = ParseExpression(arena, input, symbolTable, enclosingBlock, &rightSide);
    if(success)
    {
      if(rightSide)
      {
        if(leftNode->kind == AstNodeKind_VarOccur)
        {
          AstNode* exprNode = PushElement(arena, AstNode, 1);
          exprNode->kind = AstNodeKind_BinExpr;
          BinExpr* expr = &exprNode->binExpr;
          expr->op = OperatorKind_Assign;
          expr->leftOperand = leftNode;
          expr->rightOperand = rightSide;

          *node = exprNode;
        } else {
          SyntaxError(input, "Variable is required on the left side of assignment");
          success = false;
        }
      } else {
        SyntaxError(input, "Missing right side of assignment");
        success = false;
      }
    }
  } else
    *node = leftNode;

  return success;
}/*<<<*/

bool32 ParseExpression(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                       Block* enclosingBlock, AstNode** node)
{/*>>>*/
  AstNode* assgnNode = 0;
  AstNode* exprNode = 0;

  bool32 success = ParseAssignmentTerm(arena, input, symbolTable, enclosingBlock, &assgnNode);
  if(success && assgnNode)
    success = ParseRestOfAssignmentTerms(arena, input, symbolTable,
                                         enclosingBlock, assgnNode, &exprNode);

  *node = exprNode;
  return success;
}/*<<<*/

bool32 ParseVarStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                         Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Var)
  {
    ConsumeToken(input, symbolTable);
    if(input->token == Token_Id)
    {
      AstNode* varNode = PushElement(arena, AstNode, 1);
      varNode->kind = AstNodeKind_VarDecl;
      *node = varNode;

      Symbol* symbol = SymbolRegisterNew(symbolTable, input, SymbolKind_Var);
      if(symbol)
      {
        VarDecl* varDecl = &varNode->varDecl;
        varDecl->symbol = symbol;
        varDecl->name = symbol->name;
        varDecl->dataSize = 1;
        symbol->var = varDecl;

        ConsumeToken(input, symbolTable);
      }
    } else {
      SyntaxError(input, "Missing identifier");
      success = false;
    }
  }

  return success;
}/*<<<*/

bool32 ParseFormalArgument(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                           Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Var)
  {
    ConsumeToken(input, symbolTable);
    if(input->token == Token_Id)
    {
      AstNode* varNode = PushElement(arena, AstNode, 1);
      varNode->kind = AstNodeKind_VarDecl;
      *node = varNode;

      Symbol* symbol = SymbolRegisterNew(symbolTable, input, SymbolKind_Var);
      if(symbol)
      {
        VarDecl* varDecl = &varNode->varDecl;
        varDecl->symbol = symbol;
        varDecl->name   = symbol->name;
        symbol->var = varDecl;

        ConsumeToken(input, symbolTable);
      } else {
        SyntaxError(input, "Identifier re-declared : %s", input->lexval.id);
        success = false;
      }
    } else {
      SyntaxError(input, "Expecting an identifier token");
      success = false;
    }
  }

  return success;
}/*<<<*/

bool32 ParseFormalArgumentList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                               Block* enclosingBlock, Proc* proc)
{/*>>>*/
  bool32 success = true;

  AstNode* argNode = 0;
  success = ParseFormalArgument(arena, input, symbolTable, enclosingBlock, &argNode);
  if(success && argNode)
  {
    ListAppend(arena, &proc->formalArgs, argNode);

    if(input->token == Token_Comma)
    {
      ConsumeToken(input, symbolTable);
      success = ParseFormalArgumentList(arena, input, symbolTable, enclosingBlock, proc);
    }
  }

  return success;
}/*<<<*/

bool32 ParseActualArgumentList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                               Block* enclosingBlock, Call* call)
{/*>>>*/
  bool32 success = true;

  AstNode* argNode = 0;
  success = ParseExpression(arena, input, symbolTable, enclosingBlock, &argNode);
  if(success && argNode)
  {
    ListAppend(arena, &call->actualArgs, argNode);

    if(input->token == Token_Comma)
    {
      ConsumeToken(input, symbolTable);
      success = ParseActualArgumentList(arena, input, symbolTable, enclosingBlock, call);
    }
  }

  return success;
}/*<<<*/

bool32 ParseWhileStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                           Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_While)
  {
    ConsumeToken(input, symbolTable);
    AstNode* exprNode = 0;
    success = ParseExpression(arena, input, symbolTable, enclosingBlock, &exprNode);
    if(success)
    {
      AstNode* whileNode = PushElement(arena, AstNode, 1);
      whileNode->kind = AstNodeKind_WhileStmt;
      WhileStmt* whileStmt = &whileNode->whileStmt;
      whileStmt->expr = exprNode;
      *node = whileNode;

      if(input->token == Token_OpenBrace)
      {
        ConsumeToken(input, symbolTable);

        success = ScopeBegin(symbolTable);
        if(success)
        {
          AstNode* blockNode = PushElement(arena, AstNode, 1);
          blockNode->kind = AstNodeKind_Block;
          Block* block = &blockNode->block;
          block->owner = whileNode;
          BlockInit(symbolTable, block);

          success = ParseStatementList(arena, input, symbolTable, block);
          if(success)
          {
            whileStmt->body = blockNode;

            if(input->token == Token_CloseBrace)
            {
              ConsumeToken(input, symbolTable);
              ScopeEnd(symbolTable);
            } else {
              SyntaxError(input, "Missing '}'");
              success = false;
            }
          }
        }
      } else {
        SyntaxError(input, "Missing '{'");
        success = false;
      }
    }
  }

  return success;
}/*<<<*/

bool32 ParseIfStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                        Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_If)
  {
    ConsumeToken(input, symbolTable);
    AstNode* exprNode = 0;
    success = ParseExpression(arena, input, symbolTable, enclosingBlock, &exprNode);
    if(success)
    {
      AstNode* ifNode = PushElement(arena, AstNode, 1);
      ifNode->kind = AstNodeKind_IfStmt;
      IfStmt* ifStmt = &ifNode->ifStmt;
      ifStmt->expr = exprNode;
      *node = ifNode;

      if(input->token == Token_OpenBrace)
      {
        ConsumeToken(input, symbolTable);

        success = ScopeBegin(symbolTable);
        if(success)
        {
          AstNode* blockNode = PushElement(arena, AstNode, 1);
          blockNode->kind = AstNodeKind_Block;
          Block* block = &blockNode->block;
          block->owner = ifNode;
          BlockInit(symbolTable, block);

          success = ParseStatementList(arena, input, symbolTable, block);
          if(success)
          {
            ifStmt->body = blockNode;

            if(input->token == Token_CloseBrace)
            {
              ConsumeToken(input, symbolTable);
              ScopeEnd(symbolTable);

              if(input->token == Token_Else)
              {
                ConsumeToken(input, symbolTable);
                AstNode* elseNode = 0;
                success = ParseIfStatement(arena, input, symbolTable, enclosingBlock, &elseNode);
                if(success)
                {
                  if(elseNode)
                    ifStmt->elseNode = elseNode;
                  else if(input->token == Token_OpenBrace)
                  {
                    ConsumeToken(input, symbolTable);

                    success = ScopeBegin(symbolTable);
                    if(success)
                    {
                      AstNode* blockNode = PushElement(arena, AstNode, 1);
                      blockNode->kind = AstNodeKind_Block;
                      Block* block = &blockNode->block;
                      block->owner = ifNode;
                      BlockInit(symbolTable, block);

                      success = ParseStatementList(arena, input, symbolTable, block);
                      if(success)
                      {
                        ifStmt->elseNode = blockNode;
                        if(input->token == Token_CloseBrace)
                        {
                          ConsumeToken(input, symbolTable);
                          ScopeEnd(symbolTable);
                        } else {
                          SyntaxError(input, "Missing '}'");
                          success = false;
                        }
                      }
                    }
                  } else {
                    SyntaxError(input, "Missing '{'");
                    success = false;
                  }
                }
              }
            } else {
              SyntaxError(input, "Missing '}'");
              success = false;
            }
          }
        }
      } else {
        SyntaxError(input, "Missing '{'");
        success = false;
      }
    }
  }
  return success;
}/*<<<*/

bool32 ParseProcedure(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                      Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Proc)
  {
    AstNode* procNode = PushElement(arena, AstNode, 1);
    procNode->kind = AstNodeKind_Proc;
    *node = procNode;

    ConsumeToken(input, symbolTable);
    if(input->token == Token_Id)
    {
      Symbol* symbol = SymbolRegisterNew(symbolTable, input, SymbolKind_Proc);
      if(symbol)
      {
        Proc* proc = &procNode->proc;
        proc->symbol = symbol;
        proc->name   = symbol->name;
        proc->retDataSize = 1;
        ListInit(&proc->formalArgs);
        symbol->proc = proc;

        ConsumeToken(input, symbolTable);
        if(input->token == Token_OpenParens)
        {
          ConsumeToken(input, symbolTable);

          // arguments
          success = ScopeBegin(symbolTable);
          if(success)
          {
            AstNode* blockNode = PushElement(arena, AstNode, 1);
            blockNode->kind = AstNodeKind_Block;
            proc->body = blockNode;
            Block* block = &blockNode->block;
            block->owner = procNode;
            BlockInit(symbolTable, block);

            success = ParseFormalArgumentList(arena, input, symbolTable, block, proc);
            if(success)
            {
              ListItem* argItem = ListFirstItem(&proc->formalArgs);
              while(argItem)
              {
                AstNode* node = argItem->elem;
                assert(node->kind == AstNodeKind_VarDecl);
                VarDecl* arg = &node->varDecl;
                Symbol* symbol = node->varDecl.symbol;
                symbol->var = arg;

                arg->location = proc->argsDataSize;
                arg->dataSize = 1;
                proc->argsDataSize += arg->dataSize;

                argItem = argItem->next;
              }

              if(input->token == Token_CloseParens)
              {
                ConsumeToken(input, symbolTable);

                if(input->token == Token_OpenBrace)
                {
                  // body
                  ConsumeToken(input, symbolTable);

                  success = ParseStatementList(arena, input, symbolTable, block);
                  if(success)
                  {
                    BlockProcessVars(arena, block);

                    if(input->token == Token_CloseBrace)
                    {
                      ConsumeToken(input, symbolTable);
                      ScopeEnd(symbolTable); // body
                    } else {
                      SyntaxError(input, "Missing '}'");
                      success = false;
                    }
                  }
                } else {
                  SyntaxError(input, "Missing '{'");
                  success = false;
                }
              } else {
                if(input->token == Token_Id)
                  SyntaxError(input, "Missing 'var' keyword", input->lexval.id);
                else
                  SyntaxError(input, "Missing ')'");
                success = false;
              }
            }
          }
        } else {
          SyntaxError(input, "Missing '('");
          success = false;
        }
      }
    } else {
      SyntaxError(input, "Missing identifier");
      success = false;
    }
  }

  return success;
}/*<<<*/

bool32 ParseProcedureList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                          Block* enclosingBlock, Module* module)
{/*>>>*/
  bool32 success = true;

  AstNode* procNode = 0;
  success = ParseProcedure(arena, input, symbolTable, enclosingBlock, &procNode);
  if(success && procNode)
  {
    ListAppend(arena, &module->procList, procNode);

    success = ParseProcedureList(arena, input, symbolTable, enclosingBlock, module);
  }

  return success;
}/*<<<*/

bool32 ParseReturnStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                            Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Return)
  {
    ConsumeToken(input, symbolTable);
    AstNode* exprNode = 0;
    success = ParseExpression(arena, input, symbolTable, enclosingBlock, &exprNode);
    if(success)
    {
      if(exprNode)
      {
        AstNode* retNode = PushElement(arena, AstNode, 1);
        retNode->kind = AstNodeKind_ReturnStmt;
        ReturnStmt* retStmt = &retNode->retStmt;
        retStmt->expr = exprNode;
        *node = retNode;

        int depth = 0;
        Block* block = enclosingBlock;
        while(block)
        {
          AstNode* owner = block->owner;
          if(owner->kind == AstNodeKind_Proc)
            break;
          depth++;
          block = block->enclosingBlock;
        }
        assert(block);
        retStmt->proc = block->owner;
        retStmt->interveningBlockCount = depth;
      } else {
        SyntaxError(input, "Expression required after the 'return' keyword");
        success = false;
      }
    }
  }

  return success;
}/*<<<*/

bool32 ParseStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                      Block* enclosingBlock, AstNode** node)
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
        success = ParseExpression(arena, input, symbolTable, enclosingBlock, &stmtNode);
        if(success)
        {
          if(stmtNode)
          {
            alt = Alt__Null;
            if(input->token == Token_Semicolon)
            {
              ConsumeToken(input, symbolTable);

              if(stmtNode->kind == AstNodeKind_BinExpr)
              {
                BinExpr* expr = &stmtNode->binExpr;
                if(expr->op == OperatorKind_Assign)
                  expr->isStatement = true;
                else {
                  SyntaxError(input, "Assignment expression required");
                  success = false;
                }
              }
              else if(stmtNode->kind == AstNodeKind_Call)
              {
                Call* call = &stmtNode->call;
                call->isStatement = true;
              }
              else {
                SyntaxError(input, "Expression is not a statement");
                success = false;
              }
            }
            else {
              SyntaxError(input, "Missing ';'");
              success = false;
            }
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_If:
      {
        success = ParseIfStatement(arena, input, symbolTable, enclosingBlock, &stmtNode);
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
        success = ParseWhileStatement(arena, input, symbolTable, enclosingBlock, &stmtNode);
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
        success = ParseReturnStatement(arena, input, symbolTable, enclosingBlock, &stmtNode);
        if(success)
        {
          if(stmtNode)
          {
            alt = Alt__Null;
            if(input->token == Token_Semicolon)
              ConsumeToken(input, symbolTable);
            else {
              SyntaxError(input, "Missing ';'");
              success = false;
            }
          } else
            alt = (Alternative)((int)alt+1);
        } else
          alt = Alt__Null;
      } break;

      case Alt_Var:
      {
        success = ParseVarStatement(arena, input, symbolTable, enclosingBlock, &stmtNode);
        if(success)
        {
          if(stmtNode)
          {
            alt = Alt__Null;
            if(input->token == Token_Semicolon)
              ConsumeToken(input, symbolTable);
            else {
              SyntaxError(input, "Missing ';'");
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

bool32 ParseStatementList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                          Block* block)
{/*>>>*/
  bool32 success = true;

  AstNode* stmtNode = 0;
  success = ParseStatement(arena, input, symbolTable, block, &stmtNode);
  if(success && stmtNode)
  {
    while(input->token == Token_Semicolon)
      ConsumeToken(input, symbolTable);

    if(stmtNode->kind == AstNodeKind_VarDecl)
      ListAppend(arena, &block->declVars, stmtNode);
    else
      ListAppend(arena, &block->stmtList, stmtNode);

    success = ParseStatementList(arena, input, symbolTable, block); //FIXME: Is this tail-recursion - can it be optimized?
  }
  return success;
}/*<<<*/

bool32 ParseModule(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                   AstNode** node)
{/*>>>*/
  bool32 success = true;

  success = ScopeBegin(symbolTable);
  if(success)
  {
    AstNode* moduleNode = PushElement(arena, AstNode, 1);
    moduleNode->kind = AstNodeKind_Module;

    AstNode* blockNode = PushElement(arena, AstNode, 1);
    blockNode->kind = AstNodeKind_Block;
    Block* block = &blockNode->block;
    block->owner = moduleNode;
    BlockInit(symbolTable, block);

    Module* module = &moduleNode->module;
    module->body = blockNode;
    ListInit(&module->procList);

    success = ParseProcedureList(arena, input, symbolTable, block, module);
    if(success)
    {
      ScopeEnd(symbolTable);

      if(input->token == Token_EndOfInput)
        *node = moduleNode;
      else {
        SyntaxError(input, "End of file expected");
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
  bool32 isNonLocal;
  int    accessLinkLocation;
  int    dataLocation;
}
IrVar;

typedef struct
{
  IrProc* proc;
  List    actualArgs;
}
IrCall;

typedef struct
{
  IrValueKind kind;

  union {
    int32   intNum;
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
  IrStackArea accessLinks;
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

//typedef struct IrProcActivation_
//{
//  struct IrProcActivation_*  parent;
//  IrActivationRecord* actvRecord;
//}
//IrProcActivation;

typedef struct
{
  List avRecordStack;
  IrActivationRecord* avRecord;
  int fp;
  int sp;
}
IrExecutionContext;

typedef struct
{
  IrOpKind op;
  IrNode*  leftOperand;
  IrNode*  rightOperand;
}
IrBinOp;

typedef struct IrProc_
{
  char* label;
  char* endLabel;
  List  instrList;
  IrActivationRecord* actvRecord;
}
IrProc;

typedef struct
{
  IrProc* mainProc;
  List    procList;
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

void IrSetVarValue(IrValue* irValue, IrExecutionContext* execContext, VarOccur* varOccur)
{
  Symbol* symbol = varOccur->symbol;
  IrVar* irVar = &irValue->var;
  irVar->isNonLocal = varOccur->isNonLocal;
  if(irVar->isNonLocal)
  {
    AccessLink* link = varOccur->accessLink;
    IrActivationRecord* actvRecord = execContext->avRecord;
    assert(actvRecord->kind == IrAvRecord_Block);
    IrBlockAvRecord* blockAv = &actvRecord->block;
    IrStackArea* accessLinksArea = &blockAv->accessLinks;
    irValue->var.accessLinkLocation = -(actvRecord->fp - accessLinksArea->loc + 1) + link->index;
  }
  irVar->dataLocation = symbol->var->location;
}

IrNode* IrBuildLValue(MemoryArena* arena, IrExecutionContext* execContext, VarOccur* varOccur)
{
  IrNode* irNode = PushElement(arena, IrNode, 1);
  irNode->kind = IrNodeKind_LoadLValue;
  IrValue* irValue = &irNode->value;
  irValue->kind = IrValueKind_Var;
  IrSetVarValue(irValue, execContext, varOccur);
  return irNode;
}

IrNode* IrBuildRValue(MemoryArena* arena, IrExecutionContext* execContext, AstNode* astNode)
{
  IrNode* irNode = PushElement(arena, IrNode, 1);
  if(astNode->kind == AstNodeKind_VarOccur)
  {
    VarOccur* varOccur = &astNode->varOccur;

    irNode->kind = IrNodeKind_LoadRValue;
    IrValue* irValue = &irNode->value;
    irValue->kind = IrValueKind_Var;
    IrSetVarValue(irValue, execContext, varOccur);
  }
  else if(astNode->kind == AstNodeKind_IntNum)
  {
    irNode->kind = IrNodeKind_LoadRValue;
    IrValue* irValue = &irNode->value;
    irValue->kind = IrValueKind_IntNum;
    irValue->intNum = astNode->intNum.value;
  }
  return irNode;
}

IrNode* IrBuildBinExpr(MemoryArena* arena, IrExecutionContext* execContext, BinExpr* binExpr)
{
  IrNode* irNode = 0;
  AstNode* leftOperand = binExpr->leftOperand;
  AstNode* rightOperand = binExpr->rightOperand;
  if(binExpr->op == OperatorKind_Assign)
  {
    assert(leftOperand->kind == AstNodeKind_VarOccur);
    
    irNode = PushElement(arena, IrNode, 1);
    irNode->kind = IrNodeKind_BinOp;
    IrBinOp* irOp = &irNode->op;
    irOp->op = IrOpKind_Store;
    irOp->leftOperand = IrBuildLValue(arena, execContext, &leftOperand->varOccur);
    irOp->rightOperand = IrBuildRValue(arena, execContext, rightOperand);
  }
  return irNode;
}

IrNode* IrBuildCall(MemoryArena* arena, IrExecutionContext* execContext, Call* astCall)
{
  IrNode* irCallNode = PushElement(arena, IrNode, 1);
  irCallNode->kind = IrNodeKind_Call;
  IrCall* irCall = &irCallNode->call;
  IrProc* irProc = astCall->proc->irProc;
  irCall->proc = irProc;

  ListInit(&irCall->actualArgs);
  ListItem* argItem = ListFirstItem(&astCall->actualArgs);
  while(argItem)
  {
    AstNode* astArgNode = argItem->elem;
    IrNode* irArgValue = IrBuildRValue(arena, execContext, astArgNode);
    ListAppend(arena, &irCall->actualArgs, irArgValue);
    argItem = argItem->next;
  }

  return irCallNode;
}

IrNode* IrBuildCallValue(MemoryArena* arena, IrExecutionContext* execContext, Call* astCall)
{
  IrNode* irCallNode = IrBuildCall(arena, execContext, astCall);
  IrNode* irValueNode = PushElement(arena, IrNode, 1);
  irValueNode->kind = IrNodeKind_LoadRValue;
  IrValue* irValue = &irValueNode->value;
  irValue->kind = IrValueKind_Call;
  irValue->call = &irCallNode->call;
  return irValueNode;
}

IrNode* IrBuildProc(MemoryArena* arena, IrExecutionContext* execContext, Proc* proc)
{
  AstNode* bodyNode = proc->body;
  assert(bodyNode->kind == AstNodeKind_Block);
  Block* bodyBlock = &bodyNode->block;

  IrActivationRecord* newActvRecord = PushElement(arena, IrActivationRecord, 1);
  newActvRecord->kind = IrAvRecord_Proc;
  IrProcAvRecord* procAv = &newActvRecord->proc;
  int offset = 0;

  IrStackArea* area = &procAv->ret;
  area->loc = offset;
  area->size = proc->retDataSize;
  offset += area->size;

  area = &procAv->args;
  area->loc = offset;
  area->size = proc->argsDataSize;
  offset += area->size;

  area = &procAv->ctrlLinks;
  area->loc = offset;
  area->size = 3; // ip+sp+fp
  offset += area->size;

  newActvRecord->fp = offset;

  area = &procAv->locals;
  area->loc = offset;
  area->size = bodyBlock->localsDataSize;
  offset += area->size;

  newActvRecord->sp = offset;

//  ListInit(&newActvRecord->foreignAvRecords);
//  {/*>>> access links are not for procs! */
//    ListItem* item = ListFirstItem(&bodyBlock->accessLinks);
//    while(item)
//    {
//      AccessLink* link = item->elem;
//      ListItem* avItem = ListFirstItem(&execContext->avRecordStack);
//      int offset = link->actvRecordOffset - 1;
//      while(offset-- > 0)
//        avItem = avItem->prev;
//      ListAppend(arena, &newActvRecord->foreignAvRecords, avItem->elem);
//
//      item = item->next;
//    }
//  }/*<<<*/

  IrNode* irNode = PushElement(arena, IrNode, 1);
  irNode->kind = IrNodeKind_Proc;
  IrProc* irProc = &irNode->proc;
  irProc->actvRecord = newActvRecord;
  irProc->label = proc->name;
  ListInit(&irProc->instrList);

  String endLabel = {0};
  StringInit(&endLabel, arena);
  AppendString(&endLabel, proc->name);
  AppendString(&endLabel, ".proc-end");
  irProc->endLabel = endLabel.start;

  ListItem* nodeItem = ListFirstItem(&bodyBlock->stmtList);
  while(nodeItem)
  {
    AstNode* astNode = nodeItem->elem;
    if(astNode->kind == AstNodeKind_BinExpr)
    {
      BinExpr* binExpr = &astNode->binExpr;
      assert(binExpr->op == OperatorKind_Assign);
      IrNode* irExpr = IrBuildBinExpr(arena, execContext, binExpr);
      ListAppend(arena, &irProc->instrList, irExpr);
    }
    else if(astNode->kind == AstNodeKind_Call)
    {
      Call* call = &astNode->call;
      IrNode* irCall = IrBuildCall(arena, execContext, call);
      ListAppend(arena, &irProc->instrList, irCall);
    }
    nodeItem = nodeItem->next;
  }

  return irNode;
}

IrNode* IrBuildModule(MemoryArena* arena, IrExecutionContext* execContext, Module* module)
{
  IrNode* irModuleNode = PushElement(arena, IrNode, 1);
  irModuleNode->kind = IrNodeKind_Module;
  IrModule* irModule = &irModuleNode->module;
  ListInit(&irModule->procList);

  ListItem* procItem = ListFirstItem(&module->procList);
  while(procItem)
  {
    AstNode* astProcNode = procItem->elem;
    assert(astProcNode->kind == AstNodeKind_Proc);
    Proc* astProc = &astProcNode->proc;
    IrNode* irProcNode = IrBuildProc(arena, execContext, &astProcNode->proc);
    astProc->irProc = &irProcNode->proc;
    ListAppend(arena, &irModule->procList, irProcNode);
    if(StrMatch(astProcNode->proc.name, "Main"))
      irModule->mainProc = &irProcNode->proc;

    procItem = procItem->next;
  }
  return irModuleNode;
}
/*<<<*/

void PrintInstruction(VmProgram* vmProgram, char* code, ...)
{/*>>>*/
  static char strbuf[128] = {0};
  va_list args;

  va_start(args, code);
  vmProgram->textLen += vsprintf(strbuf, code, args);
  va_end(args);

  AppendString(&vmProgram->text, strbuf);
  AppendString(&vmProgram->text, "\n");
  vmProgram->textLen++;
}/*<<<*/

void EmitInstrReg(MemoryArena* arena, List* instrList, Opcode opcode, RegName reg)
{
  Instruction* instr = PushElement(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->paramType = ParamType_Reg;
  instr->param.reg = reg;
  ListAppend(arena, instrList, instr);
}

void EmitInstrInt(MemoryArena* arena, List* instrList, Opcode opcode, int32 intNum)
{
  Instruction* instr = PushElement(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->paramType = ParamType_Int32;
  instr->param.intNum = intNum;
  ListAppend(arena, instrList, instr);
}

void EmitInstr(MemoryArena* arena, List* instrList, Opcode opcode)
{
  Instruction* instr = PushElement(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->paramType = ParamType__Null;
  ListAppend(arena, instrList, instr);
}

void EmitInstrStr(MemoryArena* arena, List* instrList, Opcode opcode, char* str)
{
  Instruction* instr = PushElement(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->paramType = ParamType_String;
  instr->param.str = str;
  ListAppend(arena, instrList, instr);
}

void GenLoadRValue(MemoryArena*, List*, IrValue*);

void GenCall(MemoryArena* arena, List* code, IrCall* call)
{
  IrProc* proc = call->proc;
  IrActivationRecord* actvRecord = proc->actvRecord;
  assert(actvRecord->kind == IrAvRecord_Proc);
  IrProcAvRecord* procAv = &actvRecord->proc;
  EmitInstrInt(arena, code, Opcode_ALLOC, procAv->ret.size);

  ListItem* argItem = ListFirstItem(&call->actualArgs);
  while(argItem)
  {
    IrValue* argValue = argItem->elem;
    GenLoadRValue(arena, code, argValue);
    argItem = argItem->next;
  }

  EmitInstrStr(arena, code, Opcode_CALL, proc->label);
}

void GenLoadLValue(MemoryArena* arena, List* code, IrValue* value)
{
  assert(value->kind == IrValueKind_Var);
  if(value->var.isNonLocal)
  {
    EmitInstrReg(arena, code, Opcode_PUSH, RegName_FP);
    EmitInstrInt(arena, code, Opcode_PUSH, value->var.accessLinkLocation);
    EmitInstr(arena, code, Opcode_ADD);
    EmitInstr(arena, code, Opcode_LOAD);
  } else
  {
    EmitInstrReg(arena, code, Opcode_PUSH, RegName_FP);
  }
  EmitInstrInt(arena, code, Opcode_PUSH, value->var.dataLocation);
  EmitInstr(arena, code, Opcode_ADD);
}

void GenLoadRValue(MemoryArena* arena, List* code, IrValue* value)
{
  if(value->kind == IrValueKind_Var)
  {
    GenLoadLValue(arena, code, value);
    EmitInstr(arena, code, Opcode_LOAD);
  }
  else if(value->kind == IrValueKind_Call)
  {
    GenCall(arena, code, value->call);
  }
  else if(value->kind == IrValueKind_IntNum)
  {
    EmitInstrInt(arena, code, Opcode_PUSH, value->intNum);
  }
}

void GenCode(MemoryArena* arena, List* code, IrNode* irNode)
{
  switch(irNode->kind)
  {
    case(IrNodeKind_Module):
    {
      IrModule* module = &irNode->module;
      ListItem* item = ListFirstItem(&module->procList);
      while(item)
      {
        IrNode* irNode = item->elem;
        GenCode(arena, code, irNode);
        item = item->next;
      }
    } break;

    case(IrNodeKind_Proc):
    {
      IrProc* proc = &irNode->proc;
      EmitInstrStr(arena, code, Opcode_LABEL, proc->label);

//      {/*>>> Access links - are not for procs! */
//        IrActivationRecord* actvRecord = proc->actvRecord;
//        ListItem* item = ListFirstItem(&actvRecord->foreignAvRecords);
//        while(item)
//        {
//          IrActivationRecord* foreignActvRecord = item->elem;
//          int offsetFp = foreignActvRecord->fp - actvRecord->fp;
//          EmitInstrReg(arena, code, Opcode_PUSH, RegName_FP);
//          EmitInstrInt(arena, code, Opcode_PUSH, offsetFp);
//          EmitInstr(arena, code, Opcode_ADD);
//        }
//      }/*<<<*/

      IrActivationRecord* avRecord = proc->actvRecord;
      IrProcAvRecord* procAv = &avRecord->proc;
      IrStackArea* localsArea = &procAv->locals;
      EmitInstrInt(arena, code, Opcode_ALLOC, localsArea->size);

      {
        ListItem* item = ListFirstItem(&proc->instrList);
        while(item)
        {
          IrNode* irNode = item->elem;
          GenCode(arena, code, irNode);
          item = item->next;
        }
      }

      EmitInstrStr(arena, code, Opcode_LABEL, proc->endLabel);
      EmitInstr(arena, code, Opcode_RETURN);
    } break;

    case(IrNodeKind_LoadLValue):
    {
      IrValue* value = &irNode->value;
      GenLoadLValue(arena, code, value);
    } break;

    case(IrNodeKind_LoadRValue):
    {
      IrValue* value = &irNode->value;
      GenLoadRValue(arena, code, value);
    } break;

    case(IrNodeKind_BinOp):
    {
      IrBinOp* op = &irNode->op;
      GenCode(arena, code, op->leftOperand);
      GenCode(arena, code, op->rightOperand);
      if(op->op == IrOpKind_Store)
        EmitInstr(arena, code, Opcode_STORE);
      else if(op->op == IrOpKind_Add)
        EmitInstr(arena, code, Opcode_ADD);
      else if(op->op == IrOpKind_Sub)
        EmitInstr(arena, code, Opcode_SUB);
      else if(op->op == IrOpKind_Mul)
        EmitInstr(arena, code, Opcode_MUL);
      else if(op->op == IrOpKind_Div)
        EmitInstr(arena, code, Opcode_DIV);
      else
        assert(false);
    } break;

    case(IrNodeKind_Call):
    {
      IrCall* call = &irNode->call;
      GenCall(arena, code, call);
      EmitInstr(arena, code, Opcode_POP);
    } break;
  }
}

char* GetRegNameStr(RegName reg)
{
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
}

void PrintCode(VmProgram* vmProgram)
{
  ListItem* item = ListFirstItem(&vmProgram->instrList);
  while(item)
  {
    Instruction* instr = item->elem;
    switch(instr->opcode)
    {
      case(Opcode_PUSH):
      {
        if(instr->paramType == ParamType_Reg)
          PrintInstruction(vmProgram, "push %s", GetRegNameStr(instr->param.reg));
        else if(instr->paramType == ParamType_Int32)
          PrintInstruction(vmProgram, "push %d", instr->param.intNum);
        else
          assert(false);
      } break;

      case(Opcode_POP):
      {
        if(instr->paramType == ParamType_Reg)
          PrintInstruction(vmProgram, "pop %s", GetRegNameStr(instr->param.reg));
        else if(instr->paramType == ParamType__Null)
          PrintInstruction(vmProgram, "pop");
        else
          assert(false);
      } break;

      case(Opcode_ADD):
      {
        assert(instr->paramType == ParamType__Null);
        PrintInstruction(vmProgram, "add");
      } break;

      case(Opcode_LOAD):
      {
        assert(instr->paramType == ParamType__Null);
        PrintInstruction(vmProgram, "load");
      } break;

      case(Opcode_STORE):
      {
        assert(instr->paramType == ParamType__Null);
        PrintInstruction(vmProgram, "store");
      } break;

      case(Opcode_LABEL):
      {
        assert(instr->paramType == ParamType_String);
        PrintInstruction(vmProgram, "label %s", instr->param.str);
      } break;

      case(Opcode_RETURN):
      {
        assert(instr->paramType == ParamType__Null);
        PrintInstruction(vmProgram, "return");
      } break;

      case(Opcode_ALLOC):
      {
        assert(instr->paramType == ParamType_Int32);
        PrintInstruction(vmProgram, "alloc %d", instr->param.intNum);
      } break;

      case(Opcode_CALL):
      {
        assert(instr->paramType == ParamType_String);
        PrintInstruction(vmProgram, "call %s", instr->param.str);
      } break;

      default:
        assert(false);
    }
    item = item->next;
  }
}

/*>>> Old code gen */
//bool32 BuildIr(MemoryArena* arena, SymbolTable* symbolTable, AstNode* ast)
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
//      ActivationRecord* actvRecord = &block->actvRecord;
//      actvRecord->kind = ActvRecord_Block;
//
//      switch(block->kind)
//      {
//        case Block_Module:
//          {
//            Ast_ListItem* procList = block->module.procList;
//            while(procList && success)
//            {
//              AstNode* procAst = procList->ast;
//              success = BuildIr(arena, symbolTable, procAst);
//
//              procList = procList->nextItem;
//            }
//          } break;
//
//        case Block_Proc:
//          {
//            actvRecord->kind = ActvRecord_Proc;
//            actvRecord->proc.retAreaSize = 1;
//
//            Ast_ListItem* argList = block->proc.argList;
//            int argsAreaSize = 0;
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
//              irArg->location = -(argsAreaSize + CONTROL_LINKS_DATA_SIZE + 1);
//              irArg->size     = 1;
//
//              argsAreaSize += irArg->size;
//
//              DataObjList* irArgItem = PushElement(arena, DataObjList, 1);
//              irArgItem->dataObj = irArg;
//              irArgItem->nextItem = irArgs;
//              irArgs = irArgItem;
//
//              argList = argList->nextItem;
//              argsCount++;
//            }
//            actvRecord->proc.args = irArgs;
//            actvRecord->proc.argsAreaSize = argsAreaSize;
//            actvRecord->proc.argsCount = argsCount;
//
//            Symbol* procSymbol = block->proc.symbol;
//
//            //FIXME Looks like a hack
//            if(StrMatch(procSymbol->name, "Main"))
//            {
//              if(block->proc.argList)
//              {
//                Error("Main() must not have arguments");
//                success = false;
//              }
//            }
//          } break;
//
//        case Block_WhileStmt:
//          {
//            success = BuildIr(arena, symbolTable, block->whileStmt.expr);
//          } break;
//
//        default:
//          assert(false && !"Not implemented");
//      }
//
//      // Process the declared vars
//      Ast_ListItem* declList = block->declVars;
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
//        DataObjList* irLocalItem = PushElement(arena, DataObjList, 1);
//        irLocalItem->dataObj = dataObj;
//        irLocalItem->nextItem = irLocalsList;
//        irLocalsList = irLocalItem;
//
//        declList = declList->nextItem;
//      }
//      actvRecord->localObjects = irLocalsList;
//      actvRecord->localAreaSize = localAreaSize;
//
//      // Process the non-local refs
//      Ast_ListItem* nonLocalsList = block->nonLocalVars;
//      int accessLinkCount = 0;
//      AccessLink* accessLinks = 0;
//      while(nonLocalsList)
//      {
//        AstNode* idAst = nonLocalsList->ast;
//        Symbol* idSymbol = idAst->id.symbol;
//
//        assert(idAst->kind == Ast_Id);
//        assert(idAst->id.declBlockOffset > 0);
//        assert(idSymbol->kind == SymbolKind_Var);
//
//        AccessLink* accessLink = 0;
//        {
//          AccessLink* link = accessLinks;
//          while(link)
//          {
//            if(link->actvRecordOffset == idAst->id.declBlockOffset)
//            {
//              accessLink = link;
//              break;
//            }
//            link = link->nextLink;
//          }
//          if(!accessLink)
//          {
//            accessLink = PushElement(arena, AccessLink, 1);
//            accessLink->actvRecordOffset = idAst->id.declBlockOffset;
//            accessLink->index = accessLinkCount++;
//
//            accessLink->nextLink = accessLinks;
//            accessLinks = accessLink;
//          }
//        }
//
//        idAst->id.accessLink = accessLink;
//
//        nonLocalsList = nonLocalsList->nextItem;
//      }
//      actvRecord->accessLinks = accessLinks;
//      actvRecord->accessLinkCount = accessLinkCount;
//
//      if(success)
//      {
//        Ast_ListItem* stmtList = block->stmtList;
//        while(stmtList && success)
//        {
//          AstNode* stmtAst = stmtList->ast;
//          success = BuildIr(arena, symbolTable, stmtAst);
//          stmtList = stmtList->nextItem;
//        }
//      }
//    } break;
//
//    case Ast_Return:
//    {
//      AstNode* exprAst = ast->ret.expr;
//      success = BuildIr(arena, symbolTable, exprAst);
//      if(success)
//        ast->ret.actvRecord = &ast->ret.block->actvRecord;
//    } break;
//
//    case Ast_Expr:
//    {
//      AstNode* leftOperand = ast->expr.leftOperand;
//      AstNode* rightOperand = ast->expr.rightOperand;
//      success = BuildIr(arena, symbolTable, leftOperand);
//      if(rightOperand)
//        success &= BuildIr(arena, symbolTable, rightOperand);
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
//void GenCodeLValue(VmProgram* vmProgram, AstNode* ast)
//{/*>>>*/
//  switch(ast->kind)
//  {
//    case Ast_Id:
//      {
//        Symbol* symbol = ast->id.symbol;
//        DataObj* dataObj = &symbol->dataObj;
//
//        if(ast->id.isNonLocal)
//        {
//          AccessLink* accessLink = ast->id.accessLink;
//
//          Emit(vmProgram, ";begin load l-value of non-local '%s'", symbol->name);
//          Emit(vmProgram, "push fp");
//          int accessLinkLocation = 2 + accessLink->index;
//          Emit(vmProgram, "push -%d", accessLinkLocation);
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
//void GenCodeRValue(VmProgram* vmProgram, AstNode* ast)
//{/*>>>*/
//  switch(ast->kind)
//  {
//    case Ast_Id:
//      {
//        GenCodeLValue(vmProgram, ast);
//        Emit(vmProgram, "load ;r-value");
//      } break;
//
//    case Ast_Expr:
//      {
//        switch(ast->expr.op)
//        {
//          case Operator_Call:
//            {
//              AstNode* callAst = ast->expr.leftOperand;
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
//                GenCodeRValue(vmProgram, argAst);
//                argList = argList->nextItem;
//              }
//              Emit(vmProgram, ";end arg-eval");
//
//              Emit(vmProgram, "call %s", callAst->call.name);
//
//              ActivationRecord* actvRecord = callAst->call.actvRecord;
//              assert(actvRecord->kind == ActvRecord_Proc);
//              int restoreSp = actvRecord->proc.argsAreaSize;
//
//              if(ast->expr.isStatement)
//                restoreSp += 1; // discard retval
//              if(restoreSp > 0)
//                Emit(vmProgram, "pop %d ;restore callee sp", restoreSp);
//            } break;
//
//          case Operator_Assign:
//            {
//              AstNode* rightSide = ast->expr.rightOperand;
//              GenCodeRValue(vmProgram, rightSide);
//
//              AstNode* leftSide = ast->expr.leftOperand;
//              assert(leftSide->kind == Ast_Id);
//              GenCodeLValue(vmProgram, leftSide);
//              Emit(vmProgram, "store ;'%s'", leftSide->id.name);
//
//              if(ast->expr.isStatement)
//                Emit(vmProgram, "pop"); // discard the r-value
//            } break;
//
//          case Operator_Mul:
//          case Operator_Add:
//          case Operator_Div:
//          case Operator_Sub:
//            {
//              AstNode* leftOperand = ast->expr.leftOperand;
//              GenCodeRValue(vmProgram, leftOperand);
//              AstNode* rightOperand = ast->expr.rightOperand;
//              GenCodeRValue(vmProgram, rightOperand);
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
//        Emit(vmProgram, "push %d", ast->literal.intNum);
//      } break;
//
//    case Ast_Neg:
//      {
//        AstNode* expr = ast->neg.expr;
//        if(expr->kind == Ast_IntNum)
//        {
//          Emit(vmProgram, "push -%d", expr->literal.intNum);
//        } else {
//          GenCodeRValue(vmProgram, expr);
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
//void GenCode(VmProgram* vmProgram, SymbolTable* symbolTable,
//             Block* block, AstNode* ast)
//{/*>>>*/
//  switch(ast->kind)
//  {
//    case Ast_IntNum:
//    case Ast_Expr:
//    case Ast_Id:
//      {
//        GenCodeRValue(vmProgram, ast);
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
//              Emit(vmProgram, "call Main");
//              Emit(vmProgram, "halt");
//
//              Ast_ListItem* procList = block->module.procList;
//              while(procList)
//              {
//                AstNode* procAst = procList->ast;
//                GenCode(vmProgram, symbolTable, block, procAst);
//
//                procList = procList->nextItem;
//              }
//            } break;
//
//          case Block_Proc:
//            {
//              Symbol* procSymbol = block->proc.symbol;
//              Emit(vmProgram, "label %s", procSymbol->name); // entry point
//
//              ActivationRecord* actvRecord = &block->actvRecord;
//              int dataAreaSize = actvRecord->localAreaSize;
//              if(dataAreaSize > 0)
//                Emit(vmProgram, "alloc %d ;local storage", dataAreaSize);
//
//              Ast_ListItem* stmtList = block->stmtList;
//              while(stmtList)
//              {
//                AstNode* stmt = stmtList->ast;
//                GenCode(vmProgram, symbolTable, block, stmt);
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
//              ActivationRecord* actvRecord = &block->actvRecord;
//
//              char label[32] = {};
//              MakeUniqueLabel(symbolTable, label);
//              Emit(vmProgram, "label %s.while-expr", label);
//
//              // conditional expr
//              GenCodeRValue(vmProgram, block->whileStmt.expr);
//              Emit(vmProgram, "jumpz %s.while-break", label);
//
//              if(actvRecord->accessLinkCount > 0)
//              {
//                // Highest indexed access link is first from the top
//                Emit(vmProgram, ";begin set-up of access links");
//
//                AccessLink* accessLink = actvRecord->accessLinks;
//                while(accessLink)
//                {
//                  Emit(vmProgram, "push fp"); // first level up is the caller's activ. record
//
//                  assert(accessLink->actvRecordOffset > 0);
//                  int offset = accessLink->actvRecordOffset;
//                  offset--;
//                  while(offset--)
//                  {
//                    Emit(vmProgram, "decr"); // offset to the fp of actv. record n-1
//                    Emit(vmProgram, "load");
//                  }
//
//                  accessLink = accessLink->nextLink;
//                }
//                Emit(vmProgram, ";end set-up of access links");
//              }
//
//              Emit(vmProgram, "enter");
//
//              if(actvRecord->localAreaSize > 0)
//                Emit(vmProgram, "alloc %d ;local storage", actvRecord->localAreaSize);
//
//              // body
//              Ast_ListItem* stmtList = block->stmtList;
//              while(stmtList)
//              {
//                AstNode* stmt = stmtList->ast;
//                GenCode(vmProgram, symbolTable, block, stmt);
//
//                stmtList = stmtList->nextItem;
//              }
//
//              Emit(vmProgram, "leave");
//              Emit(vmProgram, "pop %d ;discard access links", actvRecord->accessLinkCount);
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
//        GenCodeRValue(vmProgram, ast->ret.expr);
//
//        ActivationRecord* actvRecord = ast->ret.actvRecord;
//        assert(actvRecord->kind == ActvRecord_Proc);
//
//        //FIXME: 'retval' is a DataObj
//        int retValLocation = CONTROL_LINKS_DATA_SIZE + actvRecord->proc.argsAreaSize +
//          actvRecord->proc.retAreaSize;
//
//        // Load l-value of the 'ret' data object
//        Emit(vmProgram, "push fp");
//        assert(ast->ret.interveningBlockCount >= 0);
//        int level = ast->ret.interveningBlockCount;
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
//        int depth = ast->ret.interveningBlockCount;
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
//        GenCodeRValue(vmProgram, ast->ifStmt.expr);
//
//        char* label = MakeUniqueLabel(symbolTable);
//
//        if(ast->ifStmt.bodyElse)
//          Emit(vmProgram, "jumpz %s.else", label);
//        else
//          Emit(vmProgram, "jumpz %s.if-end", label);
//
//        GenCode(vmProgram, symbolTable, ast->ifStmt.body);
//        if(ast->ifStmt.bodyElse)
//        {
//          Emit(vmProgram, "goto %s.if-end", label);
//          Emit(vmProgram, "label %s.else", label);
//          GenCode(vmProgram, symbolTable, ast->ifStmt.bodyElse);
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

uint WriteBytesToFile(char* fileName, char* text, int count)
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

bool32 TranslateHoc(MemoryArena* arena, char* filePath, char* hocProgram, VmProgram* vmProgram)
{/*>>>*/
  bool32 success = false;

  SymbolTable symbolTable = {0};
  symbolTable.arena = arena;

  TokenStream tokenStream = {0};
  tokenStream.arena = arena;
  tokenStream.text = hocProgram;
  tokenStream.cursor = tokenStream.text;
  tokenStream.lineNr = 1;
  //TODO: Compute the absolute path to the file, so that Vim could properly
  // jump from the QuickFix window to the error line in the file.
  tokenStream.filePath = filePath;

  RegisterKeywords(&symbolTable);
  ConsumeToken(&tokenStream, &symbolTable);

  AstNode* astNode = 0;
  success = ParseModule(arena, &tokenStream, &symbolTable, &astNode);

  if(success)
  {
    assert(astNode->kind == AstNodeKind_Module);
    assert(symbolTable.scopeId == 0);
    assert(symbolTable.nestingDepth == 0);

    IrActivationRecord topActvRecord = {0};
    IrExecutionContext execContext = {0};
    ListInit(&execContext.avRecordStack);
    ListAppend(arena, &execContext.avRecordStack, &topActvRecord);
    IrNode* irModuleNode = IrBuildModule(arena, &execContext, &astNode->module);

    ListInit(&vmProgram->instrList);
    GenCode(arena, &vmProgram->instrList, irModuleNode);

    StringInit(&vmProgram->text, arena);
    PrintCode(vmProgram);
  }

  return success;
}/*<<<*/

