#include "lib.cpp"

enum struct Token
{/*>>>*/
  _Null,
  EndOfInput,

  _KeywordBegin,
  If,
  Else,
  While,
  Type,
  Of,
  Array,
  Char,
  Float,
  Void,
  Int,
  Var,
  Proc,
  Struct,
  Return,
  True,
  False,
  _KeywordEnd,

  Id,
  Dot,
  IntNum,
  UpArrow,
  RightArrow,
  Literal,
  OpenBracket,
  CloseBracket,
  Semicolon,
  Colon,
  Comma,
  Star,
  FwdSlash,
  Plus,
  Minus,
  UnaryMinus,
  Equals,
  OpenParens,
  CloseParens,
  OpenBrace,
  CloseBrace,
};/*<<<*/

struct TokenStream
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
};

struct Symbol;
struct AstNode;

enum struct SymbolKind
{
  _Null,
  Keyword,
  Proc,
  Var,
};

struct SymbolTable
{
  Symbol* symbol;
  int     scopeId;
  int     lastScopeId;
  int     nestingDepth;
  int     activeScopes[32];
  char    label[64];
  int     lastLabelId;
  MemoryArena* arena;
};

struct Symbol
{
  SymbolKind kind;
  char*    name;
  int      blockId;
  int      nestingDepth;
  AstNode* astNode;
  Token    kwToken;
  Symbol*  nextSymbol;
};

enum struct OperatorKind
{
  _Null,
  Add,
  Sub,
  Assign,
  Div,
  Mul,
  Call,
  Neg,
};

enum struct AstNodeKind
{
  _Null,
  BinExpr,
  UnrExpr,
  IntNum,
  VarDecl,
  VarOccur,
  Call,
  Block,
  Proc,
  WhileStmt,
  IfStmt,
  ReturnStmt,
  Module,
};

struct AccessLink
{
  int actvRecordOffset;
  int index;
};

struct Module
{
  MListHeader procList;
  AstNode*       body;
};

struct VarDecl
{
  Symbol* symbol;
  char*   name;
  int     location; // relative to fp
  int     dataSize;
};

struct Call
{
  Symbol*     symbol;
  char*       name;
  MListHeader actualArgs;
  AstNode*    proc;
  bool32      isStatement;
};

struct BinExpr
{
  OperatorKind op;
  AstNode* leftOperand;
  AstNode* rightOperand;
  bool32   isStatement;
};

struct UnrExpr
{
  OperatorKind op;
  AstNode* operand;
};

struct VarOccur
{
  Symbol* symbol;
  char*   name;
  int     declBlockOffset;
  bool32  isNonLocal;
  AccessLink* accessLink; // if non-local
};

struct Literal
{
  int32 intNum;
};

struct ReturnStmt
{
  AstNode* expr;
  int interveningBlockCount;
  AstNode* proc;
};

struct Proc
{
  Symbol* symbol;
  char*   name;
  MListHeader formalArgs;
  AstNode* body;
  int argsDataSize;
  int retDataSize;
};

struct IfStmt
{
  AstNode* expr;
  AstNode* body;
  AstNode* elseNode;
};

struct WhileStmt
{
  AstNode* expr;
  AstNode* body;
};

struct Block
{
  AstNode* owner;
  int blockId;
  int nestingDepth;

  MListHeader declVars;
  MListHeader localOccurs;
  MListHeader nonLocalOccurs;
  MListHeader stmtList;
  Block* enclosingBlock;

  int localsDataSize;
  MListHeader accessLinks;
};

struct AstNode
{
  AstNodeKind  kind;

  union {
    BinExpr    binExpr;
    UnrExpr    unrExpr;
    VarDecl    varDecl;
    VarOccur   varOccur;
    Literal    literal;
    Call       call;
    Proc       proc;
    Module     module;
    Block      block;
    ReturnStmt retStmt;
    WhileStmt  whileStmt;
    IfStmt     ifStmt;
  };
};

void BlockInit(SymbolTable* symbolTable, Block* block)
{/*>>>*/
  block->blockId      = symbolTable->scopeId;
  block->nestingDepth = symbolTable->nestingDepth;

  MListHeaderInit(&block->localOccurs);
  MListHeaderInit(&block->nonLocalOccurs);
  MListHeaderInit(&block->stmtList);
  MListHeaderInit(&block->declVars);
  MListHeaderInit(&block->accessLinks);
}/*<<<*/

void BlockProcess(MemoryArena* arena, Block* block)
{/*>>>*/
  MListItem* nodeItem = 0;

  /*>>> Process declared vars */
  nodeItem = MListFirstItem(&block->declVars);
  while(nodeItem)
  {
    AstNode* node = 0;
    VarDecl* varDecl = 0;
    Symbol*  symbol = 0;

    node    = (AstNode*)nodeItem->elem;
    varDecl = &node->varDecl;
    symbol  = node->varDecl.symbol;

    varDecl->location = block->localsDataSize;
    block->localsDataSize += varDecl->dataSize;

    nodeItem = nodeItem->next;
  }
  /*<<<*/
  /*>>> Process non-local var occurrences */
  nodeItem = MListFirstItem(&block->nonLocalOccurs);
  while(nodeItem)
  {
    AstNode*     node = 0;
    VarOccur*    varOccur = 0;
    Symbol*      symbol = 0;
    AccessLink*  link = 0;
    MListItem*   linkItem = 0;
    MListHeader* linksList = 0;

    node = (AstNode*)nodeItem->elem;
    varOccur = &node->varOccur;
    symbol = node->varOccur.symbol;
    linksList = &block->accessLinks;

    linkItem = MListFirstItem(&block->accessLinks);
    while(linkItem)
    {
      link = (AccessLink*)linkItem->elem;
      if(link->actvRecordOffset == varOccur->declBlockOffset)
        break;
      linkItem = linkItem->next;
    }
    if(!link)
    {
      link = PushElement(arena, AccessLink, 1);
      link->actvRecordOffset = varOccur->declBlockOffset;
      link->index = linksList->count;
      MListAppend(arena, linksList, link);
    }
  }
  /*<<<*/
}/*<<<*/

/* old_ip + old_sp + old_fp */
#define MACHINE_STATUS_DATA_SIZE 3

struct IrProgram
{
  String text;
  int    textLen;
  MemoryArena* arena;
};

void SyntaxError(TokenStream* input, char* message, ...)
{
  va_list args;

  fprintf(stderr, "%s(%d) : ", input->filePath, input->lineNr);

  va_start(args, message);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
}

Symbol* SymbolLookup(SymbolTable* symbolTable, char* name)
{/*>>>*/
  Symbol* result = 0;
  Symbol* symbol = 0;

  symbol = symbolTable->symbol;
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
  assert(input->token == Token::Id);

  Symbol* result = 0;
  Symbol* symbol = 0;

  symbol = SymbolLookup(symbolTable, input->lexval.id);
  if(!symbol)
  {
    result = SymbolAdd(symbolTable, input->lexval.id, kind);
  } else {
    if(symbol->kind != SymbolKind::Keyword)
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
  Symbol* symbol = SymbolAdd(symbolTable, name, SymbolKind::Keyword);
  symbol->kwToken = token;
  return symbol;
}/*<<<*/

void RegisterKeywords(SymbolTable* symbolTable)
{/*>>>*/
  AddKeyword(symbolTable, "int", Token::If);
  AddKeyword(symbolTable, "float", Token::Float);
  AddKeyword(symbolTable, "void", Token::Void);
  AddKeyword(symbolTable, "char", Token::Char);
  AddKeyword(symbolTable, "var", Token::Var);
  AddKeyword(symbolTable, "proc", Token::Proc);
  AddKeyword(symbolTable, "type", Token::Type);
  AddKeyword(symbolTable, "struct", Token::Type);
  AddKeyword(symbolTable, "array", Token::Array);
  AddKeyword(symbolTable, "of", Token::Of);
  AddKeyword(symbolTable, "if", Token::If);
  AddKeyword(symbolTable, "else", Token::Else);
  AddKeyword(symbolTable, "while", Token::While);
  AddKeyword(symbolTable, "return", Token::Return);
  AddKeyword(symbolTable, "true", Token::True);
  AddKeyword(symbolTable, "false", Token::False);
}/*<<<*/

bool32 ScopeBegin(SymbolTable* symbolTable)
{/*>>>*/
  int nestingDepth = 0;
  int scopeId = 0;
  
  scopeId = ++symbolTable->lastScopeId;
  symbolTable->scopeId = scopeId;

  nestingDepth = ++symbolTable->nestingDepth;
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
  int     nestingDepth = 0;
  int     scopeId = 0;
  Symbol* symbol = 0;
  
  nestingDepth = --symbolTable->nestingDepth;
  scopeId = symbolTable->activeScopes[nestingDepth];
  assert(scopeId >= 0);
  symbolTable->scopeId = scopeId;

  symbol = symbolTable->symbol;
  while(symbol && symbol->blockId > symbolTable->scopeId)
    symbol = symbol->nextSymbol;
  symbolTable->symbol = symbol;
}/*<<<*/

void MakeUniqueLabel(SymbolTable* symbolTable, char* label)
{
  sprintf(label, "L%d", symbolTable->lastLabelId++);
}

bool32 TokenIsKeyword(Token token)
{/*>>>*/
  return token > Token::_KeywordBegin && token < Token::_KeywordEnd;
}/*<<<*/

char* InstallLexeme(TokenStream* input, char* beginChar, char* endChar)
{/*>>>*/
  //FIXME: If the lexeme had been previously installed then return it.
  int len = 0;
  char* lexeme = 0;

  len = (int)(endChar - beginChar + 1);
  lexeme = PushElement(input->arena, char, len + 1);
  CopySubstr(lexeme, beginChar, endChar);
  return lexeme;
}/*<<<*/

void ConsumeToken(TokenStream* input, SymbolTable* symbolTable)
{/*>>>*/
  char c = 0;

  input->prevToken = input->token;
  input->token = Token::_Null;
  input->lexval = {};

  input->srcLine = input->cursor;
  c = *input->cursor;

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
    char* beginChar = 0, *endChar = 0;
    char* lexeme = 0;
    Symbol* symbol = 0;

    beginChar = input->cursor;
    c = *(++input->cursor);

    while(IsLetterChar(c) || IsNumericChar(c) || c == '_')
      c = *(++input->cursor);

    endChar = input->cursor - 1;
    lexeme = InstallLexeme(input, beginChar, endChar);
    input->lexval.id = lexeme;

    symbol = SymbolLookup(symbolTable, lexeme);
    if(symbol && symbol->kind == SymbolKind::Keyword)
      input->token = symbol->kwToken;
    else
      input->token = Token::Id;
  }
  else if(IsNumericChar(c))
  {
    int num = 0;
    int *value = 0;

    num = c - '0';
    c = *(++input->cursor);

    while(IsNumericChar(c))
    {
      num = (10 * num) + (c - '0');
      c = *(++input->cursor);
    }

    value = PushElement(input->arena, int, 1);
    *value = num;
    input->token = Token::IntNum;
    input->lexval.intNum = value;
  }
  else if(c == '-')
  {
    c = *(++input->cursor);
    if(c == '>')
    {
      input->token = Token::RightArrow;
      ++input->cursor;
    }
    else if(input->prevToken == Token::Equals ||
            input->prevToken == Token::OpenParens ||
            input->prevToken == Token::Star ||
            input->prevToken == Token::Plus ||
            input->prevToken == Token::Comma ||
            input->prevToken == Token::FwdSlash ||
            input->prevToken == Token::Return)
      input->token = Token::UnaryMinus;
    else
      input->token = Token::Minus;
  }
  else if(c == '*')
  {
    input->token = Token::Star;
    ++input->cursor;
  }
  else if(c == '.')
  {
    input->token = Token::Dot;
    ++input->cursor;
  }
  else if(c == '}')
  {
    input->token = Token::CloseBrace;
    ++input->cursor;
  }
  else if(c == '{')
  {
    input->token = Token::OpenBrace;
    ++input->cursor;
  }
  else if(c == '=')
  {
    input->token = Token::Equals;
    ++input->cursor;
  }
  else if(c == '+')
  {
    input->token = Token::Plus;
    ++input->cursor;
  }
  else if(c == '/')
  {
    input->token = Token::FwdSlash;
    ++input->cursor;
  }
  else if(c == '(')
  {
    input->token = Token::OpenParens;
    ++input->cursor;
  }
  else if(c == ')')
  {
    input->token = Token::CloseParens;
    ++input->cursor;
  }
  else if(c == ';')
  {
    input->token = Token::Semicolon;
    ++input->cursor;
  }
  else if(c == ',')
  {
    input->token = Token::Comma;
    ++input->cursor;
  }
  else if(c == ':')
  {
    input->token = Token::Colon;
    ++input->cursor;
  }
  else if(c == '[')
  {
    input->token = Token::OpenParens;
    ++input->cursor;
  }
  else if(c == ']')
  {
    input->token = Token::CloseBracket;
    ++input->cursor;
  }
  else if(c == '^')
  {
    input->token = Token::UpArrow;
    ++input->cursor;
  }
  else if(c == '\0')
    input->token = Token::EndOfInput;
}/*<<<*/

bool32 ParseExpression(MemoryArena*, TokenStream*, SymbolTable*, Block*, AstNode**);
bool32 ParseStatementList(MemoryArena*, TokenStream*, SymbolTable*, Block*);
bool32 ParseActualArgumentList(MemoryArena*, TokenStream*, SymbolTable*, Block*, Call*);
bool32 ParseTerm(MemoryArena*, TokenStream*, SymbolTable*, Block*, AstNode**);

bool32 ParseFactor(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                   Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token::OpenParens)
  {
    ConsumeToken(input, symbolTable);
    success = ParseExpression(arena, input, symbolTable,
                              enclosingBlock, node);
    if(success)
    {
      if(input->token == Token::CloseParens)
        ConsumeToken(input, symbolTable);
      else {
        SyntaxError(input, "Missing ')'");
        success = false;
      }
    }
  }
  else if(input->token == Token::UnaryMinus)
  {
    AstNode* operand = 0;

    ConsumeToken(input, symbolTable);
    success = ParseTerm(arena, input, symbolTable, enclosingBlock, &operand);
    if(success)
    {
      if(operand)
      {
        AstNode* negNode = 0;
        UnrExpr* expr = 0;

        negNode = PushElement(arena, AstNode, 1);
        negNode->kind = AstNodeKind::UnrExpr;
        expr          = &negNode->unrExpr;
        expr->op      = OperatorKind::Neg;
        expr->operand = operand;
        *node = negNode;
      } else {
        SyntaxError(input, "Expression expected after '-'");
        success = false;
      }
    }
  }
  else if(input->token == Token::IntNum)
  {
    AstNode* numNode = 0;

    numNode = PushElement(arena, AstNode, 1);
    numNode->kind = AstNodeKind::IntNum;
    numNode->literal.intNum = *(int32*)input->lexval.intNum;
    *node = numNode;

    ConsumeToken(input, symbolTable);
  }
  else if(input->token == Token::Id)
  {
    Symbol* symbol = 0;

    symbol = SymbolLookup(symbolTable, input->lexval.id);
    if(symbol)
    {
      AstNode* idNode = 0;

      idNode = PushElement(arena, AstNode, 1);
      *node  = idNode;

      ConsumeToken(input, symbolTable);

      if(symbol->kind == SymbolKind::Var)
      {
        VarOccur* varOccur = 0;

        idNode->kind = AstNodeKind::VarOccur;
        varOccur = &idNode->varOccur;
        varOccur->symbol          = symbol;
        varOccur->name            = symbol->name;
        varOccur->declBlockOffset = (symbolTable->nestingDepth - symbol->nestingDepth);
        varOccur->isNonLocal      = (varOccur->declBlockOffset > 0);

        if(varOccur->isNonLocal)
        {
          MListAppend(arena, &enclosingBlock->nonLocalOccurs, idNode);
        }
        else
        {
          assert(varOccur->declBlockOffset == 0);
          MListAppend(arena, &enclosingBlock->localOccurs, idNode);
        }
      }
      else if(symbol->kind == SymbolKind::Proc)
      {
        Call*     call = 0;

        idNode->kind = AstNodeKind::Call;
        call         = &idNode->call;
        call->symbol = symbol;
        call->name   = symbol->name;
        MListHeaderInit(&call->actualArgs);

        if(input->token == Token::OpenParens)
        {
          ConsumeToken(input, symbolTable);
          success = ParseActualArgumentList(arena, input, symbolTable, enclosingBlock, call);
          if(success)
          {
            if(input->token == Token::CloseParens)
            {
              AstNode*     procNode = 0;
              MListHeader* procArgList = 0, *callArgList = 0;

              ConsumeToken(input, symbolTable);
              procNode = symbol->astNode;
              call->proc = symbol->astNode;

              procArgList = &procNode->proc.formalArgs;
              callArgList = &call->actualArgs;
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
      else if(symbol->kind == SymbolKind::Keyword)
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
  else if(input->token == Token::True || input->token == Token::False)
  {
    AstNode* numNode = 0;

    numNode = PushElement(arena, AstNode, 1);
    numNode->kind = AstNodeKind::IntNum;
    numNode->literal.intNum = (input->token == Token::True ? 1 : 0);
    *node = numNode;

    ConsumeToken(input, symbolTable);
  }

  return success;
}/*<<<*/

bool32 ParseRestOfFactors(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                          Block* enclosingBlock, AstNode* leftNode, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token::Star ||
      input->token == Token::FwdSlash)
  {
    AstNode* factorNode = 0;
    AstNode* exprNode = 0;
    BinExpr* expr = 0;

    exprNode = PushElement(arena, AstNode, 1);
    exprNode->kind = AstNodeKind::BinExpr;
    expr = &exprNode->binExpr;
    if(input->token == Token::Star)
      expr->op = OperatorKind::Mul;
    else if(input->token == Token::FwdSlash)
      expr->op = OperatorKind::Div;
    else
      assert(false);

    ConsumeToken(input, symbolTable);
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

  if(input->token == Token::Plus ||
      input->token == Token::Minus)
  {
    AstNode* termNode = 0;
    AstNode* exprNode = 0;
    BinExpr* expr = 0;

    exprNode = PushElement(arena, AstNode, 1);
    exprNode->kind = AstNodeKind::BinExpr;
    expr = &exprNode->binExpr;
    if(input->token == Token::Plus)
      expr->op = OperatorKind::Add;
    else if(input->token == Token::Minus)
      expr->op = OperatorKind::Sub;
    else
      assert(false);

    ConsumeToken(input, symbolTable);
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

  if(input->token == Token::Equals)
  {
    AstNode* rightSide = 0;

    ConsumeToken(input, symbolTable);
    success = ParseExpression(arena, input, symbolTable, enclosingBlock, &rightSide);
    if(success)
    {
      if(rightSide)
      {
        if(leftNode->kind == AstNodeKind::VarOccur)
        {
          AstNode* exprNode = 0;
          BinExpr* expr = 0;

          exprNode = PushElement(arena, AstNode, 1);
          exprNode->kind = AstNodeKind::BinExpr;
          expr = &exprNode->binExpr;
          expr->op = OperatorKind::Assign;
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

  if(input->token == Token::Var)
  {
    ConsumeToken(input, symbolTable);
    if(input->token == Token::Id)
    {
      AstNode* varNode = 0;
      Symbol* symbol = 0;

      varNode = PushElement(arena, AstNode, 1);
      varNode->kind = AstNodeKind::VarDecl;
      *node = varNode;

      symbol = SymbolRegisterNew(symbolTable, input, SymbolKind::Var);
      if(symbol)
      {
        VarDecl* varDecl = 0;

        symbol->astNode = varNode;
        varDecl = &varNode->varDecl;
        varDecl->symbol = symbol;
        varDecl->name   = symbol->name;

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

  if(input->token == Token::Var)
  {
    ConsumeToken(input, symbolTable);
    if(input->token == Token::Id)
    {
      AstNode* varNode = 0;
      Symbol* symbol = 0;

      varNode = PushElement(arena, AstNode, 1);
      varNode->kind = AstNodeKind::VarDecl;
      *node = varNode;

      symbol = SymbolRegisterNew(symbolTable, input, SymbolKind::Var);
      if(symbol)
      {
        VarDecl* varDecl = 0;

        symbol->astNode = varNode;
        varDecl = &varNode->varDecl;
        varDecl->symbol = symbol;
        varDecl->name   = symbol->name;

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
    MListAppend(arena, &proc->formalArgs, argNode);

    if(input->token == Token::Comma)
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
    MListAppend(arena, &call->actualArgs, argNode);

    if(input->token == Token::Comma)
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

  if(input->token == Token::While)
  {
    AstNode* exprNode = 0;

    ConsumeToken(input, symbolTable);
    success = ParseExpression(arena, input, symbolTable, enclosingBlock, &exprNode);
    if(success)
    {
      AstNode*   whileNode = 0;
      WhileStmt* whileStmt = 0;

      whileNode = PushElement(arena, AstNode, 1);
      whileNode->kind = AstNodeKind::WhileStmt;
      whileStmt = &whileNode->whileStmt;
      whileStmt->expr = exprNode;
      *node = whileNode;

      if(input->token == Token::OpenBrace)
      {
        ConsumeToken(input, symbolTable);

        success = ScopeBegin(symbolTable);
        if(success)
        {
          AstNode* blockNode = 0;
          Block*   block = 0;

          blockNode = PushElement(arena, AstNode, 1);
          blockNode->kind = AstNodeKind::Block;
          block = &blockNode->block;
          block->owner = whileNode;
          BlockInit(symbolTable, block);

          success = ParseStatementList(arena, input, symbolTable, block);
          if(success)
          {
            whileStmt->body = blockNode;

            if(input->token == Token::CloseBrace)
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

  if(input->token == Token::If)
  {
    AstNode* exprNode = 0;

    ConsumeToken(input, symbolTable);
    success = ParseExpression(arena, input, symbolTable, enclosingBlock, &exprNode);
    if(success)
    {
      AstNode* ifNode = 0;
      IfStmt*  ifStmt = 0;

      ifNode = PushElement(arena, AstNode, 1);
      ifNode->kind = AstNodeKind::IfStmt;
      ifStmt = &ifNode->ifStmt;
      ifStmt->expr = exprNode;
      *node = ifNode;

      if(input->token == Token::OpenBrace)
      {
        ConsumeToken(input, symbolTable);

        success = ScopeBegin(symbolTable);
        if(success)
        {
          AstNode* blockNode = 0;
          Block*   block = 0;

          blockNode = PushElement(arena, AstNode, 1);
          blockNode->kind = AstNodeKind::Block;
          block = &blockNode->block;
          block->owner = ifNode;
          BlockInit(symbolTable, block);

          success = ParseStatementList(arena, input, symbolTable, block);
          if(success)
          {
            ifStmt->body = blockNode;

            if(input->token == Token::CloseBrace)
            {
              ConsumeToken(input, symbolTable);
              ScopeEnd(symbolTable);

              if(input->token == Token::Else)
              {
                AstNode* elseNode = 0;

                ConsumeToken(input, symbolTable);
                success = ParseIfStatement(arena, input, symbolTable, enclosingBlock, &elseNode);
                if(success)
                {
                  if(elseNode)
                    ifStmt->elseNode = elseNode;
                  else if(input->token == Token::OpenBrace)
                  {
                    ConsumeToken(input, symbolTable);

                    success = ScopeBegin(symbolTable);
                    if(success)
                    {
                      AstNode* blockNode = 0;
                      Block*   block = 0;

                      blockNode = PushElement(arena, AstNode, 1);
                      blockNode->kind = AstNodeKind::Block;
                      block = &blockNode->block;
                      block->owner = ifNode;
                      BlockInit(symbolTable, block);

                      success = ParseStatementList(arena, input, symbolTable, block);
                      if(success)
                      {
                        ifStmt->elseNode = blockNode;
                        if(input->token == Token::CloseBrace)
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

  if(input->token == Token::Proc)
  {
    AstNode* procNode = 0;

    procNode = PushElement(arena, AstNode, 1);
    procNode->kind = AstNodeKind::Proc;
    *node = procNode;

    ConsumeToken(input, symbolTable);
    if(input->token == Token::Id)
    {
      Symbol* symbol = 0;

      symbol = SymbolRegisterNew(symbolTable, input, SymbolKind::Proc);
      if(symbol)
      {
        Proc* proc = 0;

        symbol->astNode = procNode;
        proc = &procNode->proc;
        proc->symbol = symbol;
        proc->name   = symbol->name;
        proc->retDataSize = 1;
        MListHeaderInit(&proc->formalArgs);

        ConsumeToken(input, symbolTable);
        if(input->token == Token::OpenParens)
        {
          ConsumeToken(input, symbolTable);

          // arguments
          success = ScopeBegin(symbolTable);
          if(success)
          {
            AstNode*  blockNode = 0;
            Block* block = 0;

            blockNode = PushElement(arena, AstNode, 1);
            blockNode->kind = AstNodeKind::Block;
            proc->body = blockNode;
            block = &blockNode->block;
            block->owner = procNode;
            BlockInit(symbolTable, block);

            success = ParseFormalArgumentList(arena, input, symbolTable, block, proc);
            if(success)
            {
              MListItem* argItem = 0;

              argItem = MListFirstItem(&proc->formalArgs);
              while(argItem)
              {
                AstNode*    node = 0;
                VarDecl* arg = 0;
                Symbol*  symbol = 0;

                node = (AstNode*)argItem->elem;
                assert(node->kind == AstNodeKind::VarDecl);
                arg = &node->varDecl;
                symbol = node->varDecl.symbol;

                arg->location = -(proc->argsDataSize + MACHINE_STATUS_DATA_SIZE + 1);
                arg->dataSize = 1;
                proc->argsDataSize += arg->dataSize;

                argItem = argItem->next;
              }

              if(input->token == Token::CloseParens)
              {
                ConsumeToken(input, symbolTable);

                if(input->token == Token::OpenBrace)
                {
                  // body
                  ConsumeToken(input, symbolTable);

                  success = ParseStatementList(arena, input, symbolTable, block);
                  if(success)
                  {
                    BlockProcess(arena, block);

                    if(input->token == Token::CloseBrace)
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
                if(input->token == Token::Id)
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
    MListAppend(arena, &module->procList, procNode);

    success = ParseProcedureList(arena, input, symbolTable, enclosingBlock, module);
  }

  return success;
}/*<<<*/

bool32 ParseReturnStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                            Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token::Return)
  {
    AstNode* exprNode = 0;

    ConsumeToken(input, symbolTable);
    success = ParseExpression(arena, input, symbolTable, enclosingBlock, &exprNode);
    if(success)
    {
      if(exprNode)
      {
        AstNode*    retNode = 0;
        ReturnStmt* retStmt = 0;
        Block*      block = 0;

        retNode = PushElement(arena, AstNode, 1);
        retNode->kind = AstNodeKind::ReturnStmt;
        retStmt = &retNode->retStmt;
        retStmt->expr = exprNode;
        *node = retNode;

        int depth = 0;
        block = enclosingBlock;
        while(block)
        {
          AstNode* owner = block->owner;
          if(owner->kind == AstNodeKind::Proc)
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

  enum Alternative
  {
    Alt__Null,
    Alt_Var,
    Alt_Expr,
    Alt_If,
    Alt_While,
    Alt_Return,
  };

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
              if(input->token == Token::Semicolon)
              {
                ConsumeToken(input, symbolTable);

                if(stmtNode->kind == AstNodeKind::BinExpr)
                {
                  BinExpr* expr = &stmtNode->binExpr;
                  if(expr->op == OperatorKind::Assign)
                    expr->isStatement = true;
                  else {
                    SyntaxError(input, "Assignment expression required");
                    success = false;
                  }
                }
                else if(stmtNode->kind == AstNodeKind::Call)
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
              if(input->token == Token::Semicolon)
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
              if(input->token == Token::Semicolon)
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
    while(input->token == Token::Semicolon)
      ConsumeToken(input, symbolTable);

    if(stmtNode->kind == AstNodeKind::VarDecl)
      MListAppend(arena, &block->declVars, stmtNode);
    else
      MListAppend(arena, &block->stmtList, stmtNode);

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
    AstNode* moduleNode = 0;
    AstNode* blockNode = 0;
    Block*   block = 0;
    Module*  module = 0;

    moduleNode = PushElement(arena, AstNode, 1);
    moduleNode->kind = AstNodeKind::Module;

    blockNode = PushElement(arena, AstNode, 1);
    blockNode->kind = AstNodeKind::Block;
    block        = &blockNode->block;
    block->owner = moduleNode;
    BlockInit(symbolTable, block);

    module = &moduleNode->module;
    module->body = blockNode;
    MListHeaderInit(&module->procList);

    success = ParseProcedureList(arena, input, symbolTable, block, module);
    if(success)
    {
      ScopeEnd(symbolTable);

      if(input->token == Token::EndOfInput)
        *node = moduleNode;
      else {
        SyntaxError(input, "End of file expected");
        success = false;
      }
    }
  }

  return success;
}/*<<<*/

enum struct IrNodeKind
{
  _Null,
  LoadRValue,
  LoadLValue,
  BinOperation,
};

struct IrNode
{
  IrNodeKind kind;

  AstNode* astNode;

  union {
    OperatorKind binOp;
  };
};

void BuildIrCode(MemoryArena* arena, MListHeader* code, AstNode* node)
{
  switch(node->kind)
  {
    case AstNodeKind::Module:
      {
        MListItem* procItem = MListFirstItem(&node->module.procList);
        while(procItem)
        {
          AstNode* procNode = (AstNode*)procItem->elem;
          assert(procNode->kind == AstNodeKind::Proc);
          BuildIrCode(arena, code, procNode);
          procItem = procItem->next;
        }
      } break;

    case AstNodeKind::Proc:
      {
        AstNode* bodyNode = node->proc.body;
        assert(bodyNode->kind == AstNodeKind::Block);
        Block* bodyBlock = &bodyNode->block;

        MListItem* nodeItem = MListFirstItem(&bodyBlock->stmtList);
        while(nodeItem)
        {
          AstNode* node = (AstNode*)nodeItem->elem;
          BuildIrCode(arena, code, node);
          nodeItem = nodeItem->next;
        }
      } break;

    case AstNodeKind::BinExpr:
      {
        BinExpr* binExpr = &node->binExpr;
        AstNode* leftOperand = binExpr->leftOperand;
        AstNode* rightOperand = binExpr->rightOperand;

        IrNode* irNode;
        irNode = PushElement(arena, IrNode, 1);
        irNode->kind = IrNodeKind::LoadRValue;
        irNode->astNode = leftOperand;
        MListAppend(arena, code, irNode);

        irNode = PushElement(arena, IrNode, 1);
        irNode->kind = IrNodeKind::LoadRValue;
        irNode->astNode = rightOperand;
        MListAppend(arena, code, irNode);

        irNode = PushElement(arena, IrNode, 1);
        irNode->kind = IrNodeKind::BinOperation;
        irNode->binOp = binExpr->op;
        MListAppend(arena, code, irNode);
      } break;

//      default:
//        assert(false);
  }
}
#if 0/*>>>*/
void EmitStrInstr(MemoryArena* arena, InstrList* code, Opcode opcode, char* str)
{
  Instruction* instr = PushElement(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->paramType = ParamType::String;
  instr->param.str = str;
  InstrList::Add(arena, code, instr);
}

void EmitIntInstr(MemoryArena* arena, InstrList* code, Opcode opcode, int32 intNum)
{
  Instruction* instr = PushElement(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->paramType = ParamType::Int32;
  instr->param.intNum = intNum;
  InstrList::Add(arena, code, instr);
}

void EmitRegInstr(MemoryArena* arena, InstrList* code, Opcode opcode, RegName reg)
{
  Instruction* instr = PushElement(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->paramType = ParamType::Reg;
  instr->param.reg = reg;
  InstrList::Add(arena, code, instr);
}

void EmitInstr(MemoryArena* arena, InstrList* code, Opcode opcode)
{
  Instruction* instr = PushElement(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->paramType = ParamType::_Null;
  instr->param = {};
  InstrList::Add(arena, code, instr);
}

void CodeLValue(MemoryArena* arena, InstrList* code, Node* node)
{
  switch(node->kind)
  {
    case NodeKind::VarOccur:
      {
        VarOccur* varOccur = 0;
        VarDecl*  varDecl = 0;
        Symbol*   symbol = 0;
        Node*     declNode = 0;

        varOccur = &node->varOccur;
        symbol = varOccur->symbol;
        declNode = symbol->astNode;
        assert(declNode->kind == NodeKind::VarDecl);
        varDecl = &declNode->varDecl;

        if(varOccur->isNonLocal)
        {
          AccessLink* link = 0;
          int linkLocation = 0;

          link = varOccur->accessLink;
          EmitRegInstr(arena, code, Opcode::PUSH, RegName::FP);
          linkLocation = 2 + link->index;
          EmitIntInstr(arena, code, Opcode::PUSH, linkLocation);
          EmitInstr(arena, code, Opcode::ADD);
          EmitInstr(arena, code, Opcode::LOAD); // access link is on the stack now
        } else
        {
          EmitRegInstr(arena, code, Opcode::PUSH, RegName::FP);
        }
        EmitIntInstr(arena, code, Opcode::PUSH, varDecl->location);
      } break;

    default:
      assert(false);
  }
}

void CodeRValue(MemoryArena* arena, InstrList* code, Node* node)
{
  switch(node->kind)
  {
    case NodeKind::Call:
      {
        Call* call = &node->call;
        Symbol* symbol = call->symbol;
        Proc* proc = &symbol->astNode->proc;

        EmitIntInstr(arena, code, Opcode::ALLOC, proc->retDataSize);
        NodeItem* nodeItem = NodeList::FirstItem(&call->actualArgs);
        while(nodeItem)
        {
          Node* argNode = nodeItem->elem;
          CodeRValue(arena, code, argNode);
          nodeItem = nodeItem->nextItem;
        }
        EmitStrInstr(arena, code, Opcode::CALL, proc->name);
      } break;

    case NodeKind::IntNum:
      {
        EmitIntInstr(arena, code, Opcode::PUSH, node->literal.intNum);
      } break;

    case NodeKind::BinExpr:
      {
        BinExpr* binExpr = &node->binExpr;
        Node* leftOperand = binExpr->leftOperand;
        Node* rightOperand = binExpr->rightOperand;

        switch(binExpr->op)
        {
          case OperatorKind::Assign:
            {
              assert(leftOperand->kind == NodeKind::VarOccur);

              CodeRValue(arena, code, rightOperand);
              CodeLValue(arena, code, leftOperand);
              EmitInstr(arena, code, Opcode::STORE);
            } break;

          case OperatorKind::Mul:
          case OperatorKind::Add:
          case OperatorKind::Div:
          case OperatorKind::Sub:
            {
              CodeRValue(arena, code, rightOperand);
              CodeRValue(arena, code, leftOperand);
              if(binExpr->op == OperatorKind::Mul)
                EmitInstr(arena, code, Opcode::MUL);
              else if(binExpr->op == OperatorKind::Add)
                EmitInstr(arena, code, Opcode::ADD);
              else if(binExpr->op == OperatorKind::Div)
                EmitInstr(arena, code, Opcode::DIV);
              else if(binExpr->op == OperatorKind::Sub)
                EmitInstr(arena, code, Opcode::SUB);
              else
                assert(false);
            } break;

          default:
            assert(false);
        }
      } break;
  }
}

void Code(MemoryArena* arena, InstrList* code, Node* node)
{
  switch(node->kind)
  {
    case NodeKind::Module:
      {
        NodeItem* procItem = NodeList::FirstItem(&node->module.procList);
        while(procItem)
        {
          Node* procNode = procItem->elem;
          assert(procNode->kind == NodeKind::Proc);
          Code(arena, code, procNode);
          procItem = procItem->nextItem;
        }
      } break;

    case NodeKind::Proc:
      {
        Node* bodyNode = node->proc.body;
        assert(bodyNode->kind == NodeKind::Block);
        Block* bodyBlock = &bodyNode->block;
        if(bodyBlock->localsDataSize > 0)
          EmitIntInstr(arena, code, Opcode::ALLOC, bodyBlock->localsDataSize);

        NodeItem* nodeItem = NodeList::FirstItem(&bodyBlock->stmtList);
        while(nodeItem)
        {
          Node* node = nodeItem->elem;
          Code(arena, code, node);
          nodeItem = nodeItem->nextItem;
        }
      } break;

    case NodeKind::BinExpr:
    case NodeKind::Call:
      {
        CodeRValue(arena, code, node);
        if(node->binExpr.isStatement)
          EmitInstr(arena, code, Opcode::POP);
      } break;
  }
}
#endif/*<<<*/

#if 0
void Emit(IrProgram* irProgram, char* code, ...)
{/*>>>*/
  static char strbuf[128] = {};
  va_list args;

  va_start(args, code);
  irProgram->textLen += vsprintf(strbuf, code, args);
  va_end(args);

  AppendString(&irProgram->text, strbuf);
  AppendString(&irProgram->text, "\n");
  irProgram->textLen++;
}/*<<<*/

bool32 BuildIr(MemoryArena* arena, SymbolTable* symbolTable, AstNode* ast)
{/*>>>*/
  bool32 success = true;

  switch(ast->kind)
  {
    case Ast_Call:
      {
        int actualArgCount = ast->call.argCount;
        Symbol* procSymbol = ast->call.symbol;
        Block* procBlock = procSymbol->ast->block;
        assert(procBlock->kind == Block_Proc);
        assert(actualArgCount == procBlock->proc.argCount);
      } break;

    case Ast_Block:
      {
        Block* block = ast->block;
        ActivationRecord* actvRecord = &block->actvRecord;
        actvRecord->kind = ActvRecord_Block;

        switch(block->kind)
        {
          case Block_Module:
            {
              Ast::ListItem* procList = block->module.procList;
              while(procList && success)
              {
                AstNode* procAst = procList->ast;
                success = BuildIr(arena, symbolTable, procAst);

                procList = procList->nextItem;
              }
            } break;

          case Block_Proc:
            {
              actvRecord->kind = ActvRecord_Proc;
              actvRecord->proc.retAreaSize = 1;

              Ast::ListItem* argList = block->proc.argList;
              int argsAreaSize = 0;
              int argsCount = 0;
              DataObjList* irArgs = 0;
              while(argList)
              {
                AstNode* argAst = argList->ast;
                Symbol* argSymbol = argAst->id.symbol;

                assert(argAst->kind == Ast_Var);
                assert(argSymbol->kind == SymbolKind::Var);

                DataObj* irArg = &argSymbol->dataObj;
                // Note that the storage locations for arguments are negative
                irArg->location = -(argsAreaSize + MACHINE_STATUS_DATA_SIZE + 1);
                irArg->size     = 1;

                argsAreaSize += irArg->size;

                DataObjList* irArgItem = PushElement(arena, DataObjList, 1);
                irArgItem->dataObj = irArg;
                irArgItem->nextItem = irArgs;
                irArgs = irArgItem;

                argList = argList->nextItem;
                argsCount++;
              }
              actvRecord->proc.args = irArgs;
              actvRecord->proc.argsAreaSize = argsAreaSize;
              actvRecord->proc.argsCount = argsCount;

              Symbol* procSymbol = block->proc.symbol;

              //FIXME Looks like a hack
              if(StrMatch(procSymbol->name, "Main"))
              {
                if(block->proc.argList)
                {
                  Error("Main() must not have arguments");
                  success = false;
                }
              }
            } break;

          case Block_WhileStmt:
            {
              success = BuildIr(arena, symbolTable, block->whileStmt.expr);
            } break;

          default:
            assert(false && !"Not implemented");
        }

        // Process the declared vars
        Ast::ListItem* declList = block->declVars;
        DataObjList* irLocalsList = 0;
        int localAreaSize = 0;
        while(declList)
        {
          AstNode* varAst = declList->ast;
          Symbol* varSymbol = varAst->var.symbol;

          assert(varAst->kind == Ast_Var);
          assert(varSymbol->kind == SymbolKind::Var);

          DataObj* dataObj = &varSymbol->dataObj;
          dataObj->location = localAreaSize;
          localAreaSize += dataObj->size;

          DataObjList* irLocalItem = PushElement(arena, DataObjList, 1);
          irLocalItem->dataObj = dataObj;
          irLocalItem->nextItem = irLocalsList;
          irLocalsList = irLocalItem;

          declList = declList->nextItem;
        }
        actvRecord->localObjects = irLocalsList;
        actvRecord->localAreaSize = localAreaSize;

        // Process the non-local refs
        Ast::ListItem* nonLocalsList = block->nonLocalVars;
        int accessLinkCount = 0;
        AccessLink* accessLinks = 0;
        while(nonLocalsList)
        {
          AstNode* idAst = nonLocalsList->ast;
          Symbol* idSymbol = idAst->id.symbol;

          assert(idAst->kind == Ast_Id);
          assert(idAst->id.declBlockOffset > 0);
          assert(idSymbol->kind == SymbolKind::Var);

          AccessLink* accessLink = 0;
          {
            AccessLink* link = accessLinks;
            while(link)
            {
              if(link->actvRecordOffset == idAst->id.declBlockOffset)
              {
                accessLink = link;
                break;
              }
              link = link->nextLink;
            }
            if(!accessLink)
            {
              accessLink = PushElement(arena, AccessLink, 1);
              accessLink->actvRecordOffset = idAst->id.declBlockOffset;
              accessLink->index = accessLinkCount++;

              accessLink->nextLink = accessLinks;
              accessLinks = accessLink;
            }
          }

          idAst->id.accessLink = accessLink;

          nonLocalsList = nonLocalsList->nextItem;
        }
        actvRecord->accessLinks = accessLinks;
        actvRecord->accessLinkCount = accessLinkCount;

        if(success)
        {
          Ast::ListItem* stmtList = block->stmtList;
          while(stmtList && success)
          {
            AstNode* stmtAst = stmtList->ast;
            success = BuildIr(arena, symbolTable, stmtAst);
            stmtList = stmtList->nextItem;
          }
        }
      } break;

    case Ast_Return:
      {
        AstNode* exprAst = ast->ret.expr;
        success = BuildIr(arena, symbolTable, exprAst);
        if(success)
          ast->ret.actvRecord = &ast->ret.block->actvRecord;
      } break;

    case Ast_Expr:
      {
        AstNode* leftOperand = ast->expr.leftOperand;
        AstNode* rightOperand = ast->expr.rightOperand;
        success = BuildIr(arena, symbolTable, leftOperand);
        if(rightOperand)
          success &= BuildIr(arena, symbolTable, rightOperand);
      } break;

    case Ast_Id:
    case Ast_Var:
    case Ast_IntNum:
    case Ast_Neg:
      {} break;

    default:
      assert(false && !"Not implemented");
  }

  return success;
}/*<<<*/

void GenCodeLValue(IrProgram* irProgram, AstNode* ast)
{/*>>>*/
  switch(ast->kind)
  {
    case Ast_Id:
      {
        Symbol* symbol = ast->id.symbol;
        DataObj* dataObj = &symbol->dataObj;

        if(ast->id.isNonLocal)
        {
          AccessLink* accessLink = ast->id.accessLink;

          Emit(irProgram, ";begin load l-value of non-local '%s'", symbol->name);
          Emit(irProgram, "push fp");
          int accessLinkLocation = 2/*magic!*/ + accessLink->index;
          Emit(irProgram, "push -%d", accessLinkLocation);
          Emit(irProgram, "add");
          Emit(irProgram, "load ;access link"); // access link is on the stack now
        } else
        {
          Emit(irProgram, ";begin load l-value of local '%s'", symbol->name);
          Emit(irProgram, "push fp");
        }
        Emit(irProgram, "push %d", dataObj->location);
        Emit(irProgram, "add");
        Emit(irProgram, ";end load of l-value");
      } break;

    default:
      assert(false);
  }
}/*<<<*/

void GenCodeRValue(IrProgram* irProgram, AstNode* ast)
{/*>>>*/
  switch(ast->kind)
  {
    case Ast_Id:
      {
        GenCodeLValue(irProgram, ast);
        Emit(irProgram, "load ;r-value");
      } break;

    case Ast_Expr:
      {
        switch(ast->expr.op)
        {
          case Operator_Call:
            {
              AstNode* callAst = ast->expr.leftOperand;
              Symbol* procSymbol = callAst->call.symbol;

              assert(callAst->kind == Ast_Call);
              assert(procSymbol->kind == SymbolKind::Proc);

              Emit(irProgram, "push 0 ;retval of %s", callAst->call.name);

              Emit(irProgram, ";begin arg-eval");
              Ast::ListItem* argList = callAst->call.argList;
              while(argList)
              {
                AstNode* argAst = argList->ast;
                GenCodeRValue(irProgram, argAst);
                argList = argList->nextItem;
              }
              Emit(irProgram, ";end arg-eval");

              Emit(irProgram, "call %s", callAst->call.name);

              ActivationRecord* actvRecord = callAst->call.actvRecord;
              assert(actvRecord->kind == ActvRecord_Proc);
              int restoreSp = actvRecord->proc.argsAreaSize;

              if(ast->expr.isStatement)
                restoreSp += 1; // discard retval
              if(restoreSp > 0)
                Emit(irProgram, "pop %d ;restore callee sp", restoreSp);
            } break;

          case Operator_Assign:
            {
              AstNode* rightSide = ast->expr.rightOperand;
              GenCodeRValue(irProgram, rightSide);

              AstNode* leftSide = ast->expr.leftOperand;
              assert(leftSide->kind == Ast_Id);
              GenCodeLValue(irProgram, leftSide);
              Emit(irProgram, "store ;'%s'", leftSide->id.name);

              if(ast->expr.isStatement)
                Emit(irProgram, "pop"); // discard the r-value
            } break;

          case Operator_Mul:
          case Operator_Add:
          case Operator_Div:
          case Operator_Sub:
            {
              AstNode* leftOperand = ast->expr.leftOperand;
              GenCodeRValue(irProgram, leftOperand);
              AstNode* rightOperand = ast->expr.rightOperand;
              GenCodeRValue(irProgram, rightOperand);

              if(ast->expr.op == Operator_Mul)
                Emit(irProgram, "mul");
              else if(ast->expr.op == Operator_Add)
                Emit(irProgram, "add");
              else if(ast->expr.op == Operator_Div)
                Emit(irProgram, "div");
              else if(ast->expr.op == Operator_Sub)
                Emit(irProgram, "sub");
              else
                assert(false);
            } break;
        }
      } break;

    case Ast_IntNum:
      {
        Emit(irProgram, "push %d", ast->literal.intNum);
      } break;

    case Ast_Neg:
      {
        AstNode* expr = ast->neg.expr;
        if(expr->kind == Ast_IntNum)
        {
          Emit(irProgram, "push -%d", expr->literal.intNum);
        } else {
          GenCodeRValue(irProgram, expr);
          Emit(irProgram, "push -1");
          Emit(irProgram, "mul");
        }
      } break;

    default:
      assert(false);
  }
}/*<<<*/

void GenCode(IrProgram* irProgram, SymbolTable* symbolTable,
             Block* block, AstNode* ast)
{/*>>>*/
  switch(ast->kind)
  {
    case Ast_IntNum:
    case Ast_Expr:
    case Ast_Id:
      {
        GenCodeRValue(irProgram, ast);
      } break;

    case Ast_Var:
      {} break;

    case Ast_Block:
      {
        Block* block = ast->block;

        switch(block->kind)
        {
          case Block_Module:
            {
              Emit(irProgram, "push 0 ;main retval");
              Emit(irProgram, "call Main");
              Emit(irProgram, "halt");

              Ast::ListItem* procList = block->module.procList;
              while(procList)
              {
                AstNode* procAst = procList->ast;
                GenCode(irProgram, symbolTable, block, procAst);

                procList = procList->nextItem;
              }
            } break;

          case Block_Proc:
            {
              Symbol* procSymbol = block->proc.symbol;
              Emit(irProgram, "label %s", procSymbol->name); // entry point

              ActivationRecord* actvRecord = &block->actvRecord;
              int dataAreaSize = actvRecord->localAreaSize;
              if(dataAreaSize > 0)
                Emit(irProgram, "alloc %d ;local storage", dataAreaSize);

              Ast::ListItem* stmtList = block->stmtList;
              while(stmtList)
              {
                AstNode* stmt = stmtList->ast;
                GenCode(irProgram, symbolTable, block, stmt);

                stmtList = stmtList->nextItem;
              }

              Emit(irProgram, "label %s.end-proc", procSymbol->name);
              Emit(irProgram, "return");
            } break;

          case Block_WhileStmt:
            {
              ActivationRecord* actvRecord = &block->actvRecord;

              char label[32] = {};
              MakeUniqueLabel(symbolTable, label);
              Emit(irProgram, "label %s.while-expr", label);

              // conditional expr
              GenCodeRValue(irProgram, block->whileStmt.expr);
              Emit(irProgram, "jumpz %s.while-break", label);

              if(actvRecord->accessLinkCount > 0)
              {
                // Highest indexed access link is first from the top
                Emit(irProgram, ";begin set-up of access links");

                AccessLink* accessLink = actvRecord->accessLinks;
                while(accessLink)
                {
                  Emit(irProgram, "push fp"); // first level up is the caller's activ. record

                  assert(accessLink->actvRecordOffset > 0);
                  int offset = accessLink->actvRecordOffset;
                  offset--;
                  while(offset--)
                  {
                    Emit(irProgram, "decr"); // offset to the fp of actv. record n-1
                    Emit(irProgram, "load");
                  }

                  accessLink = accessLink->nextLink;
                }
                Emit(irProgram, ";end set-up of access links");
              }

              Emit(irProgram, "enter");

              if(actvRecord->localAreaSize > 0)
                Emit(irProgram, "alloc %d ;local storage", actvRecord->localAreaSize);

              // body
              Ast::ListItem* stmtList = block->stmtList;
              while(stmtList)
              {
                AstNode* stmt = stmtList->ast;
                GenCode(irProgram, symbolTable, block, stmt);

                stmtList = stmtList->nextItem;
              }

              Emit(irProgram, "leave");
              Emit(irProgram, "pop %d ;discard access links", actvRecord->accessLinkCount);

              Emit(irProgram, "goto %s.while-expr", label);
              Emit(irProgram, "label %s.while-break", label);
            } break;

          default:
            assert(false && !"Not implemented");
        }
      } break;

    case Ast_Call:
      {
        Emit(irProgram, "call %s", ast->call.name);
      } break;

    case Ast_Return:
      {
        GenCodeRValue(irProgram, ast->ret.expr);

        ActivationRecord* actvRecord = ast->ret.actvRecord;
        assert(actvRecord->kind == ActvRecord_Proc);

        //FIXME: 'retval' is a DataObj
        int retValLocation = MACHINE_STATUS_DATA_SIZE + actvRecord->proc.argsAreaSize +
          actvRecord->proc.retAreaSize;

        // Load l-value of the 'ret' data object
        Emit(irProgram, "push fp");
        assert(ast->ret.interveningBlockCount >= 0);
        int level = ast->ret.interveningBlockCount;
        while(level--)
        {
          Emit(irProgram, "decr"); // offset to the fp of actv. record n-1
          Emit(irProgram, "load");
        }
        //--

        Emit(irProgram, "push %d ;location of retval", -retValLocation); // note the negative sign
        Emit(irProgram, "add");
        Emit(irProgram, "store ;retval");

        // Exit from the enclosing procedure and any of the intervening blocks
        Block* block = ast->ret.block; 
        Symbol* procSymbol = block->proc.symbol;
        int depth = ast->ret.interveningBlockCount;
        assert(depth >= 0);
        while(depth--)
          Emit(irProgram, "leave");
        Emit(irProgram, "goto %s.end-proc", procSymbol->name);
      } break;
#if 0
    case Ast_IfStmt:
      {
        Emit(irProgram, ";if-begin");

        // conditional
        GenCodeRValue(irProgram, ast->ifStmt.expr);

        char* label = MakeUniqueLabel(symbolTable);

        if(ast->ifStmt.bodyElse)
          Emit(irProgram, "jumpz %s.else", label);
        else
          Emit(irProgram, "jumpz %s.if-end", label);

        GenCode(irProgram, symbolTable, ast->ifStmt.body);
        if(ast->ifStmt.bodyElse)
        {
          Emit(irProgram, "goto %s.if-end", label);
          Emit(irProgram, "label %s.else", label);
          GenCode(irProgram, symbolTable, ast->ifStmt.bodyElse);
        }

        Emit(irProgram, "label %s.if-end", label);

      } break;

    case Ast_WhileStmt:
      {
      } break;
#endif
    default:
      assert(false && !"Not implemented");
  }
}/*<<<*/
#endif

uint WriteBytesToFile(char* fileName, char* text, int count)
{
  uint bytesWritten = 0;
  FILE* hFile = fopen(fileName, "wb");
  if(hFile)
  {
    bytesWritten = (uint)fwrite(text, 1, count, hFile);
    fclose(hFile);
  }
  return bytesWritten;
}

bool32 TranslateHoc(MemoryArena* arena, char* filePath, char* hocProgram, IrProgram* irProgram)
{/*>>>*/
  SymbolTable symbolTable = {};
  TokenStream tokenStream = {};
  AstNode*       module = 0;
  bool32           success = false;

  symbolTable.arena = arena;

  tokenStream.arena  = arena;
  tokenStream.text   = hocProgram;
  tokenStream.cursor = tokenStream.text;
  tokenStream.lineNr = 1;
  //TODO: Compute the absolute path to the file, so that Vim could properly
  // jump from the QuickFix window to the error line in the file.
  tokenStream.filePath = filePath;

  RegisterKeywords(&symbolTable);
  ConsumeToken(&tokenStream, &symbolTable);

  success = ParseModule(arena, &tokenStream, &symbolTable, &module);
//  if(success)
//    success = BuildIr(arena, &symbolTable, moduleAst);

  if(success)
  {
    MListHeader code = {};
    MListHeaderInit(&code);
    BuildIrCode(arena, &code, module);

    StringInit(&irProgram->text, arena);

    //GenCode(irProgram, &symbolTable, block, moduleAst);

    assert(symbolTable.scopeId == 0);
    assert(symbolTable.nestingDepth == 0);
  }

  return success;
}/*<<<*/
