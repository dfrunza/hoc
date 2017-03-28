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
  List     procList;
  AstNode* body;
};

struct VarDecl
{
  Symbol* symbol;
  char*   name;
  int     location; // relative to fp
  int     dataSize;
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
  Symbol*     symbol;
  char*       name;
  int         declBlockOffset;
  bool32      isNonLocal;
  AccessLink* accessLink; // if non-local
  VarDecl*    varDecl;
};

struct IntNum
{
  int32 value;
};

struct ReturnStmt
{
  AstNode* expr;
  int interveningBlockCount;
  AstNode* proc;
};

struct IrProc;
struct Proc
{
  Symbol*  symbol;
  char*    name;
  List     formalArgs;
  AstNode* body;
  int      argsDataSize;
  int      retDataSize;
  IrProc   *irProc;
};

struct Call
{
  Symbol* symbol;
  char*   name;
  List    actualArgs;
  Proc*   proc;
  bool32  isStatement;
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
  int      blockId;
  int      nestingDepth;
  List     declVars;
  List     localOccurs;
  List     nonLocalOccurs;
  List     stmtList;
  Block*   enclosingBlock;
  int      localsDataSize;
  List     accessLinks;
  int      accessLinksAreaSize;
};

struct AstNode
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
};

enum struct SymbolKind
{
  _Null,
  Keyword,
  Proc,
  Var,
};

struct Symbol
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
};

struct VmProgram
{
  String       text;
  int          textLen;
  List         instrList;
  MemoryArena* arena;
};

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
    AstNode* node = (AstNode*)nodeItem->elem;
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
    AstNode* node = (AstNode*)nodeItem->elem;
    VarOccur* varOccur = &node->varOccur;
    List* linksList = &block->accessLinks;

    ListItem* linkItem = ListFirstItem(&block->accessLinks);
    AccessLink* link = 0;
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
      ListAppend(arena, linksList, link);
    }
    block->accessLinksAreaSize = linksList->count;
  }
  /*<<<*/
}/*<<<*/

/* old_ip + old_sp + old_fp */
//#define CONTROL_LINKS_DATA_SIZE 3

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
  assert(input->token == Token::Id);

  Symbol* result = 0;

  Symbol* symbol = SymbolLookup(symbolTable, input->lexval.id);
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
  return token > Token::_KeywordBegin && token < Token::_KeywordEnd;
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
  input->token = Token::_Null;
  input->lexval = {};

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
    if(symbol && symbol->kind == SymbolKind::Keyword)
      input->token = symbol->kwToken;
    else
      input->token = Token::Id;
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
    ConsumeToken(input, symbolTable);
    AstNode* operand = 0;
    success = ParseTerm(arena, input, symbolTable, enclosingBlock, &operand);
    if(success)
    {
      if(operand)
      {
        AstNode* negNode = PushElement(arena, AstNode, 1);
        negNode->kind = AstNodeKind::UnrExpr;
        UnrExpr* expr = &negNode->unrExpr;
        expr->op = OperatorKind::Neg;
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
    AstNode* numNode = PushElement(arena, AstNode, 1);
    numNode->kind = AstNodeKind::IntNum;
    numNode->intNum.value = *(int32*)input->lexval.intNum;
    *node = numNode;

    ConsumeToken(input, symbolTable);
  }
  else if(input->token == Token::Id)
  {
    Symbol* symbol = SymbolLookup(symbolTable, input->lexval.id);
    if(symbol)
    {
      AstNode* idNode = PushElement(arena, AstNode, 1);
      *node = idNode;

      ConsumeToken(input, symbolTable);

      if(symbol->kind == SymbolKind::Var)
      {
        idNode->kind = AstNodeKind::VarOccur;
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
      else if(symbol->kind == SymbolKind::Proc)
      {
        idNode->kind = AstNodeKind::Call;
        Call* call = &idNode->call;
        call->symbol = symbol;
        call->name = symbol->name;
        call->proc = symbol->proc;
        ListInit(&call->actualArgs);

        if(input->token == Token::OpenParens)
        {
          ConsumeToken(input, symbolTable);
          success = ParseActualArgumentList(arena, input, symbolTable, enclosingBlock, call);
          if(success)
          {
            if(input->token == Token::CloseParens)
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
    AstNode* numNode = PushElement(arena, AstNode, 1);
    numNode->kind = AstNodeKind::IntNum;
    numNode->intNum.value = (input->token == Token::True ? 1 : 0);
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
    AstNode* exprNode = PushElement(arena, AstNode, 1);
    exprNode->kind = AstNodeKind::BinExpr;
    BinExpr* expr = &exprNode->binExpr;
    if(input->token == Token::Star)
      expr->op = OperatorKind::Mul;
    else if(input->token == Token::FwdSlash)
      expr->op = OperatorKind::Div;
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

  if(input->token == Token::Plus ||
      input->token == Token::Minus)
  {
    AstNode* exprNode = PushElement(arena, AstNode, 1);
    exprNode->kind = AstNodeKind::BinExpr;
    BinExpr* expr = &exprNode->binExpr;
    if(input->token == Token::Plus)
      expr->op = OperatorKind::Add;
    else if(input->token == Token::Minus)
      expr->op = OperatorKind::Sub;
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

  if(input->token == Token::Equals)
  {
    ConsumeToken(input, symbolTable);
    AstNode* rightSide = 0;
    success = ParseExpression(arena, input, symbolTable, enclosingBlock, &rightSide);
    if(success)
    {
      if(rightSide)
      {
        if(leftNode->kind == AstNodeKind::VarOccur)
        {
          AstNode* exprNode = PushElement(arena, AstNode, 1);
          exprNode->kind = AstNodeKind::BinExpr;
          BinExpr* expr = &exprNode->binExpr;
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
      AstNode* varNode = PushElement(arena, AstNode, 1);
      varNode->kind = AstNodeKind::VarDecl;
      *node = varNode;

      Symbol* symbol = SymbolRegisterNew(symbolTable, input, SymbolKind::Var);
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

  if(input->token == Token::Var)
  {
    ConsumeToken(input, symbolTable);
    if(input->token == Token::Id)
    {
      AstNode* varNode = PushElement(arena, AstNode, 1);
      varNode->kind = AstNodeKind::VarDecl;
      *node = varNode;

      Symbol* symbol = SymbolRegisterNew(symbolTable, input, SymbolKind::Var);
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
    ListAppend(arena, &call->actualArgs, argNode);

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
    ConsumeToken(input, symbolTable);
    AstNode* exprNode = 0;
    success = ParseExpression(arena, input, symbolTable, enclosingBlock, &exprNode);
    if(success)
    {
      AstNode* whileNode = PushElement(arena, AstNode, 1);
      whileNode->kind = AstNodeKind::WhileStmt;
      WhileStmt* whileStmt = &whileNode->whileStmt;
      whileStmt->expr = exprNode;
      *node = whileNode;

      if(input->token == Token::OpenBrace)
      {
        ConsumeToken(input, symbolTable);

        success = ScopeBegin(symbolTable);
        if(success)
        {
          AstNode* blockNode = PushElement(arena, AstNode, 1);
          blockNode->kind = AstNodeKind::Block;
          Block* block = &blockNode->block;
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
    ConsumeToken(input, symbolTable);
    AstNode* exprNode = 0;
    success = ParseExpression(arena, input, symbolTable, enclosingBlock, &exprNode);
    if(success)
    {
      AstNode* ifNode = PushElement(arena, AstNode, 1);
      ifNode->kind = AstNodeKind::IfStmt;
      IfStmt* ifStmt = &ifNode->ifStmt;
      ifStmt->expr = exprNode;
      *node = ifNode;

      if(input->token == Token::OpenBrace)
      {
        ConsumeToken(input, symbolTable);

        success = ScopeBegin(symbolTable);
        if(success)
        {
          AstNode* blockNode = PushElement(arena, AstNode, 1);
          blockNode->kind = AstNodeKind::Block;
          Block* block = &blockNode->block;
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
                ConsumeToken(input, symbolTable);
                AstNode* elseNode = 0;
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
                      AstNode* blockNode = PushElement(arena, AstNode, 1);
                      blockNode->kind = AstNodeKind::Block;
                      Block* block = &blockNode->block;
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
    AstNode* procNode = PushElement(arena, AstNode, 1);
    procNode->kind = AstNodeKind::Proc;
    *node = procNode;

    ConsumeToken(input, symbolTable);
    if(input->token == Token::Id)
    {
      Symbol* symbol = SymbolRegisterNew(symbolTable, input, SymbolKind::Proc);
      if(symbol)
      {
        Proc* proc = &procNode->proc;
        proc->symbol = symbol;
        proc->name   = symbol->name;
        proc->retDataSize = 1;
        ListInit(&proc->formalArgs);
        symbol->proc = proc;

        ConsumeToken(input, symbolTable);
        if(input->token == Token::OpenParens)
        {
          ConsumeToken(input, symbolTable);

          // arguments
          success = ScopeBegin(symbolTable);
          if(success)
          {
            AstNode* blockNode = PushElement(arena, AstNode, 1);
            blockNode->kind = AstNodeKind::Block;
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
                AstNode* node = (AstNode*)argItem->elem;
                assert(node->kind == AstNodeKind::VarDecl);
                VarDecl* arg = &node->varDecl;
                Symbol* symbol = node->varDecl.symbol;
                symbol->var = arg;

                arg->location = proc->argsDataSize;
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
                    BlockProcessVars(arena, block);

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
    ListAppend(arena, &module->procList, procNode);

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
    ConsumeToken(input, symbolTable);
    AstNode* exprNode = 0;
    success = ParseExpression(arena, input, symbolTable, enclosingBlock, &exprNode);
    if(success)
    {
      if(exprNode)
      {
        AstNode* retNode = PushElement(arena, AstNode, 1);
        retNode->kind = AstNodeKind::ReturnStmt;
        ReturnStmt* retStmt = &retNode->retStmt;
        retStmt->expr = exprNode;
        *node = retNode;

        int depth = 0;
        Block* block = enclosingBlock;
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
    moduleNode->kind = AstNodeKind::Module;

    AstNode* blockNode = PushElement(arena, AstNode, 1);
    blockNode->kind = AstNodeKind::Block;
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
/*<<<*/

/*>>> IR */
enum struct IrNodeKind
{
  _Null,
  LoadRValue,
  LoadLValue,
  BinOp,
  Proc,
  Module,
  Call,
};

enum struct IrOpKind
{
  _Null,
  Add,
  Mul,
  Sub,
  Div,
  Store,
};

struct IrNode;

enum struct IrValueKind
{
  _Null,
  Var,
  IntNum,
  Call,
};

struct IrVar
{
  bool32 isNonLocal;
  int    accessLinkLocation;
  int    dataLocation;
};

struct IrCall
{
  IrProc* proc;
  List    actualArgs;
};

struct IrValue
{
  IrValueKind kind;

  union {
    int32   intNum;
    IrVar   var;
    IrCall* call;
  };
};

struct IrStackArea
{
  int size;
  int loc;
};

struct IrActivationRecord
{
  IrStackArea ret;
  IrStackArea args;
  IrStackArea accessLinks;
  IrStackArea ctrlLinks;
  IrStackArea locals;
  int fpLoc;
  int spLoc;
};

struct IrBinOp
{
  IrOpKind op;
  IrNode*  leftOperand;
  IrNode*  rightOperand;
};

struct IrProc
{
  char* label;
  char* endLabel;
  List  instrList;
  IrActivationRecord* actvRecord;
};

struct IrModule
{
  IrProc* mainProc;
  List    procList;
};

struct IrNode
{
  IrNodeKind kind;

  union {
    IrValue  value;
    IrBinOp  op;
    IrProc   proc;
    IrModule module;
    IrCall   call;
  };
};

void IrSetVarValue(IrValue* irValue, IrActivationRecord* actvRecord, VarOccur* varOccur)
{
  Symbol* symbol = varOccur->symbol;
  IrVar* irVar = &irValue->var;
  irVar->isNonLocal = varOccur->isNonLocal;
  if(irVar->isNonLocal)
  {
    AccessLink* link = varOccur->accessLink;
    IrStackArea* accessLinksArea = &actvRecord->accessLinks;
    irValue->var.accessLinkLocation = -(actvRecord->fpLoc - accessLinksArea->loc + 1) + link->index;
  }
  irVar->dataLocation = symbol->var->location;
}

IrNode* IrBuildLValue(MemoryArena* arena, IrActivationRecord* actvRecord, VarOccur* varOccur)
{
  IrNode* irNode = PushElement(arena, IrNode, 1);
  irNode->kind = IrNodeKind::LoadLValue;
  IrValue* irValue = &irNode->value;
  irValue->kind = IrValueKind::Var;
  IrSetVarValue(irValue, actvRecord, varOccur);
  return irNode;
}

IrNode* IrBuildRValue(MemoryArena* arena, IrActivationRecord* actvRecord, AstNode* astNode)
{
  IrNode* irNode = PushElement(arena, IrNode, 1);
  if(astNode->kind == AstNodeKind::VarOccur)
  {
    VarOccur* varOccur = &astNode->varOccur;

    irNode->kind = IrNodeKind::LoadRValue;
    IrValue* irValue = &irNode->value;
    irValue->kind = IrValueKind::Var;
    IrSetVarValue(irValue, actvRecord, varOccur);
  }
  else if(astNode->kind == AstNodeKind::IntNum)
  {
    irNode->kind = IrNodeKind::LoadRValue;
    IrValue* irValue = &irNode->value;
    irValue->kind = IrValueKind::IntNum;
    irValue->intNum = astNode->intNum.value;
  }
  return irNode;
}

IrNode* IrBuildBinExpr(MemoryArena* arena, IrActivationRecord* actvRecord, BinExpr* binExpr)
{
  IrNode* irNode = 0;
  AstNode* leftOperand = binExpr->leftOperand;
  AstNode* rightOperand = binExpr->rightOperand;
  if(binExpr->op == OperatorKind::Assign)
  {
    assert(leftOperand->kind == AstNodeKind::VarOccur);
    
    irNode = PushElement(arena, IrNode, 1);
    irNode->kind = IrNodeKind::BinOp;
    IrBinOp* irOp = &irNode->op;
    irOp->op = IrOpKind::Store;
    irOp->leftOperand = IrBuildLValue(arena, actvRecord, &leftOperand->varOccur);
    irOp->rightOperand = IrBuildRValue(arena, actvRecord, rightOperand);
  }
  return irNode;
}

IrNode* IrBuildCall(MemoryArena* arena, IrActivationRecord* actvRecord, Call* astCall)
{
  IrNode* irCallNode = PushElement(arena, IrNode, 1);
  irCallNode->kind = IrNodeKind::Call;
  IrCall* irCall = &irCallNode->call;
  IrProc* irProc = astCall->proc->irProc;
  irCall->proc = irProc;

  IrActivationRecord* callActvRecord = irProc->actvRecord;

  ListInit(&irCall->actualArgs);
  ListItem* argItem = ListFirstItem(&astCall->actualArgs);
  while(argItem)
  {
    AstNode* astArgNode = (AstNode*)argItem->elem;
    IrNode* irArgValue = IrBuildRValue(arena, actvRecord, astArgNode);
    ListAppend(arena, &irCall->actualArgs, irArgValue);
    argItem = argItem->next;
  }

  return irCallNode;
}

IrNode* IrBuildCallValue(MemoryArena* arena, IrActivationRecord* actvRecord, Call* astCall)
{
  IrNode* irCallNode = IrBuildCall(arena, actvRecord, astCall);
  IrNode* irValueNode = PushElement(arena, IrNode, 1);
  irValueNode->kind = IrNodeKind::LoadRValue;
  IrValue* irValue = &irValueNode->value;
  irValue->kind = IrValueKind::Call;
  irValue->call = &irCallNode->call;
  return irValueNode;
}

IrNode* IrBuildProc(MemoryArena* arena, Proc* proc)
{
  AstNode* bodyNode = proc->body;
  assert(bodyNode->kind == AstNodeKind::Block);
  Block* bodyBlock = &bodyNode->block;

  IrActivationRecord* actvRecord = PushElement(arena, IrActivationRecord, 1);
  int offset = 0;

  IrStackArea* area = &actvRecord->ret;
  area->loc = offset;
  area->size = proc->retDataSize;
  offset += area->size;

  area = &actvRecord->args;
  area->loc = offset;
  area->size = proc->argsDataSize;
  offset += area->size;

  area = &actvRecord->accessLinks;
  area->loc = offset;
  area->size = bodyBlock->accessLinks.count;
  offset += area->size;

  area = &actvRecord->ctrlLinks;
  area->loc = offset;
  area->size = 3; // ip+sp+fp
  offset += area->size;
  
  actvRecord->fpLoc = offset;

  area = &actvRecord->locals;
  area->loc = offset;
  area->size = bodyBlock->localsDataSize;
  offset += area->size;

  actvRecord->spLoc = offset;

  IrNode* irNode = PushElement(arena, IrNode, 1);
  irNode->kind = IrNodeKind::Proc;
  IrProc* irProc = &irNode->proc;
  irProc->actvRecord = actvRecord;
  irProc->label = proc->name;
  ListInit(&irProc->instrList);

  String endLabel = {};
  StringInit(&endLabel, arena);
  AppendString(&endLabel, proc->name);
  AppendString(&endLabel, ".proc-end");
  irProc->endLabel = endLabel.start;

  ListItem* nodeItem = ListFirstItem(&bodyBlock->stmtList);
  while(nodeItem)
  {
    AstNode* astNode = (AstNode*)nodeItem->elem;
    if(astNode->kind == AstNodeKind::BinExpr)
    {
      BinExpr* binExpr = &astNode->binExpr;
      assert(binExpr->op == OperatorKind::Assign);
      IrNode* irExpr = IrBuildBinExpr(arena, actvRecord, binExpr);
      ListAppend(arena, &irProc->instrList, irExpr);
    }
    else if(astNode->kind == AstNodeKind::Call)
    {
      Call* call = &astNode->call;
      IrNode* irCall = IrBuildCall(arena, actvRecord, call);
      ListAppend(arena, &irProc->instrList, irCall);
    }
    nodeItem = nodeItem->next;
  }

  return irNode;
}

IrNode* IrBuildModule(MemoryArena* arena, Module* module)
{
  IrNode* irModuleNode = PushElement(arena, IrNode, 1);
  irModuleNode->kind = IrNodeKind::Module;
  IrModule* irModule = &irModuleNode->module;
  ListInit(&irModule->procList);

  ListItem* procItem = ListFirstItem(&module->procList);
  while(procItem)
  {
    AstNode* astProcNode = (AstNode*)procItem->elem;
    assert(astProcNode->kind == AstNodeKind::Proc);
    Proc* astProc = &astProcNode->proc;
    IrNode* irProcNode = IrBuildProc(arena, &astProcNode->proc);
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
  static char strbuf[128] = {};
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
  instr->paramType = ParamType::Reg;
  instr->param.reg = reg;
  ListAppend(arena, instrList, instr);
}

void EmitInstrInt(MemoryArena* arena, List* instrList, Opcode opcode, int32 intNum)
{
  Instruction* instr = PushElement(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->paramType = ParamType::Int32;
  instr->param.intNum = intNum;
  ListAppend(arena, instrList, instr);
}

void EmitInstr(MemoryArena* arena, List* instrList, Opcode opcode)
{
  Instruction* instr = PushElement(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->paramType = ParamType::_Null;
  ListAppend(arena, instrList, instr);
}

void EmitInstrStr(MemoryArena* arena, List* instrList, Opcode opcode, char* str)
{
  Instruction* instr = PushElement(arena, Instruction, 1);
  instr->opcode = opcode;
  instr->paramType = ParamType::String;
  instr->param.str = str;
  ListAppend(arena, instrList, instr);
}

void GenLoadRValue(MemoryArena*, List*, IrValue*);

void GenCall(MemoryArena* arena, List* code, IrCall* call)
{
  IrProc* proc = call->proc;
  IrActivationRecord* actvRecord = proc->actvRecord;
  EmitInstrInt(arena, code, Opcode::ALLOC, actvRecord->ret.size);

  ListItem* argItem = ListFirstItem(&call->actualArgs);
  while(argItem)
  {
    IrValue* argValue = (IrValue*)argItem->elem;
    GenLoadRValue(arena, code, argValue);
    argItem = argItem->next;
  }
}

void GenLoadLValue(MemoryArena* arena, List* code, IrValue* value)
{
  assert(value->kind == IrValueKind::Var);
  if(value->var.isNonLocal)
  {
    EmitInstrReg(arena, code, Opcode::PUSH, RegName::FP);
    EmitInstrInt(arena, code, Opcode::PUSH, value->var.accessLinkLocation);
    EmitInstr(arena, code, Opcode::ADD);
    EmitInstr(arena, code, Opcode::LOAD);
  } else
  {
    EmitInstrReg(arena, code, Opcode::PUSH, RegName::FP);
  }
  EmitInstrInt(arena, code, Opcode::PUSH, value->var.dataLocation);
  EmitInstr(arena, code, Opcode::ADD);
}

void GenLoadRValue(MemoryArena* arena, List* code, IrValue* value)
{
  if(value->kind == IrValueKind::Var)
  {
    GenLoadLValue(arena, code, value);
    EmitInstr(arena, code, Opcode::LOAD);
  }
  else if(value->kind == IrValueKind::Call)
  {
    GenCall(arena, code, value->call);
  }
  else if(value->kind == IrValueKind::IntNum)
  {
    EmitInstrInt(arena, code, Opcode::PUSH, value->intNum);
  }
}

void GenCode(MemoryArena* arena, List* code, IrNode* irNode)
{
  switch(irNode->kind)
  {
    case(IrNodeKind::Module):
    {
      IrModule* module = &irNode->module;
      ListItem* item = ListFirstItem(&module->procList);
      while(item)
      {
        IrNode* irNode = (IrNode*)item->elem;
        GenCode(arena, code, irNode);
        item = item->next;
      }
    } break;

    case(IrNodeKind::Proc):
    {
      IrProc* proc = &irNode->proc;
      EmitInstrStr(arena, code, Opcode::LABEL, proc->label);
      IrStackArea* localsArea = &proc->actvRecord->locals;
      EmitInstrInt(arena, code, Opcode::ALLOC, localsArea->size);

      ListItem* item = ListFirstItem(&proc->instrList);
      while(item)
      {
        IrNode* irNode = (IrNode*)item->elem;
        GenCode(arena, code, irNode);
        item = item->next;
      }

      EmitInstrStr(arena, code, Opcode::LABEL, proc->endLabel);
      EmitInstr(arena, code, Opcode::RETURN);
    } break;

    case(IrNodeKind::LoadLValue):
    {
      IrValue* value = &irNode->value;
      GenLoadLValue(arena, code, value);
    } break;

    case(IrNodeKind::LoadRValue):
    {
      IrValue* value = &irNode->value;
      GenLoadRValue(arena, code, value);
    } break;

    case(IrNodeKind::BinOp):
    {
      IrBinOp* op = &irNode->op;
      GenCode(arena, code, op->leftOperand);
      GenCode(arena, code, op->rightOperand);
      if(op->op == IrOpKind::Store)
        EmitInstr(arena, code, Opcode::STORE);
      else
        assert(false);
    } break;

    case(IrNodeKind::Call):
    {
      IrCall* call = &irNode->call;
      GenCall(arena, code, call);
      //Discard ret val
    } break;
  }
}

char* GetRegNameStr(RegName reg)
{
  static char* regFP = "fp";
  static char* regSP = "sp";
  static char* regIP = "ip";
  char* regStr = 0;

  if(reg == RegName::FP)
    regStr = regFP;
  else if(reg == RegName::SP)
    regStr = regSP;
  else if(reg == RegName::IP)
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
    Instruction* instr = (Instruction*)item->elem;
    switch(instr->opcode)
    {
      case(Opcode::PUSH):
      {
        if(instr->paramType == ParamType::Reg)
          PrintInstruction(vmProgram, "push %s", GetRegNameStr(instr->param.reg));
        else if(instr->paramType == ParamType::Int32)
          PrintInstruction(vmProgram, "push %d", instr->param.intNum);
        else
          assert(false);
      } break;

      case(Opcode::POP):
      {
        if(instr->paramType == ParamType::Reg)
          PrintInstruction(vmProgram, "pop %s", GetRegNameStr(instr->param.reg));
        else if(instr->paramType == ParamType::_Null)
          PrintInstruction(vmProgram, "pop");
        else
          assert(false);
      } break;

      case(Opcode::ADD):
      {
        if(instr->paramType == ParamType::_Null)
          PrintInstruction(vmProgram, "add");
        else
          assert(false);
      } break;

      case(Opcode::LOAD):
      {
        if(instr->paramType == ParamType::_Null)
          PrintInstruction(vmProgram, "load");
        else
          assert(false);
      } break;

      case(Opcode::STORE):
      {
        if(instr->paramType == ParamType::_Null)
          PrintInstruction(vmProgram, "store");
        else
          assert(false);
      } break;

      case(Opcode::LABEL):
      {
        if(instr->paramType == ParamType::String)
          PrintInstruction(vmProgram, "label %s", instr->param.str);
        else
          assert(false);
      } break;

      case(Opcode::RETURN):
      {
        if(instr->paramType == ParamType::_Null)
          PrintInstruction(vmProgram, "return");
        else
          assert(false);
      } break;

      case(Opcode::ALLOC):
      {
        if(instr->paramType == ParamType::Int32)
          PrintInstruction(vmProgram, "alloc %d", instr->param.intNum);
        else
          assert(false);
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
//            Ast::ListItem* procList = block->module.procList;
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
//            Ast::ListItem* argList = block->proc.argList;
//            int argsAreaSize = 0;
//            int argsCount = 0;
//            DataObjList* irArgs = 0;
//            while(argList)
//            {
//              AstNode* argAst = argList->ast;
//              Symbol* argSymbol = argAst->id.symbol;
//
//              assert(argAst->kind == Ast_Var);
//              assert(argSymbol->kind == SymbolKind::Var);
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
//      Ast::ListItem* declList = block->declVars;
//      DataObjList* irLocalsList = 0;
//      int localAreaSize = 0;
//      while(declList)
//      {
//        AstNode* varAst = declList->ast;
//        Symbol* varSymbol = varAst->var.symbol;
//
//        assert(varAst->kind == Ast_Var);
//        assert(varSymbol->kind == SymbolKind::Var);
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
//      Ast::ListItem* nonLocalsList = block->nonLocalVars;
//      int accessLinkCount = 0;
//      AccessLink* accessLinks = 0;
//      while(nonLocalsList)
//      {
//        AstNode* idAst = nonLocalsList->ast;
//        Symbol* idSymbol = idAst->id.symbol;
//
//        assert(idAst->kind == Ast_Id);
//        assert(idAst->id.declBlockOffset > 0);
//        assert(idSymbol->kind == SymbolKind::Var);
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
//        Ast::ListItem* stmtList = block->stmtList;
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
//              assert(procSymbol->kind == SymbolKind::Proc);
//
//              Emit(vmProgram, "push 0 ;retval of %s", callAst->call.name);
//
//              Emit(vmProgram, ";begin arg-eval");
//              Ast::ListItem* argList = callAst->call.argList;
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
//              Ast::ListItem* procList = block->module.procList;
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
//              Ast::ListItem* stmtList = block->stmtList;
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
//              Ast::ListItem* stmtList = block->stmtList;
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

  SymbolTable symbolTable = {};
  symbolTable.arena = arena;

  TokenStream tokenStream = {};
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
    assert(astNode->kind == AstNodeKind::Module);
    assert(symbolTable.scopeId == 0);
    assert(symbolTable.nestingDepth == 0);

    IrNode* irModuleNode = IrBuildModule(arena, &astNode->module);

    ListInit(&vmProgram->instrList);
    GenCode(arena, &vmProgram->instrList, irModuleNode);

    StringInit(&vmProgram->text, arena);
    PrintCode(vmProgram);
  }

  return success;
}/*<<<*/

