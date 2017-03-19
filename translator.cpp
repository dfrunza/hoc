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

enum struct SymbolKind
{
  _Null,
  Keyword,
  Proc,
  Var,
};

//enum BlockKind
//{
//  Block__Null,
//  Block_Module,
//  Block_Proc,
//  Block_Stmt,
//  Block_Anonymous,
//};

//struct ActivationRecord;
struct Symbol;
struct TokenStream;

struct SymbolTable
{
  Symbol* currSymbol;
  int     currScopeId;
  int     lastScopeId;
  int     nestingDepth;
  int     activeScopes[32];
  char    label[64];
  int     lastLabelId;
  MemoryArena* arena;
};

//namespace AccessLink
//{/*>>>*/
//  struct Link
//  {
//    int actvRecordOffset;
//    int index;
//    Link* nextLink;
//    Link* prevLink;
//  };
//
//  struct LinkList
//  {
//    Link  sentinel;
//    Link* lastLink;
//    int count;
//
//    static void Add(LinkList* list, Link* link)
//    {/*>>>*/
//      list->lastLink->nextLink = link;
//      link->prevLink = list->lastLink;
//      list->lastLink = link;
//      list->count++;
//    }/*<<<*/
//
//    static void Init(LinkList* list)
//    {/*>>>*/
//      list->lastLink = &list->sentinel;
//    }/*<<<*/
//  };
//}/*<<<*/

template<typename T>
struct TListItem
{
  T* elem;
  TListItem* nextItem;
  TListItem* prevItem;
};

template<typename T>
struct TList
{
  TListItem<T>  sentinel;
  TListItem<T>* lastItem;
  int           count;

  template<typename T>
  static void Add(TList<T>* list, TListItem<T>* item)
  {/*>>>*/
    list->lastItem->nextItem = item;
    item->prevItem = list->lastItem;
    list->lastItem = item;
    list->count++;
  }/*<<<*/

  template<typename T>
  static void Init(TList<T>* list)
  {/*>>>*/
    list->lastItem = &list->sentinel;
  }/*<<<*/
};

namespace Ast
{/*>>>*/
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

  enum struct NodeKind
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

  struct Node;
  using List = TList<Node>;
  using ListItem = TListItem<Node>;
  
  struct AccessLink
  {
    int actvRecordOffset;
    int index;
  };

  struct Module
  {
    List  procList;
    Node* body;
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
    Symbol* symbol;
    char*   name;
    List    argList;
    Node*   proc;
  };

  struct BinExpr
  {
    OperatorKind op;
    Node*  leftOperand;
    Node*  rightOperand;
    bool32 isStatement;
  };

  struct UnrExpr
  {
    OperatorKind op;
    Node* operand;
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
    Node* expr;
    int   interveningBlockCount;
    Node* proc;
    //ActivationRecord* actvRecord; // <-- can this be taken from the block?
  };

  struct Proc
  {
    Symbol* symbol;
    char*   name;
    List    argList;
    Node*   body;
    int     argsDataSize;
  };

  struct IfStmt
  {
    Node* expr;
    Node* body;
    Node* elseNode;
  };

  struct WhileStmt
  {
    Node* expr;
    Node* body;
  };

  struct Block
  {
    using LinkList = TList<AccessLink>;
    using List = TList<Node>;

    Node* owner;
    int   blockId;
    int   nestingDepth;

    TList<Node> declVars;
    TList<Node> localOccurs;
    TList<Node> nonLocalOccurs;
    TList<Node> stmtList;
    Block*      enclosingBlock;

    int      localsDataSize;
    LinkList accessLinks;

    static void Init(SymbolTable* symbolTable, Block* block)
    {/*>>>*/
      block->blockId      = symbolTable->currScopeId;
      block->nestingDepth = symbolTable->nestingDepth;

      List::Init(&block->localOccurs);
      List::Init(&block->nonLocalOccurs);
      List::Init(&block->stmtList);
      List::Init(&block->declVars);
      LinkList::Init(&block->accessLinks);
    }/*<<<*/
  };

  struct Node
  {
    NodeKind  kind;

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
}/*<<<*/

struct Symbol
{
  SymbolKind kind;
  char*      name;
  int        blockId;
  int        nestingDepth;
  Ast::Node* astNode;
  Token      kwToken;

  Symbol*    nextSymbol;

  static Symbol* Lookup(SymbolTable*, char*);
  static Symbol* Add(SymbolTable*, char*, SymbolKind);
  static Symbol* RegisterNew(SymbolTable*, TokenStream*, SymbolKind);
  static Symbol* AddKeyword(SymbolTable*, char*, Token);
};

/* old_ip + old_sp + old_fp */
#define MACHINE_STATUS_DATA_SIZE 3

//enum ActivationRecordKind
//{
//  ActvRecord__Null,
//  ActvRecord_Proc,
//  ActvRecord_Block,
//};

//struct ActivationRecord
//{
//  ActivationRecordKind kind;
//
////  DataObjList* localObjects;
//  int          localAreaSize;
//
//  AccessLink* accessLinks;
//  int         accessLinkCount;
//
//  struct {
////    DataObjList* args;
//    int argsCount;
//    int argsAreaSize;
//
////    DataObj* ret;
//    int      retAreaSize;
//  } proc;
//};

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

Symbol* Symbol::Lookup(SymbolTable* symbolTable, char* name)
{/*>>>*/
   Symbol* result = 0;
   Symbol *symbol = symbolTable->currSymbol;

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

Symbol* Symbol::Add(SymbolTable* symbolTable, char* name, SymbolKind kind)
{/*>>>*/
  Symbol* symbol = PushElement(symbolTable->arena, Symbol, 1);
  symbol->name = name;
  symbol->kind = kind;
  symbol->blockId = symbolTable->currScopeId;
  symbol->nestingDepth = symbolTable->nestingDepth;
  symbol->nextSymbol = symbolTable->currSymbol;
  symbolTable->currSymbol = symbol;
  return symbol;
}/*<<<*/

Symbol* Symbol::RegisterNew(SymbolTable* symbolTable, TokenStream* input, SymbolKind kind)
{/*>>>*/
  assert(input->token == Token::Id);

  Symbol* result = 0;
  Symbol* symbol = 0;

  symbol = Lookup(symbolTable, input->lexval.id);
  if(!symbol)
  {
    result = Add(symbolTable, input->lexval.id, kind);
  } else {
    if(symbol->kind != SymbolKind::Keyword)
    {
      if(symbol->blockId != symbolTable->currScopeId ||
         symbol->kind != kind)
      {
        assert(symbol->nestingDepth <= symbolTable->nestingDepth);

        result = Add(symbolTable, input->lexval.id, kind);
      } else
        SyntaxError(input, "Redeclaration of identifier: %s", symbol->name);
    } else
      SyntaxError(input, "Keyword used as identifier: %s", symbol->name);
  }
  return result;
}/*<<<*/

Symbol* Symbol::AddKeyword(SymbolTable* symbolTable, char* name, Token token)
{/*>>>*/
  Symbol* symbol = Add(symbolTable, name, SymbolKind::Keyword);
  symbol->kwToken = token;
  return symbol;
}/*<<<*/

bool32 BeginScope(SymbolTable* symbolTable)
{/*>>>*/
  int nestingDepth = 0;
  int currScopeId = 0;
  
  currScopeId = ++symbolTable->lastScopeId;
  symbolTable->currScopeId = currScopeId;

  nestingDepth = ++symbolTable->nestingDepth;
  if(nestingDepth < SizeofArray(symbolTable->activeScopes))
  {
    symbolTable->activeScopes[nestingDepth] = currScopeId;
  } else {
    Error("Reached the maximum scope nesting depth");
    return false;
  }

  return true;
}/*<<<*/

void EndScope(SymbolTable* symbolTable)
{/*>>>*/
  int nestingDepth = 0;
  int currScopeId = 0;
  Symbol* symbol = 0;
  
  nestingDepth = --symbolTable->nestingDepth;
  currScopeId = symbolTable->activeScopes[nestingDepth];
  assert(currScopeId >= 0);
  symbolTable->currScopeId = currScopeId;

  symbol = symbolTable->currSymbol;
  while(symbol && symbol->blockId > symbolTable->currScopeId)
    symbol = symbol->nextSymbol;
  symbolTable->currSymbol = symbol;
}/*<<<*/

void MakeUniqueLabel(SymbolTable* symbolTable, char* label)
{
  sprintf(label, "L%d", symbolTable->lastLabelId++);
}

bool32 IsKeyword(Token token)
{
  return token > Token::_KeywordBegin && token < Token::_KeywordEnd;
}

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

    symbol = Symbol::Lookup(symbolTable, lexeme);
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

namespace Parse
{
  using namespace Ast;

  bool32 Expression(MemoryArena*, TokenStream*, SymbolTable*, Block*, Node**);
  bool32 StatementList(MemoryArena*, TokenStream*, SymbolTable*, Block*);
  bool32 ActualArgumentList(MemoryArena*, TokenStream*, SymbolTable*, Block*, Call*);
  bool32 Term(MemoryArena*, TokenStream*, SymbolTable*, Block*, Node**);

  bool32 Factor(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                Block* enclosingBlock, Node** node)
  {/*>>>*/
    bool32 success = true;

    if(input->token == Token::OpenParens)
    {
      ConsumeToken(input, symbolTable);
      success = Expression(arena, input, symbolTable,
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
      Node* operand = 0;

      ConsumeToken(input, symbolTable);
      success = Term(arena, input, symbolTable, enclosingBlock, &operand);
      if(success)
      {
        if(operand)
        {
          Node* negNode = 0;
          UnrExpr* expr = 0;

          negNode = PushElement(arena, Node, 1);
          negNode->kind = NodeKind::UnrExpr;
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
      Node* numNode = 0;

      numNode = PushElement(arena, Node, 1);
      numNode->kind = NodeKind::IntNum;
      numNode->literal.intNum = *(int32*)input->lexval.intNum;
      *node = numNode;

      ConsumeToken(input, symbolTable);
    }
    else if(input->token == Token::Id)
    {
      Symbol* symbol = 0;

      symbol = Symbol::Lookup(symbolTable, input->lexval.id);
      if(symbol)
      {
        Node* idNode = 0;

        idNode = PushElement(arena, Node, 1);
        *node  = idNode;

        ConsumeToken(input, symbolTable);

        if(symbol->kind == SymbolKind::Var)
        {
          using ListItem = TListItem<Node>;

          VarOccur* varOccur = 0;
          ListItem* idItem = 0;

          idNode->kind = NodeKind::VarOccur;
          varOccur = &idNode->varOccur;
          varOccur->symbol          = symbol;
          varOccur->name            = symbol->name;
          varOccur->declBlockOffset = (symbolTable->nestingDepth - symbol->nestingDepth);
          varOccur->isNonLocal      = (varOccur->declBlockOffset > 0);

          idItem = PushElement(arena, ListItem, 1);
          idItem->elem = idNode;
          if(varOccur->isNonLocal)
          {
            List* nonLocalOccurs = 0;

            nonLocalOccurs = &enclosingBlock->nonLocalOccurs;
            List::Add(nonLocalOccurs, idItem);
          }
          else
          {
            List* localOccurs = 0;

            assert(varOccur->declBlockOffset == 0);
            localOccurs = &enclosingBlock->localOccurs;
            List::Add(localOccurs, idItem);
          }
        }
        else if(symbol->kind == SymbolKind::Proc)
        {
          Call*    call = 0;
          List* argList = 0;

          idNode->kind = NodeKind::Call;
          call         = &idNode->call;
          call->symbol = symbol;
          call->name   = symbol->name;
          argList = &call->argList;
          argList->lastItem = &argList->sentinel;

          if(input->token == Token::OpenParens)
          {
            ConsumeToken(input, symbolTable);
            success = ActualArgumentList(arena, input, symbolTable, enclosingBlock, call);
            if(success)
            {
              if(input->token == Token::CloseParens)
              {
                Node* procNode = 0;
                List* procArgList = 0, *callArgList = 0;

                ConsumeToken(input, symbolTable);
                procNode = symbol->astNode;
                call->proc = symbol->astNode;

                procArgList = &procNode->proc.argList;
                callArgList = &call->argList;
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
      Node* numNode = 0;

      numNode = PushElement(arena, Node, 1);
      numNode->kind = NodeKind::IntNum;
      numNode->literal.intNum = (input->token == Token::True ? 1 : 0);
      *node = numNode;

      ConsumeToken(input, symbolTable);
    }

    return success;
  }/*<<<*/

  bool32 RestOfFactors(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                       Block* enclosingBlock, Node* leftNode, Node** node)
  {/*>>>*/
    bool32 success = true;

    if(input->token == Token::Star ||
       input->token == Token::FwdSlash)
    {
      Node* factorNode = 0;
      Node* exprNode = 0;
      BinExpr* expr = 0;

      exprNode = PushElement(arena, Node, 1);
      exprNode->kind = NodeKind::BinExpr;
      expr = &exprNode->binExpr;
      if(input->token == Token::Star)
        expr->op = OperatorKind::Mul;
      else if(input->token == Token::FwdSlash)
        expr->op = OperatorKind::Div;
      else
        assert(false);

      ConsumeToken(input, symbolTable);
      success = Factor(arena, input, symbolTable, enclosingBlock, &factorNode);

      if(success && factorNode)
      {
        expr->rightOperand = factorNode;
        expr->leftOperand = leftNode;
        success = RestOfFactors(arena, input, symbolTable,
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

  bool32 Term(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
              Block* enclosingBlock, Node** node)
  {/*>>>*/
    Node* factorNode = 0;
    Node* exprNode = 0;

    bool32 success = Factor(arena, input, symbolTable, enclosingBlock, &factorNode);
    if(success && factorNode)
      success = RestOfFactors(arena, input, symbolTable,
                              enclosingBlock, factorNode, &exprNode);

    *node = exprNode;
    return success;
  }/*<<<*/

  bool32 RestOfTerms(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                     Block* enclosingBlock, Node* leftNode, Node** node)
  {/*>>>*/
    bool32 success = true;

    if(input->token == Token::Plus ||
       input->token == Token::Minus)
    {
      Node* termNode = 0;
      Node* exprNode = 0;
      BinExpr* expr = 0;

      exprNode = PushElement(arena, Node, 1);
      exprNode->kind = NodeKind::BinExpr;
      expr = &exprNode->binExpr;
      if(input->token == Token::Plus)
        expr->op = OperatorKind::Add;
      else if(input->token == Token::Minus)
        expr->op = OperatorKind::Sub;
      else
        assert(false);

      ConsumeToken(input, symbolTable);
      success = Term(arena, input, symbolTable, enclosingBlock, &termNode);

      if(success && termNode)
      {
        expr->rightOperand = termNode;
        expr->leftOperand = leftNode;
        success = RestOfTerms(arena, input, symbolTable, enclosingBlock, exprNode, node);
      } else {
        SyntaxError(input, "Expression term expected");
        success = false;
      }
    }
    else
      *node = leftNode;

    return success;
  }/*<<<*/

  bool32 AssignmentTerm(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                        Block* enclosingBlock, Node** node)
  {/*>>>*/
    Node* termNode = 0;
    Node* exprNode = 0;

    bool32 success = Term(arena, input, symbolTable, enclosingBlock, &termNode);
    if(success && termNode)
      success = RestOfTerms(arena, input, symbolTable, enclosingBlock, termNode, &exprNode);

    *node = exprNode;
    return success;
  }/*<<<*/

  bool32 RestOfAssignmentTerms(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                               Block* enclosingBlock, Node* leftNode, Node** node)
  {/*>>>*/
    bool32 success = true;

    if(input->token == Token::Equals)
    {
      Node* rightSide = 0;

      ConsumeToken(input, symbolTable);
      success = Expression(arena, input, symbolTable, enclosingBlock, &rightSide);
      if(success)
      {
        if(rightSide)
        {
          if(leftNode->kind == NodeKind::VarOccur)
          {
            Node* exprNode = 0;
            BinExpr* expr = 0;

            exprNode       = PushElement(arena, Node, 1);
            exprNode->kind = NodeKind::BinExpr;
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

  bool32 Expression(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                    Block* enclosingBlock, Node** node)
  {/*>>>*/
    Node* assgnNode = 0;
    Node* exprNode = 0;

    bool32 success = AssignmentTerm(arena, input, symbolTable, enclosingBlock, &assgnNode);
    if(success && assgnNode)
      success = RestOfAssignmentTerms(arena, input, symbolTable,
                                      enclosingBlock, assgnNode, &exprNode);

    *node = exprNode;
    return success;
  }/*<<<*/

  bool32 VarStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                      Block* enclosingBlock, Node** node)
  {/*>>>*/
    bool32 success = true;

    if(input->token == Token::Var)
    {
      ConsumeToken(input, symbolTable);
      if(input->token == Token::Id)
      {
        Node* varNode = 0;
        Symbol* symbol = 0;

        varNode = PushElement(arena, Node, 1);
        varNode->kind = NodeKind::VarDecl;
        *node = varNode;

        symbol = Symbol::RegisterNew(symbolTable, input, SymbolKind::Var);
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

  bool32 FormalArgument(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                        Block* enclosingBlock, Node** node)
  {/*>>>*/
    bool32 success = true;

    if(input->token == Token::Var)
    {
      ConsumeToken(input, symbolTable);
      if(input->token == Token::Id)
      {
        Node* varNode = 0;
        Symbol* symbol = 0;

        varNode = PushElement(arena, Node, 1);
        varNode->kind = NodeKind::VarDecl;
        *node = varNode;

        symbol = Symbol::RegisterNew(symbolTable, input, SymbolKind::Var);
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

  bool32 FormalArgumentList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                            Block* enclosingBlock, Proc* proc)
  {/*>>>*/
    bool32 success = true;
    Node* argNode = 0;

    success = FormalArgument(arena, input, symbolTable, enclosingBlock, &argNode);
    if(success && argNode)
    {
      using ListItem = TListItem<Node>;
      using List = TList<Node>;
      ListItem* argItem = 0;
      List* argList = 0;

      argItem = PushElement(arena, ListItem, 1);
      argItem->elem  = argNode;
      argList = &proc->argList;
      List::Add(argList, argItem);

      if(input->token == Token::Comma)
      {
        ConsumeToken(input, symbolTable);
        success = FormalArgumentList(arena, input, symbolTable, enclosingBlock, proc);
      }
    }

    return success;
  }/*<<<*/

  bool32 ActualArgumentList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                            Block* enclosingBlock, Call* call)
  {/*>>>*/
    bool32 success = true;

    Node* argNode = 0;
    success = Expression(arena, input, symbolTable, enclosingBlock, &argNode);
    if(success && argNode)
    {
      using ListItem = TListItem<Node>;
      using List = TList<Node>;
      ListItem* argItem = 0;

      argItem = PushElement(arena, ListItem, 1);
      argItem->elem  = argNode;
      List::Add(&call->argList, argItem);

      if(input->token == Token::Comma)
      {
        ConsumeToken(input, symbolTable);
        success = ActualArgumentList(arena, input, symbolTable, enclosingBlock, call);
      }
    }

    return success;
  }/*<<<*/

  bool32 WhileStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                        Block* enclosingBlock, Node** node)
  {/*>>>*/
    bool32 success = true;

    if(input->token == Token::While)
    {
      Node* exprNode = 0;

      ConsumeToken(input, symbolTable);
      success = Expression(arena, input, symbolTable, enclosingBlock, &exprNode);
      if(success)
      {
        Node* whileNode = 0;
        WhileStmt* whileStmt = 0;

        whileNode       = PushElement(arena, Node, 1);
        whileNode->kind = NodeKind::WhileStmt;
        whileStmt       = &whileNode->whileStmt;
        whileStmt->expr = exprNode;
        *node = whileNode;

        if(input->token == Token::OpenBrace)
        {
          ConsumeToken(input, symbolTable);

          success = BeginScope(symbolTable);
          if(success)
          {
            Node* blockNode = 0;
            Block*   block = 0;

            blockNode       = PushElement(arena, Node, 1);
            blockNode->kind = NodeKind::Block;
            block        = &blockNode->block;
            block->owner = whileNode;
            Block::Init(symbolTable, block);

            success = StatementList(arena, input, symbolTable, block);
            if(success)
            {
              whileStmt->body = blockNode;

              if(input->token == Token::CloseBrace)
              {
                ConsumeToken(input, symbolTable);
                EndScope(symbolTable);
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

  bool32 IfStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                     Block* enclosingBlock, Node** node)
  {/*>>>*/
    bool32 success = true;

    if(input->token == Token::If)
    {
      Node* exprNode = 0;

      ConsumeToken(input, symbolTable);
      success = Expression(arena, input, symbolTable, enclosingBlock, &exprNode);
      if(success)
      {
        Node* ifNode = 0;
        IfStmt*  ifStmt = 0;

        ifNode = PushElement(arena, Node, 1);
        ifNode->kind = NodeKind::IfStmt;
        ifStmt = &ifNode->ifStmt;
        ifStmt->expr = exprNode;
        *node = ifNode;

        if(input->token == Token::OpenBrace)
        {
          ConsumeToken(input, symbolTable);

          success = BeginScope(symbolTable);
          if(success)
          {
            Node* blockNode = 0;
            Block*   block = 0;

            blockNode = PushElement(arena, Node, 1);
            blockNode->kind = NodeKind::Block;
            block = &blockNode->block;
            block->owner = ifNode;
            Block::Init(symbolTable, block);

            success = StatementList(arena, input, symbolTable, block);
            if(success)
            {
              ifStmt->body = blockNode;

              if(input->token == Token::CloseBrace)
              {
                ConsumeToken(input, symbolTable);
                EndScope(symbolTable);

                if(input->token == Token::Else)
                {
                  Node* elseNode = 0;

                  ConsumeToken(input, symbolTable);
                  success = IfStatement(arena, input, symbolTable, enclosingBlock, &elseNode);
                  if(success)
                  {
                    if(elseNode)
                      ifStmt->elseNode = elseNode;
                    else if(input->token == Token::OpenBrace)
                    {
                      ConsumeToken(input, symbolTable);

                      success = BeginScope(symbolTable);
                      if(success)
                      {
                        Node* blockNode = 0;
                        Block*   block = 0;

                        blockNode = PushElement(arena, Node, 1);
                        blockNode->kind = NodeKind::Block;
                        block = &blockNode->block;
                        block->owner = ifNode;
                        Block::Init(symbolTable, block);

                        success = StatementList(arena, input, symbolTable, block);
                        if(success)
                        {
                          ifStmt->elseNode = blockNode;
                          if(input->token == Token::CloseBrace)
                          {
                            ConsumeToken(input, symbolTable);
                            EndScope(symbolTable);
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

  bool32 Procedure(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                   Block* enclosingBlock, Node** node)
  {/*>>>*/
    bool32 success = true;

    if(input->token == Token::Proc)
    {
      Node* procNode = 0;

      procNode = PushElement(arena, Node, 1);
      procNode->kind = NodeKind::Proc;
      *node = procNode;

      ConsumeToken(input, symbolTable);
      if(input->token == Token::Id)
      {
        Symbol* symbol = 0;

        symbol = Symbol::RegisterNew(symbolTable, input, SymbolKind::Proc);
        if(symbol)
        {
          Proc* proc = 0;
          List* argList = 0;

          symbol->astNode = procNode;
          proc = &procNode->proc;
          proc->symbol = symbol;
          proc->name   = symbol->name;
          argList = &proc->argList;
          argList->lastItem = &argList->sentinel;

          ConsumeToken(input, symbolTable);
          if(input->token == Token::OpenParens)
          {
            ConsumeToken(input, symbolTable);

            // arguments
            success = BeginScope(symbolTable);
            if(success)
            {
              Node* blockNode = 0;
              Block*   block = 0;

              blockNode       = PushElement(arena, Node, 1);
              blockNode->kind = NodeKind::Block;
              proc->body = blockNode;
              block        = &blockNode->block;
              block->owner = procNode;
              Block::Init(symbolTable, block);

              success = FormalArgumentList(arena, input, symbolTable, block, proc);
              if(success)
              {
                ListItem* argItem = 0;

                argItem = proc->argList.sentinel.nextItem;
                while(argItem)
                {
                  Node*    node = 0;
                  VarDecl* arg = 0;
                  Symbol*  symbol = 0;

                  node = argItem->elem;
                  assert(node->kind == NodeKind::VarDecl);
                  arg = &node->varDecl;
                  symbol = node->varDecl.symbol;

                  arg->location = -(proc->argsDataSize + MACHINE_STATUS_DATA_SIZE + 1);
                  arg->dataSize = 1;
                  proc->argsDataSize += arg->dataSize;

                  argItem = argItem->nextItem;
                }

                if(input->token == Token::CloseParens)
                {
                  ConsumeToken(input, symbolTable);

                  if(input->token == Token::OpenBrace)
                  {
                    // body
                    ConsumeToken(input, symbolTable);

                    success = StatementList(arena, input, symbolTable, block);
                    if(success)
                    {
                      /*>>> Process the local vars */
                      ListItem* listItem = 0;

                      listItem = block->declVars.sentinel.nextItem;
                      while(listItem)
                      {
                        Node* node = 0;
                        VarDecl* varDecl = 0;
                        Symbol*  symbol = 0;

                        node = listItem->elem;
                        varDecl = &node->varDecl;
                        symbol = node->varDecl.symbol;

                        varDecl->location = block->localsDataSize;
                        block->localsDataSize += varDecl->dataSize;

                        listItem = listItem->nextItem;
                      }/*<<<*/

                      listItem = block->nonLocalOccurs.sentinel.nextItem;
                      while(listItem)
                      {
                        using LinkList = TList<AccessLink>;
                        using LinkItem = TListItem<AccessLink>;

                        Node*       node = 0;
                        VarOccur*   varOccur = 0;
                        Symbol*     symbol = 0;
                        AccessLink* link = 0;
                        LinkItem*   linkItem = 0;
                        LinkList*   linksList = 0;

                        node      = listItem->elem;
                        varOccur  = &node->varOccur;
                        symbol    = node->varOccur.symbol;
                        linksList = &block->accessLinks;

                        linkItem = block->accessLinks.sentinel.nextItem;
                        while(linkItem)
                        {
                          link = linkItem->elem;
                          if(link->actvRecordOffset == varOccur->declBlockOffset)
                            break;
                          linkItem = linkItem->nextItem;
                        }
                        if(!link)
                        {
                          link = PushElement(arena, AccessLink, 1);
                          link->actvRecordOffset = varOccur->declBlockOffset;
                          link->index = linksList->count;
                          linkItem = PushElement(arena, LinkItem, 1);
                          linkItem->elem = link;
                          LinkList::Add(linksList, linkItem);
                        }
                      }

                      if(input->token == Token::CloseBrace)
                      {
                        ConsumeToken(input, symbolTable);
                        EndScope(symbolTable); // body
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

  bool32 ProcedureList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                       Block* enclosingBlock, Ast::Module* module)
  {/*>>>*/
    bool32 success = true;
    Node* procNode = 0;

    success = Procedure(arena, input, symbolTable, enclosingBlock, &procNode);
    if(success && procNode)
    {
      ListItem* procItem = 0;

      procItem = PushElement(arena, ListItem, 1);
      procItem->elem = procNode;
      List::Add(&module->procList, procItem);
//      procList->lastItem->nextItem = procItem;
//      procList->lastItem = procItem;
//      procList->count++;

      success = ProcedureList(arena, input, symbolTable, enclosingBlock, module);
    }

    return success;
  }/*<<<*/

  bool32 ReturnStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                         Block* enclosingBlock, Node** node)
  {/*>>>*/
    bool32 success = true;

    if(input->token == Token::Return)
    {
      Node* exprNode = 0;

      ConsumeToken(input, symbolTable);
      success = Expression(arena, input, symbolTable, enclosingBlock, &exprNode);
      if(success)
      {
        if(exprNode)
        {
          Node* retNode = 0;
          ReturnStmt* retStmt = 0; 
          Block* block = 0;

          retNode       = PushElement(arena, Node, 1);
          retNode->kind = NodeKind::ReturnStmt;
          retStmt       = &retNode->retStmt;
          retStmt->expr = exprNode;
          *node = retNode;

          int depth = 0;
          block = enclosingBlock;
          while(block)
          {
            Node* owner = block->owner;
            if(owner->kind == NodeKind::Proc)
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

  bool32 Statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                   Block* enclosingBlock, Node** node)
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
    Node* stmtNode = 0;

    while(alt) {
      switch(alt) {
        case Alt_Expr:
          {
            success = Expression(arena, input, symbolTable, enclosingBlock, &stmtNode);
            if(success)
            {
              if(stmtNode)
              {
                alt = Alt__Null;
                if(input->token == Token::Semicolon)
                {
                  BinExpr* expr = 0;

                  ConsumeToken(input, symbolTable);
                  expr = &stmtNode->binExpr;
                  if(expr->op == OperatorKind::Assign)
                    expr->isStatement = true;
                  else {
                    SyntaxError(input, "Assignment expression required");
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
            success = IfStatement(arena, input, symbolTable, enclosingBlock, &stmtNode);
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
            success = WhileStatement(arena, input, symbolTable, enclosingBlock, &stmtNode);
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
            success = ReturnStatement(arena, input, symbolTable, enclosingBlock, &stmtNode);
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
            success = VarStatement(arena, input, symbolTable, enclosingBlock, &stmtNode);
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

  bool32 StatementList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                       Block* block)
  {/*>>>*/
    bool32 success = true;
    Node* stmtNode = 0;

    success = Statement(arena, input, symbolTable, block, &stmtNode);
    if(success && stmtNode)
    {
      ListItem* stmtItem = 0;

      stmtItem = PushElement(arena, ListItem, 1);
      stmtItem->elem = stmtNode;

      while(input->token == Token::Semicolon)
        ConsumeToken(input, symbolTable);

      if(stmtNode->kind == NodeKind::VarDecl)
        List::Add(&block->declVars, stmtItem);
      else
        List::Add(&block->stmtList, stmtItem);

      success = StatementList(arena, input, symbolTable, block); //FIXME: Is this tail-recursion - can it be optimized?
    }
    return success;
  }/*<<<*/

  bool32 Module(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                Node** node)
  {/*>>>*/
    bool32 success = true;

    success = BeginScope(symbolTable);
    if(success)
    {
      Node*        moduleNode = 0;
      Node*        blockNode = 0;
      Block*       block = 0;
      Ast::Module* module = 0;
      List*        procList = 0;

      moduleNode = PushElement(arena, Node, 1);
      moduleNode->kind = NodeKind::Module;

      blockNode = PushElement(arena, Node, 1);
      blockNode->kind = NodeKind::Block;
      block        = &blockNode->block;
      block->owner = moduleNode;
      Block::Init(symbolTable, block);

      module = &moduleNode->module;
      module->body = blockNode;
      procList = &module->procList;
      procList->lastItem = &procList->sentinel;

      success = ProcedureList(arena, input, symbolTable, block, module);
      if(success)
      {
        EndScope(symbolTable);

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
}

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

#if 0
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

bool32 TranslateHocToIr(MemoryArena* arena, char* filePath, char* hocProgram, IrProgram* irProgram)
{/*>>>*/
  SymbolTable symbolTable = {};
  TokenStream tokenStream = {};
  Ast::Node* module = 0;
  bool32 success = false;

  symbolTable.arena = arena;

  tokenStream.arena = arena;
  tokenStream.text = hocProgram;
  tokenStream.cursor = tokenStream.text;
  tokenStream.lineNr = 1;
  //TODO: Compute the absolute path to the file, so that Vim could properly
  // jump from the QuickFix window to the error line in the file.
  tokenStream.filePath = filePath;

  Symbol::AddKeyword(&symbolTable, "int", Token::If);
  Symbol::AddKeyword(&symbolTable, "float", Token::Float);
  Symbol::AddKeyword(&symbolTable, "void", Token::Void);
  Symbol::AddKeyword(&symbolTable, "char", Token::Char);
  Symbol::AddKeyword(&symbolTable, "var", Token::Var);
  Symbol::AddKeyword(&symbolTable, "proc", Token::Proc);
  Symbol::AddKeyword(&symbolTable, "type", Token::Type);
  Symbol::AddKeyword(&symbolTable, "struct", Token::Type);
  Symbol::AddKeyword(&symbolTable, "array", Token::Array);
  Symbol::AddKeyword(&symbolTable, "of", Token::Of);
  Symbol::AddKeyword(&symbolTable, "if", Token::If);
  Symbol::AddKeyword(&symbolTable, "else", Token::Else);
  Symbol::AddKeyword(&symbolTable, "while", Token::While);
  Symbol::AddKeyword(&symbolTable, "return", Token::Return);
  Symbol::AddKeyword(&symbolTable, "true", Token::True);
  Symbol::AddKeyword(&symbolTable, "false", Token::False);

  ConsumeToken(&tokenStream, &symbolTable);

  success = Parse::Module(arena, &tokenStream, &symbolTable, &module);
//  if(success)
//    success = BuildIr(arena, &symbolTable, moduleAst);

  if(success)
  {
    StringInit(&irProgram->text, arena);

    //GenCode(irProgram, &symbolTable, block, moduleAst);

    assert(symbolTable.currScopeId == 0);
    assert(symbolTable.nestingDepth == 0);
  }

  return success;
}/*<<<*/
