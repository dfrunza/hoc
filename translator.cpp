#include "lib.cpp"

enum Token
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
};/*<<<*/

enum OperatorType
{
  Operator__Null,
  Operator_Add,
  Operator_Sub,
  Operator_Assign,
  Operator_Div,
  Operator_Mul,
  Operator_Call,
  Operator_Neg,
};

enum SymbolKind
{
  Symbol__Null,
  Symbol_Keyword,
  Symbol_Proc,
  Symbol_Var,
};

enum AstKind
{
  Ast__Null,
  Ast_BinExpr,
  Ast_UnExpr,
  Ast_IntNum,
  Ast_VarDecl,
  Ast_VarOccur,
  Ast_Call,
  Ast_Block,
  Ast_Proc,
  Ast_While,
  Ast_Return,
  Ast_Module,
};

enum BlockKind
{
  Block__Null,
  Block_Module,
  Block_Proc,
  Block_Stmt,
  Block_Anonymous,
};

struct AstNode;
struct ActivationRecord;
struct Symbol;

struct DataObj
{
  int location; // relative to fp
  int size;
};

struct DataObjList
{
  DataObj*     dataObj;
  DataObjList* nextItem;
};

struct Symbol
{
  SymbolKind kind;
  char*      name;
  int        blockId;
  int        nestingDepth;
  AstNode*   astNode;

  Symbol*    nextSymbol;

  union {
    DataObj dataObj;
    Token   kwToken;
  };
};

struct AstList
{
  AstNode* astNode;
  AstList* nextItem;
};

struct AccessLink
{
  int actvRecordOffset;
  int index;
  AccessLink* nextLink;
};

enum ActivationRecordKind
{
  ActvRecord__Null,
  ActvRecord_Proc,
  ActvRecord_Block,
};

/* old_ip + old_sp + old_fp */
#define MACHINE_STATUS_AREA_SIZE 3

struct ActivationRecord
{
  ActivationRecordKind kind;

  DataObjList* localObjects;
  int          localAreaSize;

  AccessLink* accessLinks;
  int         accessLinkCount;

  struct {
    DataObjList* args;
    int argsCount;
    int argsAreaSize;

    DataObj* ret;
    int      retAreaSize;
  } proc;
};

struct VarDecl
{
  Symbol* symbol;
  char*   name;
};

struct Call
{
  Symbol*  symbol;
  char*    name;
  AstList  argList;
  int      argCount;
  AstNode* proc;
};

struct BinExpr
{
  OperatorType op;
  AstNode* leftOperand;
  AstNode* rightOperand;
  bool32   isStatement;
};

struct UnExpr
{
  OperatorType op;
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
  int      interveningBlockCount;
  AstNode* proc;
  ActivationRecord* actvRecord; // <-- can this be taken from the block?
};

struct Proc
{
  Symbol*  symbol;
  char*    name;
  AstList  argList;
  int      argCount;
  AstNode* body;
};

struct Module
{
  AstList* procList;
  AstNode* body;
} module;

struct WhileStmt
{
  AstNode* expr;
  AstNode* body;
};

struct Block
{
  AstNode*  owner;
  int       blockId;
  int       nestingDepth;
  
  AstList   declVars;
  int       declVarCount;

  AstList*  localVars;
  int       localVarCount;

  AstList*  nonLocalVars;
  int       nonLocalVarCount;

  AstList   stmtList;
  int       stmtCount;

  Block*    enclosingBlock;
  ActivationRecord actvRecord;
};

struct AstNode
{
  AstKind kind;

  union {
    BinExpr    binExpr;
    UnExpr     unExpr;
    VarDecl    varDecl;
    VarOccur   varOccur;
    Literal    literal;
    Call       call;
    Proc       proc;
    Module     module;
    Block      block;
    ReturnStmt retStmt;
    WhileStmt  whileStmt;
  };
};

struct TokenStream
{
  Token        prevToken;
  Token        token;
  char*        text;
  char*        cursor;
  MemoryArena* arena;

  char* filePath;
  int   lineNr;
  char* srcLine;

  union {
    int*  intNum;
    char* id;
  } lexval;
};

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

Symbol* LookupSymbol(SymbolTable* symbolTable, char* name)
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

Symbol* AddSymbol(SymbolTable* symbolTable, char* name, SymbolKind kind)
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

Symbol* RegisterNewSymbol(SymbolTable* symbolTable, TokenStream* input, SymbolKind kind)
{/*>>>*/
  assert(input->token == Token_Id);

  Symbol* result = 0;
  Symbol* symbol = LookupSymbol(symbolTable, input->lexval.id);
  if(!symbol)
  {
    result = AddSymbol(symbolTable, input->lexval.id, kind);
  } else {
    if(symbol->kind != Symbol_Keyword)
    {
      if(symbol->blockId != symbolTable->currScopeId ||
         symbol->kind != kind)
      {
        assert(symbol->nestingDepth <= symbolTable->nestingDepth);

        result = AddSymbol(symbolTable, input->lexval.id, kind);
      } else
        SyntaxError(input, "Redeclaration of identifier: %s", symbol->name);
    } else
      SyntaxError(input, "Keyword used as identifier: %s", symbol->name);
  }
  return result;
}/*<<<*/

Symbol* AddKeyword(SymbolTable* symbolTable, char* name, Token token)
{
  Symbol* symbol = AddSymbol(symbolTable, name, Symbol_Keyword);
  symbol->kwToken = token;
  return symbol;
}

bool32 BeginScope(SymbolTable* symbolTable)
{/*>>>*/
  int currScopeId = ++symbolTable->lastScopeId;
  symbolTable->currScopeId = currScopeId;

  int nestingDepth = ++symbolTable->nestingDepth;
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
  int nestingDepth = --symbolTable->nestingDepth;
  int currScopeId = symbolTable->activeScopes[nestingDepth];
  assert(currScopeId >= 0);
  symbolTable->currScopeId = currScopeId;

  Symbol* symbol = symbolTable->currSymbol;
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
  return token > Token__KeywordBegin && token < Token__KeywordEnd;
}

char* InstallLexeme(TokenStream* input, char* beginChar, char* endChar)
{
  //FIXME: If the lexeme had been previously installed then return it.
  int len = (int)(endChar - beginChar + 1);
  char* lexeme = PushElement(input->arena, char, len + 1);
  CopySubstr(lexeme, beginChar, endChar);
  return lexeme;
}

void ConsumeToken(TokenStream* input, SymbolTable* symbolTable)
{/*>>>*/
  input->prevToken = input->token;
  input->token = Token__Null;
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

    Symbol* symbol = LookupSymbol(symbolTable, lexeme);
    if(symbol && symbol->kind == Symbol_Keyword)
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

bool32 Expression(MemoryArena*, TokenStream*, SymbolTable*, Block*, AstNode**);
bool32 BlockAst(MemoryArena*, TokenStream*, SymbolTable*, Block*, AstList*, AstList*);
bool32 ActualArgumentsList(MemoryArena*, TokenStream*, SymbolTable*, Block*, AstNode*, AstList*);
bool32 Term(MemoryArena*, TokenStream*, SymbolTable*, Block*, AstNode**);

bool32 Factor(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
              Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_OpenParens)
  {
    ConsumeToken(input, symbolTable);
    success = Expression(arena, input, symbolTable,
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
    AstNode* operand = 0;

    ConsumeToken(input, symbolTable);
    success = Term(arena, input, symbolTable,
                   enclosingBlock, &operand);
    if(success)
    {
      if(operand)
      {
        AstNode* neg = 0;
        UnExpr*  expr = 0;

        neg       = PushElement(arena, AstNode, 1);
        neg->kind = Ast_UnExpr;
        expr          = &neg->unExpr;
        expr->op      = Operator_Neg;
        expr->operand = operand;

        *node = neg;
      } else {
        SyntaxError(input, "Expression expected after '-'");
        success = false;
      }
    }
  }
  else if(input->token == Token_IntNum)
  {
    AstNode* num = 0;
    
    num       = PushElement(arena, AstNode, 1);
    num->kind = Ast_IntNum;
    num->literal.intNum = *(int32*)input->lexval.intNum;
    *node = num;

    ConsumeToken(input, symbolTable);
  }
  else if(input->token == Token_Id)
  {
    Symbol* symbol = 0;
    
    symbol = LookupSymbol(symbolTable, input->lexval.id);
    if(symbol)
    {
      AstNode* idNode = 0;
      
      idNode = PushElement(arena, AstNode, 1);
      *node  = idNode;

      ConsumeToken(input, symbolTable);

      if(symbol->kind == Symbol_Var)
      {
        VarOccur* varOccur = 0;
        AstList* idItem = 0;

        idNode->kind = Ast_VarOccur;
        varOccur                  = &idNode->varOccur;
        varOccur->symbol          = symbol;
        varOccur->name            = symbol->name;
        varOccur->declBlockOffset = (symbolTable->nestingDepth - symbol->nestingDepth);
        varOccur->isNonLocal      = (varOccur->declBlockOffset > 0);

        idItem          = PushElement(arena, AstList, 1);
        idItem->astNode = idNode;
        if(varOccur->isNonLocal)
        {
          idItem->nextItem = enclosingBlock->nonLocalVars;
          enclosingBlock->nonLocalVars = idItem;
        }
        else
        {
          assert(varOccur->declBlockOffset == 0);
          idItem->nextItem = enclosingBlock->localVars;
          enclosingBlock->localVars = idItem;
        }
      }
      else if(symbol->kind == Symbol_Proc)
      {
        Call* call = 0;

        idNode->kind = Ast_Call;
        call         = &idNode->call;
        call->symbol = symbol;
        call->name   = symbol->name;

        if(input->token == Token_OpenParens)
        {
          ConsumeToken(input, symbolTable);
          success = ActualArgumentsList(arena, input, symbolTable,
                                        enclosingBlock, idNode, &call->argList);
          if(success)
          {
            if(input->token == Token_CloseParens)
            {
              AstNode* proc = 0;

              ConsumeToken(input, symbolTable);
              proc = symbol->astNode;
              call->proc = symbol->astNode;

              if(proc->proc.argCount != idNode->call.argCount)
              {
                SyntaxError(input, "Incorrect number of arguments in the call '%s(..)'", symbol->name);
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
      else if(symbol->kind == Symbol_Keyword)
      {
        SyntaxError(input, "Keyword used as identifier: '%s'", input->lexval.id);
        success = false;
      }
      else
        assert(false);
    } else {
      SyntaxError(input, "Unknown identifier: '%s'", input->lexval.id);
      success = false;
    }
  }
  else if(input->token == Token_True || input->token == Token_False)
  {
    AstNode* num = 0;
      
    num       = PushElement(arena, AstNode, 1);
    num->kind = Ast_IntNum;
    num->literal.intNum = (input->token == Token_True ? 1 : 0);
    *node = num;

    ConsumeToken(input, symbolTable);
  }

  return success;
}/*<<<*/

bool32 RestOfFactors(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                     Block* enclosingBlock, AstNode* leftNode, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Star ||
     input->token == Token_FwdSlash)
  {
    AstNode* factorNode = 0;
    AstNode* exprNode = 0;
    BinExpr* expr = 0;
    
    exprNode = PushElement(arena, AstNode, 1);
    exprNode->kind = Ast_BinExpr;
    expr = &exprNode->binExpr;
    if(input->token == Token_Star)
      expr->op = Operator_Mul;
    else if(input->token == Token_FwdSlash)
      expr->op = Operator_Div;
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
            Block* enclosingBlock, AstNode** node)
{/*>>>*/
  AstNode* factorNode = 0;
  AstNode* exprNode = 0;

  bool32 success = Factor(arena, input, symbolTable,
                          enclosingBlock, &factorNode);
  if(success && factorNode)
    success = RestOfFactors(arena, input, symbolTable,
                            enclosingBlock, factorNode, &exprNode);

  *node = exprNode;
  return success;
}/*<<<*/

bool32 RestOfTerms(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                   Block* enclosingBlock, AstNode* leftNode, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Plus ||
     input->token == Token_Minus)
  {
    AstNode* termNode = 0;
    AstNode* exprNode = 0;
    BinExpr* expr = 0;
    
    exprNode = PushElement(arena, AstNode, 1);
    exprNode->kind = Ast_BinExpr;
    expr = &exprNode->binExpr;
    if(input->token == Token_Plus)
      expr->op = Operator_Add;
    else if(input->token == Token_Minus)
      expr->op = Operator_Sub;
    else
      assert(false);

    ConsumeToken(input, symbolTable);
    success = Term(arena, input, symbolTable, enclosingBlock, &termNode);

    if(success && termNode)
    {
      expr->rightOperand = termNode;
      expr->leftOperand = leftNode;
      success = RestOfTerms(arena, input, symbolTable,
                            enclosingBlock, exprNode, node);
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
                      Block* enclosingBlock, AstNode** node)
{/*>>>*/
  AstNode* termNode = 0;
  AstNode* exprNode = 0;

  bool32 success = Term(arena, input, symbolTable,
                        enclosingBlock, &termNode);
  if(success && termNode)
    success = RestOfTerms(arena, input, symbolTable,
                          enclosingBlock, termNode, &exprNode);

  *node = exprNode;
  return success;
}/*<<<*/

bool32 RestOfAssignmentTerms(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                             Block* enclosingBlock, AstNode* leftNode, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Equals)
  {
    AstNode* rightSide = 0;

    ConsumeToken(input, symbolTable);
    success = Expression(arena, input, symbolTable,
                         enclosingBlock, &rightSide);
    if(success)
    {
      if(rightSide)
      {
        if(leftNode->kind == Ast_VarOccur)
        {
          AstNode* exprNode = 0;
          BinExpr* expr = 0;
          
          exprNode       = PushElement(arena, AstNode, 1);
          exprNode->kind = Ast_BinExpr;
          expr = &exprNode->binExpr;
          expr->op = Operator_Assign;
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
                  Block* enclosingBlock, AstNode** node)
{/*>>>*/
  AstNode* assgnNode = 0;
  AstNode* exprNode = 0;

  bool32 success = AssignmentTerm(arena, input, symbolTable,
                                  enclosingBlock, &assgnNode);
  if(success && assgnNode)
    success = RestOfAssignmentTerms(arena, input, symbolTable,
                                    enclosingBlock, assgnNode, &exprNode);

  *node = exprNode;
  return success;
}/*<<<*/

bool32 VarStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                    Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Var)
  {
    ConsumeToken(input, symbolTable);
    if(input->token == Token_Id)
    {
      AstNode* varNode = 0;
      Symbol* symbol = 0;
      
      varNode = PushElement(arena, AstNode, 1);
      varNode->kind = Ast_VarDecl;
      
      symbol = RegisterNewSymbol(symbolTable, input, Symbol_Var);
      if(symbol)
      {
        VarDecl* varDecl = 0;

        symbol->astNode = varNode;
        varDecl = &varNode->varDecl;
        varDecl->symbol = symbol;
        varDecl->name   = symbol->name;

        ConsumeToken(input, symbolTable);
        *node = varNode;
      }
    } else {
      SyntaxError(input, "Missing identifier");
      success = false;
    }
  }

  return success;
}/*<<<*/

bool32 FormalArgument(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                      Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Var)
  {
    ConsumeToken(input, symbolTable);
    if(input->token == Token_Id)
    {
      AstNode* varNode = 0;
      Symbol* symbol = 0;

      varNode = PushElement(arena, AstNode, 1);
      varNode->kind = Ast_VarDecl;

      symbol = RegisterNewSymbol(symbolTable, input, Symbol_Var);
      if(symbol)
      {
        VarDecl* varDecl = 0;

        symbol->astNode = varNode;
        varDecl = &varNode->varDecl;
        varDecl->symbol = symbol;
        varDecl->name   = symbol->name;

        ConsumeToken(input, symbolTable);
        *node = varNode;
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

bool32 FormalArgumentsList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                           Block* enclosingBlock, AstNode* node, AstList* lastArgItem)
{/*>>>*/
  assert(node->kind == Ast_Proc);

  bool32 success = true;
  AstNode* argNode = 0;

  success = FormalArgument(arena, input, symbolTable,
                           enclosingBlock, &argNode);
  if(success && argNode)
  {
    AstList* argItem = 0;
    
    argItem = PushElement(arena, AstList, 1);
    argItem->astNode  = argNode;
    lastArgItem->nextItem = argItem;
    node->proc.argCount++;

    if(input->token == Token_Comma)
    {
      ConsumeToken(input, symbolTable);
      success = FormalArgumentsList(arena, input, symbolTable,
                                    enclosingBlock, node, argItem);
    }
  }

  return success;
}/*<<<*/

bool32 ActualArgumentsList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                           Block* enclosingBlock, AstNode* node, AstList* lastArgItem)
{/*>>>*/
  bool32 success = true;

  AstNode* argNode = 0;
  success = Expression(arena, input, symbolTable,
                       enclosingBlock, &argNode);
  if(success && argNode)
  {
    AstList* argItem = 0;
    
    argItem = PushElement(arena, AstList, 1);
    argItem->astNode  = argNode;
    lastArgItem->nextItem = argItem;
    node->call.argCount++;

    if(input->token == Token_Comma)
    {
      ConsumeToken(input, symbolTable);
      success = ActualArgumentsList(arena, input, symbolTable,
                                    enclosingBlock, node, argItem);
    }
  }

  return success;
}/*<<<*/

bool32 WhileStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                      Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_While)
  {
    AstNode* expr = 0;

    ConsumeToken(input, symbolTable);
    success = Expression(arena, input, symbolTable,
                         enclosingBlock, &expr);
    if(success)
    {
      AstNode* whileNode = 0;
      WhileStmt* whileStmt = 0;
      
      whileNode       = PushElement(arena, AstNode, 1);
      whileNode->kind = Ast_While;
      whileStmt       = &whileNode->whileStmt;
      whileStmt->expr = expr;

      if(input->token == Token_OpenBrace)
      {
        ConsumeToken(input, symbolTable);

        success = BeginScope(symbolTable);
        if(success)
        {
          AstNode* blockNode = 0;
          Block* block = 0;
          
          blockNode       = PushElement(arena, AstNode, 1);
          blockNode->kind = Ast_Block;

          block               = &blockNode->block;
          block->owner        = whileNode;
          block->blockId      = symbolTable->currScopeId;
          block->nestingDepth = symbolTable->nestingDepth;

          success = BlockAst(arena, input, symbolTable,
                             block, &block->stmtList, &block->declVars);
          if(success)
          {
            whileStmt->body = blockNode;

            if(input->token == Token_CloseBrace)
            {
              ConsumeToken(input, symbolTable);
              EndScope(symbolTable);

              *node = whileNode;
            } else {
              SyntaxError(input, "Missing '}'");
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

  return success;
}/*<<<*/

#if 0
bool32 IfStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                   Block* in_enclosingBlock, AstNode** out_ifAst,
                   AstList** inOut_localsList, AstList** inOut_nonLocalsList)
{/*>>>*/
  *out_ifAst = 0;
  bool32 success = true;

  if(input->token == Token_If)
  {
    ConsumeToken(input, symbolTable);

    AstNode* ifAst = PushElement(arena, AstNode, 1);
    ifAst->kind = Ast_IfStmt;

    AstNode* expr = 0;
    success = Expression(arena, input, symbolTable, &expr);
    if(success)
    {
      ifAst->ifStmt.expr = expr;

      if(input->token == Token_OpenBrace)
      {
        ConsumeToken(input, symbolTable);

        AstNode* block = 0;
        success = BeginScope(symbolTable) &&
          Block(arena, input, symbolTable, &block);
        if(success)
        {
          ifAst->ifStmt.body = block;

          if(input->token == Token_CloseBrace)
          {
            ConsumeToken(input, symbolTable);
            EndScope(symbolTable);
            *out_ifAst = ifAst;

            if(input->token == Token_Else)
            {
              ConsumeToken(input, symbolTable);

              if(input->token == Token_OpenBrace)
              {
                ConsumeToken(input, symbolTable);

                AstNode* block = 0;
                success = BeginScope(symbolTable) &&
                  Block(arena, input, symbolTable, &block);
                if(success)
                {
                  ifAst->ifStmt.bodyElse = block;

                  if(input->token == Token_CloseBrace)
                  {
                    ConsumeToken(input, symbolTable);
                    EndScope(symbolTable);
                  } else {
                    SyntaxError(input, "Missing '}'");
                    success = false;
                  }
                }
              } else {
                SyntaxError(input, "Missing '{'");
                success = false;
              }
            }
          } else {
            SyntaxError(input, "Missing '}'");
            success = false;
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
#endif

bool32 Procedure(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                 Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Proc)
  {
    AstNode* procNode = 0;

    ConsumeToken(input, symbolTable);
    
    procNode = PushElement(arena, AstNode, 1);
    procNode->kind = Ast_Proc;

    if(input->token == Token_Id)
    {
      Symbol* symbol = 0;
      
      symbol = RegisterNewSymbol(symbolTable, input, Symbol_Proc);
      if(symbol)
      {
        Proc* proc = 0;

        symbol->astNode = procNode;
        proc         = &procNode->proc;
        proc->symbol = symbol;
        proc->name   = symbol->name;
        ConsumeToken(input, symbolTable);

        if(input->token == Token_OpenParens)
        {
          ConsumeToken(input, symbolTable);

          // arguments
          success = BeginScope(symbolTable);
          if(success)
          {
            AstNode* blockNode = 0;
            Block* block = 0; 

            blockNode       = PushElement(arena, AstNode, 1);
            blockNode->kind = Ast_Block;
            block               = &blockNode->block;
            block->owner        = procNode;
            block->blockId      = symbolTable->currScopeId;
            block->nestingDepth = symbolTable->nestingDepth;

            success = FormalArgumentsList(arena, input, symbolTable,
                                          block, procNode, &proc->argList);
            if(success)
            {
              if(input->token == Token_CloseParens)
              {
                ConsumeToken(input, symbolTable);

                if(input->token == Token_OpenBrace)
                {
                  // body
                  ConsumeToken(input, symbolTable);

                  success = BlockAst(arena, input, symbolTable,
                                     block, &block->stmtList, &block->declVars);
                  if(success)
                  {
                    assert(blockNode);
                    proc->body = blockNode;
                    *node = procNode;
                  }

                  if(input->token == Token_CloseBrace)
                  {
                    ConsumeToken(input, symbolTable);
                    EndScope(symbolTable); // body
                  } else {
                    SyntaxError(input, "Missing '}'");
                    success = false;
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

bool32 ProcedureList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                     Block* enclosingBlock, AstNode* node)
{/*>>>*/
  bool32 success = true;
  AstNode* procNode = 0;

  success = Procedure(arena, input, symbolTable, enclosingBlock, &procNode);
  if(success && procNode)
  {
    AstList* procItem = 0;
    
    procItem           = PushElement(arena, AstList, 1);
    procItem->astNode  = procNode;
    procItem->nextItem = node->module.procList;
    node->module.procList = procItem;

    success = ProcedureList(arena, input, symbolTable,
                            enclosingBlock, node);
  }

  return success;
}/*<<<*/

bool32 ReturnStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                       Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Return)
  {
    AstNode* exprNode = 0;

    ConsumeToken(input, symbolTable);
    success = Expression(arena, input, symbolTable,
                         enclosingBlock, &exprNode);
    if(success)
    {
      if(exprNode)
      {
        AstNode* retNode = 0;
        ReturnStmt* retStmt = 0; 
        Block* block = 0;

        retNode       = PushElement(arena, AstNode, 1);
        retNode->kind = Ast_Return;
        retStmt       = &retNode->retStmt;
        retStmt->expr = exprNode;

        int depth = 0;
        block = enclosingBlock;
        while(block)
        {
          AstNode* owner = block->owner;
          if(owner->kind == Ast_Proc)
            break;
          depth++;
          block = block->enclosingBlock;
        }
        assert(block);
        retStmt->proc = block->owner;
        retStmt->interveningBlockCount = depth;

        *node = retNode;
      } else {
        SyntaxError(input, "Expression required after the 'return' keyword");
        success = false;
      }
    }
  }

  return success;
}/*<<<*/

bool32 Statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                 Block* enclosingBlock, AstNode** node)
{/*>>>*/
  bool32 success = true;

  enum Alternative
  {
    Alt__Null,
    Alt_Var,
    Alt_Expr,
//    Alt_If,
    Alt_While,
    Alt_Return,
  };

  Alternative alt = (Alternative)1;
  AstNode* stmtNode = 0;

  while(alt) {
    switch(alt) {
      case Alt_Expr:
        {
          success = Expression(arena, input, symbolTable,
                               enclosingBlock, &stmtNode);
          if(success)
          {
            if(stmtNode)
            {
              alt = Alt__Null;
              if(input->token == Token_Semicolon)
              {
                BinExpr* expr = 0;

                ConsumeToken(input, symbolTable);
                expr = &stmtNode->binExpr;
                if(expr->op == Operator_Assign)
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
#if 0
      case Alt_If:
        {
          success = IfStatement(arena, input, symbolTable,
                                in_enclosingBlock, &stmtNode, inOut_localsList, inOut_nonLocalsList);
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
#endif
      case Alt_While:
        {
          success = WhileStatement(arena, input, symbolTable,
                                   enclosingBlock, &stmtNode);
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
          success = ReturnStatement(arena, input, symbolTable,
                                    enclosingBlock, &stmtNode);
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
          success = VarStatement(arena, input, symbolTable,
                                 enclosingBlock, &stmtNode);
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

bool32 BlockAst(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                Block* block, AstList* lastStmtItem, AstList* lastVarDeclItem)
{/*>>>*/
  bool32 success = true;
  AstNode* stmtNode = 0;

  success = Statement(arena, input, symbolTable,
                      block, &stmtNode);
  if(success && stmtNode)
  {
    AstList* stmtItem = 0;
    
    stmtItem          = PushElement(arena, AstList, 1);
    stmtItem->astNode = stmtNode;

    while(input->token == Token_Semicolon)
      ConsumeToken(input, symbolTable);

    if(stmtNode->kind == Ast_VarDecl)
    {
      lastVarDeclItem->nextItem = stmtItem;
      block->declVarCount++;
      BlockAst(arena, input, symbolTable,
               block, lastStmtItem, stmtItem);
    }
    else
    {
      lastStmtItem->nextItem = stmtItem;
      block->stmtCount++;
      BlockAst(arena, input, symbolTable,
               block, stmtItem, lastVarDeclItem);
    }
  }
  return success;
}/*<<<*/

bool32 ModuleAst(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                 AstNode** node)
{/*>>>*/
  bool32 success = true;

  success = BeginScope(symbolTable);
  if(success)
  {
    AstNode* moduleNode = 0;
    AstNode* blockNode = 0;
    Block* block = 0;

    moduleNode       = PushElement(arena, AstNode, 1);
    moduleNode->kind = Ast_Module;

    blockNode       = PushElement(arena, AstNode, 1);
    blockNode->kind = Ast_Block;
    block               = &blockNode->block;
    block->owner        = moduleNode;
    block->blockId      = symbolTable->currScopeId;
    block->nestingDepth = symbolTable->nestingDepth;

    moduleNode->module.body = blockNode;

    success = ProcedureList(arena, input, symbolTable, block, moduleNode);
    if(success)
    {
      EndScope(symbolTable);

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
              AstList* procList = block->module.procList;
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

              AstList* argList = block->proc.argList;
              int argsAreaSize = 0;
              int argsCount = 0;
              DataObjList* irArgs = 0;
              while(argList)
              {
                AstNode* argAst = argList->ast;
                Symbol* argSymbol = argAst->id.symbol;

                assert(argAst->kind == Ast_Var);
                assert(argSymbol->kind == Symbol_Var);

                DataObj* irArg = &argSymbol->dataObj;
                // Note that the storage locations for arguments are negative
                irArg->location = -(argsAreaSize + MACHINE_STATUS_AREA_SIZE + 1);
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
        AstList* declList = block->declVars;
        DataObjList* irLocalsList = 0;
        int localAreaSize = 0;
        while(declList)
        {
          AstNode* varAst = declList->ast;
          Symbol* varSymbol = varAst->var.symbol;

          assert(varAst->kind == Ast_Var);
          assert(varSymbol->kind == Symbol_Var);

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
        AstList* nonLocalsList = block->nonLocalVars;
        int accessLinkCount = 0;
        AccessLink* accessLinks = 0;
        while(nonLocalsList)
        {
          AstNode* idAst = nonLocalsList->ast;
          Symbol* idSymbol = idAst->id.symbol;

          assert(idAst->kind == Ast_Id);
          assert(idAst->id.declBlockOffset > 0);
          assert(idSymbol->kind == Symbol_Var);

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
          AstList* stmtList = block->stmtList;
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
              assert(procSymbol->kind == Symbol_Proc);

              Emit(irProgram, "push 0 ;retval of %s", callAst->call.name);

              Emit(irProgram, ";begin arg-eval");
              AstList* argList = callAst->call.argList;
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

              AstList* procList = block->module.procList;
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

              AstList* stmtList = block->stmtList;
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
              AstList* stmtList = block->stmtList;
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
        int retValLocation = MACHINE_STATUS_AREA_SIZE + actvRecord->proc.argsAreaSize +
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
{
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

  AddKeyword(&symbolTable, "int", Token_If);
  AddKeyword(&symbolTable, "float", Token_Float);
  AddKeyword(&symbolTable, "void", Token_Void);
  AddKeyword(&symbolTable, "char", Token_Char);
  AddKeyword(&symbolTable, "var", Token_Var);
  AddKeyword(&symbolTable, "proc", Token_Proc);
  AddKeyword(&symbolTable, "type", Token_Type);
  AddKeyword(&symbolTable, "struct", Token_Type);
  AddKeyword(&symbolTable, "array", Token_Array);
  AddKeyword(&symbolTable, "of", Token_Of);
  AddKeyword(&symbolTable, "if", Token_If);
  AddKeyword(&symbolTable, "else", Token_Else);
  AddKeyword(&symbolTable, "while", Token_While);
  AddKeyword(&symbolTable, "return", Token_Return);
  AddKeyword(&symbolTable, "true", Token_True);
  AddKeyword(&symbolTable, "false", Token_False);

  ConsumeToken(&tokenStream, &symbolTable);

  AstNode* module = 0;
  bool32 success = ModuleAst(arena, &tokenStream, &symbolTable, &module);
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
}
