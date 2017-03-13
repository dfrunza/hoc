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
};

enum SymbolKind
{
  Symbol__Null,
  Symbol_Keyword,
  Symbol_Proc,
  Symbol_DataObj,
};

enum AstKind
{
  Ast__Null,
  Ast_Expr,
  Ast_IntNum,
  Ast_Var,
  Ast_Id, // occurrence of a variable
  Ast_Call, // occurrence of a procedure
  Ast_Block,
  Ast_Return,
};

enum BlockKind
{
  Block__Null,
  Block_Module,
  Block_Proc,
  Block_WhileStmt,
  Block_IfStmt,
  Block_Anonymous,
};

struct AstNode;
struct IrActivationRecord;
struct Symbol;

struct IrDataObj
{
  int location; // relative to fp
  int size;
};

struct IrDataObjList
{
  IrDataObj*     dataObj;
  IrDataObjList* nextItem;
};

struct Symbol
{
  SymbolKind kind;
  char*      name;
  int        blockId;
  int        nestingDepth;
  AstNode*   ast;

  Symbol*    nextSymbol;

  union {
    IrDataObj dataObj;
    Token kwToken;
  };
};

struct AstList
{
  AstNode* ast;
  AstList* nextListItem;
};

struct IrAccessLink
{
  int actvRecordOffset;
  int index;

  IrAccessLink*   nextLink;
};

enum IrActivationRecordKind
{
  ActvRecord__Null,
  ActvRecord_Proc,
  ActvRecord_Block,
};

/* old_ip + old_sp + old_fp */
#define MACHINE_STATUS_AREA_SIZE 3

struct IrActivationRecord
{
  IrActivationRecordKind kind;

  IrDataObjList* localObjects;
  int            localAreaSize;

  IrAccessLink* accessLinks;
  int           accessLinkCount;

  struct {
    IrDataObjList* args;
    int            argsCount;
    int            argsAreaSize;

    IrDataObj* ret;
    int        retAreaSize;
  } proc;
};

struct Block
{
  BlockKind kind;
  int       blockId;
  int       nestingDepth;
  AstList*  declIds;
  AstList*  localIds;
  AstList*  nonLocalIds;
  AstList*  stmtList;
  Block*    enclosingBlock;
  IrActivationRecord actvRecord;

  union {
    struct {
      Symbol* symbol;
      char* name;
      AstList* argList;
      int      argCount;
      AstNode* ret;
    } proc;

    struct {
      AstList* procList;
    } module;

    struct {
      Symbol*  procSymbol;
      char*    procName;
      AstNode* expr;
      AstNode* elseStmtList;
    } ifStmt;

    struct {
      AstNode* expr;
    } whileStmt;
  };
};

struct AstNode
{
  AstKind kind;

  union {
    struct {
      OperatorType op;
      AstNode* leftOperand;
      AstNode* rightOperand;
      bool32   isStatement;
    } expr;

    struct {
      Symbol* symbol;
      char*   name;
      int     declBlockOffset;
      bool32  isNonLocal;
      IrAccessLink* accessLink; // non-local object
    } id;

    struct {
      Symbol* symbol;
      char*   name;
    } var;

    struct {
      int32 intNum;
    } literal;

    struct {
      Symbol*  symbol;
      char*    name;
      AstList* argList;
      int      argCount;
      IrActivationRecord* actvRecord;
    } call;

    struct {
      AstNode* expr;
      int      interveningBlockCount;
      Block*   block;
      IrActivationRecord* actvRecord;
    } ret;

    Block* block;
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
  char*  labels;
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

bool32 IdListContainsSymbol(AstList* astList, AstNode* idAst)
{
  assert(idAst->kind == Ast_Id);

  bool32 result = false;
  AstList* listItem = astList;
  while(listItem)
  {
    AstNode* ast = listItem->ast;
    assert(ast->kind == Ast_Id);
    if(ast->id.symbol == idAst->id.symbol)
    {
      result = true;
      break;
    }
    listItem = listItem->nextListItem;
  }
  return result;
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

    while(IsLetterChar(c) || IsNumericChar(c))
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

bool32 Expression(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                  AstNode** out_exprAst,
                  AstList** inOut_localsList, AstList** inOut_nonLocalsList);

bool32 BlockAst(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                BlockKind blockKind, Block* in_enclosingBlock, Block** out_block);

bool32 ActualArgumentsList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                           AstList* in_tailItem, AstList** out_argList, int* out_argCount,
                           AstList** inOut_localsList, AstList** inOut_nonLocalsList);

bool32 Factor(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
              AstNode** out_factorAst,
              AstList** inOut_localsList, AstList** inOut_nonLocalsList)
{/*>>>*/
  *out_factorAst = 0;
  bool32 success = true;

  if(input->token == Token_OpenParens)
  {
    ConsumeToken(input, symbolTable);
    success = Expression(arena, input, symbolTable,
                         out_factorAst, inOut_localsList, inOut_nonLocalsList);
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
  else if(input->token == Token_IntNum)
  {
    AstNode* numAst = PushElement(arena, AstNode, 1);
    numAst->kind = Ast_IntNum;
    numAst->literal.intNum = *(int32*)input->lexval.intNum;
    *out_factorAst = numAst;
    ConsumeToken(input, symbolTable);
  }
  else if(input->token == Token_Id)
  {
    AstNode* idAst = PushElement(arena, AstNode, 1);
    idAst->kind = Ast_Id;
    idAst->id.name = input->lexval.id;

    Symbol* symbol = LookupSymbol(symbolTable, input->lexval.id);
    if(symbol)
    {
      idAst->id.symbol = symbol;
      *out_factorAst = idAst;
      ConsumeToken(input, symbolTable);

      if(symbol->kind == Symbol_Proc)
      {
        if(input->token == Token_OpenParens)
        {
          // This is a procedure call
          ConsumeToken(input, symbolTable);
          AstList* argList = 0;
          int argCount = 0;
          success = ActualArgumentsList(arena, input, symbolTable,
                                        0, &argList, &argCount, inOut_localsList, inOut_nonLocalsList);
          if(success)
          {
            if(input->token == Token_CloseParens)
            {
              ConsumeToken(input, symbolTable);
              idAst->kind          = Ast_Call;
              idAst->call.symbol   = symbol;
              idAst->call.name     = symbol->name;
              idAst->call.argList  = argList;
              idAst->call.argCount = argCount;

              AstNode* procAst = symbol->ast;
              assert(procAst->kind == Ast_Block);
              assert(procAst->block->kind == Block_Proc);
              Block* procBlock = procAst->block;
              idAst->call.actvRecord = &procBlock->actvRecord;
              if(procBlock->proc.argCount == argCount)
              {
                AstNode* exprAst = PushElement(arena, AstNode, 1);
                exprAst->kind             = Ast_Expr;
                exprAst->expr.op          = Operator_Call;
                exprAst->expr.leftOperand = idAst;

                *out_factorAst = exprAst;
              } else {
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
      else if(symbol->kind == Symbol_DataObj)
      {
        idAst->id.declBlockOffset = (symbolTable->nestingDepth - symbol->nestingDepth);
        AstList* idItem = PushElement(arena, AstList, 1);
        idItem->ast = idAst;
        idAst->id.isNonLocal = (idAst->id.declBlockOffset > 0);
        if(idAst->id.isNonLocal)
        {
          idItem->nextListItem = *inOut_nonLocalsList;
          *inOut_nonLocalsList = idItem;
        }
        else
        {
          assert(idAst->id.declBlockOffset == 0);
          idItem->nextListItem = *inOut_localsList;
          *inOut_localsList = idItem;
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
    AstNode* numAst = PushElement(arena, AstNode, 1);
    numAst->kind = Ast_IntNum;
    numAst->literal.intNum = (input->token == Token_True ? 1 : 0);
    *out_factorAst = numAst;
    ConsumeToken(input, symbolTable);
  }

  return success;
}/*<<<*/

bool32 RestOfFactors(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                     AstNode* in_leftFactorAst, AstNode** out_factorAst,
                     AstList** inOut_localsList, AstList** inOut_nonLocalsList)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Star ||
     input->token == Token_FwdSlash)
  {
    AstNode* astNode = PushElement(arena, AstNode, 1);
    astNode->kind = Ast_Expr;
    if(input->token == Token_Star)
      astNode->expr.op = Operator_Mul;
    else if(input->token == Token_FwdSlash)
      astNode->expr.op = Operator_Div;
    else
      assert(false);

    ConsumeToken(input, symbolTable);
    AstNode* factorNode = 0;
    success = Factor(arena, input, symbolTable,
                     &factorNode, inOut_localsList, inOut_nonLocalsList);
    if(success && factorNode)
    {
        astNode->expr.rightOperand = factorNode;
        astNode->expr.leftOperand = in_leftFactorAst;
        success = RestOfFactors(arena, input, symbolTable,
                                astNode, out_factorAst, inOut_localsList, inOut_nonLocalsList);
    } else {
      SyntaxError(input, "Factor expected");
      success = false;
    }
  }
  else
    *out_factorAst = in_leftFactorAst;

  return success;
}/*<<<*/

bool32 Term(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
            AstNode** out_termAst,
            AstList** inOut_localsList, AstList** inOut_nonLocalsList)
{/*>>>*/
  AstNode* factor = 0;
  AstNode* expr = 0;

  bool32 success = Factor(arena, input, symbolTable,
                          &factor, inOut_localsList, inOut_nonLocalsList);
  if(success && factor)
    success = RestOfFactors(arena, input, symbolTable,
                            factor, &expr, inOut_localsList, inOut_nonLocalsList);

  *out_termAst = expr;
  return success;
}/*<<<*/

bool32 RestOfTerms(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                   AstNode* in_leftTermAst, AstNode** out_termAst,
                   AstList** inOut_localsList, AstList** inOut_nonLocalsList)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Plus ||
     input->token == Token_Minus)
  {
    AstNode* opAst = PushElement(arena, AstNode, 1);
    opAst->kind = Ast_Expr;
    if(input->token == Token_Plus)
      opAst->expr.op = Operator_Add;
    else if(input->token == Token_Minus)
      opAst->expr.op = Operator_Sub;
    else
      assert(false);

    ConsumeToken(input, symbolTable);
    AstNode* termAst = 0;
    success = Term(arena, input, symbolTable,
                   &termAst, inOut_localsList, inOut_nonLocalsList);
    if(success && termAst)
    {
      opAst->expr.rightOperand = termAst;
      opAst->expr.leftOperand = in_leftTermAst;
      success = RestOfTerms(arena, input, symbolTable,
                            opAst, out_termAst, inOut_localsList, inOut_nonLocalsList);
    } else {
      SyntaxError(input, "Expression term expected");
      success = false;
    }
  }
  else
   *out_termAst = in_leftTermAst;

  return success;
}/*<<<*/

bool32 AssignmentTerm(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                      AstNode** out_termAst,
                      AstList** inOut_localsList, AstList** inOut_nonLocalsList)
{/*>>>*/
  AstNode* termAst = 0;
  AstNode* exprAst = 0;

  bool32 success = Term(arena, input, symbolTable,
                        &termAst, inOut_localsList, inOut_nonLocalsList);
  if(success && termAst)
    success = RestOfTerms(arena, input, symbolTable,
                          termAst, &exprAst, inOut_localsList, inOut_nonLocalsList);

  *out_termAst = exprAst;
  return success;
}/*<<<*/

bool32 RestOfAssignmentTerms(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                             AstNode* in_leftTermAst, AstNode** out_termAst,
                             AstList** inOut_localsList, AstList** inOut_nonLocalsList)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Equals)
  {
    ConsumeToken(input, symbolTable);

    AstNode* rightSide = 0;
    success = Expression(arena, input, symbolTable,
                         &rightSide, inOut_localsList, inOut_nonLocalsList);
    if(success)
    {
      if(rightSide)
      {
        if(in_leftTermAst->kind == Ast_Id)
        {
          //TODO: Validate that the left side is an L-value
          AstNode* assgnAst = PushElement(arena, AstNode, 1);
          assgnAst->kind = Ast_Expr;
          assgnAst->expr.op = Operator_Assign;
          assgnAst->expr.leftOperand = in_leftTermAst;
          assgnAst->expr.rightOperand = rightSide;
          *out_termAst = assgnAst;
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
    *out_termAst = in_leftTermAst;

  return success;
}/*<<<*/

bool32 Expression(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                  AstNode** out_exprAst,
                  AstList** inOut_localsList, AstList** inOut_nonLocalsList)
{/*>>>*/
  AstNode* assgnAst = 0;
  AstNode* exprAst = 0;

  bool32 success = AssignmentTerm(arena, input, symbolTable,
                                  &assgnAst, inOut_localsList, inOut_nonLocalsList);
  if(success && assgnAst)
    success = RestOfAssignmentTerms(arena, input, symbolTable,
                                    assgnAst, &exprAst, inOut_localsList, inOut_nonLocalsList);

  *out_exprAst = exprAst;
  return success;
}/*<<<*/

bool32 VarStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                    AstNode** out_varStmt)
{/*>>>*/
  *out_varStmt = 0;
  bool32 success = true;

  if(input->token == Token_Var)
  {
    ConsumeToken(input, symbolTable);
    if(input->token == Token_Id)
    {
      AstNode* varStmt = PushElement(arena, AstNode, 1);
      varStmt->kind = Ast_Var;

      Symbol* symbol = LookupSymbol(symbolTable, input->lexval.id);
      if(!symbol)
      {
        symbol = AddSymbol(symbolTable, input->lexval.id, Symbol_DataObj);
        IrDataObj* dataObj = &symbol->dataObj;
        dataObj->size   = 1;
        symbol->ast     = varStmt;

        varStmt->var.symbol = symbol;
        varStmt->var.name = symbol->name;
        ConsumeToken(input, symbolTable);

        *out_varStmt = varStmt;
      } else {
        if(symbol->kind == Symbol_Keyword)
          SyntaxError(input, "Keyword '%s' used as identifier", symbol->name);
        else
          SyntaxError(input, "Name is used in a previous declaration: '%s'", symbol->name);
        success = false;
      }
    } else {
      SyntaxError(input, "Missing identifier");
      success = false;
    }
  }

  return success;
}/*<<<*/

bool32 FormalArgumentsList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                           AstList* in_prevItem, AstList** out_argList, int* out_argCount)
{/*>>>*/
  *out_argList = 0;
  bool32 success = true;

  AstNode* varAst = 0;
  success = VarStatement(arena, input, symbolTable, &varAst);
  if(success && varAst)
  {
    AstList* varItem = PushElement(arena, AstList, 1);
    varItem->ast = varAst;
    (*out_argCount)++;

    if(input->token == Token_Comma)
    {
      ConsumeToken(input, symbolTable);
      AstList* nextItem = 0;
      success = FormalArgumentsList(arena, input, symbolTable,
                                    0, &nextItem, out_argCount);

      varItem->nextListItem = nextItem;
    }

    *out_argList = varItem;
  }

  return success;
}/*<<<*/

bool32 ActualArgumentsList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                           AstList* in_prevItem, AstList** out_argList, int* out_argCount,
                           AstList** inOut_localsList, AstList** inOut_nonLocalsList)
{/*>>>*/
  *out_argList = 0;
  bool32 success = true;

  AstNode* argAst = 0;
  success = Expression(arena, input, symbolTable,
                       &argAst, inOut_localsList, inOut_nonLocalsList);
  if(success && argAst)
  {
    AstList* argItem = PushElement(arena, AstList, 1);
    argItem->ast = argAst;
    argItem->nextListItem = in_prevItem;
    (*out_argCount)++;
    *out_argList = argItem;

    if(input->token == Token_Comma)
    {
      ConsumeToken(input, symbolTable);
      AstList* nextItem = 0;
      success = ActualArgumentsList(arena, input, symbolTable,
                                    argItem, &nextItem, out_argCount,
                                    inOut_localsList, inOut_nonLocalsList);
      *out_argList = nextItem;
    }
  }
  return success;
}/*<<<*/

bool32 WhileStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                      Block* in_enclosingBlock, AstNode** out_whileAst,
                      AstList** inOut_localsList, AstList** inOut_nonLocalsList)
{/*>>>*/
  *out_whileAst = 0;
  bool32 success = true;

  if(input->token == Token_While)
  {
    ConsumeToken(input, symbolTable);

    AstNode* expr = 0;
    success = Expression(arena, input, symbolTable,
                         &expr, inOut_localsList, inOut_nonLocalsList);
    if(success)
    {
      if(input->token == Token_OpenBrace)
      {
        ConsumeToken(input, symbolTable);

        Block* block = 0;
        success = BeginScope(symbolTable) &&
          BlockAst(arena, input, symbolTable, Block_WhileStmt, in_enclosingBlock, &block);
        if(success)
        {
          block->whileStmt.expr = expr;

          AstNode* whileAst = PushElement(arena, AstNode, 1);
          whileAst->kind = Ast_Block;
          whileAst->block = block;

          if(input->token == Token_CloseBrace)
          {
            ConsumeToken(input, symbolTable);
            EndScope(symbolTable);
            *out_whileAst = whileAst;
          } else {
            SyntaxError(input, "Missing '}'");
            success = false;
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

bool32 IfStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                   Block* in_enclosingBlock, AstNode** out_ifAst,
                   AstList** inOut_localsList, AstList** inOut_nonLocalsList)
{/*>>>*/
  *out_ifAst = 0;
  bool32 success = true;

#if 0
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
#endif

  return success;
}/*<<<*/

bool32 Procedure(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                 Block* in_enclosingBlock, AstNode** out_procAst)
{/*>>>*/
  *out_procAst = 0;
  bool32 success = true;

  if(input->token == Token_Proc)
  {
    ConsumeToken(input, symbolTable);
    if(input->token == Token_Id)
    {
      Symbol* symbol = LookupSymbol(symbolTable, input->lexval.id);
      if(!symbol)
      {
        symbol = AddSymbol(symbolTable, input->lexval.id, Symbol_Proc);

        ConsumeToken(input, symbolTable);
        if(input->token == Token_OpenParens)
        {
          ConsumeToken(input, symbolTable);

          // arguments
          AstList *argList = 0;
          int argCount = 0;
          success = BeginScope(symbolTable) &&
            FormalArgumentsList(arena, input, symbolTable, 0, &argList, &argCount);
          if(success)
          {
            if(input->token == Token_CloseParens)
            {
              ConsumeToken(input, symbolTable);

              if(input->token == Token_OpenBrace)
              {
                // body
                ConsumeToken(input, symbolTable);

                Block* block = 0;
                success = BlockAst(arena, input, symbolTable, Block_Proc, in_enclosingBlock, &block);
                if(success)
                {
                  block->proc.symbol = symbol;
                  block->proc.name = symbol->name;
                  block->proc.argList = argList;
                  block->proc.argCount = argCount;

                  AstNode* procAst = PushElement(arena, AstNode, 1);
                  procAst->kind = Ast_Block;
                  procAst->block = block;

                  symbol->ast = procAst;

                  *out_procAst = procAst;

                  if(input->token == Token_CloseBrace)
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
              if(input->token == Token_Id)
                SyntaxError(input, "Missing 'var' keyword", input->lexval.id);
              else
                SyntaxError(input, "Missing ')'");
              success = false;
            }
          }
        } else {
          SyntaxError(input, "Missing '('");
          success = false;
        }
      } else {
        if(symbol->kind == Symbol_Keyword)
          SyntaxError(input, "Keyword '%s' used as identifier", symbol->name);
        else
          SyntaxError(input, "Name '%s' used in a previous declaration", symbol->name);
        success = false;
      }
    } else {
      SyntaxError(input, "Missing identifier");
      success = false;
    }
  }

  return success;
}/*<<<*/

bool32 ProcedureList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                     Block* in_enclosingBlock, AstList** out_procList)
{/*>>>*/
  *out_procList = 0;

  AstNode* procAst = 0;
  bool32 success = Procedure(arena, input, symbolTable, in_enclosingBlock, &procAst);
  if(success && procAst)
  {
    AstList* procItem = PushElement(arena, AstList, 1);
    procItem->ast = procAst;

    AstList* nextProcItem = 0;
    success = ProcedureList(arena, input, symbolTable, in_enclosingBlock, &nextProcItem);
    if(success)
      procItem->nextListItem = nextProcItem;
    *out_procList = procItem;
  }
  return success;
}/*<<<*/

bool32 ReturnStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                       Block* in_enclosingBlock, AstNode** out_stmtAst,
                       AstList** inOut_localsList, AstList** inOut_nonLocalsList)
{/*>>>*/
  *out_stmtAst = 0;
  bool32 success = true;

  if(input->token == Token_Return)
  {
    ConsumeToken(input, symbolTable);

    AstNode* expr = 0;
    success = Expression(arena, input, symbolTable,
                         &expr, inOut_localsList, inOut_nonLocalsList);
    if(success)
    {
      if(expr)
      {
        AstNode* retAst = PushElement(arena, AstNode, 1);
        retAst->kind = Ast_Return;
        retAst->ret.expr = expr;

        int depth = 0;
        Block* block = in_enclosingBlock;
        while(block)
        {
          if(block->kind == Block_Proc)
            break;
          depth++;
          block = block->enclosingBlock;
        }
        assert(block);
        block->proc.ret = retAst;
        retAst->ret.block = block;
        retAst->ret.interveningBlockCount = depth;

        *out_stmtAst = retAst;
      } else {
        SyntaxError(input, "Expression required after the 'return' keyword");
        success = false;
      }
    }
  }

  return success;
}/*<<<*/

bool32 Statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                 Block* in_enclosingBlock, AstNode** out_stmtAst,
                 AstList** inOut_localsList, AstList** inOut_nonLocalsList)
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
  AstNode* stmtAst = 0;

  while(alt) {
    switch(alt) {
      case Alt_Expr:
        {
          success = Expression(arena, input, symbolTable,
                               &stmtAst, inOut_localsList, inOut_nonLocalsList);
          if(success)
          {
            if(stmtAst)
            {
              alt = Alt__Null;
              if(input->token == Token_Semicolon)
              {
                ConsumeToken(input, symbolTable);
                if(stmtAst->expr.op == Operator_Assign)
                  stmtAst->expr.isStatement = true;
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
          success = IfStatement(arena, input, symbolTable,
                                in_enclosingBlock, &stmtAst, inOut_localsList, inOut_nonLocalsList);
          if(success)
          {
            if(stmtAst)
            {
              alt = Alt__Null;
            } else
              alt = (Alternative)((int)alt+1);
          } else
            alt = Alt__Null;
        } break;

      case Alt_While:
        {
          success = WhileStatement(arena, input, symbolTable,
                                   in_enclosingBlock, &stmtAst, inOut_localsList, inOut_nonLocalsList);
          if(success)
          {
            if(stmtAst)
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
                                    in_enclosingBlock, &stmtAst, inOut_localsList, inOut_nonLocalsList);
          if(success)
          {
            if(stmtAst)
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
          success = VarStatement(arena, input, symbolTable, &stmtAst);
          if(success)
          {
            if(stmtAst)
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

  if(stmtAst)
    *out_stmtAst = stmtAst;

  return success;
}/*<<<*/

bool32 StatementList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                     Block* in_enclosingBlock, AstList** out_stmtList, AstList** out_declList,
                     AstList** inOut_localsList, AstList** inOut_nonLocalsList)
{/*>>>*/
  *out_stmtList = *out_declList = 0;
  bool32 success = true;

  AstNode* stmt = 0;
  success = Statement(arena, input, symbolTable,
                      in_enclosingBlock, &stmt, inOut_localsList, inOut_nonLocalsList);
  if(success && stmt)
  {
    AstList* stmtItem = PushElement(arena, AstList, 1);
    stmtItem->ast = stmt;

    AstList* declItem = 0;
    if(stmt->kind == Ast_Var)
    {
      declItem = stmtItem;
      stmtItem = 0;
    }

    while(input->token == Token_Semicolon)
      ConsumeToken(input, symbolTable);

    AstList* nextStmtItem = 0, *nextVarItem = 0;
    success = StatementList(arena, input, symbolTable,
                            in_enclosingBlock, &nextStmtItem, &nextVarItem,
                            inOut_localsList, inOut_nonLocalsList);
    if(success)
    {
      if(declItem)
      {
        stmtItem = nextStmtItem;
        declItem->nextListItem = nextVarItem;
      }
      else
      {
        declItem = nextVarItem;
        stmtItem->nextListItem = nextStmtItem;
      }
    }
    *out_stmtList = stmtItem;
    *out_declList = declItem;
  }
  return success;
}/*<<<*/

bool32 BlockAst(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                BlockKind blockKind, Block* in_enclosingBlock, Block** out_block)
{/*>>>*/
  *out_block = 0;
  bool32 success = true;

  Block* block = PushElement(arena, Block, 1);
  block->blockId = symbolTable->currScopeId;
  block->kind = blockKind;
  block->nestingDepth = symbolTable->nestingDepth;
  block->enclosingBlock = in_enclosingBlock;

  AstList *stmtList = 0,
          *declList = 0,
          *localsList = 0,
          *nonLocalsList = 0;
  success = StatementList(arena, input, symbolTable,
                          block, &stmtList, &declList, &localsList, &nonLocalsList);
  if(success)
  {
    block->declIds = declList;
    block->localIds = localsList;
    block->nonLocalIds = nonLocalsList;
    block->stmtList = stmtList;

    *out_block = block;
  }

  return success;
}/*<<<*/

bool32 ModuleAst(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                 AstNode** out_module)
{/*>>>*/
  *out_module = 0;
  bool32 success = true;

  AstList* procList = 0;
  success = BeginScope(symbolTable);
  if(success)
  {
    Block* block = PushElement(arena, Block, 1);
    block->kind = Block_Module;
    block->blockId = symbolTable->currScopeId;
    block->nestingDepth = symbolTable->nestingDepth;

    success = ProcedureList(arena, input, symbolTable, block, &procList);
    if(success)
    {
      block->module.procList = procList;

      AstNode* module = PushElement(arena, AstNode, 1);
      module->kind = Ast_Block;
      module->block = block;

      EndScope(symbolTable);

      if(input->token == Token_EndOfInput)
        *out_module = module;
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
        IrActivationRecord* actvRecord = &block->actvRecord;
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

                procList = procList->nextListItem;
              }
            } break;

          case Block_Proc:
            {
              actvRecord->kind = ActvRecord_Proc;
              actvRecord->proc.retAreaSize = 1;

              AstList* argList = block->proc.argList;
              int argsAreaSize = 0;
              int argsCount = 0;
              IrDataObjList* irArgs = 0;
              while(argList)
              {
                AstNode* argAst = argList->ast;
                Symbol* argSymbol = argAst->id.symbol;

                assert(argAst->kind == Ast_Var);
                assert(argSymbol->kind == Symbol_DataObj);

                IrDataObj* irArg = &argSymbol->dataObj;
                // Note that the storage locations for arguments are negative
                irArg->location = -(argsAreaSize + MACHINE_STATUS_AREA_SIZE + 1);
                irArg->size     = 1;

                argsAreaSize += irArg->size;

                IrDataObjList* irArgItem = PushElement(arena, IrDataObjList, 1);
                irArgItem->dataObj = irArg;
                irArgItem->nextItem = irArgs;
                irArgs = irArgItem;

                argList = argList->nextListItem;
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
        AstList* declList = block->declIds;
        IrDataObjList* irLocalsList = 0;
        int localAreaSize = 0;
        while(declList)
        {
          AstNode* varAst = declList->ast;
          Symbol* varSymbol = varAst->var.symbol;

          assert(varAst->kind == Ast_Var);
          assert(varSymbol->kind == Symbol_DataObj);

          IrDataObj* dataObj = &varSymbol->dataObj;
          dataObj->location = localAreaSize;
          localAreaSize += dataObj->size;

          IrDataObjList* irLocalItem = PushElement(arena, IrDataObjList, 1);
          irLocalItem->dataObj = dataObj;
          irLocalItem->nextItem = irLocalsList;
          irLocalsList = irLocalItem;

          declList = declList->nextListItem;
        }
        actvRecord->localObjects = irLocalsList;
        actvRecord->localAreaSize = localAreaSize;

        // Process the non-local refs
        AstList* nonLocalsList = block->nonLocalIds;
        int accessLinkCount = 0;
        IrAccessLink* accessLinks = 0;
        while(nonLocalsList)
        {
          AstNode* idAst = nonLocalsList->ast;
          Symbol* idSymbol = idAst->id.symbol;

          assert(idAst->kind == Ast_Id);
          assert(idAst->id.declBlockOffset > 0);
          assert(idSymbol->kind == Symbol_DataObj);

          IrAccessLink* accessLink = 0;
          {
            IrAccessLink* link = accessLinks;
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
              accessLink = PushElement(arena, IrAccessLink, 1);
              accessLink->actvRecordOffset = idAst->id.declBlockOffset;
              accessLink->index = accessLinkCount++;

              accessLink->nextLink = accessLinks;
              accessLinks = accessLink;
            }
          }

          idAst->id.accessLink = accessLink;

          nonLocalsList = nonLocalsList->nextListItem;
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
            stmtList = stmtList->nextListItem;
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
        IrDataObj* dataObj = &symbol->dataObj;

        if(ast->id.isNonLocal)
        {
          IrAccessLink* accessLink = ast->id.accessLink;

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
                argList = argList->nextListItem;
              }
              Emit(irProgram, ";end arg-eval");

              Emit(irProgram, "call %s", callAst->call.name);

              IrActivationRecord* actvRecord = callAst->call.actvRecord;
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

                procList = procList->nextListItem;
              }
            } break;

          case Block_Proc:
            {
              Symbol* procSymbol = block->proc.symbol;
              Emit(irProgram, "label %s", procSymbol->name); // entry point

              IrActivationRecord* actvRecord = &block->actvRecord;
              int dataAreaSize = actvRecord->localAreaSize;
              if(dataAreaSize > 0)
                Emit(irProgram, "alloc %d ;local storage", dataAreaSize);

              AstList* stmtList = block->stmtList;
              while(stmtList)
              {
                AstNode* stmt = stmtList->ast;
                GenCode(irProgram, symbolTable, block, stmt);

                stmtList = stmtList->nextListItem;
              }

              Emit(irProgram, "label %s.end-proc", procSymbol->name);
              Emit(irProgram, "return");
            } break;

          case Block_WhileStmt:
            {
              IrActivationRecord* actvRecord = &block->actvRecord;

              char label[32] = {};
              MakeUniqueLabel(symbolTable, label);
              Emit(irProgram, "label %s.while-expr", label);

              // conditional expr
              GenCodeRValue(irProgram, block->whileStmt.expr);
              Emit(irProgram, "jumpz %s.while-break", label);

              // Highest indexed access link is first from the top
              Emit(irProgram, ";begin set-up of access links");
              IrAccessLink* accessLink = actvRecord->accessLinks;
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

              Emit(irProgram, "enter");

              if(actvRecord->localAreaSize > 0)
                Emit(irProgram, "alloc %d ;local storage", actvRecord->localAreaSize);

              // body
              AstList* stmtList = block->stmtList;
              while(stmtList)
              {
                AstNode* stmt = stmtList->ast;
                GenCode(irProgram, symbolTable, block, stmt);

                stmtList = stmtList->nextListItem;
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

        IrActivationRecord* actvRecord = ast->ret.actvRecord;
        assert(actvRecord->kind == ActvRecord_Proc);
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

  AstNode* moduleAst = 0;
  bool32 success = ModuleAst(arena, &tokenStream, &symbolTable, &moduleAst) &&
    BuildIr(arena, &symbolTable, moduleAst);
  if(success)
  {
    StringInit(&irProgram->text, arena);

    assert(moduleAst->kind == Ast_Block);
    Block* block = moduleAst->block;
    GenCode(irProgram, &symbolTable, block, moduleAst);

    assert(symbolTable.currScopeId == 0);
    assert(symbolTable.nestingDepth == 0);
  }

  return success;
}
