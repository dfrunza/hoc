#include "lib.cpp"

enum Token
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
};

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

struct Symbol
{
  SymbolKind kind;
  char* name;
  int blockId;
  int nestingDepth;
  Symbol* nextSymbol;

  union {
    struct {
      int dataSize;
    } dataObj;

    struct {
      int argCount;
    } proc;

    struct {
      Token token;
    } kw;
  };
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

struct AstNode;

struct AstList
{
  AstNode* ast;
  AstList* nextListItem;
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

struct AccessLink
{
  int actvRecordOffset;
  int index;
  AccessLink* nextLink;
};

struct IrDataObject
{
  int     storageLocation;
  bool32  isNonLocal;
  int     accessLinkIndex; // if non-local
  IrDataObject* nextObj;
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

  IrDataObject* nonLocals;
  AccessLink*   accessLinks;
  int           accessLinkCount;

  IrDataObject* locals;
  int           localsAreaSize;

  struct {
    IrDataObject*   args;
    int             argsCount;
    int             argsAreaSize;
    IrDataObject*   ret;
    int             retAreaSize;
  } proc;
};

struct Block
{
  BlockKind   kind;
  int         blockId;
  int         nestingDepth;
  Block*      enclosingBlock;
  AstList*    declIds;
  AstList*    localIds;
  AstList*    nonLocalIds;
  AstList*    stmtList;
  IrActivationRecord* actvRecord;

  union {
    struct {
      Symbol* symbol;
      char* name;
      AstList* argList;
    } proc;

    struct {
      AstList* procList;
    } module;

    struct {
      Symbol* procSymbol;
      char* procName;
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
      bool32 isStatement;
    } expr;

    struct {
      Symbol* symbol;
      char* name;
      int declBlockOffset;
      IrDataObject* dataObj;
    } id;

    struct {
      Symbol* symbol;
      char* name;
    } var;

    struct {
      int32 intNum;
    } literal;

    struct {
      Symbol* proc;
      char* name;
      AstList* argList;
      IrActivationRecord* actvRecord;
    } call;

    struct {
      int blockId;
      Symbol* proc;
      AstNode* expr;
      IrActivationRecord* actvRecord;
    } ret;

    Block* block;
  };
};

struct TokenStream
{
  Token prevToken;
  Token token;
  char* text;
  char* cursor;
  MemoryArena* arena;

  char* filePath;
  int lineNr;
  char* srcLine;

  union {
    int* intNum;
    char* id;
  } lexval;
};

struct SymbolTable
{
  Symbol* currSymbol;
  int currBlockId;
  int lastBlockId;
  int nestingDepth;
  int activeBlocks[32];
  char label[64];
  int lastLabelId;
  MemoryArena* arena;
};

struct IrProgram
{
  String text;
  int textLen;
  char* labels;
  MemoryArena* arena;
};

struct Translator
{
  MemoryArena* arena;
  TokenStream tokenStream;
  SymbolTable symbolTable;
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

Symbol* LookupEnclosingProc(SymbolTable* symbolTable)
{/*>>>*/
  Symbol* proc = 0;
  Symbol* symbol = symbolTable->currSymbol;

  while(symbol)
  {
    if(symbol->kind == Symbol_Proc)
    {
      proc = symbol;
      break;
    }
    symbol = symbol->nextSymbol;
  }
  return proc;
}/*<<<*/

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
  symbol->blockId = symbolTable->currBlockId;
  symbol->nestingDepth = symbolTable->nestingDepth;
  symbol->nextSymbol = symbolTable->currSymbol;
  symbolTable->currSymbol = symbol;
  return symbol;
}/*<<<*/

Symbol* AddKeyword(SymbolTable* symbolTable, char* name, Token token)
{
  Symbol* symbol = AddSymbol(symbolTable, name, Symbol_Keyword);
  symbol->kw.token = token;
  return symbol;
}

bool32 BeginBlock(SymbolTable* symbolTable)
{/*>>>*/
  int currBlockId = ++symbolTable->lastBlockId;
  symbolTable->currBlockId = currBlockId;

  int nestingDepth = ++symbolTable->nestingDepth;
  if(nestingDepth < SizeofArray(symbolTable->activeBlocks))
  {
    symbolTable->activeBlocks[nestingDepth] = currBlockId;
  } else {
    Error("Reached the maximum block nesting depth");
    return false;
  }

  return true;
}/*<<<*/

void EndBlock(SymbolTable* symbolTable)
{/*>>>*/
  int nestingDepth = --symbolTable->nestingDepth;
  int currBlockId = symbolTable->activeBlocks[nestingDepth];
  assert(currBlockId >= 0);
  symbolTable->currBlockId = currBlockId;

  Symbol* symbol = symbolTable->currSymbol;
  while(symbol && symbol->blockId > symbolTable->currBlockId)
    symbol = symbol->nextSymbol;
  symbolTable->currSymbol = symbol;
}/*<<<*/

void MakeUniqueLabel(SymbolTable* symbolTable, char* label)
{
  sprintf(label, "L%d", symbolTable->lastLabelId++);
}

char* MakeProcLabel(SymbolTable* symbolTable, Block* block)
{
  char* blockTag = 0;
  switch(block->kind)
  {
    case Block_WhileStmt:
      blockTag = "while"; break;
    default:
      assert(false && !"Not implemented");
  }
  sprintf(symbolTable->label, "B%d.%s", block->blockId, blockTag);
  return symbolTable->label;
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
      input->token = symbol->kw.token;
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
                  AstList** inOut_localsList, AstList** inOut_nonLocalsList, Block* in_enclosingBlock);

bool32 ActualArgumentsList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                           AstList* in_tailItem, AstList** out_argList,
                           AstList** inOut_localsList, AstList** inOut_nonLocalsList, Block* in_enclosingBlock);

bool32 Factor(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
              AstNode** out_factorAst,
              AstList** inOut_localsList, AstList** inOut_nonLocalsList, Block* in_enclosingBlock)
{/*>>>*/
  *out_factorAst = 0;
  bool32 success = true;

  if(input->token == Token_OpenParens)
  {
    ConsumeToken(input, symbolTable);
    success = Expression(arena, input, symbolTable,
                         out_factorAst, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
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
        ConsumeToken(input, symbolTable);
        if(input->token == Token_OpenParens)
        {
          // This is a procedure call
          ConsumeToken(input, symbolTable);
          AstList* argList = 0;
          success = ActualArgumentsList(arena, input, symbolTable,
                                        0, &argList, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
          if(success)
          {
            if(input->token == Token_CloseParens)
            {
              ConsumeToken(input, symbolTable);
              idAst->kind = Ast_Call;
              idAst->call.argList = argList;

              AstNode* exprAst = PushElement(arena, AstNode, 1);
              exprAst->kind = Ast_Expr;
              exprAst->expr.op = Operator_Call;
              exprAst->expr.leftOperand = idAst;

              *out_factorAst = exprAst;
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
        idAst->id.declBlockOffset = (in_enclosingBlock->nestingDepth - symbol->nestingDepth);
        AstList* idItem = PushElement(arena, AstList, 1);
        idItem->ast = idAst;
        idItem->nextListItem = *inOut_nonLocalsList;
        if(idAst->id.declBlockOffset > 0)
          *inOut_nonLocalsList = idItem;
        else
        {
          assert(idAst->id.declBlockOffset == 0);
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
                     AstList** inOut_localsList, AstList** inOut_nonLocalsList, Block* in_enclosingBlock)
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
                     &factorNode, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
    if(success && factorNode)
    {
        astNode->expr.rightOperand = factorNode;
        astNode->expr.leftOperand = in_leftFactorAst;
        success = RestOfFactors(arena, input, symbolTable,
                                astNode, out_factorAst, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
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
            AstNode** out_termAst, AstList** inOut_localsList, AstList** inOut_nonLocalsList, Block* in_enclosingBlock)
{/*>>>*/
  AstNode* factor = 0;
  AstNode* expr = 0;

  bool32 success = Factor(arena, input, symbolTable,
                          &factor, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
  if(success && factor)
    success = RestOfFactors(arena, input, symbolTable,
                            factor, &expr, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);

  *out_termAst = expr;
  return success;
}/*<<<*/

bool32 RestOfTerms(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                   AstNode* in_leftTermAst, AstNode** out_termAst,
                   AstList** inOut_localsList, AstList** inOut_nonLocalsList, Block* in_enclosingBlock)
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
                   &termAst, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
    if(success && termAst)
    {
      opAst->expr.rightOperand = termAst;
      opAst->expr.leftOperand = in_leftTermAst;
      success = RestOfTerms(arena, input, symbolTable,
                            opAst, out_termAst, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
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
                      AstList** inOut_localsList, AstList** inOut_nonLocalsList, Block* in_enclosingBlock)
{/*>>>*/
  AstNode* termAst = 0;
  AstNode* exprAst = 0;

  bool32 success = Term(arena, input, symbolTable,
                        &termAst, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
  if(success && termAst)
    success = RestOfTerms(arena, input, symbolTable,
                          termAst, &exprAst, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);

  *out_termAst = exprAst;
  return success;
}/*<<<*/

bool32 RestOfAssignmentTerms(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                             AstNode* in_leftTermAst, AstNode** out_termAst,
                             AstList** inOut_localsList, AstList** inOut_nonLocalsList, Block* in_enclosingBlock)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Equals)
  {
    ConsumeToken(input, symbolTable);

    AstNode* rightSide = 0;
    success = Expression(arena, input, symbolTable,
                         &rightSide, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
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
                  AstList** inOut_localsList, AstList** inOut_nonLocalsList, Block* in_enclosingBlock)
{/*>>>*/
  AstNode* assgnAst = 0;
  AstNode* exprAst = 0;

  bool32 success = AssignmentTerm(arena, input, symbolTable,
                                  &assgnAst, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
  if(success && assgnAst)
    success = RestOfAssignmentTerms(arena, input, symbolTable,
                                    assgnAst, &exprAst, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);

  *out_exprAst = exprAst;
  return success;
}/*<<<*/

bool32 VarStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                    AstNode** out_varStmt, Block* in_enclosingBlock)
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
        symbol->dataObj.dataSize = 1;
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
                           AstList* in_prevItem, AstList** out_argList, Block* in_enclosingBlock)
{/*>>>*/
  *out_argList = 0;
  bool32 success = true;

  AstNode* varAst = 0;
  success = VarStatement(arena, input, symbolTable, &varAst, in_enclosingBlock);
  if(success && varAst)
  {
    AstList* varItem = PushElement(arena, AstList, 1);
    varItem->ast = varAst;

    if(input->token == Token_Comma)
    {
      ConsumeToken(input, symbolTable);
      AstList* nextItem = 0;
      success = FormalArgumentsList(arena, input, symbolTable,
                                    0, &nextItem, in_enclosingBlock);

      varItem->nextListItem = nextItem;
    }

    *out_argList = varItem;
  }

  return success;
}/*<<<*/

bool32 ActualArgumentsList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                           AstList* in_prevItem, AstList** out_argList,
                           AstList** inOut_localsList, AstList** inOut_nonLocalsList, Block* in_enclosingBlock)
{/*>>>*/
  *out_argList = 0;
  bool32 success = true;

  AstNode* argAst = 0;
  success = Expression(arena, input, symbolTable,
                       &argAst, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
  if(success && argAst)
  {
    AstList* argItem = PushElement(arena, AstList, 1);
    argItem->ast = argAst;
    argItem->nextListItem = in_prevItem;
    *out_argList = argItem;

    if(input->token == Token_Comma)
    {
      ConsumeToken(input, symbolTable);
      AstList* nextItem = 0;
      success = ActualArgumentsList(arena, input, symbolTable,
                                    argItem, &nextItem, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
      *out_argList = nextItem;
    }
  }
  return success;
}/*<<<*/

bool32 Block_(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
              BlockKind blockKind, Block** out_block, Block* in_enclosingBlock);

bool32 WhileStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                      AstNode** out_whileAst,
                      AstList** inOut_localsList, AstList** inOut_nonLocalsList, Block* in_enclosingBlock)
{/*>>>*/
  *out_whileAst = 0;
  bool32 success = true;

  if(input->token == Token_While)
  {
    ConsumeToken(input, symbolTable);

    AstNode* expr = 0;
    success = Expression(arena, input, symbolTable,
                         &expr, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
    if(success)
    {
      if(input->token == Token_OpenBrace)
      {
        ConsumeToken(input, symbolTable);

        Block* block = 0;
        success = BeginBlock(symbolTable) &&
          Block_(arena, input, symbolTable, Block_WhileStmt, &block, in_enclosingBlock);
        if(success)
        {
          block->whileStmt.expr = expr;

          AstNode* whileAst = PushElement(arena, AstNode, 1);
          whileAst->kind = Ast_Block;
          whileAst->block = block;

          if(input->token == Token_CloseBrace)
          {
            ConsumeToken(input, symbolTable);
            EndBlock(symbolTable);
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
                   AstNode** out_ifAst,
                   AstList** inOut_localsList, AstList** inOut_nonLocalsList, Block* in_enclosingBlock)
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
        success = BeginBlock(symbolTable) &&
          Block(arena, input, symbolTable, &block);
        if(success)
        {
          ifAst->ifStmt.body = block;

          if(input->token == Token_CloseBrace)
          {
            ConsumeToken(input, symbolTable);
            EndBlock(symbolTable);
            *out_ifAst = ifAst;

            if(input->token == Token_Else)
            {
              ConsumeToken(input, symbolTable);

              if(input->token == Token_OpenBrace)
              {
                ConsumeToken(input, symbolTable);

                AstNode* block = 0;
                success = BeginBlock(symbolTable) &&
                  Block(arena, input, symbolTable, &block);
                if(success)
                {
                  ifAst->ifStmt.bodyElse = block;

                  if(input->token == Token_CloseBrace)
                  {
                    ConsumeToken(input, symbolTable);
                    EndBlock(symbolTable);
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
                 AstNode** out_procAst, Block* in_enclosingBlock)
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
          success = BeginBlock(symbolTable) &&
            FormalArgumentsList(arena, input, symbolTable,
                                0, &argList, in_enclosingBlock);
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
                success = Block_(arena, input, symbolTable, Block_Proc, &block, in_enclosingBlock);
                if(success)
                {
                  block->proc.symbol = symbol;
                  block->proc.name = symbol->name;
                  block->proc.argList = argList;

                  AstNode* procAst = PushElement(arena, AstNode, 1);
                  procAst->kind = Ast_Block;
                  procAst->block = block;

                  *out_procAst = procAst;

                  if(input->token == Token_CloseBrace)
                  {
                    ConsumeToken(input, symbolTable);
                    EndBlock(symbolTable);
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
                     AstList** out_procList, Block* in_enclosingBlock)
{/*>>>*/
  *out_procList = 0;

  AstNode* procAst = 0;
  bool32 success = Procedure(arena, input, symbolTable, &procAst, in_enclosingBlock);
  if(success && procAst)
  {
    AstList* procItem = PushElement(arena, AstList, 1);
    procItem->ast = procAst;

    AstList* nextProcItem = 0;
    success = ProcedureList(arena, input, symbolTable, &nextProcItem, in_enclosingBlock);
    if(success)
      procItem->nextListItem = nextProcItem;
    *out_procList = procItem;
  }
  return success;
}/*<<<*/

bool32 ReturnStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                       AstNode** out_stmtAst,
                       AstList** inOut_localsList, AstList** inOut_nonLocalsList, Block* in_enclosingBlock)
{/*>>>*/
  *out_stmtAst = 0;
  bool32 success = true;

  if(input->token == Token_Return)
  {
    ConsumeToken(input, symbolTable);
    
    AstNode* expr = 0;
    success = Expression(arena, input, symbolTable,
                         &expr, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
    if(success)
    {
      if(expr)
      {
        AstNode* retAst = PushElement(arena, AstNode, 1);
        retAst->kind = Ast_Return;
        retAst->ret.expr = expr;
        Symbol* procSymbol = LookupEnclosingProc(symbolTable);
        retAst->ret.proc = procSymbol;
        *out_stmtAst = retAst;
      } else {
        SyntaxError(input, "Expression is required after the 'return' keyword");
        success = false;
      }
    }
  }

  return success;
}/*<<<*/

bool32 Statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                 AstNode** out_stmtAst,
                 AstList** inOut_localsList, AstList** inOut_nonLocalsList, Block* in_enclosingBlock)
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
                               &stmtAst, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
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
                                &stmtAst, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
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
                                   &stmtAst, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
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
                                    &stmtAst, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
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
          success = VarStatement(arena, input, symbolTable, &stmtAst, in_enclosingBlock);
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
                          AstList** out_stmtList, AstList** out_declList,
                          AstList** inOut_localsList, AstList** inOut_nonLocalsList,
                          Block* in_enclosingBlock)
{/*>>>*/
  *out_stmtList = *out_declList = 0;
  bool32 success = true;

  AstNode* stmt = 0;
  success = Statement(arena, input, symbolTable,
                      &stmt, inOut_localsList, inOut_nonLocalsList, in_enclosingBlock);
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

    if(stmt->kind == Ast_Return)
      stmt->ret.blockId = symbolTable->currBlockId;

    while(input->token == Token_Semicolon)
      ConsumeToken(input, symbolTable);

    AstList* nextStmtItem = 0, *nextVarItem = 0;
    success = StatementList(arena, input, symbolTable,
                            &nextStmtItem, &nextVarItem,
                            inOut_localsList, inOut_nonLocalsList,
                            in_enclosingBlock);
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

bool32 Block_(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
             BlockKind blockKind, Block** out_block, Block* in_enclosingBlock)
{/*>>>*/
  *out_block = 0;
  bool32 success = true;

  Block* block = PushElement(arena, Block, 1);
  block->blockId = symbolTable->currBlockId;
  block->kind = blockKind;
  block->nestingDepth = symbolTable->nestingDepth;
  block->enclosingBlock = in_enclosingBlock;

  AstList *stmtList = 0,
          *declList = 0,
          *localsList = 0,
          *nonLocalsList = 0;
  success = StatementList(arena, input, symbolTable,
                          &stmtList, &declList, &localsList, &nonLocalsList, block);
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

bool32 BuildAst(MemoryArena* arena, TokenStream* input,
              SymbolTable* symbolTable, AstNode** out_module, Block* in_enclosingBlock)
{/*>>>*/
  *out_module = 0;
  bool32 success = true;

  AstList* procList = 0;
  success = BeginBlock(symbolTable);
  if(success)
  {
    Block* block = PushElement(arena, Block, 1);
    block->kind = Block_Module;
    block->blockId = symbolTable->currBlockId;
    block->nestingDepth = symbolTable->nestingDepth;
    block->enclosingBlock = in_enclosingBlock;

    success = ProcedureList(arena, input, symbolTable, &procList, block);
    if(success)
    {
      block->module.procList = procList;

      AstNode* module = PushElement(arena, AstNode, 1);
      module->kind = Ast_Block;
      module->block = block;

      EndBlock(symbolTable);

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

struct BlockList
{
  Block* block;
  BlockList* nextListItem;
};

bool32 BlockListContainsBlock(BlockList* blockList, Block* block)
{
  BlockList* listItem = blockList;
  while(listItem)
  {
    Block* b = listItem->block;
    if(b == block)
      return true;
    listItem = listItem->nextListItem;
  }
  return false;
}

BlockList* BlockListAddNewBlock(MemoryArena* arena, BlockList* blockList, Block* block)
{
  BlockList* newListItem = PushElement(arena, BlockList, 1);
  newListItem->block = block;
  newListItem->nextListItem = blockList;
  return newListItem;
}

struct IrFrame
{
  int ret;
  int args;
  int accessLinks;
  int machineStatus;
  int locals;
};

bool32 BuildIr(MemoryArena* arena, SymbolTable* symbolTable, AstNode* ast)
{/*>>>*/
  bool32 success = true;

  switch(ast->kind)
  {
    case Ast_Call:
      {
        AstList* actualArgList = ast->call.argList;
        int actualArgCount = 0;
        while(actualArgList)
        {
          actualArgList = actualArgList->nextListItem;
          actualArgCount++;
        }

        Symbol* procSymbol = ast->call.proc;
        if(actualArgCount != procSymbol->proc.argCount)
        {
          Error("Missmatch between actual and formal arguments count : %s(..)", ast->call.name);
          success = false;
        }
      } break;

    case Ast_Block:
      {
        Block* block = ast->block;
        IrActivationRecord* actvRecord = PushElement(arena, IrActivationRecord, 1);
        actvRecord->kind = ActvRecord_Block;

        // Process the declared vars
        AstList* declList = block->declIds;
        IrDataObject* irLocals = 0;
        int localsAreaSize = 0;
        while(declList)
        {
          AstNode* varAst = declList->ast;
          Symbol* varSymbol = varAst->var.symbol;

          assert(varAst->kind == Ast_Var);
          assert(varSymbol->kind == Symbol_DataObj);

          IrDataObject* irLocal = PushElement(arena, IrDataObject, 1);
          irLocal->storageLocation = localsAreaSize;
          localsAreaSize += varSymbol->dataObj.dataSize;

          irLocal->nextObj = irLocals;
          irLocals = irLocal;

          declList = declList->nextListItem;
        }
        actvRecord->localsAreaSize = localsAreaSize;

        // Process the non-local vars
        AstList* nonLocalsList = block->nonLocalIds;
        IrDataObject* irNonLocals = 0;
        int accessLinkCount = 0;
        AccessLink* accessLinks = 0;
        while(nonLocalsList)
        {
          AstNode* idAst = nonLocalsList->ast;
          Symbol* varSymbol = idAst->id.symbol;

          assert(idAst->kind == Ast_Id);
          assert(idAst->id.declBlockOffset > 0);
          assert(varSymbol->kind == Symbol_DataObj);

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

          IrDataObject* irNonLocal = PushElement(arena, IrDataObject, 1);
          irNonLocal->isNonLocal = true;
          irNonLocal->accessLinkIndex = accessLink->index;
          irNonLocal->nextObj = irNonLocals;
          idAst->id.dataObj = irNonLocal;
          irNonLocals = irNonLocal;

          nonLocalsList = nonLocalsList->nextListItem;
        }

        actvRecord->nonLocals = irNonLocals;
        actvRecord->accessLinks = accessLinks;
        actvRecord->accessLinkCount = accessLinkCount;
        block->actvRecord = actvRecord;

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
              IrDataObject* irArgs = 0;
              while(argList)
              {
                AstNode* argAst = argList->ast;
                Symbol* argSymbol = argAst->id.symbol;

                assert(argAst->kind == Ast_Id);
                assert(argSymbol->kind == Symbol_DataObj);

                IrDataObject* irArg = PushElement(arena, IrDataObject, 1);
                // Note that the storage locations for arguments are negative
                irArg->storageLocation = -(argsAreaSize + MACHINE_STATUS_AREA_SIZE);
                argAst->id.dataObj = irArg;
                irArgs = irArg;

                argsAreaSize += argSymbol->dataObj.dataSize;

                argList = argList->nextListItem;
                argsCount++;
              }
              actvRecord->proc.argsAreaSize = argsAreaSize;
              actvRecord->proc.argsCount = argsCount;
              //procSymbol->proc.argsCount = argsCount;

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
        IrDataObject* dataObj = ast->id.dataObj;

        if(dataObj->isNonLocal)
        {
          assert(false);
        } else
        {
          Emit(irProgram, ";begin load l-value of '%s'", symbol->name);
          Emit(irProgram, "push fp");
          Emit(irProgram, "push %d", dataObj->storageLocation);
          Emit(irProgram, "add");
          Emit(irProgram, ";end load");
        }
      } break;

    default:
      assert(false);
  }
}/*<<<*/

void GenCodeRValue(IrProgram* irProgram, Block* block, AstNode* ast)
{/*>>>*/
  switch(ast->kind)
  {
    case Ast_Id:
      {
        Symbol* symbol = ast->id.symbol;
        IrDataObject* dataObj = ast->id.dataObj;

        if(dataObj->isNonLocal)
        {
          assert(false);
        } else
        {
          Emit(irProgram, ";load r-value of '%s'", symbol->name);
          Emit(irProgram, "push fp");
          Emit(irProgram, "push %d", dataObj->storageLocation);
          Emit(irProgram, "add");
          Emit(irProgram, "load");
          Emit(irProgram, ";end load");
        }
      } break;

    case Ast_Expr:
      {
        switch(ast->expr.op)
        {
          case Operator_Call:
            {
              AstNode* callAst = ast->expr.leftOperand;
              Symbol* procSymbol = callAst->call.proc;

              assert(callAst->kind == Ast_Call);
              assert(procSymbol->kind == Symbol_Proc);

              Emit(irProgram, "push 0 ;retval of %s", callAst->call.name);

              Emit(irProgram, ";begin arg-eval");
              AstList* argList = callAst->call.argList;
              while(argList)
              {
                AstNode* argAst = argList->ast;
                GenCodeRValue(irProgram, block, argAst);
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
              GenCodeRValue(irProgram, block, rightSide);

              AstNode* leftSide = ast->expr.leftOperand;
              assert(leftSide->kind == Ast_Id);
              GenCodeLValue(irProgram, leftSide);
              Emit(irProgram, "store ;'%s'", leftSide->id.name);
            } break;

          case Operator_Mul:
          case Operator_Add:
          case Operator_Div:
          case Operator_Sub:
            {
              AstNode* leftOperand = ast->expr.leftOperand;
              GenCodeRValue(irProgram, block, leftOperand);
              AstNode* rightOperand = ast->expr.rightOperand;
              GenCodeRValue(irProgram, block, rightOperand);

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
        GenCodeRValue(irProgram, block, ast);
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

              IrActivationRecord* actvRecord = block->actvRecord;
              int localDataSize = actvRecord->localsAreaSize;
              if(localDataSize > 0)
                Emit(irProgram, "alloc %d ;locals", localDataSize);

              AstList* stmtList = block->stmtList;
              while(stmtList)
              {
                AstNode* stmt = stmtList->ast;
                GenCode(irProgram, symbolTable, block, stmt);

                stmtList = stmtList->nextListItem;
              }

              Emit(irProgram, "return");
            } break;

          case Block_WhileStmt:
            {
              IrActivationRecord* actvRecord = block->actvRecord;

              Emit(irProgram, "; set-up access links");
              AccessLink* accessLink = actvRecord->accessLinks;
              while(accessLink)
              {
                assert(accessLink->actvRecordOffset > 0);
                int offset = accessLink->actvRecordOffset;
                Emit(irProgram, "push fp");
                while(offset--)
                {
                  Emit(irProgram, "push -%d", MACHINE_STATUS_AREA_SIZE); // offset to the fp of actv. record n-1
                  Emit(irProgram, "add");
                  Emit(irProgram, "load");
                }
                accessLink = accessLink->nextLink;
              }

              char* procName = MakeProcLabel(symbolTable, block);
              Emit(irProgram, "call %s", procName); // enter while block
              Emit(irProgram, "label %s", procName);

              char label[64] = {};
              MakeUniqueLabel(symbolTable, label);
              Emit(irProgram, "label %s.while-eval", label);

              // conditional expr
              GenCodeRValue(irProgram, block, block->whileStmt.expr);
              Emit(irProgram, "jumpz %s.while-break", label);

              // body
              int localDataSize = actvRecord->localsAreaSize;
              if(localDataSize > 0)
                Emit(irProgram, "alloc %d ;locals", localDataSize);

              AstList* stmtList = block->stmtList;
              while(stmtList)
              {
                AstNode* stmt = stmtList->ast;
                GenCode(irProgram, symbolTable, block, stmt);

                stmtList = stmtList->nextListItem;
              }

              Emit(irProgram, "goto %s.while-eval", label);
              Emit(irProgram, "label %s.while-break", label);
              Emit(irProgram, "return");
              Emit(irProgram, "pop %d ; discard access links", actvRecord->accessLinkCount);
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
        GenCodeRValue(irProgram, block, ast->ret.expr);

        IrActivationRecord* actvRecord = ast->ret.actvRecord;
        assert(actvRecord->kind == ActvRecord_Proc);
        int retOffset = MACHINE_STATUS_AREA_SIZE + actvRecord->proc.argsAreaSize +
          actvRecord->proc.retAreaSize;

        Emit(irProgram, "push fp");
        Emit(irProgram, "push %d", -retOffset);
        Emit(irProgram, "add");
        Emit(irProgram, "store ;retval");

        Emit(irProgram, "return");
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

  // BuildAst() && BuildIr() ??
  AstNode* moduleAst = 0;
  bool32 success = BuildAst(arena, &tokenStream, &symbolTable, &moduleAst, 0) &&
    BuildIr(arena, &symbolTable, moduleAst);
  if(success)
  {
    StringInit(&irProgram->text, arena);

    assert(moduleAst->kind == Ast_Block);
    Block* block = moduleAst->block;
    GenCode(irProgram, &symbolTable, block, moduleAst);
  }

  assert(symbolTable.currBlockId == 0);
  assert(symbolTable.nestingDepth == 0);

  return success;
}
