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
  Symbol_Var,
};

struct Symbol
{
  SymbolKind kind;
  char* name;
  int scopeId;
  Symbol* nextSymbol;

  union {
    struct {
      int dataSize;
      int storageLocation; // relative to FP
    } var;

    struct {
      int scopeId;
      int retAreaSize;
      int argAreaSize;
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
  Ast_Id,
  Ast_VarDecl,
  Ast_ProcDecl,
  Ast_Call,
  Ast_Scope,
  Ast_Program,
  Ast_IfStmt,
  Ast_WhileStmt,
  Ast_Return,
};

struct AstNode;

struct AstList
{
  AstNode* ast;
  AstList* nextListItem;
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
    } id;

    struct {
      int32 intNum;
    } literal;

    struct {
      Symbol* symbol;
      char* name;
      AstList* argList;
      AstNode* body;
    } proc;

    struct {
      Symbol* proc;
      char* name;
      AstList* argList;
    } call;

    struct {
      int scopeId;
      AstList* localVars;
      AstList* statements;
      int localsAreaSize;
    } scope;

    struct {
      AstList* procList;
    } program;

    struct {
      int scopeId;
      Symbol* proc;
      AstNode* expr;
    } ret;

    struct {
      AstNode* expr;
      AstNode* body;
      AstNode* bodyElse;
    } ifStmt;

    struct {
      AstNode* expr;
      AstNode* body;
    } whileStmt;
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
  int currScopeId;
  int lastScopeId;
  int currOpenIndex;
  int openedScopes[32];
  MemoryArena* arena;
};

struct ProgramText
{
  String text;
  int textLen;
  char label[32];
  int lastLabelId;
};

struct Translator
{
  MemoryArena* arena;
  TokenStream tokenStream;
  SymbolTable symbolTable;
};

/* old_ip + old_sp + old_fp */
#define CONTROL_AREA_SIZE 3

void SyntaxError(TokenStream* input, char* message, ...)
{
  va_list args;

  fprintf(stderr, "%s(%d) : ", input->filePath, input->lineNr);

  va_start(args, message);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
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
  symbol->scopeId = symbolTable->currScopeId;
  symbol->kind = kind;
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

bool32 OpenScope(SymbolTable* symbolTable)
{/*>>>*/
  int currScopeId = ++symbolTable->lastScopeId;
  symbolTable->currScopeId = currScopeId;

  int currOpenIndex = ++symbolTable->currOpenIndex;
  if(currOpenIndex < SizeofArray(symbolTable->openedScopes))
  {
    symbolTable->openedScopes[currOpenIndex] = currScopeId;
  } else {
    Error("Reached the maximum scope nesting depth");
    return false;
  }

  return true;
}/*<<<*/

void CloseScope(SymbolTable* symbolTable)
{/*>>>*/
  int currOpenIndex = --symbolTable->currOpenIndex;
  int currScopeId = symbolTable->openedScopes[currOpenIndex];
  assert(currScopeId >= 0);
  symbolTable->currScopeId = currScopeId;

  Symbol* symbol = symbolTable->currSymbol;
  while(symbol && symbol->scopeId > symbolTable->currScopeId)
    symbol = symbol->nextSymbol;
  symbolTable->currSymbol = symbol;
}/*<<<*/

char* MakeUniqueLabel(ProgramText* irProgram)
{
  sprintf(irProgram->label, "L%d", irProgram->lastLabelId++);
  return irProgram->label;
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
                  AstNode** out_exprAst);

bool32 ActualArgumentsList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                           AstList* in_tailItem, AstList** out_argList);

bool32 Factor(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
              AstNode** out_factorAst)
{/*>>>*/
  *out_factorAst = 0;
  bool32 success = true;

  if(input->token == Token_OpenParens)
  {
    ConsumeToken(input, symbolTable);
    success = Expression(arena, input, symbolTable, out_factorAst);
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
    idAst->kind = Ast_Id; // tentative
    idAst->id.name = input->lexval.id;

    Symbol* symbol = LookupSymbol(symbolTable, input->lexval.id);
    if(symbol)
    {
      if(symbol->kind != Symbol_Keyword)
      {
        idAst->id.symbol = symbol;
        *out_factorAst = idAst;
        ConsumeToken(input, symbolTable);

        if(symbol->kind == Symbol_Proc)
        {
          // This is a procedure call
          if(input->token == Token_OpenParens)
          {
            ConsumeToken(input, symbolTable);
            AstList* argList = 0;
            success = ActualArgumentsList(arena, input, symbolTable, 0, &argList);
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
      } else {
        SyntaxError(input, "Keyword '%s' used as identifier", input->lexval.id);
        success = false;
      }
    } else {
      SyntaxError(input, "Unknown identifier");
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
                     AstNode* in_leftFactorAst, AstNode** out_factorAst)
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
    success = Factor(arena, input, symbolTable, &factorNode);
    if(success && factorNode)
    {
        astNode->expr.rightOperand = factorNode;
        astNode->expr.leftOperand = in_leftFactorAst;
        success = RestOfFactors(arena, input, symbolTable, astNode, out_factorAst);
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
            AstNode** out_termAst)
{/*>>>*/
  AstNode* factorNode = 0;
  AstNode* exprAst = 0;

  bool32 success = Factor(arena, input, symbolTable, &factorNode);
  if(success && factorNode)
    success = RestOfFactors(arena, input, symbolTable, factorNode, &exprAst);

  *out_termAst = exprAst;
  return success;
}/*<<<*/

bool32 RestOfTerms(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                   AstNode* in_leftTermAst, AstNode** out_termAst)
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
    success = Term(arena, input, symbolTable, &termAst);
    if(success && termAst)
    {
      opAst->expr.rightOperand = termAst;
      opAst->expr.leftOperand = in_leftTermAst;
      return RestOfTerms(arena, input, symbolTable, opAst, out_termAst);
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
                      AstNode** out_termAst)
{/*>>>*/
  AstNode* termAst = 0;
  AstNode* exprAst = 0;

  bool32 success = Term(arena, input, symbolTable, &termAst);
  if(success && termAst)
    success = RestOfTerms(arena, input, symbolTable, termAst, &exprAst);

  *out_termAst = exprAst;
  return success;
}/*<<<*/

bool32 RestOfAssignmentTerms(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                             AstNode* in_leftTermAst, AstNode** out_termAst)
{/*>>>*/
  bool32 success = true;

  if(input->token == Token_Equals)
  {
    ConsumeToken(input, symbolTable);

    AstNode* rightSide = 0;
    success = Expression(arena, input, symbolTable, &rightSide);
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
                  AstNode** out_exprAst)
{/*>>>*/
  AstNode* assgnAst = 0;
  AstNode* exprAst = 0;

  bool32 success = AssignmentTerm(arena, input, symbolTable, &assgnAst);
  if(success && assgnAst)
    success = RestOfAssignmentTerms(arena, input, symbolTable, assgnAst, &exprAst);

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
      varStmt->kind = Ast_VarDecl;

      Symbol* symbol = LookupSymbol(symbolTable, input->lexval.id);
      if(!symbol)
      {
        symbol = AddSymbol(symbolTable, input->lexval.id, Symbol_Var);
        symbol->var.dataSize = 1;
        varStmt->id.symbol = symbol;
        varStmt->id.name = symbol->name;
        ConsumeToken(input, symbolTable);

        *out_varStmt = varStmt;
      } else {
        if(symbol->kind == Symbol_Keyword)
          SyntaxError(input, "Keyword '%s' used as identifier", symbol->name);
        else
          SyntaxError(input, "Name used in a previous declaration", symbol->name);
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
                           AstList* in_prevItem, AstList** out_argList)
{/*>>>*/
  *out_argList = 0;
  bool32 success = true;

  AstNode* varAst = 0;
  success = VarStatement(arena, input, symbolTable, &varAst);
  if(success && varAst)
  {
    AstList* varItem = PushElement(arena, AstList, 1);
    varItem->ast = varAst;

    if(input->token == Token_Comma)
    {
      ConsumeToken(input, symbolTable);
      AstList* nextItem = 0;
      success = FormalArgumentsList(arena, input, symbolTable, 0, &nextItem);

      varItem->nextListItem = nextItem;
    }

    *out_argList = varItem;
  }

  return success;
}/*<<<*/

bool32 ActualArgumentsList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                           AstList* in_prevItem, AstList** out_argList)
{/*>>>*/
  *out_argList = 0;
  bool32 success = true;

  AstNode* argAst = 0;
  success = Expression(arena, input, symbolTable, &argAst);
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
      success = ActualArgumentsList(arena, input, symbolTable, argItem, &nextItem);
      *out_argList = nextItem;
    }
  }
  return success;
}/*<<<*/

bool32 Scope(MemoryArena* arena, TokenStream* input,
             SymbolTable* symbolTable, AstNode** out_scopeAst);

bool32 WhileStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                      AstNode** out_whileAst)
{/*>>>*/
  *out_whileAst = 0;
  bool32 success = true;

  if(input->token == Token_While)
  {
    ConsumeToken(input, symbolTable);

    AstNode* whileAst = PushElement(arena, AstNode, 1);
    whileAst->kind = Ast_WhileStmt;

    AstNode* expr = 0;
    success = Expression(arena, input, symbolTable, &expr);
    if(success)
    {
      whileAst->whileStmt.expr = expr;

      if(input->token == Token_OpenBrace)
      {
        ConsumeToken(input, symbolTable);

        AstNode* scope = 0;
        success = OpenScope(symbolTable) &&
          Scope(arena, input, symbolTable, &scope);
        if(success)
        {
          whileAst->whileStmt.body = scope;

          if(input->token == Token_CloseBrace)
          {
            ConsumeToken(input, symbolTable);
            CloseScope(symbolTable);
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
                   AstNode** out_ifAst)
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

        AstNode* scope = 0;
        success = OpenScope(symbolTable) &&
          Scope(arena, input, symbolTable, &scope);
        if(success)
        {
          ifAst->ifStmt.body = scope;

          if(input->token == Token_CloseBrace)
          {
            ConsumeToken(input, symbolTable);
            CloseScope(symbolTable);
            *out_ifAst = ifAst;

            if(input->token == Token_Else)
            {
              ConsumeToken(input, symbolTable);

              if(input->token == Token_OpenBrace)
              {
                ConsumeToken(input, symbolTable);

                AstNode* scope = 0;
                success = OpenScope(symbolTable) &&
                  Scope(arena, input, symbolTable, &scope);
                if(success)
                {
                  ifAst->ifStmt.bodyElse = scope;

                  if(input->token == Token_CloseBrace)
                  {
                    ConsumeToken(input, symbolTable);
                    CloseScope(symbolTable);
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

bool32 ProcDeclaration(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                       AstNode** out_procAst)
{/*>>>*/
  *out_procAst = 0;
  bool32 success = true;

  if(input->token == Token_Proc)
  {
    ConsumeToken(input, symbolTable);
    if(input->token == Token_Id)
    {
      AstNode* proc = PushElement(arena, AstNode, 1);
      proc->kind = Ast_ProcDecl;

      Symbol* symbol = LookupSymbol(symbolTable, input->lexval.id);
      if(!symbol)
      {
        symbol = AddSymbol(symbolTable, input->lexval.id, Symbol_Proc);
        proc->proc.symbol = symbol;
        proc->proc.name = symbol->name;

        ConsumeToken(input, symbolTable);
        if(input->token == Token_OpenParens)
        {
          ConsumeToken(input, symbolTable);

          // arguments
          AstList *argList = 0;
          success = OpenScope(symbolTable) &&
            FormalArgumentsList(arena, input, symbolTable, 0, &argList);
          if(success)
          {
            proc->proc.argList = argList;

            if(input->token == Token_CloseParens)
            {
              ConsumeToken(input, symbolTable);

              if(input->token == Token_OpenBrace)
              {
                // body
                ConsumeToken(input, symbolTable);

                AstNode* scope = 0;
                success = Scope(arena, input, symbolTable, &scope);
                if(success)
                {
                  proc->proc.body = scope;
                  *out_procAst = proc;

                  if(input->token == Token_CloseBrace)
                  {
                    ConsumeToken(input, symbolTable);
                    CloseScope(symbolTable);
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
          SyntaxError(input, "Name used in a previous declaration", symbol->name);
        success = false;
      }
    } else {
      SyntaxError(input, "Missing identifier");
      success = false;
    }
  }

  return success;
}/*<<<*/

bool32 Program(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
               AstList** out_procList)
{/*>>>*/
  *out_procList = 0;

  AstNode* procAst = 0;
  bool32 success = ProcDeclaration(arena, input, symbolTable, &procAst);
  if(success && procAst)
  {
    AstList* procItem = PushElement(arena, AstList, 1);
    procItem->ast = procAst;

    AstList* nextProcItem = 0;
    success = Program(arena, input, symbolTable, &nextProcItem);
    if(success)
      procItem->nextListItem = nextProcItem;
    *out_procList = procItem;
  }
  return success;
}/*<<<*/

bool32 ReturnStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                       AstNode** out_stmtAst)
{/*>>>*/
  *out_stmtAst = 0;
  bool32 success = true;

  if(input->token == Token_Return)
  {
    ConsumeToken(input, symbolTable);
    
    AstNode* expr = 0;
    success = Expression(arena, input, symbolTable, &expr);
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
                 AstNode** out_stmtAst)
{/*>>>*/
  AstNode* stmtAst = 0;
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

  while(alt) {
    switch(alt) {
      case Alt_Expr:
        {
          success = Expression(arena, input, symbolTable, &stmtAst);
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
          success = IfStatement(arena, input, symbolTable, &stmtAst);
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
          success = WhileStatement(arena, input, symbolTable, &stmtAst);
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
          success = ReturnStatement(arena, input, symbolTable, &stmtAst);
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

bool32 ScopeStatementList(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                          AstList** out_stmtList, AstList** out_varList)
{/*>>>*/
  *out_stmtList = *out_varList = 0;
  bool32 success = true;

  AstNode* stmt = 0;
  success = Statement(arena, input, symbolTable, &stmt);
  if(success && stmt)
  {
    AstList* stmtItem = PushElement(arena, AstList, 1);
    stmtItem->ast = stmt;

    AstList* varItem = 0;
    if(stmt->kind == Ast_VarDecl)
    {
      varItem = stmtItem;
      stmtItem = 0;
    }

    if(stmt->kind == Ast_Return)
      stmt->ret.scopeId = symbolTable->currScopeId;

    while(input->token == Token_Semicolon)
      ConsumeToken(input, symbolTable);

    AstList* nextStmtItem = 0, *nextVarItem = 0;
    success = ScopeStatementList(arena, input, symbolTable, &nextStmtItem, &nextVarItem);
    if(success)
    {
      if(varItem)
      {
        stmtItem = nextStmtItem;
        varItem->nextListItem = nextVarItem;
      }
      else
      {
        varItem = nextVarItem;
        stmtItem->nextListItem = nextStmtItem;
      }
    }
    *out_stmtList = stmtItem;
    *out_varList = varItem;
  }
  return success;
}/*<<<*/

bool32 Scope(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
             AstNode** out_scopeAst)
{/*>>>*/
  *out_scopeAst = 0;
  bool32 success = true;

  AstList* stmtList = 0, *varList = 0;
  success = ScopeStatementList(arena, input, symbolTable, &stmtList, &varList);
  if(success)
  {
    AstNode* scope = PushElement(arena, AstNode, 1);
    scope->kind = Ast_Scope;
    scope->scope.statements = stmtList;
    scope->scope.localVars = varList;
    scope->scope.scopeId = symbolTable->currScopeId;

    *out_scopeAst = scope;
  }

  return success;
}/*<<<*/

bool32 SyntacticAnalysis(MemoryArena* arena, TokenStream* input,
                         SymbolTable* symbolTable, AstNode** out_programAst)
{/*>>>*/
  *out_programAst = 0;
  bool32 success = true;

  AstList* procList = 0;
  success = Program(arena, input, symbolTable, &procList);
  if(success)
  {
    AstNode* programAst = PushElement(arena, AstNode, 1);
    programAst->kind = Ast_Program;
    programAst->program.procList = procList;

    if(input->token == Token_EndOfInput)
      *out_programAst = programAst;
    else {
      SyntaxError(input, "End of file expected");
      success = false;
    }
  }

  return success;
}/*<<<*/

void Emit(ProgramText* irProgram, char* code, ...)
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

bool32 SemanticAnalysis(MemoryArena* arena, SymbolTable* symbolTable, AstNode* ast)
{/*>>>*/
  bool32 success = true;

  switch(ast->kind)
  {
    case Ast_ProcDecl:
      {
        AstNode* scopeAst = ast->proc.body;
        Symbol* procSymbol = ast->proc.symbol;
        procSymbol->proc.retAreaSize = 1;

        AstList* argList = ast->proc.argList;
        int argAreaSize = 0;
        int argCount = 0;
        while(argList)
        {
          AstNode* argAst = argList->ast;
          Symbol* argSymbol = argAst->id.symbol;

          assert(argAst->kind == Ast_VarDecl);
          assert(argSymbol->kind == Symbol_Var);

          // Note that the storage location is a negative value
          argAreaSize += argSymbol->var.dataSize;
          argSymbol->var.storageLocation = -(argAreaSize + CONTROL_AREA_SIZE);

          argList = argList->nextListItem;
          argCount++;
        }
        procSymbol->proc.argAreaSize = argAreaSize;
        procSymbol->proc.argCount = argCount;

        //FIXME Looks like a hack
        if(StrMatch(procSymbol->name, "Main"))
        {
          if(ast->proc.argList)
          {
            Error("Main() must not have arguments");
            success = false;
          }
        }

        if(success)
          success = SemanticAnalysis(arena, symbolTable, scopeAst);
      } break;

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

    case Ast_Scope:
      {
        AstList* varList = ast->scope.localVars;
        int localsAreaSize = 0;
        while(varList)
        {
          AstNode* varAst = varList->ast;
          Symbol* varSymbol = varAst->id.symbol;

          assert(varAst->kind == Ast_VarDecl);
          assert(varSymbol->kind == Symbol_Var);

          varSymbol->var.storageLocation = localsAreaSize;
          localsAreaSize += varSymbol->var.dataSize;

          varList = varList->nextListItem;
        }
        ast->scope.localsAreaSize = localsAreaSize;

        AstList* stmtList = ast->scope.statements;
        while(stmtList && success)
        {
          AstNode* stmtAst = stmtList->ast;
          success = SemanticAnalysis(arena, symbolTable, stmtAst);
          stmtList = stmtList->nextListItem;
        }
      } break;

    case Ast_Return:
      {
        AstNode* exprAst = ast->ret.expr;
        success = SemanticAnalysis(arena, symbolTable, exprAst);
      } break;

    case Ast_Expr:
      {
        AstNode* leftOperand = ast->expr.leftOperand;
        AstNode* rightOperand = ast->expr.rightOperand;
        success = SemanticAnalysis(arena, symbolTable, leftOperand);
        if(rightOperand)
          success &= SemanticAnalysis(arena, symbolTable, rightOperand);
      } break;

    case Ast_IfStmt:
    case Ast_WhileStmt:
    case Ast_Id:
    case Ast_IntNum:
      {} break;

    case Ast_Program:
      {
        AstList* procList = ast->program.procList;
        while(procList && success)
        {
          AstNode* procAst = procList->ast;
          success = SemanticAnalysis(arena, symbolTable, procAst);

          procList = procList->nextListItem;
        }
      } break;

    default:
      assert(false && !"Not implemented");
  }

  return success;
}/*<<<*/

void GenCodeLValue(ProgramText* irProgram, AstNode* ast)
{/*>>>*/
  switch(ast->kind)
  {
    case Ast_Id:
      {
        Symbol* varSymbol = ast->id.symbol;
        Emit(irProgram, ";begin load l-value of '%s'", varSymbol->name);
        Emit(irProgram, "push fp");
        Emit(irProgram, "push %d", varSymbol->var.storageLocation);
        Emit(irProgram, "add");
        Emit(irProgram, ";end load");
      } break;

    default:
      assert(false);
  }
}/*<<<*/

void GenCodeRValue(ProgramText* irProgram, AstNode* ast)
{/*>>>*/
  switch(ast->kind)
  {
    case Ast_Id:
      {
        Symbol* varSymbol = ast->id.symbol;
        Emit(irProgram, ";load r-value of '%s'", varSymbol->name);
        Emit(irProgram, "push fp");
        Emit(irProgram, "push %d", varSymbol->var.storageLocation);
        Emit(irProgram, "add");
        Emit(irProgram, "load");
        Emit(irProgram, ";end load");
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
                GenCodeRValue(irProgram, argAst);
                argList = argList->nextListItem;
              }
              Emit(irProgram, ";end arg-eval");

              Emit(irProgram, "call %s", callAst->call.name);

              int restoreSp = procSymbol->proc.argAreaSize;
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

void GenCode(ProgramText* irProgram, AstNode* ast)
{/*>>>*/
  switch(ast->kind)
  {
    case Ast_Return:
      {
        GenCodeRValue(irProgram, ast->ret.expr);

        Symbol* procSymbol = ast->ret.proc;

        int retOffset = CONTROL_AREA_SIZE + procSymbol->proc.argAreaSize +
          procSymbol->proc.retAreaSize;

        Emit(irProgram, "push fp");
        Emit(irProgram, "push %d", -retOffset);
        Emit(irProgram, "add");
        Emit(irProgram, "store ;retval");

        Emit(irProgram, "goto %s.end", procSymbol->name);
      } break;

    case Ast_IntNum:
    case Ast_Expr:
    case Ast_Id:
      {
        GenCodeRValue(irProgram, ast);
      } break;

    case Ast_Scope:
      {
        int localDataSize = ast->scope.localsAreaSize;
        if(localDataSize > 0)
          Emit(irProgram, "alloc %d ;locals", localDataSize);

        AstList* stmtList = ast->scope.statements;
        while(stmtList)
        {
          AstNode* stmt = stmtList->ast;
          GenCode(irProgram, stmt);

          stmtList = stmtList->nextListItem;
        }
      } break;

    case Ast_ProcDecl:
      {
        Symbol* procSymbol = ast->proc.symbol;
        Emit(irProgram, "label %s", procSymbol->name); // entry point
        AstNode* scopeAst = ast->proc.body;
        GenCode(irProgram, scopeAst);
        Emit(irProgram, "label %s.end", procSymbol->name); // exit point
        Emit(irProgram, "return");
      } break;

    case Ast_Program:
      {
        Emit(irProgram, "push 0 ;main retval");
        Emit(irProgram, "call Main");
        Emit(irProgram, "halt");

        AstList* procList = ast->program.procList;
        while(procList)
        {
          AstNode* procAst = procList->ast;
          GenCode(irProgram, procAst);

          procList = procList->nextListItem;
        }
      } break;

    case Ast_IfStmt:
      {
        Emit(irProgram, ";if-begin");

        // conditional expression
        GenCodeRValue(irProgram, ast->ifStmt.expr);

        char* label = MakeUniqueLabel(irProgram);

        if(ast->ifStmt.bodyElse)
          Emit(irProgram, "jumpz %s.else", label);
        else
          Emit(irProgram, "jumpz %s.if-end", label);

        GenCode(irProgram, ast->ifStmt.body);
        if(ast->ifStmt.bodyElse)
        {
          Emit(irProgram, "goto %s.if-end", label);
          Emit(irProgram, "label %s.else", label);
          GenCode(irProgram, ast->ifStmt.bodyElse);
        }

        Emit(irProgram, "label %s.if-end", label);

      } break;

    case Ast_WhileStmt:
      {
        assert(!"FIXME!!");
      } break;

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

void InitTranslator(Translator* trans, MemoryArena* arena)
{
  trans->arena = arena;
  trans->tokenStream.arena = arena;

  SymbolTable* symbolTable = &trans->symbolTable;
  symbolTable->arena = arena;

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
}

bool32 TranslateHocToIr(Translator* trans, char* filePath, char* hocProgram, ProgramText* irProgram)
{
  TokenStream* tokenStream = &trans->tokenStream;
  tokenStream->text = hocProgram;
  tokenStream->cursor = tokenStream->text;
  tokenStream->lineNr = 1;

  SymbolTable* symbolTable = &trans->symbolTable;

  //TODO: Compute the absolute path to the file, so that Vim could properly
  // jump from the QuickFix window to the error line in the file.
  tokenStream->filePath = filePath;

  ConsumeToken(tokenStream, symbolTable);

  MemoryArena* arena = trans->arena;
  AstNode* ast = 0;
  bool32 success = SyntacticAnalysis(arena, tokenStream, symbolTable, &ast) &&
    SemanticAnalysis(arena, symbolTable, ast);
  if(success)
  {
    StringInit(&irProgram->text, arena);

    assert(ast->kind == Ast_Program);
    GenCode(irProgram, ast);
  }

  return success;
}
