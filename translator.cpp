#include "lib.cpp"

enum TokenClass
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
      int storageLocation;
    } var;

    struct {
      int scopeId;
      int returnDataSize;
    } proc;

    struct {
      TokenClass tokenClass;
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
      AstNode* body;
    } proc;

    struct {
      Symbol* symbol;
      char* name;
    } call;

    struct {
      AstList* localVars;
      AstList* statements;
      int storageTop;
    } scope;

    struct {
      AstList* procList;
    } program;

    struct {
      AstNode* expr;
    } ret;
  };
};

struct TokenStream
{
  TokenClass tokenClass;
  char* text;
  char* cursor;
  MemoryArena* arena;
  char* filePath;
  int lineNr;

  union {
    int* intNum;
    char* id;
  } lexval;
};

struct SymbolTable
{
  Symbol* lastSymbol;
  int scopeId;
  int lastScopeId;
  MemoryArena* arena;
};

struct ProgramText
{
  String text;
  int textLen;
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

  int charPos = (int)(input->cursor - input->text);
  fprintf(stderr, "%s(%d) : ", input->filePath, charPos);

  va_start(args, message);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
}

Symbol* LookupSymbol(SymbolTable* symbolTable, char* name)
{/*>>>*/
   Symbol* result = 0;
   Symbol *symbol = symbolTable->lastSymbol;

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
  symbol->scopeId = symbolTable->scopeId;
  symbol->kind = kind;
  symbol->nextSymbol = symbolTable->lastSymbol;
  symbolTable->lastSymbol = symbol;
  return symbol;
}/*<<<*/

Symbol* AddKeyword(SymbolTable* symbolTable, char* name, TokenClass tokenClass)
{
  Symbol* symbol = AddSymbol(symbolTable, name, Symbol_Keyword);
  symbol->kw.tokenClass = tokenClass;
  return symbol;
}

void OpenScope(SymbolTable* symbolTable)
{
  symbolTable->scopeId = ++symbolTable->lastScopeId;
}

void CloseScope(SymbolTable* symbolTable)
{
  symbolTable->scopeId--;
  assert(symbolTable->scopeId >= 0);
  Symbol* symbol = symbolTable->lastSymbol;
  while(symbol && symbol->scopeId > symbolTable->scopeId)
    symbol = symbol->nextSymbol;
  symbolTable->lastSymbol = symbol;
}

bool32 IsKeyword(TokenClass tokenClass)
{
  return tokenClass > Token__KeywordBegin && tokenClass < Token__KeywordEnd;
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
  input->tokenClass = Token__Null;
  input->lexval = {};

  char c = *input->cursor;

  while(c == ' ' || c == '\r' || c == '\n')
    c = *(++input->cursor);

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
      input->tokenClass = symbol->kw.tokenClass;
    else
      input->tokenClass = Token_Id;
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
    input->tokenClass = Token_IntNum;
    input->lexval.intNum = value;
  }
  else if(c == '-')
  {
    c = *(++input->cursor);
    if(c == '>')
    {
      input->tokenClass = Token_RightArrow;
      ++input->cursor;
    }
    else
      input->tokenClass = Token_Minus;
  } 
  else if(c == '*')
  {
    input->tokenClass = Token_Star;
    ++input->cursor;
  }
  else if(c == '.')
  {
    input->tokenClass = Token_Dot;
    ++input->cursor;
  }
  else if(c == '}')
  {
    input->tokenClass = Token_CloseBrace;
    ++input->cursor;
  }
  else if(c == '{')
  {
    input->tokenClass = Token_OpenBrace;
    ++input->cursor;
  }
  else if(c == '=')
  {
    input->tokenClass = Token_Equals;
    ++input->cursor;
  }
  else if(c == '+')
  {
    input->tokenClass = Token_Plus;
    ++input->cursor;
  }
  else if(c == '/')
  {
    input->tokenClass = Token_FwdSlash;
    ++input->cursor;
  }
  else if(c == '(')
  {
    input->tokenClass = Token_OpenParens;
    ++input->cursor;
  }
  else if(c == ')')
  {
    input->tokenClass = Token_CloseParens;
    ++input->cursor;
  }
  else if(c == ';')
  {
    input->tokenClass = Token_Semicolon;
    ++input->cursor;
  }
  else if(c == ',')
  {
    input->tokenClass = Token_Comma;
    ++input->cursor;
  }
  else if(c == ':')
  {
    input->tokenClass = Token_Colon;
    ++input->cursor;
  }
  else if(c == '[')
  {
    input->tokenClass = Token_OpenParens;
    ++input->cursor;
  }
  else if(c == ']')
  {
    input->tokenClass = Token_CloseBracket;
    ++input->cursor;
  }
  else if(c == '^')
  {
    input->tokenClass = Token_UpArrow;
    ++input->cursor;
  }
  else if(c == '\0')
    input->tokenClass = Token_EndOfInput;
}/*<<<*/

bool32 Expression(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                  AstNode** nodeOut);

bool32 Factor(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
              AstNode** out_factor)
{/*>>>*/
  *out_factor = 0;

  if(input->tokenClass == Token_OpenParens)
  {
    ConsumeToken(input, symbolTable);
    if(Expression(arena, input, symbolTable, out_factor))
    {
      if(input->tokenClass == Token_CloseParens)
        ConsumeToken(input, symbolTable);
      else {
        SyntaxError(input, "Missing ')'");
        return false;
      }
    }
  }
  else if(input->tokenClass == Token_IntNum)
  {
    AstNode* numNode = PushElement(arena, AstNode, 1);
    numNode->kind = Ast_IntNum;
    numNode->literal.intNum = *(int32*)input->lexval.intNum;
    *out_factor = numNode;
    ConsumeToken(input, symbolTable);
  }
  else if(input->tokenClass == Token_Id)
  {
    AstNode* idNode = PushElement(arena, AstNode, 1);
    idNode->kind = Ast_Id; // tentative
    idNode->id.name = input->lexval.id;

    Symbol* symbol = LookupSymbol(symbolTable, input->lexval.id);
    if(symbol)
    {
      if(symbol->kind != Symbol_Keyword)
      {
        idNode->id.symbol = symbol;
        *out_factor = idNode;

        ConsumeToken(input, symbolTable);
        if(input->tokenClass == Token_OpenParens)
        {
          // This is a procedure call
          ConsumeToken(input, symbolTable);
          //TODO: Parse the argument list.
          if(input->tokenClass == Token_CloseParens)
          {
            ConsumeToken(input, symbolTable);
            AstNode* exprNode = PushElement(arena, AstNode, 1);
            exprNode->kind = Ast_Expr;
            exprNode->expr.op = Operator_Call;
            exprNode->expr.leftOperand = idNode;
            idNode->kind = Ast_Call;

            *out_factor = exprNode;
          } else {
            SyntaxError(input, "Missing ')'");
            return false;
          }
        }
      } else {
        SyntaxError(input, "Keyword '%s' used as identifier", input->lexval.id);
        return false;
      }
    } else {
      SyntaxError(input, "Unknown identifier");
      return false;
    }
  }

  return true;
}/*<<<*/

bool32 RestOfFactors(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                     AstNode* in_nodeIn, AstNode** out_nodeOut)
{/*>>>*/
  if(input->tokenClass == Token_Star ||
     input->tokenClass == Token_FwdSlash)
  {
    AstNode* opNode = PushElement(arena, AstNode, 1);
    opNode->kind = Ast_Expr;
    if(input->tokenClass == Token_Star)
      opNode->expr.op = Operator_Mul;
    else if(input->tokenClass == Token_FwdSlash)
      opNode->expr.op = Operator_Div;
    else
      assert(false);

    ConsumeToken(input, symbolTable);
    AstNode* factorNode = 0;
    if(Factor(arena, input, symbolTable, &factorNode) && factorNode)
    {
        opNode->expr.rightOperand = factorNode;
        opNode->expr.leftOperand = in_nodeIn;
        return RestOfFactors(arena, input, symbolTable, opNode, out_nodeOut);
    } else {
      SyntaxError(input, "Factor expected");
      return false;
    }
  }
  else
    *out_nodeOut = in_nodeIn;

  return true;
}/*<<<*/

bool32 Term(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
            AstNode** nodeOut)
{/*>>>*/
  AstNode* factorNode = 0;
  AstNode* exprNode = 0;

  bool32 success = Factor(arena, input, symbolTable, &factorNode);
  if(success && factorNode)
    success = RestOfFactors(arena, input, symbolTable, factorNode, &exprNode);

  *nodeOut = exprNode;
  return success;
}/*<<<*/

bool32 RestOfTerms(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                   AstNode* nodeIn, AstNode** nodeOut)
{/*>>>*/
  if(input->tokenClass == Token_Plus ||
     input->tokenClass == Token_Minus)
  {
    AstNode* opNode = PushElement(arena, AstNode, 1);
    opNode->kind = Ast_Expr;
    if(input->tokenClass == Token_Plus)
      opNode->expr.op = Operator_Add;
    else if(input->tokenClass == Token_Minus)
      opNode->expr.op = Operator_Sub;
    else
      assert(false);

    ConsumeToken(input, symbolTable);
    AstNode* termNode = 0;
    if(Term(arena, input, symbolTable, &termNode) && termNode)
    {
        opNode->expr.rightOperand = termNode;
        opNode->expr.leftOperand = nodeIn;
        return RestOfTerms(arena, input, symbolTable, opNode, nodeOut);
    } else {
      SyntaxError(input, "Term expected");
      return false;
    }
  }
  else
   *nodeOut = nodeIn;

  return true;
}/*<<<*/

bool32 AssignmentTerm(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                      AstNode** nodeOut)
{/*>>>*/
  AstNode* termNode = 0;
  AstNode* exprNode = 0;

  bool32 success = Term(arena, input, symbolTable, &termNode);
  if(success && termNode)
    success = RestOfTerms(arena, input, symbolTable, termNode, &exprNode);

  *nodeOut = exprNode;
  return success;
}/*<<<*/

bool32 RestOfAssignmentTerms(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                             AstNode* in_leftSide, AstNode** out_nodeOut)
{/*>>>*/
  if(input->tokenClass == Token_Equals)
  {
    ConsumeToken(input, symbolTable);

    AstNode* rightSide = 0;
    if(Expression(arena, input, symbolTable, &rightSide))
    {
      if(rightSide)
      {
        if(in_leftSide->kind == Ast_Id)
        {
          //TODO: Validate that the left side is an L-value
          AstNode* assgnNode = PushElement(arena, AstNode, 1);
          assgnNode->kind = Ast_Expr;
          assgnNode->expr.op = Operator_Assign;
          assgnNode->expr.leftOperand = in_leftSide;
          assgnNode->expr.rightOperand = rightSide;
          *out_nodeOut = assgnNode;
        } else {
          SyntaxError(input, "Variable expected on the left side of assignment");
          return false;
        }
      } else {
        SyntaxError(input, "Right side of assignment expected");
        return false;
      }
    }
  } else
    *out_nodeOut = in_leftSide;

  return true;
}/*<<<*/

bool32 Expression(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                  AstNode** nodeOut)
{
  AstNode* assgnNode = 0;
  AstNode* exprNode = 0;

  bool32 success = AssignmentTerm(arena, input, symbolTable, &assgnNode);
  if(success && assgnNode)
    success = RestOfAssignmentTerms(arena, input, symbolTable, assgnNode, &exprNode);

  *nodeOut = exprNode;
  return success;
}

bool32 IfStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                   AstNode** stmtOut)
{
  *stmtOut = 0;

  if(input->tokenClass == Token_If)
  {
    //yada yada
  }

  return true;
}

bool32 Scope(MemoryArena* arena, TokenStream* input,
             SymbolTable* symbolTable, AstList** stmtList, AstList** varList);

bool32 ProcDeclaration(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                       AstNode** out_nodeOut)
{/*>>>*/
  *out_nodeOut = 0;

  if(input->tokenClass == Token_Proc)
  {
    ConsumeToken(input, symbolTable);
    if(input->tokenClass == Token_Id)
    {
      AstNode* procNode = PushElement(arena, AstNode, 1);
      procNode->kind = Ast_ProcDecl;

      Symbol* symbol = LookupSymbol(symbolTable, input->lexval.id);
      if(!symbol)
      {
        symbol = AddSymbol(symbolTable, input->lexval.id, Symbol_Proc);
        procNode->proc.symbol = symbol;
        procNode->proc.name = symbol->name;

        ConsumeToken(input, symbolTable);
        if(input->tokenClass == Token_OpenParens)
        {
          OpenScope(symbolTable);
          // arguments
          ConsumeToken(input, symbolTable);
          if(input->tokenClass == Token_CloseParens)
          {
            ConsumeToken(input, symbolTable);

            if(input->tokenClass == Token_OpenBrace)
            {
              // body
              ConsumeToken(input, symbolTable);
              AstList* stmtList = 0, *varList = 0;
              if(Scope(arena, input, symbolTable, &stmtList, &varList))
              {
                AstNode* scopeAst = PushElement(arena, AstNode, 1);
                scopeAst->kind = Ast_Scope;
                scopeAst->scope.statements = stmtList;
                scopeAst->scope.localVars = varList;
                procNode->proc.body = scopeAst;
                *out_nodeOut = procNode;

                if(input->tokenClass == Token_CloseBrace)
                {
                  ConsumeToken(input, symbolTable);
                  CloseScope(symbolTable);
                } else {
                  SyntaxError(input, "Missing '}'");
                  return false;
                }
              }
            } else {
              SyntaxError(input, "Missing '{'");
              return false;
            }
          } else {
            SyntaxError(input, "Missing ')'");
            return false;
          }
        } else {
          SyntaxError(input, "Missing '('");
          return false;
        }
      } else {
        if(symbol->kind == Symbol_Keyword)
          SyntaxError(input, "Keyword '%s' used as identifier", symbol->name);
        else
          SyntaxError(input, "Name used in a previous declaration", symbol->name);
        return false;
      }
    } else {
      SyntaxError(input, "Missing identifier");
      return false;
    }
  }

  return true;
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

bool32 VarStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                        AstNode** stmtOut)
{/*>>>*/
  *stmtOut = 0;

  if(input->tokenClass == Token_Var)
  {
    ConsumeToken(input, symbolTable);
    if(input->tokenClass == Token_Id)
    {
      AstNode* astNode = PushElement(arena, AstNode, 1);
      astNode->kind = Ast_VarDecl;

      Symbol* symbol = LookupSymbol(symbolTable, input->lexval.id);
      if(!symbol)
      {
        symbol = AddSymbol(symbolTable, input->lexval.id, Symbol_Var);
        symbol->var.dataSize = 1;
        astNode->id.symbol = symbol;
        astNode->id.name = symbol->name;
        ConsumeToken(input, symbolTable);

        *stmtOut = astNode;
      } else {
        if(symbol->kind == Symbol_Keyword)
          SyntaxError(input, "Keyword '%s' used as identifier", symbol->name);
        else
          SyntaxError(input, "Name used in a previous declaration", symbol->name);
        return false;
      }
    } else {
      SyntaxError(input, "Missing identifier");
      return false;
    }
  }
  return true;
}/*<<<*/

bool32 ReturnStatement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                       AstNode** out_stmt)
{
  *out_stmt = 0;

  if(input->tokenClass == Token_Return)
  {
    ConsumeToken(input, symbolTable);
    
    AstNode* expr = 0;
    if(Expression(arena, input, symbolTable, &expr) && expr)
    {
      AstNode* retNode = PushElement(arena, AstNode, 1);
      retNode->kind = Ast_Return;
      retNode->ret.expr = expr;
      *out_stmt = retNode;
    } else {
      SyntaxError(input, "Invalid expression after the 'return' keyword");
      return false;
    }
  }

  return true;
}

bool32 Statement(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
                 AstNode** stmtOut)
{/*>>>*/
  AstNode* stmtNode = 0;

  enum Alternative
  {
    Alt__Null,
    Alt_Var,
    Alt_Expr,
    Alt_If,
    Alt_Return,
  };

  Alternative alt = (Alternative)1;

  while(alt) {
    switch(alt) {
      case Alt_Expr:
        {
          if(Expression(arena, input, symbolTable, &stmtNode))
          {
            if(stmtNode)
            {
              alt = Alt__Null;
              if(input->tokenClass == Token_Semicolon)
              {
                ConsumeToken(input, symbolTable);
                if(stmtNode->expr.op == Operator_Assign)
                  stmtNode->expr.isStatement = true;
                else {
                  SyntaxError(input, "Assignment expression required");
                  return false;
                }
              }
              else {
                SyntaxError(input, "Missing ';'");
                return false;
              }
            } else
              alt = (Alternative)((int)alt+1);
          } else
            alt = Alt__Null;
        } break;

      case Alt_If:
        {
          if(IfStatement(arena, input, symbolTable, &stmtNode))
          {
            if(stmtNode)
            {
              alt = Alt__Null;
              if(input->tokenClass == Token_Semicolon)
              {
                SyntaxError(input, "Unexpected ';'");
                return false;
              }
            } else
              alt = (Alternative)((int)alt+1);
          } else
            alt = Alt__Null;
        } break;

      case Alt_Return:
        {
          if(ReturnStatement(arena, input, symbolTable, &stmtNode))
          {
            if(stmtNode)
            {
              alt = Alt__Null;
              if(input->tokenClass == Token_Semicolon)
                ConsumeToken(input, symbolTable);
              else {
                SyntaxError(input, "Missing ';'");
                return false;
              }
            } else
              alt = (Alternative)((int)alt+1);
          } else
            alt = Alt__Null;
        } break;

      case Alt_Var:
        {
          if(VarStatement(arena, input, symbolTable, &stmtNode))
          {
            if(stmtNode)
            {
              alt = Alt__Null;
              if(input->tokenClass == Token_Semicolon)
                ConsumeToken(input, symbolTable);
              else {
                SyntaxError(input, "Missing ';'");
                return false;
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

  if(stmtNode)
    *stmtOut = stmtNode;

  return true;
}/*<<<*/

bool32 Scope(MemoryArena* arena, TokenStream* input, SymbolTable* symbolTable,
             AstList** out_stmtList, AstList** out_varList)
{/*>>>*/
  *out_stmtList = *out_varList = 0;

  AstNode* stmtAst = 0;
  bool32 success = Statement(arena, input, symbolTable, &stmtAst);
  if(success && stmtAst)
  {
    AstList* stmtItem = PushElement(arena, AstList, 1);
    stmtItem->ast = stmtAst;

    AstList* varItem = 0;
    if(stmtAst->kind == Ast_VarDecl)
    {
      varItem = PushElement(arena, AstList, 1);
      varItem->ast = stmtAst;
    }

    while(input->tokenClass == Token_Semicolon)
      ConsumeToken(input, symbolTable);

    AstList* nextStmtItem = 0, *nextVarItem = 0;
    success = Scope(arena, input, symbolTable, &nextStmtItem, &nextVarItem);
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

bool32 SyntacticAnalysis(MemoryArena* arena, TokenStream* input,
                         SymbolTable* symbolTable, AstNode** out_programAst)
{/*>>>*/
  *out_programAst = 0;

  AstList* procList = 0;
  if(Program(arena, input, symbolTable, &procList))
  {
    AstNode* programAst = PushElement(arena, AstNode, 1);
    programAst->kind = Ast_Program;
    programAst->program.procList = procList;

    if(input->tokenClass == Token_EndOfInput)
      *out_programAst = programAst;
    else {
      SyntaxError(input, "End of file expected");
      return false;
    }
  }
  return true;
}/*<<<*/

void Emit(ProgramText* irProgram, char* code, ...)
{
  static char strbuf[128] = {};
  va_list args;

  va_start(args, code);
  irProgram->textLen += vsprintf(strbuf, code, args);
  va_end(args);

  AppendString(&irProgram->text, strbuf);
  AppendString(&irProgram->text, "\n");
  irProgram->textLen++;
}

void SemanticAnalysis(MemoryArena* arena, SymbolTable* symbolTable, AstNode* ast)
{
  switch(ast->kind)
  {
    case Ast_ProcDecl:
      {
        AstNode* scopeAst = ast->proc.body;
        Symbol* procSymbol = ast->proc.symbol;
        procSymbol->proc.returnDataSize = 1;
        SemanticAnalysis(arena, symbolTable, scopeAst);
      } break;

    case Ast_Scope:
      {
        AstList* varList = ast->scope.localVars;
        ast->scope.storageTop = 0;
        while(varList)
        {
          AstNode* varAst = varList->ast;
          Symbol* varSymbol = varAst->id.symbol;
          varSymbol->var.storageLocation = ast->scope.storageTop;
          ast->scope.storageTop += varSymbol->var.dataSize;

          varList = varList->nextListItem;
        }
      } break;

    case Ast_Program:
      {
        AstList* procList = ast->program.procList;
        while(procList)
        {
          AstNode* procAst = procList->ast;
          SemanticAnalysis(arena, symbolTable, procAst);

          procList = procList->nextListItem;
        }
      } break;

    default:
      assert(false);
  }
}

void GenCodeLValue(ProgramText* irProgram, AstNode* ast)
{
  switch(ast->kind)
  {
    case Ast_Id:
      {
        Symbol* varSymbol = ast->id.symbol;
        Emit(irProgram, "; load l-value of '%s'", varSymbol->name);
        Emit(irProgram, "push fp");
        Emit(irProgram, "push %d", varSymbol->var.storageLocation);
        Emit(irProgram, "add");
        Emit(irProgram, "; end load");
      } break;

    default:
      assert(false);
  }
}

void GenCodeRValue(ProgramText* irProgram, AstNode* ast)
{
  switch(ast->kind)
  {
    case Ast_Id:
      {
        Symbol* varSymbol = ast->id.symbol;
        Emit(irProgram, "; load r-value of '%s'", varSymbol->name);
        Emit(irProgram, "push fp");
        Emit(irProgram, "push %d", varSymbol->var.storageLocation);
        Emit(irProgram, "add");
        Emit(irProgram, "load");
        Emit(irProgram, "; end load");
      } break;

    case Ast_Expr:
      {
        switch(ast->expr.op)
        {
          case Operator_Call:
            {
              AstNode* callAst = ast->expr.leftOperand;
              assert(callAst->kind == Ast_Call);

              Emit(irProgram, "push 0 ; retval");
              Emit(irProgram, "alloc 1 ; args");
              Emit(irProgram, "call %s", callAst->call.name);
              Emit(irProgram, "pop ; discard args");
              if(ast->expr.isStatement)
                Emit(irProgram, "pop ; discard retval");
            } break;

          case Operator_Assign:
            {
              AstNode* rightSide = ast->expr.rightOperand;
              GenCodeRValue(irProgram, rightSide);

              AstNode* leftSide = ast->expr.leftOperand;
              assert(leftSide->kind == Ast_Id);
              GenCodeLValue(irProgram, leftSide);
              Emit(irProgram, "store ; '%s'", leftSide->id.name);
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
}

void GenCode(ProgramText* irProgram, AstNode* ast)
{
  switch(ast->kind)
  {
    case Ast_Return:
      {
        GenCodeRValue(irProgram, ast->ret.expr);

        Emit(irProgram, "push fp");
        int retOffset =
          3     /* old_ip + old_sp + old_fp */
          + 1   /* args */
          + 1;  /* ret */
        Emit(irProgram, "push %d", retOffset);
        Emit(irProgram, "sub");
        Emit(irProgram, "store ; retval");
      } break;

    case Ast_IntNum:
    case Ast_Expr:
    case Ast_Id:
      {
        GenCodeRValue(irProgram, ast);
      } break;

    case Ast_Scope:
      {
        int localStorageSize = ast->scope.storageTop;
        Emit(irProgram, "alloc %d ; locals", localStorageSize);
        AstList* stmtList = ast->scope.statements;
        while(stmtList)
        {
          AstNode* stmtAst = stmtList->ast;
          GenCode(irProgram, stmtAst);

          stmtList = stmtList->nextListItem;
        }
      } break;

    case Ast_ProcDecl:
      {
        Symbol* procSymbol = ast->proc.symbol;
        Emit(irProgram, "label %s", procSymbol->name); // entry point
        AstNode* scopeAst = ast->proc.body;
        GenCode(irProgram, scopeAst);
        Emit(irProgram, "return");
      } break;

    case Ast_Program:
      {
        Emit(irProgram, "push 0 ; main retval");
        Emit(irProgram, "alloc 1 ; main args");
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

    default:
      assert(false);
  }
}

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
}

bool32 TranslateHocToIr(Translator* trans, char* filePath, char* hocProgram, ProgramText* irProgram)
{
  TokenStream* tokenStream = &trans->tokenStream;
  tokenStream->text = hocProgram;
  tokenStream->cursor = tokenStream->text;

  SymbolTable* symbolTable = &trans->symbolTable;

  //TODO: Compute the absolute path to the file, so that Vim could properly
  // jump from the QuickFix window to the error line in the file.
  tokenStream->filePath = filePath;

  ConsumeToken(tokenStream, symbolTable);

  MemoryArena* arena = trans->arena;
  AstNode* ast = 0;
  bool32 success = SyntacticAnalysis(arena, tokenStream, symbolTable, &ast);
  if(success)
  {
    SemanticAnalysis(arena, symbolTable, ast);
    StringInit(&irProgram->text, arena);

    assert(ast->kind == Ast_Program);
    GenCode(irProgram, ast);
  }

  return success;
}
