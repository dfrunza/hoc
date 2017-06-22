#pragma once

typedef enum TokenKind
{
  TokenKind__Null,
  // 'Simple' tokens must be listed at the beginning of the enum
  TokenKind_Dot,
  TokenKind_OpenBracket,
  TokenKind_CloseBracket,
  TokenKind_OpenParens,
  TokenKind_CloseParens,
  TokenKind_OpenBrace,
  TokenKind_CloseBrace,
  TokenKind_Semicolon,
  TokenKind_Colon,
  TokenKind_Comma,
  TokenKind_Percent,
  TokenKind_Star,
  TokenKind_PtrDeref,
  TokenKind_FwdSlash,
  TokenKind_BackSlash,
  TokenKind_Plus,
  TokenKind_PlusPlus,
  TokenKind_Minus,
  TokenKind_UnaryMinus,
  TokenKind_MinusMinus,
  TokenKind_Bang,
  TokenKind_Equals,
  TokenKind_EqualsEquals,
  TokenKind_BangEquals,
  TokenKind_AngleRight,
  TokenKind_AngleRightEquals,
  TokenKind_AngleLeft,
  TokenKind_AngleLeftEquals,
  TokenKind_Amprsnd,
  TokenKind_AmprsndAmprsnd,
  TokenKind_AddressOf,
  TokenKind_Pipe,
  TokenKind_PipePipe,

  TokenKind_Id,
  TokenKind_IntNum,
  TokenKind_FloatNum,
  TokenKind_String,

  TokenKind_EndOfInput,

  TokenKind__KeywordBegin,
  TokenKind_If,
  TokenKind_Else,
  TokenKind_While,
  TokenKind_Proc,
  TokenKind_Var,
  TokenKind_Struct,
  TokenKind_Return,
  TokenKind_Break,
  TokenKind_Include,
  TokenKind_True,
  TokenKind_False,
  TokenKind_Print,
  TokenKind_Cast,
  TokenKind__KeywordEnd,
}
TokenKind;

typedef struct
{
  TokenKind kind;
  char* lexeme;

  union
  {
    int* int_val;
    float* float_val;
    char* str;
  };
}
Token;

typedef struct
{
  char* file_path;
  int line_nr;
  char* src_line;
}
SourceLocation;

typedef struct TokenStream
{
  Token prev_tokens[2];
  Token token;
  char* text;
  char* cursor;

  SourceLocation src_loc;
}
TokenStream;

internal Token keyword_list[] = 
{
  {TokenKind_If, "if"},
  {TokenKind_Else, "else"},
  {TokenKind_While, "while"},
  {TokenKind_Return, "return"},
  {TokenKind_Break, "break"},
  {TokenKind_Include, "include"},
  {TokenKind_True, "true"},
  {TokenKind_False, "false"},
  {TokenKind_Cast, "cast"},
  {TokenKind_Proc, "proc"},
  {TokenKind_Var, "var"},
  {TokenKind__Null, 0}, // terminator
};

internal char* simple_lexeme_list[] =
{
  "(null)", ".", "[", "]", "(", ")", "{", "}", ";", ":", ",", "%", "*", "*", "/", "\\",
  "+", "++", "-", "-", "--", "!", "=", "==", "!=", ">", ">=", "<", "<=", "&", "&", "&&", "|", "||", 
};

