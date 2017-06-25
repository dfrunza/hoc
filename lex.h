#pragma once
#include "lib.h"

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
  TokenKind_Pointer,
  TokenKind_FwdSlash,
  TokenKind_BackSlash,
  TokenKind_Plus,
  TokenKind_PlusPlus,
  TokenKind_Minus,
  TokenKind_NegativeSign,
  TokenKind_MinusMinus,
  TokenKind_Exclam,
  TokenKind_ExclamEquals,
  TokenKind_Equals,
  TokenKind_EqualsEquals,
  TokenKind_AngleRight,
  TokenKind_AngleRightEquals,
  TokenKind_AngleLeft,
  TokenKind_AngleLeftEquals,
  TokenKind_Ampersand,
  TokenKind_AddressOf,
  TokenKind_AmpersandAmpersand,
  TokenKind_Pipe,
  TokenKind_PipePipe,
  TokenKind_Unknown,

  TokenKind_Id,
  TokenKind_IntNum,
  TokenKind_FloatNum,
  TokenKind_String,
  TokenKind_Char,
  TokenKind_True,
  TokenKind_False,

  TokenKind__KeywordBegin,
  TokenKind_If,
  TokenKind_Else,
  TokenKind_While,
  TokenKind_For,
  TokenKind_Proc,
  TokenKind_Var,
  TokenKind_Struct,
  TokenKind_Return,
  TokenKind_Break,
  TokenKind_Include,
  TokenKind_Cast,
  TokenKind_Enum,
  TokenKind__KeywordEnd,

  TokenKind_EndOfInput,
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
    char char_val;
    char* str;
  };
}
Token;

typedef struct TokenStream
{
  Token prev_tokens[2];
  Token token;
  char* text;
  char* cursor;

  SourceLocation src_loc;
}
TokenStream;

Token* get_next_token(MemoryArena* arena, TokenStream* input);
void token_stream_init(TokenStream* token_stream, char* text, char* file_path);

