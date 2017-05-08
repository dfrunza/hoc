typedef enum TokenKind
{
  TokenKind__Null,
  TokenKind_EndOfInput,

  TokenKind__KeywordBegin,
  TokenKind_If,
  TokenKind_Else,
  TokenKind_While,
  TokenKind_Type,
  TokenKind_Of,
  TokenKind_Array,
  TokenKind_Var,
  TokenKind_Proc,
  TokenKind_Struct,
  TokenKind_Return,
  TokenKind_Break,
  TokenKind_Include,
  TokenKind_True,
  TokenKind_False,
  TokenKind_Print,
  TokenKind__KeywordEnd,

  TokenKind_Id,
  TokenKind_Dot,
  TokenKind_IntNum,
  TokenKind_UpArrow,
  TokenKind_RightArrow,
  TokenKind_Literal,
  TokenKind_OpenBracket,
  TokenKind_CloseBracket,
  TokenKind_Semicolon,
  TokenKind_Colon,
  TokenKind_Comma,
  TokenKind_Percent,
  TokenKind_Star,
  TokenKind_FwdSlash,
  TokenKind_BackSlash,
  TokenKind_Plus,
  TokenKind_PlusPlus,
  TokenKind_Minus,
  TokenKind_MinusMinus,
  TokenKind_UnaryMinus,
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
  TokenKind_Pipe,
  TokenKind_PipePipe,
  TokenKind_OpenParens,
  TokenKind_CloseParens,
  TokenKind_OpenBrace,
  TokenKind_CloseBrace,
  TokenKind_String,
}
TokenKind;

typedef struct
{
  TokenKind kind;
  char* lexeme;

  union
  {
    int* int_val;
    int* float_val;
    char* str;
    Symbol* symbol;
  };
}
Token;

typedef struct TokenStream
{
//  TokenKind prev_token_kind;
//  TokenKind token_kind;
  Token prev_token;
  Token token;
  char* text;
  char* cursor;

  char* file_path;
  int line_nr;
  char* src_line;

  /*
  union {
    int* int_num;
    char* str;
  } lexeme;
  */
}
TokenStream;
