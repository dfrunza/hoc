typedef enum Token_
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
  Token_Var,
  Token_Proc,
  Token_Struct,
  Token_Return,
  Token_Break,
  Token_Include,
  Token_True,
  Token_False,
  Token_Print,
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
  Token_Percent,
  Token_Star,
  Token_FwdSlash,
  Token_BackSlash,
  Token_Plus,
  Token_PlusPlus,
  Token_Minus,
  Token_MinusMinus,
  Token_UnaryMinus,
  Token_Bang,
  Token_Equals,
  Token_EqualsEquals,
  Token_BangEquals,
  Token_AngleRight,
  Token_AngleRightEquals,
  Token_AngleLeft,
  Token_AngleLeftEquals,
  Token_Amprsnd,
  Token_AmprsndAmprsnd,
  Token_Pipe,
  Token_PipePipe,
  Token_OpenParens,
  Token_CloseParens,
  Token_OpenBrace,
  Token_CloseBrace,
  Token_String,
}
Token;

typedef struct TokenStream_
{
  Token prev_token;
  Token token;
  char* text;
  char* cursor;
  MemoryArena* arena;

  char* file_path;
  int line_nr;
  char* src_line;

  union {
    int* int_num;
    char* str;
  } lexeme;
}
TokenStream;

