#include "hocc.h"

extern MemoryArena* arena;

local Token keyword_list[] = 
{
  {TokenKind_Var, "var"},
  {TokenKind_If, "if"},
  {TokenKind_Else, "else"},
  {TokenKind_While, "while"},
  {TokenKind_For, "for"},
  {TokenKind_Return, "return"},
  {TokenKind_Break, "break"},
  {TokenKind_Continue, "continue"},
  {TokenKind_Goto, "goto"},
  {TokenKind_Include, "include"},
  {TokenKind_True, "true"},
  {TokenKind_False, "false"},
  {TokenKind_Proc, "proc"},
  {TokenKind_Struct, "struct"},
  {TokenKind_Union, "union"},
  {TokenKind_Enum, "enum"},
  {TokenKind_Cast, "cast"},
  {TokenKind__Null, 0}, /* terminator */
};

local Token*
lookup_keyword(Token* list, char* lexeme)
{
  Token* result = 0;
  Token* token;

  for(int i = 0;
      (token = &list[i])->kind;
      token = &list[++i])
  {
    if(cstr_match(lexeme, token->lexeme))
    {
      result = token;
      break;
    }
  }
  return result;
}

bool32
is_keyword_token(TokenKind kind)
{
  return (kind >= TokenKind_If) && (kind <= TokenKind_False);
}

bool32
is_literal_token(TokenKind kind)
{
  return (kind >= TokenKind_IntNum) && (kind <= TokenKind_Char);
}

local char*
install_lexeme(char* begin_char, char* end_char)
{
  assert(end_char >= begin_char);

  /* TODO: Search the lexeme, and if found, then return it. */
  size_t len = end_char - begin_char + 1;
  char* lexeme = mem_push_count_nz(arena, char, len + 1); // +NULL
  cstr_copy_substr(lexeme, begin_char, end_char);
  lexeme[len] = 0; // cap the string
  return lexeme;
}

local bool32
is_valid_escape_char(char c)
{
  return c == 't' || c == 'n' || c == 'r' || c == '0' ||
    c == '\"' || c == '\'' || c == '\\';
}

typedef struct
{
  char quote;
  int len;
  char* begin;
  char* end;
}
EscapedStr;

local bool32
escaped_string(TokenStream* input, EscapedStr* estr, char* file, int line)
{
  bool32 success = true;
  estr->len = 0;
  estr->end = input->cursor;
  estr->begin = input->cursor;

  /* find the closing `"` and count the length of the escaped string at the same time */
  char c = *(++estr->end);
  while(success && (c != estr->quote) && (c != '\0'))
  {
    if(c == '\\')
    {
      c = *(++estr->end);
      if(!is_valid_escape_char(c))
        success = compile_error_f(&input->src_loc, file, line, "Invalid escape char `%c`", c);
    }
    estr->len++;
    c = *(++estr->end);
  }
  if(success)
  {
    if(*estr->end != estr->quote)
      success = compile_error_f(&input->src_loc, file, line,
                                "Malformed string literal, missing the closing `%c`", estr->quote);
  }
  assert((estr->end - estr->begin) >= 1);
  return success;
}

local char*
install_escaped_str(EscapedStr* estr)
{
  assert(estr->begin <= estr->end);

  char* lexeme = mem_push_count_nz(arena, char, estr->len+1); /* +NULL */

  if(estr->len > 0)
  {
    assert((estr->end - estr->begin) >= 2);
    char* dest_str = lexeme;
    char* src_str = estr->begin+1;
    for(int i = 0; i < estr->len; i++)
    {
      *dest_str = *src_str;
      if(*dest_str == '\\')
      {
        src_str++;
        assert(*src_str != '\0');

        if(*src_str == 't')
          *dest_str = '\t';
        else if(*src_str == 'n')
          *dest_str = '\n';
        else if(*src_str == 'r')
          *dest_str = '\r';
        else if(*src_str == '0')
          *dest_str = '\0';
        else if(*src_str == '"')
          *dest_str = '"';
        else if(*src_str == '\\')
          *dest_str = '\\';
        else if(*src_str == '\'')
          *dest_str = '\'';
        else
          assert(false);
      }
      src_str++;
      dest_str++;
    }
  }
  lexeme[estr->len] = '\0';

  return lexeme;
}

void
print_char(char buf[3], char raw_char)
{
  if(raw_char == '\0')
    cstr_copy(buf, "\\0");
  else if(raw_char == '\t')
    cstr_copy(buf, "\\t");
  else if(raw_char == '\n')
    cstr_copy(buf, "\\n");
  else if(raw_char == '\r')
    cstr_copy(buf, "\\r");
  else if(raw_char == '\'')
    cstr_copy(buf, "\\'");
  else
    *buf = raw_char;
}

char*
get_token_printstr(Token* token)
{
  static char char_print_buf[3] = {0};
  char* result = "???";

  if(token->kind == TokenKind__Null)
    result = "(null)";
  else if(token->kind == TokenKind_Dot)
    result = ".";
  else if(token->kind == TokenKind_ArrowRight)
    result = "->";
  else if(token->kind == TokenKind_OpenBracket)
    result = "[";
  else if(token->kind == TokenKind_CloseBracket)
    result = "]";
  else if(token->kind == TokenKind_OpenParens)
    result = "(";
  else if(token->kind == TokenKind_CloseParens)
    result = ")";
  else if(token->kind == TokenKind_OpenBrace)
    result = "{";
  else if(token->kind == TokenKind_CloseBrace)
    result = "}";
  else if(token->kind == TokenKind_Semicolon)
    result = ";";
  else if(token->kind == TokenKind_Colon)
    result = ":";
  else if(token->kind == TokenKind_Comma)
    result = ",";
  else if(token->kind == TokenKind_Percent)
    result = "%";
  else if(token->kind == TokenKind_Star)
    result = "*";
  else if(token->kind == TokenKind_FwdSlash)
    result = "/";
  else if(token->kind == TokenKind_BackSlash)
    result = "\\";
  else if(token->kind == TokenKind_Plus)
    result = "+";
  else if(token->kind == TokenKind_PlusPlus)
    result = "++";
  else if(token->kind == TokenKind_Minus)
    result = "-";
  else if(token->kind == TokenKind_MinusMinus)
    result = "--";
  else if(token->kind == TokenKind_Exclam)
    result = "!";
  else if(token->kind == TokenKind_ExclamEquals)
    result = "!=";
  else if(token->kind == TokenKind_Equals)
    result = "=";
  else if(token->kind == TokenKind_EqualsEquals)
    result = "==";
  else if(token->kind == TokenKind_AngleRight)
    result = ">";
  else if(token->kind == TokenKind_AngleRightEquals)
    result = ">=";
  else if(token->kind == TokenKind_AngleLeft)
    result = "<";
  else if(token->kind == TokenKind_AngleLeftEquals)
    result = "<=";
  else if(token->kind == TokenKind_Ampersand)
    result = "&";
  else if(token->kind == TokenKind_AmpersandAmpersand)
    result = "&&";
  else if(token->kind == TokenKind_Pipe)
    result = "|";
  else if(token->kind == TokenKind_PipePipe)
    result = "||";
  else if(token->kind == TokenKind_EndOfInput)
    result = "end-of-file";
  else if(token->kind == TokenKind_Var)
    result = "var";
  else if(token->kind == TokenKind_If)
    result = "if";
  else if(token->kind == TokenKind_Else)
    result = "else";
  else if(token->kind == TokenKind_While)
    result = "while";
  else if(token->kind == TokenKind_For)
    result = "for";
  else if(token->kind == TokenKind_Proc)
    result = "proc";
  else if(token->kind == TokenKind_Struct)
    result = "struct";
  else if(token->kind == TokenKind_Union)
    result = "union";
  else if(token->kind == TokenKind_Return)
    result = "return";
  else if(token->kind == TokenKind_Break)
    result = "break";
  else if(token->kind == TokenKind_Continue)
    result = "continue";
  else if(token->kind == TokenKind_Include)
    result = "include";
  else if(token->kind == TokenKind_Enum)
    result = "enum";
  else if(token->kind == TokenKind_Cast)
    result = "cast";
  else if(token->kind == TokenKind_True)
    result = "true";
  else if(token->kind == TokenKind_False)
    result = "false";
  else if(token->kind == TokenKind_Id ||
          token->kind == TokenKind_IntNum ||
          token->kind == TokenKind_FloatNum)
    result = token->lexeme;
  else if(token->kind == TokenKind_String)
    result = token->str; // TODO: Substitute non-printable chars
  else if(token->kind == TokenKind_Char ||
          token->kind == TokenKind_Unknown)
    print_char(result = char_print_buf, token->char_val);

  return result;
}

void
init_token_stream(TokenStream* stream, char* text, char* file_path)
{
  stream->text = text;
  stream->cursor = stream->text;
  SourceLocation* src_loc = &stream->src_loc;
  src_loc->line_nr = 1;
  /* TODO: Compute the absolute path to the file, so that Vim could properly
     jump from the QuickFix window to the error line in the file. */
  src_loc->file_path = file_path;
  stream->prev_state = mem_push_struct(arena, TokenStream);
}

void
putback_token(TokenStream* input)
{
  *input = *input->prev_state;
}

Token*
get_prev_token(TokenStream* input)
{
  Token* token = &input->token;
  if(input->prev_state)
    token = &input->prev_state->token;
  return token;
}

bool32
get_next_token(TokenStream* input)
{
  bool32 success = true;
  *input->prev_state = *input;
  mem_zero_struct(&input->token, Token);
  SourceLocation* src_loc = &input->src_loc;
  src_loc->src_line = input->cursor;
  char c;

  Token* token = &input->token;
loop:
  c = *input->cursor;
  while(c == ' ' || c == '\t' ||
        c == '\r' || c == '\n')
  {
    if(c == '\n')
    {
      src_loc->line_nr++;
      src_loc->src_line = input->cursor;
    }
    c = *(++input->cursor);
  }

  if(char_is_letter(c) || c == '_')
  {
    char* begin_char = input->cursor;
    c = *(++input->cursor);

    while(char_is_letter(c) || char_is_numeric(c) || c == '_')
      c = *(++input->cursor);

    char* end_char = input->cursor - 1;
    char* lexeme = install_lexeme(begin_char, end_char);

    token->kind = TokenKind_Id;
    token->lexeme = lexeme;
    Token* keyword = lookup_keyword(keyword_list, lexeme);
    if(keyword)
      token->kind = keyword->kind;
  }
  else if(char_is_numeric(c))
  {
    char digit_buf[32] = {0};
    bool32 is_float = false;
    int i = 0;
    for(; i < sizeof_array(digit_buf)-1 && (char_is_numeric(c) || c == '.'); i++)
    {
      digit_buf[i] = c;
      if(c == '.')
      {
        if(is_float)
          break;
        is_float = true;
      }
      c = *(++input->cursor);
    }
    digit_buf[i] = '\0';
    token->lexeme = install_lexeme(digit_buf, digit_buf + i-1);

    if(is_float)
    {
      token->kind = TokenKind_FloatNum;
      token->float_val = mem_push_struct(arena, float);
      sscanf(digit_buf, "%f", token->float_val);
    }
    else
    {
      token->kind = TokenKind_IntNum;
      token->int_val = mem_push_struct(arena, int);
      sscanf(digit_buf, "%d", token->int_val);
    }
  }
  else if(c == '-')
  {
    token->kind = TokenKind_Minus;
    c = *(++input->cursor);
    if(c == '-')
    {
      token->kind = TokenKind_MinusMinus;
      ++input->cursor;
    }
    else if(c == '>')
    {
      token->kind = TokenKind_ArrowRight;
      ++input->cursor;
    }
  }
  else if(c == '<')
  {
    token->kind = TokenKind_AngleLeft;
    c = *(++input->cursor);
    if(c == '=')
    {
      token->kind = TokenKind_AngleLeftEquals;
      ++input->cursor;
    }
  }
  else if(c == '&')
  {
    token->kind = TokenKind_Ampersand;
    c = *(++input->cursor);
    if(c == '&')
    {
      token->kind = TokenKind_AmpersandAmpersand;
      ++input->cursor;
    }
  }
  else if(c == '/')
  {
    /* multi-line comments */
    char* fwd_cursor = input->cursor;

    c = *(++fwd_cursor);
    if(c == '*')
    {
      c = *(++fwd_cursor);

      while(true)
      {
        while(c != '*' && c != '\0')
        {
          if(c == '\n')
          {
            src_loc->line_nr++;
            src_loc->src_line = input->cursor;
          }
          c = *(++fwd_cursor);
        }
        if(c == '*')
        {
          c = *(++fwd_cursor);
          if(c == '/')
            break;
        } else if(c == '\0')
          break;
      }
      input->cursor = ++fwd_cursor;
      goto loop;
    }
    else
    {
      token->kind = TokenKind_FwdSlash;
      ++input->cursor;
    }
  }
  else if(c == '"')
  {
    /* double-quoted escaped string */
    EscapedStr estr = {0};
    estr.quote = '"';

    if(success = escaped_string(input, &estr, __FILE__, __LINE__))
    {
      token->str = install_escaped_str(&estr);;
      token->kind = TokenKind_String;
      input->cursor = ++estr.end;
    }
  }
  else if(c == '\'')
  {
    /* single-quoted escaped char */
    EscapedStr estr = {0};
    estr.quote = '\'';

    if(success = escaped_string(input, &estr, __FILE__, __LINE__))
    {
      char* lexeme = install_escaped_str(&estr);

      if(estr.len != 1)
        success = compile_error(&input->src_loc, "Invalid char literal '%s'", lexeme);
      else
      {
        token->char_val = *lexeme;
        token->kind = TokenKind_Char;
        input->cursor = ++estr.end;
      }
    }
  }
  else if(c == '=')
  {
    token->kind = TokenKind_Equals;
    c = *(++input->cursor);
    if(c == '=')
    {
      token->kind = TokenKind_EqualsEquals;
      ++input->cursor;
    }
  }
  else if(c == '>')
  {
    token->kind = TokenKind_AngleRight;
    c = *(++input->cursor);
    if(c == '=')
    {
      token->kind = TokenKind_AngleRightEquals;
      ++input->cursor;
    }
  }
  else if(c == '|')
  {
    token->kind = TokenKind_Pipe;
    c = *(++input->cursor);
    if(c == '|')
    {
      token->kind = TokenKind_PipePipe;
      ++input->cursor;
    }
  }
  else if(c == '!')
  {
    token->kind = TokenKind_Exclam;
    c = *(++input->cursor);
    if(c == '=')
    {
      token->kind = TokenKind_ExclamEquals;
      ++input->cursor;
    }
  }
  else if(c == '+')
  {
    token->kind = TokenKind_Plus;
    c = *(++input->cursor);
    if(c == '+')
    {
      token->kind = TokenKind_PlusPlus;
      ++input->cursor;
    }
  }
  else if(c == '*')
  {
    token->kind = TokenKind_Star;
    ++input->cursor;
  }
  else if(c == '%')
  {
    token->kind = TokenKind_Percent;
    ++input->cursor;
  }
  else if(c == '\\')
  {
    token->kind = TokenKind_BackSlash;
    ++input->cursor;
  }
  else if(c == '.')
  {
    token->kind = TokenKind_Dot;
    ++input->cursor;
  }
  else if(c == '}')
  {
    token->kind = TokenKind_CloseBrace;
    ++input->cursor;
  }
  else if(c == '{')
  {
    token->kind = TokenKind_OpenBrace;
    ++input->cursor;
  }
  else if(c == '(')
  {
    token->kind = TokenKind_OpenParens;
    ++input->cursor;
  }
  else if(c == ')')
  {
    token->kind = TokenKind_CloseParens;
    ++input->cursor;
  }
  else if(c == ';')
  {
    token->kind = TokenKind_Semicolon;
    ++input->cursor;
  }
  else if(c == ',')
  {
    token->kind = TokenKind_Comma;
    ++input->cursor;
  }
  else if(c == ':')
  {
    token->kind = TokenKind_Colon;
    ++input->cursor;
  }
  else if(c == '[')
  {
    token->kind = TokenKind_OpenBracket;
    ++input->cursor;
  }
  else if(c == ']')
  {
    token->kind = TokenKind_CloseBracket;
    ++input->cursor;
  }
  else if(c == '\0')
  {
    token->kind = TokenKind_EndOfInput;
  }
  else
  {
    token->kind = TokenKind_Unknown;
    token->char_val = c;
  }
  return success;
}

