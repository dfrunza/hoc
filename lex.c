#include "hocc.h"

extern MemoryArena* arena;

internal Token keyword_list[] = 
{
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
  {TokenKind_Var, "var"},
  {TokenKind_Struct, "struct"},
  {TokenKind_Union, "union"},
  {TokenKind_Enum, "enum"},
  {TokenKind__Null, 0}, /* terminator */
};

internal char unk_char[2] = {0};

internal char* simple_lexeme_tags[] =
{
  "(null)", ".", "->", "[", "]", "(", ")", "{", "}", ";", ":", ",", "%", "*", "/", "\\",
  "+", "++", "-", "--", "!", "!=", "=", "==", ">", ">=", "<", "<=", "&", "&&", "|", "||",
  unk_char,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /* guards */
};

internal Token*
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

internal Token*
get_prev_token(TokenStream* input, int index)
{
  assert(index == 0 || index == 1);
  return &input->prev_tokens[index];
}

internal bool
is_keyword(TokenKind token_kind)
{
  return (token_kind > TokenKind__KeywordBegin) && (token_kind < TokenKind__KeywordEnd);
}

internal char*
install_lexeme(char* begin_char, char* end_char)
{
  assert(end_char >= begin_char);

  /* TODO: Search the lexeme, and if found, then return it. */
  size_t len = end_char - begin_char + 1;
  char* lexeme = mem_push_struct(arena, char, len + 1);
  cstr_copy_substr(lexeme, begin_char, end_char);
  return lexeme;
}

internal bool
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

internal bool
escaped_string(TokenStream* input, EscapedStr* estr, char* file, int line)
{
  bool success = true;
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
        success = compile_error(&input->src_loc, file, line, "Invalid escape char `%c`", c);
    }
    estr->len++;
    c = *(++estr->end);
  }
  if(success)
  {
    if(*estr->end != estr->quote)
      success = compile_error(&input->src_loc, file, line,
                              "Malformed string literal, missing the closing `%c`", estr->quote);
  }
  assert((estr->end - estr->begin) >= 1);
  return success;
}

internal char*
install_escaped_str(EscapedStr* estr)
{
  assert(estr->begin <= estr->end);

  char* lexeme = mem_push_struct(arena, char, estr->len+1); /* +NULL */

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
init_token_stream(TokenStream* token_stream, char* text, char* file_path)
{
  token_stream->text = text;
  token_stream->cursor = token_stream->text;
  SourceLocation* src_loc = &token_stream->src_loc;
  src_loc->line_nr = 1;
  /* TODO: Compute the absolute path to the file, so that Vim could properly
     jump from the QuickFix window to the error line in the file. */
  src_loc->file_path = file_path;
}

Token*
get_next_token(TokenStream* input)
{
  input->prev_tokens[1] = input->prev_tokens[0];
  input->prev_tokens[0] = input->token;
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
    bool is_float = false;
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
      token->float_val = mem_push_struct(arena, float, 1);
      sscanf(digit_buf, "%f", token->float_val);
    }
    else
    {
      token->kind = TokenKind_IntNum;
      token->int_val = mem_push_struct(arena, int, 1);
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
    token->lexeme = simple_lexeme_tags[token->kind];
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
    token->lexeme = simple_lexeme_tags[token->kind];
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
    token->lexeme = simple_lexeme_tags[token->kind];
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
      token->lexeme = simple_lexeme_tags[token->kind];
      ++input->cursor;
    }
  }
  else if(c == '"')
  {
    /* double-quoted escaped string */
    EscapedStr estr = {0};
    estr.quote = '"';

    if(escaped_string(input, &estr, __FILE__, __LINE__))
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

    if(escaped_string(input, &estr, __FILE__, __LINE__))
    {
      char* lexeme = install_escaped_str(&estr);

      if(estr.len != 1)
      {
        compile_error(&input->src_loc, __FILE__, __LINE__,
                      "Invalid char literal '%s'", lexeme);
      }
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
    token->lexeme = simple_lexeme_tags[token->kind];
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
    token->lexeme = simple_lexeme_tags[token->kind];
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
    token->lexeme = simple_lexeme_tags[token->kind];
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
    token->lexeme = simple_lexeme_tags[token->kind];
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
    token->lexeme = simple_lexeme_tags[token->kind];
  }
  else if(c == '*')
  {
    token->kind = TokenKind_Star;
    ++input->cursor;
    token->lexeme = simple_lexeme_tags[token->kind];
  }
  else if(c == '%')
  {
    token->kind = TokenKind_Percent;
    token->lexeme = simple_lexeme_tags[token->kind];
    ++input->cursor;
  }
  else if(c == '\\')
  {
    token->kind = TokenKind_BackSlash;
    token->lexeme = simple_lexeme_tags[token->kind];
    ++input->cursor;
  }
  else if(c == '.')
  {
    token->kind = TokenKind_Dot;
    token->lexeme = simple_lexeme_tags[token->kind];
    ++input->cursor;
  }
  else if(c == '}')
  {
    token->kind = TokenKind_CloseBrace;
    token->lexeme = simple_lexeme_tags[token->kind];
    ++input->cursor;
  }
  else if(c == '{')
  {
    token->kind = TokenKind_OpenBrace;
    token->lexeme = simple_lexeme_tags[token->kind];
    ++input->cursor;
  }
  else if(c == '(')
  {
    token->kind = TokenKind_OpenParens;
    token->lexeme = simple_lexeme_tags[token->kind];
    ++input->cursor;
  }
  else if(c == ')')
  {
    token->kind = TokenKind_CloseParens;
    token->lexeme = simple_lexeme_tags[token->kind];
    ++input->cursor;
  }
  else if(c == ';')
  {
    token->kind = TokenKind_Semicolon;
    token->lexeme = simple_lexeme_tags[token->kind];
    ++input->cursor;
  }
  else if(c == ',')
  {
    token->kind = TokenKind_Comma;
    token->lexeme = simple_lexeme_tags[token->kind];
    ++input->cursor;
  }
  else if(c == ':')
  {
    token->kind = TokenKind_Colon;
    token->lexeme = simple_lexeme_tags[token->kind];
    ++input->cursor;
  }
  else if(c == '[')
  {
    token->kind = TokenKind_OpenBracket;
    token->lexeme = simple_lexeme_tags[token->kind];
    ++input->cursor;
  }
  else if(c == ']')
  {
    token->kind = TokenKind_CloseBracket;
    token->lexeme = simple_lexeme_tags[token->kind];
    ++input->cursor;
  }
  else if(c == '\0')
  {
    token->lexeme = 0;
    token->kind = TokenKind_EndOfInput;
  }
  else
  {
    token->kind = TokenKind_Unknown;
    simple_lexeme_tags[TokenKind_Unknown][0] = c;
    token->lexeme = simple_lexeme_tags[TokenKind_Unknown];
  }
  return &input->token;
}

