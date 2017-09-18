Token keyword_list[] = 
{
  {Token_Var, "var"},
  {Token_If, "if"},
  {Token_Else, "else"},
  {Token_While, "while"},
  {Token_For, "for"},
  {Token_Return, "return"},
  {Token_Break, "break"},
  {Token_Continue, "continue"},
  {Token_Goto, "goto"},
  {Token_Include, "include"},
  {Token_True, "true"},
  {Token_False, "false"},
  {Token_Proc, "proc"},
  {Token_Struct, "struct"},
  {Token_Union, "union"},
  {Token_Enum, "enum"},
  {Token_Cast, "cast"},
  {Token_New, "new"},
  {Token_Putc, "putc"},
  {Token__None, 0}, /* terminator */
};

Token*
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

bool
is_keyword_token(TokenKind kind)
{
  return (kind >= Token_If) && (kind <= Token_False);
}

bool
is_literal_token(TokenKind kind)
{
  return (kind >= Token_IntNum) && (kind <= Token_Char);
}

char*
install_lexeme(char* begin_char, char* end_char)
{
  assert(end_char >= begin_char);

  /* TODO: Search the lexeme, and if found, then return it. */
  size_t len = end_char - begin_char + 1;
  char* lexeme = mem_push_array_nz(arena, char, len + 1); // +NULL
  cstr_copy_substr(lexeme, begin_char, end_char);
  lexeme[len] = 0; // cap the string
  return lexeme;
}

bool
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

bool
escaped_string(char* file, int line, TokenStream* input, EscapedStr* estr)
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
        success = compile_error_f(file, line, &input->src_loc, "invalid escape char `%c`", c);
    }
    estr->len++;
    c = *(++estr->end);
  }
  if(success)
  {
    if(*estr->end != estr->quote)
      success = compile_error_f(file, line, &input->src_loc, 
                                "malformed string literal, missing the closing `%c`", estr->quote);
  }
  assert((estr->end - estr->begin) >= 1);
  return success;
}

char*
install_escaped_str(EscapedStr* estr)
{
  assert(estr->begin <= estr->end);

  char* lexeme = mem_push_array_nz(arena, char, estr->len+1); /* +NULL */

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
  char* result = 0;

  if(token->kind == Token_Dot)
    result = ".";
  else if(token->kind == Token_ArrowRight)
    result = "->";
  else if(token->kind == Token_OpenBracket)
    result = "[";
  else if(token->kind == Token_CloseBracket)
    result = "]";
  else if(token->kind == Token_OpenParens)
    result = "(";
  else if(token->kind == Token_CloseParens)
    result = ")";
  else if(token->kind == Token_OpenBrace)
    result = "{";
  else if(token->kind == Token_CloseBrace)
    result = "}";
  else if(token->kind == Token_Semicolon)
    result = ";";
  else if(token->kind == Token_Colon)
    result = ":";
  else if(token->kind == Token_Comma)
    result = ",";
  else if(token->kind == Token_Percent)
    result = "%";
  else if(token->kind == Token_Star)
    result = "*";
  else if(token->kind == Token_FwdSlash)
    result = "/";
  else if(token->kind == Token_BackSlash)
    result = "\\";
  else if(token->kind == Token_Plus)
    result = "+";
  else if(token->kind == Token_PlusPlus)
    result = "++";
  else if(token->kind == Token_Minus)
    result = "-";
  else if(token->kind == Token_MinusMinus)
    result = "--";
  else if(token->kind == Token_Exclam)
    result = "!";
  else if(token->kind == Token_ExclamEquals)
    result = "!=";
  else if(token->kind == Token_Equals)
    result = "=";
  else if(token->kind == Token_EqualsEquals)
    result = "==";
  else if(token->kind == Token_AngleRight)
    result = ">";
  else if(token->kind == Token_AngleRightEquals)
    result = ">=";
  else if(token->kind == Token_AngleLeft)
    result = "<";
  else if(token->kind == Token_AngleLeftEquals)
    result = "<=";
  else if(token->kind == Token_Ampersand)
    result = "&";
  else if(token->kind == Token_AmpersandAmpersand)
    result = "&&";
  else if(token->kind == Token_Pipe)
    result = "|";
  else if(token->kind == Token_PipePipe)
    result = "||";
  else if(token->kind == Token_EndOfInput)
    result = "end-of-input";
  else if(token->kind == Token_Var)
    result = "var";
  else if(token->kind == Token_If)
    result = "if";
  else if(token->kind == Token_Else)
    result = "else";
  else if(token->kind == Token_While)
    result = "while";
  else if(token->kind == Token_For)
    result = "for";
  else if(token->kind == Token_Proc)
    result = "proc";
  else if(token->kind == Token_Struct)
    result = "struct";
  else if(token->kind == Token_Union)
    result = "union";
  else if(token->kind == Token_Return)
    result = "return";
  else if(token->kind == Token_Break)
    result = "break";
  else if(token->kind == Token_Continue)
    result = "continue";
  else if(token->kind == Token_Include)
    result = "include";
  else if(token->kind == Token_Enum)
    result = "enum";
  else if(token->kind == Token_Cast)
    result = "cast";
  else if(token->kind == Token_Goto)
    result = "goto";
  else if(token->kind == Token_New)
    result = "new";
  else if(token->kind == Token_Putc)
    result = "putc";
  else if(token->kind == Token_True)
    result = "true";
  else if(token->kind == Token_False)
    result = "false";
  else if(token->kind == Token_Id ||
          token->kind == Token_IntNum ||
          token->kind == Token_FloatNum)
  {
    result = token->lexeme;
  }
  else if(token->kind == Token_String)
    result = token->str; // TODO: Substitute non-printable chars
  else if(token->kind == Token_Char ||
          token->kind == Token_Unknown)
  {
    print_char(result = char_print_buf, token->char_val);
  }
  else
    result = "???";

  return result;
}

void
init_token_stream(TokenStream* stream, char* text, char* file_path)
{
  stream->text = text;
  stream->cursor = stream->text;
  SourceLoc* src_loc = &stream->src_loc;
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

bool
get_next_token(TokenStream* input)
{
  bool success = true;
  *input->prev_state = *input;
  mem_zero_struct(&input->token, Token);
  SourceLoc* src_loc = &input->src_loc;
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

    token->kind = Token_Id;
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
      token->kind = Token_FloatNum;
      token->float_val = mem_push_struct(arena, float);
      sscanf(digit_buf, "%f", token->float_val);
    }
    else
    {
      token->kind = Token_IntNum;
      token->int_val = mem_push_struct(arena, int);
      sscanf(digit_buf, "%d", token->int_val);
    }
  }
  else if(c == '-')
  {
    token->kind = Token_Minus;
    c = *(++input->cursor);
    if(c == '-')
    {
      token->kind = Token_MinusMinus;
      ++input->cursor;
    }
    else if(c == '>')
    {
      token->kind = Token_ArrowRight;
      ++input->cursor;
    }
  }
  else if(c == '<')
  {
    token->kind = Token_AngleLeft;
    c = *(++input->cursor);
    if(c == '=')
    {
      token->kind = Token_AngleLeftEquals;
      ++input->cursor;
    }
  }
  else if(c == '&')
  {
    token->kind = Token_Ampersand;
    c = *(++input->cursor);
    if(c == '&')
    {
      token->kind = Token_AmpersandAmpersand;
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
      token->kind = Token_FwdSlash;
      ++input->cursor;
    }
  }
  else if(c == '"')
  {
    /* double-quoted escaped string */
    EscapedStr estr = {0};
    estr.quote = '"';

    if(success = escaped_string(__FILE__, __LINE__, input, &estr))
    {
      token->str = install_escaped_str(&estr);;
      token->kind = Token_String;
      input->cursor = ++estr.end;
    }
  }
  else if(c == '\'')
  {
    /* single-quoted escaped char */
    EscapedStr estr = {0};
    estr.quote = '\'';

    if(success = escaped_string(__FILE__, __LINE__, input, &estr))
    {
      char* lexeme = install_escaped_str(&estr);

      if(estr.len != 1)
        success = compile_error(&input->src_loc, "invalid char literal '%s'", lexeme);
      else
      {
        token->char_val = *lexeme;
        token->kind = Token_Char;
        input->cursor = ++estr.end;
      }
    }
  }
  else if(c == '=')
  {
    token->kind = Token_Equals;
    c = *(++input->cursor);
    if(c == '=')
    {
      token->kind = Token_EqualsEquals;
      ++input->cursor;
    }
  }
  else if(c == '>')
  {
    token->kind = Token_AngleRight;
    c = *(++input->cursor);
    if(c == '=')
    {
      token->kind = Token_AngleRightEquals;
      ++input->cursor;
    }
  }
  else if(c == '|')
  {
    token->kind = Token_Pipe;
    c = *(++input->cursor);
    if(c == '|')
    {
      token->kind = Token_PipePipe;
      ++input->cursor;
    }
  }
  else if(c == '!')
  {
    token->kind = Token_Exclam;
    c = *(++input->cursor);
    if(c == '=')
    {
      token->kind = Token_ExclamEquals;
      ++input->cursor;
    }
  }
  else if(c == '+')
  {
    token->kind = Token_Plus;
    c = *(++input->cursor);
    if(c == '+')
    {
      token->kind = Token_PlusPlus;
      ++input->cursor;
    }
  }
  else if(c == '*')
  {
    token->kind = Token_Star;
    ++input->cursor;
  }
  else if(c == '%')
  {
    token->kind = Token_Percent;
    ++input->cursor;
  }
  else if(c == '\\')
  {
    token->kind = Token_BackSlash;
    ++input->cursor;
  }
  else if(c == '.')
  {
    token->kind = Token_Dot;
    ++input->cursor;
  }
  else if(c == '}')
  {
    token->kind = Token_CloseBrace;
    ++input->cursor;
  }
  else if(c == '{')
  {
    token->kind = Token_OpenBrace;
    ++input->cursor;
  }
  else if(c == '(')
  {
    token->kind = Token_OpenParens;
    ++input->cursor;
  }
  else if(c == ')')
  {
    token->kind = Token_CloseParens;
    ++input->cursor;
  }
  else if(c == ';')
  {
    token->kind = Token_Semicolon;
    ++input->cursor;
  }
  else if(c == ',')
  {
    token->kind = Token_Comma;
    ++input->cursor;
  }
  else if(c == ':')
  {
    token->kind = Token_Colon;
    ++input->cursor;
  }
  else if(c == '[')
  {
    token->kind = Token_OpenBracket;
    ++input->cursor;
  }
  else if(c == ']')
  {
    token->kind = Token_CloseBracket;
    ++input->cursor;
  }
  else if(c == '\0')
  {
    token->kind = Token_EndOfInput;
  }
  else
  {
    token->kind = Token_Unknown;
    token->char_val = c;
  }
  return success;
}

