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

Token*
get_prev_token(TokenStream* input, int index)
{
  assert(index == 0 || index == 1);
  return &input->prev_tokens[index];
}

bool32
token_is_keyword(TokenKind token_kind)
{
  return (token_kind > TokenKind__KeywordBegin) && (token_kind < TokenKind__KeywordEnd);
}

char*
lexeme_install_id(MemoryArena* arena, char* begin_char, char* end_char)
{
  assert(end_char >= begin_char);

  //TODO: Search the lexeme, and if found, then return it.
  size_t len = end_char - begin_char + 1;
  char* lexeme = mem_push_struct(arena, char, len + 1);
  cstr_copy_substr(lexeme, begin_char, end_char);
  return lexeme;
}

char*
lexeme_install_dquot_str(MemoryArena* arena, char* begin_char, char* end_char)
{
  assert(end_char >= begin_char);
  assert(*begin_char == '"' && *end_char == '"');

  size_t len = (end_char - begin_char + 1) - 2; // minus the quotes
  char* lexeme = mem_push_struct(arena, char, len + 1); // +NULL

  char* dest_str = lexeme;
  char* src_str = begin_char+1;
  for(uint i = 0; i < len; i++)
  {
    *dest_str++ = *src_str++;
  }
  cstr_copy_substr(lexeme, begin_char+1, end_char-1);
  return lexeme;
}

void
token_stream_init(TokenStream* token_stream, char* text, char* file_path)
{
  token_stream->text = text;
  token_stream->cursor = token_stream->text;
  SourceLocation* src_loc = &token_stream->src_loc;
  src_loc->line_nr = 1;
  //TODO: Compute the absolute path to the file, so that Vim could properly
  // jump from the QuickFix window to the error line in the file.
  src_loc->file_path = file_path;
}

bool32
is_unary_leading_token(TokenKind token_kind)
{
  return token_kind == TokenKind_Equals ||
      token_kind == TokenKind_OpenParens ||
      token_kind == TokenKind_OpenBracket ||
      token_kind == TokenKind_OpenBrace ||
      token_kind == TokenKind_Star ||
      token_kind == TokenKind_Plus ||
      token_kind == TokenKind_Comma ||
      token_kind == TokenKind_FwdSlash ||
      token_kind == TokenKind_Return ||
      token_kind == TokenKind_PtrDeref;
}

void
get_next_token(MemoryArena* arena, TokenStream* input)
{
  input->prev_tokens[1] = input->prev_tokens[0];
  input->prev_tokens[0] = input->token;
  mem_zero(&input->token);
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
    char* lexeme = lexeme_install_id(arena, begin_char, end_char);

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
    if(is_unary_leading_token(get_prev_token(input, 0)->kind))
    {
       token->kind = TokenKind_UnaryMinus;
    }
    else if(c == '-')
    {
      token->kind = TokenKind_MinusMinus;
      ++input->cursor;
    }
    token->lexeme = simple_lexeme_list[token->kind];
  }
  else if(c == '*')
  {
    token->kind = TokenKind_Star;
    ++input->cursor;
    if(is_unary_leading_token(get_prev_token(input, 0)->kind))
    {
      token->kind = TokenKind_PtrDeref;
    }
    token->lexeme = simple_lexeme_list[token->kind];
  }
  else if(c == '&')
  {
    token->kind = TokenKind_Amprsnd;
    c = *(++input->cursor);
    if(is_unary_leading_token(get_prev_token(input, 0)->kind))
    {
      token->kind = TokenKind_AddressOf;
    }
    else if(c == '&')
    {
      token->kind = TokenKind_AmprsndAmprsnd;
      ++input->cursor;
    }
    token->lexeme = simple_lexeme_list[token->kind];
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
      token->lexeme = simple_lexeme_list[token->kind];
      ++input->cursor;
    }
  }
  else if(c == '"')
  {
    /* double-quoted (unescaped) strings */
    char* fwd_cursor = input->cursor;

    c = *(++fwd_cursor);
    while(c != '"' && c != '\0')
      c = *(++fwd_cursor);

    if(c == '"')
    {
      char* lexeme = lexeme_install_dquot_str(arena, input->cursor, fwd_cursor);
      token->str = lexeme;
      token->kind = TokenKind_String;
      input->cursor = ++fwd_cursor;
    }
    else
      compile_error(&input->src_loc, "Missing closing '\"'\n");
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
    token->lexeme = simple_lexeme_list[token->kind];
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
    token->lexeme = simple_lexeme_list[token->kind];
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
    token->lexeme = simple_lexeme_list[token->kind];
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
    token->lexeme = simple_lexeme_list[token->kind];
  }
  else if(c == '!')
  {
    token->kind = TokenKind_Bang;
    c = *(++input->cursor);
    if(c == '=')
    {
      token->kind = TokenKind_BangEquals;
      ++input->cursor;
    }
    token->lexeme = simple_lexeme_list[token->kind];
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
    token->lexeme = simple_lexeme_list[token->kind];
  }
  else if(c == '%')
  {
    token->kind = TokenKind_Percent;
    token->lexeme = simple_lexeme_list[token->kind];
    ++input->cursor;
  }
  else if(c == '\\')
  {
    token->kind = TokenKind_BackSlash;
    token->lexeme = simple_lexeme_list[token->kind];
    ++input->cursor;
  }
  else if(c == '.')
  {
    token->kind = TokenKind_Dot;
    token->lexeme = simple_lexeme_list[token->kind];
    ++input->cursor;
  }
  else if(c == '}')
  {
    token->kind = TokenKind_CloseBrace;
    token->lexeme = simple_lexeme_list[token->kind];
    ++input->cursor;
  }
  else if(c == '{')
  {
    token->kind = TokenKind_OpenBrace;
    token->lexeme = simple_lexeme_list[token->kind];
    ++input->cursor;
  }
  else if(c == '(')
  {
    token->kind = TokenKind_OpenParens;
    token->lexeme = simple_lexeme_list[token->kind];
    ++input->cursor;
  }
  else if(c == ')')
  {
    token->kind = TokenKind_CloseParens;
    token->lexeme = simple_lexeme_list[token->kind];
    ++input->cursor;
  }
  else if(c == ';')
  {
    token->kind = TokenKind_Semicolon;
    token->lexeme = simple_lexeme_list[token->kind];
    ++input->cursor;
  }
  else if(c == ',')
  {
    token->kind = TokenKind_Comma;
    token->lexeme = simple_lexeme_list[token->kind];
    ++input->cursor;
  }
  else if(c == ':')
  {
    token->kind = TokenKind_Colon;
    token->lexeme = simple_lexeme_list[token->kind];
    ++input->cursor;
  }
  else if(c == '[')
  {
    token->kind = TokenKind_OpenBracket;
    token->lexeme = simple_lexeme_list[token->kind];
    ++input->cursor;
  }
  else if(c == ']')
  {
    token->kind = TokenKind_CloseBracket;
    token->lexeme = simple_lexeme_list[token->kind];
    ++input->cursor;
  }
  else if(c == '\0')
  {
    token->lexeme = 0;
    token->kind = TokenKind_EndOfInput;
  }
}

