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

void
get_next_token(MemoryArena* arena, TokenStream* input)
{
  input->prev_tokens[1] = input->prev_tokens[0];
  input->prev_tokens[0] = input->token;
  mem_zero(&input->token);
  SourceLocation* src_loc = &input->src_loc;
  src_loc->src_line = input->cursor;
  char c;

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

    input->token.kind = TokenKind_Id;
    input->token.lexeme = lexeme;
    Token* keyword = lookup_keyword(keyword_list, lexeme);
    if(keyword)
      input->token.kind = keyword->kind;
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
      input->token.kind = TokenKind_FloatNum;
      input->token.float_val = mem_push_struct(arena, float, 1);
      sscanf(digit_buf, "%f", input->token.float_val);
    }
    else
    {
      input->token.kind = TokenKind_IntNum;
      input->token.int_val = mem_push_struct(arena, int, 1);
      sscanf(digit_buf, "%d", input->token.int_val);
    }
  }
  else if(c == '-')
  {
    Token* prev_token = get_prev_token(input, 0);
    input->token.kind = TokenKind_Minus;
    c = *(++input->cursor);
    if(c == '>')
    {
      input->token.kind = TokenKind_RightArrow;
      ++input->cursor;
    }
    else if(prev_token->kind == TokenKind_Equals ||
            prev_token->kind == TokenKind_OpenParens ||
            prev_token->kind == TokenKind_Star ||
            prev_token->kind == TokenKind_Plus ||
            prev_token->kind == TokenKind_Comma ||
            prev_token->kind == TokenKind_FwdSlash ||
            prev_token->kind == TokenKind_Return)
    {
       input->token.kind = TokenKind_UnaryMinus;
    }
    else if(c == '-')
    {
      input->token.kind = TokenKind_MinusMinus;
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
    } else
    {
      input->token.kind = TokenKind_FwdSlash;
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
      input->token.str = lexeme;
      input->token.kind = TokenKind_String;
      input->cursor = ++fwd_cursor;
    } else
      compile_error(&input->src_loc, "Missing closing '\"'\n");
  }
  else if(c == '=')
  {
    input->token.kind = TokenKind_Equals;
    c = *(++input->cursor);
    if(c == '=')
    {
      input->token.kind = TokenKind_EqualsEquals;
      ++input->cursor;
    }
  }
  else if(c == '<')
  {
    input->token.kind = TokenKind_AngleLeft;
    c = *(++input->cursor);
    if(c == '=')
    {
      input->token.kind = TokenKind_AngleLeftEquals;
      ++input->cursor;
    }
  }
  else if(c == '>')
  {
    input->token.kind = TokenKind_AngleRight;
    c = *(++input->cursor);
    if(c == '=')
    {
      input->token.kind = TokenKind_AngleRightEquals;
      ++input->cursor;
    }
  }
  else if(c == '&')
  {
    input->token.kind = TokenKind_Amprsnd;
    c = *(++input->cursor);
    if(c == '&')
    {
      input->token.kind = TokenKind_AmprsndAmprsnd;
      ++input->cursor;
    }
  }
  else if(c == '|')
  {
    input->token.kind = TokenKind_Pipe;
    c = *(++input->cursor);
    if(c == '|')
    {
      input->token.kind = TokenKind_PipePipe;
      ++input->cursor;
    }
  }
  else if(c == '!')
  {
    input->token.kind = TokenKind_Bang;
    c = *(++input->cursor);
    if(c == '=')
    {
      input->token.kind = TokenKind_BangEquals;
      ++input->cursor;
    }
  }
  else if(c == '+')
  {
    input->token.kind = TokenKind_Plus;
    c = *(++input->cursor);
    if(c == '+')
    {
      input->token.kind = TokenKind_PlusPlus;
      ++input->cursor;
    }
  }
  else if(c == '%')
  {
    input->token.kind = TokenKind_Percent;
    ++input->cursor;
  }
  else if(c == '\\')
  {
    input->token.kind = TokenKind_BackSlash;
    ++input->cursor;
  }
  else if(c == '*')
  {
    input->token.kind = TokenKind_Star;
    ++input->cursor;
  }
  else if(c == '.')
  {
    input->token.kind = TokenKind_Dot;
    ++input->cursor;
  }
  else if(c == '}')
  {
    input->token.kind = TokenKind_CloseBrace;
    ++input->cursor;
  }
  else if(c == '{')
  {
    input->token.kind = TokenKind_OpenBrace;
    ++input->cursor;
  }
  else if(c == '(')
  {
    input->token.kind = TokenKind_OpenParens;
    ++input->cursor;
  }
  else if(c == ')')
  {
    input->token.kind = TokenKind_CloseParens;
    ++input->cursor;
  }
  else if(c == ';')
  {
    input->token.kind = TokenKind_Semicolon;
    ++input->cursor;
  }
  else if(c == ',')
  {
    input->token.kind = TokenKind_Comma;
    ++input->cursor;
  }
  else if(c == ':')
  {
    input->token.kind = TokenKind_Colon;
    ++input->cursor;
  }
  else if(c == '[')
  {
    input->token.kind = TokenKind_OpenParens;
    ++input->cursor;
  }
  else if(c == ']')
  {
    input->token.kind = TokenKind_CloseBracket;
    ++input->cursor;
  }
  else if(c == '^')
  {
    input->token.kind = TokenKind_UpArrow;
    ++input->cursor;
  }
  else if(c == '\0')
    input->token.kind = TokenKind_EndOfInput;
}

