bool32
token_is_keyword(Token token)
{
  return token > Token__KeywordBegin && token < Token__KeywordEnd;
}

char*
lexeme_install_id(TokenStream* input, char* begin_char, char* end_char)
{
  assert(end_char >= begin_char);

  //TODO: Search the lexeme, and if found, then return it.
  size_t len = end_char - begin_char + 1;
  char* lexeme = mem_push_struct(input->arena, char, len + 1);
  cstr_copy_substr(lexeme, begin_char, end_char);
  return lexeme;
}

char*
lexeme_install_dquot_str(TokenStream* input, char* begin_char, char* end_char)
{
  assert(end_char >= begin_char);
  assert(*begin_char == '"' && *end_char == '"');

  size_t len = (end_char - begin_char + 1) - 2; // minus the quotes
  char* lexeme = mem_push_struct(input->arena, char, len + 1); // +NULL

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
token_stream_init(TokenStream* token_stream, MemoryArena* arena, char* text, char* file_path)
{
  token_stream->arena = arena;
  token_stream->text = text;
  token_stream->cursor = token_stream->text;
  token_stream->line_nr = 1;
  //TODO: Compute the absolute path to the file, so that Vim could properly
  // jump from the QuickFix window to the error line in the file.
  token_stream->file_path = file_path;
}

void
consume_token(TokenStream* input, SymbolTable* symbol_table)
{
  input->prev_token = input->token;
  input->token = Token__Null;
  mem_zero(&input->lexeme);

  input->src_line = input->cursor;
  char c;

loop:
  c = *input->cursor;
  while(c == ' ' || c == '\t' ||
        c == '\r' || c == '\n')
  {
    if(c == '\n')
    {
      input->line_nr++;
      input->src_line = input->cursor;
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
    char* lexeme = lexeme_install_id(input, begin_char, end_char);
    input->lexeme.str = lexeme;

    Symbol* symbol = lookup_symbol(symbol_table, lexeme);
    if(symbol && symbol->kind == SymbolKind_Keyword)
      input->token = symbol->kw_token;
    else
      input->token = Token_Id;
  }
  else if(char_is_numeric(c))
  {
    int num = c - '0';
    c = *(++input->cursor);

    while(char_is_numeric(c))
    {
      num = (10 * num) + (c - '0');
      c = *(++input->cursor);
    }

    int* value = mem_push_struct(input->arena, int, 1);
    *value = num;
    input->token = Token_IntNum;
    input->lexeme.int_num = value;
  }
  else if(c == '-')
  {
    input->token = Token_Minus;
    c = *(++input->cursor);
    if(c == '>')
    {
      input->token = Token_RightArrow;
      ++input->cursor;
    }
    else if(input->prev_token == Token_Equals ||
            input->prev_token == Token_OpenParens ||
            input->prev_token == Token_Star ||
            input->prev_token == Token_Plus ||
            input->prev_token == Token_Comma ||
            input->prev_token == Token_FwdSlash ||
            input->prev_token == Token_Return)
    {
      input->token = Token_UnaryMinus;
    }
    else if(c == '-')
    {
      input->token = Token_MinusMinus;
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
          c = *(++fwd_cursor);
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
      input->token = Token_FwdSlash;
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
      char* lexeme = lexeme_install_dquot_str(input, input->cursor, fwd_cursor);
      input->lexeme.str = lexeme;
      input->token = Token_String;
      input->cursor = ++fwd_cursor;
    } else
      syntax_error(input, "Missing closing '\"'\n");
  }
  else if(c == '=')
  {
    input->token = Token_Equals;
    c = *(++input->cursor);
    if(c == '=')
    {
      input->token = Token_EqualsEquals;
      ++input->cursor;
    }
  }
  else if(c == '<')
  {
    input->token = Token_AngleLeft;
    c = *(++input->cursor);
    if(c == '=')
    {
      input->token = Token_AngleLeftEquals;
      ++input->cursor;
    }
  }
  else if(c == '>')
  {
    input->token = Token_AngleRight;
    c = *(++input->cursor);
    if(c == '=')
    {
      input->token = Token_AngleRightEquals;
      ++input->cursor;
    }
  }
  else if(c == '&')
  {
    input->token = Token_Amprsnd;
    c = *(++input->cursor);
    if(c == '&')
    {
      input->token = Token_AmprsndAmprsnd;
      ++input->cursor;
    }
  }
  else if(c == '|')
  {
    input->token = Token_Pipe;
    c = *(++input->cursor);
    if(c == '|')
    {
      input->token = Token_PipePipe;
      ++input->cursor;
    }
  }
  else if(c == '!')
  {
    input->token = Token_Bang;
    c = *(++input->cursor);
    if(c == '=')
    {
      input->token = Token_BangEquals;
      ++input->cursor;
    }
  }
  else if(c == '+')
  {
    input->token = Token_Plus;
    c = *(++input->cursor);
    if(c == '+')
    {
      input->token = Token_PlusPlus;
      ++input->cursor;
    }
  }
  else if(c == '%')
  {
    input->token = Token_Percent;
    ++input->cursor;
  }
  else if(c == '\\')
  {
    input->token = Token_BackSlash;
    ++input->cursor;
  }
  else if(c == '*')
  {
    input->token = Token_Star;
    ++input->cursor;
  }
  else if(c == '.')
  {
    input->token = Token_Dot;
    ++input->cursor;
  }
  else if(c == '}')
  {
    input->token = Token_CloseBrace;
    ++input->cursor;
  }
  else if(c == '{')
  {
    input->token = Token_OpenBrace;
    ++input->cursor;
  }
  else if(c == '(')
  {
    input->token = Token_OpenParens;
    ++input->cursor;
  }
  else if(c == ')')
  {
    input->token = Token_CloseParens;
    ++input->cursor;
  }
  else if(c == ';')
  {
    input->token = Token_Semicolon;
    ++input->cursor;
  }
  else if(c == ',')
  {
    input->token = Token_Comma;
    ++input->cursor;
  }
  else if(c == ':')
  {
    input->token = Token_Colon;
    ++input->cursor;
  }
  else if(c == '[')
  {
    input->token = Token_OpenParens;
    ++input->cursor;
  }
  else if(c == ']')
  {
    input->token = Token_CloseBracket;
    ++input->cursor;
  }
  else if(c == '^')
  {
    input->token = Token_UpArrow;
    ++input->cursor;
  }
  else if(c == '\0')
    input->token = Token_EndOfInput;
}

