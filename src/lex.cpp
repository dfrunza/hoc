global_var Token keyword_list[] =
{
  {eToken::asm_, "asm"},
  {eToken::if_, "if"},
  {eToken::else_, "else"},
  {eToken::do_, "do"},
  {eToken::while_, "while"},
  {eToken::return_, "return"},
  {eToken::break_, "break"},
  {eToken::continue_, "continue"},
  {eToken::goto_, "goto"},
  {eToken::include, "include"},
  {eToken::true_, "true"},
  {eToken::false_, "false"},
  {eToken::struct_, "struct"},
  {eToken::union_, "union"},
  {eToken::enum_, "enum"},
  {eToken::extern_, "extern"},
  {eToken::const_, "const"},
  {eToken::and_, "and"},
  {eToken::or_, "or"},
  {eToken::not_, "not"},
  {eToken::mod, "mod"},
  {eToken::int_, "int"},
  {eToken::float_, "float"},
  {eToken::bool_, "bool"},
  {eToken::char_, "char"},
  {eToken::void_, "void"},
  {eToken::auto_, "auto"},
  {eToken::None, 0}, /* terminator */
};

Token* Lexer::lookup_keyword(char* lexeme)
{
  Token* result = 0;
  Token* token;

  for(int i = 0;
      (token = &keyword_list[i])->kind != eToken::None;
      token = &keyword_list[++i])
  {
    if(Cstr::match(lexeme, token->lexeme))
    {
      result = token;
      break;
    }
  }
  return result;
}

char* Lexer::install_lexeme(char* begin_char, char* end_char)
{
  assert(end_char >= begin_char);

  /* TODO: Search the lexeme, and if found, then return it. */
  int len = (int)(end_char - begin_char + 1);
  char* lexeme = mem_push_array(arena, char, len + 1); // +NULL
  Cstr::copy_substr(lexeme, begin_char, end_char);
  lexeme[len] = 0; // cap the string
  return lexeme;
}

bool Lexer::is_valid_escape_char(char c)
{
  return c == 't' || c == 'n' || c == 'r' || c == '0' ||
    c == '\"' || c == '\'' || c == '\\';
}

bool Lexer::escaped_string(char* file, int line, EscapedStr* estr)
{
  bool success = true;
  estr->len = 0;
  estr->end = cursor;
  estr->begin = cursor;

  /* find the closing `"` and count the length of the escaped string at the same time */
  char c = *(++estr->end);
  while(success && (c != estr->quote) && (c != '\0'))
  {
    if(c == '\\')
    {
      c = *(++estr->end);
      if(!is_valid_escape_char(c))
        success = compile_error_(arena, file, line, &src_loc, "invalid escape char `%c`", c);
    }
    estr->len++;
    c = *(++estr->end);
  }
  if(success)
  {
    if(*estr->end != estr->quote)
      success = compile_error_(arena, file, line, &src_loc, 
                                "malformed string literal, missing the closing `%c`", estr->quote);
  }
  assert((estr->end - estr->begin) >= 1);
  return success;
}

char* Lexer::install_escaped_str(EscapedStr* estr)
{
  assert(estr->begin <= estr->end);

  char* lexeme = mem_push_array(arena, char, estr->len+1); /* +NULL */

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

char* Token::get_printstr()
{
  local_persist char char_print_buf[3] = {0};
  char* result = 0;

  switch(kind)
  {
    case eToken::dot:
      result = ".";
    break;

    case eToken::arrow_right:
      result = "->";
    break;

    case eToken::open_bracket:
      result = "[";
    break;

    case eToken::close_bracket:
      result = "]";
    break;

    case eToken::open_parens:
      result = "(";
    break;

    case eToken::close_parens:
      result = ")";
    break;

    case eToken::open_brace:
      result = "{";
    break;
    case eToken::close_brace:
      result = "}";
    break;

    case eToken::semicolon:
      result = ";";
    break;

    case eToken::colon:
      result = ":";
    break;

    case eToken::comma:
      result = ",";
    break;

    case eToken::star:
      result = "*";
    break;

    case eToken::mul:
      result = "×";
    break;

    case eToken::fwd_slash:
      result = "/";
    break;

    case eToken::back_slash:
      result = "\\";
    break;

    case eToken::plus:
      result = "+";
    break;

    case eToken::plus_plus:
      result = "++";
    break;

    case eToken::minus:
      result = "-";
    break;

    case eToken::minus_minus:
      result = "--";
    break;

    case eToken::exclam:
      result = "!";
    break;

#if 0
    case eToken::exclam_eq:
      result = "!=";
      break;

#endif
    case eToken::eq:
      result = "=";
    break;

    case eToken::eq_eq:
      result = "==";
    break;

    case eToken::angle_right:
      result = ">";
    break;

    case eToken::angle_right_eq:
      result = ">=";
    break;

    case eToken::angle_right_right:
      result = ">>";
    break;

    case eToken::angle_left:
      result = "<";
    break;

    case eToken::angle_left_eq:
      result = "<=";
    break;

    case eToken::angle_left_left:
      result = "<<";
    break;

    case eToken::ampersand:
      result = "&";
    break;

    case eToken::pipe:
      result = "|";
    break;

    case eToken::tilde:
      result = "~";
    break;

    case eToken::circumflex:
      result = "^";
    break;

    case eToken::end_of_input:
      result = "end-of-input";
    break;

    case eToken::if_:
      result = "if";
    break;

    case eToken::else_:
      result = "else";
    break;

    case eToken::do_:
      result = "do";
    break;

    case eToken::while_:
      result = "while";
    break;

    case eToken::struct_:
      result = "struct";
    break;

    case eToken::union_:
      result = "union";
    break;

    case eToken::return_:
      result = "return";
    break;

    case eToken::break_:
      result = "break";
    break;

    case eToken::continue_:
      result = "continue";
    break;

    case eToken::include:
      result = "include";
    break;

    case eToken::enum_:
      result = "enum";
    break;

    case eToken::goto_:
      result = "goto";
    break;

    case eToken::true_:
      result = "true";
    break;

    case eToken::false_:
      result = "false";
    break;

    case eToken::extern_:
      result = "extern";
    break;

    case eToken::const_:
      result = "const";
    break;

    case eToken::and_:
      result = "and";
    break;

    case eToken::or_:
      result = "or";
    break;

    case eToken::not_:
      result = "not";
    break;

    case eToken::mod:
      result = "mod";
    break;

    case eToken::int_:
      result = "int";
    break;

    case eToken::float_:
      result = "float";
    break;

    case eToken::bool_:
      result = "bool";
    break;

    case eToken::char_:
      result = "char";
    break;

    case eToken::void_:
      result = "void";
    break;

    case eToken::auto_:
      result = "auto";
    break;

    case eToken::id:
    case eToken::int_val:
    case eToken::float_val:
      result = lexeme;
    break;

    case eToken::str_val:
      result = str_val; // TODO: Substitute non-printable chars
    break;

    case eToken::char_val:
    case eToken::unknown_char:
      Cstr::print_char(result = char_print_buf, char_val);
    break;

    default:
      result = "???";
  }
  return result;
}

Lexer* Lexer::create(MemoryArena* arena)
{
  Lexer* lexer = mem_push_struct(arena, Lexer);
  lexer->arena = arena;

  return lexer;
}

void Lexer::set_input(char* text, char* file_path)
{
  this->text = text;
  cursor = text;

  src_loc.line_nr = 1;
  /* TODO: Compute the absolute path to the file, so that Vim could properly
     jump from the QuickFix window to the error line in the file. */
  src_loc.file_path = file_path;

  last_state = mem_push_struct(arena, Lexer);
  *last_state = *this;
}

void Lexer::putback_token()
{
  *this = *this->last_state;
}

Token* Lexer::get_prev_token()
{
  if(last_state)
    token = last_state->token;
  return &token;
}

/* Returns the first non-whitepace char. */
char Lexer::skip_whitespace(char* whitechars)
{
  char c = *cursor;

  while(Cstr::contains_char(whitechars, c))
  {
    if(c == '\n')
    {
      src_loc.line_nr++;
      src_loc.src_line = cursor;
    }
    c = *(++cursor);
  }
  return c;
}

bool Lexer::get_asm_text()
{
  bool success = true;
  *last_state = *this;
  mem_zero_struct(&token, Token);
  src_loc.src_line = cursor;
  char c;

  skip_whitespace(" \r\n\t");
  char* begin_char = cursor;

  c = *begin_char;
  while(c != '\0' && c != '}')
  {
    c = *(++cursor);
    c = skip_whitespace(" \r\n\t");
  }

  if(c == '}')
  {
    char* end_char = cursor - 1;
    char* lexeme = install_lexeme(begin_char, end_char);

    token.kind = eToken::asm_text;
    token.lexeme = lexeme;
  }
  else if(c == '\0')
  {
    token.kind = eToken::end_of_input;
  }
  else
  {
    token.kind = eToken::unknown_char;
    token.char_val = c;
  }
  return success;
}

bool Lexer::get_next_token()
{
  bool success = true;
  *last_state = *this;
  mem_zero_struct(&token, Token);
  src_loc.src_line = cursor;
  char c;

loop:
  skip_whitespace(" \r\n\t");
  c = *cursor;

  if(Cstr::is_letter(c) || c == '_')
  {
    char* begin_char = cursor;
    c = *(++cursor);

    while(Cstr::is_letter(c) || Cstr::is_dec_digit(c) || c == '_')
    {
      c = *(++cursor);
    }

    char* end_char = cursor - 1;
    char* lexeme = install_lexeme(begin_char, end_char);

    token.kind = eToken::id;
    token.lexeme = lexeme;
    Token* keyword = lookup_keyword(lexeme);
    if(keyword)
    {
      token.kind = keyword->kind;
    }
  }
  else if(Cstr::is_dec_digit(c))
  {
    char digit_buf[32] = {0};
    bool is_float = false;
    bool is_hex = false;

    int i = 0;
    digit_buf[i++] = c;
    c = *(++cursor);

    if(c == 'x') // hexadecimal
    {
      is_hex = true;
      c = *(++cursor);

      for(; i < sizeof_array(digit_buf)-1 && Cstr::is_hex_digit(c);
          i++)
      {
        digit_buf[i] = c;
        c = *(++cursor);
      }
    }
    else if(Cstr::is_dec_digit(c) || c == '.')
    {
      for(; i < sizeof_array(digit_buf)-1 && ((Cstr::is_dec_digit(c) || c == '.'));
          i++)
      {
        digit_buf[i] = c;
        if(c == '.')
        {
          if(is_float)
            break;
          is_float = true;
        }
        c = *(++cursor);
      }
    }
    digit_buf[i] = '\0';
    token.lexeme = install_lexeme(digit_buf, digit_buf + i-1);

    if(is_float)
    {
      token.kind = eToken::float_val;
      token.float_val = mem_push_struct(arena, float);
      Platform::sscanf(digit_buf, "%f", token.float_val);
    }
    else
    {
      token.kind = eToken::int_val;
      token.int_val = mem_push_struct(arena, int);
      if(is_hex)
        Platform::sscanf(digit_buf, "%x", token.int_val);
      else
        Platform::sscanf(digit_buf, "%d", token.int_val);
    }
  }
  else if(c == '-')
  {
    token.kind = eToken::minus;
    c = *(++cursor);
    if(c == '-')
    {
      token.kind = eToken::minus_minus;
      ++cursor;
    }
    else if(c == '>')
    {
      token.kind = eToken::arrow_right;
      ++cursor;
    }
  }
  else if(c == '<')
  {
    token.kind = eToken::angle_left;
    c = *(++cursor);
    if(c == '=')
    {
      token.kind = eToken::angle_left_eq;
      ++cursor;
    }
    else if(c == '<')
    {
      token.kind = eToken::angle_left_left;
      ++cursor;
    }
    else if(c == '>')
    {
      token.kind = eToken::angle_left_right;
      ++cursor;
    }
  }
  else if(c == '&')
  {
    token.kind = eToken::ampersand;
    c = *(++cursor);
  }
  else if(c == '/')
  {
    /* multi-line comments */
    char* fwd_cursor = cursor;

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
            src_loc.line_nr++;
            src_loc.src_line = cursor;
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
      cursor = ++fwd_cursor;
      goto loop;
    }
    else
    {
      token.kind = eToken::fwd_slash;
      ++cursor;
    }
  }
  else if(c == '"')
  {
    /* double-quoted escaped string */
    EscapedStr estr = {0};
    estr.quote = '"';

    if(success = escaped_string(__FILE__, __LINE__, &estr))
    {
      token.str_val = install_escaped_str(&estr);;
      token.kind = eToken::str_val;
      cursor = ++estr.end;
    }
  }
  else if(c == '\'')
  {
    /* single-quoted escaped char */
    EscapedStr estr = {0};
    estr.quote = '\'';

    if(success = escaped_string(__FILE__, __LINE__, &estr))
    {
      char* lexeme = install_escaped_str(&estr);

      if(estr.len != 1)
        success = compile_error(arena, &src_loc, "invalid char literal '%s'", lexeme);
      else
      {
        token.char_val = *lexeme;
        token.kind = eToken::char_val;
        cursor = ++estr.end;
      }
    }
  }
  else if(c == '=')
  {
    token.kind = eToken::eq;
    c = *(++cursor);
    if(c == '=')
    {
      token.kind = eToken::eq_eq;
      ++cursor;
    }
  }
  else if(c == '>')
  {
    token.kind = eToken::angle_right;
    c = *(++cursor);
    if(c == '=')
    {
      token.kind = eToken::angle_right_eq;
      ++cursor;
    }
    else if(c == '>')
    {
      token.kind = eToken::angle_right_right;
      ++cursor;
    }
  }
  else if(c == '|')
  {
    token.kind = eToken::pipe;
    c = *(++cursor);
  }
  else if(c == '~')
  {
    token.kind = eToken::tilde;
    c = *(++cursor);
  }
  else if(c == '!')
  {
    token.kind = eToken::exclam;
    c = *(++cursor);
#if 0
    if(c == '=')
    {
      token.kind = eToken::exclam_eq;
      ++cursor;
    }
#endif
  }
  else if(c == 'ª')
  {
    token.kind = eToken::logic_not;
    c = *(++cursor);
  }
  else if(c == '+')
  {
    token.kind = eToken::plus;
    c = *(++cursor);
    if(c == '+')
    {
      token.kind = eToken::plus_plus;
      ++cursor;
    }
  }
  else if(c == '*')
  {
    token.kind = eToken::star;
    ++cursor;
  }
  else if(c == '×')
  {
    token.kind = eToken::mul;
    c = *(++cursor);
  }
  else if(c == '^')
  {
    token.kind = eToken::circumflex;
    ++cursor;
  }
  else if(c == '\\')
  {
    token.kind = eToken::back_slash;
    ++cursor;
  }
  else if(c == '.')
  {
    token.kind = eToken::dot;
    ++cursor;
  }
  else if(c == '}')
  {
    token.kind = eToken::close_brace;
    ++cursor;
  }
  else if(c == '{')
  {
    token.kind = eToken::open_brace;
    ++cursor;
  }
  else if(c == '(')
  {
    token.kind = eToken::open_parens;
    ++cursor;
  }
  else if(c == ')')
  {
    token.kind = eToken::close_parens;
    ++cursor;
  }
  else if(c == ';')
  {
    token.kind = eToken::semicolon;
    ++cursor;
  }
  else if(c == ',')
  {
    token.kind = eToken::comma;
    ++cursor;
  }
  else if(c == ':')
  {
    token.kind = eToken::colon;
    ++cursor;
  }
  else if(c == '[')
  {
    token.kind = eToken::open_bracket;
    ++cursor;
  }
  else if(c == ']')
  {
    token.kind = eToken::close_bracket;
    ++cursor;
  }
  else if(c == '\0')
  {
    token.kind = eToken::end_of_input;
  }
  else
  {
    token.kind = eToken::unknown_char;
    token.char_val = c;
  }
  return success;
}

