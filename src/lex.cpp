global_var Token keyword_list[] =
{
  {eToken_asm, "asm"},
  {eToken_if, "if"},
  {eToken_else, "else"},
  {eToken_do, "do"},
  {eToken_while, "while"},
  {eToken_return, "return"},
  {eToken_break, "break"},
  {eToken_continue, "continue"},
  {eToken_goto, "goto"},
  {eToken_include, "include"},
  {eToken_true, "true"},
  {eToken_false, "false"},
  {eToken_struct, "struct"},
  {eToken_union, "union"},
  {eToken_enum, "enum"},
  {eToken_extern, "extern"},
  {eToken_const, "const"},
  {eToken_and, "and"},
  {eToken_or, "or"},
  {eToken_not, "not"},
  {eToken_int, "int"},
  {eToken_float, "float"},
  {eToken_bool, "bool"},
  {eToken_char, "char"},
  {eToken_void, "void"},
  {eToken_auto, "auto"},
  {eToken_cast, "cast"},
  {eToken_None, 0}, /* terminator */
};

Token* lookup_keyword(char* lexeme)
{
  Token* result = 0;
  Token* token;

  for(int i = 0;
      (token = &keyword_list[i])->kind != eToken_None;
      token = &keyword_list[++i])
  {
    if(cstr_match(lexeme, token->lexeme))
    {
      result = token;
      break;
    }
  }
  return result;
}

char* lex_install_lexeme(Lexer* lex, char* begin_char, char* end_char)
{
  assert(end_char >= begin_char);

  /* TODO: Search the lexeme, and if found, then return it. */
  int len = (int)(end_char - begin_char + 1);
  char* lexeme = push_array(lex->arena, char, len + 1); // +NULL
  cstr_copy_substr(lexeme, begin_char, end_char);
  lexeme[len] = 0; // cap the string
  return lexeme;
}

bool is_valid_escape_char(char c)
{
  return c == 't' || c == 'n' || c == 'r' || c == '0' ||
    c == '\"' || c == '\'' || c == '\\';
}

bool lex_escaped_string(Lexer* lex, char* file, int line, EscapedStr* estr)
{
  bool success = true;
  estr->len = 0;
  estr->end = lex->cursor;
  estr->begin = lex->cursor;

  /* find the closing `"` and count the length of the escaped string at the same time */
  char c = *(++estr->end);
  while(success && (c != estr->quote) && (c != '\0'))
  {
    if(c == '\\')
    {
      c = *(++estr->end);
      if(!is_valid_escape_char(c))
        success = compile_error_(lex->arena, file, line, &lex->src_loc, "invalid escape char `%c`", c);
    }
    estr->len++;
    c = *(++estr->end);
  }
  if(success)
  {
    if(*estr->end != estr->quote)
      success = compile_error_(lex->arena, file, line, &lex->src_loc, 
                                "malformed string literal, missing the closing `%c`", estr->quote);
  }
  assert((estr->end - estr->begin) >= 1);
  return success;
}

char* lex_install_escaped_str(Lexer* lex, EscapedStr* estr)
{
  assert(estr->begin <= estr->end);

  char* lexeme = push_array(lex->arena, char, estr->len+1); /* +NULL */

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

char* get_token_printstr(Token* token)
{
  local_persist char char_print_buf[3] = {0};
  char* result = 0;

  switch(token->kind)
  {
    case eToken_dot:
      result = ".";
    break;

    case eToken_arrow_right:
      result = "->";
    break;

    case eToken_open_bracket:
      result = "[";
    break;

    case eToken_close_bracket:
      result = "]";
    break;

    case eToken_open_parens:
      result = "(";
    break;

    case eToken_close_parens:
      result = ")";
    break;

    case eToken_open_brace:
      result = "{";
    break;
    case eToken_close_brace:
      result = "}";
    break;

    case eToken_semicolon:
      result = ";";
    break;

    case eToken_colon:
      result = ":";
    break;

    case eToken_comma:
      result = ",";
    break;

    case eToken_star:
      result = "*";
    break;

    case eToken_mul:
      result = "×";
    break;

    case eToken_fwd_slash:
      result = "/";
    break;

    case eToken_percent:
      result = "%";
    break;

    case eToken_back_slash:
      result = "\\";
    break;

    case eToken_plus:
      result = "+";
    break;

    case eToken_plus_plus:
      result = "++";
    break;

    case eToken_minus:
      result = "-";
    break;

    case eToken_minus_minus:
      result = "--";
    break;

    case eToken_exclam:
      result = "!";
    break;

#if 0
    case eToken_exclam_eq:
      result = "!=";
      break;

#endif
    case eToken_eq:
      result = "=";
    break;

    case eToken_eq_eq:
      result = "==";
    break;

    case eToken_angle_right:
      result = ">";
    break;

    case eToken_angle_right_eq:
      result = ">=";
    break;

    case eToken_angle_right_right:
      result = ">>";
    break;

    case eToken_angle_left:
      result = "<";
    break;

    case eToken_angle_left_eq:
      result = "<=";
    break;

    case eToken_angle_left_left:
      result = "<<";
    break;

    case eToken_ampersand:
      result = "&";
    break;

    case eToken_pipe:
      result = "|";
    break;

    case eToken_tilde:
      result = "~";
    break;

    case eToken_circumflex:
      result = "^";
    break;

    case eToken_end_of_input:
      result = "end-of-input";
    break;

    case eToken_if:
      result = "if";
    break;

    case eToken_else:
      result = "else";
    break;

    case eToken_do:
      result = "do";
    break;

    case eToken_while:
      result = "while";
    break;

    case eToken_struct:
      result = "struct";
    break;

    case eToken_union:
      result = "union";
    break;

    case eToken_return:
      result = "return";
    break;

    case eToken_break:
      result = "break";
    break;

    case eToken_continue:
      result = "continue";
    break;

    case eToken_include:
      result = "include";
    break;

    case eToken_enum:
      result = "enum";
    break;

    case eToken_goto:
      result = "goto";
    break;

    case eToken_true:
      result = "true";
    break;

    case eToken_false:
      result = "false";
    break;

    case eToken_extern:
      result = "extern";
    break;

    case eToken_const:
      result = "const";
    break;

    case eToken_and:
      result = "and";
    break;

    case eToken_or:
      result = "or";
    break;

    case eToken_not:
      result = "not";
    break;

    case eToken_int:
      result = "int";
    break;

    case eToken_float:
      result = "float";
    break;

    case eToken_bool:
      result = "bool";
    break;

    case eToken_char:
      result = "char";
    break;

    case eToken_void:
      result = "void";
    break;

    case eToken_auto:
      result = "auto";
    break;

    case eToken_cast:
      result = "cast";
    break;

    case eToken_id:
    case eToken_int_val:
    case eToken_float_val:
      result = token->lexeme;
    break;

    case eToken_str_val:
      result = token->str_val; // TODO: Substitute non-printable chars
    break;

    case eToken_char_val:
    case eToken_unknown_char:
      cstr_print_char(result = char_print_buf, token->char_val);
    break;

    default:
      result = "???";
  }
  return result;
}

Lexer* new_lexer(MemoryArena* arena)
{
  Lexer* lexer = push_struct(arena, Lexer);
  lexer->arena = arena;

  return lexer;
}

void lex_set_input(Lexer* lex, char* text, char* file_path)
{
  lex->text = text;
  lex->cursor = text;

  lex->src_loc.line_nr = 1;
  /* TODO: Compute the absolute path to the file, so that Vim could properly
     jump from the QuickFix window to the error line in the file. */
  lex->src_loc.file_path = file_path;

  lex->last_state = push_struct(lex->arena, Lexer);
  *lex->last_state = *lex;
}

void lex_putback_token(Lexer* lex)
{
  *lex = *lex->last_state;
}

Token* lex_get_prev_token(Lexer* lex)
{
  if(lex->last_state)
    lex->token = lex->last_state->token;
  return &lex->token;
}

/* Returns the first non-whitepace char. */
char lex_skip_whitespace(Lexer* lex, char* whitechars)
{
  char c = *lex->cursor;

  while(cstr_contains_char(whitechars, c))
  {
    if(c == '\n')
    {
      lex->src_loc.line_nr++;
      lex->src_loc.src_line = lex->cursor;
    }
    c = *(++lex->cursor);
  }
  return c;
}

bool lex_get_asm_text(Lexer* lex)
{
  bool success = true;
  *lex->last_state = *lex;
  zero_struct(&lex->token, Token);
  lex->src_loc.src_line = lex->cursor;
  char c;

  lex_skip_whitespace(lex, " \r\n\t");
  char* begin_char = lex->cursor;

  c = *begin_char;
  while(c != '\0' && c != '}')
  {
    c = *(++lex->cursor);
    c = lex_skip_whitespace(lex, " \r\n\t");
  }

  if(c == '}')
  {
    char* end_char = lex->cursor - 1;
    char* lexeme = lex_install_lexeme(lex, begin_char, end_char);

    lex->token.kind = eToken_asm_text;
    lex->token.lexeme = lexeme;
  }
  else if(c == '\0')
  {
    lex->token.kind = eToken_end_of_input;
  }
  else
  {
    lex->token.kind = eToken_unknown_char;
    lex->token.char_val = c;
  }
  return success;
}

bool lex_get_next_token(Lexer* lex)
{
  bool success = true;
  *lex->last_state = *lex;
  zero_struct(&lex->token, Token);
  lex->src_loc.src_line = lex->cursor;
  char c;

loop:
  lex_skip_whitespace(lex, " \r\n\t");
  c = *lex->cursor;

  if(cstr_is_letter(c) || c == '_')
  {
    char* begin_char = lex->cursor;
    c = *(++lex->cursor);

    while(cstr_is_letter(c) || cstr_is_dec_digit(c) || c == '_')
    {
      c = *(++lex->cursor);
    }

    char* end_char = lex->cursor - 1;
    char* lexeme = lex_install_lexeme(lex, begin_char, end_char);

    lex->token.kind = eToken_id;
    lex->token.lexeme = lexeme;
    Token* keyword = lookup_keyword(lexeme);
    if(keyword)
    {
      lex->token.kind = keyword->kind;
    }
  }
  else if(cstr_is_dec_digit(c))
  {
    char digit_buf[32] = {0};
    bool is_float = false;
    bool is_hex = false;

    int i = 0;
    digit_buf[i++] = c;
    c = *(++lex->cursor);

    if(c == 'x') // hexadecimal
    {
      is_hex = true;
      c = *(++lex->cursor);

      for(; i < sizeof_array(digit_buf)-1 && cstr_is_hex_digit(c);
          i++)
      {
        digit_buf[i] = c;
        c = *(++lex->cursor);
      }
    }
    else if(cstr_is_dec_digit(c) || c == '.')
    {
      for(; i < sizeof_array(digit_buf)-1 && ((cstr_is_dec_digit(c) || c == '.'));
          i++)
      {
        digit_buf[i] = c;
        if(c == '.')
        {
          if(is_float)
            break;
          is_float = true;
        }
        c = *(++lex->cursor);
      }
    }
    digit_buf[i] = '\0';
    lex->token.lexeme = lex_install_lexeme(lex, digit_buf, digit_buf + i-1);

    if(is_float)
    {
      lex->token.kind = eToken_float_val;
      lex->token.float_val = push_struct(lex->arena, float);
      platform_sscanf(digit_buf, "%f", lex->token.float_val);
    }
    else
    {
      lex->token.kind = eToken_int_val;
      lex->token.int_val = push_struct(lex->arena, int);
      if(is_hex)
        platform_sscanf(digit_buf, "%x", lex->token.int_val);
      else
        platform_sscanf(digit_buf, "%d", lex->token.int_val);
    }
  }
  else if(c == '-')
  {
    lex->token.kind = eToken_minus;
    c = *(++lex->cursor);
    if(c == '-')
    {
      lex->token.kind = eToken_minus_minus;
      ++lex->cursor;
    }
    else if(c == '>')
    {
      lex->token.kind = eToken_arrow_right;
      ++lex->cursor;
    }
  }
  else if(c == '<')
  {
    lex->token.kind = eToken_angle_left;
    c = *(++lex->cursor);
    if(c == '=')
    {
      lex->token.kind = eToken_angle_left_eq;
      ++lex->cursor;
    }
    else if(c == '<')
    {
      lex->token.kind = eToken_angle_left_left;
      ++lex->cursor;
    }
    else if(c == '>')
    {
      lex->token.kind = eToken_angle_left_right;
      ++lex->cursor;
    }
  }
  else if(c == '&')
  {
    lex->token.kind = eToken_ampersand;
    c = *(++lex->cursor);
  }
  else if(c == '/')
  {
    /* multi-line comments */
    char* fwd_cursor = lex->cursor;

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
            lex->src_loc.line_nr++;
            lex->src_loc.src_line = lex->cursor;
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
      lex->cursor = ++fwd_cursor;
      goto loop;
    }
    else
    {
      lex->token.kind = eToken_fwd_slash;
      ++lex->cursor;
    }
  }
  else if(c == '"')
  {
    /* double-quoted escaped string */
    EscapedStr estr = {0};
    estr.quote = '"';

    if(success = lex_escaped_string(lex, __FILE__, __LINE__, &estr))
    {
      lex->token.str_val = lex_install_escaped_str(lex, &estr);;
      lex->token.kind = eToken_str_val;
      lex->cursor = ++estr.end;
    }
  }
  else if(c == '\'')
  {
    /* single-quoted escaped char */
    EscapedStr estr = {0};
    estr.quote = '\'';

    if(success = lex_escaped_string(lex, __FILE__, __LINE__, &estr))
    {
      char* lexeme = lex_install_escaped_str(lex, &estr);

      if(estr.len != 1)
        success = compile_error(lex->arena, &lex->src_loc, "invalid char literal '%s'", lexeme);
      else
      {
        lex->token.char_val = *lexeme;
        lex->token.kind = eToken_char_val;
        lex->cursor = ++estr.end;
      }
    }
  }
  else if(c == '=')
  {
    lex->token.kind = eToken_eq;
    c = *(++lex->cursor);
    if(c == '=')
    {
      lex->token.kind = eToken_eq_eq;
      ++lex->cursor;
    }
  }
  else if(c == '>')
  {
    lex->token.kind = eToken_angle_right;
    c = *(++lex->cursor);
    if(c == '=')
    {
      lex->token.kind = eToken_angle_right_eq;
      ++lex->cursor;
    }
    else if(c == '>')
    {
      lex->token.kind = eToken_angle_right_right;
      ++lex->cursor;
    }
  }
  else if(c == '|')
  {
    lex->token.kind = eToken_pipe;
    c = *(++lex->cursor);
  }
  else if(c == '~')
  {
    lex->token.kind = eToken_tilde;
    c = *(++lex->cursor);
  }
  else if(c == '!')
  {
    lex->token.kind = eToken_exclam;
    c = *(++lex->cursor);
#if 0
    if(c == '=')
    {
      token.kind = eToken_exclam_eq;
      ++cursor;
    }
#endif
  }
  else if(c == '+')
  {
    lex->token.kind = eToken_plus;
    c = *(++lex->cursor);
    if(c == '+')
    {
      lex->token.kind = eToken_plus_plus;
      ++lex->cursor;
    }
  }
  else if(c == '*')
  {
    lex->token.kind = eToken_star;
    ++lex->cursor;
  }
  else if(c == '%')
  {
    lex->token.kind = eToken_percent;
    ++lex->cursor;
  }
  else if(c == '×')
  {
    lex->token.kind = eToken_mul;
    c = *(++lex->cursor);
  }
  else if(c == '^')
  {
    lex->token.kind = eToken_circumflex;
    ++lex->cursor;
  }
  else if(c == '\\')
  {
    lex->token.kind = eToken_back_slash;
    ++lex->cursor;
  }
  else if(c == '.')
  {
    lex->token.kind = eToken_dot;
    ++lex->cursor;
  }
  else if(c == '}')
  {
    lex->token.kind = eToken_close_brace;
    ++lex->cursor;
  }
  else if(c == '{')
  {
    lex->token.kind = eToken_open_brace;
    ++lex->cursor;
  }
  else if(c == '(')
  {
    lex->token.kind = eToken_open_parens;
    ++lex->cursor;
  }
  else if(c == ')')
  {
    lex->token.kind = eToken_close_parens;
    ++lex->cursor;
  }
  else if(c == ';')
  {
    lex->token.kind = eToken_semicolon;
    ++lex->cursor;
  }
  else if(c == ',')
  {
    lex->token.kind = eToken_comma;
    ++lex->cursor;
  }
  else if(c == ':')
  {
    lex->token.kind = eToken_colon;
    ++lex->cursor;
  }
  else if(c == '[')
  {
    lex->token.kind = eToken_open_bracket;
    ++lex->cursor;
  }
  else if(c == ']')
  {
    lex->token.kind = eToken_close_bracket;
    ++lex->cursor;
  }
  else if(c == '\0')
  {
    lex->token.kind = eToken_end_of_input;
  }
  else
  {
    lex->token.kind = eToken_unknown_char;
    lex->token.char_val = c;
  }
  return success;
}

