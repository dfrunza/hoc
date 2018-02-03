Token keyword_list[] = 
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
  {eToken_mod, "mod"},
  {eToken_int, "int"},
  {eToken_float, "float"},
  {eToken_bool, "bool"},
  {eToken_char, "char"},
  {eToken_void, "void"},
  {eToken_auto, "auto"},
  {eToken_None, 0}, /* terminator */
};

Token* lookup_keyword(Token* list, char* lexeme)
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

char* install_lexeme(MemoryArena* arena, char* begin_char, char* end_char)
{
  assert(end_char >= begin_char);

  /* TODO: Search the lexeme, and if found, then return it. */
  int len = (int)(end_char - begin_char + 1);
  char* lexeme = mem_push_array_nz(arena, char, len + 1); // +NULL
  cstr_copy_substr(lexeme, begin_char, end_char);
  lexeme[len] = 0; // cap the string
  return lexeme;
}

bool is_valid_escape_char(char c)
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

bool escaped_string(char* file, int line, Lexer* lexer, EscapedStr* estr)
{
  bool success = true;
  estr->len = 0;
  estr->end = lexer->cursor;
  estr->begin = lexer->cursor;

  /* find the closing `"` and count the length of the escaped string at the same time */
  char c = *(++estr->end);
  while(success && (c != estr->quote) && (c != '\0'))
  {
    if(c == '\\')
    {
      c = *(++estr->end);
      if(!is_valid_escape_char(c))
        success = compile_error_(lexer->arena, file, line, &lexer->src_loc, "invalid escape char `%c`", c);
    }
    estr->len++;
    c = *(++estr->end);
  }
  if(success)
  {
    if(*estr->end != estr->quote)
      success = compile_error_(lexer->arena, file, line, &lexer->src_loc, 
                                "malformed string literal, missing the closing `%c`", estr->quote);
  }
  assert((estr->end - estr->begin) >= 1);
  return success;
}

char* install_escaped_str(MemoryArena* arena, EscapedStr* estr)
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

    case eToken_xor:
      result = "xor";
    break;

    case eToken_not:
      result = "not";
    break;

    case eToken_mod:
      result = "mod";
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
      print_char(result = char_print_buf, token->char_val);
    break;

    default:
      result = "???";
  }
  return result;
}

Lexer* lexer_new(MemoryArena* arena)
{
  Lexer* lexer = mem_push_struct(arena, Lexer);
  lexer->arena = arena;

  return lexer;
}

void lexer_set_input(Lexer* lexer, char* text, char* file_path)
{
  lexer->text = text;
  lexer->cursor = lexer->text;

  SourceLoc* src_loc = &lexer->src_loc;
  src_loc->line_nr = 1;
  /* TODO: Compute the absolute path to the file, so that Vim could properly
     jump from the QuickFix window to the error line in the file. */
  src_loc->file_path = file_path;

  lexer->last_state = mem_push_struct(lexer->arena, Lexer);
  *lexer->last_state = *lexer;
}

void lexer_putback_token(Lexer* lexer)
{
  *lexer = *lexer->last_state;
}

Token* get_prev_token(Lexer* lexer)
{
  Token* token = &lexer->token;
  if(lexer->last_state)
    token = &lexer->last_state->token;
  return token;
}

/* Returns the first non-whitepace char. */
char skip_whitespace(Lexer* lexer, char* whitechars)
{
  SourceLoc* src_loc = &lexer->src_loc;
  char c = *lexer->cursor;

  while(cstr_contains_char(whitechars, c))
  {
    if(c == '\n')
    {
      src_loc->line_nr++;
      src_loc->src_line = lexer->cursor;
    }
    c = *(++lexer->cursor);
  }
  return c;
}

bool get_asm_text(Lexer* lexer)
{
  bool success = true;
  *lexer->last_state = *lexer;
  mem_zero_struct(&lexer->token, Token);
  SourceLoc* src_loc = &lexer->src_loc;
  src_loc->src_line = lexer->cursor;
  char c;
  Token* token = &lexer->token;

  skip_whitespace(lexer, " \r\n\t");
  char* begin_char = lexer->cursor;

  c = *begin_char;
  while(c != '\0' && c != '}')
  {
    c = *(++lexer->cursor);
    c = skip_whitespace(lexer, " \r\n\t");
  }

  if(c == '}')
  {
    char* end_char = lexer->cursor - 1;
    char* lexeme = install_lexeme(lexer->arena, begin_char, end_char);

    token->kind = eToken_asm_text;
    token->lexeme = lexeme;
  }
  else if(c == '\0')
  {
    token->kind = eToken_end_of_input;
  }
  else
  {
    token->kind = eToken_unknown_char;
    token->char_val = c;
  }
  return success;
}

bool lexer_get_next_token(Lexer* lexer)
{
  bool success = true;
  *lexer->last_state = *lexer;
  mem_zero_struct(&lexer->token, Token);
  SourceLoc* src_loc = &lexer->src_loc;
  src_loc->src_line = lexer->cursor;
  char c;

  Token* token = &lexer->token;
loop:
  skip_whitespace(lexer, " \r\n\t");
  c = *lexer->cursor;

  if(char_is_letter(c) || c == '_')
  {
    char* begin_char = lexer->cursor;
    c = *(++lexer->cursor);

    while(char_is_letter(c) || char_is_dec_digit(c) || c == '_')
    {
      c = *(++lexer->cursor);
    }

    char* end_char = lexer->cursor - 1;
    char* lexeme = install_lexeme(lexer->arena, begin_char, end_char);

    token->kind = eToken_id;
    token->lexeme = lexeme;
    Token* keyword = lookup_keyword(keyword_list, lexeme);
    if(keyword)
    {
      token->kind = keyword->kind;
    }
  }
  else if(char_is_dec_digit(c))
  {
    char digit_buf[32] = {0};
    bool is_float = false;
    bool is_hex = false;

    int i = 0;
    digit_buf[i++] = c;
    c = *(++lexer->cursor);

    if(c == 'x') // hexadecimal
    {
      is_hex = true;
      c = *(++lexer->cursor);

      for(; i < sizeof_array(digit_buf)-1 && char_is_hex_digit(c);
          i++)
      {
        digit_buf[i] = c;
        c = *(++lexer->cursor);
      }
    }
    else if(char_is_dec_digit(c) || c == '.')
    {
      for(; i < sizeof_array(digit_buf)-1 && ((char_is_dec_digit(c) || c == '.'));
          i++)
      {
        digit_buf[i] = c;
        if(c == '.')
        {
          if(is_float)
            break;
          is_float = true;
        }
        c = *(++lexer->cursor);
      }
    }
    digit_buf[i] = '\0';
    token->lexeme = install_lexeme(lexer->arena, digit_buf, digit_buf + i-1);

    if(is_float)
    {
      token->kind = eToken_float_val;
      token->float_val = mem_push_struct(lexer->arena, float);
      platform_sscanf(digit_buf, "%f", token->float_val);
    }
    else
    {
      token->kind = eToken_int_val;
      token->int_val = mem_push_struct(lexer->arena, int);
      if(is_hex)
        platform_sscanf(digit_buf, "%x", token->int_val);
      else
        platform_sscanf(digit_buf, "%d", token->int_val);
    }
  }
  else if(c == '-')
  {
    token->kind = eToken_minus;
    c = *(++lexer->cursor);
    if(c == '-')
    {
      token->kind = eToken_minus_minus;
      ++lexer->cursor;
    }
    else if(c == '>')
    {
      token->kind = eToken_arrow_right;
      ++lexer->cursor;
    }
  }
  else if(c == '<')
  {
    token->kind = eToken_angle_left;
    c = *(++lexer->cursor);
    if(c == '=')
    {
      token->kind = eToken_angle_left_eq;
      ++lexer->cursor;
    }
    else if(c == '<')
    {
      token->kind = eToken_angle_left_left;
      ++lexer->cursor;
    }
    else if(c == '>')
    {
      token->kind = eToken_angle_left_right;
      ++lexer->cursor;
    }
  }
  else if(c == '&')
  {
    token->kind = eToken_ampersand;
    c = *(++lexer->cursor);
  }
  else if(c == '/')
  {
    /* multi-line comments */
    char* fwd_cursor = lexer->cursor;

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
            src_loc->src_line = lexer->cursor;
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
      lexer->cursor = ++fwd_cursor;
      goto loop;
    }
    else
    {
      token->kind = eToken_fwd_slash;
      ++lexer->cursor;
    }
  }
  else if(c == '"')
  {
    /* double-quoted escaped string */
    EscapedStr estr = {0};
    estr.quote = '"';

    if(success = escaped_string(__FILE__, __LINE__, lexer, &estr))
    {
      token->str_val = install_escaped_str(lexer->arena, &estr);;
      token->kind = eToken_str_val;
      lexer->cursor = ++estr.end;
    }
  }
  else if(c == '\'')
  {
    /* single-quoted escaped char */
    EscapedStr estr = {0};
    estr.quote = '\'';

    if(success = escaped_string(__FILE__, __LINE__, lexer, &estr))
    {
      char* lexeme = install_escaped_str(lexer->arena, &estr);

      if(estr.len != 1)
        success = compile_error(lexer->arena, &lexer->src_loc, "invalid char literal '%s'", lexeme);
      else
      {
        token->char_val = *lexeme;
        token->kind = eToken_char_val;
        lexer->cursor = ++estr.end;
      }
    }
  }
  else if(c == '=')
  {
    token->kind = eToken_eq;
    c = *(++lexer->cursor);
    if(c == '=')
    {
      token->kind = eToken_eq_eq;
      ++lexer->cursor;
    }
  }
  else if(c == '>')
  {
    token->kind = eToken_angle_right;
    c = *(++lexer->cursor);
    if(c == '=')
    {
      token->kind = eToken_angle_right_eq;
      ++lexer->cursor;
    }
    else if(c == '>')
    {
      token->kind = eToken_angle_right_right;
      ++lexer->cursor;
    }
  }
  else if(c == '|')
  {
    token->kind = eToken_pipe;
    c = *(++lexer->cursor);
  }
  else if(c == '~')
  {
    token->kind = eToken_tilde;
    c = *(++lexer->cursor);
  }
  else if(c == '!')
  {
    token->kind = eToken_exclam;
    c = *(++lexer->cursor);
#if 0
    if(c == '=')
    {
      token->kind = eToken_exclam_eq;
      ++lexer->cursor;
    }
#endif
  }
  else if(c == 'ª')
  {
    token->kind = eToken_logic_not;
    c = *(++lexer->cursor);
  }
  else if(c == '+')
  {
    token->kind = eToken_plus;
    c = *(++lexer->cursor);
    if(c == '+')
    {
      token->kind = eToken_plus_plus;
      ++lexer->cursor;
    }
  }
  else if(c == '*')
  {
    token->kind = eToken_star;
    ++lexer->cursor;
  }
  else if(c == '×')
  {
    token->kind = eToken_mul;
    c = *(++lexer->cursor);
  }
  else if(c == '^')
  {
    token->kind = eToken_circumflex;
    ++lexer->cursor;
  }
  else if(c == '\\')
  {
    token->kind = eToken_back_slash;
    ++lexer->cursor;
  }
  else if(c == '.')
  {
    token->kind = eToken_dot;
    ++lexer->cursor;
  }
  else if(c == '}')
  {
    token->kind = eToken_close_brace;
    ++lexer->cursor;
  }
  else if(c == '{')
  {
    token->kind = eToken_open_brace;
    ++lexer->cursor;
  }
  else if(c == '(')
  {
    token->kind = eToken_open_parens;
    ++lexer->cursor;
  }
  else if(c == ')')
  {
    token->kind = eToken_close_parens;
    ++lexer->cursor;
  }
  else if(c == ';')
  {
    token->kind = eToken_semicolon;
    ++lexer->cursor;
  }
  else if(c == ',')
  {
    token->kind = eToken_comma;
    ++lexer->cursor;
  }
  else if(c == ':')
  {
    token->kind = eToken_colon;
    ++lexer->cursor;
  }
  else if(c == '[')
  {
    token->kind = eToken_open_bracket;
    ++lexer->cursor;
  }
  else if(c == ']')
  {
    token->kind = eToken_close_bracket;
    ++lexer->cursor;
  }
  else if(c == '\0')
  {
    token->kind = eToken_end_of_input;
  }
  else
  {
    token->kind = eToken_unknown_char;
    token->char_val = c;
  }
  return success;
}

