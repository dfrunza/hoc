Token keyword_list[] = 
{
  {eToken_var, "var"},
  {eToken_asm, "asm"},
  {eToken_type, "type"},
  {eToken_if, "if"},
  {eToken_else, "else"},
  {eToken_while, "while"},
  {eToken_for, "for"},
  {eToken_return, "return"},
  {eToken_break, "break"},
  {eToken_continue, "continue"},
  {eToken_goto, "goto"},
  {eToken_include, "include"},
  {eToken_true, "true"},
  {eToken_false, "false"},
  {eToken_proc, "proc"},
  {eToken_struct, "struct"},
  {eToken_union, "union"},
  {eToken_enum, "enum"},
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

bool is_keyword_token(eToken kind)
{
  return (kind >= eToken_if) && (kind <= eToken_false);
}

bool is_literal_token(eToken kind)
{
  return (kind >= eToken_int_num) && (kind <= eToken_char);
}

char* install_lexeme(char* begin_char, char* end_char)
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

bool escaped_string(char* file, int line, TokenStream* input, EscapedStr* estr)
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

char* install_escaped_str(EscapedStr* estr)
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
  static char char_print_buf[3] = {0};
  char* result = 0;

  if(token->kind == eToken_dot)
    result = ".";
  else if(token->kind == eToken_arrow_right)
    result = "->";
  else if(token->kind == eToken_open_bracket)
    result = "[";
  else if(token->kind == eToken_close_bracket)
    result = "]";
  else if(token->kind == eToken_open_parens)
    result = "(";
  else if(token->kind == eToken_close_parens)
    result = ")";
  else if(token->kind == eToken_open_brace)
    result = "{";
  else if(token->kind == eToken_close_brace)
    result = "}";
  else if(token->kind == eToken_semicolon)
    result = ";";
  else if(token->kind == eToken_colon)
    result = ":";
  else if(token->kind == eToken_comma)
    result = ",";
  else if(token->kind == eToken_percent)
    result = "%";
  else if(token->kind == eToken_star)
    result = "*";
  else if(token->kind == eToken_fwd_slash)
    result = "/";
  else if(token->kind == eToken_back_slash)
    result = "\\";
  else if(token->kind == eToken_plus)
    result = "+";
  else if(token->kind == eToken_plus_plus)
    result = "++";
  else if(token->kind == eToken_minus)
    result = "-";
  else if(token->kind == eToken_minus_minus)
    result = "--";
  else if(token->kind == eToken_exclam)
    result = "!";
  else if(token->kind == eToken_exclam_eq)
    result = "!=";
  else if(token->kind == eToken_eq)
    result = "=";
  else if(token->kind == eToken_eq_eq)
    result = "==";
  else if(token->kind == eToken_angle_right)
    result = ">";
  else if(token->kind == eToken_angle_right_eq)
    result = ">=";
  else if(token->kind == eToken_angle_left)
    result = "<";
  else if(token->kind == eToken_angle_left_eq)
    result = "<=";
  else if(token->kind == eToken_ampersand)
    result = "&";
  else if(token->kind == eToken_ampersand_ampersand)
    result = "&&";
  else if(token->kind == eToken_pipe)
    result = "|";
  else if(token->kind == eToken_pipe_pipe)
    result = "||";
  else if(token->kind == eToken_end_of_input)
    result = "end-of-input";
  else if(token->kind == eToken_var)
    result = "var";
  else if(token->kind == eToken_if)
    result = "if";
  else if(token->kind == eToken_else)
    result = "else";
  else if(token->kind == eToken_while)
    result = "while";
  else if(token->kind == eToken_for)
    result = "for";
  else if(token->kind == eToken_proc)
    result = "proc";
  else if(token->kind == eToken_struct)
    result = "struct";
  else if(token->kind == eToken_union)
    result = "union";
  else if(token->kind == eToken_return)
    result = "return";
  else if(token->kind == eToken_break)
    result = "break";
  else if(token->kind == eToken_continue)
    result = "continue";
  else if(token->kind == eToken_include)
    result = "include";
  else if(token->kind == eToken_enum)
    result = "enum";
  else if(token->kind == eToken_type)
    result = "type";
  else if(token->kind == eToken_goto)
    result = "goto";
  else if(token->kind == eToken_true)
    result = "true";
  else if(token->kind == eToken_false)
    result = "false";
  else if(token->kind == eToken_id ||
          token->kind == eToken_int_num ||
          token->kind == eToken_float_num)
  {
    result = token->lexeme;
  }
  else if(token->kind == eToken_string)
    result = token->str_val; // TODO: Substitute non-printable chars
  else if(token->kind == eToken_char || token->kind == eToken_unknown_char)
  {
    print_char(result = char_print_buf, token->char_val);
  }
  else
    result = "???";

  return result;
}

void init_token_stream(TokenStream* stream, char* text, char* file_path)
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

void putback_token(TokenStream* input)
{
  *input = *input->prev_state;
}

Token* get_prev_token(TokenStream* input)
{
  Token* token = &input->token;
  if(input->prev_state)
    token = &input->prev_state->token;
  return token;
}

/* Returns the first non-whitepace char. */
char skip_whitespace(TokenStream* input, char* whitechars)
{
  SourceLoc* src_loc = &input->src_loc;
  char c = *input->cursor;

  while(cstr_contains_char(whitechars, c))
  {
    if(c == '\n')
    {
      src_loc->line_nr++;
      src_loc->src_line = input->cursor;
    }
    c = *(++input->cursor);
  }
  return c;
}

bool get_asm_text(TokenStream* input)
{
  bool success = true;
  *input->prev_state = *input;
  mem_zero_struct(&input->token, Token);
  SourceLoc* src_loc = &input->src_loc;
  src_loc->src_line = input->cursor;
  char c;
  Token* token = &input->token;

  skip_whitespace(input, " \r\n\t");
  char* begin_char = input->cursor;

  c = *begin_char;
  while(c != '\0' && c != '}')
  {
    c = *(++input->cursor);
    c = skip_whitespace(input, " \r\n\t");
  }

  if(c == '}')
  {
    char* end_char = input->cursor - 1;
    char* lexeme = install_lexeme(begin_char, end_char);

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

bool get_next_token(TokenStream* input)
{
  bool success = true;
  *input->prev_state = *input;
  mem_zero_struct(&input->token, Token);
  SourceLoc* src_loc = &input->src_loc;
  src_loc->src_line = input->cursor;
  char c;

  Token* token = &input->token;
loop:
  skip_whitespace(input, " \r\n\t");
  c = *input->cursor;

  if(char_is_letter(c) || c == '_')
  {
    char* begin_char = input->cursor;
    c = *(++input->cursor);

    while(char_is_letter(c) || char_is_numeric(c) || c == '_')
    {
      c = *(++input->cursor);
    }

    char* end_char = input->cursor - 1;
    char* lexeme = install_lexeme(begin_char, end_char);

    token->kind = eToken_id;
    token->lexeme = lexeme;
    Token* keyword = lookup_keyword(keyword_list, lexeme);
    if(keyword)
    {
      token->kind = keyword->kind;
    }
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
      token->kind = eToken_float_num;
      token->float_val = mem_push_struct(arena, float);
      h_sscanf(digit_buf, "%f", token->float_val);
    }
    else
    {
      token->kind = eToken_int_num;
      token->int_val = mem_push_struct(arena, int);
      h_sscanf(digit_buf, "%d", token->int_val);
    }
  }
  else if(c == '-')
  {
    token->kind = eToken_minus;
    c = *(++input->cursor);
    if(c == '-')
    {
      token->kind = eToken_minus_minus;
      ++input->cursor;
    }
    else if(c == '>')
    {
      token->kind = eToken_arrow_right;
      ++input->cursor;
    }
  }
  else if(c == '<')
  {
    token->kind = eToken_angle_left;
    c = *(++input->cursor);
    if(c == '=')
    {
      token->kind = eToken_angle_left_eq;
      ++input->cursor;
    }
  }
  else if(c == '&')
  {
    token->kind = eToken_ampersand;
    c = *(++input->cursor);
    if(c == '&')
    {
      token->kind = eToken_ampersand_ampersand;
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
      token->kind = eToken_fwd_slash;
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
      token->str_val = install_escaped_str(&estr);;
      token->kind = eToken_string;
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
        token->kind = eToken_char;
        input->cursor = ++estr.end;
      }
    }
  }
  else if(c == '=')
  {
    token->kind = eToken_eq;
    c = *(++input->cursor);
    if(c == '=')
    {
      token->kind = eToken_eq_eq;
      ++input->cursor;
    }
  }
  else if(c == '>')
  {
    token->kind = eToken_angle_right;
    c = *(++input->cursor);
    if(c == '=')
    {
      token->kind = eToken_angle_right_eq;
      ++input->cursor;
    }
  }
  else if(c == '|')
  {
    token->kind = eToken_pipe;
    c = *(++input->cursor);
    if(c == '|')
    {
      token->kind = eToken_pipe_pipe;
      ++input->cursor;
    }
  }
  else if(c == '!')
  {
    token->kind = eToken_exclam;
    c = *(++input->cursor);
    if(c == '=')
    {
      token->kind = eToken_exclam_eq;
      ++input->cursor;
    }
  }
  else if(c == '+')
  {
    token->kind = eToken_plus;
    c = *(++input->cursor);
    if(c == '+')
    {
      token->kind = eToken_plus_plus;
      ++input->cursor;
    }
  }
  else if(c == '*')
  {
    token->kind = eToken_star;
    ++input->cursor;
  }
  else if(c == '%')
  {
    token->kind = eToken_percent;
    ++input->cursor;
  }
  else if(c == '\\')
  {
    token->kind = eToken_back_slash;
    ++input->cursor;
  }
  else if(c == '.')
  {
    token->kind = eToken_dot;
    ++input->cursor;
  }
  else if(c == '}')
  {
    token->kind = eToken_close_brace;
    ++input->cursor;
  }
  else if(c == '{')
  {
    token->kind = eToken_open_brace;
    ++input->cursor;
  }
  else if(c == '(')
  {
    token->kind = eToken_open_parens;
    ++input->cursor;
  }
  else if(c == ')')
  {
    token->kind = eToken_close_parens;
    ++input->cursor;
  }
  else if(c == ';')
  {
    token->kind = eToken_semicolon;
    ++input->cursor;
  }
  else if(c == ',')
  {
    token->kind = eToken_comma;
    ++input->cursor;
  }
  else if(c == ':')
  {
    token->kind = eToken_colon;
    ++input->cursor;
  }
  else if(c == '[')
  {
    token->kind = eToken_open_bracket;
    ++input->cursor;
  }
  else if(c == ']')
  {
    token->kind = eToken_close_bracket;
    ++input->cursor;
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
