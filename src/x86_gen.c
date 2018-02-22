void init_x86_registers(X86Context* ctx)
{
  ctx->register_count = 0;

  /* eax */
  X86Location* loc = &ctx->eax;
  loc->kind = eX86Location_eax;
  loc->type = ctx->basic_type_int;
  ctx->registers._[ctx->register_count++] = loc;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* al */
  X86Location* subloc = &ctx->al;
  loc->subloc[0] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_al;
  subloc->type = ctx->basic_type_char;
  ctx->registers._[ctx->register_count++] = subloc;
  list_init(&subloc->occupants, ctx->gp_arena, eList_symbol);

  /* ah */
  subloc = &ctx->ah;
  loc->subloc[1] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_ah;
  subloc->type = ctx->basic_type_char;
  ctx->registers._[ctx->register_count++] = subloc;
  list_init(&subloc->occupants, ctx->gp_arena, eList_symbol);

  /* ebx */
  loc = &ctx->ebx;
  loc->kind = eX86Location_ebx;
  loc->type = ctx->basic_type_int;
  ctx->registers._[ctx->register_count++] = loc;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* bl */
  subloc = &ctx->bl;
  loc->subloc[0] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_bl;
  subloc->type = ctx->basic_type_char;
  ctx->registers._[ctx->register_count++] = subloc;
  list_init(&subloc->occupants, ctx->gp_arena, eList_symbol);

  /* bh */
  subloc = &ctx->bh;
  loc->subloc[1] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_bh;
  subloc->type = ctx->basic_type_char;
  ctx->registers._[ctx->register_count++] = subloc;
  list_init(&subloc->occupants, ctx->gp_arena, eList_symbol);

  /* ecx */
  loc = &ctx->ecx;
  loc->kind = eX86Location_ecx;
  loc->type = ctx->basic_type_int;
  ctx->registers._[ctx->register_count++] = loc;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* cl */
  subloc = &ctx->cl;
  loc->subloc[0] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_cl;
  subloc->type = ctx->basic_type_char;
  ctx->registers._[ctx->register_count++] = subloc;
  list_init(&subloc->occupants, ctx->gp_arena, eList_symbol);

  /* ch */
  subloc = &ctx->ch;
  loc->subloc[1] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_ch;
  subloc->type = ctx->basic_type_char;
  ctx->registers._[ctx->register_count++] = subloc;
  list_init(&subloc->occupants, ctx->gp_arena, eList_symbol);

  /* edx */
  loc = &ctx->edx;
  loc->kind = eX86Location_edx;
  loc->type = ctx->basic_type_int;
  ctx->registers._[ctx->register_count++] = loc;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* dl */
  subloc = &ctx->dl;
  loc->subloc[0] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_dl;
  subloc->type = ctx->basic_type_char;
  ctx->registers._[ctx->register_count++] = subloc;
  list_init(&subloc->occupants, ctx->gp_arena, eList_symbol);

  /* dh */
  subloc = &ctx->dh;
  loc->subloc[1] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_dh;
  subloc->type = ctx->basic_type_char;
  ctx->registers._[ctx->register_count++] = subloc;
  list_init(&subloc->occupants, ctx->gp_arena, eList_symbol);

  /* esi */
  loc = &ctx->esi;
  loc->kind = eX86Location_esi;
  loc->type = ctx->basic_type_int;
  ctx->registers._[ctx->register_count++] = loc;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* edi */
  loc = &ctx->edi;
  loc->kind = eX86Location_edi;
  loc->type = ctx->basic_type_int;
  ctx->registers._[ctx->register_count++] = loc;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* memory */
  loc = &ctx->memory;
  loc->kind = eX86Location_memory;
  loc->type = ctx->basic_type_void;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* ebp */
  loc = &ctx->ebp;
  loc->kind = eX86Location_ebp;
  loc->type = ctx->basic_type_int;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* esp */
  loc = &ctx->esp;
  loc->kind = eX86Location_esp;
  loc->type = ctx->basic_type_int;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* xmm0 */
  loc = &ctx->xmm0;
  loc->kind = eX86Location_xmm0;
  loc->type = ctx->basic_type_float;
  ctx->registers._[ctx->register_count++] = loc;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* xmm1 */
  loc = &ctx->xmm1;
  loc->kind = eX86Location_xmm1;
  loc->type = ctx->basic_type_float;
  ctx->registers._[ctx->register_count++] = loc;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* xmm2 */
  loc = &ctx->xmm2;
  loc->kind = eX86Location_xmm2;
  loc->type = ctx->basic_type_float;
  ctx->registers._[ctx->register_count++] = loc;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* xmm3 */
  loc = &ctx->xmm3;
  loc->kind = eX86Location_xmm3;
  loc->type = ctx->basic_type_float;
  ctx->registers._[ctx->register_count++] = loc;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* xmm4 */
  loc = &ctx->xmm4;
  loc->kind = eX86Location_xmm4;
  loc->type = ctx->basic_type_float;
  ctx->registers._[ctx->register_count++] = loc;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* xmm5 */
  loc = &ctx->xmm5;
  loc->kind = eX86Location_xmm5;
  loc->type = ctx->basic_type_float;
  ctx->registers._[ctx->register_count++] = loc;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* xmm6 */
  loc = &ctx->xmm6;
  loc->kind = eX86Location_xmm6;
  loc->type = ctx->basic_type_float;
  ctx->registers._[ctx->register_count++] = loc;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  /* xmm7 */
  loc = &ctx->xmm7;
  loc->kind = eX86Location_xmm7;
  loc->type = ctx->basic_type_float;
  ctx->registers._[ctx->register_count++] = loc;
  list_init(&loc->occupants, ctx->gp_arena, eList_symbol);

  assert(ctx->register_count <= sizeof_array(ctx->registers._));
}

void init_x86_context(X86Context* ctx, MemoryArena* gp_arena, MemoryArena* stmt_arena, MemoryArena* text_arena,
                      TypePass* type_pass, IrPass* ir_context, SymbolPass* sym_pass)
{
  ctx->basic_type_bool  = type_pass->basic_type_bool;
  ctx->basic_type_int   = type_pass->basic_type_int;
  ctx->basic_type_char  = type_pass->basic_type_char;
  ctx->basic_type_float = type_pass->basic_type_float;
  ctx->basic_type_void  = type_pass->basic_type_void;
  ctx->basic_type_str   = type_pass->basic_type_str;

  ctx->gp_arena = gp_arena;
  ctx->stmt_arena = stmt_arena;
  ctx->stmt_array = (X86Stmt*)ctx->stmt_arena->base;
  ctx->machine_word_size = 4;
  ctx->data_alignment = 4;
  init_x86_registers(ctx);
  ctx->text = str_new(text_arena);

  ir_context->x86_context = ctx;
  sym_pass->x86_context = ctx;
}

void print_register(String* text, X86Location* reg)
{
  switch(reg->kind)
  {
    /* 8-bit */
    case eX86Location_al:
      str_append(text, "al");
    break;

    case eX86Location_ah:
      str_append(text, "ah");
    break;

    case eX86Location_bl:
      str_append(text, "bl");
    break;

    case eX86Location_bh:
      str_append(text, "bh");
    break;

    case eX86Location_cl:
      str_append(text, "cl");
    break;

    case eX86Location_ch:
      str_append(text, "ch");
    break;

    case eX86Location_dl:
      str_append(text, "dl");
    break;

    case eX86Location_dh:
      str_append(text, "dh");
    break;

    /* 32-bit */
    case eX86Location_eax:
      str_append(text, "eax");
    break;

    case eX86Location_ebx:
      str_append(text, "ebx");
    break;

    case eX86Location_ecx:
      str_append(text, "ecx");
    break;

    case eX86Location_edx:
      str_append(text, "edx");
    break;

    case eX86Location_ebp:
      str_append(text, "ebp");
    break;

    case eX86Location_esp:
      str_append(text, "esp");
    break;

    case eX86Location_esi:
      str_append(text, "esi");
    break;

    case eX86Location_edi:
      str_append(text, "edi");
    break;

    /* xmm */
    case eX86Location_xmm0:
      str_append(text, "xmm0");
    break;

    case eX86Location_xmm1:
      str_append(text, "xmm1");
    break;

    case eX86Location_xmm2:
      str_append(text, "xmm2");
    break;

    case eX86Location_xmm3:
      str_append(text, "xmm3");
    break;

    case eX86Location_xmm4:
      str_append(text, "xmm4");
    break;

    case eX86Location_xmm5:
      str_append(text, "xmm5");
    break;

    case eX86Location_xmm6:
      str_append(text, "xmm6");
    break;

    case eX86Location_xmm7:
      str_append(text, "xmm7");
    break;

    default: assert(0);
  }
}

char* make_x86_type_directive(X86Context* ctx, Type* type)
{
  char* directive = "";

  if(type_eq(type, ctx->basic_type_char))
  {
    directive = "byte";
  }
  else if(type_eq(type, ctx->basic_type_int) || type_eq(type, ctx->basic_type_float)
          || type_eq(type, ctx->basic_type_bool) || (type->kind == eType_pointer) || (type->kind == eType_array))
  {
    directive = "dword";
  }
  else assert(0);

  return directive;
}

void print_x86_operand(X86Context* ctx, String* text, X86Operand* operand)
{
  switch(operand->kind)
  {
    case eX86Operand_memory:
    case eX86Operand_address:
    {
      if(operand->kind == eX86Operand_memory)
      {
        str_format(text, "%s ptr [", make_x86_type_directive(ctx, operand->index.type));
      }
      else if(operand->kind == eX86Operand_address)
      {
        str_append(text, "[");
      }
      else assert(0);

      X86Operand* base = operand->index.base;
      print_x86_operand(ctx, text, base);

      X86Operand* offset = operand->index.offset;
      if(offset)
      {
        char* sign = "+";
        if(offset->kind == eX86Operand_constant)
        {
          struct X86Operand_Constant* constant = &offset->constant;
          assert(constant->kind == eX86Constant_int);

          if(constant->int_val < 0)
          {
            sign = "";
          }
        }
        str_format(text, "%s", sign);
        print_x86_operand(ctx, text, offset);
      }
      str_append(text, "]");
    }
    break;

    case eX86Operand_register:
    {
      print_register(text, operand->reg);
    }
    break;

    case eX86Operand_constant:
    {
      struct X86Operand_Constant* constant = &operand->constant;

      if(constant->kind == eX86Constant_int)
      {
        str_format(text, "%d", constant->int_val);
      }
      else if(constant->kind == eX86Constant_float)
      {
        union BitcastFtoI
        {
          float float_val;
          int int_val;
        };

        union BitcastFtoI val = {0};
        val.float_val = constant->float_val;

        str_format(text, "0%xh", val.int_val);
      }
      else if(constant->kind == eX86Constant_char)
      {
        if(constant->char_val >= ' ' && constant->char_val <= '~')
          str_format(text, "'%c'", constant->char_val);
        else
          str_format(text, "%d", constant->char_val);
      }
      else assert(0);
    }
    break;

    case eX86Operand_id:
    {
      str_format(text, "%s", operand->id);
    }
    break;

    default: assert(0);
  }
}

void print_x86_opcode(String* text, eX86Stmt opcode)
{
  switch(opcode)
  {
    case eX86Stmt_call:
      str_append(text, "call ");
    break;

    case eX86Stmt_pop:
      str_append(text, "pop ");
    break;

    case eX86Stmt_push:
      str_append(text, "push ");
    break;

    case eX86Stmt_lea:
      str_append(text, "lea ");
    break;

    /* integer ops */
    case eX86Stmt_mov:
      str_append(text, "mov ");
    break;

    case eX86Stmt_add:
      str_append(text, "add ");
    break;

    case eX86Stmt_sub:
      str_append(text, "sub ");
    break;

    case eX86Stmt_imul:
      str_append(text, "imul ");
    break;

    case eX86Stmt_cdq:
      str_append(text, "cdq");
    break;

    case eX86Stmt_idiv:
      str_append(text, "idiv ");
    break;

    case eX86Stmt_neg:
      str_append(text, "neg ");
    break;

    case eX86Stmt_cmp:
      str_append(text, "cmp ");
    break;

    case eX86Stmt_or:
      str_append(text, "or ");
    break;

    case eX86Stmt_and:
      str_append(text, "and ");
    break;

    case eX86Stmt_not:
      str_append(text, "not ");
    break;

    /* integer jumps */
    case eX86Stmt_jz:
      str_append(text, "jz ");
    break;

    case eX86Stmt_jnz:
      str_append(text, "jnz ");
    break;

    case eX86Stmt_jl:
      str_append(text, "jl ");
    break;

    case eX86Stmt_jle:
      str_append(text, "jle ");
    break;

    case eX86Stmt_jg:
      str_append(text, "jg ");
    break;

    case eX86Stmt_jge:
      str_append(text, "jge ");
    break;

    /* floating point ops */
    case eX86Stmt_movss:
      str_append(text, "movss ");
    break;

    case eX86Stmt_addss:
      str_append(text, "addss ");
    break;

    case eX86Stmt_subss:
      str_append(text, "subss ");
    break;

    case eX86Stmt_mulss:
      str_append(text, "mulss ");
    break;

    case eX86Stmt_divss:
      str_append(text, "divss ");
    break;

    case eX86Stmt_ucomiss:
      str_append(text, "ucomiss ");
    break;

    /* floating point jumps */
    case eX86Stmt_jb:
      str_append(text, "jb ");
    break;

    case eX86Stmt_jbe:
      str_append(text, "jbe ");
    break;

    case eX86Stmt_ja:
      str_append(text, "ja ");
    break;

    case eX86Stmt_jae:
      str_append(text, "jae ");
    break;

    case eX86Stmt_je:
      str_append(text, "je ");
    break;

    case eX86Stmt_jne:
      str_append(text, "jne ");
    break;

    case eX86Stmt_jmp:
      str_append(text, "jmp ");
    break;

    case eX86Stmt_nop:
      //str_printfln("nop");
    break;

    case eX86Stmt_ret:
      str_append(text, "ret ");
    break;

    /* conversion ops */
    case eX86Stmt_cvtsi2ss:
      str_append(text, "cvtsi2ss ");
    break;

    case eX86Stmt_cvttss2si:
      str_append(text, "cvttss2si ");
    break;

    default: assert(0);
  }
}

void print_x86_stmt(X86Context* ctx, String* text, X86Stmt* stmt)
{
  switch(stmt->opcode)
  {
    case eX86Stmt_label:
    {
      print_x86_operand(ctx, text, stmt->operand1);
      str_append(text, ":");
    }
    break;

    case eX86Stmt_extern_proc:
    {
      str_append(text, "extern ");
      print_x86_operand(ctx, text, stmt->operand1);
      str_append(text, ":proc");
    }
    break;

    default:
    {
      print_x86_opcode(text, stmt->opcode);
      if(stmt->operand1)
      {
        print_x86_operand(ctx, text, stmt->operand1);
      }

      if(stmt->operand2)
      {
        str_append(text, ", ");
        print_x86_operand(ctx, text, stmt->operand2);
      }
    }
  }

  str_nl(text);
}

X86Stmt* new_x86_stmt(X86Context* ctx, eX86Stmt opcode)
{
  X86Stmt* stmt = push_struct(ctx->stmt_arena, X86Stmt);
  ctx->stmt_count++;
  stmt->opcode = opcode;
  stmt->operand1 = stmt->operand2 = 0;

  return stmt;
}

bool is_object_in_location(Symbol* obj, X86Location* loc)
{
  return (obj->locations._[loc->kind] != 0);
}

void new_object_location_entry(Symbol* object, X86Location* loc)
{
  object->locations._[loc->kind] = loc;

  List* occupants = &loc->occupants;
  for(ListItem* li = occupants->first;
      li;
      li = li->next)
  {
    assert(object != KIND(li, eList_symbol)->symbol);
  }
  list_append(occupants, object, eList_symbol);
}

void remove_object_from_location(Symbol* object, X86Location* loc)
{
  if(is_object_in_location(object, loc))
  {
    object->locations._[loc->kind] = 0;

    List* occupants = &loc->occupants;
    ListItem* li = occupants->first;

    for(; li; li = li->next)
    {
      if(object == KIND(li, eList_symbol)->symbol)
        break;
    }
    if(li)
    {
      list_remove_item(occupants, li);
    }
  }
}

void add_object_to_location(Symbol* object, X86Location* loc)
{
  if(is_object_in_location(object, loc))
  {
    List* occupants = &loc->occupants;
    ListItem* li = occupants->first;
    for(; li; li = li->next)
    {
      if(object == KIND(li, eList_symbol)->symbol)
        break;
    }
    assert(li);
  }
  else
  {
    new_object_location_entry(object, loc);
  }
}

bool X86Location_is_parent_free(X86Location* loc)
{
  bool result = true;

  if(loc->occupants.count == 0)
  {
    if(loc->parent)
    {
      result = X86Location_is_parent_free(loc->parent);
    }
  }
  else
  {
    result = false;
  }

  return result;
}

bool X86Location_is_subloc_free(X86Location* loc)
{
  bool result = true;

  if(loc->occupants.count == 0)
  {
    if(loc->subloc[0])
    {
      result = X86Location_is_subloc_free(loc->subloc[0]);
    }

    if(result && loc->subloc[1])
    {
      result = X86Location_is_subloc_free(loc->subloc[1]);
    }
  }
  else
  {
    result = false;
  }

  return result;
}

bool X86Location_is_free(X86Location* loc)
{
  bool result = true;

  if(loc->occupants.count == 0)
  {
    if(loc->parent)
    {
      result = X86Location_is_parent_free(loc->parent);
    }

    if(result && loc->subloc[0])
    {
      result = X86Location_is_subloc_free(loc->subloc[0]);
    }

    if(result && loc->subloc[1])
    {
      result = X86Location_is_subloc_free(loc->subloc[1]);
    }
  }
  else
  {
    result = false;
  }

  return result;
}

X86Location* X86Location_get_top(X86Location* loc)
{
  X86Location* top = loc;

  if(loc->parent)
  {
    top = X86Location_get_top(loc->parent);
  }

  return top;
}

void clean_register(X86Location* loc)
{
  List* occupants = &loc->occupants;
  for(ListItem* li = occupants->first;
      li;
      li = li->next)
  {
    Symbol* occupant = KIND(li, eList_symbol)->symbol;
    remove_object_from_location(occupant, loc);
  }
}

void clean_register_all_sizes(X86Location* loc)
{
  clean_register(X86Location_get_top(loc));

  if(loc->subloc[0])
  {
    clean_register(loc->subloc[0]);
  }

  if(loc->subloc[1])
  {
    clean_register(loc->subloc[1]);
  }
}

void set_exclusive_object_location(X86Context* ctx, Symbol* object, X86Location* loc)
{
  for(int i = 0; i < ctx->register_count; i++)
  {
    remove_object_from_location(object, ctx->registers._[i]);
  }
  remove_object_from_location(object, &ctx->memory);

  add_object_to_location(object, loc);
}

bool is_register_location(X86Context* ctx, X86Location* loc)
{
  bool is_register = false;

  for(int i = 0;
      i < ctx->register_count && !is_register;
      i++)
  {
    X86Location* reg = ctx->registers._[i];
    is_register = (reg == loc);
  }

  //NOTE: make_register_operand() accepts EBP and ESP parameters.
  is_register |= (loc == &ctx->ebp) | (loc == &ctx->esp);

  return is_register;
}

bool is_memory_location(X86Context* ctx, X86Location* loc)
{
  return (loc == &ctx->memory);
}

bool type_fits_into_register(X86Context* ctx, Type* type, X86Location* reg)
{
  bool result = false;

  if(type_eq(type, ctx->basic_type_bool)
     || type->kind == eType_pointer || type->kind == eType_array)
  {
    result = type_eq(reg->type, ctx->basic_type_int);
  }
  else
  {
    result = type_eq(type, reg->type);
  }

  return result;
}

X86Location* find_free_register(X86Context* ctx, Type* type)
{
  X86Location* reg = 0;

  for(int i = 0; i < ctx->register_count; i++)
  {
    reg = ctx->registers._[i];
    if(X86Location_is_free(reg) && type_fits_into_register(ctx, type, reg))
    {
      break;
    }
    reg = 0;
  }

  return reg;
}

X86Location* lookup_object_location(X86Context* ctx, Symbol* object)
{
  X86Location* loc = 0;

  for(int i = 0; i < ctx->register_count; i++)
  {
    loc = ctx->registers._[i];
    if(is_object_in_location(object, loc))
      break;
    loc = 0;
  }

  if(!loc)
  {
    loc = object->locations._[eX86Location_memory];
  }

  return loc;
}

bool is_object_in_register(X86Context* ctx, Symbol* object)
{
  bool in_register = false;

  for(int i = 0;
      i < ctx->register_count && !in_register;
      i++)
  {
    X86Location* loc = object->locations._[i];
    in_register = (loc && is_register_location(ctx, loc));
  }

  return in_register;
}

bool is_single_occupant_register(X86Context* ctx, X86Location* reg, Symbol* object)
{
  assert(is_register_location(ctx, reg));

  bool result = false;
  List* occupants = &reg->occupants;
  if(occupants->count == 1)
  {
    Symbol* single_object = KIND(occupants->first, eList_symbol)->symbol;
    result = (single_object == object);
  }

  return result;
}

// X86 stack grows toward 0
X86Operand* make_index_x86_operand(X86Context* ctx, eX86Operand kind, Symbol* object)
{
  X86Operand* operand = push_struct(ctx->gp_arena, X86Operand);
  operand->kind = kind;
  operand->index.type = object->ty;

  X86Operand* base = operand->index.base = push_struct(ctx->gp_arena, X86Operand);
  X86Operand* offset = operand->index.offset = push_struct(ctx->gp_arena, X86Operand);

  offset->kind = eX86Operand_constant;
  struct X86Operand_Constant* constant = &offset->constant;
  constant->kind = eX86Constant_int;

  switch(object->storage_space)
  {
    case eStorageSpace_local:
    {
      base->kind = eX86Operand_register;
      base->reg = &ctx->ebp;

      constant->int_val = -(object->data_loc + object->allocd_size);
    }
    break;

    case eStorageSpace_static:
    {
      base->kind = eX86Operand_id;
      base->id = "static_area";

      constant->int_val = object->data_loc;
    }
    break;

    case eStorageSpace_actual_param:
    {
      base->kind = eX86Operand_register;
      base->reg = &ctx->esp;

      constant->int_val = -(object->data_loc + object->allocd_size);
    }
    break;

    case eStorageSpace_formal_param:
    {
      base->kind = eX86Operand_register;
      base->reg = &ctx->ebp;

      int scope_allocd_size = object->scope->allocd_size;
      int machine_area_size = 2*ctx->machine_word_size; // instruction_pointer + frame_pointer
      constant->int_val = (scope_allocd_size + machine_area_size) - (object->data_loc + object->allocd_size);
    }
    break;

    default: assert(0);
  }

  return operand;
}

X86Operand* make_register_x86_operand(X86Context* ctx, X86Location* reg)
{
  assert(is_register_location(ctx, reg));

  X86Operand* operand = push_struct(ctx->gp_arena, X86Operand);
  operand->kind = eX86Operand_register;
  operand->reg = reg;

  return operand;
}

X86Operand* make_int_constant_x86_operand(X86Context* ctx, int int_val)
{
  X86Operand* operand = push_struct(ctx->gp_arena, X86Operand);
  operand->kind = eX86Operand_constant;

  struct X86Operand_Constant* constant = &operand->constant;
  constant->kind = eX86Constant_int;
  constant->int_val = int_val;

  return operand;
}

X86Operand* make_memory_x86_operand(X86Context* ctx, Type* type, X86Operand* base, X86Operand* offset)
{
  X86Operand* operand = push_struct(ctx->gp_arena, X86Operand);
  operand->kind = eX86Operand_memory;

  operand->index.type = type;
  operand->index.base = base;
  operand->index.offset = offset;

  return operand;
}

X86Operand* make_object_address_x86_operand(X86Context* ctx, Symbol* object)
{
  return make_index_x86_operand(ctx, eX86Operand_address, object);
}

X86Operand* make_object_memory_x86_operand(X86Context* ctx, Symbol* object)
{
  X86Operand* operand = 0;

  if(object->kind == eSymbol_constant)
  {
    if(type_eq(object->ty, ctx->basic_type_str))
    {
      operand = make_object_address_x86_operand(ctx, object);
    }
    else
    {
      operand = push_struct(ctx->gp_arena, X86Operand);
      operand->kind = eX86Operand_constant;

      struct X86Operand_Constant* constant = &operand->constant;

      if(type_eq(object->ty, ctx->basic_type_int))
      {
        constant->kind = eX86Constant_int;
        constant->int_val = object->int_val;
      }
      else if(type_eq(object->ty, ctx->basic_type_float))
      {
        operand = make_index_x86_operand(ctx, eX86Operand_memory, object);
      }
      else if(type_eq(object->ty, ctx->basic_type_char))
      {
        constant->kind = eX86Constant_char;
        constant->char_val = object->char_val;
      }
      else if(type_eq(object->ty, ctx->basic_type_bool))
      {
        constant->kind = eX86Constant_int;
        constant->int_val = object->int_val;
      }
      else assert(0);
    }
  }
  else
  {
    operand = make_index_x86_operand(ctx, eX86Operand_memory, object);
  }

  return operand;
}

X86Operand* make_object_x86_operand(X86Context* ctx, Symbol* object)
{
  X86Operand* operand = 0;

  X86Location* object_loc = lookup_object_location(ctx, object);
  if(is_register_location(ctx, object_loc))
  {
    operand = make_register_x86_operand(ctx, object_loc);
  }
  else if(is_memory_location(ctx, object_loc))
  {
    operand = make_object_memory_x86_operand(ctx, object);
  }
  else assert(0);

  return operand;
}

X86Operand* make_id_x86_operand(X86Context* ctx, char* id)
{
  X86Operand* operand = push_struct(ctx->gp_arena, X86Operand);
  operand->kind = eX86Operand_id;
  operand->id = id;
  return operand;
}

void emit_mov(X86Context* ctx, Type* type, X86Operand* dest_operand, X86Operand* source_operand)
{
  eX86Stmt mov_op = eX86Stmt_None;

  if(type_eq(type, ctx->basic_type_int) || type_eq(type, ctx->basic_type_bool)
     || type_eq(type, ctx->basic_type_char)
     || (type->kind == eType_pointer) || (type->kind == eType_array))
  {
    mov_op = eX86Stmt_mov;
  }
  else if(type_eq(type, ctx->basic_type_float))
  {
    mov_op = eX86Stmt_movss;
  }

  X86Stmt* mov_stmt = new_x86_stmt(ctx, mov_op);
  mov_stmt->operand1 = dest_operand;
  mov_stmt->operand2 = source_operand;
}

void load_object_value(X86Context* ctx, Symbol* object, X86Location* dest_loc)
{
  assert(is_register_location(ctx, dest_loc));

  if(!is_object_in_location(object, dest_loc))
  {
    emit_mov(ctx, object->ty, make_register_x86_operand(ctx, dest_loc), make_object_x86_operand(ctx, object));
  }
}

void load_object_address(X86Context* ctx, Symbol* object, X86Location* dest_loc)
{
  assert(is_register_location(ctx, dest_loc));

  if(!is_object_in_location(object, dest_loc))
  {
    X86Stmt* stmt = new_x86_stmt(ctx, eX86Stmt_lea);

    stmt->operand1 = make_register_x86_operand(ctx, dest_loc);
    stmt->operand2 = make_object_address_x86_operand(ctx, object);
  }
}

void load_object(X86Context* ctx, Symbol* object, X86Location* dest_loc)
{
  assert(is_register_location(ctx, dest_loc));

  if(object->ty->kind == eType_array)
  {
    load_object_address(ctx, object, dest_loc);
  }
  else
  {
    load_object_value(ctx, object, dest_loc);
  }

  add_object_to_location(object, dest_loc);
}

void store_object(X86Context* ctx, Symbol* object)
{
  if(!is_object_in_location(object, &ctx->memory) && (object->ty->kind != eType_array))
  {
    X86Location* object_loc = lookup_object_location(ctx, object);

    emit_mov(ctx, object->ty, make_object_memory_x86_operand(ctx, object), make_register_x86_operand(ctx, object_loc));
  }
}

eX86Stmt conv_ir_op_to_x86_opcode(X86Context* ctx, eIrOp ir_op, Type* type)
{
  eX86Stmt x86_opcode = eX86Stmt_None;

  if(type_eq(type, ctx->basic_type_int) || type_eq(type, ctx->basic_type_bool)
     || type_eq(type, ctx->basic_type_char)
     || (type->kind == eType_pointer) || (type->kind == eType_array))
  {
    switch(ir_op)
    {
      case eIrOp_add:
        x86_opcode = eX86Stmt_add;
      break;

      case eIrOp_sub:
        x86_opcode = eX86Stmt_sub;
      break;

      case eIrOp_mul:
        x86_opcode = eX86Stmt_imul;
      break;

      case eIrOp_div:
        x86_opcode = eX86Stmt_idiv;
      break;

      case eIrOp_less:
        x86_opcode = eX86Stmt_jl;
      break;

      case eIrOp_less_eq:
        x86_opcode = eX86Stmt_jle;
      break;

      case eIrOp_greater:
        x86_opcode = eX86Stmt_jg;
      break;

      case eIrOp_greater_eq:
        x86_opcode = eX86Stmt_jge;
      break;

      case eIrOp_eq:
        x86_opcode = eX86Stmt_jz;
      break;

      case eIrOp_not_eq:
        x86_opcode = eX86Stmt_jnz;
      break;

      case eIrOp_neg:
        x86_opcode = eX86Stmt_neg;
      break;

      case eIrOp_ftoi:
        x86_opcode = eX86Stmt_cvttss2si;
      break;

      case eIrOp_bit_or:
        x86_opcode = eX86Stmt_or;
      break;

      case eIrOp_bit_and:
        x86_opcode = eX86Stmt_and;
      break;

      case eIrOp_bit_not:
        x86_opcode = eX86Stmt_not;
      break;

      default: assert(0);
    }
  }
  else if(type_eq(type, ctx->basic_type_float))
  {
    switch(ir_op)
    {
      case eIrOp_add:
        x86_opcode = eX86Stmt_addss;
      break;

      case eIrOp_sub:
        x86_opcode = eX86Stmt_subss;
      break;

      case eIrOp_mul:
        x86_opcode = eX86Stmt_mulss;
      break;

      case eIrOp_div:
        x86_opcode = eX86Stmt_divss;
      break;

      case eIrOp_less:
        x86_opcode = eX86Stmt_jb;
      break;

      case eIrOp_less_eq:
        x86_opcode = eX86Stmt_jbe;
      break;

      case eIrOp_greater:
        x86_opcode = eX86Stmt_ja;
      break;

      case eIrOp_greater_eq:
        x86_opcode = eX86Stmt_jae;
      break;

      case eIrOp_eq:
        x86_opcode = eX86Stmt_je;
      break;

      case eIrOp_not_eq:
        x86_opcode = eX86Stmt_jne;
      break;

      case eIrOp_itof:
        x86_opcode = eX86Stmt_cvtsi2ss;
      break;

      default: assert(0);
    }
  }
  else assert(0);


  return x86_opcode;
}

X86Location* get_first_fit_register(X86Context* ctx, Type* type)
{
  X86Location* reg = 0;

  for(int i = 0; i < ctx->register_count; i++)
  {
    reg = ctx->registers._[i];

    if(type_fits_into_register(ctx, type, reg))
      break;

    reg = 0;
  }

  assert(reg);
  return reg;
}

X86Location* find_least_used_register(X86Context* ctx, Type* type)
{
  X86Location* result = 0;
  int next_use = 0;

  for(int i = 0; i < ctx->register_count; i++)
  {
    X86Location* reg = ctx->registers._[i];

    if(type_fits_into_register(ctx, type, reg))
    {
      List* occupants = &reg->occupants;

      for(ListItem* li = occupants->first;
          li;
          li = li->next)
      {
        Symbol* object = KIND(li, eList_symbol)->symbol;

        if(!object->is_live || object->next_use >= next_use)
        {
          next_use = object->next_use;
          result = reg;
        }
      }
    }
  }

  assert(result);
  return result;
}

void add_object_to_memory(X86Context* ctx, Symbol* object)
{
  add_object_to_location(object, &ctx->memory);
}

void save_object_to_memory(X86Context* ctx, Symbol* object)
{
  store_object(ctx, object);
  add_object_to_memory(ctx, object);
}

void save_register(X86Context* ctx, X86Location* reg, bool free_reg)
{
  List* occupants = &reg->occupants;

  for(ListItem* li = occupants->first; li; )
  {
    ListItem* li_next = li->next;

    Symbol* object = KIND(li, eList_symbol)->symbol;
    if(object->is_live)
    {
      save_object_to_memory(ctx, object);
    }

    if(free_reg)
    {
      remove_object_from_location(object, reg);
    }

    li = li_next;
  }
}

void save_register_all_sizes(X86Context* ctx, X86Location* loc, bool free_reg)
{
  save_register(ctx, X86Location_get_top(loc), free_reg);

  if(loc->subloc[0])
  {
    save_register(ctx, loc->subloc[0], free_reg);
  }

  if(loc->subloc[1])
  {
    save_register(ctx, loc->subloc[1], free_reg);
  }
}

X86Location* get_best_available_register(X86Context* ctx, Type* type)
{
  X86Location* best_reg = 0;

  X86Location* free_reg = find_free_register(ctx, type);
  if(free_reg)
  {
    best_reg = free_reg;
  }
  else
  {
    best_reg = find_least_used_register(ctx, type);
    save_register(ctx, best_reg, true);
  }

  return best_reg;
}

void discard_unused_arg(X86Context* ctx, IrArg* arg, X86Location* arg_loc)
{
  if(is_register_location(ctx, arg_loc)
     && (arg->next_use == NextUse_None && !arg->is_live))
  {
    remove_object_from_location(arg->object, arg_loc);
  }
}

void discard_all_unused_args(X86Context* ctx, IrStmt_Assign* assign)
{
  X86Location* arg1_loc = lookup_object_location(ctx, assign->arg1->object);
  discard_unused_arg(ctx, assign->arg1, arg1_loc);

  if(assign->arg2)
  {
    X86Location* arg2_loc = lookup_object_location(ctx, assign->arg2->object);
    discard_unused_arg(ctx, assign->arg2, arg2_loc);
  }
}

void save_all_registers(X86Context* ctx, bool free_reg)
{
  for(int i = 0; i < ctx->register_count; i++)
  {
    X86Location* reg = ctx->registers._[i];
    save_register(ctx, reg, free_reg);
  }
}

void cgen_divmod_op(X86Context* ctx, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  assert(assign->op == eIrOp_div || assign->op == eIrOp_mod);

  if(type_eq(arg1->object->ty, ctx->basic_type_int))
  {
    X86Location* remainder_loc = &ctx->edx;
    save_register_all_sizes(ctx, remainder_loc, true);

    X86Location* dividend_loc = &ctx->eax;
    save_register_all_sizes(ctx, dividend_loc, true);

    if(!is_object_in_location(arg1->object, dividend_loc))
    {
      save_object_to_memory(ctx, arg1->object);
      load_object(ctx, arg1->object, dividend_loc);
    }

    X86Stmt* x86_stmt = new_x86_stmt(ctx, eX86Stmt_cdq);

    X86Location* arg2_loc = lookup_object_location(ctx, arg2->object);
    if(!is_register_location(ctx, arg2_loc))
    {
      arg2_loc = get_best_available_register(ctx, arg2->object->ty);
      load_object(ctx, arg2->object, arg2_loc);
    }

    x86_stmt = new_x86_stmt(ctx, eX86Stmt_idiv);
    x86_stmt->operand1 = make_object_x86_operand(ctx, arg2->object);

    clean_register_all_sizes(dividend_loc);
    clean_register_all_sizes(remainder_loc);
    if(assign->op == eIrOp_div)
    {
      set_exclusive_object_location(ctx, result->object, dividend_loc);
    }
    else if(assign->op == eIrOp_mod)
    {
      set_exclusive_object_location(ctx, result->object, remainder_loc);
    }
    else assert(0);
  }
  else if(type_eq(arg1->object->ty, ctx->basic_type_char))
  {
    fail("TODO");
  }
  else assert(0);
}

// result = arg1[arg2]
void cgen_index_source(X86Context* ctx, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  X86Location* result_loc = lookup_object_location(ctx, result->object);
  if(!is_register_location(ctx, result_loc))
  {
    result_loc = get_best_available_register(ctx, result->object->ty);
  }

  X86Location* arg1_loc = lookup_object_location(ctx, arg1->object);
  if(!is_register_location(ctx, arg1_loc))
  {
    arg1_loc = get_best_available_register(ctx, arg1->object->ty);
    load_object(ctx, arg1->object, arg1_loc);
  }

  X86Location* arg2_loc = lookup_object_location(ctx, arg2->object);
  if(!is_register_location(ctx, arg2_loc))
  {
    arg2_loc = get_best_available_register(ctx, arg2->object->ty);
    load_object(ctx, arg2->object, arg2_loc);
  }

  emit_mov(ctx, result->object->ty,
           make_register_x86_operand(ctx, result_loc),
           make_memory_x86_operand(ctx, result->object->ty,
                                   make_register_x86_operand(ctx, arg1_loc),
                                   make_register_x86_operand(ctx, arg2_loc)));

  clean_register_all_sizes(result_loc);
  set_exclusive_object_location(ctx, result->object, result_loc);
}

// result[arg2] = arg1
void cgen_index_dest(X86Context* ctx, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  X86Location* result_loc = lookup_object_location(ctx, result->object);
  if(!is_register_location(ctx, result_loc))
  {
    result_loc = get_best_available_register(ctx, result->object->ty);
    load_object(ctx, result->object, result_loc);
  }

  X86Location* arg1_loc = lookup_object_location(ctx, arg1->object);
  if(!is_register_location(ctx, arg1_loc))
  {
    arg1_loc = get_best_available_register(ctx, arg1->object->ty);
    assert(arg1_loc != result_loc);
    load_object(ctx, arg1->object, arg1_loc);
  }

  X86Location* arg2_loc = lookup_object_location(ctx, arg2->object);
  if(!is_register_location(ctx, arg2_loc))
  {
    arg2_loc = get_best_available_register(ctx, arg2->object->ty);
    assert(arg2_loc != result_loc && arg2_loc != arg1_loc);
    load_object(ctx, arg2->object, arg2_loc);
  }

  emit_mov(ctx, arg1->object->ty,
           make_memory_x86_operand(ctx, arg1->object->ty,
                                   make_register_x86_operand(ctx, result_loc),
                                   make_register_x86_operand(ctx, arg2_loc)),
           make_register_x86_operand(ctx, arg1_loc));
}

// result = ^arg1
void cgen_deref_source(X86Context* ctx, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* result_loc = lookup_object_location(ctx, result->object);
  if(!is_register_location(ctx, result_loc))
  {
    result_loc = get_best_available_register(ctx, result->object->ty);
  }

  X86Location* arg1_loc = lookup_object_location(ctx, arg1->object);
  if(!is_register_location(ctx, arg1_loc))
  {
    arg1_loc = get_best_available_register(ctx, arg1->object->ty);
    load_object(ctx, arg1->object, arg1_loc);
  }

  emit_mov(ctx, result->object->ty,
           make_register_x86_operand(ctx, result_loc),
           make_memory_x86_operand(ctx, result->object->ty,
                                   make_register_x86_operand(ctx, arg1_loc), 0));

  clean_register_all_sizes(result_loc);
  set_exclusive_object_location(ctx, result->object, result_loc);
}

// ^result = arg1
void cgen_deref_dest(X86Context* ctx, IrStmt_Assign *assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* result_loc = lookup_object_location(ctx, result->object);
  if(!is_register_location(ctx, result_loc))
  {
    result_loc = get_best_available_register(ctx, result->object->ty);
    load_object(ctx, result->object, result_loc);
  }

  X86Location* arg1_loc = lookup_object_location(ctx, arg1->object);
  if(!is_register_location(ctx, arg1_loc))
  {
    arg1_loc = get_best_available_register(ctx, arg1->object->ty);
    load_object(ctx, arg1->object, arg1_loc);
  }

  emit_mov(ctx, arg1->object->ty,
           make_memory_x86_operand(ctx, arg1->object->ty,
                                   make_register_x86_operand(ctx, result_loc), 0),
           make_register_x86_operand(ctx, arg1_loc));
}

// result = arg1
void cgen_equal(X86Context* ctx, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* arg1_loc = lookup_object_location(ctx, arg1->object);

  if(is_register_location(ctx, arg1_loc))
  {
    set_exclusive_object_location(ctx, result->object, arg1_loc);
  }
  else if(is_memory_location(ctx, arg1_loc))
  {
    X86Location* result_loc = lookup_object_location(ctx, result->object);

    if(result->next_use == NextUse_None
       && is_register_location(ctx, result_loc)
       && is_single_occupant_register(ctx, result_loc, result->object))
    {
      load_object(ctx, arg1->object, result_loc);
    }
    else
    {
      result_loc = get_best_available_register(ctx, result->object->ty);
      load_object(ctx, arg1->object, result_loc);
    }

    clean_register_all_sizes(result_loc);
    set_exclusive_object_location(ctx, result->object, result_loc);
  }
  else assert(0);
}

// result = &arg1
void cgen_address_of(X86Context* ctx, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* result_loc = get_best_available_register(ctx, result->object->ty);
  load_object_address(ctx, arg1->object, result_loc);

  clean_register_all_sizes(result_loc);
  set_exclusive_object_location(ctx, result->object, result_loc);
}

// result = arg1 op arg2
void cgen_bin_expr(X86Context* ctx, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  if((type_eq(result->object->ty, ctx->basic_type_int)
      || type_eq(result->object->ty, ctx->basic_type_char))
     && (assign->op == eIrOp_div || assign->op == eIrOp_mod))
  {
    if(assign->op == eIrOp_mod)
    {
      cgen_divmod_op(ctx, assign);
    }
    else if(assign->op == eIrOp_div)
    {
      cgen_divmod_op(ctx, assign);
    }
    else assert(0);
  }
  else
  {
    X86Location* arg1_loc = lookup_object_location(ctx, arg1->object);
    X86Location* result_loc = arg1_loc;

    if(is_register_location(ctx, arg1_loc)
       && is_single_occupant_register(ctx, arg1_loc, arg1->object)
       && (arg1->next_use == NextUse_None && !arg1->is_live))
    {
      remove_object_from_location(arg1->object, arg1_loc);
    }
    else
    {
      result_loc = get_best_available_register(ctx, arg1->object->ty);
      load_object(ctx, arg1->object, result_loc);
    }

    X86Stmt* x86_stmt = new_x86_stmt(ctx, conv_ir_op_to_x86_opcode(ctx, assign->op, result->object->ty));
    x86_stmt->operand1 = make_register_x86_operand(ctx, result_loc);
    x86_stmt->operand2 = make_object_x86_operand(ctx, arg2->object);

    clean_register_all_sizes(result_loc);
    set_exclusive_object_location(ctx, result->object, result_loc);
  }
}

// result = op arg1
void cgen_unr_expr(X86Context* ctx, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* arg1_loc = lookup_object_location(ctx, arg1->object);
  X86Location* result_loc = arg1_loc;

  if(is_cast_ir_op(assign->op))
  {
    switch(assign->op)
    {
      /* converting ops */
      case eIrOp_itof:
      case eIrOp_ftoi:
      {
        result_loc = get_best_available_register(ctx, result->object->ty);

        X86Stmt* x86_stmt = new_x86_stmt(ctx, conv_ir_op_to_x86_opcode(ctx, assign->op, result->object->ty));
        x86_stmt->operand1 = make_register_x86_operand(ctx, result_loc);
        x86_stmt->operand2 = make_object_x86_operand(ctx, arg1->object);
      }
      break;

      /* non-converting ops */
      case eIrOp_itob:
      case eIrOp_btoi:
      break;

      default:
        fail("todo");
    }
  }
  else
  {
    if(is_register_location(ctx, arg1_loc)
       && is_single_occupant_register(ctx, arg1_loc, arg1->object)
       && (arg1->next_use == NextUse_None && !arg1->is_live))
    {
      remove_object_from_location(arg1->object, arg1_loc);
    }
    else
    {
      result_loc = get_best_available_register(ctx, arg1->object->ty);
      load_object(ctx, arg1->object, result_loc);
    }

    if(assign->op == eIrOp_neg && type_eq(arg1->object->ty, ctx->basic_type_float))
    {
      X86Stmt* x86_stmt = new_x86_stmt(ctx, eX86Stmt_mulss);
      x86_stmt->operand1 = make_register_x86_operand(ctx, result_loc);
      x86_stmt->operand2 = make_object_x86_operand(ctx, ctx->float_minus_one);
    }
    else
    {
      X86Stmt* x86_stmt = new_x86_stmt(ctx, conv_ir_op_to_x86_opcode(ctx, assign->op, result->object->ty));
      x86_stmt->operand1 = make_register_x86_operand(ctx, result_loc);
    }
  }

  clean_register_all_sizes(result_loc);
  set_exclusive_object_location(ctx, result->object, result_loc);
}

void cgen_assign(X86Context* ctx, IrStmt_Assign* assign)
{
  if(assign->op != eIrOp_None)
  {
    if(assign->arg2)
    {
      switch(assign->op)
      {
        case eIrOp_index_source:
        {
          // result = arg1[arg2]
          cgen_index_source(ctx, assign);
        }
        break;

        case eIrOp_index_dest:
        {
          // result[arg2] = arg1
          cgen_index_dest(ctx, assign);
        }
        break;

        default:
        {
          // result = arg1 op arg2
          cgen_bin_expr(ctx, assign);
        }
      }
    }
    else
    {
      switch(assign->op)
      {
        case eIrOp_deref_source:
        {
          // result = ^arg1
          cgen_deref_source(ctx, assign);
        }
        break;

        case eIrOp_deref_dest:
        {
          // ^result = arg1
          cgen_deref_dest(ctx, assign);
        }
        break;

        case eIrOp_address_of:
        {
          // result = &arg1
          cgen_address_of(ctx, assign);
        }
        break;

        default:
        {
          // result = op arg1
          cgen_unr_expr(ctx, assign);
        }
      }
    }
  }
  else
  {
    // result = arg1
    cgen_equal(ctx, assign);
  }

  // IMPORTANT: The updated live-info is used in the find_least_used_register() function.
  // The update must be done *here*, so that when the next statement is processed,
  // a particular object will have the live-info of the previous last statement it appeared in.
  // As a bonus, the objects of 'result', 'arg1' and 'arg2' are automatically excluded from the find_least_used_register() search function.
  update_object_live_info(assign);

  discard_all_unused_args(ctx, assign);
}

// goto L
void cgen_goto(X86Context* ctx, IrStmt_Goto* goto_)
{
  save_all_registers(ctx, false);

  X86Stmt* jump_stmt = new_x86_stmt(ctx, eX86Stmt_jmp);
  jump_stmt->operand1 = make_id_x86_operand(ctx, goto_->goto_label->name);
}

// if arg1 relop arg2 goto L
void cgen_cond_goto(X86Context* ctx, IrStmt_CondGoto* cond_goto)
{
  IrArg* arg1 = cond_goto->arg1;
  IrArg* arg2 = cond_goto->arg2;

  save_all_registers(ctx, false);

  if(!is_object_in_register(ctx, arg1->object))
  {
    X86Location* arg1_loc = get_best_available_register(ctx, arg1->object->ty);
    load_object(ctx, arg1->object, arg1_loc);
  }

  X86Stmt* cmp_stmt = 0;
  if(type_eq(arg1->object->ty, ctx->basic_type_int)
    || type_eq(arg1->object->ty, ctx->basic_type_char)
    || type_eq(arg1->object->ty, ctx->basic_type_bool))
  {
    cmp_stmt = new_x86_stmt(ctx, eX86Stmt_cmp);
  }
  else if(type_eq(arg1->object->ty, ctx->basic_type_float))
  {
    cmp_stmt = new_x86_stmt(ctx, eX86Stmt_ucomiss);
  }
  else assert(0);

  cmp_stmt->operand1 = make_object_x86_operand(ctx, arg1->object);
  cmp_stmt->operand2 = make_object_x86_operand(ctx, arg2->object);

  X86Stmt* jump_stmt = new_x86_stmt(ctx, conv_ir_op_to_x86_opcode(ctx, cond_goto->relop, arg1->object->ty));
  jump_stmt->operand1 = make_id_x86_operand(ctx, cond_goto->goto_label->name);
}

void cgen_call(X86Context* ctx, IrStmt_Call* call)
{
  save_all_registers(ctx, true);

  /* sub esp, #param_size */
  X86Stmt* stmt = new_x86_stmt(ctx, eX86Stmt_sub);
  stmt->operand1 = make_register_x86_operand(ctx, &ctx->esp);
  stmt->operand2 = make_int_constant_x86_operand(ctx, call->param_scope->allocd_size);

  /* call #proc_name */
  stmt = new_x86_stmt(ctx, eX86Stmt_call);
  stmt->operand1 = make_id_x86_operand(ctx, call->name->name);

  if(call->is_extern)
  {
    /* add esp, #retvar_size */
    stmt = new_x86_stmt(ctx, eX86Stmt_add);
    stmt->operand1 = make_register_x86_operand(ctx, &ctx->esp);
    stmt->operand2 = make_int_constant_x86_operand(ctx, call->retvar->allocd_size);

    if(!type_eq(call->retvar->ty, ctx->basic_type_void))
    {
      clean_register_all_sizes(&ctx->eax);
      set_exclusive_object_location(ctx, call->retvar, &ctx->eax);
    }
  }
  else
  {
    /* add esp, #param_size */
    stmt = new_x86_stmt(ctx, eX86Stmt_add);
    stmt->operand1 = make_register_x86_operand(ctx, &ctx->esp);
    stmt->operand2 = make_int_constant_x86_operand(ctx, call->param_scope->allocd_size);

    add_object_to_memory(ctx, call->retvar);
  }
}

void cgen_basic_block(X86Context* ctx, BasicBlock* bb)
{
  if(bb->label)
  {
    X86Stmt* label_stmt = new_x86_stmt(ctx, eX86Stmt_label);
    label_stmt->operand1 = make_id_x86_operand(ctx, bb->label->name);
  }

  for(int i = 0; i < bb->stmt_count; i++)
  {
    IrStmt* ir_stmt = bb->stmt_array[i];

    switch(ir_stmt->kind)
    {
      case eIrStmt_assign:
      {
        cgen_assign(ctx, &ir_stmt->assign);
      }
      break;

      case eIrStmt_goto:
      {
        // goto L
        cgen_goto(ctx, &ir_stmt->goto_);
      }
      break;

      case eIrStmt_cond_goto:
      {
        // if arg1 relop arg2 goto L
        cgen_cond_goto(ctx, &ir_stmt->cond_goto);
      }
      break;

      case eIrStmt_call:
      {
        cgen_call(ctx, &ir_stmt->call);
      }
      break;

      case eIrStmt_return:
      case eIrStmt_nop:
      break;

      default: assert(0);
    }
  }
}

void cgen_proc(X86Context* ctx, AstNode* proc)
{
  if(is_extern_proc(proc))
  {
    /* extern #proc_name:proc */
    X86Stmt* stmt = new_x86_stmt(ctx, eX86Stmt_extern_proc);
    Label* label_name = &proc->proc.label_name;
    stmt->operand1 = make_id_x86_operand(ctx, label_name->name);
  }
  else
  {
    /* #proc_name: */
    X86Stmt* stmt = new_x86_stmt(ctx, eX86Stmt_label);
    stmt->operand1 = make_id_x86_operand(ctx, proc->proc.name);

    /* push ebp */
    stmt = new_x86_stmt(ctx, eX86Stmt_push);
    stmt->operand1 = make_register_x86_operand(ctx, &ctx->ebp);

    /* mov ebp, esp */
    stmt = new_x86_stmt(ctx, eX86Stmt_mov);
    stmt->operand1 = make_register_x86_operand(ctx, &ctx->ebp);
    stmt->operand2 = make_register_x86_operand(ctx, &ctx->esp);

    /* sub esp, #frame_size */
    stmt = new_x86_stmt(ctx, eX86Stmt_sub);
    stmt->operand1 = make_register_x86_operand(ctx, &ctx->esp);
    stmt->operand2 = make_int_constant_x86_operand(ctx, proc->proc.scope->allocd_size);

    List* basic_blocks = proc->proc.basic_blocks;

    for(ListItem* li = basic_blocks->first;
        li;
        li = li->next)
    {
      BasicBlock* bb = KIND(li, eList_basic_block)->basic_block;

      cgen_basic_block(ctx, bb);
      save_all_registers(ctx, true);
    }

    /* mov esp, ebp */
    stmt = new_x86_stmt(ctx, eX86Stmt_mov);
    stmt->operand1 = make_register_x86_operand(ctx, &ctx->esp);
    stmt->operand2 = make_register_x86_operand(ctx, &ctx->ebp);

    /* pop ebp */
    stmt = new_x86_stmt(ctx, eX86Stmt_pop);
    stmt->operand1 = make_register_x86_operand(ctx, &ctx->ebp);

    /* ret */
    new_x86_stmt(ctx, eX86Stmt_ret);
  }
}

void write_data_bytes(String* text, uint8* p_data, int data_size)
{
  int i;
  for(i = 0; i < data_size - 1; i++)
  {
    str_format(text, "0%xh,", p_data[i]);
  }

  if(data_size > 0)
  {
    str_format(text, "0%xh", p_data[i]);
  }

  str_nl(text);
}

void write_static_data_text(String* text, Scope* scope)
{
  for(ListItem* li = scope->decl_syms.first;
      li;
      li = li->next)
  {
    Symbol* object = KIND(li, eList_symbol)->symbol;
    if(object->storage_space == eStorageSpace_static && object->allocd_size > 0)
    {
      if(object->data)
      {
        str_append(text, "byte ");

        write_data_bytes(text, (uint8*)object->data, object->ty->width);

        int padding_size = object->allocd_size - object->ty->width;
        if(padding_size > 0)
        {
          str_format_nl(text, "byte %d dup(?)", padding_size);
        }
      }
      else
      {
        str_format_nl(text, "byte %d dup(?)", object->allocd_size);
      }
    }
  }
}

void cgen(X86Context* ctx, AstNode* module, char* title)
{
  for(ListItem* li = module->module.procs.first;
      li;
      li = li->next)
  {
    AstNode* proc = KIND(li, eList_ast_node)->ast_node;
    cgen_proc(ctx, proc);
  }

  str_format_nl(ctx->text, "title %s", title);
  str_append_nl(ctx->text, ".686");
  str_append_nl(ctx->text, ".xmm");
  str_append_nl(ctx->text, ".model flat, C");
  str_format_nl(ctx->text, ".stack %d", 1*MEGABYTE);
  str_append_nl(ctx->text, ".data");
  str_append_nl(ctx->text, "static_area label byte");
  str_format_nl(ctx->text, "align %d", ctx->data_alignment);

  write_static_data_text(ctx->text, module->module.scope);

  str_append_nl(ctx->text, ".code");
  str_append_nl(ctx->text, "public startup");

  for(int i = 0; i < ctx->stmt_count; i++)
  {
    print_x86_stmt(ctx, ctx->text, &ctx->stmt_array[i]);
  }

  str_append_nl(ctx->text, "end");
}
