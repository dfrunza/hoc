void X86Context::init(MemoryArena* gp_arena, MemoryArena* stmt_arena, MemoryArena* text_arena,
                      TypePass* type_pass, IrContext* ir_context, SymbolPass* sym_pass)
{
  basic_type_bool  = type_pass->basic_type_bool;
  basic_type_int   = type_pass->basic_type_int;
  basic_type_char  = type_pass->basic_type_char;
  basic_type_float = type_pass->basic_type_float;
  basic_type_void  = type_pass->basic_type_void;
  basic_type_str   = type_pass->basic_type_str;

  this->gp_arena = gp_arena;
  this->stmt_arena = stmt_arena;
  stmt_array = (X86Stmt*)stmt_arena->base;
  machine_word_size = 4;
  data_alignment = 4;
  init_registers();
  text = str_new(text_arena);

  ir_context->x86_context = this;
  sym_pass->x86_context = this;
}

void X86Context::init_registers()
{
  register_count = 0;

  /* eax */
  X86Location* loc = &eax;
  *loc = {};
  loc->kind = eX86Location_eax;
  loc->type = basic_type_int;
  registers._[register_count++] = loc;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* al */
  X86Location* subloc = &al;
  *subloc = {};
  loc->subloc[0] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_al;
  subloc->type = basic_type_char;
  registers._[register_count++] = subloc;
  list_init(&subloc->occupants, gp_arena, eList_symbol);

  /* ah */
  subloc = &ah;
  *subloc = {};
  loc->subloc[1] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_ah;
  subloc->type = basic_type_char;
  registers._[register_count++] = subloc;
  list_init(&subloc->occupants, gp_arena, eList_symbol);

  /* ebx */
  loc = &ebx;
  *loc = {};
  loc->kind = eX86Location_ebx;
  loc->type = basic_type_int;
  registers._[register_count++] = loc;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* bl */
  subloc = &bl;
  *subloc = {};
  loc->subloc[0] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_bl;
  subloc->type = basic_type_char;
  registers._[register_count++] = subloc;
  list_init(&subloc->occupants, gp_arena, eList_symbol);

  /* bh */
  subloc = &bh;
  *subloc = {};
  loc->subloc[1] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_bh;
  subloc->type = basic_type_char;
  registers._[register_count++] = subloc;
  list_init(&subloc->occupants, gp_arena, eList_symbol);

  /* ecx */
  loc = &ecx;
  *loc = {};
  loc->kind = eX86Location_ecx;
  loc->type = basic_type_int;
  registers._[register_count++] = loc;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* cl */
  subloc = &cl;
  *subloc = {};
  loc->subloc[0] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_cl;
  subloc->type = basic_type_char;
  registers._[register_count++] = subloc;
  list_init(&subloc->occupants, gp_arena, eList_symbol);

  /* ch */
  subloc = &ch;
  *subloc = {};
  loc->subloc[1] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_ch;
  subloc->type = basic_type_char;
  registers._[register_count++] = subloc;
  list_init(&subloc->occupants, gp_arena, eList_symbol);

  /* edx */
  loc = &edx;
  *loc = {};
  loc->kind = eX86Location_edx;
  loc->type = basic_type_int;
  registers._[register_count++] = loc;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* dl */
  subloc = &dl;
  *subloc = {};
  loc->subloc[0] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_dl;
  subloc->type = basic_type_char;
  registers._[register_count++] = subloc;
  list_init(&subloc->occupants, gp_arena, eList_symbol);

  /* dh */
  subloc = &dh;
  *subloc = {};
  loc->subloc[1] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location_dh;
  subloc->type = basic_type_char;
  registers._[register_count++] = subloc;
  list_init(&subloc->occupants, gp_arena, eList_symbol);

  /* esi */
  loc = &esi;
  *loc = {};
  loc->kind = eX86Location_esi;
  loc->type = basic_type_int;
  registers._[register_count++] = loc;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* edi */
  loc = &edi;
  *loc = {};
  loc->kind = eX86Location_edi;
  loc->type = basic_type_int;
  registers._[register_count++] = loc;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* memory */
  loc = &memory;
  *loc = {};
  loc->kind = eX86Location_memory;
  loc->type = basic_type_void;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* ebp */
  loc = &ebp;
  *loc = {};
  loc->kind = eX86Location_ebp;
  loc->type = basic_type_int;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* esp */
  loc = &esp;
  *loc = {};
  loc->kind = eX86Location_esp;
  loc->type = basic_type_int;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* xmm0 */
  loc = &xmm0;
  *loc = {};
  loc->kind = eX86Location_xmm0;
  loc->type = basic_type_float;
  registers._[register_count++] = loc;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* xmm1 */
  loc = &xmm1;
  *loc = {};
  loc->kind = eX86Location_xmm1;
  loc->type = basic_type_float;
  registers._[register_count++] = loc;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* xmm2 */
  loc = &xmm2;
  *loc = {};
  loc->kind = eX86Location_xmm2;
  loc->type = basic_type_float;
  registers._[register_count++] = loc;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* xmm3 */
  loc = &xmm3;
  *loc = {};
  loc->kind = eX86Location_xmm3;
  loc->type = basic_type_float;
  registers._[register_count++] = loc;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* xmm4 */
  loc = &xmm4;
  *loc = {};
  loc->kind = eX86Location_xmm4;
  loc->type = basic_type_float;
  registers._[register_count++] = loc;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* xmm5 */
  loc = &xmm5;
  *loc = {};
  loc->kind = eX86Location_xmm5;
  loc->type = basic_type_float;
  registers._[register_count++] = loc;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* xmm6 */
  loc = &xmm6;
  *loc = {};
  loc->kind = eX86Location_xmm6;
  loc->type = basic_type_float;
  registers._[register_count++] = loc;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  /* xmm7 */
  loc = &xmm7;
  *loc = {};
  loc->kind = eX86Location_xmm7;
  loc->type = basic_type_float;
  registers._[register_count++] = loc;
  list_init(&loc->occupants, gp_arena, eList_symbol);

  assert(register_count <= sizeof_array(registers._));
}

void X86Context::print_register(String* text, X86Location* reg)
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

char* X86Context::make_type_directive(Type* type)
{
  char* directv = "";

  if(type->equal(basic_type_char))
  {
    directv = "byte";
  }
  else if(type->equal(basic_type_int) || type->equal(basic_type_float)
          || type->equal(basic_type_bool) || (type->kind == eType_pointer) || (type->kind == eType_array))
  {
    directv = "dword";
  }
  else assert(0);

  return directv;
}

void X86Context::print_operand(String* text, X86Operand* operand)
{
  switch(operand->kind)
  {
    case eX86Operand_memory:
    case eX86Operand_address:
    {
      if(operand->kind == eX86Operand_memory)
      {
        str_format(text, "%s ptr [", make_type_directive(operand->index.type));
      }
      else if(operand->kind == eX86Operand_address)
      {
        str_append(text, "[");
      }
      else assert(0);

      X86Operand* base = operand->index.base;
      print_operand(text, base);

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
        print_operand(text, offset);
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

void X86Context::print_opcode(String* text, eX86Stmt opcode)
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

void X86Context::print_stmt(String* text, X86Stmt* stmt)
{
  switch(stmt->opcode)
  {
    case eX86Stmt_label:
    {
      print_operand(text, stmt->operand1);
      str_append(text, ":");
    }
    break;

    case eX86Stmt_extern_proc:
    {
      str_append(text, "extern ");
      print_operand(text, stmt->operand1);
      str_append(text, ":proc");
    }
    break;

    default:
    {
      print_opcode(text, stmt->opcode);
      if(stmt->operand1)
      {
        print_operand(text, stmt->operand1);
      }

      if(stmt->operand2)
      {
        str_append(text, ", ");
        print_operand(text, stmt->operand2);
      }
    }
  }

  str_nl(text);
}

X86Stmt* X86Context::create_stmt(eX86Stmt opcode)
{
  X86Stmt* stmt = push_struct(stmt_arena, X86Stmt);
  stmt_count++;
  stmt->opcode = opcode;
  stmt->operand1 = stmt->operand2 = 0;

  return stmt;
}

bool Symbol::is_in_location(X86Location* loc)
{
  return (locations._[loc->kind] != 0);
}

void X86Context::new_object_location_entry(Symbol* object, X86Location* loc)
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

void X86Context::remove_object_from_location(Symbol* object, X86Location* loc)
{
  if(object->is_in_location(loc))
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

void X86Context::add_object_to_location(Symbol* object, X86Location* loc)
{
  if(object->is_in_location(loc))
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

bool X86Location::is_parent_free()
{
  bool result = true;

  if(occupants.count == 0)
  {
    if(parent)
    {
      result = parent->is_parent_free();
    }
  }
  else
  {
    result = false;
  }

  return result;
}

bool X86Location::is_subloc_free()
{
  bool result = true;

  if(occupants.count == 0)
  {
    if(subloc[0])
    {
      result = subloc[0]->is_subloc_free();
    }

    if(result && subloc[1])
    {
      result = subloc[1]->is_subloc_free();
    }
  }
  else
  {
    result = false;
  }

  return result;
}

bool X86Location::is_free()
{
  bool result = true;

  if(occupants.count == 0)
  {
    if(parent)
    {
      result = parent->is_parent_free();
    }

    if(result && subloc[0])
    {
      result = subloc[0]->is_subloc_free();
    }

    if(result && subloc[1])
    {
      result = subloc[1]->is_subloc_free();
    }
  }
  else
  {
    result = false;
  }

  return result;
}

X86Location* X86Location::get_top()
{
  X86Location* top = this;

  if(parent)
  {
    top = parent->get_top();
  }

  return top;
}

void X86Context::clean_register(X86Location* loc)
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

void X86Context::clean_register_all_sizes(X86Location* loc)
{
  clean_register(loc->get_top());

  if(loc->subloc[0])
  {
    clean_register(loc->subloc[0]);
  }

  if(loc->subloc[1])
  {
    clean_register(loc->subloc[1]);
  }
}

void X86Context::set_exclusive_object_location(Symbol* object, X86Location* loc)
{
  for(int i = 0; i < register_count; i++)
  {
    remove_object_from_location(object, registers._[i]);
  }
  remove_object_from_location(object, &memory);

  add_object_to_location(object, loc);
}

bool X86Context::is_register_location(X86Location* loc)
{
  bool is_register = false;

  for(int i = 0;
      i < register_count && !is_register;
      i++)
  {
    X86Location* reg = registers._[i];
    is_register = (reg == loc);
  }

  //XXX: make_register_operand() allows EBP and ESP parameters.
  is_register |= (loc == &ebp) | (loc == &esp);

  return is_register;
}

bool X86Context::is_memory_location(X86Location* loc)
{
  return (loc == &memory);
}

bool X86Context::type_fits_into_register(Type* type, X86Location* reg)
{
  bool result = false;

  if(type->equal(basic_type_bool)
     || type->kind == eType_pointer || type->kind == eType_array)
  {
    result = reg->type->equal(basic_type_int);
  }
  else
  {
    result = type->equal(reg->type);
  }

  return result;
}

X86Location* X86Context::find_free_register(Type* type)
{
  X86Location* reg = 0;

  for(int i = 0; i < register_count; i++)
  {
    reg = registers._[i];
    if(reg->is_free() && type_fits_into_register(type, reg))
    {
      break;
    }
    reg = 0;
  }

  return reg;
}

X86Location* X86Context::lookup_object_location(Symbol* object)
{
  X86Location* loc = 0;

  for(int i = 0; i < register_count; i++)
  {
    loc = registers._[i];
    if(object->is_in_location(loc))
      break;
    loc = 0;
  }

  if(!loc)
  {
    loc = object->locations._[eX86Location_memory];
  }

  return loc;
}

bool X86Context::is_object_in_register(Symbol* object)
{
  bool in_register = false;

  for(int i = 0;
      i < register_count && !in_register;
      i++)
  {
    X86Location* loc = object->locations._[i];
    in_register = (loc && is_register_location(loc));
  }

  return in_register;
}

bool X86Context::is_single_occupant_register(X86Location* reg, Symbol* object)
{
  assert(is_register_location(reg));

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
X86Operand* X86Context::make_index_operand(eX86Operand kind, Symbol* object)
{
  X86Operand* operand = push_struct(gp_arena, X86Operand);
  operand->kind = kind;
  operand->index.type = object->ty;

  X86Operand* base = operand->index.base = push_struct(gp_arena, X86Operand);
  X86Operand* offset = operand->index.offset = push_struct(gp_arena, X86Operand);

  offset->kind = eX86Operand_constant;
  struct X86Operand_Constant* constant = &offset->constant;
  constant->kind = eX86Constant_int;

  switch(object->storage_space)
  {
    case eStorageSpace_local:
    {
      base->kind = eX86Operand_register;
      base->reg = &ebp;

      constant->int_val = -(object->data_loc + object->allocd_size);
    }
    break;

    case eStorageSpace_static_:
    {
      base->kind = eX86Operand_id;
      base->id = "static_area";

      constant->int_val = object->data_loc;
    }
    break;

    case eStorageSpace_actual_param:
    {
      base->kind = eX86Operand_register;
      base->reg = &esp;

      constant->int_val = -(object->data_loc + object->allocd_size);
    }
    break;

    case eStorageSpace_formal_param:
    {
      base->kind = eX86Operand_register;
      base->reg = &ebp;

      int scope_allocd_size = object->scope->allocd_size;
      int machine_area_size = 2*machine_word_size; // instruction_pointer + frame_pointer
      constant->int_val = (scope_allocd_size + machine_area_size) - (object->data_loc + object->allocd_size);
    }
    break;

    default: assert(0);
  }

  return operand;
}

X86Operand* X86Context::make_register_operand(X86Location* reg)
{
  assert(is_register_location(reg));

  X86Operand* operand = push_struct(gp_arena, X86Operand);
  operand->kind = eX86Operand_register;
  operand->reg = reg;

  return operand;
}

X86Operand* X86Context::make_int_constant_operand(int int_val)
{
  X86Operand* operand = push_struct(gp_arena, X86Operand);
  operand->kind = eX86Operand_constant;

  struct X86Operand_Constant* constant = &operand->constant;
  constant->kind = eX86Constant_int;
  constant->int_val = int_val;

  return operand;
}

X86Operand* X86Context::make_memory_operand(Type* type, X86Operand* base, X86Operand* offset)
{
  X86Operand* operand = push_struct(gp_arena, X86Operand);
  operand->kind = eX86Operand_memory;

  operand->index.type = type;
  operand->index.base = base;
  operand->index.offset = offset;

  return operand;
}

X86Operand* X86Context::make_object_address_operand(Symbol* object)
{
  return make_index_operand(eX86Operand_address, object);
}

X86Operand* X86Context::make_object_memory_operand(Symbol* object)
{
  X86Operand* operand = 0;

  if(object->kind == eSymbol_constant)
  {
    if(object->ty->equal(basic_type_str))
    {
      operand = make_object_address_operand(object);
    }
    else
    {
      operand = push_struct(gp_arena, X86Operand);
      operand->kind = eX86Operand_constant;

      struct X86Operand_Constant* constant = &operand->constant;

      if(object->ty->equal(basic_type_int))
      {
        constant->kind = eX86Constant_int;
        constant->int_val = object->int_val;
      }
      else if(object->ty->equal(basic_type_float))
      {
        operand = make_index_operand(eX86Operand_memory, object);
      }
      else if(object->ty->equal(basic_type_char))
      {
        constant->kind = eX86Constant_char;
        constant->char_val = object->char_val;
      }
      else if(object->ty->equal(basic_type_bool))
      {
        constant->kind = eX86Constant_int;
        constant->int_val = object->int_val;
      }
      else assert(0);
    }
  }
  else
  {
    operand = make_index_operand(eX86Operand_memory, object);
  }

  return operand;
}

X86Operand* X86Context::make_object_operand(Symbol* object)
{
  X86Operand* operand = 0;

  X86Location* object_loc = lookup_object_location(object);
  if(is_register_location(object_loc))
  {
    operand = make_register_operand(object_loc);
  }
  else if(is_memory_location(object_loc))
  {
    operand = make_object_memory_operand(object);
  }
  else assert(0);

  return operand;
}

X86Operand* X86Context::make_id_operand(char* id)
{
  X86Operand* operand = push_struct(gp_arena, X86Operand);
  operand->kind = eX86Operand_id;
  operand->id = id;
  return operand;
}

void X86Context::emit_mov(Type* type, X86Operand* dest_operand, X86Operand* source_operand)
{
  eX86Stmt mov_op = eX86Stmt_None;

  if(type->equal(basic_type_int) || type->equal(basic_type_bool)
     || type->equal(basic_type_char)
     || (type->kind == eType_pointer) || (type->kind == eType_array))
  {
    mov_op = eX86Stmt_mov;
  }
  else if(type->equal(basic_type_float))
  {
    mov_op = eX86Stmt_movss;
  }

  X86Stmt* mov_stmt = create_stmt(mov_op);
  mov_stmt->operand1 = dest_operand;
  mov_stmt->operand2 = source_operand;
}

void X86Context::load_object_value(Symbol* object, X86Location* dest_loc)
{
  assert(is_register_location(dest_loc));

  if(!object->is_in_location(dest_loc))
  {
    emit_mov(object->ty, make_register_operand(dest_loc), make_object_operand(object));
  }
}

void X86Context::load_object_address(Symbol* object, X86Location* dest_loc)
{
  assert(is_register_location(dest_loc));

  if(!object->is_in_location(dest_loc))
  {
    X86Stmt* stmt = create_stmt(eX86Stmt_lea);

    stmt->operand1 = make_register_operand(dest_loc);
    stmt->operand2 = make_object_address_operand(object);
  }
}

void X86Context::load_object(Symbol* object, X86Location* dest_loc)
{
  assert(is_register_location(dest_loc));

  if(object->ty->kind == eType_array)
  {
    load_object_address(object, dest_loc);
  }
  else
  {
    load_object_value(object, dest_loc);
  }

  add_object_to_location(object, dest_loc);
}

void X86Context::store_object(Symbol* object)
{
  if(!object->is_in_location(&memory) && (object->ty->kind != eType_array))
  {
    X86Location* object_loc = lookup_object_location(object);

    emit_mov(object->ty, make_object_memory_operand(object), make_register_operand(object_loc));
  }
}

eX86Stmt X86Context::conv_ir_op_to_x86_opcode(eIrOp ir_op, Type* type)
{
  eX86Stmt x86_opcode = eX86Stmt_None;

  if(type->equal(basic_type_int) || type->equal(basic_type_bool)
     || type->equal(basic_type_char)
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
  else if(type->equal(basic_type_float))
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

X86Location* X86Context::get_first_fit_register(Type* type)
{
  X86Location* reg = 0;

  for(int i = 0; i < register_count; i++)
  {
    reg = registers._[i];

    if(type_fits_into_register(type, reg))
      break;

    reg = 0;
  }

  assert(reg);
  return reg;
}

X86Location* X86Context::find_least_used_register(Type* type)
{
  X86Location* result = 0;
  int next_use = 0;

  for(int i = 0; i < register_count; i++)
  {
    X86Location* reg = registers._[i];

    if(type_fits_into_register(type, reg))
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

void X86Context::add_object_to_memory(Symbol* object)
{
  add_object_to_location(object, &memory);
}

void X86Context::save_object_to_memory(Symbol* object)
{
  store_object(object);
  add_object_to_memory(object);
}

void X86Context::save_register(X86Location* reg, bool free_reg)
{
  List* occupants = &reg->occupants;

  for(ListItem* li = occupants->first; li; )
  {
    ListItem* li_next = li->next;

    Symbol* object = KIND(li, eList_symbol)->symbol;
    if(object->is_live)
    {
      save_object_to_memory(object);
    }

    if(free_reg)
    {
      remove_object_from_location(object, reg);
    }

    li = li_next;
  }
}

void X86Context::save_register_all_sizes(X86Location* loc, bool free_reg)
{
  save_register(loc->get_top(), free_reg);

  if(loc->subloc[0])
  {
    save_register(loc->subloc[0], free_reg);
  }

  if(loc->subloc[1])
  {
    save_register(loc->subloc[1], free_reg);
  }
}

X86Location* X86Context::get_best_available_register(Type* type)
{
  X86Location* best_reg = 0;

  X86Location* free_reg = find_free_register(type);
  if(free_reg)
  {
    best_reg = free_reg;
  }
  else
  {
    best_reg = find_least_used_register(type);
    save_register(best_reg, true);
  }

  return best_reg;
}

void X86Context::discard_unused_arg(IrArg* arg, X86Location* arg_loc)
{
  if(is_register_location(arg_loc)
     && (arg->next_use == NextUse_None && !arg->is_live))
  {
    remove_object_from_location(arg->object, arg_loc);
  }
}

void X86Context::discard_all_unused_args(IrStmt_Assign* assign)
{
  X86Location* arg1_loc = lookup_object_location(assign->arg1->object);
  discard_unused_arg(assign->arg1, arg1_loc);

  if(assign->arg2)
  {
    X86Location* arg2_loc = lookup_object_location(assign->arg2->object);
    discard_unused_arg(assign->arg2, arg2_loc);
  }
}

void X86Context::save_all_registers(bool free_reg)
{
  for(int i = 0; i < register_count; i++)
  {
    X86Location* reg = registers._[i];
    save_register(reg, free_reg);
  }
}

void X86Context::gen_divmod_op(IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  assert(assign->op == eIrOp_div || assign->op == eIrOp_mod);

  if(arg1->object->ty->equal(basic_type_int))
  {
    X86Location* remainder_loc = &edx;
    save_register_all_sizes(remainder_loc, true);

    X86Location* dividend_loc = &eax;
    save_register_all_sizes(dividend_loc, true);

    if(!arg1->object->is_in_location(dividend_loc))
    {
      save_object_to_memory(arg1->object);
      load_object(arg1->object, dividend_loc);
    }

    X86Stmt* x86_stmt = create_stmt(eX86Stmt_cdq);

    X86Location* arg2_loc = lookup_object_location(arg2->object);
    if(!is_register_location(arg2_loc))
    {
      arg2_loc = get_best_available_register(arg2->object->ty);
      load_object(arg2->object, arg2_loc);
    }

    x86_stmt = create_stmt(eX86Stmt_idiv);
    x86_stmt->operand1 = make_object_operand(arg2->object);

    clean_register_all_sizes(dividend_loc);
    clean_register_all_sizes(remainder_loc);
    if(assign->op == eIrOp_div)
    {
      set_exclusive_object_location(result->object, dividend_loc);
    }
    else if(assign->op == eIrOp_mod)
    {
      set_exclusive_object_location(result->object, remainder_loc);
    }
    else assert(0);
  }
  else if(arg1->object->ty->equal(basic_type_char))
  {
    fail("TODO");
  }
  else assert(0);
}

// result = arg1[arg2]
void X86Context::gen_index_source(IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  X86Location* result_loc = lookup_object_location(result->object);
  if(!is_register_location(result_loc))
  {
    result_loc = get_best_available_register(result->object->ty);
  }

  X86Location* arg1_loc = lookup_object_location(arg1->object);
  if(!is_register_location(arg1_loc))
  {
    arg1_loc = get_best_available_register(arg1->object->ty);
    load_object(arg1->object, arg1_loc);
  }

  X86Location* arg2_loc = lookup_object_location(arg2->object);
  if(!is_register_location(arg2_loc))
  {
    arg2_loc = get_best_available_register(arg2->object->ty);
    load_object(arg2->object, arg2_loc);
  }

  emit_mov(result->object->ty,
           make_register_operand(result_loc),
           make_memory_operand(result->object->ty,
                               make_register_operand(arg1_loc),
                               make_register_operand(arg2_loc)));

  clean_register_all_sizes(result_loc);
  set_exclusive_object_location(result->object, result_loc);
}

// result[arg2] = arg1
void X86Context::gen_index_dest(IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  X86Location* result_loc = lookup_object_location(result->object);
  if(!is_register_location(result_loc))
  {
    result_loc = get_best_available_register(result->object->ty);
    load_object(result->object, result_loc);
  }

  X86Location* arg1_loc = lookup_object_location(arg1->object);
  if(!is_register_location(arg1_loc))
  {
    arg1_loc = get_best_available_register(arg1->object->ty);
    assert(arg1_loc != result_loc);
    load_object(arg1->object, arg1_loc);
  }

  X86Location* arg2_loc = lookup_object_location(arg2->object);
  if(!is_register_location(arg2_loc))
  {
    arg2_loc = get_best_available_register(arg2->object->ty);
    assert(arg2_loc != result_loc && arg2_loc != arg1_loc);
    load_object(arg2->object, arg2_loc);
  }

  emit_mov(arg1->object->ty,
           make_memory_operand(arg1->object->ty,
                               make_register_operand(result_loc),
                               make_register_operand(arg2_loc)),
           make_register_operand(arg1_loc));
}

// result = ^arg1
void X86Context::gen_deref_source(IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* result_loc = lookup_object_location(result->object);
  if(!is_register_location(result_loc))
  {
    result_loc = get_best_available_register(result->object->ty);
  }

  X86Location* arg1_loc = lookup_object_location(arg1->object);
  if(!is_register_location(arg1_loc))
  {
    arg1_loc = get_best_available_register(arg1->object->ty);
    load_object(arg1->object, arg1_loc);
  }

  emit_mov(result->object->ty,
           make_register_operand(result_loc),
           make_memory_operand(result->object->ty,
                               make_register_operand(arg1_loc), 0));

  clean_register_all_sizes(result_loc);
  set_exclusive_object_location(result->object, result_loc);
}

// ^result = arg1
void X86Context::gen_deref_dest(IrStmt_Assign *assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* result_loc = lookup_object_location(result->object);
  if(!is_register_location(result_loc))
  {
    result_loc = get_best_available_register(result->object->ty);
    load_object(result->object, result_loc);
  }

  X86Location* arg1_loc = lookup_object_location(arg1->object);
  if(!is_register_location(arg1_loc))
  {
    arg1_loc = get_best_available_register(arg1->object->ty);
    load_object(arg1->object, arg1_loc);
  }

  emit_mov(arg1->object->ty,
           make_memory_operand(arg1->object->ty,
                               make_register_operand(result_loc), 0),
           make_register_operand(arg1_loc));
}

// result = arg1
void X86Context::gen_equal(IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* arg1_loc = lookup_object_location(arg1->object);

  if(is_register_location(arg1_loc))
  {
    set_exclusive_object_location(result->object, arg1_loc);
  }
  else if(is_memory_location(arg1_loc))
  {
    X86Location* result_loc = lookup_object_location(result->object);

    if(result->next_use == NextUse_None
       && is_register_location(result_loc)
       && is_single_occupant_register(result_loc, result->object))
    {
      load_object(arg1->object, result_loc);
    }
    else
    {
      result_loc = get_best_available_register(result->object->ty);
      load_object(arg1->object, result_loc);
    }

    clean_register_all_sizes(result_loc);
    set_exclusive_object_location(result->object, result_loc);
  }
  else assert(0);
}

// result = &arg1
void X86Context::gen_address_of(IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* result_loc = get_best_available_register(result->object->ty);
  load_object_address(arg1->object, result_loc);

  clean_register_all_sizes(result_loc);
  set_exclusive_object_location(result->object, result_loc);
}

// result = arg1 op arg2
void X86Context::gen_bin_expr(IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  if((result->object->ty->equal(basic_type_int)
      || result->object->ty->equal(basic_type_char))
     && (assign->op == eIrOp_div || assign->op == eIrOp_mod))
  {
    if(assign->op == eIrOp_mod)
    {
      gen_divmod_op(assign);
    }
    else if(assign->op == eIrOp_div)
    {
      gen_divmod_op(assign);
    }
    else assert(0);
  }
  else
  {
    X86Location* arg1_loc = lookup_object_location(arg1->object);
    X86Location* result_loc = arg1_loc;

    if(is_register_location(arg1_loc)
       && is_single_occupant_register(arg1_loc, arg1->object)
       && (arg1->next_use == NextUse_None && !arg1->is_live))
    {
      remove_object_from_location(arg1->object, arg1_loc);
    }
    else
    {
      result_loc = get_best_available_register(arg1->object->ty);
      load_object(arg1->object, result_loc);
    }

    X86Stmt* x86_stmt = create_stmt(conv_ir_op_to_x86_opcode(assign->op, result->object->ty));
    x86_stmt->operand1 = make_register_operand(result_loc);
    x86_stmt->operand2 = make_object_operand(arg2->object);

    clean_register_all_sizes(result_loc);
    set_exclusive_object_location(result->object, result_loc);
  }
}

// result = op arg1
void X86Context::gen_unr_expr(IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* arg1_loc = lookup_object_location(arg1->object);
  X86Location* result_loc = arg1_loc;

  if(IrContext::is_cast_op(assign->op))
  {
    switch(assign->op)
    {
      /* converting ops */
      case eIrOp_itof:
      case eIrOp_ftoi:
      {
        result_loc = get_best_available_register(result->object->ty);

        X86Stmt* x86_stmt = create_stmt(conv_ir_op_to_x86_opcode(assign->op, result->object->ty));
        x86_stmt->operand1 = make_register_operand(result_loc);
        x86_stmt->operand2 = make_object_operand(arg1->object);
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
    if(is_register_location(arg1_loc)
       && is_single_occupant_register(arg1_loc, arg1->object)
       && (arg1->next_use == NextUse_None && !arg1->is_live))
    {
      remove_object_from_location(arg1->object, arg1_loc);
    }
    else
    {
      result_loc = get_best_available_register(arg1->object->ty);
      load_object(arg1->object, result_loc);
    }

    if(assign->op == eIrOp_neg && arg1->object->ty->equal(basic_type_float))
    {
      X86Stmt* x86_stmt = create_stmt(eX86Stmt_mulss);
      x86_stmt->operand1 = make_register_operand(result_loc);
      x86_stmt->operand2 = make_object_operand(float_minus_one);
    }
    else
    {
      X86Stmt* x86_stmt = create_stmt(conv_ir_op_to_x86_opcode(assign->op, result->object->ty));
      x86_stmt->operand1 = make_register_operand(result_loc);
    }
  }

  clean_register_all_sizes(result_loc);
  set_exclusive_object_location(result->object, result_loc);
}

void X86Context::gen_assign(IrStmt_Assign* assign)
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
          gen_index_source(assign);
        }
        break;

        case eIrOp_index_dest:
        {
          // result[arg2] = arg1
          gen_index_dest(assign);
        }
        break;

        default:
        {
          // result = arg1 op arg2
          gen_bin_expr(assign);
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
          gen_deref_source(assign);
        }
        break;

        case eIrOp_deref_dest:
        {
          // ^result = arg1
          gen_deref_dest(assign);
        }
        break;

        case eIrOp_address_of:
        {
          // result = &arg1
          gen_address_of(assign);
        }
        break;

        default:
        {
          // result = op arg1
          gen_unr_expr(assign);
        }
      }
    }
  }
  else
  {
    // result = arg1
    gen_equal(assign);
  }

  // IMPORTANT: The updated live-info is used in the find_least_used_register() function.
  // The update must be done *here*, so that when the next statement is processed,
  // a particular object will have the live-info of the previous last statement it appeared in.
  // As a bonus, the objects of 'result', 'arg1' and 'arg2' are automatically excluded from the find_least_used_register() search function.
  assign->update_object_live_info();

  discard_all_unused_args(assign);
}

// goto L
void X86Context::gen_goto(IrStmt_Goto* goto_)
{
  save_all_registers(false);

  X86Stmt* jump_stmt = create_stmt(eX86Stmt_jmp);
  jump_stmt->operand1 = make_id_operand(goto_->goto_label->name);
}

// if arg1 relop arg2 goto L
void X86Context::gen_cond_goto(IrStmt_CondGoto* cond_goto)
{
  IrArg* arg1 = cond_goto->arg1;
  IrArg* arg2 = cond_goto->arg2;

  save_all_registers(false);

  if(!is_object_in_register(arg1->object))
  {
    X86Location* arg1_loc = get_best_available_register(arg1->object->ty);
    load_object(arg1->object, arg1_loc);
  }

  X86Stmt* cmp_stmt = 0;
  if(arg1->object->ty->equal(basic_type_int)
    || arg1->object->ty->equal(basic_type_char)
    || arg1->object->ty->equal(basic_type_bool))
  {
    cmp_stmt = create_stmt(eX86Stmt_cmp);
  }
  else if(arg1->object->ty->equal(basic_type_float))
  {
    cmp_stmt = create_stmt(eX86Stmt_ucomiss);
  }
  else assert(0);

  cmp_stmt->operand1 = make_object_operand(arg1->object);
  cmp_stmt->operand2 = make_object_operand(arg2->object);

  X86Stmt* jump_stmt = create_stmt(conv_ir_op_to_x86_opcode(cond_goto->relop, arg1->object->ty));
  jump_stmt->operand1 = make_id_operand(cond_goto->goto_label->name);
}

void X86Context::gen_call(IrStmt_Call* call)
{
  save_all_registers(true);

  /* sub esp, #param_size */
  X86Stmt* stmt = create_stmt(eX86Stmt_sub);
  stmt->operand1 = make_register_operand(&esp);
  stmt->operand2 = make_int_constant_operand(call->param_scope->allocd_size);

  /* call #proc_name */
  stmt = create_stmt(eX86Stmt_call);
  stmt->operand1 = make_id_operand(call->name->name);

  if(call->is_extern)
  {
    /* add esp, #retvar_size */
    stmt = create_stmt(eX86Stmt_add);
    stmt->operand1 = make_register_operand(&esp);
    stmt->operand2 = make_int_constant_operand(call->retvar->allocd_size);

    if(!call->retvar->ty->equal(basic_type_void))
    {
      clean_register_all_sizes(&eax);
      set_exclusive_object_location(call->retvar, &eax);
    }
  }
  else
  {
    /* add esp, #param_size */
    stmt = create_stmt(eX86Stmt_add);
    stmt->operand1 = make_register_operand(&esp);
    stmt->operand2 = make_int_constant_operand(call->param_scope->allocd_size);

    add_object_to_memory(call->retvar);
  }
}

void X86Context::gen_basic_block(BasicBlock* bb)
{
  if(bb->label)
  {
    X86Stmt* label_stmt = create_stmt(eX86Stmt_label);
    label_stmt->operand1 = make_id_operand(bb->label->name);
  }

  for(int i = 0; i < bb->stmt_count; i++)
  {
    IrStmt* ir_stmt = bb->stmt_array[i];

    switch(ir_stmt->kind)
    {
      case eIrStmt_assign:
      {
        gen_assign(&ir_stmt->assign);
      }
      break;

      case eIrStmt_goto:
      {
        // goto L
        gen_goto(&ir_stmt->goto_);
      }
      break;

      case eIrStmt_cond_goto:
      {
        // if arg1 relop arg2 goto L
        gen_cond_goto(&ir_stmt->cond_goto);
      }
      break;

      case eIrStmt_call:
      {
        gen_call(&ir_stmt->call);
      }
      break;

      case eIrStmt_return:
      case eIrStmt_nop:
      break;

      default: assert(0);
    }
  }
}

void X86Context::gen_proc(AstNode* proc)
{
  if(is_extern_proc(proc))
  {
    /* extern #proc_name:proc */
    X86Stmt* stmt = create_stmt(eX86Stmt_extern_proc);
    Label* label_name = &proc->proc.label_name;
    stmt->operand1 = make_id_operand(label_name->name);
  }
  else
  {
    /* #proc_name: */
    X86Stmt* stmt = create_stmt(eX86Stmt_label);
    stmt->operand1 = make_id_operand(proc->proc.name);

    /* push ebp */
    stmt = create_stmt(eX86Stmt_push);
    stmt->operand1 = make_register_operand(&ebp);

    /* mov ebp, esp */
    stmt = create_stmt(eX86Stmt_mov);
    stmt->operand1 = make_register_operand(&ebp);
    stmt->operand2 = make_register_operand(&esp);

    /* sub esp, #frame_size */
    stmt = create_stmt(eX86Stmt_sub);
    stmt->operand1 = make_register_operand(&esp);
    Scope* proc_scope = proc->proc.scope;
    stmt->operand2 = make_int_constant_operand(proc_scope->allocd_size);

    List* basic_blocks = proc->proc.basic_blocks;

    for(ListItem* li = basic_blocks->first;
        li;
        li = li->next)
    {
      BasicBlock* bb = KIND(li, eList_basic_block)->basic_block;

      gen_basic_block(bb);
      save_all_registers(true);
    }

    /* mov esp, ebp */
    stmt = create_stmt(eX86Stmt_mov);
    stmt->operand1 = make_register_operand(&esp);
    stmt->operand2 = make_register_operand(&ebp);

    /* pop ebp */
    stmt = create_stmt(eX86Stmt_pop);
    stmt->operand1 = make_register_operand(&ebp);

    /* ret */
    create_stmt(eX86Stmt_ret);
  }
}

void X86Context::write_data_bytes(String* text, uint8* p_data, int data_size)
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

void X86Context::write_static_data_text(String* text, Scope* scope)
{
  for(ListItem* li = scope->decl_syms.first;
      li;
      li = li->next)
  {
    Symbol* object = KIND(li, eList_symbol)->symbol;
    if(object->storage_space == eStorageSpace_static_ && object->allocd_size > 0)
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

void X86Context::gen_module(AstNode* module)
{
  List* procs = &module->module.procs;
  for(ListItem* li = procs->first;
      li;
      li = li->next)
  {
    AstNode* proc = KIND(li, eList_ast_node)->ast_node;

    gen_proc(proc);
  }
}

void X86Context::gen(AstNode* module, char* title)
{
  gen_module(module);

  str_format_nl(text, "title %s", title);
  str_append_nl(text, ".686");
  str_append_nl(text, ".xmm");
  str_append_nl(text, ".model flat, C");
  str_format_nl(text, ".stack %d", 1*MEGABYTE);
  str_append_nl(text, ".data");
  str_append_nl(text, "static_area label byte");
  str_format_nl(text, "align %d", data_alignment);

  write_static_data_text(text, module->module.scope);

  str_append_nl(text, ".code");
  str_append_nl(text, "public startup");

  for(int i = 0; i < stmt_count; i++)
  {
    print_stmt(text, &stmt_array[i]);
  }

  str_append_nl(text, "end");
}
