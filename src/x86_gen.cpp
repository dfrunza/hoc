void x86_print_register(String* text, X86Location* reg)
{
  switch(reg->kind)
  {
    /* 8-bit */
    case eX86Location::al:
      text->printf("al");
    break;

    case eX86Location::ah:
      text->printf("ah");
    break;

    case eX86Location::bl:
      text->printf("bl");
    break;

    case eX86Location::bh:
      text->printf("bh");
    break;

    case eX86Location::cl:
      text->printf("cl");
    break;

    case eX86Location::ch:
      text->printf("ch");
    break;

    case eX86Location::dl:
      text->printf("dl");
    break;

    case eX86Location::dh:
      text->printf("dh");
    break;

    /* 32-bit */
    case eX86Location::eax:
      text->printf("eax");
    break;

    case eX86Location::ebx:
      text->printf("ebx");
    break;

    case eX86Location::ecx:
      text->printf("ecx");
    break;

    case eX86Location::edx:
      text->printf("edx");
    break;

    case eX86Location::ebp:
      text->printf("ebp");
    break;

    case eX86Location::esp:
      text->printf("esp");
    break;

    case eX86Location::esi:
      text->printf("esi");
    break;

    case eX86Location::edi:
      text->printf("edi");
    break;

    /* xmm */
    case eX86Location::xmm0:
      text->printf("xmm0");
    break;

    case eX86Location::xmm1:
      text->printf("xmm1");
    break;

    case eX86Location::xmm2:
      text->printf("xmm2");
    break;

    case eX86Location::xmm3:
      text->printf("xmm3");
    break;

    case eX86Location::xmm4:
      text->printf("xmm4");
    break;

    case eX86Location::xmm5:
      text->printf("xmm5");
    break;

    case eX86Location::xmm6:
      text->printf("xmm6");
    break;

    case eX86Location::xmm7:
      text->printf("xmm7");
    break;

    default: assert(0);
  }
}

char* x86_make_type_directive(Type* type)
{
  char* directv = "";

  if(type->equal(basic_type_char))
  {
    directv = "byte";
  }
  else if(type->equal(basic_type_int) || type->equal(basic_type_float)
          || type->equal(basic_type_bool) || (type->kind == eType::pointer) || (type->kind == eType::array))
  {
    directv = "dword";
  }
  else assert(0);

  return directv;
}

void x86_print_operand(String* text, X86Operand* operand)
{
  switch(operand->kind)
  {
    case eX86Operand::memory:
    case eX86Operand::address:
    {
      if(operand->kind == eX86Operand::memory)
      {
        text->printf("%s ptr [", x86_make_type_directive(operand->index.type));
      }
      else if(operand->kind == eX86Operand::address)
      {
        text->printf("[");
      }
      else assert(0);

      X86Operand* base = operand->index.base;
      x86_print_operand(text, base);

      X86Operand* offset = operand->index.offset;
      if(offset)
      {
        char* sign = "+";
        if(offset->kind == eX86Operand::constant)
        {
          struct X86Operand_constant* constant = &offset->constant;
          assert(constant->kind == eX86Constant::int_);

          if(constant->int_val < 0)
          {
            sign = "";
          }
        }
        text->printf("%s", sign);
        x86_print_operand(text, offset);
      }
      text->printf("]");
    }
    break;

    case eX86Operand::register_:
    {
      x86_print_register(text, operand->reg);
    }
    break;

    case eX86Operand::constant:
    {
      struct X86Operand_constant* constant = &operand->constant;

      if(constant->kind == eX86Constant::int_)
      {
        text->printf("%d", constant->int_val);
      }
      else if(constant->kind == eX86Constant::float_)
      {
        union BitcastFtoI
        {
          float float_val;
          int int_val;
        };

        union BitcastFtoI val = {0};
        val.float_val = constant->float_val;

        text->printf("0%xh", val.int_val);
      }
      else if(constant->kind == eX86Constant::char_)
      {
        if(constant->char_val >= ' ' && constant->char_val <= '~')
          text->printf("'%c'", constant->char_val);
        else
          text->printf("%d", constant->char_val);
      }
      else assert(0);
    }
    break;

    case eX86Operand::id:
    {
      text->printf("%s", operand->id);
    }
    break;

    default: assert(0);
  }
}

void x86_print_opcode(String* text, eX86Stmt opcode)
{
  switch(opcode)
  {
    case eX86Stmt::call:
      text->printf("call ");
    break;

    case eX86Stmt::pop:
      text->printf("pop ");
    break;

    case eX86Stmt::push:
      text->printf("push ");
    break;

    case eX86Stmt::lea:
      text->printf("lea ");
    break;

    /* integer ops */
    case eX86Stmt::mov:
      text->printf("mov ");
    break;

    case eX86Stmt::add:
      text->printf("add ");
    break;

    case eX86Stmt::sub:
      text->printf("sub ");
    break;

    case eX86Stmt::imul:
      text->printf("imul ");
    break;

    case eX86Stmt::cdq:
      text->printf("cdq");
    break;

    case eX86Stmt::idiv:
      text->printf("idiv ");
    break;

    case eX86Stmt::neg:
      text->printf("neg ");
    break;

    case eX86Stmt::cmp:
      text->printf("cmp ");
    break;

    case eX86Stmt::or_:
      text->printf("or ");
    break;

    case eX86Stmt::and_:
      text->printf("and ");
    break;

    case eX86Stmt::not_:
      text->printf("not ");
    break;

    /* integer jumps */
    case eX86Stmt::jz:
      text->printf("jz ");
    break;

    case eX86Stmt::jnz:
      text->printf("jnz ");
    break;

    case eX86Stmt::jl:
      text->printf("jl ");
    break;

    case eX86Stmt::jle:
      text->printf("jle ");
    break;

    case eX86Stmt::jg:
      text->printf("jg ");
    break;

    case eX86Stmt::jge:
      text->printf("jge ");
    break;

    /* floating point ops */
    case eX86Stmt::movss:
      text->printf("movss ");
    break;

    case eX86Stmt::addss:
      text->printf("addss ");
    break;

    case eX86Stmt::subss:
      text->printf("subss ");
    break;

    case eX86Stmt::mulss:
      text->printf("mulss ");
    break;

    case eX86Stmt::divss:
      text->printf("divss ");
    break;

    case eX86Stmt::ucomiss:
      text->printf("ucomiss ");
    break;

    /* floating point jumps */
    case eX86Stmt::jb:
      text->printf("jb ");
    break;

    case eX86Stmt::jbe:
      text->printf("jbe ");
    break;

    case eX86Stmt::ja:
      text->printf("ja ");
    break;

    case eX86Stmt::jae:
      text->printf("jae ");
    break;

    case eX86Stmt::je:
      text->printf("je ");
    break;

    case eX86Stmt::jne:
      text->printf("jne ");
    break;

    case eX86Stmt::jmp:
      text->printf("jmp ");
    break;

    case eX86Stmt::nop:
      //str_printfln("nop");
    break;

    case eX86Stmt::ret:
      text->printf("ret ");
    break;

    /* conversion ops */
    case eX86Stmt::cvtsi2ss:
      text->printf("cvtsi2ss ");
    break;

    case eX86Stmt::cvttss2si:
      text->printf("cvttss2si ");
    break;

    default: assert(0);
  }
}

void x86_print_stmt(String* text, X86Stmt* stmt)
{
  switch(stmt->opcode)
  {
    case eX86Stmt::label:
    {
      x86_print_operand(text, stmt->operand1);
      text->printf(":");
    }
    break;

    case eX86Stmt::extern_proc:
    {
      text->printf("extern ");
      x86_print_operand(text, stmt->operand1);
      text->printf(":proc");
    }
    break;

    default:
    {
      x86_print_opcode(text, stmt->opcode);
      if(stmt->operand1)
      {
        x86_print_operand(text, stmt->operand1);
      }

      if(stmt->operand2)
      {
        text->printf(", ");
        x86_print_operand(text, stmt->operand2);
      }
    }
  }

  text->println();
}

X86Stmt* x86_new_stmt(X86Context* context, eX86Stmt opcode)
{
  X86Stmt* stmt = mem_push_struct(context->stmt_arena, X86Stmt);
  context->stmt_count++;
  stmt->opcode = opcode;
  stmt->operand1 = stmt->operand2 = 0;

  return stmt;
}

internal inline
bool is_object_in_location(Symbol* object, X86Location* loc)
{
  return (object->locations._[(int)loc->kind] != 0);
}

void new_object_location_entry(X86Context* context, Symbol* object, X86Location* loc)
{
  object->locations._[(int)loc->kind] = loc;

  List* occupants = &loc->occupants;
  for(ListItem* li = occupants->first;
      li;
      li = li->next)
  {
    assert(object != KIND(li, eList::symbol)->symbol);
  }
  occupants->append(object, eList::symbol);
}

void delete_object_from_location(X86Context* context, Symbol* object, X86Location* loc)
{
  if(is_object_in_location(object, loc))
  {
    object->locations._[(int)loc->kind] = 0;

    List* occupants = &loc->occupants;
    ListItem* li = occupants->first;

    for(; li; li = li->next)
    {
      if(object == KIND(li, eList::symbol)->symbol)
        break;
    }
    if(li)
    {
      occupants->remove_item(li);
    }
  }
}

void add_object_to_location(X86Context* context, Symbol* object, X86Location* loc)
{
  if(is_object_in_location(object, loc))
  {
    List* occupants = &loc->occupants;
    ListItem* li = occupants->first;
    for(; li; li = li->next)
    {
      if(object == KIND(li, eList::symbol)->symbol)
        break;
    }
    assert(li);
  }
  else
  {
    new_object_location_entry(context, object, loc);
  }
}

bool reg_is_parent_free(X86Location* reg)
{
  bool is_free = true;

  List* occupants = &reg->occupants;

  if(occupants->count == 0)
  {
    if(reg->parent)
    {
      is_free = reg_is_parent_free(reg->parent);
    }
  }
  else
  {
    is_free = false;
  }

  return is_free;
}

bool reg_is_subloc_free(X86Location* reg)
{
  bool is_free = true;

  List* occupants = &reg->occupants;

  if(occupants->count == 0)
  {
    if(reg->subloc[0])
    {
      is_free = reg_is_subloc_free(reg->subloc[0]);
    }

    if(is_free && reg->subloc[1])
    {
      is_free = reg_is_subloc_free(reg->subloc[1]);
    }
  }
  else
  {
    is_free = false;
  }

  return is_free;
}

bool reg_is_free(X86Location* reg)
{
  bool is_free = true;

  List* occupants = &reg->occupants;

  if(occupants->count == 0)
  {
    if(reg->parent)
    {
      is_free = reg_is_parent_free(reg->parent);
    }

    if(is_free && reg->subloc[0])
    {
      is_free = reg_is_subloc_free(reg->subloc[0]);
    }

    if(is_free && reg->subloc[1])
    {
      is_free = reg_is_subloc_free(reg->subloc[1]);
    }
  }
  else
  {
    is_free = false;
  }

  return is_free;
}

X86Location* reg_get_top(X86Location* reg)
{
  X86Location* top = reg;

  if(reg->parent)
  {
    top = reg_get_top(reg->parent);
  }

  return top;
}

void clean_register(X86Context* context, X86Location* loc)
{
  List* occupants = &loc->occupants;
  for(ListItem* li = occupants->first;
      li;
      li = li->next)
  {
    Symbol* occupant = KIND(li, eList::symbol)->symbol;
    delete_object_from_location(context, occupant, loc);
  }
}

void clean_register_all_sizes(X86Context* context, X86Location* loc)
{
  clean_register(context, reg_get_top(loc));

  if(loc->subloc[0])
  {
    clean_register(context, loc->subloc[0]);
  }

  if(loc->subloc[1])
  {
    clean_register(context, loc->subloc[1]);
  }
}

void set_exclusive_object_location(X86Context* context, Symbol* object, X86Location* loc)
{
  for(int i = 0; i < context->register_count; i++)
  {
    delete_object_from_location(context, object, context->registers._[i]);
  }
  delete_object_from_location(context, object, &context->memory);

  add_object_to_location(context, object, loc);
}

internal inline
bool is_register_location(X86Context* context, X86Location* loc)
{
  bool is_register = false;

  for(int i = 0;
      i < context->register_count && !is_register;
      i++)
  {
    X86Location* reg = context->registers._[i];
    is_register = (reg == loc);
  }

  //XXX: make_register_operand() allows EBP and ESP parameters.
  is_register |= (loc == &context->ebp) | (loc == &context->esp);

  return is_register;
}

internal inline
bool is_memory_location(X86Context* context, X86Location* loc)
{
  return (loc == &context->memory);
}

bool type_fits_into_register(Type* type, X86Location* reg)
{
  bool result = false;

  if(type->equal(basic_type_bool)
     || type->kind == eType::pointer || type->kind == eType::array)
  {
    result = reg->type->equal(basic_type_int);
  }
  else
  {
    result = type->equal(reg->type);
  }

  return result;
}

X86Location* find_free_register(X86Context* context, Type* type)
{
  X86Location* reg = 0;

  for(int i = 0; i < context->register_count; i++)
  {
    reg = context->registers._[i];
    if(reg_is_free(reg) && type_fits_into_register(type, reg))
    {
      break;
    }
    reg = 0;
  }

  return reg;
}

X86Location* lookup_object_location(X86Context* context, Symbol* object)
{
  X86Location* loc = 0;

  for(int i = 0; i < context->register_count; i++)
  {
    loc = context->registers._[i];
    if(is_object_in_location(object, loc))
      break;
    loc = 0;
  }

  if(!loc)
  {
    loc = object->locations._[(int)eX86Location::memory];
  }

  return loc;
}

bool is_object_in_register(X86Context* context, Symbol* object)
{
  bool in_register = false;

  for(int i = 0;
      i < context->register_count && !in_register;
      i++)
  {
    X86Location* loc = object->locations._[i];
    in_register = (loc && is_register_location(context, loc));
  }

  return in_register;
}

bool is_single_occupant_register(X86Context* context, X86Location* reg, Symbol* object)
{
  assert(is_register_location(context, reg));

  bool result = false;
  List* occupants = &reg->occupants;
  if(occupants->count == 1)
  {
    Symbol* single_object = KIND(occupants->first, eList::symbol)->symbol;
    result = (single_object == object);
  }

  return result;
}

// X86 stack grows toward 0
X86Operand* x86_make_index_operand(X86Context* context, eX86Operand kind, Symbol* object)
{
  X86Operand* operand = mem_push_struct(context->gp_arena, X86Operand);
  operand->kind = kind;
  operand->index.type = object->ty;

  X86Operand* base = operand->index.base = mem_push_struct(context->gp_arena, X86Operand);
  X86Operand* offset = operand->index.offset = mem_push_struct(context->gp_arena, X86Operand);

  offset->kind = eX86Operand::constant;
  struct X86Operand_constant* constant = &offset->constant;
  constant->kind = eX86Constant::int_;

  switch(object->storage_space)
  {
    case eStorageSpace::local:
    {
      base->kind = eX86Operand::register_;
      base->reg = &context->ebp;

      constant->int_val = -(object->data_loc + object->allocd_size);
    }
    break;

    case eStorageSpace::static_:
    {
      base->kind = eX86Operand::id;
      base->id = "static_area";

      constant->int_val = object->data_loc;
    }
    break;

    case eStorageSpace::actual_param:
    {
      base->kind = eX86Operand::register_;
      base->reg = &context->esp;

      constant->int_val = -(object->data_loc + object->allocd_size);
    }
    break;

    case eStorageSpace::formal_param:
    {
      base->kind = eX86Operand::register_;
      base->reg = &context->ebp;

      int scope_allocd_size = object->scope->allocd_size;
      int machine_area_size = 2*context->machine_word_size; // instruction_pointer + frame_pointer
      constant->int_val = (scope_allocd_size + machine_area_size) - (object->data_loc + object->allocd_size);
    }
    break;

    default: assert(0);
  }

  return operand;
}

X86Operand* x86_make_register_operand(X86Context* context, X86Location* reg)
{
  assert(is_register_location(context, reg));

  X86Operand* operand = mem_push_struct(context->gp_arena, X86Operand);
  operand->kind = eX86Operand::register_;
  operand->reg = reg;

  return operand;
}

X86Operand* x86_make_int_constant_operand(X86Context* context, int int_val)
{
  X86Operand* operand = mem_push_struct(context->gp_arena, X86Operand);
  operand->kind = eX86Operand::constant;

  struct X86Operand_constant* constant = &operand->constant;
  constant->kind = eX86Constant::int_;
  constant->int_val = int_val;

  return operand;
}

X86Operand* x86_make_memory_operand(X86Context* context, Type* type, X86Operand* base, X86Operand* offset)
{
  X86Operand* operand = mem_push_struct(context->gp_arena, X86Operand);
  operand->kind = eX86Operand::memory;

  operand->index.type = type;
  operand->index.base = base;
  operand->index.offset = offset;

  return operand;
}

internal inline
X86Operand* x86_make_object_address_operand(X86Context* context, Symbol* object)
{
  return x86_make_index_operand(context, eX86Operand::address, object);
}

internal inline
X86Operand* x86_make_object_memory_operand(X86Context* context, Symbol* object)
{
  X86Operand* operand = 0;

  if(object->kind == eSymbol::constant)
  {
    if(object->ty->equal(basic_type_str))
    {
      operand = x86_make_object_address_operand(context, object);
    }
    else
    {
      operand = mem_push_struct(context->gp_arena, X86Operand);
      operand->kind = eX86Operand::constant;

      struct X86Operand_constant* constant = &operand->constant;

      if(object->ty->equal(basic_type_int))
      {
        constant->kind = eX86Constant::int_;
        constant->int_val = object->int_val;
      }
      else if(object->ty->equal(basic_type_float))
      {
        operand = x86_make_index_operand(context, eX86Operand::memory, object);
      }
      else if(object->ty->equal(basic_type_char))
      {
        constant->kind = eX86Constant::char_;
        constant->char_val = object->char_val;
      }
      else if(object->ty->equal(basic_type_bool))
      {
        constant->kind = eX86Constant::int_;
        constant->int_val = object->int_val;
      }
      else assert(0);
    }
  }
  else
  {
    operand = x86_make_index_operand(context, eX86Operand::memory, object);
  }

  return operand;
}

X86Operand* x86_make_object_operand(X86Context* context, Symbol* object)
{
  X86Operand* operand = 0;

  X86Location* object_loc = lookup_object_location(context, object);
  if(is_register_location(context, object_loc))
  {
    operand = x86_make_register_operand(context, object_loc);
  }
  else if(is_memory_location(context, object_loc))
  {
    operand = x86_make_object_memory_operand(context, object);
  }
  else assert(0);

  return operand;
}

X86Operand* x86_make_id_operand(X86Context* context, char* id)
{
  X86Operand* operand = mem_push_struct(context->gp_arena, X86Operand);
  operand->kind = eX86Operand::id;
  operand->id = id;
  return operand;
}

void x86_emit_mov(X86Context* context, Type* type, X86Operand* dest_operand, X86Operand* source_operand)
{
  eX86Stmt mov_op = eX86Stmt::None;

  if(type->equal(basic_type_int) || type->equal(basic_type_bool)
     || type->equal(basic_type_char)
     || (type->kind == eType::pointer) || (type->kind == eType::array))
  {
    mov_op = eX86Stmt::mov;
  }
  else if(type->equal(basic_type_float))
  {
    mov_op = eX86Stmt::movss;
  }

  X86Stmt* mov_stmt = x86_new_stmt(context, mov_op);
  mov_stmt->operand1 = dest_operand;
  mov_stmt->operand2 = source_operand;
}

void x86_load_object_value(X86Context* context, Symbol* object, X86Location* dest_loc)
{
  assert(is_register_location(context, dest_loc));

  if(!is_object_in_location(object, dest_loc))
  {
    x86_emit_mov(context, object->ty,
                 x86_make_register_operand(context, dest_loc),
                 x86_make_object_operand(context, object));
  }
}

void x86_load_object_address(X86Context* context, Symbol* object, X86Location* dest_loc)
{
  assert(is_register_location(context, dest_loc));

  if(!is_object_in_location(object, dest_loc))
  {
    X86Stmt* stmt = x86_new_stmt(context, eX86Stmt::lea);

    stmt->operand1 = x86_make_register_operand(context, dest_loc);
    stmt->operand2 = x86_make_object_address_operand(context, object);
  }
}

void x86_load_object(X86Context* context, Symbol* object, X86Location* dest_loc)
{
  assert(is_register_location(context, dest_loc));

  if(object->ty->kind == eType::array)
  {
    x86_load_object_address(context, object, dest_loc);
  }
  else
  {
    x86_load_object_value(context, object, dest_loc);
  }

  add_object_to_location(context, object, dest_loc);
}

void x86_store_object(X86Context* context, Symbol* object)
{
  if(!is_object_in_location(object, &context->memory) && (object->ty->kind != eType::array))
  {
    X86Location* object_loc = lookup_object_location(context, object);

    x86_emit_mov(context, object->ty,
                 x86_make_object_memory_operand(context, object),
                 x86_make_register_operand(context, object_loc));
  }
}

bool is_ir_cast_op(eIrOp ir_op)
{
  bool is_conv = false;

  switch(ir_op)
  {
    case eIrOp::itof:
    case eIrOp::itoc:
    case eIrOp::itob:
    case eIrOp::ftoi:
    case eIrOp::ctoi:
    case eIrOp::btoi:
      is_conv = true;
    break;
  }

  return is_conv;
}

eX86Stmt conv_ir_op_to_x86_opcode(eIrOp ir_op, Type* type)
{
  eX86Stmt x86_opcode = eX86Stmt::None;

  if(type->equal(basic_type_int) || type->equal(basic_type_bool)
     || type->equal(basic_type_char)
     || (type->kind == eType::pointer) || (type->kind == eType::array))
  {
    switch(ir_op)
    {
      case eIrOp::add:
        x86_opcode = eX86Stmt::add;
      break;

      case eIrOp::sub:
        x86_opcode = eX86Stmt::sub;
      break;

      case eIrOp::mul:
        x86_opcode = eX86Stmt::imul;
      break;

      case eIrOp::div:
        x86_opcode = eX86Stmt::idiv;
      break;

      case eIrOp::less:
        x86_opcode = eX86Stmt::jl;
      break;

      case eIrOp::less_eq:
        x86_opcode = eX86Stmt::jle;
      break;

      case eIrOp::greater:
        x86_opcode = eX86Stmt::jg;
      break;

      case eIrOp::greater_eq:
        x86_opcode = eX86Stmt::jge;
      break;

      case eIrOp::eq:
        x86_opcode = eX86Stmt::jz;
      break;

      case eIrOp::not_eq_:
        x86_opcode = eX86Stmt::jnz;
      break;

      case eIrOp::neg:
        x86_opcode = eX86Stmt::neg;
      break;

      case eIrOp::ftoi:
        x86_opcode = eX86Stmt::cvttss2si;
      break;

      case eIrOp::bit_or:
        x86_opcode = eX86Stmt::or_;
      break;

      case eIrOp::bit_and:
        x86_opcode = eX86Stmt::and_;
      break;

      case eIrOp::bit_not:
        x86_opcode = eX86Stmt::not_;
      break;

      default: assert(0);
    }
  }
  else if(type->equal(basic_type_float))
  {
    switch(ir_op)
    {
      case eIrOp::add:
        x86_opcode = eX86Stmt::addss;
      break;

      case eIrOp::sub:
        x86_opcode = eX86Stmt::subss;
      break;

      case eIrOp::mul:
        x86_opcode = eX86Stmt::mulss;
      break;

      case eIrOp::div:
        x86_opcode = eX86Stmt::divss;
      break;

      case eIrOp::less:
        x86_opcode = eX86Stmt::jb;
      break;

      case eIrOp::less_eq:
        x86_opcode = eX86Stmt::jbe;
      break;

      case eIrOp::greater:
        x86_opcode = eX86Stmt::ja;
      break;

      case eIrOp::greater_eq:
        x86_opcode = eX86Stmt::jae;
      break;

      case eIrOp::eq:
        x86_opcode = eX86Stmt::je;
      break;

      case eIrOp::not_eq_:
        x86_opcode = eX86Stmt::jne;
      break;

      case eIrOp::itof:
        x86_opcode = eX86Stmt::cvtsi2ss;
      break;

      default: assert(0);
    }
  }
  else assert(0);


  return x86_opcode;
}

X86Location* get_first_fit_register(X86Context* context, Type* type)
{
  X86Location* reg = 0;

  for(int i = 0; i < context->register_count; i++)
  {
    reg = context->registers._[i];

    if(type_fits_into_register(type, reg))
      break;

    reg = 0;
  }

  assert(reg);
  return reg;
}

X86Location* find_least_used_register(X86Context* context, Type* type)
{
  X86Location* result = 0;
  int next_use = 0;

  for(int i = 0; i < context->register_count; i++)
  {
    X86Location* reg = context->registers._[i];

    if(type_fits_into_register(type, reg))
    {
      List* occupants = &reg->occupants;

      for(ListItem* li = occupants->first;
          li;
          li = li->next)
      {
        Symbol* object = KIND(li, eList::symbol)->symbol;

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

void add_object_to_memory(X86Context* context, Symbol* object)
{
  add_object_to_location(context, object, &context->memory);
}

void x86_save_object_to_memory(X86Context* context, Symbol* object)
{
  x86_store_object(context, object);
  add_object_to_memory(context, object);
}

void x86_save_register(X86Context* context, X86Location* reg, bool free_reg)
{
  List* occupants = &reg->occupants;

  for(ListItem* li = occupants->first; li; )
  {
    ListItem* li_next = li->next;

    Symbol* object = KIND(li, eList::symbol)->symbol;
    if(object->is_live)
    {
      x86_save_object_to_memory(context, object);
    }

    if(free_reg)
    {
      delete_object_from_location(context, object, reg);
    }

    li = li_next;
  }
}

void x86_save_register_all_sizes(X86Context* context, X86Location* loc, bool free_reg)
{
  x86_save_register(context, reg_get_top(loc), free_reg);

  if(loc->subloc[0])
  {
    x86_save_register(context, loc->subloc[0], free_reg);
  }

  if(loc->subloc[1])
  {
    x86_save_register(context, loc->subloc[1], free_reg);
  }
}

X86Location* x86_get_best_available_register(X86Context* context, Type* type)
{
  X86Location* best_reg = 0;

  X86Location* free_reg = find_free_register(context, type);
  if(free_reg)
  {
    best_reg = free_reg;
  }
  else
  {
    best_reg = find_least_used_register(context, type);
    x86_save_register(context, best_reg, true);
  }

  return best_reg;
}

void discard_unused_arg(X86Context* context, IrArg* arg, X86Location* arg_loc)
{
  if(is_register_location(context, arg_loc)
     && (arg->next_use == NextUse_None && !arg->is_live))
  {
    delete_object_from_location(context, arg->object, arg_loc);
  }
}

void discard_all_unused_args(X86Context* context, IrStmt_Assign* assign)
{
  X86Location* arg1_loc = lookup_object_location(context, assign->arg1->object);
  discard_unused_arg(context, assign->arg1, arg1_loc);

  if(assign->arg2)
  {
    X86Location* arg2_loc = lookup_object_location(context, assign->arg2->object);
    discard_unused_arg(context, assign->arg2, arg2_loc);
  }
}

void x86_save_all_registers(X86Context* context, bool free_reg)
{
  for(int i = 0; i < context->register_count; i++)
  {
    X86Location* reg = context->registers._[i];
    x86_save_register(context, reg, free_reg);
  }
}

void x86_gen_divmod_op(X86Context* context, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  assert(assign->op == eIrOp::div || assign->op == eIrOp::mod);

  if(arg1->object->ty->equal(basic_type_int))
  {
    X86Location* remainder_loc = &context->edx;
    x86_save_register_all_sizes(context, remainder_loc, true);

    X86Location* dividend_loc = &context->eax;
    x86_save_register_all_sizes(context, dividend_loc, true);

    if(!is_object_in_location(arg1->object, dividend_loc))
    {
      x86_save_object_to_memory(context, arg1->object);
      x86_load_object(context, arg1->object, dividend_loc);
    }

    X86Stmt* x86_stmt = x86_new_stmt(context, eX86Stmt::cdq);

    X86Location* arg2_loc = lookup_object_location(context, arg2->object);
    if(!is_register_location(context, arg2_loc))
    {
      arg2_loc = x86_get_best_available_register(context, arg2->object->ty);
      x86_load_object(context, arg2->object, arg2_loc);
    }

    x86_stmt = x86_new_stmt(context, eX86Stmt::idiv);
    x86_stmt->operand1 = x86_make_object_operand(context, arg2->object);

    if(assign->op == eIrOp::div)
    {
      clean_register_all_sizes(context, dividend_loc);
      set_exclusive_object_location(context, result->object, dividend_loc);
    }
    else if(assign->op == eIrOp::mod)
    {
      clean_register_all_sizes(context, remainder_loc);
      set_exclusive_object_location(context, result->object, remainder_loc);
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
void x86_gen_index_source(X86Context* context, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  X86Location* result_loc = lookup_object_location(context, result->object);
  if(!is_register_location(context, result_loc))
  {
    result_loc = x86_get_best_available_register(context, result->object->ty);
  }

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);
  if(!is_register_location(context, arg1_loc))
  {
    arg1_loc = x86_get_best_available_register(context, arg1->object->ty);
    x86_load_object(context, arg1->object, arg1_loc);
  }

  X86Location* arg2_loc = lookup_object_location(context, arg2->object);
  if(!is_register_location(context, arg2_loc))
  {
    arg2_loc = x86_get_best_available_register(context, arg2->object->ty);
    x86_load_object(context, arg2->object, arg2_loc);
  }

  x86_emit_mov(context, result->object->ty,
               x86_make_register_operand(context, result_loc),
               x86_make_memory_operand(context, result->object->ty,
                                       x86_make_register_operand(context, arg1_loc),
                                       x86_make_register_operand(context, arg2_loc)));

  clean_register_all_sizes(context, result_loc);
  set_exclusive_object_location(context, result->object, result_loc);
}

// result[arg2] = arg1
void x86_gen_index_dest(X86Context* context, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  X86Location* result_loc = lookup_object_location(context, result->object);
  if(!is_register_location(context, result_loc))
  {
    result_loc = x86_get_best_available_register(context, result->object->ty);
    x86_load_object(context, result->object, result_loc);
  }

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);
  if(!is_register_location(context, arg1_loc))
  {
    arg1_loc = x86_get_best_available_register(context, arg1->object->ty);
    assert(arg1_loc != result_loc);
    x86_load_object(context, arg1->object, arg1_loc);
  }

  X86Location* arg2_loc = lookup_object_location(context, arg2->object);
  if(!is_register_location(context, arg2_loc))
  {
    arg2_loc = x86_get_best_available_register(context, arg2->object->ty);
    assert(arg2_loc != result_loc && arg2_loc != arg1_loc);
    x86_load_object(context, arg2->object, arg2_loc);
  }

  x86_emit_mov(context, arg1->object->ty,
               x86_make_memory_operand(context, arg1->object->ty,
                                       x86_make_register_operand(context, result_loc),
                                       x86_make_register_operand(context, arg2_loc)),
               x86_make_register_operand(context, arg1_loc));
}

// result = ^arg1
void x86_gen_deref_source(X86Context* context, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* result_loc = lookup_object_location(context, result->object);
  if(!is_register_location(context, result_loc))
  {
    result_loc = x86_get_best_available_register(context, result->object->ty);
  }

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);
  if(!is_register_location(context, arg1_loc))
  {
    arg1_loc = x86_get_best_available_register(context, arg1->object->ty);
    x86_load_object(context, arg1->object, arg1_loc);
  }

  x86_emit_mov(context, result->object->ty,
               x86_make_register_operand(context, result_loc),
               x86_make_memory_operand(context, result->object->ty,
                                       x86_make_register_operand(context, arg1_loc), 0));

  clean_register_all_sizes(context, result_loc);
  set_exclusive_object_location(context, result->object, result_loc);
}

// ^result = arg1
void x86_gen_deref_dest(X86Context* context, IrStmt_Assign *assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* result_loc = lookup_object_location(context, result->object);
  if(!is_register_location(context, result_loc))
  {
    result_loc = x86_get_best_available_register(context, result->object->ty);
    x86_load_object(context, result->object, result_loc);
  }

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);
  if(!is_register_location(context, arg1_loc))
  {
    arg1_loc = x86_get_best_available_register(context, arg1->object->ty);
    x86_load_object(context, arg1->object, arg1_loc);
  }

  x86_emit_mov(context, arg1->object->ty,
               x86_make_memory_operand(context, arg1->object->ty,
                                       x86_make_register_operand(context, result_loc), 0),
               x86_make_register_operand(context, arg1_loc));
}

// result = arg1
void x86_gen_equal(X86Context* context, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);

  if(is_register_location(context, arg1_loc))
  {
    set_exclusive_object_location(context, result->object, arg1_loc);
  }
  else if(is_memory_location(context, arg1_loc))
  {
    X86Location* result_loc = lookup_object_location(context, result->object);

    if(result->next_use == NextUse_None
       && is_register_location(context, result_loc)
       && is_single_occupant_register(context, result_loc, result->object))
    {
      x86_load_object(context, arg1->object, result_loc);
    }
    else
    {
      result_loc = x86_get_best_available_register(context, result->object->ty);
      x86_load_object(context, arg1->object, result_loc);
    }

    clean_register_all_sizes(context, result_loc);
    set_exclusive_object_location(context, result->object, result_loc);
  }
  else assert(0);
}

// result = &arg1
void x86_gen_address_of(X86Context* context, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* result_loc = x86_get_best_available_register(context, result->object->ty);
  x86_load_object_address(context, arg1->object, result_loc);

  clean_register_all_sizes(context, result_loc);
  set_exclusive_object_location(context, result->object, result_loc);
}

// result = arg1 op arg2
void x86_gen_bin_expr(X86Context* context, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  if((result->object->ty->equal(basic_type_int)
      || result->object->ty->equal(basic_type_char))
     && (assign->op == eIrOp::div || assign->op == eIrOp::mod))
  {
    if(assign->op == eIrOp::mod)
    {
      x86_gen_divmod_op(context, assign);
    }
    else if(assign->op == eIrOp::div)
    {
      x86_gen_divmod_op(context, assign);
    }
    else assert(0);
  }
  else
  {
    X86Location* arg1_loc = lookup_object_location(context, arg1->object);
    X86Location* result_loc = arg1_loc;

    if(is_register_location(context, arg1_loc)
       && is_single_occupant_register(context, arg1_loc, arg1->object)
       && (arg1->next_use == NextUse_None && !arg1->is_live))
    {
      delete_object_from_location(context, arg1->object, arg1_loc);
    }
    else
    {
      result_loc = x86_get_best_available_register(context, arg1->object->ty);
      x86_load_object(context, arg1->object, result_loc);
    }

    X86Stmt* x86_stmt = x86_new_stmt(context, conv_ir_op_to_x86_opcode(assign->op, result->object->ty));
    x86_stmt->operand1 = x86_make_register_operand(context, result_loc);
    x86_stmt->operand2 = x86_make_object_operand(context, arg2->object);

    clean_register_all_sizes(context, result_loc);
    set_exclusive_object_location(context, result->object, result_loc);
  }
}

// result = op arg1
void x86_gen_unr_expr(X86Context* context, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);
  X86Location* result_loc = arg1_loc;

  if(is_ir_cast_op(assign->op))
  {
    switch(assign->op)
    {
      /* converting ops */
      case eIrOp::itof:
      case eIrOp::ftoi:
      {
        result_loc = x86_get_best_available_register(context, result->object->ty);

        X86Stmt* x86_stmt = x86_new_stmt(context, conv_ir_op_to_x86_opcode(assign->op, result->object->ty));
        x86_stmt->operand1 = x86_make_register_operand(context, result_loc);
        x86_stmt->operand2 = x86_make_object_operand(context, arg1->object);
      }
      break;

      /* non-converting ops */
      case eIrOp::itob:
      case eIrOp::btoi:
      break;

      default:
        fail("todo");
    }
  }
  else
  {
    if(is_register_location(context, arg1_loc)
       && is_single_occupant_register(context, arg1_loc, arg1->object)
       && (arg1->next_use == NextUse_None && !arg1->is_live))
    {
      delete_object_from_location(context, arg1->object, arg1_loc);
    }
    else
    {
      result_loc = x86_get_best_available_register(context, arg1->object->ty);
      x86_load_object(context, arg1->object, result_loc);
    }

    if(assign->op == eIrOp::neg && arg1->object->ty->equal(basic_type_float))
    {
      X86Stmt* x86_stmt = x86_new_stmt(context, eX86Stmt::mulss);
      x86_stmt->operand1 = x86_make_register_operand(context, result_loc);
      x86_stmt->operand2 = x86_make_object_operand(context, context->float_minus_one);
    }
    else
    {
      X86Stmt* x86_stmt = x86_new_stmt(context, conv_ir_op_to_x86_opcode(assign->op, result->object->ty));
      x86_stmt->operand1 = x86_make_register_operand(context, result_loc);
    }
  }

  clean_register_all_sizes(context, result_loc);
  set_exclusive_object_location(context, result->object, result_loc);
}

void x86_gen_assign(X86Context* context, IrStmt_Assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  if(assign->op != eIrOp::None)
  {
    if(arg2)
    {
      switch(assign->op)
      {
        case eIrOp::index_source:
        {
          // result = arg1[arg2]
          x86_gen_index_source(context, assign);
        }
        break;

        case eIrOp::index_dest:
        {
          // result[arg2] = arg1
          x86_gen_index_dest(context, assign);
        }
        break;

        default:
        {
          // result = arg1 op arg2
          x86_gen_bin_expr(context, assign);
        }
      }
    }
    else
    {
      switch(assign->op)
      {
        case eIrOp::deref_source:
        {
          // result = ^arg1
          x86_gen_deref_source(context, assign);
        }
        break;

        case eIrOp::deref_dest:
        {
          // ^result = arg1
          x86_gen_deref_dest(context, assign);
        }
        break;

        case eIrOp::address_of:
        {
          // result = &arg1
          x86_gen_address_of(context, assign);
        }
        break;

        default:
        {
          // result = op arg1
          x86_gen_unr_expr(context, assign);
        }
      }
    }
  }
  else
  {
    // result = arg1
    x86_gen_equal(context, assign);
  }

  // IMPORTANT: The updated live-info is used in the find_least_used_register() function.
  // The update must be done *here*, so that when the next statement is processed,
  // a particular object will have the live-info of the previous last statement it appeared in.
  // As a bonus, the objects of 'result', 'arg1' and 'arg2' are automatically excluded from the find_least_used_register() search function.
  update_object_live_info(result, arg1, arg2);

  discard_all_unused_args(context, assign);
}

// goto L
void x86_gen_goto(X86Context* context, IrStmt_Goto* goto_)
{
  x86_save_all_registers(context, false);

  X86Stmt* jump_stmt = x86_new_stmt(context, eX86Stmt::jmp);
  jump_stmt->operand1 = x86_make_id_operand(context, goto_->goto_label->name);
}

// if arg1 relop arg2 goto L
void x86_gen_cond_goto(X86Context* context, IrStmt_CondGoto* cond_goto)
{
  IrArg* arg1 = cond_goto->arg1;
  IrArg* arg2 = cond_goto->arg2;

  x86_save_all_registers(context, false);

  if(!is_object_in_register(context, arg1->object))
  {
    X86Location* arg1_loc = x86_get_best_available_register(context, arg1->object->ty);
    x86_load_object(context, arg1->object, arg1_loc);
  }

  X86Stmt* cmp_stmt = 0;
  if(arg1->object->ty->equal(basic_type_int)
    || arg1->object->ty->equal(basic_type_char)
    || arg1->object->ty->equal(basic_type_bool))
  {
    cmp_stmt = x86_new_stmt(context, eX86Stmt::cmp);
  }
  else if(arg1->object->ty->equal(basic_type_float))
  {
    cmp_stmt = x86_new_stmt(context, eX86Stmt::ucomiss);
  }
  else assert(0);

  cmp_stmt->operand1 = x86_make_object_operand(context, arg1->object);
  cmp_stmt->operand2 = x86_make_object_operand(context, arg2->object);

  X86Stmt* jump_stmt = x86_new_stmt(context, conv_ir_op_to_x86_opcode(cond_goto->relop, arg1->object->ty));
  jump_stmt->operand1 = x86_make_id_operand(context, cond_goto->goto_label->name);
}

void x86_gen_call(X86Context* context, IrStmt_Call* call)
{
  x86_save_all_registers(context, true);

  /* sub esp, #param_size */
  X86Stmt* stmt = x86_new_stmt(context, eX86Stmt::sub);
  stmt->operand1 = x86_make_register_operand(context, &context->esp);
  stmt->operand2 = x86_make_int_constant_operand(context, call->param_scope->allocd_size);

  /* call #proc_name */
  stmt = x86_new_stmt(context, eX86Stmt::call);
  stmt->operand1 = x86_make_id_operand(context, call->name->name);

  if(call->is_extern)
  {
    /* add esp, #retvar_size */
    stmt = x86_new_stmt(context, eX86Stmt::add);
    stmt->operand1 = x86_make_register_operand(context, &context->esp);
    stmt->operand2 = x86_make_int_constant_operand(context, call->retvar->allocd_size);

    if(!call->retvar->ty->equal(basic_type_void))
    {
      clean_register_all_sizes(context, &context->eax);
      set_exclusive_object_location(context, call->retvar, &context->eax);
    }
  }
  else
  {
    /* add esp, #param_size */
    stmt = x86_new_stmt(context, eX86Stmt::add);
    stmt->operand1 = x86_make_register_operand(context, &context->esp);
    stmt->operand2 = x86_make_int_constant_operand(context, call->param_scope->allocd_size);

    add_object_to_memory(context, call->retvar);
  }
}

void x86_gen_basic_block(X86Context* context, BasicBlock* bb)
{
  if(bb->label)
  {
    X86Stmt* label_stmt = x86_new_stmt(context, eX86Stmt::label);
    label_stmt->operand1 = x86_make_id_operand(context, bb->label->name);
  }

  for(int i = 0; i < bb->stmt_count; i++)
  {
    IrStmt* ir_stmt = bb->stmt_array[i];

    switch(ir_stmt->kind)
    {
      case eIrStmt::assign:
      {
        x86_gen_assign(context, &ir_stmt->assign);
      }
      break;

      case eIrStmt::goto_:
      {
        // goto L
        x86_gen_goto(context, &ir_stmt->goto_);
      }
      break;

      case eIrStmt::cond_goto:
      {
        // if arg1 relop arg2 goto L
        x86_gen_cond_goto(context, &ir_stmt->cond_goto);
      }
      break;

      case eIrStmt::call:
      {
        x86_gen_call(context, &ir_stmt->call);
      }
      break;

      case eIrStmt::return_:
      case eIrStmt::nop:
      break;

      default: assert(0);
    }
  }
}

void x86_gen_extern_proc(X86Context* context, AstNode* proc)
{
  /* extern #proc_name:proc */
  X86Stmt* stmt = x86_new_stmt(context, eX86Stmt::extern_proc);
  Label* label_name = &proc->proc.label_name;
  stmt->operand1 = x86_make_id_operand(context, label_name->name);
}

void x86_gen_proc(X86Context* context, AstNode* proc)
{
  if(is_extern_proc(proc))
  {
    x86_gen_extern_proc(context, proc);
  }
  else
  {
    /* #proc_name: */
    X86Stmt* stmt = x86_new_stmt(context, eX86Stmt::label);
    stmt->operand1 = x86_make_id_operand(context, proc->proc.name);

    /* push ebp */
    stmt = x86_new_stmt(context, eX86Stmt::push);
    stmt->operand1 = x86_make_register_operand(context, &context->ebp);

    /* mov ebp, esp */
    stmt = x86_new_stmt(context, eX86Stmt::mov);
    stmt->operand1 = x86_make_register_operand(context, &context->ebp);
    stmt->operand2 = x86_make_register_operand(context, &context->esp);

    /* sub esp, #frame_size */
    stmt = x86_new_stmt(context, eX86Stmt::sub);
    stmt->operand1 = x86_make_register_operand(context, &context->esp);
    Scope* proc_scope = proc->proc.scope;
    stmt->operand2 = x86_make_int_constant_operand(context, proc_scope->allocd_size);

    List* basic_blocks = proc->proc.basic_blocks;

    for(ListItem* li = basic_blocks->first;
        li;
        li = li->next)
    {
      BasicBlock* bb = KIND(li, eList::basic_block)->basic_block;

      x86_gen_basic_block(context, bb);
      x86_save_all_registers(context, true);
    }

    /* mov esp, ebp */
    stmt = x86_new_stmt(context, eX86Stmt::mov);
    stmt->operand1 = x86_make_register_operand(context, &context->esp);
    stmt->operand2 = x86_make_register_operand(context, &context->ebp);

    /* pop ebp */
    stmt = x86_new_stmt(context, eX86Stmt::pop);
    stmt->operand1 = x86_make_register_operand(context, &context->ebp);

    /* ret */
    x86_new_stmt(context, eX86Stmt::ret);
  }
}

void x86_init_registers(X86Context* context)
{
  context->register_count = 0;

  /* eax */
  X86Location* loc = &context->eax;
  *loc = {};
  loc->kind = eX86Location::eax;
  loc->type = basic_type_int;
  context->registers._[context->register_count++] = loc;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* al */
  X86Location* subloc = &context->al;
  *subloc = {};
  loc->subloc[0] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location::al;
  subloc->type = basic_type_char;
  context->registers._[context->register_count++] = subloc;
  subloc->occupants.init(context->gp_arena, eList::symbol);

  /* ah */
  subloc = &context->ah;
  *subloc = {};
  loc->subloc[1] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location::ah;
  subloc->type = basic_type_char;
  context->registers._[context->register_count++] = subloc;
  subloc->occupants.init(context->gp_arena, eList::symbol);

  /* ebx */
  loc = &context->ebx;
  *loc = {};
  loc->kind = eX86Location::ebx;
  loc->type = basic_type_int;
  context->registers._[context->register_count++] = loc;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* bl */
  subloc = &context->bl;
  *subloc = {};
  loc->subloc[0] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location::bl;
  subloc->type = basic_type_char;
  context->registers._[context->register_count++] = subloc;
  subloc->occupants.init(context->gp_arena, eList::symbol);

  /* bh */
  subloc = &context->bh;
  *subloc = {};
  loc->subloc[1] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location::bh;
  subloc->type = basic_type_char;
  context->registers._[context->register_count++] = subloc;
  subloc->occupants.init(context->gp_arena, eList::symbol);

  /* ecx */
  loc = &context->ecx;
  *loc = {};
  loc->kind = eX86Location::ecx;
  loc->type = basic_type_int;
  context->registers._[context->register_count++] = loc;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* cl */
  subloc = &context->cl;
  *subloc = {};
  loc->subloc[0] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location::cl;
  subloc->type = basic_type_char;
  context->registers._[context->register_count++] = subloc;
  subloc->occupants.init(context->gp_arena, eList::symbol);

  /* ch */
  subloc = &context->ch;
  *subloc = {};
  loc->subloc[1] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location::ch;
  subloc->type = basic_type_char;
  context->registers._[context->register_count++] = subloc;
  subloc->occupants.init(context->gp_arena, eList::symbol);

  /* edx */
  loc = &context->edx;
  *loc = {};
  loc->kind = eX86Location::edx;
  loc->type = basic_type_int;
  context->registers._[context->register_count++] = loc;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* dl */
  subloc = &context->dl;
  *subloc = {};
  loc->subloc[0] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location::dl;
  subloc->type = basic_type_char;
  context->registers._[context->register_count++] = subloc;
  subloc->occupants.init(context->gp_arena, eList::symbol);

  /* dh */
  subloc = &context->dh;
  *subloc = {};
  loc->subloc[1] = subloc;
  subloc->parent = loc;
  subloc->kind = eX86Location::dh;
  subloc->type = basic_type_char;
  context->registers._[context->register_count++] = subloc;
  subloc->occupants.init(context->gp_arena, eList::symbol);

  /* esi */
  loc = &context->esi;
  *loc = {};
  loc->kind = eX86Location::esi;
  loc->type = basic_type_int;
  context->registers._[context->register_count++] = loc;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* edi */
  loc = &context->edi;
  *loc = {};
  loc->kind = eX86Location::edi;
  loc->type = basic_type_int;
  context->registers._[context->register_count++] = loc;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* memory */
  loc = &context->memory;
  *loc = {};
  loc->kind = eX86Location::memory;
  loc->type = basic_type_void;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* ebp */
  loc = &context->ebp;
  *loc = {};
  loc->kind = eX86Location::ebp;
  loc->type = basic_type_int;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* esp */
  loc = &context->esp;
  *loc = {};
  loc->kind = eX86Location::esp;
  loc->type = basic_type_int;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* xmm0 */
  loc = &context->xmm0;
  *loc = {};
  loc->kind = eX86Location::xmm0;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* xmm1 */
  loc = &context->xmm1;
  *loc = {};
  loc->kind = eX86Location::xmm1;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* xmm2 */
  loc = &context->xmm2;
  *loc = {};
  loc->kind = eX86Location::xmm2;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* xmm3 */
  loc = &context->xmm3;
  *loc = {};
  loc->kind = eX86Location::xmm3;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* xmm4 */
  loc = &context->xmm4;
  *loc = {};
  loc->kind = eX86Location::xmm4;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* xmm5 */
  loc = &context->xmm5;
  *loc = {};
  loc->kind = eX86Location::xmm5;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* xmm6 */
  loc = &context->xmm6;
  *loc = {};
  loc->kind = eX86Location::xmm6;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  loc->occupants.init(context->gp_arena, eList::symbol);

  /* xmm7 */
  loc = &context->xmm7;
  *loc = {};
  loc->kind = eX86Location::xmm7;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  loc->occupants.init(context->gp_arena, eList::symbol);

  assert(context->register_count <= sizeof_array(context->registers._));
}

void x86_write_data_bytes(String* text, uint8* p_data, int data_size)
{
  int i;
  for(i = 0; i < data_size - 1; i++)
  {
    text->printf("0%xh,", p_data[i]);
  }

  if(data_size > 0)
  {
    text->printf("0%xh", p_data[i]);
  }

  text->println();
}

void x86_write_static_data_text(String* text, Scope* scope)
{
  for(ListItem* li = scope->decl_syms.first;
      li;
      li = li->next)
  {
    Symbol* object = KIND(li, eList::symbol)->symbol;
    if(object->storage_space == eStorageSpace::static_ && object->allocd_size > 0)
    {
      if(object->data)
      {
        text->printf("byte ");

        x86_write_data_bytes(text, (uint8*)object->data, object->ty->width);

        int padding_size = object->allocd_size - object->ty->width;
        if(padding_size > 0)
        {
          text->printfln("byte %d dup(?)", padding_size);
        }
      }
      else
      {
        text->printfln("byte %d dup(?)", object->allocd_size);
      }
    }
  }
}

void x86_gen_module(X86Context* x86_context, AstNode* module)
{
  List* procs = &module->module.procs;
  for(ListItem* li = procs->first;
      li;
      li = li->next)
  {
    AstNode* proc = KIND(li, eList::ast_node)->ast_node;

    x86_gen_proc(x86_context, proc);
  }
}

void x86_gen(X86Context* x86_context, AstNode* module)
{
  x86_gen_module(x86_context, module);

  x86_context->text->printfln(".686");
  x86_context->text->printfln(".xmm");
  x86_context->text->printfln(".model flat, C");
  x86_context->text->printfln(".stack %d", 1*MEGABYTE);
  x86_context->text->printfln(".data");
  x86_context->text->printfln("static_area label byte");
  x86_context->text->printfln("align %d", x86_context->data_alignment);

  x86_write_static_data_text(x86_context->text, module->module.scope);

  x86_context->text->printfln(".code");
  x86_context->text->printfln("public startup");

  for(int i = 0; i < x86_context->stmt_count; i++)
  {
    x86_print_stmt(x86_context->text, &x86_context->stmt_array[i]);
  }

  x86_context->text->printfln("end");
}
