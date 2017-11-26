DataArea* new_data_area(MemoryArena* arena, eDataArea kind)
{
 DataArea* result = mem_push_struct(arena, DataArea);
  result->kind = kind;
  return result;
}

int compute_area_loc(int sp, DataArea* area)
{
  area->loc = sp;
  sp = area->loc + area->size;
  if(area->subareas)
  {
    int loc = area->loc;
    for(ListItem* list_item = area->subareas->first;
        list_item;
        list_item = list_item->next)
    {
      DataArea* subarea = ITEM(list_item, data_area);
      subarea->loc = loc;
      loc = compute_area_loc(loc, subarea);
    }
    assert(loc == sp);
  }
  return sp;
}

void fixup_area_loc(int fp, List* areas)
{
  for(ListItem* list_item = areas->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* area = ITEM(list_item, data_area);
    area->loc = area->loc - fp;

    if(area->subareas)
    {
      fixup_area_loc(fp, area->subareas);
    }
  }
}

void compute_area_locations(List* pre_fp_areas, List* post_fp_areas)
{
  int sp = 0;
  int size = 0;
  for(ListItem* list_item = pre_fp_areas->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* area = ITEM(list_item, data_area);
    sp = compute_area_loc(sp, area);
    size += area->size;
  }
  assert(sp == size);

  int fp = sp;
  for(ListItem* list_item = post_fp_areas->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* area = ITEM(list_item, data_area);
    sp = compute_area_loc(sp, area);
    size += area->size;
  }
  assert(sp == size);

  fixup_area_loc(fp, pre_fp_areas);
  fixup_area_loc(fp, post_fp_areas);
}

void compute_decl_areas(Scope* scope, eSymbol* kind_set, DataArea* decl_area)
{
  for(eSymbol* kind = kind_set;
      *kind != eSymbol_None;
      kind++)
  {
    for(ListItem* list_item = scope->decls[*kind]->first;
        list_item;
        list_item = list_item->next)
    {
      Symbol* symbol = ITEM(list_item, symbol);
      DataArea* area = symbol->data_area = new_data_area(arena, eDataArea_var);
      area->size = symbol->type->width;
      area->data = symbol->data;

      decl_area->size += area->size;
      append_list_elem(decl_area->subareas, area, eList_data_area);
    }
  }
}

void compute_occur_areas(Scope* scope, eSymbol* kind_set, DataArea* link)
{
  for(eSymbol* kind = kind_set;
      *kind != eSymbol_None;
      kind++)
  {
    for(ListItem* list_item = scope->occurs[*kind]->first;
        list_item;
        list_item = list_item->next)
    {
      Symbol* occur_sym = ITEM(list_item, symbol);
      Symbol* decl_sym = occur_sym->decl;

      int decl_scope_offset = occur_sym->nesting_depth - decl_sym->nesting_depth;
      if(decl_scope_offset > 0)
      {
        occur_sym->data_area = link;
      }
      else if(decl_scope_offset == 0)
      {
        occur_sym->data_area = decl_sym->data_area;
      }
    }
  }
}

void build_runtime(List* scopes_list)
{
  for(ListItem* list_item = scopes_list->first;
      list_item;
      list_item = list_item->next)
  {
    Scope* scope = ITEM(list_item, scope);
    switch(scope->kind)
    {
      case eScope_global:
        break; //skip

      case eScope_module:
        {
          scope->pre_fp_areas = new_list(arena, eList_data_area);
          scope->post_fp_areas = new_list(arena, eList_data_area);

          DataArea* null_area = new_data_area(arena, eDataArea_var);
          append_list_elem(scope->pre_fp_areas, null_area, eList_data_area);
          int32* null = null_area->data = mem_push_struct(arena, int32);
          *null = 0xffffffff;
          null_area->size = sizeof(*null);

          DataArea* link_area = &scope->link_area;
          append_list_elem(scope->pre_fp_areas, link_area, eList_data_area);

          DataArea* ctrl_area = &scope->ctrl_area;
          append_list_elem(scope->pre_fp_areas, ctrl_area, eList_data_area);
          ctrl_area->size = 2*4; // IP + FP

          /*----------- FP --------------*/

          DataArea* locals_area = &scope->locals_area;
          locals_area->subareas = new_list(arena, eList_data_area);
          append_list_elem(scope->post_fp_areas, locals_area, eList_data_area);
          compute_decl_areas(scope, (eSymbol[]){eSymbol_var, eSymbol_None}, locals_area);

          compute_area_locations(scope->pre_fp_areas, scope->post_fp_areas);
        }
        break;

      case eScope_proc:
        {
          scope->pre_fp_areas = new_list(arena, eList_data_area);
          scope->post_fp_areas = new_list(arena, eList_data_area);

          DataArea* ret_area = &scope->ret_area;
          ret_area->subareas = new_list(arena, eList_data_area);
          append_list_elem(scope->pre_fp_areas, ret_area, eList_data_area);
          compute_decl_areas(scope, (eSymbol[]){eSymbol_ret_var, eSymbol_None}, ret_area);

          DataArea* args_area = &scope->args_area;
          args_area->subareas = new_list(arena, eList_data_area);
          append_list_elem(scope->pre_fp_areas, args_area, eList_data_area);
          compute_decl_areas(scope, (eSymbol[]){eSymbol_formal_arg, eSymbol_None}, args_area);

          DataArea* link_area = &scope->link_area;
          append_list_elem(scope->pre_fp_areas, link_area, eList_data_area);
          compute_occur_areas(scope, (eSymbol[])
              {eSymbol_var, eSymbol_ret_var, eSymbol_formal_arg, eSymbol_None}, link_area);

          DataArea* ctrl_area = &scope->ctrl_area;
          append_list_elem(scope->pre_fp_areas, ctrl_area, eList_data_area);
          ctrl_area->size = 2*4; // IP + FP

          /*----------- FP --------------*/

          DataArea* locals_area = &scope->locals_area;
          locals_area->subareas = new_list(arena, eList_data_area);
          append_list_elem(scope->post_fp_areas, locals_area, eList_data_area);
          compute_decl_areas(scope, (eSymbol[]){eSymbol_var, eSymbol_None}, locals_area);

          compute_area_locations(scope->pre_fp_areas, scope->post_fp_areas);
        }
        break;

      case eScope_block:
      case eScope_loop:
        {
          scope->pre_fp_areas = new_list(arena, eList_data_area);
          scope->post_fp_areas = new_list(arena, eList_data_area);

          DataArea* link_area = &scope->link_area;
          append_list_elem(scope->pre_fp_areas, link_area, eList_data_area);
          compute_occur_areas(scope, (eSymbol[])
              {eSymbol_var, eSymbol_ret_var, eSymbol_formal_arg, eSymbol_None}, link_area);

          DataArea* ctrl_area = &scope->ctrl_area;
          append_list_elem(scope->pre_fp_areas, ctrl_area, eList_data_area);
          ctrl_area->size = 2*4; // IP + FP

          /*----------- FP --------------*/

          DataArea* locals_area = &scope->locals_area;
          locals_area->subareas = new_list(arena, eList_data_area);
          append_list_elem(scope->post_fp_areas, locals_area, eList_data_area);
          compute_decl_areas(scope, (eSymbol[]){eSymbol_var, eSymbol_None}, locals_area);

          compute_area_locations(scope->pre_fp_areas, scope->post_fp_areas);
        }
        break;

      default:
        assert(0);
    }
  }
}


