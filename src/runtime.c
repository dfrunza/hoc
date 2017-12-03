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
      area->value = symbol->data;

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


