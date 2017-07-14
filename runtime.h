#pragma once

void build_stmt(MemoryArena*, AstNode*);
void build_block_stmts(MemoryArena*, List*);

static int last_label_id;

