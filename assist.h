
#pragma once
#include "ast.h"
#include "exprfndef.h"
#include "exprstructdef.h"
#include "stringtable.h"

void assist_find_symbol(Node* n, Scope* sc, Name symbol);