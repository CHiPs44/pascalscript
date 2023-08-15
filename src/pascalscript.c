/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>

#include "pascalscript.h"
#include "symbol_table.h"
#include "vm.h"
#include "lexer.h"
#include "pascalscript.tab.h"

vm_t vm_data;
vm_t *vm = &vm_data;

int main(void)
{
  /* Initialize VM and display banner on stdout */
  vm_init(vm);
  // symbol_table_dump(&vm->globals,"Init");
  symbol_t *ps_version = vm_global_get(vm, "__PS_VERSION__");
  printf("PascalScript v%d.%d.%d.%d => %08x %d\n",
         PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_COUNT,
         ps_version->value.i, ps_version->value.i);
  /* Interpret stdin */
  return yyparse();
}

/* EOF */
