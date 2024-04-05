/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "pascalscript.h"
#include "symbol_table.h"
#include "vm.h"

// #include "vm.c"
// #include "source.c"

vm_t _vm;
vm_t *vm = &_vm;

char *hello =
    "PROGRAM hello;\n"
    "CONST\n"
    "  PI = 3.14159265359;\n"
    "VAR\n"
    "  test: INTEGER;\n"
    "BEGIN\n"
    "  test := 1 + 2;\n"
    "  WriteLn('test=', test);\n"
    "END.\n";

int main(int argc, char *argv[])
{
  /* Initialize VM and display banner on stdout */
  vm_init(vm);
  // symbol_table_dump(&vm->globals,"Init");
  symbol_t *ps_version = vm_global_get(vm, "__PS_VERSION__");
  printf("PascalScript v%d.%d.%d.%d => %08x %d\n",
         PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_INDEX,
         ps_version->value.i, ps_version->value.i);
  if (vm_set_source(vm, hello, strlen(hello)))
  // if (vm_load_file(vm,"../examples/00-hello.pas"))
  {
    printf("Loaded!\n");
    vm_list_source(vm, 0, vm->line_count);
    // vm_exec(vm);
  }
  return 0;
}

/* EOF */
