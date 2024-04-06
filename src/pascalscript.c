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
#include "lexer.h"
#include "source.h"

vm_t _vm;
vm_t *vm = &_vm;

char *hello =
    "program hello;\n"
    "{ comment with curly brackets }"
    "const\n"
    "  chips = 44;\n"
    "var\n"
    "  test: integer;\n"
    "begin\n"
    "(*\n"
    " multi-line comment enclosed in\n"
    " parenthesis plus stars\n"
    "*)\n"
    "  test := 1 + chips;\n"
    "  writeln('test=', test);\n"
    "end.\n";

int main(int argc, char *argv[])
{
  /* Initialize VM and display banner on stdout */
  vm_init(vm);
  // symbol_table_dump(&vm->globals,"Init");
  symbol_t *ps_version = vm_global_get(vm, "__PS_VERSION__");
  printf("PascalScript v%d.%d.%d.%d => %08x %d\n",
         PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_INDEX,
         ps_version->value.i, ps_version->value.i);
  if (ERROR_NONE == source_set_text(vm, hello, strlen(hello)))
  // if (source_load_file(vm,"../examples/00-hello.pas"))
  {
    printf("Loaded!\n");
    source_list_text(vm, 0, vm->line_count);
    // vm_exec(vm);
    token_t token;
    do {
      lexer_read_token(vm, &token);
      lexer_dump_token(&token);
    } while (true);
  }
  return 0;
}

/* EOF */
