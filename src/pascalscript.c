/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "ps_config.h"
#include "ps_lexer.h"
#include "ps_buffer.h"
#include "ps_parser.h"
#include "ps_symbol_table.h"
#include "ps_symbol.h"
// #include "ps_vm.h"

// vm_t _vm;
// vm_t *vm = &_vm;

char *minimal =
    "PROGRAM Minimal;\n"
    "BeGiN\n"
    "End.\n";

char *hello =
    "program hello;\n"
    "{ comment with curly brackets }\n"
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
  // vm_init(vm);
  symbol_table_dump(&vm->symbols, "Init");
  symbol_t *ps_version = vm_global_get(vm, "__PS_VERSION__");
  printf("PascalScript v%d.%d.%d.%d => %08x %d\n",
         PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_INDEX,
         ps_version->value.i, ps_version->value.i);
  if (!buffer_set_text(vm, minimal, strlen(minimal)))
  // if (!buffer_set_text(vm, hello, strlen(hello)))
  // if (!buffer_load_file(vm,"examples/00-hello.pas"))
  {
    printf("Not loaded!\n");
    return 1;
  }
  printf("Loaded!\n");
  buffer_dump(vm, 0, BUFFER_MAX_LINES);
  printf("Listed!\n");
  parser_start(vm);
  return 0;
}

/* EOF */
