/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "ps_version.h"
#include "ps_config.h"
#include "ps_lexer.h"
#include "ps_buffer.h"
#include "ps_parser.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_vm.h"

ps_vm _vm;
ps_vm *vm = &_vm;

char *minimal_source =
    "Program Minimal;\n"
    "Begin\n"
    "End.\n";

char *hello_source =
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
  ps_vm_init_runtime(vm);
  ps_symbol_table_dump(vm->symbols, "Init", stderr);
  ps_symbol *ps_version = ps_vm_global_get(vm, "__PS_VERSION__");
  printf("PascalScript v%d.%d.%d.%d => %08x %d\n",
         PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_INDEX,
         ps_version->value->data.i, ps_version->value->data.i);
  if (!ps_vm_load_source(vm, minimal_source, strlen(minimal_source)))
  // if (!ps_vm_load_source(vm, hello, strlen(hello)))
  // if (!ps_vm_load_source(vm,"examples/00-hello.pas"))
  {
    printf("Not loaded!\n");
    return 1;
  }
  printf("Loaded!\n");
  ps_buffer_dump(vm->parser->lexer->buffer, 0, PS_BUFFER_MAX_LINES - 1);
  printf("Listed!\n");
  ps_parser_start(vm->parser);
  return 0;
}

/* EOF */
