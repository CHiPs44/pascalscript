/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "ps_config.h"
#include "ps_lexer.h"
#include "ps_source.h"
#include "ps_symbol_table.h"
#include "ps_symbol.h"
#include "ps_vm.h"

vm_t _vm;
vm_t *vm = &_vm;

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
  vm_init(vm);
  symbol_table_dump(&vm->symbols, "Init");
  symbol_t *ps_version = vm_global_get(vm, "__PS_VERSION__");
  printf("PascalScript v%d.%d.%d.%d => %08x %d\n",
         PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_INDEX,
         ps_version->value.i, ps_version->value.i);
  if (ERROR_ZERO == source_set_text(vm, minimal, strlen(minimal)))
  // if (ERROR_ZERO == source_set_text(vm, hello, strlen(hello)))
  // if (source_load_file(vm,"examples/00-hello.pas"))
  {
    printf("Loaded!\n");
    source_list_text(vm, 0, vm->line_count);
    printf("Listed!\n");
    symbol_t program;
    do
    {
      lexer_read_identifier_or_keyword(vm);
      if (vm->current_token.type != TOKEN_PROGRAM)
      {
        lexer_dump_token(&vm->current_token);
        fprintf(stderr, "PROGRAM expected!");
        return 1;
      }
      lexer_read_identifier_or_keyword(vm);
      lexer_dump_token(&vm->current_token);
      if (vm->current_token.type != TOKEN_IDENTIFIER)
      {
        fprintf(stderr, "IDENTIFIER expected!");
        return 1;
      }
      program.kind = KIND_CONSTANT;
      strcpy(program.name, "PROGRAM");
      program.size = 0;
      program.type = TYPE_STRING;
      strcpy(program.value.s, vm->current_token.value.s);
      symbol_table_add(&vm->symbols, &program);
      symbol_table_dump(&vm->symbols, "Program");
      lexer_read_identifier_or_keyword(vm);
      if (vm->current_token.type != TOKEN_BEGIN)
      {
        fprintf(stderr, "BEGIN expected!");
        return 1;
      }
      lexer_read_identifier_or_keyword(vm);
      if (vm->current_token.type != TOKEN_END)
      {
        fprintf(stderr, "END expected!");
        return 1;
      }
      // lexer_read_token(vm, &vm->current_token);
      // if (vm->current_token.type != TOKEN_BEGIN)
      // {
      //   fprintf(stderr, ". expected!");
      //   return 1;
      // }
    } while (false);
  }
  return 0;
}

/* EOF */
