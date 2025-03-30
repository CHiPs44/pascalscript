/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "ps_buffer.h"
#include "ps_config.h"
#include "ps_lexer.h"
#include "ps_parser.h"
#include "ps_interpreter.h"
#include "ps_symbol_table.h"
#include "ps_symbol.h"
#include "ps_version.h"

ps_interpreter *interpreter = NULL;

char *minimal_source =
    "Program Minimal;\n"
    // "Const Int1 = 12345;\n"
    // "      Num1 = 123.45;\n"
    // "      Num2 = 1.17549435082228750796873653722224568e-38;\n"
    // "      Num3 = 3.40282346638528859811704183484516925e38;\n"
    // "      Hex1 = $AABBCCDD;\n"
    // "      Oct1 = &1000;\n"
    // "      Bin1 = %1111000010101010;\n"
    // "Var   R : Real;\n"
    // "      U : Unsigned;\n"
    // "Const Int2 = 54321;\n"
    "Var   I : Integer;\n"
    "Begin\n"
    // "  {BBB := AAA;}\n"
    " I := 12345;\n"
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
  /* Display banner on stdout */
  // ps_symbol *ps_version = ps_vm_global_get(vm, "__PS_VERSION__");
  printf(
      "PascalScript v%d.%d.%d.%d\n",
      PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_INDEX);
  interpreter = ps_interpreter_init(NULL);
  if (interpreter == NULL)
  {
    printf("Could not allocate interpreter!\n");
    return 1;
  }
  interpreter->parser->trace = true;
  interpreter->parser->debug = false;
  fprintf(
      stderr,
      "interpreter: %p, parser %p, symbols %p\n",
      interpreter, interpreter->parser, interpreter->parser->symbols);
  ps_symbol_table_dump(interpreter->parser->symbols, "Initialization", stderr);
  if (!ps_interpreter_load_string(interpreter, minimal_source, strlen(minimal_source)))
  // if (!ps_interpreter_load_string(interpreter, hello_source, strlen(hello_source)))
  // if (!ps_interpreter_load_string(interpreter, "examples/00-hello.pas"))
  {
    printf("Not loaded!\n");
    return 1;
  }
  printf("Loaded!\n");
  ps_lexer *lexer = ps_parser_get_lexer(interpreter->parser);
  ps_buffer_dump(lexer->buffer, 0, PS_BUFFER_MAX_LINES - 1);
  printf("Listed!\n");
  ps_lexer_reset(lexer);
  ps_buffer_read_next_char(lexer->buffer);
  if (ps_interpreter_run(interpreter))
    ps_symbol_table_dump(interpreter->parser->symbols, "End", stderr);
  ps_interpreter_done(interpreter);
  interpreter = NULL;
  return 0;
}

/* EOF */
