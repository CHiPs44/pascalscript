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
    // "Const Int1 = 1073741824;\n"
    "Const Int1 = 23456;\n"
    // "      Num1 = 123.45;\n"
    "      MinReal = 1.17549435082228750796873653722224568e-38;\n"
    "      MaxReal = 3.40282346638528859811704183484516925e38;\n"
    // "      Hex1 = $AABBCCDD;\n"
    // "      Oct1 = &1000;\n"
    // "      Bin1 = %11110000101010100000111101010101;\n"
    "Var   R : Real;\n"
    "Const Int2 = 10;\n"
    // "      Real1 = 12.34;\n"
    "Var   I : Integer;\n"
    "      U1 : Unsigned;\n"
    "      U2 : Unsigned;\n"
    "      U : Unsigned;\n"
    // "      C : Char;\n"
    "Begin\n"
    // "   WriteLn;\n"
    // "   WriteLn();\n"
    // "   { No strings yet! }\n"
    // "   WriteLn('M', 'i', 'n', 'R', '=', MinReal);\n"
    // "   WriteLn(MinReal);\n"
    // "   WriteLn('M', 'R', '=', MaxReal);\n"
    // "   U1 := Int1;\n"
    // "   U2 := Int2;\n"
    // "   Write('U'); Write('1'); Write('='); WriteLn(U1);\n"
    // "   Write('U'); Write('2'); Write('='); WriteLn(U2);\n"
    // "   U := (U1 + U2) * 2;\n"
    // "   Write('U'); Write('='); WriteLn(U);\n"
    // "   U := U + 1;\n"
    // "   Write('U'); Write('='); WriteLn(U);\n"
    // "   I := U div 2;\n"
    // "   Write('I'); Write('='); WriteLn(I);\n"
    // "   C := Chr(65);\n"
    // "   Write('C'); Write('='); WriteLn(C);\n"
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
    printf("Could not initialize interpreter!\n");
    return EXIT_FAILURE;
  }
  // interpreter->trace = false;
  interpreter->trace = true;
  interpreter->debug = interpreter->trace;
  interpreter->parser->trace = interpreter->trace;
  interpreter->parser->debug = interpreter->debug;
  // ps_symbol_table_dump(interpreter->parser->symbols, "Initialization", stderr);
  if (!ps_interpreter_load_string(interpreter, minimal_source, strlen(minimal_source)))
  // if (!ps_interpreter_load_string(interpreter, hello_source, strlen(hello_source)))
  // if (!ps_interpreter_load_string(interpreter, "examples/00-hello.pas"))
  {
    printf("Not loaded!\n");
    return 1;
  }
  printf("Loaded!\n");
  ps_lexer *lexer = ps_parser_get_lexer(interpreter->parser);
  // ps_buffer_dump(lexer->buffer, 0, PS_BUFFER_MAX_LINES);
  // printf("Listed!\n");
  ps_lexer_reset(lexer);
  ps_buffer_read_next_char(lexer->buffer);
  printf("================================================================================\n");
  bool ok = ps_interpreter_run(interpreter);
  printf("================================================================================\n");
  // ps_symbol_table_dump(interpreter->parser->symbols, "End", stderr);
  ps_interpreter_done(interpreter);
  interpreter = NULL;
  return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

/* EOF */
