/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>

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
    "Const Int1 = 1073741824;\n"
    "Const Num1 = 123.45;\n"
    "      MinReal2 = 1.17549435082228750796873653722224568e-38;\n"
    "      MaxReal2 = 3.40282346638528859811704183484516925e38;\n"
    "      EpsReal2 = 1.19209289550781250000000000000000000e-7;\n"
    "      Hex1 = $AABBCCDD;\n"
    "      Oct1 = &1000;\n"
    "      Bin1 = %11110000101010100000111101010101;\n"
    "Var   R : Real;\n"
    "Const Int2 = 10;\n"
    "      Real1 = 12.34;\n"
    "Var   I : Integer;\n"
    "      U1 : Unsigned;\n"
    "      U2 : Unsigned;\n"
    "      U : Unsigned;\n"
    "      C : Char;\n"
    "      B: Boolean;\n"
    "Begin\n"
    "   WriteLn;\n"
    "   WriteLn();\n"
    "   { No strings yet! }\n"
    "   WriteLn('M', 'i', 'n', 'R', 'e', 'a', 'l', '=', MinReal);\n"
    "   WriteLn('M', 'a', 'x', 'R', 'e', 'a', 'l', '=', MaxReal);\n"
    "   B := MinReal > MaxReal;\n"
    "   WriteLn(B);\n"
    "   U1 := Int1;\n"
    "   U2 := Int2;\n"
    "   WriteLn('U', '1', '=', U1);\n"
    "   WriteLn('U', '2', '=', U2);\n"
    "   U := (U1 + U2) * 2;\n"
    "   WriteLn('U', '=', U);\n"
    "   U := U + 1;\n"
    "   WriteLn('U', '=', U);\n"
    "   I := U div 2;\n"
    "   WriteLn('I', '=', I);\n"
    "   C := Chr(65);\n"
    "   WriteLn('C', '=', C);\n"
    "End.\n";

/*char *hello_source =
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
    "end.\n";*/

int main(int argc, char *argv[])
{
  bool trace = false; // argc > 1 && strcmp(argv[1], "-t") == 0;
  bool debug = trace;
  bool dump_symbols = false;
  bool dump_buffer = false;
  char *current_path = NULL;
  // char *program_file = "./examples/01-first.pas";
  char *program_file = NULL;
  char source_file[256] = {0};

  int opt;
  int arg = 0;
  while ((opt = getopt(argc, argv, "tdsb")) != -1)
  {
    switch (opt)
    {
    case 't':
      trace = true;
      arg++;
      break;
    case 'd':
      debug = true;
      arg++;
      break;
    case 's':
      dump_symbols = true;
      arg++;
      break;
    case 'b':
      dump_buffer = true;
      arg++;
      break;
    default:
      fprintf(stderr, "Usage: %s [-t] [-d] [-s] [-b] [program_file]\n", argv[0]);
      exit(EXIT_FAILURE);
    }
  }

  current_path = getcwd(NULL, 0);
  size_t len = strlen(current_path);
  fprintf(stderr, "Current working directory: %s\n", current_path);
  if (arg + 1 < argc)
  {
    program_file = argv[argc - 1];
  }
  else
  {
    // gdb runs from src directory
    if (len > 3 &&
        current_path[len - 4] == '/' &&
        current_path[len - 3] == 's' &&
        current_path[len - 2] == 'r' &&
        current_path[len - 1] == 'c')
    {
      // program_file = "../examples/01-first.pas";
      // program_file = "../examples/03-if-then-else.pas";
      // program_file = "../examples/04-repeat-until.pas";
      program_file = "../examples/04-repeat-until-real.pas";
      // program_file = "../examples/05-while-do.pas";
      // program_file = "../examples/06-for-do.pas";
      // program_file = "../examples/41-circle.pas";
    }
    else
    {
      // program_file = "/examples/01-first.pas";
      // program_file = "examples/03-if-then-else.pas";
      // program_file = "examples/04-repeat-until.pas";
      program_file = "examples/04-repeat-until-real.pas";
      // program_file = "examples/05-while-do.pas";
      // program_file = "examples/06-for-do.pas";
      // program_file = "examples/41-circle.pas";
    }
  }
  snprintf(source_file, sizeof(source_file) - 1, "%s/%s", current_path, program_file);
  fprintf(stderr, "Source file: %s\n", source_file);
  // exit(EXIT_FAILURE);

  /* Display banner on stdout */
  printf("PascalScript v%d.%d.%d.%d\n",
         PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_INDEX);

  /* Initialize interpreter */
  interpreter = ps_interpreter_init(NULL);
  if (interpreter == NULL)
  {
    printf("Could not initialize interpreter!\n");
    return EXIT_FAILURE;
  }
  interpreter->trace = trace;
  interpreter->debug = debug;
  interpreter->parser->trace = interpreter->trace;
  interpreter->parser->debug = interpreter->debug;

  /* List symbols */
  if (dump_symbols)
  {
    ps_symbol_table_dump(interpreter->parser->symbols, "Initialization", stderr);
  }

  /* Load program source from string or file */
  if (current_path == NULL)
  {
    if (!ps_interpreter_load_string(interpreter, minimal_source, strlen(minimal_source)))
    // if (!ps_interpreter_load_string(interpreter, hello_source, strlen(hello_source)))
    {
      fprintf(stderr, "Source not loaded!\n");
      return EXIT_FAILURE;
    }
  }
  else
  {
    if (!ps_interpreter_load_file(interpreter, source_file))
    {
      fprintf(stderr, "File %s not loaded!\n", source_file);
      return EXIT_FAILURE;
    }
  }
  fprintf(stderr, "Loaded!\n");

  /* List program */
  ps_lexer *lexer = ps_parser_get_lexer(interpreter->parser);
  if (dump_buffer)
  {
    ps_buffer_dump(lexer->buffer, 0, PS_BUFFER_MAX_LINES);
    fprintf(stderr, "Listed!\n");
  }

  /* Run program */
  printf("================================================================================\n");
  ps_lexer_reset(lexer);
  ps_buffer_read_next_char(lexer->buffer);
  bool ok = ps_interpreter_run(interpreter, true);
  printf("================================================================================\n");

  /* List symbols */
  if (dump_symbols)
  {
    ps_symbol_table_dump(interpreter->parser->symbols, "End", stderr);
  }

  /* Terminate interpreter */
  ps_interpreter_done(interpreter);
  interpreter = NULL;
  return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

/* EOF */
