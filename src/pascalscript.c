/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <getopt.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "ps_buffer.h"
#include "ps_config.h"
#include "ps_interpreter.h"
#include "ps_lexer.h"
#include "ps_parser.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_version.h"

ps_interpreter *interpreter = NULL;

/* char *minimal_source = "Program Minimal;\n"
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
*/

/* char *hello_source =
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
*/

void usage(char *program_name)
{
    fprintf(stderr, "Usage: %s [-t] [-d] [-s] [-b] [-v] [program_file]\n", program_name);
    fprintf(stderr, "  -b : dump source buffer after loading\n");
    fprintf(stderr, "  -d : debug (more verbose trace)\n");
    fprintf(stderr, "  -n : do not execute program, just parse source code\n");
    fprintf(stderr, "  -s : dump symbols at initialization and termination\n");
    fprintf(stderr, "  -t : trace execution\n");
    fprintf(stderr, "  -v : verbose (display banner and other infos)\n");
}

int main(int argc, char *argv[])
{
    bool trace = false;
    bool debug = trace;
    bool verbose = false;
    bool dump_symbols = false;
    bool dump_buffer = false;
    bool exec = true;
    char *current_path = NULL;
    char *example_path = NULL;
    char *program_file = NULL;
    char source_file[256] = {0};

    int opt;
    int arg = 0;
    while ((opt = getopt(argc, argv, "ctdsbvn")) != -1)
    {
        switch (opt)
        {
        case 'c':
            ps_config_report();
            exit(0);
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
        case 'v':
            verbose = true;
            arg++;
            break;
        case 'n':
            exec = false;
            arg++;
            break;
        default:
            usage(argv[0]);
            exit(EXIT_FAILURE);
        }
    }

    current_path = getcwd(NULL, 0);
    size_t len = strlen(current_path);
    if (verbose)
        fprintf(stderr, "Current working directory: %s\n", current_path);
    example_path = "";
    if (arg + 1 < argc)
    {
        program_file = argv[argc - 1];
    }
    else
    {
        // gdb runs from src directory
        // fprintf(stderr, "cwd=%s", current_path);
        if (len > 3 && current_path[len - 4] == '/' && current_path[len - 3] == 's' && current_path[len - 2] == 'r' &&
            current_path[len - 1] == 'c')
        {
            example_path = "../examples";
        }
        else
        {
            example_path = "examples";
        }
        // program_file = "00-hello.pas";
        // program_file = "00-hello1.pas";
        // program_file = "00-minimal.pas";
        // program_file = "01-first.pas";
        // program_file = "02-second.pas";
        // program_file = "03-if-then-else.pas";
        // program_file = "04-repeat-until.pas";
        // program_file = "04-repeat-until-real.pas";
        // program_file = "05-while-do.pas";
        // program_file = "06-for-do.pas";
        // program_file = "07-random.pas";
        // program_file = "08-math.pas";
        // program_file = "09-boolean.pas";
        // program_file = "10-strings.pas";
        // program_file = "12-toayue-powersoftwo.pas";
        program_file = "20-procedure1.pas";
        // program_file = "41-circle.pas";
    }
    snprintf(source_file, sizeof(source_file) - 1, "%s/%s/%s", current_path, example_path, program_file);
    free(current_path);
    current_path = NULL;

    /* Display banner */
    if (verbose)
    {
        fprintf(stdout, "PascalScript v%d.%d.%d.%d - License: LGPL 3.0 or later, see LICENSE\n", PS_VERSION_MAJOR,
                PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_INDEX);
        fprintf(stderr, "Source file: %s\n", source_file);
    }

    /* Initialize interpreter */
    interpreter = ps_interpreter_alloc();
    if (interpreter == NULL)
    {
        fprintf(stderr, "Could not initialize interpreter!\n");
        return EXIT_FAILURE;
    }
    interpreter->debug = debug ? DEBUG_TRACE : trace ? DEBUG_VERBOSE : DEBUG_NONE;
    interpreter->parser->trace = interpreter->debug >= DEBUG_TRACE;
    interpreter->parser->debug = interpreter->debug >= DEBUG_VERBOSE;

    /* List symbols */
    if (dump_symbols)
        ps_symbol_table_dump(NULL, "Initialization",
                             interpreter->environments[PS_INTERPRETER_ENVIRONMENT_SYSTEM]->symbols);

    /* Load program source from string or file */
    // if (!ps_interpreter_load_string(interpreter, minimal_source, strlen(minimal_source)))
    // // if (!ps_interpreter_load_string(interpreter, hello_source, strlen(hello_source)))
    // {
    //     fprintf(stderr, "Source not loaded!\n");
    //     return EXIT_FAILURE;
    // }
    if (!ps_interpreter_load_file(interpreter, source_file))
    {
        fprintf(stderr, "File %s not loaded!\n", source_file);
        return EXIT_FAILURE;
    }
    if (verbose)
        fprintf(stderr, "Loaded %s!\n", source_file);

    /* List program */
    ps_lexer *lexer = ps_parser_get_lexer(interpreter->parser);
    if (dump_buffer)
    {
        ps_buffer_dump(lexer->buffer, 0, PS_BUFFER_MAX_LINES);
        if (verbose)
            fprintf(stderr, "Listed!\n");
    }

    /* Run program */
    if (verbose)
        printf("================================================================================\n");
    bool ok = ps_interpreter_run(interpreter, exec);
    if (verbose)
        printf("================================================================================\n");

    /* List symbols */
    if (dump_symbols)
        ps_symbol_table_dump(NULL, "End", interpreter->environments[PS_INTERPRETER_ENVIRONMENT_SYSTEM]->symbols);

    /* Terminate interpreter */
    ps_interpreter_free(interpreter);
    interpreter = NULL;
    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

/* EOF */
