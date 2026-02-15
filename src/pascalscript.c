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
#include "ps_memory.h"
#include "ps_parser.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_version.h"

#define DEBUGGER_SOURCE "examples/400-subrange.pas"

ps_interpreter *interpreter = NULL;

void banner(FILE *out)
{
    fprintf(out, "PascalScript v%s (%d bits) - License: LGPL 3.0 or later, see LICENSE\n", PS_VERSION, PS_BITNESS);
}

void usage(char *program_name)
{
    banner(stderr);
    fprintf(stderr, "Usage: %s [-t] [-d] [-s] [-b] [-v] [program_file]\n", program_name);
    fprintf(stderr, "Runtime options:\n");
    fprintf(stderr, "  -b : flips short circuit boolean evaluation (default: false, {$B})\n");
    fprintf(stderr, "  -i : flips I/O error checking (default: true, ${I})\n");
    fprintf(stderr, "  -r : flips range checking (default: true, {$R})\n");
    fprintf(stderr, "Other options:\n");
    fprintf(stderr, "  -c : display configuration and exit\n");
    fprintf(stderr, "  -d : debug (more verbose trace)\n");
    fprintf(stderr, "  -h : display this help message and exit\n");
    fprintf(stderr, "  -m : display memory usage at the end\n");
    fprintf(stderr, "  -n : do not execute program, just parse source code\n");
    fprintf(stderr, "  -s : dump symbols at initialization and termination\n");
    fprintf(stderr, "  -t : trace execution\n");
    fprintf(stderr, "  -u : dump source buffer after loading\n");
    fprintf(stderr, "  -v : verbose (display banner and other infos)\n");
    fprintf(stderr, "  program_file : path to the Pascal source file to run (default: %s)\n", DEBUGGER_SOURCE);
}

int main(int argc, char *argv[])
{
    // Runtime options
    bool bool_eval = false;
    bool io_check = true;
    bool range_check = true;
    // Others options
    bool debug = false;
    bool dump_buffer = false;
    bool dump_symbols = false;
    bool exec = true;
    bool memory = false;
    bool trace = false;
    bool verbose = false;
    // Paths & file names
    char *current_path = NULL;
    char *example_path = NULL;
    char *program_file = NULL;
    char source_file[256] = {0};

    // Force when debugging as I didn't find how to pass command line options
    // trace = true;
    // debug = true;

    int opt;
    int arg = 0;
    while ((opt = getopt(argc, argv, "bircdhmnstuv")) != -1)
    {
        switch (opt)
        {
        case 'b':
            bool_eval = !bool_eval;
            arg++;
            break;
        case 'i':
            io_check = !io_check;
            arg++;
            break;
        case 'r':
            range_check = !range_check;
            arg++;
            break;
        case 'c':
            ps_config_report(stdout);
            exit(EXIT_SUCCESS);
        case 'd':
            debug = true;
            arg++;
            break;
        case 'h':
            usage(argv[0]);
            exit(EXIT_SUCCESS);
        case 'm':
            memory = true;
            arg++;
            break;
        case 'n':
            exec = false;
            arg++;
            break;
        case 's':
            dump_symbols = true;
            arg++;
            break;
        case 't':
            trace = true;
            arg++;
            break;
        case 'u':
            dump_buffer = true;
            arg++;
            break;
        case 'v':
            verbose = true;
            arg++;
            break;
        default:
            usage(argv[0]);
            exit(EXIT_FAILURE);
        }
    }

    /* Display banner and intepreter runtime options */
    if (verbose)
    {
        banner(stdout);
        fprintf(stdout, "Runtime options:\n");
        fprintf(stdout, " - boolean evaluation: $B%c (*FUTURE*)\n", bool_eval ? '+' : '-');
        fprintf(stdout, " - IO check          : $I%c (*FUTURE*)\n", io_check ? '+' : '-');
        fprintf(stdout, " - Range check       : $R%c\n", range_check ? '+' : '-');
    }

    current_path = getcwd(NULL, 0);
    if (verbose)
        fprintf(stderr, "Current working directory: %s\n", current_path);
    example_path = "";
    if (arg + 1 < argc)
    {
        program_file = argv[argc - 1];
        snprintf(source_file, sizeof(source_file) - 1, "%s/%s", current_path, program_file);
    }
    else
    {
        example_path = "../examples";
        if (verbose)
            fprintf(stderr, "Example path: %s\n", example_path);
        program_file = DEBUGGER_SOURCE;
        if (program_file != NULL)
            snprintf(source_file, sizeof(source_file) - 1, "%s/%s/%s", current_path, example_path, program_file);
        else
            source_file[0] = '\0';
    }
    free(current_path);
    current_path = NULL;
    if (strlen(source_file) == 0)
    {
        fprintf(stderr, "No file to run!\n");
        usage(argv[0]);
        return EXIT_FAILURE;
    }
    if (verbose)
    {
        fprintf(stderr, "Source file: %s\n", source_file);
    }

    /* Initialize interpreter */
    interpreter = ps_interpreter_alloc(range_check, bool_eval, io_check);
    if (interpreter == NULL)
    {
        fprintf(stderr, "Could not initialize interpreter!\n");
        return EXIT_FAILURE;
    }
    interpreter->debug = trace ? DEBUG_VERBOSE : debug ? DEBUG_TRACE : DEBUG_NONE;
    interpreter->parser->trace = interpreter->debug >= DEBUG_TRACE;
    interpreter->parser->debug = interpreter->debug >= DEBUG_VERBOSE;

    /* List symbols */
    if (dump_symbols)
        ps_symbol_table_dump(NULL, "Initialization",
                             interpreter->environments[PS_INTERPRETER_ENVIRONMENT_SYSTEM]->symbols);

    if (!ps_interpreter_load_file(interpreter, source_file))
    {
        fprintf(stderr, "File %s not loaded!\n", source_file);
        fprintf(stderr, "Error %d %s\n", interpreter->error, ps_error_get_message(interpreter->error));
        fprintf(stderr, "Error %d %s\n", interpreter->parser->error, ps_error_get_message(interpreter->parser->error));
        fprintf(stderr, "Error %d %s\n", interpreter->parser->lexers[0]->error,
                ps_error_get_message(interpreter->parser->lexers[0]->error));
        interpreter = ps_interpreter_free(interpreter);
        return EXIT_FAILURE;
    }
    if (verbose)
        fprintf(stderr, "Loaded %s!\n", source_file);

    /* List program */
    if (dump_buffer)
    {
        ps_lexer *lexer = ps_parser_get_lexer(interpreter->parser);
        ps_buffer_dump(stderr, lexer->buffer, 0, PS_BUFFER_MAX_LINES);
        if (verbose)
            fprintf(stderr, "Listed!\n");
    }

    /* Run program */
    if (verbose)
        printf("===================================== BEGIN ====================================\n");
    bool ok = ps_interpreter_run(interpreter, exec);
    if (verbose)
        printf("====================================== END =====================================\n");

    /* List symbols */
    if (dump_symbols)
        ps_symbol_table_dump(NULL, "End", interpreter->environments[PS_INTERPRETER_ENVIRONMENT_SYSTEM]->symbols);

    if (memory)
        ps_memory_debug(stderr);

    /* Terminate interpreter */
    interpreter = ps_interpreter_free(interpreter);
    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

/* EOF */
