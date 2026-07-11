/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_ast_test.h"

// int main(int argc, char *argv[])
// {
//     (void)argc; // silence unused variable warning
//     (void)argv; // silence unused variable warning

//     bool result = ps_ast_test();

//     return result ? EXIT_SUCCESS : EXIT_FAILURE;
// }

#include <assert.h>
#include <getopt.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "ps_buffer.h"
#include "ps_compiler.h"
#include "ps_config.h"
#include "ps_interpreter.h"
#include "ps_lexer.h"
#include "ps_memory.h"
#include "ps_parser.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_version.h"

#define DEBUGGER_SOURCE "examples/000-minimal.pas"
// #define DEBUGGER_SOURCE "examples/002-test-expr1.pas"
// #define DEBUGGER_SOURCE "examples/005-first.pas"
// TODO needs variable handle management
// #define DEBUGGER_SOURCE "examples/007-strings2.pas"
// #define DEBUGGER_SOURCE "examples/010-operators.pas"

// Runtime options
bool bool_eval = false;
bool io_check = true;
bool range_check = true;

// Others options
bool ast_test = false;
bool run = false;
bool debug = false;
bool dump_buffer = false;
bool dump_symbols = false;
bool exec = true;
bool memory = false;
bool trace = false;
bool verbose = false;

ps_ast_block *system_block = NULL;
ps_compiler *compiler = NULL;
ps_ast_block *program = NULL;
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
    fprintf(stderr, "  -a : launch AST tests\n");
    fprintf(stderr, "  -c : display configuration and exit\n");
    fprintf(stderr, "  -d : debug (more verbose trace)\n");
    fprintf(stderr, "  -h : display this help message and exit\n");
    fprintf(stderr, "  -m : display memory usage at end\n");
    fprintf(stderr, "  -n : do not execute program, just parse source code\n");
    fprintf(stderr, "  -s : dump symbols at initialization and termination\n");
    fprintf(stderr, "  -t : trace execution\n");
    fprintf(stderr, "  -u : dump source buffer after loading\n");
    fprintf(stderr, "  -v : verbose (display banner and other infos)\n");
    fprintf(stderr, "  program_file : path to the Pascal source file to run (default: %s)\n", DEBUGGER_SOURCE);
}

int get_options(int argc, char *argv[])
{
    int opt;
    int arg = 0;
    while ((opt = getopt(argc, argv, "abircdhmnstuv")) != -1)
    {
        switch (opt)
        {
        case 'a':
            ast_test = true;
            break;
        case 'c':
            ps_config_report(stdout);
            exit(EXIT_SUCCESS);
        case 'h':
            usage(argv[0]);
            exit(EXIT_SUCCESS);
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
        case 'd':
            debug = true;
            arg++;
            break;
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
    return arg;
}

bool compile(const char *source_file)
{
    assert(NULL != compiler);

    bool ok = false;

    compiler->debug = PS_DEBUG_FATAL;
    if (trace)
        compiler->debug = PS_DEBUG_VERBOSE;
    else if (debug)
        compiler->debug = PS_DEBUG_TRACE;

    if (!ps_compiler_load_file(compiler, source_file))
    {
        fprintf(stderr, "File %s not loaded!\n", source_file);
        fprintf(stderr, "Error %d %s\n", compiler->error, ps_error_get_message(compiler->error)); // NOSONAR
        return ok;
    }
    ps_buffer_dump(stderr, ps_parser_get_lexer(compiler->parser)->buffer, 0, PS_BUFFER_MAX_LINES);
    if (verbose)
        fprintf(stderr, "Loaded %s!\n", source_file);

    /* List program */
    if (dump_buffer)
    {
        const ps_lexer *lexer = ps_parser_get_lexer(compiler->parser);
        ps_buffer_dump(stderr, lexer->buffer, 0, PS_BUFFER_MAX_LINES);
        if (verbose)
            fprintf(stderr, "Listed!\n");
    }

    /* Compile program */
    if (compiler->debug >= PS_DEBUG_TRACE)
        printf("=============================== BEGIN COMPILATION ==============================\n");
    ok = ps_compiler_compile(compiler, &program);
    if (compiler->debug >= PS_DEBUG_TRACE)
        printf("================================ END COMPILATION ===============================\n");

    return ok;
}

bool execute(const ps_ast_block *program)
{
    assert(NULL != interpreter);
    assert(NULL != program);

    bool ok = false;

    interpreter->debug = PS_DEBUG_FATAL;
    if (trace)
        interpreter->debug = PS_DEBUG_VERBOSE;
    else if (debug)
        interpreter->debug = PS_DEBUG_TRACE;

    /* List symbols BEFORE */
    if (dump_symbols)
        ps_symbol_table_dump(NULL, "Initialization", program->symbols);

    /* Run program */
    if (verbose)
        printf("================================ BEGIN EXECUTION ===============================\n");
    ok = ps_interpreter_run(interpreter, program);
    if (verbose)
        printf("================================= END EXECUTION ================================\n");

    /* List symbols AFTER */
    if (dump_symbols)
        ps_symbol_table_dump(NULL, "End", program->symbols);

    return ok;
}

int main(int argc, char *argv[])
{
    bool ok = false;
    // Paths & file names
    char *current_path = NULL;
    char *program_file = NULL;
    char source_file[256] = {0};

    int arg = get_options(argc, argv);

    // Force when debugging as I didn't find how to pass command line options
    // trace = true;
    // debug = true;

    ast_test = true;
    if (ast_test && !ps_ast_test())
    {
        fprintf(stderr, "AST tests failed!\n");
        exit(EXIT_FAILURE);
    }
    fprintf(stderr, "AST tests passed!\n");
    exit(EXIT_SUCCESS);

    current_path = getcwd(NULL, 0);
    if (arg + 1 < argc)
    {
        program_file = argv[argc - 1];
        snprintf(source_file, sizeof(source_file) - 1, "%s/%s", current_path, program_file);
    }
    else
    {
        program_file = DEBUGGER_SOURCE;
        if (program_file != NULL)
            snprintf(source_file, sizeof(source_file) - 1, "%s/../%s", current_path, program_file);
        else
            source_file[0] = '\0';
    }
    if (strlen(source_file) == 0)
    {
        fprintf(stderr, "No file to run!\n");
        usage(argv[0]);
        return EXIT_FAILURE;
    }

    /* Display banner, intepreter runtime options, current path & source file, ...  */
    if (verbose)
    {
        banner(stdout);
        printf("Runtime options:\n");
        printf(" - boolean evaluation: $B%c (*FUTURE*)\n", bool_eval ? '+' : '-');
        printf(" - IO check          : $I%c (*FUTURE*)\n", io_check ? '+' : '-');
        printf(" - Range check       : $R%c\n", range_check ? '+' : '-');
        printf("Current working directory: %s\n", current_path);
        printf("Source file: %s\n", source_file);
    }
    free(current_path);
    current_path = NULL;

    // trace = false;
    // debug = false;
    // verbose = false;

    /* Initialize compiler */
    system_block = ps_system_alloc();
    ps_symbol_table_dump(stderr, "SYSTEM SYMBOLS", system_block->symbols);

    compiler = ps_compiler_alloc(system_block);
    if (compiler == NULL)
    {
        fprintf(stderr, "Could not initialize compiler!\n");
        return EXIT_FAILURE;
    }

    if (compile(source_file))
    {
        ok = true;
        if (verbose)
            fprintf(stderr, "Compiled %s!\n", source_file);
    }

    ps_symbol_table_dump(stderr, "SYMBOL TABLE FOR PROGRAM", program->symbols);
    printf("AST DUMP for %s:\n", source_file);
    ps_ast_debug_node(0, (ps_ast_node *)program);

    if (run)
    {
        /* Initialize interpreter */
        interpreter = ps_interpreter_alloc(compiler->system, compiler->string_heap, range_check, bool_eval, io_check);
        if (interpreter == NULL)
        {
            fprintf(stderr, "Could not initialize interpreter!\n");
            return EXIT_FAILURE;
        }
        /* Run program */
        execute(program);
        /* Terminate interpreter */
        interpreter = ps_interpreter_free(interpreter);
    }

    compiler = ps_compiler_free(compiler);
    system_block = (ps_ast_block *)ps_ast_free_block(system_block);

    if (memory)
        ps_memory_debug(stderr);

    fprintf(stderr, "%s\n", ok ? "SUCCESS!" : "FAILURE!");
    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

/* EOF */
