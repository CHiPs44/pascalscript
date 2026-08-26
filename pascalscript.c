/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>

#include <assert.h>
#include <getopt.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_ast_test.h"
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

// #define DEBUG_SOURCE "examples/000-minimal.pas"
// #define DEBUG_SOURCE "examples/001-hello.pas"
// #define DEBUG_SOURCE "examples/002-test-expr1.pas"
// #define DEBUG_SOURCE "examples/005-first.pas"
// #define DEBUG_SOURCE "examples/008-strings2.pas"
// #define DEBUG_SOURCE "examples/010-operators.pas"
// #define DEBUG_SOURCE "examples/011-subrange1.pas"
// #define DEBUG_SOURCE "examples/012-typedef0.pas"
// #define DEBUG_SOURCE "examples/013-typedef1.pas"
// #define DEBUG_SOURCE "examples/021-repeat-until.pas"
// #define DEBUG_SOURCE "examples/024-for-do.pas"
// #define DEBUG_SOURCE "examples/030-array0.pas"
#define DEBUG_SOURCE "examples/034-array4.pas"
// #define DEBUG_SOURCE "examples/070-random.pas"
// #define DEBUG_SOURCE "examples/080-math.pas"
// #define DEBUG_SOURCE "examples/090-boolean.pas"
// #define DEBUG_SOURCE "examples/120-toayue-powersoftwo.pas"
// #define DEBUG_SOURCE "examples/130-big-loops.pas"
// #define DEBUG_SOURCE "examples/030-array0.pas"

// Runtime options
bool bool_eval = false;
bool io_check = true;
bool range_check = true;

// Others options
bool ast_test = false;
bool debug = false;
bool dump_buffer = false;
bool dump_symbols = false;
bool exec = true;
bool memory = false;
bool trace = false;
bool verbose = false;

// clang-format off
ps_ast_block   *system_block = NULL;
ps_string_heap *string_heap  = NULL;
ps_compiler    *compiler     = NULL;
ps_ast_block   *program      = NULL;
ps_interpreter *interpreter  = NULL;
// clang-format on

void banner(FILE *output)
{
    fprintf(output, "PascalScript v%s (%d bits) - License: LGPL 3.0 or later, see LICENSE\n", PS_VERSION, PS_BITNESS);
}

void usage(FILE *output, char *program_name)
{
    banner(output);
    fprintf(output, "Usage: %s [-t] [-d] [-s] [-b] [-v] [program_file]\n", program_name);
    fprintf(output, "Runtime options:\n");
    fprintf(output, "  -b : flips short circuit boolean evaluation (default: false, {$B})\n");
    fprintf(output, "  -i : flips I/O error checking (default: true, ${I})\n");
    fprintf(output, "  -r : flips range checking (default: true, {$R})\n");
    fprintf(output, "Other options:\n");
    fprintf(output, "  -a : launch AST tests\n");
    fprintf(output, "  -c : display configuration and exit\n");
    fprintf(output, "  -d : debug (more verbose trace)\n");
    fprintf(output, "  -h : display this help message and exit\n");
    fprintf(output, "  -m : display memory usage at end\n");
    fprintf(output, "  -n : do not execute program, just parse source code\n");
    fprintf(output, "  -s : dump symbols at initialization and termination\n");
    fprintf(output, "  -t : trace execution\n");
    fprintf(output, "  -u : dump source buffer after loading\n");
    fprintf(output, "  -v : verbose (display banner and other infos)\n");
    fprintf(output, "  program_file : path to the Pascal source file to run (default: %s)\n", DEBUG_SOURCE);
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
            usage(stdout, argv[0]);
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
            usage(stderr, argv[0]);
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
    if (verbose)
        fprintf(stderr, "=============================== BEGIN COMPILATION ==============================\n");
    ok = ps_compiler_compile(compiler, &program);
    if (ok)
        fprintf(stderr, "Compilation OK\n");
    if (compiler->error != PS_ERROR_NONE)
        fprintf(stderr, "Compiler error:   %d %s\n", compiler->error, ps_error_get_message(compiler->error));
    if (strlen(compiler->message) > 0)
        fprintf(stderr, "         message: %s\n", compiler->message);
    if (verbose)
        fprintf(stderr, "================================ END COMPILATION ===============================\n");

    return ok;
}

bool execute()
{
    assert(NULL != interpreter);
    assert(NULL != program);

    bool ok = false;

    interpreter->logger->debug_level = PS_DEBUG_FATAL;
    if (verbose)
        interpreter->logger->debug_level = PS_DEBUG_VERBOSE;
    else if (trace)
        interpreter->logger->debug_level = PS_DEBUG_TRACE;

    /* List symbols BEFORE execution */
    if (dump_symbols)
        ps_symbol_table_dump(NULL, "Initialization", program->symbols);

    /* Run program */
    if (verbose)
        fprintf(stderr, "================================ BEGIN EXECUTION ===============================\n");
    ok = ps_interpreter_run(interpreter, program);
    if (verbose)
        fprintf(stderr, "================================= END EXECUTION ================================\n");

    /* List symbols AFTER execution */
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

    // ast_test = true;
    if (ast_test)
    {
        if (!ps_ast_test())
        {
            fprintf(stderr, "AST tests failed!\n");
            exit(EXIT_FAILURE);
        }
        fprintf(stderr, "AST tests passed!\n");
        exit(EXIT_SUCCESS);
    }

    current_path = getcwd(NULL, 0);
    if (arg + 1 < argc)
    {
        program_file = argv[argc - 1];
        snprintf(source_file, sizeof(source_file) - 1, "%s/%s", current_path, program_file);
    }
    else
    {
        program_file = DEBUG_SOURCE;
        if (program_file != NULL)
        {
            // char *executable_path = realpath(argv[0], NULL);
            char self[128] = {0};
            int nchar = readlink("/proc/self/exe", self, sizeof(self));
            if (nchar < 0 || nchar >= (int)sizeof(self))
            {
                fprintf(stderr, "/proc/self/exe readlink : %s\n", self);
                exit(EXIT_FAILURE);
            }
            char *separator = strrchr(self, '/');
            if (separator != NULL)
                *separator = '\0';
            snprintf(source_file, sizeof(source_file), "%s/../%s", /*executable_path*/ self, program_file);
            // free(executable_path);
        }
        else
            source_file[0] = '\0';
    }
    if (strlen(source_file) == 0)
    {
        fprintf(stderr, "No file to run!\n");
        usage(stderr, argv[0]);
        return EXIT_FAILURE;
    }

    /* Display banner, intepreter runtime options, current path & source file, ...  */
    if (verbose)
    {
        banner(stdout);
        fprintf(stderr, "Runtime options:\n");
        fprintf(stderr, " - boolean evaluation: $B%c (*FUTURE*)\n", bool_eval ? '+' : '-');
        fprintf(stderr, " - IO check          : $I%c (*FUTURE*)\n", io_check ? '+' : '-');
        fprintf(stderr, " - Range check       : $R%c\n", range_check ? '+' : '-');
        fprintf(stderr, "Current working directory: %s\n", current_path);
        fprintf(stderr, "Source file: %s\n", source_file);
    }
    free(current_path);
    current_path = NULL;

    // trace = false;
    // debug = false;
    // verbose = true;

    /* Initialize compiler */
    system_block = ps_system_alloc();
    if (verbose)
        ps_symbol_table_dump(stderr, "SYSTEM SYMBOLS", system_block->symbols);
    string_heap = ps_string_heap_alloc(PS_STRING_HEAP_SIZE, PS_STRING_HEAP_MORE);

    compiler = ps_compiler_alloc(system_block, string_heap);
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

    if (verbose && program != NULL && program->symbols != NULL)
        ps_symbol_table_dump(stderr, "SYMBOL TABLE FOR PROGRAM", program->symbols);

    if (verbose)
    {
        fprintf(stderr, "AST DUMP for %s:\n", source_file);
        bool save = ps_ast_debug;
        ps_ast_debug = true;
        ps_ast_debug_node(0, (ps_ast_node *)program);
        ps_ast_debug = save;
    }

    if (exec)
    {
        if (verbose)
            fprintf(stderr, "Executing %s...\n", source_file);
        /* Initialize interpreter */
        interpreter = ps_interpreter_alloc(compiler->system, compiler->string_heap, range_check, bool_eval, io_check);
        if (interpreter == NULL)
        {
            fprintf(stderr, "Could not initialize interpreter!\n");
            return EXIT_FAILURE;
        }
        /* Run program */
        ok = execute();
        /* Terminate interpreter */
        interpreter = ps_interpreter_free(interpreter);
    }

    /* Terminate compiler & system */
    compiler = ps_compiler_free(compiler);
    system_block = (ps_ast_block *)ps_ast_free_block(system_block);
    string_heap = ps_string_heap_free(string_heap);

    if (memory)
        ps_memory_debug(stderr);

    fprintf(stderr, "%s\n", ok ? "SUCCESS!" : "FAILURE!");
    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

/* EOF */
