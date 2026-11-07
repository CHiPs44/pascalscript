/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_ast.h"
#include "ps_compiler.h"
#include "ps_error.h"
#include "ps_string_heap.h"
#include "ps_system.h"

static bool compile_source(ps_ast_block *system, ps_string_heap *string_heap, char *source, bool expected_success,
                           ps_error expected_error)
{
    ps_compiler *compiler = ps_compiler_alloc(system, string_heap);
    if (compiler == NULL)
        return false;

    ps_ast_block *program = NULL;
    bool compiled = ps_compiler_load_string(compiler, source, strlen(source)) &&
                    ps_compiler_compile(compiler, &program) && compiler->error == PS_ERROR_NONE;
    bool passed = compiled == expected_success && (expected_success || compiler->error == expected_error);
    if (!passed)
        fprintf(stderr, "Expected compile %s (error %d), got %s (error %d)\n", expected_success ? "success" : "failure",
                expected_error, compiled ? "success" : "failure", compiler->error);

    ps_ast_free_node((ps_ast_node *)program);
    ps_compiler_free(compiler);
    return passed;
}

int main(void)
{
    ps_ast_block *system = ps_system_alloc();
    ps_string_heap *string_heap = ps_string_heap_alloc(8, 4);
    if (system == NULL || string_heap == NULL)
    {
        ps_string_heap_free(string_heap);
        ps_system_free(system);
        return EXIT_FAILURE;
    }

    char mismatched_start[] = "Program P; Var B: Boolean; Begin For B := 1 To 3 Do ; End.";
    char mismatched_finish[] = "Program P; Var B: Boolean; Begin For B := True To 3 Do ; End.";
    char matching_integer[] = "Program P; Var I: Integer; Begin For I := 1 To 3 Do ; End.";
    char matching_subrange[] = "Program P; Type S = 1..3; Var I: S; Begin For I := 1 To 3 Do ; End.";
    char matching_unsigned_conversion[] = "Program P; Var U: Unsigned; Begin For U := 0 To 3 Do ; End.";

    bool passed = compile_source(system, string_heap, mismatched_start, false, PS_ERROR_TYPE_MISMATCH) &&
                  compile_source(system, string_heap, mismatched_finish, false, PS_ERROR_TYPE_MISMATCH) &&
                  compile_source(system, string_heap, matching_integer, true, PS_ERROR_NONE) &&
                  compile_source(system, string_heap, matching_subrange, true, PS_ERROR_NONE) &&
                  compile_source(system, string_heap, matching_unsigned_conversion, true, PS_ERROR_NONE);

    ps_string_heap_free(string_heap);
    ps_system_free(system);
    return passed ? EXIT_SUCCESS : EXIT_FAILURE;
}
