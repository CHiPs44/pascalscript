/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_memory.h"
#include "ps_procedures.h"
#include "ps_string_heap.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"

/**
 * @brief Test minimal Pascal program
 *  L/C 12345678901234567890
 *  1   Program Minimal;
 *  2   Begin
 *  3   End.
 */
void ps_ast_test_minimal()
{
    bool result;

    // Create an interpreter first as we may need it very soon
    ps_interpreter *interpreter = ps_interpreter_alloc(true, false, false);
    assert(interpreter != NULL);

    // Create a PROGRAM node with name "MINIMAL" at line 1, column 1
    ps_ast_block *block = ps_ast_create_block(1, 1, PS_AST_PROGRAM, "MINIMAL");
    assert(block != NULL);

    // Check that the node has the expected group, kind, line, column and name
    assert(block->group == PS_AST_STATEMENT);
    assert(block->kind == PS_AST_PROGRAM);
    assert(block->line == 1);
    assert(block->column == 1);
    assert(strcmp(block->name, "MINIMAL") == 0);

    // Create an default empty symbol table for the program block
    block->symbols = ps_symbol_table_alloc(0, 0);
    assert(block->symbols != NULL);

    // Enter environment for the program block
    result = ps_interpreter_enter_environment(interpreter, block->name, NULL, 0, NULL);
    assert(result);

    // Create a PROGRAM symbol and add it to the current environment symbol table and the block symbol table
    ps_symbol *symbol = ps_symbol_alloc(PS_SYMBOL_KIND_PROGRAM, "MINIMAL", NULL);
    result = ps_interpreter_add_symbol(interpreter, symbol);
    assert(result);
    ps_symbol_table_error error = ps_symbol_table_add(block->symbols, symbol);
    assert(error == PS_SYMBOL_TABLE_ERROR_NONE);

    // Run the program (which does nothing) and check that it returns true
    result = ps_ast_run_program(interpreter, block);
    assert(result);

    // Exit environment for the program block
    result = ps_interpreter_exit_environment(interpreter);
    assert(result);

    // Free program block
    block = ps_ast_free_node(block);
    assert(block == NULL);

    // Free interpreter
    interpreter = ps_interpreter_free(interpreter);
    assert(interpreter == NULL);
}

/**
 * @brief Test Hello Pascal program
 *      123456789012345678901234567890
 *  1   Program Hello;
 *  2   Begin
 *  3       Writeln('Hello, World!');
 *  4   End.
 */
void ps_ast_test_hello()
{
    ps_interpreter *interpreter = ps_interpreter_alloc(true, false, false);
    assert(interpreter != NULL);

    // Create a PROGRAM node with name "HELLO" at line 1, column 1
    ps_ast_block *block = ps_ast_create_block(1, 1, PS_AST_PROGRAM, "HELLO");
    assert(block != NULL);
    assert(strcmp(block->name, "HELLO") == 0);

    // Create an default empty symbol table for the program block
    block->symbols = ps_symbol_table_alloc(0, 0);
    assert(block->symbols != NULL);

    // Enter environment for the program block
    bool result = ps_interpreter_enter_environment(interpreter, block->name, NULL, 0, NULL);
    assert(result);

    // Create a PROGRAM symbol and add it to the current environment symbol table and the block symbol table
    ps_symbol *symbol = ps_symbol_alloc(PS_SYMBOL_KIND_PROGRAM, "HELLO", NULL);
    result = ps_interpreter_add_symbol(interpreter, symbol);
    assert(result);
    ps_symbol_table_error error = ps_symbol_table_add(block->symbols, symbol);
    assert(error == PS_SYMBOL_TABLE_ERROR_NONE);

    // Create a statement list with one statement:
    //  a PROCEDURE CALL to Writeln with one argument: a string value "Hello, World!"
    block->statement_list = ps_ast_create_statement_list(3, 5, 1);
    assert(block->statement_list != NULL);

    // Create the by value argument
    ps_string *hello = ps_string_heap_create(interpreter->string_heap, "Hello, World!");
    assert(hello != NULL);
    ps_value argument_value = {.allocated = false, .type = &ps_system_string, .data.s = hello};
    ps_ast_value *argument_value_node = ps_ast_create_rvalue_const(3, 13, argument_value);
    ps_ast_node *argument = ps_ast_create_argument(3, 13, PS_AST_ARG_EXPR, argument_value_node);
    assert(argument != NULL);

    // Create the argument list for the procedure call
    ps_ast_argument *args[] = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_argument));
    assert(args != NULL);
    args[0]->arg = argument;

    // Create the PROCEDURE CALL statement
    ps_ast_call *statement =
        ps_ast_create_call(3, 5, PS_AST_PROCEDURE_CALL, ps_system_procedure_writeln.value->data.x, 1, args);
    assert(statement != NULL);

    // Add the statement to the statement list
    block->statement_list->statements[0] = statement;

    // Run the program and check that it returns true
    ps_interpreter *interpreter = ps_interpreter_alloc(true, false, false);
    assert(interpreter != NULL);
    bool result = ps_ast_run_program(interpreter, block);
    assert(result);

    // Free program & interpreter
    block = ps_ast_free_node(block);
    assert(block == NULL);
    interpreter = ps_interpreter_free(interpreter);
    assert(interpreter == NULL);
}
