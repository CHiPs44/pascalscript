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
#include "ps_memory.h"
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
    // Create a PROGRAM node with name "MINIMAL" at line 1, column 1
    ps_ast_node *node = ps_ast_create_block(1, 1, PS_AST_PROGRAM, "MINIMAL");
    assert(node != NULL);

    // Check that the node has the expected group, kind, line, column and name
    assert(node->group == PS_AST_GROUP_STATEMENT);
    assert(node->kind == PS_AST_PROGRAM);
    assert(node->line == 1);
    assert(node->column == 1);
    assert(strcmp(node->block->name, "MINIMAL") == 0);

    // No signature, result type, variables, statements or executables
    node->block->signature = NULL;
    node->block->result_type = NULL;
    node->block->n_vars = 0;
    node->block->statement_list = ps_ast_create_statement_list(2, 1, 0);
    assert(node->block->statement_list != NULL);
    assert(node->block->statement_list->statement_list->count == 0);
    assert(node->block->statement_list->statement_list->statements == NULL);
    node->block->n_executables = 0;
    node->block->executables = NULL;

    // Create an empty symbol table and add a PROGRAM symbol to it
    node->block->symbols = ps_symbol_table_alloc(0, 0);
    assert(node->block->symbols != NULL);
    ps_symbol *symbol = ps_symbol_alloc(PS_SYMBOL_KIND_PROGRAM, "MINIMAL", NULL);
    ps_error error = ps_symbol_table_add(node->block->symbols, symbol);
    assert(error == PS_ERROR_NONE);

    // Run the program (which does nothing) and check that it returns true
    ps_interpreter *interpreter = ps_interpreter_alloc(true, false, false);
    assert(interpreter != NULL);
    bool result = ps_ast_run_program(interpreter, node);
    assert(result);

    // Free the node & interpreter and check that they are NULL
    node = ps_ast_free_node(node);
    assert(node == NULL);
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
    // Create a PROGRAM node with name "HELLO" at line 1, column 1
    ps_ast_node *node = ps_ast_create_block(1, 1, PS_AST_PROGRAM, "HELLO");
    assert(node != NULL);

    // Check that the node has the expected group, kind, line, column and name
    assert(node->group == PS_AST_GROUP_STATEMENT);
    assert(node->kind == PS_AST_PROGRAM);
    assert(node->line == 1);
    assert(node->column == 1);
    assert(strcmp(node->block->name, "HELLO") == 0);

    // No signature, result type, variables or executables
    node->block->signature = NULL;
    node->block->result_type = NULL;
    node->block->n_vars = 0;
    node->block->n_executables = 0;
    node->block->executables = NULL;

    // Create an empty symbol table and add a PROGRAM symbol to it
    node->block->symbols = ps_symbol_table_alloc(0, 0);
    assert(node->block->symbols != NULL);
    ps_symbol *symbol = ps_symbol_alloc(PS_SYMBOL_KIND_PROGRAM, "HELLO", NULL);
    ps_error error = ps_symbol_table_add(node->block->symbols, symbol);
    assert(error == PS_ERROR_NONE);

    // Create a statement list with one statement:
    //  a PROCEDURE CALL to Writeln with one argument: a string value "Hello, World!"
    node->block->statement_list = ps_ast_create_statement_list(3, 5, 1);
    assert(node->block->statement_list != NULL);
    ps_value argument_value = {
        .allocated = false, .type = &ps_system_string, .data.s = {.str = "Hello, World!", .len = 13, .max = 13}};
    ps_ast_node *argument = ps_ast_create_value(3, 13, &argument_value);
    assert(argument != NULL);
    ps_ast_node *args = ps_ast_create_argument_list(1);
    assert(args != NULL);
    args[0] = argument;
    ps_ast_node *statement = ps_ast_create_procedure_call(3, 5, "Writeln", 1, args);
    assert(statement != NULL);
    // Run the program (which does nothing) and check that it returns true
    ps_interpreter *interpreter = ps_interpreter_alloc(true, false, false);
    assert(interpreter != NULL);
    bool result = ps_ast_run_program(interpreter, node);
    assert(result);

    // Free the node & interpreter and check that they are NULL
    node = ps_ast_free_node(node);
    assert(node == NULL);
    interpreter = ps_interpreter_free(interpreter);
    assert(interpreter == NULL);
}
