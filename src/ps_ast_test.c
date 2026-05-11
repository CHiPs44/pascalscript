/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_ast_execute.h"
#include "ps_error.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_memory.h"
#include "ps_procedures.h"
#include "ps_string_heap.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"

#define ASSERT(expr)                                                                                                   \
    do                                                                                                                 \
    {                                                                                                                  \
        if (!(expr))                                                                                                   \
        {                                                                                                              \
            ps_ast_debug_line("Assertion failed: %s, function %s, file %s, line %d.", #expr, __func__, __FILE__,       \
                              __LINE__);                                                                               \
            exit(EXIT_FAILURE);                                                                                        \
        }                                                                                                              \
    } while (0)

ps_ast_block *ps_ast_test_create_program(const char *name)
{
    ps_ast_block *block = ps_ast_create_block(1, 1, PS_AST_PROGRAM, name);
    if (block == NULL)
        return NULL;
    block->symbols = ps_symbol_table_alloc(0, 0);
    if (block->symbols == NULL)
        goto cleanup;
    ps_symbol *symbol = ps_symbol_alloc(PS_SYMBOL_KIND_PROGRAM, name, NULL);
    if (symbol == NULL)
        goto cleanup;
    ps_symbol_table_error error = ps_symbol_table_add(block->symbols, symbol);
    if (error != PS_SYMBOL_TABLE_ERROR_NONE)
        goto cleanup;
    return block;
cleanup:
    ps_ast_free_block(block);
    return NULL;
}

/**
 * @brief Test minimal Pascal program
 *  L/C 12345678901234567890
 *  1   Program Minimal;
 *  2   Begin
 *  3   End.
 */
bool ps_ast_test_minimal()
{
    bool result;
    (void)result; // silence unused variable warning with asserts

    ps_ast_debug_line("Create an interpreter first as we may need it very soon");
    ps_interpreter *interpreter = ps_interpreter_alloc(true, false, false);
    ASSERT(interpreter != NULL);

    ps_ast_debug_line("Create a PROGRAM node with name 'MINIMAL' at line 1, column 1");
    ps_ast_debug_line("                      and symbol table with the program symbol itself");
    ps_ast_block *block = ps_ast_test_create_program("MINIMAL");
    ASSERT(block != NULL);

    ps_ast_debug_line("Check that the node has the expected values");
    ASSERT(block->group == PS_AST_BLOCK);
    ASSERT(block->kind == PS_AST_PROGRAM);
    ASSERT(block->line == 1);
    ASSERT(block->column == 1);
    ASSERT(strcmp(block->name, "MINIMAL") == 0);
    ASSERT(block->n_vars == 0);
    ASSERT(block->symbols != NULL);
    ASSERT(block->n_executables == 0);
    ASSERT(block->executables == NULL);
    ASSERT(block->statement_list == NULL);
    ASSERT(block->signature == NULL);
    ASSERT(block->result_type == NULL);

    ps_symbol *symbol = ps_symbol_table_get(block->symbols, "MINIMAL");
    ASSERT(symbol != NULL);
    ps_ast_debug_line("Found symbol: %p %s", (void *)symbol, symbol->name);
    ps_symbol_debug(stderr, "Found symbol: ", symbol);
    ASSERT(symbol->kind == PS_SYMBOL_KIND_PROGRAM);
    ASSERT(symbol->system == false);
    ASSERT(symbol->allocated == true);
    ASSERT(symbol->value == NULL);

    ps_ast_debug_line("Enter environment for the program block");
    result = ps_interpreter_enter_environment(interpreter, block->name, NULL, 0, NULL);
    ASSERT(result);

    ps_ast_debug_line("Add PROGRAM symbol to the current environment symbol table");
    result = ps_interpreter_add_symbol(interpreter, symbol);
    ASSERT(result);

    ps_ast_debug_line("Debug print the program");
    ps_ast_debug = true;
    ps_ast_debug_line("================================================================");
    ps_ast_debug_node((ps_ast_node *)block);
    ps_ast_debug_line("================================================================");

    ps_ast_debug_line("Run the program (which does nothing) and check that it returns true");
    result = ps_ast_run_program(interpreter, block);
    if (!result)
    {
        fprintf(stderr, "Error running the program: %s\n", interpreter->message);
    }

    ps_ast_debug_line("Exit environment for the program block");
    result = ps_interpreter_exit_environment(interpreter);
    ASSERT(result);

    ps_ast_debug_line("Free program block");
    block = (ps_ast_block *)ps_ast_free_block(block);
    ASSERT(block == NULL);

    ps_ast_debug_line("Free interpreter");
    interpreter = ps_interpreter_free(interpreter);
    ASSERT(interpreter == NULL);

    return true;
}

/**
 * @brief Test Assignment Pascal program
 * L/C 123456789012345678901234567890123456789012345678901234567890
 * 1   Program Assignment;
 * 2   Var I: Integer;
 * 3   Begin
 * 4       I := 42 * 2;
 * 5   End.
 */
bool ps_ast_test_assignment()
{
    bool result;
    (void)result; // silence unused variable warning with asserts

    ps_ast_debug_line("Create an interpreter first as we may need it very soon");
    ps_interpreter *interpreter = ps_interpreter_alloc(true, false, false);
    ASSERT(interpreter != NULL);

    ps_ast_debug_line("Create a PROGRAM node with name 'ASSIGNMENT' at line 1, column 1");
    ps_ast_debug_line("                      and symbol table with the program symbol itself");
    ps_ast_block *block = ps_ast_test_create_program("ASSIGNMENT");
    ASSERT(block != NULL);

    ps_symbol *symbol = ps_symbol_table_get(block->symbols, "ASSIGNMENT");
    ASSERT(symbol != NULL);

    ps_ast_debug_line("Enter environment for the program block");
    result = ps_interpreter_enter_environment(interpreter, block->name, NULL, 0, NULL);
    ASSERT(result);

    ps_ast_debug_line("Add PROGRAM symbol to the current environment symbol table");
    result = ps_interpreter_add_symbol(interpreter, symbol);
    ASSERT(result);

    ps_ast_debug_line("Create a variable symbol I of type Integer and add it to the symbol tables");
    ps_value i_value = {.allocated = false, .type = &ps_system_integer, .data.i = 0};
    const ps_symbol *i_symbol = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, "I", &i_value);
    result = ps_interpreter_add_symbol(interpreter, (ps_symbol *)i_symbol);
    ASSERT(result);
    ps_symbol_table_error error = ps_symbol_table_add(block->symbols, (ps_symbol *)i_symbol);
    ASSERT(error == PS_SYMBOL_TABLE_ERROR_NONE);
    block->n_vars = 1;

    ps_ast_debug_line("Create a statement list with one statement");
    //  a PROCEDURE CALL to Writeln with one argument: a string value "Hello, World!"
    block->statement_list = ps_ast_create_statement_list(3, 5, 1);
    ASSERT(block->statement_list != NULL);

    ps_ast_debug_line("Create the assignment statement I := 42 * 2;");
    ps_ast_variable_simple *variable_simple =
        ps_ast_create_variable_simple(3, 5, PS_AST_LVALUE_SIMPLE, (ps_symbol *)i_symbol);
    ASSERT(variable_simple != NULL);
    ps_value value_i_42 = {.allocated = false, .type = &ps_system_unsigned, .data.u = 42};
    ps_ast_value *rvalue_i_42 = ps_ast_create_rvalue_const(3, 10, value_i_42);
    ASSERT(rvalue_i_42 != NULL);
    ps_value value_i_2 = {.allocated = false, .type = &ps_system_unsigned, .data.u = 2};
    ps_ast_value *rvalue_i_2 = ps_ast_create_rvalue_const(3, 15, value_i_2);
    ASSERT(rvalue_i_2 != NULL);
    ps_ast_binary_operation *mul_operation =
        ps_ast_create_binary_operation(3, 13, PS_OP_MUL, (ps_ast_node *)rvalue_i_42, (ps_ast_node *)rvalue_i_2);
    ASSERT(mul_operation != NULL);
    ps_ast_assignment *assignment =
        ps_ast_create_assignment(3, 5, (ps_ast_node *)variable_simple, (ps_ast_node *)mul_operation);
    ASSERT(assignment != NULL);
    block->statement_list->statements[0] = (ps_ast_node *)assignment;

    ps_ast_debug_line("Debug print the program");
    ps_ast_debug = true;
    ps_ast_debug_line("================================================================");
    ps_ast_debug_node((ps_ast_node *)block);
    ps_ast_debug_line("================================================================");

    ps_ast_debug_line("Run the program and check that it returns true");
    result = ps_ast_run_program(interpreter, block);
    ps_ast_debug_line("Interpreter error:   %s", ps_error_get_message(interpreter->error));
    ps_ast_debug_line("Interpreter message: %s", interpreter->message);
    ASSERT(result);

    ps_ast_debug_line("Check that variable I has the expected value 84");
    ASSERT(i_symbol->value != NULL);
    ASSERT(i_symbol->value->type == &ps_system_integer);
    ps_ast_debug_line("Variable I value: %d", i_symbol->value->data.i);
    ASSERT(i_symbol->value->data.i == 84);

    ps_ast_debug_line("Free symbol table for the program block");
    ps_memory_free(PS_MEMORY_AST, block->symbols);
    block->symbols = NULL;

    ps_ast_debug_line("Free program block");
    block = (ps_ast_block *)ps_ast_free_block(block);
    ASSERT(block == NULL);

    ps_ast_debug_line("Free interpreter");
    interpreter = ps_interpreter_free(interpreter);
    ASSERT(interpreter == NULL);

    return true;
}

/**
 * @brief Test Hello Pascal program
 *  L/C 123456789012345678901234567890
 *  1   Program Hello;
 *  2   Begin
 *  3       Writeln('Hello, World!');
 *  4   End.
 */
bool ps_ast_test_hello()
{
    bool result;
    (void)result; // silence unused variable warning with asserts

    ps_ast_debug_line("Create an interpreter first as we may need it very soon");
    ps_interpreter *interpreter = ps_interpreter_alloc(true, false, false);
    ASSERT(interpreter != NULL);

    ps_ast_debug_line("Create a PROGRAM node with name 'HELLO' at line 1, column 1");
    ps_ast_block *block = ps_ast_test_create_program("HELLO");
    ASSERT(block != NULL);

    ps_ast_debug_line("Enter environment for the program block");
    result = ps_interpreter_enter_environment(interpreter, block->name, NULL, 0, NULL);
    ASSERT(result);

    ps_ast_debug_line(
        "Create a PROGRAM symbol and add it to the current environment symbol table and the block symbol table");
    ps_symbol *symbol = ps_symbol_alloc(PS_SYMBOL_KIND_PROGRAM, "HELLO", NULL);
    result = ps_interpreter_add_symbol(interpreter, symbol);
    ASSERT(result);
    ps_symbol_table_error error = ps_symbol_table_add(block->symbols, symbol);
    (void)error; // silence unused variable warning
    ASSERT(error == PS_SYMBOL_TABLE_ERROR_NONE);

    ps_ast_debug_line("Create a statement list with one statement");
    //  a PROCEDURE CALL to Writeln with one argument: a string value "Hello, World!"
    block->statement_list = ps_ast_create_statement_list(3, 5, 1);
    ASSERT(block->statement_list != NULL);

    ps_ast_debug_line("Create the by value argument");
    ps_string *hello = ps_string_heap_create(interpreter->string_heap, "Hello, World!");
    ASSERT(hello != NULL);
    ps_value argument_value = {.allocated = false, .type = &ps_system_string, .data.s = hello};
    ps_ast_value *argument_value_node = ps_ast_create_rvalue_const(3, 13, argument_value);
    ASSERT(argument_value_node != NULL);
    ps_ast_argument *argument = ps_ast_create_argument(3, 13, PS_AST_ARG_EXPR, (ps_ast_node *)argument_value_node);
    ASSERT(argument != NULL);

    ps_ast_debug_line("Create the argument list for the procedure call");
    ps_ast_argument **args = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_argument *));
    ASSERT(args != NULL);
    args[0] = argument;

    ps_ast_debug_line("Create the PROCEDURE CALL statement");
    ps_ast_call *statement = ps_ast_create_call(3, 5, PS_AST_PROCEDURE_CALL, &ps_system_procedure_writeln, 1, args);
    ASSERT(statement != NULL);

    ps_ast_debug_line("Add the statement to the statement list");
    block->statement_list->statements[0] = (ps_ast_node *)statement;

    ps_ast_debug_line("Debug print the program");
    ps_ast_debug = true;
    ps_ast_debug_line("================================================================");
    ps_ast_debug_node((ps_ast_node *)block);
    ps_ast_debug_line("================================================================");

    ps_ast_debug_line("Run the program and check that it returns true");
    result = ps_ast_run_program(interpreter, block);
    if (!result)
    {
        fprintf(stderr, "Error running the program\n");
    }

    ps_ast_debug_line("Free program block");
    block = (ps_ast_block *)ps_ast_free_block(block);
    ASSERT(block == NULL);

    ps_ast_debug_line("Free interpreter");
    interpreter = ps_interpreter_free(interpreter);
    ASSERT(interpreter == NULL);

    return true;
}
