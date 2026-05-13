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

#define ASSERT_RETURN_FALSE(expr)                                                                                      \
    do                                                                                                                 \
    {                                                                                                                  \
        if (!(expr))                                                                                                   \
        {                                                                                                              \
            ps_ast_debug_line("Assertion failed: %s, function %s, file %s, line %d.", #expr, __func__, __FILE__,       \
                              __LINE__);                                                                               \
            return false;                                                                                              \
        }                                                                                                              \
    } while (0)

#define ASSERT_GOTO_CLEANUP(expr)                                                                                      \
    do                                                                                                                 \
    {                                                                                                                  \
        if (!(expr))                                                                                                   \
        {                                                                                                              \
            ps_ast_debug_line("Assertion failed: %s, function %s, file %s, line %d.", #expr, __func__, __FILE__,       \
                              __LINE__);                                                                               \
            goto cleanup;                                                                                              \
        }                                                                                                              \
    } while (0)

ps_ast_block *ps_ast_test_create_block_program(const char *name)
{
    ps_ast_debug_line("Create a PROGRAM node with name '%s' at line 1, column 1", name);
    ps_ast_block *block_program = ps_ast_create_block(1, 1, PS_AST_PROGRAM, name);
    if (block_program == NULL)
        return NULL;

    ps_ast_debug_line("Create symbol table");
    block_program->symbols = ps_symbol_table_alloc(0, 0);
    ASSERT_GOTO_CLEANUP(block_program->symbols != NULL);

    ps_symbol *symbol_program = ps_symbol_alloc(PS_SYMBOL_KIND_PROGRAM, name, NULL);
    ASSERT_GOTO_CLEANUP(symbol_program != NULL);

    ps_symbol_table_error error = ps_symbol_table_add(block_program->symbols, symbol_program);
    ASSERT_GOTO_CLEANUP(error == PS_SYMBOL_TABLE_ERROR_NONE);

    ps_ast_debug_line("Check that the PROGRAM node has the expected values");
    ASSERT_GOTO_CLEANUP(block_program->group == PS_AST_BLOCK);
    ASSERT_GOTO_CLEANUP(block_program->kind == PS_AST_PROGRAM);
    ASSERT_GOTO_CLEANUP(block_program->line == 1);
    ASSERT_GOTO_CLEANUP(block_program->column == 1);
    ASSERT_GOTO_CLEANUP(strcmp(block_program->name, name) == 0);
    ASSERT_GOTO_CLEANUP(block_program->n_vars == 0);
    ASSERT_GOTO_CLEANUP(block_program->symbols != NULL);
    ASSERT_GOTO_CLEANUP(block_program->n_executables == 0);
    ASSERT_GOTO_CLEANUP(block_program->executables == NULL);
    ASSERT_GOTO_CLEANUP(block_program->statement_list == NULL);
    ASSERT_GOTO_CLEANUP(block_program->signature == NULL);
    ASSERT_GOTO_CLEANUP(block_program->result_type == NULL);

    return block_program;
cleanup:
    ps_ast_free_block(block_program);
    return NULL;
}

bool ps_ast_test_delete_block_program(ps_ast_block *block_program)
{
    ps_ast_debug_line("Free the PROGRAM block node");
    block_program = (ps_ast_block *)ps_ast_free_block(block_program);
    ASSERT_RETURN_FALSE(block_program == NULL);
    return true;
}

ps_interpreter *ps_ast_test_create_interpreter(ps_ast_block *block_program)
{
    ps_ast_debug_line("Create an interpreter");
    ps_interpreter *interpreter = ps_interpreter_alloc(true, false, false);
    ASSERT_GOTO_CLEANUP(interpreter != NULL);

    ps_ast_debug_line("Enter environment for the program %s", block_program->name);
    ASSERT_GOTO_CLEANUP(ps_interpreter_enter_environment(interpreter, block_program->name, NULL, 0, NULL));

    ps_ast_debug_line("Add PROGRAM symbol to the current environment symbol table");
    ps_symbol *symbol_program = ps_symbol_table_get(block_program->symbols, block_program->name);
    ASSERT_GOTO_CLEANUP(symbol_program != NULL);
    ASSERT_GOTO_CLEANUP(symbol_program->kind == PS_SYMBOL_KIND_PROGRAM);
    ASSERT_GOTO_CLEANUP(symbol_program->system == false);
    ASSERT_GOTO_CLEANUP(symbol_program->allocated == true);
    ASSERT_GOTO_CLEANUP(symbol_program->value == NULL);
    ASSERT_GOTO_CLEANUP(ps_interpreter_add_symbol(interpreter, symbol_program));

    return interpreter;
cleanup:
    return NULL;
}

bool ps_ast_test_delete_interpreter(ps_interpreter *interpreter, ps_ast_block *block_program)
{
    ps_ast_debug_line("Exit environment for the program %s", block_program->name);
    ASSERT_RETURN_FALSE(ps_interpreter_exit_environment(interpreter));
    ps_ast_debug_line("Free interpreter");
    interpreter = ps_interpreter_free(interpreter);
    ASSERT_RETURN_FALSE(interpreter == NULL);
    return true;
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

    ps_ast_block *block_program = ps_ast_test_create_block_program("MINIMAL");
    ASSERT_RETURN_FALSE(block_program != NULL);

    ps_interpreter *interpreter = ps_ast_test_create_interpreter(block_program);
    ASSERT_RETURN_FALSE(interpreter != NULL);

    ps_ast_debug_line("Debug print the program");
    ps_ast_debug = true;
    ps_ast_debug_line("================================================================");
    ps_ast_debug_node((ps_ast_node *)block_program);
    ps_ast_debug_line("================================================================");

    ps_ast_debug_line("Run the program and check that it returns true");
    result = ps_ast_run_program(interpreter, block_program);
    if (!result)
    {
        fprintf(stderr, "Error running the program: %s\n", interpreter->message);
    }

    ps_ast_test_delete_interpreter(interpreter, block_program);

    ps_ast_test_delete_block_program(block_program);

    return true;
}

/**
 * @brief Test Assignment Pascal program
 * L/C 123456789012345678901234567890123456789012345678901234567890
 * 1   Program Assignment;
 * 2   Var I, J: Integer;
 * 3   Begin
 * 4       I := 21 * 2;
 * 4       J := I;
 * 5   End.
 */
bool ps_ast_test_assignment()
{
    bool result;

    ps_ast_block *block_program = ps_ast_test_create_block_program("ASSIGNMENT");
    ASSERT_RETURN_FALSE(block_program != NULL);

    ps_interpreter *interpreter = ps_ast_test_create_interpreter(block_program);
    ASSERT_RETURN_FALSE(interpreter != NULL);

    ps_ast_debug_line("Create variable symbols I ² J of type Integer and add them to the symbol tables");
    ps_value value_i = {.allocated = false, .type = &ps_system_integer, .data.i = 0};
    ps_symbol *symbol_i = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, "I", &value_i);
    result = ps_interpreter_add_symbol(interpreter, symbol_i);
    ASSERT_RETURN_FALSE(result);
    ps_value value_j = {.allocated = false, .type = &ps_system_integer, .data.i = 0};
    ps_symbol *symbol_j = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, "J", &value_j);
    result = ps_interpreter_add_symbol(interpreter, symbol_j);
    ASSERT_RETURN_FALSE(result);
    ps_symbol_table_error error = ps_symbol_table_add(block_program->symbols, symbol_i);
    ASSERT_RETURN_FALSE(error == PS_SYMBOL_TABLE_ERROR_NONE);
    error = ps_symbol_table_add(block_program->symbols, symbol_j);
    ASSERT_RETURN_FALSE(error == PS_SYMBOL_TABLE_ERROR_NONE);
    block_program->n_vars = 2;

    ps_ast_debug_line("Create a statement list with 2 statements");
    block_program->statement_list = ps_ast_create_statement_list(3, 5, 2);
    ASSERT_RETURN_FALSE(block_program->statement_list != NULL);

    ps_ast_debug_line("Create the assignment statement I := 21 * 2;");
    ps_ast_variable_simple *variable_i = ps_ast_create_variable_simple(3, 5, PS_AST_LVALUE_SIMPLE, symbol_i);
    ASSERT_RETURN_FALSE(variable_i != NULL);
    ps_value value_u_21 = {.allocated = false, .type = &ps_system_unsigned, .data.u = 21};
    ps_ast_value *rvalue_u_21 = ps_ast_create_rvalue_const(3, 10, value_u_21);
    ASSERT_RETURN_FALSE(rvalue_u_21 != NULL);
    ps_value value_u_2 = {.allocated = false, .type = &ps_system_unsigned, .data.u = 2};
    ps_ast_value *rvalue_u_2 = ps_ast_create_rvalue_const(3, 15, value_u_2);
    ASSERT_RETURN_FALSE(rvalue_u_2 != NULL);
    ps_ast_binary_operation *mul_operation =
        ps_ast_create_binary_operation(3, 13, PS_OP_MUL, (ps_ast_node *)rvalue_u_21, (ps_ast_node *)rvalue_u_2);
    ASSERT_RETURN_FALSE(mul_operation != NULL);
    ps_ast_assignment *assignment_i =
        ps_ast_create_assignment(3, 5, (ps_ast_node *)variable_i, (ps_ast_node *)mul_operation);
    ASSERT_RETURN_FALSE(assignment_i != NULL);
    block_program->statement_list->statements[0] = (ps_ast_node *)assignment_i;

    ps_ast_debug_line("Create the assignment statement J := I;");
    ps_ast_variable_simple *variable_j = ps_ast_create_variable_simple(4, 5, PS_AST_LVALUE_SIMPLE, symbol_j);
    ASSERT_RETURN_FALSE(variable_j != NULL);
    ps_ast_variable_simple *rvalue_i = ps_ast_create_variable_simple(4, 10, PS_AST_RVALUE_SIMPLE, symbol_i);
    ps_ast_assignment *assignment_j =
        ps_ast_create_assignment(4, 5, (ps_ast_node *)variable_j, (ps_ast_node *)rvalue_i);
    ASSERT_RETURN_FALSE(assignment_i != NULL);
    block_program->statement_list->statements[1] = (ps_ast_node *)assignment_j;

    ps_ast_debug_line("Debug print the program");
    ps_ast_debug = true;
    ps_ast_debug_line("================================================================");
    ps_ast_debug_node((ps_ast_node *)block_program);
    ps_ast_debug_line("================================================================");

    ps_ast_debug_line("Run the program and check that it returns true");
    result = ps_ast_run_program(interpreter, block_program);
    ps_ast_debug_line("Interpreter error:   %s", ps_error_get_message(interpreter->error));
    ps_ast_debug_line("Interpreter message: %s", interpreter->message);
    ASSERT_RETURN_FALSE(result);

    ps_ast_debug_line("Check that variable I has the expected value 42");
    ASSERT_RETURN_FALSE(symbol_i->value != NULL);
    ASSERT_RETURN_FALSE(symbol_i->value->type == &ps_system_integer);
    ps_ast_debug_line("Variable I value: %d", symbol_i->value->data.i);
    ASSERT_RETURN_FALSE(symbol_i->value->data.i == 42);

    ps_ast_debug_line("Check that variable J has the expected value 42");
    ASSERT_RETURN_FALSE(symbol_j->value != NULL);
    ASSERT_RETURN_FALSE(symbol_j->value->type == &ps_system_integer);
    ps_ast_debug_line("Variable J value: %d", symbol_i->value->data.i);
    ASSERT_RETURN_FALSE(symbol_j->value->data.i == 42);

    ps_symbol_table_dump(stderr, NULL, block_program->symbols);

    ps_ast_debug_line("Free symbol table for the program %s", block_program->name);
    ps_memory_free(PS_MEMORY_AST, block_program->symbols);
    block_program->symbols = NULL;

    ps_ast_debug_line("Free program %s", block_program->name);
    block_program = (ps_ast_block *)ps_ast_free_block(block_program);
    ASSERT_RETURN_FALSE(block_program == NULL);

    ps_ast_debug_line("Free interpreter");
    interpreter = ps_interpreter_free(interpreter);
    ASSERT_RETURN_FALSE(interpreter == NULL);

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

    ps_ast_block *block_program = ps_ast_test_create_block_program("HELLO");
    ASSERT_RETURN_FALSE(block_program != NULL);

    ps_interpreter *interpreter = ps_ast_test_create_interpreter(block_program);
    ASSERT_RETURN_FALSE(interpreter != NULL);

    ps_ast_debug_line("Create a statement list with one statement");
    block_program->statement_list = ps_ast_create_statement_list(3, 5, 1);
    ASSERT_RETURN_FALSE(block_program->statement_list != NULL);

    ps_ast_debug_line("Create the by value argument");
    ps_string *hello = ps_string_heap_create(interpreter->string_heap, "Hello, World!");
    ASSERT_RETURN_FALSE(hello != NULL);
    ps_value argument_value = {.allocated = false, .type = &ps_system_string, .data.s = hello};
    ps_ast_value *argument_value_node = ps_ast_create_rvalue_const(3, 13, argument_value);
    ASSERT_RETURN_FALSE(argument_value_node != NULL);
    ps_ast_argument *argument = ps_ast_create_argument(3, 13, PS_AST_ARG_EXPR, (ps_ast_node *)argument_value_node);
    ASSERT_RETURN_FALSE(argument != NULL);

    ps_ast_debug_line("Create the argument list for the procedure call");
    ps_ast_argument **args = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_argument *));
    ASSERT_RETURN_FALSE(args != NULL);
    args[0] = argument;

    ps_ast_debug_line("Create the PROCEDURE CALL statement");
    ps_ast_call *statement = ps_ast_create_call(3, 5, PS_AST_PROCEDURE_CALL, &ps_system_procedure_writeln, 1, args);
    ASSERT_RETURN_FALSE(statement != NULL);

    ps_ast_debug_line("Add the statement to the statement list");
    block_program->statement_list->statements[0] = (ps_ast_node *)statement;

    ps_ast_debug_line("Debug print the program");
    ps_ast_debug = true;
    ps_ast_debug_line("================================================================");
    ps_ast_debug_node((ps_ast_node *)block_program);
    ps_ast_debug_line("================================================================");

    ps_ast_debug_line("Run the program and check that it returns true");
    result = ps_ast_run_program(interpreter, block_program);
    if (!result)
    {
        ps_ast_debug_line("Error running the program: %s (%d)", interpreter->error,
                          ps_error_get_message(interpreter->error));
    }

    ps_ast_debug_line("Free symbol table for the program %s", block_program->name);
    ps_memory_free(PS_MEMORY_AST, block_program->symbols);
    block_program->symbols = NULL;

    ps_ast_debug_line("Free program %s", block_program->name);
    block_program = (ps_ast_block *)ps_ast_free_block(block_program);
    ASSERT_RETURN_FALSE(block_program == NULL);

    ps_ast_debug_line("Free interpreter");
    interpreter = ps_interpreter_free(interpreter);
    ASSERT_RETURN_FALSE(interpreter == NULL);

    return true;
}
