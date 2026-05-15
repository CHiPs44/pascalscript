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
            ps_ast_debug_line(0, "Assertion failed: %s, function %s, file %s, line %d.", #expr, __func__, __FILE__,    \
                              __LINE__);                                                                               \
            return false;                                                                                              \
        }                                                                                                              \
    } while (0)

#define ASSERT_GOTO_CLEANUP(expr)                                                                                      \
    do                                                                                                                 \
    {                                                                                                                  \
        if (!(expr))                                                                                                   \
        {                                                                                                              \
            ps_ast_debug_line(0, "Assertion failed: %s, function %s, file %s, line %d.", #expr, __func__, __FILE__,    \
                              __LINE__);                                                                               \
            goto cleanup;                                                                                              \
        }                                                                                                              \
    } while (0)

ps_ast_block *ps_ast_test_create_block_program(const char *name)
{
    ps_ast_debug_line(0, "Create a PROGRAM node with name '%s' at line 1, column 1", name);
    ps_ast_block *block_program = ps_ast_create_block(1, 1, PS_AST_PROGRAM, name);
    if (block_program == NULL)
        return NULL;

    ps_ast_debug_line(0, "Create symbol table");
    block_program->symbols = ps_symbol_table_alloc(0, 0);
    ASSERT_GOTO_CLEANUP(block_program->symbols != NULL);

    ps_symbol *symbol_program = ps_symbol_alloc(PS_SYMBOL_KIND_PROGRAM, name, NULL);
    ASSERT_GOTO_CLEANUP(symbol_program != NULL);

    ps_symbol_table_error error = ps_symbol_table_add(block_program->symbols, symbol_program);
    ASSERT_GOTO_CLEANUP(error == PS_SYMBOL_TABLE_ERROR_NONE);

    ps_ast_debug_line(0, "Check that the PROGRAM node has the expected values");
    ASSERT_GOTO_CLEANUP(block_program->group == PS_AST_BLOCK);
    ASSERT_GOTO_CLEANUP(block_program->kind == PS_AST_PROGRAM);
    ASSERT_GOTO_CLEANUP(block_program->line == 1);
    ASSERT_GOTO_CLEANUP(block_program->column == 1);
    ASSERT_GOTO_CLEANUP(strcmp(block_program->name, name) == 0);
    ASSERT_GOTO_CLEANUP(block_program->n_vars == 0);
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
    if (block_program->symbols != NULL)
    {
        ps_ast_debug_line(0, "Free symbol table for program %s", block_program->name);
        ps_memory_free(PS_MEMORY_SYMBOL, block_program->symbols->symbols);
        ps_memory_free(PS_MEMORY_AST, block_program->symbols);
        block_program->symbols = NULL;
    }

    ps_ast_debug_line(0, "Free the PROGRAM block node");
    block_program = (ps_ast_block *)ps_ast_free_block(block_program);
    ASSERT_RETURN_FALSE(block_program == NULL);

    return true;
}

ps_interpreter *ps_ast_test_create_interpreter(ps_ast_block *block_program)
{
    ps_ast_debug_line(0, "Create an interpreter");
    ps_interpreter *interpreter = ps_interpreter_alloc(true, false, false);
    ASSERT_GOTO_CLEANUP(interpreter != NULL);

    ps_ast_debug_line(0, "Enter environment for the program %s", block_program->name);
    ASSERT_GOTO_CLEANUP(ps_interpreter_enter_environment(interpreter, block_program->name, NULL, 0, NULL));

    ps_ast_debug_line(0, "Add PROGRAM symbol to the current environment symbol table");
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
    ps_ast_debug_line(0, "Exit environment for the program %s", block_program->name);
    ASSERT_RETURN_FALSE(ps_interpreter_exit_environment(interpreter));
    ps_ast_debug_line(0, "Free interpreter");
    interpreter = ps_interpreter_free(interpreter);
    ASSERT_RETURN_FALSE(interpreter == NULL);
    return true;
}

/**
 * @brief Test minimal Pascal program
 *  L/C 123456789012345678901234567890123456789012345678901234567890
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

    ps_ast_debug_line(0, "Debug print the program");
    ps_ast_debug = true;
    ps_ast_debug_line(0, "================================================================");
    ps_ast_debug_node(0, (ps_ast_node *)block_program);
    ps_ast_debug_line(0, "================================================================");

    ps_ast_debug_line(0, "Run the program and check that it returns true");
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

    ps_ast_debug_line(0, "Create variable symbols I ² J of type Integer and add them to the symbol tables");
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

    ps_ast_debug_line(0, "Create a statement list with 2 statements");
    block_program->statement_list = ps_ast_create_statement_list(3, 5, 2);
    ASSERT_RETURN_FALSE(block_program->statement_list != NULL);

    ps_ast_debug_line(0, "Create the assignment statement I := 21 * 2;");
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

    ps_ast_debug_line(0, "Create the assignment statement J := I;");
    ps_ast_variable_simple *variable_j = ps_ast_create_variable_simple(4, 5, PS_AST_LVALUE_SIMPLE, symbol_j);
    ASSERT_RETURN_FALSE(variable_j != NULL);
    ps_ast_variable_simple *rvalue_i = ps_ast_create_variable_simple(4, 10, PS_AST_RVALUE_SIMPLE, symbol_i);
    ps_ast_assignment *assignment_j =
        ps_ast_create_assignment(4, 5, (ps_ast_node *)variable_j, (ps_ast_node *)rvalue_i);
    ASSERT_RETURN_FALSE(assignment_i != NULL);
    block_program->statement_list->statements[1] = (ps_ast_node *)assignment_j;

    ps_ast_debug_line(0, "Debug print the program");
    ps_ast_debug = true;
    ps_ast_debug_line(0, "================================================================");
    ps_ast_debug_node(0, (ps_ast_node *)block_program);
    ps_ast_debug_line(0, "================================================================");

    ps_ast_debug_line(0, "Run the program and check that it returns true");
    result = ps_ast_run_program(interpreter, block_program);
    ps_ast_debug_line(0, "Interpreter error:   %s", ps_error_get_message(interpreter->error));
    ps_ast_debug_line(0, "Interpreter message: %s", interpreter->message);
    ASSERT_RETURN_FALSE(result);

    ps_ast_debug_line(0, "Check that variable I has the expected value 42");
    ASSERT_RETURN_FALSE(symbol_i->value != NULL);
    ASSERT_RETURN_FALSE(symbol_i->value->type == &ps_system_integer);
    ps_ast_debug_line(0, "Variable I value: %d", symbol_i->value->data.i);
    ASSERT_RETURN_FALSE(symbol_i->value->data.i == 42);

    ps_ast_debug_line(0, "Check that variable J has the expected value 42");
    ASSERT_RETURN_FALSE(symbol_j->value != NULL);
    ASSERT_RETURN_FALSE(symbol_j->value->type == &ps_system_integer);
    ps_ast_debug_line(0, "Variable J value: %d", symbol_i->value->data.i);
    ASSERT_RETURN_FALSE(symbol_j->value->data.i == 42);

    ps_symbol_table_dump(stderr, NULL, block_program->symbols);

    ps_ast_test_delete_interpreter(interpreter, block_program);

    ps_ast_test_delete_block_program(block_program);

    return true;
}

/**
 * @brief Test If-Then-Else Pascal program
 * L/C 123456789012345678901234567890123456789012345678901234567890
 * 1   Program IfThenElse;
 * 2   Var I, J: Integer;
 * 3   Begin
 * 4       I := 10;
 * 5       J := 0;
 * 6       If I = 10 Then
 * 7           J := 42
 * 8       Else
 * 9           J := 99;
 * 10  End.
 */
bool ps_ast_test_if_then_else()
{
    bool result;

    ps_ast_block *block_program = ps_ast_test_create_block_program("IFTHENELSE");
    ASSERT_RETURN_FALSE(block_program != NULL);

    ps_interpreter *interpreter = ps_ast_test_create_interpreter(block_program);
    ASSERT_RETURN_FALSE(interpreter != NULL);

    ps_ast_debug_line(0, "Create variable symbols I and J of type Integer and add them to the symbol tables");
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

    ps_ast_debug_line(0, "Create a statement list with 3 statements (I := 10; J := 0; If...)");
    block_program->statement_list = ps_ast_create_statement_list(4, 5, 3);
    ASSERT_RETURN_FALSE(block_program->statement_list != NULL);

    ps_ast_debug_line(0, "Create the first assignment statement I := 10;");
    ps_ast_variable_simple *variable_i = ps_ast_create_variable_simple(4, 5, PS_AST_LVALUE_SIMPLE, symbol_i);
    ASSERT_RETURN_FALSE(variable_i != NULL);
    ps_value value_u_10 = {.allocated = false, .type = &ps_system_unsigned, .data.u = 10};
    ps_ast_value *rvalue_u_10 = ps_ast_create_rvalue_const(4, 10, value_u_10);
    ASSERT_RETURN_FALSE(rvalue_u_10 != NULL);
    ps_ast_assignment *assignment_i =
        ps_ast_create_assignment(4, 5, (ps_ast_node *)variable_i, (ps_ast_node *)rvalue_u_10);
    ASSERT_RETURN_FALSE(assignment_i != NULL);
    block_program->statement_list->statements[0] = (ps_ast_node *)assignment_i;

    ps_ast_debug_line(0, "Create the second assignment statement J := 0;");
    ps_ast_variable_simple *variable_j_1 = ps_ast_create_variable_simple(5, 5, PS_AST_LVALUE_SIMPLE, symbol_j);
    ASSERT_RETURN_FALSE(variable_j_1 != NULL);
    ps_value value_u_0 = {.allocated = false, .type = &ps_system_unsigned, .data.u = 0};
    ps_ast_value *rvalue_u_0 = ps_ast_create_rvalue_const(5, 10, value_u_0);
    ASSERT_RETURN_FALSE(rvalue_u_0 != NULL);
    ps_ast_assignment *assignment_j_init =
        ps_ast_create_assignment(5, 5, (ps_ast_node *)variable_j_1, (ps_ast_node *)rvalue_u_0);
    ASSERT_RETURN_FALSE(assignment_j_init != NULL);
    block_program->statement_list->statements[1] = (ps_ast_node *)assignment_j_init;

    ps_ast_debug_line(0, "Create the condition I = 10");
    ps_ast_variable_simple *rvalue_i = ps_ast_create_variable_simple(6, 8, PS_AST_RVALUE_SIMPLE, symbol_i);
    ASSERT_RETURN_FALSE(rvalue_i != NULL);
    ps_value value_u_10_cond = {.allocated = false, .type = &ps_system_unsigned, .data.u = 10};
    ps_ast_value *rvalue_u_10_cond = ps_ast_create_rvalue_const(6, 12, value_u_10_cond);
    ASSERT_RETURN_FALSE(rvalue_u_10_cond != NULL);
    ps_ast_binary_operation *condition =
        ps_ast_create_binary_operation(6, 10, PS_OP_EQ, (ps_ast_node *)rvalue_i, (ps_ast_node *)rvalue_u_10_cond);
    ASSERT_RETURN_FALSE(condition != NULL);

    ps_ast_debug_line(0, "Create the THEN branch with J := 42");
    ps_ast_statement_list *then_branch = ps_ast_create_statement_list(7, 9, 1);
    ASSERT_RETURN_FALSE(then_branch != NULL);
    ps_ast_variable_simple *variable_j_then = ps_ast_create_variable_simple(7, 9, PS_AST_LVALUE_SIMPLE, symbol_j);
    ASSERT_RETURN_FALSE(variable_j_then != NULL);
    ps_value value_u_42 = {.allocated = false, .type = &ps_system_unsigned, .data.u = 42};
    ps_ast_value *rvalue_u_42 = ps_ast_create_rvalue_const(7, 14, value_u_42);
    ASSERT_RETURN_FALSE(rvalue_u_42 != NULL);
    ps_ast_assignment *assignment_j_then =
        ps_ast_create_assignment(7, 9, (ps_ast_node *)variable_j_then, (ps_ast_node *)rvalue_u_42);
    ASSERT_RETURN_FALSE(assignment_j_then != NULL);
    then_branch->statements[0] = (ps_ast_node *)assignment_j_then;

    ps_ast_debug_line(0, "Create the ELSE branch with J := 99");
    ps_ast_statement_list *else_branch = ps_ast_create_statement_list(9, 9, 1);
    ASSERT_RETURN_FALSE(else_branch != NULL);
    ps_ast_variable_simple *variable_j_else = ps_ast_create_variable_simple(9, 9, PS_AST_LVALUE_SIMPLE, symbol_j);
    ASSERT_RETURN_FALSE(variable_j_else != NULL);
    ps_value value_u_99 = {.allocated = false, .type = &ps_system_unsigned, .data.u = 99};
    ps_ast_value *rvalue_u_99 = ps_ast_create_rvalue_const(9, 14, value_u_99);
    ASSERT_RETURN_FALSE(rvalue_u_99 != NULL);
    ps_ast_assignment *assignment_j_else =
        ps_ast_create_assignment(9, 9, (ps_ast_node *)variable_j_else, (ps_ast_node *)rvalue_u_99);
    ASSERT_RETURN_FALSE(assignment_j_else != NULL);
    else_branch->statements[0] = (ps_ast_node *)assignment_j_else;

    ps_ast_debug_line(0, "Create the IF statement");
    ps_ast_if *if_statement = ps_ast_create_if(6, 5, (ps_ast_node *)condition, then_branch, else_branch);
    ASSERT_RETURN_FALSE(if_statement != NULL);
    block_program->statement_list->statements[2] = (ps_ast_node *)if_statement;

    ps_ast_debug_line(0, "Debug print the program");
    ps_ast_debug = true;
    ps_ast_debug_line(0, "================================================================");
    ps_ast_debug_node(0, (ps_ast_node *)block_program);
    ps_ast_debug_line(0, "================================================================");

    ps_ast_debug_line(0, "Run the program and check that it returns true");
    result = ps_ast_run_program(interpreter, block_program);
    ps_ast_debug_line(0, "Interpreter error:   %s", ps_error_get_message(interpreter->error));
    ps_ast_debug_line(0, "Interpreter message: %s", interpreter->message);
    ASSERT_RETURN_FALSE(result);

    ps_ast_debug_line(0, "Check that variable I has the expected value 10");
    ASSERT_RETURN_FALSE(symbol_i->value != NULL);
    ASSERT_RETURN_FALSE(symbol_i->value->type == &ps_system_integer);
    ps_ast_debug_line(0, "Variable I value: %d", symbol_i->value->data.i);
    ASSERT_RETURN_FALSE(symbol_i->value->data.i == 10);

    ps_ast_debug_line(0, "Check that variable J has the expected value 42 (from THEN branch)");
    ASSERT_RETURN_FALSE(symbol_j->value != NULL);
    ASSERT_RETURN_FALSE(symbol_j->value->type == &ps_system_integer);
    ps_ast_debug_line(0, "Variable J value: %d", symbol_j->value->data.i);
    ASSERT_RETURN_FALSE(symbol_j->value->data.i == 42);

    ps_symbol_table_dump(stderr, NULL, block_program->symbols);

    ps_ast_test_delete_interpreter(interpreter, block_program);

    ps_ast_test_delete_block_program(block_program);

    return true;
}

/**
 * @brief Test While-Do Pascal program
 * L/C 123456789012345678901234567890123456789012345678901234567890
 * 1   Program WhileDo;
 * 2   Var I: Integer;
 * 3   Begin
 * 4       I := 5;
 * 5       While I > 0 Do
 * 6           I := I - 1;
 * 7   End.
 */
bool ps_ast_test_while_do()
{
    bool result;

    ps_ast_block *block_program = ps_ast_test_create_block_program("WHILEDO");
    ASSERT_RETURN_FALSE(block_program != NULL);

    ps_interpreter *interpreter = ps_ast_test_create_interpreter(block_program);
    ASSERT_RETURN_FALSE(interpreter != NULL);

    ps_ast_debug_line(0, "Create variable symbol I of type Integer and add it to the symbol tables");
    ps_value value_i = {.allocated = false, .type = &ps_system_integer, .data.i = 0};
    ps_symbol *symbol_i = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, "I", &value_i);
    result = ps_interpreter_add_symbol(interpreter, symbol_i);
    ASSERT_RETURN_FALSE(result);
    ps_symbol_table_error error = ps_symbol_table_add(block_program->symbols, symbol_i);
    ASSERT_RETURN_FALSE(error == PS_SYMBOL_TABLE_ERROR_NONE);
    block_program->n_vars = 1;

    ps_ast_debug_line(0, "Create a statement list with 2 statements (I := 5; While loop)");
    block_program->statement_list = ps_ast_create_statement_list(4, 5, 2);
    ASSERT_RETURN_FALSE(block_program->statement_list != NULL);

    ps_ast_debug_line(0, "Create the first assignment statement I := 5;");
    ps_ast_variable_simple *variable_i_init = ps_ast_create_variable_simple(4, 5, PS_AST_LVALUE_SIMPLE, symbol_i);
    ASSERT_RETURN_FALSE(variable_i_init != NULL);
    ps_value value_i_5 = {.allocated = false, .type = &ps_system_integer, .data.i = 5};
    ps_ast_value *rvalue_i_5 = ps_ast_create_rvalue_const(4, 10, value_i_5);
    ASSERT_RETURN_FALSE(rvalue_i_5 != NULL);
    ps_ast_assignment *assignment_i_init =
        ps_ast_create_assignment(4, 5, (ps_ast_node *)variable_i_init, (ps_ast_node *)rvalue_i_5);
    ASSERT_RETURN_FALSE(assignment_i_init != NULL);
    block_program->statement_list->statements[0] = (ps_ast_node *)assignment_i_init;

    ps_ast_debug_line(0, "Create the WHILE condition I > 0");
    ps_ast_variable_simple *rvalue_i_cond = ps_ast_create_variable_simple(5, 10, PS_AST_RVALUE_SIMPLE, symbol_i);
    ASSERT_RETURN_FALSE(rvalue_i_cond != NULL);
    ps_value value_i_0 = {.allocated = false, .type = &ps_system_integer, .data.i = 0};
    ps_ast_value *rvalue_i_0 = ps_ast_create_rvalue_const(5, 14, value_i_0);
    ASSERT_RETURN_FALSE(rvalue_i_0 != NULL);
    ps_ast_binary_operation *while_condition =
        ps_ast_create_binary_operation(5, 12, PS_OP_GT, (ps_ast_node *)rvalue_i_cond, (ps_ast_node *)rvalue_i_0);
    ASSERT_RETURN_FALSE(while_condition != NULL);

    ps_ast_debug_line(0, "Create the WHILE loop body: I := I - 1");
    ps_ast_statement_list *while_body = ps_ast_create_statement_list(6, 9, 1);
    ASSERT_RETURN_FALSE(while_body != NULL);
    ps_ast_variable_simple *variable_i_body = ps_ast_create_variable_simple(6, 9, PS_AST_LVALUE_SIMPLE, symbol_i);
    ASSERT_RETURN_FALSE(variable_i_body != NULL);
    ps_ast_variable_simple *rvalue_i_body = ps_ast_create_variable_simple(6, 19, PS_AST_RVALUE_SIMPLE, symbol_i);
    ASSERT_RETURN_FALSE(rvalue_i_body != NULL);
    ps_value value_i_1 = {.allocated = false, .type = &ps_system_integer, .data.i = 1};
    ps_ast_value *rvalue_i_1 = ps_ast_create_rvalue_const(6, 23, value_i_1);
    ASSERT_RETURN_FALSE(rvalue_i_1 != NULL);
    ps_ast_binary_operation *i_minus_1 =
        ps_ast_create_binary_operation(6, 21, PS_OP_SUB, (ps_ast_node *)rvalue_i_body, (ps_ast_node *)rvalue_i_1);
    ASSERT_RETURN_FALSE(i_minus_1 != NULL);
    ps_ast_assignment *assignment_i_body =
        ps_ast_create_assignment(6, 9, (ps_ast_node *)variable_i_body, (ps_ast_node *)i_minus_1);
    ASSERT_RETURN_FALSE(assignment_i_body != NULL);
    while_body->statements[0] = (ps_ast_node *)assignment_i_body;

    ps_ast_debug_line(0, "Create the WHILE statement");
    ps_ast_while *while_statement = ps_ast_create_while(5, 5, (ps_ast_node *)while_condition, while_body);
    ASSERT_RETURN_FALSE(while_statement != NULL);
    block_program->statement_list->statements[1] = (ps_ast_node *)while_statement;

    ps_ast_debug_line(0, "Debug print the program");
    ps_ast_debug = true;
    ps_ast_debug_line(0, "================================================================");
    ps_ast_debug_node(0, (ps_ast_node *)block_program);
    ps_ast_debug_line(0, "================================================================");

    ps_ast_debug_line(0, "Run the program and check that it returns true");
    result = ps_ast_run_program(interpreter, block_program);
    ps_ast_debug_line(0, "Interpreter error:   %s", ps_error_get_message(interpreter->error));
    ps_ast_debug_line(0, "Interpreter message: %s", interpreter->message);
    ASSERT_RETURN_FALSE(result);

    ps_ast_debug_line(0, "Check that variable I has the expected value 0 (loop condition became false)");
    ASSERT_RETURN_FALSE(symbol_i->value != NULL);
    ASSERT_RETURN_FALSE(symbol_i->value->type == &ps_system_integer);
    ps_ast_debug_line(0, "Variable I value: %d", symbol_i->value->data.i);
    ASSERT_RETURN_FALSE(symbol_i->value->data.i == 0);

    ps_symbol_table_dump(stderr, NULL, block_program->symbols);

    ps_ast_test_delete_interpreter(interpreter, block_program);

    ps_ast_test_delete_block_program(block_program);

    return true;
}

/**
 * @brief Test Repeat-Until Pascal program
 * L/C 123456789012345678901234567890123456789012345678901234567890
 * 1   Program RepeatUntil;
 * 2   Var I: Integer;
 * 3   Begin
 * 4       I := 5;
 * 5       Repeat
 * 6           I := I - 1;
 * 7       Until I = 0;
 * 8   End.
 */
bool ps_ast_test_repeat_until()
{
    bool result;

    ps_ast_block *block_program = ps_ast_test_create_block_program("REPEATUNTIL");
    ASSERT_RETURN_FALSE(block_program != NULL);

    ps_interpreter *interpreter = ps_ast_test_create_interpreter(block_program);
    ASSERT_RETURN_FALSE(interpreter != NULL);

    ps_ast_debug_line(0, "Create variable symbol I of type Integer and add it to the symbol tables");
    ps_value value_i = {.allocated = false, .type = &ps_system_integer, .data.i = 0};
    ps_symbol *symbol_i = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, "I", &value_i);
    result = ps_interpreter_add_symbol(interpreter, symbol_i);
    ASSERT_RETURN_FALSE(result);
    ps_symbol_table_error error = ps_symbol_table_add(block_program->symbols, symbol_i);
    ASSERT_RETURN_FALSE(error == PS_SYMBOL_TABLE_ERROR_NONE);
    block_program->n_vars = 1;

    ps_ast_debug_line(0, "Create a statement list with 2 statements (I := 5; Repeat loop)");
    block_program->statement_list = ps_ast_create_statement_list(4, 5, 2);
    ASSERT_RETURN_FALSE(block_program->statement_list != NULL);

    ps_ast_debug_line(0, "Create the first assignment statement I := 5;");
    ps_ast_variable_simple *variable_i_init = ps_ast_create_variable_simple(4, 5, PS_AST_LVALUE_SIMPLE, symbol_i);
    ASSERT_RETURN_FALSE(variable_i_init != NULL);
    ps_value value_i_5 = {.allocated = false, .type = &ps_system_integer, .data.i = 5};
    ps_ast_value *rvalue_i_5 = ps_ast_create_rvalue_const(4, 10, value_i_5);
    ASSERT_RETURN_FALSE(rvalue_i_5 != NULL);
    ps_ast_assignment *assignment_i_init =
        ps_ast_create_assignment(4, 5, (ps_ast_node *)variable_i_init, (ps_ast_node *)rvalue_i_5);
    ASSERT_RETURN_FALSE(assignment_i_init != NULL);
    block_program->statement_list->statements[0] = (ps_ast_node *)assignment_i_init;

    ps_ast_debug_line(0, "Create the REPEAT loop body: I := I - 1");
    ps_ast_statement_list *repeat_body = ps_ast_create_statement_list(6, 9, 1);
    ASSERT_RETURN_FALSE(repeat_body != NULL);
    ps_ast_variable_simple *variable_i_body = ps_ast_create_variable_simple(6, 9, PS_AST_LVALUE_SIMPLE, symbol_i);
    ASSERT_RETURN_FALSE(variable_i_body != NULL);
    ps_ast_variable_simple *rvalue_i_body = ps_ast_create_variable_simple(6, 19, PS_AST_RVALUE_SIMPLE, symbol_i);
    ASSERT_RETURN_FALSE(rvalue_i_body != NULL);
    ps_value value_i_1 = {.allocated = false, .type = &ps_system_integer, .data.i = 1};
    ps_ast_value *rvalue_i_1 = ps_ast_create_rvalue_const(6, 23, value_i_1);
    ASSERT_RETURN_FALSE(rvalue_i_1 != NULL);
    ps_ast_binary_operation *i_minus_1 =
        ps_ast_create_binary_operation(6, 21, PS_OP_SUB, (ps_ast_node *)rvalue_i_body, (ps_ast_node *)rvalue_i_1);
    ASSERT_RETURN_FALSE(i_minus_1 != NULL);
    ps_ast_assignment *assignment_i_body =
        ps_ast_create_assignment(6, 9, (ps_ast_node *)variable_i_body, (ps_ast_node *)i_minus_1);
    ASSERT_RETURN_FALSE(assignment_i_body != NULL);
    repeat_body->statements[0] = (ps_ast_node *)assignment_i_body;

    ps_ast_debug_line(0, "Create the REPEAT condition I = 0");
    ps_ast_variable_simple *rvalue_i_cond = ps_ast_create_variable_simple(7, 10, PS_AST_RVALUE_SIMPLE, symbol_i);
    ASSERT_RETURN_FALSE(rvalue_i_cond != NULL);
    ps_value value_i_0 = {.allocated = false, .type = &ps_system_integer, .data.i = 0};
    ps_ast_value *rvalue_i_0 = ps_ast_create_rvalue_const(7, 14, value_i_0);
    ASSERT_RETURN_FALSE(rvalue_i_0 != NULL);
    ps_ast_binary_operation *repeat_condition =
        ps_ast_create_binary_operation(7, 12, PS_OP_EQ, (ps_ast_node *)rvalue_i_cond, (ps_ast_node *)rvalue_i_0);
    ASSERT_RETURN_FALSE(repeat_condition != NULL);

    ps_ast_debug_line(0, "Create the REPEAT statement");
    ps_ast_repeat *repeat_statement = ps_ast_create_repeat(5, 5, repeat_body, (ps_ast_node *)repeat_condition);
    ASSERT_RETURN_FALSE(repeat_statement != NULL);
    block_program->statement_list->statements[1] = (ps_ast_node *)repeat_statement;

    ps_ast_debug_line(0, "Debug print the program");
    ps_ast_debug = true;
    ps_ast_debug_line(0, "================================================================");
    ps_ast_debug_node(0, (ps_ast_node *)block_program);
    ps_ast_debug_line(0, "================================================================");

    ps_ast_debug_line(0, "Run the program and check that it returns true");
    result = ps_ast_run_program(interpreter, block_program);
    ps_ast_debug_line(0, "Interpreter error:   %s", ps_error_get_message(interpreter->error));
    ps_ast_debug_line(0, "Interpreter message: %s", interpreter->message);
    ASSERT_RETURN_FALSE(result);

    ps_ast_debug_line(0, "Check that variable I has the expected value 0 (loop condition became true)");
    ASSERT_RETURN_FALSE(symbol_i->value != NULL);
    ASSERT_RETURN_FALSE(symbol_i->value->type == &ps_system_integer);
    ps_ast_debug_line(0, "Variable I value: %d", symbol_i->value->data.i);
    ASSERT_RETURN_FALSE(symbol_i->value->data.i == 0);

    ps_symbol_table_dump(stderr, NULL, block_program->symbols);

    ps_ast_test_delete_interpreter(interpreter, block_program);

    ps_ast_test_delete_block_program(block_program);

    return true;
}

/**
 * @brief Test For-Do Pascal program
 * L/C 123456789012345678901234567890123456789012345678901234567890
 * 1   Program ForDo;
 * 2   Var I, Sum: Integer;
 * 3   Begin
 * 4       Sum := 0;
 * 5       For I := 1 To 5 Do
 * 6           Sum := Sum + I;
 * 7   End.
 */
bool ps_ast_test_for_do()
{
    bool result;
    ps_symbol_table_error error;

    ps_ast_block *block_program = ps_ast_test_create_block_program("FORDO");
    ASSERT_RETURN_FALSE(block_program != NULL);

    ps_interpreter *interpreter = ps_ast_test_create_interpreter(block_program);
    ASSERT_RETURN_FALSE(interpreter != NULL);

    ps_ast_debug_line(0, "Create variable symbols I and Sum of type Integer and add them to the symbol tables");
    block_program->n_vars = 2;

    ps_value value_i = {.allocated = false, .type = &ps_system_integer, .data.i = 0};
    ps_symbol *symbol_i = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, "I", &value_i);
    result = ps_interpreter_add_symbol(interpreter, symbol_i);
    ASSERT_RETURN_FALSE(result);
    error = ps_symbol_table_add(block_program->symbols, symbol_i);
    ASSERT_RETURN_FALSE(error == PS_SYMBOL_TABLE_ERROR_NONE);

    ps_value value_sum = {.allocated = false, .type = &ps_system_integer, .data.i = 0};
    ps_symbol *symbol_sum = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, "SUM", &value_sum);
    result = ps_interpreter_add_symbol(interpreter, symbol_sum);
    ASSERT_RETURN_FALSE(result);
    error = ps_symbol_table_add(block_program->symbols, symbol_sum);
    ASSERT_RETURN_FALSE(error == PS_SYMBOL_TABLE_ERROR_NONE);

    ps_ast_debug_line(0, "Create a statement list with 2 statements (Sum := 0; For loop)");
    block_program->statement_list = ps_ast_create_statement_list(4, 5, 2);
    ASSERT_RETURN_FALSE(block_program->statement_list != NULL);

    ps_ast_debug_line(0, "Create the first assignment statement Sum := 0;");
    ps_ast_variable_simple *variable_sum_init = ps_ast_create_variable_simple(4, 5, PS_AST_LVALUE_SIMPLE, symbol_sum);
    ASSERT_RETURN_FALSE(variable_sum_init != NULL);
    ps_value value_u_0 = {.allocated = false, .type = &ps_system_unsigned, .data.u = 0};
    ps_ast_value *rvalue_u_0 = ps_ast_create_rvalue_const(4, 13, value_u_0);
    ASSERT_RETURN_FALSE(rvalue_u_0 != NULL);
    ps_ast_assignment *assignment_sum_init =
        ps_ast_create_assignment(4, 5, (ps_ast_node *)variable_sum_init, (ps_ast_node *)rvalue_u_0);
    ASSERT_RETURN_FALSE(assignment_sum_init != NULL);
    block_program->statement_list->statements[0] = (ps_ast_node *)assignment_sum_init;

    ps_ast_debug_line(0, "Create the FOR loop body: Sum := Sum + I");
    ps_ast_statement_list *for_body = ps_ast_create_statement_list(6, 9, 1);
    ASSERT_RETURN_FALSE(for_body != NULL);
    ps_ast_variable_simple *variable_sum_body = ps_ast_create_variable_simple(6, 9, PS_AST_LVALUE_SIMPLE, symbol_sum);
    ASSERT_RETURN_FALSE(variable_sum_body != NULL);
    ps_ast_variable_simple *rvalue_sum = ps_ast_create_variable_simple(6, 19, PS_AST_RVALUE_SIMPLE, symbol_sum);
    ASSERT_RETURN_FALSE(rvalue_sum != NULL);
    ps_ast_variable_simple *rvalue_i = ps_ast_create_variable_simple(6, 26, PS_AST_RVALUE_SIMPLE, symbol_i);
    ASSERT_RETURN_FALSE(rvalue_i != NULL);
    ps_ast_binary_operation *add_sum_i =
        ps_ast_create_binary_operation(6, 24, PS_OP_ADD, (ps_ast_node *)rvalue_sum, (ps_ast_node *)rvalue_i);
    ASSERT_RETURN_FALSE(add_sum_i != NULL);
    ps_ast_assignment *assignment_sum_body =
        ps_ast_create_assignment(6, 9, (ps_ast_node *)variable_sum_body, (ps_ast_node *)add_sum_i);
    ASSERT_RETURN_FALSE(assignment_sum_body != NULL);
    for_body->statements[0] = (ps_ast_node *)assignment_sum_body;

    ps_ast_debug_line(0, "Create the FOR loop variable I");
    ps_ast_variable_simple *for_variable = ps_ast_create_variable_simple(5, 8, PS_AST_LVALUE_SIMPLE, symbol_i);
    ASSERT_RETURN_FALSE(for_variable != NULL);

    ps_ast_debug_line(0, "Create the FOR loop start value 1");
    ps_value value_u_1 = {.allocated = false, .type = &ps_system_unsigned, .data.u = 1};
    ps_ast_value *for_start = ps_ast_create_rvalue_const(5, 13, value_u_1);
    ASSERT_RETURN_FALSE(for_start != NULL);

    ps_ast_debug_line(0, "Create the FOR loop end value 5");
    ps_value value_u_5 = {.allocated = false, .type = &ps_system_unsigned, .data.u = 5};
    ps_ast_value *for_end = ps_ast_create_rvalue_const(5, 18, value_u_5);
    ASSERT_RETURN_FALSE(for_end != NULL);

    ps_ast_debug_line(0, "Create the FOR statement: For I := 1 To 5 Do Sum := Sum + I");
    ps_ast_for *for_statement =
        ps_ast_create_for(5, 5, for_variable, (ps_ast_node *)for_start, (ps_ast_node *)for_end, 1, for_body);
    ASSERT_RETURN_FALSE(for_statement != NULL);
    block_program->statement_list->statements[1] = (ps_ast_node *)for_statement;

    ps_ast_debug_line(0, "Debug print the program");
    ps_ast_debug = true;
    ps_ast_debug_line(0, "================================================================");
    ps_ast_debug_node(0, (ps_ast_node *)block_program);
    ps_ast_debug_line(0, "================================================================");

    ps_ast_debug_line(0, "Run the program and check that it returns true");
    result = ps_ast_run_program(interpreter, block_program);
    ps_ast_debug_line(0, "Interpreter error:   %s", ps_error_get_message(interpreter->error));
    ps_ast_debug_line(0, "Interpreter message: %s", interpreter->message);
    ASSERT_RETURN_FALSE(result);

    ps_ast_debug_line(0, "Check that variable Sum has the expected value 15 (1+2+3+4+5)");
    ASSERT_RETURN_FALSE(symbol_sum->value != NULL);
    ASSERT_RETURN_FALSE(symbol_sum->value->type == &ps_system_integer);
    ps_ast_debug_line(0, "Variable Sum value: %d", symbol_sum->value->data.i);
    ASSERT_RETURN_FALSE(symbol_sum->value->data.i == 15);

    ps_symbol_table_dump(stderr, NULL, block_program->symbols);

    ps_ast_test_delete_interpreter(interpreter, block_program);

    ps_ast_test_delete_block_program(block_program);

    return true;
}

/**
 * @brief Test Hello Pascal program
 *      0        1         2         3
 *  L/C 123456789012345678901234567890123456789012345678901234567890
 *  1   Program Hello;
 *  2   Begin
 *  3       WriteLn('Hello, World!');
 *  4       WriteLn(-42);
 *  5   End.
 */
bool ps_ast_test_hello()
{
    bool result;

    ps_ast_block *block_program = ps_ast_test_create_block_program("HELLO");
    ASSERT_RETURN_FALSE(block_program != NULL);

    ps_interpreter *interpreter = ps_ast_test_create_interpreter(block_program);
    ASSERT_RETURN_FALSE(interpreter != NULL);

    ps_ast_debug_line(0, "Create a statement list with 2 statements");
    block_program->statement_list = ps_ast_create_statement_list(3, 5, 2);
    ASSERT_RETURN_FALSE(block_program->statement_list != NULL);

    ps_ast_debug_line(0, "Create the first by value argument");
    ps_string *hello = ps_string_heap_create(interpreter->string_heap, "Hello, World!");
    ASSERT_RETURN_FALSE(hello != NULL);
    ps_value value_hello = {.allocated = false, .type = &ps_system_string, .data.s = hello};
    ps_ast_value *argument_hello = ps_ast_create_rvalue_const(3, 13, value_hello);
    ASSERT_RETURN_FALSE(argument_hello != NULL);

    ps_ast_debug_line(0, "Create the first by value argument");
    ps_value value_i_42 = {.allocated = false, .type = &ps_system_integer, .data.i = -42};
    ps_ast_value *argument_i_42 = ps_ast_create_rvalue_const(4, 13, value_i_42);
    ASSERT_RETURN_FALSE(argument_i_42 != NULL);

    ps_ast_debug_line(0, "Create the argument list for the procedure call");
    ps_ast_node **args1 = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node *));
    ASSERT_RETURN_FALSE(args1 != NULL);
    args1[0] = (ps_ast_node *)argument_hello;

    ps_ast_debug_line(0, "Create the first PROCEDURE CALL statement");
    ps_ast_call *statement1 = ps_ast_create_call(3, 5, PS_AST_PROCEDURE_CALL, &ps_system_procedure_writeln, 1, args1);
    ASSERT_RETURN_FALSE(statement1 != NULL);

    ps_ast_debug_line(0, "Create the argument list for the second procedure call");
    ps_ast_node **args2 = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node *));
    ASSERT_RETURN_FALSE(args2 != NULL);
    args2[0] = (ps_ast_node *)argument_i_42;

    ps_ast_debug_line(0, "Create the second PROCEDURE CALL statement");
    ps_ast_call *statement2 = ps_ast_create_call(3, 5, PS_AST_PROCEDURE_CALL, &ps_system_procedure_writeln, 1, args2);
    ASSERT_RETURN_FALSE(statement2 != NULL);

    ps_ast_debug_line(0, "Add the statements to the statement list");
    block_program->statement_list->statements[0] = (ps_ast_node *)statement1;
    block_program->statement_list->statements[1] = (ps_ast_node *)statement2;

    ps_ast_debug_line(0, "Debug print the program");
    ps_ast_debug = true;
    ps_ast_debug_line(0, "================================================================");
    ps_ast_debug_node(0, (ps_ast_node *)block_program);
    ps_ast_debug_line(0, "================================================================");

    ps_ast_debug_line(0, "Run the program and check that it returns true");
    result = ps_ast_run_program(interpreter, block_program);
    if (!result)
    {
        ps_ast_debug_line(0, "Error running the program: %s (%d)", interpreter->error,
                          ps_error_get_message(interpreter->error));
    }

    ps_ast_test_delete_interpreter(interpreter, block_program);

    ps_ast_test_delete_block_program(block_program);

    return true;
}

bool ps_ast_test()
{
    bool result = true;

    ps_ast_debug_line(0, "****************************************************************");
    result &= ps_ast_test_minimal();
    ps_ast_debug_line(0, "****************************************************************");
    result &= ps_ast_test_assignment();
    ps_ast_debug_line(0, "****************************************************************");
    result &= ps_ast_test_if_then_else();
    ps_ast_debug_line(0, "****************************************************************");
    result &= ps_ast_test_while_do();
    ps_ast_debug_line(0, "****************************************************************");
    result &= ps_ast_test_repeat_until();
    ps_ast_debug_line(0, "****************************************************************");
    result &= ps_ast_test_for_do();
    ps_ast_debug_line(0, "****************************************************************");
    result &= ps_ast_test_hello();
    ps_ast_debug_line(0, "****************************************************************");

    return result;
}
