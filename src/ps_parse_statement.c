/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <stdio.h>

#include "ps_array.h"
#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_compiler.h"
#include "ps_functions.h"
#include "ps_parse.h"
#include "ps_parse_assignment.h"
#include "ps_parse_call.h"
#include "ps_parse_executable.h"
#include "ps_parse_expression.h"
#include "ps_parse_statement.h"
#include "ps_procedures.h"
#include "ps_symbol.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_type_definition.h"

/**
 * Parse statement:
 *  - compound: Begin ... End
 *  - assignment: Variable := Expression
 *  - procedure call: Procedure(Arguments)
 *  - conditional:
 *     - If ... Then ... Else ...
 *  - loops:
 *     - Repeat ... Until ...
 *     - While ... Do ...
 *     - For variable := start To/Downto finish Do ...
 *  Next:
 *   - Case ... Of ... [ Else|Otherwise ... ] End
 */
bool ps_parse_statement(ps_compiler *compiler, ps_ast_block *block, ps_ast_node **statement_ptr)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(statement_ptr != NULL);

    PARSE_BEGIN("STATEMENT", "");
    (void)start_line;
    (void)start_column;

    ps_ast_statement_list *statement_list = NULL;
    ps_ast_if *if_statement = NULL;
    ps_ast_repeat *repeat_statement = NULL;
    ps_ast_while *while_statement = NULL;
    ps_ast_for *for_statement = NULL;

    switch (lexer->current_token.type)
    {
    case PS_TOKEN_BEGIN:
        if (!ps_parse_compound_statement(compiler, block, &statement_list))
            TRACE_ERROR("COMPOUND")
        *statement_ptr = (ps_ast_node *)statement_list;
        break;
    case PS_TOKEN_IDENTIFIER:
        if (!ps_parse_assignment_or_procedure_call(compiler, block, statement_ptr))
            TRACE_ERROR("ASSIGNMENT/PROCEDURE")
        break;
    case PS_TOKEN_IF:
        if (!ps_parse_if_then_else(compiler, block, &if_statement))
            TRACE_ERROR("IF")
        *statement_ptr = (ps_ast_node *)if_statement;
        break;
    case PS_TOKEN_REPEAT:
        if (!ps_parse_repeat_until(compiler, block, &repeat_statement))
            TRACE_ERROR("REPEAT")
        *statement_ptr = (ps_ast_node *)repeat_statement;
        break;
    case PS_TOKEN_WHILE:
        if (!ps_parse_while_do(compiler, block, &while_statement))
            TRACE_ERROR("WHILE")
        *statement_ptr = (ps_ast_node *)while_statement;
        break;
    case PS_TOKEN_FOR:
        if (!ps_parse_for_do(compiler, block, &for_statement))
            TRACE_ERROR("FOR")
        *statement_ptr = (ps_ast_node *)for_statement;
        break;
    case PS_TOKEN_SEMI_COLON:
        statement_list = ps_ast_create_statement_list(start_line, start_column, 0);
        if (statement_list == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
        *statement_ptr = (ps_ast_node *)statement_list;
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }

    PARSE_END("OK")
}

/**
 * Parse compound statement:
 *      'BEGIN'
 *          [ STATEMENT [ ';' STATEMENT ]* ] [ ';' ]
 *      'END'
 * NB: ';' or '.' or whatever after END is analyzed in the caller
 */
bool ps_parse_compound_statement(ps_compiler *compiler, ps_ast_block *block, ps_ast_statement_list **statement_list_ptr)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(statement_list_ptr != NULL);

    PARSE_BEGIN("STATEMENT", "COMPOUND");

    // 'BEGIN'
    EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_BEGIN);
    READ_NEXT_TOKEN_OR_RETURN_FALSE

    // [ STATEMENT [ ';' STATEMENT ]* ] [ ';' ]
    ps_ast_statement_list *statement_list = NULL;
    if (!ps_parse_statement_list(compiler, block, &statement_list, PS_TOKEN_END))
        TRACE_ERROR("STATEMENT_LIST")
    // Fix statement list position to BEGIN token, not first statement (if any)
    statement_list->line = start_line;
    statement_list->column = start_column;
    *statement_list_ptr = statement_list;

    // 'END'
    EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_END)
    READ_NEXT_TOKEN_OR_RETURN_FALSE

    PARSE_END("OK")
}

/**
 * Parse
 *      'READ'  | 'READLN' | 'READSTR' [ '('
 *          variable [ ',' variable ] ]*
 *      ')' ] ;
 * Next steps:
 *   Read from text file:
 *      'READ' | 'READLN' [ '('
 *          [ file_variable ',' ]
 *          variable [ ',' variable ]*
 *      ')' ] ;
 *   Read from binary file:
 *      'READ' '('
 *          file_variable ',' variable
 *      ')' ;
 */
bool ps_parse_read_or_readln_or_readstr(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call_ptr,
                                        ps_read_write_mode mode)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(call_ptr != NULL);

    (void)compiler;
    (void)block;
    (void)call_ptr;
    (void)mode;
    PARSE_BEGIN("STATEMENT", "READ_OR_READLN_READSTR")
    (void)start_line;
    (void)start_column;
    ps_compiler_set_error_message(compiler, PS_ERROR_NOT_IMPLEMENTED, "Read, ReadLn and ReadStr not implemented yet!");
    TRACE_ERROR("NOT_IMPLEMENTED")
}

static inline bool ps_parse_write_format(ps_compiler *compiler, ps_ast_block *block, int16_t *width, int16_t *precision)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(width != NULL);
    assert(precision != NULL);

    PARSE_BEGIN("WRITE_OR_WRITELN", "FORMAT");
    (void)start_line;
    (void)start_column;

    *width = 0;
    *precision = 0;
    if (lexer->current_token.type == PS_TOKEN_COLON)
    {
        READ_NEXT_TOKEN_OR_RETURN_FALSE
        EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_INTEGER_VALUE)
        int16_t w = (int16_t)(lexer->current_token.value.i);
        if (w < 0 || w > 255)
            RETURN_ERROR(PS_ERROR_OUT_OF_RANGE)
        *width = w;
        READ_NEXT_TOKEN_OR_RETURN_FALSE
        if (lexer->current_token.type == PS_TOKEN_COLON)
        {
            READ_NEXT_TOKEN_OR_RETURN_FALSE
            EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_INTEGER_VALUE)
            int16_t p = (int16_t)(lexer->current_token.value.i);
            if (p < 0 || p > 255)
                RETURN_ERROR(PS_ERROR_OUT_OF_RANGE)
            *precision = p;
            READ_NEXT_TOKEN_OR_RETURN_FALSE
        }
    }

    PARSE_END("OK")
}

static bool ps_parse_writestr_string(ps_compiler *compiler, ps_ast_block *block, ps_read_write_mode mode,
                                     uint16_t *n_args, ps_ast_node *args[PS_PARAMETERS_MAX])
{
    ps_symbol *variable = NULL;
    ps_ast_variable *variable_node = NULL;
    ps_ast_block *owner = NULL;

    PARSE_BEGIN("STATEMENT", "WRITESTR_STRING");

    if (mode == PS_READ_WRITE_MODE_STR)
    {
        // STRING_VARIABLE
        //  - must be an ordinal simple variable, not an array item
        if (!ps_parse_variable_reference(compiler, block, &owner, &variable))
        {
            ps_compiler_set_error_message(compiler, PS_ERROR_EXPECTED_VARIABLE, "Expected variable, got %s",
                                          ps_symbol_get_kind_name(variable->kind));
            TRACE_ERROR("EXPECTED_VARIABLE")
        }
        if (!ps_value_is_string(variable->value))
        {
            ps_compiler_set_error_message(compiler, PS_ERROR_EXPECTED_STRING, "Expected string variable, got %s",
                                          ps_value_get_type_name(variable->value));
            TRACE_ERROR("STRING_VARIABLE")
        }
        variable_node = ps_ast_create_variable_simple(start_line, start_column, owner, PS_AST_LVALUE, variable);
        if (variable_node == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
        args[0] = (ps_ast_node *)variable_node;
        *n_args = 1;
        EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_COMMA)
        READ_NEXT_TOKEN_OR_RETURN_FALSE
    }

    PARSE_END("OK")
}

static bool ps_parse_write_expression(ps_compiler *compiler, ps_ast_block *block, uint16_t *n_args,
                                      ps_ast_node *args[PS_PARAMETERS_MAX], ps_ast_format formats[PS_PARAMETERS_MAX])
{
    PARSE_BEGIN("WRITE", "EXPRESSION")

    int16_t width = 0;
    int16_t precision = 0;
    ps_ast_node *expression = NULL;

    // Check number of arguments
    if (*n_args >= PS_PARAMETERS_MAX)
    {
        ps_compiler_set_error_message(compiler, PS_ERROR_TOO_MANY_ARGUMENTS, "Too many arguments, maximum is %d",
                                      PS_PARAMETERS_MAX);
        TRACE_ERROR("TOO_MANY_ARGUMENTS")
    }

    // Parse argument value
    if (!ps_parse_expression(compiler, block, &expression))
        TRACE_ERROR("EXPRESSION")

    // Retrieve optional string/numeric format :width[:precision]
    if (!ps_parse_write_format(compiler, block, &width, &precision))
        TRACE_ERROR("FORMAT")

    // Store argument and format
    args[*n_args] = expression;
    formats[*n_args].width = width;
    formats[*n_args].precision = precision;
    *n_args += 1;

    PARSE_END("OK")
}

/**
 * Parse
 *      'WRITE' | 'WRITELN' | 'WRITESTR' [ '('
 *         [ string_variable ',' ]
 *          expression [ ':' width [ ':' precision ] ]
 *          [ ',' expression [ ':' width [ ':' precision ] ] ]*
 *      ')' ] ;
 * Next steps:
 *   Write to text file:
 *      'WRITE' | 'WRITELN' [ '('
 *          file_variable ','
 *          expression [ ':' width [ ':' precision ] ]
 *          [ ',' expression [ ':' width [ ':' precision ] ] ]*
 *      ')' ] ;
 *   Write to binary file:
 *      'WRITE' '('
 *          file_variable ',' expression
 *      ')' ;
 */
bool ps_parse_write_or_writeln_or_writestr(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call_ptr,
                                           ps_read_write_mode mode)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(call_ptr != NULL);

    PARSE_BEGIN("STATEMENT", "WRITE_OR_WRITELN_OR_WRITESTR");

    uint16_t n_args = 0;
    ps_ast_node *args[PS_PARAMETERS_MAX] = {0};
    ps_ast_format formats[PS_PARAMETERS_MAX] = {0};
    bool loop = true;

    // "Write[Ln];" or "Write[Ln] Else|End|Until"?
    // (Write without parameters is "legal" but is a no-op)
    if (PS_TOKEN_NONE != ps_parser_expect_statement_end_token(compiler->parser))
    {
        // "WriteStr" only is not "legal", it needs a string variable
        if (mode == PS_READ_WRITE_MODE_STR)
            RETURN_ERROR(PS_ERROR_EXPECTED_VARIABLE)
    }
    else
    {
        // '('
        EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_LEFT_PARENTHESIS)
        READ_NEXT_TOKEN_OR_RETURN_FALSE
        // ')' =>"Write[Ln]()"?
        if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
        {
            // "WriteStr()" is not "legal"
            if (mode == PS_READ_WRITE_MODE_STR)
                RETURN_ERROR(PS_ERROR_EXPECTED_VARIABLE)
            READ_NEXT_TOKEN_OR_RETURN_FALSE
            loop = false;
        }
        // "WriteStr"?
        if (!ps_parse_writestr_string(compiler, block, mode, &n_args, args))
            TRACE_ERROR("STRING")
        // Expressions + formats
        while (loop)
        {
            // EXPRESSION [ ':' WIDTH [ ':' PRECISION ] ]
            if (!ps_parse_write_expression(compiler, block, &n_args, args, formats))
                TRACE_ERROR("PARAMETER")
            // ',' => another argument
            if (lexer->current_token.type == PS_TOKEN_COMMA)
                READ_NEXT_TOKEN_OR_RETURN_FALSE
            else
            {
                // ')' => end of arguments
                EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_RIGHT_PARENTHESIS)
                READ_NEXT_TOKEN_OR_RETURN_FALSE
                loop = false;
            }
        }
    }

    // Create AST node for this call
    ps_symbol *procedure = &ps_system_procedure_write;
    if (mode == PS_READ_WRITE_MODE_LN)
        procedure = &ps_system_procedure_writeln;
    else if (mode == PS_READ_WRITE_MODE_STR)
        procedure = &ps_system_procedure_writestr;
    *call_ptr = ps_ast_create_call(start_line, start_column, PS_AST_PROCEDURE_CALL, procedure, n_args,
                                   n_args > 0 ? args : NULL, formats);
    if (*call_ptr == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")
}

/**
 * Parse assignment or procedure call:
 *  this is determined by the symbol kind:
 *      - variable: assignment
 *      - procedure: procedure call
 */
bool ps_parse_assignment_or_procedure_call(ps_compiler *compiler, ps_ast_block *block, ps_ast_node **statement_ptr)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(statement_ptr != NULL);

    PARSE_BEGIN("STATEMENT", "ASSIGNMENT OR PROCEDURE CALL");
    (void)start_line;
    (void)start_column;

    ps_identifier identifier;
    ps_ast_block *owner = NULL;
    ps_symbol *symbol;
    ps_ast_call *call = NULL;
    ps_ast_assignment *assignement = NULL;
    bool is_result = false;
    bool symbol_found = false;

    COPY_IDENTIFIER(identifier)
    READ_NEXT_TOKEN_OR_RETURN_FALSE

    // First, check if this is an assignment to the current function name
    symbol = block->parent == NULL ? NULL : ps_symbol_table_find(block->parent->symbols, identifier);
    if (symbol != NULL && symbol->kind == PS_SYMBOL_KIND_FUNCTION && strcmp(symbol->name, block->name) == 0)
    {
        if (compiler->debug >= PS_DEBUG_VERBOSE)
            fprintf(stderr, "INFO\tAssignment to current function '%s' as Result\n", (char *)identifier);
        // Assign to the not so implicit "Result" local variable
        symbol_found = ps_compiler_find_symbol(compiler, block, "RESULT", false, &owner, &symbol);
        is_result = true;
    }
    else
    {
        // Normal lookup - can be variable, constant, procedure, or function
        symbol_found = ps_compiler_find_symbol(compiler, block, identifier, false, &owner, &symbol);
    }
    if (!symbol_found)
        RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
    if (compiler->debug >= PS_DEBUG_VERBOSE)
        ps_symbol_debug(stderr, "DEBUG\tFound symbol ", symbol);

    // Determine if this is an assignment or a procedure call based on symbol kind
    switch (symbol->kind)
    {
    case PS_SYMBOL_KIND_VARIABLE:
        // fprintf(stderr,
        //         "DEBUG\tps_parse_assignment_or_procedure_call\tParsing assignment to variable '%s' of type '%s'\n",
        //         symbol->name, ps_type_definition_get_name(symbol->value->type->value->data.t));
        if (!ps_parse_assignment(compiler, block, &assignement, owner, symbol))
            TRACE_ERROR("ASSIGNMENT")
        // fprintf(stderr,
        //         "DEBUG\tps_parse_assignment_or_procedure_call\tParsed assignment to variable '%s' of type '%s' as
        //         %p\n", symbol->name, ps_type_definition_get_name(symbol->value->type->value->data.t), (void
        //         *)(assignement));
        // ps_ast_debug_node(1, (ps_ast_node *)assignement);
        *statement_ptr = (ps_ast_node *)(assignement);
        break;
    case PS_SYMBOL_KIND_PROCEDURE:
        // Procedure call: parse arguments and create call node
        if (!ps_parse_procedure_or_function_call(compiler, block, &call, symbol))
            TRACE_ERROR("PROCEDURE_CALL")
        *statement_ptr = (ps_ast_node *)(call);
        break;
    case PS_SYMBOL_KIND_FUNCTION:
        // Assignment to current function name => assignment to Result
        if (!is_result)
        {
            ps_compiler_set_message(compiler, "Cannot assign to %s from %s", symbol->name, block->name);
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        }
        if (!ps_parse_assignment(compiler, block, &assignement, owner, symbol))
            TRACE_ERROR("ASSIGNMENT")
        *statement_ptr = (ps_ast_node *)(assignement);
        break;
    case PS_SYMBOL_KIND_CONSTANT:
        ps_compiler_set_error_message(compiler, PS_ERROR_ASSIGN_TO_CONST, "Constant '%s' cannot be assigned",
                                      symbol->name);
        TRACE_ERROR("ASSIGN_TO_CONST")
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }

    PARSE_END("OK")
}

/**
 * Parse
 *      'IF' expression 'THEN' statement [ 'ELSE' statement ]
 */
bool ps_parse_if_then_else(ps_compiler *compiler, ps_ast_block *block, ps_ast_if **if_statement)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(if_statement != NULL);

    PARSE_BEGIN("STATEMENT", "IF_THEN_ELSE")

    ps_ast_node *condition = NULL;
    ps_ast_statement_list *then_branch = NULL;
    ps_ast_node *then_node = NULL;
    ps_ast_statement_list *else_branch = NULL;
    ps_ast_node *else_node = NULL;
    const char *trace_error = NULL;

    // IF
    EXPECT_TOKEN_OR_GOTO_CLEANUP(PS_TOKEN_IF)
    READ_NEXT_TOKEN_OR_GOTO_CLEANUP

    // Condition: boolean expression
    if (!ps_parse_expression(compiler, block, &condition))
    {
        trace_error = "CONDITION";
        goto cleanup;
    }
    const ps_symbol *result_type = ps_ast_node_get_type(condition);
    if (result_type != &ps_system_boolean)
        GOTO_CLEANUP(PS_ERROR_EXPECTED_BOOLEAN);

    // THEN
    EXPECT_TOKEN_OR_GOTO_CLEANUP(PS_TOKEN_THEN)
    READ_NEXT_TOKEN_OR_GOTO_CLEANUP

    // An immediate ELSE means the then-branch is an empty statement.
    if (lexer->current_token.type == PS_TOKEN_ELSE)
    {
        then_branch = ps_ast_create_statement_list(lexer->start_line, lexer->start_column, 0);
        if (then_branch == NULL)
            GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
    }
    else
    {
        // Statement or compound statement
        if (!ps_parse_statement(compiler, block, &then_node))
        {
            trace_error = "THEN";
            goto cleanup;
        }
        if (then_node->kind == PS_AST_STATEMENT_LIST)
        {
            then_branch = (ps_ast_statement_list *)(then_node);
            then_node = NULL;
        }
        else
        {
            // 1 statement in statement list
            then_branch = ps_ast_create_statement_list(then_node->line, then_node->column, 1);
            if (then_branch == NULL)
                GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
            then_branch->statements[0] = then_node;
            then_node = NULL;
        }
    }

    // ELSE?
    if (lexer->current_token.type == PS_TOKEN_ELSE)
    {
        READ_NEXT_TOKEN_OR_GOTO_CLEANUP
        // Statement
        if (!ps_parse_statement(compiler, block, &else_node))
        {
            trace_error = "ELSE";
            goto cleanup;
        }
        if (else_node->kind == PS_AST_STATEMENT_LIST)
        {
            else_branch = (ps_ast_statement_list *)else_node;
            else_node = NULL;
        }
        else
        {
            else_branch = ps_ast_create_statement_list(else_node->line, else_node->column, 1);
            if (else_branch == NULL)
                GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
            else_branch->statements[0] = else_node;
            else_node = NULL;
        }
    }

    *if_statement = ps_ast_create_if(start_line, start_column, condition, then_branch, else_branch);
    if (*if_statement == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")

cleanup:
    ps_ast_free_node(condition);
    ps_ast_free_node(then_node);
    ps_ast_free_node((ps_ast_node *)then_branch);
    ps_ast_free_node(else_node);
    ps_ast_free_node((ps_ast_node *)else_branch);
    if (trace_error != NULL)
        TRACE_ERROR(trace_error)
    return false;
}

/**
 * Parse
 *      'REPEAT' statement_list [ ';' ] 'UNTIL' expression ;
 */
bool ps_parse_repeat_until(ps_compiler *compiler, ps_ast_block *block, ps_ast_repeat **repeat_statement)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(repeat_statement != NULL);

    PARSE_BEGIN("STATEMENT", "REPEAT_UNTIL");

    ps_ast_statement_list *body = NULL;
    ps_ast_node *condition = NULL;
    const char *trace_error = NULL;

    // REPEAT
    EXPECT_TOKEN_OR_GOTO_CLEANUP(PS_TOKEN_REPEAT)
    READ_NEXT_TOKEN_OR_GOTO_CLEANUP

    // STATEMENT LIST
    if (!ps_parse_statement_list(compiler, block, &body, PS_TOKEN_UNTIL))
    {
        trace_error = "STATEMENTS";
        goto cleanup;
    }

    // UNTIL
    EXPECT_TOKEN_OR_GOTO_CLEANUP(PS_TOKEN_UNTIL)
    READ_NEXT_TOKEN_OR_GOTO_CLEANUP

    // CONDITION
    if (!ps_parse_expression(compiler, block, &condition))
    {
        trace_error = "EXPRESSION";
        goto cleanup;
    }
    const ps_symbol *result_type = ps_ast_node_get_type(condition);
    if (result_type != &ps_system_boolean)
        GOTO_CLEANUP(PS_ERROR_EXPECTED_BOOLEAN);

    // AST NODE => REPEAT(BODY, CONDITION)
    *repeat_statement = ps_ast_create_repeat(start_line, start_column, body, condition);
    if (*repeat_statement == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")

cleanup:
    ps_ast_free_node((ps_ast_node *)body);
    ps_ast_free_node(condition);
    if (trace_error != NULL)
        TRACE_ERROR(trace_error)
    return false;
}

/**
 * Parse
 *      'WHILE' expression 'DO' statement
 */
bool ps_parse_while_do(ps_compiler *compiler, ps_ast_block *block, ps_ast_while **while_statement)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(while_statement != NULL);

    PARSE_BEGIN("STATEMENT", "WHILE_DO");

    ps_ast_node *condition = NULL;
    ps_ast_statement_list *statement_list = NULL;
    ps_ast_node *body = NULL;
    const char *trace_error = NULL;

    // WHILE
    EXPECT_TOKEN_OR_GOTO_CLEANUP(PS_TOKEN_WHILE)
    READ_NEXT_TOKEN_OR_GOTO_CLEANUP

    // CONDITION
    if (!ps_parse_expression(compiler, block, &condition))
    {
        trace_error = "EXPRESSION";
        goto cleanup;
    }
    const ps_symbol *result_type = ps_ast_node_get_type(condition);
    if (result_type != &ps_system_boolean)
        GOTO_CLEANUP(PS_ERROR_EXPECTED_BOOLEAN);

    // DO
    EXPECT_TOKEN_OR_GOTO_CLEANUP(PS_TOKEN_DO);
    READ_NEXT_TOKEN_OR_GOTO_CLEANUP

    // BODY
    if (!ps_parse_statement(compiler, block, &body))
    {
        trace_error = "STATEMENT";
        goto cleanup;
    }
    if (body->kind == PS_AST_STATEMENT_LIST)
    {
        statement_list = (ps_ast_statement_list *)body;
        body = NULL;
    }
    else
    {
        statement_list = ps_ast_create_statement_list(body->line, body->column, 1);
        if (statement_list == NULL)
            GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
        statement_list->statements[0] = body;
        body = NULL;
    }

    *while_statement = ps_ast_create_while(start_line, start_column, condition, statement_list);
    if (*while_statement == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")

cleanup:
    ps_ast_free_node(condition);
    ps_ast_free_node(body);
    ps_ast_free_node((ps_ast_node *)statement_list);
    if (trace_error != NULL)
        TRACE_ERROR(trace_error)
    return false;
}

/**
 * Parse
 *      'FOR' control_variable ':=' expression ( 'TO' | 'DOWNTO' ) expression 'DO' statement ;
 */
bool ps_parse_for_do(ps_compiler *compiler, ps_ast_block *block, ps_ast_for **for_statement)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(for_statement != NULL);

    PARSE_BEGIN("STATEMENT", "FOR_DO");

    ps_ast_block *owner = NULL;
    ps_symbol *variable = NULL;
    ps_ast_variable *variable_node = NULL;
    ps_ast_node *start = NULL;
    ps_ast_node *finish = NULL;
    bool downto = false;
    ps_ast_node *statement = NULL;
    ps_ast_statement_list *body = NULL;
    const char *trace_error = NULL;

    // FOR
    EXPECT_TOKEN_OR_GOTO_CLEANUP(PS_TOKEN_FOR)
    READ_NEXT_TOKEN_OR_GOTO_CLEANUP

    // CONTROL_VARIABLE
    //  - must be an ordinal simple variable, not an array item
    if (!ps_parse_variable_reference(compiler, block, &owner, &variable))
    {
        if (variable != NULL)
            ps_compiler_set_error_message(compiler, PS_ERROR_EXPECTED_VARIABLE, "Expected variable, got %s",
                                          ps_symbol_get_kind_name(variable->kind));
        trace_error = "EXPECTED_VARIABLE";
        goto cleanup;
    }
    if (!ps_value_is_ordinal(variable->value))
    {
        ps_compiler_set_error_message(compiler, PS_ERROR_EXPECTED_ORDINAL, "Expected ordinal variable, got %s",
                                      ps_value_get_type_name(variable->value));
        trace_error = "ORDINAL_VARIABLE";
        goto cleanup;
    }

    // :=
    EXPECT_TOKEN_OR_GOTO_CLEANUP(PS_TOKEN_ASSIGN)
    READ_NEXT_TOKEN_OR_GOTO_CLEANUP

    // START VALUE
    if (!ps_parse_expression(compiler, block, &start))
    {
        trace_error = "START";
        goto cleanup;
    }

    // TO | DOWNTO
    if (lexer->current_token.type == PS_TOKEN_TO)
        downto = false;
    else if (lexer->current_token.type == PS_TOKEN_DOWNTO)
        downto = true;
    else
        GOTO_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN_OR_GOTO_CLEANUP

    // FINISH VALUE
    if (!ps_parse_expression(compiler, block, &finish))
    {
        trace_error = "FINISH";
        goto cleanup;
    }

    // DO
    EXPECT_TOKEN_OR_GOTO_CLEANUP(PS_TOKEN_DO)
    READ_NEXT_TOKEN_OR_GOTO_CLEANUP

    if (!ps_parse_statement(compiler, block, &statement))
    {
        trace_error = "BODY";
        goto cleanup;
    }
    if (PS_AST_STATEMENT_LIST == statement->kind)
    {
        body = (ps_ast_statement_list *)statement;
        statement = NULL;
    }
    else
    {
        body = ps_ast_create_statement_list(statement->line, statement->column, 1);
        if (body == NULL)
            GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
        body->statements[0] = statement;
        statement = NULL;
    }

    variable_node = ps_ast_create_variable_simple(start_line, start_column, owner, PS_AST_LVALUE, variable);
    if (variable_node == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)

    *for_statement = ps_ast_create_for(start_line, start_column, variable_node, start, finish, downto, body);
    if (*for_statement == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")

cleanup:
    ps_ast_free_node((ps_ast_node *)variable_node);
    ps_ast_free_node(start);
    ps_ast_free_node(finish);
    ps_ast_free_node(statement);
    ps_ast_free_node((ps_ast_node *)body);
    if (trace_error != NULL)
        TRACE_ERROR(trace_error)
    return false;
}

/**
 * Parse statement sequence, stopping at "stop" token (END or UNTIL)
 *  skip/ignore ';' before end
 */
bool ps_parse_statement_list(ps_compiler *compiler, ps_ast_block *block, ps_ast_statement_list **statement_list,
                             ps_token_type stop)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(statement_list != NULL);

    PARSE_BEGIN("STATEMENT", "STATEMENT_LIST");

    // Empty block?
    if (lexer->current_token.type == stop)
    {
        *statement_list = ps_ast_create_statement_list(start_line, start_column, 0);
        if (*statement_list == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
        PARSE_END("EMPTY")
    }

    // Let's go!
    ps_ast_node *statement = NULL;
    // Up to 256 statements for now
    int count = 0;
    ps_ast_node *statements[256] = {0};
    bool loop = true;
    bool failed_to_parse_statement = false;
    do
    {
        if (!ps_parse_statement(compiler, block, &statement))
        {
            failed_to_parse_statement = true;
            goto cleanup;
        }
        count += 1;
        if (count > 255)
        {
            ps_ast_free_node(statement);
            GOTO_CLEANUP(PS_ERROR_TOO_MANY_ARGUMENTS) // should be PS_ERROR_TOO_MANY_STATEMENTS
        }
        statements[count - 1] = statement;
        // NB: semi-colon at statement list end is optional
        if (lexer->current_token.type == PS_TOKEN_SEMI_COLON)
        {
            READ_NEXT_TOKEN_OR_GOTO_CLEANUP
            if (lexer->current_token.type == stop) // NOSONAR
                loop = false;
        }
        else if (lexer->current_token.type == stop)
            loop = false;
        else
            GOTO_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN)
    } while (loop);

    // Create statement list and copy statements into it
    *statement_list = ps_ast_create_statement_list(start_line, start_column, count);
    if (*statement_list == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
    memcpy((*statement_list)->statements, statements, sizeof(ps_ast_node *) * count);

    PARSE_END("OK")

cleanup:
    for (int i = 0; i < count && i < 255; i++)
        ps_ast_free_node(statements[i]);
    if (failed_to_parse_statement)
        TRACE_ERROR("STATEMENT")
    return false;
}
