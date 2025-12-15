/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_executable.h"
#include "ps_lexer.h"
#include "ps_procedures.h"
#include "ps_signature.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_type_definition.h"
#include "ps_value.h"
#include "ps_visit.h"

/**
 * Visit variable reference:
 *      IDENTIFIER
 * Next steps:
 *  Array access:
 *      IDENTIFIER '[' EXPRESSION [ ',' EXPRESSION ]* ']'
 *  "Namespace" access (System.MaxInt, System.Sin, <Program>.Variable, ...):
 *      IDENTIFIER '.' IDENTIFIER
 *  Pointer dereference:
 *      VARIABLE_REFERENCE '^'
 */
bool ps_visit_variable_reference(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol **variable)
{
    VISIT_BEGIN("VARIABLE_REFERENCE", "");
    ps_identifier identifier;
    ps_symbol *symbol;

    *variable = NULL;
    EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
    COPY_IDENTIFIER(identifier);
    if (mode == MODE_EXEC)
    {
        symbol = ps_interpreter_find_symbol(interpreter, &identifier, false);
        if (symbol == NULL)
        {
            RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
        }
        if (symbol->kind != PS_SYMBOL_KIND_VARIABLE)
        {
            RETURN_ERROR(PS_ERROR_EXPECTED_VARIABLE);
        }
        *variable = symbol;
    }
    READ_NEXT_TOKEN;

    VISIT_END("OK");
}

/**
 * Visit parameter definition:
 *      [ 'VAR' ] IDENTIFIER [ ',' IDENTIFIER ]* ':' TYPE_REFERENCE
 *
 * Add the parameter(s) to the signature and to the current environment.
 * Up to 8 parameters at once.
 */
bool ps_visit_parameter_definition(ps_interpreter *interpreter, ps_interpreter_mode mode,
                                   ps_formal_signature *signature)
{
    VISIT_BEGIN("PARAMETER_DEFINITION", "");

    ps_identifier names[8] = {0};
    int index = -1;
    ps_symbol *parameter = NULL;
    ps_symbol __attribute__((aligned(4))) *type_reference = NULL;
    bool byref;
    ps_value *value = NULL;

    // Default is "by value"
    byref = false;
    if (lexer->current_token.type == PS_TOKEN_VAR)
    {
        byref = true;
        READ_NEXT_TOKEN;
    }

    // One or more parameter names
    do
    {
        // Parameter name
        EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
        index += 1;
        if (index > 7)
            RETURN_ERROR(PS_ERROR_TOO_MANY_VARIABLES);
        COPY_IDENTIFIER(names[index]);
        // Check that the parameter name does not already exist in the other parameters
        // e.g. procedure P(a, b, a: Integer);
        for (int i = 0; i < index; i++)
        {
            if (0 == strncmp((char *)names[i], (char *)names[index], PS_IDENTIFIER_LEN))
                RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS);
        }
        // Check that the parameter name does not already exist in the signature
        // e.g. procedure P(a: Integer; a: Boolean);
        if (ps_formal_signature_find_parameter(signature, &names[index]) != NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS);
        READ_NEXT_TOKEN;
        // ',' introduces another parameter name, for example: procedure P(a, b, c: Integer);
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            READ_NEXT_TOKEN;
            continue;
        }
        // ':' introduces the parameter(s) type
        if (lexer->current_token.type == PS_TOKEN_COLON)
        {
            READ_NEXT_TOKEN;
            break;
        }
        // Anything else is an error
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    } while (true);

    // Then parameter type
    if (!ps_visit_type_reference(interpreter, mode, &type_reference))
            TRACE_ERROR("TYPE REFERENCE");

    // Add the parameters to the signature and to the current environment if executing
    for (int i = 0; i <= index; i++)
    {
        if (!ps_formal_signature_add_parameter(signature, byref, &names[i], type_reference))
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
        // if (mode == MODE_EXEC)
        // {
        value = ps_value_alloc(type_reference, (ps_value_data){.v = NULL});
        if (value == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
        parameter = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, &names[i], value);
        if (parameter == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
        if (!ps_interpreter_add_symbol(interpreter, parameter))
            TRACE_ERROR("ADD SYMBOL");
        // }
    }

    VISIT_END("OK");
}

/**
 * Visit actual signature:
 *      '(' [ actual_parameter [ ',' actual_parameter ]* ] ')'
 *      where actual_parameter is:
 *          expression or variable_reference
 */
bool ps_visit_actual_signature(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol *procedure)
{
    VISIT_BEGIN("ACTUAL_SIGNATURE", "");

    ps_formal_signature *formal_signature = procedure->value->data.x->formal_signature;
    ps_formal_parameter *parameter = NULL;
    ps_symbol *argument = NULL;
    ps_value *value = NULL;
    ps_symbol *variable = NULL;
    ps_value result;
    uint8_t parameter_count = formal_signature->parameter_count;
    uint8_t i = 0;

    EXPECT_TOKEN(PS_TOKEN_LEFT_PARENTHESIS);

    // No parameters?
    READ_NEXT_TOKEN;
    if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
    {
        if (parameter_count != 0)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        VISIT_END("NO_PARAMETERS");
    }
    if (parameter_count == 0)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);

    // Parse actual parameters
    do
    {
        parameter = &formal_signature->parameters[i];
        if (parameter->byref)
        {
            if (!ps_visit_variable_reference(interpreter, mode, &variable))
                TRACE_ERROR("VARIABLE");
            argument = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, &parameter->name, variable->value);
            if (argument == NULL)
            {
                interpreter->error = PS_ERROR_OUT_OF_MEMORY;
                TRACE_ERROR("ARGUMENT_BYREF");
            }
            if (!ps_environment_add_symbol(ps_interpreter_get_environment(interpreter), argument))
            {
                argument = ps_symbol_free(argument);
                interpreter->error = PS_ERROR_OUT_OF_MEMORY;
                TRACE_ERROR("ADD_BYREF");
            }
        }
        else
        {
            result.type = parameter->type; //&ps_system_none;
            result.data.v = NULL;
            // interpreter->debug = DEBUG_VERBOSE;
            if (!ps_visit_expression(interpreter, mode, &result))
                TRACE_ERROR("EXPRESSION");
            if (mode == MODE_EXEC)
            {
                value = ps_value_alloc(parameter->type, (ps_value_data){.v = NULL});
                if (value == NULL)
                {
                    interpreter->error = PS_ERROR_OUT_OF_MEMORY;
                    TRACE_ERROR("VALUE");
                }
                if (!ps_interpreter_copy_value(interpreter, &result, value))
                {
                    value = ps_value_free(value);
                    TRACE_ERROR("COPY");
                }
                argument = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, &parameter->name, value);
                if (argument == NULL)
                {
                    value = ps_value_free(value);
                    interpreter->error = PS_ERROR_OUT_OF_MEMORY;
                    TRACE_ERROR("ARGUMENT_BYVAL");
                }
                if (!ps_environment_add_symbol(ps_interpreter_get_environment(interpreter), argument))
                {
                    argument = ps_symbol_free(argument);
                    interpreter->error = PS_ERROR_OUT_OF_MEMORY;
                    TRACE_ERROR("ADD_BYVAL");
                }
            }
        }
        i += 1;
        if (i >= parameter_count)
        {
            if (lexer->current_token.type != PS_TOKEN_RIGHT_PARENTHESIS)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
            break;
        }
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            fprintf(stderr, " INFO\tACTUAL_SIGNATURE: ',' encountered\n");
            READ_NEXT_TOKEN;
            continue;
        }
        if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
        {
            fprintf(stderr, " INFO\tACTUAL_SIGNATURE: ')' encountered\n");
            READ_NEXT_TOKEN;
            break;
        }
    } while (true);

    VISIT_END("OK");
}

/**
 * Visit procedure declaration:
 *      PROCEDURE IDENTIFIER [ ( PARAMETER_DEFINITION [ , PARAMETER_DEFINITION ] ) ] ;
 *      [ CONST ... TYPE ... VAR ... ]*
 *      BEGIN
 *          COMPOUND_STATEMENT [ ; ]
 *      END ;
 *      PARAMETER_DEFINITION = IDENTIFIER [ ':' TYPE_REFERENCE ] [ 'VAR' ] ;
 * Done:
 *  - allow procedure parameters
 *  - allow by reference parameters
 * Next steps:
 *  - functions with return type
 *      FUNCTION IDENTIFIER [ ( PARAMETER_DEFINITION [ , PARAMETER_DEFINITION ] ) ] : TYPE_REFERENCE ;
 *      [ CONST ... TYPE ... VAR ... ]*
 *      BEGIN
 *          COMPOUND_STATEMENT [ ; ]
 *      END ;
 */
bool ps_visit_procedure_or_function(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol_kind kind)
{
    VISIT_BEGIN("PROCEDURE_OR_FUNCTION", "");

    ps_identifier identifier;
    ps_symbol *symbol = NULL;
    ps_value *value = NULL;
    ps_formal_signature *signature = NULL;
    ps_executable *executable = NULL;
    uint16_t line = 0;
    uint16_t column = 0;
    // ps_value result = {0};
    bool has_environment = false;
    ps_symbol *result_symbol = NULL;
    ps_value *result_value = NULL;

    if (kind != PS_SYMBOL_KIND_PROCEDURE && kind != PS_SYMBOL_KIND_FUNCTION)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);

    // Get procedure or function name
    READ_NEXT_TOKEN_OR_CLEANUP;
    EXPECT_TOKEN_OR_CLEANUP(PS_TOKEN_IDENTIFIER);
    // Does it already exist in the current environment?
    COPY_IDENTIFIER(identifier);
    symbol = ps_interpreter_find_symbol(interpreter, &identifier, true);
    if (symbol != NULL)
        RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS);
    // Create new environment for the procedure/function
    if (!ps_interpreter_enter_environment(interpreter, &identifier))
    {
        goto cleanup;
    }
    has_environment = true;
    READ_NEXT_TOKEN;

    // Initialize signature
    signature = ps_formal_signature_alloc(0, &ps_system_none);
    if (signature == NULL)
    {
        interpreter->error = PS_ERROR_OUT_OF_MEMORY;
        goto cleanup;
    }

    // Parameters?
    if (PS_TOKEN_LEFT_PARENTHESIS == lexer->current_token.type)
    {
        READ_NEXT_TOKEN_OR_CLEANUP;
        // Empty parameter list?
        if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
        {
            READ_NEXT_TOKEN_OR_CLEANUP;
        }
        else
        {
            // interpreter->debug = DEBUG_TRACE;
            do
            {
                if (!ps_visit_parameter_definition(interpreter, mode, signature))
                    goto cleanup;

                // `,` introduces another parameter
                if (lexer->current_token.type == PS_TOKEN_COMMA)
                {
                    READ_NEXT_TOKEN_OR_CLEANUP;
                    continue;
                }
                // `)` ends the parameter list
                if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
                {
                    READ_NEXT_TOKEN_OR_CLEANUP;
                    break;
                }
                RETURN_ERROR_OR_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN);
            } while (true);
        }
    }
    // ps_formal_signature_debug(stderr, "SIGNATURE", signature);

    if (kind == PS_SYMBOL_KIND_FUNCTION)
    {
        // Function must have a return type
        EXPECT_TOKEN_OR_CLEANUP(PS_TOKEN_COLON);
        READ_NEXT_TOKEN_OR_CLEANUP;
        ps_symbol *type_reference = NULL;
        if (!ps_visit_type_reference(interpreter, mode, &type_reference))
            goto cleanup;
        signature->result_type = type_reference;
    }

    if (!ps_lexer_get_cursor(lexer, &line, &column))
    {
        interpreter->error = PS_ERROR_GENERIC; // TODO better error code
        goto cleanup;
    }
    EXPECT_TOKEN_OR_CLEANUP(PS_TOKEN_SEMI_COLON);

    executable = ps_executable_alloc(signature, line, column);
    if (executable == NULL)
    {
        interpreter->error = PS_ERROR_OUT_OF_MEMORY;
        goto cleanup;
    }
    if (interpreter->debug >= DEBUG_VERBOSE)
    {
        fprintf(stderr, "================================================================================\n");
        ps_executable_debug(stderr, "EXECUTABLE", executable);
        ps_formal_signature_debug(stderr, "SIGNATURE", signature);
        fprintf(stderr, "================================================================================\n");
    }

    symbol = ps_symbol_alloc(PS_SYMBOL_KIND_PROCEDURE, &identifier, NULL);
    if (symbol == NULL)
    {
        interpreter->error = PS_ERROR_OUT_OF_MEMORY;
        goto cleanup;
    }
    symbol->kind = kind;
    value = ps_value_alloc(&ps_system_procedure, (ps_value_data){.x = executable});
    if (value == NULL)
    {
        interpreter->error = PS_ERROR_OUT_OF_MEMORY;
        goto cleanup;
    }
    symbol->value = value;
    // Add the procedure/function to the parent environment
    ps_environment *parent_environment = ps_interpreter_get_environment(interpreter)->parent;
    // if (!ps_interpreter_add_symbol(interpreter, callable))
    if (parent_environment == NULL || !ps_environment_add_symbol(parent_environment, symbol))
    {
        goto cleanup;
    }
    // Function have a return value
    if (kind == PS_SYMBOL_KIND_FUNCTION)
    {
        result_value = ps_value_alloc(signature->result_type, (ps_value_data){.v = NULL});
        if (result_value == NULL)
        {
            interpreter->error = PS_ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
        result_value->type = signature->result_type;
        result_value->data.v = NULL;
        result_value->allocated = true;
        result_symbol = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, (ps_identifier *)"RESULT", result_value);
        if (result_symbol == NULL)
        {
            interpreter->error = PS_ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
        if (!ps_environment_add_symbol(ps_interpreter_get_environment(interpreter), result_symbol))
        {
            result_symbol = ps_symbol_free(result_symbol);
            interpreter->error = PS_ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
    }

    // Skip block
    READ_NEXT_TOKEN;
    if (!ps_visit_block(interpreter, MODE_SKIP))
    {
        goto cleanup;
    }
    EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
    READ_NEXT_TOKEN;

cleanup:
    if (has_environment)
        ps_interpreter_exit_environment(interpreter);
    if (symbol != NULL)
    {
        symbol = ps_symbol_free(symbol);
        value = NULL;
    }
    if (value != NULL)
    {
        value = ps_value_free(value);
    }
    if (executable != NULL)
        executable = ps_executable_free(executable);
    if (interpreter->error != PS_ERROR_NONE)
        TRACE_ERROR("CLEANUP");
    VISIT_END("OK");
}

/**
 * Visit procedure or function call:
 *    IDENTIFIER [ '(' actual_parameter [ ',' actual_parameter ]* ')' ]
 *    where actual_parameter is:
 *      expression or variable_reference
 */
bool ps_visit_procedure_or_function_call(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol *executable,
                                         ps_value *result_value)
{
    VISIT_BEGIN("PROCEDURE_OR_FUNCTION_CALL", "");

    uint16_t line = 0;
    uint16_t column = 0;
    bool has_environment = false;
    ps_symbol *result_symbol = NULL;

    if (executable == &ps_system_procedure_write || executable == &ps_system_procedure_writeln)
    {
        // Write or WriteLn
        if (!ps_visit_write_or_writeln(interpreter, mode, executable == &ps_system_procedure_writeln))
            TRACE_ERROR("WRITE[LN]");
    }
    else if (executable == &ps_system_procedure_read || executable == &ps_system_procedure_readln)
    {
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        TRACE_ERROR("READ[LN] NOT IMPLEMENTED");
        // Read or ReadLn
        // if (!ps_visit_read_or_readln(interpreter, mode, executable == &ps_system_procedure_readln))
        //     TRACE_ERROR("READ[LN]");
    }
    else if (executable == &ps_system_procedure_randomize)
    {
        // Randomize
        if (mode == MODE_EXEC)
            if (!ps_procedure_randomize(interpreter, NULL, NULL))
                TRACE_ERROR("RANDOMIZE");
    }
    else
    {
        // User defined procedure call
        // Enter environment for procedure or function
        if (!ps_interpreter_enter_environment(interpreter, &executable->name))
            TRACE_ERROR("ENTER_ENVIRONMENT");
        has_environment = true;
        // Parse actual parameters
        if (lexer->current_token.type == PS_TOKEN_LEFT_PARENTHESIS)
        {
            if (!ps_visit_actual_signature(interpreter, mode, executable))
                TRACE_ERROR("SIGNATURE");
            EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
            SAVE_CURSOR(line, column);
            READ_NEXT_TOKEN;
        }
        else
        {
            // No parameters
            ps_formal_signature *formal_signature = executable->value->data.x->formal_signature;
            if (formal_signature->parameter_count != 0)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
            SAVE_CURSOR(line, column);
        }
        if (executable->kind == PS_SYMBOL_KIND_PROCEDURE)
        {
            ps_token_type token_type = ps_parser_expect_statement_end_token(interpreter->parser);
            if (token_type == PS_TOKEN_NONE)
            {
                interpreter->error = PS_ERROR_UNEXPECTED_TOKEN;
                goto cleanup;
            }
        }
        else if (executable->kind == PS_SYMBOL_KIND_FUNCTION)
        {
            // Function have a return value
            result_value->type = executable->value->data.x->formal_signature->result_type;
            result_value->data.v = NULL;
            result_value->allocated = false;
            result_symbol = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, (ps_identifier *)"RESULT", result_value);
            if (result_symbol == NULL)
            {
                interpreter->error = PS_ERROR_OUT_OF_MEMORY;
                goto cleanup;
            }
            if (!ps_environment_add_symbol(ps_interpreter_get_environment(interpreter), result_symbol))
            {
                result_symbol = ps_symbol_free(result_symbol);
                interpreter->error = PS_ERROR_OUT_OF_MEMORY;
                goto cleanup;
            }
        }
        // Execute procedure or function
        if (mode == MODE_EXEC)
        {
            if (!ps_lexer_set_cursor(lexer, executable->value->data.x->line, executable->value->data.x->column))
                RETURN_ERROR_OR_CLEANUP(PS_ERROR_GENERIC); // TODO better error code
            READ_NEXT_TOKEN;
            // Run procedure body
            if (!ps_visit_block(interpreter, mode))
                goto cleanup;
            // Restore cursor position
            if (!ps_lexer_set_cursor(lexer, line, column))
                RETURN_ERROR_OR_CLEANUP(PS_ERROR_GENERIC); // TODO better error code
            // READ_NEXT_TOKEN;
        }
    }

cleanup:
    if (has_environment)
    {
        // Empty byref parameters to avoid freeing values still used in the caller environment
        for (uint8_t i = 0; i < executable->value->data.x->formal_signature->parameter_count; i++)
        {
            ps_formal_parameter *parameter = &executable->value->data.x->formal_signature->parameters[i];
            if (parameter->byref)
            {
                ps_symbol *symbol =
                    ps_environment_find_symbol(ps_interpreter_get_environment(interpreter), &parameter->name, true);
                if (symbol != NULL)
                    symbol->value = NULL;
            }
        }
        ps_interpreter_exit_environment(interpreter);
    }
    if (interpreter->error != PS_ERROR_NONE)
        TRACE_ERROR("CLEANUP");
    VISIT_END("OK");
}
