/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_executable.h"
#include "ps_lexer.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_visit.h"

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
    bool by_reference;
    ps_value *value = NULL;

    // Default is "by value"
    by_reference = false;
    if (lexer->current_token.type == PS_TOKEN_VAR)
    {
        by_reference = true;
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
        // ',' introduces another parameter name
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
        RETURN_ERROR(interpreter->error);

    // Add the parameters to the signature and to the current environment if executing
    for (int i = 0; i <= index; i++)
    {
        if (!ps_formal_signature_add_parameter(signature, by_reference, &names[i], type_reference))
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
            RETURN_ERROR(interpreter->error);
        // }
    }

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
    ps_symbol *callable = NULL;
    ps_value *value = NULL;
    ps_formal_signature *signature = NULL;
    ps_executable *executable = NULL;
    uint16_t line = 0;
    uint16_t column = 0;
    // ps_value result = {0};
    bool has_environment = false;

    if (kind != PS_SYMBOL_KIND_PROCEDURE && kind != PS_SYMBOL_KIND_FUNCTION)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    // For now, only procedures
    if (kind == PS_SYMBOL_KIND_FUNCTION)
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);

    // Get procedure/function name
    READ_NEXT_TOKEN_OR_CLEANUP;
    EXPECT_TOKEN_OR_CLEANUP(PS_TOKEN_IDENTIFIER);
    // Does it already exist in the current environment?
    COPY_IDENTIFIER(identifier);
    callable = ps_interpreter_find_symbol(interpreter, &identifier, true);
    if (callable != NULL)
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
    // ps_interpreter_debug debug = interpreter->debug;
    // interpreter->debug = DEBUG_TRACE;
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
    // interpreter->debug = debug;
    // ps_formal_signature_debug(stderr, "SIGNATURE", signature);

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
    // fprintf(stderr, "================================================================================\n");
    // ps_executable_debug(stderr, "EXECUTABLE", executable);
    // fprintf(stderr, "================================================================================\n");
    callable = ps_symbol_alloc(PS_SYMBOL_KIND_PROCEDURE, &identifier, NULL);
    if (callable == NULL)
    {
        interpreter->error = PS_ERROR_OUT_OF_MEMORY;
        goto cleanup;
    }
    callable->kind = kind;
    value = ps_value_alloc(&ps_system_procedure, (ps_value_data){.x = executable});
    if (value == NULL)
    {
        interpreter->error = PS_ERROR_OUT_OF_MEMORY;
        goto cleanup;
    }
    callable->value = value;
    // Add the procedure/function to the parent environment
    ps_environment *parent_environment = ps_interpreter_get_environment(interpreter)->parent;
    // if (!ps_interpreter_add_symbol(interpreter, callable))
    if (parent_environment == NULL || !ps_environment_add_symbol(parent_environment, callable))
    {
        goto cleanup;
    }
    // Skip block
    READ_NEXT_TOKEN;
    if (!ps_visit_block(interpreter, MODE_SKIP))
    {
        goto cleanup;
    }
    if (!ps_interpreter_exit_environment(interpreter))
    {
        has_environment = false;
        goto cleanup;
    }
    EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
    READ_NEXT_TOKEN;

    VISIT_END("OK");

cleanup:
    if (has_environment)
        ps_interpreter_exit_environment(interpreter);
    if (callable != NULL)
    {
        callable = ps_symbol_free(callable);
        value = NULL;
    }
    if (value != NULL)
    {
        value = ps_value_free(value);
    }
    if (executable != NULL)
        free(executable); // TODO ps_executable_free(executable);
    if (has_environment)
        ps_interpreter_exit_environment(interpreter);
    if (interpreter->error == PS_ERROR_NONE)
        interpreter->error = PS_ERROR_GENERIC;
    TRACE_ERROR("CLEANUP");
}
