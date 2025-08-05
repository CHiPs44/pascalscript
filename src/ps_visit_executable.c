/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_executable.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_visit.h"

/**
 * Visit
 *      PROCEDURE IDENTIFIER ;
 * Next steps:
 *  - allow procedure block with empty body:
 *      PROCEDURE IDENTIFIER ;
 *      BEGIN
 *      END ;
 *  - allow procedure block (constants, variables, body):
 *      PROCEDURE IDENTIFIER
 *      [ CONST ... TYPE ... VAR ... ]*
 *      BEGIN
 *          COMPOUND_STATEMENT [ ; ]
 *      END ;
 *  - allow procedure parameters
 */
bool ps_visit_procedure_or_function(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol_kind kind)
{
    VISIT_BEGIN("PROCEDURE_OR_FUNCTION", "");

    ps_identifier identifier;
    ps_symbol *callable = NULL;
    ps_value *value = NULL;
    ps_executable *executable = NULL;
    uint16_t line = 0;
    uint8_t column = 0;
    bool has_environment = false;

    if (kind != PS_SYMBOL_KIND_PROCEDURE && kind != PS_SYMBOL_KIND_FUNCTION)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    if (kind == PS_SYMBOL_KIND_FUNCTION)
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);

    READ_NEXT_TOKEN;
    EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
    COPY_IDENTIFIER(identifier);
    callable = ps_interpreter_find_symbol(interpreter, &identifier, true);
    if (callable != NULL)
        RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS);

    TRACE_CURSOR;
    if (!ps_lexer_get_cursor(lexer, &line, &column))
    {
        interpreter->error = PS_ERROR_GENERIC; // TODO better error code
        goto cleanup;
    }
    READ_NEXT_TOKEN;
    // NB: no parameters nor parenthesis for now
    EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);

    if (mode == MODE_EXEC)
    {
        executable = ps_executable_alloc(NULL, ps_system_none.value->type, line, column);
        if (executable == NULL)
        {
            interpreter->error = PS_ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
        callable = ps_symbol_alloc(PS_SYMBOL_KIND_PROCEDURE, &identifier, NULL);
        if (callable == NULL)
        {
            interpreter->error = PS_ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
        callable->kind = kind;
        value = ps_value_alloc(ps_system_procedure.value->data.t, (ps_value_data){.x = executable});
        if (value == NULL)
        {
            interpreter->error = PS_ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
        callable->value = value;
        if (!ps_interpreter_add_symbol(interpreter, callable))
        {
            goto cleanup;
        }
        if (!ps_interpreter_enter_environment(interpreter, &identifier))
        {
            goto cleanup;
        }
        has_environment = true;
    }
    // Skip block
    fprintf(stderr, "================================================================================\n");
    ps_token_debug(stderr, "CURRENT", &lexer->current_token);
    ps_executable_debug(stderr, "EXECUTABLE", executable);
    READ_NEXT_TOKEN;
    fprintf(stderr, "================================================================================\n");
    if (!ps_visit_block(interpreter, MODE_SKIP))
    {
        goto cleanup;
    }
    if (mode == MODE_EXEC)
    {
        if (!ps_interpreter_exit_environment(interpreter))
        {
            has_environment = false;
            goto cleanup;
        }
    }
    EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
    READ_NEXT_TOKEN;

    VISIT_END("OK");

cleanup:
    if (has_environment)
        ps_interpreter_exit_environment(interpreter);
    if (callable != NULL)
        ps_symbol_free(callable);
    if (value != NULL)
        ps_value_free(value);
    if (executable != NULL)
        free(executable); // TODO ps_executable_free(executable);
    if (has_environment)
        ps_interpreter_exit_environment(interpreter);
    if (interpreter->error == PS_ERROR_NONE)
        interpreter->error = PS_ERROR_GENERIC;
    TRACE_ERROR("CLEANUP");
}
