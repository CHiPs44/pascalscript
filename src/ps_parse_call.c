/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_ast.h"
#include "ps_compiler.h"
#include "ps_executable.h"
#include "ps_functions.h"
#include "ps_parse.h"
#include "ps_parse_call.h"
#include "ps_parse_expression.h"
#include "ps_procedures.h"
#include "ps_system.h"

/**
 * Parse variable reference:
 *      IDENTIFIER
 * Next steps:
 *  Array access:
 *      IDENTIFIER '[' EXPRESSION [ ',' EXPRESSION ]* ']'
 *  "Namespace" access (System.MaxInt, System.Sin, <Program>.<Variable>, <Procedure>.<Variable>, ...):
 *      IDENTIFIER '.' IDENTIFIER
 *  Pointer dereference:
 *      VARIABLE_REFERENCE '^'
 * "Nested" access (Record.Field.SubField, Array[0].Field, Pointer^.Field, ...):
 *      IDENTIFIER [ '.' IDENTIFIER ]* '.' IDENTIFIER
 */
static bool ps_parse_variable_reference(ps_compiler *compiler, ps_ast_block *block, ps_ast_block **owner,
                                        ps_symbol **variable)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(owner != NULL);
    assert(variable != NULL);

    PARSE_BEGIN("EXECUTABLE", "VARIABLE_REFERENCE")
    (void)start_line;
    (void)start_column;

    ps_identifier identifier;
    ps_symbol *symbol;

    // Re-check
    EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_IDENTIFIER)
    COPY_IDENTIFIER(identifier)
    READ_NEXT_TOKEN_OR_RETURN_FALSE

    // Non-existing symbol?
    if (!ps_compiler_find_symbol(compiler, block, identifier, false, owner, &symbol))
        RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);

    // Variable?
    if (symbol->kind != PS_SYMBOL_KIND_VARIABLE)
        RETURN_ERROR(PS_ERROR_EXPECTED_VARIABLE)

    *variable = symbol;

    PARSE_END("OK")
}

static bool ps_parse_byref_argument(ps_compiler *compiler, ps_ast_block *block, const ps_formal_parameter *parameter,
                                    ps_ast_node *args[PS_PARAMETERS_MAX], int i)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(parameter != NULL);
    assert(args != NULL);
    assert(i >= 0 && i < PS_PARAMETERS_MAX);

    PARSE_BEGIN("EXECUTABLE", "BYREF_ARGUMENT")

    ps_ast_block *owner = NULL;
    ps_symbol *variable = NULL;

    if (!ps_parse_variable_reference(compiler, block, &owner, &variable))
        TRACE_ERROR("VARIABLE");

    // Check that the variable type matches the parameter type
    const ps_type_definition *variable_type = ps_symbol_get_type_def(variable);
    const ps_type_definition *parameter_type = ps_symbol_get_type_def(parameter->type);
    if (variable_type != parameter_type)
        RETURN_ERROR(PS_ERROR_TYPE_MISMATCH)

    // Create a new symbol for the byref argument
    ps_symbol *toto = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, parameter->name, variable->value);
    args[i] = ps_ast_create_variable_simple(start_line, start_column, owner, toto);
    if (args[i] == NULL)
    {
        compiler->error = PS_ERROR_OUT_OF_MEMORY;
        TRACE_ERROR("ARGUMENT_BYREF");
    }

    // Add the argument to the current block
    if (!ps_compiler_add_symbol(compiler, block, args[i]))
    {
        args[i] = ps_symbol_free(args[i]);
        compiler->error = PS_ERROR_OUT_OF_MEMORY;
        TRACE_ERROR("ADD_BYREF");
    }

    /*
     * For by-reference parameters the argument symbol must not take
     * ownership of the variable's value; it is only an alias. Mark
     * the symbol as not allocated so we don't free the value twice.
     */
    // args[i]->allocated = false;

    PARSE_END("OK")
}

static bool ps_parse_byval_argument(ps_compiler *compiler, ps_ast_block *block, const ps_formal_parameter *parameter,
                                    ps_ast_node *args[PS_PARAMETERS_MAX], int i)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(parameter != NULL);
    assert(args != NULL);
    assert(i >= 0 && i < PS_PARAMETERS_MAX);

    PARSE_BEGIN("EXECUTABLE", "BYVAL_ARGUMENT")

    ps_value *value = NULL;
    ps_ast_node *expression = NULL;

    if (!ps_parse_expression(compiler, block, &expression))
        TRACE_ERROR("EXPRESSION");

    // Check that the expression type matches the parameter type
    const ps_type_definition *expression_type = ps_ast_node_get_type(expression);
    const ps_type_definition *parameter_type = ps_symbol_get_type_def(parameter->type);
    if (expression_type != parameter_type)
        RETURN_ERROR(PS_ERROR_TYPE_MISMATCH)

    args[i] = expression;

    value = ps_value_alloc(parameter->type, (ps_value_data){.h = i});
    if (value == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
    args[i] = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, parameter->name, value);
    if (args[i] == NULL)
    {
        compiler->error = PS_ERROR_OUT_OF_MEMORY;
        TRACE_ERROR("ARGUMENT_BYVAL");
    }

    if (!ps_compiler_add_symbol(compiler, block, args[i]))
    {
        args[i] = ps_symbol_free(args[i]);
        compiler->error = PS_ERROR_OUT_OF_MEMORY;
        TRACE_ERROR("ADD_BYVAL");
    }

    PARSE_END("OK")
}

ps_ast_block *ps_symbol_get_executable_block(ps_symbol *executable)
{
    if (executable == NULL || executable->value == NULL || executable->value->data.x == NULL ||
        executable->value->data.x->block == NULL)
        return NULL;
    return executable->value->data.x->block;
}

/**
 * Parse actual signature:
 *      '(' [ actual_parameter [ ',' actual_parameter ]* ] ')'
 *      where actual_parameter is:
 *          expression or variable_reference
 */
static bool ps_parse_actual_signature(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call,
                                      ps_symbol *executable)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(call != NULL);
    assert(executable != NULL);

    PARSE_BEGIN("EXECUTABLE", "ACTUAL_SIGNATURE")

    const ps_ast_block *executable_block = ps_symbol_get_executable_block(executable);
    const ps_formal_signature *formal_signature = executable_block->signature;
    const ps_formal_parameter *parameter = NULL;
    uint8_t parameter_count = formal_signature->parameter_count;
    uint8_t i = 0;
    ps_ast_node *args[PS_PARAMETERS_MAX] = {0};

    EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_LEFT_PARENTHESIS)

    // No parameters?
    READ_NEXT_TOKEN_OR_RETURN_FALSE
    if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
    {
        if (parameter_count != 0)
        {
            ps_compiler_set_error_message(compiler, PS_ERROR_UNEXPECTED_TOKEN,
                                          "Procedure or function %s expects %d parameter%s, got none", executable->name,
                                          parameter_count, parameter_count > 1 ? "s" : "");
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        }
        PARSE_END("NO_PARAMETERS");
    }
    if (parameter_count == 0)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)

    // Parse actual parameters
    do
    {
        parameter = &formal_signature->parameters[i];
        if (parameter->byref)
        {
            if (!ps_parse_byref_argument(compiler, block, parameter, args, i))
                TRACE_ERROR("ARGUMENT_BYREF");
        }
        else
        {
            if (!ps_parse_byval_argument(compiler, block, parameter, args, i))
                TRACE_ERROR("ARGUMENT_BYVAL");
        }
        i += 1;
        if (i >= parameter_count)
        {
            if (lexer->current_token.type != PS_TOKEN_RIGHT_PARENTHESIS)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
            break;
        }
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            READ_NEXT_TOKEN_OR_RETURN_FALSE
            continue;
        }
        if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
        {
            READ_NEXT_TOKEN_OR_RETURN_FALSE
        }
    } while (true);

    ps_ast_node_kind node_kind =
        executable->kind == PS_SYMBOL_KIND_PROCEDURE ? PS_AST_PROCEDURE_CALL : PS_AST_FUNCTION_CALL;
    *call = ps_ast_create_call(start_line, start_column, node_kind, executable, parameter_count, args, NULL);
    if (*call == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")
}

static bool ps_parse_formal_signature(ps_compiler *compiler, ps_ast_block *block, ps_formal_signature **signature)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(signature != NULL);

    PARSE_BEGIN("EXECUTABLE", "FORMAL_SIGNATURE")

    // Allocate signature
    *signature = ps_formal_signature_alloc(0, NULL);
    if (signature == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)

    // No parameter list?
    if (PS_TOKEN_LEFT_PARENTHESIS != lexer->current_token.type)
        PARSE_END("NO_PARAMETERS");
    READ_NEXT_TOKEN_OR_GOTO_CLEANUP

    // Empty parameter list? ()
    if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
    {
        READ_NEXT_TOKEN_OR_GOTO_CLEANUP
        PARSE_END("NO_PARAMETERS");
    }

    bool loop = true;
    do
    {
        if (!ps_parse_parameter_definition(compiler, block, signature))
            loop = false;
        else
            switch (lexer->current_token.type)
            {
            case PS_TOKEN_COMMA:
                // , introduces antoher parameter
                READ_NEXT_TOKEN
                loop = compiler->error == PS_ERROR_NONE;
                break;
            case PS_TOKEN_RIGHT_PARENTHESIS:
                // ) ends parameter list
                READ_NEXT_TOKEN
                loop = false;
                break;
            default:
                compiler->error = PS_ERROR_UNEXPECTED_TOKEN;
                loop = false;
            }
    } while (loop);
    if (compiler->error != PS_ERROR_NONE)
        goto cleanup;

    PARSE_END("OK")

cleanup:
    TRACE_ERROR("FORMAL_SIGNATURE")
}

bool ps_parse_procedure_or_function_call_user(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call,
                                              ps_symbol *executable)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(call != NULL);
    assert(executable != NULL);

    PARSE_BEGIN("EXECUTABLE", "PROCEDURE_OR_FUNCTION_CALL_USER")

    ps_symbol *result_symbol = NULL;
    ps_identifier result_identifier = "RESULT";
    ps_value *result_value = NULL;

    if (executable->kind != PS_SYMBOL_KIND_PROCEDURE && executable->kind != PS_SYMBOL_KIND_FUNCTION)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)

    // Parse actual parameters
    if (lexer->current_token.type == PS_TOKEN_LEFT_PARENTHESIS)
    {
        if (!ps_parse_actual_signature(compiler, block, call, executable))
            TRACE_ERROR("SIGNATURE")
        EXPECT_TOKEN_OR_GOTO_CLEANUP(PS_TOKEN_RIGHT_PARENTHESIS)
        READ_NEXT_TOKEN_OR_GOTO_CLEANUP
    }
    else
    {
        // No parameters
        const ps_ast_block *executable_block = ps_symbol_get_executable_block(executable);
        const ps_formal_signature *formal_signature = executable_block->signature;
        if (formal_signature->parameter_count != 0)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }
    if (executable->kind == PS_SYMBOL_KIND_PROCEDURE)
    {
        ps_token_type token_type = ps_parser_expect_statement_end_token(compiler->parser);
        if (token_type == PS_TOKEN_NONE)
        {
            compiler->error = PS_ERROR_UNEXPECTED_TOKEN;
            goto cleanup;
        }
    }
    else if (executable->kind == PS_SYMBOL_KIND_FUNCTION)
    {
        // Function have a return value stored in a variable named RESULT
        const ps_ast_block *executable_block = ps_symbol_get_executable_block(executable);
        const ps_formal_signature *signature = executable_block->signature;
        result_value = ps_value_alloc(signature->result_type, (ps_value_data){.h = 0});
        if (result_value == NULL)
            GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
        result_symbol = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, result_identifier, result_value);
        if (result_symbol == NULL)
            GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
        if (!ps_compiler_add_symbol(compiler, block, result_symbol))
        {
            ps_symbol_free(result_symbol);
            goto cleanup;
        }
    }

    // Build AST node for CALL
    ps_ast_node_kind executable_kind =
        executable->kind == PS_SYMBOL_KIND_PROCEDURE ? PS_AST_PROCEDURE_CALL : PS_AST_FUNCTION_CALL;
    *call = ps_ast_create_call(start_line, start_column, executable_kind, executable, 0, NULL, NULL);

    PARSE_END("OK")

cleanup:
    TRACE_ERROR("CLEANUP")
}

static bool ps_parse_randomize(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call)
{
    PARSE_BEGIN("EXECUTABLE", "RANDOMIZE")

    ps_ast_node *args[1] = {0};
    uint16_t n_args = 0;

    // Even whenb Randomize is called without argument, if can be called with parenthesis
    if (lexer->current_token.type == PS_TOKEN_LEFT_PARENTHESIS)
    {
        READ_NEXT_TOKEN_OR_RETURN_FALSE
        if (lexer->current_token.type != PS_TOKEN_RIGHT_PARENTHESIS)
        {
            if (!ps_parse_expression(compiler, block, &args[0]))
                TRACE_ERROR("EXPRESSION")
            n_args = 1;
        }
        EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_RIGHT_PARENTHESIS)
    }

    *call = ps_ast_create_call(start_line, start_column, PS_AST_PROCEDURE_CALL, &ps_system_procedure_randomize, n_args,
                               n_args == 0 ? NULL : args, NULL);

    PARSE_END("OK")
}

bool ps_parse_procedure_or_function_call(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call,
                                         ps_symbol *executable)
{
    PARSE_BEGIN("EXECUTABLE", "PROCEDURE_OR_FUNCTION_CALL")
    (void)start_line;
    (void)start_column;

    if (executable == &ps_system_procedure_write || executable == &ps_system_procedure_writeln)
    {
        // Write or WriteLn
        if (!ps_parse_write_or_writeln(compiler, block, call, executable == &ps_system_procedure_writeln))
            TRACE_ERROR("WRITE[LN]");
    }
    else if (executable == &ps_system_procedure_read || executable == &ps_system_procedure_readln)
    {
        // Read or ReadLn
        compiler->error = PS_ERROR_NOT_IMPLEMENTED;
        if (!ps_parse_read_or_readln(compiler, block, call, executable == &ps_system_procedure_readln))
            TRACE_ERROR("READ[LN]");
    }
    else if (executable == &ps_system_procedure_randomize)
    {
        // Randomize has 0 or 1 argument
        if (!ps_parse_randomize(compiler, block, call))
            TRACE_ERROR("RANDOMIZE");
    }
    else if (executable->system)
    {
        // All other system procedures and functions have 1 argument
        ps_compiler_set_message(compiler, "TODO: call %s", executable->name);
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
    }
    // User defined procedure or function call
    else if (!ps_parse_procedure_or_function_call_user(compiler, block, call, executable))
        TRACE_ERROR("USER");

    PARSE_END("OK")
}
