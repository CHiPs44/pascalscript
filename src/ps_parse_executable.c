/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_ast.h"
#include "ps_compiler.h"
#include "ps_executable.h"
#include "ps_functions.h"
#include "ps_lexer.h"
#include "ps_parse.h"
#include "ps_parse_declaration.h"
#include "ps_parse_executable.h"
#include "ps_parse_expression.h"
#include "ps_parse_statement.h"
#include "ps_parse_type.h"
#include "ps_procedures.h"
#include "ps_signature.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_type_definition.h"
#include "ps_value.h"

/**
 * Visit variable reference:
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
bool ps_parse_variable_reference(ps_compiler *compiler, ps_ast_block *block, ps_symbol **variable)
{
    PARSE_BEGIN("EXECUTABLE", "VARIABLE_REFERENCE")
    (void)start_line;
    (void)start_column;

    ps_identifier identifier;
    ps_symbol *symbol;

    // Re-check
    EXPECT_TOKEN(PS_TOKEN_IDENTIFIER)
    COPY_IDENTIFIER(identifier)
    READ_NEXT_TOKEN

    // Existing symbol?
    symbol = ps_compiler_find_symbol(compiler, block, identifier, false);
    if (symbol == NULL)
        RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);

    // Variable?
    if (symbol->kind != PS_SYMBOL_KIND_VARIABLE)
        RETURN_ERROR(PS_ERROR_EXPECTED_VARIABLE)

    *variable = symbol;

    PARSE_END("OK")
}

/**
 * Visit parameter definition:
 *      [ 'VAR' ] IDENTIFIER [ ',' IDENTIFIER ]* ':' TYPE_REFERENCE
 *
 * Add the parameter(s) to the signature
 * Up to 8 parameters at once.
 */
bool ps_parse_parameter_definition(ps_compiler *compiler, ps_ast_block *block, ps_formal_signature *signature)
{
    PARSE_BEGIN("EXECUTABLE", "PARAMETER_DEFINITION");
    (void)start_line;
    (void)start_column;

    ps_identifier names[8] = {0};
    int index = 0;
    ps_symbol *type_reference = NULL;
    bool byref = false;

    // Default is "by value"
    if (lexer->current_token.type == PS_TOKEN_VAR || lexer->current_token.type == PS_TOKEN_OUT)
    {
        byref = true;
        READ_NEXT_TOKEN
    }

    // One or more parameter names
    do
    {
        // Parameter name
        EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
        if (index == 8)
            RETURN_ERROR(PS_ERROR_TOO_MANY_VARIABLES);
        COPY_IDENTIFIER(names[index])
        index += 1;
        // Check that the parameter name does not already exist in the other parameters
        // e.g. procedure P(a, b, a: Integer);
        for (int i = 0; i < index; i++)
        {
            if (0 == strncmp((char *)names[i], (char *)names[index], PS_IDENTIFIER_LEN))
                RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS);
        }
        // Check that the parameter name does not already exist in the signature
        // e.g. procedure P(a: Integer; a: Boolean);
        if (ps_formal_signature_find_parameter(signature, names[index]) != NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS);
        READ_NEXT_TOKEN
        // ',' introduces another parameter name, for example: procedure P(a, b, c: Integer);
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            READ_NEXT_TOKEN
            continue;
        }
        // ':' introduces the parameter(s) type
        if (lexer->current_token.type == PS_TOKEN_COLON)
        {
            READ_NEXT_TOKEN
            break;
        }
        // Anything else is an error
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    } while (true);
    // Then parameter type
    if (!ps_parse_type_reference(compiler, block, &type_reference, NULL))
        TRACE_ERROR("TYPE REFERENCE");
    // Add the parameters to the signature and to the current environment
    for (int i = 0; i <= index; i++)
    {
        if (!ps_formal_signature_add_parameter(signature, byref, names[i], type_reference))
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
        if (!ps_compiler_add_variable(compiler, block, names[i], type_reference))
            TRACE_ERROR("ADD SYMBOL");
    }

    PARSE_END("OK")
}

/**
 * Visit actual signature:
 *      '(' [ actual_parameter [ ',' actual_parameter ]* ] ')'
 *      where actual_parameter is:
 *          expression or variable_reference
 */
bool ps_parse_actual_signature(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call, ps_symbol *executable)
{
    PARSE_BEGIN("ACTUAL_SIGNATURE", "");

    ps_formal_signature *formal_signature = executable->value->data.x->block->signature;
    ps_formal_parameter *parameter = NULL;
    ps_symbol *argument = NULL;
    ps_value *value = NULL;
    ps_symbol *variable = NULL;
    uint8_t parameter_count = formal_signature->parameter_count;
    uint8_t i = 0;
    ps_ast_node *args[16] = {0};

    EXPECT_TOKEN(PS_TOKEN_LEFT_PARENTHESIS);

    // No parameters?
    READ_NEXT_TOKEN
    if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
    {
        if (parameter_count != 0)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
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
            if (!ps_parse_variable_reference(compiler, block, &variable))
                TRACE_ERROR("VARIABLE");
            argument = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, parameter->name, variable->value);
            if (argument == NULL)
            {
                compiler->error = PS_ERROR_OUT_OF_MEMORY;
                TRACE_ERROR("ARGUMENT_BYREF");
            }
            /*
             * For by-reference parameters the argument symbol must not take
             * ownership of the variable's value; it is only an alias. Mark
             * the symbol as not allocated so we don't free the value twice.
             */
            if (argument != NULL)
                argument->allocated = false;
            if (!ps_compiler_add_symbol(compiler, block, argument))
            {
                ps_symbol_free(argument);
                compiler->error = PS_ERROR_OUT_OF_MEMORY;
                TRACE_ERROR("ADD_BYREF");
            }
        }
        else
        {
            // result.type = parameter->type;
            // result.data.v = NULL;
            if (!ps_parse_expression(compiler, block, &args[i]))
                TRACE_ERROR("EXPRESSION");
            argument = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, parameter->name, value);
            if (argument == NULL)
            {
                compiler->error = PS_ERROR_OUT_OF_MEMORY;
                TRACE_ERROR("ARGUMENT_BYVAL");
            }
            if (!ps_compiler_add_symbol(compiler, block, argument))
            {
                ps_symbol_free(argument);
                compiler->error = PS_ERROR_OUT_OF_MEMORY;
                TRACE_ERROR("ADD_BYVAL");
            }
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
            READ_NEXT_TOKEN
            continue;
        }
        if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
        {
            READ_NEXT_TOKEN
        }
    } while (true);

    ps_ast_node_kind node_kind = executable->kind == PS_SYMBOL_KIND_PROCEDURE ? PS_AST_PROCEDURE : PS_AST_FUNCTION;
    *call = ps_ast_create_call(start_line, start_column, node_kind, executable, parameter_count, args, NULL, NULL);
    if (*call == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")
}

/**
 * Visit procedure or functions (with return type) declaration:
 *      PROCEDURE IDENTIFIER [ '(' PARAMETER_DEFINITION [ ',' PARAMETER_DEFINITION ] ')' ] ';'
 *      FUNCTION IDENTIFIER [ '(' PARAMETER_DEFINITION [ ',' PARAMETER_DEFINITION ] ')' ] ':' TYPE_REFERENCE ';'
 *      [ CONST ... TYPE ... VAR ... ]*
 *      'BEGIN'
 *          COMPOUND_STATEMENT [ ';' ]
 *      'END' ';'
 *      PARAMETER_DEFINITION =  [ 'VAR' ] IDENTIFIER ':' TYPE_REFERENCE
 */
bool ps_parse_procedure_or_function_declaration(ps_compiler *compiler, ps_ast_block *block,
                                                ps_ast_block **block_executable, ps_symbol_kind kind)
{
    PARSE_BEGIN("EXECUTABLE", "PROCEDURE_OR_FUNCTION");

    ps_identifier identifier = {0};
    ps_symbol *executable_symbol = NULL;
    ps_value *value = NULL;
    ps_formal_signature *signature = NULL;
    ps_executable *executable = NULL;
    ps_symbol *result_symbol = NULL;
    ps_value *result_value = NULL;
    bool result_symbol_added = false;
    bool executable_symbol_added = false;
    ps_identifier result_identifier = "RESULT";

    *block_executable = NULL;

    // 'PROCEDURE' or 'FUNCTION'
    if (kind != PS_SYMBOL_KIND_PROCEDURE && kind != PS_SYMBOL_KIND_FUNCTION)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)

    // Get procedure or function name
    READ_NEXT_TOKEN_OR_CLEANUP;
    EXPECT_TOKEN_OR_CLEANUP(PS_TOKEN_IDENTIFIER);
    // Does it already exist in the current block?
    COPY_IDENTIFIER(identifier)
    executable_symbol = ps_compiler_find_symbol(compiler, block, identifier, true);
    if (executable_symbol != NULL)
        RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS);

    // Create a new block for the procedure or function, and set its parent to the current block
    ps_ast_node_kind node_kind = kind == PS_SYMBOL_KIND_PROCEDURE ? PS_AST_PROCEDURE : PS_AST_FUNCTION;
    *block_executable = ps_ast_create_block(start_line, start_column, block, node_kind, identifier);
    if (*block_executable == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
    READ_NEXT_TOKEN

    // Initialize signature
    signature = ps_formal_signature_alloc(0, &ps_system_none);
    if (signature == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)

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
            do
            {
                if (!ps_parse_parameter_definition(compiler, block, signature))
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
                GOTO_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN)
            } while (true);
        }
    }

    // ':' TYPE_REFERENCE for functions
    if (kind == PS_SYMBOL_KIND_FUNCTION)
    {
        // Function must have a return type
        EXPECT_TOKEN_OR_CLEANUP(PS_TOKEN_COLON);
        READ_NEXT_TOKEN_OR_CLEANUP;
        ps_symbol **type_reference = NULL;
        if (!ps_parse_type_reference(compiler, block, type_reference, NULL))
            goto cleanup;
        signature->result_type = *type_reference;
    }

    // ';' after procedure or function declaration
    EXPECT_TOKEN_OR_CLEANUP(PS_TOKEN_SEMI_COLON);

    // Create executable and symbol for the procedure/function
    ps_executable_kind executable_kind =
        kind == PS_SYMBOL_KIND_PROCEDURE ? PS_EXECUTABLE_PROC_USER : PS_EXECUTABLE_FUNC_USER;
    executable = ps_executable_alloc(executable_kind, *block_executable);
    if (executable == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)

    if (compiler->debug >= PS_DEBUG_VERBOSE)
    {
        fprintf(stderr, "================================================================================\n");
        ps_executable_debug(stderr, "EXECUTABLE", executable);
        ps_formal_signature_dump(stderr, "SIGNATURE", signature);
        fprintf(stderr, "================================================================================\n");
    }

    executable_symbol = ps_symbol_alloc(kind, identifier, NULL);
    if (executable_symbol == NULL)
    {
        compiler->error = PS_ERROR_OUT_OF_MEMORY;
        goto cleanup;
    }
    value = ps_value_alloc(kind == PS_SYMBOL_KIND_PROCEDURE ? &ps_system_procedure : &ps_system_function,
                           (ps_value_data){.x = executable});
    if (value == NULL)
    {
        compiler->error = PS_ERROR_OUT_OF_MEMORY;
        goto cleanup;
    }
    executable_symbol->value = value;
    // Add the procedure/function to the parent environment
    // ps_environment *parent_environment = ps_compiler_get_environment(compiler)->parent;
    // if (parent_environment == NULL || !ps_environment_add_symbol(parent_environment, executable_symbol))
    // {
    //     goto cleanup;
    // }
    /* Ownership of 'value' is transferred to the environment via the symbol */
    value = NULL;
    executable_symbol_added = true;
    // Function have a return value
    if (kind == PS_SYMBOL_KIND_FUNCTION)
    {
        result_value = ps_value_alloc(signature->result_type, (ps_value_data){.v = NULL});
        if (result_value == NULL)
        {
            compiler->error = PS_ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
        result_value->type = signature->result_type;
        result_value->data.v = NULL;
        result_value->allocated = true;
        result_symbol = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, result_identifier, result_value);
        if (result_symbol == NULL)
        {
            compiler->error = PS_ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
        if (!ps_compiler_add_symbol(compiler, block, result_symbol))
        {
            result_symbol = ps_symbol_free(result_symbol);
            result_value = NULL;
            compiler->error = PS_ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
        result_symbol_added = true;
    }

    READ_NEXT_TOKEN
    if (!ps_parse_block(compiler, block))
    {
        goto cleanup;
    }
    EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
    READ_NEXT_TOKEN

cleanup:
    if (compiler->debug >= PS_DEBUG_VERBOSE)
        fprintf(stderr, "INFO\tPROCEDURE_OR_FUNCTION: CLEANUP\n");
    // if (has_environment)
    //     ps_compiler_exit_environment(compiler);
    if (compiler->debug >= PS_DEBUG_VERBOSE)
        fprintf(stderr, "DEBUG\texecutable_symbol: %p%s\n", (void *)executable_symbol,
                executable_symbol_added ? " (added)" : " (not added)");
    if (executable_symbol != NULL && !executable_symbol_added)
    {
        if (compiler->debug >= PS_DEBUG_VERBOSE)
            fprintf(stderr, "DEBUG\tfreeing executable_symbol\n");
        ps_symbol_free(executable_symbol);
        value = NULL;
    }
    if (compiler->debug >= PS_DEBUG_VERBOSE)
        fprintf(stderr, "DEBUG\tresult_symbol: %p%s\n", (void *)result_symbol,
                result_symbol_added ? " (added)" : " (not added)");
    if (result_symbol != NULL && !result_symbol_added)
    {
        if (compiler->debug >= PS_DEBUG_VERBOSE)
            fprintf(stderr, "DEBUG\tfreeing result_symbol\n");
        ps_symbol_free(result_symbol);
        result_value = NULL;
    }
    if (value != NULL)
        ps_value_free(value);
    if (compiler->error != PS_ERROR_NONE)
    {
        if (executable != NULL)
            ps_executable_free(executable);
        TRACE_ERROR("CLEANUP");
    }
    PARSE_END("OK")
}

bool ps_parse_procedure_or_function_call_user(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call,
                                              ps_symbol *executable)
{
    PARSE_BEGIN("PROCEDURE_OR_FUNCTION_CALL", "")
    (void)start_line;
    (void)start_column;

    uint16_t line = 0;
    uint16_t column = 0;
    ps_symbol *result_symbol = NULL;
    ps_identifier result_identifier = "RESULT";
    ps_value *result_value = NULL;

    if (executable->kind != PS_SYMBOL_KIND_PROCEDURE && executable->kind != PS_SYMBOL_KIND_FUNCTION)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)

    // Enter environment for procedure or function
    // has_environment = ps_compiler_enter_environment(compiler, executable->name);
    // if (!has_environment)
    //     TRACE_ERROR("ENTER_ENVIRONMENT")
    // Parse actual parameters
    if (lexer->current_token.type == PS_TOKEN_LEFT_PARENTHESIS)
    {
        if (!ps_parse_actual_signature(compiler, block, call, executable))
            TRACE_ERROR("SIGNATURE")
        EXPECT_TOKEN_OR_CLEANUP(PS_TOKEN_RIGHT_PARENTHESIS)
        // SAVE_CURSOR_OR_CLEANUP(line, column)
        READ_NEXT_TOKEN_OR_CLEANUP
    }
    else
    {
        // No parameters
        const ps_formal_signature *formal_signature = executable->value->data.x->block->signature;
        if (formal_signature->parameter_count != 0)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        // SAVE_CURSOR(line, column)
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
        // Function have a return value
        result_value =
            ps_value_alloc(executable->value->data.x->block->signature->result_type, (ps_value_data){.h = 0});
        if (result_value == NULL)
            GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
        result_symbol = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, result_identifier, result_value);
        if (result_symbol == NULL)
            GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
        if (!ps_compiler_add_symbol(compiler, block, result_symbol))
        {
            ps_symbol_free(result_symbol);
            compiler->error = PS_ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
    }

    // TODO build AST node for CALL
    *call = ps_ast_create_call(line, column,
                               executable->kind == PS_SYMBOL_KIND_PROCEDURE ? PS_AST_PROCEDURE : PS_AST_FUNCTION,
                               executable, 0, NULL, NULL, NULL);

cleanup:
    if (compiler->error != PS_ERROR_NONE)
        TRACE_ERROR("CLEANUP");
    PARSE_END("OK")
}

/**
 * Visit procedure or function call, be it system or user defined:
 *    IDENTIFIER [ '(' actual_parameter [ ',' actual_parameter ]* ')' ]
 *    where actual_parameter is:
 *      expression or variable_reference
 */
bool ps_parse_procedure_or_function_call(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call,
                                         ps_symbol *executable)
{
    PARSE_BEGIN("PROCEDURE_OR_FUNCTION_CALL", "")
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
        compiler->error = PS_ERROR_NOT_IMPLEMENTED;
        if (!ps_parse_read_or_readln(compiler, block, call, executable == &ps_system_procedure_readln))
            TRACE_ERROR("READ[LN]");
    }
    else if (executable == &ps_system_procedure_randomize)
    {
        // Randomize
        ps_compiler_set_message(compiler, "TODO: call RANDOMIZE", executable->name);
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
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
