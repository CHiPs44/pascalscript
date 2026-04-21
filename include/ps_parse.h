/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_PARSE_H
#define _PS_PARSE_H

#include <stdint.h>

#include "ps_compiler.h"
#include "ps_lexer.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /* src/ps_parse.c */
    bool ps_parse_start(ps_compiler *compiler);

    /* src/ps_parse_declaration.c */
    bool ps_parse_program(ps_compiler *compiler);
    bool ps_parse_uses(ps_compiler *compiler);
    bool ps_parse_const(ps_compiler *compiler);
    bool ps_parse_type(ps_compiler *compiler);
    bool ps_parse_var(ps_compiler *compiler);
    bool ps_parse_block(ps_compiler *compiler);

    /* src/ps_parse_executable.c */
    bool ps_parse_procedure_or_function_declaration(ps_compiler *compiler, ps_symbol_kind kind);
    bool ps_parse_procedure_or_function_call(ps_compiler *compiler, ps_symbol *executable, ps_value *result);
    bool ps_parse_variable_reference(ps_compiler *compiler, ps_symbol **variable);

    /* src/ps_parse_expression.c */
    bool ps_parse_and_expression(ps_compiler *compiler, ps_value *result);
    bool ps_parse_expression(ps_compiler *compiler, ps_value *result);
    bool ps_parse_factor(ps_compiler *compiler, ps_value *result);
    bool ps_parse_function_call(ps_compiler *compiler, ps_symbol *function, ps_value *result);
    bool ps_parse_or_expression(ps_compiler *compiler, ps_value *result);
    bool ps_parse_relational_expression(ps_compiler *compiler, ps_value *result);
    bool ps_parse_simple_expression(ps_compiler *compiler, ps_value *result);
    bool ps_parse_term(ps_compiler *compiler, ps_value *result);
    bool ps_parse_constant_expression(ps_compiler *compiler, ps_value *constant);

    /* src/ps_parse_statement.c */
    bool ps_parse_assignment_or_procedure_call(ps_compiler *compiler);
    bool ps_parse_assignment(ps_compiler *compiler, ps_symbol *variable);
    bool ps_parse_compound_statement(ps_compiler *compiler);
    bool ps_parse_for_do(ps_compiler *compiler);
    bool ps_parse_if_then_else(ps_compiler *compiler);
    bool ps_parse_read_or_readln(ps_compiler *compiler, bool newline);
    bool ps_parse_repeat_until(ps_compiler *compiler);
    bool ps_parse_statement_list(ps_compiler *compiler, ps_token_type stop);
    bool ps_parse_statement_or_compound_statement(ps_compiler *compiler);
    bool ps_parse_statement(ps_compiler *compiler);
    bool ps_parse_while_do(ps_compiler *compiler);
    bool ps_parse_write_or_writeln(ps_compiler *compiler, bool newline);

    /* src/ps_parse_type.c */
    bool ps_parse_type_definition(ps_compiler *compiler);
    bool ps_parse_type_reference(ps_compiler *compiler, ps_symbol **type_symbol, const char *type_name);
    bool ps_parse_type_reference_enum(ps_compiler *compiler, ps_symbol **type_symbol, const char *type_name);
    bool ps_parse_type_reference_subrange(ps_compiler *compiler, ps_symbol **type_symbol, const char *type_name);
    bool ps_parse_type_reference_array(ps_compiler *compiler, ps_symbol **type_symbol, const char *type_name);

#define PARSE_BEGIN(__PARSE__, __PLUS__)                                                                               \
    ps_lexer *lexer = ps_parser_get_lexer(compiler->parser);                                                           \
    static char *visit = __PARSE__;                                                                                    \
    if (compiler->debug >= DEBUG_TRACE)                                                                                \
    {                                                                                                                  \
        fprintf(stderr, "%*cBEGIN\t%-32s %-32s ", (compiler->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ', visit, \
                __PLUS__);                                                                                             \
        ps_token_debug(stderr, "BEGIN", &lexer->current_token);                                                        \
    }

#define PARSE_END(__PLUS__)                                                                                            \
    {                                                                                                                  \
        if (compiler->debug >= DEBUG_TRACE)                                                                            \
        {                                                                                                              \
            fprintf(stderr, "%*cEND\t%-32s %-32s ", (compiler->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ',      \
                    visit, __PLUS__);                                                                                  \
            ps_token_debug(stderr, "END", &lexer->current_token);                                                      \
        }                                                                                                              \
        return true;                                                                                                   \
    }

#define READ_NEXT_TOKEN                                                                                                \
    {                                                                                                                  \
        if (!ps_lexer_read_token(lexer))                                                                               \
            return false;                                                                                              \
        if (compiler->debug >= DEBUG_TRACE)                                                                            \
        {                                                                                                              \
            fprintf(stderr, "%*cTOKEN\t%-32s %-32s ", (compiler->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ',    \
                    "", "");                                                                                           \
            ps_token_debug(stderr, "NEXT", &lexer->current_token);                                                     \
        }                                                                                                              \
    }

#define EXPECT_TOKEN(__PS_TOKEN_TYPE__)                                                                                \
    if (!ps_parser_expect_token_type(compiler->parser, __PS_TOKEN_TYPE__))                                             \
    {                                                                                                                  \
        if (compiler->debug >= DEBUG_TRACE)                                                                            \
        {                                                                                                              \
            fprintf(stderr, "%*cTOKEN\t%-32s %-32s ", (compiler->level - 1) * 8 - 1, MODE_EXEC ? '*' : ' ',            \
                    "EXPECTED", ps_token_type_dump_value(__PS_TOKEN_TYPE__, "UNKNOWN"));                               \
            ps_token_debug(stderr, "NEXT", &lexer->current_token);                                                     \
        }                                                                                                              \
        ps_compiler_set_message(compiler, "Expected '%s'", ps_token_get_keyword(__PS_TOKEN_TYPE__));                \
        compiler->error = PS_ERROR_UNEXPECTED_TOKEN;                                                                   \
        return false;                                                                                                  \
    }

#define READ_NEXT_TOKEN_OR_CLEANUP                                                                                     \
    if (!ps_lexer_read_token(lexer))                                                                                   \
    {                                                                                                                  \
        if (compiler->debug >= DEBUG_TRACE)                                                                            \
        {                                                                                                              \
            fprintf(stderr, "%*cTOKEN\t%-32s %-32s ", (compiler->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ',    \
                    "", "");                                                                                           \
            ps_token_debug(stderr, "NEXT", &lexer->current_token);                                                     \
        }                                                                                                              \
        goto cleanup;                                                                                                  \
    }

#define EXPECT_TOKEN_OR_CLEANUP(__PS_TOKEN_TYPE__)                                                                     \
    if (!ps_parser_expect_token_type(compiler->parser, __PS_TOKEN_TYPE__))                                             \
    {                                                                                                                  \
        if (compiler->debug >= DEBUG_TRACE)                                                                            \
        {                                                                                                              \
            fprintf(stderr, "%*cTOKEN\t%-32s %-32s ", (compiler->level - 1) * 8 - 1, MODE_EXEC ? '*' : ' ',            \
                    "EXPECTED", ps_token_type_dump_value(__PS_TOKEN_TYPE__, "UNKNOWN"));                               \
            ps_token_debug(stderr, "NEXT", &lexer->current_token);                                                     \
        }                                                                                                              \
        ps_compiler_set_message(interpreter, "Expected '%s'", ps_token_get_keyword(__PS_TOKEN_TYPE__));             \
        goto cleanup;                                                                                                  \
    }

#define COPY_IDENTIFIER(__IDENTIFIER__)                                                                                \
    memcpy(__IDENTIFIER__, lexer->current_token.value.identifier, PS_IDENTIFIER_SIZE);

#define RETURN_ERROR(__PS_ERROR__)                                                                                     \
    {                                                                                                                  \
        if (compiler->debug >= DEBUG_TRACE)                                                                            \
        {                                                                                                              \
            fprintf(stderr, "%*cRETURN\t%-32s %-8d ", (compiler->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ',    \
                    visit, __PS_ERROR__);                                                                              \
            ps_token_debug(stderr, "RETURN", &lexer->current_token);                                                   \
        }                                                                                                              \
        return ps_interpreter_return_false(interpreter, __PS_ERROR__);                                                 \
    }

#define GOTO_CLEANUP(__PS_ERROR__)                                                                                     \
    {                                                                                                                  \
        if (compiler->debug >= DEBUG_TRACE)                                                                            \
        {                                                                                                              \
            fprintf(stderr, "%*cRETURN\t%-32s %-8d ", (compiler->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ',    \
                    visit, __PS_ERROR__);                                                                              \
            ps_token_debug(stderr, "RETURN", &lexer->current_token);                                                   \
        }                                                                                                              \
        compiler->error = __PS_ERROR__;                                                                                \
        goto cleanup;                                                                                                  \
    }

#define TRACE_ERROR(__PLUS__)                                                                                          \
    {                                                                                                                  \
        if (compiler->debug >= DEBUG_TRACE)                                                                            \
        {                                                                                                              \
            fprintf(stderr, "%*cERROR\t%-32s %-32s ", (compiler->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ',    \
                    visit, __PLUS__);                                                                                  \
            ps_token_debug(stderr, "TRACE", &lexer->current_token);                                                    \
        }                                                                                                              \
        return false;                                                                                                  \
    }

#define SAVE_CURSOR(__LINE__, __COLUMN__)                                                                              \
    if (!ps_lexer_get_cursor(lexer, &__LINE__, &__COLUMN__))                                                           \
        TRACE_ERROR("CURSOR!");

#define SAVE_CURSOR_OR_CLEANUP(__LINE__, __COLUMN__)                                                                   \
    if (compiler->debug >= DEBUG_TRACE)                                                                                \
    {                                                                                                                  \
        fprintf(stderr, "%*cCURSOR\t%-32s %-32s %d %d ", (compiler->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ', \
                visit, "SAVE", lexer->buffer->current_line, lexer->buffer->current_column);                            \
        ps_token_debug(stderr, "TRACE", &lexer->current_token);                                                        \
    }                                                                                                                  \
    if (!ps_lexer_get_cursor(lexer, &__LINE__, &__COLUMN__))                                                           \
    {                                                                                                                  \
        if (compiler->debug >= DEBUG_TRACE)                                                                            \
        {                                                                                                              \
            fprintf(stderr, "%*cERROR\t%-32s %-32s ", (compiler->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ',    \
                    visit, "");                                                                                        \
            ps_token_debug(stderr, "TRACE", &lexer->current_token);                                                    \
        }                                                                                                              \
        goto cleanup;                                                                                                  \
    }

#define RESTORE_CURSOR(__LINE__, __COLUMN__)                                                                           \
    if (compiler->debug >= DEBUG_TRACE)                                                                                \
    {                                                                                                                  \
        fprintf(stderr, "%*cCURSOR\t%-32s %-32s %d %d ", (compiler->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ', \
                visit, "RESTORE", __LINE__, __COLUMN__);                                                               \
        ps_token_debug(stderr, "TRACE", &lexer->current_token);                                                        \
    }                                                                                                                  \
    if (!ps_lexer_set_cursor(lexer, __LINE__, __COLUMN__))                                                             \
        TRACE_ERROR("CURSOR!");

#define TRACE_CURSOR                                                                                                   \
    if (compiler->debug >= DEBUG_TRACE)                                                                                \
    {                                                                                                                  \
        uint16_t line = 0;                                                                                             \
        uint16_t column = 0;                                                                                           \
        if (!ps_lexer_get_cursor(lexer, &line, &column))                                                               \
            TRACE_ERROR("CURSOR");                                                                                     \
        fprintf(stderr, "%*cCURSOR\t*** LINE=%d, COLUMN=%d ***\n", (compiler->level - 1) * 8 - 1,                      \
                mode == MODE_EXEC ? '*' : ' ', line, column);                                                          \
        ps_token_debug(stderr, "TRACE", &lexer->current_token);                                                        \
    }

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSE_H */
