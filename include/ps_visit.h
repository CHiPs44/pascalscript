/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_COMPILE_H
#define _PS_COMPILE_H

#include <stdint.h>

#include "ps_interpreter.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /* src/ps_visit.c*/
    bool ps_visit_start(ps_interpreter *interpreter, ps_interpreter_mode mode);
    /* src/ps_visit_declaration.c */
    bool ps_visit_program(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_visit_block(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_visit_const(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_visit_type(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_visit_var(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_visit_procedure_or_function(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol_kind kind);
    /* src/ps_visit_expression.c */
    bool ps_visit_expression(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result);
    /* */
    bool ps_visit_if_then_else(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_visit_repeat_until(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_visit_while_do(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_visit_for_do(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_visit_statement_or_compound_statement(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_visit_statement_list(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_token_type stop);
    bool ps_visit_statement(ps_interpreter *interpreter, ps_interpreter_mode mode);

#define VISIT_BEGIN(__VISIT__, __PLUS__)                                                                               \
    ps_lexer *lexer = ps_parser_get_lexer(interpreter->parser);                                                        \
    static char *visit = __VISIT__;                                                                                    \
    if (interpreter->debug >= DEBUG_TRACE)                                                                             \
    {                                                                                                                  \
        fprintf(stderr, "%*cBEGIN\t%-32s %-32s ", (interpreter->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ',     \
                visit, __PLUS__);                                                                                      \
        ps_token_debug(stderr, "BEGIN", &lexer->current_token);                                                        \
    }

#define VISIT_END(__PLUS__)                                                                                            \
    if (interpreter->debug >= DEBUG_TRACE)                                                                             \
    {                                                                                                                  \
        fprintf(stderr, "%*cEND\t%-32s %-32s ", (interpreter->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ',       \
                visit, __PLUS__);                                                                                      \
        ps_token_debug(stderr, "END", &lexer->current_token);                                                          \
    }                                                                                                                  \
    return true;

#define READ_NEXT_TOKEN                                                                                                \
    {                                                                                                                  \
        if (!ps_lexer_read_token(lexer))                                                                               \
            return false;                                                                                              \
        if (interpreter->debug >= DEBUG_TRACE)                                                                         \
        {                                                                                                              \
            fprintf(stderr, "%*cTOKEN\t%-32s %-32s ", (interpreter->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ', \
                    "", "");                                                                                           \
            ps_token_debug(stderr, "NEXT", &lexer->current_token);                                                     \
        }                                                                                                              \
    }

#define EXPECT_TOKEN(__PS_TOKEN_TYPE__)                                                                                \
    if (!ps_parser_expect_token_type(interpreter->parser, __PS_TOKEN_TYPE__))                                          \
    return false

#define RETURN_ERROR(__PS_ERROR__)                                                                                     \
    {                                                                                                                  \
        if (interpreter->debug >= DEBUG_TRACE)                                                                         \
        {                                                                                                              \
            fprintf(stderr, "%*cRETURN\t%-32s %-8d ", (interpreter->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ', \
                    visit, __PS_ERROR__);                                                                              \
            ps_token_debug(stderr, "RETURN", &lexer->current_token);                                                   \
        }                                                                                                              \
        return ps_interpreter_return_error(interpreter, __PS_ERROR__);                                                 \
    }

#define COPY_IDENTIFIER(__IDENTIFIER__)                                                                                \
    strncpy(__IDENTIFIER__, lexer->current_token.value.identifier, PS_IDENTIFIER_LEN)
#define TRACE_ERROR(__PLUS__)                                                                                          \
    {                                                                                                                  \
        if (interpreter->debug >= DEBUG_TRACE)                                                                         \
        {                                                                                                              \
            fprintf(stderr, "%*cERROR\t%-32s %-32s ", (interpreter->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ', \
                    visit, __PLUS__);                                                                                  \
            ps_token_debug(stderr, "TRACE", &lexer->current_token);                                                    \
        }                                                                                                              \
        return false;                                                                                                  \
    }

#define TRACE_CURSOR                                                                                                   \
    if (interpreter->debug >= DEBUG_TRACE)                                                                             \
    {                                                                                                                  \
        uint16_t line = 0;                                                                                             \
        uint8_t column = 0;                                                                                            \
        if (!ps_lexer_get_cursor(lexer, &line, &column))                                                               \
            TRACE_ERROR("CURSOR!");                                                                                    \
        fprintf(stderr, "%*cCURSOR\t*** LINE=%d, COLUMN=%d ***\n", (interpreter->level - 1) * 8 - 1,                   \
                mode == MODE_EXEC ? '*' : ' ', line, column);                                                          \
        ps_token_debug(stderr, "TRACE", &lexer->current_token);                                                        \
    }

#ifdef __cplusplus
}
#endif

#endif /* _PS_COMPILE_H */
