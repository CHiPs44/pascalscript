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
    bool ps_parse_start(ps_compiler *compiler, ps_ast_block *block_program);

#define PARSE_BEGIN(__PARSE__, __PLUS__)                                                                               \
    ps_lexer *lexer = ps_parser_get_lexer(compiler->parser);                                                           \
    static char *visit = __PARSE__;                                                                                    \
    if (compiler->debug >= COMPILER_DEBUG_TRACE)                                                                       \
    {                                                                                                                  \
        fprintf(stderr, "BEGIN\t%-32s %-32s %-32s ", block->name, visit, __PLUS__);                                    \
        ps_token_debug(stderr, "BEGIN", &lexer->current_token);                                                        \
    }                                                                                                                  \
    uint16_t start_line = lexer->start_line;                                                                           \
    uint16_t start_column = lexer->start_column;

#define PARSE_END(__PLUS__)                                                                                            \
    {                                                                                                                  \
        if (compiler->debug >= COMPILER_DEBUG_TRACE)                                                                   \
        {                                                                                                              \
            fprintf(stderr, "END\t%-32s  %-32s %-32s ", block->name, visit, __PLUS__);                                 \
            ps_token_debug(stderr, "END", &lexer->current_token);                                                      \
        }                                                                                                              \
        return true;                                                                                                   \
    }

#define READ_NEXT_TOKEN                                                                                                \
    {                                                                                                                  \
        if (!ps_lexer_read_token(lexer))                                                                               \
            return false;                                                                                              \
        if (compiler->debug >= COMPILER_DEBUG_TRACE)                                                                   \
        {                                                                                                              \
            fprintf(stderr, "TOKEN\t%-32s %-32s %-32s ", block->name, "", "");                                         \
            ps_token_debug(stderr, "NEXT", &lexer->current_token);                                                     \
        }                                                                                                              \
    }

#define EXPECT_TOKEN(__PS_TOKEN_TYPE__)                                                                                \
    if (!ps_parser_expect_token_type(compiler->parser, __PS_TOKEN_TYPE__))                                             \
    {                                                                                                                  \
        if (compiler->debug >= COMPILER_DEBUG_TRACE)                                                                   \
        {                                                                                                              \
            fprintf(stderr, "TOKEN\t%-32s %-32s %-32s ", block->name, "EXPECTED",                                      \
                    ps_token_type_dump_value(__PS_TOKEN_TYPE__, "UNKNOWN"));                                           \
            ps_token_debug(stderr, "NEXT", &lexer->current_token);                                                     \
        }                                                                                                              \
        ps_compiler_set_message(compiler, "Expected '%s'", ps_token_get_keyword(__PS_TOKEN_TYPE__));                   \
        compiler->error = PS_ERROR_UNEXPECTED_TOKEN;                                                                   \
        return false;                                                                                                  \
    }

#define READ_NEXT_TOKEN_OR_CLEANUP                                                                                     \
    if (!ps_lexer_read_token(lexer))                                                                                   \
    {                                                                                                                  \
        if (compiler->debug >= COMPILER_DEBUG_TRACE)                                                                   \
        {                                                                                                              \
            fprintf(stderr, "TOKEN\t%-32s %-32s %-32s ", block->name, "", "");                                         \
            ps_token_debug(stderr, "NEXT", &lexer->current_token);                                                     \
        }                                                                                                              \
        goto cleanup;                                                                                                  \
    }

#define EXPECT_TOKEN_OR_CLEANUP(__PS_TOKEN_TYPE__)                                                                     \
    if (!ps_parser_expect_token_type(compiler->parser, __PS_TOKEN_TYPE__))                                             \
    {                                                                                                                  \
        if (compiler->debug >= COMPILER_DEBUG_TRACE)                                                                   \
        {                                                                                                              \
            fprintf(stderr, "TOKEN\t%-32s -%32s %-32s ", block->name, "EXPECTED",                                      \
                    ps_token_type_dump_value(__PS_TOKEN_TYPE__, "UNKNOWN"));                                           \
            ps_token_debug(stderr, "NEXT", &lexer->current_token);                                                     \
        }                                                                                                              \
        ps_compiler_set_message(compiler, "Expected '%s'", ps_token_get_keyword(__PS_TOKEN_TYPE__));                   \
        goto cleanup;                                                                                                  \
    }

#define COPY_IDENTIFIER(__IDENTIFIER__)                                                                                \
    memcpy(__IDENTIFIER__, lexer->current_token.value.identifier, PS_IDENTIFIER_SIZE);

#define RETURN_ERROR(__PS_ERROR__)                                                                                     \
    {                                                                                                                  \
        if (compiler->debug >= COMPILER_DEBUG_TRACE)                                                                   \
        {                                                                                                              \
            fprintf(stderr, "RETURN\t%-32s %-32s %-8d ", block->name, visit, __PS_ERROR__);                            \
            ps_token_debug(stderr, "RETURN", &lexer->current_token);                                                   \
        }                                                                                                              \
        compiler->error = __PS_ERROR__;                                                                                \
        return false;                                                                                                  \
    }

#define GOTO_CLEANUP(__PS_ERROR__)                                                                                     \
    {                                                                                                                  \
        if (compiler->debug >= COMPILER_DEBUG_TRACE)                                                                   \
        {                                                                                                              \
            fprintf(stderr, "RETURN\t%-32s %-32s %-8d ", block->name, visit, __PS_ERROR__);                            \
            ps_token_debug(stderr, "RETURN", &lexer->current_token);                                                   \
        }                                                                                                              \
        compiler->error = __PS_ERROR__;                                                                                \
        goto cleanup;                                                                                                  \
    }

#define TRACE_ERROR(__PLUS__)                                                                                          \
    {                                                                                                                  \
        if (compiler->debug >= COMPILER_DEBUG_TRACE)                                                                   \
        {                                                                                                              \
            fprintf(stderr, "ERROR\t%-32s %-32s %-32s ", block->name, visit, __PLUS__);                                \
            ps_token_debug(stderr, "TRACE", &lexer->current_token);                                                    \
        }                                                                                                              \
        return false;                                                                                                  \
    }

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSE_H */
