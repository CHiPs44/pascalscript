/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _LEXER_H
#define _LEXER_H

#ifdef __cplusplus
extern "C"
{
#endif

#define MAX_IDENTIFIER 31

typedef enum _token_type_t
{
    IDENTIFIER,
    INT_VAL
} token_type_t;

typedef struct _token_t
{
    token_type_t type;
    union
    {
        int int_val;
        char identifier[MAX_IDENTIFIER + 1];
    } value;
} token_t;

extern token_t yylval;
extern token_t *token;

extern error_code_t copy_identifier(const char *buffer);

extern error_code_t copy_integer_value(const char *buffer);

#ifdef __cplusplus
}
#endif

#endif /* _LEXER_H */
