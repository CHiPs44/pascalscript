/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _LEXER_H
#define _LEXER_H

#include "error.h"

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
        char identifier[MAX_IDENTIFIER + 1];
        int int_val;
    } value;
} token_t;

extern token_t yylval;
extern char *yytext;

extern error_t lexer_copy_identifier();

extern error_t lexer_copy_integer_value();

// extern char *error_get_message(error_t code);

#ifdef __cplusplus
}
#endif

#endif /* _LEXER_H */
