/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _ERROR_H
#define _ERROR_H

#ifdef __cplusplus
extern "C"
{
#endif

typedef enum _error_code_t {
    ERROR_NONE,
    /* lexer */
    ERROR_IDENTIFIER_TOO_LONG,
    ERROR_OVERFLOW,
    /* parser */
    ERROR_SYNTAX,
    ERROR_UNEXPECTED,
    ERROR_UNKOWN_IDENTIFIER,
    ERROR_CONSTANT_VALUE,
    /* ...*/
} error_code_t;

extern char *error_get_message(error_code_t code);

#ifdef __cplusplus
}
#endif

#endif /* _ERROR_H */
