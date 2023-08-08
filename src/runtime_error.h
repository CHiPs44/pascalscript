/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _RUNTIME_ERROR_H_
#define _RUNTIME_ERROR_H_

#ifdef __cplusplus
extern "C"
{
#endif

typedef enum _runtime_error_t
{
    RUNTIME_OK,
    RUNTIME_STACK_EMPTY,
    RUNTIME_STACK_OVERFLOW,
    RUNTIME_GLOBAL_TABLE_FULL,
    RUNTIME_UNKNOWN_UNARY_OPERATOR,
    RUNTIME_UNKNOWN_BINARY_OPERATOR,
    RUNTIME_EXPECTED_NUMBER,
    RUNTIME_END_ENUM,
} runtime_error_t;

const char *runtime_error_messages[RUNTIME_END_ENUM] = {
    "OK",
    "Stack empty",
    "Stack overflow",
    "Global table full",
    "Unknown unary operator",
    "Unknown binary operator",
    "Number expected"};

extern const char *runtime_error_get_message(const runtime_error_t runtime_error);

#ifdef __cplusplus
}
#endif

#endif /* _RUNTIME_ERROR_H_ */
