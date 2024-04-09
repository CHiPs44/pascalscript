/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _LEXER_H
#define _LEXER_H

#include <stdbool.h>

#include "pascalscript.h"
#include "error.h"
#include "vm.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef MAX_IDENTIFIER
#define MAX_IDENTIFIER 31
#endif

    extern void lexer_dump_token(token_t *token);

    // extern error_t lexer_copy_identifier();
    // extern error_t lexer_copy_integer_value();
    // extern error_t lexer_copy_real_value();
    // extern error_t lexer_copy_char_value();
    // extern error_t lexer_copy_string_value();

    // error_t lexer_read_token(vm_t *vm, token_t *token);
    bool lexer_read_identifier_or_keyword(vm_t *vm, token_t *token);
    bool lexer_read_number(vm_t *vm, token_t *token);

    bool lexer_expect_token_type(vm_t *vm, token_t token, token_type_t token_type);
    bool lexer_expect_token_types(vm_t *vm, token_t token, size_t token_type_count, token_type_t token_type[]);

#ifdef __cplusplus
}
#endif

#endif /* _LEXER_H */
