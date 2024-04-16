/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_LEXER_H
#define _PS_LEXER_H

#include <stdbool.h>

#include "ps_config.h"
#include "ps_error.h"
#include "ps_token.h"
#include "ps_vm.h"

#ifdef __cplusplus
extern "C"
{
#endif

    extern void lexer_dump_token(token_t *token);

    // error_t lexer_read_token(vm_t *vm);
    bool lexer_read_identifier_or_keyword(vm_t *vm);
    bool lexer_read_number(vm_t *vm);

#ifdef __cplusplus
}
#endif

#endif /* _PS_LEXER_H */
