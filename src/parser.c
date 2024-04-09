/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdlib.h>

#include "vm.h"
#include "lexer.h"

bool parser_start(vm_t *vm)
{
    symbol_t program;

    if (lexer_expect_token_type(vm, TOKEN_PROGRAM))
    {
        if (lexer_expect_token_type(vm, TOKEN_IDENTIFIER))
        {
            program.kind = KIND_CONSTANT;
            strcpy(program.name, "PROGRAM");
            program.size = 0;
            program.type = TYPE_STRING;
            strcpy(program.value.s, vm->current_token.value.s);
            symbol_table_add(&vm->globals, &program);
        }
    }

    return false;
}