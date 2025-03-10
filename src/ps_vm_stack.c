/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_config.h"
#include "ps_error.h"
#include "ps_symbol.h"
#include "ps_value.h"
#include "ps_version.h"
#include "ps_vm.h"

int ps_vm_push(ps_vm *vm, ps_symbol *symbol)
{
    return ps_symbol_stack_push(&vm->stack, symbol);
}

ps_symbol *ps_vm_stack_pop(ps_vm *vm)
{
    return ps_symbol_stack_pop(&vm->stack);
}

/* EOF */
