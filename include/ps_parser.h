/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_PARSER_H
#define _PS_PARSER_H

#include <stdbool.h>

#include "ps_config.h"
#include "ps_error.h"
#include "ps_token.h"
#include "ps_vm.h"

#ifdef __cplusplus
extern "C"
{
#endif

    bool parser_start(vm_t *vm);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSER_H */
