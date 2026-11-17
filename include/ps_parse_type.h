/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_PARSE_TYPE_H
#define _PS_PARSE_TYPE_H

#include <stdint.h>

#include "ps_ast.h"
#include "ps_compiler.h"
#include "ps_symbol.h"

#ifdef __cplusplus
extern "C"
{
#endif

    bool ps_parse_type_definition(ps_compiler *compiler, ps_ast_block *block);
    bool ps_parse_type_reference(ps_compiler *compiler, ps_ast_block *block, ps_symbol **type_symbol,
                                 const ps_char *type_name);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSE_TYPE_H */
