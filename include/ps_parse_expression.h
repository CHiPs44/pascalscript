/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_PARSE_EXPRESSION_H
#define _PS_PARSE_EXPRESSION_H

#include <stdint.h>

#include "ps_ast.h"
#include "ps_compiler.h"
#include "ps_symbol.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    bool ps_parse_expression(ps_compiler *compiler, ps_ast_block *block, ps_ast_node **expression);
    bool ps_parse_constant_expression(ps_compiler *compiler, ps_ast_block *block, ps_value *constant);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSE_EXPRESSION_H */
