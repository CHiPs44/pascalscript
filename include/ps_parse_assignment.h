/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_PARSE_ASSIGNMENT_H
#define _PS_PARSE_ASSIGNMENT_H

#include "ps_ast.h"
#include "ps_compiler.h"

#ifdef __cplusplus
extern "C"
{
#endif

    bool ps_parse_assignment(ps_compiler *compiler, ps_ast_block *block, ps_ast_assignment **assignment_ptr,
                             ps_ast_block *owner, ps_symbol *variable);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSE_ASSIGNMENT_H */
