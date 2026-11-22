/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_PARSE_READ_H
#define _PS_PARSE_READ_H

#include "ps_ast.h"
#include "ps_compiler.h"
#include "ps_parse_statement.h"

#ifdef __cplusplus
extern "C"
{
#endif

    bool ps_parse_read_or_readln_or_readstr(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call_ptr,
                                            ps_read_write_mode mode);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSE_READ_H */
