/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_PARSE_EXECUTABLE_H
#define _PS_PARSE_EXECUTABLE_H

#include <stdint.h>

#include "ps_compiler.h"
#include "ps_lexer.h"
#include "ps_symbol.h"

#ifdef __cplusplus
extern "C"
{
#endif

    bool ps_parse_procedure_or_function_declaration(ps_compiler *compiler, ps_ast_block *block,
                                                    ps_ast_block **executable, ps_symbol_kind kind);
    bool ps_parse_procedure_or_function_call(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call,
                                             ps_symbol *executable);
    bool ps_parse_variable_reference(ps_compiler *compiler, ps_ast_block *block, ps_symbol **variable);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSE_EXECUTABLE_H */
