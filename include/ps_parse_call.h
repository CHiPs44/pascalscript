/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_PARSE_CALL_H
#define _PS_PARSE_CALL_H

#include <stdint.h>

#include "ps_compiler.h"
#include "ps_lexer.h"
#include "ps_symbol.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /**
     * @brief Parse procedure or function call, be it system or user defined:
     *
     *    IDENTIFIER [ '(' actual_parameter [ ',' actual_parameter ]* ')' ]
     *
     *    where actual_parameter is expression or variable_reference
     */
    bool ps_parse_procedure_or_function_call(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call,
                                             ps_symbol *executable);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSE_CALL_H */
