/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_PARSE_STATEMENT_H
#define _PS_PARSE_STATEMENT_H

#include <stdint.h>

#include "ps_compiler.h"
#include "ps_lexer.h"

#ifdef __cplusplus
extern "C"
{
#endif

    bool ps_parse_statement(ps_compiler *compiler);
    bool ps_parse_compound_statement(ps_compiler *compiler);
    bool ps_parse_assignment(ps_compiler *compiler, ps_symbol *variable);
    bool ps_parse_read_or_readln(ps_compiler *compiler, bool newline);
    bool ps_parse_write_or_writeln(ps_compiler *compiler, bool newline);
    bool ps_parse_assignment_or_procedure_call(ps_compiler *compiler);
    bool ps_parse_if_then_else(ps_compiler *compiler);
    bool ps_parse_repeat_until(ps_compiler *compiler);
    bool ps_parse_while_do(ps_compiler *compiler);
    bool ps_parse_for_do(ps_compiler *compiler);
    bool ps_parse_statement_list(ps_compiler *compiler, ps_token_type stop);
    bool ps_parse_statement_or_compound_statement(ps_compiler *compiler);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSE_STATEMENT_H */
