/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_PARSE_DECLARATION_H
#define _PS_PARSE_DECLARATION_H

#include <stdint.h>

#include "ps_interpreter.h"
#include "ps_lexer.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /* src/ps_parse_declaration.c */
    bool ps_parse_program(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_parse_uses(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_parse_const(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_parse_type(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_parse_var(ps_interpreter *interpreter, ps_interpreter_mode mode);
    bool ps_parse_block(ps_interpreter *interpreter, ps_interpreter_mode mode);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSE_DECLARATION_H */
