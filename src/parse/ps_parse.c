/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_compiler.h"
#include "ps_functions.h"
#include "ps_parse.h"
#include "ps_parse_declaration.h"
#include "ps_parser.h"
#include "ps_procedures.h"
#include "ps_string.h"
#include "ps_system.h"

/**
 * Parse
 *  PROGRAM IDENTIFIER [ '(' [ IDENTIFIER { ',' IDENTIFIER } ] ')' ] ';'
 *  BLOCK
 *  '.'
 */
bool ps_parse_start(ps_compiler *compiler)
{
    PARSE_BEGIN("START", "")

    READ_NEXT_TOKEN
    switch (lexer->current_token.type)
    {
    case PS_TOKEN_PROGRAM:
        if (!ps_parse_program(compiler))
            TRACE_ERROR("PROGRAM")
        break;
    case PS_TOKEN_UNIT:
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }

    PARSE_END("OK")
}
