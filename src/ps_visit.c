/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_functions.h"
#include "ps_parser.h"
#include "ps_procedures.h"
#include "ps_string.h"
#include "ps_system.h"
#include "ps_visit.h"

const ps_identifier ps_identifier_result = "RESULT";

/**
 * Visit
 *  PROGRAM IDENTIFIER ';'
 *  BLOCK
 *  '.'
 */
bool ps_visit_start(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("START", "")

    READ_NEXT_TOKEN
    switch (lexer->current_token.type)
    {
    case PS_TOKEN_PROGRAM:
        if (!ps_visit_program(interpreter, mode))
            TRACE_ERROR("PROGRAM")
        break;
    case PS_TOKEN_UNIT:
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }

    VISIT_END("OK")
    return false;
}
