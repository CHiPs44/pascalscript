/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <math.h>
#include <string.h>
#include <time.h>

#include "ps_error.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_value.h"

bool ps_procedure_write_text(ps_interpreter *interpreter, FILE *f, ps_value *value)
{
    char *display_value = ps_value_get_display_string(value);
    if (display_value == NULL)
    {
        interpreter->error = PS_RUNTIME_ERROR_EXPECTED_STRING;
        return false;
    }
    if (interpreter->debug)
        fprintf(f, "WRITE\t'%s'\n", display_value);
    else
        fprintf(f, "%s", display_value);
    return true;
}

bool ps_procedure_randomize(ps_interpreter *interpreter)
{
    srand((unsigned int)time(NULL));
    return true;
}
