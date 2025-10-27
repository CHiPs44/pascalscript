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

bool ps_procedure_read(ps_interpreter *interpreter, FILE *f, ps_value *value)
{
    ((void)f);
    ((void)value);
    interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
    return false;
}

bool ps_procedure_readln(ps_interpreter *interpreter, FILE *f, ps_value *value)
{
    ((void)f);
    ((void)value);
    interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
    return false;
}

bool ps_procedure_write(ps_interpreter *interpreter, FILE *f, ps_value *value)
{
    char *display_value = ps_value_get_display_string(value);
    if (display_value == NULL)
    {
        interpreter->error = PS_ERROR_EXPECTED_STRING;
        return false;
    }
    if (interpreter->debug)
        fprintf(f, "WRITE('%s')\n", display_value);
    else
        fprintf(f, "%s", display_value);
    return true;
}

bool ps_procedure_writeln(ps_interpreter *interpreter, FILE *f, ps_value *value)
{
    char *display_value = ps_value_get_display_string(value);
    if (display_value == NULL)
    {
        interpreter->error = PS_ERROR_EXPECTED_STRING;
        return false;
    }
    if (interpreter->debug)
        fprintf(f, "WRITELN('%s')\n", display_value);
    else
        fprintf(f, "%s", display_value);
    return true;
}

bool ps_procedure_randomize(ps_interpreter *interpreter, FILE *f, ps_value *value)
{
    (void)f;
    unsigned int seed = 0;
    // No argument: use current time as seed
    if (value != NULL && value->type != NULL && value->type->value != NULL)
    {
        // Argument: use its value as seed
        switch (value->type->value->data.t->base)
        {
        case PS_TYPE_INTEGER:
            seed = (unsigned int)(value->data.i);
            break;
        case PS_TYPE_UNSIGNED:
            seed = (unsigned int)(value->data.u);
            break;
        default:
            interpreter->error = PS_ERROR_UNEXPECTED_TYPE;
            return false;
        }
        srand(seed);
        if (interpreter->debug >= DEBUG_VERBOSE)
            fprintf(stderr, "RANDOMIZE(%u)\n", seed);
        return true;
    }
    srand((unsigned int)time(NULL));
    if (interpreter->debug >= DEBUG_VERBOSE)
        fprintf(stderr, "RANDOMIZE\n");
    return true;
}
