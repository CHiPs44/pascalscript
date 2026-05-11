/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <math.h>
#include <string.h>
#include <time.h>

#include "ps_environment.h"
#include "ps_error.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_procedures.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_value.h"

/**********************************************************************************************************************/
/* STANDARD PROCEDURES                                                                                                */
/**********************************************************************************************************************/

/* clang-format off */
// PS_SYSTEM_PROCEDURE(procedure, dec      , "DEC"      , .proc_1arg      , &ps_procedure_dec      );
// PS_SYSTEM_PROCEDURE(procedure, inc      , "INC"      , .proc_1arg      , &ps_procedure_inc      );
PS_SYSTEM_PROCEDURE(procedure, randomize, "RANDOMIZE", .proc_1arg      , &ps_procedure_randomize);
PS_SYSTEM_PROCEDURE(procedure, read     , "READ"     , .proc_file_read , &ps_procedure_read     );
PS_SYSTEM_PROCEDURE(procedure, readln   , "READLN"   , .proc_file_read , &ps_procedure_readln   );
PS_SYSTEM_PROCEDURE(procedure, write    , "WRITE"    , .proc_file_write, &ps_procedure_write    );
PS_SYSTEM_PROCEDURE(procedure, writeln  , "WRITELN"  , .proc_file_write, &ps_procedure_writeln  );
/* clang-format on */

bool ps_procedures_init(ps_environment *system)
{
    (void)system;
    // ADD_SYSTEM_SYMBOL(ps_system_procedure_dec)
    // ADD_SYSTEM_SYMBOL(ps_system_procedure_inc)
    ADD_SYSTEM_SYMBOL(ps_system_procedure_randomize)
    ADD_SYSTEM_SYMBOL(ps_system_procedure_read)
    ADD_SYSTEM_SYMBOL(ps_system_procedure_readln)
    ADD_SYSTEM_SYMBOL(ps_system_procedure_write)
    ADD_SYSTEM_SYMBOL(ps_system_procedure_writeln)
    return true;
error:
    return false;
}

// bool ps_procedure_inc_or_dec(ps_interpreter *interpreter, ps_value *value, bool is_inc)
// {
//     if (!ps_value_is_ordinal(value))
//         return ps_interpreter_return_false(interpreter, PS_ERROR_UNEXPECTED_TYPE);
//     ps_value new_value = {.allocated = false, .type = value->type, .data = {0}};
//     ps_value_type base = ps_value_get_base(value);
//     switch (base)
//     {
//     case PS_TYPE_CHAR:
//     case PS_TYPE_INTEGER:
//     case PS_TYPE_UNSIGNED:
//         ps_error error = is_inc ? ps_function_succ(interpreter, value, &new_value)
//                                 : ps_function_pred(interpreter, value, &new_value);
//         if (error != PS_ERROR_NONE)
//             return ps_interpreter_return_false(interpreter, error);
//         break;
//     default:
//         return ps_interpreter_return_false(interpreter, PS_ERROR_UNEXPECTED_TYPE);
//     }
//     ps_error error = ps_value_copy(&new_value, value, interpreter->range_check);
//     if (error != PS_ERROR_NONE)
//         return ps_interpreter_return_false(interpreter, error);
//     if (interpreter->debug >= DEBUG_VERBOSE)
//         fprintf(stderr, "%s(%s)\n", is_inc ? "INC" : "DEC", ps_value_get_display_string(value, 0, 0));
//     return true;
// }

// bool ps_procedure_dec(ps_interpreter *interpreter, ps_value *value)
// {
//     return ps_procedure_inc_or_dec(interpreter, value, false);
// }

// bool ps_procedure_inc(ps_interpreter *interpreter, ps_value *value)
// {
//     return ps_procedure_inc_or_dec(interpreter, value, true);
// }

bool ps_procedure_randomize(ps_interpreter *interpreter, const ps_value *value)
{
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
            seed = (unsigned int)(value->data.u); // NOSONAR
            break;
        default:
            return ps_interpreter_return_false(interpreter, PS_ERROR_UNEXPECTED_TYPE);
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

bool ps_procedure_read(ps_interpreter *interpreter, FILE *f, ps_value *value) // NOSONAR
{
    ((void)f);
    ((void)value);
    ps_interpreter_set_message(interpreter, "READ not implemented");
    return ps_interpreter_return_false(interpreter, PS_ERROR_NOT_IMPLEMENTED);
}

bool ps_procedure_readln(ps_interpreter *interpreter, FILE *f, ps_value *value) // NOSONAR
{
    ((void)f);
    ((void)value);
    ps_interpreter_set_message(interpreter, "READLN not implemented");
    return ps_interpreter_return_false(interpreter, PS_ERROR_NOT_IMPLEMENTED);
}

bool ps_procedure_write(ps_interpreter *interpreter, FILE *f, const ps_value *value, int16_t width, int16_t precision)
{
    char *display_value = ps_value_get_display_string(value, width, precision);
    if (display_value == NULL)
        return ps_interpreter_return_false(interpreter, PS_ERROR_EXPECTED_STRING);
    if (interpreter->debug)
        fprintf(f, "WRITE('%s')\n", display_value);
    else
        fprintf(f, "%s", display_value);
    return true;
}

bool ps_procedure_writeln(ps_interpreter *interpreter, FILE *f, const ps_value *value, int16_t width, int16_t precision)
{
    char *display_value = ps_value_get_display_string(value, width, precision);
    if (display_value == NULL)
        return ps_interpreter_return_false(interpreter, PS_ERROR_EXPECTED_STRING);
    if (interpreter->debug)
        fprintf(f, "WRITELN('%s')\n", display_value);
    else
        fprintf(f, "%s", display_value);
    return true;
}
