/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdlib.h>

#include "ps_environment.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_signature.h"
#include "ps_symbol.h"
#include "ps_value.h"

ps_signature *ps_signature_init(uint8_t parameter_count, ps_symbol *result_type)
{
    ps_signature *signature = calloc(1, sizeof(ps_signature));
    if (signature == NULL)
        return NULL;
    signature->parameters = calloc(parameter_count, sizeof(ps_formal_parameter));
    if (signature->parameters == NULL)
    {
        ps_signature_done(signature);
        return NULL;
    }
    signature->result_type = result_type;
    signature->parameter_count = parameter_count;
    return signature;
}

ps_signature *ps_signature_done(ps_signature *signature)
{
    if (signature != NULL)
    {
        if (signature->parameters != NULL)
            free(signature->parameters);
        free(signature);
    }
    return NULL;
}

ps_error ps_signature_compare(ps_signature *formal, ps_signature *actual)
{
    return PS_ERROR_NOT_IMPLEMENTED;
    // ps_type_definition *formal_type, *actual_type;

    // // Same number of parameters?
    // if (formal->parameter_count != actual->parameter_count)
    //     return PS_ERROR_PARAMETER_COUNT_MISMATCH;
    // // Compare parameters one by one
    // for (int i = 0; i < formal->parameter_count; i++)
    // {
    //     // Same type?
    //     formal_type = formal->parameters[i].data.actual.type->value->type;
    //     actual_type = formal->parameters[i].data.formal.value->data.t;
    //     if (formal_type != actual_type)
    //         return PS_ERROR_UNEXPECTED_TYPE;
    //     // Byref parameters must go to a variable
    //     if (formal->parameters[i].by_ref && !actual->parameters[i].by_ref)
    //         return PS_ERROR_EXPECTED_VARIABLE;
    //     if (actual->parameters[i].kind != PS_SYMBOL_KIND_VARIABLE)
    //         return PS_ERROR_EXPECTED_VARIABLE;
    // }

    // return PS_ERROR_NONE;
}

bool ps_signature_assign(ps_interpreter *interpreter, ps_signature *formal, ps_signature *actual)
{
    return false;
    // ps_symbol *variable;
    // ps_environment *environment = ps_interpreter_get_environment(interpreter);
    // for (uint8_t i = 0; i < formal->parameter_count; i++)
    // {
    //     variable = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, &formal->parameters[i].value->name, NULL);
    //     if (variable == NULL)
    //         return false;
    //     if (!ps_environment_add_symbol(environment, variable))
    //     {
    //         ps_symbol_free(variable);
    //         return false;
    //     }
    //     if (formal->parameters[i].by_ref)
    //     {
    //         // 2 variables point to the same value
    //         variable->value = actual->parameters[i].value->value;
    //     }
    //     else
    //     {
    //         if (!ps_interpreter_copy_value(interpreter, actual->parameters[i].value->value, variable->value))
    //         {
    //             ps_symbol_free(variable);
    //             return false;
    //         }
    //     }
    // }
    // return true;
}
