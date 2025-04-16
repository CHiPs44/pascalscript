/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdlib.h>

#include "ps_interpreter.h"
#include "ps_signature.h"
#include "ps_symbol.h"
#include "ps_value.h"

ps_signature *ps_signature_init(uint8_t size, ps_symbol *result_type)
{
    ps_signature *signature = calloc(1, sizeof(ps_signature));
    if (signature == NULL)
        return NULL;
    if (size == 0)
        size == PS_SIGNATURE_PARAMETER_COUNT;
    signature->parameters = calloc(size, sizeof(ps_parameter));
    if (signature->parameters == NULL)
    {
        ps_signature_done(signature);
        return NULL;
    }
    signature->result_type = result_type;
    signature->size = PS_SIGNATURE_PARAMETER_COUNT;
    signature->used = 0;
    return signature;
}

void ps_signature_done(ps_signature *signature)
{
    if (signature == NULL)
        return;
    if (signature->parameters != NULL)
        free(signature->parameters);
    free(signature);
}

bool ps_signature_add_parameter(ps_signature *signature, ps_symbol *value, bool byref)
{
    if (signature->used >= signature->size)
        return false;
    signature->parameters[signature->used].value = value;
    signature->parameters[signature->used].byref = byref;
    signature->used++;
    return true;
}

bool ps_signature_compare(ps_signature *formal, ps_signature *actual)
{
    // Same number of parameters?
    if (formal->used != actual->used)
        return false;
    for (uint8_t i = 0; i < formal->used; i++)
    {
        // Same type?
        if (formal->parameters[i].value->value->type !=
            actual->parameters[i].value->value->type)
            return false;
        // Byref parameters must go to a variable
        if (formal->parameters[i].byref &&
            actual->parameters[i].value->kind != PS_SYMBOL_KIND_VARIABLE)
            return false;
    }
    return true;
}

bool ps_signature_assign(ps_interpreter *interpreter, ps_symbol_scope scope, ps_signature *formal, ps_signature *actual)
{
    ps_symbol *variable;
    for (uint8_t i = 0; i < formal->used; i++)
    {
        if (formal->parameters[i].byref)
        {
            // TODO
        }
        else
        {
            variable = ps_symbol_init(
                scope, PS_SYMBOL_KIND_VARIABLE, &formal->parameters[i].value->name, NULL);
            if (variable == NULL)
                return false;
            if (ps_symbol_table_add(interpreter->parser->symbols, variable) == NULL)
            {
                ps_symbol_free(variable);
                return false;
            }
            if (!ps_function_copy_value(interpreter, actual->parameters[i].value, variable->value))
            {
                ps_symbol_free(variable);
                return false;
            }
            // TODO variable->value->data = actual->parameters[i].value->value.data;
        }
    }
    return true;
}