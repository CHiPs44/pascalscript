/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdlib.h>

#include "ps_environment.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_memory.h"
#include "ps_signature.h"
#include "ps_symbol.h"
#include "ps_value.h"

ps_formal_signature *ps_formal_signature_alloc(uint8_t parameter_count, ps_symbol *result_type)
{
    ps_formal_signature *signature = ps_memory_malloc(sizeof(ps_formal_signature));
    if (signature == NULL)
        return NULL;
    signature->size = parameter_count;
    signature->parameter_count = parameter_count;
    signature->result_type = result_type;
    signature->parameters = NULL;
    if (parameter_count > 0)
    {
        signature->parameters = ps_memory_calloc(parameter_count, sizeof(ps_formal_parameter));
        if (signature->parameters == NULL)
        {
            ps_formal_signature_free(signature);
            return NULL;
        }
    }
    return signature;
}

ps_formal_signature *ps_formal_signature_free(ps_formal_signature *signature)
{
    if (signature != NULL)
    {
        if (signature->parameters != NULL)
            ps_memory_free(signature->parameters);
        ps_memory_free(signature);
    }
    return NULL;
}

ps_actual_signature *ps_actual_signature_alloc(uint8_t parameter_count, ps_symbol *result_type)
{
    ps_actual_signature *signature = ps_memory_malloc(sizeof(ps_actual_signature));
    if (signature == NULL)
        return NULL;
    signature->parameters = ps_memory_calloc(parameter_count, sizeof(ps_actual_parameter));
    if (signature->parameters == NULL)
    {
        ps_actual_signature_free(signature);
        return NULL;
    }
    signature->result_type = result_type;
    signature->parameter_count = parameter_count;
    return signature;
}

ps_actual_signature *ps_actual_signature_free(ps_actual_signature *signature)
{
    if (signature != NULL)
    {
        if (signature->parameters != NULL)
            ps_memory_free(signature->parameters);
        ps_memory_free(signature);
    }
    return NULL;
}

ps_formal_parameter *ps_formal_signature_find_parameter(ps_formal_signature *signature, ps_identifier *name)
{
    if (signature->parameter_count == 0)
        return NULL;
    for (int i = 0; i < signature->parameter_count; i++)
    {
        if (0 == strncmp((char *)signature->parameters[i].name, (char *)name, PS_IDENTIFIER_LEN))
            return &signature->parameters[i];
    }
    return NULL;
}

bool ps_formal_signature_add_parameter(ps_formal_signature *signature, bool byref, ps_identifier *name, ps_symbol *type)
{
    ps_formal_parameter *new_parameters;
    if (signature->parameter_count == 0)
    {
        signature->parameters = ps_memory_calloc(1, sizeof(ps_formal_parameter));
        if (signature->parameters == NULL)
            return false;
        signature->size = 1;
    }
    else if (signature->parameter_count >= signature->size)
    {
        // Increment size and ps_memory_realloc if needed
        new_parameters = ps_memory_realloc(signature->parameters, (signature->size + 1) * sizeof(ps_formal_parameter));
        if (new_parameters == NULL)
            return false;
        signature->size += 1;
        signature->parameters = new_parameters;
    }
    signature->parameters[signature->parameter_count].byref = byref;
    memcpy(&signature->parameters[signature->parameter_count].name, name, PS_IDENTIFIER_SIZE);
    signature->parameters[signature->parameter_count].type = type;
    signature->parameter_count += 1;
    return true;
}

bool ps_actual_signature_add_byval_parameter(ps_actual_signature *signature, ps_value *value)
{
    // TODO? double size and ps_memory_realloc if needed
    if (signature->parameter_count >= signature->size)
        return false;
    signature->parameters[signature->parameter_count].byref = false;
    signature->parameters[signature->parameter_count].data.value = value;
    signature->parameter_count += 1;
    return true;
}

bool ps_actual_signature_add_byref_parameter(ps_actual_signature *signature, ps_symbol *variable)
{
    // TODO double size and ps_memory_realloc if needed
    if (signature->parameter_count >= signature->size)
        return false;
    signature->parameters[signature->parameter_count].byref = true;
    signature->parameters[signature->parameter_count].data.variable = variable;
    signature->parameter_count += 1;
    return true;
}

bool ps_signature_compare(ps_interpreter *interpreter, ps_formal_signature *formal, ps_actual_signature *actual)
{
    return PS_ERROR_NOT_IMPLEMENTED;
    /*
    ps_type_definition *formal_type, *actual_type;

    // Same number of parameters?
    if (formal->parameter_count != actual->parameter_count)
        return PS_ERROR_PARAMETER_COUNT_MISMATCH;
    // Compare parameters one by one
    for (int i = 0; i < formal->parameter_count; i++)
    {
        // Same type?
        formal_type = formal->parameters[i].data.actual.type->value->type;
        actual_type = formal->parameters[i].data.formal.value->data.t;
        if (formal_type != actual_type)
            return PS_ERROR_UNEXPECTED_TYPE;
        // Byref parameters must go to a variable
        if (formal->parameters[i].byref && !actual->parameters[i].byref)
            return PS_ERROR_EXPECTED_VARIABLE;
        if (actual->parameters[i].kind != PS_SYMBOL_KIND_VARIABLE)
            return PS_ERROR_EXPECTED_VARIABLE;
    }

    return PS_ERROR_NONE;
    */
}

bool ps_signature_assign(ps_interpreter *interpreter, ps_formal_signature *formal, ps_actual_signature *actual)
{
    return false;
    /*
    ps_symbol *variable;
    ps_environment *environment = ps_interpreter_get_environment(interpreter);
    for (int i = 0; i < formal->parameter_count; i++)
    {
        variable = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, &formal->parameters[i].value->name, NULL);
        if (variable == NULL)
            return false;
        if (!ps_environment_add_symbol(environment, variable))
        {
            ps_symbol_free(variable);
            return false;
        }
        if (formal->parameters[i].byref)
        {
            // 2 variables point to the same value
            variable->value = actual->parameters[i].value->value;
        }
        else
        {
            if (!ps_interpreter_copy_value(interpreter, actual->parameters[i].value->value, variable->value))
            {
                ps_symbol_free(variable);
                return false;
            }
        }
    }
    return true;
    */
}

void ps_formal_signature_debug(FILE *output, char *message, ps_formal_signature *signature)
{
    if (output == NULL)
        output = stderr;
    fprintf(output, "%s: Formal signature at %p:\n", message, (void *)signature);
    if (signature == NULL)
        return;
    fprintf(output, "\tResult type: %p\n", (void *)signature->result_type);
    if (signature->result_type != NULL)
        ps_symbol_debug(output, "  Result type: ", signature->result_type);
    fprintf(output, "\tParameter count: %u\n", signature->parameter_count);
    for (int i = 0; i < signature->parameter_count; i++)
    {
        fprintf(output, "\tParameter %d: %s %s of type %p\n", i, signature->parameters[i].byref ? "ref" : "val",
                signature->parameters[i].name, (void *)signature->parameters[i].type);
        if (signature->parameters[i].type != NULL)
            ps_symbol_debug(output, "  Parameter type", signature->parameters[i].type);
    }
}

void ps_actual_signature_debug(FILE *output, char *message, ps_actual_signature *signature)
{
    ps_actual_parameter *parameter;
    if (output == NULL)
        output = stderr;
    fprintf(output, "%s: Actual signature at %p:\n", message, (void *)signature);
    if (signature == NULL)
        return;
    fprintf(output, "\tResult type: %p\n", (void *)signature->result_type);
    if (signature->result_type != NULL)
        ps_symbol_debug(output, "  Result type: ", signature->result_type);
    fprintf(output, "\tParameter count: %u\n", signature->parameter_count);
    for (int i = 0; i < signature->parameter_count; i++)
    {
        parameter = &signature->parameters[i];
        fprintf(output, "\tParameter %d: %s of type %p\n", i, parameter->byref ? "ref" : "val",
                (void *)(parameter->byref ? (void *)parameter->data.variable : (void *)parameter->data.value));
        if (parameter->byref && parameter->data.variable != NULL)
            ps_symbol_debug(output, "  Parameter variable", parameter->data.variable);
        else if (!parameter->byref && parameter->data.value != NULL)
            ps_value_debug(output, "  Parameter value", parameter->data.value);
    }
}
