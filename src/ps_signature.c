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
    ps_formal_signature *signature = ps_memory_malloc(PS_MEMORY_SIGNATURE, sizeof(ps_formal_signature));
    if (signature == NULL)
        return NULL;
    signature->size = parameter_count;
    signature->parameter_count = parameter_count;
    signature->result_type = result_type;
    signature->parameters = NULL;
    if (parameter_count > 0)
    {
        signature->parameters = ps_memory_calloc(PS_MEMORY_SIGNATURE, parameter_count, sizeof(ps_formal_parameter));
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
    fprintf(stderr, "ps_formal_signature_free(%p)\n", (void *)signature);
    if (signature != NULL)
    {
        if (signature->parameters != NULL)
            ps_memory_free(PS_MEMORY_SIGNATURE, signature->parameters);
        ps_memory_free(PS_MEMORY_SIGNATURE, signature);
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
        signature->parameters = ps_memory_calloc(PS_MEMORY_SIGNATURE, 1, sizeof(ps_formal_parameter));
        if (signature->parameters == NULL)
            return false;
        signature->size = 1;
    }
    else if (signature->parameter_count >= signature->size)
    {
        // Increment size and ps_memory_realloc if needed
        new_parameters = ps_memory_realloc(PS_MEMORY_SIGNATURE, signature->parameters,
                                           (signature->size + 1) * sizeof(ps_formal_parameter));
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

void ps_formal_signature_dump(FILE *output, char *message, ps_formal_signature *signature)
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
            ps_symbol_debug(output, "  Parameter type: ", signature->parameters[i].type);
    }
}
