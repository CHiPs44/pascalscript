/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdbool.h>

#include "ps_config.h"
#include "ps_symbol.h"
#include "ps_type_definition.h"

bool subrange_debug = false;

ps_type_definition *ps_subrange_create_char(ps_char min, ps_char max)
{
    ps_type_definition *type_def = ps_type_definition_alloc(PS_TYPE_SUBRANGE, PS_TYPE_CHAR);
    if (type_def == NULL)
        return NULL;
    type_def->def.g.c.min = min;
    type_def->def.g.c.max = max;
    return type_def;
}

ps_type_definition *ps_subrange_create_integer(ps_integer min, ps_integer max)
{
    ps_type_definition *type_def = ps_type_definition_alloc(PS_TYPE_SUBRANGE, PS_TYPE_INTEGER);
    if (type_def == NULL)
        return NULL;
    type_def->def.g.i.min = min;
    type_def->def.g.i.max = max;
    return type_def;
}

ps_type_definition *ps_subrange_create_unsigned(ps_unsigned min, ps_unsigned max)
{
    ps_type_definition *type_def = ps_type_definition_alloc(PS_TYPE_SUBRANGE, PS_TYPE_UNSIGNED);
    if (type_def == NULL)
        return NULL;
    type_def->def.g.u.min = min;
    type_def->def.g.u.max = max;
    return type_def;
}

ps_type_definition *ps_subrange_create_enum(ps_symbol *symbol_enum, ps_enum_value min, ps_enum_value max)
{
    ps_type_definition *type_def = ps_type_definition_alloc(PS_TYPE_SUBRANGE, PS_TYPE_ENUM);
    if (type_def == NULL)
        return NULL;
    type_def->def.g.e.symbol_enum = symbol_enum;
    type_def->def.g.e.min = min;
    type_def->def.g.e.max = max;
    return type_def;
}

ps_unsigned ps_subrange_get_count(const ps_type_definition *subrange)
{
    if (subrange_debug)
        ps_type_definition_debug(stderr, "ps_subrange_get_count, type_def:", subrange);
    ps_unsigned count = PS_UNSIGNED_MAX;
    if (subrange_debug)
        fprintf(stderr, "SUBRANGE? %s\n", subrange->type == PS_TYPE_SUBRANGE ? "YES" : "NO");
    if (subrange->type == PS_TYPE_SUBRANGE)
        switch (subrange->base)
        {
        case PS_TYPE_CHAR:
            count = subrange->def.g.c.max - subrange->def.g.c.min + 1;
            if (subrange_debug)
                fprintf(stderr, "ps_subrange_get_count: CHAR %u - %u + 1 = %u\n", subrange->def.g.c.max,
                        subrange->def.g.c.min, count);
            break;
        case PS_TYPE_UNSIGNED:
            count = subrange->def.g.u.max - subrange->def.g.u.min + 1;
            if (subrange_debug)
                fprintf(stderr, "ps_subrange_get_count: UNSIGNED %u - %u + 1 = %u\n", subrange->def.g.u.max,
                        subrange->def.g.u.min, count);
            break;
        case PS_TYPE_INTEGER:
            count = subrange->def.g.i.max - subrange->def.g.i.min + 1;
            if (subrange_debug)
                fprintf(stderr, "ps_subrange_get_count: INTEGER %d - %d + 1 = %d\n", subrange->def.g.i.max,
                        subrange->def.g.i.min, count);
            break;
        case PS_TYPE_ENUM:
            count = subrange->def.g.e.max - subrange->def.g.e.min + 1;
            if (subrange_debug)
                fprintf(stderr, "ps_subrange_get_count: ENUM %u - %u + 1 = %u\n", subrange->def.g.e.max,
                        subrange->def.g.e.min, count);
            break;
        default:
            break;
        }
    return count;
}

ps_unsigned ps_subrange_get_offset(const ps_type_definition *subrange, const ps_value *index)
{
    ps_unsigned offset = PS_UNSIGNED_MAX;
    if (subrange->type != PS_TYPE_SUBRANGE)
        return offset;
    if (subrange_debug)
    {
        fprintf(stderr, " DEBUG\tSUBRANGE\ttype = %s\n", ps_value_type_get_name(subrange->type));
        fprintf(stderr, " DEBUG\tSUBRANGE\tbase = %s\n", ps_value_type_get_name(subrange->base));
        fprintf(stderr, " DEBUG\tSUBRANGE\trange= %u..%u\n", subrange->def.g.u.min, subrange->def.g.u.max);
        fprintf(stderr, " DEBUG\tSUBRANGE\tindex= %s/%s/%s: %s\n", index->type->name,
                ps_value_type_get_name(ps_value_get_type(index)), ps_value_type_get_name(ps_value_get_base(index)),
                ps_value_get_debug_string(index));
    }
    switch (subrange->base)
    {
    case PS_TYPE_CHAR:
        // 'C' from 'A'..'F' => Ord('C') - Ord('A') => offset = 2
        // 'G' from 'A'..'F' => 'G' is out of range => offset = PS_UNSIGNED_MAX
        ps_char c = index->data.c;
        if (ps_value_get_base(index) == PS_TYPE_CHAR && c >= subrange->def.g.c.min && c <= subrange->def.g.c.max)
            offset = c - subrange->def.g.c.min;
        break;
    case PS_TYPE_UNSIGNED:
        // 3 from 1..10 => 3 - 1 => 2
        ps_unsigned u = 0;
        bool valid = false;
        if (ps_value_get_base(index) == PS_TYPE_INTEGER && index->data.i >= 0)
        {
            u = index->data.i;
            valid = true;
        }
        else if (ps_value_get_base(index) == PS_TYPE_UNSIGNED)
        {
            u = index->data.u;
            valid = true;
        }
        else if (subrange_debug)
            fprintf(stderr, "NOT INTEGER NOR UNSIGNED\n");
        if (valid && u >= subrange->def.g.u.min && u <= subrange->def.g.u.max)
            offset = index->data.u - subrange->def.g.u.min;
        if (subrange_debug)
            fprintf(stderr, " DEBUG\tSUBRANGE\toffset= %u, %u, %s\n", offset, u, valid ? "YES" : "NO");
        break;
    case PS_TYPE_INTEGER:
        // 3 from -4..4 => 3 - -4 => 7
        //         0..8
        ps_integer i = 0;
        bool valid = false;
        if (ps_value_get_base(index) == PS_TYPE_UNSIGNED && index->data.u <= PS_INTEGER_MAX)
        {
            i = index->data.u;
            valid = true;
        }
        else if (ps_value_get_base(index) == PS_TYPE_INTEGER)
        {
            i = index->data.i;
            valid = true;
        }
        if (valid && i >= subrange->def.g.i.min && i <= subrange->def.g.i.max)
            offset = i - subrange->def.g.i.min;
        break;
    case PS_TYPE_ENUM:
        // Wednesday from Monday..Friday (from (Monday, ..., Sunday) => Ord(Wednesday) - Ord(Monday) => 2
        if (ps_value_get_type(index) == PS_TYPE_ENUM && index->data.u >= subrange->def.g.e.min &&
            index->data.u <= subrange->def.g.e.max)
            offset = index->data.u - subrange->def.g.u.min;
        break;
    default:
        break;
    }
    return offset;
}
