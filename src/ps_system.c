/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include "ps_symbol.h"
#include "ps_system.h"
#include "ps_system_types.h"
#include "ps_value.h"
#include "ps_version.h"
#include "ps_interpreter.h"

ps_value ps_value_version_major;
ps_value ps_value_version_minor;
ps_value ps_value_version_patch;
ps_value ps_value_version_index;
ps_symbol ps_symbol_version_major = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_CONSTANT, .name = "PS_VERSION_MAJOR", .value = &ps_value_version_major};
ps_symbol ps_symbol_version_minor = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_CONSTANT, .name = "PS_VERSION_MINOR", .value = &ps_value_version_minor};
ps_symbol ps_symbol_version_patch = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_CONSTANT, .name = "PS_VERSION_PATCH", .value = &ps_value_version_patch};
ps_symbol ps_symbol_version_index = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_CONSTANT, .name = "PS_VERSION_INDEX", .value = &ps_value_version_index};

ps_value ps_value_pi;
ps_symbol ps_symbol_pi = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_CONSTANT, .name = "PI", .value = &ps_value_pi};


void ps_system_init(ps_interpreter *interpreter)
{
    // Version MAJOR, MINOR, PATCH, INDEX, STRING
    ps_value_version_major.type = ps_symbol_unsigned.value->type;
    ps_value_version_major.data.u = PS_VERSION_MAJOR;
    ps_value_version_minor.type = ps_symbol_unsigned.value->type;
    ps_value_version_minor.data.u = PS_VERSION_MINOR;
    ps_value_version_patch.type = ps_symbol_unsigned.value->type;
    ps_value_version_patch.data.u = PS_VERSION_PATCH;
    ps_value_version_index.type = ps_symbol_unsigned.value->type;
    ps_value_version_index.data.u = PS_VERSION_INDEX;
    ps_symbol_table_add(interpreter->parser->symbols, &ps_symbol_version_major);
    ps_symbol_table_add(interpreter->parser->symbols, &ps_symbol_version_minor);
    ps_symbol_table_add(interpreter->parser->symbols, &ps_symbol_version_patch);
    ps_symbol_table_add(interpreter->parser->symbols, &ps_symbol_version_index);
    // No strings yet!
    // snprintf(buffer, sizeof(buffer) - 1, "%d.%d.%d.%d", PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_INDEX);
    // value = ps_value_set_string(NULL, buffer, strlen(buffer), strlen(buffer));
    // ps_interpreter_add_system_constant(vm, "PS_VERSION", value);

    // Limits
    // value = ps_value_set_integer(NULL, ps_integer_max);
    // ps_interpreter_add_system_constant(vm, "MAXINT", value);
    // value = ps_value_set_unsigned(NULL, ps_unsigned_max);
    // ps_interpreter_add_system_constant(vm, "MAXUINT", value);

    // These are keywords for now (until enums are implemented)
    // value = ps_value_set_boolean(NULL, ps_false);
    // ps_interpreter_add_system_constant(vm, "FALSE", value);
    // value = ps_value_set_boolean(NULL, ps_true);
    // ps_interpreter_add_system_constant(vm, "TRUE", value);

    // Reals without PI is not conceivable
    ps_value_pi.type=ps_symbol_real.value->type;
    ps_value_pi.data.r=3.141592653589793; // 115997963468544185161590576171875;
    ps_symbol_table_add(interpreter->parser->symbols, &ps_symbol_pi);

    // ...
}

void ps_system_done(ps_interpreter *interpreter)
{
    ps_symbol_table_delete(interpreter->parser->symbols, (ps_identifier *)&ps_symbol_version_major.name);
    ps_symbol_table_delete(interpreter->parser->symbols, (ps_identifier *)&ps_symbol_version_minor.name);
    ps_symbol_table_delete(interpreter->parser->symbols, (ps_identifier *)&ps_symbol_version_patch.name);
    ps_symbol_table_delete(interpreter->parser->symbols, (ps_identifier *)&ps_symbol_version_index.name);
    ps_symbol_table_delete(interpreter->parser->symbols, (ps_identifier *)&ps_symbol_pi.name);
    // ...
}
