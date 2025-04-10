/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include "ps_symbol.h"
#include "ps_system_types.h"
#include "ps_system.h"
#include "ps_value.h"
#include "ps_version.h"

/******************************************************************************/
/* SYSTEM TYPE DEFINITIONS AND CONSTANTS                                      */
/******************************************************************************/

ps_type_definition ps_type_def_type_def = {.type = PS_TYPE_DEFINITION, .base = PS_TYPE_DEFINITION};
ps_value ps_value_type_def = {.type = &ps_type_def_type_def, .data = {.t = &ps_type_def_type_def}};
ps_symbol ps_system_type_def = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_TYPE_DEFINITION,
    .name = "TYPE_DEF",
    .value = &ps_value_type_def};

ps_type_definition ps_type_def_boolean = {.type = PS_TYPE_BOOLEAN, .base = PS_TYPE_BOOLEAN};
ps_value ps_value_boolean = {.type = &ps_type_def_type_def, .data = {.t = &ps_type_def_boolean}};
ps_symbol ps_system_boolean = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_TYPE_DEFINITION,
    .name = "BOOLEAN",
    .value = &ps_value_boolean};

ps_type_definition ps_type_def_char = {.type = PS_TYPE_CHAR, .base = PS_TYPE_CHAR};
ps_value ps_value_char = {.type = &ps_type_def_type_def, .data = {.t = &ps_type_def_char}};
ps_symbol ps_system_char = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_TYPE_DEFINITION,
    .name = "CHAR",
    .value = &ps_value_char};

ps_type_definition ps_type_def_integer = {.type = PS_TYPE_INTEGER, .base = PS_TYPE_INTEGER};
ps_value ps_value_integer = {.type = &ps_type_def_type_def, .data = {.t = &ps_type_def_integer}};
ps_symbol ps_system_integer = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_TYPE_DEFINITION,
    .name = "INTEGER",
    .value = &ps_value_integer};

ps_type_definition ps_type_def_unsigned = {.type = PS_TYPE_UNSIGNED, .base = PS_TYPE_UNSIGNED};
ps_value ps_value_unsigned = {.type = &ps_type_def_type_def, .data = {.t = &ps_type_def_unsigned}};
ps_symbol ps_system_unsigned = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_TYPE_DEFINITION,
    .name = "UNSIGNED",
    .value = &ps_value_unsigned};

ps_type_definition ps_type_def_real = {.type = PS_TYPE_REAL, .base = PS_TYPE_REAL};
ps_value ps_value_real = {.type = &ps_type_def_type_def, .data = {.t = &ps_type_def_real}};
ps_symbol ps_system_real = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_TYPE_DEFINITION,
    .name = "REAL",
    .value = &ps_value_real};

ps_value ps_value_boolean_false = {.type = &ps_type_def_boolean, .data = {.b = (ps_boolean) false}};
ps_symbol ps_system_boolean_false = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_CONSTANT,
    .name = "FALSE",
    .value = &ps_value_boolean_false};

ps_value ps_value_boolean_true = {.type = &ps_type_def_boolean, .data = {.b = (ps_boolean) true}};
ps_symbol ps_system_boolean_true = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_CONSTANT,
    .name = "TRUE",
    .value = &ps_value_boolean_true};

ps_value ps_value_maxint = {.type = &ps_type_def_integer, .data = {.i = PS_INTEGER_MAX}};
ps_symbol ps_system_maxint = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_CONSTANT,
    .name = "MAXINT",
    .value = &ps_value_maxint};
ps_value ps_value_maxuint = {.type = &ps_type_def_unsigned, .data = {.u = PS_UNSIGNED_MAX}};
ps_symbol ps_system_maxuint = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_CONSTANT,
    .name = "MAXUINT",
    .value = &ps_value_maxuint};

ps_value ps_value_pi = {
    .type = &ps_type_def_real,
    .data = {.t = &ps_type_def_real, .r = 3.141592653589793 /*115997963468544185161590576171875*/},
};
ps_symbol ps_system_pi = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_CONSTANT,
    .name = "PI",
    .value = &ps_value_pi,
};

ps_value ps_value_version_major = {.type = &ps_type_def_unsigned, .data = {.u = PS_VERSION_MAJOR}};
ps_value ps_value_version_minor = {.type = &ps_type_def_unsigned, .data = {.u = PS_VERSION_MINOR}};
ps_value ps_value_version_patch = {.type = &ps_type_def_unsigned, .data = {.u = PS_VERSION_PATCH}};
ps_value ps_value_version_index = {.type = &ps_type_def_unsigned, .data = {.u = PS_VERSION_INDEX}};
ps_symbol ps_system_version_major = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_CONSTANT,
    .name = "PS_VERSION_MAJOR",
    .value = &ps_value_version_major,
};
ps_symbol ps_system_version_minor = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_CONSTANT,
    .name = "PS_VERSION_MINOR",
    .value = &ps_value_version_minor,
};
ps_symbol ps_system_version_patch = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_CONSTANT,
    .name = "PS_VERSION_PATCH",
    .value = &ps_value_version_patch,
};
ps_symbol ps_system_version_index = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_CONSTANT,
    .name = "PS_VERSION_INDEX",
    .value = &ps_value_version_index,
};

bool ps_system_init(ps_interpreter *interpreter)
{
    ps_symbol_table *symbols = interpreter->parser->symbols;
    if (ps_symbol_table_available(symbols) < 5 + 2)
        return false;

    /**************************************************************************/
    /* TYPES                                                                  */
    /**************************************************************************/

    ps_value_boolean.data.t = &ps_type_def_boolean;
    ps_value_char.data.t = &ps_type_def_char;
    ps_value_integer.data.t = &ps_type_def_integer;
    ps_value_unsigned.data.t = &ps_type_def_unsigned;
    ps_value_real.data.t = &ps_type_def_real;
    ps_symbol_table_add(symbols, &ps_system_boolean);
    ps_symbol_table_add(symbols, &ps_system_char);
    ps_symbol_table_add(symbols, &ps_system_integer);
    ps_symbol_table_add(symbols, &ps_system_real);
    ps_symbol_table_add(symbols, &ps_system_unsigned);

    /**************************************************************************/
    /* CONSTANTS                                                              */
    /**************************************************************************/

    ps_symbol_table_add(symbols, &ps_system_boolean_false);
    ps_symbol_table_add(symbols, &ps_system_boolean_true);
    ps_symbol_table_add(symbols, &ps_system_maxint);
    ps_symbol_table_add(symbols, &ps_system_maxuint);
    ps_symbol_table_add(symbols, &ps_system_pi);
    ps_symbol_table_add(symbols, &ps_system_version_major);
    ps_symbol_table_add(symbols, &ps_system_version_minor);
    ps_symbol_table_add(symbols, &ps_system_version_patch);
    ps_symbol_table_add(symbols, &ps_system_version_index);
    // No strings yet!
    // snprintf(buffer, sizeof(buffer) - 1, "%d.%d.%d.%d", PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_INDEX);
    // value = ps_value_set_string(NULL, buffer, strlen(buffer), strlen(buffer));
    // ps_interpreter_add_system_constant(vm, "PS_VERSION", value);

    // ...
    return true;
}

void ps_system_done(ps_interpreter *interpreter)
{
    ps_symbol_table_delete(interpreter->parser->symbols, (ps_identifier *)&ps_system_version_major.name);
    ps_symbol_table_delete(interpreter->parser->symbols, (ps_identifier *)&ps_system_version_minor.name);
    ps_symbol_table_delete(interpreter->parser->symbols, (ps_identifier *)&ps_system_version_patch.name);
    ps_symbol_table_delete(interpreter->parser->symbols, (ps_identifier *)&ps_system_version_index.name);
    ps_symbol_table_delete(interpreter->parser->symbols, (ps_identifier *)&ps_system_pi.name);
    // ...
}
