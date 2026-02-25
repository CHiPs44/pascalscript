/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <math.h>

#include "ps_environment.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_procedures.h"
#include "ps_signature.h"
#include "ps_string.h"
#include "ps_symbol.h"
#include "ps_system.h"
#include "ps_system_types.h"
#include "ps_type_definition.h"
#include "ps_value.h"
#include "ps_version.h"

/**********************************************************************************************************************/
/* SYSTEM TYPE DEFINITIONS AND CONSTANTS                                                                              */
/**********************************************************************************************************************/

/* clang-format off */

ps_type_definition ps_type_def_type_def = {.type = PS_TYPE_DEFINITION            , .base = PS_TYPE_DEFINITION                                                           };
ps_symbol          ps_symbol_type_def   = {.kind = PS_SYMBOL_KIND_TYPE_DEFINITION, .name = "##TYPE_DEF", .value = NULL              , .system = true, .allocated = false};
ps_value           ps_value_type_def    = {.type = &ps_symbol_type_def           , .data = {.t = &ps_type_def_type_def}                             , .allocated = false};
ps_symbol          ps_system_type_def   = {.kind = PS_SYMBOL_KIND_TYPE_DEFINITION, .name = "#TYPE_DEF" , .value = &ps_value_type_def, .system = true, .allocated = false};

/* clang-format on */
#define PS_SYSTEM_TYPE(__name__, __NAME__, __VALUE_TYPE__, __VALUE_BASE__)                        \
    ps_type_definition ps_type_def_##__name__ = {.type = __VALUE_TYPE__, .base = __VALUE_BASE__}; \
    ps_value ps_value_##__name__ = {                                                              \
        .allocated = false, .type = &ps_symbol_type_def, .data = {.t = &ps_type_def_##__name__}}; \
    ps_symbol ps_system_##__name__ = {.kind = PS_SYMBOL_KIND_TYPE_DEFINITION,                     \
                                      .name = __NAME__,                                           \
                                      .value = &ps_value_##__name__,                              \
                                      .system = true,                                             \
                                      .allocated = false}
/* clang-format off */

PS_SYSTEM_TYPE(none             , "#NONE"             , PS_TYPE_NONE      , PS_TYPE_NONE                              );
PS_SYSTEM_TYPE(integer          , "INTEGER"           , PS_TYPE_INTEGER   , PS_TYPE_INTEGER                           );
PS_SYSTEM_TYPE(unsigned         , "UNSIGNED"          , PS_TYPE_UNSIGNED  , PS_TYPE_UNSIGNED                          );
PS_SYSTEM_TYPE(boolean          , "BOOLEAN"           , PS_TYPE_BOOLEAN   , PS_TYPE_BOOLEAN                           );
PS_SYSTEM_TYPE(char             , "CHAR"              , PS_TYPE_CHAR      , PS_TYPE_CHAR                              );
PS_SYSTEM_TYPE(real             , "REAL"              , PS_TYPE_REAL      , PS_TYPE_REAL                              );
PS_SYSTEM_TYPE(string           , "STRING"            , PS_TYPE_STRING    , PS_TYPE_STRING                            );
PS_SYSTEM_TYPE(array            , "#ARRAY"            , PS_TYPE_ARRAY     , PS_TYPE_ARRAY                             );
PS_SYSTEM_TYPE(subrange_char    , "#SUBRANGE_CHAR"    , PS_TYPE_SUBRANGE  , PS_TYPE_CHAR                              );
PS_SYSTEM_TYPE(subrange_integer , "#SUBRANGE_INTEGER" , PS_TYPE_SUBRANGE  , PS_TYPE_INTEGER                           );
PS_SYSTEM_TYPE(subrange_unsigned, "#SUBRANGE_UNSIGNED", PS_TYPE_SUBRANGE  , PS_TYPE_UNSIGNED                          );
PS_SYSTEM_TYPE(subrange_enum    , "#SUBRANGE_ENUM"    , PS_TYPE_SUBRANGE  , PS_TYPE_ENUM                              );
PS_SYSTEM_TYPE(enum             , "#ENUM"             , PS_TYPE_ENUM      , PS_TYPE_UNSIGNED                          );
PS_SYSTEM_TYPE(record           , "#RECORD"           , PS_TYPE_RECORD    , PS_TYPE_RECORD                            );
PS_SYSTEM_TYPE(procedure        , "#PROCEDURE"        , PS_TYPE_EXECUTABLE, PS_TYPE_EXECUTABLE                        );
PS_SYSTEM_TYPE(function         , "#FUNCTION"         , PS_TYPE_EXECUTABLE, PS_TYPE_EXECUTABLE                        );

/**********************************************************************************************************************/
/* CONSTANTS                                                                                                          */
/**********************************************************************************************************************/

/* clang-format on */
#define PS_SYSTEM_CONSTANT(TYPE, VALUE, NAME, FIELD, VALUE2)                                     \
    ps_value ps_value_##TYPE##_##VALUE = {.type = &ps_system_##TYPE, .data = {.FIELD = VALUE2}}; \
    ps_symbol ps_system_constant_##TYPE##_##VALUE = {.kind = PS_SYMBOL_KIND_CONSTANT,            \
                                                     .name = NAME,                               \
                                                     .value = &ps_value_##TYPE##_##VALUE,        \
                                                     .system = true,                             \
                                                     .allocated = false}
/* clang-format off */

PS_SYSTEM_CONSTANT(boolean , false  , "FALSE"  , b, (ps_boolean)false                                                 );
PS_SYSTEM_CONSTANT(boolean , true   , "TRUE"   , b, (ps_boolean)true                                                  );
PS_SYSTEM_CONSTANT(integer , maxint , "MAXINT" , i, (ps_integer)PS_INTEGER_MAX                                        );
PS_SYSTEM_CONSTANT(integer , minint , "MININT" , i, (ps_integer)PS_INTEGER_MIN                                        );
PS_SYSTEM_CONSTANT(unsigned, maxuint, "MAXUINT", u, (ps_unsigned)PS_UNSIGNED_MAX                                      );
PS_SYSTEM_CONSTANT(real    , maxreal, "MAXREAL", r, (ps_real)PS_REAL_MAX                                              );
PS_SYSTEM_CONSTANT(real    , minreal, "MINREAL", r, (ps_real)PS_REAL_MIN                                              );
PS_SYSTEM_CONSTANT(real    , epsreal, "EPSREAL", r, (ps_real)PS_REAL_EPSILON                                          );
// does not work with (ps_real)M_PI...
PS_SYSTEM_CONSTANT(real    , pi     , "PI"     , r, (ps_real)3.14159265358979323846                                   );

/**********************************************************************************************************************/
/* VARIABLES                                                                                                          */
/**********************************************************************************************************************/

/* clang-format on */
#define PS_SYSTEM_VARIABLE(TYPE, VALUE, NAME, FIELD, VALUE2)                                     \
    ps_value ps_value_##TYPE##_##VALUE = {.type = &ps_system_##TYPE, .data = {.FIELD = VALUE2}}; \
    ps_symbol ps_system_variable_##TYPE##_##VALUE = {.kind = PS_SYMBOL_KIND_VARIABLE,            \
                                                     .name = NAME,                               \
                                                     .value = &ps_value_##TYPE##_##VALUE,        \
                                                     .system = true,                             \
                                                     .allocated = false}
/* clang-format off */

PS_SYSTEM_VARIABLE(integer, ioresult, "IORESULT", i, (ps_integer)0                                                    );
PS_SYSTEM_VARIABLE(integer, exitcode, "EXITCODE", i, (ps_integer)0                                                    );

/**********************************************************************************************************************/
/* BITNESS & VERSION                                                                                                  */
/**********************************************************************************************************************/

PS_SYSTEM_CONSTANT(unsigned, ps_bitness      , "PS_BITNESS"      , u, PS_BITNESS                                      );
ps_string ps_version_string = {.max = 0, .len = 0, .str = "PS_VERSION"};
PS_SYSTEM_CONSTANT(string  , ps_version      , "PS_VERSION"      , s, &ps_version_string                              );

/**********************************************************************************************************************/
/* STANDARD + MATH LIBRARY                                                    */
/**********************************************************************************************************************/

/* clang-format on */
bool ps_system_init(ps_environment *system)
{
    /**************************************************************************/
    /* TYPES                                                                  */
    /**************************************************************************/

    // String: max length
    // ps_system_string.value->type->value->data.t->def.s.max = PS_STRING_MAX_LEN;
    // Enum: empty
    // ps_system_enum.value->type->value->data.t->def.e.count = 0;
    // ps_system_enum.value->type->value->data.t->def.e.values = NULL;
    // Subranges: "empty"
    // ps_system_subrange_char.value->data.t->def.g.c.min = '\0';
    // ps_system_subrange_char.value->data.t->def.g.c.max = '\0';
    // ps_system_subrange_integer.value->data.t->def.g.i.min = 0;
    // ps_system_subrange_integer.value->data.t->def.g.i.max = 0;
    // ps_system_subrange_unsigned.value->data.t->def.g.u.min = 0;
    // ps_system_subrange_unsigned.value->data.t->def.g.u.max = 0;
    // ps_system_subrange_enum.value->data.t->def.e.count = 0;
    // ps_system_subrange_enum.value->data.t->def.e.values = NULL;
    // Register the system types
    ADD_SYSTEM_SYMBOL(ps_system_none)
    ADD_SYSTEM_SYMBOL(ps_system_type_def)
    ADD_SYSTEM_SYMBOL(ps_system_boolean)
    ADD_SYSTEM_SYMBOL(ps_system_char)
    ADD_SYSTEM_SYMBOL(ps_system_integer)
    ADD_SYSTEM_SYMBOL(ps_system_unsigned)
    ADD_SYSTEM_SYMBOL(ps_system_real)
    ADD_SYSTEM_SYMBOL(ps_system_string)
    ADD_SYSTEM_SYMBOL(ps_system_procedure)
    ADD_SYSTEM_SYMBOL(ps_system_function)
    ADD_SYSTEM_SYMBOL(ps_system_subrange_char)
    ADD_SYSTEM_SYMBOL(ps_system_subrange_integer)
    ADD_SYSTEM_SYMBOL(ps_system_subrange_unsigned)
    ADD_SYSTEM_SYMBOL(ps_system_subrange_enum)
    ADD_SYSTEM_SYMBOL(ps_system_enum)
    ADD_SYSTEM_SYMBOL(ps_system_array)
    ADD_SYSTEM_SYMBOL(ps_system_record)

    /**************************************************************************/
    /* VARIABLES                                                              */
    /**************************************************************************/

    ADD_SYSTEM_SYMBOL(ps_system_variable_integer_exitcode)
    ADD_SYSTEM_SYMBOL(ps_system_variable_integer_ioresult)

    /**************************************************************************/
    /* CONSTANTS                                                              */
    /**************************************************************************/

    ADD_SYSTEM_SYMBOL(ps_system_constant_boolean_false)
    ADD_SYSTEM_SYMBOL(ps_system_constant_boolean_true)
    ADD_SYSTEM_SYMBOL(ps_system_constant_integer_maxint)
    ADD_SYSTEM_SYMBOL(ps_system_constant_integer_minint)
    ADD_SYSTEM_SYMBOL(ps_system_constant_unsigned_maxuint)
    ADD_SYSTEM_SYMBOL(ps_system_constant_real_maxreal)
    ADD_SYSTEM_SYMBOL(ps_system_constant_real_minreal)
    ADD_SYSTEM_SYMBOL(ps_system_constant_real_epsreal)
    ADD_SYSTEM_SYMBOL(ps_system_constant_real_pi)

    /**************************************************************************/
    /* BITNESS & VERSION                                                      */
    /**************************************************************************/

    ADD_SYSTEM_SYMBOL(ps_system_constant_unsigned_ps_bitness)
    ps_system_constant_string_ps_version.value->data.s->len = strlen(PS_VERSION);
    ps_system_constant_string_ps_version.value->data.s->max = strlen(PS_VERSION);
    ADD_SYSTEM_SYMBOL(ps_system_constant_string_ps_version)

#ifdef DEBUG_INIT
    ps_symbol_table_dump(NULL, "SYSTEM INIT", system->symbols);
#endif

    return true;

error:
    return false;
}

void ps_system_done(const ps_environment *system)
{
    (void)system;
}
