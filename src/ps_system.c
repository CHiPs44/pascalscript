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
#define PS_SYSTEM_TYPE(__name__, __NAME__, __VALUE_TYPE__)                                                             \
    ps_type_definition ps_type_def_##__name__ = {.type = __VALUE_TYPE__, .base = __VALUE_TYPE__};                      \
    ps_value ps_value_##__name__ = {                                                                                   \
        .allocated = false, .type = &ps_symbol_type_def, .data = {.t = &ps_type_def_##__name__}};                      \
    ps_symbol ps_system_##__name__ = {.kind = PS_SYMBOL_KIND_TYPE_DEFINITION,                                          \
                                      .name = __NAME__,                                                                \
                                      .value = &ps_value_##__name__,                                                   \
                                      .system = true,                                                                  \
                                      .allocated = false}
/* clang-format off */

PS_SYSTEM_TYPE(none     , "#NONE"     , PS_TYPE_NONE                                                                  );
PS_SYSTEM_TYPE(integer  , "INTEGER"   , PS_TYPE_INTEGER                                                               );
PS_SYSTEM_TYPE(unsigned , "UNSIGNED"  , PS_TYPE_UNSIGNED                                                              );
PS_SYSTEM_TYPE(boolean  , "BOOLEAN"   , PS_TYPE_BOOLEAN                                                               );
PS_SYSTEM_TYPE(char     , "CHAR"      , PS_TYPE_CHAR                                                                  );
PS_SYSTEM_TYPE(real     , "REAL"      , PS_TYPE_REAL                                                                  );
PS_SYSTEM_TYPE(string   , "STRING"    , PS_TYPE_STRING                                                                );
PS_SYSTEM_TYPE(array    , "#ARRAY"    , PS_TYPE_ARRAY                                                                 );
PS_SYSTEM_TYPE(subrange , "#SUBRANGE" , PS_TYPE_SUBRANGE                                                              );
PS_SYSTEM_TYPE(enum     , "#ENUM"     , PS_TYPE_ENUM                                                                  );
PS_SYSTEM_TYPE(record   , "#RECORD"   , PS_TYPE_RECORD                                                                );
PS_SYSTEM_TYPE(procedure, "#PROCEDURE", PS_TYPE_EXECUTABLE                                                            );
PS_SYSTEM_TYPE(function , "#FUNCTION" , PS_TYPE_EXECUTABLE                                                            );

/**********************************************************************************************************************/
/* CONSTANTS                                                                                                          */
/**********************************************************************************************************************/

/* clang-format on */
#define PS_SYSTEM_CONSTANT(TYPE, VALUE, NAME, FIELD, VALUE2)                                                           \
    ps_value ps_value_##TYPE##_##VALUE = {.type = &ps_system_##TYPE, .data = {.FIELD = VALUE2}};                       \
    ps_symbol ps_system_constant_##TYPE##_##VALUE = {.kind = PS_SYMBOL_KIND_CONSTANT,                                  \
                                                     .name = NAME,                                                     \
                                                     .value = &ps_value_##TYPE##_##VALUE,                              \
                                                     .system = true,                                                   \
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
PS_SYSTEM_CONSTANT(real    , pi     , "PI"     , r, (ps_real)M_PI                                                     );

/**********************************************************************************************************************/
/* VARIABLES                                                                                                          */
/**********************************************************************************************************************/

/* clang-format on */
#define PS_SYSTEM_VARIABLE(TYPE, VALUE, NAME, FIELD, VALUE2)                                                           \
    ps_value ps_value_##TYPE##_##VALUE = {.type = &ps_system_##TYPE, .data = {.FIELD = VALUE2}};                       \
    ps_symbol ps_system_variable_##TYPE##_##VALUE = {.kind = PS_SYMBOL_KIND_VARIABLE,                                  \
                                                     .name = NAME,                                                     \
                                                     .value = &ps_value_##TYPE##_##VALUE,                              \
                                                     .system = true,                                                   \
                                                     .allocated = false}
/* clang-format off */

PS_SYSTEM_VARIABLE(integer, ioresult, "IORESULT", i, (ps_integer)0                                                    );
PS_SYSTEM_VARIABLE(integer, exitcode, "EXITCODE", i, (ps_integer)0                                                    );

/**********************************************************************************************************************/
/* BITNESS & VERSION                                                                                                  */
/**********************************************************************************************************************/

PS_SYSTEM_CONSTANT(unsigned, ps_bitness      , "PS_BITNESS"      , u, PS_BITNESS                                      );
// PS_SYSTEM_CONSTANT(unsigned, ps_version_major, "PS_VERSION_MAJOR", u, PS_VERSION_MAJOR                                );
// PS_SYSTEM_CONSTANT(unsigned, ps_version_minor, "PS_VERSION_MINOR", u, PS_VERSION_MINOR                                );
// PS_SYSTEM_CONSTANT(unsigned, ps_version_patch, "PS_VERSION_PATCH", u, PS_VERSION_PATCH                                );
// PS_SYSTEM_CONSTANT(unsigned, ps_version_index, "PS_VERSION_INDEX", u, PS_VERSION_INDEX                                );
PS_SYSTEM_CONSTANT(string  , ps_version      , "PS_VERSION"      , s, NULL                                            );

/**********************************************************************************************************************/
/* STANDARD + MATH LIBRARY                                                    */
/**********************************************************************************************************************/

/* clang-format on */
#define PS_SYSTEM_FUNCTION(TYPE, VALUE, NAME, CALLABLE_FIELD, CALLABLE)                                                \
    ps_executable ps_executable_##TYPE##_##VALUE = {CALLABLE_FIELD = CALLABLE, .formal_signature = NULL, .line = 0,    \
                                                    .column = 0};                                                      \
    ps_value ps_value_##TYPE##_##VALUE = {.type = &ps_system_##TYPE, .data = {.x = &ps_executable_##TYPE##_##VALUE}};  \
    ps_symbol ps_system_##TYPE##_##VALUE = {.kind = PS_SYMBOL_KIND_FUNCTION,                                           \
                                            .name = NAME,                                                              \
                                            .value = &ps_value_##TYPE##_##VALUE,                                       \
                                            .system = true,                                                            \
                                            .allocated = false}
#define PS_SYSTEM_PROCEDURE(TYPE, VALUE, NAME, CALLABLE_FIELD, CALLABLE)                                               \
    ps_executable ps_executable_##TYPE##_##VALUE = {CALLABLE_FIELD = CALLABLE, .formal_signature = NULL, .line = 0,    \
                                                    .column = 0};                                                      \
    ps_value ps_value_##TYPE##_##VALUE = {.type = &ps_system_##TYPE, .data = {.x = &ps_executable_##TYPE##_##VALUE}};  \
    ps_symbol ps_system_##TYPE##_##VALUE = {.kind = PS_SYMBOL_KIND_PROCEDURE,                                          \
                                            .name = NAME,                                                              \
                                            .value = &ps_value_##TYPE##_##VALUE,                                       \
                                            .system = true,                                                            \
                                            .allocated = false}
/* clang-format off */

PS_SYSTEM_FUNCTION (function , abs           , "ABS"         , .func_1arg      , &ps_function_abs               );
PS_SYSTEM_FUNCTION (function , arctan        , "ARCTAN"      , .func_1arg      , &ps_function_arctan            );
PS_SYSTEM_FUNCTION (function , chr           , "CHR"         , .func_1arg      , &ps_function_chr               );
PS_SYSTEM_FUNCTION (function , cos           , "COS"         , .func_1arg      , &ps_function_cos               );
PS_SYSTEM_FUNCTION (function , even          , "EVEN"        , .func_1arg      , &ps_function_even              );
PS_SYSTEM_FUNCTION (function , exp           , "EXP"         , .func_1arg      , &ps_function_exp               );
PS_SYSTEM_FUNCTION (function , frac          , "FRAC"        , .func_1arg      , &ps_function_frac              );
PS_SYSTEM_FUNCTION (function , get_tick_count, "GETTICKCOUNT", .func_1arg      , &ps_function_get_tick_count    );
PS_SYSTEM_FUNCTION (function , int           , "INT"         , .func_1arg      , &ps_function_int               );
PS_SYSTEM_FUNCTION (function , length        , "LENGTH"      , .func_1arg      , &ps_function_length            );
PS_SYSTEM_FUNCTION (function , ln            , "LN"          , .func_1arg      , &ps_function_ln                );
PS_SYSTEM_FUNCTION (function , log           , "LOG"         , .func_1arg      , &ps_function_log               );
PS_SYSTEM_FUNCTION (function , lowercase     , "LOWERCASE"   , .func_1arg      , &ps_function_lowercase         );
PS_SYSTEM_FUNCTION (function , odd           , "ODD"         , .func_1arg      , &ps_function_odd               );
PS_SYSTEM_FUNCTION (function , ord           , "ORD"         , .func_1arg      , &ps_function_ord               );
PS_SYSTEM_FUNCTION (function , pred          , "PRED"        , .func_1arg      , &ps_function_pred              );
PS_SYSTEM_FUNCTION (function , random        , "RANDOM"      , .func_1arg      , &ps_function_random            );
PS_SYSTEM_FUNCTION (function , round         , "ROUND"       , .func_1arg      , &ps_function_round             );
PS_SYSTEM_FUNCTION (function , sin           , "SIN"         , .func_1arg      , &ps_function_sin               );
PS_SYSTEM_FUNCTION (function , sqr           , "SQR"         , .func_1arg      , &ps_function_sqr               );   
PS_SYSTEM_FUNCTION (function , sqrt          , "SQRT"        , .func_1arg      , &ps_function_sqrt              );
PS_SYSTEM_FUNCTION (function , succ          , "SUCC"        , .func_1arg      , &ps_function_succ              );
PS_SYSTEM_FUNCTION (function , tan           , "TAN"         , .func_1arg      , &ps_function_tan               );
PS_SYSTEM_FUNCTION (function , trunc         , "TRUNC"       , .func_1arg      , &ps_function_trunc             );
PS_SYSTEM_FUNCTION (function , uppercase     , "UPPERCASE"   , .func_1arg      , &ps_function_uppercase         );
PS_SYSTEM_PROCEDURE(procedure, randomize     , "RANDOMIZE"   , .proc_1arg      , &ps_procedure_randomize        );
PS_SYSTEM_PROCEDURE(procedure, read          , "READ"        , .proc_file_read , &ps_procedure_read             );
PS_SYSTEM_PROCEDURE(procedure, readln        , "READLN"      , .proc_file_read , &ps_procedure_readln           );
PS_SYSTEM_PROCEDURE(procedure, write         , "WRITE"       , .proc_file_write, &ps_procedure_write            );
PS_SYSTEM_PROCEDURE(procedure, writeln       , "WRITELN"     , .proc_file_write, &ps_procedure_writeln          );

/* clang-format on */

bool ps_system_init(ps_environment *system)
{
    bool error = false;

    /**************************************************************************/
    /* TYPES                                                                  */
    /**************************************************************************/

    // // Array: empty
    // ps_system_array.value->type->def.def_array.count = 0;
    // ps_system_array.value->type->def.def_array.range = NULL;
    // ps_system_array.value->type->def.def_array.type_def = NULL;
    // // String: max length
    // ps_system_string.value->type->def.def_string.max = PS_STRING_MAX_LEN;
    // // Subrange: 0..0 as integers
    // ps_system_subrange.value->type->def.def_subrange.def = &ps_type_def_integer;
    // ps_system_subrange.value->type->def.def_subrange.max.i = 0;
    // ps_system_subrange.value->type->def.def_subrange.min.i = 0;
    // // Enum: empty
    // ps_system_enum.value->type->def.def_enum.count = 0;
    // ps_system_enum.value->type->def.def_enum.values = NULL;
    // Register the system types
    error = error || !ps_environment_add_symbol(system, &ps_system_none);
    error = error || !ps_environment_add_symbol(system, &ps_system_type_def);
    error = error || !ps_environment_add_symbol(system, &ps_system_boolean);
    error = error || !ps_environment_add_symbol(system, &ps_system_char);
    error = error || !ps_environment_add_symbol(system, &ps_system_integer);
    error = error || !ps_environment_add_symbol(system, &ps_system_unsigned);
    error = error || !ps_environment_add_symbol(system, &ps_system_real);
    error = error || !ps_environment_add_symbol(system, &ps_system_string);
    error = error || !ps_environment_add_symbol(system, &ps_system_procedure);
    error = error || !ps_environment_add_symbol(system, &ps_system_function);
    error = error || !ps_environment_add_symbol(system, &ps_system_subrange);
    error = error || !ps_environment_add_symbol(system, &ps_system_enum);
    error = error || !ps_environment_add_symbol(system, &ps_system_array);
    error = error || !ps_environment_add_symbol(system, &ps_system_record);

    /**************************************************************************/
    /* VARIABLES                                                              */
    /**************************************************************************/

    error = error || !ps_environment_add_symbol(system, &ps_system_variable_integer_exitcode);
    error = error || !ps_environment_add_symbol(system, &ps_system_variable_integer_ioresult);

    /**************************************************************************/
    /* CONSTANTS                                                              */
    /**************************************************************************/

    error = error || !ps_environment_add_symbol(system, &ps_system_constant_boolean_false);
    error = error || !ps_environment_add_symbol(system, &ps_system_constant_boolean_true);
    error = error || !ps_environment_add_symbol(system, &ps_system_constant_integer_maxint);
    error = error || !ps_environment_add_symbol(system, &ps_system_constant_integer_minint);
    error = error || !ps_environment_add_symbol(system, &ps_system_constant_unsigned_maxuint);
    error = error || !ps_environment_add_symbol(system, &ps_system_constant_real_maxreal);
    error = error || !ps_environment_add_symbol(system, &ps_system_constant_real_minreal);
    error = error || !ps_environment_add_symbol(system, &ps_system_constant_real_epsreal);
    error = error || !ps_environment_add_symbol(system, &ps_system_constant_real_pi);

    /**************************************************************************/
    /* BITNESS & VERSION                                                      */
    /**************************************************************************/

    error = error || !ps_environment_add_symbol(system, &ps_system_constant_unsigned_ps_bitness);
    if (!error)
    {
        ps_string *ps_version_string = ps_string_create(PS_VERSION, strlen(PS_VERSION));
        if (ps_version_string == NULL)
            error = true;
        ps_system_constant_string_ps_version.value->data.s = ps_version_string;
    }
    // error = error || !ps_environment_add_symbol(system, &ps_system_constant_unsigned_ps_version_major);
    // error = error || !ps_environment_add_symbol(system, &ps_system_constant_unsigned_ps_version_minor);
    // error = error || !ps_environment_add_symbol(system, &ps_system_constant_unsigned_ps_version_patch);
    // error = error || !ps_environment_add_symbol(system, &ps_system_constant_unsigned_ps_version_index);
    error = error || !ps_environment_add_symbol(system, &ps_system_constant_string_ps_version);

    /**************************************************************************/
    /* STANDARD PROCEDURES & FUNCTIONS                                        */
    /**************************************************************************/

    error = error || !ps_environment_add_symbol(system, &ps_system_function_abs);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_arctan);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_chr);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_cos);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_even);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_exp);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_frac);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_get_tick_count);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_int);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_length);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_ln);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_log);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_lowercase);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_odd);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_ord);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_pred);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_random);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_round);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_sin);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_sqr);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_sqrt);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_succ);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_tan);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_trunc);
    error = error || !ps_environment_add_symbol(system, &ps_system_function_uppercase);
    error = error || !ps_environment_add_symbol(system, &ps_system_procedure_randomize);
    error = error || !ps_environment_add_symbol(system, &ps_system_procedure_read);
    error = error || !ps_environment_add_symbol(system, &ps_system_procedure_readln);
    error = error || !ps_environment_add_symbol(system, &ps_system_procedure_write);
    error = error || !ps_environment_add_symbol(system, &ps_system_procedure_writeln);

    // ps_symbol_table_dump(NULL, "SYSTEM INIT", system->symbols);

    if (error)
    {
        ps_system_done(system);
        return false;
    }

    return true;
}

void ps_system_done(ps_environment *system)
{
    (void)system;
    if (ps_system_constant_string_ps_version.value->data.s != NULL)
    {
        ps_string_free(ps_system_constant_string_ps_version.value->data.s);
        ps_system_constant_string_ps_version.value->data.s = NULL;
    }
}
