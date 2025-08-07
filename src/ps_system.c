/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_system.h"
#include "ps_environment.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_procedures.h"
#include "ps_signature.h"
#include "ps_string.h"
#include "ps_symbol.h"
#include "ps_system_types.h"
#include "ps_type_definition.h"
#include "ps_value.h"
#include "ps_version.h"

/**********************************************************************************************************************/
/* SYSTEM TYPE DEFINITIONS AND CONSTANTS                                      */
/**********************************************************************************************************************/

#define PS_SYSTEM_TYPE(__name__, __NAME__, __VALUE_TYPE__)                                                             \
    ps_type_definition ps_type_def_##__name__ = {.type = __VALUE_TYPE__, .base = __VALUE_TYPE__};                      \
    ps_value ps_value_##__name__ = {.type = &ps_type_def_type_def, .data = {.t = &ps_type_def_##__name__}};            \
    ps_symbol ps_system_##__name__ = {                                                                                 \
        .kind = PS_SYMBOL_KIND_TYPE_DEFINITION, .name = __NAME__, .value = &ps_value_##__name__};

/* clang-format off */

ps_type_definition ps_type_def_type_def = {.type = PS_TYPE_DEFINITION, .base = PS_TYPE_DEFINITION                                 };
ps_value           ps_value_type_def    = {.type = &ps_type_def_type_def, .data = {.t = &ps_type_def_type_def}                    };
ps_symbol          ps_system_type_def   = {.kind = PS_SYMBOL_KIND_TYPE_DEFINITION, .name = "TYPE_DEF", .value = &ps_value_type_def};

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
PS_SYSTEM_TYPE(procedure, "#PROCEDURE", PS_TYPE_EXECUTABLE                                                            );
PS_SYSTEM_TYPE(function , "#FUNCTION" , PS_TYPE_EXECUTABLE                                                            );

/**********************************************************************************************************************/
/* CONSTANTS                                                                                                          */
/**********************************************************************************************************************/

/* clang-format on */
#define PS_SYSTEM_CONSTANT(TYPE, VALUE, NAME, FIELD, VALUE2)                                                           \
    ps_value ps_value_##TYPE##_##VALUE = {.type = &ps_type_def_##TYPE, .data = {.FIELD = VALUE2}};                     \
    ps_symbol ps_system_constant_##TYPE##_##VALUE = {                                                                  \
        .kind = PS_SYMBOL_KIND_CONSTANT, .name = NAME, .value = &ps_value_##TYPE##_##VALUE};
/* clang-format off */

PS_SYSTEM_CONSTANT(boolean , false  , "FALSE"  , b, (ps_boolean)false                                                 );
PS_SYSTEM_CONSTANT(boolean , true   , "TRUE"   , b, (ps_boolean)true                                                  );
PS_SYSTEM_CONSTANT(integer , maxint , "MAXINT" , i, (ps_integer)PS_INTEGER_MAX                                        );
PS_SYSTEM_CONSTANT(integer , minint , "MININT" , i, (ps_integer)PS_INTEGER_MIN                                        );
PS_SYSTEM_CONSTANT(unsigned, maxuint, "MAXUINT", u, (ps_unsigned)PS_UNSIGNED_MAX                                      );
PS_SYSTEM_CONSTANT(real    , maxreal, "MAXREAL", r, (ps_real)PS_REAL_MAX                                              );
PS_SYSTEM_CONSTANT(real    , minreal, "MINREAL", r, (ps_real)PS_REAL_MIN                                              );
PS_SYSTEM_CONSTANT(real    , epsreal, "EPSREAL", r, (ps_real)PS_REAL_EPSILON                                          );
PS_SYSTEM_CONSTANT(real    , pi     , "PI"     , r, (ps_real)3.141592653589793115997963468544185161590576171875       );

/**********************************************************************************************************************/
/* VARIABLES                                                                                                          */
/**********************************************************************************************************************/

/* clang-format on */
#define PS_SYSTEM_VARIABLE(TYPE, VALUE, NAME, FIELD, VALUE2)                                                           \
    ps_value ps_value_##TYPE##_##VALUE = {.type = &ps_type_def_##TYPE, .data = {.FIELD = VALUE2}};                     \
    ps_symbol ps_system_variable_##TYPE##_##VALUE = {                                                                  \
        .kind = PS_SYMBOL_KIND_VARIABLE, .name = NAME, .value = &ps_value_##TYPE##_##VALUE};
/* clang-format off */

PS_SYSTEM_VARIABLE(integer, ioresult, "IORESULT", i, (ps_integer)0                                                    );
PS_SYSTEM_VARIABLE(integer, exitcode, "EXITCODE", i, (ps_integer)0                                                    );

/**********************************************************************************************************************/
/* BITNESS & VERSION                                                                                                  */
/**********************************************************************************************************************/

PS_SYSTEM_CONSTANT(unsigned, ps_bitness      , "PS_BITNESS"      , u, PS_BITNESS                                      );
PS_SYSTEM_CONSTANT(unsigned, ps_version_major, "PS_VERSION_MAJOR", u, PS_VERSION_MAJOR                                );
PS_SYSTEM_CONSTANT(unsigned, ps_version_minor, "PS_VERSION_MINOR", u, PS_VERSION_MINOR                                );
PS_SYSTEM_CONSTANT(unsigned, ps_version_patch, "PS_VERSION_PATCH", u, PS_VERSION_PATCH                                );
PS_SYSTEM_CONSTANT(unsigned, ps_version_index, "PS_VERSION_INDEX", u, PS_VERSION_INDEX                                );
PS_SYSTEM_CONSTANT(string  , ps_version      , "PS_VERSION"      , s, NULL                                            );

/**********************************************************************************************************************/
/* STANDARD + MATH LIBRARY                                                    */
/**********************************************************************************************************************/

/* clang-format on */
#define PS_SYSTEM_CALLABLE(TYPE, KIND, VALUE, NAME, FIELD, VALUE2)                                                     \
    ps_value ps_value_##TYPE##_##VALUE = {.type = &ps_type_def_##TYPE, .data = {.FIELD = VALUE2}};                     \
    ps_symbol ps_system_##TYPE##_##VALUE = {.kind = KIND, .name = NAME, .value = &ps_value_##TYPE##_##VALUE};

/* clang-format off */

PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , abs      , "ABS"      , v, &ps_function_abs                   );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , arctan   , "ARCTAN"   , v, &ps_function_arctan                );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , chr      , "CHR"      , v, &ps_function_chr                   );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , cos      , "COS"      , v, &ps_function_cos                   );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , even     , "EVEN"     , v, &ps_function_even                  );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , exp      , "EXP"      , v, &ps_function_exp                   );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , frac     , "FRAC"     , v, &ps_function_frac                  );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , int      , "INT"      , v, &ps_function_int                   );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , ln       , "LN"       , v, &ps_function_ln                    );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , log      , "LOG"      , v, &ps_function_log                   );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , odd      , "ODD"      , v, &ps_function_odd                   );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , ord      , "ORD"      , v, &ps_function_ord                   );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , pred     , "PRED"     , v, &ps_function_pred                  );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , random   , "RANDOM"   , v, &ps_function_random                );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , round    , "ROUND"    , v, &ps_function_round                 );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , sin      , "SIN"      , v, &ps_function_sin                   );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , sqr      , "SQR"      , v, &ps_function_sqr                   );   
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , sqrt     , "SQRT"     , v, &ps_function_sqrt                  );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , succ     , "SUCC"     , v, &ps_function_succ                  );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , tan      , "TAN"      , v, &ps_function_tan                   );
PS_SYSTEM_CALLABLE(function , PS_SYMBOL_KIND_FUNCTION , trunc    , "TRUNC"    , v, &ps_function_trunc                 );
PS_SYSTEM_CALLABLE(procedure, PS_SYMBOL_KIND_PROCEDURE, randomize, "RANDOMIZE", v, &ps_procedure_randomize            );
PS_SYSTEM_CALLABLE(procedure, PS_SYMBOL_KIND_PROCEDURE, read     , "READ"     , v, &ps_procedure_read                 );
PS_SYSTEM_CALLABLE(procedure, PS_SYMBOL_KIND_PROCEDURE, readln   , "READLN"   , v, &ps_procedure_readln               );
PS_SYSTEM_CALLABLE(procedure, PS_SYMBOL_KIND_PROCEDURE, write    , "WRITE"    , v, &ps_procedure_write                );
PS_SYSTEM_CALLABLE(procedure, PS_SYMBOL_KIND_PROCEDURE, writeln  , "WRITELN"  , v, &ps_procedure_writeln              );

/* clang-format on */

ps_environment *ps_system_init()
{
    bool error = false;
    ps_identifier system_name = "SYSTEM";
    ps_environment *environment = ps_environment_init(NULL, &system_name, 64);
    if (environment == NULL)
        error = true;

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
    error = error || !ps_environment_add_symbol(environment, &ps_system_none);
    error = error || !ps_environment_add_symbol(environment, &ps_system_type_def);
    error = error || !ps_environment_add_symbol(environment, &ps_system_boolean);
    error = error || !ps_environment_add_symbol(environment, &ps_system_char);
    error = error || !ps_environment_add_symbol(environment, &ps_system_integer);
    error = error || !ps_environment_add_symbol(environment, &ps_system_unsigned);
    error = error || !ps_environment_add_symbol(environment, &ps_system_real);
    error = error || !ps_environment_add_symbol(environment, &ps_system_string);
    // error = error || !ps_environment_add_symbol(environment, &ps_system_subrange);
    // error = error || !ps_environment_add_symbol(environment, &ps_system_enum);
    // error = error || !ps_environment_add_symbol(environment, &ps_system_array);
    error = error || !ps_environment_add_symbol(environment, &ps_system_procedure);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function);

    /**************************************************************************/
    /* VARIABLES                                                              */
    /**************************************************************************/

    error = error || !ps_environment_add_symbol(environment, &ps_system_variable_integer_exitcode);
    error = error || !ps_environment_add_symbol(environment, &ps_system_variable_integer_ioresult);

    /**************************************************************************/
    /* CONSTANTS                                                              */
    /**************************************************************************/

    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_boolean_false);
    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_boolean_true);
    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_integer_maxint);
    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_integer_minint);
    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_unsigned_maxuint);
    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_real_maxreal);
    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_real_minreal);
    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_real_epsreal);
    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_real_pi);

    /**************************************************************************/
    /* BITNESS & VERSION                                                      */
    /**************************************************************************/

    if (!error)
    {
        char version[16]; // from 0.0.0.0 (4+3=7) to 255.255.255.255 (12+3=15)
        int length = snprintf(version, sizeof(version) - 1, "%d.%d.%d.%d", (uint8_t)PS_VERSION_MAJOR,
                              (uint8_t)PS_VERSION_MINOR, (uint8_t)PS_VERSION_PATCH, (uint8_t)PS_VERSION_INDEX);
        ps_string *ps_version_string = ps_string_create(version, length);
        if (ps_version_string == NULL)
            error = true;
        ps_system_constant_string_ps_version.value->data.s = ps_version_string;
    }
    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_unsigned_ps_bitness);
    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_unsigned_ps_version_major);
    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_unsigned_ps_version_minor);
    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_unsigned_ps_version_patch);
    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_unsigned_ps_version_index);
    error = error || !ps_environment_add_symbol(environment, &ps_system_constant_string_ps_version);

    /**************************************************************************/
    /* STANDARD PROCEDURES & FUNCTIONS                                        */
    /**************************************************************************/

    error = error || !ps_environment_add_symbol(environment, &ps_system_procedure_read);
    error = error || !ps_environment_add_symbol(environment, &ps_system_procedure_readln);
    error = error || !ps_environment_add_symbol(environment, &ps_system_procedure_write);
    error = error || !ps_environment_add_symbol(environment, &ps_system_procedure_writeln);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_abs);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_arctan);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_cos);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_even);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_exp);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_frac);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_int);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_ln);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_log);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_odd);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_random);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_round);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_sin);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_sqr);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_sqrt);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_tan);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_trunc);
    error = error || !ps_environment_add_symbol(environment, &ps_system_procedure_randomize);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_chr);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_ord);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_pred);
    error = error || !ps_environment_add_symbol(environment, &ps_system_function_succ);

    if (error)
    {
        ps_environment_done(environment);
        ps_system_done();
        return NULL;
    }

    return environment;
}

void ps_system_done()
{
    if (ps_system_constant_string_ps_version.value->data.s != NULL)
    {
        ps_string_free(ps_system_constant_string_ps_version.value->data.s);
        ps_system_constant_string_ps_version.value->data.s = NULL;
    }
    // ps_environment_done(interpreter->environments[PS_INTERPRETER_ENVIRONMENT_SYSTEM]);
    // interpreter->environments[PS_INTERPRETER_ENVIRONMENT_SYSTEM] = NULL;
}
