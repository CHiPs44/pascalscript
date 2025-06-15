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
#include "ps_value.h"
#include "ps_version.h"

/* clang-format off */

/******************************************************************************/
/* SYSTEM TYPE DEFINITIONS AND CONSTANTS                                      */
/******************************************************************************/

ps_type_definition  ps_type_def_type_def    = {.type = PS_TYPE_DEFINITION, .base = PS_TYPE_DEFINITION};
ps_value            ps_value_type_def       = {.type = &ps_type_def_type_def, .data = {.t = &ps_type_def_type_def}};
ps_symbol           ps_system_type_def      = {.kind = PS_SYMBOL_KIND_TYPE_DEFINITION, .name = "TYPE_DEF", .value = &ps_value_type_def};

// clang-format on

#define PS_SYSTEM_TYPE(__name__, __NAME__, __VALUE_TYPE__)                                                             \
    ps_type_definition ps_type_def_##__name__ = {.type = __VALUE_TYPE__, .base = __VALUE_TYPE__};                      \
    ps_value ps_value_##__name__ = {.type = &ps_type_def_type_def, .data = {.t = &ps_type_def_##__name__}};            \
    ps_symbol ps_system_##__name__ = {                                                                                 \
        .kind = PS_SYMBOL_KIND_TYPE_DEFINITION, .name = __NAME__, .value = &ps_value_##__name__};
// clang-format off

PS_SYSTEM_TYPE(none     , "#NONE"    , PS_TYPE_NONE    );
PS_SYSTEM_TYPE(integer  , "INTEGER"  , PS_TYPE_INTEGER );
PS_SYSTEM_TYPE(unsigned , "UNSIGNED" , PS_TYPE_UNSIGNED);
PS_SYSTEM_TYPE(boolean  , "BOOLEAN"  , PS_TYPE_BOOLEAN );
PS_SYSTEM_TYPE(char     , "CHAR"     , PS_TYPE_CHAR    );
PS_SYSTEM_TYPE(real     , "REAL"     , PS_TYPE_REAL    );
PS_SYSTEM_TYPE(string   , "STRING"   , PS_TYPE_STRING  );
PS_SYSTEM_TYPE(array    , "#ARRAY"   , PS_TYPE_ARRAY   );
PS_SYSTEM_TYPE(subrange , "#SUBRANGE", PS_TYPE_SUBRANGE);
PS_SYSTEM_TYPE(enum     , "#ENUM"    , PS_TYPE_ENUM    );
PS_SYSTEM_TYPE(procedure, "PROCEDURE", PS_TYPE_NONE    );
PS_SYSTEM_TYPE(function , "FUNCTION" , PS_TYPE_NONE    );

/* CONSTANTS */

// clang-format on
#define PS_SYSTEM_CONSTANT(TYPE, VALUE, NAME, FIELD, VALUE2)                                                           \
    ps_value ps_value_##TYPE##_##VALUE = {.type = &ps_type_def_##TYPE, .data = {.FIELD = VALUE2}};                     \
    ps_symbol ps_system_constant_##TYPE##_##VALUE = {                                                                  \
        .kind = PS_SYMBOL_KIND_CONSTANT, .name = NAME, .value = &ps_value_##TYPE##_##VALUE};
// clang-format off

PS_SYSTEM_CONSTANT(boolean , false  , "FALSE"  , b, (ps_boolean)false                                          );
PS_SYSTEM_CONSTANT(boolean , true   , "TRUE"   , b, (ps_boolean)true                                           );
PS_SYSTEM_CONSTANT(integer , maxint , "MAXINT" , i, (ps_integer)PS_INTEGER_MAX                                 );
PS_SYSTEM_CONSTANT(integer , minint , "MININT" , i, (ps_integer)PS_INTEGER_MIN                                 );
PS_SYSTEM_CONSTANT(unsigned, maxuint, "MAXUINT", u, (ps_unsigned)PS_UNSIGNED_MAX                               );
PS_SYSTEM_CONSTANT(real    , maxreal, "MAXREAL", r, (ps_real)PS_REAL_MAX                                       );
PS_SYSTEM_CONSTANT(real    , minreal, "MINREAL", r, (ps_real)PS_REAL_MIN                                       );
PS_SYSTEM_CONSTANT(real    , epsreal, "EPSREAL", r, (ps_real)PS_REAL_EPSILON                                   );
PS_SYSTEM_CONSTANT(real    , pi     , "PI"     , r, (ps_real)3.141592653589793115997963468544185161590576171875);

/* BITNESS & VERSION */
PS_SYSTEM_CONSTANT(unsigned, ps_bitness      , "PS_BITNESS"      , u, PS_BITNESS      );
PS_SYSTEM_CONSTANT(unsigned, ps_version_major, "PS_VERSION_MAJOR", u, PS_VERSION_MAJOR);
PS_SYSTEM_CONSTANT(unsigned, ps_version_minor, "PS_VERSION_MINOR", u, PS_VERSION_MINOR);
PS_SYSTEM_CONSTANT(unsigned, ps_version_patch, "PS_VERSION_PATCH", u, PS_VERSION_PATCH);
PS_SYSTEM_CONSTANT(unsigned, ps_version_index, "PS_VERSION_INDEX", u, PS_VERSION_INDEX);
PS_SYSTEM_CONSTANT(string  , ps_version      , "PS_VERSION"      , s, NULL            );

/* STANDARD + MATH LIBRARY */

/* procedures */
ps_value ps_value_procedure_randomize   = {.type = &ps_type_def_procedure, .data = {.v = &ps_procedure_randomize}};
ps_value ps_value_procedure_read        = {.type = &ps_type_def_procedure, .data = {.v = &ps_procedure_read     }};
ps_value ps_value_procedure_readln      = {.type = &ps_type_def_procedure, .data = {.v = &ps_procedure_readln   }};
ps_value ps_value_procedure_write       = {.type = &ps_type_def_procedure, .data = {.v = &ps_procedure_write    }};
ps_value ps_value_procedure_writeln     = {.type = &ps_type_def_procedure, .data = {.v = &ps_procedure_writeln  }};
ps_symbol ps_system_procedure_randomize = {.kind = PS_SYMBOL_KIND_PROCEDURE, .name = "RANDOMIZE", .value = &ps_value_procedure_randomize};
ps_symbol ps_system_procedure_read      = {.kind = PS_SYMBOL_KIND_PROCEDURE, .name = "READ"     , .value = &ps_value_procedure_read     };
ps_symbol ps_system_procedure_readln    = {.kind = PS_SYMBOL_KIND_PROCEDURE, .name = "READLN"   , .value = &ps_value_procedure_readln   };
ps_symbol ps_system_procedure_write     = {.kind = PS_SYMBOL_KIND_PROCEDURE, .name = "WRITE"    , .value = &ps_value_procedure_write    };
ps_symbol ps_system_procedure_writeln   = {.kind = PS_SYMBOL_KIND_PROCEDURE, .name = "WRITELN"  , .value = &ps_value_procedure_writeln  };

/* ordinal types functions */
ps_value ps_value_function_chr    = {.type = &ps_type_def_function, .data = {.v = &ps_function_chr }};
ps_value ps_value_function_even   = {.type = &ps_type_def_function, .data = {.v = &ps_function_even}};
ps_value ps_value_function_odd    = {.type = &ps_type_def_function, .data = {.v = &ps_function_odd }};
ps_value ps_value_function_ord    = {.type = &ps_type_def_function, .data = {.v = &ps_function_ord }};
ps_value ps_value_function_pred   = {.type = &ps_type_def_function, .data = {.v = &ps_function_pred}};
ps_value ps_value_function_succ   = {.type = &ps_type_def_function, .data = {.v = &ps_function_succ}};
ps_symbol ps_system_function_chr  = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "CHR"    , .value = &ps_value_function_chr  };
ps_symbol ps_system_function_even = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "EVEN"   , .value = &ps_value_function_even };
ps_symbol ps_system_function_odd  = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "ODD"    , .value = &ps_value_function_odd  };
ps_symbol ps_system_function_ord  = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "ORD"    , .value = &ps_value_function_ord  };
ps_symbol ps_system_function_pred = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "PRED"   , .value = &ps_value_function_pred };
ps_symbol ps_system_function_succ = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "SUCC"   , .value = &ps_value_function_succ };
/* functions with zero or one integer/real argument returning integer/real type */
ps_value ps_value_function_random       = {.type = &ps_type_def_function, .data = {.v = &ps_function_random}};
ps_symbol ps_system_function_random   = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "RANDOM" , .value = &ps_value_function_random};
/* functions with one integer/real argument returning integer/real type */
ps_value ps_value_function_abs        = {.type = &ps_type_def_function, .data = {.v = &ps_function_abs    }};
ps_value ps_value_function_frac       = {.type = &ps_type_def_function, .data = {.v = &ps_function_frac   }};
ps_value ps_value_function_int        = {.type = &ps_type_def_function, .data = {.v = &ps_function_int    }};
ps_value ps_value_function_round      = {.type = &ps_type_def_function, .data = {.v = &ps_function_round  }};
ps_value ps_value_function_trunc      = {.type = &ps_type_def_function, .data = {.v = &ps_function_trunc  }};
ps_symbol ps_system_function_abs      = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "ABS"    , .value = &ps_value_function_abs   };
ps_symbol ps_system_function_frac     = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "FRAC"   , .value = &ps_value_function_frac  };
ps_symbol ps_system_function_int      = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "INT"    , .value = &ps_value_function_int   };
ps_symbol ps_system_function_round    = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "ROUND"  , .value = &ps_value_function_round };
ps_symbol ps_system_function_trunc    = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "TRUNC"  , .value = &ps_value_function_trunc };
/* functions with one real argument returning real type */
ps_value ps_value_function_arctan     = {.type = &ps_type_def_function, .data = {.v = &ps_function_arctan }};
ps_value ps_value_function_cos        = {.type = &ps_type_def_function, .data = {.v = &ps_function_cos    }};
ps_value ps_value_function_exp        = {.type = &ps_type_def_function, .data = {.v = &ps_function_exp    }};
ps_value ps_value_function_ln         = {.type = &ps_type_def_function, .data = {.v = &ps_function_ln     }};
ps_value ps_value_function_log        = {.type = &ps_type_def_function, .data = {.v = &ps_function_log    }};
ps_value ps_value_function_sin        = {.type = &ps_type_def_function, .data = {.v = &ps_function_sin    }};
ps_value ps_value_function_sqr        = {.type = &ps_type_def_function, .data = {.v = &ps_function_sqr    }};
ps_value ps_value_function_sqrt       = {.type = &ps_type_def_function, .data = {.v = &ps_function_sqrt   }};
ps_value ps_value_function_tan        = {.type = &ps_type_def_function, .data = {.v = &ps_function_tan    }};
ps_symbol ps_system_function_arctan   = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "ARCTAN" , .value = &ps_value_function_arctan};
ps_symbol ps_system_function_cos      = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "COS"    , .value = &ps_value_function_cos   };
ps_symbol ps_system_function_exp      = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "EXP"    , .value = &ps_value_function_exp   };
ps_symbol ps_system_function_ln       = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "LN"     , .value = &ps_value_function_ln    };
ps_symbol ps_system_function_log      = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "LOG"    , .value = &ps_value_function_log   };
ps_symbol ps_system_function_sin      = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "SIN"    , .value = &ps_value_function_sin   };
ps_symbol ps_system_function_sqr      = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "SQR"    , .value = &ps_value_function_sqr   };
ps_symbol ps_system_function_sqrt     = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "SQRT"   , .value = &ps_value_function_sqrt  };
ps_symbol ps_system_function_tan      = {.kind = PS_SYMBOL_KIND_FUNCTION , .name = "TAN"    , .value = &ps_value_function_tan   };

/* clang-format on */

ps_environment *ps_system_init()
{
    ps_environment *environment = ps_environment_init(NULL, PS_ENVIRONMENT_TYPE_SYSTEM, "SYSTEM", 64);
    if (environment == NULL)
        return NULL;

    /**************************************************************************/
    /* TYPES                                                                  */
    /**************************************************************************/

    // Array: empty
    ps_system_array.value->type->def.def_array.count = 0;
    ps_system_array.value->type->def.def_array.range = NULL;
    ps_system_array.value->type->def.def_array.type_def = NULL;
    // String: max length
    ps_system_string.value->type->def.def_string.max = PS_STRING_MAX_LEN;
    // Subrange: 0..0 as integers
    ps_system_subrange.value->type->def.def_subrange.base = PS_TYPE_INTEGER;
    ps_system_subrange.value->type->def.def_subrange.max.type = &ps_system_integer;
    ps_system_subrange.value->type->def.def_subrange.max.data.i = 0;
    ps_system_subrange.value->type->def.def_subrange.min.type = &ps_system_integer;
    ps_system_subrange.value->type->def.def_subrange.min.data.i = 0;
    // Enum: empty
    ps_system_enum.value->type->def.def_enum.count = 0;
    ps_system_enum.value->type->def.def_enum.values = NULL;
    // Register the system types
    ps_environment_add_symbol(environment, &ps_system_none);
    ps_environment_add_symbol(environment, &ps_system_type_def);
    ps_environment_add_symbol(environment, &ps_system_boolean);
    ps_environment_add_symbol(environment, &ps_system_char);
    ps_environment_add_symbol(environment, &ps_system_integer);
    ps_environment_add_symbol(environment, &ps_system_unsigned);
    ps_environment_add_symbol(environment, &ps_system_real);
    ps_environment_add_symbol(environment, &ps_system_string);
    ps_environment_add_symbol(environment, &ps_system_subrange);
    ps_environment_add_symbol(environment, &ps_system_enum);
    ps_environment_add_symbol(environment, &ps_system_array);
    ps_environment_add_symbol(environment, &ps_system_procedure);
    ps_environment_add_symbol(environment, &ps_system_function);

    /**************************************************************************/
    /* CONSTANTS                                                              */
    /**************************************************************************/

    ps_environment_add_symbol(environment, &ps_system_constant_boolean_false);
    ps_environment_add_symbol(environment, &ps_system_constant_boolean_true);
    ps_environment_add_symbol(environment, &ps_system_constant_integer_maxint);
    ps_environment_add_symbol(environment, &ps_system_constant_integer_minint);
    ps_environment_add_symbol(environment, &ps_system_constant_unsigned_maxuint);
    ps_environment_add_symbol(environment, &ps_system_constant_real_maxreal);
    ps_environment_add_symbol(environment, &ps_system_constant_real_minreal);
    ps_environment_add_symbol(environment, &ps_system_constant_real_epsreal);
    ps_environment_add_symbol(environment, &ps_system_constant_real_pi);

    /**************************************************************************/
    /* BITNESS & VERSION                                                      */
    /**************************************************************************/

    char version[16]; // from 0.0.0.0 (7) to 255.255.255.255 (15)
    snprintf(version, sizeof(version) - 1, "%d.%d.%d.%d", (uint8_t)PS_VERSION_MAJOR, (uint8_t)PS_VERSION_MINOR,
             (uint8_t)PS_VERSION_PATCH, (uint8_t)PS_VERSION_INDEX);
    ps_string *ps_version_string = ps_string_create(version);
    if (ps_version_string == NULL)
        return false;
    ps_system_constant_string_ps_version.value->data.s = ps_version_string;
    ps_environment_add_symbol(environment, &ps_system_constant_unsigned_ps_bitness);
    ps_environment_add_symbol(environment, &ps_system_constant_unsigned_ps_version_major);
    ps_environment_add_symbol(environment, &ps_system_constant_unsigned_ps_version_minor);
    ps_environment_add_symbol(environment, &ps_system_constant_unsigned_ps_version_patch);
    ps_environment_add_symbol(environment, &ps_system_constant_unsigned_ps_version_index);
    ps_environment_add_symbol(environment, &ps_system_constant_string_ps_version);

    /**************************************************************************/
    /* STANDARD PROCEDURES & FUNCTIONS                                        */
    /**************************************************************************/

    ps_environment_add_symbol(environment, &ps_system_function_abs);
    ps_environment_add_symbol(environment, &ps_system_function_arctan);
    ps_environment_add_symbol(environment, &ps_system_function_chr);
    ps_environment_add_symbol(environment, &ps_system_function_cos);
    ps_environment_add_symbol(environment, &ps_system_function_even);
    ps_environment_add_symbol(environment, &ps_system_function_exp);
    ps_environment_add_symbol(environment, &ps_system_function_frac);
    ps_environment_add_symbol(environment, &ps_system_function_int);
    ps_environment_add_symbol(environment, &ps_system_function_ln);
    ps_environment_add_symbol(environment, &ps_system_function_log);
    ps_environment_add_symbol(environment, &ps_system_function_odd);
    ps_environment_add_symbol(environment, &ps_system_function_ord);
    ps_environment_add_symbol(environment, &ps_system_function_pred);
    ps_environment_add_symbol(environment, &ps_system_function_random);
    ps_environment_add_symbol(environment, &ps_system_function_round);
    ps_environment_add_symbol(environment, &ps_system_function_sin);
    ps_environment_add_symbol(environment, &ps_system_function_sqr);
    ps_environment_add_symbol(environment, &ps_system_function_sqrt);
    ps_environment_add_symbol(environment, &ps_system_function_succ);
    ps_environment_add_symbol(environment, &ps_system_function_tan);
    ps_environment_add_symbol(environment, &ps_system_function_trunc);
    ps_environment_add_symbol(environment, &ps_system_procedure_randomize);
    ps_environment_add_symbol(environment, &ps_system_procedure_read);
    ps_environment_add_symbol(environment, &ps_system_procedure_readln);
    ps_environment_add_symbol(environment, &ps_system_procedure_write);
    ps_environment_add_symbol(environment, &ps_system_procedure_writeln);

    return true;
}

void ps_system_done(ps_interpreter *interpreter)
{
    if (ps_system_constant_string_ps_version.value->data.s != NULL)
    {
        ps_string_free(ps_system_constant_string_ps_version.value->data.s);
        ps_system_constant_string_ps_version.value->data.s = NULL;
    }
}
