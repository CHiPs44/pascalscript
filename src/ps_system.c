/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_system.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
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
ps_symbol           ps_system_type_def      = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_TYPE_DEFINITION, .name = "TYPE_DEF", .value = &ps_value_type_def};

#define PS_SYSTEM_TYPE(__name__, __NAME__, __VALUE_TYPE__)\
ps_type_definition  ps_type_def_##__name__    = {.type = __VALUE_TYPE__, .base = __VALUE_TYPE__}; \
ps_value            ps_value_##__name__       = {.type = &ps_type_def_type_def, .data = {.t = &ps_type_def_##__name__}}; \
ps_symbol           ps_system_##__name__      = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_TYPE_DEFINITION, .name = __NAME__, .value = &ps_value_##__name__};

PS_SYSTEM_TYPE(none     , "_N_O_N_E_", PS_TYPE_NONE      );
PS_SYSTEM_TYPE(boolean  , "BOOLEAN"  , PS_TYPE_BOOLEAN   );
PS_SYSTEM_TYPE(char     , "CHAR"     , PS_TYPE_CHAR      );
PS_SYSTEM_TYPE(integer  , "INTEGER"  , PS_TYPE_INTEGER   );
PS_SYSTEM_TYPE(unsigned , "UNSIGNED" , PS_TYPE_UNSIGNED  );
PS_SYSTEM_TYPE(real     , "REAL"     , PS_TYPE_REAL      );
PS_SYSTEM_TYPE(string   , "STRING"   , PS_TYPE_STRING    );
PS_SYSTEM_TYPE(procedure, "PROCEDURE", PS_TYPE_NONE      );
PS_SYSTEM_TYPE(function , "FUNCTION" , PS_TYPE_NONE      );

/* STANDARD + MATH LIBRARY */

/* procedures */
ps_symbol ps_system_procedure_read      = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_PROCEDURE, .name = "READ"     , .value = NULL};
ps_symbol ps_system_procedure_readln    = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_PROCEDURE, .name = "READLN"   , .value = NULL};
ps_symbol ps_system_procedure_write     = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_PROCEDURE, .name = "WRITE"    , .value = NULL};
ps_symbol ps_system_procedure_writeln   = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_PROCEDURE, .name = "WRITELN"  , .value = NULL};
ps_symbol ps_system_procedure_randomize = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_PROCEDURE, .name = "RANDOMIZE", .value = NULL};

/* ordinal types functions */
ps_value ps_value_function_odd        = {.type = &ps_type_def_function, .data = {.v = &ps_function_odd }};
ps_value ps_value_function_even       = {.type = &ps_type_def_function, .data = {.v = &ps_function_even}};
ps_value ps_value_function_chr        = {.type = &ps_type_def_function, .data = {.v = &ps_function_chr }};
ps_value ps_value_function_ord        = {.type = &ps_type_def_function, .data = {.v = &ps_function_ord }};
ps_value ps_value_function_succ       = {.type = &ps_type_def_function, .data = {.v = &ps_function_succ}};
ps_value ps_value_function_pred       = {.type = &ps_type_def_function, .data = {.v = &ps_function_pred}};
ps_symbol ps_system_function_odd      = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "ODD"    , .value = &ps_value_function_odd  };
ps_symbol ps_system_function_even     = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "EVEN"   , .value = &ps_value_function_even };
ps_symbol ps_system_function_chr      = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "CHR"    , .value = &ps_value_function_chr  };
ps_symbol ps_system_function_ord      = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "ORD"    , .value = &ps_value_function_ord  };
ps_symbol ps_system_function_succ     = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "SUCC"   , .value = &ps_value_function_succ };
ps_symbol ps_system_function_pred     = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "PRED"   , .value = &ps_value_function_pred };
/* functions with zero or one integer/real argument returning integer/real type */
ps_value ps_value_function_random       = {.type = &ps_type_def_function, .data = {.v = &ps_function_random}};
ps_symbol ps_system_function_random   = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "RANDOM" , .value = &ps_value_function_random};
/* functions with one integer/real argument returning integer/real type */
ps_value ps_value_function_abs        = {.type = &ps_type_def_function, .data = {.v = &ps_function_abs    }};
ps_value ps_value_function_trunc      = {.type = &ps_type_def_function, .data = {.v = &ps_function_trunc  }};
ps_value ps_value_function_round      = {.type = &ps_type_def_function, .data = {.v = &ps_function_round  }};
ps_value ps_value_function_int        = {.type = &ps_type_def_function, .data = {.v = &ps_function_int    }};
ps_value ps_value_function_frac       = {.type = &ps_type_def_function, .data = {.v = &ps_function_frac   }};
/* functions with one real argument returning real type */
ps_value ps_value_function_sin        = {.type = &ps_type_def_function, .data = {.v = &ps_function_sin    }};
ps_value ps_value_function_cos        = {.type = &ps_type_def_function, .data = {.v = &ps_function_cos    }};
ps_value ps_value_function_arctan     = {.type = &ps_type_def_function, .data = {.v = &ps_function_arctan }};
ps_value ps_value_function_sqr        = {.type = &ps_type_def_function, .data = {.v = &ps_function_sqr    }};
ps_value ps_value_function_sqrt       = {.type = &ps_type_def_function, .data = {.v = &ps_function_sqrt   }};
ps_value ps_value_function_exp        = {.type = &ps_type_def_function, .data = {.v = &ps_function_exp    }};
ps_value ps_value_function_ln         = {.type = &ps_type_def_function, .data = {.v = &ps_function_ln     }};
ps_value ps_value_function_log        = {.type = &ps_type_def_function, .data = {.v = &ps_function_log    }};
ps_symbol ps_system_function_abs      = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "ABS"    , .value = &ps_value_function_abs   };
ps_symbol ps_system_function_trunc    = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "TRUNC"  , .value = &ps_value_function_trunc };
ps_symbol ps_system_function_round    = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "ROUND"  , .value = &ps_value_function_round };
ps_symbol ps_system_function_int      = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "INT"    , .value = &ps_value_function_int   };
ps_symbol ps_system_function_frac     = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "FRAC"   , .value = &ps_value_function_frac  };
ps_symbol ps_system_function_sin      = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "SIN"    , .value = &ps_value_function_sin   };
ps_symbol ps_system_function_cos      = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "COS"    , .value = &ps_value_function_cos   };
ps_symbol ps_system_function_arctan   = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "ARCTAN" , .value = &ps_value_function_arctan};
ps_symbol ps_system_function_sqr      = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "SQR"    , .value = &ps_value_function_sqr   };
ps_symbol ps_system_function_sqrt     = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "SQRT"   , .value = &ps_value_function_sqrt  };
ps_symbol ps_system_function_exp      = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "EXP"    , .value = &ps_value_function_exp   };
ps_symbol ps_system_function_ln       = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "LN"     , .value = &ps_value_function_ln    };
ps_symbol ps_system_function_log      = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_FUNCTION , .name = "LOG"    , .value = &ps_value_function_log   };

/* CONSTANTS */

#define PS_SYSTEM_CONSTANT(TYPE, VALUE, NAME, FIELD, VALUE2)\
ps_value  ps_value_##TYPE##_##VALUE  = {.type = &ps_type_def_##TYPE, .data = {.FIELD = VALUE2}};\
ps_symbol ps_system_constant_##TYPE##_##VALUE = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_CONSTANT, .name = NAME, .value = &ps_value_##TYPE##_##VALUE};

PS_SYSTEM_CONSTANT(boolean , false  , "FALSE"  , b, (ps_boolean) false);
PS_SYSTEM_CONSTANT(boolean , true   , "TRUE"   , b, (ps_boolean) true );
PS_SYSTEM_CONSTANT(integer , maxint , "MAXINT" , i, PS_INTEGER_MAX);
PS_SYSTEM_CONSTANT(integer , minint , "MININT" , i, PS_INTEGER_MIN);
PS_SYSTEM_CONSTANT(unsigned, maxuint, "MAXUINT", u, PS_UNSIGNED_MAX);
PS_SYSTEM_CONSTANT(real    , maxreal, "MAXREAL", r, (ps_real)PS_REAL_MAX);
PS_SYSTEM_CONSTANT(real    , minreal, "MINREAL", r, PS_REAL_MIN);
PS_SYSTEM_CONSTANT(real    , epsreal, "EPSREAL", r, PS_REAL_EPSILON);
PS_SYSTEM_CONSTANT(real    , pi     , "PI"     , r, 3.141592653589793115997963468544185161590576171875);

// Keeped for reference
// ps_value ps_value_boolean_false     = {.type = &ps_type_def_boolean , .data = {.b = (ps_boolean) false}};        
// ps_symbol ps_system_boolean_false   = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_CONSTANT, .name = "FALSE"    , .value = &ps_value_boolean_false      };

/* VERSION */
PS_SYSTEM_CONSTANT(integer, ps_bitness      , "PS_BITNESS"      , u, PS_BITNESS      );
PS_SYSTEM_CONSTANT(integer, ps_version_major, "PS_VERSION_MAJOR", u, PS_VERSION_MAJOR);
PS_SYSTEM_CONSTANT(integer, ps_version_minor, "PS_VERSION_MINOR", u, PS_VERSION_MINOR);
PS_SYSTEM_CONSTANT(integer, ps_version_patch, "PS_VERSION_PATCH", u, PS_VERSION_PATCH);
PS_SYSTEM_CONSTANT(integer, ps_version_index, "PS_VERSION_INDEX", u, PS_VERSION_INDEX);
PS_SYSTEM_CONSTANT(string , ps_version      , "PS_VERSION"      , s, NULL            );

// Keeped for reference
// ps_value ps_value_version_major = {.type = &ps_type_def_unsigned, .data = {.u = PS_VERSION_MAJOR}};
// ps_symbol ps_system_version_major = {.scope = PS_SYMBOL_SCOPE_SYSTEM, .kind = PS_SYMBOL_KIND_CONSTANT, .name = "PS_VERSION_MAJOR", .value = &ps_value_version_major};

/* clang-format on */

bool ps_system_init(ps_interpreter *interpreter)
{
    ps_symbol_table *symbols = interpreter->parser->symbols;

    /**************************************************************************/
    /* TYPES                                                                  */
    /**************************************************************************/

    if (ps_symbol_table_available(symbols) < 10)
        return false;
    ps_symbol_table_add(symbols, &ps_system_none);
    ps_symbol_table_add(symbols, &ps_system_type_def);
    ps_symbol_table_add(symbols, &ps_system_boolean);
    ps_symbol_table_add(symbols, &ps_system_char);
    ps_symbol_table_add(symbols, &ps_system_integer);
    ps_symbol_table_add(symbols, &ps_system_real);
    ps_symbol_table_add(symbols, &ps_system_unsigned);
    ps_symbol_table_add(symbols, &ps_system_string);
    ps_symbol_table_add(symbols, &ps_system_procedure);
    ps_symbol_table_add(symbols, &ps_system_function);

    /**************************************************************************/
    /* PROCEDURES & FUNCTIONS                                                 */
    /**************************************************************************/
    if (ps_symbol_table_available(symbols) < 5 + 6 + 14)
        return false;
    // procedures
    ps_symbol_table_add(symbols, &ps_system_procedure_read);
    ps_symbol_table_add(symbols, &ps_system_procedure_readln);
    ps_symbol_table_add(symbols, &ps_system_procedure_write);
    ps_symbol_table_add(symbols, &ps_system_procedure_writeln);
    ps_symbol_table_add(symbols, &ps_system_procedure_randomize);
    // ordinal types functions
    ps_symbol_table_add(symbols, &ps_system_function_odd);
    ps_symbol_table_add(symbols, &ps_system_function_even);
    ps_symbol_table_add(symbols, &ps_system_function_chr);
    ps_symbol_table_add(symbols, &ps_system_function_ord);
    ps_symbol_table_add(symbols, &ps_system_function_succ);
    ps_symbol_table_add(symbols, &ps_system_function_pred);
    // math functions
    ps_symbol_table_add(symbols, &ps_system_function_random);
    ps_symbol_table_add(symbols, &ps_system_function_abs);
    ps_symbol_table_add(symbols, &ps_system_function_trunc);
    ps_symbol_table_add(symbols, &ps_system_function_round);
    ps_symbol_table_add(symbols, &ps_system_function_int);
    ps_symbol_table_add(symbols, &ps_system_function_frac);
    ps_symbol_table_add(symbols, &ps_system_function_sin);
    ps_symbol_table_add(symbols, &ps_system_function_cos);
    ps_symbol_table_add(symbols, &ps_system_function_arctan);
    ps_symbol_table_add(symbols, &ps_system_function_sqr);
    ps_symbol_table_add(symbols, &ps_system_function_sqrt);
    ps_symbol_table_add(symbols, &ps_system_function_exp);
    ps_symbol_table_add(symbols, &ps_system_function_ln);
    ps_symbol_table_add(symbols, &ps_system_function_log);

    /**************************************************************************/
    /* CONSTANTS                                                              */
    /**************************************************************************/
    if (ps_symbol_table_available(symbols) < 9)
        return false;
    ps_symbol_table_add(symbols, &ps_system_constant_boolean_false);
    ps_symbol_table_add(symbols, &ps_system_constant_boolean_true);
    ps_symbol_table_add(symbols, &ps_system_constant_integer_maxint);
    ps_symbol_table_add(symbols, &ps_system_constant_integer_minint);
    ps_symbol_table_add(symbols, &ps_system_constant_unsigned_maxuint);
    ps_symbol_table_add(symbols, &ps_system_constant_real_maxreal);
    ps_symbol_table_add(symbols, &ps_system_constant_real_minreal);
    ps_symbol_table_add(symbols, &ps_system_constant_real_epsreal);
    ps_symbol_table_add(symbols, &ps_system_constant_real_pi);

    /**************************************************************************/
    /* VERSION                                                                */
    /**************************************************************************/
    if (ps_symbol_table_available(symbols) < 6)
        return false;
    ps_symbol_table_add(symbols, &ps_system_constant_integer_ps_bitness);
    ps_symbol_table_add(symbols, &ps_system_constant_integer_ps_version_major);
    ps_symbol_table_add(symbols, &ps_system_constant_integer_ps_version_minor);
    ps_symbol_table_add(symbols, &ps_system_constant_integer_ps_version_patch);
    ps_symbol_table_add(symbols, &ps_system_constant_integer_ps_version_index);
    char buffer[16];
    snprintf(buffer, sizeof(buffer) - 1, "%d.%d.%d.%d", PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH,
             PS_VERSION_INDEX);
    ps_string *ps_version_string = ps_string_create(strlen(buffer), buffer);
    if (ps_version_string == NULL)
        return false;
    ps_system_constant_string_ps_version.value->data.s = ps_version_string;
    ps_symbol_table_add(symbols, &ps_system_constant_string_ps_version);

    return true;
}

void ps_system_done(ps_interpreter *interpreter)
{
    // ...
}
