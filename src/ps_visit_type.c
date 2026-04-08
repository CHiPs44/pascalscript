/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>

#include "ps_executable.h"
#include "ps_memory.h"
#include "ps_symbol.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_type_definition.h"
#include "ps_value.h"
#include "ps_visit.h"

/**
 * @brief Visit type declaration
 * @details
 *      'TYPE' TYPE_DEFINITION ';'
 *             [ TYPE_DEFINITION ';' ]*
 */
bool ps_visit_type(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("TYPE", "");

    EXPECT_TOKEN(PS_TOKEN_TYPE);
    READ_NEXT_TOKEN
    if (lexer->current_token.type != PS_TOKEN_IDENTIFIER)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    do
    {
        if (!ps_visit_type_definition(interpreter, mode))
            TRACE_ERROR("TYPE_DEFINITION");
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);

    VISIT_END("OK")
}

static bool ps_type_definition_register(ps_interpreter *interpreter, ps_interpreter_mode mode, const char *name,
                                        ps_type_definition *type_def, ps_symbol **symbol)
{
    VISIT_BEGIN("REGISTER", "")

    ps_value *value = ps_value_alloc(&ps_system_type_def, (ps_value_data){.t = type_def});
    if (value == NULL)
    {
        ps_type_definition_free(type_def);
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
    }
    *symbol = ps_symbol_alloc(PS_SYMBOL_KIND_TYPE_DEFINITION, name, value);
    if (*symbol == NULL)
    {
        ps_value_free(value);
        ps_type_definition_free(type_def);
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
    }
    if (!ps_interpreter_add_symbol(interpreter, *symbol))
    {
        ps_symbol_free(*symbol);
        ps_value_free(value);
        ps_type_definition_free(type_def);
        TRACE_ERROR("REGISTER")
    }

    VISIT_END("OK");
}

/**
 * @brief Visit type definition
 * @details
 *    IDENTIFIER '=' TYPE_REFERENCE
 */
bool ps_visit_type_definition(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("TYPE_DEFINITION", "");

    ps_symbol *type_symbol = NULL;
    ps_identifier type_name = {0};

    // IDENTIFIER
    if (lexer->current_token.type != PS_TOKEN_IDENTIFIER)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    COPY_IDENTIFIER(type_name)
    READ_NEXT_TOKEN
    // '='
    EXPECT_TOKEN(PS_TOKEN_EQ)
    READ_NEXT_TOKEN
    // TYPE_REFERENCE
    if (!ps_visit_type_reference(interpreter, mode, &type_symbol, type_name))
        TRACE_ERROR("TYPE REFERENCE")

    VISIT_END("OK")
}

bool ps_visit_type_reference_string(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol **type_symbol,
                                    const char *type_name)
{
    VISIT_BEGIN("TYPE_REFERENCE_STRING", "")
    ssize_t len = 0;

    if (lexer->current_token.type != PS_TOKEN_STRING)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN
    if (lexer->current_token.type == PS_TOKEN_LEFT_BRACKET)
    {
        // String[MY_CONSTANT]
        READ_NEXT_TOKEN
        ps_value constant = {.type = &ps_system_none, .data.v = NULL};
        if (!ps_visit_constant_expression(interpreter, mode, &constant))
            TRACE_ERROR("CONSTANT_EXPRESSION");
        switch (constant.type->value->data.t->base)
        {
        case PS_TYPE_INTEGER:
            len = constant.data.i;
            break;
        case PS_TYPE_UNSIGNED:
            len = constant.data.u;
            break;
        default:
            RETURN_ERROR(PS_ERROR_EXPECTED_STRING_LENGTH)
        }
        if (len < 1 || len > PS_STRING_MAX_LEN)
            RETURN_ERROR(PS_ERROR_EXPECTED_STRING_LENGTH)
        ps_type_definition *type_def = ps_type_definition_alloc(PS_TYPE_DEFINITION, PS_TYPE_STRING);
        if (type_def == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
        type_def->def.s.max = (ps_string_len)len;
        ps_value *value = ps_value_alloc(&ps_system_type_def, (ps_value_data){.t = type_def});
        ps_identifier name = {0};
        if (type_name == NULL)
            snprintf(name, sizeof(name) - 1, "#STRING_%08X", ps_symbol_get_auto_num());
        else
            memcpy(name, type_name, PS_IDENTIFIER_SIZE);
        *type_symbol = ps_symbol_alloc(PS_SYMBOL_KIND_TYPE_DEFINITION, name, value);
    }
    else
    {
        *type_symbol = &ps_system_string;
    }

    VISIT_END("OK")
}

/**
 * @details
 *  Visit type reference, for now, only base types:
 *      'INTEGER' | 'UNSIGNED' | 'REAL' | 'BOOLEAN' | 'CHAR'
 *      | 'STRING' [ '[' IDENTIFIER | UNSIGNED ']' ] |
 *      | IDENTIFIER
 *      | ENUMERATION =   '(' IDENTIFIER [ ',' IDENTIFIER ]* ')'
 *        Examples:
 *          TGender         = (Male, Female, Other)
 *          TAbilities      = (Strength, Intelligence, Wisdom, Dexterity, Constitution, Charisma)
 *          TCharacterClass = (Fighter, Wizard, Cleric, Rogue)
 *          TCharacterRace  = (Human, Elf, Dwarf, Halfling, Gnome, HalfOrc)
 *          TDie            = (D4, D6, D8, D10, D12, D20, D100)
 *          Days            = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
 *      | SUBRANGE    =   LOW_OR_HIGH '..' LOW_OR_HIGH
 *        LOW_OR_HIGH =   CONSTANT_EXPRESSION
 *        Examples:
 *          Score            = -MaxScore..MaxScore
 *          WorkDays         = Monday..Friday
 *          WeekEnd          = Saturday..Sunday
 *          UppercaseLetters = 'A'..'Z'
 *          LowercaseLetters = 'a'..'z'
 *      | ARRAY       =   'ARRAY' '[' SUBRANGE | IDENTIFIER ']' 'OF' TYPE_REFERENCE
 *        Example:
 *          TCharacterics = Array [1..10] Of Integer
 *  Next steps:
 *      | ARRAY       =   'ARRAY' '[' SUBRANGE | IDENTIFIER [ ',' SUBRANGE | IDENTIFIER ]* ']' 'OF' TYPE_REFERENCE
 *        Example:
 *          CheckerBoard = Array [1..8, 1..8] Of (Empty, White, Black)
 *      | SET         =   'SET' 'OF' ORDINAL_TYPE_REFERENCE
 *        Examples:
 *          Options = Set Of UppercaseLetters
 *      | FILE        =   'TEXT' | 'FILE' [ 'OF' TYPE_REFERENCE ]
 *        Examples:
 *          InputFile = Text
 *          ResultFile = File Of Integer
 *      | RECORD      =   'RECORD'
 *                          FIELD [ ';' FIELD ]*
 *                          [ ';' ]
 *                      'END'
 *        Examples:
 *          ComplexNumber = Record
 *            Re: Real;
 *            Im: Real;
 *          End;
 *          TCharacter = Record
 *            Name: String;
 *            Age: Unsigned;
 *            Gender: TGender;
 *            Class: TCharacterClass;
 *            Race: TCharacterRace;
 *            HitDie: TDie;
 *            HitPoints: Integer;
 *            Level: Unsigned;
 *            Experience: Unsigned;
 *            Abilities: Array[TAbilities] Of Unsigned;
 *            Modifiers: Array[TAbilities] Of Integer;
 *          End;
 *        FIELD       = IDENTIFIER ':' TYPE_REFERENCE
 * ???:
 *      | TPOINTER    = '^' TYPE_REFERENCE
 *      | POINTER     = 'POINTER'
 */
bool ps_visit_type_reference(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol **type_symbol,
                             const char *type_name)
{
    VISIT_BEGIN("TYPE_REFERENCE", "");

    ps_symbol *symbol = NULL;
    bool advance = true;

    *type_symbol = NULL;
    switch (lexer->current_token.type)
    {
        /* ********** Base types ********** */
    case PS_TOKEN_INTEGER:
        *type_symbol = &ps_system_integer;
        break;
    case PS_TOKEN_UNSIGNED:
        *type_symbol = &ps_system_unsigned;
        break;
    case PS_TOKEN_REAL:
        *type_symbol = &ps_system_real;
        break;
    case PS_TOKEN_BOOLEAN:
        *type_symbol = &ps_system_boolean;
        break;
    case PS_TOKEN_CHAR:
        *type_symbol = &ps_system_char;
        break;
    case PS_TOKEN_STRING:
        advance = false;
        if (!ps_visit_type_reference_string(interpreter, mode, type_symbol, type_name))
            TRACE_ERROR("TYPE_REFERENCE_STRING")
        break;
    case PS_TOKEN_IDENTIFIER:
        advance = false;
        // This could be:
        //  - a copy of an existing type
        //  - a subrange definition from an enumeration
        //  - a subrange definition beginning with a constant expression
        symbol = ps_interpreter_find_symbol(interpreter, lexer->current_token.value.identifier, false);
        if (symbol == NULL)
            RETURN_ERROR(PS_ERROR_UNKOWN_IDENTIFIER);
        if (symbol->kind == PS_SYMBOL_KIND_CONSTANT)
        {
            // Subrange from enumeration/char/integer/unsigned constant
            ps_value_type type = ps_value_get_type(symbol->value);
            if ((type == PS_TYPE_ENUM || type == PS_TYPE_CHAR || type == PS_TYPE_INTEGER || type == PS_TYPE_UNSIGNED) &&
                !ps_visit_type_reference_subrange(interpreter, mode, type_symbol, type_name))
                RETURN_ERROR(interpreter->error)
        }
        else if (symbol->kind == PS_SYMBOL_KIND_TYPE_DEFINITION)
        {
            *type_symbol = symbol;
            READ_NEXT_TOKEN
        }
        else
            RETURN_ERROR(PS_ERROR_EXPECTED_TYPE);
        break;
        /* ********** Other types ********** */
    case PS_TOKEN_CHAR_VALUE:
    case PS_TOKEN_INTEGER_VALUE:
    case PS_TOKEN_MINUS:
    case PS_TOKEN_UNSIGNED_VALUE:
        // => SUBRANGE
        advance = false;
        if (!ps_visit_type_reference_subrange(interpreter, mode, type_symbol, type_name))
            TRACE_ERROR("TYPE_REFERENCE_SUBRANGE")
        break;
    case PS_TOKEN_LEFT_PARENTHESIS:
        // => ENUMERATION
        advance = false;
        if (!ps_visit_type_reference_enum(interpreter, mode, type_symbol, type_name))
            TRACE_ERROR("TYPE_REFERENCE_ENUM")
        break;
    case PS_TOKEN_ARRAY:
        // NB: array can be something like "ARRAY [1..10] OF (Value1, Value2, ...)"
        // and may recursively call ps_visit_type_reference()
        advance = false;
        if (!ps_visit_type_reference_array(interpreter, mode, type_symbol, type_name))
            TRACE_ERROR("TYPE_REFERENCE_ARRAY")
        break;
    case PS_TOKEN_SET:    // set
    case PS_TOKEN_FILE:   // file
    case PS_TOKEN_TEXT:   // text
    case PS_TOKEN_RECORD: // record
    case PS_TOKEN_CARET:  // pointer
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }
    if (advance)
        READ_NEXT_TOKEN

    VISIT_END("OK")
}

// TODO? replace with a symbol table?
typedef struct s_ps_enum_values_pool
{
    ps_unsigned size;
    ps_unsigned more;
    ps_unsigned used;
    ps_symbol **values;
} ps_enum_values_pool;

ps_enum_values_pool *ps_enum_values_pool_alloc(int size, int more)
{
    ps_enum_values_pool *pool = ps_memory_malloc(PS_MEMORY_PARSER, sizeof(ps_enum_values_pool));
    if (pool == NULL)
        return NULL; // errno = ENOMEM
    pool->values = ps_memory_calloc(PS_MEMORY_PARSER, size, sizeof(ps_symbol *));
    if (pool->values == NULL)
    {
        ps_memory_free(PS_MEMORY_PARSER, pool);
        return NULL; // errno = ENOMEM
    }
    pool->size = size;
    pool->more = more;
    pool->used = 0;
    return pool;
}

void ps_enum_values_pool_free(ps_enum_values_pool *pool, bool free_symbols)
{
    if (free_symbols)
        for (ps_unsigned i = 0; i < pool->used; i++)
        {
            ps_symbol_free(pool->values[i]);
        }
    ps_memory_free(PS_MEMORY_PARSER, pool->values);
    ps_memory_free(PS_MEMORY_PARSER, pool);
}

bool ps_enum_values_pool_grow(ps_enum_values_pool *pool)
{
    // still room for new values?
    if (pool->used < pool->size)
        return true;
    // grow pool
    if (pool->size + pool->more > 256)
        return false;
    ps_symbol **new_values =
        ps_memory_realloc(PS_MEMORY_PARSER, pool->values, (pool->size + pool->more) * sizeof(ps_symbol *));
    if (new_values == NULL)
        return false; // errno = ENOMEM
    pool->values = new_values;
    pool->size += pool->more;
    return true;
}

bool ps_enum_values_pool_find(const ps_enum_values_pool *pool, const char *name)
{
    for (ps_unsigned i = 0; i < pool->used; i++)
        if (strcmp(name, pool->values[i]->name) == 0)
            return true;
    return false;
}

ps_symbol *ps_enum_values_pool_add(ps_enum_values_pool *pool, ps_symbol *type_symbol, const char *name)
{
    // Create a new symbol for the enumeration value
    ps_value *value = ps_value_alloc(type_symbol, (ps_value_data){.u = pool->used});
    if (value == NULL)
        return NULL;
    ps_symbol *symbol = ps_symbol_alloc(PS_SYMBOL_KIND_CONSTANT, name, value);
    if (symbol == NULL)
    {
        ps_value_free(value);
        return NULL;
    }
    if (!ps_enum_values_pool_grow(pool))
        return NULL;
    pool->values[pool->used] = symbol;
    pool->used += 1;
    return symbol;
}

bool ps_visit_type_reference_enum(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol **type_symbol,
                                  const char *type_name)
{
    VISIT_BEGIN("TYPE_REFERENCE_ENUM", "");

    // Up to 256 values in an enumeration, re-allocate 16 more if exhausted
    ps_enum_values_pool *pool = ps_enum_values_pool_alloc(16, 16);
    if (pool == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    // Re-check that current token is '('
    if (lexer->current_token.type != PS_TOKEN_LEFT_PARENTHESIS)
        GOTO_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN_OR_CLEANUP
    // Empty enumeration not allowed
    if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
        GOTO_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN)

    // Create enumeration type definition
    ps_type_definition *type_def = ps_type_definition_create_enum();
    if (type_def == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
    // Register it
    if (!ps_type_definition_register(interpreter, mode, type_name, type_def, type_symbol))
        GOTO_CLEANUP(interpreter->error)
    // Parse enumeration values
    do // NOSONAR
    {
        if (pool->used == 256)
            GOTO_CLEANUP(PS_ERROR_OVERFLOW);
        if (lexer->current_token.type != PS_TOKEN_IDENTIFIER)
            GOTO_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN)
        // Check that enumeration value does not already exist:
        //  - locally in the same enumeration
        //  - or globally in the symbol tables
        if (ps_enum_values_pool_find(pool, lexer->current_token.value.identifier) ||
            (ps_interpreter_find_symbol(interpreter, lexer->current_token.value.identifier, false) != NULL))
            GOTO_CLEANUP(PS_ERROR_SYMBOL_EXISTS)
        ps_symbol *value_symbol = ps_enum_values_pool_add(pool, *type_symbol, lexer->current_token.value.identifier);
        if (value_symbol == NULL)
            GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
        if (!ps_interpreter_add_symbol(interpreter, value_symbol))
            GOTO_CLEANUP(PS_ERROR_SYMBOL_NOT_ADDED)
        READ_NEXT_TOKEN_OR_CLEANUP
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            READ_NEXT_TOKEN_OR_CLEANUP
            continue;
        }
        if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
            break;
    } while (true);
    READ_NEXT_TOKEN_OR_CLEANUP
    // Copy enum values to enum type definition
    if (!ps_type_definition_set_enum_values(type_def, pool->used, pool->values))
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
    ps_enum_values_pool_free(pool, false);
    VISIT_END("OK")
cleanup:
    // TODO remove type symbol from table
    ps_enum_values_pool_free(pool, true);
    return false;
}

bool ps_visit_type_reference_subrange_min(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *min_value,
                                          ps_value_type *min_base, ps_type_definition_subrange *subrange)
{
    VISIT_BEGIN("TYPE_REFERENCE_SUBRANGE", "")

    if (!ps_visit_constant_expression(interpreter, mode, min_value))
        TRACE_ERROR("MIN")
    *min_base = ps_value_get_type(min_value);
    switch (*min_base)
    {
    case PS_TYPE_CHAR:
        if (ps_value_get_type(min_value) != PS_TYPE_CHAR)
            RETURN_ERROR(PS_ERROR_EXPECTED_CHAR)
        subrange->c.min = min_value->data.c;
        break;
    case PS_TYPE_INTEGER:
        if (ps_value_get_type(min_value) != PS_TYPE_INTEGER)
            RETURN_ERROR(PS_ERROR_EXPECTED_INTEGER)
        subrange->i.min = min_value->data.i;
        break;
    case PS_TYPE_UNSIGNED:
        if (ps_value_get_type(min_value) != PS_TYPE_UNSIGNED)
            RETURN_ERROR(PS_ERROR_EXPECTED_UNSIGNED)
        subrange->u.min = min_value->data.u;
        break;
    case PS_TYPE_ENUM:
        if (ps_value_get_type(min_value) != PS_TYPE_ENUM)
            RETURN_ERROR(PS_ERROR_EXPECTED_ENUM)
        subrange->e.min = min_value->data.u;
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }

    VISIT_END("OK")
}

bool ps_visit_type_reference_subrange_max(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *max_value,
                                          ps_value_type *max_base, ps_type_definition_subrange *subrange)
{
    VISIT_BEGIN("TYPE_REFERENCE_SUBRANGE", "")

    if (!ps_visit_constant_expression(interpreter, mode, max_value))
        TRACE_ERROR("MAX")
    *max_base = ps_value_get_type(max_value);
    switch (*max_base)
    {
    case PS_TYPE_CHAR:
        if (ps_value_get_type(max_value) != PS_TYPE_CHAR)
            RETURN_ERROR(PS_ERROR_EXPECTED_CHAR)
        subrange->c.max = max_value->data.c;
        break;
    case PS_TYPE_INTEGER:
        if (ps_value_get_type(max_value) != PS_TYPE_INTEGER)
            RETURN_ERROR(PS_ERROR_EXPECTED_INTEGER)
        subrange->i.max = max_value->data.i;
        break;
    case PS_TYPE_UNSIGNED:
        if (ps_value_get_type(max_value) != PS_TYPE_UNSIGNED)
            RETURN_ERROR(PS_ERROR_EXPECTED_UNSIGNED)
        subrange->u.max = max_value->data.u;
        break;
    case PS_TYPE_ENUM:
        if (ps_value_get_type(max_value) != PS_TYPE_ENUM)
            RETURN_ERROR(PS_ERROR_EXPECTED_ENUM)
        subrange->e.max = max_value->data.u;
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }

    VISIT_END("OK")
}

bool ps_visit_type_reference_subrange_register_type_def(ps_interpreter *interpreter, ps_interpreter_mode mode,
                                                        const char *type_name, ps_symbol **type_symbol, ps_symbol *type,
                                                        const ps_type_definition_subrange *subrange)
{
    VISIT_BEGIN("TYPE_REFERENCE_SUBRANGE", "")

    ps_type_definition *type_def = NULL;
    ps_identifier name = {0};

    // Create type definition for subrange
    switch (type->value->data.t->type)
    {
    case PS_TYPE_CHAR:
        type_def = ps_type_definition_create_subrange_char(subrange->c.min, subrange->c.max);
        if (type_name == NULL)
            snprintf(name, sizeof(name) - 1, "#SUBRANGE_C_%08X", ps_symbol_get_auto_num());
        else
            memcpy(name, type_name, PS_IDENTIFIER_SIZE);
        break;
    case PS_TYPE_INTEGER:
        type_def = ps_type_definition_create_subrange_integer(subrange->i.min, subrange->i.max);
        if (type_name == NULL)
            snprintf(name, sizeof(name) - 1, "#SUBRANGE_I_%08X", ps_symbol_get_auto_num());
        else
            memcpy(name, type_name, PS_IDENTIFIER_SIZE);
        break;
    case PS_TYPE_UNSIGNED:
        type_def = ps_type_definition_create_subrange_unsigned(subrange->u.min, subrange->u.max);
        if (type_name == NULL)
            snprintf(name, sizeof(name) - 1, "#SUBRANGE_U_%08X", ps_symbol_get_auto_num());
        else
            memcpy(name, type_name, PS_IDENTIFIER_SIZE);
        break;
    case PS_TYPE_ENUM:
        type_def = ps_type_definition_create_subrange_enum(type, subrange->e.min, subrange->e.max);
        if (type_name == NULL)
            snprintf(name, sizeof(name) - 1, "#SUBRANGE_E_%08X", ps_symbol_get_auto_num());
        else
            memcpy(name, type_name, PS_IDENTIFIER_SIZE);
        break;
    default:
        RETURN_ERROR(PS_ERROR_INVALID_SUBRANGE)
    }
    if (type_def == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
    // Register new type definition in symbol table
    if (!ps_type_definition_register(interpreter, mode, name, type_def, type_symbol))
        RETURN_ERROR(interpreter->error)

    VISIT_END("OK")
}

bool ps_visit_type_reference_subrange(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol **type_symbol,
                                      const char *type_name)
{
    VISIT_BEGIN("TYPE_REFERENCE_SUBRANGE", "")

    ps_type_definition_subrange subrange = {0};
    ps_value min_value = {0};
    ps_value tmp_value = {0};
    ps_value max_value = {0};
    ps_value_type min_base = PS_TYPE_NONE;
    ps_value_type max_base = PS_TYPE_NONE;

    // *** Parse min value of subrange as a constant expression
    if (!ps_visit_type_reference_subrange_min(interpreter, mode, &min_value, &min_base, &subrange))
        TRACE_ERROR("MIN")
    // *** Parse '..'
    EXPECT_TOKEN(PS_TOKEN_RANGE)
    READ_NEXT_TOKEN
    // *** Parse max value of subrange as a constant expression
    if (!ps_visit_type_reference_subrange_max(interpreter, mode, &tmp_value, &max_base, &subrange))
        TRACE_ERROR("MAX")
    // *** Copy value to max with same type as min
    max_value.type = min_value.type;
    if (!ps_interpreter_copy_value(interpreter, &tmp_value, &max_value))
    {
        ps_interpreter_set_message(interpreter, "Min and max value of subrange type mismatch: %s %s",
                                   min_value.type->name, max_value.type->name);
        TRACE_ERROR("COPY_MAX")
    }
    // *** Check that subrange min is less than max
    if ((max_base == PS_TYPE_CHAR && subrange.c.max <= subrange.c.min) ||
        (max_base == PS_TYPE_INTEGER && subrange.i.max <= subrange.i.min) ||
        (max_base == PS_TYPE_UNSIGNED && subrange.u.max <= subrange.u.min))
        RETURN_ERROR(PS_ERROR_INVALID_SUBRANGE)
    else if (max_base == PS_TYPE_ENUM)
    {
        if (ps_value_get_type(&max_value) != PS_TYPE_ENUM)
            RETURN_ERROR(PS_ERROR_EXPECTED_INTEGER)
        if (subrange.u.max <= subrange.u.min)
            RETURN_ERROR(PS_ERROR_INVALID_SUBRANGE)
    }
    else
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE)
    // *** Register subrange
    if (!ps_visit_type_reference_subrange_register_type_def(interpreter, mode, type_name, type_symbol, min_value.type,
                                                            &subrange))
        TRACE_ERROR("TYPE_DEF_SUBRANGE")

    VISIT_END("OK")
}

/**
 * 'ARRAY' '[' SUBRANGE | IDENTIFIER [ ',' SUBRANGE | IDENTIFIER ]* ']' 'OF' TYPE_REFERENCE
 */
bool ps_visit_type_reference_array(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol **type_symbol,
                                   const char *type_name)
{
    VISIT_BEGIN("TYPE_REFERENCE_ARRAY", "");

    ps_symbol *subranges[8] = {0};
    int dimensions = 0;
    ps_symbol *item_type = NULL;
    ps_symbol *subrange = NULL;

    // 'ARRAY'
    if (lexer->current_token.type != PS_TOKEN_ARRAY)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN
    // '['
    if (lexer->current_token.type != PS_TOKEN_LEFT_BRACKET)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN
    do
    {
        // SUBRANGE: LOW '..' HIGH | IDENTIFIER
        if (!ps_visit_type_reference(interpreter, mode, &subrange, NULL))
            TRACE_ERROR("DIMENSION")
        // Dimension *must* be a subrange
        if (subrange->kind != PS_SYMBOL_KIND_TYPE_DEFINITION || subrange->value->data.t->type != PS_TYPE_SUBRANGE)
            RETURN_ERROR(PS_ERROR_EXPECTED_SUBRANGE)
        subranges[dimensions] = subrange;
        dimensions += 1;
        // ',' ?
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            if (dimensions == 8)
                RETURN_ERROR(PS_ERROR_TOO_MANY_DIMENSIONS)
            READ_NEXT_TOKEN
            continue;
        }
        // ']'
        if (lexer->current_token.type == PS_TOKEN_RIGHT_BRACKET)
        {
            READ_NEXT_TOKEN
            break;
        }
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    } while (true);
    // 'OF'
    if (lexer->current_token.type != PS_TOKEN_OF)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN
    // ITEM_TYPE
    if (!ps_visit_type_reference(interpreter, mode, &item_type, NULL))
        TRACE_ERROR("ITEM_TYPE")
    // Item type can be any type, even another array
    if (item_type->kind != PS_SYMBOL_KIND_TYPE_DEFINITION)
        RETURN_ERROR(PS_ERROR_EXPECTED_TYPE)
    // For now, item_type cannot be an array type
    if (item_type->value->data.t->type == PS_TYPE_ARRAY)
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
    // Create type definition for array
    ps_type_definition *type_def = NULL;
    ps_identifier name = {0};
    if (type_name == NULL)
        snprintf(name, sizeof(name) - 1, "#ARRAY_%08X", ps_symbol_get_auto_num());
    else
        memcpy(name, type_name, PS_IDENTIFIER_SIZE);
    type_def = ps_type_definition_alloc(PS_TYPE_ARRAY, PS_TYPE_ARRAY);
    if (type_def == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
    type_def->def.a.subranges = ps_memory_calloc(PS_MEMORY_TYPE, dimensions, sizeof(ps_symbol *));
    if (type_def->def.a.subranges == NULL)
    {
        ps_type_definition_free(type_def);
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
    }
    for (int i = 0; i <= dimensions; i++)
    {
        type_def->def.a.subranges[i] = subranges[i];
    }
    type_def->def.a.dimensions = dimensions;
    type_def->def.a.item_type = item_type;
    // Register new type definition in symbol table
    if (!ps_type_definition_register(interpreter, mode, name, type_def, type_symbol))
        RETURN_ERROR(interpreter->error)

    VISIT_END("OK")
}
