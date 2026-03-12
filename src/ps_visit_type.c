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
        RETURN_ERROR(PS_ERROR_SYMBOL_NOT_ADDED)
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
            snprintf(name, sizeof(name) - 1, "#STRING_%d", (int)len);
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
 *  Next steps:
 *      | ARRAY       =   'ARRAY' '[' SUBRANGE | IDENTIFIER [ ',' SUBRANGE | IDENTIFIER ]* ']' 'OF' TYPE_REFERENCE
 *        Examples:
 *          CheckerBoard = Array [1..8, 1..8] Of Boolean
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
        // This could be:
        //  - DONE a copy of an existing type
        //  - a subrange definition beginning with a constant expression
        //  - a subrange definition from an enumeration
        symbol = ps_interpreter_find_symbol(interpreter, lexer->current_token.value.identifier, false);
        if (symbol == NULL)
            RETURN_ERROR(PS_ERROR_UNKOWN_IDENTIFIER);
        if (symbol->kind == PS_SYMBOL_KIND_CONSTANT)
        {
            // Subrange from enumeration
            if (!ps_visit_type_reference_subrange(interpreter, mode, type_symbol, type_name, PS_TYPE_ENUM))
                RETURN_ERROR(interpreter->error)
            break;
        }
        if (symbol->kind != PS_SYMBOL_KIND_TYPE_DEFINITION)
            RETURN_ERROR(PS_ERROR_EXPECTED_TYPE);
        *type_symbol = symbol;
        break;
        /* ********** Other types ********** */
    case PS_TOKEN_CHAR_VALUE: // subrange
        advance = false;
        if (!ps_visit_type_reference_subrange(interpreter, mode, type_symbol, type_name, PS_TYPE_CHAR))
            TRACE_ERROR("TYPE_REFERENCE_SUBRANGE")
        break;
    case PS_TOKEN_INTEGER_VALUE: // subrange
    case PS_TOKEN_MINUS:         // subrange with negative integer or constant
        advance = false;
        if (!ps_visit_type_reference_subrange(interpreter, mode, type_symbol, type_name, PS_TYPE_INTEGER))
            TRACE_ERROR("TYPE_REFERENCE_SUBRANGE")
        break;
    case PS_TOKEN_UNSIGNED_VALUE: // subrange
        advance = false;
        if (!ps_visit_type_reference_subrange(interpreter, mode, type_symbol, type_name, PS_TYPE_UNSIGNED))
            TRACE_ERROR("TYPE_REFERENCE_SUBRANGE")
        break;
    // case PS_TOKEN_BOOLEAN_VALUE:
    // subrange => not really useful but possible
    case PS_TOKEN_LEFT_PARENTHESIS: // enumeration
        advance = false;
        if (!ps_visit_type_reference_enum(interpreter, mode, type_symbol, type_name))
            TRACE_ERROR("TYPE_REFERENCE_ENUM")
        break;
    case PS_TOKEN_ARRAY:
        // NB: array can be something like "ARRAY [1..10] OF (Value1, Value2, ...)"
        // and may recursively call ps_visit_type_reference()
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

void ps_enum_values_pool_free(ps_enum_values_pool *pool, bool free)
{
    if (free)
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

    // Up to 256 values in an enumeration, realloc by 16 more if exhausted
    ps_enum_values_pool *pool = ps_enum_values_pool_alloc(16, 16);
    if (pool == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    // Re-check that current token is '('
    if (lexer->current_token.type != PS_TOKEN_LEFT_PARENTHESIS)
        GOTO_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN_OR_CLEANUP
    // empty enumeration not allowed
    if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
        GOTO_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN)

    ps_type_definition *type_def = ps_type_definition_create_enum();
    if (type_def == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
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
        READ_NEXT_TOKEN_OR_CLEANUP if (lexer->current_token.type == PS_TOKEN_COMMA)
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

bool ps_visit_type_reference_subrange(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol **type_symbol,
                                      const char *type_name, ps_value_type base)
{
    VISIT_BEGIN("TYPE_REFERENCE_SUBRANGE", "CHAR");

    ps_type_definition_subrange_char c = {0};
    ps_type_definition_subrange_integer i = {0};
    ps_type_definition_subrange_unsigned u = {0};

    // Parse min value of subrange
    if (base == PS_TYPE_CHAR)
    {
        if (lexer->current_token.type != PS_TOKEN_CHAR_VALUE)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        c.min = lexer->current_token.value.c;
    }
    else if (base == PS_TYPE_INTEGER)
    {
        if (lexer->current_token.type == PS_TOKEN_MINUS)
        {
            // Negative integer subrange: -10..-1
            READ_NEXT_TOKEN
            if (lexer->current_token.type != PS_TOKEN_INTEGER_VALUE)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
            i.min = -lexer->current_token.value.i;
        }
        else
        {
            if (lexer->current_token.type != PS_TOKEN_INTEGER_VALUE)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
            i.min = lexer->current_token.value.i;
        }
    }
    else if (base == PS_TYPE_UNSIGNED)
    {
        if (lexer->current_token.type != PS_TOKEN_UNSIGNED_VALUE)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        u.min = lexer->current_token.value.u;
    }
    else
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    // Parse '..'
    READ_NEXT_TOKEN
    EXPECT_TOKEN(PS_TOKEN_RANGE);
    // Parse max value of subrange
    READ_NEXT_TOKEN
    if (base == PS_TYPE_CHAR)
    {
        if (lexer->current_token.type != PS_TOKEN_CHAR_VALUE)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        c.max = lexer->current_token.value.c;
        if (c.max <= c.min)
            RETURN_ERROR(PS_ERROR_INVALID_SUBRANGE)
    }
    else if (base == PS_TYPE_INTEGER)
    {
        if (lexer->current_token.type == PS_TOKEN_MINUS)
        {
            // Negative integer subrange: -10..-1
            READ_NEXT_TOKEN
            if (lexer->current_token.type != PS_TOKEN_INTEGER_VALUE)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
            i.max = -lexer->current_token.value.i;
        }
        else
        {
            if (lexer->current_token.type != PS_TOKEN_INTEGER_VALUE)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
            i.max = lexer->current_token.value.i;
        }
        if (i.max <= i.min)
            RETURN_ERROR(PS_ERROR_INVALID_SUBRANGE)
    }
    else if (base == PS_TYPE_UNSIGNED)
    {
        if (lexer->current_token.type != PS_TOKEN_UNSIGNED_VALUE)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        u.max = lexer->current_token.value.u;
    }
    else
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN
    // Create type definition for subrange
    ps_type_definition *type_def = NULL;
    ps_identifier name = {0};
    switch (base)
    {
    case PS_TYPE_CHAR:
        type_def = ps_type_definition_create_subrange_char(c.min, c.max);
        if (type_name == NULL)
            snprintf(name, sizeof(name) - 1, "#SUBRANGE_C_%d_%d", c.min, c.max);
        else
            memcpy(name, type_name, PS_IDENTIFIER_SIZE);
        break;
    case PS_TYPE_INTEGER:
        type_def = ps_type_definition_create_subrange_integer(i.min, i.max);
        if (type_name == NULL)
            snprintf(name, sizeof(name) - 1, "#SUBRANGE_I_%" PS_INTEGER_FMT_10 "_%" PS_INTEGER_FMT_10, i.min, i.max);
        else
            memcpy(name, type_name, PS_IDENTIFIER_SIZE);
        break;
    case PS_TYPE_UNSIGNED:
        type_def = ps_type_definition_create_subrange_unsigned(u.min, u.max);
        if (type_name == NULL)
            snprintf(name, sizeof(name) - 1, "#SUBRANGE_U_%" PS_UNSIGNED_FMT_10 "_%" PS_UNSIGNED_FMT_10, u.min, u.max);
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
