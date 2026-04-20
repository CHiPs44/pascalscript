/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>

#include "ps_executable.h"
#include "ps_memory.h"
#include "ps_symbol.h"
#include "ps_symbol_list.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_type_definition.h"
#include "ps_value.h"
#include "ps_visit.h"

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
    // Check that type name does not already exist in local symbol table
    if (ps_interpreter_find_symbol(interpreter, type_name, true) != NULL)
        RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS)
    READ_NEXT_TOKEN
    // '='
    EXPECT_TOKEN(PS_TOKEN_EQ)
    READ_NEXT_TOKEN
    // TYPE_REFERENCE
    if (!ps_visit_type_reference(interpreter, mode, &type_symbol, type_name))
        TRACE_ERROR("TYPE REFERENCE")

    VISIT_END("OK")
}

/**
 * @brief Register type definition in symbol table
 * @details
 *  - Allocate value for type definition
 *  - Allocate symbol for type definition
 *  - Register type symbol in symbol table
 */
static bool ps_type_definition_register(ps_interpreter *interpreter, ps_interpreter_mode mode, const char *name,
                                        ps_type_definition *type_def, ps_symbol **symbol)
{
    VISIT_BEGIN("REGISTER", "")

    // Allocate value for type definition
    ps_value *value = ps_value_alloc(&ps_system_type_def, (ps_value_data){.t = type_def});
    if (value == NULL)
    {
        ps_type_definition_free(type_def);
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
    }
    // Allocate symbol for type definition
    *symbol = ps_symbol_alloc(PS_SYMBOL_KIND_TYPE_DEFINITION, name, value);
    if (*symbol == NULL)
    {
        ps_value_free(value);
        ps_type_definition_free(type_def);
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
    }
    // Register type symbol in symbol table
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
 * @brief Visit type reference for string type
 *   'STRING' [ '[' IDENTIFIER | UNSIGNED ']' ]
 */
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
        // String[CONSTANT]
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
 * @brief Visit type reference: base types, alias of existing type, subrange, enumeration, array definition, ...
 * @details
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
 *      | ARRAY       =   'ARRAY' '[' SUBRANGE | IDENTIFIER [ ',' SUBRANGE | IDENTIFIER ]* ']' 'OF' TYPE_REFERENCE
 *        Examples:
 *          TCharacterics = Array [1..10] Of Integer
 *          CellState =  (Empty, White, Black)
 *          CheckerBoard = Array [1..8, 1..8] Of CellState
 *  Next steps:
 *      | FILE        =   'TEXT' | 'FILE' [ 'OF' TYPE_REFERENCE ]
 *        Examples:
 *          InputFile = Text
 *          ResultFile = File Of Integer
 *      | SET         =   'SET' 'OF' ORDINAL_TYPE_REFERENCE
 *        Examples:
 *          Options = Set Of UppercaseLetters
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
 *      | POINTER     = 'POINTER'
 *      | TPOINTER    = '^' TYPE_REFERENCE
 */
bool ps_visit_type_reference(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol **type_symbol,
                             const char *type_name)
{
    VISIT_BEGIN("TYPE_REFERENCE", "");

    ps_symbol *symbol = NULL;
    // By default, we advance to next token after processing type reference,
    // but some cases (like copy of existing type) may already have advanced it,
    // so we won't advance in those cases
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
        // => ARRAY
        // NB: this can be something like "ARRAY [1..10] OF (Value1, Value2, ...)"
        // and may recursively call ps_visit_type_reference()
        advance = false;
        if (!ps_visit_type_reference_array(interpreter, mode, type_symbol, type_name))
            TRACE_ERROR("TYPE_REFERENCE_ARRAY")
        break;
        /* ********** Identifier can be many things ********** */
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
                TRACE_ERROR("TYPE_REFERENCE_SUBRANGE")
        }
        else if (symbol->kind == PS_SYMBOL_KIND_TYPE_DEFINITION)
        {
            *type_symbol = symbol;
            READ_NEXT_TOKEN
        }
        else
            RETURN_ERROR(PS_ERROR_EXPECTED_TYPE);
        break;
        /* ********** UNIMPLEMENTED ********** */
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

bool ps_visit_type_reference_enum(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol **type_symbol,
                                  const char *type_name)
{
    VISIT_BEGIN("TYPE_REFERENCE_ENUM", "");

    // Up to 256 values in an enumeration, re-allocate 16 more if exhausted
    ps_symbol_list *list = ps_symbol_list_alloc(16, 16);
    if (list == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    // Re-check that current token is '('
    if (lexer->current_token.type != PS_TOKEN_LEFT_PARENTHESIS)
        GOTO_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN_OR_CLEANUP
    // Empty enumeration not allowed
    if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
        GOTO_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN)

    // Create enumeration type definition
    ps_type_definition *type_def = ps_enum_create();
    if (type_def == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
    // Register it
    if (!ps_type_definition_register(interpreter, mode, type_name, type_def, type_symbol))
        GOTO_CLEANUP(interpreter->error)
    // Parse enumeration values
    do // NOSONAR
    {
        if (list->used == 256)
            GOTO_CLEANUP(PS_ERROR_OVERFLOW);
        if (lexer->current_token.type != PS_TOKEN_IDENTIFIER)
            GOTO_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN)
        // Check that enumeration value does not already exist:
        //  - locally in the same enumeration
        //  - or globally in the symbol table
        if (ps_symbol_list_find(list, lexer->current_token.value.identifier) ||
            (ps_interpreter_find_symbol(interpreter, lexer->current_token.value.identifier, true) != NULL))
            GOTO_CLEANUP(PS_ERROR_SYMBOL_EXISTS)
        ps_symbol *value_symbol = ps_symbol_list_add(list, *type_symbol, lexer->current_token.value.identifier);
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
    if (!ps_enum_set_values(type_def, list->used, list->values))
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
    ps_symbol_list_free(list, false);
    VISIT_END("OK")
cleanup:
    // TODO? remove type symbol from table
    ps_symbol_list_free(list, true);
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
        type_def = ps_subrange_create_char(subrange->c.min, subrange->c.max);
        if (type_name == NULL)
            snprintf(name, sizeof(name) - 1, "#SUBRANGE_C_%08X", ps_symbol_get_auto_num());
        else
            memcpy(name, type_name, PS_IDENTIFIER_SIZE);
        break;
    case PS_TYPE_INTEGER:
        type_def = ps_subrange_create_integer(subrange->i.min, subrange->i.max);
        if (type_name == NULL)
            snprintf(name, sizeof(name) - 1, "#SUBRANGE_I_%08X", ps_symbol_get_auto_num());
        else
            memcpy(name, type_name, PS_IDENTIFIER_SIZE);
        break;
    case PS_TYPE_UNSIGNED:
        type_def = ps_subrange_create_unsigned(subrange->u.min, subrange->u.max);
        if (type_name == NULL)
            snprintf(name, sizeof(name) - 1, "#SUBRANGE_U_%08X", ps_symbol_get_auto_num());
        else
            memcpy(name, type_name, PS_IDENTIFIER_SIZE);
        break;
    case PS_TYPE_ENUM:
        type_def = ps_subrange_create_enum(type, subrange->e.min, subrange->e.max);
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
 * Visit
 *  'ARRAY' '[' SUBRANGE | IDENTIFIER [ ',' SUBRANGE | IDENTIFIER ]* ']' 'OF' TYPE_REFERENCE
 */
bool ps_visit_type_reference_array(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol **type_symbol,
                                   const char *type_name)
{
    VISIT_BEGIN("TYPE_REFERENCE_ARRAY", "");

    ps_symbol *subranges[8] = {0};
    uint8_t dimensions = 0;
    ps_symbol *item_type = NULL;
    ps_symbol *subrange = NULL;

    // Expect 'ARRAY'
    if (lexer->current_token.type != PS_TOKEN_ARRAY)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN
    // Expect '['
    if (lexer->current_token.type != PS_TOKEN_LEFT_BRACKET)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN
    do
    {
        // Expect SUBRANGE: LOW '..' HIGH | IDENTIFIER
        if (!ps_visit_type_reference(interpreter, mode, &subrange, NULL))
            TRACE_ERROR("DIMENSION")
        // Dimension *must* be a subrange
        if (subrange->kind != PS_SYMBOL_KIND_TYPE_DEFINITION || subrange->value->data.t->type != PS_TYPE_SUBRANGE)
            RETURN_ERROR(PS_ERROR_EXPECTED_SUBRANGE)
        subranges[dimensions] = subrange;
        dimensions += 1;
        // ',' starts another dimension
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            if (dimensions == 8)
                RETURN_ERROR(PS_ERROR_TOO_MANY_DIMENSIONS)
            READ_NEXT_TOKEN
            continue;
        }
        // ']' ends dimensions definitions
        if (lexer->current_token.type == PS_TOKEN_RIGHT_BRACKET)
        {
            READ_NEXT_TOKEN
            break;
        }
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    } while (true);
    // For now, only accept one dimension
    // We should define and register an array type definition for each dimension
    // and "chain" them, exactly as if array[dim1, dim2] of item would have been
    // written as array[dim1] of array[dim2] of item
    if (dimensions > 1)
    {
        ps_interpreter_set_message(interpreter, "%d dimensions for an array is TODO/WIP", dimensions);
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
    }
    // Expect 'OF'
    if (lexer->current_token.type != PS_TOKEN_OF)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN
    // Item type (may be another array definition)
    if (!ps_visit_type_reference(interpreter, mode, &item_type, NULL))
        TRACE_ERROR("ITEM_TYPE")
    // Item type can be any type, even another array
    if (item_type->kind != PS_SYMBOL_KIND_TYPE_DEFINITION)
        RETURN_ERROR(PS_ERROR_EXPECTED_TYPE)
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
    type_def->def.a.item_type = item_type;
    type_def->def.a.dimensions = dimensions;
    // Register new type definition in symbol table
    if (!ps_type_definition_register(interpreter, mode, name, type_def, type_symbol))
        RETURN_ERROR(interpreter->error)

    VISIT_END("OK")
}
