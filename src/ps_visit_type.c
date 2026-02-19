/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_executable.h"
#include "ps_symbol.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_type_definition.h"
#include "ps_value.h"
#include "ps_visit.h"

/**
 * Visit
 *      'TYPE' TYPE_DEFINITION ';' [ TYPE_DEFINITION ';' ]*
 */
bool ps_visit_type(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("TYPE", "");

    EXPECT_TOKEN(PS_TOKEN_TYPE);
    READ_NEXT_TOKEN;
    if (lexer->current_token.type != PS_TOKEN_IDENTIFIER)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    do
    {
        if (!ps_visit_type_definition(interpreter, mode))
            TRACE_ERROR("TYPE_DEFINITION");
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN;
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);

    VISIT_END("OK");
}

/**
 * Visit type definition:
 *    IDENTIFIER '=' TYPE_REFERENCE
 */
bool ps_visit_type_definition(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("TYPE_DEFINITION", "");

    ps_symbol *type = NULL;
    ps_value *value = NULL;
    ps_symbol *type_symbol = NULL;
    ps_identifier type_name = {0};
    ps_value_data data = {0};

    // IDENTIFIER
    if (lexer->current_token.type != PS_TOKEN_IDENTIFIER)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    COPY_IDENTIFIER(type_name);
    READ_NEXT_TOKEN;
    // '='
    EXPECT_TOKEN(PS_TOKEN_EQ);
    READ_NEXT_TOKEN;
    // TYPE_REFERENCE
    if (!ps_visit_type_reference(interpreter, mode, &type_symbol))
        TRACE_ERROR("TYPE REFERENCE");

    // Register new type definition in symbol table
    data.t = type_symbol->value->data.t;
    value = ps_value_alloc(type, data);
    if (value == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
    type_symbol = ps_symbol_alloc(PS_SYMBOL_KIND_TYPE_DEFINITION, &type_name, value);
    if (type_symbol == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
    if (!ps_interpreter_add_symbol(interpreter, type_symbol))
        TRACE_ERROR("ADD SYMBOL");

    VISIT_END("OK");
}

/**
 * Visit constant expression:
 *      INTEGER_VALUE | UNSIGNED_VALUE | CHAR_VALUE | REAL_VALUE | BOOLEAN_VALUE | IDENTIFIER
 * Next steps:
 *      | STRING_VALUE
 *      | NIL
 *      | CONSTANT_EXPRESSION
 */
bool ps_visit_constant_expression(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *constant)
{
    VISIT_BEGIN("CONSTANT_EXPRESSION", "");

    ps_identifier identifier = {0};
    ps_symbol *symbol = NULL;

    switch (lexer->current_token.type)
    {
    case PS_TOKEN_INTEGER_VALUE:
        constant->type = &ps_system_integer;
        constant->data.i = lexer->current_token.value.i;
        break;
    case PS_TOKEN_UNSIGNED_VALUE:
        constant->type = &ps_system_unsigned;
        constant->data.u = lexer->current_token.value.u;
        break;
    case PS_TOKEN_CHAR_VALUE:
        constant->type = &ps_system_char;
        constant->data.c = lexer->current_token.value.c;
        break;
    case PS_TOKEN_BOOLEAN_VALUE:
        constant->type = &ps_system_boolean;
        constant->data.b = lexer->current_token.value.b;
        break;
    // case PS_TOKEN_STRING_VALUE:
    //     constant->type = &ps_system_string;
    //     constant->data.s = ps_string_heap_create(interpreter->string_heap, lexer->current_token.value.s);
    //     if (constant->data.s == NULL)
    //     {
    //         interpreter->error = PS_ERROR_OUT_OF_MEMORY;
    //         TRACE_ERROR("STRING_VALUE");
    //     }
    //     break;
    case PS_TOKEN_IDENTIFIER:
        COPY_IDENTIFIER(identifier);
        symbol = ps_interpreter_find_symbol(interpreter, &identifier, false);
        if (symbol == NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
        if (symbol->kind != PS_SYMBOL_KIND_CONSTANT)
            RETURN_ERROR(PS_ERROR_EXPECTED_CONSTANT);
        constant->type = &ps_system_none;
        constant->data.v = NULL;
        if (!ps_interpreter_copy_value(interpreter, symbol->value, constant))
            TRACE_ERROR("COPY");
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    }
    READ_NEXT_TOKEN;

    VISIT_END("OK");
}

/**
 * @details
 *  Visit type reference, for now, only base types:
 *      'INTEGER' | 'UNSIGNED' | 'REAL' | 'BOOLEAN' | 'CHAR'
 *      | 'STRING' [ '[' IDENTIFIER | UNSIGNED ']' ] |
 *      | IDENTIFIER
 *      | ENUMERATION =   '(' IDENTIFIER [ ',' IDENTIFIER ]* ')'
 *        Examples:
 *          TGender = (Male, Female, Other)
 *          TAbilities = (Strength, Intelligence, Wisdom, Dexterity, Constitution, Charisma)
 *          TCharacterClass = (Fighter, Wizard, Cleric, Rogue)
 *          TCharacterRace = (Human, Elf, Dwarf, Halfling, Gnome, HalfOrc)
 *          TDie = (D4, D6, D8, D10, D12, D20, D100)
 *          Days = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
 *      | SUBRANGE    =   LOW_OR_HIGH '..' LOW_OR_HIGH
 *        Examples:
 *          Score = 1..20
 *          WorkDays = Monday..Friday
 *          WeekEnd = Saturday..Sunday
 *          UppercaseLetters = 'A'..'Z'
 *        LOW_OR_HIGH =   UNSIGNED | INTEGER | ORDINAL_CONSTANT_IDENTIFIER
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
bool ps_visit_type_reference(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol **type_symbol)
{
    VISIT_BEGIN("TYPE_REFERENCE", "");

    ps_symbol *symbol = NULL;
    bool advance = true;
    ssize_t len = 0;

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
        READ_NEXT_TOKEN;
        advance = false;
        if (lexer->current_token.type == PS_TOKEN_LEFT_BRACKET)
        {
            // String[MY_CONSTANT]
            READ_NEXT_TOKEN;
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
                RETURN_ERROR(PS_ERROR_EXPECTED_STRING_LENGTH);
            }
            if (len < 1 || len > PS_STRING_MAX_LEN)
                RETURN_ERROR(PS_ERROR_EXPECTED_STRING_LENGTH);
            ps_type_definition *type_def = ps_type_definition_create(PS_TYPE_STRING, PS_TYPE_STRING);
            if (type_def == NULL)
                RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
            type_def->def.s.max = (ps_string_len)len;
            ps_value *value = ps_value_alloc(&ps_system_type_def, (ps_value_data){.t = type_def});
            ps_identifier name = {0};
            snprintf(name, sizeof(name) - 1, "#STRING_%d", (int)len);
            *type_symbol = ps_symbol_alloc(PS_SYMBOL_KIND_TYPE_DEFINITION, &name, value);
        }
        else
        {
            *type_symbol = &ps_system_string;
        }
        break;
    case PS_TOKEN_IDENTIFIER:
        // TODO could be a subrange definition from an enumeration
        symbol = ps_interpreter_find_symbol(interpreter, &lexer->current_token.value.identifier, false);
        if (symbol == NULL)
            RETURN_ERROR(PS_ERROR_UNKOWN_IDENTIFIER);
        if (symbol->kind == PS_SYMBOL_KIND_CONSTANT)
        {
            // Subrange from enumeration
            RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
        }
        if (symbol->kind != PS_SYMBOL_KIND_TYPE_DEFINITION)
            RETURN_ERROR(PS_ERROR_EXPECTED_TYPE);
        *type_symbol = symbol;
        break;
        /* ********** Other types ********** */
    case PS_TOKEN_ARRAY:
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
        // NB: array can be something like "ARRAY [1..10] OF (Value1, Value2, ...)"
        // and may recursively call ps_visit_type_reference()
        // type = ps_system_array.value->data.t; // TODO: parse array definition
        // data.s = NULL; // TODO: allocate array
        // break;
    case PS_TOKEN_CHAR_VALUE: // subrange
        advance = false;
        if (!ps_visit_type_reference_subrange(interpreter, mode, type_symbol, PS_TYPE_CHAR))
            TRACE_ERROR("TYPE_REFERENCE_SUBRANGE");
        break;
    case PS_TOKEN_INTEGER_VALUE: // subrange
    case PS_TOKEN_MINUS:         // subrange with negative integer
        advance = false;
        if (!ps_visit_type_reference_subrange(interpreter, mode, type_symbol, PS_TYPE_INTEGER))
            TRACE_ERROR("TYPE_REFERENCE_SUBRANGE");
        break;
    case PS_TOKEN_UNSIGNED_VALUE: // subrange
        advance = false;
        if (!ps_visit_type_reference_subrange(interpreter, mode, type_symbol, PS_TYPE_UNSIGNED))
            TRACE_ERROR("TYPE_REFERENCE_SUBRANGE");
        break;
    // case PS_TOKEN_BOOLEAN_VALUE:    // subrange => not really useful but possible
    case PS_TOKEN_LEFT_PARENTHESIS: // enumeration
        advance = false;
        if (!ps_visit_type_reference_enum(interpreter, mode, type_symbol))
            TRACE_ERROR("TYPE_REFERENCE_ENUM");
        break;
    case PS_TOKEN_SET:    // set
    case PS_TOKEN_FILE:   // file
    case PS_TOKEN_TEXT:   // text
    case PS_TOKEN_RECORD: // record
    case PS_TOKEN_CARET:  // pointer
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    }

    if (advance)
        READ_NEXT_TOKEN;

    VISIT_END("OK");
}

bool ps_visit_type_reference_enum(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol **type_symbol)
{
    VISIT_BEGIN("TYPE_REFERENCE_ENUM", "");

    // Up to 256 values in an enumeration
    ps_symbol *values[256] = {0};
    int count = 0;

    // re-check that current token is '('
    if (lexer->current_token.type != PS_TOKEN_LEFT_PARENTHESIS)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    READ_NEXT_TOKEN;
    // empty enumeration not allowed
    if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    // Parse enumeration values
    // fprintf(stderr, "ENUM VALUES:\n");
    do
    {
        if (count == 256)
            RETURN_ERROR(PS_ERROR_OVERFLOW);
        if (lexer->current_token.type != PS_TOKEN_IDENTIFIER)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        // Check that enumeration value does not already exist locally (in the same enumeration) or globally (in the
        // symbol table)
        if (ps_interpreter_find_symbol(interpreter, &lexer->current_token.value.identifier, true) != NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS);
        // Create a new symbol for the enumeration value
        ps_value *value = ps_value_alloc(&ps_system_unsigned, (ps_value_data){.u = count});
        ps_symbol *value_symbol =
            ps_symbol_alloc(PS_SYMBOL_KIND_CONSTANT, &lexer->current_token.value.identifier, value);
        if (value_symbol == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
        if (!ps_interpreter_add_symbol(interpreter, value_symbol))
            RETURN_ERROR(PS_ERROR_SYMBOL_NOT_ADDED);
        values[count] = value_symbol;
        count += 1;
        READ_NEXT_TOKEN;
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            READ_NEXT_TOKEN;
            continue;
        }
        if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
            break;
    } while (true);
    READ_NEXT_TOKEN;

    // Register new type definition in symbol table
    ps_symbol_hash_key hash_key_0 = ps_symbol_get_hash_key((char *)values[0]->name);
    ps_symbol_hash_key hash_key_1 = ps_symbol_get_hash_key((char *)values[count - 1]->name);
    ps_identifier name = {0};
    snprintf(name, sizeof(name) - 1, "#ENUM_%d_%08x_%08x", count, hash_key_0, hash_key_1);
    // fprintf(stderr, "ENUM TYPE NAME: '%*s'\n", -(int)PS_IDENTIFIER_LEN, name);
    ps_type_definition *type_def = ps_type_definition_create_enum(count, values);
    if (type_def == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
    ps_value *value = ps_value_alloc(&ps_system_type_def, (ps_value_data){.t = type_def});
    if (value == NULL)
    {
        type_def = ps_type_definition_free(type_def);
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
    }
    ps_symbol *symbol = ps_symbol_alloc(PS_SYMBOL_KIND_TYPE_DEFINITION, &name, value);
    if (symbol == NULL)
    {
        value = ps_value_free(value);
        type_def = ps_type_definition_free(type_def);
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
    }
    if (!ps_interpreter_add_symbol(interpreter, symbol))
    {
        symbol = ps_symbol_free(symbol);
        value = ps_value_free(value);
        type_def = ps_type_definition_free(type_def);
        RETURN_ERROR(PS_ERROR_GENERIC); // TODO: specific error
    }
    *type_symbol = symbol;

    VISIT_END("OK");
}

bool ps_visit_type_reference_subrange(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol **type_symbol,
                                      ps_value_type base)
{
    VISIT_BEGIN("TYPE_REFERENCE_SUBRANGE", "CHAR");

    ps_type_definition_subrange_char c;
    ps_type_definition_subrange_integer i;
    ps_type_definition_subrange_unsigned u;
    // ps_type_definition_subrange_enum e;

    // Parse min value for subrange
    if (base == PS_TYPE_CHAR)
    {
        if (lexer->current_token.type != PS_TOKEN_CHAR_VALUE)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        c.min = lexer->current_token.value.c;
    }
    else if (base == PS_TYPE_INTEGER)
    {
        if (lexer->current_token.type == PS_TOKEN_MINUS)
        {
            // Negative integer subrange: -10..-1
            READ_NEXT_TOKEN;
            if (lexer->current_token.type != PS_TOKEN_INTEGER_VALUE)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
            i.min = -lexer->current_token.value.i;
        }
        else
        {
            if (lexer->current_token.type != PS_TOKEN_INTEGER_VALUE)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
            i.min = lexer->current_token.value.i;
        }
    }
    else if (base == PS_TYPE_UNSIGNED)
    {
        if (lexer->current_token.type != PS_TOKEN_UNSIGNED_VALUE)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        u.min = lexer->current_token.value.u;
    }
    else
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    // Parse '..'
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(PS_TOKEN_RANGE);
    // Parse max value for subrange
    READ_NEXT_TOKEN;
    if (base == PS_TYPE_CHAR)
    {
        if (lexer->current_token.type != PS_TOKEN_CHAR_VALUE)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        c.max = lexer->current_token.value.c;
        if (c.max <= c.min)
            RETURN_ERROR(PS_ERROR_INVALID_SUBRANGE);
    }
    else if (base == PS_TYPE_INTEGER)
    {
        if (lexer->current_token.type == PS_TOKEN_MINUS)
        {
            // Negative integer subrange: -10..-1
            READ_NEXT_TOKEN;
            if (lexer->current_token.type != PS_TOKEN_INTEGER_VALUE)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
            i.max = -lexer->current_token.value.i;
        }
        else
        {
            if (lexer->current_token.type != PS_TOKEN_INTEGER_VALUE)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
            i.max = lexer->current_token.value.i;
        }
        if (i.max <= i.min)
            RETURN_ERROR(PS_ERROR_INVALID_SUBRANGE);
    }
    else if (base == PS_TYPE_UNSIGNED)
    {
        if (lexer->current_token.type != PS_TOKEN_UNSIGNED_VALUE)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        u.max = lexer->current_token.value.u;
    }
    else
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    READ_NEXT_TOKEN;
    // Create type definition for subrange
    ps_type_definition *type_def = NULL;
    ps_identifier name = {0};
    switch (base)
    {
    case PS_TYPE_CHAR:
        type_def = ps_type_definition_create_subrange_char(c.min, c.max);
        snprintf(name, sizeof(name) - 1, "#SUBRANGE_CHAR_%d_%d", c.min, c.max);
        break;
    case PS_TYPE_INTEGER:
        type_def = ps_type_definition_create_subrange_integer(i.min, i.max);
        snprintf(name, sizeof(name) - 1, "#SUBRANGE_INTEGER_%" PS_INTEGER_FMT_10 "_%" PS_INTEGER_FMT_10, i.min, i.max);
        break;
    case PS_TYPE_UNSIGNED:
        type_def = ps_type_definition_create_subrange_unsigned(u.min, u.max);
        snprintf(name, sizeof(name) - 1, "#SUBRANGE_UNSIGNED_%" PS_UNSIGNED_FMT_10 "_%" PS_UNSIGNED_FMT_10, u.min,
                 u.max);
        break;
    default:
        RETURN_ERROR(PS_ERROR_GENERIC);
    }
    if (type_def == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);

    // Register new type definition in symbol table
    ps_value *value = ps_value_alloc(&ps_system_type_def, (ps_value_data){.t = type_def});
    if (value == NULL)
    {
        type_def = ps_type_definition_free(type_def);
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
    }
    ps_symbol *symbol = ps_symbol_alloc(PS_SYMBOL_KIND_TYPE_DEFINITION, &name, value);
    if (symbol == NULL)
    {
        value = ps_value_free(value);
        type_def = ps_type_definition_free(type_def);
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
    }
    if (!ps_interpreter_add_symbol(interpreter, symbol))
    {
        symbol = ps_symbol_free(symbol);
        value = ps_value_free(value);
        type_def = ps_type_definition_free(type_def);
        RETURN_ERROR(PS_ERROR_GENERIC); // TODO: specific error
    }
    *type_symbol = symbol;

    VISIT_END("OK");
}