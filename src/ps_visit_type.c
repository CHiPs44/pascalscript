/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_executable.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_visit.h"

/**
 * Visit type reference, for now, only base types:
 *      'INTEGER' | 'UNSIGNED' | 'REAL' | 'BOOLEAN' | 'CHAR' 
 *      | 'STRING'
 *      | IDENTIFIER
 * Next steps:
 *      'STRING' [ '[' IDENTIFIER | UNSIGNED ']' ] |
 *      ENUMERATION =   '(' IDENTIFIER [ ',' IDENTIFIER ]* ')'
 *        Examples:
 *          TAbilities = (Strength, Intelligence, Wisdom, Dexterity, Constitution, Charisma)
 *          TCharacterClass = (Fighter, Wizard, Cleric, Rogue)
 *          TCharacterRace = (Human, Elf, Dwarf, Halfling, Gnome, HalfOrc)
 *          TDie = (D4, D6, D8, D10, D12, D20, D100)
 *          Days = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
 *      SUBRANGE    =   LOW_OR_HIGH '..' LOW_OR_HIGH
 *        Examples:
 *          Score = 1..20
 *          WorkDays = Monday..Friday
 *          WeekEnd = Saturday..Sunday
 *          UppercaseLetters = Set Of 'A'..'Z'
 *      LOW_OR_HIGH =   UNSIGNED | INTEGER | ORDINAL_CONSTANT_IDENTIFIER
 *      ARRAY       =   'ARRAY' '[' SUBRANGE | IDENTIFIER [ ',' SUBRANGE | IDENTIFIER ]* ']' 'OF' TYPE_REFERENCE
 *        Examples:
 *          CheckerBoard = Array [1..8, 1..8] Of Boolean
 *      SET         =   'SET' 'OF' ORDINAL_TYPE_REFERENCE
 *        Examples:
 *          Options = Set Of UppercaseLetters
 *      FILE        =   'TEXT' | 'FILE' [ 'OF' TYPE_REFERENCE ]
 *        Examples:
 *          InputFile = Text
 *          ResultFile = File Of Integer
 *      RECORD      =   'RECORD'
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
 *            Genre: Char;
 *            Class: TCharacterClass;
 *            Race: TCharacterRace;
 *            HitDie: TDie;
 *            HitPoints: Integer;
 *            Level: Unsigned;
 *            Experience: Unsigned;
 *            Abilities: Array[TAbilities] Of Unsigned;
 *            Modifiers: Array[TAbilities] Of Integer;
 *          End;
 *      FIELD       = IDENTIFIER ':' TYPE_REFERENCE
 * ???:
 *      POINTER     =   'POINTER'
 */
bool ps_visit_type_reference(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol *type_symbol)
{
    VISIT_BEGIN("TYPE_REFERENCE", "");

    ps_unsigned len = 0;
    ps_symbol *symbol = NULL;
    ps_value_data data = {.t = ps_system_none.value->data.t};
    ps_type_definition *type_def = NULL;
    bool advance = true;

    type_symbol = NULL;
    switch (lexer->current_token.type)
    {
        /* ********** Base types ********** */
    case PS_TOKEN_INTEGER:
        type_symbol = &ps_system_integer;
        break;
    case PS_TOKEN_UNSIGNED:
        type_symbol = &ps_system_unsigned;
        break;
    case PS_TOKEN_REAL:
        type_symbol = &ps_system_real;
        break;
    case PS_TOKEN_BOOLEAN:
        type_symbol = &ps_system_boolean;
        break;
    case PS_TOKEN_CHAR:
        type_symbol = &ps_system_char;
        break;
    case PS_TOKEN_STRING:
        READ_NEXT_TOKEN;
        advance = false;
        if (lexer->current_token.type == PS_TOKEN_LEFT_BRACKET)
        {
            RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
            // READ_NEXT_TOKEN;
            // if (lexer->current_token.type == PS_TOKEN_UNSIGNED_VALUE)
            // {
            //     // String[123]
            //     len = lexer->current_token.value.u;
            // }
            // else if (lexer->current_token.type == PS_TOKEN_IDENTIFIER)
            // {
            //     // String[MY_CONSTANT]
            //     symbol = ps_interpreter_find_symbol(interpreter, &lexer->current_token.value.identifier, false);
            //     if (symbol == NULL)
            //         RETURN_ERROR(PS_ERROR_UNKOWN_IDENTIFIER);
            //     if (symbol->kind != PS_SYMBOL_KIND_CONSTANT)
            //         RETURN_ERROR(PS_ERROR_EXPECTED_CONSTANT);
            //     if (symbol->value->type != ps_system_unsigned.value->data.t ||
            //         symbol->value->type != ps_system_integer.value->data.t || symbol->value->data.i <= 0)
            //         RETURN_ERROR(PS_ERROR_EXPECTED_UNSIGNED);
            //     len = symbol->value->type != ps_system_unsigned.value->data.t ? symbol->value->data.u
            //                                                                   : symbol->value->data.i;
            // }
            // else
            //     RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
            // if (len < 1 || len > PS_STRING_MAX_LEN)
            //     RETURN_ERROR(PS_ERROR_EXPECTED_STRING_LENGTH);
            // // data.t.def.def_string.length = len;
            // // type_def
            // // value = ps_value_alloc(&ps_system_type_def, data);
        }
        else
        {
            type_symbol = &ps_system_string;
        }
        break;
    case PS_TOKEN_IDENTIFIER:
        symbol = ps_interpreter_find_symbol(interpreter, &lexer->current_token.value.identifier, false);
        if (symbol == NULL)
            RETURN_ERROR(PS_ERROR_UNKOWN_IDENTIFIER);
        if (symbol->kind != PS_SYMBOL_KIND_TYPE_DEFINITION)
            RETURN_ERROR(PS_ERROR_EXPECTED_TYPE);
        type_symbol = symbol;
        /* ********** Other types ********** */
    case PS_TOKEN_ARRAY:
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
        // NB: array can be something like "ARRAY [1..10] OF (Value1, Value2, ...)"
        // and may call recusively ps_visit_type_reference()
        // type = ps_system_array.value->data.t; // TODO: parse array definition
        // data.s = NULL; // TODO: allocate array
        // break;
    case PS_TOKEN_LEFT_PARENTHESIS:
    case PS_TOKEN_UNSIGNED_VALUE:
    case PS_TOKEN_INTEGER_VALUE:
    case PS_TOKEN_CHAR_VALUE:
    case PS_TOKEN_BOOLEAN_VALUE:
    case PS_TOKEN_SET:
    case PS_TOKEN_FILE:
    case PS_TOKEN_RECORD:
    case PS_TOKEN_CARET:
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    }

    if (advance)
        READ_NEXT_TOKEN;

    VISIT_END("OK");
}

/**
 * Visit type definition:
 *    IDENTIFIER '=' TYPE_REFERENCE
 */
bool ps_visit_type_definition(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("TYPE_DEFINITION", "");

    ps_type_definition *type_def = NULL;
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
    EXPECT_TOKEN(PS_TOKEN_EQUAL);
    READ_NEXT_TOKEN;
    // TYPE_REFERENCE
    if (!ps_visit_type_reference(interpreter, mode, type_symbol))
        RETURN_ERROR(interpreter->error);

    if (mode == MODE_EXEC)
    {
        // Register new type definition in symbol table
        data.t = type_symbol->value->data.t;
        value = ps_value_alloc(type_def, data);
        if (value == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
        type_symbol = ps_symbol_alloc(PS_SYMBOL_KIND_TYPE_DEFINITION, &type_name, value);
        if (type_symbol == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
        if (!ps_interpreter_add_symbol(interpreter, type_symbol))
            RETURN_ERROR(interpreter->error);
    }

    VISIT_END("OK");
}

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
