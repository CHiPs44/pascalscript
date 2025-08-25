/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_TOKEN_H
#define _PS_TOKEN_H

#include "ps_config.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef PS_IDENTIFIER_MAX
#define PS_IDENTIFIER_MAX 31
#endif

    /*
        1. Mimimalistic
            PROGRAM         BEGIN           END
            CONST           VAR
            INTEGER         UNSIGNED        REAL
            CHAR            STRING
        2. Expressions
            DIV             MOD
        3. Decision making
            BOOLEAN         TRUE            FALSE
            IF              THEN            ELSE
            AND             OR              XOR
            NOT             CASE            OTHERWISE
        4. Loops
            REPEAT          UNTIL
            WHILE           DO
            FOR             DOWNTO          TO
        5. Types
            type            array           of
            file            nil
            record          with
        6. More types
            set             in
        7. Mother of all evil?
            goto            label
        8. "Modularity"
            function        procedure
        9. More operators
            shl             shr             xor
        10. More reserved words
        11. Units
            unit            uses        interface
            implementation
        12. Objects
            ABSTRACT        AS              CLASS
            CONSTRUCTOR     DESTRUCTOR      DYNAMIC
            INHERITED       IS              OBJECT
            OVVERIDE        PRIVATE         PROTECTED
            PUBLIC          REINTRODUCE     SELF
            VIRTUAL
        13. Exceptions
            TRY             EXCEPT         FINALLY
            RAISE           ON
        14. Modifiers
            ABSOLUTE
            ASM
            INLINE
            OPERATOR
            PACKED


        -file           -packed         -record             -nil        -with
        -absolute       -asm            -inline             -operator   -reintroduce
        -unit           -interface      -implementation     -uses
        -constructor    -destructor     -inherited          -object     -self

        forward
    */

    /* THESE ARE NOT TOKENS!
        Comments
            PS_TOKEN_LEFT_COMMENT,        (*
            PS_TOKEN_LEFT_CURLY_BRACKET,  {
            PS_TOKEN_RIGHT_COMMENT,       *)
            PS_TOKEN_RIGHT_CURLY_BRACKET, }
        Numerical base prefixes
            PS_TOKEN_AMPERSAND,         & => Octal
            PS_TOKEN_DOLLAR,            $ => Hexadecimal
            PS_TOKEN_PERCENT,           % => Binary
     */

    typedef enum e_ps_token_type
    {
        PS_TOKEN_NONE = 0,
        // Numeric values
        PS_TOKEN_INTEGER_VALUE,
        PS_TOKEN_UNSIGNED_VALUE,
        PS_TOKEN_REAL_VALUE,
        // Other value types
        PS_TOKEN_BOOLEAN_VALUE,
        PS_TOKEN_CHAR_VALUE,
        PS_TOKEN_STRING_VALUE,
        // Identifier
        PS_TOKEN_IDENTIFIER,
        // Single character tokens
        PS_TOKEN_AT_SIGN = '@',           // @  *FUTURE* address of
        PS_TOKEN_CARET = '^',             // ^  *FUTURE* pointer to
        PS_TOKEN_COLON = ':',             // :  various uses: type definition, field access, etc.
        PS_TOKEN_COMMA = ',',             // ,  various uses: separating identifiers, parameters, etc.
        PS_TOKEN_DOT = '.',               // .  various uses: final dot of program / unit, field access, etc.
        PS_TOKEN_EQUAL = '=',             // =  equals to
        PS_TOKEN_GREATER_THAN = '>',      // >  greater than
        PS_TOKEN_LEFT_BRACKET = '[',      // [  *FUTURE* array access: opening
        PS_TOKEN_LEFT_PARENTHESIS = '(', // (  various uses: opening in expressions, function calls, etc.
        PS_TOKEN_LESS_THAN = '<',         // <  less than
        PS_TOKEN_MINUS = '-',             // -  substraction / negation (integer or real)
        PS_TOKEN_PLUS = '+',              // +  addition (integer or real) / unary plus
        PS_TOKEN_RIGHT_BRACKET = ']',     // ]  *FUTURE* array access: closing
        PS_TOKEN_RIGHT_PARENTHESIS = ')', // )  various uses: closing for expressions, function calls, etc.
        PS_TOKEN_SEMI_COLON = ';',        // ;  various uses: end of statement, etc.
        PS_TOKEN_SLASH = '/',             // /  division (real)
        PS_TOKEN_STAR = '*',              // *  multiplication (integer or real)
        // 2 characters symbols
        PS_TOKEN_ASSIGN = 0x80,    // :=  assignment
        PS_TOKEN_RANGE,            // ..  *FUTURE* ranges, i.e. 1..10
        PS_TOKEN_POWER,            // **  *FUTURE* exponentiation
        PS_TOKEN_NOT_EQUAL,        // <>
        PS_TOKEN_LESS_OR_EQUAL,    // <=
        PS_TOKEN_GREATER_OR_EQUAL, // >=
        // Reserved words
        PS_TOKEN_PROGRAM,
        PS_TOKEN_CONST,
        PS_TOKEN_TYPE,
        PS_TOKEN_VAR,
        PS_TOKEN_FUNCTION,
        PS_TOKEN_PROCEDURE,
        PS_TOKEN_BEGIN,
        PS_TOKEN_END,
        PS_TOKEN_INTEGER,
        PS_TOKEN_UNSIGNED,
        PS_TOKEN_BOOLEAN,
        PS_TOKEN_CHAR,
        PS_TOKEN_REAL,
        PS_TOKEN_STRING,
        PS_TOKEN_NIL,    // *FUTURE*
        PS_TOKEN_ARRAY,  // *FUTURE*
        PS_TOKEN_OF,     // *FUTURE*
        PS_TOKEN_SET,    // *FUTURE*
        PS_TOKEN_RECORD, // *FUTURE*
        PS_TOKEN_WITH,   // *FUTURE*
        PS_TOKEN_FILE,   // *FUTURE*
        PS_TOKEN_IF,
        PS_TOKEN_THEN,
        PS_TOKEN_ELSE,
        PS_TOKEN_DO,
        PS_TOKEN_WHILE,
        PS_TOKEN_REPEAT,
        PS_TOKEN_UNTIL,
        PS_TOKEN_FOR,
        PS_TOKEN_TO,
        PS_TOKEN_DOWNTO,
        PS_TOKEN_CASE,           // *FUTURE*
        PS_TOKEN_OTHERWISE,      // *FUTURE*
        PS_TOKEN_GOTO,           // *FUTURE*
        PS_TOKEN_LABEL,          // *FUTURE*
        PS_TOKEN_UNIT,           // *FUTURE*
        PS_TOKEN_USES,           // *FUTURE*
        PS_TOKEN_INTERFACE,      // *FUTURE*
        PS_TOKEN_IMPLEMENTATION, // *FUTURE*
        // Operators
        PS_TOKEN_DIV, // division (integer)
        PS_TOKEN_MOD, // modulo (integer)
        PS_TOKEN_AND, // logical or binary and
        PS_TOKEN_OR,  // logical or binary or
        PS_TOKEN_XOR, // logical or binary exclusive or
        PS_TOKEN_NOT, // logical or binary not
        PS_TOKEN_SHL, // shift left
        PS_TOKEN_SHR, // shift right
        PS_TOKEN_IN,  // *FUTURE* in operator for sets, i.e. 1 in [1,2,3]
        // Make sure token value fits in one byte
        PS_TOKEN_MAX = UINT8_MAX
    } ps_token_type;

    typedef union u_ps_token_value {
        char identifier[PS_IDENTIFIER_MAX + 1];
        ps_integer i;
        ps_unsigned u;
        ps_real r;
        ps_char c;
        ps_boolean b;
        char s[PS_STRING_MAX_LEN + 1];
    } ps_token_value;

    typedef struct s_ps_token
    {
        ps_token_type type;
        ps_token_value value;
    } ps_token;

#define PS_TOKEN_TYPE_SIZE sizeof(ps_token_type)
#define PS_TOKEN_VALUE_SIZE sizeof(ps_token_value)
#define PS_TOKEN_SIZE sizeof(ps_token)

    char *ps_token_get_reserved_symbol(ps_token_type token_type);
    void ps_token_debug(FILE *output, char *message, ps_token *token);
    char *ps_token_dump_value(ps_token *token);
    char *ps_token_get_keyword(ps_token_type token_type);
    ps_token_type ps_token_is_keyword(char *identifier);

#ifdef __cplusplus
}
#endif

#endif /* _PS_TOKEN_H */
