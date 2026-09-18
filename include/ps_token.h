/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_TOKEN_H
#define _PS_TOKEN_H

#include "ps_config.h"
#include "ps_system_types.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /*
        1. Mimimalistic
            PROGRAM         BEGIN           END
            CONST           VAR
            BOOLEAN         TRUE*           FALSE*
            INTEGER         UNSIGNED        REAL
            CHAR            STRING
            * these are not reserved words, but they are reserved identifiers
        2. Expressions
            DIV             MOD
            AND             OR              NOT
        3. Decision making
            BOOLEAN         TRUE            FALSE
            IF              THEN            ELSE
            AND             OR              NOT
            CASE            OTHERWISE
        4. Loops
            REPEAT          UNTIL
            WHILE           DO
            FOR             DOWNTO          TO
        5. Types
            TYPE            ARRAY           OF
            FILE            NIL
            RECORD          WITH
        6. More types
            set             in
        7. Mother of all evil?
            goto            label
        8. "Modularity"
            function        procedure
        9. More operators
            SHL             SHR             XOR
        10. More reserved words
            OUT             IN              ON
        11. Units
            UNIT            USES        INTERFACE
            IMPLEMENTATION
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

    /** @brief Token types */
    typedef enum e_ps_token_type
    {
        PS_TOKEN_NONE = 0,
        // Base type values
        // ====================================================================================================c
        PS_TOKEN_INTEGER_VALUE,
        PS_TOKEN_UNSIGNED_VALUE,
        PS_TOKEN_REAL_VALUE,
        PS_TOKEN_BOOLEAN_VALUE,
        PS_TOKEN_CHAR_VALUE,
        PS_TOKEN_STRING_VALUE,
        // Identifier
        // ====================================================================================================c
        PS_TOKEN_IDENTIFIER,
        // Single character tokens: ASCII value
        // ====================================================================================================c
        PS_TOKEN_AT_SIGN = '@',           // "@"  _FUTURE_ address of
        PS_TOKEN_CARET = '^',             // "^"  _FUTURE_ pointer to
        PS_TOKEN_COLON = ':',             // ":"  various uses: type definition, field definition, etc.
        PS_TOKEN_COMMA = ',',             // ","  various uses: separating identifiers, parameters, etc.
        PS_TOKEN_DOT = '.',               // "."  various uses: final dot of program / unit, field access, etc.
        PS_TOKEN_EQ = '=',                // "="  equals to
        PS_TOKEN_GT = '>',                // ">"  greater than
        PS_TOKEN_LEFT_BRACKET = '[',      // "["  array access: opening
        PS_TOKEN_LEFT_PARENTHESIS = '(',  // "("  various uses: opening in expressions, function calls, etc.
        PS_TOKEN_LT = '<',                // "<"  less than
        PS_TOKEN_MINUS = '-',             // "-"  substraction / negation (integer or real)
        PS_TOKEN_PLUS = '+',              // "+"  addition (integer or real) / unary plus
        PS_TOKEN_RIGHT_BRACKET = ']',     // "]"  array access: closing
        PS_TOKEN_RIGHT_PARENTHESIS = ')', // ")"  various uses: closing for expressions, function calls, etc.
        PS_TOKEN_SEMI_COLON = ';',        // ";"  various uses: end of statement, etc.
        PS_TOKEN_SLASH = '/',             // "/"  division (real)
        PS_TOKEN_STAR = '*',              // "*"  multiplication (integer or real)
        // 2 characters symbols: more than 0x7f to avoid conflict with single char tokens
        // ====================================================================================================c
        PS_TOKEN_ASSIGN = 0x80, // := assignment
        PS_TOKEN_GE,            // >= greater or equal
        PS_TOKEN_LE,            // <= less or equal
        PS_TOKEN_NE,            // <> not equal
        PS_TOKEN_POWER,         // ** _FUTURE_ exponentiation
        PS_TOKEN_RANGE,         // .. ranges, i.e. 1..10 or Monday..Friday
        // Operators
        // ====================================================================================================c
        PS_TOKEN_DIV, // division (integer)
        PS_TOKEN_MOD, // modulo (integer)
        PS_TOKEN_AND, // logical or binary and
        PS_TOKEN_OR,  // logical or binary or
        PS_TOKEN_XOR, // logical or binary exclusive or
        PS_TOKEN_NOT, // logical or binary not
        PS_TOKEN_SHL, // shift left
        PS_TOKEN_SHR, // shift right
        PS_TOKEN_IN,  // _FUTURE_ in operator for sets, i.e. 1 in [1,2,3]
        // Reserved words: more than 0x8f to avoid conflict with 2 char symbols & operators
        // ====================================================================================================c
        PS_TOKEN_PROGRAM = 0x90, // PROGRAM
        PS_TOKEN_CONST,          // Constant definition
        PS_TOKEN_TYPE,           // Type definition
        PS_TOKEN_VAR,            // Variable definition / by reference parameter definition
        PS_TOKEN_OUT,            // By reference parameter definition
        PS_TOKEN_FUNCTION,       // Function definition
        PS_TOKEN_PROCEDURE,      // Procedure definition
        PS_TOKEN_BEGIN,          // Beginning of compound statement
        PS_TOKEN_END,            // End of compound statement
        PS_TOKEN_INTEGER,        // Integer type
        PS_TOKEN_UNSIGNED,       // Unsigned integer type
        PS_TOKEN_BOOLEAN,        // Boolean type
        PS_TOKEN_CHAR,           // Char type
        PS_TOKEN_REAL,           // Real type
        PS_TOKEN_STRING,         // String type
        PS_TOKEN_ARRAY,          // Array type
        PS_TOKEN_OF,             // Used by xxx and SET
        PS_TOKEN_IF,             // IF statement
        PS_TOKEN_THEN,           // THEN part of IF statement
        PS_TOKEN_ELSE,           // ELSE part of IF statement
        PS_TOKEN_DO,             // Start of WHILE statement or FOR part
        PS_TOKEN_WHILE,          // WHILE statement
        PS_TOKEN_REPEAT,         // REPEAT statement
        PS_TOKEN_UNTIL,          // UNTIL part of REPEAT statement
        PS_TOKEN_FOR,            // FOR statement
        PS_TOKEN_TO,             // TO part of FOR statement
        PS_TOKEN_DOWNTO,         // DOWNTO part of FOR statement
        PS_TOKEN_NIL,            // _FUTURE_ NIL pointer (NULL / Not In List)
        PS_TOKEN_SET,            // _FUTURE_ SET OF type
        PS_TOKEN_RECORD,         // _FUTURE_ RECORD definition
        PS_TOKEN_WITH,           // _FUTURE_ WITH record access
        PS_TOKEN_FILE,           // _FUTURE_ FILE OF type
        PS_TOKEN_TEXT,           // _FUTURE_ TEXT file type
        PS_TOKEN_CASE,           // _FUTURE_ CASE statement
        PS_TOKEN_OTHERWISE,      // _FUTURE_ OTHERWISE case default
        PS_TOKEN_GOTO,           // _FUTURE_ Root of all evil ;-)
        PS_TOKEN_LABEL,          // _FUTURE_ Part of Root of all evil ;-)
        PS_TOKEN_UNIT,           // _FUTURE_ UNIT definition
        PS_TOKEN_USES,           // _FUTURE_ USES unit reference
        PS_TOKEN_INTERFACE,      // _FUTURE_ INTERFACE unit part
        PS_TOKEN_IMPLEMENTATION, // _FUTURE_ IMPLEMENTATION unit part
        // Make sure token value fits in one byte
        // ====================================================================================================c
        PS_TOKEN_MAX = UINT8_MAX
    } ps_token_type;

    /** @brief Token value */
    typedef union u_ps_token_value {
        char identifier[PS_IDENTIFIER_LEN + 1]; /** @brief Identifier             */
        ps_integer i;                           /** @brief Integer value          */
        ps_unsigned u;                          /** @brief Unsigned integer value */
        ps_real r;                              /** @brief Real value             */
        ps_char c;                              /** @brief Char value             */
        ps_boolean b;                           /** @brief Boolean value          */
        char s[PS_STRING_MAX_LEN + 1];          /** @brief String value           */
    } ps_token_value;

    /** @brief Token itself: type + value*/
    typedef struct s_ps_token
    {
        ps_token_type type;
        ps_token_value value;
    } ps_token;

#define PS_TOKEN_TYPE_SIZE sizeof(ps_token_type)
#define PS_TOKEN_VALUE_SIZE sizeof(ps_token_value)
#define PS_TOKEN_SIZE sizeof(ps_token)

    /** @brief Convert token type to string */
    char *ps_token_type_get_string(ps_token_type token_type, char *default_value);

    /** @brief Dump token to stderr */
    void ps_token_debug(FILE *output, const char *message, ps_token *token);

    /** @brief Convert token to string */
    char *ps_token_dump_value(ps_token *token);

    /** @brief Convert keyword to string */
    char *ps_token_get_keyword(ps_token_type token_type);

    /** @brief Convert identifier to keyword */
    ps_token_type ps_token_is_keyword(const char *identifier);

#ifdef __cplusplus
}
#endif

#endif /* _PS_TOKEN_H */
