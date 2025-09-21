/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_error.h"
#include "ps_token.h"

void ps_token_debug(FILE *output, char *message, ps_token *token)
{
    if (output == NULL)
        output = stderr;
    fprintf(output, "%s\t%s\n", message == NULL || 0 == strlen(message) ? "TOKEN: " : message,
            ps_token_dump_value(token));
}

char *ps_token_dump_value(ps_token *token)
{
    ps_token_type token_type;
    char *type;
    static char buffer[128];
    static char value[80];
    static char string[64];

    switch (token->type)
    {
    case PS_TOKEN_NONE:
        type = "NONE";
        snprintf(value, sizeof(value) - 1, "%04d: NONE", token->type);
        break;
    case PS_TOKEN_INTEGER_VALUE:
        type = "INTEGER";
        snprintf(value, sizeof(value) - 1, "%04d: %" PS_INTEGER_FMT_10, token->type, token->value.i);
        break;
    case PS_TOKEN_UNSIGNED_VALUE:
        type = "UNSIGNED";
        snprintf(value, sizeof(value) - 1, "%04d: %" PS_UNSIGNED_FMT_10, token->type, token->value.u);
        break;
    case PS_TOKEN_REAL_VALUE:
        type = "REAL";
        snprintf(value, sizeof(value) - 1, "%04d: %" PS_REAL_FMT, token->type, token->value.r);
        break;
    case PS_TOKEN_BOOLEAN_VALUE:
        type = "BOOLEAN";
        snprintf(value, sizeof(value) - 1, "%04d: '%s'", token->type, token->value.b ? "TRUE" : "FALSE");
        break;
    case PS_TOKEN_CHAR_VALUE:
        type = "CHAR";
        snprintf(value, sizeof(value) - 1, "%04d: '%c'", token->type, token->value.c);
        break;
    case PS_TOKEN_STRING_VALUE:
        type = "STRING";
        strncpy(string, (char *)token->value.s, sizeof(string) - 1);
        snprintf(value, sizeof(value) - 1, "%04d: '%s'", token->type, string);
        break;
    case PS_TOKEN_IDENTIFIER:
        type = "IDENTIFIER";
        snprintf(value, sizeof(value) - 1, "%04d: '%s'", token->type, token->value.identifier);
        break;
    /* These are now handled as keywords, see below */
    // 1 character symbols
    case PS_TOKEN_AT_SIGN:           // @
    case PS_TOKEN_CARET:             // ^
    case PS_TOKEN_COLON:             // :
    case PS_TOKEN_COMMA:             // ,
    case PS_TOKEN_DOT:               // .
    case PS_TOKEN_EQUAL:             // =
    case PS_TOKEN_GT:                // >
    case PS_TOKEN_LEFT_BRACKET:      // [
    case PS_TOKEN_LEFT_PARENTHESIS:  // (
    case PS_TOKEN_LT:                // <
    case PS_TOKEN_MINUS:             // -
    case PS_TOKEN_PLUS:              // +
    case PS_TOKEN_RIGHT_BRACKET:     // ]
    case PS_TOKEN_RIGHT_PARENTHESIS: // )
    case PS_TOKEN_SEMI_COLON:        // ;
    case PS_TOKEN_SLASH:             // /
    case PS_TOKEN_STAR:              // *
    // 2 characters symbols
    case PS_TOKEN_ASSIGN: // :=
    case PS_TOKEN_GE:     // >=
    case PS_TOKEN_LE:     // <=
    case PS_TOKEN_NE:     // <>
    case PS_TOKEN_POWER:  // **
    case PS_TOKEN_RANGE:  // ..
        type = "KEYWORD";
        snprintf(value, sizeof(value) - 1, "%04d: '%s'", token->type, token->value.identifier);
        break;
    /**/
    default:
        token_type = ps_token_is_keyword(token->value.identifier);
        if (token_type == PS_TOKEN_IDENTIFIER)
        {
            type = "UNKNOWN";
            snprintf(value, sizeof(value) - 1, "'%s'", "?");
        }
        else
        {
            type = "KEYWORD";
            snprintf(value, sizeof(value) - 1, "%04d: '%s'", token->type, token->value.identifier);
        }
        break;
    }
    snprintf(buffer, sizeof(buffer) - 1, "type=%-16s, value=%s", type, value);
    return buffer;
}

/** @brief PascalScript reserved keywords */
/** @note *MUST* be sorted in alphabetical order for dichotomic search below to work */
struct s_ps_keyword
{
    char *keyword;
    ps_token_type token_type;
    bool is_symbol;
} ps_keywords[] = {
    // clang-format off
    { .keyword = "-"                , .token_type = PS_TOKEN_MINUS            , .is_symbol = true  },
    { .keyword = ","                , .token_type = PS_TOKEN_COMMA            , .is_symbol = true  },
    { .keyword = ";"                , .token_type = PS_TOKEN_SEMI_COLON       , .is_symbol = true  },
    { .keyword = ":"                , .token_type = PS_TOKEN_COLON            , .is_symbol = true  },
    { .keyword = ":="               , .token_type = PS_TOKEN_ASSIGN           , .is_symbol = true  },
    { .keyword = ".."               , .token_type = PS_TOKEN_RANGE            , .is_symbol = true  },
    { .keyword = "."                , .token_type = PS_TOKEN_DOT              , .is_symbol = true  },
    { .keyword = "("                , .token_type = PS_TOKEN_LEFT_PARENTHESIS , .is_symbol = true  },
    { .keyword = ")"                , .token_type = PS_TOKEN_RIGHT_PARENTHESIS, .is_symbol = true  },
    { .keyword = "["                , .token_type = PS_TOKEN_LEFT_BRACKET     , .is_symbol = true  },
    { .keyword = "]"                , .token_type = PS_TOKEN_RIGHT_BRACKET    , .is_symbol = true  },
    { .keyword = "@"                , .token_type = PS_TOKEN_AT_SIGN          , .is_symbol = true  },
    { .keyword = "*"                , .token_type = PS_TOKEN_STAR             , .is_symbol = true  },
    { .keyword = "**"               , .token_type = PS_TOKEN_POWER            , .is_symbol = true  },
    { .keyword = "/"                , .token_type = PS_TOKEN_SLASH            , .is_symbol = true  },
    { .keyword = "^"                , .token_type = PS_TOKEN_CARET            , .is_symbol = true  },
    { .keyword = "+"                , .token_type = PS_TOKEN_PLUS             , .is_symbol = true  },
    { .keyword = "<"                , .token_type = PS_TOKEN_LT               , .is_symbol = true  },
    { .keyword = "<="               , .token_type = PS_TOKEN_LE               , .is_symbol = true  },
    { .keyword = "<>"               , .token_type = PS_TOKEN_NE               , .is_symbol = true  },
    { .keyword = "="                , .token_type = PS_TOKEN_EQUAL            , .is_symbol = true  },
    { .keyword = ">"                , .token_type = PS_TOKEN_GT               , .is_symbol = true  },
    { .keyword = ">="               , .token_type = PS_TOKEN_GE               , .is_symbol = true  },
    { .keyword = "AND"              , .token_type = PS_TOKEN_AND              , .is_symbol = false },
    { .keyword = "ARRAY"            , .token_type = PS_TOKEN_ARRAY            , .is_symbol = false },
    { .keyword = "BEGIN"            , .token_type = PS_TOKEN_BEGIN            , .is_symbol = false },
    { .keyword = "BOOLEAN"          , .token_type = PS_TOKEN_BOOLEAN          , .is_symbol = false },
    { .keyword = "CASE"             , .token_type = PS_TOKEN_CASE             , .is_symbol = false },
    { .keyword = "CHAR"             , .token_type = PS_TOKEN_CHAR             , .is_symbol = false },
    { .keyword = "CONST"            , .token_type = PS_TOKEN_CONST            , .is_symbol = false },
    { .keyword = "DIV"              , .token_type = PS_TOKEN_DIV              , .is_symbol = false },
    { .keyword = "DO"               , .token_type = PS_TOKEN_DO               , .is_symbol = false },
    { .keyword = "DOWNTO"           , .token_type = PS_TOKEN_DOWNTO           , .is_symbol = false },
    { .keyword = "ELSE"             , .token_type = PS_TOKEN_ELSE             , .is_symbol = false },
    { .keyword = "END"              , .token_type = PS_TOKEN_END              , .is_symbol = false },
    { .keyword = "FILE"             , .token_type = PS_TOKEN_FILE             , .is_symbol = false },
    { .keyword = "FOR"              , .token_type = PS_TOKEN_FOR              , .is_symbol = false },
    { .keyword = "FUNCTION"         , .token_type = PS_TOKEN_FUNCTION         , .is_symbol = false },
    { .keyword = "GOTO"             , .token_type = PS_TOKEN_GOTO             , .is_symbol = false },
    { .keyword = "IF"               , .token_type = PS_TOKEN_IF               , .is_symbol = false },
    { .keyword = "IMPLEMENTATION"   , .token_type = PS_TOKEN_IMPLEMENTATION   , .is_symbol = false },
    { .keyword = "IN"               , .token_type = PS_TOKEN_IN               , .is_symbol = false },
    { .keyword = "INTEGER"          , .token_type = PS_TOKEN_INTEGER          , .is_symbol = false },
    { .keyword = "INTERFACE"        , .token_type = PS_TOKEN_INTERFACE        , .is_symbol = false },
    { .keyword = "LABEL"            , .token_type = PS_TOKEN_LABEL            , .is_symbol = false },
    { .keyword = "MOD"              , .token_type = PS_TOKEN_MOD              , .is_symbol = false },
    { .keyword = "NIL"              , .token_type = PS_TOKEN_NIL              , .is_symbol = false },
    { .keyword = "NOT"              , .token_type = PS_TOKEN_NOT              , .is_symbol = false },
    { .keyword = "OF"               , .token_type = PS_TOKEN_OF               , .is_symbol = false },
    { .keyword = "OR"               , .token_type = PS_TOKEN_OR               , .is_symbol = false },
    { .keyword = "OTHERWISE"        , .token_type = PS_TOKEN_OTHERWISE        , .is_symbol = false },
    { .keyword = "PROCEDURE"        , .token_type = PS_TOKEN_PROCEDURE        , .is_symbol = false },
    { .keyword = "PROGRAM"          , .token_type = PS_TOKEN_PROGRAM          , .is_symbol = false },
    { .keyword = "REAL"             , .token_type = PS_TOKEN_REAL             , .is_symbol = false },
    { .keyword = "RECORD"           , .token_type = PS_TOKEN_RECORD           , .is_symbol = false },
    { .keyword = "REPEAT"           , .token_type = PS_TOKEN_REPEAT           , .is_symbol = false },
    { .keyword = "SET"              , .token_type = PS_TOKEN_SET              , .is_symbol = false },
    { .keyword = "SHL"              , .token_type = PS_TOKEN_SHL              , .is_symbol = false },
    { .keyword = "SHR"              , .token_type = PS_TOKEN_SHR              , .is_symbol = false },
    { .keyword = "STRING"           , .token_type = PS_TOKEN_STRING           , .is_symbol = false },
    { .keyword = "TEXT"             , .token_type = PS_TOKEN_TEXT             , .is_symbol = false },
    { .keyword = "THEN"             , .token_type = PS_TOKEN_THEN             , .is_symbol = false },
    { .keyword = "TO"               , .token_type = PS_TOKEN_TO               , .is_symbol = false },
    { .keyword = "TYPE"             , .token_type = PS_TOKEN_TYPE             , .is_symbol = false },
    { .keyword = "UNIT"             , .token_type = PS_TOKEN_UNIT             , .is_symbol = false },
    { .keyword = "UNSIGNED"         , .token_type = PS_TOKEN_UNSIGNED         , .is_symbol = false },
    { .keyword = "UNTIL"            , .token_type = PS_TOKEN_UNTIL            , .is_symbol = false },
    { .keyword = "USES"             , .token_type = PS_TOKEN_USES             , .is_symbol = false },
    { .keyword = "VAR"              , .token_type = PS_TOKEN_VAR              , .is_symbol = false },
    { .keyword = "WHILE"            , .token_type = PS_TOKEN_WHILE            , .is_symbol = false },
    { .keyword = "WITH"             , .token_type = PS_TOKEN_WITH             , .is_symbol = false },
    { .keyword = "XOR"              , .token_type = PS_TOKEN_XOR              , .is_symbol = false },
    // clang-format on.
};
#define PS_KEYWORDS_COUNT (sizeof(ps_keywords) / sizeof(struct s_ps_keyword))

char *ps_token_get_keyword(ps_token_type token_type)
{
    for (size_t i = 0; i < PS_KEYWORDS_COUNT; i++)
    {
        if (ps_keywords[i].token_type == token_type)
            return ps_keywords[i].keyword;
    }
    return NULL;
}

ps_token_type ps_token_is_keyword(char *identifier)
{
    // NB: identifier must already be normalized to uppercase
    int left = 0;
    int right = PS_KEYWORDS_COUNT - 1;
    int mid, cmp;
    while (left <= right)
    {
        mid = left + (right - left) / 2;
        cmp = strcmp(identifier, ps_keywords[mid].keyword);
        if (cmp == 0)
            return ps_keywords[mid].token_type;
        else if (cmp < 0)
            right = mid - 1;
        else
            left = mid + 1;
    }
    return PS_TOKEN_IDENTIFIER;
}
