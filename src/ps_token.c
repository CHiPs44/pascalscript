/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_error.h"
#include "ps_token.h"

struct s_ps_reserved_symbol
{
    ps_token_type token_type;
    char *symbol;
} ps_reserved_symbols[] = {
    // clang-format off
    { .token_type = PS_TOKEN_AT_SIGN,           .symbol = "@"  },
    { .token_type = PS_TOKEN_CARET,             .symbol = "^"  },
    { .token_type = PS_TOKEN_COLON,             .symbol = ":"  },
    { .token_type = PS_TOKEN_COMMA,             .symbol = ","  },
    { .token_type = PS_TOKEN_ASSIGN,            .symbol = ":=" },
    { .token_type = PS_TOKEN_RANGE,             .symbol = ".." },
    { .token_type = PS_TOKEN_DOT,               .symbol = "."  },
    { .token_type = PS_TOKEN_EQUAL,             .symbol = "="  },
    { .token_type = PS_TOKEN_GREATER_OR_EQUAL,  .symbol = ">=" },
    { .token_type = PS_TOKEN_GREATER_THAN,      .symbol = ">"  },
    { .token_type = PS_TOKEN_LEFT_BRACKET,      .symbol = "["  },
    { .token_type = PS_TOKEN_LEFT_PARENTHESIS,  .symbol = "("  },
    { .token_type = PS_TOKEN_LESS_OR_EQUAL,     .symbol = "<=" },
    { .token_type = PS_TOKEN_LESS_THAN,         .symbol = "<"  },
    { .token_type = PS_TOKEN_MINUS,             .symbol = "-"  },
    { .token_type = PS_TOKEN_NOT_EQUAL,         .symbol = "<>" },
    { .token_type = PS_TOKEN_PLUS,              .symbol = "+"  },
    { .token_type = PS_TOKEN_POWER,             .symbol = "**" },
    { .token_type = PS_TOKEN_RIGHT_BRACKET,     .symbol = "]"  },
    { .token_type = PS_TOKEN_RIGHT_PARENTHESIS, .symbol = ")"  },
    { .token_type = PS_TOKEN_SEMI_COLON,        .symbol = ";"  },
    { .token_type = PS_TOKEN_SLASH,             .symbol = "/"  },
    { .token_type = PS_TOKEN_STAR,              .symbol = "*"  },
    // clang-format on
};

char *ps_token_get_reserved_symbol(ps_token_type token_type)
{
    for (size_t i = 0; i < sizeof(ps_reserved_symbols) / sizeof(struct s_ps_reserved_symbol); i++)
    {
        if (ps_reserved_symbols[i].token_type == token_type)
            return ps_reserved_symbols[i].symbol;
    }
    return NULL;
}

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
    case PS_TOKEN_END_OF_FILE:
        type = "EOF";
        snprintf(value, sizeof(value) - 1, "%04d: END_OF_FILE", token->type);
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
    case PS_TOKEN_AT_SIGN:           // @
    case PS_TOKEN_CARET:             // ^
    case PS_TOKEN_COLON:             // :
    case PS_TOKEN_COMMA:             // ,
    case PS_TOKEN_ASSIGN:            // :=
    case PS_TOKEN_RANGE:             // ..
    case PS_TOKEN_DOT:               // .
    case PS_TOKEN_EQUAL:             // =
    case PS_TOKEN_GREATER_OR_EQUAL:  // >=
    case PS_TOKEN_GREATER_THAN:      // >
    case PS_TOKEN_LEFT_BRACKET:      // [
    case PS_TOKEN_LEFT_PARENTHESIS:  // (
    case PS_TOKEN_LESS_OR_EQUAL:     // <=
    case PS_TOKEN_LESS_THAN:         // <
    case PS_TOKEN_MINUS:             // -
    case PS_TOKEN_NOT_EQUAL:         // <>
    case PS_TOKEN_PLUS:              // +
    case PS_TOKEN_POWER:             // **
    case PS_TOKEN_RIGHT_BRACKET:     // ]
    case PS_TOKEN_RIGHT_PARENTHESIS: // )
    case PS_TOKEN_SEMI_COLON:        // ;
    case PS_TOKEN_SLASH:             // /
    case PS_TOKEN_STAR:              // *
        type = "RESERVED";
        snprintf(value, sizeof(value) - 1, "%04d: '%s'", token->type, token->value.identifier);
        break;
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

/// @brief Pascal keywords
/// @note *MUST* be sorted in alphabetical order for dichotomic search below to work
struct s_ps_keyword
{
    char *keyword;
    ps_token_type token_type;
} ps_keywords[] = {
    // clang-format off
    { .keyword = "AND"              , .token_type = PS_TOKEN_AND           },
    { .keyword = "ARRAY"            , .token_type = PS_TOKEN_ARRAY         },
    { .keyword = "BEGIN"            , .token_type = PS_TOKEN_BEGIN         },
    { .keyword = "BOOLEAN"          , .token_type = PS_TOKEN_BOOLEAN       },
    { .keyword = "CASE"             , .token_type = PS_TOKEN_CASE          },
    { .keyword = "CHAR"             , .token_type = PS_TOKEN_CHAR          },
    { .keyword = "CONST"            , .token_type = PS_TOKEN_CONST         },
    { .keyword = "DIV"              , .token_type = PS_TOKEN_DIV           },
    { .keyword = "DO"               , .token_type = PS_TOKEN_DO            },
    { .keyword = "DOWNTO"           , .token_type = PS_TOKEN_DOWNTO        },
    { .keyword = "ELSE"             , .token_type = PS_TOKEN_ELSE          },
    { .keyword = "END"              , .token_type = PS_TOKEN_END           },
    { .keyword = "FILE"             , .token_type = PS_TOKEN_FILE          },
    { .keyword = "FOR"              , .token_type = PS_TOKEN_FOR           },
    { .keyword = "FUNCTION"         , .token_type = PS_TOKEN_FUNCTION      },
    { .keyword = "GOTO"             , .token_type = PS_TOKEN_GOTO          },
    { .keyword = "IF"               , .token_type = PS_TOKEN_IF            },
    { .keyword = "IMPLEMENTATION"   , .token_type = PS_TOKEN_IMPLEMENTATION},
    { .keyword = "IN"               , .token_type = PS_TOKEN_IN            },
    { .keyword = "INTEGER"          , .token_type = PS_TOKEN_INTEGER       },
    { .keyword = "INTERFACE"        , .token_type = PS_TOKEN_INTERFACE     },
    { .keyword = "LABEL"            , .token_type = PS_TOKEN_LABEL         },
    { .keyword = "MOD"              , .token_type = PS_TOKEN_MOD           },
    { .keyword = "NIL"              , .token_type = PS_TOKEN_NIL           },
    { .keyword = "NOT"              , .token_type = PS_TOKEN_NOT           },
    { .keyword = "OF"               , .token_type = PS_TOKEN_OF            },
    { .keyword = "OR"               , .token_type = PS_TOKEN_OR            },
    { .keyword = "OTHERWISE"        , .token_type = PS_TOKEN_OTHERWISE     },
    { .keyword = "PROCEDURE"        , .token_type = PS_TOKEN_PROCEDURE     },
    { .keyword = "PROGRAM"          , .token_type = PS_TOKEN_PROGRAM       },
    { .keyword = "REAL"             , .token_type = PS_TOKEN_REAL          },
    { .keyword = "RECORD"           , .token_type = PS_TOKEN_RECORD        },
    { .keyword = "REPEAT"           , .token_type = PS_TOKEN_REPEAT        },
    { .keyword = "SET"              , .token_type = PS_TOKEN_SET           },
    { .keyword = "SHL"              , .token_type = PS_TOKEN_SHL           },
    { .keyword = "SHR"              , .token_type = PS_TOKEN_SHR           },
    { .keyword = "STRING"           , .token_type = PS_TOKEN_STRING        },
    { .keyword = "THEN"             , .token_type = PS_TOKEN_THEN          },
    { .keyword = "TO"               , .token_type = PS_TOKEN_TO            },
    { .keyword = "TYPE"             , .token_type = PS_TOKEN_TYPE          },
    { .keyword = "UNIT"             , .token_type = PS_TOKEN_UNIT          },
    { .keyword = "UNSIGNED"         , .token_type = PS_TOKEN_UNSIGNED      },
    { .keyword = "UNTIL"            , .token_type = PS_TOKEN_UNTIL         },
    { .keyword = "USES"             , .token_type = PS_TOKEN_USES          },
    { .keyword = "VAR"              , .token_type = PS_TOKEN_VAR           },
    { .keyword = "WHILE"            , .token_type = PS_TOKEN_WHILE         },
    { .keyword = "WITH"             , .token_type = PS_TOKEN_WITH          },
    { .keyword = "XOR"              , .token_type = PS_TOKEN_XOR           },
    // { .keyword = "AS"               , .token_type = PS_TOKEN_AS            },
    // { .keyword = "IS"               , .token_type = PS_TOKEN_IS            },
    // clang-format on
};

char *ps_token_get_keyword(ps_token_type token_type)
{
    for (size_t i = 0; i < sizeof(ps_keywords) / sizeof(struct s_ps_keyword); i++)
    {
        if (ps_keywords[i].token_type == token_type)
            return ps_keywords[i].keyword;
    }
    return NULL;
}

ps_token_type ps_token_is_keyword(char *identifier)
{
    // NB: identifier should already be normalized to uppercase
    int left = 0;
    int right = sizeof(ps_keywords) / sizeof(struct s_ps_keyword) - 1;
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
