/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_error.h"
#include "ps_token.h"

void ps_token_dump(ps_token *token)
{
    ps_token_type token_type;
    char *type;
    static char buffer[128];
    static char string[96];

    switch (token->type)
    {
    case PS_TOKEN_NONE:
        type = "NONE";
        snprintf(buffer, sizeof(buffer) - 1, "%04d: NONE", token->type);
        break;
    case PS_TOKEN_END_OF_FILE:
        type = "EOF";
        snprintf(buffer, sizeof(buffer) - 1, "%04d: END_OF_FILE", token->type);
        break;
    case PS_TOKEN_INTEGER_VALUE:
        type = "INTEGER";
        snprintf(buffer, sizeof(buffer) - 1, "%04d: %d", token->type, token->value.i);
        break;
    case PS_TOKEN_UNSIGNED_VALUE:
        type = "UNSIGNED";
        snprintf(buffer, sizeof(buffer) - 1, "%04d: %u", token->type, token->value.u);
        break;
    case PS_TOKEN_REAL_VALUE:
        type = "REAL";
        snprintf(buffer, sizeof(buffer) - 1, "%04d: %f", token->type, token->value.r);
        break;
    case PS_TOKEN_BOOLEAN_VALUE:
        type = "BOOLEAN";
        snprintf(buffer, sizeof(buffer) - 1, "%04d: '%s'", token->type, token->value.b ? "TRUE" : "FALSE");
        break;
    case PS_TOKEN_CHAR_VALUE:
        type = "CHAR";
        snprintf(buffer, sizeof(buffer) - 1, "%04d: '%c'", token->type, token->value.c);
        break;
    case PS_TOKEN_STRING_VALUE:
        type = "STRING";
        strncpy(string, (char *)token->value.s, sizeof(string) - 1);
        snprintf(buffer, sizeof(buffer) - 1, "%04d: '%s'", token->type, string);
        break;
    case PS_TOKEN_IDENTIFIER:
        type = "IDENTIFIER";
        snprintf(buffer, sizeof(buffer) - 1, "%04d: '%s'", token->type, token->value.identifier);
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
        snprintf(buffer, sizeof(buffer) - 1, "%04d: '%s'", token->type, token->value.identifier);
        break;
    default:
        token_type = ps_token_is_keyword(token->value.identifier);
        if (token_type == PS_TOKEN_IDENTIFIER)
        {
            type = "UNKNOWN";
            snprintf(buffer, sizeof(buffer) - 1, "'%s'", "?");
        }
        else
        {
            type = "KEYWORD";
            snprintf(buffer, sizeof(buffer) - 1, "%04d: '%s'", token->type, token->value.identifier);
        }
        break;
    }
    fprintf(stderr, "TOKEN: type=%-16s, value=%s\n", type, buffer);
}

struct s_ps_keyword
{
    char *keyword;
    ps_token_type token_type;
} ps_keywords[] = {
    // clang-format off
    { .keyword = "AND"              , .token_type = PS_TOKEN_AND           },
    { .keyword = "ARRAY"            , .token_type = PS_TOKEN_ARRAY         },
    { .keyword = "AS"               , .token_type = PS_TOKEN_AS            },
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
    { .keyword = "FALSE"            , .token_type = PS_TOKEN_FALSE         },
    { .keyword = "FILE"             , .token_type = PS_TOKEN_FILE          },
    { .keyword = "FOR"              , .token_type = PS_TOKEN_FOR           },
    { .keyword = "FUNCTION"         , .token_type = PS_TOKEN_FUNCTION      },
    { .keyword = "GOTO"             , .token_type = PS_TOKEN_GOTO          },
    { .keyword = "IF"               , .token_type = PS_TOKEN_IF            },
    { .keyword = "IMPLEMENTATION"   , .token_type = PS_TOKEN_IMPLEMENTATION},
    { .keyword = "IN"               , .token_type = PS_TOKEN_IN            },
    { .keyword = "INTEGER"          , .token_type = PS_TOKEN_INTEGER       },
    { .keyword = "INTERFACE"        , .token_type = PS_TOKEN_INTERFACE     },
    { .keyword = "IS"               , .token_type = PS_TOKEN_IS            },
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
    { .keyword = "TRUE"             , .token_type = PS_TOKEN_TRUE          },
    { .keyword = "TYPE"             , .token_type = PS_TOKEN_TYPE          },
    { .keyword = "UNIT"             , .token_type = PS_TOKEN_UNIT          },
    { .keyword = "UNSIGNED"         , .token_type = PS_TOKEN_UNSIGNED      },
    { .keyword = "UNTIL"            , .token_type = PS_TOKEN_UNTIL         },
    { .keyword = "USES"             , .token_type = PS_TOKEN_USES          },
    { .keyword = "VAR"              , .token_type = PS_TOKEN_VAR           },
    { .keyword = "WHILE"            , .token_type = PS_TOKEN_WHILE         },
    { .keyword = "WITH"             , .token_type = PS_TOKEN_WITH          },
    { .keyword = "XOR"              , .token_type = PS_TOKEN_XOR           },
    // clang-format on
};

ps_token_type ps_token_is_keyword(ps_identifier *identifier)
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
