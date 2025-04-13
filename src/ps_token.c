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
    static char string[(PS_STRING_MAX_LEN / 4) + 1];

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
    ps_token_type token_type;
    char *keyword;
} ps_keywords[] = {
    // clang-format off
    { .token_type = PS_TOKEN_AND           , .keyword = "AND"              },
    { .token_type = PS_TOKEN_ARRAY         , .keyword = "ARRAY"            },
    { .token_type = PS_TOKEN_BEGIN         , .keyword = "BEGIN"            },
    { .token_type = PS_TOKEN_BOOLEAN       , .keyword = "BOOLEAN"          },
    { .token_type = PS_TOKEN_CASE          , .keyword = "CASE"             },
    { .token_type = PS_TOKEN_CHAR          , .keyword = "CHAR"             },
    { .token_type = PS_TOKEN_CONST         , .keyword = "CONST"            },
    { .token_type = PS_TOKEN_DIV           , .keyword = "DIV"              },
    { .token_type = PS_TOKEN_DO            , .keyword = "DO"               },
    { .token_type = PS_TOKEN_DOWNTO        , .keyword = "DOWNTO"           },
    { .token_type = PS_TOKEN_ELSE          , .keyword = "ELSE"             },
    { .token_type = PS_TOKEN_END           , .keyword = "END"              },
    { .token_type = PS_TOKEN_FALSE         , .keyword = "FALSE"            },
    { .token_type = PS_TOKEN_FILE          , .keyword = "FILE"             },
    { .token_type = PS_TOKEN_FOR           , .keyword = "FOR"              },
    { .token_type = PS_TOKEN_FUNCTION      , .keyword = "FUNCTION"         },
    { .token_type = PS_TOKEN_GOTO          , .keyword = "GOTO"             },
    { .token_type = PS_TOKEN_IF            , .keyword = "IF"               },
    { .token_type = PS_TOKEN_IMPLEMENTATION, .keyword = "IMPLEMENTATION"   },
    { .token_type = PS_TOKEN_IN            , .keyword = "IN"               },
    { .token_type = PS_TOKEN_INTEGER       , .keyword = "INTEGER"          },
    { .token_type = PS_TOKEN_INTERFACE     , .keyword = "INTERFACE"        },
    { .token_type = PS_TOKEN_LABEL         , .keyword = "LABEL"            },
    { .token_type = PS_TOKEN_MOD           , .keyword = "MOD"              },
    { .token_type = PS_TOKEN_NIL           , .keyword = "NIL"              },
    { .token_type = PS_TOKEN_NOT           , .keyword = "NOT"              },
    { .token_type = PS_TOKEN_OF            , .keyword = "OF"               },
    { .token_type = PS_TOKEN_OR            , .keyword = "OR"               },
    { .token_type = PS_TOKEN_OTHERWISE     , .keyword = "OTHERWISE"        },
    { .token_type = PS_TOKEN_PROCEDURE     , .keyword = "PROCEDURE"        },
    { .token_type = PS_TOKEN_PROGRAM       , .keyword = "PROGRAM"          },
    { .token_type = PS_TOKEN_REAL          , .keyword = "REAL"             },
    { .token_type = PS_TOKEN_RECORD        , .keyword = "RECORD"           },
    { .token_type = PS_TOKEN_REPEAT        , .keyword = "REPEAT"           },
    { .token_type = PS_TOKEN_SET           , .keyword = "SET"              },
    { .token_type = PS_TOKEN_SHL           , .keyword = "SHL"              },
    { .token_type = PS_TOKEN_SHR           , .keyword = "SHR"              },
    { .token_type = PS_TOKEN_STRING        , .keyword = "STRING"           },
    { .token_type = PS_TOKEN_THEN          , .keyword = "THEN"             },
    { .token_type = PS_TOKEN_TO            , .keyword = "TO"               },
    { .token_type = PS_TOKEN_TRUE          , .keyword = "TRUE"             },
    { .token_type = PS_TOKEN_TYPE          , .keyword = "TYPE"             },
    { .token_type = PS_TOKEN_UNIT          , .keyword = "UNIT"             },
    { .token_type = PS_TOKEN_UNSIGNED      , .keyword = "UNSIGNED"         },
    { .token_type = PS_TOKEN_UNTIL         , .keyword = "UNTIL"            },
    { .token_type = PS_TOKEN_USES          , .keyword = "USES"             },
    { .token_type = PS_TOKEN_VAR           , .keyword = "VAR"              },
    { .token_type = PS_TOKEN_WHILE         , .keyword = "WHILE"            },
    { .token_type = PS_TOKEN_WITH          , .keyword = "WITH"             },
    { .token_type = PS_TOKEN_WRITE         , .keyword = "WRITE"            },
    { .token_type = PS_TOKEN_WRITELN       , .keyword = "WRITELN"          },
    { .token_type = PS_TOKEN_XOR           , .keyword = "XOR"              },
    // clang-format on
};

ps_token_type ps_token_is_keyword(char *identifier)
{
    // NB: identifier should already be normalized to uppercase
    // TODO dichotomic search instead of sequential one
    for (int i = 0; i < sizeof(ps_keywords) / sizeof(struct s_ps_keyword); i += 1)
    {
        // printf("ps_token_is_keyword: test=%s, keyword=%s\n", identifier, ps_keywords[i].keyword);
        if (strcmp(identifier, ps_keywords[i].keyword) == 0)
        {
            return ps_keywords[i].token_type;
        }
    }
    return PS_TOKEN_IDENTIFIER;
}
