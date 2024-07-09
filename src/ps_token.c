/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <string.h>

#include "ps_error.h"
#include "ps_token.h"

void ps_token_dump(ps_token *token)
{
    ps_token_type token_type;
    char *type;
    char buffer[256 + 16];

    switch (token->type)
    {
    case TOKEN_IDENTIFIER:
        type = "IDENTIFIER";
        snprintf(buffer, 256 + 16, "%04d: '%s'", token->type, token->value.identifier);
        break;
    case TOKEN_INTEGER_VALUE:
        type = "INTEGER";
        snprintf(buffer, 256 + 16, "%04d: %d", token->type, token->value.i);
        break;
    case TOKEN_CARDINAL_VALUE:
        type = "CARDINAL";
        snprintf(buffer, 256 + 16, "%04d: %u", token->type, token->value.u);
        break;
    case TOKEN_REAL_VALUE:
        type = "REAL";
        snprintf(buffer, 256 + 16, "%04d: %f", token->type, token->value.r);
        break;
    case TOKEN_CHAR_VALUE:
        type = "CHAR";
        snprintf(buffer, 256 + 16, "%04d: '%c'", token->type, token->value.c);
        break;
    case TOKEN_STRING_VALUE:
        type = "STRING";
        snprintf(buffer, 256 + 16, "%04d: '%s'", token->type, token->value.s);
        break;
    case TOKEN_AT_SIGN:           // @
    case TOKEN_CARET:             // ^
    case TOKEN_COLON:             // :
    case TOKEN_COMMA:             // ,
    case TOKEN_DOT_COLON:         // :=
    case TOKEN_DOT_DOT:           // ..
    case TOKEN_DOT:               // .
    case TOKEN_EQUAL:             // =
    case TOKEN_GREATER_OR_EQUAL:  // >=
    case TOKEN_GREATER_THAN:      // >
    case TOKEN_LEFT_BRACKET:      // [
    case TOKEN_LEFT_PARENTHESIS:  // (
    case TOKEN_LESS_OR_EQUAL:     // <=
    case TOKEN_LESS_THAN:         // <
    case TOKEN_MINUS:             // -
    case TOKEN_NOT_EQUAL:         // <>
    case TOKEN_PLUS:              // +
    case TOKEN_RIGHT_BRACKET:     // ]
    case TOKEN_RIGHT_PARENTHESIS: // )
    case TOKEN_SEMI_COLON:        // ;
    case TOKEN_SLASH:             // /
    case TOKEN_STAR:              // *
        type = "RESERVED";
        snprintf(buffer, 256 + 16, "%04d: '%s'", token->type, token->value.identifier);
        break;
    default:
        token_type = ps_token_is_keyword(token->value.identifier);
        if (token_type == TOKEN_IDENTIFIER)
        {
            type = "UNKNOWN";
            snprintf(buffer, 256 + 16, "'%s'", "?");
        }
        else
        {
            type = "KEYWORD";
            snprintf(buffer, 256 + 16, "%04d: '%s'", token->type, token->value.identifier);
        }
        break;
    }
    printf("TOKEN: type=%-16s, value=%s\n", type, buffer);
}

ps_keyword ps_keywords[] = {
    // clang-format off
    { .token_type = TOKEN_AND           , .keyword = "AND"              },
    { .token_type = TOKEN_ARRAY         , .keyword = "ARRAY"            },
    { .token_type = TOKEN_BEGIN         , .keyword = "BEGIN"            },
    { .token_type = TOKEN_BOOLEAN       , .keyword = "BOOLEAN"          },
    { .token_type = TOKEN_CARDINAL      , .keyword = "CARDINAL"         },
    { .token_type = TOKEN_CASE          , .keyword = "CASE"             },
    { .token_type = TOKEN_CHAR          , .keyword = "CHAR"             },
    { .token_type = TOKEN_CONST         , .keyword = "CONST"            },
    { .token_type = TOKEN_DIV           , .keyword = "DIV"              },
    { .token_type = TOKEN_DO            , .keyword = "DO"               },
    { .token_type = TOKEN_DOWNTO        , .keyword = "DOWNTO"           },
    { .token_type = TOKEN_ELSE          , .keyword = "ELSE"             },
    { .token_type = TOKEN_END           , .keyword = "END"              },
    { .token_type = TOKEN_FALSE         , .keyword = "FALSE"            },
    { .token_type = TOKEN_FILE          , .keyword = "FILE"             },
    { .token_type = TOKEN_FOR           , .keyword = "FOR"              },
    { .token_type = TOKEN_FUNCTION      , .keyword = "FUNCTION"         },
    { .token_type = TOKEN_GOTO          , .keyword = "GOTO"             },
    { .token_type = TOKEN_IF            , .keyword = "IF"               },
    { .token_type = TOKEN_IMPLEMENTATION, .keyword = "IMPLEMENTATION"   },
    { .token_type = TOKEN_IN            , .keyword = "IN"               },
    { .token_type = TOKEN_INTEGER       , .keyword = "INTEGER"          },
    { .token_type = TOKEN_INTERFACE     , .keyword = "INTERFACE"        },
    { .token_type = TOKEN_LABEL         , .keyword = "LABEL"            },
    { .token_type = TOKEN_MOD           , .keyword = "MOD"              },
    { .token_type = TOKEN_NIL           , .keyword = "NIL"              },
    { .token_type = TOKEN_NOT           , .keyword = "NOT"              },
    { .token_type = TOKEN_OF            , .keyword = "OF"               },
    { .token_type = TOKEN_OR            , .keyword = "OR"               },
    { .token_type = TOKEN_PROCEDURE     , .keyword = "PROCEDURE"        },
    { .token_type = TOKEN_PROGRAM       , .keyword = "PROGRAM"          },
    { .token_type = TOKEN_RECORD        , .keyword = "RECORD"           },
    { .token_type = TOKEN_REPEAT        , .keyword = "REPEAT"           },
    { .token_type = TOKEN_SET           , .keyword = "SET"              },
    { .token_type = TOKEN_SHL           , .keyword = "SHL"              },
    { .token_type = TOKEN_SHR           , .keyword = "SHR"              },
    { .token_type = TOKEN_STRING        , .keyword = "STRING"           },
    { .token_type = TOKEN_THEN          , .keyword = "THEN"             },
    { .token_type = TOKEN_TO            , .keyword = "TO"               },
    { .token_type = TOKEN_TRUE          , .keyword = "TRUE"             },
    { .token_type = TOKEN_TYPE          , .keyword = "TYPE"             },
    { .token_type = TOKEN_UNIT          , .keyword = "UNIT"             },
    { .token_type = TOKEN_UNTIL         , .keyword = "UNTIL"            },
    { .token_type = TOKEN_USES          , .keyword = "USES"             },
    { .token_type = TOKEN_VAR           , .keyword = "VAR"              },
    { .token_type = TOKEN_WHILE         , .keyword = "WHILE"            },
    { .token_type = TOKEN_WITH          , .keyword = "WITH"             },
    { .token_type = TOKEN_XOR           , .keyword = "XOR"              },
    // clang-format on
};

ps_token_type ps_token_is_keyword(char *text)
{
    // NB: text should already be normalized to uppercase
    // TODO? dichotomic search instead of sequential one
    for (int i = 0; i < sizeof(ps_keywords) / sizeof(ps_keyword); i += 1)
    {
        // printf("ps_token_is_keyword: test=%s, keyword=%s\n", text, ps_keywords[i].keyword);
        if (strcmp(text, ps_keywords[i].keyword) == 0)
        {
            return ps_keywords[i].token_type;
        }
    }
    return TOKEN_IDENTIFIER;
}
