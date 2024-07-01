/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <string.h>

#include "ps_error.h"
#include "ps_token.h"

void ps_token_dump(token_t *token)
{
    ps_token_type_t token_type;
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

ps_keyword_t ps_keywords[] = {
    // clang-format off
    { .token = TOKEN_ARRAY          , .keyword = "ARRAY"            },
    { .token = TOKEN_BEGIN          , .keyword = "BEGIN"            },
    { .token = TOKEN_BOOLEAN        , .keyword = "BOOLEAN"          },
    { .token = TOKEN_CARDINAL       , .keyword = "CARDINAL"         },
    { .token = TOKEN_CASE           , .keyword = "CASE"             },
    { .token = TOKEN_CHAR           , .keyword = "CHAR"             },
    { .token = TOKEN_CONST          , .keyword = "CONST"            },
    { .token = TOKEN_DO             , .keyword = "DO"               },
    { .token = TOKEN_DOWNTO         , .keyword = "DOWNTO"           },
    { .token = TOKEN_ELSE           , .keyword = "ELSE"             },
    { .token = TOKEN_END            , .keyword = "END"              },
    { .token = TOKEN_FALSE          , .keyword = "FALSE"            },
    { .token = TOKEN_FILE           , .keyword = "FILE"             },
    { .token = TOKEN_FOR            , .keyword = "FOR"              },
    { .token = TOKEN_FUNCTION       , .keyword = "FUNCTION"         },
    { .token = TOKEN_GOTO           , .keyword = "GOTO"             },
    { .token = TOKEN_IF             , .keyword = "IF"               },
    { .token = TOKEN_IMPLEMENTATION , .keyword = "IMPLEMENTATION"   },
    { .token = TOKEN_IN             , .keyword = "IN"               },
    { .token = TOKEN_INTEGER        , .keyword = "INTEGER"          },
    { .token = TOKEN_INTERFACE      , .keyword = "INTERFACE"        },
    { .token = TOKEN_LABEL          , .keyword = "LABEL"            },
    { .token = TOKEN_NIL            , .keyword = "NIL"              },
    { .token = TOKEN_OF             , .keyword = "OF"               },
    { .token = TOKEN_AND            , .keyword = "AND"              },
    { .token = TOKEN_DIV            , .keyword = "DIV"              },
    { .token = TOKEN_MOD            , .keyword = "MOD"              },
    { .token = TOKEN_NOT            , .keyword = "NOT"              },
    { .token = TOKEN_OR             , .keyword = "OR"               },
    { .token = TOKEN_SHL            , .keyword = "SHL"              },
    { .token = TOKEN_SHR            , .keyword = "SHR"              },
    { .token = TOKEN_XOR            , .keyword = "XOR"              },
    { .token = TOKEN_PROCEDURE      , .keyword = "PROCEDURE"        },
    { .token = TOKEN_PROGRAM        , .keyword = "PROGRAM"          },
    { .token = TOKEN_RECORD         , .keyword = "RECORD"           },
    { .token = TOKEN_REPEAT         , .keyword = "REPEAT"           },
    { .token = TOKEN_SET            , .keyword = "SET"              },
    { .token = TOKEN_STRING         , .keyword = "STRING"           },
    { .token = TOKEN_THEN           , .keyword = "THEN"             },
    { .token = TOKEN_TO             , .keyword = "TO"               },
    { .token = TOKEN_TRUE           , .keyword = "TRUE"             },
    { .token = TOKEN_TYPE           , .keyword = "TYPE"             },
    { .token = TOKEN_UNIT           , .keyword = "UNIT"             },
    { .token = TOKEN_UNTIL          , .keyword = "UNTIL"            },
    { .token = TOKEN_USES           , .keyword = "USES"             },
    { .token = TOKEN_VAR            , .keyword = "VAR"              },
    { .token = TOKEN_WHILE          , .keyword = "WHILE"            },
    { .token = TOKEN_WITH           , .keyword = "WITH"             },
    // clang-format on
};

ps_token_type_t ps_token_is_keyword(char *text)
{
    // NB: text should already be normalized to uppercase
    // TODO? dichotomic search instead of sequential one
    for (int i = 0; i < sizeof(ps_keywords) / sizeof(ps_keyword_t); i += 1)
    {
        // printf("ps_token_is_keyword: test=%s, keyword=%s\n", text, ps_keywords[i].keyword);
        if (strcmp(text, ps_keywords[i].keyword) == 0)
        {
            return ps_keywords[i].token;
        }
    }
    return TOKEN_IDENTIFIER;
}
