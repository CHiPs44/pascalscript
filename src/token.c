/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <string.h>

#include "error.h"
#include "vm.h"
#include "token.h"

keyword_t keywords[] = {
    // clang-format off
    {.token = TOKEN_BEGIN               , .keyword = "BEGIN"        },
    {.token = TOKEN_BOOLEAN             , .keyword = "BOOLEAN"      },
    {.token = TOKEN_CHAR                , .keyword = "CHAR"         },
    {.token = TOKEN_CONST               , .keyword = "CONST"        },
    {.token = TOKEN_DO                  , .keyword = "DO"           },
    {.token = TOKEN_ELSE                , .keyword = "ELSE"         },
    {.token = TOKEN_END                 , .keyword = "END"          },
    {.token = TOKEN_FALSE               , .keyword = "FALSE"        },
    {.token = TOKEN_FUNCTION            , .keyword = "FUNCTION"     },
    {.token = TOKEN_IF                  , .keyword = "IF"           },
    {.token = TOKEN_INTEGER             , .keyword = "INTEGER"      },
    {.token = TOKEN_PROCEDURE           , .keyword = "PROCEDURE"    },
    {.token = TOKEN_PROGRAM             , .keyword = "PROGRAM"      },
    {.token = TOKEN_REPEAT              , .keyword = "REPEAT"       },
    {.token = TOKEN_STRING              , .keyword = "STRING"       },
    {.token = TOKEN_THEN                , .keyword = "THEN"         },
    {.token = TOKEN_TRUE                , .keyword = "TRUE"         },
    {.token = TOKEN_TYPE                , .keyword = "TYPE"         },
    {.token = TOKEN_UNTIL               , .keyword = "UNTIL"        },
    {.token = TOKEN_VAR                 , .keyword = "VAR"          },
    {.token = TOKEN_WHILE               , .keyword = "WHILE"        },
    /*
    // Various symbols
    {.token = TOKEN_ASSIGN              , .keyword = ":="           },
    {.token = TOKEN_CARET               , .keyword = "^"            },
    {.token = TOKEN_COLON               , .keyword = ":"            },
    {.token = TOKEN_COMMA               , .keyword = ","            },
    {.token = TOKEN_DOT_DOT             , .keyword = ".."           },
    {.token = TOKEN_DOT                 , .keyword = "."            },
    {.token = TOKEN_SEMI_COLON          , .keyword = ";"            },
    {.token = TOKEN_LEFT_PARENTHESIS    , .keyword = "("            },
    {.token = TOKEN_RIGHT_PARENTHESIS   , .keyword = ")"            },
    {.token = TOKEN_LEFT_BRACKET        , .keyword = "["            },
    {.token = TOKEN_RIGHT_BRACKET       , .keyword = "]"            },
    {.token = TOKEN_LEFT_CURLY_BRACKET  , .keyword = "{"            },
    {.token = TOKEN_RIGHT_CURLY_BRACKET , .keyword = "}"            },
    {.token = TOKEN_DOLLAR              , .keyword = "$"            },
    {.token = TOKEN_LEFT_COMMENT        , .keyword = "(*"           },
    {.token = TOKEN_RIGHT_COMMENT       , .keyword = "*)"           },
    // Operators
    {.token = TOKEN_ADD                 , .keyword = "+"            },
    {.token = TOKEN_SUB                 , .keyword = "-"            },
    {.token = TOKEN_MUL                 , .keyword = "*"            },
    {.token = TOKEN_DIV_REAL            , .keyword = "/"            },
    {.token = TOKEN_DIV                 , .keyword = "DIV"          },
    {.token = TOKEN_MOD                 , .keyword = "MOD"          },
    // Comparison operators
    {.token = TOKEN_EQ                  , .keyword = "="            },
    {.token = TOKEN_NE                  , .keyword = "<>"           },
    {.token = TOKEN_LT                  , .keyword = "<"            },
    {.token = TOKEN_LE                  , .keyword = "<="           },
    {.token = TOKEN_GT                  , .keyword = ">"            },
    {.token = TOKEN_GE                  , .keyword = ">="           },
    // Logical/binary operators
    {.token = TOKEN_AND                 , .keyword = "AND"          },
    {.token = TOKEN_OR                  , .keyword = "OR"           },
    {.token = TOKEN_XOR                 , .keyword = "XOR"          },
    {.token = TOKEN_NOT                 , .keyword = "NOT"          },
    {.token = TOKEN_LEFT_SHIFT          , .keyword = "<<"           },
    {.token = TOKEN_RIGHT_SHIFT         , .keyword = ">>", true     },
    */
    // clang-format on
};

token_type_t keywords_search_by_text(char *text)
{
    token_type_t token_type;
    for (int i = 0; i < sizeof(keywords) / sizeof(keyword_t); i += 1)
    {
        if (strcmp(text, keywords[i].keyword) == 0)
        {
            return keywords[i].token;
        }
    }
    return TOKEN_NONE;
}
