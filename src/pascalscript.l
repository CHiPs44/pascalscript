%{
    #include <stdio.h>
    #include <string.h>
    #include "error.h"
    #include "lexer.h"
    #include "pascalscript.tab.h"

    int yyerror(char *message);
%}

%option yylineno

identifier    [a-zA-Z][a-zA-Z0-9]*
integer_value [0-9]+
whitespace    [ \t]+
eol           [\r\n]

%%

"+"       { return T_PLUS;              }
"-"       { return T_MINUS;             }
"*"       { return T_STAR;              }
"/"       { return T_SLASH;             }
"div"     { return T_DIV;               }
"mod"     { return T_MOD;               }
":="      { return T_ASSIGN;            }
"("       { return T_LEFT_PARENTHESIS;  }
")"       { return T_RIGHT_PARENTHESIS; }
":"       { return T_COLON;             }
";"       { return T_SEMICOLON;         }
"."       { return T_DOT;               }
"="       { return T_EQUALS;            }
"program" { return T_PROGRAM;           }
"const"   { return T_CONST;             }
"var"     { return T_VAR;               }
"begin"   { return T_BEGIN;             }
"end"     { return T_END;               }
"writeln" { return T_WRITELN;           }
"integer" { return T_INTEGER;           }

{integer_value} {
  int zzerror = copy_integer_value(yytext);
  if (zzerror == ERROR_NONE)
    return T_INTEGER_VALUE;
  yyerror("Invalid integer value");
}

{identifier} {
  int zzerror = copy_identifier(yytext);
  if (zzerror == ERROR_NONE)
    return T_IDENTIFIER;
  yyerror("Invalid identifier");
}

{whitespace} {
  fprintf( stderr, " [ ]");
}
{eol} {
  fprintf( stderr, "\n%d ", yylineno );
}
. {
  fprintf( stderr, " {%c}", yytext[0] );
  // return yytext[0];
  yyerror("Invalid source");
  yyterminate();
}

%%

int yyerror(char *message) 
{
  fprintf(stderr, "Error: %s\n", message);
  // exit(1);
}

int yywrap()
{
  return 1;
}

/* EOF */
