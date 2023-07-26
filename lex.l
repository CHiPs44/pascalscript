%{
    #include <stdio.h>
    #include <string.h>
    #include "lexer.h"
    // int yylval;
    extern token_t yylval;
%}

%option yylineno

identifier  [a-zA-Z][a-zA-Z0-9]*
int_val     [0-9]+
whitespace  [ \t]+
eol         [\r\n]

%%

{identifier} {
  int error = copy_identifier(yytext);
  if (error == ERROR_NONE)
    return T_IDENTIFIER;
  yyerror()
}

{int_val} {
    yylval.type = INT_VAL;
    yylval.value.int_val = atoi(yytext); 
    return T_INTEGER_VALUE; 
}

"+"       { return T_PLUS;    }
"-"       { return T_MINUS;   }
"*"       { return T_STAR;    }
"/"       { return T_SLASH;   }
"div"     { return T_DIV;     }
"mod"     { return T_MOD;     }
":="      { return T_ASSIGN;  }

"program" { return T_PROGRAM; }

"var"     { return T_VAR;     }
"integer" { return T_INTEGER; }

 /* identifier tokens */
{identifier}  {
          fprintf( stderr, " [IDENT:%s]", yylval.sval = strdup(yytext));
          return IDENT;
        }

/* ignore whitespace and set default */
{whitespace}  {
                ;
              }
{eol}         {
                fprintf( stderr, "\n%d ", yylineno );
              }
.             {
                fprintf( stderr, " {%c}", yytext[0] );
                return yytext[0];
              }

%%

/* EOF */
