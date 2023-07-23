%{
    #include <stdio.h>
    #include <string.h>
    #include "lexer.h"
    // int yylval;
    token_t yylval;
%}

/* "real" keywords */
%tokens T_PROGRAM T_CONST T_VAR T_BEGIN T_END
/* operators */
%tokens T_PLUS T_MINUS T_STAR T_SLASH T_DIV T_MOD
/* assignment */
%tokens T_ASSIGN
/* standard library */
%tokens T_WRITELN
/* type(s) */
%tokens T_INTEGER
/* values / constants */
%tokens T_INT_VAL

%option yylineno

identifier  [a-zA-Z][a-zA-Z0-9]*
int_val     [0-9]+
whitespace  [ \t]+
eol         [\r\n]

%%

{identifier} {
    yylval.type = IDENTIFIER;
    int n = strncpy(yylval.value.identifier, yytext); 
    return T_INT_VAL; 
}

{int_val} {
    yylval.type = INT_VAL;
    yylval.value.int_val = atoi(yytext); 
    return T_INT_VAL; 
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
{string}    {
          fprintf( stderr, " [STRING:%s]", yylval.sval = strdup(yytext) );
          return STRING;
        }

 /* ignore whitespace and set default */
{whitespace}   { ; }
{endline}     {
          fprintf( stderr, "\n%d ", yylineno );
        }
.        {
          fprintf( stderr, " {%c}", yytext[0] );
          return yytext[0];
        }

%%

/* EOF */
