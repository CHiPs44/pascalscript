%{
    #include <stdio.h>
    int yylval;
%}

%tokens T_PROGRAM T_CONST T_VAR T_BEGIN T_END
%tokens T_INTEGER
%tokens T_WRITELN

%tokens T_INT_VAL

%option yylineno

identifier  [a-zA-Z][a-zA-Z0-9]*
int_val     [0-9]+
whitespace  [ \t]+
eol         [\r\n]

%%

{int_val} {
    yylval = atoi(yytext); 
    return T_INT_VAL; 
}

+         { return T_PLUS; }
-         { return T_MINUS; }
*         { return T_STAR; }
/         { return T_SLASH; }
div       { return T_DIV; }
mod       { return T_MOD; }
:=        { return T_ASSIGN; }

program      {
          fprintf( stderr, " [PROGRAM]" );
          yylval.sval = strdup(yytext);
          return PROGRAM;
        }

var        {
          fprintf( stderr, " [VAR]" );
          yylval.sval = strdup(yytext);
          return VAR;
        }
integer      {
          fprintf( stderr, " [INTEGER]" );
          yylval.sval = strdup(yytext);
          return INTEGER;
        }

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
