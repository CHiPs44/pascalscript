%{
    #include <stdio.h>
%}
%token NOMBRE /* liste des terminaux */
%%
expression: expression '+' terme
| expression '-' terme
| terme
;
terme: terme '*' facteur
| terme '/' facteur
| facteur
;
facteur: '(' expression ')'
| '-' facteur
| NOMBRE
;
%%
int yyerror(void)
{ fprintf(stderr, "erreur de syntaxe\n"); return 1;}
