%{
%}

/* "real" keywords */
%token T_PROGRAM T_CONST T_VAR T_BEGIN T_END
/* operators */
%token T_PLUS T_MINUS T_STAR T_SLASH T_DIV T_MOD
/* assignment */
%token T_ASSIGN
/* standard library */
%token T_WRITELN
/* type(s) */
%token T_INTEGER
/* values / constants */
%token T_INT_VAL
/* identifier */
%token T_IDENTIFIER

%%

expression: expression ’+’ terme
| expression ’-’ terme
| terme
;
terme: terme ’*’ facteur
| terme ’/’ facteur
| facteur
;
facteur: ’(’ expression ’)’
| ’-’ facteur
| NOMBRE
;
%%
int yyerror(void)
{ fprintf(stderr, "erreur de syntaxe\n"); return 1;}