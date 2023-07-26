%{

#include "global.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

%}

%token  NOMBRE
%token  PLUS  MOINS FOIS  DIVISE  PUISSANCE
%token  PARENTHESE_GAUCHE PARENTHESE_DROITE
%token  FIN

%left PLUS  MOINS
%left FOIS  DIVISE
%left NEG
%right  PUISSANCE

%start Input
%%

Input:
    /* Vide */
  | Input Ligne
  ;

Ligne:
    FIN
  | Expression FIN    { printf("Resultat : %f\n",$1); }
  ;

Expression:
    NOMBRE      { $$=$1; }
  | Expression PLUS Expression  { $$=$1+$3; }
  | Expression MOINS Expression { $$=$1-$3; }
  | Expression FOIS Expression  { $$=$1*$3; }
  | Expression DIVISE Expression  { $$=$1/$3; }
  | MOINS Expression %prec NEG  { $$=-$2; }
  | Expression PUISSANCE Expression { $$=pow($1,$3); }
  | PARENTHESE_GAUCHE Expression PARENTHESE_DROITE  { $$=$2; }
  ;

%%

int yyerror(char *s) {
  printf("%s\n",s);
}

int main(void) {
  yyparse();
}
