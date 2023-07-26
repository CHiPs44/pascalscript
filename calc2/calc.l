%{

#include "global.h"
#include "calc.h"

#include <stdlib.h>

%}

blancs    [ \t]+

chiffre   [0-9]
entier    {chiffre}+
exposant  [eE][+-]?{entier}

reel    {entier}("."{entier})?{exposant}?

%%

{blancs}  { /* On ignore */ }

{reel}    {
      yylval=atof(yytext);
      return(NOMBRE);
    }

"+"   return(PLUS);
"-"   return(MOINS);

"*"   return(FOIS);
"/"   return(DIVISE);

"^"   return(PUISSANCE);

"("   return(PARENTHESE_GAUCHE);
")"   return(PARENTHESE_DROITE);

"\n"  return(FIN);
