#include "lexer.h"
#include "pascalscript.tab.h"

int main(void)
{
  yyparse();
  return 0;
}
