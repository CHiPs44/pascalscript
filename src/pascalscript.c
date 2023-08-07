/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include "lexer.h"
#include "pascalscript.tab.h"

int main(void)
{
  yyparse();
  return 0;
}
