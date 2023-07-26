
pascalscript.y:
	bison -opascalscript.tab.c -d pascalscript.y

pascalscript.l:
	flex -opascalscript.lex.c pascalscript.l

pascalscript.c