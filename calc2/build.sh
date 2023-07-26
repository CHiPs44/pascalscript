bison -d calc.y
# mv calc.tab.h calc.h
# mv calc.tab.c calc.y.c
flex calc.l
# mv lex.yy.c calc.lex.c
gcc -c calc.lex.c -o calc.lex.o
gcc -c calc.y.c -o calc.y.o
gcc -o calc calc.lex.o calc.y.o -ll -lm
