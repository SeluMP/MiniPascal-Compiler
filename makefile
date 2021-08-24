minip: lex.yy.c main.c sintactico.tab.c sintactico.tab.h listaSimbolos.c listaCodigo.c
	gcc lex.yy.c main.c sintactico.tab.c listaSimbolos.c listaCodigo.c -lfl -o minip -g

sintactico.tab.c sintactico.tab.h: sintactico.y
	bison -d sintactico.y

lex.yy.c: lexico.l
	flex lexico.l

clean:
	rm lex.yy.c sintactico.tab.c sintactico.tab.h minip

run:
	./minip 
