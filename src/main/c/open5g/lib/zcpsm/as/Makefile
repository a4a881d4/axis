YACC = byacc
LEX = flex
CC = ../../../../../../../../bin/zcc

all:zas

lex.yy.c:zas.lex
	$(LEX) zas.lex

y.tab.h:zas.y
	$(YACC)  -d zas.y

zas:y.tab.h lex.yy.c zasmain.c
	gcc -g  -DYYDEBUG -o zas lex.yy.c zasmain.c y.tab.c

clean:
	rm -f zas; rm -f lex.yy.c; rm -f y.tab.c; rm -f y.tab.h; rm -f *.bak; rm -f *~; rm *.psm; 
	

test:test.psm zas
	./zas testzcpsm test.psm

test.psm:test.c
	$(CC) test.c > test.psm

install: zas
	cp zas ../../../../../../../../bin/zas
	
