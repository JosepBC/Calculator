#!/bin/sh
bison -t -d -v sintactico.y
flex lexico.l
gcc lex.yy.c sintactico.tab.c symtab.c types.c -lfl -g -o julia_compiler
