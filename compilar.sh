#!/bin/sh
bison -t -d -v sintactico.y
flex lexico.l
gcc lex.yy.c sintactico.tab.c -lfl -o julia_compiler
