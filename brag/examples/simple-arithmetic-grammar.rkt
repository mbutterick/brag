#lang brag

expr : term ('+' term)*
term : factor ('*' factor)*
factor : INT
