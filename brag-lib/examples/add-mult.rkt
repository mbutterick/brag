#lang brag

expr : term (/'+' term)*
@term : factor (/'*' @factor)*
factor : ("0" | "1" | "2" | "3"
       |  "4" | "5" | "6" | "7"
       | "8" | "9")+
