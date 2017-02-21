#lang brag
top : expr (/"," expr)*
expr : "x" | list
list : "(" expr ("," expr)* ")"