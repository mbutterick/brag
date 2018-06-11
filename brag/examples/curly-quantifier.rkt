#lang brag
;; test the curly quantifier
start : a-rule | b-rule | c-rule | d-rule
a-rule : "a"{2} ; exactly 2
b-rule : "b"{,2} ; up to 2
c-rule : "c"{2,} ; 2 or more
d-rule : "d"{2,3} ; 2 or 3