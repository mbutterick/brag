#lang brag

;; Lua parser, adapted from:
;; http://www.lua.org/manual/5.1/manual.html#8
;;


chunk : (stat ["; "])* [laststat ["; "]]

block : chunk

stat :  varlist "=" explist | 
          functioncall | 
          DO block END | 
          WHILE exp DO block END | 
          REPEAT block UNTIL exp | 
          IF exp THEN block (ELSEIF exp THEN block)* [ELSE block] END | 
          FOR NAME "=" exp "," exp ["," exp] DO block END | 
          FOR namelist IN explist DO block END | 
          FUNCTION funcname funcbody | 
          LOCAL FUNCTION NAME funcbody | 
          LOCAL namelist ["=" explist] 

laststat : RETURN [explist] | BREAK

funcname : NAME ("." NAME)* [":" NAME]

varlist : var ("," var)*

var :  NAME | prefixexp "[" exp "]" | prefixexp "." NAME 

namelist : NAME ("," NAME)*

explist : (exp ",")* exp
  
              
;; Note by dyoo: The parsing of exp deviates from Lua in that we have these administrative
;; rules to explicitly represent the precedence rules.
;;              
;; See: http://www.lua.org/manual/5.1/manual.html#2.5.6
;;
;; Ragg doesn't yet automatically desugar operator precedence rules.
;; I'm doing it by hand at the moment, which is not ideal, so a future version of
;; ragg will have a story about describing precedence.
;;              
;; Operator precedence in Lua follows the table below, from lower to higher priority:
;;
;;     or                                                 exp_1
;;     and                                                exp_2 
;;     <     >     <=    >=    ~=    ==                   exp_3
;;     ..                                                 exp_4
;;     +     -                                            exp_5
;;     *     /     %                                      exp_6
;;     not   #     - (unary)                              exp_7
;;     ^                                                  exp_8
;;
;; As usual, you can use parentheses to change the precedences of an expression.
;; The concatenation ('..') and exponentiation ('^') operators are right associative.
;; All other binary operators are left associative.
;;
;; The original grammar rule before encoding precedence was:
;;
;; exp :  NIL | FALSE | TRUE | NUMBER | STRING | "..." | function | 
;;         prefixexp | tableconstructor | exp binop exp | unop exp 
                
exp :  exp_1
exp_1: exp_1 binop_1 exp_2 | exp_2
exp_2: exp_2 binop_2 exp_3 | exp_3
exp_3: exp_3 binop_3 exp_4 | exp_4
exp_4: exp_5 binop_4 exp_4 | exp_5           ;; right associative
exp_5: exp_5 binop_5 exp_6 | exp_6
exp_6: exp_6 binop_6 exp_7 | exp_7
exp_7: unop exp_8
exp_8: exp_9 binop_8 exp_8 | exp_9           ;; right associative
exp_9: NIL | FALSE | TRUE | NUMBER | STRING | "..." | function | 
         prefixexp | tableconstructor 
binop_1: OR
binop_2: AND
binop_3: "<" | ">" | "<=" | ">=" | "~=" | "=="
binop_4: ".."
binop_5: "+" | "-"
binop_6: "*" | "/" | "%"
binop_8: "^"               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


prefixexp : var | functioncall | "(" exp ")"

functioncall :  prefixexp args | prefixexp ":" NAME args 

args :  "(" [explist] ")" | tableconstructor | STRING 

function : FUNCTION funcbody

funcbody : "(" [parlist] ")" block END

parlist : namelist ["," "..."] | "..."

tableconstructor : "{" [fieldlist] "}"

fieldlist : field (fieldsep field)* [fieldsep]

field : "[" exp "]" "=" exp | NAME "=" exp | exp

fieldsep : "," | ";"

binop : "+" | "-" | "*" | "/" | "^" | "%" | ".." | 
          "<" | "<=" | ">" | ">=" | "==" | "~=" | 
          AND | OR

unop : "-" | NOT | "#"