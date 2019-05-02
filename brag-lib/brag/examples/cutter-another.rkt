#lang brag
top : w | x | y | z | a | b | c
w : /"w" ; atom
 x : /("x") ; seq
 y : /("y" "y") ; seq
 z : /("w" | "z") ; choice
 a : /["a"] ; opt
 b : /(["b"] "b") ; opt in seq
 c : /"c"+ ; repeat