#lang brag
start: (tab | space | newline | letter | return | all)*
tab: '\t'
space: " "
newline: "\n"
return : "\r"
all : "\a" "\b" "\t" "\n" "\v" "\f" "\r" "\e"
letter: "x" | "y" | "z"