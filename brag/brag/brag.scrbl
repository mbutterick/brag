#lang scribble/manual
@(require scribble/eval
          racket/date
          file/md5
          (for-label racket
                     brag/support
                     brag/examples/nested-word-list
                     (only-in br-parser-tools/lex lexer-src-pos)
                     (only-in syntax/parse syntax-parse ~literal)))


@(define (lookup-date filename [default ""])
   (cond
     [(file-exists? filename)
      (define modify-seconds (file-or-directory-modify-seconds filename))
      (define a-date (seconds->date modify-seconds))
      (date->string a-date)]
     [else
      default]))

@(define (compute-md5sum filename [default ""])
   (cond [(file-exists? filename)
          (bytes->string/utf-8 (call-with-input-file filename md5 #:mode 'binary))]
         [else
          default]))



@title{brag: the Beautiful Racket AST Generator}
@author["Danny Yoo (95%)" "Matthew Butterick (5%)"]

@defmodulelang[brag]

@section{Quick start}

@(define my-eval (make-base-eval))
@(my-eval '(require brag/examples/nested-word-list 
                    racket/list
                    racket/match))

Suppose we're given the
following string:
@racketblock["(radiant (humble))"]


How would we turn this string into a structured value?  That is, how would we @emph{parse} it? (Let's also suppose we've never heard of @racket[read].)

First, we need to consider the structure of the things we'd like to parse. The
string above looks like a nested list of words. Good start.

Second, how might we describe this formally — meaning, in a way that a computer could understand? A common notation to describe the structure of these things is @link["http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form"]{Backus-Naur Form} (BNF). So let's try to notate the structure of nested word lists in BNF.

@nested[#:style 'code-inset]{
 @verbatim{
  nested-word-list: WORD
  | LEFT-PAREN nested-word-list* RIGHT-PAREN
}}

What we intend by this notation is this: @racket[nested-word-list] is either a @racket[WORD], or a parenthesized list of @racket[nested-word-list]s. We use the character @litchar{*} to represent zero or more repetitions of the previous thing. We treat the uppercased @racket[LEFT-PAREN], @racket[RIGHT-PAREN], and @racket[WORD] as placeholders for @emph{tokens} (a @tech{token} being the smallest meaningful item in the parsed string):

Here are a few examples of tokens:
@interaction[#:eval my-eval
             (require brag/support)
             (token 'LEFT-PAREN)
             (token 'WORD "crunchy" #:span 7)
             (token 'RIGHT-PAREN)]

This BNF description is also known as a @deftech{grammar}. Just as it does in a natural language like English or French, a grammar describes something in terms of what elements can fit where.

Have we made progress?  We have a valid grammar. But we're still missing a @emph{parser}: a function that can use that description to make structures out of a sequence of tokens.

Meanwhile, it's clear that we don't yet have a valid program because there's no @litchar{#lang} line. Let's add one: put @litchar{#lang brag} at the top of the grammar, and save it as a file called @filepath{nested-word-list.rkt}.

@filebox["nested-word-list.rkt"]{
 @verbatim{
  #lang brag
  nested-word-list: WORD
  | LEFT-PAREN nested-word-list* RIGHT-PAREN
}}

Now it's a proper program. But what does it do?

@interaction[#:eval my-eval
             @eval:alts[(require "nested-word-list.rkt") (void)]
             parse
             ]

It gives us a @racket[parse] function. Let's investigate what @racket[parse]
does. What happens if we pass it a sequence of tokens?

@interaction[#:eval my-eval
             (define a-parsed-value
               (parse (list (token 'LEFT-PAREN "(")
                            (token 'WORD "some")
                            (token 'LEFT-PAREN "[") 
                            (token 'WORD "pig")
                            (token 'RIGHT-PAREN "]") 
                            (token 'RIGHT-PAREN ")"))))
             a-parsed-value]

Those who have messed around with macros will recognize this as a @seclink["stx-obj" #:doc '(lib "scribblings/guide/guide.scrbl")]{syntax object}.

@interaction[#:eval my-eval
             (syntax->datum a-parsed-value)
             ]

That's @racket[(some [pig])], essentially.

What happens if we pass our @racket[parse] function a bigger source of tokens?

@interaction[#:eval my-eval
             @code:comment{tokenize: string -> (sequenceof token-struct?)}
             @code:comment{Generate tokens from a string:}
             (define (tokenize s)
               (for/list ([str (regexp-match* #px"\\(|\\)|\\w+" s)])
                         (match str
                           ["("
                            (token 'LEFT-PAREN str)]
                           [")"
                            (token 'RIGHT-PAREN str)]
                           [else
                            (token 'WORD str)])))

             @code:comment{For example:}
             (define token-source (tokenize "(welcome (to (((brag)) ())))"))
             (define v (parse token-source))
             (syntax->datum v)
             ]

Welcome to @tt{brag}.




@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{Introduction}

@tt{brag} is a parser generator designed to be easy
to use:

@itemize[

 @item{It provides a @litchar{#lang} for writing BNF grammars.
  A module written in @litchar{#lang brag} automatically generates a
  parser. The grammar controls the structure of the syntax objects it generates.}

 @item{The language uses a few conventions to simplify the expression of
  grammars. The first rule in the grammar is assumed to be the
  starting production. Identifiers in @tt{UPPERCASE} are treated as
  terminal tokens. All other identifiers are treated as nonterminals.}

 @item{Tokenizers can be developed independently of parsers.
  @tt{brag} takes a liberal view on tokens: they can be strings,
  symbols, or instances constructed with @racket[token]. Tokens can optionally provide source location, in which case a syntax object generated by the parser will too.}

 @item{The parser can usually handle ambiguous grammars.}

 @item{It integrates with the rest of the Racket
  @link["http://docs.racket-lang.org/guide/languages.html"]{language toolchain}.}

 ]



@subsection{Example: a small DSL for ASCII diagrams}

Suppose we'd like to define a language for
drawing simple ASCII diagrams. So if we write something like this:

@nested[#:style 'inset]{
 @verbatim|{
  3 9 X;
  6 3 b 3 X 3 b;
  3 9 X;
  }|}

It should generate the following picture:

@nested[#:style 'inset]{
@verbatim|{
XXXXXXXXX
XXXXXXXXX
XXXXXXXXX
   XXX   
   XXX   
   XXX   
   XXX   
   XXX   
   XXX   
XXXXXXXXX
XXXXXXXXX
XXXXXXXXX
}|}


This makes sense in a casual way. But let's be more precise about how the language works.

Each line of the program has a semicolon at the end, and describes the output of several @emph{rows} of the line drawing. Let's look at two of the lines in the example:

@itemize[
 @item{@litchar{3 9 X;}: ``Repeat the following 3 times: print @racket["X"] nine times, followed by
  a newline.''}

 @item{@litchar{6 3 b 3 X 3 b;}: ``Repeat the following 6 times: print @racket[" "] three times, 
  followed by @racket["X"] three times, followed by @racket[" "] three times, followed by a newline.''}
 ]

Then each line consists of a @emph{repeat} number, followed by pairs of
(number, character) @emph{chunks}. We'll assume here that the intent of the lowercased character @litchar{b} is to represent the printing of a 1-character whitespace @racket[" "], and for other uppercase letters to represent the printing of themselves.

By understanding the pieces of each line, we can more easily capture that meaning in a grammar. Once we have each instruction of our ASCII DSL in a structured format, we should be able to parse it.

Here's a first pass at expressing the structure of these line-drawing programs.

@subsection{Parsing the concrete syntax}

@filebox["simple-line-drawing.rkt"]{
 @verbatim|{
  #lang brag
  drawing: rows*
  rows: repeat chunk+ ";"
  repeat: INTEGER
  chunk: INTEGER STRING
  }|
}

@margin-note{@secref{brag-syntax} describes @tt{brag}'s syntax in more detail.}
We write a @tt{brag} program as an BNF grammar, where patterns can be:
@itemize[
 @item{the names of other rules (e.g. @racket[chunk])}
 @item{literal and symbolic token names (e.g. @racket[";"], @racket[INTEGER])}
 @item{quantified patterns (e.g. @litchar{+} to represent one-or-more repetitions)}
 ]
The result of a @tt{brag} program is a module with a @racket[parse] function
that can parse tokens and produce a syntax object as a result.

Let's try this function:

@interaction[#:eval my-eval
             (require brag/support)
             @eval:alts[(require "simple-line-drawing.rkt") 
                        (require brag/examples/simple-line-drawing)]
             (define stx
               (parse (list (token 'INTEGER 6) 
                            (token 'INTEGER 2)
                            (token 'STRING " ")
                            (token 'INTEGER 3)
                            (token 'STRING "X")
                            ";")))
             (syntax->datum stx)
             ]

A @emph{token} is the smallest meaningful element of a source program. Tokens can be  strings, symbols, or instances of the @racket[token] data structure. (Plus a few other special cases, which we'll discuss later.) Usually, a token holds a single character from the source program. But sometimes it makes sense to package a sequence of characters into a single token, if the sequence has an indivisible meaning.

If possible, we also want to attach source location information to each token. Why? Because this information will be incorporated into the syntax objects produced by @racket[parse].

A parser often works in conjunction with a helper function called a @emph{lexer} that converts the raw code of the source program into tokens. The @racketmodname[br-parser-tools/lex] library can help us write a position-sensitive
tokenizer:

@interaction[#:eval my-eval
             (require br-parser-tools/lex)
             (define (tokenize ip)
               (port-count-lines! ip)
               (define my-lexer
                 (lexer-src-pos 
                  [(repetition 1 +inf.0 numeric)
                   (token 'INTEGER (string->number lexeme))]
                  [upper-case
                   (token 'STRING lexeme)]
                  ["b"
                   (token 'STRING " ")]
                  [";"
                   (token ";" lexeme)]
                  [whitespace
                   (token 'WHITESPACE lexeme #:skip? #t)]
                  [(eof)
                   (void)]))
               (define (next-token) (my-lexer ip))
               next-token)

             (define a-sample-input-port (open-input-string "6 2 b 3 X;"))
             (define token-thunk (tokenize a-sample-input-port))
             @code:comment{Now we can pass token-thunk to the parser:}
             (define another-stx (parse token-thunk))
             (syntax->datum another-stx)
             @code:comment{The syntax object has location information:}
             (syntax-line another-stx)
             (syntax-column another-stx)
             (syntax-span another-stx)
             ]


Note also from this lexer example: 

@itemize[

 @item{@racket[parse] accepts as input either a sequence of tokens, or a
  function that produces tokens (which @racket[parse] will call repeatedly to get the next token).}

 @item{As an alternative to the basic @racket[token] structure, a token can also be an instance of the @racket[position-token] structure (also found in @racketmodname[br-parser-tools/lex]). In that case, the token will try to derive its position from that of the position-token.}

 @item{@racket[parse] will stop if it gets @racket[void] (or @racket['eof]) as a token.}

 @item{@racket[parse] will skip any token that has
  @racket[#:skip?] attribute set to @racket[#t]. For instance, tokens representing comments often use @racket[#:skip?].}

 ]


@subsection{From parsing to interpretation}

We now have a parser for programs written in this simple-line-drawing language.
Our parser will return syntax objects:

@interaction[#:eval my-eval
             (define parsed-program
               (parse (tokenize (open-input-string "3 9 X; 6 3 b 3 X 3 b; 3 9 X;"))))
             (syntax->datum parsed-program)
             ]

Better still, these syntax objects will have a predictable
structure that follows the grammar:

@racketblock[
 (drawing (rows (repeat <number>)
                (chunk <number> <string>) ... ";")
          ...)
 ]

where @racket[drawing], @racket[rows], @racket[repeat], and @racket[chunk]
should be treated literally, and everything else will be numbers or strings.


Still, these syntax-object values are just inert structures. How do we
interpret them, and make them @emph{print}?  We claimed at the beginning of
this section that these syntax objects should be easy to interpret. So let's do it.

@margin-note{This is a very quick-and-dirty treatment of @racket[syntax-parse].
 See the @racketmodname[syntax/parse] documentation for a gentler guide to its
 features.}  Racket provides a special form called @racket[syntax-parse] in the
@racketmodname[syntax/parse] library. @racket[syntax-parse] lets us do a
structural case-analysis on syntax objects: we provide it a set of patterns to
parse and actions to perform when those patterns match.


As a simple example, we can write a function that looks at a syntax object and
says @racket[#t] if it's the literal @racket[yes], and @racket[#f] otherwise:

@interaction[#:eval my-eval
             (require syntax/parse)
             @code:comment{yes-syntax-object?: syntax-object -> boolean}
             @code:comment{Returns true if the syntax-object is yes.}
             (define (yes-syntax-object? stx)
               (syntax-parse stx
                 [(~literal yes)
                  #t]
                 [else
                  #f]))
             (yes-syntax-object? #'yes)
             (yes-syntax-object? #'nooooooooooo)
             ]

Here, we use @racket[~literal] to let @racket[syntax-parse] know that
@racket[yes] should show up literally in the syntax object. The patterns can
also have some structure to them, such as:
@racketblock[({~literal drawing} rows-stxs ...)]
which matches on syntax objects that begin, literally, with @racket[drawing],
followed by any number of rows (which are syntax objects too).


Now that we know a little bit more about @racket[syntax-parse], 
we can use it to do a case analysis on the syntax
objects that our @racket[parse] function gives us.
We start by defining a function on syntax objects of the form @racket[(drawing
                                                                       rows-stx ...)].
@interaction[#:eval my-eval
             (define (interpret-drawing drawing-stx)
               (syntax-parse drawing-stx
                 [({~literal drawing} rows-stxs ...)

                  (for ([rows-stx (syntax->list #'(rows-stxs ...))])
                       (interpret-rows rows-stx))]))]

When we encounter a syntax object with @racket[(drawing rows-stx
                                                        ...)], then @racket[interpret-rows] each @racket[rows-stx].

@;The pattern we
@;express in @racket[syntax-parse] above marks what things should be treated
@;literally, and the @racket[...] is a a part of the pattern matching language
@;known by @racket[syntax-parse] that lets us match multiple instances of the
@;last pattern.


Let's define @racket[interpret-rows] now:
@interaction[#:eval my-eval
             (define (interpret-rows rows-stx)
               (syntax-parse rows-stx
                 [({~literal rows}
                   ({~literal repeat} repeat-number)
                   chunks ... ";")

                  (for ([i (syntax-e #'repeat-number)])
                       (for ([chunk-stx (syntax->list #'(chunks ...))])
                            (interpret-chunk chunk-stx))
                       (newline))]))]

For a @racket[rows], we extract out the @racket[repeat-number] out of the
syntax object and use it as the range of the @racket[for] loop. The inner loop
walks across each @racket[chunk-stx] and calls @racket[interpret-chunk] on it.


Finally, we need to write a definition for @racket[interpret-chunk]. We want
it to extract out the @racket[chunk-size] and @racket[chunk-string] portions,
and print to standard output:

@interaction[#:eval my-eval
             (define (interpret-chunk chunk-stx)
               (syntax-parse chunk-stx
                 [({~literal chunk} chunk-size chunk-string)

                  (for ([k (syntax-e #'chunk-size)])
                       (display (syntax-e #'chunk-string)))]))
             ]


@margin-note{Here are the definitions in a single file:
 @link["examples/simple-line-drawing/interpret.rkt"]{interpret.rkt}.}
With these definitions in hand, now we can pass it syntax objects 
that we construct directly by hand:

@interaction[#:eval my-eval
             (interpret-chunk #'(chunk 3 "X"))
             (interpret-drawing #'(drawing (rows (repeat 5) (chunk 3 "X") ";")))
             ]

or we can pass it the result generated by our parser:
@interaction[#:eval my-eval
             (define parsed-program
               (parse (tokenize (open-input-string "3 9 X; 6 3 b 3 X 3 b; 3 9 X;"))))
             (interpret-drawing parsed-program)]

And now we've got an interpreter!


@subsection{From interpretation to compilation}

@margin-note{For a gentler tutorial on writing @litchar{#lang}-based languages, see
 @link["http://beautifulracket.com"]{Beautiful Racket}.}  (Just as a
warning: the following material is slightly more advanced, but shows how
writing a compiler for the line-drawing language reuses the ideas for the
interpreter.)

Wouldn't it be nice to be able to write something like:

@nested[#:style 'inset]{
 @verbatim|{
  3 9 X;
  6 3 b 3 X 3 b;
  3 9 X;
  }|}

and have Racket automatically compile this down to something like this?
@racketblock[
 (for ([i 3])
      (for ([k 9]) (displayln "X"))
      (newline))

 (for ([i 6])
      (for ([k 3]) (displayln " "))
      (for ([k 3]) (displayln "X"))
      (for ([k 3]) (displayln " "))
      (newline))

 (for ([i 3])
      (for ([k 9]) (displayln "X"))
      (newline))
 ]

Well, of course it won't work: we don't have a @litchar{#lang} line.

Let's add one.

@filebox["letter-i.rkt"]{
 @verbatim|{
  #lang brag/examples/simple-line-drawing
  3 9 X;
  6 3 b 3 X 3 b;
  3 9 X;
  }|
}

Now @filepath{letter-i.rkt} is a program.


How does this work?  From the previous sections, we've seen how to take the
contents of a file and interpret it. What we want to do now is teach Racket
how to compile programs labeled with this @litchar{#lang} line. We'll do two
things:

@itemize[
 @item{Tell Racket to use the @tt{brag}-generated parser and lexer we defined
  earlier whenever it sees a program written with
  @litchar{#lang brag/examples/simple-line-drawing}.}

 @item{Define transformation rules for @racket[drawing], @racket[rows], and
  @racket[chunk] to rewrite these into standard Racket forms.}
 ]

The second part, the writing of the transformation rules, will look very
similar to the definitions we wrote for the interpreter, but the transformation
will happen at compile-time. (We @emph{could} just resort to simply calling
into the interpreter we just wrote up, but this section is meant to show that
compilation is also viable.)


We do the first part by defining a @emph{module reader}: a
@link["http://docs.racket-lang.org/guide/syntax_module-reader.html"]{module
 reader} tells Racket how to parse and compile a file. Whenever Racket sees a
@litchar{#lang <name>}, it looks for a corresponding module reader in
@filepath{<name>/lang/reader}.

Here's the definition for
@filepath{brag/examples/simple-line-drawing/lang/reader.rkt}:

@filebox["brag/examples/simple-line-drawing/lang/reader.rkt"]{
 @codeblock|{
  #lang s-exp syntax/module-reader
  brag/examples/simple-line-drawing/semantics
  #:read my-read
  #:read-syntax my-read-syntax
  #:whole-body-readers? #t

  (require brag/examples/simple-line-drawing/lexer
  brag/examples/simple-line-drawing/grammar)

  (define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

  (define (my-read-syntax src ip)
  (list (parse src (tokenize ip))))
  }|
}

We use a helper module @racketmodname[syntax/module-reader], which provides
utilities for creating a module reader. It uses the lexer and
@tt{brag}-generated parser we defined earlier, and also tells Racket that it should compile the forms in the syntax
object using a module called @filepath{semantics.rkt}.

Let's look into @filepath{semantics.rkt} and see what's involved in
compilation:
@filebox["brag/examples/simple-line-drawing/semantics.rkt"]{
 @codeblock|{
  #lang racket/base
  (require (for-syntax racket/base syntax/parse))

  (provide #%module-begin
  ;; We reuse Racket's treatment of raw datums, specifically
  ;; for strings and numbers:
  #%datum
         
  ;; And otherwise, we provide definitions of these three forms.
  ;; During compiliation, Racket uses these definitions to 
  ;; rewrite into for loops, displays, and newlines.
  drawing rows chunk)

  ;; Define a few compile-time functions to do the syntax rewriting:
  (begin-for-syntax
  (define (compile-drawing drawing-stx)
  (syntax-parse drawing-stx
  [({~literal drawing} rows-stxs ...)

  (syntax/loc drawing-stx
  (begin rows-stxs ...))]))

  (define (compile-rows rows-stx)
  (syntax-parse rows-stx
  [({~literal rows}
  ({~literal repeat} repeat-number)
  chunks ... 
  ";")

  (syntax/loc rows-stx
  (for ([i repeat-number])
  chunks ...
  (newline)))]))

  (define (compile-chunk chunk-stx)
  (syntax-parse chunk-stx
  [({~literal chunk} chunk-size chunk-string)

  (syntax/loc chunk-stx
  (for ([k chunk-size])
  (display chunk-string)))])))


  ;; Wire up the use of "drawing", "rows", and "chunk" to these
  ;; transformers:
  (define-syntax drawing compile-drawing)
  (define-syntax rows compile-rows)
  (define-syntax chunk compile-chunk)
  }|
}

The semantics hold definitions for @racket[compile-drawing],
@racket[compile-rows], and @racket[compile-chunk], similar to what we had for
interpretation with @racket[interpret-drawing], @racket[interpret-rows], and
@racket[interpret-chunk]. However, compilation is not the same as
interpretation: each definition does not immediately execute the act of
drawing, but rather returns a syntax object whose evaluation will do the actual
work.

There are a few things to note:

@margin-note{By the way, we can just as easily rewrite the semantics so that
 @racket[compile-rows] does explicitly call @racket[compile-chunk]. Often,
 though, it's easier to write the transformation functions in this piecemeal way
 and depend on the Racket macro expansion system to do the rewriting as it
 encounters each of the forms.}


@itemize[

 @item{@tt{brag}'s native data structure is the syntax object because the
  majority of Racket's language-processing infrastructure knows how to read and
  write this structured value.}


 @item{Unlike in interpretation, @racket[compile-rows] doesn't
  compile each chunk by directly calling @racket[compile-chunk]. Rather, it
  depends on the Racket macro expander to call each @racket[compile-XXX] function
  as it encounters a @racket[drawing], @racket[rows], or @racket[chunk] in the
  parsed value. The three statements at the bottom of @filepath{semantics.rkt} inform
  the macro expansion system to do this:

  @racketblock[
 (define-syntax drawing compile-drawing)
 (define-syntax rows compile-rows)
 (define-syntax chunk compile-chunk)
 ]}
 ]


Altogether, @tt{brag}'s intent is to be a parser generator for Racket
that's easy and fun to use. It's meant to fit naturally with the other tools
in the Racket language toolchain. Hopefully, it will reduce the friction in
making new languages with alternative concrete syntaxes.

The rest of this document describes the @tt{brag} language and the parsers it
generates.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{The language}

@subsection[#:tag "brag-syntax"]{Syntax and terminology}
A program in the @tt{brag} language consists of the language line
@litchar{#lang brag}, followed by a collection of @tech{rule}s and
@tech{line comment}s.

A @deftech{rule} is a sequence consisting of: a @tech{rule identifier}, a colon
@litchar{":"}, and a @tech{pattern}.

A @deftech{rule identifier} is an @tech{identifier} that is not in upper case.

A @deftech{symbolic token identifier} is an @tech{identifier} that is in upper case.

An @deftech{identifier} is a character sequence of letters, numbers, and
characters in @racket["-.!$%&/<=>?^_~@"]. It must not contain
@litchar{*} or @litchar{+}, as those characters are used to denote
quantification.


A @deftech{pattern} is one of the following:
@itemize[
 @item{an implicit sequence of @tech{pattern}s separated by whitespace}
 @item{a terminal: either a literal string or a @tech{symbolic token identifier}. 

  When used in a pattern, both these terminals will match the same set of inputs. A literal string can match the string itself, or a @racket[token] structure whose type field contains that string (or its symbol form). So @racket["FOO"] would  match @racket["FOO"], @racket[(token "FOO" "bar")], or @racket[(token 'FOO "bar")]. A symbolic token identifier can also match the string version of the identifier, or a @racket[token] whose type field is the symbol or string form of the identifier. So @racket[FOO] would also match @racket["FOO"], @racket[(token 'FOO "bar")], or @racket[(token "FOO" "bar")]. (In every case, the value of a token, like @racket["bar"], can be anything, and may or may not be the same as its type.)

  Because their underlying meanings are the same, the symbolic token identifier ends up being a notational convenience for readability inside a grammar pattern. Typically, the literal string @racket["FOO"] is used to connote ``match the string @racket["FOO"] exactly'' and the symbolic token identifier @racket[FOO] specially connotes ``match any token of type @racket['FOO]''.

  You @bold{cannot} use the literal string @racket["error"] as a terminal in a grammar, because it's reserved for @tt{brag}. You can, however, adjust your lexer to package it inside a token structure — say, @racket[(token ERROR "error")] — and then use the symbolic token identifier @racket[ERROR] in the grammar to match this token structure.
}

 @item{a @tech{rule identifier}}
 @item{a @deftech{choice pattern}: a sequence of @tech{pattern}s delimited with @litchar{|} characters.}
 @item{a @deftech{quantifed pattern}: a @tech{pattern} followed by either @litchar{*} (``zero or more'') or @litchar{+} (``one or more'')}
 @item{an @deftech{optional pattern}: a @tech{pattern} surrounded by @litchar{[} and @litchar{]}}
 @item{an explicit sequence: a @tech{pattern} surrounded by @litchar{(} and @litchar{)}}]

A @deftech{line comment} begins with either @litchar{#} or @litchar{;} and
continues till the end of the line.


For example, in the following program:
@nested[#:style 'inset
        @verbatim|{
         #lang brag
         ;; A parser for a silly language
         sentence: verb optional-adjective object
         verb: greeting
         optional-adjective: ["happy" | "frumpy"]
         greeting: "hello" | "hola" | "aloha"
         object: "world" | WORLD
         }|]

the elements @tt{sentence}, @tt{verb}, @tt{greeting}, and @tt{object} are rule
identifiers. The first rule, @litchar{sentence: verb optional-adjective
 object}, is a rule whose right side is an implicit pattern sequence of three
sub-patterns. The uppercased @tt{WORLD} is a symbolic token identifier. The fourth rule in the program associates @tt{greeting} with a @tech{choice pattern}.



More examples:
@itemize[

 @item{A
  BNF for binary
  strings that contain an equal number of zeros and ones.
  @verbatim|{
   #lang brag
   equal: [zero one | one zero]   ;; equal number of "0"s and "1"s.
   zero: "0" equal | equal "0"    ;; has an extra "0" in it.
   one: "1" equal | equal "1"     ;; has an extra "1" in it.
   }|
 }

 @item{A BNF for
  @link["http://www.json.org/"]{JSON}-like structures.
  @verbatim|{
    #lang brag
    json: number | string
    | array  | object
    number: NUMBER
    string: STRING
    array: "[" [json ("," json)*] "]"
    object: "{" [kvpair ("," kvpair)*] "}"
    kvpair: ID ":" json
    }|
 }
 ]

@subsection{Cuts & splices}

By default, every matched token shows up in the parse tree. But sometimes that means that the parse tree ends up holding a bunch of tokens that were only needed to complete the parsing. Once they've served their purpose, it's sometimes useful to filter them out (for instance, to simplify the implementation of a language expander). To help with this kind of housekeeping, @racket[brag] supports @emph{cuts} and @emph{splices}.

A @deftech{cut} in a grammar will delete an item from the parse tree. A cut is notated by prefixing either the left-hand rule name or a right-hand pattern element with a slash @litchar{/}. 

If the cut is applied to a left-hand rule name, the rule name is omitted from the parse tree, but its node and its matched elements remain. 

If the cut is applied to a right-hand pattern element, then that element is omitted from every node matching that rule.

For instance, consider this simple grammar for arithmetic expressions:

@verbatim|{
#lang brag
expr : term ('+' term)*
term : factor ('*' factor)*
factor : ("0" | "1" | "2" | "3"
       |  "4" | "5" | "6" | "7"
       | "8"  | "9")+
}|

If we use it to parse this string:

@verbatim|{1+2*3}|

We get this parse tree:

@racketblock['(expr (term (factor "1")) "+" (term (factor "2") "*" (factor "3")))]

Suppose we felt the @litchar{+} and @litchar{*} characters were superfluous. We can add cuts to the grammar by prefixing these pattern elements with @litchar{/}:

@verbatim|{
#lang brag
expr : term (/'+' term)*
term : factor (/'*' factor)*
factor : ("0" | "1" | "2" | "3"
       |  "4" | "5" | "6" | "7"
       | "8"  | "9")+
}|

Our parse tree changes accordingly:

@racketblock['(expr (term (factor "1")) (term (factor "2") (factor "3")))]

Now suppose we apply a cut on the rule name, @racket[factor]:

@verbatim|{
#lang brag
expr : term (/'+' term)*
term : factor (/'*' factor)*
/factor : ("0" | "1" | "2" | "3"
       |  "4" | "5" | "6" | "7"
       | "8"  | "9")+
}|

This time, the rule name disppears from the parse tree, but its nodes and elements remain:

@racketblock['(expr (term ("1")) (term ("2") ("3")))]

A @deftech{splice} in a grammar will merge the elements of a node into the surrounding node. A splice is notated by prefixing either the left-hand rule name or a right-hand pattern element with an at sign @litchar|{@}|. 

If the splice is applied to a left-hand rule name, then the splice is applied every time the rule is used in the parse tree. 

If the splice is applied to a right-hand pattern element, that element is spliced only when it appears as part of the production for that rule. 

Suppose we remove the cut from the @racket[factor] rule name and instead splice the second appearance of @racket[factor] in the pattern for the @racket[term] rule:

@verbatim|{
#lang brag
expr : term (/'+' term)*
term : factor (/'*' @factor)*
factor : ("0" | "1" | "2" | "3"
       |  "4" | "5" | "6" | "7"
       | "8"  | "9")+
}|

The @racket[factor] elements matching the first position of the @racket[term] pattern remain as they were, but the @racket[factor] element matching the second position is spliced into the surrounding node:

@racketblock['(expr (term (factor "1")) (term (factor "2") "3"))]

Finally, suppose we add a splice to the @racket[term] rule name:

@verbatim|{
#lang brag
expr : term (/'+' term)*
@term : factor (/'*' @factor)*
factor : ("0" | "1" | "2" | "3"
       |  "4" | "5" | "6" | "7"
       | "8"  | "9")+
}|

This time, all the appearances of @racket[term] nodes in the parse tree will have their elements spliced into the surrounding nodes:

@racketblock['(expr (factor "1") (factor "2") "3")]


As a convenience, when a grammar element is spliced, or a rule name is cut, @racket[brag] preserves the rule name by adding it as a syntax property to the residual elements, using the rule name as a key, and the original syntax object representing the rule name as the value.


@subsection{Syntax errors}

Besides the basic syntax errors that can occur with a malformed grammar, there
are a few other classes of situations that @litchar{#lang brag} will consider
as syntax errors.

@tt{brag} will raise a syntax error if the grammar:
@itemize[
 @item{doesn't have any rules.}

 @item{has a rule with the same left hand side as any other rule.}

 @item{refers to rules that have not been defined. e.g. the
  following program:
  @nested[#:style 'code-inset
          @verbatim|{
             #lang brag
             foo: [bar]
             }|
          ]
  should raise an error because @tt{bar} has not been defined, even though
  @tt{foo} refers to it in an @tech{optional pattern}.}


 @item{uses the token name @racket[EOF]; the end-of-file token type is reserved
  for internal use by @tt{brag}.}


 @item{contains a rule that has no finite derivation. e.g. the following
  program:
  @nested[#:style 'code-inset
          @verbatim|{
             #lang brag
             infinite-a: "a" infinite-a
             }|
          ]
  should raise an error because no finite sequence of tokens will satisfy
  @tt{infinite-a}.}

 ]

Otherwise, @tt{brag} should be fairly tolerant and permit even ambiguous
grammars.

@subsection{Semantics}
@declare-exporting[brag/examples/nested-word-list]

A program written in @litchar{#lang brag} produces a module that provides a few
bindings. The most important of these is @racket[parse]:

@defproc[(parse [source any/c #f] 
                [token-source (or/c (sequenceof token)
                                    (-> token))])
         syntax?]{

 Parses the sequence of @tech{tokens} according to the rules in the grammar, using the
 first rule as the start production. The parse must completely consume
 @racket[token-source].

 The @deftech{token source} can either be a sequence, or a 0-arity function that
 produces @tech{tokens}.

 A @deftech{token} in @tt{brag} can be any of the following values:
 @itemize[
 @item{a string}
 @item{a symbol}
 @item{an instance produced by @racket[token]}
 @item{an instance produced by the token constructors of @racketmodname[br-parser-tools/lex]}
 @item{an instance of @racketmodname[br-parser-tools/lex]'s @racket[position-token] whose 
   @racket[position-token-token] is a @tech{token}.}
 ]

 A token whose type is either @racket[void] or @racket['EOF] terminates the
 source.


 If @racket[parse] succeeds, it will return a structured syntax object. The
 structure of the syntax object follows the overall structure of the rules in
 the BNF grammar. For each rule @racket[r] and its associated pattern @racket[p],
 @racket[parse] generates a syntax object @racket[#'(r p-value)] where
 @racket[p-value]'s structure follows a case analysis on @racket[p]:

 @itemize[
 @item{For implicit and explicit sequences of @tech{pattern}s @racket[p1],
   @racket[p2], ..., the corresponding values, spliced into the
   structure.}
 @item{For terminals, the value of the token.}
 @item{For @tech{rule identifier}s: the associated parse value for the rule.}
 @item{For @tech{choice pattern}s: the associated parse value for one of the matching subpatterns.}
 @item{For @tech{quantifed pattern}s and @tech{optional pattern}s: the corresponding values, spliced into the structure.}
 ]

 Consequently, it's only the presence of @tech{rule identifier}s in a rule's
 pattern that informs the parser to introduces nested structure into the syntax
 object.


 If the grammar is ambiguous, @tt{brag} will choose one of the possible parse results, though it doesn't guarantee which.


 If the parse cannot be performed successfully, or if a token in the
 @racket[token-source] uses a type that isn't mentioned in the grammar, then
 @racket[parse] raises an instance of @racket[exn:fail:parsing].}


@defproc[(parse-to-datum [source any/c #f] 
                         [token-source (or/c (sequenceof token)
                                             (-> token))])
         list?]{
 Same as @racket[parse], but the result is converted into a plain datum. Useful for testing or debugging a parser.
}


@defform[#:id make-rule-parser
         (make-rule-parser name)]{
 Constructs a parser for the @racket[name] of one of the non-terminals
 in the grammar. 

 For example, given the @tt{brag} program
 @filepath{simple-arithmetic-grammar.rkt}:
 @filebox["simple-arithmetic-grammar.rkt"]{
  @verbatim|{
   #lang brag
   expr : term ('+' term)*
   term : factor ('*' factor)*
   factor : INT
   }|
 }
 the following interaction shows how to extract a parser for @racket[term]s.
 @interaction[#:eval my-eval
              @eval:alts[(require "simple-arithmetic-grammar.rkt") 
                         (require brag/examples/simple-arithmetic-grammar)]
              (define term-parse (make-rule-parser term))
              (define tokens (list (token 'INT 3) 
                                   "*" 
                                   (token 'INT 4)))
              (syntax->datum (parse tokens))
              (syntax->datum (term-parse tokens))

              (define another-token-sequence
                (list (token 'INT 1) "+" (token 'INT 2)
                      "*" (token 'INT 3)))
              (syntax->datum (parse another-token-sequence))
              @code:comment{Note that term-parse will break on another-token-sequence}
              @code:comment{as it does not know what to do with the "+"}
              (term-parse another-token-sequence)
              ]

}


@defthing[all-token-types (setof symbol?)]{
 A set of all the token types used in a grammar.

 For example:
 @interaction[#:eval my-eval
              @eval:alts[(require "simple-arithmetic-grammar.rkt") 
                         (require brag/examples/simple-arithmetic-grammar)]
              all-token-types
              ]

}



@section{Support API}

@defmodule[brag/support]

The @racketmodname[brag/support] module provides functions to interact with
@tt{brag} programs. The most useful is the @racket[token] function, which
produces tokens to be parsed.

In addition to the exports shown below, the @racketmodname[brag/support] module also provides everything from @racketmodname[brag/support], and everything from @racketmodname[br-parser-tools/lex].


@defproc[(token [type (or/c string? symbol?)]
                [val any/c #f]
                [#:line line (or/c positive-integer? #f) #f]
                [#:column column (or/c natural-number? #f) #f]
                [#:position position (or/c positive-integer? #f) #f]
                [#:span span (or/c natural-number? #f) #f]
                [#:skip? skip? boolean? #f]
                )
         token-struct?]{
 Creates instances of @racket[token-struct]s.

 The syntax objects produced by a parse will inject the value @racket[val] in
 place of the token name in the grammar.

 If @racket[#:skip?] is true, then the parser will skip over it during a
 parse.}


@defstruct[token-struct ([type symbol?]
                         [val any/c]
                         [position (or/c positive-integer? #f)]
                         [line (or/c natural-number? #f)]
                         [column (or/c positive-integer? #f)]
                         [span (or/c natural-number? #f)]
                         [skip? boolean?])
           #:transparent]{
 The token structure type.

 Rather than directly using the @racket[token-struct] constructor, please use
 the helper function @racket[token] to construct instances.
}




@defstruct[(exn:fail:parsing exn:fail) 
           ([message string?]
            [continuation-marks continuation-mark-set?]
            [srclocs (listof srcloc?)])]{
 The exception raised when parsing fails.

 @racket[exn:fail:parsing] implements Racket's @racket[prop:exn:srcloc]
 property, so if this exception reaches DrRacket's default error handler,
 DrRacket should highlight the offending locations in the source.}



@defproc[(apply-tokenizer-maker [tokenizer-maker procedure?] 
                                [source (or/c string?
                                              input-port?)])
         list?]{
 Repeatedly apply @racket[tokenizer-maker] to @racket[source], gathering the resulting tokens into a list. @racket[source] can be a string or an input port. Useful for testing or debugging a tokenizer.
}

@defproc[(apply-lexer [lexer procedure?] 
                      [source (or/c string?
                                    input-port?)])
         list?]{
 Repeatedly apply @racket[lexer] to @racket[source], gathering the resulting tokens into a list. @racket[source] can be a string or an input port. Useful for testing or debugging a lexer.
}


@defproc[(trim-ends [left-str string?]
                    [str string?]
                    [right-str string?])
         string?]{
 Remove @racket[left-str] from the left side of @racket[str], and @racket[right-str] from its right side. Intended as a helper function for @racket[from/to].
}


@defform[(:* re ...)]{

 Repetition of @racket[re] sequence 0 or more times.}

@defform[(:+ re ...)]{

 Repetition of @racket[re] sequence 1 or more times.}

@defform[(:? re ...)]{

 Zero or one occurrence of @racket[re] sequence.}

@defform[(:= n re ...)]{

 Exactly @racket[n] occurrences of @racket[re] sequence, where
 @racket[n] must be a literal exact, non-negative number.}

@defform[(:>= n re ...)]{

 At least @racket[n] occurrences of @racket[re] sequence, where
 @racket[n] must be a literal exact, non-negative number.}

@defform[(:** n m re ...)]{

 Between @racket[n] and @racket[m] (inclusive) occurrences of
 @racket[re] sequence, where @racket[n] must be a literal exact,
 non-negative number, and @racket[m] must be literally either
 @racket[#f], @racket[+inf.0], or an exact, non-negative number; a
 @racket[#f] value for @racket[m] is the same as @racket[+inf.0].}

@defform[(:or re ...)]{

 Same as @racket[(union re ...)].}

@deftogether[(
              @defform[(:: re ...)]
               @defform[(:seq re ...)]
               )]{

 Both forms concatenate the @racket[re]s.}

@defform[(:& re ...)]{

 Intersects the @racket[re]s.}

@defform[(:- re ...)]{

 The set difference of the @racket[re]s.}

@defform[(:~ re ...)]{

 Character-set complement, which each @racket[re] must match exactly
 one character.}

@defform[(:/ char-or-string ...)]{

 Character ranges, matching characters between successive pairs of
 characters.}

@defform[(from/to open close)]{

 A string that is bounded by @racket[open] and @racket[close]. Matching is non-greedy (meaning, it stops at the first occurence of @racket[close]). The resulting lexeme includes @racket[open] and @racket[close]. To remove them, see @racket[trim-ends].}

@defform[(from/stop-before open close)]{

 Like @racket[from/to], a string that is bounded by @racket[open] and @racket[close], except that @racket[close] is not included in the resulting lexeme. Matching is non-greedy (meaning, it stops at the first occurence of @racket[close]).}


@close-eval[my-eval]
