#lang scribble/manual

@title[#:tag "Software"]{Software}

We will use Racket as the implementation language and source language of our interpreters.

@section{Installing Racket}

Racket is available for all major operting systems from: @link["https://racket-lang.org/"]{https://racket-lang.org/}

We will use Racket 8.14, but any recent version should work fine.

There are two essential references:

@itemlist[
@item{@link["https://docs.racket-lang.org/guide/"]{The Racket Guide} - intended for those new to Racket, i.e. @emph{you!}}
@item{@link["https://docs.racket-lang.org/reference/"]{The Racket Reference} - the definitive, comprehensive manual for Racket}
]

Racket is a large, full-featured, batteries-included language platform. However, we will be using only a small subset of Racket. This subset should be easy to pick up for anyone familiar with functional programming. If you’re comfortable with basic Haskell or JavaScript, you shouldn’t have much trouble learning the parts of Racket we will be using.

@section{IDE}

Racket comes with it’s own IDE: DrRacket, which is the recommended way to edit Racket files. We will also be running Racket and its associated tools from the command line. Using any other editor is fine, too.
