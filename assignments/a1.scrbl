#lang scribble/manual

@title[#:tag "A1"]{A1: Racket Primer}

@bold{Due:} February 10, 2025

The goal of this assignment is to gain practice programming in Racket.

You are given a @tt{hw1.rkt} file (on Canvas under "Files"), that contains a number of sections. In each section there are several function “stubs,” i.e. incomplete function definitions with type signatures, descriptions, and a small set of tests. Each function has a bogus (but type correct) body marked with a “TODO” comment. Your job is to replace each of these expressions with a correct implementation of the function.

Rename the file to @tt{main.rkt}. This step is important! Also make sure not to change the name or signature of any function given to you. You may add any additional functions that help you solve the overall problem you’re tackling.

@section{Testing}

@margin-note{@bold{Note:} Running @tt{racket main.rkt} from the command line will not run the tests.}

You can test your code in several ways:
@itemlist[
@item{Running the code in DrRacket will (by default) run the test submodule and print out a report of any test failures. This is actually a configurable preference, but it is on by default.}
@item{Using the command line @tt{raco test main.rkt} from the same directory as @tt{main.rkt}.}
]

@section{Submitting}

Make sure your file is named @tt{main.rkt}. Submit your filled-in @tt{main.rkt} file on Gradescope. You can submit multiple times, but only the latest submission will be counted.

You can @tt{Cmd + F} or @tt{Ctrl + F} for "TODO" through your submission to find if you have any remaining tasks.

@section{Grading}

Your submission will be graded for correctness. Passing the unit tests included in the file is necessary but @bold{not sufficient} to receive a perfect score. You are strongly encouraged to add your own tests to ensure the correctness of your solutions.
