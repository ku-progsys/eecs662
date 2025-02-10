#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict "model.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "amount" f))))))
	   '("interp.rkt"))

@(define core-racket
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator 'racket)))

@(core-racket '(require racket/match))

@(define-syntax-rule (eval)
  (scale (text "⇓") 1.5))

@(define-syntax-rule (ex e ...)
  (filebox (emph "Racket REPL")
    (examples #:eval core-racket #:label #f e ...)))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path "examples" "amount")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@title[#:tag "Amount"]{Amount: A Language of Numbers}

@table-of-contents[]

@section{Overview}

An interpreter is a single component of a @emph{programming language}. So if you want to make an interpreter, you have to settle on the programming language you want to evaluate.

The specification of a programming language consists of two parts:

@itemlist[#:style 'ordered
@item{@bold{Syntax:} that defines the form of programs}
@item{@bold{Semantics:} that define the meaning of programs}
]

Syntax, while important is a fairly superficial aspect of programming languages. The real heart of a programming language is its semantics and we will spend more time concerned with this aspect.

There are a few ways in which the meaning of language is specified:

@itemlist[
@item{By example}
@item{By informal description}
@item{By reference to an implementation, often an interpreter}
@item{By formal (mathematical) definitions}
]

Each approach has it's strengths and weaknesses. Examples are easy to understand, but they are incomplete and can get complicated for complex program behaviors. Informal descriptions on the other hand can be intuitive, but can be open to interpretation and ambiguity. Reference implementations provide precise, executable specifications but may over specify the language details by tying them to the implementation artifacts. In some cases, we might not have a reference implementation readily available. Lastly, formal definitions can balance both precision and under-specification, but require detailed definitions and training to understand.

For this class, we will use a combination of each wherever possible.

To begin, let us start with a very small language: @emph{Amount}. The only kind of expression in @emph{Amount} are integer literals. Running an @emph{Amount} program produces that integer. Simple, isn't it?

@section{Concrete syntax for Amount}

We will simplify matters of syntax by using the Lisp notation of s-expression for the @bold{concrete} form of program phrases. The job of a @emph{parser} is to construct an abstract syntax tree from the textual representation of a program. We will consider parsing in two phases:

@itemlist[
@item{the first converts a stream of textual input into an s-expression, and}
@item{the second converts an s-expression into an instance of a datatype for representing expressions called an @bold{AST}.}
]

For the first phase, we rely on the @racket[read] function to take care of converting strings to s-expressions. In order to parse s-expressions into ASTs, we will write fairly straightforward functions that convert between the representations.

Amount, like many of the other languages studied in this course, is designed to be a subset of Racket. This has two primary benefits:

@itemlist[
@item{the Racket interpreter can be used as a reference implementation of the languages we build, and}
@item{there are built-in facilities for reading and writing data in the parenthesized form that Racket uses, which we can borrow to make parsing easy.}
]

The concrete form of an Amount program will consist of, like Racket, the line of text:

@verbatim{
#lang racket
}

followed by a (concrete) expression. The grammar of expressions is very simple:

@centered{@(scale (render-language L) 1.5)}

So, @racket[0], @racket[120], @racket[-42], etc. are concrete Amount expressions and a complete Amount program looks like this:

@codeblock-include["amount/42.rkt"]

Reading Amount programs from ports, files, strings, etc. consists of reading (and ignoring) the @tt{#lang racket} line and then using the @racket[read] function to parse the concrete expression as an s-expression.

@section{Abstract syntax for Amount}

@margin-note{Later in the semester, we will look at changing parsers to parse different syntax but use our existing language semantics.}

The program as written by the user is any sequence of characters. Before the program can be evaluated, it has be checked if it is in the concrete syntax of the language! After that, all programs have to converted to a tree representation that removes all ambiguities from the concrete syntax. In most language implementations these are done by a component known as a @emph{parser}. While a parser is interesting in it's own right, it's scope is limited to the syntax of the user language. Parsers are covered in depth in @link["https://compilers.cool"]{EECS 665}. In this class, we will focus on the language semantics rather than the syntax!

Recall that all Racket programs are written as s-expressions which are already tree-like structures and are un-ambiguous. So we can skip the parser entirely and directly look at the language semantics for this class!

@section{Meaning of Amount programs}

Even though the semantics for Amount is obvious, we can provide a formal definition using operational semantics.

An operational semantics is a mathematical definition that characterizes the meaning of programs. We will defined the semantics of Amount as a binary relation between programs and their meanings. So in the setting of Amount, this binary relation will be a set of pairs of expressions and integers. This relation will be defined inductively using inference rules. For such a simple language, just a single inference rule suffices:

@centered{@(scale (render-eval-rules-judgment) 1.5)}

Here, we are defining a binary relation, called @(eval), and saying every integer literal expression is paired with the integer itself in the relation. So @math{(2, 2)} is in @(eval), @math{(5, 5)} is in @(eval), and so on.

@margin-note{This part probably seems opaque at the moment, but it will become clearer as we work through more example, so don't worry.}

The inference rules define the binary relation by defining the evidence for being in the relation. The rule makes use of meta-variables drawn from the non-terminals of the language grammar. A pair is in the relation if you can construct an instance of the rule (substituting some integer) in the rule.

The operational semantics @emph{defines} the meaning of Amount programs. The interpreter @emph{computes} that meaning. We can view the semantics as a specification, and the interpreter as an implementation.

Characterizing the correctness of the interpreter boils down to the following statement:

@bold{Interpreter Correctness:} @emph{For all expressions @racket[e] and integers @racket[v], if @racket[e] @(eval) @racket[v], then the interpreter @racket[(interp e)] equals @racket[v].}

We now have a complete (if overly simple) programming language with an operational semantics. Now let's write an interpreter.

@section{An Interpreter for Amount}

We can write an ``interpreter'' that consumes an expression and produces it's meaning, i.e., a translation of the operational semantics to code:

@codeblock-include["amount/interp.rkt"]

Examples:

@#reader scribble/comment-reader
(examples #:eval ev
(interp 42)
(interp -8)
)

We can add a command line wrapper program for interpreting Amount programs saved in files:

@codeblock-include["amount/main.rkt"]

The details here aren't important (and you won't be asked to write this kind of code), but this program reads the contents of a file given on the command line. It then runs the interpreter and displays the result.

For example, interpreting the program @tt{42.rkt} shown above:

@(shellbox "racket -t main.rkt -m 42.rkt")

@section{But is it @emph{Correct?}}

At this point, we have an interpreter for Amount. But is it correct?

Recall the statement for interpreter correctness, which states our interpreter will capture the operational semantics of Amount. Though proving this is an interesting endeavour, however, from a practical standpoint we can use the other ways of specifying the language's behavior to check our interpreter for Amount does what we expect it to do. Since we can run the interpreter (unlike the operational semantics), we can @emph{test} it on multiple inputs to see if our interpreter is correct.

One of the ways to specify the meaning of programs is through examples. We can turn this into tests. For Amount, the output of the program is the program itself.

@#reader scribble/comment-reader
(examples #:eval ev
(check-eqv? (interp 42) 42)
(check-eqv? (interp 37) 37)
(check-eqv? (interp -8) -8)
)

We can go one step further. Another way to specify programs is to check if it is in compliance with a reference interpreter. As the language we just built, Amount, is a subset of Racket we can also run amount programs in Racket. In other words, given a Amount program, if our interpreter agrees with Racket, we have a correct interpreter.

We have the following equivalence:

@centered{
@racket[(interp e)] @emph{equals} @racket[(eval e)]
}

where @racket[eval] runs a program in the Racket interpreter.

We can turn this in a property-based test, i.e. a function that computes a test expressing a single instance of our interpreter correctness claim:

@#reader scribble/comment-reader
(examples #:eval ev
(define (check-interp e)
  (check-eqv? (interp e) (eval e)))
(check-interp 42)
(check-interp 37)
(check-interp -8)
)

This is a powerful testing technique when combined with random generation. Since our correctness claim should hold for all Amount programs, we can randomly generate any Amount program and check that it holds.

@#reader scribble/comment-reader
(examples #:eval ev
(check-interp (random 100))
(for ([i (in-range 10)])
  (check-interp (random 10000)))
)

The last expression is taking @racket[10] samples from the space of Amount programs in @math{[0,10000)} and checking the interpreter correctness claim on them. If the claim doesn't hold for any of these samples, a test failure would be reported.

Finding an input to @racket[check-interp] that fails would refute the interpreter correctness claim and mean that we have a bug. Such an input is called a @bold{counter-example}.

On the other hand we gain more confidence with each passing test. While passing tests increase our confidence, we cannot test all possible inputs this way, so we can't be sure our interpreter is correct by testing alone. To really be sure, we'd need to write a proof, but that's beyond the scope of this class.
