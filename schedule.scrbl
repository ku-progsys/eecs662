#lang scribble/manual

@(require "constants.rkt")

@(define (wk d) (nonbreaking (bold d)))

@title[#:style 'unnumbered]{Schedule}

@emph{Schedule beyond next week is subject to change!}

@tabular[#:style 'boxed
         #:sep @hspace[1]
         #:row-properties '(bottom-border)
         (list (list @bold{Week}
                     @bold{Due}
                     @bold{Tuesday}
                     @bold{Thursday})

               (list @wk{1/21}
                      ""
                     @secref["Intro"]
                     @elem{@emph{@prof-initials Traveling}})

               (list @wk{1/28}
                      ""
                     @secref["Hs2Rkt"]
                     @elem{@secref["Hs2Rkt"] (Cont.)})

               (list @wk{2/4}
                      ""
                     @secref["Amount"]
                     @elem{Arithmetic})

               (list @wk{2/11}
                      ""
                     @elem{Booleans}
                     @elem{Formal Semantics})

               (list @wk{2/18}
                      ""
                     @elem{Errors}
                     @elem{Lambda})

               (list @wk{2/25}
                      ""
                     @elem{Lambda Calculus}
                     @elem{})

               (list @wk{3/4}
                      ""
                     @elem{Let bindings}
                     @elem{Programs as Data Structures})

               (list @wk{3/11}
                      ""
                     @elem{Midterm Review}
                     @elem{Midterm})

               (list @wk{3/18}
                      ""
                     @bold{Spring Break}
                     @elem{})

               (list @wk{3/25}
                      ""
                     @elem{}
                     @elem{})

               (list @wk{4/1}
                      ""
                     @elem{}
                     @elem{})

               (list @wk{4/8}
                      ""
                     @elem{}
                     @elem{})

               (list @wk{4/15}
                      ""
                     @elem{}
                     @elem{})

               (list @wk{4/22}
                      ""
                     @elem{}
                     @elem{})

               (list @wk{4/29}
                      ""
                     @elem{}
                     @elem{})

               (list @wk{5/6}
                      ""
                     @elem{}
                     @elem{})

               (list @wk{5/13}
                      ""
                     @bold{Finals Week}
                     @elem{})
)]
