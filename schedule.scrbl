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
                     @secref{Intro}
                     @elem{@emph{@prof-initials Traveling}})

               (list @wk{1/28}
                      ""
                     @secref{Hs2Rkt}
                     @elem{@secref{Hs2Rkt} (Cont.)})

               (list @wk{2/4}
                     @secref{A1}
                     @secref{Amount}
                     @secref{Arithmetic})

               (list @wk{2/11}
                      ""
                     @secref{Formal}
                     @secref{Booleans})

               (list @wk{2/18}
                      ""
                     @secref{Errors}
                     @secref{Let})

               (list @wk{2/25}
                      ""
                     @secref{Lambda}
                     @elem{@emph{@ta-initials Teaching}})

               (list @wk{3/4}
                     @secref{A2}
                     @elem{@secref{Lambda} (Cont.)}
                     @elem{@secref{Lambda} (Cont.)})

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
                     @secref{Env}
                     @elem{@secref{Env} (Cont.)})

               (list @wk{4/1}
                      ""
                     @secref{Rewrites}
                     @secref{State})

               (list @wk{4/8}
                     @secref{A3}
                     @elem{@secref{State} (Cont.)}
                     @elem{GC})

               (list @wk{4/15}
                      ""
                     @elem{Types}
                     @elem{Type Inference})
            
               (list @wk{4/22}
                      ""
                     @elem{Objects}
                     @elem{})

               (list @wk{5/6}
                      ""
                     @elem{Analysis}
                     @elem{})

               (list @wk{5/6}
                      ""
                     @elem{Declarative}
                     @elem{Parser})

               (list @wk{5/13}
                      ""
                     @bold{Finals Week}
                     @elem{})
)]
