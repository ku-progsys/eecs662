#lang scribble/manual

@(require "constants.rkt")

@(define (wk d) (nonbreaking (bold d)))

@title[#:style 'unnumbered]{Schedule}

@;{ Fall }
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
                     @elem{@prof-initials Traveling})

               (list @wk{1/28}
                      ""
                     @elem{@secref["Hs2Rkt"]}                     
                     @elem{@secref["Hs2Rkt"]})

               (list @wk{2/4}
                      ""
                     @elem{@secref["Amount"]}                     
                     @elem{})

               (list @wk{2/11}
                      ""
                     @elem{}                     
                     @elem{})

               (list @wk{2/18}
                      ""
                     @elem{}                     
                     @elem{})

               (list @wk{2/25}
                      ""
                     @elem{}                     
                     @elem{})

               (list @wk{3/4}
                      ""
                     @elem{}                     
                     @elem{})

               (list @wk{3/11}
                      ""
                     @elem{}                     
                     @elem{})

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
