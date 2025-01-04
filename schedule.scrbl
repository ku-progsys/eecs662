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
)]
