#lang scribble/manual

@title[#:style '(toc unnumbered)]{Notes}

These are the course notes for EECS 662. They can be a bit rough around the
edges in some places, but please keep in mind this is a @emph{living} document.
If you spot errors, confusing prose, missing parts, or generally have
suggestions for improving the material, @bold{please} submit an
@link["https://github.com/ku-progsys/eecs662"]{issue on Github}.

@local-table-of-contents[#:style 'immediate-only]

@include-section{notes/1/interpreter.scrbl}
@include-section{notes/2/hs2rkt.scrbl}
@include-section{notes/3/numbers.scrbl}
@;{
@include-section{notes/4/arithmetic.scrbl}
}

@;{
@centered{@(scale (renderer (λ () (derivation->pict L (car (build-derivations (eval () ((let ((x 3)) (λ (y) (+ y x))) 2) 5)))))) 1.5)}
}
