#lang scribble/manual

@(require redex pict "model.rkt")

@title[#:style '(toc unnumbered)]{Notes}

Testing Redex + Scribble

@centered{@(scale (render-language L) 1.5)}

@centered{@(scale (render-eval-rules-judgment) 1.5)}

@centered{@(scale (renderer (λ () (derivation->pict L (car (build-derivations (eval () (Let x (UnOp 'add1 (Val 0)) (UnOp 'zero? (UnOp 'sub1 (Var x)))) #t)))))) 1.5)}
