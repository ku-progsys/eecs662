# EECS 662: Programming Languages

_University of Kansas, Spring 2025_

[Website](https://ku-progsys.github.io/eecs662/)

This repository contains the course material for EECS 662: Programming Languages, taught at the University of Kansas.

## Build Instructions

```bash
raco scribble --htmls ++style css/fancyverb.css ++xref-in setup/xref load-collections-xref \
              --redirect-main http://docs.racket-lang.org/ \
              main.scrbl
```
