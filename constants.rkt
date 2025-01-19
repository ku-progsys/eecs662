#lang racket
(provide (all-defined-out))
(require scribble/core scribble/html-properties scribble/manual) 

(define semester "Spring, 2025")
(define lec-time-loc "Tuesday and Thursday, 9:30 - 10:45am, LEEP2 G415")

(define desc #<<course-desc
Programming Languages is an introduction to basic principles of defining, describing and implementing
programming languages and their interpreters. The fundamental goal is establishing a vocabulary for
discussing what programming languages and programs written in them do. Topics covered to accomplish
this are data representation and types; declarations, bindings and variable assignment; parameter
passing and function evaluation; statements; and objects and types. The course uses an
implementation-based approach with students developing interpreters for languages that demonstrate
features presented in class.
course-desc
)

(define prof (link "https://sankhs.com" "Sankha Narayan Guria"))
(define prof-pronouns "he/him")
(define prof-email (link "mailto:sankha@ku.edu" "sankha@ku.edu"))
(define prof-initials "SNG")
(define prof-office "2034 Eaton Hall")
(define prof-oh "Tuesday, 11:00 - 12:00pm")

(define ta "Bryan Richlinski")
(define ta-pronouns "he/him")
(define ta-email (link "mailto:b748r023@ku.edu" "b748r023@ku.edu"))
(define ta-initials "BR")
(define ta-office "TBD")
(define ta-oh "Monday and Friday, 2:00 - 3:00pm")

(define canvas (link "https://canvas.ku.edu/courses/143647" "Canvas"))
