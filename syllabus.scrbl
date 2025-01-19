#lang scribble/manual

@(require "constants.rkt")

@title[#:tag "Syllabus" #:style 'unnumbered]{Syllabus}

@section{Instructor Details}

@bold{Instructor:} @prof (@prof-pronouns) @(linebreak)
Office Hours: @prof-oh @(linebreak)
Office: @prof-office @(linebreak)
@prof-email

@bold{Teaching Assistant:} @ta (@ta-pronouns) @(linebreak)
Office Hours: @ta-oh @(linebreak)
Office: @ta-office @(linebreak)
@ta-email

@emph{Please email to set an appointment if standard office hours times do not work for you.}

@section{Class Time and Location}

@bold{Lectures:} @lec-time-loc

@section{Course Hours}

@bold{Credit Hours:} 3

A @link["https://policy.ku.edu/registrar/credit-hour"]{credit hour} is a way
to measure the amount of work you're expected to do for a class. It's based on
the learning goals of the course and how much time you'll spend on it. During a
full length (15-week) semester, you can expect to spend about one hour in class
and another two hours on homework or studying each week for each credit hour.

For classes completed in other formats, like an 8-week semester, the time
commitment may be different. The exact hours can vary, but you can generally
expect to spend a similar amount of time in class and on coursework in total,
adjusted for the shorter timeframe.

@section{Instructional Methods}

All lectures will be held in-person, unless other wise notified. If the
instructor is traveling alternative classes may be organized on Zoom.

@section{Course Description}

@desc

@section{Topics}

@itemlist[@item{Introduction}
          @item{Expression Interpreters}
          @item{Programs as Data Structures}
          @item{Identifiers and Substitution}
          @item{Functions and Scoping}
          @item{Recursion}
          @item{State}
          @item{Types and Type checking}
          @item{Objects}
          @item{Laziness}]

@section{Learning Outcomes}

For those of us who write software, the programming language is our primary
tool. Some languages make it easier to write certain kinds programs,
while others prevent us from writing certain kinds of bad programs altogether.
Some languages prioritize freedom and flexibility, while others seek a balance
between expressiveness and limitations. Understanding these trade-offs not only
deepens our understanding of computation but also makes us better programmers.

This course offers a framework to help you gain this understanding. The goal is
to help you: explore the fundamental components of modern programming languages,
use these components to compare and contrast different languages (and prepare
you to create your own!), and reflect on the relationship between languages,
programming, and, in some cases, programmers themselves.

@section{Course Materials}

This class has no required textbook. All course materials can be found in the
@secref{Notes}. A few references (all available for free) are listed in @secref{Texts}.

@section{Minimal Technical Skills Needed}

The class has a prerequisite of EECS 368 or EECS 468 and EECS 330 or EECS 560.

@section{Course Assignments and Requirements}

@itemlist[@item{Midterm Exam 20%}
          @item{Final Exam 20%}
          @item{Assignments 60%}]

@section{Student Survey of Teaching}

You will have multiple opportunities to provide feedback on your experience in
this course. Suggestions and constructive criticism are encouraged throughout
the course and may be particularly valuable early in the semester. To that end,
I may use surveys and/or reflection assignments to gather input on what is
working well and what could be improved. You will also be asked to complete an
end-of-semester, online Student Survey of Teaching, which could inform
modifications to this course (and other courses that I teach) in the future.

@section{Grading}

Homework assignments, quizzes, and exams will be graded and returned to the
class via Gradescope. Students have one week from when an item is returned in
class to request a regrade.

Your final grade in the class will be computed by using the weighted average
given above, and the following scale:

@itemlist[@item{A = 92-100%}
          @item{A- = 90-92%}
          @item{B+ = 88-90%}
          @item{B = 82-88%}
          @item{B- = 80-82%}
          @item{C+ = 78-80%}
          @item{C = 72-78%}
          @item{C- = 70-72%}
          @item{D+ = 68-70%}
          @item{D = 62-68%}
          @item{D- = 60-62%}
          @item{F = 0-60%}]

Your total scores will not be rounded up. If you are on the boundary, you will
be given the higher grade. For example, if your total is exact 88, you will get
B+. However, if your total is 87.9 you will get B.

Depending on overall student performance in the course, I reserve the right to
lower (but not raise) the above grade cutoffs. However, this is not something
you should count on.

@section{Attendance Policy}

I do not take attendance. You are however, expected and encouraged to attend and
participate in class. If you have any problems which will cause you to miss a
class or an assignment deadline please email me.

The @link["https://policy.ku.edu/governance/USRR#excused"]{University Excused
Absence Policy} is a good reference for allowed reasons.

@section{Academic Integrity}

Academic misconduct of any kind will automatically result in a 0 score on the homework, lab, project,
or exam in question and your actions will be reported to the department chair. Your homework, exams
and projects must be individually prepared unless otherwise noted. Posting your assignments to
internet discussion lists is considered academic misconduct. Sharing your solutions with others is
considered academic misconduct. Turning in solutions from previous semesters is considered academic
misconduct. Paying people to prepare solutions is academic misconduct. Automated mechanisms are
available for checking the originality of source code.

@section{Subject to Change Statement}

The schedule for the class beyond next week is subject to change. Please consult the course website to
know the latest version of the schedule and syllabus. Any changes in assignments and syllabus policies
will conveyed via Canvas announcements.

@section{Student Resources and University Policies}

Please visit the Student Resources website (@link["https://academicsuccess.ku.edu/student-resources-0"]{KU Academic Success}) for a list of student
resources and university policies. 

@section{Accommodation}

The Student Access Center (SAC) coordinates academic accommodations and services for all eligible KU
students with disabilities. If you have a disability for which you wish to request accommodations and
have not contacted SAC, please do so as soon as possible. They are located in 22 Strong Hall and can
be reached at 785-864-4064 (V/TTY). Information about their services can be found at
@link["https://access.ku.edu"]{access.ku.edu}. Please contact me privately in regard to your needs
in this course.
