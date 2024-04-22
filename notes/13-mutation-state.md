---
layout: page
parent: Notes
title: "13. Mutation and State"
---

# Mutation and State
{: .no_toc }

<details open markdown="block">
  <summary>
    Table of contents
  </summary>
  {: .text-delta }
- TOC
{:toc}
</details>

## Imperative languages

Until this point in the class, we developed a pure functional programming language, i.e., you could create and pass functions around or apply them to other values. More importantly, all computations were expressed as function applications---arguments went in and the computed value was returned. This is a general model for expressing all kinds of computations, but it might still feel limiting. Many of you are used to imperative languages (like C or Python), where instead of function applications being nested, you are used to a sequence of statments that execute with the program. The goal of this module is to learn how to model such languages and understand how they work.

The key thing in these language is an _implicit state_. As the language executes individual statements, the statements compute a return value and/or modify this implicit state. Contrast this with our functional language where the only thing computed is the value returned, there is no state hidden away from the programmer like what happens in imperative languages. Through the following sections we will look at how we add state to our program.

## Concrete Syntax

<!--
\begin{array}{lccl}
\textrm{Values}      & v & ::=  & \textrm{Integer} \mid \texttt{\#t} \mid \texttt{\#f} \mid \texttt{($\lambda$ ($x\ \ldots\ x$) $e$)} \mid l\\
\textrm{Expressions} & e & ::=  & v \mid x \mid l \mid \ldots \\
                     &   & \mid & \texttt{(begin $e_1 \ldots e_n$)} \mid \texttt{(new $e$)} \\
                     &   & \mid & \texttt{(deref $e$)} \mid \texttt{(set! $e$ $e$)}
\end{array}
-->
![State Grammar]({{site.baseurl}}/images/state-grammar.png)

## Abstract Syntax

<!--
\begin{array}{lccl}
\textrm{Expressions} & e & ::=  & \ldots \mid \texttt{(Seq $es$)} \mid \texttt{(New $e$)} \mid \texttt{(Deref $e$)} \mid \texttt{(Set $e$ $e$)}
\end{array}
-->
![State AST]({{site.baseurl}}/images/state-ast.png)

## Meaning of state

<!--
\frac{D, E \vdash \langle S, e_1 \rangle \Downarrow \langle S_1, v_1 \rangle \qquad D, E \vdash \langle S_1, e_2 \rangle \Downarrow \langle S_2, v_2 \rangle}
{D, E \vdash \langle S, \texttt{(Seq $e_1$ $e_2$)} \rangle \Downarrow \langle S_2, v_2 \rangle} \quad [\textsc{E-Seq}]
-->
![State E-Seq]({{site.baseurl}}/images/state-e-seq.png)

<!--
\frac{D, E \vdash \langle S, e \rangle \Downarrow \langle S_1, v \rangle \qquad l \text{ is fresh}}
{D, E \vdash \langle S, \texttt{(New $e$)} \rangle \Downarrow \langle S_1[l \mapsto v], l\rangle} \quad [\textsc{E-New}]
-->
![State E-Seq]({{site.baseurl}}/images/state-e-new.png)

<!--
\frac{D, E \vdash \langle S, e \rangle \Downarrow \langle S_1, l \rangle}
{D, E \vdash \langle S, \texttt{(Deref $e$)} \rangle \Downarrow \langle S_1, S_1(l) \rangle} \quad [\textsc{E-Deref}]
-->
![State E-Seq]({{site.baseurl}}/images/state-e-deref.png)

<!--
\frac{D, E \vdash \langle S, e_1 \rangle \Downarrow \langle S_1, l \rangle \qquad D, E \vdash \langle S_1, e_2 \rangle \Downarrow \langle S_2, v \rangle}
{D, E \vdash \langle S, \texttt{(Set $e_1$ $e_2$)} \rangle \Downarrow \langle S_2[l \mapsto v], v \rangle} \quad [\textsc{E-Set}]
-->
![State E-Seq]({{site.baseurl}}/images/state-e-set.png)

## Interpreter

## Testing

## Mutability
