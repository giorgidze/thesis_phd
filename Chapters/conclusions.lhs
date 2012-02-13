\chapter{Directions for Future Work and Conclusions}
\label{chapConclusions}

In this thesis, we described a new approach to the design and implementation
of noncausal modelling and simulation languages. From the language design
point of view, the key idea was to embed equational models as first-class
entities into a functional programming language. We provided a range of
examples demonstrating how the notion of first-class models can be used for
higher-order and (unbounded) structurally dynamic modelling, and thus going
beyond to what is expressible in current noncausal modelling languages. From
the language implementation point of view, the key idea was to enable
efficient simulation of noncausal models that are generated at simulation
runtime by runtime symbolic processing and just-in-time compilation. We
defined a formal semantics for the language developed in this thesis and
provided an in-depth description of its implementation. We hope that this work
will facilitate adoption of the aforementioned approaches by designers and
implementers of modelling and simulation languages.

Throughout the thesis we have identified a number of directions for future
work. Let us conclude the thesis by consolidating these directions in the list
given below.

\begin{itemize}

\item Introduce the notion of first-class models in mainstream noncausal
modelling languages such as Modelica. Modelica already supports first-class
functions. Taking this development one step further by supporting first-class
models would allow for improved higher-order and structurally dynamic
modelling capabilities as demonstrated in this thesis.

\item Make use of the ideal semantics for verification of simulation results.
In particular, based on the ideal semantics, it should be possible to develop
a tool for automatic verification of simulation results.

\item Investigate properties of the ideal semantics developed in this thesis
and apply them to the problem of verification of the language implementation.
Although challenging, it would be interesting to investigate possibilities of
producing formally verified symbolic processors and numerical simulators for
noncausal languages like Hydra.

\item Develop symbolic methods for reducing mode switching overheads,
especially those overheads that are associated to just-in-time compilation.
Merging of Hydra's implementation approach to that of Sol would be a good
starting point.

\item Entirely avoid recompilation for discrete changes that are not
structural changes. Introduction of the notion of impulses in Hydra would be a
good starting point.

\item Combine FHM and FRP frameworks in a single coherent language. The first
step into this direction would be to introduce support for statefull signal
functions in Hydra.

\item Compare different approaches for specifying noncausal connections
(including the approach based on combinators used in this thesis).

\end{itemize}