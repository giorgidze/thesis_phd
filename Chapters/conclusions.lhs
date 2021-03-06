\chapter{Directions for Future Work and Conclusions}
\label{chapConclusions}

In this thesis, we described a new approach to the design and
implementation of noncausal modelling and simulation languages. From the
language design point of view, the key idea was to embed equational
models as first-class entities into a functional programming language.
We provided a range of examples demonstrating how the notion of
first-class models can be used for higher-order and (unbounded)
structurally dynamic modelling, and thus going beyond to what is
expressible in current noncausal modelling languages. From the language
implementation point of view, the key idea was to enable efficient
simulation of noncausal models that are generated at simulation runtime
by runtime symbolic processing and just-in-time compilation. We defined
a formal semantics for the language developed in this thesis and
provided an in-depth description of its implementation. We hope that
this work will facilitate adoption of the aforementioned approaches by
designers and implementers of modelling and simulation languages.

Throughout the thesis we have identified a number of directions for
future work. Let us conclude the thesis by consolidating these
directions in the list given below.

\begin{itemize}

\item Introduce the notion of first-class models in mainstream noncausal
modelling languages such as Modelica. This would allow for improved
higher-order and structurally dynamic modelling capabilities, as
demonstrated in this thesis. Sol \citep{Zimmer2008a}, which is a
Modelica-like language, already supports language constructs for dynamic
addition and removal of equational constraints. Coupling the Modelica
language extensions suggested by Zimmer with the just-in-time
compilation techniques described in this thesis would be a good starting
point for extending Modelica and its implementations.

In addition to enabling the introduction of the aforementioned new
language constructs to Modelica, the just-in-time compilation techniques
described in this thesis allow for some restrictions on existing
language constructs to be lifted. Such restrictions include the
requirements on conditional equations described in
Section~\ref{secHybridModelling}. Lifting the restrictions on
conditional equations would enable bounded structurally dynamic
modelling. The new language constructs proposed by \citet{Zimmer2008a}
would still be needed for unbounded structurally dynamic modelling.
\citet{Zimmer2007} gives a list of restrictions on Modelica language
constructs that can be lifted with a more dynamic treatment of
equational constraints. Devising of a comprehensive list of such
restrictions is a subject of future work.

Although the notion of first-class models and the aforementioned
just-in-time compilation techniques would benefit Modelica, full
realisation of Hydra's two-level design, which extends a purely
functional programming language with noncausal modelling capabilities,
in the Modelica setting is difficult. This is because Modelica's syntax
and semantics are deeply rooted in the object-oriented paradigm.
Integration of Hydra's design into Modelica is an instance of a more
general problem of integration of purely functional and object-oriented
programming paradigms. This is something that we have not yet
considered.

\item Make use of the ideal semantics for verification of simulation
results. In particular, based on the ideal semantics, it should be
possible to develop a tool for automatic verification of simulation
results.

\item Investigate properties of the ideal semantics developed in this
thesis and apply them to the problem of verification of the language
implementation. Although challenging, it would be interesting to
investigate possibilities of producing formally verified symbolic
processors and numerical simulators for noncausal languages like Hydra.

\item Develop symbolic methods for reducing mode switching overheads,
especially those overheads that are associated to just-in-time
compilation. Merging of Hydra's implementation approach to that of Sol
would be a good starting point.

\item Entirely avoid recompilation for discrete changes that are not
structural changes. Introduction of the notion of impulses in Hydra
would be a good starting point.

\item Combine FHM and FRP frameworks in a single coherent language. The
first step into this direction would be to introduce support for
stateful signal functions in Hydra.

\end{itemize}