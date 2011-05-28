\chapter{Related Work}
\label{chapRelatedWork}

\section{Embedded Domain Specific Languages}

The deep-embedding techniques used in the Hydra implementation for
domain-specific optimisations and efficient code generation draws from the
extensive work on compiling staged domain-specific embedded languages.
Examples include \citet{Elliott2000} and \citet{Mainland2008}. However, these
works are concerned with compiling programs all at once, meaning the host
language is used only for meta-programming, not for running the actual
programs.

The use of quasiquoting in the implementation of Hydra draws its inspiration
from Flask, a domain-specific embedded language for programming sensor
networks \citep{Mainland2008}. However, we had to use a different approach to
type checking. A Flask program is type checked by a domain-specific type
checker after being generated, just before the subsequent compilation into the
code to be deployed on the sensor network nodes. This happens at host-language
run-time. Because Hydra is iteratively staged, we cannot use this approach: we
need to move type checking back to host-language compile-time. The Hydra
implementation thus translates embedded programs into typed combinators at the
stage of quasiquoting, charging the host-language type checker with checking
the embedded terms. This ensures only well-typed programs are generated at
run-time.

The FHM design was originally inspired by Functional Reactive Programming
(FRP) \citep{Elliott1997}, particularly Yampa \citep{Nilsson2002a}. A key
difference is that FRP provides functions on signals whereas FHM generalises
this to relations on signals. FRP can thus be seen as a framework for causal
simulation, while FHM supports noncausal simulation. Signal functions are
first class entities in most incarnations of FRP, and new ones can be computed
and integrated into a running system dynamically. As we have seen, this
capability has also been carried over to FHM. This means that these FRP
versions, including Yampa, also are examples of iteratively staged languages.
However, as all FRP versions supporting highly dynamic system structure so far
have been interpreted, the program generation aspect is much less pronounced
than what is the case for Hydra. That said, in Yampa, program fragments are
generated and then optimised dynamically \citep{Nilsson2005a}. It would be
interesting to try to apply the implementation approaches described in this
thesis (i.e., runtime symbolic processing and JIT compilation) to a version of
FRP, especially in the context of the recently proposed optimisations by
\citet{Liu2009a} and \citet{Sculthorpe2011a}.

\section{Noncausal Modelling and Simulation Languages}
\subsection{Sol}

Sol is a Modelica-like language \citep{Zimmer2007,Zimmer2008a}. It introduces
language constructs that enable the description of systems where objects are
dynamically created and deleted, thus aiming at supporting modelling of highly
structurally dynamic systems. So far, the research emphasis has been on the
design of the language itself along with support for incremental dynamic
recausalisation and dynamic handling of structural singularities. An
interpreter is used for simulation. The work on Sol is thus complementary to
ours: techniques for dynamic compilation would be of interest in the context
of Sol to enable it to target high-end simulation tasks; conversely,
algorithms for incremental recausalisation is of interest to us to minimise
the amount of work needed to regenerate simulation code after structural
changes.

\subsection{MOSILAB}

MOSILAB is an extension of the Modelica language that supports the description
of structural changes using object-oriented statecharts
\citep{Nytsch-Geusen2005a}. This enables modelling of structurally dynamic
systems. The language extension has a compiled implementation. However, the
statechart approach implies that all structural modes must be explicitly
specified in advance, meaning that MOSILAB does not support highly
structurally dynamic systems. Even so, if the number of possible
configurations is large (perhaps generated mechanically by meta-modelling),
higher-order and structurally dynamic modelling techniques and their
implementations investigated here might be of interest also in the
implementation of MOSILAB.

\subsection{Modelling Kernel Language}

Broman \citep{Broman2007a,Broman2008a} developed Modelling Kernel Language
(MKL). The language is intended to be a core language for noncausal modelling
languages such as Modelica. Broman takes a functional approach to noncausal
modelling, similar to the FHM approach proposed by \citet{Nilsson2003a}. One
of the main goals of MKL is to provide a formal semantics of the core
language. The semantics is based on an untyped, effectful $\lambda$-calculus.

Similarly to Hydra, MKL provides a $\lambda$-abstraction for defining
functions and an abstraction similar to |rel| for defining noncausal
models. Both functions and noncausal models are first-class entities in MKL,
enabling higher-order, noncausal modelling. The similarity of the basic
abstractions in Hydra and MKL leads to a similar style of modelling in both
languages.

Thus far, the work on MKL has not specifically considered support for
structural dynamics, meaning that its expressive power in that respect is
similar to current main-stream, noncausal modelling and simulation languages
like Modelica. However, given the similarities between MKL and Hydra, MKL
should be a good setting for exploring support for structural dynamics, which
ultimately could carry over to better support for structural dynamics for any
higher-level language that has a semantics defined by translation into MKL.
Again, the implementation techniques discussed in this paper should be of
interest in such a setting.

\subsection{Acumen}

\citep{Taha2010a}.

