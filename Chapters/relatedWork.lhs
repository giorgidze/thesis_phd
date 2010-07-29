\chapter{Related Work}
\label{chapRelatedwork}

\section{Embedded Domain Specific Languages}

The deep embedding techniques used in the Hydra implementation for
domain-specific optimisations and efficient code generation draws from the
extensive work on compiling staged domain-specific embedded languages. 
Examples include Elliott et al. \cite{Elliott2000} and Mainland et al.
\cite{Mainland2008}. However, these works are concerned with compiling
programs all at once, meaning the host language is used only for
meta-programming, not for running the actual programs.

The use of quasiquoting in the implementation of Hydra was inspired by
Flask, a domain-specific embedded language for programming sensor networks
\cite{Mainland2008}. However, we had to use a different approach to type
checking. A Flask program is type checked by a domain-specific type checker
\emph{after} being generated, just before the subsequent compilation into the
code to be deployed on the sensor network nodes. This happens at \emph{host
language run-time}. Because Hydra is iteratively staged, we cannot use this
approach: we need to move type checking back to \emph{host language
compile-time}. The Hydra implementation thus translates embedded programs into
typed combinators at the stage of quasiquoting, charging the host language
type checker with checking the embedded terms. This ensures only well-typed
programs are generated at run-time.

Lee et al. are developing a DSL embedded in Haskell for data-parallel array
computations on a graphics processing unit (GPU) \cite{Lee2009a}. GPU programs
are first-class entities. The embedded language is being designed for run-time
code generation, compilation and execution, with results being fed back for
use in further host language computations. Thus, this is another example of
what we term iterative staging. At the time of writing, the implementation is
interpreted. However, a JIT compiler for a GPU architecture is currently being
developed.

The FHM design was originally inspired by Functional Reactive Programming
(FRP) \cite{Elliott1997}, particularly Yampa \cite{Nilsson2002a}. A key
difference is that FRP provides \emph{functions} on signals whereas FHM
generalises this to \emph{relations} on signals. FRP can thus be seen as a
framework for \emph{causal} simulation, while FHM supports non-causal
simulation. Signal functions are first class entities in most incarnations of
FRP, and new ones can be computed and integrated into a running system
dynamically.  As we have seen, this capability has also been carried over to
FHM.  This means that these FRP versions, including Yampa, also are examples
of iteratively staged languages. However, as all FRP versions supporting
highly dynamic system structure so far have been interpreted, the program
generation aspect is much less pronounced than what is the case for FHM.  That
said, in Yampa, program fragments are generated and then optimised dynamically
\cite{Nilsson2005a}. It would be interesting to try to apply an implementation
approach like the one we have described here to a version of FRP.

\section{Non-causal Modelling and Simulation Languages}
\subsection{Sol}

Sol is a Modelica-like language \cite{Zimmer2007,Zimmer2008a}. It introduces
language constructs that enable the description of systems where objects are
dynamically created and deleted, thus aiming at supporting modelling of
highly structurally dynamic systems. So far, the research emphasis has been on
the design of the language itself along with support for incremental dynamic
recausalisation and dynamic handling of structural singularities. An
interpreter is used for simulation. The work on Sol is thus complementary to
ours: techniques for dynamic compilation would be of interest in the context
of Sol to enable it to target high-end simulation tasks; conversely,
algorithms for incremental recausalisation is of interest to us to minimise
the amount of work needed to regenerate simulation code after structural
changes (see Section \ref{sec:futurework}).

\subsection{MOSILAB}

MOSILAB is an extension of the Modelica language that supports the description
of structural changes using object-oriented statecharts
\cite{Nytsch-Geusen2005a}. This enables modelling of structurally dynamic
systems. It is a compiled implementation. However, the statechart approach
implies that all structural modes must be explicitly specified in advance,
meaning that MOSILAB does not support \emph{highly} structurally dynamic
systems. Even so, if the number of possible configurations is large (perhaps
generated mechanically by meta-modelling), techniques like those we have
investigated here might be of interest also in the implementation of MOSILAB.

\subsection{Modelling Kernel Language}

Broman \cite{Broman2007a,Broman2008a} is developing the Modelling Kernel
Language (MKL) that is intended to be a core language for non-causal modelling
languages such as Modelica. Broman takes a functional approach to non-causal
modelling, similar to the FHM approach \cite{Nilsson2003a,Nilsson2007}. One of
the main goals of MKL is to provide a formal semantics of the core language.
Currently, this semantics is based on an untyped, effectful
$\lambda$-calculus.

Similarly to Hydra, MKL provides a $\lambda$-abstraction for defining
functions and an abstraction similar to |sigrel| for defining non-causal
models. Both functions and non-causal models are first-class entities in MKL,
enabling higher-order, non-causal modelling. The similarity of the basic
abstractions in Hydra and MKL leads to a similar style of modelling in both
languages.

Thus far, the work on MKL has not specifically considered support
for structural dynamics, meaning that its expressive power in that respect
is similar to current main-stream, non-causal modelling and simulation
languages like Modelica. However, given the similarities between MKL and
FHM/Hydra, MKL should be a good setting for exploring support for
structural dynamics, which ultimately could carry over to better support
for structural dynamics for any higher-level language that has a semantics
defined by translation into MKL. Again, the implementation techniques
discussed in this paper should be of interest in such a setting.



