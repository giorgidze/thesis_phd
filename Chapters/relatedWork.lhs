\chapter{Related Work}
\label{chapRelatedWork}

\section{Embedded Domain Specific Languages}

The deep-embedding techniques used in the Hydra implementation for
domain-specific optimisations and efficient code generation draws from the
extensive work on compiling embedded, staged DSLs. Examples include
\citet{Elliott2000} and \citet{Mainland2008}. However, these works are
concerned with compiling programs all at once, meaning the host language is
used only for meta-programming, not for running the actual programs. Hydra
combines the aforementioned deep-embedding techniques with shallow embedding
techniques in order to allow the host language to participate in runtime
generation, optimisation, compilation and execution of embedded programs.

The use of quasiquoting in the implementation of Hydra draws its inspiration
from Flask, a domain-specific embedded language for programming sensor
networks \citep{Mainland2008}. However, we had to use a different approach to
type checking. A Flask program is type checked by a domain-specific type
checker after being generated, just before the subsequent compilation into the
code to be deployed on the sensor network nodes. This happens at host-language
runtime. Because Hydra is iteratively staged, we cannot use this approach: we
need to move type checking back to host-language compile-time. The Hydra
implementation thus translates embedded programs into typed combinators at the
stage of quasiquoting, charging the host-language type checker with checking
the embedded terms. This also ensures that only well-typed programs are
generated at runtime.

As discussed in Chapter~\ref{chapIntroduction}, many language features of
Hydra follow closely those proposed by \cite{Nilsson2003a}, in the context of
the FHM framework. The FHM framework itself was originally inspired by
Functional Reactive Programming (FRP) \citep{Elliott1997}, particularly Yampa
\citep{Nilsson2002a}. A key difference between FHM and FRP is that FRP
provides functions on signals whereas FHM generalises this to relations on
signals. FRP can thus be seen as a framework for causal modelling, while FHM
is a framework for noncausal modelling. Signal functions are first class
entities in most incarnations of FRP, and new ones can be computed and
integrated into a running system dynamically. As we have seen, this capability
has also been carried over to Hydra. This means that these FRP versions,
including Yampa, are also examples of iteratively staged languages. However,
as all FRP versions supporting unbounded structural dynamism so far have been
interpreted, the program generation aspect is much less pronounced than what
is the case for Hydra. That said, in Yampa, program fragments are generated
and then optimised dynamically \citep{Nilsson2005a}. It would be interesting
to try to apply the implementation approaches described in this thesis (i.e.,
runtime symbolic processing and JIT compilation) to a version of FRP,
especially in the context of the recently proposed optimisations by
\citet{Liu2009a} and \citet{Sculthorpe2011a}.

\section{Noncausal Modelling and Simulation Languages}

\subsection{Modelling Kernel Language}

Broman \citep{Broman2007a,Broman2008a} developed Modelling Kernel Language
(MKL). The language is intended to be a core language for noncausal modelling
languages such as Modelica. Broman takes a functional approach to noncausal
modelling, similar to the FHM approach proposed by \citet{Nilsson2003a}.

MKL is based on an untyped, effectful $\lambda$-calculus. The effectful part
of the underling $\lambda$-calculus is used for specification of noncausal
connections. Similarly to Hydra, MKL provides a $\lambda$-abstraction for
defining functions and an abstraction similar to |rel| for defining noncausal
models. Both functions and noncausal models are first-class entities in MKL,
enabling higher-order, noncausal modelling. The similarity of the basic
abstractions in Hydra and MKL leads to a similar style of modelling in both
languages.

The work on MKL has not considered support for structural dynamics, meaning
that its expressive power in that respect is similar to current mainstream,
noncausal modelling and simulation languages like Modelica. However, given the
similarities between MKL and Hydra, MKL should be a good setting for exploring
support for structural dynamics, which ultimately could carry over to better
support for structural dynamics for any higher-level language that has a
semantics defined by translation into MKL. The language design and
implementation approaches (especially those related to structural dynamism)
discussed in this paper should be of interest in the MKL setting.

\subsection{Sol}

Sol is a Modelica-like language \citep{Zimmer2007,Zimmer2008a}. It introduces
language constructs that enable the description of systems where objects are
dynamically created and deleted, thus supporting modelling of unbounded
structurally dynamic systems. The work on Sol is complementary to ours in a
number of respects outlined in the following.

Sol explores how structurally dynamic systems can be modelled in an
object-oriented, noncausal language. Hydra extends a purely functional
programming language with constructs for structurally dynamic, noncausal
modelling.

The implementation of Sol makes use of symbolic methods that for each
structural change aim to identify the smallest number of equations that need
to be modified or added in order to model the structural change. It would be
interesting to combine these symbolic methods with the runtime code generation
approach used in Hydra in order to reduce the JIT compilation overheads by
only compiling the modified and added equations for each structural change.

Sol features only an interpreted implementation. The dynamic compilation
techniques featured in the implementation of Hydra would be of interest in the
context of Sol to enable it to target high-end simulation tasks.

\subsection{MOSILAB}

MOSILAB is an extension of the Modelica language that supports the description
of structural changes using object-oriented statecharts
\citep{Nytsch-Geusen2005a}. This enables modelling of structurally dynamic
systems. The language extension has a compiled implementation. However, the
statechart approach implies that all structural modes must be explicitly
specified in advance, meaning that MOSILAB does not support unbounded
structural dynamism. Even so, if the number of possible configurations is
large (perhaps generated mechanically by meta-modelling), higher-order and
structurally dynamic modelling techniques and their implementations
investigated here might be of interest also in the implementation of MOSILAB.

\subsection{Acumen}

Acumen is a language for modelling and simulation of cyber-physical systems
\citep{Taha2010a}. In Acumen, a digital component can be modelled using a
variant of FRP called Event-driven FRP \citep{Wan2002a}, while a continuous
component can be modelled using a combination of DAEs and partial differential
equations (PDEs). The implementation of Acumen features advanced symbolic
processing methods that reduce a combination of DAEs and PDEs to the
corresponding system of ODEs whenever possible. Acumen supports bounded
structural dynamism, but unbounded structural dynamism is not supported.

The symbolic processing methods developed for Acumen and its tight integration
with an FRP variant would benefit Hydra, while Hydra's support for unbounded
structural dynamism would benefit Acumen. Such a combination is feasible.
Currently, the development of a new version of Acumen is underway\footnote{The
development is being documented on the \url{www.acumen-language.org}
website.}. This new version aims to support modelling and simulation of
unbounded structurally dynamic systems. New language constructs for dynamic
addition and removal of equational constraints at discrete points in time have
already been introduced (see the work-in-progress report by \citet{Taha2011a}
for details). In this new version of Acumen simulation is carried out through
interpretation. Because Acumen models are converted into flat list of
differential equations, it is feasible to integrate the JIT compilation
approach described in this thesis in the implementation of Acumen.

\section{Semantics}

Proposed by \citet{Henzinger1996a}, a hybrid automaton is a formal model for a
hybrid system. The formalism allows a hybrid system to be specified in terms
of a finite set of continuous, time-varying variables and a graph with DAEs
constraining the variables at the graph nodes and switching conditions at the
graph edges. Noncausal, hybrid languages can be given semantics by translation
into the formalism. The modelling and simulation language Chi
\citep{Beek2008a} takes this approach. Because, a hybrid automaton can only
describe a bounded structurally dynamic system and does not allow new
equations to be computed at switches (i.e., does not feature equational
constrains as first-class entities) we did not use the hybrid automata as a
target formalism when defining the ideal semantics of Hydra.

A formal semantics for the MKL language was defined by \citet{Broman2007a}. A
(higher-order) model is given semantics by translation into a flat system of
equations. The support for structural dynamism and its formal specification
has not been considered in the setting of MKL.

\citet{Wan2000} define an ideal semantics for a simple FRP language. In
addition to the ideal semantics, they provide an operational semantics that
makes use of discrete sampling. They show that the operational semantics
converges to the ideal semantics when the discrete sampling rate tends to
zero. Applying a similar approach to an FHM language implementation, in order
to prove its correctness, is a subject of future work.