\chapter{Introduction}
\label{chapIntroduction}

The field of physical modelling and simulation plays a vital role in the
design, implementation and analysis of systems in numerous areas of science
and engineering. Examples include electronics, mechanics, thermodynamics,
chemical reaction kinetics, population dynamics and neural networks
\citep{Cellier1991}. To cope with the increasing size and complexity of
physical models, a number of modelling and simulation languages have been
developed. The modelling and simulation languages can be divided into two
broad categories: \emph{causal} and \emph{noncausal}.

A causal model is formulated in terms of \emph{explicit} equations, for
example, \emph{ordinary differential equations} (ODEs) in explicit form; that
is, the cause-effect relationship is explicitly specified by the modeller
\citep{Cellier2006}. In other words, the equations are directed: only
\emph{unknown} variables can appear on the left hand side of the equals sign,
and only \emph{known} variables on the other side. Since the equations are
directed, it is relatively straightforward to translate a causal model into
low-level simulation code (e.g., into a sequence of assignment statements) and
simulate it. Simulink is a prominent representative of causal modelling
languages \citep[2008]{Simulink}.

A noncausal model is formulated in terms of \emph{implicit} equations, for
example, \emph{differential algebraic equations} (DAEs) in implicit form. In
other words, the equations are undirected: both known and unknown variables
may appear on both sides of the equals sign \citep{Cellier2006}. The
translation of noncausal models into simulation code involves additional
symbolic processing and numerical simulation methods that are not required for
causal modelling and simulation. Examples include symbolic transformations
that try to causalise noncausal models and, if this is not possible, numerical
solvers for (nonlinear) implicit equations. Modelica is a prominent,
state-of-the-art representative of noncausal modelling languages
\citep[2010]{Modelica}.

Noncausal modelling has a number of advantages over causal modelling. The most
important ones are outlined in the following list.

\begin{itemize}

\item In many physical domains models are more naturally represented using
noncausal equations, and in some physical domains models cannot be represented
using only causal equations \citep{Cellier1991,Brenan1996a}.

\item Noncausal languages are declarative and approach modelling problems from
a higher level of abstraction by focusing on \emph{what} to model rather than
\emph{how} to model to enable simulation
\citep{Cellier1991,Cellier1996,Nilsson2003a}.

\item Noncausal models are more reusable, as equations can be used in a number
of different ways depending on their context of usage (i.e., effectively
causalised in a number of different ways)
\citep{Cellier1991,Cellier1996,Cellier2006}.

\end{itemize}

Although causal modelling remains a dominant paradigm, interest in noncausal
modelling has grown recently as evidenced by the wide adoption of the Modelica
language both in industry and academia, and by release of noncausal modelling
and simulation tools by prominent vendors such as Maple (MapleSim) and
MathWorks (Simscape).


\section{First-class Models}
\label{secFirstClassModels}

A language entity is \emph{first-class} if it can be (1) passed as a parameter
to functions, (2) returned as a result from functions, (3) constructed at
runtime and (4) placed in data structures \citep{Scott2009a}. To my knowledge,
this notion was first introduced by Christopher Strachey \citep{Burstall2000a}
in the context of functions being first-class values in higher-order,
functional programming languages. Current, mainstream noncausal languages do
not treat models as first-class values \citep{Nilsson2003a}. This limits their
expressiveness for \emph{higher-order} and \emph{structurally dynamic}
modelling.

\subsection{Higher-order Modelling}

Higher-order modelling allows parametrisation of models on other models
\citep{Nilsson2003a}. For instance, a car model can be parametrised on the
list of tyres it is using, and an electrical transmission line model can be
parametrised on the list of electrical components on the line. Mainstream,
noncausal languages provide limited support for this style of modelling. Tool
specific and external scripting languages are often used to generate noncausal
models for particular instances of higher-order models \citep{Broman2008a}.
This is practical for some applications, but the aforementioned advantages of
noncausal languages can be better realised with a coherent language supporting
noncausal as well as higher-order modelling.

This thesis formally defines a language that supports higher-order modelling
by treating noncausal models as first-class values in a purely functional
programming language and describes its implementation. In this setting, a
function from model (or from collections of models placed in a suitable data
structure) to model can be seen as a higher-order model and an application of
this function can be seen as an instantiation of the higher-order model.

The idea of treating noncausal models as first-class values in a functional
programming language is introduced by \citet{Nilsson2003a} in the context of a
framework called Functional Hybrid Modelling (FHM) for designing and
implementing noncausal modelling languages. However, the paper postpones the
concrete language definition and implementation for future work. In addition,
the FHM framework proposes to exploit the first-class nature of noncausal
models for modelling \emph{hybrid} systems (i.e., systems that exhibit both
continuous and discrete behaviour); this is relevant in the following section.

\citet{Broman2007a} defined and implemented a noncausal language that supports
parametrisation of models on other models and allows for a form of
higher-order modelling. However, construction of noncausal models at
simulation runtime and manipulation of collections of models placed in data
structures were not considered.

\subsection{Structurally Dynamic Modelling}

Major changes in system behaviour are often modelled by changing the equations
that describe the system \citep{Mosterman1997}. A model where the equational
description changes over time is called \emph{structurally dynamic}. Each
structural configuration of the model is known as a \emph{mode} of operation.
\citet{Cellier2006} refer to structurally dynamic systems as
\emph{variable-structure} systems. Structurally dynamic systems are an example
of the more general notion of hybrid systems \citep{Nilsson2003a}. The term
structurally dynamic emphasises only one discrete aspect, that is, the change
of equations at discrete points in time.

\emph{Cyber-physical} systems \citep{Lee2008a}, where digital computers
interact with continuous physical systems, can also be seen as instances of
hybrid systems. In this context, structurally dynamic modelling is relevant;
as modelling of a cyber-physical system where the digital part's influence
causes major changes in the physical part may require changing the equations
that describe the behaviour of the continuous part. Recently, the US National
Science Foundation identified cyber-physical systems as one of its key
research areas \citep[2008]{NSF2008a}.

Currently, noncausal languages offer limited support for modelling
structurally dynamic systems \citep{Mosterman1997, Mosterman1999a, Zauner2007,
Zimmer2008a}. There are a number of reasons for this. However, this thesis
concentrates on one particular reason related to the design and implementation
of modelling and simulation languages: the prevalent assumption that most or
all processing to put a model into a form suitable for simulation will take
place \emph{prior} to simulation \citep{Nilsson2007,Zimmer2007}. By enforcing
this assumption in the design of a modelling language, its implementation can
be simplified as there is no need for simulation-time support for handling
structural changes. For instance, a compiler can typically generate static
simulation code (often just a sequence of assignment statements) with little
or no need for dynamic memory or code management. This results in good
performance, but such language design and implementation approaches restrict
the number of modes to be modest as, in general, separate code must be
generated for each mode. This rules out supporting structurally dynamic
systems where the number of modes is a priori unbounded. We refer to this kind
of system as \emph{unbounded structurally dynamic}. Systems with a priori
bounded number of modes are referred as \emph{bounded structurally dynamic}.

There are a number of efforts to design and implement modelling and simulation
languages with improved support for structural dynamism. Examples include:
HYBRSIM \citep{Mosterman1998}, MOSILAB \citep{Nytsch-Geusen2005a}, Sol
\citep{Zimmer2008a} and Acumen \citep{Taha2010a}. However, thus far,
implementations have either been interpreted (HYBRSIM and Sol) and thus
sacrificing efficiency, or the languages have been restricted so as to limit
the number of modes to make it feasible to compile code for all modes prior to
simulation (MOSILAB and Acumen).

\section{Contributions to the Field of Noncausal Modelling and Simulation}

This dissertation presents a novel approach to the design and implementation
of noncausal modelling and simulation languages with first-class models
supporting higher-order and structurally dynamic modelling. The thesis
formally defines a noncausal modelling language called Hydra and describes its
implementation in detail. Hydra provides noncausal modelling and simulation
capabilities that go beyond the state of the art and represents significant
progress in the field of design and implementation of declarative modelling
and simulation languages. The following list summarises the contributions to
the field of noncausal modelling and simulation.

\begin{itemize}

\item The thesis shows how to enable higher-order modelling capabilities by
embedding noncausal models as first-class entities into a purely functional
programming language. To my knowledge, Hydra is the first noncausal language
that faithfully treats equational models as first-class values (i.e., supports
all four points outlined in the beginning of
Section~\ref{secFirstClassModels}). See Section~\ref{secModelica},
Section~\ref{secHybridModelling}, Chapter~\ref{chapConcepts} and
Chapter~\ref{chapHydra} for details.

\item The thesis shows how to use runtime symbolic processing and
\emph{just-in-time} (JIT) compilation to enable efficient simulation of
noncausal models that are generated at simulation runtime. To my knowledge,
Hydra is the first noncausal language that enables support \emph{both} for
modelling and simulation of unbounded structurally dynamic systems and for
compilation of simulation code for efficiency. See
Chapter~\ref{chapImplementation} for details.

\item The thesis formally defines the Hydra language. To my knowledge, Hydra
is the first noncausal language that features a formal specification capturing
both continuous and discrete aspects of unbounded structural dynamism. See
Chapter~\ref{chapDefinition} for details.

\end{itemize}

In addition to presenting the language definition and implementation, the
aforementioned claims are also backed up by a range of example physical
systems that cannot be modelled and simulated in current, noncausal languages.
The examples are carefully chosen to showcase those language features of Hydra
that are lacking in other noncausal modelling languages.

The language design choices and implementation approaches presented here can
be used to enhance existing noncausal modelling and simulation languages, as
well as to design and implement new modelling languages. This thesis provides
a self-contained reference for such an undertaking by defining the language
semantics formally and providing an in-depth description of the
implementation.

Many language features of Hydra follow closely those proposed by
\cite{Nilsson2003a} in the context of the FHM framework and can be seen as the
first concrete language definition and implementation that is based on the FHM
framework. However, as already mentioned, at this stage Hydra supports only
one aspect of hybrid modelling, namely, structural dynamism. Other discrete
aspects that do not lead to structural reconfigurations (e.g., \emph{impulses}
\citep{Nilsson2003a,Nilsson2003b}) are not considered in this thesis, but, in
principle, can be incorporated in the Hydra language.

This work can be seen as an application of successful ideas developed in
functional programming languages research to declarative modelling and
simulation languages. I hope that this work will aid to further
cross-fertilisation and the exchange of ideas between these research
communities.

\section{Embedding}
\label{secEmbedding}

Hydra is a Haskell-embedded \emph{domain-specific language} (DSL). Here, the
domain is noncausal modelling and simulation using implicitly formulated DAEs.
Haskell is a purely functional, higher-order, statically typed programming
language \citep{Haskell98}, which is widely used for embedded DSL development
\citep{Stewart2009a}.

Embedding is a powerful and popular way to implement DSLs \citep{Hudak1998}.
Compared with implementing a language from scratch, extending a suitable
general-purpose programming language, the \emph{host language}, with notions
addressing a particular application or problem domain tends to save a lot of
design and implementation effort. The motivation behind using an embedding
approach for Hydra is to concentrate the language design and implementation
effort on noncausal modelling notions that are domain specific and absent in
the host language, and to reuse the rest from the host language.

Having said that, the concept of first-class models, and the runtime symbolic
processing and JIT compilation approaches implemented in Hydra, are not
predicated on embedded implementation. These language design and
implementation approaches can be used in other noncausal modelling languages,
embedded or otherwise.

There are two basic approaches to language embeddings: \emph{shallow} and
\emph{deep}. In a shallow embedding, domain-specific notions are expressed
directly in host-language terms. A shallow embedding is commonly realised as a
higher-order combinator library. This is a light-weight approach for
leveraging the facilities of the host language \citep{Hudak1998}. In contrast,
a deep embedding is about building embedded language terms as data in a
suitable representation. These terms are given meaning by interpretation or
compilation \citep{Hudak1998}. This is a more heavy-weight approach, but also
more flexible one. Indeed, it is often necessary to inspect the embedded
language terms for optimisation or compilation. To benefit from the advantages
of both shallow and deep embeddings, a combined approach called
\emph{mixed-level} embedding can be used \citep{Giorgidze2010a}. The
aforementioned embedding approaches are not specific to Haskell. They can be
realised in other higher-order programming languages (e.g., languages in ML
and Lisp families).

As mentioned in Section~\ref{secFirstClassModels}, Hydra supports runtime
generation and JIT compilation of noncausal models. Specifically, in response
to \emph{events} occurring at discrete points in time, the simulation is
stopped and, depending on the simulation results thus far, new equations are
generated for further simulation \citep{Giorgidze2009a}. In this thesis and in
\citet{Giorgidze2010a} this kind of DSLs are referred to as \emph{iteratively
staged}, emphasising that the domain is characterised by repeated program
generation, compilation and execution. An iteratively-staged language is a
special kind of a \emph{multi-staged} language \citep{Taha2004} with the
aforementioned characteristics.

Because performance is a primary concern in the domain, the numerical
simulation code for each mode of the model has to be compiled. As this code is
determined dynamically, this necessitates JIT compilation. For the numerical
part of the language Hydra employs deep embedding techniques, along with the
Low Level Virtual Machine (LLVM) compiler infrastructure \citep{Lattner2002a},
a language-independent, portable, optimising, compiler backend with JIT
support. In contrast, shallow embedding techniques are used for the parts of
Hydra that are concerned with high-level, symbolic computations
\citep{Giorgidze2010a}.

An alternative might have been to use a multi-staged host language like
MetaOCaml \citep{Taha2004}. The built-in runtime code generation capabilities
of the host language then would have been used instead of relying on an
external code generation framework such as LLVM. This approach has not been
pursued, as tight control over the dynamically generated numerical code is
essential in this application domain.

\section{Contributions to the Field of DSL Embedding}

Compilation of embedded DSLs is today a standard tool in the DSL-implementer's
tool box. The seminal example is the work by Elliott et al. on compiling
embedded languages, specifically the image synthesis and manipulation language
Pan \citep{Elliott2000}. Pan, like Hydra, provides for program generation by
leveraging the host language combined with compilation to speed up the
resulting performance-critical computations. However, the program to be
compiled is generated once and for all, meaning the host language acts as a
powerful, but fundamentally conventional macro language: program generation,
compilation, and execution is a process with a fixed number of stages.

Hydra is iteratively staged and the host language is part of the dynamic
semantics of the embedded language through the shallow parts of the embedding
(instead of acting merely as a meta language that is out of the picture once
the generated program is ready for execution). We thus add further tools to
the DSL tool box for embedding a class of languages that thus far has not been
studied much from an embedding and staged programming perspective.

While embedded DSL development methodology is not the main focus of this work,
I nevertheless think that the thesis should be of interest to embedded DSL
implementers, as it presents an application of a new embedding technique. The
following list summarises the contributions to the field of DSL embedding.

\begin{itemize}

\item The thesis presents a case study of mixed-level embedding of an
iteratively staged DSL in a host language that does not provide built-in
multi-stage programming capabilities. See Chapter~\ref{chapConcepts} and
Chapter~\ref{chapHydra} for details.

\item The thesis describes how to use JIT compilation to implement an
iteratively staged embedded DSL efficiently. See
Chapter~\ref{chapImplementation} for details.

\end{itemize}

\section{Overview of Peer-reviewed Publications}

The content of this thesis is partly based on the peer-reviewed publications
that are listed in this section. I wrote the papers in collaboration with my
coauthors. This thesis was written by myself and presents my own
contributions. I have implemented the Hydra language described in this
dissertation and in the following papers. The software is available on my
webpage\footnote{\url{http://www.cs.nott.ac.uk/~ggg/}} under the open source
BSD3 license.

The following four papers describe various aspects of the design and
implementation of Hydra, as well as a number of its applications.

\begin{itemize}

\item
George Giorgidze and Henrik Nilsson.
\newblock Embedding a {F}unctional {H}ybrid {M}odelling language in {H}askell.
\newblock In {\em Revised selected papers of the 20th international symposium
  on Implementation and Application of Functional Languages, Hatfield,
  England}, volume 5836 of {\em Lecture Notes in Computer Science}. Springer,
  2008.

\item
George Giorgidze and Henrik Nilsson.
\newblock Higher-order non-causal modelling and simulation of structurally
  dynamic systems.
\newblock In {\em Proceedings of the 7th International Modelica Conference,
  Como, Italy}. Link{\"o}ping University Electronic Press, 2009.

\item
George Giorgidze and Henrik Nilsson.
\newblock Mixed-level embedding and {JIT} compilation for an iteratively staged
  {DSL}.
\newblock In {\em Revised selected papers of the 19th international workshop on
  Functional and (Constraint) Logic Programming, Madrid, Spain}, volume 6559 of
  {\em Lecture Notes in Computer Science}. Springer, 2010.

\item
Henrik Nilsson and George Giorgidze.
\newblock Exploiting structural dynamism in {F}unctional {H}ybrid {M}odelling
  for simulation of ideal diodes.
\newblock In {\em Proceedings of the 7th EUROSIM Congress on Modelling and
  Simulation, Prague, Czech Republic}. Czech Technical University Publishing
  House, 2010.

\end{itemize}

The following two papers are about unbounded structurally dynamic, causal
modelling and simulation using Yampa, a Haskell-embedded Functional Reactive
Programming (FRP) language \citep{Hudak2003}. The combinator that allows
switching of equations during simulation runtime in Hydra draws its
inspiration from switching combinators featured in Yampa
\citep{Nilsson2002a,Courtney2003a}.

\begin{itemize}

\item
George Giorgidze and Henrik Nilsson.
\newblock Demo outline: {S}witched-on {Y}ampa.
\newblock In {\em Proceedings of the ACM SIGPLAN Haskell workshop, Freiburg,
  Germany}. ACM, 2007.

\item
George Giorgidze and Henrik Nilsson.
\newblock Switched-on {Y}ampa: declarative programming of modular synthesizers.
\newblock In {\em Proceedings of the 10th international symposium on Practical
  Aspects of Declarative Languages, San Francisco, CA, USA}, volume 4902 of
  {\em Lecture Notes in Computer Science}. Springer, 2008.

\end{itemize}

Some of the embedding techniques described in this thesis are also used in the
following two papers.

\begin{itemize}

\item
George Giorgidze, Torsten Grust, Tom Schreiber, and Jeroen Weijers.
\newblock {H}askell boards the {F}erry: Database-supported program execution
  for {H}askell.
\newblock In {\em Revised selected papers of the 22nd international symposium
  on Implementation and Application of Functional Languages, Alphen aan den
  Rijn, Netherlands}, volume 6647 of {\em Lecture Notes in Computer Science}.
  Springer, 2010.
\newblock Peter Landin Prize for the best paper at IFL 2010.

\item
George Giorgidze, Torsten Grust, Nils Schweinsberg, and Jeroen Weijers.
\newblock Bringing back monad comprehensions.
\newblock In {\em Proceedings of the ACM SIGPLAN Haskell symposium, Tokyo,
  Japan}. ACM, 2011.

\end{itemize}

\section{Prerequisites}

Some parts of the thesis assume that the reader is familiar with Haskell,
predicate logic, and BNF notation. Haskell is used for defining Hydra, as well
as for implementing it. BNF notation is used for specifying the concrete
syntax of Hydra. Predicate logic is used for explaining the language concepts
and to give the ideal semantics of Hydra.

Readers unfamiliar with Haskell may refer to the language report by
\citet{Haskell98} or one of the following books: \cite{Thompson1999a},
\citet{Hutton2007a}, \citet{Hudak1999a} or \citet{O'Sullivan2008a}. Having
said that, readers familiar with other higher-order, typed functional
programming languages, such as Standard ML \citep{Milner1997a}, should also be
able to follow the thesis in its entirety.

It is worthwhile to mention that only small subset of the Haskell features are
needed to model and simulate physical systems in Hydra. The modeller should
know:

\begin{itemize}

\item how to define a function by providing its name, arguments and result,

\item how to apply a function to arguments,

\item how to write a function type signature involving arbitrarily nested
pairs of basic types,

\item how to write functions that operate on lists (this is only needed for
higher-order modelling with collections of models),

\item and how to use functions as first class values.

\end{itemize}

Other features of Haskell, most notably laziness and type classes, are not
needed to model in Hydra. The aforementioned two features are not used in the
language implementation either. The implementation of Hydra makes use of the
following two Haskell extensions available in Glasgow Haskell Compiler
(GHC)\footnote{\url{http://www.haskell.org/ghc}}: quasiquoting
\citep{Mainland2007} and generalised algebraic data types (GADTs)
\citep{PeytonJones2006a}.


\section{Outline}

The rest of the dissertation is organised as follows.

\begin{itemize}

\item Chapter~\ref{chapBackground} overviews the field of physical modelling,
and the state-of-the-art causal and noncausal modelling languages.

\item Chapter~\ref{chapConcepts} introduces the central concepts of the Hydra
language and its design.

\item Chapter~\ref{chapHydra} explains how to model physical systems in Hydra
by means of instructive examples. The examples were carefully chosen to
showcase those language features that are absent in other noncausal modelling
languages.

\item Chapter~\ref{chapDefinition} formally defines Hydra's concrete syntax,
abstract syntax, type system and ideal semantics. The chapter formally defines
the equational part of Hydra. The reader is referred to \citet{Haskell98} for
the semi-formal definition of the host functional language.

\item Chapter~\ref{chapImplementation} describes how Hydra is implemented.

\item Chapter~\ref{chapRelatedWork} overviews the related work.

\item Chapter~\ref{chapConclusions} concludes the thesis.

\end{itemize}
