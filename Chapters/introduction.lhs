\chapter{Introduction}
\label{chapIntroduction}

The field of physical modelling and simulation plays vital role in design,
implementation and analysis of systems in numerous areas of science and
engineering. Examples include: electronics, mechanics, thermodynamics,
chemical reaction kinetics, population dynamics and neural networks
\citep{Cellier1991}. To cope with increasing size and complexity of physical
models, a number modelling and simulation languages have been developed. The
modelling and simulation languages can be divided in two broad categories:
\emph{causal} and \emph{noncausal}.

A causal model is formulated in terms of \emph{explicit} equations, for
example, \emph{ordinary differential equations} (ODEs) in explicit form. That
is, the cause-effect relationship is explicitly specified by the modeller
\citep{Cellier2006}. In other words, the equations are directed; only
\emph{unknown} variables can appear on the left hand side of the equal sign,
and only \emph{known} variables on the other side. Since the equations are
directed, it is relatively straight forward to translate causal models into a
low level simulation code (e.g., into a sequence of assignment statements) and
simulate it. Simulink is a prominent representative of causal modelling
languages \citep[2008]{Simulink}.

A noncausal model is formulated in terms of \emph{implicit} equations, for
example, \emph{differential algebraic equations} (DAEs) in implicit form. In
other words, the equations are undirected: both known and unknown variables
may appear on both sides of equal sign \citep{Cellier2006}. The translation of
noncausal models into simulation code involves additional symbolic processing
and numerical simulation methods that are not required for causal modelling
and simulation. Examples include: symbolic transformations that try to
causalise noncausal models and if this is not possible numerical solvers for
(nonlinear) implicit equations. Modelica is a prominent, state-of-the-art
representative of noncausal modelling languages \citep[2010]{Modelica}.

Noncausal modelling has a number of advantages over causal modelling. The
most important ones are listed below:

\begin{itemize}

\item In many physical domains models are more naturally represented using
noncausal equations and in some physical domains models cannot be represented
using only causal equations.

\item Noncausal languages are more declarative and approach modelling
problems from higher level of abstraction by focusing on \emph{what} to model
rather than \emph{how} to model to enable simulation.

\item Noncausal models are more reusable as equations can be used in a number
of different ways depending on their usage context (e.g., causalised in a
number of different ways).

\end{itemize}

Although causal modelling remains a dominant paradigm, interest in noncausal
modelling has grown recently as evidenced by release of noncausal modelling
and simulation tools by prominent vendors such as Maple (MapleSim) and
MathWorks (Simscape).


\section{First-class Models}
\label{secFirstClassModels}

A language entity is \emph{first-class} if it can be (1) passed as a parameter
to functions, (2) returned as a result from functions, (3) constructed at
runtime and (4) placed in data structures \citep{Scott2009a}. Current,
main-stream noncausal languages are not treating models as first-class values
\citep{Nilsson2003a}. This limits their expressiveness and applicability,
resulting in a very limited capabilities in the following crucial application
areas of modelling languages: \emph{higher-order} and
\emph{structurally dynamic} modelling.

\subsection{Higher-order Modelling}

Higher-order modelling allows parametrisation of models on other models
\citep{Nilsson2003a}. For instance, a car model can be parametrised on the
list of tires it is using, and an electrical transmission line model can be
parametrised on the list of electrical components on the line. This style of
modelling is not supported by main-stream, noncausal languages, including
Modelica. Tool specific and external scripting languages are often used to
generate noncausal models for particular instances of higher-order models
\citep{Broman2008a}. Whilst practical for some applications, this defeats the
purpose of a declarative, noncausal modelling language.

This thesis formally defines a language that supports higher-order modelling
by treating noncausal models as first-class values in a purely functional
programming language and describes its implementation. In this setting, a
function from model (or from collections of models placed in a suitable data
structure) to model can be seen as a higher-order model and an application of
this function can be seen as an instantiation of the higher-order model.

The idea to treat noncausal models as first-class values in a functional
programming language was introduced by \citet{Nilsson2003a} in the context of
a framework called Functional Hybrid Modelling (FHM) for designing and
implementing noncausal modelling languages. However, the paper postpones a
concrete language definition and implementation for future work. In Addition,
the FHM framework proposes to exploit the first-class nature of noncausal
models for modelling \emph{hybrid} systems (i.e., systems that exhibit both
continuous and discrete behaviour); this is relevant in the following
section.

\citet{Broman2007a} defined and implemented a noncausal language that
supports parametrisation of models on other models and allows for a form of
higher-order modelling, but construction of noncausal models at
simulation runtime and manipulation of collections of models placed
in data structures have not been considered.

\subsection{Structurally Dynamic Modelling}

Physical modelling often entails modelling of major changes in the system
behaviour by changing the equations that describe the system dynamics
\citep{Mosterman1997}. A model where equational description changes over time
is called structurally dynamic. Each structural configuration of the model is
known as a \emph{mode} of operation. \citet{Cellier2006} refer to structurally
dynamic systems as \emph{variable-structure} systems. Structurally dynamic
systems are an example of the more general notion of hybrid systems
\citep{Nilsson2003a}. The term structurally dynamic emphasises only one
discrete aspect, that is, the change of equations at discrete points in time.

\emph{Cyber-physical} systems \citep{Lee2008a}, where digital computers
interact with continuous physical systems, can also be seen as instances of
hybrid systems. In this context, structurally dynamic modelling is relevant;
as modelling of a cyber-physical system where the digital part's influence
causes major changes in the physical part may require changing of the
equations describing the dynamics of the continuous part. Recently,
the US National Science Foundation identified Cyber-physical systems as one of
its key research areas \citep[2008]{NSF2008a}.

Current noncausal languages offer a quite limited support for modelling
structurally dynamic systems \citep{Mosterman1997, Mosterman1999a, Zauner2007,
Zimmer2008a}. There are a number of reasons for this limited support. This
thesis concentrates on one particular reason related to the design and
implementation of modelling and simulation languages, specifically the
prevalent assumption that most or all processing to put a model into a form
suitable for simulation will take place \emph{prior} to simulation
\citep{Nilsson2007,Zimmer2007}. By enforcing this assumption in the design of
a modelling language, its implementation can be simplified as there is no need
for simulation-time support for handling structural changes. For instance, a
compiler can typically generate static simulation code (often just a sequence
of assignment statements) with little or no need for dynamic memory or code
management. This results in a good performance, but such language design and
implementation approach restricts the number of modes to be modest as, in
general, separate code must be generated for each mode. This rules out
supporting \emph{highly} structurally dynamic systems where the number of
modes is too large or even a priori unbounded.

There are a number of efforts to design and implement modelling and simulation
languages with improved support for structural dynamism. Examples include:
HYBRSIM \citep{Mosterman1998}, MOSILAB \citep{Nytsch-Geusen2005a}, Sol
\citep{Zimmer2008a} and Acumen \citep{Taha2010a}. However, thus far,
implementations have either been interpreted (HYBRSIM and Sol) and thus
sacrificing the efficiency, or languages have been restricted so as to limit
the number of modes to make it feasible to compile code for all modes prior to
simulation (MOSILAB and Acumen).

\subsection{Contributions}

This dissertation presents a novel approach to the design and implementation
of noncausal modelling and simulation languages with first-class models
supporting higher-order and structurally dynamic modelling. The thesis
formally defines a noncausal modelling language called Hydra and describes
its implementation in great detail. Hydra provides noncausal modelling and
simulation capabilities that go beyond the state of the art and represents a
significant progress in the field of design and implementation of declarative
modelling and simulation languages, in particular:

\begin{itemize}

\item The thesis shows how to enable higher-order modelling capabilities by
embedding noncausal models as first-class entities into a purely functional
programming language. To my knowledge, Hydra is the first language that
faithfully treats noncausal models as first-class values (i.e., supports all
four points outlined in the beginning of Section \ref{secFirstClassModels}).

\item The thesis shows how to use runtime symbolic processing and
\emph{just-in-time} (JIT) compilation to enable efficient simulation of
noncausal models that are generated at simulation runtime. To my knowledge,
Hydra is the first language that enables support \emph{both} for modelling and
simulation of highly structurally dynamic systems and for compilation of
simulation code for efficiency.

\end{itemize}

In addition to presenting the language definition and implementation, the
aforementioned claims are also backed up by illustrating a range of example
physical systems that cannot be modelled and simulated in current noncausal
languages. The examples are carefully chosen to show case those language
features of Hydra that are lacking in other noncausal modelling languages.

The language design choices and implementation approaches presented here can
be used to enhance existing noncausal modelling and simulation languages, as
well as, to design and implement new modelling languages. This thesis provides
a self-contained reference for such undertaking by defining the language
semantics formally and providing an in-depth description of the
implementation.

Many language features of Hydra follow closely those proposed by
\cite{Nilsson2003a} in the context of the FHM framework and can be seen as the
first concrete language definition and implementation that is based on the FHM
framework. However, as I have already mentioned, at this stage Hydra focuses
only on one aspect of hybrid modelling, namely, structural-dynamism. Other
discrete aspects that do not lead to structural reconfigurations (e.g.,
\emph{impulses} \citep{Nilsson2003a,Nilsson2003b}) are not considered in this
thesis, but, in principle, can be incorporated in the Hydra language.

This work can be seen as an application of successful ideas developed in
functional programming languages research to declarative modelling and
simulation languages. I hope that this work will aid to further
cross-fertilisation and exchange of ideas between these research communities.

\section{Embedding}

Hydra is a Haskell-embedded \emph{domain-specific language} (DSL). Here, the
domain is noncausal modelling and simulation using implicitly formulated
DAEs. Haskell is a purely functional, higher-order, strongly typed programming
language \citep{Haskell98}, which is widely used for embedded DSL development
\citep{Stewart2009a}.

Embedding is a powerful and popular way to implement DSLs \citep{Hudak1998}.
Compared with implementing a language from scratch, extending a suitable
general-purpose programming language, the \emph{host language}, with notions
addressing a particular application or problem domain tends to save a lot of
design and implementation efforts. The motivation behind using an embedding
approach for Hydra is to concentrate the language design and implementation
efforts on noncausal modelling notions that are domain specific and absent in
the host language, and, at the same time, to reuse the rest from the host
language.

Having said that, the concept of first-class models, and runtime symbolic
processing and JIT compilation approaches implemented in Hydra are not
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
of, both, shallow and deep embeddings, a combined approach, called
\emph{mixed-level} embedding, can be used \citep{Giorgidze2010a}.

As mentioned above (see Section \ref{secFirstClassModels}), Hydra supports
runtime generation and JIT compilation of noncausal models. Specifically, in
response to \emph{events} occurring at discrete points in time, the simulation
is stopped and, depending on simulation results thus far, new equations are
generated for further simulation \citep{Giorgidze2009a}. This kind of DSL is
referred as \emph{iteratively staged}, emphasising that the domain is
characterised by repeated program generation, compilation and execution
\citep{Giorgidze2010a}.

Because performance is a primary concern in the domain, the simulation code
for each mode of the model has to be compiled. As this code is determined
dynamically this necessitates JIT compilation. For this part of the language
Hydra employs deep embedding techniques, along with the Low Level Virtual
Machine (LLVM) compiler infrastructure \citep{Lattner2002a}, a
language-independent, portable, optimising, compiler backend with JIT support.
In contrast, shallow embedding techniques are used for the parts of Hydra
concerned with high-level, symbolic computations \citep{Giorgidze2010a}.

An alternative might have been to use a \emph{multi-staged} host language like
MetaOCaml \citep{Taha2004}. The built-in runtime code generation capabilities
of the host language then would have been used instead of relying on an
external code generation framework such as LLVM. This approach has not been
pursued, as the tight control over the dynamically generated code is essential
in this application domain.

\subsection{Contributions}

Compilation of embedded DSLs is today a standard tool in the DSL-implementer's
tool box. The seminal example is the work by Elliott et al. on compiling
embedded languages, specifically the image synthesis and manipulation language
Pan \citep{Elliott2000}. Pan, like Hydra, provides for program
generation by leveraging the host language combined with compilation to speed
up the resulting performance-critical computations. However, the program to be
compiled is generated once and for all, meaning the host language acts as a
powerful but fundamentally conventional macro language: program generation,
compilation, and execution is a process with a fixed number of stages.

Hydra is iteratively staged and, also, rather than acting merely as a powerful
meta language that is out of the picture once the generated program is ready
for execution, the host language is in this case part of the dynamic semantics
of the embedded language through the shallow parts of the embedding. We thus
add further tools to the DSL tool box for embedding a class of languages that
thus far has not been studied much from embedding and staged programming
perspective.

While embedded DSL development methodology is not the main focus of this work,
I nevertheless think that the thesis should be of interest to embedded DSL
implementers, as it presents an application of a new embedding technique. In
particular:

\begin{itemize}

\item The thesis presents a case study of mixed-level embedding of an
iteratively staged DSL in a host language that does not provide built-in
multi-stage programming capabilities.

\item The thesis describes how to use JIT compilation to implement an
iteratively staged embedded DSL efficiently.

\end{itemize}

\section{Published Peer-reviewed Contributions}

The content of this thesis is partly based on the peer-reviewed publications
that are listed in this section (see below). I wrote the papers in
collaboration with my coauthors. This thesis was written by myself. I have
implemented the software described in this dissertation and in the following
papers. The software is available on my
webpage\footnote{\url{http://www.cs.nott.ac.uk/~ggg/}} under the open source
BSD license.

The following four papers describe various aspects of the design and
implementation of Hydra, as well as, a number of its applications.

\begin{itemize}
\item
George Giorgidze and Henrik Nilsson.
\newblock Embedding a {F}unctional {H}ybrid {M}odelling language in {H}askell.
\newblock In {\em Proceedings of the 20th international symposium on
  Implementation and Application of Functional Languages}, Hatfield, England,
  2008.
\newblock To appear in peer-reviewed proceedings published by Springer LNCS.

\item
George Giorgidze and Henrik Nilsson.
\newblock Higher-order noncausal modelling and simulation of structurally
  dynamic systems.
\newblock In {\em Proceedings of the 7th International Modelica Conference},
  Como, Italy, 2009. Link{\"o}ping University Electronic Press.

\item
George Giorgidze and Henrik Nilsson.
\newblock Mixed-level embedding and {JIT} compilation for an iteratively staged
  {DSL}.
\newblock In {\em Proceedings of the 19th international workshop on Functional
  and (Constraint) Logic Programming}, Madrid, Spain, 2010.
\newblock To appear in peer-reviewed proceedings published by Springer LNCS.

\item
Henrik Nilsson and George Giorgidze.
\newblock Exploiting structural dynamism in {F}unctional {H}ybrid {M}odelling
  for simulation of ideal diodes.
\newblock In {\em Proceedings of the 7th EUROSIM Congress on Modelling and
  Simulation}, Prague, Czech Republic, 2010. Czech Technical University
  Publishing House.
\end{itemize}

The following two papers are about highly structurally dynamic, causal
modelling and simulation using Yampa, a Haskell-embedded Functional Reactive
Programming (FRP) language \citep{Hudak2003}. The combinator that allows
switching of equations during simulation runtime in Hydra draws its
inspiration from switching combinators featured in Yampa
\citep{Nilsson2002a,Courtney2003a}.

\begin{itemize}
\item
George Giorgidze and Henrik Nilsson.
\newblock Demo outline: switched-on {Y}ampa.
\newblock In {\em Proceedings of the ACM SIGPLAN Haskell workshop}, Freiburg,
  Germany, 2007. ACM.

\item
George Giorgidze and Henrik Nilsson.
\newblock Switched-on {Y}ampa: declarative programming of modular synthesizers.
\newblock In {\em Proceedings of the 10th international symposium on Practical
  Aspects of Declarative Languages}, San Francisco, CA, USA, 2008. Springer.
\end{itemize}

Some of the embedding techniques described in this thesis are also used in the
following paper.

\begin{itemize}
\item
George Giorgidze, Torsten Grust, Tom Schreiber, and Jeroen Weijers.
\newblock {H}askell boards the {F}erry: Database-supported program execution
  for {H}askell.
\newblock In {\em Proceedings of the 22nd international symposium on
  Implementation and Application of Functional Languages}, Alphen aan den Rijn,
  Netherlands, 2010.
\newblock To appear in peer-reviewed proceedings published by Springer LNCS.
\end{itemize}

\section{Prerequisites}

Some parts of the thesis assume that the reader is familiar with Haskell,
predicate logic, and BNF notation. Haskell is used for defining Hydra, as well
as, for implementing it. The BNF notation is used for specifying the concrete
syntax of Hydra. Predicate logic is used for explaining the language concepts
and to give the ideal denotational semantics of Hydra.

Readers unfamiliar with Haskell may refer to the language report by
\citet{Haskell98} or one of the following books: \citet{Hutton2007a},
\cite{Thompson1999a}, \citet{Hudak1999a} or \citet{O'Sullivan2008a}. Having
said that, readers familiar with other higher-order, typed functional
programming languages, such as Standard ML \citep{Milner1997a}, should also be
able to follow the thesis in its entirety.

\section{Outline}

The rest of the dissertation is organised as follows:

\begin{itemize}

\item Chapter \ref{chapBackground} overviews the field of physical modelling,
and the state-of-the-art causal and noncausal modelling languages.

\item Chapter \ref{chapConcepts} introduces central concepts of the Hydra
langauge.

\item Chapter \ref{chapHydra} explains how to model physical systems in Hydra
by means of instructive examples. The examples were carefully chosen to show
case those language features that are absent in other noncausal modelling
languages.

\item Chapter \ref{chapDefinition} formally defines Hydra's concrete syntax,
abstract syntax, type system and ideal denotational semantics.

\item Chapter \ref{chapImplementation} describes how Hydra is implemented in
great detail.

\item Chapter \ref{chapRelatedWork} overviews the related work and, in light
of it, positions the contributions of this thesis in further detail.

\item Chapter \ref{chapConclusions} concludes the thesis.

\end{itemize}
