\chapter{Introduction}

The field of modelling and simulation of physical systems plays an important role in design, implementation and analysis of systems in numerous areas of science and engineering, e.g., electronics, mechanics, thermodynamics, chemical reaction kinetics, population dynamics and neural networks (to mention just a few) \cite{Cellier1991}. To cope with ever increasing size and complexity of real-world systems, a number modelling and simulation languages have been developed. Modelling and simulation languages can be divided in two broad categories: \emph{causal} and \emph{non-causal}.

Causal languages model systems behaviour in terms of explicit equations, for
example, \emph{ordinary differential equations} (ODEs) in explicit form. That
is, the cause-effect relationship must be explicitly specified by the modeller
\cite{Cellier2006}. In other words, the equations are directed: only
\emph{unknown} variables can appear on the left hand side of the equal sign,
and only \emph{known} variables on the other side. Since the equations are
directed, it is relatively straight forward to translate causal models into a
low level simulation code, for example, into a sequence of assignment
statements and simulate it. Simulink is a prominent representative of causal
modelling languages \cite{Simulink2008}.

Non-causal models are formulated in terms of implicit equations, for example, \emph{differential algebraic equations} (DAEs) in implicit form. In other words, equations are undirected: both known and unknown variables may appear on both sides of equal sign \cite{Cellier2006}. The translation of non-causal models into simulation code involves additional symbolic processing and numerical simulation methods that are not necessary for causal modelling and simulation, for example, symbolic transformations that try to causalise non-causal models and if this is not possible numerical solvers for (non-linear) implicit equations. Modelica is a prominent, state-of-the-art representative of non-causal modelling languages \cite{Modelica2007}.

Non-causal modelling has a number of advantages over causal modelling:
\begin{itemize}
\item In many physical domains models are more naturally represented using
  non-causal equations and in some physical domains models can not be
  represented using only causal equations
\item Non-causal languages are more declarative and high level by focusing on
  what to model rather than how to model to enable simulation
\item Non-causal models are more reusable as equations can be used in a number
  of different ways depending on their usage context (e.g, causalised in a
  number of different ways)
\end{itemize}

\section{First-class Models}

Current non-causal languages are not treating models as \emph{first-class}
values \cite{Nilsson2003a}. We say that models are first class values in the
language if they:
\begin{itemize}
\item Can be passed as parameters to and returned as a result from functions
\item Can be constructed at run time and stored in data structures
\end{itemize}
Lack of this notion limits the expressiveness and applicability of non-causal
languages. In particular, it results in a very limited capabilities in two
crucial application areas of modelling languages: \emph{higher-order} and
\emph{structurally-dynamic}\footnote{Also referred as variable structure}
modelling and simulation.

\subsection{Higher-order Modelling}

Higher-order modelling is a style of modelling were models are being
parametrised on other models \cite{Nilsson2003a,Broman2008a}. For example, the
model of a car can be parametrised on the list of tires it is using or the
model of an electronic transmission line can be parametrised on the list of
electronic components on the line.

Since this style of modelling is not supported by non-causal languages like Modelica, external imperative programming languages are used to generate non-causal models for particular instances of higher-order models. While practical for some applications, we think that this defeats the purpose of a declarative, non-causal modelling language.

% TODO The following two paragraphs should be clarified further

MetaModelica, a Modelica extension supporting compile-time meta-programming,
has been proposed to tackle the aforementioned problems
\cite{Fritzson2005a}. It provides a functional programming language for
pattern-matching, manipulation and generation of abstract syntax trees of
Modelica models.

In this thesis we propose an alternative approach to higher-order modelling by
treating non-causal models as first-class values. In this setting, a function
from model to model can be seen as a higher-order model and an application of
this function can be seen as an instantiation of the higher-order model. A
similar approach is taken in recent work on Modelling Kernel Language (MKL)
\cite{Broman2007a,Broman2008a}. However, there are differences. In particular,
MKL, as far as it was developed in the paper, provides an untyped and
effectful functional language for higher-order modelling. While, this work is
carried out in a strongly typed and purely functional setting.

\subsection{Structurally-dynamic Modelling}

When developing dynamic models of physical systems, it is often desirable to
model major changes in system behaviour by changing the equations that
describe the dynamics of the system. These major changes can be due to the
modelled system itself exhibiting structural changes, due to a need to change
to simplified models of parts of a system for periods of time, and so on
\cite{Mosterman1997}. Models whose equational description change over time are
called structurally dynamic, and each structural configuration is known as a
\emph{mode} of operation. Structurally dynamic systems are an example of the
more general notion of \emph{hybrid} systems, systems that exhibit both
continuous and discrete behaviour.

Unfortunately, the support offered by current modelling languages for
expressing structurally dynamic systems (as well as hybrid systems in general)
is somewhat limited \cite{Mosterman1999a,Zauner2007,Zimmer2008a}. This is true
in particular for non-causal modelling languages. There are a number of
reasons for this limited support, many of them related to the technical
difficulties of simulating structurally dynamic models, such as identifying
suitable state variables for different modes and proper transfer of the state
between modes \cite{Mosterman1997,Mosterman1999a}.

However, there is also one less fundamental reason related to the design and
implementation of modelling and simulation languages, namely the common
assumption that most or all processing to put a model into a form suitable for
simulation will take place \emph{prior} to simulation
\cite{Nilsson2007,Zimmer2007}. By enforcing this assumption in the design of a
modelling language, its implementation can be simplified as there is no need
for simulation-time support for handling structural changes. For instance, a
compiler can typically generate static simulation code (often just sequences
of assignment statements) with little or no need for dynamic memory
management. This results in a good performance. But the limitations are also
obvious: for example, the number of modes must be modest as, in general,
separate code must be generated for each mode. This rules out supporting
\emph{highly} structurally dynamic systems: systems where the number of modes
is too large to make explicit enumeration feasible, or even a priori
unbounded.

There are a number of efforts to design and implement modelling and simulation
languages with improved support for structural dynamics. Examples include
HYBRSIM \cite{Mosterman1998}, MOSILAB \cite{Nytsch-Geusen2005a}, and Sol
\cite{Zimmer2008a}. However, thus far, implementations have either been
interpreted (HYBRSIM and Sol) and thus sacrificing the efficiency, or the
language has been restricted so as to limit the number of modes to make it
feasible to compile code for all modes prior to simulation (MOSILAB).

\subsection{Contributions}

In this dissertation we present a novel approach to the design and
implementation of non-causal modelling and simulation languages with
first-class models supporting higher-order modelling and structurally-dynamic
simulation. In particular:
\begin{itemize}
\item We show how to enable higher-order modelling capabilities by embedding
  non-causal models as a first-class entities into a functional programming
  language.
\item We show how to use run-time symbolic processing and \emph{just-in-time}
  (JIT) compilation to enable efficient simulation of non-causal models that
  are generated at simulation run time. This enables support both for
  modelling and simulation of highly dynamic systems and for compilation of
  simulation code for efficiency.
\end{itemize}

To achieve the above, we developed Hydra, a modelling and simulation
language. The language provides non-causal modelling and simulation
capabilities that go beyond the state of the art and represents a significant
progress in the field of design and implementation of declarative modelling
and simulation languages. The language design choices and implementation
approaches can be used in other non-causal modelling and simulation languages.

\section{Embedding}

Hydra is a Haskell \cite{Haskell98} embedded \emph{domain-specific language} (DSL). Here, the domain is non-causal modelling and simulation using implicitly formulated DAEs. Haskell is a purely functional, higher-order programming language that is widely used for embedded DSL development \cite{Stewart2009a}.

Embedding is a powerful and popular way to implement DSLs
\cite{Hudak1998}. Compared with implementing a language from scratch,
extending a suitable general-purpose programming language, the \emph{host
  language}, with notions and vocabulary addressing a particular application
or problem domain tends to save a lot of design and implementation
effort. This is what motivated us to use the embedding approach.

Having said this, the concept of first-class models, and run-time symbolic
processing and JIT compilation approaches implemented in Hydra are not
predicated on embedded implementation. These language design and
implementation approaches can be used in other non-causal modelling languages,
embedded or otherwise.

There are two basic approaches to language embeddings: \emph{shallow} and
\emph{deep}. In a shallow embedding, domain-specific notions are expressed
directly in host-language terms, typically through a higher-order combinator
library. This is a light-weight approach that makes it easy to leverage the
facilities of the host language. In contrast, a deep embedding is centred
around a \emph{representation} of embedded language terms that then are given
meaning by interpretation or compilation. This is a more heavy-weight
approach, but also more flexible. In particular, for optimisation or
compilation, it is often necessary to inspect terms, suggesting a deep
embedding. The two approaches can be combined to draw on the advantages of
each. This leads to \emph{mixed-level} embedding.

As mentioned above Hydra supports run-time generation and JIT compilation of
non-causal models. Specifically, in response to \emph{events}, which occur at
discrete points in time, the simulation is stopped and, \emph{depending} on
results thus far, (partly) new equations are \emph{generated} describing a
(partly) new problem to be solved. We refer to this kind of DSL as
\emph{iteratively staged} to emphasise that the domain is characterised by
repeated program generation, compilation and execution.

Because performance is a primary concern in the domain, the simulation code for each mode of the model has to be compiled. As this code is determined \emph{dynamically}, this necessitates JIT compilation. We use a deep embedding for this part of the language along with the Low-Level Virtual Machine (LLVM) \cite{Lattner2002a}, a language-independent, portable, optimising, compiler back-end with JIT support. In contrast, we retain a shallow embedding for the parts of the embedded language concerned with high-level, symbolic computations to get maximum leverage from the host language.

An alternative might have been to use a \emph{multi-staged} host language like
MetaOCaml \cite{Taha2004}. The built-in run-time code generation capabilities
of the host language would then have been used instead of relying on an
external code generation framework. We did not pursue this approach because we
wanted to have tight control over the generated code.  Also, not predicating
our approach on a multi-staged host language means that some of our ideas and
implementation techniques can be more readily deployed in other contexts, for
example, to enhance the capabilities of existing implementations of non-causal
languages.

Compilation of embedded DSLs is today a standard tool in the DSL-implementer's
tool box. The seminal example is the work by Elliott et al. on compiling
embedded languages, specifically the image synthesis and manipulation language
Pan \cite{Elliott2000}. Pan, like our language, provides for program
generation by leveraging the host language combined with compilation to speed
up the resulting performance-critical computations. However, the program to be
compiled is generated once and for all, meaning the host language acts as a
powerful but fundamentally conventional macro language: program generation,
compilation, and execution is a process with a fixed number of stages.

\subsection{Contributions}

As Hydra is iteratively staged, the problems we are facing are in many ways
different. Also, rather than acting merely as a powerful meta language that is
out of the picture once the generated program is ready for execution, the host
language is in our case part of the dynamic semantics of the embedded language
through the shallow parts of the embedding. We thus add further tools to the
DSL tool box for embedding a class of languages that thus far has not been
studied much from embedding and staged programming perspective.

While embedded DSL development methodology is not the main focus of this work,
we nevertheless think that the thesis should be of interest to embedded DSL
implementers as it presents an application of a new embedding technique. In
particular:
\begin{itemize}
\item
    We present a case study of mixed-level embedding of an iteratively staged
    DSL in a host language that does not provide built-in multi-stage
    programming capabilities.
\item
    We show how to use JIT compilation to implement an iteratively staged
    embedded DSL efficiently.
\end{itemize}

\section{Outline}

The rest of the dissertation is organised as follows.
