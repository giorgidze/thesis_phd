\chapter{FHM Concepts and Design of Hydra}
\label{chapConcepts}


This chapter introduces the three central concepts of the FHM framework that
the Hydra language is based on, namely: signal, signal function and signal
relation. These concepts facilitate development of and reasoning about Hydra
models, and are used both in informal (see Chapter~\ref{chapHydra}) and formal
(see Chapter~\ref{chapDefinition}) presentations of the language. This chapter
only covers conceptual definitions; how the concepts of Hydra are implemented
is covered in Chapter~\ref{chapImplementation}. This chapter also discusses
how Hydra's design facilitates the embedding of the aforementioned concepts
into a functional programming language.


\section{FHM and FRP}

The idea of treating noncausal models as first-class values in a functional
programming language is due to \citet{Nilsson2003a}. The authors propose the
FHM framework for designing and implementing noncausal modelling languages.
The FHM framework borrows the notion of signal denoting time-varying values
from the FRP languages and generalises the notion of signal function (featured
in a number of variants of FRP, most notably Yampa
\citep{Nilsson2002a,Hudak2003}) to signal relation.

Intuitively, a signal function can be understood as a block with inputs and
outputs featured in causal modelling languages, while a signal relation can be
understood as a noncausal model without explicitly specified inputs and
outputs. In other words, FRP extends a functional programming language with
causal modelling capabilities, while FHM extends a functional programming
language with noncausal modelling capabilities.

Signal functions are first-class entities in most variants of FRP and this
property also carries over to signal relations in FHM. As we will see later in
this chapter and also in Chapter~\ref{chapHydra}, the first-class nature of
signal relations is crucial for higher-order and structurally-dynamic
modelling in Hydra. To my knowledge, Hydra is the first language that features
the FHM's notion of signal relation as a first-class entity.


\section{Signal}
\label{secSignal}

Conceptually, a \emph{signal} is a time-varying value, that is, a function
from time to value:

\begin{code}
Time           ~=  Real
Signal alpha   ~=  Time -> alpha
\end{code}

|Time| is continuous and is represented as a real number. The type parameter
|alpha| specifies the type of values carried by the signal; for example, a
signal of type |Signal Real| may represent a change to the total amount of
current flowing in a certain electrical circuit over time, or a signal of type
|Signal (Real,Real)| may represent a change in position of a certain object in
a two dimensional space over time.

Hydra features signals of reals (i.e., |Signal Real|) and signals of
arbitrarily nested pairs of reals. Signals of nested pairs are useful for
grouping of related signals. As an example of a signal that carries nested
pairs of reals, consider a signal of type |Signal ((Real,Real),(Real,Real))|.
This signal can represent current and voltage pairs at the positive and
negative pins of a two-pin electrical component, for example.

In a concrete implementation, |Real| would typically be represented by a
suitable floating point type, such as |Double|. Indeed, |Double| is used in
Hydra. However, |Real| is used in most places of the presentation as we
conceptually are dealing with real numbers.

The aforementioned treatment of signals as continues can be seen as an
abstraction over the underling discretely sampled implementation. It is
cumbersome and error prone to directly work with the discrete representation
for entities that conceptually exhibit continuous dynamics. For example,
explicit handling of fixed and variable sampling rates, composition of models
using different sampling rates, and explicit accounting for numerical errors
are problematic. This is not to say that being aware of the underling
discretely sampled implementation is not important. In some cases (e.g., when
the numerical simulation fails or when the simulation performance is
unacceptably slow) it is necessary to adjust the discrete simulation time step
or change other simulation parameters. However, in most cases, assuming that
the continuous model is correct, those adjustments can be made without changes
to the model.

Although modelling with discrete streams for conceptually discrete systems
(such as digital controllers) is out of the scope of this thesis, it would be
interesting to explore a discrete variant of Hydra featuring noncausal
equations on discrete streams.

\section{Signal Function}

Conceptually, a \emph{signal function} is a function from signal to signal:

\begin{code}
SF alpha beta ~= Signal alpha -> Signal beta
\end{code}

A signal function of type |SF alpha beta| can be applied to an input signal of
type |Signal alpha|; it produces an output signal of type |Signal beta|.

Because a pair of signals, say |(Signal alpha,Signal beta)|, is isomorphic to
a signal of the pair of the signal types, in this case |Signal (alpha, beta)|,
unary signal functions suffice for handling signal functions of any arity; for
example, the binary signal function |add| that takes two signals and computes
the sum of their values at each point in time can be given the following type
and conceptual definition:

\begin{code}
add :: SF (Real,Real) Real
add s ~= \t -> fst (s t) + snd (s t)
\end{code}

Hydra provides a number of primitive signal functions that lift common
mathematical operations (e.g., |+|, |*|, |sin| and |cos|) to the signal level.
Hydra also provides the |der| signal function of type |SF Real Real|. This
signal function differentiates the given signal. Later we will see that the
use of the |der| signal function in noncausal equations allows for the
definition of differential equations.

It is worthwhile to note that except for the |der| signal function all
primitive and user definable signal functions in Hydra are \emph{stateless};
that is, their output only depends on their input at current point in time.
The |der| signal function's output depends on its input signal's current rate
of change.

\section{Signal Relation}

Conceptually, a \emph{signal relation} is a relation on signals. Stating that
some signals are in a particular relation to each other imposes
\emph{constraints} on those signals. Assuming these constraints can be
satisfied, this allows some of the signals to be determined in terms of the
others depending on which signals are known and unknown in a given context;
that is, signal relations are noncausal, unlike signal functions where the
inputs and outputs are given a priori.

An ordinary relation can be seen as a predicate that specifies whether some
given values are related or not. The same is true for signal relations:

\begin{code}
SR alpha  ~=  Time -> Time -> Signal alpha -> Prop
\end{code}

Given two points in time and a signal, a signal relation defines a proposition
constraining the signal starting from the first point in time and ending with
the second point in time. Here, |Prop| is a type for propositions defined in
second-order logic. \emph{Solving} a relation for a given period of time thus
means finding a signal that satisfies the predicate. We say that in this
period of time the signal relation instance is \emph{active}.

Just like for signal functions, unary signal relations suffice for handling
signal relations of any arity; for example, the following pseudo code
conceptually defines a binary signal relation:

\begin{code}
equal ::  SR (Real,Real)
equal t1 t2 s ~= {-" \forall \, t \in \mathbb{R} . \, "-}  t1  <= t && t <= t2  =>  fst (s t) == snd (s t)
\end{code}

\noindent This signal relation asserts that the first and second components of
the signal |s| are equal from |t1| to |t2|.

Let us consider a slightly more elaborate example of a signal relation. The
following conceptual definition gives a signal relation imposing constraints
characteristic to electrical components with two connectors (see
Figure~\ref{figTwoPin}).

\begin{code}
twoPin ::  SR (((Real,Real),(Real,Real)),Real)
twoPin t1 t2 s ~=
  {-" \forall \, t \in \mathbb{R} . \, "-} t1 <= t && t <= t2 => twoPinProp (s t)
  where
  twoPinProp (((p_i,p_v),(n_i,n_v)),u) ~=    {-" "-}     p_v  -  n_v  =  u
                                             &&          p_i  +  n_i  =  0
\end{code}

Here, |p_i|, |p_v|, |n_i|, |n_v| and |u| are the components of the tuple
carried by the signal |s|. The tuple components |p_i| and |p_v| represent the
current into the positive pin and the voltage at the positive pin,
respectively. The tuple components |n_i| and |n_v| represent the current into
the negative pin and the voltage at the negative pin, respectively. The tuple
component |u| represents the voltage drop across the electrical component.

\begin{figure}
\begin{center}
\includegraphics{Graphics/twoPin}
\end{center}
\caption{\label{figTwoPin} Electrical component with two connectors.}
\end{figure}

By \emph{applying} signal relations to signals (in the sense of predicate
application) it is possible to reuse the equational constraints. Signal
relation application also allows for definition of hierarchically structured
systems of equations. The following conceptual definition gives a signal
relation imposing constrains characteristic to a resistor (with
resistance~|r|).

\begin{code}
resistor :: Real -> SR (((Real,Real),(Real,Real)),Real)
resistor r t1 t2 s ~=
  {-" \forall \, t \in \mathbb{R} . \, "-}  t1  <= t && t <= t2 => {-" \exists \, u \in Signal \, \mathbb{R} . \, "-}   {-" "-}   twoPin t1 t2 (pairS s u)
                                                                                                                        &&        resistorProp (s t) (u t)
  where
  pairS s u t'                          ~=    (s t', u t')
  resistorProp ((p_i,p_v),(n_i,n_v)) u  ~=    r * p_i = u
\end{code}

Note how the signal relation |resistor| is defined in terms of the signal
relation application of |twoPin| and one additional equation. As we will see
in the next section and in Chapter~\ref{chapHydra}, Hydra provides a
convenient syntax for defining and applying signal relations.

\section{Design of Hydra}
\label{secDesignOfHydra}

Hydra is a two-level language. It features the \emph{functional level} and the
\emph{signal level}. The functional level allows for the definition of
ordinary functions operating on time-invariant values. The signal level allows
for the definition of signal relations and signal functions on time-varying
values.

Signal relations and signal functions are first-class entities at the
functional level. In contrast, signals are not first-class entities at the
functional level. However, crucially, instantaneous values of signals can be
passed to the functional level, allowing for the generation of new signal
relations that depend on signal values at discrete points in time.

A definition at the signal level may freely refer to entities defined at the
functional level as the latter are time-invariant, known parameters as far as
solving the signal-level equations are concerned. However, the opposite is not
allowed; that is, a time-varying entity is confined to the signal level. The
signal-level notions that exist at the functional level are signal relation
and signal function. These notions are time-invariant. Concrete examples of
signal-level and functional-level definitions as well as definitions where
these two levels interact with each other are given in
Chapter~\ref{chapHydra}.

Hydra is implemented as a Haskell-embedded DSL using quasiquoting, a Haskell
extension implemented in GHC, for providing a convenient surface
syntax\footnote{Quasiquoting is not unique to Haskell. It has been available
in other languages (most notably in the Lisp family of languages).}. As a
result, Haskell provides the functional level for free through shallow
embedding. In contrast, the signal level is realised through deep embedding;
that is, signal relations expressed in terms of Hydra-specific syntax are,
through the quasiquoting machinery, turned into an internal representation, an
abstract syntax tree (AST), that then is used for compilation into simulation
code (see Chapter~\ref{chapImplementation} for details). Note that, although
Hydra is embedded in Haskell, the two-level language design outlined earlier
in this section and the notion of first-class signal relations are not
predicated on the embedding approach.

The Haskell-embedded implementation of Hydra adopts the following syntax for
defining signal relations:

\begin{code}
[rel| pattern -> equations |]
\end{code}

The symbol | [rel|| | is the opening quasiquote and the symbol | ||] | is the
closing quasiquote. The pattern binds \emph{signal variables} that scope over
the equations that follow. An equation can be an equality constraint or a
\emph{signal relation application} (stated by using the operator |<>|). Signal
relation application is how the constraints embodied by a signal relation are
imposed on particular signals. In addition to the signal variables bound in
the pattern, equations may also introduce local signal variables. Concrete
examples of signal relations are given in Chapter~\ref{chapHydra}.

The equations are required to be well typed. For example, consider the signal
relation application |sr <> s|. Here, if |sr| has the type |SR alpha| then |s|
must have the type |Signal alpha|.

Hydra provides a conventional syntax for specifying equality constraints. For
example, the equation |x * y = 0| is an equality constraint. Here, |0| is a
constant signal, |*| is a primitive signal function, and |x| and |y| are
signal variables.

In addition to user-defined signal relations, Hydra provides for user-defined
signal functions. Hydra uses the following syntax for defining signal
functions.

\begin{code}
[fun| pattern -> expression |]
\end{code}

Just like for signal relations, quasiquoting is used for defining signal
functions. The pattern binds signal variables that scope over the expression
that follows. Signal functions can be applied to signals by juxtaposing them
together:

\begin{code}
sf s
\end{code}

Signal function applications are required to be well typed. In this example,
if |sf| has the type |SF alpha beta| then |s| must have the type |Signal
alpha|. The type of the resulting signal is |Signal beta|.

The quasiquotes, in addition to serving as an embedded DSL implementation
tool, can be seen as clear syntactic markers separating the signal level from
the functional level. These markers are useful when reading Hydra code
listings. The separation is also enforced at the type level of the host
language by the |SR| and |SF| type constructors.

Because signals are not first-class entities at the functional level, it is
not possible to construct a value of type |Signal alpha| directly at the
functional level. Signals only exist indirectly through the signal level
definitions of signal relations and signal functions.

Equality constrains can be used to describe flat systems of equations and the
signal relation application operator (i.e., |<>|) provides for hierarchically
structured systems of equations. Let us introduce a built-in (higher-order)
signal relation that allows for description of structurally dynamic signal
relations.

\begin{code}
switch :: SR a -> SF a Real -> (a -> SR a) -> SR a
\end{code}

The |switch| combinator forms a signal relation by temporal composition. The
combinator takes three arguments and returns the composite signal relation (of
type |SR a|). The first argument (of type |SR a|) is a signal relation that is
initially active. The second argument is a signal function (of type |SF a
Real|). Starting from the first point in time when the signal (of type |Signal
Real|) that is computed by applying the signal function to the signal
constrained by the composite signal relation is about to cross zero (i.e.,
when it is zero and its left derivative is nonzero), the composite behaviour
is defined by the signal relation that is computed by applying the third
argument (a function of type |a -> SR a|) to the instantaneous value of the
constrained signal at that point in time. A formally defined meaning of the
|switch| combinator is given in Chapter~\ref{chapDefinition}.

The |switch| combinator allows for definition of a signal relation whose
equational description changes over time. In addition, the |switch|
combinator allows for state transfer from the old mode and initialisation of
the new mode using the function that computes the new mode from an
instantaneous value of the constrained signal.

In the signal relation notation described earlier, the list of equations that
follows the pattern is not necessarily a static one as the equations may
contain a signal relation application of a structurally dynamic signal
relation. We show how to use the |switch| combinator for modelling and
simulation unbounded structurally dynamic systems in Chapter~\ref{chapHydra}.

As we will see in Chapter~\ref{chapImplementation}, the two-level nature of
Hydra also manifests itself in its implementation as a mixed-level embedding.
The functional level is realised by the shallow part of the embedding, while
the signal level is realised by the deep part of the embedding. This
combination of the two embedding techniques allowed us to maximise the reuse
of the host language features and, as a result, to simplify the language
implementation. Hydra's two-level design can also be realised as a deep
embedding or as a standalone implementation.