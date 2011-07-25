\chapter{FHM Concepts and Design of Hydra}
\label{chapConcepts}

This chapter introduces the three central concepts of the FHM framework that
the Hydra language based on: signal, signal function and signal relation.
These concepts facilitate development of and reasoning about Hydra models, and
are used both in informal (see Chapter \ref{chapHydra}) and formal (see
Chapter \ref{chapDefinition}) presentations of the language. This chapter only
covers conceptual definitions; how the concepts of Hydra are implemented is
covered in Chapter \ref{chapImplementation}. This chapter also discusses how
Hydra's design facilitates the embedding of the aforementioned concepts into a
functional programming language.

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

\section{Signal Function}

Conceptually, a \emph{signal function} is a function from signal to signal:

\begin{code}
SF alpha beta ~= Signal alpha -> Signal beta
\end{code}

A signal function of type |SF alpha beta| can be applied to an input signal of
type |Signal alpha|; it produces an output signal of type |Signal beta|.

Because a pair of signals, say |(Signal alpha,Signal beta)|, is isomorphic to
a signal of the pair of the carried types, in this case |Signal (alpha,
beta)|, unary signal functions suffice for handling signal functions of any
arity; for example, the binary signal function |add| that takes two signals
and computes the sum of their values at each point in time can be given the
following type and conceptual definition:

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
means finding a signal that satisfies the predicate.

Just like for signal functions, unary signal relations suffice for handling
signal relations of any arity; for example, the following pseudo code
conceptually defines a binary signal relation:

\begin{code}
equal ::  SR (Real,Real)
equal t1 t2 s ~= {-" \forall \, t \in \mathbb{R} . \, "-}  t1  <= t && t <= t2  =>  fst (s t) == snd (s t)
\end{code}

\noindent This signal relation asserts that the first and second components of
the signal |s| are equal from |t1| to |t2|.

\section{Design of Hydra}

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
and signal function. These notions are time-invariant.

The Hydra language is implemented as a Haskell-embedded DSL using quasiquoting
\citep{Mainland2007,Mainland2008}. As a result, Haskell provides the
functional level for free through shallow embedding. In contrast, the signal
level is realised through deep embedding; that is, signal relations expressed
in terms of Hydra-specific syntax are, through the quasiquoting machinery,
turned into an internal representation, an abstract syntax tree (AST), that
then is used for compilation into simulation code (see Chapter
\ref{chapImplementation} for the details). Note that, although Hydra is
embedded in Haskell, the two-level language design outlined earlier
in this section and the notion of first-class signal relation are not
predicated on the embedding approach.

The Haskell-embedded implementation of Hydra adopts the following syntax for
defining signal relations:

\begin{code}
[rel| pattern -> equations |]
\end{code}

The symbol | [rel|| | is the opening quasiquote and the symbol | ||] | is the
closing quasiquote. The pattern binds \emph{signal variables} that scope over
the equations that follow. An equations can be an equality constrain and a
\emph{signal relation applications} (stated by using the operator |<>|).
Signal relation application is how the constraints embodied by a signal
relation are imposed on particular signals. In addition to the signal
variables bound in the pattern, equations may also introduce local signal
variables. Concrete examples of signal relations are given in Chapter
\ref{chapHydra}.

The equations are required to be well typed. For example, consider the signal
relation application |sr <> s|. Here, if |sr| has the type |SR alpha| then |s|
must have the type |Signal alpha|.

Hydra provides a conventional syntax for specifying equality constrains. For
example, the equation |x * x = 0| is an equality constrain. Here, |0| is a
constant signal, |*| is a primitive signal function, and |x| is a signal
variable.

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
constrained by the composite signal relation crosses zero (i.e., changes its
sign from negative to positive or from positive to negative), the composite
behaviour is defined by the signal relation that is computed by applying the
third argument (a function of type |a -> SR a|) to the instantaneous value of
the constrained signal at that point in time. A formally defined meaning of
the |switch| combinator is given in Chapter \ref{chapDefinition}.

The |switch| combinator allows for definition of a signal relation whose
equational description changes over time. In addition, the |switch|
combinator allows for state transfer from the old mode and initialisation of
the new mode using the function that computes the new mode from an
instantaneous value of the constrained signal.

In the signal relation notation described earlier, the list of equations that
follows the pattern is not necessarily a static one as the equations may
contain a signal relation application of a structurally dynamic signal
relation. As we will see in Chapter \ref{chapHydra}, the |switch| combinator
enables modelling and simulation unbounded structurally dynamic systems.
