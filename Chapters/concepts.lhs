\chapter{Concepts of Hydra}
\label{chapConcepts}

This chapter introduces the three central concepts of the Hydra language:
signal, signal function and signal relation. These concepts facilitate
development of and reasoning about Hydra models, and are used both in informal
(see Chapter \ref{chapHydra}) and formal (see Chapter \ref{chapDefinition})
presentations of the language. This chapter only covers conceptual
definitions; how the concepts of Hydra are implemented is covered in Chapter
\ref{chapImplementation}.

\section{Signal}

Conceptually, a \emph{signal} is a time-varying value, that is, a function
from time to value:

\begin{code}
type  Time           ~=  Real
type  Signal alpha   ~=  Time -> alpha
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

\section{Signal Function}

Conceptually, a \emph{signal function} is a function from signal to signal:

\begin{code}
type SF alpha beta ~= Signal alpha -> Signal beta
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

\section{Signal Relation}

Conceptually, a \emph{signal relation} is a relation on signals. Stating that
some signals are in a particular relation to each other imposes
\emph{constraints} on those signals. Assuming these constraints can be
satisfied, this allows some of the signals to be determined in terms of the
others depending on which signals are known and unknown in a given context;
that is, signal relations are noncausal, unlike signal functions where the
knowns and unknowns (inputs and outputs) are given a priori.

An ordinary relation can be seen as a predicate that specifies whether some
given values are related or not. The same is true for signal relations:

\begin{code}
type SR alpha  ~=  Time -> Signal alpha -> Prop
\end{code}

Given a point in time and a signal, a signal relation defines a proposition
constraining the signal starting from the given point in time. Here, |Prop| is
a type for propositions defined in second-order logic. \emph{Solving} a
relation for a given starting time thus means finding a signal that satisfies
the predicate.

Just like for signal functions, unary signal relations suffice for handling
signal relations of any arity; for example, the following code defines a
binary signal relation:

\begin{code}
equal ::  SR (Real,Real)
equal t0 s ~= {-" \forall \, t \in \mathbb{R} . \, "-}  t >= t0  =>  fst (s t) == snd (s t)
\end{code}

This signal relation asserts that the first and second components of the
signal |s| are equal starting from |t0|.