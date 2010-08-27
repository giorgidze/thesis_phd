%{
%include ../Format/hydra.lhs


% August
\chapter{Modelling and Simulation in Hydra}

%Week 1
\section{Concepts of Hydra}

In this section we describe the three central concepts Hydra is based on: signal, signal function and signal relation. The conceptual definitions are only provided to reason about Hydra models. The actual implementation of theses concepts is presented in Chapter \ref{chap_implementation}.

\subsection{Signal}

Conceptually, a \emph{signal} is a time-varying value; i.e., a function from time to a value of some type |alpha|:

\begin{center}
|Signal  alpha ~= Time -> alpha|
\end{center}

|Time| is continuous, and is represented as a non-negative real number. The
type parameter |alpha| specifies the type of values carried by the signal. For example, a signal that corresponds to the amount of current flowing in an electrical circuit has type |Signal Double|, while a signal that corresponds to the position of a certain object in a two dimensional space has type |Signal (Double,Double)|. Note that a value carried by a signal may also be a tuple.

The basic signal types that are supported in Hydra are: |Signal Double| and |Signal Bool|. In addition, signals of arbitrarily nested tuples of basic types are also supported. As an example of a signal that carries nested tuples we consider a signal of type |Signal ((Double,Double),(Double,Double))| that can be used to describe change of current and voltage at the positive and negative pins of a two-pin electrical component.

\subsection{Signal Function}

A \emph{signal function} can be thought of as a function from signal to signal:

\begin{center}
|SF alpha beta ~= Signal alpha -> Signal beta|
\end{center}

When a value of type |SF alpha beta| is applied to an input signal of type |Signal alpha|, it produces an output signal of type |Signal beta|. 

Because a product of signals, say |Signal alpha| and |Signal beta|, is isomorphic to a signal of the product of the carried types, in this case |Signal (alpha, beta)|, unary signal functions actually suffice for handling signal functions of any arity. For example, the binary signal function |plus_sf| that takes two signals and computes sum of their values at each points in time can be given the following type and meaning:

\begin{code}
(plus_sf) :: SF (Double,Double) Double
(plus_sf) sf t = fst (sf t) + snd (sf t)
\end{code}

Hydra provides a number of primitive signal functions that lift common mathematical operation to signal level. Crucially, Hydra provides |der :: SF Double Double| signal function that differentiates the given signal.


\subsection{Signal Relations}

A \emph{signal relation} is simply a relation on signals. Stating that some signals are in a particular relation to each other imposes \emph{constraints} on those signals. Assuming these constraints can be satisfied, this allows some of the signals to be determined in terms of the others depending on which signals are known and unknown in a given context. That is, signal relations are non-causal, unlike signal functions where the knowns and unknowns (inputs and outputs) are given a priori.

An ordinary relation can be seen as a predicate that decides whether some given values are related or not. The same is of course true for signal relations:

\begin{center}
|SR alpha  ~=  Signal alpha -> Bool|
\end{center}

\emph{Solving} a relation thus means finding a signal that satisfies the
predicate.

Just like for signal functions, unary signal relations suffice for handling signal relations of any arity. We thus introduce the type |SR alpha| for a signal relation on a signal of type |alpha|.

For example, equality is a binary signal relation: 

\begin{code}
(=) ::  SR (alpha,alpha)
(=) s ~= forall t . fst (s t) == snd (s t)
\end{code}

\section{Syntax of Hydra}

Haskell-embedded implementation of Hydra adopts the following syntax for defining signal relations:

\begin{code}
[$rel| pattern -> equations |]
\end{code}

The pattern binds \emph{signal variables} that scope over the equations that follow. The equations are DAEs stated using \emph{signal relation application} (the operator |<>|). Signal relation application is how the constraints embodied by a signal relation are imposed on particular signals:

\begin{code}
sr <> s
\end{code}

Equations must be well typed. In this example, if |sr| has type |SR alpha|, |s| must have type |Signal alpha|.

Hydra provides a more conventional-looking syntax for application of built-in equality signal relation. For example, |a * x + b = 0| is equivalent to |(=) <> (a * x + b, 0)|.

In addition to built-in, primitive signal functions for common mathematical operations Hydra provides for user defined signal functions. Hydra uses the following syntax for defining signal functions:

\begin{code}
[$fun| pattern -> expression |]
\end{code}

The pattern binds \emph{signal variables} that scope over the expression that follows. Signal functions can be applied to signals:

\begin{code}
sf s
\end{code}

Signal function applications must be well typed. In this example, if |sf| has type |SF alpha beta|, |s| must have type |Signal alpha|.

\section{Structurally Dynamic Signal Relations}

Built in equality signal relation (i.e., |=|) is capable of describing flat systems of equations. On the other hand, signal relation application operator (i.e., |<>|) provides for hierarchically structured systems of equations. In this section we introduce one more built in (higher-order) signal relation that allows description of structurally dynamic signal relations:

\begin{code}
switch :: SR a -> SF a Bool -> (a -> SR a) -> SR a
\end{code}

The |switch| combinator takes three arguments that define the composite signal relation (of type |SR a|) returned by the combinator. The first argument (of type |SR a|) is a signal relation that is initially active. The second argument is a signal function (of type |SF a Bool|) that computes a signal of type |Signal Bool| given the signal (of type |Signal a|) constrained by the composite signal relation. Starting from the point in time when the boolean signal equals |True| for the first time the composite behaviour is defined by the signal relation that is computed by applying the third argument (a function of type |a -> SR a|) to the instantaneous value of the constrained signal.

Bearing in mind the conceptual definition of a signal relation the meaning of the |switch| combinator can be given as follows:
\begin{code}
switch sr sf f = \s ->
        (not  (exists t, sf s t)  =>  sr s) &&
        (     (exists t, sf s t)  =>  exists  t_1,  (sf s t_1) &&
                                                    ((exists t_2, sf s t_2) => t_2 >= t_1) &&
                                                    (forall t, (t < t_1 => sr s) && (t >= t_1 => f (s t_1) s)))
\end{code}

Note how the |switch| combinator allows for definition of a signal relation whose equational description changes over time. In the signal relation notation described earlier the list of equations that follows the pattern is not necessarily a static as they may contain signal relation application  structurally dynamic signal relation.


%Week 2
\section{Models with Static Structure}
\section{Higher-Order Modelling}

%Week 3
\section{Structural Dynamism}
\subsection{Breaking Pendulum}

%Week 4
\subsection{Ideal Diodes,Full/Half-way rectifiers}



%}