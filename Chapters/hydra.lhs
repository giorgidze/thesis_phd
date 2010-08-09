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

As an example, equality is a binary signal relation: 

\begin{code}
(=) ::  SR (alpha,alpha)
(=) s ~= forall t . fst (s t) == snd (s t)
\end{code}

Hydra adopts the following syntax for defining signal relations (inspired by
the arrow notation \cite{Paterson2001}): 
\begin{code}
sigrel pattern where equations
\end{code}
The pattern binds \emph{signal variables} that scope over the equations that
follow. The equations are DAEs stated using \emph{signal relation application}
(the operator |<>|). Signal relation application is how the constraints
embodied by a signal relation are imposed on particular signals:
\begin{code}
sr <> s
\end{code}
Equations must be well typed. In this example, if |sr| has type |SR alpha|,
|s| must have type |Signal alpha|. Additionally, Hydra provides a more
conventional-looking syntax for equality between signals. For example: |a * x
+ b = 0| is equivalent to |(=) <> (a * x + b, 0)|.

% TODO mention that <> is used to defined systems of equations hierarchically 
% TODO mention that systems of equations can be dynamic and present the switch combinator

\subsection{Signals}
\subsection{Events}
\subsection{Hierarchical Systems of Non-causal Equations}
\subsection{Switches}

%Week 2
\section{Models with Static Structure}
\section{Higher-Order Modelling}

%Week 3
\section{Structural Dynamism}
\subsection{Breaking Pendulum}

%Week 4
\subsection{Ideal Diodes,Full/Half-way rectifiers}



%}