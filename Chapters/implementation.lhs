% September
\chapter{Implementation of Hydra}
\label{chapImplementation}

\section{Embedding}
\label{sec:embedding}

In this section, we describe the Haskell embedding of Hydra in further detail.
First, we introduce a Haskell data type that represents an embedded signal
relation. This representation is untyped. We then introduce typed combinators
that ensures that only well-typed signal relations can be constructed.

The following data type is the central, untyped representation of signal
relations. There are two ways to form a signal relation: either from a set of
defining equations, or by composing signal relations temporally:
\begin{code}
data SigRel =
     SigRel        Pattern  [Equation]
  |  SigRelSwitch  SigRel   (Expr -> SigRel)
\end{code}

The constructor |SigRel| forms a signal relation from equations. Such a
relation is represented by a pattern and the list of defining equations. The
pattern serves the dual purpose of describing the \emph{interface} of the
signal relation in terms of the types of values carried by the signals it
relates and their time domains (continuous time or discrete time/events), and
of introducing names for these signals for use in the equations. Patterns are
just nested tuples of signal variable names along with indications of which
are event signals: we omit the details. The list of equations constitute
a system of Differential Algebraic Equations (DAEs)%
\footnote{
Although not necessarily a \emph{fixed} such system as these equations
may refer to signal relations that contain switches.
}
that defines the signal relation by expressing constraints on the (signal)
variables introduced by the pattern and any additional local signal variables.
The various kinds of equations are discussed below.

The |switch|-combinator forms a signal relation by temporal composition of two
signal relations. Internally, such a temporal composition is represented by a
signal relation constructed by |SigRelSwitch|. The first argument is the
signal relation that is initially active. The second argument is the function
that, in the case of an event occurrence from the initially active signal
relation, is used to compute a new signal relation from the value of that
occurrence. Here, the value subset of the type |Expr| is used to represent the
value. This new signal relation then becomes the active one, replacing
the initial signal relation.

Note the use of a mixture of shallow and deep techniques of embedding. The
embedded function in a signal relation constructed by |SigRelSwitch|
corresponds to the shallow part of the embedding. The rest of the data types
constitute a deep embedding, providing an explicit representation of language
terms for further symbolic processing and ultimately compilation, as we will
see in more detail below.

The following data type represents equations. There are four different kinds:
\begin{code}
data Equation  = 
  EquationInit   Expr    Expr        |  EquationEq         Expr    Expr |
  EquationEvent  String  Expr  Expr  |  EquationSigRelApp  SigRel  Expr
\end{code}

Initialisation equations, constructed by |EquationInit|, provide initial
conditions. They are only in force when a signal relation instance first
becomes active.

Equations constructed by |EquationEq| are basic equations imposing the
constraint that the valuations of the two expressions have to be equal for as
long as the containing signal relation instance is active (for example,
equations like |der (der x) = 0|).

Equations constructed by |EquationEvent| define event
signals; i.e., they represent equations like |event e = (x,y) when time =
3|. These equations are directed. The string is the name of the defined event
signal. The first expression gives the value of the event signal at event
occurrences. The second expression defines these occurrences. An event occurs
whenever the signal represented by this expression \emph{crosses} 0.  For the
above example, the expression defining the event occurrences would thus be
|time - 3|. Notation like |when x = y|, as in the example here, is standard
practice but can be misleading in that if interpreted literally, it does not
necessarily define a set of only countably many points in time. For example,
|0 = 0| is always true, implying uncountably many occurrences, which would be
flawed.  However, as the signal |0 - 0| never \emph{crosses} 0, the actual
semantics is that this does not define any event occurrences at all.

The fourth kind of equation is signal relation application,
|EquationSigRelApp|, i.e. equations like |sr <> (x, y + 2)|. This brings all
equations of a signal relation into scope by instantiating them for the
expressions to which the relation is applied.

Finally, the representation of expressions is a standard first-order term
representation making it easy to manipulate expressions symbolically
(e.g. computing symbolic derivatives) and compiling expressions to
simulation code:
\begin{code}
data Expr =  ExprUnit  |  ExprReal Double  |  ExprVar String  |  ExprTime |
             ExprTuple Expr Expr [Expr]    |  ExprApp Function [Expr]

data Function = FuncDer |  FuncNeg |  FuncAdd  |  FuncnSub  |  FuncMul  |  ...
\end{code}

We use quasiquoting, a recent Haskell extension implemented in Glasgow Haskell
Compiler (GHC), to provide a convenient surface syntax for signal
relations. We have implemented a quasiquoter that takes a string in the
concrete syntax of Hydra and generates Haskell code that builds the signal
relation in the mixed-level representation described above. GHC executes the
quasiquoter for each string between the quasiquotes before type checking.

While the internal representation of a signal relation is untyped, Hydra
itself is typed, and we thus have to make sure that only type-correct Hydra
programs are accepted. As Hydra fragments are generated dynamically, during
simulation, we cannot postpone the type checking to after program
generation. Nor can we do it early, at quasiquoting time, at least not
completely, as no type information from the context around quasiquoted program
fragments are available (e.g., types of antiquoted Haskell expressions). In
the current version of Hydra, only domain specific scoping rules (e.g., all
constrained signal variables must be declared) are checked at the stage of
quasiquoting. Fortunately, the type system of the present version of Hydra is
fairly simple; in particular, Hydra is simply typed, so by using the standard
technique of phantom types, the part of the type checking that requires type
information outside the quasiquotes is delegated to the host language type
checker \cite{Rhiger2003a}.

A phantom type is a type whose type constructor has a parameter that is not
used in its definition. We define phantom type wrappers for the untyped
representation as follows:
\begin{code}
data SR a        =  SR        SigRel
data PatternT a  =  PatternT  Pattern
data ExprT a     =  ExprT     Expr
data E a
\end{code}

Phantom types can be used to restrict a function to building only type-correct
domain-specific terms. For example, a typed combinator |SIGREL| can be defined
in the following way:
\begin{code}
SIGREL :: PatternT a -> [Equation] -> SR a
SIGREL (PatternT p) eqs = SR (SigRel p eqs)
\end{code}
As can be seen, the type of the pattern that defines the interface of the
signal relation is what determines its type. Similarly, we define a typed combinator |switch|:
\begin{code}
switch :: (Eval b) => SR (a,E b) -> (b -> SR a) -> SR a
switch sr f = SigRelSwitch (forget sr) (forget . f . eval)
\end{code}
|E| is a type constructor with no constituent data constructors. It is used to
type patterns that introduce event signals. The data for the event signals are
constructed using event equations. Typed signal relations are translated into
their untyped counterparts using the following function:
\begin{code}
forget :: SR a -> SigRel
forget (SR sr) = sr
\end{code}
The |Eval| class method |eval| is used to translate a value carried by an
event occurrence (represented by an element of the value subset of
the type |Expr|) into the corresponding Haskell value:
\begin{code}
class Eval a where
	eval :: Expr -> a
\end{code}
Instances of the |Eval| type class are at present provided for |Double| and
arbitrarily nested tuples with fields of type |Double|.

A signal relation that is defined using the |switch| combinator is
structurally dynamic. However, the type of the |switch| combinator statically
guarantees that its type (i.e., its interface) remains unchanged. Thus, a
structurally dynamic signal relation can be used in a signal relation
application just like any other signal relation.

Well-typed equations are constructed using combinators in a similar way:
\begin{code}
equationEq :: ExprT a -> ExprT a -> Equation

equationSigRelApp :: SR a -> ExprT a -> Equation
\end{code}
Typed combinators for the remaining parts of the language, including |Pattern|
and |Expr|, are defined using the same technique.

Under the hood the representation is still untyped. However, if only the typed
combinators are exposed for building of signal relations, it is guaranteed
that only well-typed terms can be constructed. The quasiquoter of Hydra has
only access to typed combinators for building signal relations.

Symbolic transformations (e.g., symbolic differentiation and flattening) on
embedded language terms work with the untyped representation. These
transformations need to be programmed with care as the Haskell type checker
cannot verify that the transformations are type preserving.

Several type system extensions of Haskell (e.g., generalised algebraic data
types, existential types, and type families) make alternative techniques for
typing EDSLs possible. One alternative would be to directly construct signal
relations in a typed representation and implement the symbolic transformations
on the typed representation. While this approach requires more work from the
EDSL implementer, it provides additional correctness guarantees (e.g., the
Haskell type checker could be used to verify that transformations are type
preserving). We have not yet evaluated the suitability of the Haskell type
system for such an undertaking, opting for the simpler, untyped representation
for now.


\section{Simulation}
\label{sec:simulation}

In this section we describe how an iteratively staged Hydra program is
run. The process is illustrated in Fig. \ref{fig:simulation} and is
conceptually divided into four stages. In the first stage, a signal relation
is flattened and subsequently transformed into a mathematical representation
suitable for numerical simulation. In the second stage, this representation is
JIT compiled into efficient machine code. In the third stage, the compiled
code is passed to a numerical solver that simulates the system until the end
of simulation or an event occurrence. In the fourth stage, in the case of an
event occurrence, the event is analysed, a corresponding new signal relation
is computed and the process is repeated from the first stage. In the
following, each stage is described in more detail.

\begin{figure}[t]
\begin{center}
\includegraphics[scale=1.0]{Graphics/simulation}
\caption{\label{fig:simulation} Execution model of Hydra}
\end{center}
\end{figure}


\subsection{Symbolic Processing}
\label{sec:simulation-processing}

As a first step, all signal variables are renamed to give them distinct names.
This helps avoiding name clashes during flattening, unfolding of signal
relation application (see Sec. \ref{sec:background-hydrabyexample}), and thus
simplifies this process. Having carried out this preparatory renaming step,
all signal relation applications are unfolded until the signal relation is
completely flattened.

Further symbolic processing is then performed to transform the flattened
signal relation into a form that is suitable for numerical simulation. In
particular, derivatives of compound signal expressions are computed
symbolically. In the case of higher-order derivatives, extra variables and
equations are introduced to ensure that all derivatives in the flattened
system are first order.

While the numerical solver used in the current implementation handles
higher-index systems of equations, it is desirable to perform index reduction
symbolically at this stage as well \cite{Brenan1996a,Zimmer2009a}. Hydra does
not yet do this, but we intend to implement symbolic index reduction in the
future.

Finally, the following equations are generated at the end of the stage of
symbolic processing: 
\begin{eqnarray}
i(\frac{d\vec{x}}{dt},\vec{x},\vec{y},t) & = & 0 \label{init-eq} \\
f(\frac{d\vec{x}}{dt},\vec{x},\vec{y},t) & = & 0 \label{main-eq} \\
e(\frac{d\vec{x}}{dt},\vec{x},\vec{y},t) & = & 0 \label{event-eq}
\end{eqnarray}
Here, $\vec{x}$ is a vector of differential variables, $\vec{y}$ is a vector
of algebraic variables, and $t$ is time. Equation (\ref{init-eq}) determines
the initial conditions for (\ref{main-eq}); that is, the values of
$\frac{d\vec{x}}{dt}$,$\vec{x}$ and $\vec{y}$ at time $t = t_{0}$, the
starting time for the current set of equations. Equation (\ref{main-eq}) is
the main DAE of the system that is integrated in time starting from the
initial conditions. Equation (\ref{event-eq}) specifies the event conditions
(signals crossing 0).

\subsection{Just-in-time Compilation}

The generated equations are implicitly formulated ones: the mathematical
representation of non-causal signal relations. In general, it is not possible
to transform these implicit equations into explicit ones; i.e., to completely
causalise them \cite{Brenan1996a}. Consequently, a system of implicit
equations needs to be solved at the start of the simulation of each structural
configuration mode and at every integration step. For example, a numerical
solution of an implicitly formulated DAE (Equation \ref{eqMain}) involves
execution of the function $f$ a number of times (sometimes hundreds or more at
each integration step), with varying arguments, until it converges to zero.
The number of executions of $f$ depends on various factors including the
required precision, the initial guess, the degree of non-linearity of the DAE,
etc.

As the functions $i$, $f$, and $e$ are invoked from within inner loops of the
solver, they have to be compiled into machine code for efficiency: any
interpretive overhead here would be considered intolerable by practitioners
for most applications. However, as Hydra allows the equations to be changed in
arbitrary ways \emph{during} simulation, the equations have to be compiled
whenever they change, as opposed to only prior to simulation. (As an
optimisation, the code compiled for equations might be cached for future,
possible reuse: see below.) Our Hydra implementation employs JIT machine code
generation using the compiler infrastructure provided by LLVM. The functions
$i$, $f$ and $e$ are compiled into LLVM instructions that in turn are compiled
by the LLVM JIT compiler into native machine code. Function pointers to the
generated machine code are then passed to the numerical solver.

\subsection{Numerical Simulation}

The numerical suite used in the current implementation of Hydra is called
SUNDIALS \cite{Sundials2005}. The components we use are KINSOL, a nonlinear
algebraic equation systems solver, and IDA, a differential algebraic equation
systems solver. The code for the function $i$ is passed to KINSOL that
numerically solves the system and returns initial values (at time $t_{0}$) of
$\frac{d\vec{x}}{dt}$,$\vec{x}$ and $\vec{y}$. These vectors together with the
code for the functions $f$ and $e$ are passed to IDA that proceeds to solve
the DAE by numerical integration. This continues until either the simulation
is complete or until one of the events defined by the function $e$ occurs.
Event detection facilities are provided by IDA.

\subsection{Event Handling}
\label{sec:simulation-eventhandling}

At the moment of an event occurrence (one of the signals monitored by $e$
crossing 0), the numerical simulator terminates and presents the following
information to an event handler: Name of the event variable for which an event
occurrence has been detected, time $t_e$ of the event occurrence and
instantaneous values of the signal variables (i.e., values of
$\frac{d\vec{x}}{dt}$, $\vec{x}$ and $\vec{y}$ at time $t_e$).

The event handler traverses the original unflattened signal relation and finds
the event value expression (a signal-level expression) that is associated with
the named event variable. In the case of the breaking pendulum model, the
expression is |((x,y),(vx,vy))|. This expression is evaluated by substituting
the instantaneous values of the corresponding signals for the variables. The
event handler applies the second argument of the |switch| combinator (i.e.,
the function to compute the new signal relation to switch into) to the
functional-level event value. In the case of the breaking pendulum model, the
function |freeFall| is applied to the instantaneous value of
|((x,y),(vx,vy))|. The result of this application is a new signal relation.
The part of the original unflattened signal relation is updated by replacing
the old signal relation with the new one. The flat system of equations for the
previous mode and the machine code that was generated for it by the LLVM JIT
compiler are discarded. The simulation process for the updated model continues
from the first stage and onwards.

In the current implementation, the new signal relation is flattened and new
equations generated without reusing old ones from previous modes. In other
words, events are not treated locally. In addition, the state of the whole
system needs to be transferred for global and explicit reinitialisation of the
entire system at every event using a \emph{top level} switch, like in the
breaking pendulum example. We hope to address these issues in the future: see
Section \ref{sec:futurework}.

\subsection{Performance}
\label{sec:simulation-performance}

In previous work \cite{Giorgidze2009b}, we conducted benchmarks to evaluate
the performance of the proposed execution model. The initial results are
encouraging. For a small system with handful of equations (e.g., the breaking
pendulum) the total time spent on run-time symbolic processing and code
generation is only a couple of hundredth of a second. To get an initial
assessment of how well our approach scales, we also conducted a few large
scale benchmarks (thousands of equations). These demonstrated that the overall
performance of the execution model seems to scale well. In particular, time
spent on run-time symbolic processing and JIT compilation increased roughly
linearly in the number of equations for these tests. The results also
demonstrate that the time spent on JIT compilation dominates over the time
spent on run-time symbolic processing. Above all, our benchmarks indicated
that the time for symbolic processing and compilation remained modest in
absolute terms, and thus should be relatively insignificant compared with the
time for simulation in typical applications.

In the current implementation of Hydra, a new flat system of equations is
generated at each mode switch without reusing the equations of the previous
mode. It may be useful to identify exactly what has changed at each
mode switch, thus enabling the reuse of \emph{unchanged} equations and
associated code from the previous mode. In particular, this could reduce the
burden placed on the JIT compiler, which in our benchmarks accounted
for most of the switching overheads. Using such techniques, it may even be
feasible to consider our kind of approach for structurally dynamic
\emph{(soft) real-time} applications.   

In particular, information about the equations that remain unchanged during
the mode switches provides opportunities for the JIT compiler to reuse the
machine code from the previous mode, thus reducing the burden on the JIT
compiler and consequently the compilation time during mode switches. In
future, we are going to investigate reusable code generation aspects in the
context of non-causal modelling and simulation of structurally dynamic
systems and suitability of proposed execution model for real-time
simulation. Currently, for large systems, the implementation is only
suitable for offline simulation.

Our approach offers new functionality in that it allows non-causal modelling
and simulation of structurally dynamic systems that simply cannot be handled
by static approaches. Thus, when evaluating the feasibility of our approach,
one should weigh the inherent overheads against the limitation and
inconvenience of not being able to model and simulate such systems
non-causally.