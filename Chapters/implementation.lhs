\chapter{Implementation of Hydra}
\label{chapImplementation}

\section{Embedding}
\label{sec:embedding}

Hydra is implemented as a Haskell embedded DSL. We use quasiquoting, a recent
Haskell extension implemented in Glasgow Haskell Compiler (GHC), to provide a
convenient surface (i.e., concrete) syntax for Hydra. The implementation uses
quasiquoting to generate the typed representation of Hydra models from strings
in the concrete syntax. Functions performing the aforementioned
transformation, so called quasiquoters, are specified in the opening
quasiquotes. In GHC, a quasiquoter generates Haskell code using using Template
Haskell \cite{Sheard2002}.

GHC executes quasiquoters before type checking, at Haskell compile time. As
the typed intermediate representation fully embodies the type system of Hydra,
we effectively delegate the task of type checking to the host language type
checker. This approach reduces the language specification and implementation
effort by reusing the host language type system and the host language type
checker. However, the disadvantage of this approach is the fact that typing
errors are not domain specific.

The implementation of Hydra provides two quasiquoters: the |rel| quasiquoter
for generating typed signal relations, and the |fun| quasiquoter for
generating typed signal functions. The implementation of the quasiquoters is
broken down into three stages: pasring, desugaring and translation into the
typed representation.

Firstly, the string in the concrete syntax of Hydra is parsed and the
corresponding untyped representation is generated as an abstract syntax tree
(AST). The BNF Converter (BNFC), a compiler front-end generator from a
Labelled BNF grammar \cite{BNFC2004}, is used to generate the parser and the
AST data type. The syntax and the AST data type is exactly the same as given
in the language definition in Chapter \ref{chapDefinition}. In addition, we
use BNFC to generate a layout resolver allowing for list of equations in |rel|
quasiquotes to be constructed without curly braces and semicolons. The layout
rules are the same as for Haskell.

Secondly, the untyped representation is desugarred exactly as it is presented
in the language definition (see Chapter \ref{chapDefinition}).

Thirdly, the desugared untyped representation is translated into the typed
representation. This step involves generation of Haskell code using Template
Haskell \cite{Sheard2002}.

We illustrate the quasiquoting process using the following signal relation
modelling the parametrised van der Pol oscillator:

\begin{code}
vanDerPol :: Double -> SR ()
vanDerPol mu = [rel| () ->
    local x y
    init (x,y) = (1,1)
    der x = y 
    der y =  - x + $mu$ * (1 - x * x) * y
|]
\end{code}

After the parsing stage the quasiquoted signal relation turns into the
following AST:

\begin{code}
SigRel  PatUnit
        [  EquLocal   (Ident "x") [Ident "y"]
        ,  EquInit    (ExprPair (ExprVar (Ident "x")) (ExprVar (Ident "y")))
                      (ExprPair (ExprInteger 1) (ExprInteger 1))
        ,  EquEqual   (ExprApp (ExprVar (Ident "der")) (ExprVar (Ident "x")))
                      (ExprVar (Ident "y"))
        ,  EquEqual   (ExprApp (ExprVar (Ident "der")) (ExprVar (Ident "y")))
                      (ExprAdd  (ExprNeg (ExprVar (Ident "x")))
                                (ExprMul  (ExprMul  (ExprAnti (HsExpr "$mu$"))
                                                    (ExprSub  (ExprInteger 1)
                                                              (ExprMul  (ExprVar (Ident "x"))
                                                                        (ExprVar (Ident "x")))))
                                          (ExprVar (Ident "x"))))
        ]
\end{code}

After the desugaring stage we get the following AST:

\begin{code}
SigRel  PatUnit
        [  EquLocal   (Ident "x") []
        ,  EquLocal   (Ident "y") []
        ,  EquInit    (ExprVar (Ident "x")) (ExprInteger 1)
        ,  EquInit    (ExprVar (Ident "y")) (ExprInteger 1)
        ,  EquEqual   (ExprApp (ExprVar (Ident "der")) (ExprVar (Ident "x")))
                      (ExprVar (Ident "y"))
        ,  EquEqual   (ExprApp (ExprVar (Ident "der")) (ExprVar (Ident "y")))
                      (ExprAdd  (ExprNeg (ExprVar (Ident "x")))
                                (ExprMul  (ExprMul  (ExprAnti (HsExpr "$mu$"))
                                                    (ExprSub  (ExprInteger 1)
                                                              (ExprMul  (ExprVar (Ident "x"))
                                                                        (ExprVar (Ident "x")))))
                                          (ExprVar (Ident "x"))))

        ]
\end{code}

After translation into the typed representation we get the following typed
representation:

\begin{code}
SR  (\() ->  [Local (\x -> [Local (\y ->  [  Init x (Const 1.0)
                                          ,  Init y (Const 1.0)
                                          ,  Equal (PrimApp Der x) y
                                          ,  Equal  (PrimApp Der y)
                                                    ((-x) + (Const mu) * ((Const 1.0) - x * x) * y)
                                          ])])])
\end{code}

Let me overview the typed representation once again, to define the |switch|
combinator in terms of the corresponding constructor of the typed
representation, to illustrate a minor change in the typed representation for
the implementation purposes, and to draw your attention on the mixed-level
embedding techniques used in the implementation.

There are two ways to form a signal relation: either from equations that
constrain the given signal, or by composing signal relations temporally:

\begin{code}
data SR a where
  SR      ::  (Signal a -> [Equation]) -> SR a
  Switch  ::  SR a -> SF a Bool -> (a -> SR a) -> SR a
\end{code}

The constructor |SigRel| forms a signal relation from a function that takes a
signal and returns a list of equations constraining the given signal. This
list of equations constitute a system of Differential Algebraic Equations
(DAEs) that defines the signal relation by expressing constraints on the
signal. Having said that, the system is not necessarily a static one as the
equations may refer to signal relations that contain switches.

The |switch|-combinator, which is used in \label{chapHydra}, forms a signal
relation by temporal composition of two signal relations. Internally, such a
temporal composition is represented by a signal relation constructed by the
|Switch| constructor:

\begin{code}
switch :: SR a -> SF a Bool -> (a -> SR a) -> SR a
switch = Switch
\end{code}

There are four kinds of equations:
\begin{code}
data Equation where
  Local :: (Signal Double -> [Equation]) -> Equation
  Equal :: Signal Double -> Signal Double -> Equation
  Init  :: Signal Double -> Signal Double -> Equation
  App   :: SR a -> Signal a -> Equation
\end{code}

The |Local| constructor constructs equations that merely introduce local
signals. As it is evident from the language definition, such signals can be
constrained only by the equations that are returned by the function that is
the first argument of the |Local| constructor. This is also enforced by the
language implementation, as we will see later in this chapter. Note that
equation generator functions in the |SigRel| constructor are allowed to be
passed a signal that is constrained elsewhere using the signal relation
application.

Initialisation equations, constructed by |Init|, provide initial conditions.
They are only in force when a signal relation instance first becomes active
(for example, equations like |init (x,y) = (1,1)|).

Equations constructed by |Equal| are basic equations imposing the constraint
that the valuations of the two signals have to be equal for as long as the
containing signal relation instance is active (for example, equations like
|der x = y|).

The fourth kind of equation is signal relation application, |App|, i.e.
equations like |sr <> (x, y + 2)|. This brings all equations of a signal
relation into scope by instantiating them for the expressions to which the
relation is applied.

The typed representation of signals is a standard first-order representation
making it easy to manipulate signal expressions symbolically and compiling
signal expressions to simulation code:

\begin{code}
data Signal a where
  Unit     ::  Signal ()
  Time     ::  Signal Double
  Const    ::  a -> Signal a
  Pair     ::  Signal a -> Signal b -> Signal (a,b)
  PrimApp  ::  PrimSF a b -> Signal a -> Signal b
  Var      ::  Integer -> Signal Double
\end{code}

As you can see, this data type definition contains one more constructor,
namely the |Var| constructor. This constructor is not used at the stage of
quasiquoting. Instead, the constructor is used later on at the stage of
flattening to instantiate each local signal variable to unique signal variable
by using the constructor's |Integer| field.

The implementation of Hydra supports the same set of primitive functions as
defined in the language definition. Hence, in the implementation we use the
same |PrimSF| data type as given in the language definition.

Note the use of a mixture of shallow and deep techniques of embedding. The
embedded functions in the |SR|, |Switch|, |Local| and |App| constructors
correspond to the shallow part of the embedding. The rest of the data
constructors, namely, |Equal|, |Init|, and all constructors of the |Signal|
data type correspond to the deep part of the embedding, providing an explicit
representation of language terms for further symbolic processing and
ultimately compilation. As we will see in more detail below, each mode of
operation can be described as a flat list of equations where each equation is
constructed, either, by the |Init| constructor or by the |Equal| constructor.
It is this representation that allows for generation of efficient simulation
code. This combination of the two embedding techniques allowed us to leverage
shallow embedding for high-level aspects of the embedded language, such as
equation generation and temporal composition, and deep embedding for low-level
aspects of the embedded language, such as simulation code generation for
efficiency.

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