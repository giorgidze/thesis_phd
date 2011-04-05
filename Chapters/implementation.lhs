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

\begin{code}
data Experiment = Experiment {
     timeStart             :: Double
  ,  timeStop              :: Double
  ,  timeStep              :: Double
  ,  jitCompile            :: Bool
  ,  symbolicProcessor     :: SymTab -> SymTab
  ,  numericalSolver       :: NumericalSolver
  ,  trajectoryVisualiser  :: CDouble -> CInt -> Ptr CDouble -> IO ()
  }
\end{code}

\begin{code}
defaultExperiment :: Experiment
defaultExperiment = Experiment {
     timeStart              = 0
  ,  timeStop               = 10
  ,  timeStep               = 0.001
  ,  jitCompile             = True
  ,  symbolicProcessor      = defaultSymbolicProcessor
  ,  numericalSolver        = defaultNumericalSolver
  ,  trajectoryVisualiser   = defaultTrajectoryVisualiser
  }
\end{code}


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

\begin{code}
data SymTab = SymTab {
     model         :: [Equation]
  ,  equations     :: [Equation]
  ,  events        :: [(Signal Bool)]
  ,  time          :: Double
  ,  instants      :: Array Int (Double,Double)
  }
\end{code}

\begin{code}
defaultSymbolicProcessor  ::  SymTab -> SymTab
defaultSymbolicProcessor  =   flattenEquations . flattenEvents . handleEvents
\end{code}

\begin{code}
handleEvents     ::  SymTab -> SymTab
handleEvents st  =   st {model = handleEvs (symtab, events st, model st)}
\end{code}

\begin{code}
handleEvs :: (SymTab,[Signal Bool],[Equation]) -> [Equation]
handleEvs (_,_,[])                                          =  []
handleEvs (st,evs,(Equal  _ _) : eqs)                       =  eq : handleEvs (st,evs,eqs)
handleEvs (st,evs,(Init   _ _) : eqs)                       =  handleEvs (st,evs,eqs)
handleEvs (st,evs,(Local f) : eqs)                          =
  Local (\s -> handleEvs (st,evs,f s)) : handleEvs (st,evs,eqs)
handleEvs (st,evs,(App (SR sr) s1 f) : eqs)                 =
  App (SR (\s2 -> handleEvs (st,evs,f s2))) s1 : handleEvs (st,evs,eqs)
handleEvs (st,evs,(App (Switch sr (SF sf) f) s) : eqs)      =
  if  elem   (sf s) evs
      then      App (f (eval (time st,instants st,s))) s : handleEvs (st,evs,eqs)
      else      App (Switch (SR (\ _ -> handleEvs (st,evs,[App sr s]))) (SF sf) f) s
             :  handleEvs (st,evs,eqs)
\end{code}

\begin{code}
flattenEvents :: SymTab -> SymTab
flattenEvents st = st {events = buildEvs (0,st{events = []},model st)} 
\end{code}

\begin{code}
buildEvs :: (Int,SymTab,[Equation]) -> SymTab
buildEvs (_,st,[])                                       = st
buildEvs (i,st,(Local f) : eqs)                          = buildEvs (i + 1,st,f (Var i) ++ eqs)
buildEvs (i,st,(Equal  _ _) : eqs)                       = buildEvs (i,st,eqs)
buildEvs (i,st,(Init   _ _) : eqs)                       = buildEvs (i,st,eqs)
buildEvs (i,st,(App (SR sr) s) : eqs)                    = buildEvs (i,st,sr s ++ eqs)
buildEvs (i,st,(App (Switch sr (SF sf) _) s) : eqs)      =
  buildEvs (i,st {events = (sf s) : (events st)},(App sr s) : eqs)
\end{code}

\begin{code}
flattenEquations :: SymTab -> SymTab
flattenEquations st = st {equations = flattenEqs (0,model st)}
\end{code}

\begin{code}
flattenEqs                                        ::  (Int,[Equation]) -> [Equation]
flattenEqs (_,[])                                 =   []
flattenEqs (i, (App (SR sr) s) : eqs)             =   flattenEqs (i,sr s ++ eqs)
flattenEqs (i, (App (Switch sr _ _) s) : eqs)     =   flattenEqs (i,(App sr s) : eqs)
flattenEqs (i, (Local f) : eqs)                   =   flattenEqs (i + 1,f (Var i) ++ eqs)
flattenEqs (i, (Equal _ _) : eqs)                 =   eq : flattenEqs (i,eqs)
flattenEqs (i, (Init _ _) : eqs)                  =   eq : flattenEqs (i,eqs)
\end{code}

\begin{code}
eval :: (Double,Array Int (Double,Double),Signal a) -> a
eval (_,_,Unit)                 = ()
eval (t,_,Time)                 = t
eval (_,_,Const c)              = c
eval (_,v,Var i)                = fst (v ! i)
eval (t,v,Pair e1 e2)           = (eval (t,v,e1),eval (t,v,e2))
eval (_,v,PrimApp Der (Var i))  = snd (v ! i)
eval (t,v,PrimApp sf e)         = (evalPrimSF sf) (eval (t,v,e))
\end{code}

\begin{code}
evalPrimSF :: PrimSF a b -> (a -> b)
evalPrimSF  Exp    = exp
evalPrimSF  Sqrt   = sqrt
evalPrimSF  Log    = log
evalPrimSF  Sin    = sin
evalPrimSF  Tan    = tan
evalPrimSF  Cos    = cos
evalPrimSF  Asin   = asin
evalPrimSF  Atan   = atan
evalPrimSF  Acos   = acos
evalPrimSF  Sinh   = sinh
evalPrimSF  Tanh   = tanh
evalPrimSF  Cosh   = cosh
evalPrimSF  Asinh  = asinh
evalPrimSF  Atanh  = atanh
evalPrimSF  Acosh  = acosh
evalPrimSF  Abs    = abs
evalPrimSF  Sgn    = signum
evalPrimSF  Add    = uncurry (+)
evalPrimSF  Mul    = uncurry (*)
evalPrimSF  Div    = uncurry (/)
evalPrimSF  Pow    = uncurry (**)
evalPrimSF  Lt     = \ d -> (d <  0)
evalPrimSF  Lte    = \ d -> (d <= 0)
evalPrimSF  Gt     = \ d -> (d >  0) 
evalPrimSF  Gte    = \ d -> (d >= 0)
evalPrimSF  Or     = uncurry (||)
evalPrimSF  And    =  uncurry (&&)
evalPrimSF  Not    = not
\end{code}


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

\section{Performance}
\label{secPerformance}

In this section we provide an initial performance evaluation of the current
prototype implementation of Hydra. We are mainly concerned with the overheads
of mode switching (computing new structural configurations at events, symbolic
processing of the equations, and JIT compilation) and how this scales when the
size of the models grow in order to establish the feasibility of our approach.
The time spent on numerical simulation is of less interest at this point: as
we are using standard numerical solvers, and as our model equations are
compiled down to native code with efficiency on par with statically generated
code (see section \ref{sec:background-llvm}), this aspect of the overall
performance should be roughly similar to what can be obtained from other
compilation-based modelling and simulation language implementations. For this
reason, and because other compilation-based, non-causal modelling and
simulation language implementations do not carry out dynamic reconfiguration,
we do not compare the performance to other simulation software. The results
would not be very meaningful.

The evaluation setup is as follows. The numerical simulator integrates the
system using variable-step, variable-order BDF (Backward Differentiation
Formula) solver \cite{Brenan1996a}. Absolute and relative tolerances for
numerical solution are set to $10^{-6}$ and trajectories are printed out at
every point where $t = 10^{-3} * k, k \ \epsilon \ \mathbb{N}$. For static
compilation and JIT compilation we use GHC 6.10.4 and LLVM 2.5
respectively. Simulations are performed on a 2.0\,GHz x86-64
Intel{\textregistered} Core{\texttrademark}2 CPU. However, presently, we do
not exploit any parallelism, running everything on a single core.

Let us first consider the model of the breaking pendulum from Section
\ref{sec:background-hydrabyexample}. We simulate it over the time interval
$t \ \epsilon \ [0,20]$, letting the pendulum break at $t = 10$. Table
\ref{table:breaking-pendulum} shows the amount of time spent simulating each
mode of the system, and within that how much time that is spent on
each of the four conceptual simulation process stages (see Section
\ref{sec:simulation}). As can be seen, most time (80--90\,\%) is
spent on numerical simulation, meaning the overheads of our dynamic
code generation approach was small in this case. Also, in absolute terms,
it can be seen that the amount of time spent on symbolic processing, JIT
compilation, and event handling was small, just fractions of a second.

\begin{table}[h]
\centering
\scriptsize
\begin{tabular}{|| p{1.1cm} || r || r || r || r ||}
  \hline

  & \multicolumn{2}{c||}{Pendulum}
  & \multicolumn{2}{c||}{Free Fall} \\

  & \multicolumn{2}{c||}{$t \  \epsilon \ [0, 10) $}
  & \multicolumn{2}{c||}{$t \  \epsilon \ [10, 20]$} \\ \hline

  & \multicolumn{2}{c||}{CPU Time}
  & \multicolumn{2}{c||}{CPU Time} \\ \hline

  & \multicolumn{1}{c||}{s} & \multicolumn{1}{c||}{\protect{\%}}
  & \multicolumn{1}{c||}{s} & \multicolumn{1}{c||}{\protect{\%}} \\ \hline

  Symbolic \mbox{Processing}  &  0.0001  &   0.2  &  0.0000  &  0.0   \\ \hline
  JIT \mbox{Compilation}      &  0.0110  &  18.0  &  0.0077  &  9.1   \\ \hline
  Numerical \mbox{Simulation} &  0.0500  &  81.8  &  0.0767  &  90.9  \\ \hline
  Event \mbox{Handling}       &  0.0000  &   0.0  &  -       &  -     \\ \hline \hline
  Total                       &  0.0611  &  100.0 &  0.0844  &  100.0 \\ \hline
\end{tabular}
\caption{
    \label{table:breaking-pendulum}
    Time profile of the breaking pendulum simulation
}
\end{table}

However, the breaking pendulum example is obviously very small (just a handful
of equations), and it only needs to be translated to simulation code twice: at
simulation start and when the pendulum breaks. To get an idea of how the
performance of the prototype implementation scales with an increasing number
of equations, we constructed a hybrid model of an RLC circuit (i.e., a circuit
consisting of resistors, inductors and capacitors) with dynamic structure. In
the first mode the circuit contains 200 components, described by 1000
equations in total (5 equations for each component). Every time $t = 10
* k$, where $k \ \epsilon \ \mathbb{N}$, the number of circuit components is
increased by 200 (and thus the number of equations by 1000) by switching the
additional components into the circuit.

\begin{table}[h]
\centering
\scriptsize

\begin{tabular}{|| p{1.1cm}
                || r@@{\hspace{2pt}} || r@@{\hspace{2pt}}
                || r@@{\hspace{2pt}} || r@@{\hspace{2pt}}
                || r@@{\hspace{2pt}} || r@@{\hspace{2pt}} ||}
  \hline

  & \multicolumn{2}{c||}{200  Components}
  & \multicolumn{2}{c||}{400  Components}
  & \multicolumn{2}{c||}{600  Components} \\

  & \multicolumn{2}{c||}{1000 Equations}
  & \multicolumn{2}{c||}{2000 Equations}
  & \multicolumn{2}{c||}{3000 Equations} \\

  & \multicolumn{2}{c||}{$t \ \epsilon \ [ 0,10) $}
  & \multicolumn{2}{c||}{$t \ \epsilon \ [10,20)$}
  & \multicolumn{2}{c||}{$t \ \epsilon \ [20,30)$} \\ \hline

  & \multicolumn{2}{c||}{CPU Time}
  & \multicolumn{2}{c||}{CPU Time}
  & \multicolumn{2}{c||}{CPU Time} \\ \hline

  & \multicolumn{1}{c||}{s} & \multicolumn{1}{c||}{\protect{\%}}
  & \multicolumn{1}{c||}{s} & \multicolumn{1}{c||}{\protect{\%}}
  & \multicolumn{1}{c||}{s} & \multicolumn{1}{c||}{\protect{\%}} \\ \hline

  Symbolic \mbox{Processing}  &  \ 0.063  \ &  \ 0.6   \  &  \ 0.147  \  & \ 0.6   \  &  \ 0.236  \  &  \ 0.5   \  \\ \hline
  JIT \mbox{Compilation}      &  \ 1.057  \ &  \ 10.2  \  &  \ 2.120  \  & \ 8.3   \  &  \ 3.213  \  &  \ 6.6   \  \\ \hline
  Numerical \mbox{Simulation} &  \ 9.273  \ &  \ 89.2  \  &  \ 23.228 \  & \ 91.1  \  &  \ 45.140 \  &  \ 92.9  \  \\ \hline
  Event \mbox{Handling}       &  \ 0.004  \ &  \ 0.0   \  &  \ 0.006  \  & \ 0.0   \  &  \ 0.008  \  &  \ 0.0   \  \\ \hline \hline
  Total                       &  \ 10.397 \ &  \ 100.0 \  &  \ 25.501 \  & \ 100.0 \  &  \ 48.598 \  &  \ 100.0 \  \\ \hline
\end{tabular}
\caption{
    \label{table:larger-system-1}
    Time profile of structurally dynamic RLC circuit simulation, part I
}
\end{table}

\begin{table}[h]
\centering
\scriptsize

\begin{tabular}{|| p{1.1cm}
                || r@@{\hspace{2pt}} || r@@{\hspace{2pt}}
                || r@@{\hspace{2pt}} || r@@{\hspace{2pt}}
                || r@@{\hspace{2pt}} || r@@{\hspace{2pt}} ||}
  \hline

  & \multicolumn{2}{c||}{800  Components}
  & \multicolumn{2}{c||}{1000 Components}
  & \multicolumn{2}{c||}{1200 Components} \\

  & \multicolumn{2}{c||}{4000 Equations}
  & \multicolumn{2}{c||}{5000 Equations}
  & \multicolumn{2}{c||}{6000 Equations} \\

  & \multicolumn{2}{c||}{$t \ \epsilon \ [30,40) $}
  & \multicolumn{2}{c||}{$t \ \epsilon \ [40,50)$}
  & \multicolumn{2}{c||}{$t \ \epsilon \ [50,60]$} \\ \hline

  & \multicolumn{2}{c||}{CPU Time}
  & \multicolumn{2}{c||}{CPU Time}
  & \multicolumn{2}{c||}{CPU Time} \\ \hline

  & \multicolumn{1}{c||}{s} & \multicolumn{1}{c||}{\protect{\%}}
  & \multicolumn{1}{c||}{s} & \multicolumn{1}{c||}{\protect{\%}}
  & \multicolumn{1}{c||}{s} & \multicolumn{1}{c||}{\protect{\%}} \\ \hline

  Symbolic \mbox{Processing}  &  \ 0.328  \ &  \ 0.4    \ &  \ 0.439   \ &  \ 0.4    \ &  \ 0.534   \ &  \ 0.3    \ \\ \hline
  JIT \mbox{Compilation}      &  \ 4.506  \ &  \ 4.9    \ &  \ 5.660   \ &  \ 5.1    \ &  \ 6.840   \ &  \ 4.3    \ \\ \hline
  Numerical \mbox{Simulation} &  \ 86.471 \ &  \ 94.7   \ &  \ 105.066 \ &  \ 94.5   \ &  \ 152.250 \ &  \ 95.4   \ \\ \hline
  Event \mbox{Handling}       &  \ 0.011  \ &  \ 0.0    \ &  \ 0.015   \ &  \ 0.0    \ &  \ -       \ &  \ -      \ \\ \hline \hline
  Total                       &  \ 91.317 \ &  \ 100.0  \ &  \ 111.179 \ &  \ 100.0  \ &  \ 159.624 \ &  \ 100.0  \ \\ \hline
\end{tabular}
\caption{
    \label{table:larger-system-2}
    Time profile of structurally dynamic RLC circuit simulation, part II
}
\end{table}

Tables \ref{table:larger-system-1} and \ref{table:larger-system-2} show the
amount of time spent in each mode of the system and in each conceptual stage
of simulation of the structurally dynamic RLC circuit. In absolute terms, it
is evident that the extra time spent on the mode switches becomes significant
as the system grows. However, in relative terms, the overheads of our dynamic
code generation approach remains low at about 10\,\% or less of the
overall simulation time.

While JIT compilation remains the dominating part of the time spent at mode
switches, Figure \ref{fig:benchmark} demonstrates that the performance of the
JIT compiler scales well. In particular, compilation time increases roughly
linearly in the number of equations. In addition, it should be noted that the
time spent on symbolic processing and event handling remains encouragingly
modest (both in relative and absolute terms) and grows slowly as model
complexity increases. There are also many opportunities for further
performance improvements: see Section \ref{sec:futurework} for
some possibilities.

\begin{figure}
\begin{center}
%include ../Graphics/benchmark.tex
\end{center}
\caption{
    \label{fig:benchmark}
    Plot demonstrating how CPU time spent on mode switches grows as number of
    equations increase in structurally dynamic RLC circuit simulation}
\end{figure}

Our approach offers new functionality in that it allows non-causal modelling
and simulation of structurally dynamic systems that simply cannot be handled
by static approaches. Thus, when evaluating the feasibility of our approach,
one should weigh the overheads against the limitation and inconvenience of not
being able to model such systems non-causally.

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