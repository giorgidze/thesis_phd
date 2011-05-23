\chapter{Implementation of Hydra}
\label{chapImplementation}

This chapter describes how Hydra is embedded in Haskell and how embedded
noncausal models are simulated. Performance of the the simulator is evaluated
by focussing on the implementation aspects that are absent from main-stream,
noncausal language implementations (i.e., runtime symbolic processing and JIT
compilation).

\section{Embedding}
\label{secEmbedding}

Hydra is implemented as a Haskell embedded DSL. We use quasiquoting, a recent
Haskell extension implemented in Glasgow Haskell Compiler (GHC), to provide a
convenient surface (i.e., concrete) syntax for Hydra. The implementation uses
quasiquoting to generate the typed representation of Hydra models from strings
in the concrete syntax. A function performing the aforementioned
transformation, a so-called quasiquoter, is specified in the opening
quasiquote. In GHC, a quasiquoter generates Haskell code using using the
Template Haskell \citep{Sheard2002} representation of Haskell programs.
Template Haskell is a compile-time meta programming facility for Haskell
implemented in GHC.

GHC executes a quasiquoter before type checking, at Haskell compile time. As
the typed abstract syntax of Hydra fully embodies the type system of Hydra, we
effectively delegate the task of type checking to the host language type
checker. This approach reduces the language specification and implementation
effort by reusing the host language type system and the host language type
checker. However, the disadvantage of this approach is the fact that typing
errors are not domain specific.

The implementation of Hydra provides two quasiquoters: the |rel| quasiquoter
for generating typed signal relations, and the |fun| quasiquoter for
generating typed signal functions. The implementation of the quasiquoters is
broken down into three stages: parsing, desugaring and translation into the
typed abstract syntax.

Firstly, the string in the concrete syntax of Hydra is parsed and the
corresponding untyped representation is generated as an abstract syntax tree
(AST). The BNF Converter (BNFC), a compiler front-end generator from a
labelled BNF grammar \citep{BNFC2004}, is used to generate the parser and the
AST data type. The labelled BNF grammar of Hydra is given in Figure
\ref{figGrammar}. The syntax that the generated parser implements and the
generated AST data type is exactly the same as given in the language
definition in Chapter \ref{chapDefinition}. In addition, we use BNFC to
generate Hydra's layout resolver allowing for list of equations in |rel|
quasiquotes to be constructed without curly braces and semicolons. The layout
rules are the same as for Haskell.

\begin{figure}
\begin{verbatim}
entrypoints SigRel, SigFun;

SigRel.         SigRel    ::= Pattern "->" "{" [Equation] "}" ;

SigFun.         SigFun    ::= Pattern "->" "{" Expr       "}" ;

PatWild.        Pattern   ::= "_" ;
PatName.        Pattern   ::= Ident ;
PatUnit.        Pattern   ::= "()" ;
PatPair.        Pattern   ::= "(" Pattern "," Pattern ")" ;

EquEqual.       Equation  ::=        Expr "=" Expr ;
EquInit.        Equation  ::= "init" Expr "=" Expr ;
EquLocal.       Equation  ::= "local" Ident;
EquSigRelApp.   Equation  ::= HsExpr "<>" Expr ;

ExprOr.         Expr1     ::= Expr1 "||" Expr2 ;
ExprAnd.        Expr2     ::= Expr2 "&&" Expr3 ;
ExprLt.         Expr3     ::= Expr4 "<"  Expr4 ;
ExprLte.        Expr3     ::= Expr4 "<=" Expr4 ;
ExprGt.         Expr3     ::= Expr4 ">"  Expr4 ;
ExprGte.        Expr3     ::= Expr4 ">=" Expr4 ;
ExprAdd.        Expr4     ::= Expr4 "+"  Expr5 ;
ExprSub.        Expr4     ::= Expr4 "-"  Expr5 ;
ExprDiv.        Expr5     ::= Expr5 "/"  Expr6 ;
ExprMul.        Expr5     ::= Expr5 "*"  Expr6 ;
ExprPow.        Expr6     ::= Expr6 "^"  Expr7 ;
ExprNeg.        Expr6     ::= "-" Expr7 ;
ExprApp.        Expr7     ::= Expr7 Expr8 ;
ExprVar.        Expr8     ::= Ident ;
ExprAnti.       Expr8     ::= HsExpr ;
ExprInteger.    Expr8     ::= Integer ;
ExprDouble.     Expr8     ::= Double ;
ExprUnit.       Expr8     ::= "()" ;
ExprPair.       Expr8     ::= "(" Expr "," Expr ")" ;
_.              Expr      ::= Expr1 ;
_.              Expr1     ::= Expr2 ;
_.              Expr2     ::= Expr3 ;
_.              Expr3     ::= Expr4 ;
_.              Expr4     ::= Expr5 ;
_.              Expr5     ::= Expr6 ;
_.              Expr6     ::= Expr7 ;
_.              Expr7     ::= Expr8 ;
_.              Expr8     ::= "(" Expr ")" ;

separator Equation ";" ;

comment "--" ;
comment "{-" "-}" ;

token HsExpr ('$' (char - '$')* '$') ;

layout "->" ;
\end{verbatim}

\caption{\label{figGrammar} Labelled BNF grammar of Hydra. This labelled BNF
grammar is used to generate Hydra's parser, untyped abstract syntax and layout
resolver.}

\end{figure}

Secondly, the untyped representation is desugared exactly as it is presented
in the language definition (see Chapter \ref{chapDefinition}).

Finally, the desugared untyped representation is translated into the typed
representation. This step implements the corresponding translation rules given
in Chapter \ref{chapDefinition}. Note that the translation rules generate
Haskell code. This is implemented by using the Template Haskell facility of
GHC.

We illustrate the quasiquoting process using the following signal relation
that models a parametrised van der Pol oscillator. The oscillator model is
given in Figure \ref{figVanDerPol}. After the parsing stage the quasiquoted
signal relation turns into the AST that is given in Figure
\ref{figVanDerPolAst}. After the desugaring stage we get the AST that is given
in Figure \ref{figVanDerPolAstDesugar}. After translation into the typed
representation we get the typed AST that is given in Figure
\ref{figVanDerPolAstTyped}.

\begin{figure}

\begin{code}
vanDerPol :: Double -> SR ()
vanDerPol mu = [rel| () ->
    local x y
    init (x,y) = (1,1)
    der x = y 
    der y =  - x + $mu$ * (1 - x * x) * y
|]
\end{code}

\caption{\label{figVanDerPol} Signal relation modelling a parametrised van der
Pol oscillator.}

\end{figure}

\begin{figure}
\small
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
                                                              (ExprMul
                                                                 (ExprVar (Ident "x"))
                                                                 (ExprVar (Ident "x")))))
                                          (ExprVar (Ident "x"))))
        ]
\end{code}

\caption{\label{figVanDerPolAst} Untyped abstract syntax tree representing the
|vanDerPol| signal relation.}

\end{figure}

\begin{figure}
\small
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
                                                              (ExprMul
                                                                 (ExprVar (Ident "x"))
                                                                 (ExprVar (Ident "x")))))
                                          (ExprVar (Ident "x"))))

        ]
\end{code}

\caption{\label{figVanDerPolAstDesugar} Desugared, untyped abstract syntax
tree representing the |vanDerPol| signal relation.}

\end{figure}

\begin{figure}
\small
\begin{code}
SR  (\() ->
      [Local (\x ->
              [Local (\y -> 
                      [  Init x (Const 1.0)
                      ,  Init y (Const 1.0)
                      ,  Equal (PrimApp Der x) y
                      ,  Equal  (PrimApp Der y)
                                (PrimApp
                                   Add
                                   (Pair  (PrimApp Neg x)
                                          (PrimApp
                                             Mul
                                             (Pair  (PrimApp
                                                       Mul
                                                       (Pair  (Const mu)
                                                              (PrimApp
                                                                 Sub
                                                                 (Pair  (Const 1.0)
                                                                        (PrimApp Mul (Pair x x)))))))
                                             y)))


                      ])])])
\end{code}

\caption{\label{figVanDerPolAstTyped} Typed abstract syntax tree representing
the |vanDerPol| signal relation.}

\end{figure}

Let us briefly overview the typed abstract syntax used in the implementation
of Hydra. This is to highlight a minor difference from the typed abstract
syntax that was presented in the language definition in Chapter
\ref{chapDefinition} and to draw your attention on the mixed-level embedding
techniques used in the implementation.

The typed abstract syntax allows for two ways to form a signal relation:
either from equations that constrain the given signal, or by composing signal
relations temporally:

\begin{code}
data SR a where
  SR      ::  (Signal a -> [Equation]) -> SR a
  Switch  ::  SR a -> SF a Bool -> (a -> SR a) -> SR a
\end{code}

The constructor |SigRel| forms a signal relation from a function that takes a
signal and returns a list of equations constraining the given signal. This
list of equations constitute a system of Differential Algebraic Equations
(DAEs) that defines the signal relation by expressing constraints on the
signal. he system of equations is not necessarily a static one as the
equations may refer to signal relations that contain switches.

The |switch|-combinator, which was introduced in Chapter \ref{chapHydra},
forms a signal relation by temporal composition of two signal relations.
Internally, in the implementation of Hydra, such a temporal composition is
represented by a signal relation constructed by the |Switch| constructor:

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

Initialisation equations, constructed by |Init|, state initial conditions.
They are only in force when a signal relation instance first becomes active
(for example, equations like |init (x,y) = (1,1)|).

Equations constructed by |Equal| are basic equations imposing the constraint
that the valuations of the two signals have to be equal for as long as the
containing signal relation instance is active (for example, equations like
|der x = y|).

The fourth kind of equation is signal relation application, |App|; that is,
equations like |sr <> (x, y + 2)|. The application constrains the given
signals by the equations defined in the signal relation.

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

As you can see, this data type definition contains one constructor that is not
featured in the language definition, namely the |Var| constructor. This
constructor is not used at the stage of quasiquoting. Instead, the constructor
is used later on at the stage of flattening to instantiate each local signal
variable to unique signal variable by using the constructor's |Integer| field.

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

In this section we describe how iteratively staged Hydra morels are simulated.
The process is conceptually divided into four stages as illustrated in Figure
\ref{figSimulation}. In the first stage, a signal relation is flattened and
subsequently transformed into a mathematical representation suitable for
numerical simulation. In the second stage, this representation is JIT compiled
into efficient machine code. In the third stage, the compiled code is passed
to a numerical solver that simulates the system until the end of simulation or
an event occurrence. In the fourth stage, in the case of an event occurrence,
the event is analysed, a corresponding new signal relation is computed and the
process is repeated from the first stage.


\begin{figure}[t]
\begin{center}
\includegraphics[width = \textwidth]{Graphics/simulation}
\end{center}
\caption{\label{figSimulation} Execution model of Hydra.}
\end{figure}

Before we describe the four stages of the simulation in detail. Let us briefly
overview a function that performs this four stages. Note that the simulator
performs the aforementioned four stages iteratively at each structural change.
The |simulate| function, which performs the simulation, has the following type
signature:
\begin{code}
simulate :: SR () -> Experiment -> IO ()
\end{code}
The function takes a signal relation and an experiment description and
simulates the system. The |Experiment| data type is defined in Figure
\ref{figExperiment}. The |timeStart| field specifies the simulation starting
time. The |timeStop| field specifies the simulation stopping time. The
|timeStep| field specifies the simulation time step. When the |jitCompile|
field is set to |True|, the simulator JIT compiles signal relations down to
the machine code for the architecture that the simulation is running on even
for the dynamically generated signal relations. When the |jitCompile| field is
set to |False|, the simulator makes use of interpretation instead of JIT
compilation. The |symbolicProcessor| specifies the simulator's runtime
symbolic processor. The |numericalSolver| specifies the simulator's numerical
solver. The |trajectoryVisualiser| specifies how to visualise the simulation
results (i.e., change of signal values over time). The data type definitions
for |SymTab|, |NumericalSover| and |TrajectoryVisualiser| are given later on
in this chapter.

The implementation of Hydra of Hydra provides the default experiment
configuration that is given in Figure \ref{figDefaultExperiment}. Note that,
the last three fields of the experiment description record are expected to be
modified by expert users willing to provide there own runtime symbolic
processor and numerical solvers. The behaviour of the
|defaultSymbolicProcessor|, |defaultNumericalSolver| and
|defaultTrajectoryVisualiser| are described in detail later in this chapter.

\begin{figure}
\begin{code}
data Experiment = Experiment {
     timeStart             :: Real
  ,  timeStop              :: Real
  ,  timeStep              :: Real
  ,  jitCompile            :: Bool
  ,  symbolicProcessor     :: SymTab -> SymTab
  ,  numericalSolver       :: NumericalSolver
  ,  trajectoryVisualiser  :: TrajectoryVisualiser
  }
\end{code}

\caption{\label{figExperiment} Data type for experiment descriptions.}

\end{figure}

\begin{figure}
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

\caption{\label{figDefaultExperiment} Default experiment description.}

\end{figure}

\section{Symbolic Processing}
\label{secSymbolicProcessing}

% As a first step, all signal variables are renamed to give them distinct names.
% This helps avoiding name clashes during flattening, unfolding of signal
% relation application (see Sec. \ref{sec:background-hydrabyexample}), and thus
% simplifies this process. Having carried out this preparatory renaming step,
% all signal relation applications are unfolded until the signal relation is
% completely flattened.

% Further symbolic processing is then performed to transform the flattened
% signal relation into a form that is suitable for numerical simulation. In
% particular, derivatives of compound signal expressions are computed
% symbolically. In the case of higher-order derivatives, extra variables and
% equations are introduced to ensure that all derivatives in the flattened
% system are first order.

% While the numerical solver used in the current implementation handles
% higher-index systems of equations, it is desirable to perform index reduction
% symbolically at this stage as well \citep{Brenan1996a,Zimmer2009a}. Hydra does
% not yet do this, but we intend to implement symbolic index reduction in the
% future.

% Finally, the following equations are generated at the end of the stage of
% symbolic processing: 
% \begin{eqnarray}
% i(\frac{d\vec{x}}{dt},\vec{x},\vec{y},t) & = & 0 \label{init-eq} \\
% f(\frac{d\vec{x}}{dt},\vec{x},\vec{y},t) & = & 0 \label{main-eq} \\
% e(\frac{d\vec{x}}{dt},\vec{x},\vec{y},t) & = & 0 \label{event-eq}
% \end{eqnarray}
% Here, $\vec{x}$ is a vector of differential variables, $\vec{y}$ is a vector
% of algebraic variables, and $t$ is time. Equation (\ref{init-eq}) determines
% the initial conditions for (\ref{main-eq}); that is, the values of
% $\frac{d\vec{x}}{dt}$,$\vec{x}$ and $\vec{y}$ at time $t = t_{0}$, the
% starting time for the current set of equations. Equation (\ref{main-eq}) is
% the main DAE of the system that is integrated in time starting from the
% initial conditions. Equation (\ref{event-eq}) specifies the event conditions
% (signals crossing 0).

In this section we describe the first stage performed by the simulator,
symbolic processing. A symbolic processor is a function from symbol table to
symbol table. The symbol table data type that used in the implementation of
Hydra is given in Figure \ref{figSymTab}. The symbol table record has five
fields.

The |model| field is for a top-level signal relation that is active. At the
start of the simulation, the |simulate| function places application its first
argument of type |SR ()| to the |Unit| signal. In other words, the |model|
field contains a system hierarchical equations with signal relation
applications and temporal compositions that is currently active.

The |equations| field is for a flat list of equations that describe an active
mode of operation. By flat we meant that the list of equations only contain
|Init| and |Equal| equations. At the start of the simulation, the |simulate|
functions places an empty list in this field.

The |events| filed is for a list of event occurrences. Recall the the type
signature of the |switch| combinator. A signal function that detects events
returns a boolean signal. The simulator places the boolean signal expressions
that describe an event occurrence at each structural change. Initially, at the
start of the simulation, the simulator places an empty list in the |events|
field of the symbol table.

The |time| field is for a current time. Initially the simulator places the
starting time given in the experiment description. This filed is modified at
each structural change with the time of an event occurrence.

The instance field is for storing instantaneous of real values signals. The
simulator stores instantaneous values of active signals at each structural
change. The instantaneous real values are stored as an array of pairs of
reals. The first field of the pair is stores the instantaneous signal value,
while the second field of the pair stores the instantaneous value of the
signal's differential.

\begin{figure}
\begin{code}
data SymTab = SymTab {
     model         :: [Equation]
  ,  equations     :: [Equation]
  ,  events        :: [(Signal Bool)]
  ,  time          :: Double
  ,  instants      :: Array Integer (Real,Real)
  }
\end{code}

\caption{\label{figSymTab} Data type for symbol tables.}

\end{figure}

The task of the symbolic processor is to handle occurred events by modifying
the |model| field, to generate a flat list of events that may occur in the
active mode of operation by updating , and to generate flat list of equations
describing to the active mode by updating the |equations| field. The
implementation of Hydra provides the default symbolic processor that is
defined as follows:

\begin{code}
defaultSymbolicProcessor  ::  SymTab -> SymTab
defaultSymbolicProcessor  =   flattenEquations . flattenEvents . handleEvents
\end{code}

As you can see, the default symbolic processor is defined as a composition of
three symbolic processing steps. The first step handles occurred events by
modifying the |model| field of the symbol table. The event handler is defined
in Figure \ref{figHandleEvents}. The second step generates a list of boolean
signal expression representing the list of possible events in the active mode
of operation as defined in Figure \ref{figFlattenEvents}. Note that this step
involves evaluation of the instantaneous signal values by using the |eval|
function the |time| and |instants| fields of the symbol table. The |eval|
function is defined in Figure \ref{figEval}. The third step flattens the
hierarchical system of equations placed in the |model| field of the symbol
table into the |equations| field of the symbol table. The flat list only
contains |Init| and |Equal| equations. The |Equal| equations define the DAE
that describes the active mode of operation. The |Init| equations describe the
initial conditions for the DAE. The flattening transformation is given in
Figure \ref{figFlattenEquations}.

\begin{figure}

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

\caption{\label{figHandleEvents} Function that handles events.}

\end{figure}

\begin{figure}
\begin{code}
flattenEvents :: SymTab -> SymTab
flattenEvents st = st {events = buildEvs (0,st{events = []},model st)} 
\end{code}

\begin{code}
buildEvs :: (Integer,SymTab,[Equation]) -> SymTab
buildEvs (_,st,[])                                       = st
buildEvs (i,st,(Local f) : eqs)                          = buildEvs (i + 1,st,f (Var i) ++ eqs)
buildEvs (i,st,(Equal  _ _) : eqs)                       = buildEvs (i,st,eqs)
buildEvs (i,st,(Init   _ _) : eqs)                       = buildEvs (i,st,eqs)
buildEvs (i,st,(App (SR sr) s) : eqs)                    = buildEvs (i,st,sr s ++ eqs)
buildEvs (i,st,(App (Switch sr (SF sf) _) s) : eqs)      =
  buildEvs (i,st {events = (sf s) : (events st)},(App sr s) : eqs)
\end{code}

\caption{\label{figFlattenEvents} Function that generates the flat list of
events that may occur in the active mode of operation.}

\end{figure}

\begin{figure}
\begin{code}
eval :: (Double,Array Integer (Double,Double),Signal a) -> a
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
evalPrimSF  And    = uncurry (&&)
evalPrimSF  Not    = not
\end{code}

\caption{\label{figEval} Functions that evaluate instantaneous signal values.}

\end{figure}


\begin{figure}
\begin{code}
flattenEquations :: SymTab -> SymTab
flattenEquations st = st {equations = flattenEqs (0,model st)}
\end{code}

\begin{code}
flattenEqs                                        ::  (Integer,[Equation]) -> [Equation]
flattenEqs (_,[])                                 =   []
flattenEqs (i, (App (SR sr) s) : eqs)             =   flattenEqs (i,sr s ++ eqs)
flattenEqs (i, (App (Switch sr _ _) s) : eqs)     =   flattenEqs (i,(App sr s) : eqs)
flattenEqs (i, (Local f) : eqs)                   =   flattenEqs (i + 1,f (Var i) ++ eqs)
flattenEqs (i, (Equal _ _) : eqs)                 =   eq : flattenEqs (i,eqs)
flattenEqs (i, (Init _ _) : eqs)                  =   eq : flattenEqs (i,eqs)
\end{code}

\caption{\label{figFlattenEquations} Functions that flatten hierarchical
systems of equations.}

\end{figure}

Each of the three steps of the default symbolic processor has compact and
self-explanatory definition, especially, the |flattenEquations| function. To
my knowledge, this is the shortest formal and executable definition of the
flattening process for noncausal modelling languages. This is partly due to
the simple abstract syntax and utilisation of shallow embedding techniques,
specifically, embedded functions in the |SR| and |Switch| constructors.

The default symbolic processor that is described in this section can be
extended by modellers. This extensibility is especially useful for providing
further symbolic processing steps that operate on flattened equations. For
example, the default symbolic processor does not implement the index reduction
transformation \citep{Cellier2006}. The index reduction transformation
minimises algebraic dependencies between equations involved in the flat DAE.
This allows numerical solvers to more efficiently simulate the DAE
\citep{Brenan1996a}. A good overview of index reduction algorithms is given in
the book by \citet{Cellier2006}. One of those algorithms can be used to extend
the default symbolic processor by running the transformation after the
|flattenEquations| step.

As an example of a symbolic processor extension the implementation of Hydra
provides a processor that handles higher-order derivatives and derivatives of
complex signal expressions (i.e., not just signal variables). Equations that
involve higher-order derivatives are translated into equivalent set of
equations involving only first-order derivatives. Derivatives of complex
signal expressions are handled using symbolic differentiation. The to steps
ensure that in the flattened model only first order derivatives of signal
variables appear.


\section{Just-in-time Compilation}

Mathematically the end result of the stage of symbolic processing is the
following list of equations:

\begin{eqnarray}
i(\frac{d\vec{x}}{dt},\vec{x},\vec{y},t) & = & \vec{r_i} \label{init-eq} \\
f(\frac{d\vec{x}}{dt},\vec{x},\vec{y},t) & = & \vec{r_f} \label{main-eq} \\
e(\frac{d\vec{x}}{dt},\vec{x},\vec{y},t) & = & \vec{r_e} \label{event-eq}
\end{eqnarray}

Here, $\vec{x}$ is a vector of differential variables, $\vec{y}$ is a vector
of algebraic variables, $t$ is time, $\vec{r_i}$ is a residual vector of
initialisation equations, $\vec{r_f}$ is a residual vector of differential
algebraic equations, and $\vec{r_e}$ is a vector of boolean signal values. The
aforementioned vectors of signals; that is, time-varying vectors.

Equation \ref{init-eq} corresponds to |Init| equations that are placed in the
|equations| field of the symbol table and determines the initial conditions
for (\ref{main-eq}); that is, the values of $\frac{d\vec{x}}{dt}$,$\vec{x}$
and $\vec{y}$ at time $t = t_{0}$, the starting time for the current set of
equations. Equation \ref{main-eq} corresponds to |Equal| equations that are
placed in the |equations| field of the symbol table, and thus is the main DAE
of the system that is integrated in time starting from the initial conditions.
Equation \ref{event-eq} corresponds to boolean signals placed in the |events|
field of the symbol table and specifies event conditions.

The task of a DAE solver is to find time varying valuations of $\vec{x}$ and
$\vec{y}$ such that the residual vectors are zero. In addition a DAE solver is
required to detect points in time when the vector $\vec{r_e}$ changes and
report it as an event occurrence.

The generated equations are implicitly formulated ones. In general, it is not
possible to transform these implicit equations into explicit ones; that is, to
completely causalise them \citep{Brenan1996a}. Consequently, a system of
implicit equations needs to be solved at the start of the simulation of each
structural configuration mode and at every integration step. For example, a
numerical solution of an implicitly formulated DAE (Equation \ref{main-eq})
involves evaluation of the function $f$ a number of times (sometimes hundreds
or more at each integration step), with varying arguments, until it converges
to zero. The number of executions of $f$ depends on various factors including
the required precision, the initial guess, the degree of nonlinearity of the
DAE and so on.

As the functions $i$, $f$, and $e$ are evaluated from within inner loops of
the solver, they have to be compiled into machine code for efficiency: any
interpretive overhead here would be considered intolerable by practitioners
for most applications. However, as Hydra allows the equations to be changed in
arbitrary ways \emph{during} simulation, the equations have to be compiled
whenever they change, as opposed to only prior to simulation. As an
optimisation, the code compiled for equations might be cached for future,
possible reuse: see Chapter \ref{chapFutureWork}. The implementation of Hydra
employs JIT machine code generation using the compiler infrastructure provided
by LLVM. The functions $i$, $f$ and $e$ are compiled into LLVM instructions
that in turn are compiled by the LLVM JIT compiler into native machine code.
Function pointers to the generated machine code are then passed to the
numerical solver.

The function pointers have the following Haskell type:

\begin{code}
data Void

type Residual = FunPtr  (       CDouble
                            ->  Ptr CDouble
                            ->  Ptr CDouble
                            ->  Ptr CDouble
                            ->  IO Void)
\end{code}

The first function argument is for time. The second argument is for vector of
real valued signal. The third argument is for vector of differentials of real
valued signals. The forth argument is for vector of residuals, or in the case
of the event specification vector of boolean signal values where |-1.0|
represent |False| and |1.0| represents |True|. The residual functions read the
first three arguments and write the residual values in the fourth argument. As
this functions are passed to numerical solvers it is critical to allow for
fast positional access of vector elements and in-place vector updates. Hence,
the use C-style arrays.

To give you an idea to what kind of code is generated for residuals that
correspond to flat system of equations, Figure \ref{figLLVMCodeUnopt} gives
unoptimised LLVM code for the parametrised van der Pol oscillator. The
corresponding optimised LLVM is given in Figure \ref{figLLVMCodeOpt}.


\begin{figure}
\singlespacing
\small
\begin{verbatim}
define void @hydra_residual_main(double, double*, double*, double*) {
entry:
  %4  = getelementptr double* %2, i32 1
  %5  = load double* %4
  %6  = getelementptr double* %1, i32 0
  %7  = load double* %6
  %8  = fmul double -1.000000e+00, %7
  %9  = getelementptr double* %1, i32 0
  %10 = load double* %9
  %11 = getelementptr double* %1, i32 0
  %12 = load double* %11
  %13 = fmul double %10, %12
  %14 = fmul double -1.000000e+00, %13
  %15 = fadd double 1.000000e+00, %14
  %16 = fmul double 3.000000e+00, %15
  %17 = getelementptr double* %1, i32 1
  %18 = load double* %17
  %19 = fmul double %16, %18
  %20 = fadd double %8, %19
  %21 = fmul double -1.000000e+00, %20
  %22 = fadd double %5, %21
  %23 = getelementptr double* %3, i32 0
  store double %22, double* %23
  br label %BB_0

BB_0:
  %24 = getelementptr double* %1, i32 1
  %25 = load double* %24
  %26 = getelementptr double* %2, i32 0
  %27 = load double* %26
  %28 = fmul double -1.000000e+00, %27
  %29 = fadd double %25, %28
  %30 = getelementptr double* %3, i32 1
  store double %29, double* %30
  br label %BB_1

BB_1:
  ret void
}
\end{verbatim}
\doublespacing

\caption{\label{figLLVMCodeUnopt} Unoptimised LLVM code for the parametrised
van der Pol oscillator.}

\end{figure}


\begin{figure}
\singlespacing
\small
\begin{verbatim}
define void @hydra_residual_main(double, double*, double*, double*) {
entry:
  %4  = getelementptr double* %2, i32 1
  %5  = load double* %4
  %6  = load double* %1
  %7  = fmul double %6, -1.000000e+00
  %8  = fmul double %6, %6
  %9  = fmul double %8, -1.000000e+00
  %10 = fadd double %9, 1.000000e+00
  %11 = fmul double %10, 3.000000e+00
  %12 = getelementptr double* %1, i32 1
  %13 = load double* %12
  %14 = fmul double %11, %13
  %15 = fadd double %7, %14
  %16 = fmul double %15, -1.000000e+00
  %17 = fadd double %5, %16
  store double %17, double* %3
  %18 = load double* %12
  %19 = load double* %2
  %20 = fmul double %19, -1.000000e+00
  %21 = fadd double %18, %20
  %22 = getelementptr double* %3, i32 1
  store double %21, double* %22
  ret void
}
\end{verbatim}
\doublespacing

\caption{\label{figLLVMCodeOpt} Optimised LLVM code for the parametrised
van der Pol oscillator.}

\end{figure}


\section{Numerical Simulation}

The numerical suite used in the current implementation of Hydra is called
SUNDIALS \citep{Sundials2005}. The components we use are KINSOL, a nonlinear
algebraic equation systems solver, and IDA, a differential algebraic equation
systems solver. The code for the function $i$ is passed to KINSOL that
numerically solves the system and returns initial values (at time $t_{0}$) of
$\frac{d\vec{x}}{dt}$,$\vec{x}$ and $\vec{y}$. These vectors together with the
code for the functions $f$ and $e$ are passed to IDA that proceeds to solve
the DAE by numerical integration. This continues until either the simulation
is complete or until one of the events defined by the function $e$ occurs.
Event detection facilities are provided by IDA.

\begin{code}
data Void
type SolverHandle = Ptr Void
type Residual = FunPtr  (       CDouble
                            ->  Ptr CDouble
                            ->  Ptr CDouble
                            ->  Ptr CDouble
                            ->  IO Void)
\end{code}

\begin{code}
data NumericalSolver = NumericalSolver {
      createSolver    :: CDouble      -- Start time
                      -> CDouble      -- Stop time
                      -> Ptr CDouble  -- Current time
                      -> CInt         -- Number of variables
                      -> Ptr CDouble  -- Variables
                      -> Ptr CDouble  -- Differentials
                      -> Ptr CInt     -- Constrained differentials
                      -> CInt         -- Number of events
                      -> Ptr CInt     -- Events
                      -> Residual     -- Initialisation equations
                      -> Residual     -- Main equations
                      -> Residual     -- Event Equations
                      -> IO SolverHandle
  ,   destroySolver   :: SolverHandle -> IO ()
  ,   solve           :: SolverHandle -> IO CInt
      -- Return value  0: soulution has been obtained succesfully
      -- Return value  1: event occurence
      -- Return value  2: stop time has been reached
  }
\end{code}

\begin{code}
type TrajectoryVisualiser = CDouble -> CInt -> Ptr CDouble -> IO ()
\end{code}

% \section{Event Handling}
% 
% At the moment of an event occurrence (one of the signals monitored by $e$
% crossing 0), the numerical simulator terminates and presents the following
% information to an event handler: Name of the event variable for which an event
% occurrence has been detected, time $t_e$ of the event occurrence and
% instantaneous values of the signal variables (i.e., values of
% $\frac{d\vec{x}}{dt}$, $\vec{x}$ and $\vec{y}$ at time $t_e$).
% 
% The event handler traverses the original unflattened signal relation and finds
% the event value expression (a signal-level expression) that is associated with
% the named event variable. In the case of the breaking pendulum model, the
% expression is |((x,y),(vx,vy))|. This expression is evaluated by substituting
% the instantaneous values of the corresponding signals for the variables. The
% event handler applies the second argument of the |switch| combinator (i.e.,
% the function to compute the new signal relation to switch into) to the
% functional-level event value. In the case of the breaking pendulum model, the
% function |freeFall| is applied to the instantaneous value of
% |((x,y),(vx,vy))|. The result of this application is a new signal relation.
% The part of the original unflattened signal relation is updated by replacing
% the old signal relation with the new one. The flat system of equations for the
% previous mode and the machine code that was generated for it by the LLVM JIT
% compiler are discarded. The simulation process for the updated model continues
% from the first stage and onwards.
% 
% In the current implementation, the new signal relation is flattened and new
% equations generated without reusing old ones from previous modes. In other
% words, events are not treated locally. In addition, the state of the whole
% system needs to be transferred for global and explicit reinitialisation of the
% entire system at every event using a \emph{top level} switch, like in the
% breaking pendulum example. We hope to address these issues in the future: see
% Section \ref{sec:futurework}.

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
reason, and because other compilation-based, noncausal modelling and
simulation language implementations do not carry out dynamic reconfiguration,
we do not compare the performance to other simulation software. The results
would not be very meaningful.

The evaluation setup is as follows. The numerical simulator integrates the
system using variable-step, variable-order BDF (Backward Differentiation
Formula) solver \citep{Brenan1996a}. Absolute and relative tolerances for
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

\begin{table}
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
\caption{\label{table:breaking-pendulum} Time profile of the breaking pendulum simulation.
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

\begin{table}
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
\caption{\label{table:larger-system-1} Time profile of structurally dynamic RLC circuit simulation (part I).
}
\end{table}

\begin{table}
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
\caption{\label{table:larger-system-2} Time profile of structurally dynamic RLC circuit simulation (part II).
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
\caption{\label{fig:benchmark} Plot demonstrating how CPU time spent on mode
switches grows as number of equations increase in structurally dynamic RLC
circuit simulation.}
\end{figure}

Our approach offers new functionality in that it allows noncausal modelling
and simulation of structurally dynamic systems that simply cannot be handled
by static approaches. Thus, when evaluating the feasibility of our approach,
one should weigh the overheads against the limitation and inconvenience of not
being able to model such systems noncausally.

In previous work \citep{Giorgidze2009b}, we conducted benchmarks to evaluate
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
context of noncausal modelling and simulation of structurally dynamic
systems and suitability of proposed execution model for real-time
simulation. Currently, for large systems, the implementation is only
suitable for offline simulation.

Our approach offers new functionality in that it allows noncausal modelling
and simulation of structurally dynamic systems that simply cannot be handled
by static approaches. Thus, when evaluating the feasibility of our approach,
one should weigh the inherent overheads against the limitation and
inconvenience of not being able to model and simulate such systems
noncausally.