\chapter{Hydra: A Functional Hybrid Modelling Language}
\label{chapHydra}

This chapter introduces the notions of signal, signal function and signal
relation. The Hydra language is based on these notions. The notions are
defined conceptually to facilitate development of and reasoning about Hydra
models. This chapter presents the Hydra language itself informally by means of
instructive examples. The formal definition of the language is given in
Chapter \ref{chapDefinition}, while the language implementation is described
in Chapter \ref{chapImplementation}.

\section{Concepts of Hydra}

Note that this section gives conceptual definitions of the notions of signal,
signal function and signal relation. This section does not describe the
implementation of these notions.

\subsection{Signal}

Conceptually, a \emph{signal} is a time-varying value, that is, a function
from time to value.

\begin{code}
type Time           ~=  Real
type Signal alpha   ~=  Time -> alpha
\end{code}

|Time| is continuous and is represented as a real number. The type parameter
|alpha| specifies the type of values carried by the signal. For example, a
signal that corresponds to the amount of current flowing in a certain
electrical circuit has type |Signal Real|, while a signal that corresponds to
the position of a certain object in a two dimensional space has type |Signal
(Real,Real)|.

The basic signal types of Hydra are real signals (i.e., |Signal Real|) and
boolean signals (i.e., |Signal Bool|). Hydra also allows for signals of
arbitrarily nested pairs of basic types. As an example of a signal that
carries nested pairs, consider a signal of type |Signal
((Real,Real),(Real,Real))| which can be used to represent current and voltage
pairs at the positive and negative pins of a two-pin electrical component.

\subsection{Signal Function}

Conceptually, a \emph{signal function} is as a function from signal to signal.

\begin{code}
type SF alpha beta ~= Signal alpha -> Signal beta
\end{code}

When a value of type |SF alpha beta| is applied to an input signal of type
|Signal alpha|, it produces an output signal of type |Signal beta|.

Since a pair of signals, say |(Signal alpha,Signal beta)|, is isomorphic to a
signal of the pair of the carried types, in this case |Signal (alpha, beta)|,
unary signal functions suffice for handling signal functions of any arity. For
example, the binary signal function |plus_sf| that takes two signals and
computes sum of their values at each point in time can be given the following
type and conceptual definition:

\begin{code}
(plus_sf) :: SF (Real,Real) Real
(plus_sf) sf t ~= fst (sf t) + snd (sf t)
\end{code}

Hydra provides a number of primitive signal functions that lift common
mathematical operations (e.g., |+|, |*|, |sin| and |cos|) to the signal level.
Hydra also provides |der :: SF Real Real| signal function that differentiates
the given signal. This signal function enables definition of differential
equations.

\subsection{Signal Relation}

Conceptually, a \emph{signal relation} is a relation on signals. Stating that
some signals are in a particular relation to each other imposes
\emph{constraints} on those signals. Assuming these constraints can be
satisfied, this allows some of the signals to be determined in terms of the
others depending on which signals are known and unknown in a given context.
That is, signal relations are non-causal, unlike signal functions where the
knowns and unknowns (inputs and outputs) are given a priori.

An ordinary relation can be seen as a predicate that decides whether some
given values are related or not. The same is true for signal relations:

\begin{code}
SR alpha  ~=  Time -> Signal alpha -> Prop
\end{code}

Given a point in time and a signal, a signal relation defines a proposition
constraining the signal starting from the given time point. Here, |Prop| is a
type of propositions defined in the second-order logic. \emph{Solving} a
relation for a given starting time thus means finding a signal that satisfies
the predicate.

Just like for signal functions, unary signal relations suffice for handling
signal relations of any arity. For example, equality is a binary signal
relation that can be given the following type and conceptual definition:

\begin{code}
(=) ::  SR (alpha,alpha)
(=) t0 s ~= {-" \forall \, t \in \mathbb{R} . \, "-}  t == t0  =>  fst (s t) == snd (s t)
\end{code}

\section{Syntax of Hydra}

There are two levels to Hydra: the \emph{functional level} and the
\emph{signal level}. The functional level is concerned with the definition of
ordinary functions operating on time-invariant values. The signal level is
concerned with the definition of signal relations and signal functions on
time-varying values (i.e., signals).

Signal relations and signal functions are \emph{first-class} entities at the
functional level. Signals, in contrast, are \emph{not} first-class entities at
the functional level. However, crucially, \emph{instantaneous values} of
signals can be propagated back to the functional level, allowing the
generation of new signal relations that depend on signal values at discrete
points in time.

The definitions at the signal level may freely refer to entities defined at
the functional level as the latter are time-invariant, known parameters as far
as solving the equations are concerned. However, the opposite is not allowed:
time-varying entities are confined to the signal level. The only signal-level
notions that exist at the functional level are the signal relation and the
signal function. Note that these notions are \emph{time-invariant}.

Hydra is implemented as an embedding in Haskell using \emph{quasiquoting}
\citep{Mainland2007,Mainland2008}. This means that Haskell provides the
functional level almost for free through shallow embedding. In contrast, the
signal level is realised through deep embedding: signal relations expressed in
terms of Hydra-specific syntax are, through the quasiquoting machinery, turned
into an internal representation, an abstract syntax tree (AST), that then is
compiled into simulation code.

Haskell-embedded implementation of Hydra adopts the following syntax for
defining signal relations:

\begin{code}
[rel| pattern -> equations |]
\end{code}

The symbols | [rel|| | and | ||] | are the opening and closing quasiquotes
respectively. At compile time, GHC applies the user-defined (in this case
defined by the embedded language implementer) parser function named in the
opening quote to the text between the quotes. In this case, the parser
function is |rel|.

The pattern binds \emph{signal variables} that scope over the equations that
follow. The equations are DAEs stated using \emph{signal relation application}
(the operator |<>|). Signal relation application is how the constraints
embodied by a signal relation are imposed on particular signals.

\begin{code}
sr <> s
\end{code}

The equations are required to be well typed. In this example, if |sr| has the
type |SR alpha| then |s| must have the type |Signal alpha|.

Hydra provides a more conventional-looking syntax for application of the
built-in equality signal relation. For example, |a * x + b = 0| is equivalent
to |(=) <> (a * x + b, 0)|.

In addition to user defined signal relations Hydra provides for user defined
signal functions. Hydra uses the following syntax for defining signal
functions:

\begin{code}
[fun| pattern -> expression |]
\end{code}

Just like for signal relations we use quasi-quoting for defining signal
functions. The pattern binds \emph{signal variables} that scope over the
expression that follows. Signal functions can be applied to signals by
juxtaposing them together:

\begin{code}
sf s
\end{code}

Signal function applications are required to be well typed. In this example,
if |sf| has the type |SF alpha beta| then |s| must have the type |Signal
alpha|. The type of resulting signal is |Signal beta|.

\section{The |switch| combinator}

The built-in equality signal relation |=| is capable of describing flat
systems of equations and the signal relation application operator (i.e., |<>|)
provides for hierarchically structured systems of equations. In this section
we introduce one more built-in (higher-order) signal relation that allows
description of structurally dynamic signal relations.

\begin{code}
switch :: SR a -> SF a Bool -> (a -> SR a) -> SR a
\end{code}

The |switch| combinator, which forms a signal relation by temporal
composition, takes three arguments to define the composite signal relation (of
type |SR a|) returned by the combinator. The first argument (of type |SR a|)
is a signal relation that is initially active. The second argument is a signal
function (of type |SF a Bool|). Starting from the first point in time when the
boolean signal that is computed by applying the signal function to the signal
constrained by the composite signal relation changes (from |True| to |False|
or from |False| to |True|), the composite behaviour is defined by the signal
relation that is computed by applying the third argument (a function of type
|a -> SR a|) to the instantaneous value of the constrained signal. A formally
defined semantics of the switch combinator is given in Chapter
\ref{chapDefinition}.

Note how the |switch| combinator allows for definition of a signal relation
whose equational description changes over time. In addition, the |switch|
combinator allows for state transfer from the old mode and initialisation of
the new mode by giving the function that computes the new mode from the
instantaneous values of the constrained signals.

In the signal relation notation described earlier the list of equations that
follows the pattern is not necessarily a static one as they may contain signal
relation application of structurally dynamic signal relation.

\section{Models with Static Structure}

Let us introduce the Haskell embedding of Hydra by modelling the circuit that
is depicted in Figure \ref{figCircuit1}. We first define the |twoPin|
signal relation that captures the common behaviour of electrical components
with two connectors (see Figure \ref{figTwoPin}):

\begin{code}
type Pin = (Real,Real)  
 
twoPin :: SR ((Pin,Pin),Real)
twoPin = [rel| (((flow p_i,p_v),(flow n_i,n_v)),u) ->
  p_v  -  n_v  =  u
  p_i  +  n_i  =  0
|]
\end{code}

The signal variables |p_i| and |p_v|, which are bound in the pattern,
represent the current into the positive pin and the voltage at the positive
pin. The signal variables |n_i| and |n_v|, which are also bound in the
pattern, represent the current into the negative and the voltage at the
negative pin. The signal variable |u| represents the voltage drop across the
electrical component.

\begin{figure}
\begin{center}
\includegraphics{Graphics/twoPin}
\end{center}
\caption{\label{figTwoPin} Electrical component with two connectors.}
\end{figure}

Signal variables in the |rel| pattern qualified as |flow| are called
\emph{flow} signal variables. Signal variables without any qualifier are
called \emph{potential} signal variables. The |flow| qualifier is just a
syntactic sugar that facilitates easy and straight forward definition of
connection equations as discussed in Section~\ref{subSecConnections}. In
particular, the above definition of the |twoPin| signal relation is desugared
to:

\begin{code}
twoPin :: SR ((Pin,Pin),Real)
twoPin = [rel| (((p_i,p_v),(n_i,n_v)),u) ->
  p_v      -  n_v      =  u
  (- p_i)  +  (- n_i)  =  0
|]
\end{code}

This way, flows are always directed from an interface into a component, as it
were, making it possible to always count flows into connection nodes as being
positive, as we will see later in this section.

We can now use the |twoPin| signal relation to define a model for a resistor
parametrised with respect to the resistance. Note that a parametrised model is
an ordinary function returning a signal relation.

\begin{code}
resistor :: Double -> SR (Pin,Pin)
resistor r = [rel| ((flow p_i,p_v),(flow n_i,n_v)) ->
    local u
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    $r$ * p_i = u
|]
\end{code}

Here |u| is declared as a local signal variable, that is, it is not exposed in
the interface of the signal relation. As a consequence, |u| can not be
constrained further unlike the rest of the variables in the interface. Note
the two kinds of variables: the functional level ones representing
time-invariant parameters, and the signal-level ones, representing
time-varying entities, the signals. Functional-level fragments, such as
variable references, are spliced into the signal level by enclosing them
between antiquotes, \$. On the other hand time-varying entities are not
allowed to escape to the functional level (meaning signal-variables are not in
scope between antiquotes).

In this case, note how antiquoting is used to splice in a copy of the |twoPin|
signal relation; that is, its equations are \emph{reused} in the context of
the resistor model. Alternatively, this can be viewed as defining the resistor
model by extending the |twoPin| model with an equation that characterises the
specific concrete electrical component, in this case Ohm's law.

To clearly see how |twoPin| contributes to the definition of the |resistor|
signal relation, let us consider what happens when the resistor model is
\emph{flattened} as part of flattening of a complete model, a transformation
that is described in detail in Chapter \ref{chapImplementation}. Intuitively,
flattening can be understood as ``inlining'' of applied signal relations.
Thus, the arguments of a signal relation application is substituted into the
body of the applied signal relation, and the entire application is then
replaced by the instantiated signal relation body. In our case, the result of
flattening the signal relation |resistor 10| is:

\begin{code}
[rel| ((flow p_i,p_v),(flow n_i,n_v)) ->
  local u
  p_v - n_v           =  u
  (- p_i) + (- n_i)   =  0
  10 * p_i            =  u
|]
\end{code}

Models for an inductor, a capacitor, a voltage source and a ground are defined
similarly:

\begin{code}
inductor :: Double -> SR (Pin,Pin)
inductor l = [rel| ((flow p_i,p_v),(flow n_i,n_v)) ->
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    $l$ * der p_i = u
|]

capacitor :: Double -> SR (Pin,Pin)
capacitor c = [rel| ((flow p_i,p_v),(flow n_i,n_v)) ->
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    $c$ * der u  = p_i
|]

vSourceAC :: Double -> Double -> SR (Pin,Pin)
vSourceAC v f = [rel| ((flow p_i,p_v),(flow n_i,n_v)) ->
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    u = $v$ * sin (2 * $pi$ * $f$ * time)
|]

ground :: SR (Pin)
ground = [rel| (flow p_i,p_v) where
    p_v = 0
|]
\end{code}

\subsection{Non-Causal Connections}
\label{subSecConnections}

To facilitate composition of signal relations, Hydra provides a
Modelica-inspired |connect| construct. Using this, a complete model for the
circuit of Figure~\ref{figCircuit1} can be defined as follows:

\begin{code}
simpleCircuit :: SR ()
simpleCircuit = [rel| () ->
    local acp_i acp_v acn_i acn_v
    $vSourceAC 1 1$  <>  ((acp_i,acp_v),(acn_i,acn_v))

    local rp_i rp_v rn_i rn_v
    $resistor 1$     <>  ((rp_i,rp_v),(rn_i,rn_v))

    local lp_i lp_v ln_i ln_v
    $inductor 1$     <>  ((lp_i,lp_v),(ln_i,ln_v))

    local cp_i cp_v cn_i cn_v
    $capacitor 1$    <>  ((cp_i,cp_v),(cn_i,cn_v))

    local gp_i gp_v
    $ground$         <>  (gp_i,gp_v)
    
    connect flow  acp_i rp_i lp_i
    connect       acp_v rp_v lp_v

    connect flow  rn_i cp_i
    connect       rn_v cp_v

    connect flow  acn_i cn_i ln_i gp_i
    connect       acn_v cn_v ln_v gp_v
|]
\end{code}

Note that the above code is a direct textual representation of how the
components are connected in the circuit.

In the setting of Hydra, the |connect| construct is just syntactic sugar.In
particular, |connected flow| generates sum-to-zero equations and just
|connect| generates equality constrains. The connect constructs of the
|simpleCircuit| model are thus expanded to the following equations:

\begin{code}
acp_i  +  rp_i  +  lp_i  =  0
acp_v  =  rp_v  =  lp_v

rn_i   +  cp_i  =  0
rn_v   =  cp_v

acn_i  +  cn_i  +  ln_i  +  gp_i  = 0
acn_v  =  cn_v  =  ln_v  =  gp_v
\end{code}

In Hydra, the expansion of connect constructs into the sum-to-zero and
equality constraints is straightforward. In particular, note that all signal
variables are counted positively in the sum to zero equations. This is
different from Modelica \citep{Modelica2007} where a special ``rule of signs''
is used to determine which flow variables go with a plus sign and which go
with a minus sign. Hydra obviates the need for the rule of signs using |flow|
qualifiers, which also is a syntactic sugar. Despite this technical difference
both constructs serve the same purpose, that is, generation of non-causal
equations representing the component connections.


\section{Higher-order Modelling in Hydra}

\begin{code}
serial :: SR (Pin,Pin) -> SR (Pin,Pin) -> SR (Pin,Pin)
serial sr1 sr2 = [rel| ((p_i, p_v), (n_i, n_v)) ->
    $sr1$  <>  ((p_i, p_v), (n1_i, n1_v))
    $sr2$  <>  ((p2_i, p2_v), (n_i, n_v))
    connect flow  n1_i p2_i
    connect       n1_v p2_v
|]
\end{code}

\begin{figure}
\includegraphics[width=\textwidth]{Graphics/serial}
\end{figure}


\begin{code}
wire :: SR (Pin,Pin)
wire = [rel| ((p_i,p_v),(n_i,n_v)) ->
    $twoPin$ <> ((p_i,p_v),(n_i,n_v),u)
    u = 0
|]
\end{code}

\begin{code}
wire `serial` sr = sr `serial` wire = sr
\end{code}

\begin{figure}
\includegraphics[width=\textwidth]{Graphics/serialWire}
\end{figure}

\begin{code}
serialise :: [SR (Pin,Pin)] -> SR (Pin,Pin)
serialise = foldr serial wire
\end{code}

\begin{code}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr serial wire [sr1, sr2, ..., srn] =
  sr1 `serial` (sr2 `serial` ... (srn `serial` wire))
\end{code}

\begin{figure}
\includegraphics[width=\textwidth]{Graphics/serialise}
\end{figure}


\begin{code}
parallel :: SR (Pin,Pin) -> SR (Pin,Pin) -> SR (Pin,Pin)
parallel sr1 sr2 = [rel| ((p_i, p_v), (n_i, n_v)) ->
    $sr1$  <>  ((p1_i, p1_v), (n1_i, n1_v))
    $sr2$  <>  ((p2_i, p2_v), (n2_i, n2_v))
    connect flow  p_i p1_i p2_i
    connect       p_v p1_v p2_v
    connect flow  n_i n1_i n2_i
    connect       n_v n1_v n2_v
|]
\end{code}

\begin{figure}
\includegraphics[width=0.5\textwidth]{Graphics/parallel}
\end{figure}

\begin{code}
noWire :: SR (Pin,Pin)
noWire = [rel| ((p_i,p_v),(n_i,n_v)) ->
    $twoPin$ <>((p_i,p_v),(n_i,n_v),u)
    p_i = 0
|]
\end{code}

\begin{code}
noWire `parallel` sr = sr `parallel` noWire = sr
\end{code}

\begin{figure}
\includegraphics[width=\textwidth]{Graphics/parallelNoWire}
\end{figure}

\begin{code}
parallelise :: [SR (Pin,Pin)] -> SR (Pin,Pin)
parallelise = foldr parallel noWire
\end{code}



\section{Structurally-dynamic Modelling}
\subsection{Breaking Pendulum}

To introduce structurally dynamic modelling in Hydra, let us model a physical
system whose structural configuration changes abruptly during simulation: a
simple pendulum that can break at a specified point in time; see Figure
\ref{figPendulum}. The pendulum is modelled as a body represented by a point
mass $m$ at the end of a rigid, mass-less rod, subject to gravity $m \vec{g}$.
If the rod breaks, the body will fall freely. This makes the differences
between the two configurations sufficiently large that, for example, Modelica
does not support non-causal modelling of this system. Instead, if simulation
across the breaking point is desired, the modeller is forced to model the
system in a causal, less declarative way \citep[pp.
31--33]{ModelicaTutorial2000}.

The following code shows how to model the two modes of the pendulum in Hydra.

\begin{code}
type Pos   =  (Double,Double)
type Vel   =  (Double,Double)
type Body  =  (Pos,Vel)

g :: Double
g = 9.81

freeFall :: Body -> SR Body
freeFall ((x0,y0),(vx0,vy0)) = [rel| ((x,y),(vx,vy)) ->
    init (x,y)       = ($x0$,$y0$)
    init (vx,vy)     = ($vx0$,$vy0$)
    (der x,der y)    = (vx,vy)
    (der vx,der vy)  = (0,-$g$)
|]

pendulum ::  Double -> Double -> SR Body
pendulum l phi0 = [rel| ((x,y),(vx,vy)) ->
    local phi

    init phi      =  $phi0$
    init der phi  =  0
    init vx       =  0
    init vy       =  0

    x             =     $l$  *  sin phi
    y             =  -  $l$  *  cos phi
    (vx,vy)       =  (der x, der y)
    der (der phi) + ($ g / l $) * sin phi = 0
|]
\end{code}

The type |Body| denotes the state of the pendulum body; that is, its position
and velocity, where position and velocity both are 2-dimensional vectors
represented by pairs of doubles. Each model is represented by a function that
maps the \emph{parameters} of the model to a relation on signals; that is, an
instance of the defining system of DAEs for specific values of the parameters.
In the unbroken mode, the parameters are the length of the rod |l| and the
initial angle of deviation |phi0|. In the broken mode, the signal relation is
parametrised on the initial state of the body.

Equations marked by the keyword |init| are initialisation equations used to
specify initial conditions.

The non-causal nature of Hydra can be seen particularly clearly in the last
equation of the unbroken mode that simply states a constraint on the angle of
deviation and its second derivative, without making any assumption regarding
which of the two time-varying entities is going to be used to solve for the
other (both |g| and |l| are time-invariant functional-level variables).

To model a pendulum that breaks at some point, we need to create a composite
model where the model that describes the dynamic behaviour of the unbroken
pendulum is replaced, at the point when it breaks, by the model describing a
free falling body. These two submodels must be suitably joined to ensure the
continuity of both the position and velocity of the body of the pendulum.

To this end, the |switch|-combinator is used:

\begin{code}
breakingPendulum :: Double -> Double -> Double -> SR Body
breakingPendulum t l phi0 =
  switch (pendulum l phi0) [fun|\_ -> time >= $t$ |] (\b -> freeFall b)
\end{code}

In our example, an event is simply generated at an \emph{a priori} specified
point in time, but the condition could be an arbitrary time-varying entity.
Not how succeeding model (i.e., |freeFall|) is initialised so as to ensure the
continuity of the position and velocity as discussed above. The simulation
results can be seen in Figure \ref{figPendulumPlot}

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/pendulumPlot.pdf}
\end{center}
\caption{\label{figPendulumPlot} Plot showing how |x| and |y| coordinates of the body on the breaking pendulum change over time.}
\end{figure}


In our particular example, the pendulum is only going to break once. In other
words, there is not much iteration going on, and it would in principle (with a
suitable language design) be straightforward to generate code for both modes
of operation prior to simulation. However, this is not the case in general.
For example, given a parametrised signal relation:

\begin{code}
sr1 :: Double -> SR ((Double, Double), E Double)
\end{code}

we can recursively define a signal relation |sr| that describes an overall
behaviour by ``stringing together'' the behaviours described by |sr1|:

\begin{code}
sr :: Double -> SR (Double, Double)
sr x = switch (sr1 x) sr
\end{code}

In this case, because the number of instantiations of |sr1| in general cannot
be determined statically (and because each instantiation can depend on the
parameter in arbitrarily complex ways), there is no way to generate all code
prior to simulation. However, the pendulum example is simple and suffice for
illustrative purposes. Moreover, despite its simplicity, it is already an
example with which present non-causal languages struggle, as mentioned above.

\subsection{Half-wave Rectifier}

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/rectifier.pdf}
\end{center}
\caption{\label{figPendulumPlot} Half-wave rectifier circuit with in-line inductor.}
\end{figure}

\begin{code}
ioDiode :: SR (Pin,Pin)
ioDiode = switch nowire  [fun| ((_,p_v),(_,n_v)) -> p_v - n_v > 0 |]  (\_ -> icDiode)
\end{code}

\begin{code}
icDiode :: SR (Pin,Pin)
icDiode = switch wire    [fun| ((p_i,_),(_,_))   -> p_i < 0 |]        (\_ -> ioDiode)
\end{code}


\begin{code}
halfWaveRectifier :: SR ()
halfWaveRectifier = [rel| () ->
    local lp_i lp_v ln_i ln_v
    $iInductor 0 1$ <> ((lp_i,  lp_v), (ln_i, ln_v))

    local r1p_i r1p_v r1n_i r1n_v
    $resistor  1$ <> ((r1p_i, r1p_v), (r1n_i, r1n_v))

    local r2p_i r2p_v r2n_i r2n_v
    $resistor  1$ <> ((r2p_i, r2p_v), (r2n_i, r2n_v))

    local dp_i  dp_v  dn_i dn_v
    $icDiode$ <> ((dp_i, dp_v), (dn_i, dn_v))

    local cp_i cp_v cn_i cn_v
    $iCapacitor 0 1$ <> ((cp_i, cp_v), (cn_i, cn_v))

    local acp_i acp_v acn_i acn_v
    $vSourceAC 1 1$ <> ((acp_i, acp_v), (acn_i, acn_v))

    local g_i g_v
    $ground$ <> (g_i,g_v)

    connect  flow  acp_i lp_i
    connect        acp_v lp_v

    connect  flow  ln_i  r1p_i
    connect        ln_v  r1n_v

    connect  flow  r1n_i dp_i
    connect        r1n_v dp_v

    connect  flow  dn_i cp_i r2p_i
    connect        dn_v cp_v r2p_v

    connect  flow  acn_i cn_i r2n_i g_i
    connect        acn_v cn_v r2n_v g_v
|]
\end{code}

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/rectifierCapVol.pdf}
\end{center}
\caption{\label{figPendulumPlot} Voltage across the capacitor in the half-wave rectifier circuit with in-line inductor.}
\end{figure}

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/rectifierIndCur.pdf}
\end{center}
\caption{\label{figPendulumPlot} Current through the inductor in the half-wave rectifier circuit with in-line inductor.}
\end{figure}


\section{Highly Structurally-dynamic Modelling}

\subsection{Bouncing Ball}

\begin{code}
bouncingBall :: Body -> SR Body
bouncingBall b = switch   (freeFall b)
                          [fun| ((_,y),_) -> y < 0 |]
                          (\(p,(vx,vy)) -> bouncingBall (p,(vx, - vy))
\end{code}

\subsection{Highly Structurally-dynamic Bouncing Ball}

\begin{code}
bouncingBall :: Body -> SR Body
bouncingBall b = switch   (freeFall b)
                          [fun| ((_,y),_) -> y < 0 |]
                          (\(p,v) -> divide (p,v))

divide :: Body -> SR Body
divide ((x0,y0),(vx0,vy0)) = [rel| ((x,y),(vx,vy)) ->
    $bouncingBall  ((x0,y0),(   vx0 / 2,  -  vy0 / 2))$   <>  ((x,y),(vx,vy))
    local x' y' vx' vy'
    $bouncingBall  ((x0,y0),(-  vx0 / 2,  -  vy0 / 2))$   <>  ((x',y'),(vx',vy'))
|]
\end{code}

