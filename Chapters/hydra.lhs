\chapter{Modelling and Simulation in Hydra}
\label{chapHydra}

This chapter presents the Hydra language informally, by means of instructive
examples. The examples are carefully chosen to back up the contributions of
the thesis by illustrating higher-order and structurally dynamic modelling and
simulation in Hydra.

\section{Models with Static Structure}

Let us illustrate the Hydra language by modelling the circuit that is depicted
in Figure \ref{figCircuit1}. Let us first define the |twoPin| signal relation
that captures the common behaviour of electrical components with two
connectors (see Figure \ref{figTwoPin}):

\begin{code}
type Pin = (Real,Real)

twoPin :: SR ((Pin,Pin),Real)
twoPin = [rel| (((p_i,p_v),(n_i,n_v)),u) ->
  p_v  -  n_v  =  u
  p_i  +  n_i  =  0
|]
\end{code}

The signal variables |p_i| and |p_v|, which are bound in the pattern,
represent the current into the positive pin and the voltage at the positive
pin, respectively. The signal variables |n_i| and |n_v|, which are also bound
in the pattern, represent the current into the negative pin and the voltage at
the negative pin, respectively. The signal variable |u| represents the voltage
drop across the electrical component.

We can now use the |twoPin| signal relation to define a signal relation that
models a resistor:

\begin{code}
resistor :: Real -> SR (Pin,Pin)
resistor r = [rel| ((p_i,p_v),(n_i,n_v)) ->
    local u
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    $r$ * p_i = u
|]
\end{code}

Note that a parametrised signal relation is an ordinary function returning a
signal relation. In the |resistor| signal relation, the signal variable |u| is
declared as a local signal variable; that is, it is not exposed in the pattern
of the signal relation. As a consequence, |u| can only be constrained in this
signal relation, unlike the rest of the variables in the pattern, which can be
constrained further.

As we have already mentioned, Hydra uses two kinds of variables: the
functional-level ones representing time-invariant entities, and the
signal-level ones, representing time-varying entities, the signals.
Functional-level fragments, such as variable references, are spliced into the
signal level by enclosing them between antiquotes, \$. On the other hand
time-varying entities are not allowed to escape to the functional level; that
is, signal-variables are not in scope between antiquotes and outside the
quasiquotes. Note that, as discussed in Section~\ref{secDesignOfHydra}, a
signal relation is a time-invariant entity and thus can be spliced into the
signal level.

The |resistor| signal relation uses antiquoting to splice in a copy of the
|twoPin| signal relation; that is, its equations are reused in the context of
the resistor model. Readers familiar with object-oriented, noncausal languages
like Modelica, can view this as a definition of the resistor model by
extending the |twoPin| model with an equation that characterises the specific
concrete electrical component, in this case Ohm's law.

To clearly see how |twoPin| contributes to the definition of the |resistor|
signal relation, let us consider what happens when the resistor model is
\emph{flattened} as part of flattening of a complete model, a transformation
that is described in detail in Chapter \ref{chapImplementation}. Intuitively,
flattening can be understood as inlining of applied signal relations to reduce
the signal relation into a flat list of equations (i.e., a flat DAE). In the
process of flattening, the arguments of a signal relation application are
substituted into the body of the applied signal relation, and the entire
application is then replaced by the instantiated signal relation body,
renaming local variables as necessary to avoid name clashes. In our case, the
result of flattening the signal relation |resistor 10| is:

\begin{code}
[rel| ((p_i,p_v),(n_i,n_v)) ->
  local u
  p_v - n_v   =  u
  p_i + n_i   =  0
  10 * p_i    =  u
|]
\end{code}

Models for an inductor, a capacitor, a voltage source and a ground reference
are defined in Figure \ref{figHydraComponents}. Note that the inductor and the
capacitor signal relations contain |init| equations. An |init| equation is
enforced only at the point in time when the signal relation becomes active. In
this example, the |init| equations are used to initialise the differential
variables involved in the inductor and the capacitor signal relations.

By default, Modelica implicitly initialises differential variables to zero.
That is why initialisation equations were not considered in the corresponding
Modelica models given in Chapter \ref{chapBackground}. Hydra does not allow
for implicit initialisation; that is, all initialisation equations must be
specified explicitly.

\begin{figure}

\begin{code}
iInductor :: Real -> Real -> SR (Pin,Pin)
inductor p_i_0 l = [rel| ((p_i,p_v),(n_i,n_v)) ->
    local u
    init p_i = p_i_0
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    $l$ * der p_i = u
|]
\end{code}

\begin{code}
iCapacitor :: Real -> Real -> SR (Pin,Pin)
iCapacitor u0 c = [rel| ((p_i,p_v),(n_i,n_v)) ->
    local u
    init u = u0
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    $c$ * der u  = p_i
|]
\end{code}

\begin{code}
vSourceAC :: Real -> Real -> SR (Pin,Pin)
vSourceAC v f = [rel| ((p_i,p_v),(n_i,n_v)) ->
    local u
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    u = $v$ * sin (2 * $pi$ * $f$ * time)
|]
\end{code}

\begin{code}
ground :: SR (Pin)
ground = [rel| (p_i,p_v) where
    p_v = 0
|]
\end{code}

\caption{\label{figHydraComponents} Hydra models for inductor, capacitor,
voltage source and ground reference.}

\end{figure}

\section{Noncausal Connections}
\label{secConnections}

In \citet{Giorgidze2008c} we describe syntactic sugar for specifying noncausal
connections. In this thesis we implement the same approach using higher-order
modelling combinators. In both cases we are able to describe noncausal
connections without a special semantic language construct. In this aspect,
Hydra is simpler than other noncausal modelling languages such as
\citet{Modelica}, MKL \citep{Broman2007a}, and Chi \citep{Beek2008a}, as these
languages feature special language constructs for specifying noncausal
connections. It is worthwhile mentioning that although the aforementioned
approaches to noncausal connections serve the same purpose, they are very
different from each other, both in their syntax and in their semantics.
Detailed comparison of approaches to noncausal connections still lies ahead,
including devising of a minimal set of higher-order combinators expressive
enough to capture all possible noncausal interconnections.

Because signal relations are first-class entities, it is possible to implement
higher-order combinators that facilitate connection of noncausal models. To
model the simple electrical circuit as an interconnection of the already
modelled components let us define three higher-order signal relations
facilitating noncausal connection of two-pin electrical components.

Firstly, we define a higher-order signal relation that takes two signal
relations modelling two-pin electrical components and returns the signal
relation that models the serial connection of the two electrical components.
The graphical representation of the signal relation is given in Figure
\ref{figSerial}.

\begin{samepage}
\begin{code}
serial :: SR (Pin,Pin) -> SR (Pin,Pin) -> SR (Pin,Pin)
serial sr1 sr2 = [rel| ((p_i, p_v),(n_i, n_v)) ->
    local p1_i; local p1_v; local n1_i; local n1_v;
    $sr1$  <>  ((p1_i, p1_v), (n1_i, n1_v))

    local p2_i; local p2_v; local n2_i; local n2_v;
    $sr2$  <>  ((p2_i, p2_v), (n2_i, n2_v))

    (- p_i) + p1_i = 0
    p_v     = p1_v

    n1_i + p2_i = 0
    n1_v = p2_v

    n2_i + (- n_i) = 0
    n2_v = n_v
|]
\end{code}
\end{samepage}

\begin{figure}
\centering
\includegraphics[scale=1.5]{Graphics/serial}

\caption{\label{figSerial} Serial connection of two electrical components.}

\end{figure}

Secondly, we define a higher-order signal relation that takes two signal
relations modelling two-pin electrical components and returns the signal
relation that models the parallel connection of the two electrical components.
The graphical representation of the signal relation is given in Figure
\ref{figParallel}.

\begin{samepage}
\begin{code}
parallel :: SR (Pin,Pin) -> SR (Pin,Pin) -> SR (Pin,Pin)
parallel sr1 sr2 = [rel| ((p_i, p_v), (n_i, n_v)) ->
    local p1_i; local p1_v; local n1_i; local n1_v;
    $sr1$  <>  ((p1_i, p1_v), (n1_i, n1_v))
    local p2_i; local p2_v; local n2_i; local n2_v;
    $sr2$  <>  ((p2_i, p2_v), (n2_i, n2_v))

    (- p_i) + p1_i + p2_i = 0
    p_v  = p1_v
    p1_v = p2_v

    (- n_i) + n1_i + n2_i = 0
    n_v  = n1_v
    n1_v = n2_v
|]
\end{code}
\end{samepage}

\begin{figure}
\centering
\includegraphics[scale=1.5]{Graphics/parallel}

\caption{\label{figParallel} Parallel connection of two electrical components.}

\end{figure}

Finally, we define a higher-order signal relation that takes two
signal relations modelling two-pin electrical components and returns the
signal relation that models the grounded circuit involving the two electrical
components. The graphical representation of the signal relation is given
in Figure \ref{figGroundedCircuit}.

\begin{samepage}
\begin{code}
groundedCircuit :: SR (Pin,Pin) -> SR (Pin,Pin) -> SR ()
groundedCircuit sr1 sr2 = [rel| () ->
    local p1_i; local p1_v; local n1_i; local n1_v;
    $sr1$  <>  ((p1_i, p1_v), (n1_i, n1_v))

    local p2_i; local p2_v; local n2_i; local n2_v;
    $sr2$  <>  ((p2_i, p2_v), (n2_i, n2_v))

    local gp_i; local gp_v;
    $ground$ <> (gp_i,gp_v)

    p1_i + p2_i = 0
    p1_v + p2_v = 0

    n1_i + n2_i + gp_i = 0
    n1_v = n2_v
    n2_v = gp_v
|]
\end{code}
\end{samepage}

\begin{figure}
\centering
\includegraphics[scale=1.5]{Graphics/groundedCircuit}

\caption{\label{figGroundedCircuit} Grounded circuit involving two electrical
components.}

\end{figure}

Now we can assemble the models of the electrical components into the simple
electrical circuit as follows:

\begin{samepage}
\begin{code}
simpleCircuit :: SR ()
simpleCircuit =
  groundedCircuit  (vSourceAC 1 1)
                   (parallel  (serial (resistor 1) (iCapacitor 0 1))
                              (iInductor 0 1))
\end{code}
\end{samepage}

Here state variables are initially set to zero and all other parameters are
set to one. Note that the above code is a direct textual representation of how
the components are connected in the circuit. Unlike the Modelica model that
specifies the noncausal connections in terms of connections of time-varying
variables, Hydra allows for definition of higher-order combinators that are
capable of specifying noncausal connections by connecting noncausal models
directly.

It is trivial in Hydra to reuse the circuit components and model the modified
circuit that is depicted in Figure \ref{figCircuit2}:

\begin{samepage}
\begin{code}
simpleCircuit2 :: SR ()
simpleCircuit2 =
  groundedCircuit  (vSourceAC 1 1)
                   (parallel  (serial  (resistor 1)  (iCapacitor  0 1))
                              (serial  (resistor 1)  (iInductor   0 1)))
\end{code}
\end{samepage}

\section{Higher-order Modelling with Collections of Models}

We have already seen several higher-order models; for example, the |serial|,
|parallel| and |groundedCircuit| signal relations. This section considers more
higher-order modelling examples, but this time concentrating on signal
relations that are parametrised on collections of signal relations. In
addition, this section puts an emphasis on how the host, higher-order
functional language can provide expressive facilities for higher-order,
noncausal modelling.

Let us define a higher-order signal relation that takes as an argument a list
of signal relations modelling two-pin electrical components and returns the
signal relation that models serial connection of the given electrical
components. This would be useful to model electronic transmission lines, for
example.

\begin{code}
serialise :: [SR (Pin,Pin)] -> SR (Pin,Pin)
serialise = foldr serial wire
\end{code}

The definition of the |serialise| signal relation begs for detailed
explanation. The |foldr| function is defined in the standard Haskell prelude
and has the following type signature and definition.

\begin{code}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     =  z
foldr f z (x:xs) =  f x (foldr f z xs)
\end{code}

The function takes as arguments a binary operator, a starting value that is
typically the right-identity of the binary operator, and a list. The |foldr|
function folds the list using the binary operator, from right to left:

\begin{code}
foldr serial wire [sr1, sr2, ..., srn] =
  sr1 `serial` (sr2 `serial` ... (srn `serial` wire) ... )
\end{code}

Here the higher-order signal relation |serial| is in the role of a binary
operator and the |wire| signal relation is in the role of a starting value
which is a right identity of the binary operator. Figure \ref{figSerialise}
graphically demonstrates the result of this application of the |foldr|
function.

\begin{figure}
\includegraphics[width=\textwidth]{Graphics/serialise}

\caption{\label{figSerialise} Serial connection of electrical components.}

\end{figure}

The |wire| signal relation models an electrical wire and is defined as follows.

\begin{code}
wire :: SR (Pin,Pin)
wire = [rel| ((p_i,p_v),(n_i,n_v)) ->
    $twoPin$ <> ((p_i,p_v),(n_i,n_v),u)
    u = 0
|]
\end{code}

Just like other two-pin electrical components, the |wire| signal relation is
modelled by extending the |twoPin| signal relation with a suitable equation.

The |wire| signal relation is both left and right identity of the |serial|
higher-order signal relation as stated by the following equation and illustrated in
Figure \ref{figSerialWire}.

\begin{code}
wire `serial` sr = sr `serial` wire = sr
\end{code}

\begin{figure}
\includegraphics[width=\textwidth]{Graphics/serialWire}

\caption{\label{figSerialWire} The |wire| signal relation as a left and right
identity of the |serial| higher-order signal relation.}

\end{figure}

The |serial| signal relation is also associative:

\begin{code}
sr1 `serial` (sr2 `serial` sr3) = (sr1 `serial` sr2) `serial` sr3
\end{code}

Here by the equality of the signal relations we mean that the signal relations
introduce equivalent constraints (i.e., one constraint implies the other and
vice versa), and not necessarily the same equations. Because the |wire| signal
relation is both left and right identity of the |serial| binary function and
the |serial| signal relation is also associative, in the definition of the
|serialise| signal relation we could also use the left fold instead of the
right fold.

Somewhat similarly to the |serialise| signal relation the higher-order signal
relation |parallelise| that takes as an argument a list of signal relations
modelling two-pin electrical components and returns the signal relation that
models parallel connection of the given electrical components can be defined
as follows:

\begin{code}
parallelise :: [SR (Pin,Pin)] -> SR (Pin,Pin)
parallelise = foldr parallel noWire
\end{code}

The |noWire| signal relation is defined as follows:

\begin{code}
noWire :: SR (Pin,Pin)
noWire = [rel| ((p_i,p_v),(n_i,n_v)) ->
    $twoPin$ <>((p_i,p_v),(n_i,n_v),u)
    p_i = 0
|]
\end{code}

The |noWire| signal relation is both left and right identity of the |parallel|
higher-order signal relation as stated by the following equation and
illustrated in Figure \ref{figParallelNoWire}.

\begin{code}
noWire `parallel` sr = sr `parallel` noWire = sr
\end{code}

\begin{figure}
\includegraphics[width=\textwidth]{Graphics/parallelNoWire}

\caption{\label{figParallelNoWire} The |noWire| signal relation as a left and
right identity of the |parallel| higher-order signal relation.}

\end{figure}

The |parallel| signal relation is associative:

\begin{code}
sr1 `parallel` (sr2 `parallel` sr3) = (sr1 `parallel` sr2) `parallel` sr3
\end{code}


\section{Structurally Dynamic Modelling}

For a concrete example of structurally dynamic modelling in Hydra, let us
model the breaking-pendulum system described in Section
\ref{secHybridModelling}. The system system has two modes of operation. The
differences between the two modes are sufficiently large that, for example,
Modelica does not support noncausal modelling of this system, as discussed in
Section \ref{secHybridModelling}.

The code that is given in Figure \ref{figHydraBreakingPendulum} shows how to
model the two modes of the pendulum in Hydra. The type |Body| denotes the
state of the pendulum body; that is, its position and velocity, where position
and velocity both are 2-dimensional vectors represented by pairs of reals.
Each model is represented by a function that maps the parameters of the model
to a relation on signals. In the unbroken mode, the parameters are the length
of the rod |l| and the initial angle of deviation |phi0|. In the broken mode,
the signal relation is parametrised on the initial state of the body. Once
again, note that the equations that are marked by the keyword |init| are
initialisation equations used to specify initial conditions.

\begin{figure}
\begin{code}
type Pos   =  (Real,Real)
type Vel   =  (Real,Real)
type Body  =  (Pos,Vel)

g :: Real
g = 9.81

freeFall :: Body -> SR Body
freeFall ((x0,y0),(vx0,vy0)) = [rel| ((x,y),(vx,vy)) ->
    init (x,y)       =  ($x0$,$y0$)
    init (vx,vy)     =  ($vx0$,$vy0$)
    (der x,der y)    =  (vx,vy)
    (der vx,der vy)  =  (0,-$g$)
|]

pendulum ::  Real -> Real -> SR Body
pendulum l phi_0 = [rel| ((x,y),(vx,vy)) ->
    local phi

    init phi      =  $phi_0$
    init der phi  =  0
    init vx       =  0
    init vy       =  0

    x             =     $l$  *  sin phi
    y             =  -  $l$  *  cos phi
    (vx,vy)       =  (der x, der y)
    der (der phi) + ($ g / l $) * sin phi = 0
|]
\end{code}

\caption{\label{figHydraBreakingPendulum} Signal relations modelling the two
modes of the pendulum.}

\end{figure}

To model a pendulum that breaks at some point, we need to create a composite
signal relation where the signal relation that models the dynamic behaviour of
the unbroken pendulum is replaced, at the point when it breaks, by the signal
relation modelling a free falling body. These two submodels must be suitably
joined to ensure the continuity of both the position and velocity of the body
of the pendulum.

To this end, the |switch| combinator is used:

\begin{code}
breakingPendulum :: Real -> Real -> Real -> SR Body
breakingPendulum t l phi0 =
  switch (pendulum l phi0) [fun| \ _ -> time - $t$ |] (\b -> freeFall b)
\end{code}

In this signal relation, the switch happens at an \emph{a priori} specified
point in time, but a switching condition can be derived from an arbitrary
time-varying entity. Note how the succeeding signal relation (i.e.,
|freeFall|) is initialised so as to ensure the continuity of the position and
velocity as discussed above. The simulation results obtained by the |simulate|
function can be seen in Figure \ref{figPendulumPlot}

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/pendulumPlot.pdf}
\end{center}

\caption{\label{figPendulumPlot} Plot showing how |x| and |y| coordinates of
the body on the breaking pendulum change over time.}

\end{figure}

In the breaking pendulum example the |switch| combinator was used to
dynamically add and remove signal variables and noncausal equations. The
|switch| combinator can also be used when the number of equations and
variables remain unchanged during the simulation. The book by
\citet{Cellier2006} gives one such example: the half-wave rectifier circuit
with an ideal diode and an in-line inductor that is depicted in Figure
\ref{figRectifier}.

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/rectifier.pdf}
\end{center}

\caption{\label{figRectifier} Half-wave rectifier circuit with an ideal diode
and an in-line inductor.}

\end{figure}

The half-wave rectifier circuit can be modelled easily in languages like
Modelica. However, any attempt to simulate this model assuming fixed
causality, as current mainstream noncausal language implementations tend to,
will fail as the causalised model will lead to a division by zero when the
switch is open: there simply is no one fixed causality model that is valid
both when the switch is open and closed.

One common solution to the division-by-zero problem is to avoid the ideal
model and opt for a leaky diode model instead. This works, but often leads to
very stiff equations. Thus, if an ideal model would suffice for the purpose at
hand, that would be preferable \citep{Cellier2006}.

Let us model the half-wave rectifier circuit in Hydra. The following two
signal relations model initially opened (|ioDiode|) and initially closed
(|icDiode|) ideal diodes. Note the use of the host language feature of mutual
recursion in the following definitions allowing for signal relations to switch
into each other.

\begin{code}
ioDiode  ::  SR (Pin,Pin)
ioDiode  =   switch   nowire  [fun| ((_,p_v),(_,n_v))  ->  p_v - n_v  |]  ( \ _ -> icDiode)
icDiode  ::  SR (Pin,Pin)
icDiode  =   switch   wire    [fun| ((p_i,_),(_,_))    ->  p_i        |]  ( \ _ -> ioDiode)
\end{code}

The switches are controlled by the polarity of the voltage and the current
through the component. Now we can assemble the half-wave rectified circuit
into a single signal relation using the higher-order connection combinators
defined earlier in this chapter.

\begin{code}
halfWaveRectifier :: SR ()
halfWaveRectifier =
  groundedCircuit  (vSourceAC 1 1)
                   (serialise  [ iInductor 0 1
                               , resistor 1
                               , parallel   (iCapacitor 0 1)
                                            (serial icDiode (resistor 1))
                               ]
                   )
\end{code}

Partial simulation results of the |halfWaveRectifier| signal relation obtained
by using the |simulate| function are presented in Figure
\ref{figRectifiedCapVol} and in Figure \ref{figRectifierIndCur}

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/rectifierCapVol.pdf}
\end{center}

\caption{\label{figRectifiedCapVol} Voltage across the capacitor in the
half-wave rectifier circuit with in-line inductor.}

\end{figure}

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/rectifierIndCur.pdf}
\end{center}

\caption{\label{figRectifierIndCur} Current through the inductor in the
half-wave rectifier circuit with in-line inductor.}

\end{figure}


Simulation of the full-wave rectifier circuit given in Figure
\ref{figRectifierFullWave} is more challenging than simulating the half-wave
rectifier \citep{Nilsson2010a}. A key difficulty is that the circuit breaks
down into two isolated halves when all diodes are open. The lack of a ground
reference for the left part means the system becomes under-determined and it
cannot be simulated.

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/rectifierFullWave.pdf}
\end{center}

\caption{\label{figRectifierFullWave} Full-wave rectifier circuit with ideal
diodes.}

\end{figure}

However, a more detailed analysis reveals that this is as it should be as the
model is incomplete: for the model to make sense, there is further tacit
modelling knowledge that needs to be stated explicitly in the form of
additional equations. If the diodes are truly ideal, this means that they are
also identical, which in turn implies that the voltage drops over them are
always going to be pairwise equal, even when they are open. The model for the
full circuit can be described along the lines we saw in the previous section,
except that two extra equations, stating the pairwise equality of the voltages
across the diodes, are needed. That is:

\begin{eqnarray}
(dp_{v_{1}} - dn_{v_{1}}) & = & (dp_{v_{3}} - dn_{v_{3}}) \label{eq:ud1=ud3} \\
(dp_{v_{2}} - dn_{v_{2}}) & = & (dp_{v_{4}} - dn_{v_{4}}) \label{eq:ud2=ud4}
\end{eqnarray}

However, adding Eq.~(\ref{eq:ud1=ud3}) and Eq.~(\ref{eq:ud2=ud4}) results in
additional complications for simulation as the system now seemingly becomes
over-determined when some diodes are closed. It turns out, though, that the
system is only trivially over-determined; that is, the extra equations are
equivalent to other equations in the system. This is easy to see: when a diode
is closed, there is an equation provided by the model of the diode itself that
states that the voltage across it is 0. If, for example, $D1$ and $D3$ are
closed, we have:

\begin{eqnarray}
(dp_{v_{1}} - dn_{v_{1}}) & = & 0 \label{eq:ud1=0} \\
(dp_{v_{3}} - dn_{v_{3}}) & = & 0 \label{eq:ud3=0}
\end{eqnarray}

But, additionally, $(dp_{v_{1}} - dn_{v_{1}})$ and $(dp_{v_{3}} - dn_{v_{3}})$
are related by Equation \ref{eq:ud1=ud3} that is provided by model of the
overall circuit.

In this case, a simple symbolic simplification pass involving substitution of
algebraic variables and constant folding suffices to eliminate the redundant
equations in the modes where the diodes are pairwise closed. Using Equation
\ref{eq:ud1=0} and Equation \ref{eq:ud3=0}, Equation \ref{eq:ud1=ud3} can be
simplified to the trivially satisfied equation $0 = 0$ that then can be
eliminated. After this the model can be simulated without further issues. Note
that dynamic generation of equations followed by symbolic processing, as
provided by Hydra, is crucial to this approach to simulating ideal diodes.
Studies to precisely characterise in which circumstances can over determined
systems of equations simplified by eliminating the redundant equations still
lie ahead.

As we have already mentioned, the implementation of Hydra provides an
extensible and configurable symbolic processor. The symbolic processor that
simplifies trivially over-determined equations is provided with the
implementation of Hydra and can be activated through the experiment
description passed to the |simulate| function.

It should be pointed out that changes caused by an instance of a switch only
concern equations originating from that switch instance. All other equations
remain as they were. Thus, even though, in the case of ideal diodes, a circuit
with $n$ diodes has up to $2^n$ distinct structural configurations or modes,
it is always entirely clear which mode to move to after a switch; there is no
need to search among the up to $2^n$ possibilities for a consistent successor
mode.

As a result, we have obtained a model of an ideal full-wave rectifier that is
constructed in a modular way from individual, reusable components. The proper
behaviour emerges from simply assembling the components, with just some minor
additional guidance from the modeller in the form of a couple of extra
equations. There is no need for any heavyweight, auxiliary mechanisms, such as
an explicit finite state machine, to control how the model moves between
structural configurations.

\section{Unbounded Structurally Dynamic Modelling}
\label{secUnboundedStructDyn}

The breaking pendulum, half-wave rectifier and full-wave rectifier examples
feature a priori bounded number modes of operation. In principle (with a
suitable language design and implementation) it is feasible to generate code
for these modes of operation prior to simulation. However, despite their
simplicity, these are examples with which mainstream noncausal languages such
as Modelica struggle, as mentioned earlier.

In general, it is not possible to compile Hydra models prior to simulation.
For example, given a parametrised signal relation |sr' :: Real -> SR Real| and
a signal function |sf :: SF Real Real| one can recursively define a signal
relation |sr| that describes an overall behaviour by ``stringing together''
the behaviours described by |sr'|:

\begin{code}
sr :: Real -> SR Real
sr x = switch (sr' x) sf sr
\end{code}

In this case, because the number of instantiations of |sr'| in general cannot
be determined statically and because each instantiation can depend on the
parameter in arbitrarily complex ways, there is no way to generate all code
prior to simulation.

Perhaps the example involving the |sr| signal relation is a bit abstract. In
the following we emphasise the same point by using a variation on the familiar
bouncing-ball example. Assuming elastic collision with the floor, the bouncing
ball system can be modelled in Hydra as follows.

\begin{code}
bouncingBall :: Body -> SR Body
bouncingBall b = switch   (freeFall b)
                          [fun| ((_,y),_) -> y |]
                          (\(p,(vx,vy)) -> bouncingBall (p,(vx, - vy))
\end{code}

This example involves stringing of the |bouncingBall| signal relation. But
even here, in principle, it is possible to generate the code prior to
simulation, because the active equations always remain the same; that is, only
initial the condition is changing.

The following code models a variation of the bouncing ball example where the
ball breaks at every collision with the floor.

\begin{code}
bouncingBall' :: Body -> SR Body
bouncingBall' b = switch  (freeFall b)
                          [fun| ((_,y),_) -> y |]
                          (\(p,v) -> divide (p,v))

divide :: Body -> SR Body
divide ((x0,y0),(vx0,vy0)) = [rel| ((x,y),(vx,vy)) ->
    $bouncingBall'  ((x0,y0),(   vx0 / 2,  -  vy0 / 2))$   <>  ((x,y),(vx,vy))
    local x' y' vx' vy'
    $bouncingBall'  ((x0,y0),(-  vx0 / 2,  -  vy0 / 2))$   <>  ((x',y'),(vx',vy'))
|]
\end{code}

The model assumes that the kinetic energy is not lost and the balls divide the
initial kinetic energy by bouncing in opposite directions. This is an example
of an unbounded structurally dynamic system where the number of modes cannot
be determined prior to simulation.

\section{Simulation}

We conclude this chapter with a brief description of how to simulate Hydra
models, including the ones that are described in this chapter. The
Haskell-embedded implementation of Hydra features the following function:

\begin{code}
simulate :: SR () -> Experiment -> IO ()
\end{code}

The |simulate| function takes two arguments. The first argument is the signal
relation that needs to be simulated. The second argument describes the
\emph{experiment}, essentially a description of what needs to happen during
the simulation. Using the second argument the modeller can set the simulation
starting and ending times, desired time step, symbolic processor, numerical
solver, or how to visualise the trajectories of the constrained signals. The
definition of the |Experiment| data type and the default experiment
description are given in Chapter \ref{chapImplementation}.

For example, the simple circuit model can be simulated using the default
experiment description as follows:

\begin{code}
simulate simpleCircuit defaultExperiment
\end{code}

With the |defaultExperiment| parameter the |simpleCircuit| signal relation is
simulated for 10 seconds of simulation time starting from the time point of
zero. The time step is set to |0.001| and the trajectories of the constrained
signals are printed to the standard output in the
gnuplot\footnote{\url{http://www.gnuplot.info/}} compatible format. The
default numerical solver is SUNDIALS \citep{Sundials2005}, but users are
allowed to provide their own symbolic processors and numerical solvers. This
and other implementation aspects are described in detail in Chapter
\ref{chapImplementation}.

The earlier sections of this chapter introduced the Hydra language using the
simple electrical-circuit example. This example allows readers familiar with
object-oriented, noncausal languages like Modelica to compare the Hydra model
given in this Chapter to the Modelica model given in Chapter
\ref{chapBackground}. The rest of the Chapter focuses on the features of Hydra
that are absent from mainstream, noncausal modelling languages. Specifically,
we discuss the higher-order and structurally dynamic modelling capabilities of
the Hydra language.
