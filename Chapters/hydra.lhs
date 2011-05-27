\chapter{Hydra by Examples}
\label{chapHydra}

This chapter presents the Hydra language informally, by means of instructive
examples. The formal definition of the language and its implementation are
given in Chapter \ref{chapDefinition} and Chapter \ref{chapImplementation},
respectively.

\section{Syntax of Hydra}

Hydra is a two-level language. It features the \emph{functional level} and the
\emph{signal level}. The functional level allows for the definition of
ordinary functions operating on time-invariant values. The signal level allows
for the definition of signal relations and signal functions on on time-varying
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
\ref{chapImplementation} for the details).

The Haskell-embedded implementation of Hydra adopts the following syntax for
defining signal relations:

\begin{code}
[rel| pattern -> equations |]
\end{code}

The symbol | [rel|| | is the opening quasiquote and the symbol | ||] | is the
closing quasiquote. The pattern binds \emph{signal variables} that scope over
the equations that follow. The equations are DAEs stated using \emph{signal
relation application} (the operator |<>|). Signal relation application is how
the constraints embodied by a signal relation are imposed on particular
signals. The equations are required to be well typed. For example, consider
the signal relation application |sr <> s|. Here, if |sr| has the type |SR
alpha| then |s| must have the type |Signal alpha|.

Hydra provides a more conventional-looking syntax for application of the
built-in equality signal relation. For example, the equation |a * x + b = 0| is
equivalent to |(=) <> (a * x + b, 0)|.

In addition to user defined signal relations Hydra provides for user defined
signal functions. Hydra uses the following syntax for defining signal
functions.

\begin{code}
[fun| pattern -> expression |]
\end{code}

Just like for signal relations quasi-quoting is used for defining signal
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
the functional level. This markers are useful when reading Hydra code
listings. The separation is also enforced at the type level of the host
language by the |SR| and |SF| type constructors.

Because signals are not first-class entities at the functional level, it is
not possible to construct a value of type |Signal alpha| directly at the
functional level. Signals only exist indirectly through the signal level
definitions of signal relations and signal functions.

\section{The |switch| Combinator}

The built-in equality signal relation (i.e., |=|) is capable of describing
flat systems of equations and the signal relation application operator (i.e.,
|<>|) provides for hierarchically structured systems of equations. In this
section we introduce one more built-in (higher-order) signal relation that
allows for description of structurally-dynamic signal relations.

\begin{code}
switch :: SR a -> SF a Bool -> (a -> SR a) -> SR a
\end{code}

The |switch| combinator forms a signal relation by temporal composition. The
combinator takes three arguments and returns the composite signal relation (of
type |SR a|). The first argument (of type |SR a|) is a signal relation that is
initially active. The second argument is a signal function (of type |SF a
Bool|). Starting from the first point in time when the boolean signal that is
computed by applying the signal function to the signal constrained by the
composite signal relation changes (from |True| to |False| or from |False| to
|True|), the composite behaviour is defined by the signal relation that is
computed by applying the third argument (a function of type |a -> SR a|) to
the instantaneous value of the constrained signal. A formally defined meaning
of the switch combinator is given in Chapter \ref{chapDefinition}.

The |switch| combinator allows for definition of a signal relation whose
equational description changes over time. In addition, the |switch| combinator
allows for state transfer from the old mode and initialisation of the new mode
using the function that computes the new mode from the instantaneous values of
the constrained signal.

In the signal relation notation described earlier the list of equations that
follows the pattern is not necessarily a static one as the equations may
contain a signal relation application of a structurally-dynamic signal
relation.

\section{Models with Static Structure}

Let us introduce the Hydra language by modelling the circuit that is depicted
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
in the pattern, represent the current into the negative and the voltage at the
negative pin, respectively. The signal variable |u| represents the voltage
drop across the electrical component.

\begin{figure}
\begin{center}
\includegraphics{Graphics/twoPin}
\end{center}
\caption{\label{figTwoPin} Electrical component with two connectors.}
\end{figure}

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

Hydra uses two kinds of variables: the functional-level ones representing
time-invariant parameters, and the signal-level ones, representing
time-varying entities, the signals. Functional-level fragments, such as
variable references, are spliced into the signal level by enclosing them
between antiquotes, \$. On the other hand time-varying entities are not
allowed to escape to the functional level; that is, signal-variables are not
in scope between antiquotes and outside the quasiquotes.

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
flattening can be understood as ``inlining'' of applied signal relations to
reduce the signal relation into a list of signal equality constrains (i.e., a
flat DAE). In the process of flattening, the arguments of a signal relation
application are substituted into the body of the applied signal relation, and
the entire application is then replaced by the instantiated signal relation
body. In our case, the result of flattening the signal relation |resistor 10|
is:

\begin{code}
[rel| ((p_i,p_v),(n_i,n_v)) ->
  local u
  p_v - n_v   =  u
  p_i + n_i   =  0
  10 * p_i    =  u
|]
\end{code}

Models for an inductor, a capacitor, a voltage source and a ground reference
are defined as follows:

\begin{code}
iInductor :: Real -> Real -> SR (Pin,Pin)
inductor p_i_0 l = [rel| ((p_i,p_v),(n_i,n_v)) ->
    init p_i = p_i_0
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    $l$ * der p_i = u
|]

iCapacitor :: Real -> Real -> SR (Pin,Pin)
iCapacitor u0 c = [rel| ((p_i,p_v),(n_i,n_v)) ->
    init u = u0
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    $c$ * der u  = p_i
|]

vSourceAC :: Real -> Real -> SR (Pin,Pin)
vSourceAC v f = [rel| ((p_i,p_v),(n_i,n_v)) ->
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    u = $v$ * sin (2 * $pi$ * $f$ * time)
|]

ground :: SR (Pin)
ground = [rel| (p_i,p_v) where
    p_v = 0
|]
\end{code}

Note that the inductor and the capacitor signal relations contain |init|
equations. An |init| equation is enforced only at the point in time when the
signal relation becomes active. In this example, the |init| equations are used
to initialise the differential variables involved in the inductor and the
capacitor signal relations.

Modelica implicitly initialises state variables to zero. That is why
initialisation equations were not considered in the corresponding Modelica
models given in Chapter \ref{chapBackground}. Hydra does not allow for
implicit initialisation; that is, all initialisation equations must be
specified explicitly.

\section{Noncausal Connections}
\label{secConnections}

Unlike Modelica, Hydra does not provide a special language construct for
specifying noncausal connections; that is, Hydra does not provide a construct
like |connect| from the Modelica language. However, thanks for signal
relations being first-class entities, it is possible to implement higher-order
combinators that facilitate connection of noncausal models.

To model the simple electrical circuit as an interconnection of the already
modelled components let us define three higher-order signal relations
facilitating noncausal connection of two-pin electrical components.

Firstly, we define a higher-order signal relation that takes two signal
relations modelling two-pin electrical components and returns the signal
relation that models the serial connection of the two electrical components.
The graphical representation of the signal relation is given in Figure
\ref{figSerial}.

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

\begin{figure}
\centering
\includegraphics[scale=1.5]{Graphics/serial}
\caption{\label{figSerial} Serial connection of two electrical components}
\end{figure}

Secondly, we define a higher-order signal relation that takes two signal
relations modelling two-pin electrical components and returns the signal
relation that models the parallel connection of the two electrical components.
The graphical representation of the signal relation is given in Figure
\ref{figParallel}.

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

\begin{figure}
\centering
\includegraphics[scale=1.5]{Graphics/parallel}
\caption{\label{figParallel} Parallel connection of two electrical components}
\end{figure}

Finally, we define a higher-order signal relation that takes two
signal relations modelling two-pin electrical components and returns the
signal relation that models the grounded circuit involving the two electrical
components. The graphical representation of the signal relation is given
in Figure \ref{figGroundedCircuit}.

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

\begin{figure}
\centering
\includegraphics[scale=1.5]{Graphics/groundedCircuit}
\caption{\label{figGroundedCircuit} Grounded circuit involving two electrical components}
\end{figure}

Now we can assemble the models of the electrical components into the simple
electrical circuit as follows:

\begin{code}
simpleCircuit :: SR ()
simpleCircuit =
  groundedCircuit  (vSourceAC 1 1)
                   (parallel  (serial (resistor 1) (iCapacitor 0 1))
                              (iInductor 0 1))
\end{code}

Note that the above code is a direct textual representation of how the
components are connected in the circuit. Having said that, unlike the Modelica
model that specifies the noncausal connections in terms of connections of
time-varying variables, Hydra allows for definition of higher-order
combinators that are capable of specifying noncausal connections by connecting
noncausal models directly.

It is trivial in Hydra to reuse the circuit components and model the modified
circuit that is depicted on Figure \ref{figCircuit2}.

\begin{code}
simpleCircuit2 :: SR ()
simpleCircuit2 =
  groundedCircuit  (vSourceAC 1 1)
                   (parallel  (serial  (resistor 1)  (iCapacitor  0 1))
                              (serial  (resistor 1)  (iInductor   0 1)))
\end{code}

\section{Simulation}

The Haskell-embedded implementation of Hydra features the following function:

\begin{code}
simulate :: SR () -> Experiment -> IO ()
\end{code}

The |simulate| function takes two arguments. The first argument is the signal
relation that needs to be simulated. The second argument describes the
\emph{experiment}, essentially a description of what needs to happen during
the simulation. Using the second argument the modeller can set the simulation
starting and ending times, desired time step, symbolic processor, numerical
solver, whether to use JIT compilation or interpretation, or how to visualise
the trajectories of the constrained signals. The definition of the
|Experiment| data type and the default experiment description are given in
Chapter \ref{chapImplementation}.

The simple circuit model can be simulated using the default experiment
description as follows:

\begin{code}
simulate simpleCircuit defaultExperiment
\end{code}

With the |defaultExperiment| parameter the |simpleCircuit| signal relation is
simulated for 10 seconds of simulation time starting from the time point of
zero. The time step is set to |0.001| and the trajectories of the constrained
signals are printed to the standard output in the
gnuplot\footnote{url{http://www.gnuplot.info/}} compatible format. By default
the simulation code is JIT compiled. The default numerical solver is SUNDIALS
\citep{Sundials2005}. The users are allowed to provide their own symbolic
processors and numerical solvers. This and other implementation aspects are
described in detail in Chapter \ref{chapImplementation}.

The earlier sections of this chapter introduced the Hydra language using the
simple electrical circuit example. This example allows readers familiar with
object-oriented, noncausal languages like Modelica to compare the Hydra model
given in this Chapter to the Modelica model given in Chapter
\ref{chapBackground}. The rest of the Chapter focuses on the features of Hydra
that are absent from main-stream, noncausal modelling languages. Specifically,
we discuss higher-order and structurally modelling capabilities of the Hydra
language.

\section{More Higher-order Modelling}

We have already seen several higher-order models; for example, |serial|,
|parallel| and |groundedCircuit| signal relations. This section considers more
higher-order modelling examples, but this time concentrating on signal
relations that are parametrised on collections of signal relations. The
examples are again from the physical domain of electronics.

Let us define a higher-order signal relation that takes as an argument a list
of signal relations modelling two-pin electrical components and returns the
signal relation that models serial connection of the given electrical
components. The following higher-order signal relation can be used to model
electronic transmission lines, for example.

\begin{code}
serialise :: [SR (Pin,Pin)] -> SR (Pin,Pin)
serialise = foldr serial wire
\end{code}

This definition begs for detailed explanation. The |foldr| function is defined
in the standard Haskell prelude and has the following type signature and
definition.

\begin{code}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     =  z
foldr f z (x:xs) =  f x (foldr f z xs)
\end{code}

The function takes as arguments a binary operator, a starting value which is
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
\caption{\label{figSerialise} Serial connection of electrical components}
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
higher-order signal as stated by the following equation and illustrated in
Figure \ref{figSerialWire}.

\begin{code}
wire `serial` sr = sr `serial` wire = sr
\end{code}

\begin{figure}
\includegraphics[width=\textwidth]{Graphics/serialWire}
\caption{\label{figSerialWire} The |wire| signal relation as a left and right identity of the |serial| higher-order signal relation.}
\end{figure}

Here by the equality of the signal relations we mean that the signal relations
introduce equivalent constrains, and not necessarily the same equations.
Because the |wire| signal relation is both left and right identity of the
|serial| binary function, in the definition of the |serialise| signal relation
we could also use the left fold instead of the right fold.

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
\caption{\label{figParallelNoWire} The |noWire| signal relation as a left and right identity of the |parallel| higher-order signal relation.}
\end{figure}

In addition to demonstrating higher-order modelling capabilities of Hydra,
this section puts an emphasis on how the host, higher-order functional
language can provide expressive facilities for higher-order, noncausal
modelling.

\section{Structurally-dynamic Modelling}

To introduce structurally-dynamic modelling in Hydra, let us model a physical
system whose structural configuration changes abruptly during simulation: a
simple pendulum that can break at a specified point in time; see Figure
\ref{figPendulum}. The pendulum is modelled as a body represented by a point
mass $m$ at the end of a rigid, mass-less rod, subject to gravity $m \vec{g}$.
If the rod breaks, the body will fall freely. This makes the differences
between the two configurations sufficiently large that, for example, Modelica
does not support noncausal modelling of this system, as we discussed in
Chapter \ref{chapBackground}.

The following code shows how to model the two modes of the pendulum in Hydra.

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
represented by pairs of reals. Each model is represented by a function that
maps the parameters of the model to a relation on signals. In the unbroken
mode, the parameters are the length of the rod |l| and the initial angle of
deviation |phi0|. In the broken mode, the signal relation is parametrised on
the initial state of the body. Once again, note that the equations that are
marked by the keyword |init| are initialisation equations used to specify
initial conditions.

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
  switch (pendulum l phi0) [fun|\_ -> time >= $t$ |] (\b -> freeFall b)
\end{code}

In this signal relation, the switch happens at an \emph{a priori} specified
point in time, but the switching condition could be an arbitrary time-varying
entity. Note how succeeding signal relation (i.e., |freeFall|) is initialised
so as to ensure the continuity of the position and velocity as discussed
above. The simulation results obtained by the |simulate| function can be seen
in Figure \ref{figPendulumPlot}

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/pendulumPlot.pdf}
\end{center}
\caption{\label{figPendulumPlot} Plot showing how |x| and |y| coordinates of the body on the breaking pendulum change over time.}
\end{figure}

We have already remonstrated how the |switch| combinator can be used to
dynamically add and remove signal variables and noncausal equations. This
flexibility is useful even when the number of equations and variables remain
unchanged during the simulation. The book by \citet{Cellier2006} gives one
such example: the half-wave rectifier circuit with ideal diode and in-line
inductor that is depicted in Figure \ref{figRectifier}.

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/rectifier.pdf}
\end{center}
\caption{\label{figRectifier} Half-wave rectifier circuit with ideal diode and in-line inductor.}
\end{figure}

The half-wave rectifier circuit can be modelled easily in languages like
Modelica. However, any attempt to simulate this model assuming fixed
causality, as current main-stream noncausal language implementations tend to,
will fail as the causalised model will lead to a division by zero when the
switch is open: there simply is no one fixed causality model that is valid
both when the switch is open and closed.

One reason is that noncausal modelling languages tend to be designed and
implemented on the assumption that the causality of the model does not change
during simulation. This assumption simplifies the language design and
facilitates the generation of efficient simulation code. In particular, the
causality can be analysed and code can be generated once and for all, at
compile time, paving the way for using a fast, explicit solver for simulation
as demonstrated in Chapter \ref{chapBackground}.

One common solution to the division-by-zero problem in models involving ideal
diodes is to avoid the ideal model and opt for a leaky diode model instead.
This works, but often leads to very stiff equations. Thus, if an ideal model
would suffice for the purpose at hand, that would many times be preferable
\citep{Cellier2006}.

In the following we model the half-wave rectifier circuit in Hydra. Since the
Hydra language is not predicated on compilation of simulation all at once and
allows for runtime symbolic processing it addresses the shortcomings of
main-stream, noncausal modelling languages discussed earlier in this section.

The following two signal relations model initially opened (|ioDiode|) and
initially closed (|icDiode|) ideal diodes. Note the use of the host language
feature of mutual recursion in the following definitions allowing for signal
relations to switch into each other.

\begin{code}
ioDiode  ::  SR (Pin,Pin)
ioDiode  =   switch   nowire  [fun| ((_,p_v),(_,n_v))  ->  p_v - n_v > 0 |]  (\_ -> icDiode)
icDiode  ::  SR (Pin,Pin)
icDiode  =   switch   wire    [fun| ((p_i,_),(_,_))    ->  p_i < 0 |]        (\_ -> ioDiode)
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
                               , icDiode
                               , parallel (iCapacitor 0 1) (resistor 1)
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
\caption{\label{figRectifiedCapVol} Voltage across the capacitor in the half-wave rectifier circuit with in-line inductor.}
\end{figure}

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/rectifierIndCur.pdf}
\end{center}
\caption{\label{figRectifierIndCur} Current through the inductor in the half-wave rectifier circuit with in-line inductor.}
\end{figure}


\section{Highly Structurally-dynamic Modelling}

In the breaking pendulum example and in the half-wave rectifier example
feature only two modes of operation. In principle (with a suitable language
design and implementation) it is possible to generate code for two modes of
operation prior to simulation. However, despite their simplicity, these are
examples with which main-stream noncausal languages such as Modelica struggle,
as mentioned earlier.

Because of the expressivity of the |switch| combinator, in general, it is not
possible to compile Hydra models prior to simulation. For example, given a
parametrised signal relation |sr' :: Real -> SR Real)| and a signal function
|sf :: SF Real Bool| one can recursively define a signal relation |sr| that
describes an overall behaviour by ``stringing together'' the behaviours
described by |sr'|:

\begin{code}
sr :: Real -> SR Real
sr x = switch (sr' x) sf sr
\end{code}

In this case, because the number of instantiations of |sr'| in general cannot
be determined statically and because each instantiation can depend on the
parameter in arbitrarily complex ways, there is no way to generate all code
prior to simulation.

Perhaps the example involving the |sr| signal relation is a bit abstract. In
the following sections we emphasise the same point by using a variation on the
familiar bouncing-ball example. Assuming the elastic collision with the floor,
the bouncing ball system can be modelled in Hydra as follows.

\begin{code}
bouncingBall :: Body -> SR Body
bouncingBall b = switch   (freeFall b)
                          [fun| ((_,y),_) -> y < 0 |]
                          (\(p,(vx,vy)) -> bouncingBall (p,(vx, - vy))
\end{code}

This example involves stringing of the |bouncingBall| signal relation. But
even here, in principal, it is possible to generate the code prior to
simulation, because the active equations always remain the same; that is, only
initial the condition is changing.

The following code models a variation of the bouncing ball example where the
ball breaks at every collision with the floor.

\begin{code}
bouncingBall' :: Body -> SR Body
bouncingBall' b = switch  (freeFall b)
                          [fun| ((_,y),_) -> y < 0 |]
                          (\(p,v) -> divide (p,v))

divide :: Body -> SR Body
divide ((x0,y0),(vx0,vy0)) = [rel| ((x,y),(vx,vy)) ->
    $bouncingBall'  ((x0,y0),(   vx0 / 2,  -  vy0 / 2))$   <>  ((x,y),(vx,vy))
    local x' y' vx' vy'
    $bouncingBall'  ((x0,y0),(-  vx0 / 2,  -  vy0 / 2))$   <>  ((x',y'),(vx',vy'))
|]
\end{code}

The model assumes that the kinetic energy is not lost and the balls divide the
initial kinetic energy by bouncing to opposite directions. This is an example
of a highly structurally-dynamic system; the number of modes can not be
determined prior to simulation and it is not feasible to generate the code
prior to simulation.

Unfortunately, due to the limitations of main-stream noncausal modelling
languages, declarative equational modelling of (highly) structurally-dynamic
systems remains an elusive application. We believe the adoption of the Hydra
features described in this chapter will remedy this unfortunate situation.