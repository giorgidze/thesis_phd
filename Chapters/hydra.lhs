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

Let us introduce the Haskell embedding of Hydra by modelling the circuit in
Figure \ref{figSimpleCircuit}. We first define a |twoPin| model: a
signal relation that captures the common behaviour of electrical components
with two connectors (see Figure \ref{figTwoPin}):

\begin{code}
twoPin :: SigRel
twoPin = [hydra|
  sigrel ((flow p_i,p_v),(flow n_i,n_v),u) where
    p_v  -  n_v  =  u
    p_i  +  n_i  =  0
|]
\end{code}

The signal variables |p_i| and |p_v| represent the current into the component
and the voltage at the positive pin. The signal variables |n_i| and |n_v|
represent the current into the component and the voltage at the negative pin.
The signal variable |u| represents the voltage drop across the electrical
component. Signal variables in the |sigrel| pattern qualified as |flow| are
called \emph{flow} signal variables. Signal variables without any qualifier are
called \emph{potential} signal variables. The distinction between flow and
potential variables is central to the meaning of the |connect| construct
as discussed in Section~\ref{subSecConnections}.

\begin{figure}
\begin{center}
\includegraphics{Graphics/twoPin}
\end{center}
\caption{An electrical component with two connectors}
\label{figTwoPin}
\end{figure}

The symbols | [hydra|| | and | ||] | are the quasiquotes. At compile time, GHC
applies the user-defined parsing function named in the opening quote to the
text between the quotes. Here, the function is called $\mathit{hydra}$.
It has type |String -> SigRel| and parses the concrete version of the Hydra 
syntax defined in Section~\ref{secSyntax}. Values of type |SigRel| are ASTs
representing Hydra signal relations. This enables the embedded Hydra 
compiler to process them symbolically and ultimately compile them into
simulation code.

% By using the |twoPin| signal relation we define signal relations
% representing a resistor:

We can now use |twoPin| to define a model for a resistor parametrised with
respect to the resistance. Note that a parametrised model simply is an
ordinary function returning a signal relation:
\begin{code}
resistor :: Double -> SigRel
resistor r = [hydra|
  sigrel ((flow p_i,p_v),(flow n_i,n_v)) where
    $twoPin$ <> ((p_i,p_v),(n_i,n_v),u)
    $r$ * p_i = u
|]
\end{code}
Expressions between dollar signs are \emph{antiquoted} Haskell expressions. All
variables in antiquoted expressions must be in the Haskell scope. Using this
technique, a modeller can splice in Haskell expressions in the Hydra
models. 

The current implementation only allows antiquoting of Haskell expressions of
type |SigRel| in the left hand side of signal relation applications and of
type |Double| in signal expressions. The result spliced in to the left in a
signal relation application is thus an entire AST representing a signal
relation, as required by the abstract syntax (see Section~\ref{secSyntax}). 
Antiquoted expressions must have the correct type, i.e |SigRel| and |Double|
respectively. Type-incorrect, antiquoted expressions are detected by GHC at
compile time.

In this case, note how antiquoting is used to splice in a copy of the |twoPin|
model; that is, its equations are \emph{reused} in the context of the resistor
model. Alternatively, this can be viewed as defining the resistor model by
extending the |twoPin| model with an equation that characterises the specific
concrete electrical component, in this case Ohm's law.

To clearly see how |twoPin| contributes to the definition of |resistor|, let
us consider what happens when the resistor model is \emph{flattened} as part
of flattening of a complete model, a transformation that is described in
detail in Section \ref{subSecFlattening}. Intuitively, flattening can be
understood as ``inlining'' of applied signal relations. Thus, the arguments of
a signal relation application is substituted into the body of the applied
signal relation, and the entire application is then replaced by the
instantiated signal relation body. In our case, the result of flattening the
signal relation |resistor 10| is:
\begin{code}
sigrel ((flow p_i,p_v),(flow n_i,n_v)) where
  p_v  -  n_v  =  u
  p_i  +  n_i  =  0
  10   *  p_i  =  u
\end{code}

% Once a complete model has been assembled (which in general involves
% expanding |connect| constructs as explained in
% Section~\ref{subSecConnections}), the resulting AST is simplified by
% substituting the arguments of a signal relation application into the body of
% the applied signal relation, and by then replacing the entire application
% with the result. This is known as \emph{flattening} of a model, and the
% process is illustrated in detail in Section \ref{subSecFlattening}. However,
% for a small example illustrating the idea, the result of flattening
% |resistor 10| would be:

Models for an inductor, a capacitor, a voltage source and a ground are defined
similarly:

\begin{code}
inductor :: Double -> SigRel
inductor l = [hydra|
  sigrel ((flow p_i,p_v),(flow n_i,n_v)) where
    $twoPin$ <> ((p_i,p_v),(n_i,n_v),u)
    $l$ * der p_i = u
|]

capacitor :: Double -> SigRel
capacitor c = [hydra|
  sigrel ((flow p_i,p_v),(flow n_i,n_v)) where
    $twoPin$ <> ((p_i,p_v),(n_i,n_v),u)
    $c$ * der u  = p_i
|]

vSourceAC :: Double -> Double -> SigRel
vSourceAC v f = [hydra|
  sigrel ((flow p_i,p_v),(flow n_i,n_v)) where
    $twoPin$ <> ((p_i,p_v),(n_i,n_v),u)
    u = $v$ * sin (2 * $pi$ * $f$ * time)
|]

ground :: SigRel
ground = [hydra|
  sigrel (flow p_i,p_v) where
    p_v = 0
|]
\end{code}

\subsection{Non-Causal Connections}
\label{subSecConnections}

To facilitate composition of signal relations, Hydra provides a
Modelica-inspired |connect| construct. Using this, a complete model for
the circuit of Figure~\ref{figSimpleCircuit} can be defined as follows:
\begin{code}
simpleCircuit :: SigRel
simpleCircuit = [hydra|
  sigrel (flow i,u) where
    $vSourceAC 1 1$  <>  ((acp_i,acp_v),(acn_i,acn_v))
    $resistor 1$     <>  ((rp_i,rp_v),(rn_i,rn_v))
    $inductor 1$     <>  ((lp_i,lp_v),(ln_i,ln_v))
    $capacitor 1$    <>  ((cp_i,cp_v),(cn_i,cn_v))
    $ground$         <>  (gp_i,gp_v)
    
    connect acp_i rp_i lp_i
    connect acp_v rp_v lp_v
    connect rn_i cp_i
    connect rn_v cp_v
    connect acn_i cn_i ln_i gp_i
    connect acn_v cn_v ln_v gp_v
    
    i = acp_i
    u = acp_v - acn_v
|]
\end{code}
Note how the above code is a direct textual representation of how the
components are connected in the example circuit.

In the setting of Hydra, the |connect| construct is just syntactic sugar with
the following rules\footnote{These rules may be relaxed in the future to allow
  connection of, for example, aggregated signal variables.}:
\begin{itemize}
\item The special keyword |connect| takes two or more signal variables.
\item A signal variable may not appear in more than one connect statement.
\item Connection of flow signal variables with potential signal variables is
      not allowed.
\end{itemize}
For connected flow variables, sum-to-zero equations are generated. In the
electrical domain, this corresponds to Kirchhoff's current law. For potential
variables, equality constraints are generated. In the electrical domain, this
asserts that the voltage at connected pins is equal. The connect constructs of
|simpleCircuit| are thus expanded to the following equations:
\begin{code}
acp_i  +  rp_i  +  lp_i  =  0
acp_v  =  rp_v  =  lp_v

rn_i   +  cp_i  =  0
rn_v   =  cp_v

acn_i  +  cn_i  +  ln_i  +  gp_i  = 0
acn_v  =  cn_v  =  ln_v  =  gp_v
\end{code}
Note that the notion of flows and potentials are common to many physical
domains. For example, the Modelica standard library employs connections for
electrical, hydraulic, and mechanical applications, among others.

In Hydra, the expansion of connect constructs into the sum-to-zero and
equality constraints is straightforward. In particular, note that all signal
variables are counted positively in the sum to zero equations. This is
different from Modelica \cite{Modelica2007} where a special ``rule of signs''
is used to determine which flow variables go with a plus sign and which go
with a minus sign. Hydra obviates the need for a rule of signs by treating
flow signal in signal relation applications specially, thus keeping
the generation of connection equations simple. The idea is to consider a flow
variable in a |sigrel| pattern as two variables, one internal and one
external, related by the equation
\begin{code}
i = - i'
\end{code}
where |i| is the internal variable and |i'| is the external variable. This
way, flows are always directed from an interface into a component, as it were,
making it possible to always count flows into connection nodes as being
positive.


\section{Higher-Order Modelling}

%Week 3
\section{Structural Dynamism}
\subsection{Breaking Pendulum}

To introduce Hydra, let us model a physical system whose structural
configuration changes abruptly during simulation: a simple pendulum that can
break at a specified point in time; see Fig. \ref{fig:pendulum}. The
pendulum is modelled as a body represented by a point mass $m$ at the end of a
rigid, mass-less rod, subject to gravity $m \vec{g}$. If the rod breaks, the
body will fall freely. This makes the differences between the two
configurations sufficiently large that e.g. Modelica does not support
non-causal modelling of this system. Instead, if simulation across the
breaking point is desired, the modeller is forced to model the system in a
causal, less declarative way \cite[pp. 31--33]{ModelicaTutorial2000}.

\begin{figure}[t]
\begin{center}
\includegraphics[scale=0.80]{Graphics/pendulum}
\caption{\label{fig:pendulum}A pendulum subject to gravity.}
\end{center}
\end{figure}

As implied by the discussion in Sec. \ref{sec:background-fhm} above, there are
two levels to Hydra: the \emph{functional level} and the \emph{signal level}.
The functional level is concerned with the definition of ordinary functions
operating on time-invariant values. The signal level is concerned with the
definition of relations on signals, the \emph{signal relations}, and,
\emph{indirectly}, the definition of the \emph{signals} themselves as
solutions satisfying these relations.

Signal relations are \emph{first-class} entities at the functional level. The
type of a signal relation is parametrised on a \emph{descriptor} of the types
of the signals it relates: essentially a tuple of the types carried by the
signals. For example, the type of a signal relation relating three real-valued
signals is |SR (Real, Real, Real)|.

Signals, in contrast to signal relations, are \emph{not} first-class
entities at the functional level. However, crucially, \emph{instantaneous
values} of signals can be propagated back to the functional level, allowing
the future system structure to depend on signal values at discrete points in
time.

The definitions at the signal level may freely refer to entities defined at
the functional level as the latter are time-invariant, known parameters as far
as solving the equations are concerned. However, the opposite is not allowed:
time-varying entities are confined to the signal level. The only signal-level
notion that exists at the functional level is the \emph{time-invariant} signal
relation.

Hydra is currently implemented as an embedding in Haskell using
\emph{quasiquoting} \cite{Mainland2007,Mainland2008}. This means Haskell
provides the functional level almost for free through shallow embedding. In
contrast, the signal level is realised through deep embedding: signal
relations expressed in terms of Hydra-specific syntax are, through the
quasiquoting machinery, turned into an internal representation, essentially an
abstract syntax tree (AST), that then is compiled into simulation code. This,
along with the reasons for using quasiquoting, is discussed in more detail in
an earlier paper \cite{Giorgidze2009a}. However, that paper only treated
\emph{structurally static} systems.

Fig. \ref{fig:pendulum-model} shows how to model the two modes of the
pendulum in Hydra. The type |Body| denotes the state of the pendulum body;
that is, its position and velocity, where position and velocity both are
2-dimensional vectors represented by pairs of doubles. Each model is
represented by a function that maps the \emph{parameters} of the model to a
relation on signals; that is, an instance of the defining system of DAEs for
specific values of the parameters. In the unbroken mode, the parameters are
the length of the rod |l| and the initial angle of deviation |phi0|. In the
broken mode, the signal relation is parametrised on the initial state of the
body.

\begin{figure}[t]
\begin{minipage}[t]{6.0cm}
\begin{code}
type Coordinate = (Double,Double)
type Velocity = (Double,Double)
type Body = (Coordinate,Velocity)

g :: Double
g = 9.81

freeFall :: Body -> SR Body
freeFall ((x0,y0),(vx0,vy0)) = HBEGIN
  sigrel ((x,y),(vx,vy)) where
    init (x,y)       = ($x0$,$y0$)
    init (vx,vy)     = ($vx0$,$vy0$)
    (der x,der y)    = (vx,vy)
    (der vx,der vy)  = (0,-$g$)
HEND
\end{code}
\end{minipage}
\begin{minipage}[t]{6.0cm}
\begin{code}
pendulum ::  Double -> Double
             -> SR Body
pendulum l phi0 = HBEGIN
  sigrel ((x,y),(vx,vy)) where
    init phi      =  $phi0$
    init der phi  =  0
    init vx       =  0
    init vy       =  0
    x             =     $l$  *  sin phi
    y             =  -  $l$  *  cos phi
    (vx,vy)       =  (der x, der y)
    der (der phi)
      + ($ g / l $) * sin phi = 0
HEND
\end{code}
\end{minipage}
\caption{\label{fig:pendulum-model}Models of the two modes of the pendulum.}
\end{figure}

The character sequences |HBEGIN| and |HEND| are the open and close
quasiquotes. Between them, we have signal-level definitions expressed in our
custom syntax. The keyword |sigrel| starts the definition of a signal
relation. It is followed by a pattern that introduces \emph{signal variables}
giving local names to the signals that are going to be constrained by the
signal relation. This pattern thus specifies the \emph{interface} of a signal
relation.

Note the two kinds of variables: the functional level ones representing
\emph{time-invariant} parameters, and the signal-level ones, representing
\emph{time-varying} entities, the signals. Functional-level fragments, such as
variable references, are spliced into the signal level by enclosing them
between antiquotes, \$. On the other hand time-varying entities are not
allowed to escape to the functional level (meaning signal-variables are not in
scope between antiquotes).

After the keyword |where| follow the equations that define the relation. These
equations may introduce additional signal variables as needed. Equations
marked by the keyword |init| are initialisation equations used to specify
initial conditions. The operator |der| indicates differentiation with
respect to time of the signal-valued expression to which it is applied.

The non-causal nature of Hydra can be seen particularly clearly in the last
equation of the unbroken mode that simply states a constraint on the angle of
deviation and its second derivative, without making any assumption regarding
which of the two time-varying entities is going to be used to solve for the
other (both $g$ and $l$ are time-invariant functional-level variables).

To model a pendulum that breaks at some point, we need to create a
composite model where the model that describes the dynamic behaviour of the
unbroken pendulum is replaced, at the point when it breaks, by the
model describing a free falling body. These two submodels must be suitably
joined to ensure the continuity of both the position and velocity of the body
of the pendulum.

To this end, the |switch|-combinator, which forms signal relations by
temporal composition, is used: 
\begin{code}
switch :: (Eval b) => SR (a, E b) -> (b -> SR a) -> SR a
\end{code}
The composite behaviour is governed by the first signal relation until an
\emph{event} of type |b| occurs (|E b| in the type signature above). At this
point, the second argument to switch is applied to the value carried by the
event to \emph{compute} the signal relation that is going to govern the
composite behaviour from then on. Event signals are \emph{discrete-time}
signals, signals that are only defined at (countably many) discrete points in
time, as opposed to the continuous-time signals that (conceptually) are
defined everywhere on a continuous interval of time. Each point of definition
of an event signal is known as an \emph{event occurrence}. Unlike
continuous-time signals, the causality of event signals is always fixed.

As we will see (Sec. \ref{sec:embedding}), the realisation of this combinator
involves mixed-level embedding techniques as signal relations constructed by
|switch| do have an explicit representation at the abstract syntax level, but
part of this representation is a host-language function. Type class |Eval|
allows for translation of instantaneous values of signals into corresponding
Haskell values. The translation is used in the implementation of the |switch|
combinator.

%% \subsubsection{Breaking the Pendulum}
%% \label{sec:background-hydrabyexample-breaking}

Fig. \ref{fig:pendulum-breaking} shows how |switch| is used to construct a
model of a breaking pendulum. The |pendulum| model is first extended into a
signal relation |pendulumBE| that also provides the event signal that defines
when the pendulum is to break: see Fig. \ref{fig:pendulum-breaking-be}. In
our example, an event is simply generated at an \emph{a priori} specified
point in time, but the condition could be an arbitrary time-varying entity.
The value of the event signal is the state (position and velocity) of the
pendulum at that point, allowing the succeeding model to be initialised so as
to ensure the continuity of the position and velocity as discussed above.

\begin{figure}[t]
% \hspace{-0.5cm}
\subfigure[Pendulum extended with a breaking event]{
% \fbox{
\begin{minipage}[t]{6.5cm}
\begin{code}
pendulumBE ::  Double -> Double -> Double
               -> SR (Body, E Body)
pendulumBE t l phi0 = HBEGIN
  sigrel (  ((x,y),(vx,vy)), event e) where
    $pendulum l phi0$  <>  ((x,y),(vx,vy))
    event  e = ((x,y),(vx,vy))
           when time = $t$
HEND
\end{code}
\end{minipage}
\label{fig:pendulum-breaking-be}
% }
}
\subfigure[Composition using switch]{
% \fbox{
\begin{minipage}[t]{4.5cm}
\begin{code}
breakingPendulum :: SR Body
breakingPendulum =
  switch 
    (pendulumBE 10 1 (pi / 4))
    freeFall
\end{code}
\vspace{0.85cm}
\end{minipage}
\label{fig:pendulum-breaking-switch}
% }
}
\caption{\label{fig:pendulum-breaking}The breaking pendulum}
\end{figure}

To bring the equations of |pendulum| into the definition of |pendulumBE|,
|pendulum| is first applied to the length of the pendulum and the initial
angle of deviation at the \emph{functional level} (within antiquotes), thus
computing a signal relation. This \emph{relation} is then \emph{applied}, at
the signal level, using the \emph{signal relation application operator} |<>|.
This instantiates the equations of |pendulum| in the context of |pendulumBE|.
Unfolding signal relation application in Hydra is straightforward: the actual
arguments (signal-valued expressions) to the right of the signal relation
application operator |<>| are simply substituted for the corresponding formal
arguments (signal variables) in the body of the signal relation to the left of
|<>|, renaming as necessary to avoid name clashes. The process of unfolding
signal relation applications is called \emph{flattening}. It is in many ways
similar to the transformation of hierarchical models in languages like
Modelica into a flat system of equations, a process usually also referred to
as flattening.
% The only real issue is that name capture
%\emph{name capture}, accidental clashes of variable names%
% \footnote{
%   For example, a local variable in a signal relation body having the same
%   name as a variable in one of the actual arguments.%
%},
% must be avoided by an appropriate renaming strategy.
See \cite{Giorgidze2009a} for further details.

Finally, a model of the breaking pendulum can be composed by switching form
|pendulumBE| to |freeFall|: see Fig. \ref{fig:pendulum-breaking-switch}.
Note that the switching event carries the state of the pendulum at the
breaking point as a value of type |Body|. This value is passed to |freeFall|,
resulting in a model of the pendulum body in free fall initialised so as to
ensure the continuity of its position and velocity.

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
In this case, because the number of instantiations of |sr1| in general
cannot be determined statically (and because each instantiation can
depend on the parameter in arbitrarily complex ways), there is no way
to generate all code prior to simulation. However, the pendulum example
is simple and suffice for illustrative purposes. Moreover, despite its
simplicity, it is already an example with which present non-causal languages
struggle, as mentioned above.

In practical terms, the |switch|-combinator is a somewhat primitive way of
describing variable model structure. Our aim is to enrich Hydra with
higher-level constructs as descried in the original FHM paper
\cite{Nilsson2003a}. The basic principles of the implementation should,
however, not change much.



%Week 4
\subsection{Ideal Diodes,Full/Half-way rectifiers}



%}