\chapter{Background}
\label{chapBackground}

\section{Modelling and Simulation of Physical Systems}
\label{secModelling}

This chapter overviews the field of physical modelling and simulation by using
simple and instructive examples. By modelling and simulating the example
physical systems, basic concepts of modelling and simulation are introduced.
Where necessary, the presentation abstracts from the concrete examples and
defines the basic concepts more generally.

\subsection{Mathematical Modelling}

Figure \ref{figCircuit1} depicts a simple electrical circuit. The circuit is
grounded and has the following four two-pin electrical components: voltage
source, resistor, inductor and capacitor. The following system of equations is
a mathematical model of the circuit.

\begin{figure}
\begin{center}
\includegraphics[width = 0.5\textwidth]{Graphics/circuit1}
\end{center}
\caption{\label{figCircuit1} Simple electrical circuit.}
\end{figure}

\begin{subequations}
\begin{eqnarray}
u_S & = & sin(2 \pi t) \\
u_R & = & R \cdot i_1 \\
i_1 & = & C \cdot \frac{du_C}{dt} \\
u_L & = & L \cdot \frac{di_2}{dt} \\
i_1 + i_2 & = & i \\
u_R + u_C & = & u_s \\
u_S & = & u_L
\end{eqnarray}
\end{subequations}

The first four equations describe the component behaviours. The last three
equations describe the circuit topology. The system of equations consists of
implicitly defined algebraic and differential equations. This mathematical
representation is a system of implicit \emph{differential algebraic
equations} (DAEs) \citep{Cellier2006}. More generally, a system of implicit DAEs
can be written in the following form:
\begin{equation}
f(\frac{d\vec{x}}{dt},\vec{x},\vec{y},t) = 0
\end{equation}
Here, $\vec{x}$ is a vector of \emph{differential variables}, also known as
\emph{state variables}, $\vec{y}$ is a vector of algebraic variables and $t$
is an independent scalar variable. In physical modelling $t$ denotes
\emph{time}.


Numerical integration is a widely used approach for deriving approximate
solutions of systems of DAEs. This is partly because, in general, exact
symbolic methods do not suffice for solving systems of DAEs
\citep{Brenan1996a}. There are a number of methods for numerical integration
of an implicit DAE. For example, there are numerical solvers that directly
operate on the implicit representation (e.g., the IDA solver from the SUNDIALS
numerical suite \citep{Sundials2005}), but in some cases it is possible to
translate a DAE into a system of explicit \emph{ordinary differential
equations} (ODEs), which makes it possible to simulate the system using an ODE
solver (e.g., the CVODE solver from the SUNDIALS numerical suite
\citep{Sundials2005}). In the following we illustrate the latter approach.

\subsection{Symbolic Manipulation}

In order to transform the implicit DAE describing the simple electrical
circuit into an explicit one, we perform the following steps. Firstly, we
identify \emph{known} and \emph{unknown} variables. Secondly, we decide which
unknown variable should be solved in which equation. Thirdly, we sort the
equations in such a way that no unknown variable is used before it is solved.

Time $t$ and the differential variables $u_c$ and $i_2$ are assumed to be
known, the rest of the variables are unknowns, including the derivatives
($\frac{du_c}{dt}$ and $\frac{di_2}{dt}$). The equations that contain only one
unknown are solved for it. After that, the solved variables are assumed to be
known and rest of the variables are solved. In this case this technique
suffices and we get the following explicit DAE:

\begin{subequations}
\begin{eqnarray}
u_S  & = & sin (2 \pi t) \\
u_L  & = & u_S \\
u_R  & = & u_S - u_C \\
i_1  & = & \frac{u_R}{R} \\
i    & = & i_1 + i_2 \\
\frac{du_C}{dt} & = & \frac{i_1}{C} \\
\frac{di_2}{dt} & = & \frac{u_L}{L}
\end{eqnarray}
\end{subequations}

This symbolic manipulation process is called \emph{causalisation}. Now the
cause-effect relationship is explicitly specified which was not the case for
the implicit DAE.

Let us substitute the variables defined in the first five equations into the
last two equations. This effectively eliminates the algebraic equations from
the system.

\begin{subequations}
\label{eqSimpleCircuitODE}
\begin{eqnarray}
\frac{du_C}{dt} & = & \frac{sin (2 \pi t) - u_C}{R \cdot C} \\
\frac{di_2}{dt} & = & \frac{sin (2 \pi t)}{L}
\end{eqnarray}
\end{subequations}

This representation is a system of explicit ODEs and can be passed to a
numerical ODE solver. This representation is also called \emph{state-space
model}. More generally, a system of explicit ODEs can be written in the
following form:
\begin{equation}
\label{eqExplODE}
\frac{d\vec{x}}{dt} = f(\vec{x},t)
\end{equation}
Here, $\vec{x}$ is a vector of differential variables and $t$ is time.

\subsection{Numerical Integration}

In the following the \emph{forward Euler} method, which is the simplest
numerical integration method for ODEs, is explained. The key idea is to
replace the derivatives with the following approximation:
\begin{equation}
\frac{d\vec{x}}{dt} \approx \frac{\vec{x}(t + h) - \vec{x}(t)}{h}
\end{equation}
Here, $h$ is a \emph{sufficiently small} positive scalar which is referred to
as the \emph{step size} of the numerical integration.

Let us make use of Equation \ref{eqExplODE} and substitute the derivative.

\begin{equation}
\vec{x}(t + h) \approx \vec{x}(t) + h \cdot f(\vec{x},t)
\end{equation}

Let us also fix the step size $h$ and construct the following discrete
sequences:
\begin{equation}
t_0  = 0, t_1 = t_0 + h, \ t_2 = t_1 + 2h, ...,  t_n = t_{n-1} + nh, ...
\end{equation}
\begin{equation}
\vec{x}_0 = \vec{x}(t_0), ..., \vec{x}_{n+1} = \vec{x}_n + h \cdot f(\vec{x}_n,t_n), \ ... \label{eqStateVectorDiscreteSeq}
\end{equation}
Here, $\vec{x}_n$ is a numerical approximation of $\vec{x}(t_n)$.

More accurate and efficient numerical integration methods are available based
on different approximations and integration algorithms. A comprehensive
presentation of this and other more sophisticated methods can be found in the
book by \citet{Cellier2006}.

\subsection{Simulation}

Once an initial condition (i.e., a value of the differential vector at time
zero) is given it is possible to numerically integrate the ODE. The Haskell
code that is given in Figure \ref{figIntegrateSimpleCircuit} numerically
integrates the ODE given in Equation \ref{eqSimpleCircuitODE} using the
forward Euler method.

\begin{figure}
\begin{code}
integrateSimpleCircuit  ::  Double -> Double -> Double -> Double
                        ->  [(Double, Double, Double)]
integrateSimpleCircuit dt r c l = go 0 0 0
  where
  go t uc i2 =  let  di2  =  (sin(2 * pi * t) / l) * dt
                     duc  =  ((sin(2 * pi * t) - uc) / (r * c)) * dt
                in   (t, i2, uc) : go (t + dt) (uc + duc) (i2 + di2)
\end{code}

\caption{\label{figIntegrateSimpleCircuit} Function that numerically
integrates the ODE given in Equation \ref{eqSimpleCircuitODE} using the
forward Euler method.}

\end{figure}

Given the numerical integration time step and the circuit parameters, this
function computes the approximate solution and delivers the values of the
differential vector at the discrete points of time given in Equation
\ref{eqStateVectorDiscreteSeq} as a list.

In the case of the simple electrical circuit model, the algebraic variables
can also be solved by adding so called \emph{output equations} in the function
that numerically integrates the system of equations. Output equations are
explicit algebraic equations where the algebraic variables are defined in
terms of the differential variables. The Haskell code given in Figure
\ref{figIntegrateSimpleCircuit2} refines the integration function by adding
the output equation that solves the algebraic variable $i_1$. Figure
\ref{figCircuitPlot1} shows a partial simulation result obtained by evaluating
the function with output equations.

\begin{figure}

\begin{code}
integrateSimpleCircuit  ::  Double -> Double -> Double -> Double
                        ->  [(Double, Double, Double, Double)]
integrateSimpleCircuit dt r c l = go 0 0 0
  where
  go t uc i2 =  let   di2  =  (sin(2 * pi * t) / l) * dt
                      duc  =  ((sin(2 * pi * t) - uc) / (r * c)) * dt
                      i1   =  (sin (2 * pi * t) - uc) / r
                in    (t, i2, uc, i1) : go (t + dt) (uc + duc) (i2 + di2)
\end{code}

\caption{\label{figIntegrateSimpleCircuit2} Function that adds two output
equations to the function given in Figure \ref{figIntegrateSimpleCircuit}.}

\end{figure}

\begin{figure}
\begin{center}
\includegraphics[width=\textwidth]{Graphics/circuitPlot1}
\end{center}

\caption{\label{figCircuitPlot1} Plot showing how variables $i_1$ and $i_2$
change over time.}

\end{figure}

The simple electrical-circuit example highlights the three essential steps
involved in the process of modelling and simulation of physical systems:

\begin{itemize}
\item Mathematical modelling of the system behaviour
\item Translation of the mathematical representation into a computer program
\item Simulation of the system by compiling and executing the computer program
\end{itemize}

As we have already seen, for some systems, it is feasible to conduct this
process manually. Indeed translation of systems of equations into code in
general purpose programming languages like Fortran, C, Java or Haskell is a
common practise. However, manual translation becomes tedious and error prone
with growing complexity. Imagine conducting the process presented in this
section for a physical system described with hundreds of thousands of
equations.

Modelling languages and simulation tools can help with all three phases
mentioned above. The following section overviews state-of-the-art
representatives of causal and noncausal modelling languages. In this thesis,
we focus on modelling languages capable of simulating mathematical models
without assuming a particular domain of physics.

\section{Causal Modelling in Simulink}
\label{secSimulink}

Simulink is a graphical block diagramming tool for causal modelling and
simulation. The block diagram depicted in Figure \ref{figCircuitBlockDiagram1}
is a model of the simple electrical circuit from Figure \ref{figCircuit1}.
Note that the diagram uses causal blocks (with input and outputs) for
multiplication, summation and integration.

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/circuitBlockDiagram1}
\end{center}

\caption{\label{figCircuitBlockDiagram1} Block diagram modelling electrical
circuit depicted in Figure \ref{figCircuit1}.}

\end{figure}

Block diagrams in causal languages correspond to systems of ODEs in explicit
form. That is, the causality is explicit: the input and differential variables
are used to define the derivatives and the output variables. The construction
of a block diagram is closely related to the process of causalisation.
Derivation of simulation code from a block diagram is done much in the same
way as described in Section \ref{secModelling}, but using more sophisticated
numerical methods.

Structurally, the block diagram in Figure \ref{figCircuitBlockDiagram1} is
quite far removed from the circuit it models. Because of this, construction of
block diagrams is generally regarded as a difficult and somewhat involved task
\citep{Nilsson2007}. Moreover, a slight change in a modelled system might
require drastic changes in the corresponding block diagram. This is because
causal models limit reuse \citep{Cellier1996}. For example, a resistor
behaviour is usually modelled using Ohm's law which can be written as $i =
\frac{u}{R}$ or $u = R \cdot i$. Unfortunately, no single causal block can
capture the resistor behaviour. If we need to compute the current from the
voltage, we should use the block that corresponds to the first equation. If we
need to compute the voltage from the current, we should use the block that
corresponds to the second equation.

To demonstrate the aforementioned reuse problem, we modify the simple
electrical circuit by adding one more resistor, as shown in Figure
\ref{figCircuit2}, and then causally model it as shown in Figure
\ref{figCircuitBlockDiagram2}. Note that we were unable to reuse the resistor
model from the original circuit diagram. Furthermore, a simple addition to the
physical system caused hardly obvious changes in the causal model.

\begin{figure}
\begin{center}
\includegraphics[width = 0.5\textwidth]{Graphics/circuit2}
\end{center}

\caption{\label{figCircuit2} Simple electrical circuit with two resistors.}

\end{figure}

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/circuitBlockDiagram2}
\end{center}

\caption{\label{figCircuitBlockDiagram2} Block diagram modelling electrical
circuit depicted in Figure \ref{figCircuit2}.}

\end{figure}

Simulink can be used to model some structurally dynamic systems: special
blocks are used to \emph{switch} between block diagrams as a response to
discrete events. This makes Simulink very useful for modelling of structurally
dynamic systems. However, the number of modes must be finite and all modes
must be predetermined before simulation. Thus Simulink does not enable
modelling and simulation of unbounded structurally dynamic systems. In
addition, Simulink block diagrams are first order, thus Simulink does not
support higher-order causal modelling.


\section{Noncausal Modelling in Modelica}
\label{secModelica}

%{
%include ../Format/modelica.lhs

Modelica is a declarative language for noncausal modelling and simulation of
physical systems. Modelica models are given using implicit DAEs. Modelica
features a class system known from object-oriented programming languages for
structuring equations and for supporting model reuse.

This section presents a Modelica model of the simple electrical circuit
depicted in Figure \ref{figCircuit1} to illustrate basic features of the
language.

The Modelica code that is given in Figure \ref{figModelicaConnector} declares
the \emph{connector} record for representing electrical connectors. The
connector record introduces the variable |i| and the variable |v| representing
the current flowing into the connector and the voltage at the connector
respectively. In Modelica, connector records do not introduce equations. The
meaning of the flow annotation is explained later on when \emph{connect
equations} are introduced.

\begin{figure}

\begin{code}
connector Pin
  flow Real i;
  Real v;
end Pin;
\end{code}

\caption{\label{figModelicaConnector} Connector record defined in Modelica.}

\end{figure}

The Modelica code that is given in Figure \ref{figModelicaTwoPin} defines the
model that captures common properties of electrical components with two
connectors. The variables |p| and |n| represent the positive and negative pins
of an electrical component. The variable |u| represents the voltage drop
across the component. The variable |i| represents the current flowing into the
positive pin. The |TwoPin| model defines the noncausal equations that these
variables satisfy.


\begin{figure}

\begin{code}
model TwoPin
  Pin   p,  n;
  Real  u,  i;
equation
  u  =  p.v  -  n.v;
  0  =  p.i  +  n.i;
  i  =  p.i;
end TwoPin;
\end{code}

\caption{\label{figModelicaTwoPin} Modelica model for two-pin electrical
components.}

\end{figure}

By \emph{extending} the |TwoPin| model with component-specific equations
Figure \ref{figModelicaComponents} defines the models representing resistor,
capacitor, inductor and voltage source. Figure \ref{figModelicaComponents}
also defines the model that represents the ground pin. Note the use of the
concept of \emph{inheritance} known from object-oriented programming languages
for reusing the equations from the |TwoPin| model.

Variables qualified as |parameter| or as |constant| remain unchanged during
simulation. The value of a constant is defined once and for all in the source
code, while a parameter can be set when an object of the class is
instantiated. In this example all parameters are provided with default values
allowing for instantiations with the default parameter values. All other
variables represent dynamic, time-varying entities.

\begin{figure}

\begin{code}
model Resistor
  extends TwoPin;
  parameter Real R = 1;
equation
  R * i = u;
end Resistor;
\end{code}

\begin{code}
model Capacitor
  extends TwoPin;
  parameter Real C = 1;
equation
  C * der(u) = i;
end Capacitor;
\end{code}

\begin{code}
model Inductor
  extends TwoPin;
  parameter Real L = 1;
equation
  u = L * der(i);
end Inductor;
\end{code}

\begin{code}
model VSourceAC
  extends TwoPin;
  parameter Real VA      =  1;
  parameter Real FreqHz  =  1;
  constant  Real PI      =  3.14159;
equation
  u = VA * sin(2 * PI * FreqHz * time);
end VSourceAC;
\end{code}

\begin{code}
model Ground
  Pin p;
equation
  p.v = 0;
end Ground;
\end{code}

\caption{\label{figModelicaComponents} Modelica models with component-specific
equations.}

\end{figure}

The Modelica model that is given in Figure \ref{figModelicaCircuit1} uses the
circuit component models to define the simple electrical circuit model by
``connecting'' appropriate pins according to Figure \ref{figCircuit1}.

\begin{figure}

\begin{code}
model SimpleCircuit
  Resistor   R;
  Capacitor  C;
  Inductor   L;
  VSourceAC  AC;
  Ground     G;
equation
  connect(AC.p,  R.p);
  connect(AC.p,  L.p);
  connect(R.n,   C.p);
  connect(AC.n,  C.n);
  connect(AC.n,  L.n);
  connect(AC.n,  G.p);
end SimpleCircuit;
\end{code}

\caption{\label{figModelicaCircuit1} Modelica model for the circuit given in
Figure \ref{figCircuit1}.}

\end{figure}

Connect statements are analysed and appropriate \emph{connection equations}
are generated by the Modelica compiler as follows. Connected flow variables
generate sum-to-zero equations. In this case the sum-to-zero equations
correspond to Kirchhoff's current law. For the |SimpleCircuit| model the
Modelica compiler generates the following sum-to-zero equations:

\begin{code}
AC.n.i + C.n.i + L.n.i + G.p.i = 0;
R.n.i + C.p.i = 0;
AC.p.i + R.p.i + L.p.i = 0;
\end{code}

Connected potential variables generate equality constraints stating that all
connected potential variables are equal at any point in time. For the
|SimpleCircuit| model the Modelica compiler generates the following equations:

\begin{code}
AC.n.v  =  C.n.v;
C.n.v   =  L.n.v;
L.n.v   =  G.p.v;

R.n.v   =  C.p.v;
AC.p.v  =  R.p.v;
R.p.v   =  L.p.v;
\end{code}

Connect-equations can be used in any physical domain where flow variables
(i.e., variables generating sum-to-zero equations at the connection points)
and potential variables (i.e, variables generating equality constrains at the
connection points) can be identified. The Modelica standard library includes
examples of their usage in electrical, hydraulic, and mechanical domains.

Modelica compilers generate executable simulation code from hierarchical
systems of equations structured using object-oriented programming constructs
by utilising state-of-the-art symbolic and numerical methods.

As we have seen, noncausal languages allow us to model physical systems at a
high level of abstraction. The structure of the models resemble the modelled
systems. Consequently, it is easy to reuse or modify existing models. For
example, it is now trivial to add one more resistor to the Modelica model as
shown in Figure \ref{figModelicaCircuit2}.

\begin{figure}

\begin{code}
model SimpleCircuit
  Resistor    R1;
  Resistor    R2;
  Capacitor   C;
  Inductor    L;
  VSourceAC   AC;
  Ground      G;
equation
  connect(AC.p,  R1.p);
  connect(AC.p,  R2.p);
  connect(R1.n,  C.p);
  connect(R2.n,  L.p);
  connect(AC.n,  C.n);
  connect(AC.n,  L.n);
  connect(AC.n,  G.p);
end SimpleCircuit;
\end{code}

\caption{\label{figModelicaCircuit2} Modelica model for the circuit given in
Figure \ref{figCircuit2}.}

\end{figure}

\section{Noncausal Modelling of Structurally Dynamic Systems}
\label{secHybridModelling}

A structurally dynamic system is usually modelled using a combination of
continuous equations and switching statements that specify discontinuous
changes in the system. This section is about structurally dynamic modelling in
noncausal languages. Current limitations are illustrated using a Modelica
model of a simple structurally dynamic system. In particular, this section
highlights the lack of expressiveness of the Modelica language when it comes
to dynamic addition and removal of time-varying variables and continuous
equations, and lack of runtime symbolic processing and code generation
facilities in Modelica implementations.

Let us model a physical system whose structural configuration changes abruptly
during simulation: a simple pendulum that can break at a specified point in
time; see Figure \ref{figPendulum}. The pendulum is modelled as a body
represented by a point mass $m$ at the end of a rigid, mass-less rod, subject
to gravity $m \vec{g}$. If the rod breaks, the body will fall freely.

\begin{figure}
\begin{center}
\includegraphics[width = 0.5\textwidth]{Graphics/pendulum}
\end{center}
\caption{\label{figPendulum} Pendulum subject to gravity.}
\end{figure}

The code that is given in Figure \ref{figModelicaBreakingPendulum} is an
attempt to model this system in Modelica that on the surface appears to solve
the problem. Unfortunately the code fails to compile. The reason is that the
latest version of the Modelica standard \citep[2010]{Modelica} asserts that
number of equations in both branches of an if statement must be equal when the
conditional expression contains a time-varying variable. If considered
separately, the equations in both branches do solve the publicly available
variables successfully. In an attempt to fix the model, the modeller might try
to add a dummy equation for the variable not needed in the second mode (i.e.,
the variable |phi|, which represents the angle of deviation of the pendulum
before it is broken). This version compiles, but the generated code fails to
simulate the system. This example was tried using the OpenModelica
\citep{OpenModelica} and Dymola \citep{Dymola} compilers.

\begin{figure}

\begin{code}
model BreakingPendulum
  parameter Real l = 1, phi_0 = pi / 4, t = 10;
  Real x, y, v_x, v_y;
  protected Real phi(start = pi / 2);
equation
   v_x  =  der x
   v_y  =  der y
  if (time < t) then
    x   =     l  *  sin phi
    y   =  -  l  *  cos phi
    0   =  der (der phi) + (g / l) * sin phi
  else
    der v_x  =  0
    der v_y  =  -g
  end if;
end BreakingPendulum;
\end{code}

\caption{\label{figModelicaBreakingPendulum} Attempt to model a breaking
pendulum in Modelica.}

\end{figure}

One of the difficulties of this example is that causality changes during the
switch between the two modes. In the first mode the position is calculated
from the differential variable |phi|, which is not the case after the switch.
This makes the job of the simulation code generator a lot harder and, as it
turns out, the Modelica tools are not able to handle it. Specifically, the
tools commit to a certain causality before they generate the simulation code.
This and related issues are covered in greater detail in
\citep[2000]{ModelicaTutorial}. The suggested Modelica solution is more
involved and requires reformulation of the model by making it causal. The need
for manual reformulation to conform to a certain causality eliminates the
advantages of working in a noncausal modelling language.

Currently, the Modelica language lacks the expressiveness to describe
structural changes. The breaking pendulum example demonstrates the problems
that arise when there is a need to change the number of variables in the
system. In addition, the Modelica compilers carry out the symbolic processing
and generate the simulation code all at once, prior to simulation, which
introduces further limitations.

%}
