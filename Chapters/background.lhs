\chapter{Background}
\label{chapBackground}

\section{Modelling and Simulation of Physical Systems}
\label{secModelling}

In this section we overview the field of modelling and simulation of physical
systems by using a simple but illustrative example. We model and simulate the
physical system and through this process introduce basic concepts of modelling
and simulation. When necessary, we abstract from the concrete example and
define the concepts generally.

\subsection{Mathematical Modelling}

Let us introduce a simple electrical circuit that is depicted in Figure
\ref{figCircuit1}.

\begin{figure}
\begin{center}
\includegraphics[width = 0.5\textwidth]{Graphics/circuit1}
\end{center}
\caption{\label{figCircuit1} Simple electrical circuit.}
\end{figure}

The following system of equations is a mathematical model of the circuit.

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

The first four equations describe the behaviour of the components. The last
three equations describe the topology of the circuit. The system of equations
consists of implicitly defined algebraic and differential equations. This
mathematical representation is called system of implicit \emph{differential
algebraic equations (DAEs)} \citep{Cellier2006}. More generally, system of
implicit DAEs can be written in the following way:

\begin{equation}
f(\frac{d\vec{x}}{dt},\vec{x},\vec{y},t) = 0
\end{equation}

where $\vec{x}$ is a vector of \emph{differential variables}, also known as
\emph{state variables}, $\vec{y}$ is a vector of algebraic variables and $t$
is an independent scalar variable, in physical modelling it represents the
\emph{time}.

In general, it is not possible to find an exact solution of a DAE using
analytical methods. Approximate solutions are derived by \emph{numerical
integration}. Their are a number of methods for numerical integration of an
implicit DAE. They can be divided into two categories. Solvers in the first
category operate directly on the implicit DAE. Solvers in the second category
transform the implicit representation into the explicit one and operate on the
explicit representation. In this section we illustrate the later.


\subsection{Symbolic Manipulation}

In order to transform this implicit DAE into an explicit one, we perform the
following steps. Firstly, we identify \emph{known} and \emph{unknown}
variables. Secondly, we decide which unknown variable should be solved in
which equation. Thirdly, we sort equations in a way that no unknown variable
is used before it is solved.

Time $t$ and state variables ($u_c$ and $i_2$), are assumed to be known, the
rest of the variables are unknowns including the state derivatives
($\frac{du_c}{dt}$ and $\frac{di_2}{dt}$). Equations that contain only one
unknown are solved for it. After that, solved variables are assumed to be
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

This symbolic manipulation process is called \emph{causalisation}. Note that
now the cause-effect relationship is explicitly specified which was not the
case for the implicit DAE.

Let us substitute the variables defined in the first five equations in the
last two equations. This effectively eliminates the algebraic equations from
the system.

\begin{subequations}
\label{eqSimpleCircuitODE}
\begin{eqnarray}
\frac{du_C}{dt} & = & \frac{sin (2 \pi t) - u_C}{R \cdot C} \\
\frac{di_2}{dt} & = & \frac{sin (2 \pi t)}{L}
\end{eqnarray}
\end{subequations}

This representation is called system of explicit \emph{ordinary differential
equations (ODEs)} and can be passed to a numerical ODE solver. This
representation is also called \emph{state-space model}. More generally, a
system of explicit ODEs can be written in the following way:

\begin{equation}
\label{eqExplODE}
\frac{d\vec{x}}{dt} = f(\vec{x},t)
\end{equation}
where $\vec{x}$ is a vector of differential variables and $t$ is the
\emph{time}.

\subsection{Numerical Integration}

In the following we explain the simplest numerical integration method for
ODEs, i.e. the \emph{forward Euler} method. The key idea is to replace the
derivatives with the following approximation:

\begin{equation}
\frac{d\vec{x}}{dt} \approx \frac{\vec{x}(t + h) - \vec{x}(t)}{h}
\end{equation}

where $h$ is a \emph{sufficiently small} positive scalar and is called a
\emph{step size} of the numerical integration \citep{Cellier2006}.

We make us of Equation \ref{eqExplODE} and substitute the derivative .

\begin{equation}
\vec{x}(t + h) \approx \vec{x}(t) + h \cdot f(\vec{x},t)
\end{equation}

We fix the step size $h$ and construct the following discrete sequences:

\begin{equation}
t_0  = 0, t_1 = t_0 + h, \ t_2 = t_1 + 2h, ...,  t_n = t_{n-1} + nh, ...
\end{equation}
\begin{equation}
\vec{x}_0 = \vec{x}(t_0), ..., \vec{x}_{n+1} = \vec{x}_n + h \cdot f(\vec{x}_n,t_n), \ ... \label{eqStateVectorDiscreteSeq}
\end{equation}

where $\vec{x}_n$ is a numerical approximation of $\vec{x}(t_n)$.

More accurate and efficient numerical integration methods are available based
on different approximations and integration algorithms. More comprehensive
presentation of this and other more sophisticated methods can be found in
\citep{Cellier2006}.

\subsection{Simulation}

Once an initial condition (i.e., a value of the state vector at time zero) is
given it is possible to numerically integrate the ODE. In the following we
give a Haskell program which performs numerical integration of the ODE, given
in Equation \ref{eqSimpleCircuitODE}, using the forward Euler method.

\begin{code}
integrateSimpleCircuit :: (Floating a) => a -> a -> a -> a -> [(a, a, a)]
integrateSimpleCircuit dt r c l = go 0 0 0
  where  go t uc i2 = (t, i2, uc) : go (t + dt) (uc + duc) (i2 + di2) 
           where  di2  =  (sin(2 * pi * t) / l) * dt
                  duc  =  ((sin(2 * pi * t) - uc) / (r * c)) * dt
\end{code}

Given a time step for the integration and the circuit parameters, the function
computes the solution by numerical integration and delivers the values of the
state vector in the discrete points of time (see Equation
\ref{eqStateVectorDiscreteSeq}) as a list that can be plotted latter.

The algebraic variables can also be solved by adding so called \emph{output
equations} in the function which performs numerical integration. Output
equations are explicit algebraic equations where the algebraic variables are
defined in terms of the state variables. We refine the integration function by
adding output equation which solves the algebraic variable $i_1$.

\begin{code}
integrateSimpleCircuit :: (Floating a) => a -> a -> a -> a -> [(a, a, a, a)]
integrateSimpleCircuit dt r c l = go 0 0 0
  where  go t uc i2 = (t, i2, uc, i1) : go (t + dt) (uc + duc) (i2 + di2) 
           where  di2  =  (sin(2 * pi * t) / l) * dt
                  duc  =  ((sin(2 * pi * t) - uc) / (r * c)) * dt
                  i1   =  (sin (2 * pi * t) - uc) / r
\end{code}

Figure \ref{figCircuitPlot1} shows the simulation result obtained by
evaluating $integrateSimpleCircuit$ function.

\begin{figure}
\begin{center}
\includegraphics[width=\textwidth]{Graphics/circuitPlot1}
\end{center}
\caption{\label{figCircuitPlot1} Plot showing how variables $i_1$ and $i_2$ change over time.}
\end{figure}

\subsection{Modelling and Simulation Process Summarised}

This example highlights the three main steps involved in the process of
modelling and simulation of physical systems:

\begin{itemize}
\item Modelling the behaviour of the system mathematically
\item Translation the mathematical representation into a computer program
\item Simulation the system by compiling and executing the computer program
\end{itemize}

As we have already seen, for some systems, it is feasible to conduct this
process manually. Indeed translation of systems of equations into code in
general purpose programming languages like C, Fortran, Java or Haskell is
still a common practise. However, manual translation becomes tedious and error
prone with growing complexity. Imagine conducting the process presented in
this section for a physical system described with hundreds of thousands of
equations. Modelling languages and simulation tools can help with all three
phases mentioned above. In the following sections we introduce state of the
art representatives of causal and non-causal modelling languages. In this
thesis we focus on modelling languages capable of simulating mathematical
models without assuming a particular domain of physics.

\section{Causal Modelling in Simulink}
\label{secSimulink}

Simulink \citep{Simulink2008} is a graphical block diagramming tool for causal
modelling and simulation. The block diagram in Figure
\ref{figCircuitBlockDiagram1} is a model of the circuit from Figure
\ref{figCircuit1}.

\begin{figure}
\begin{center}
\includegraphics[width = \textwidth]{Graphics/circuitBlockDiagram1}
\end{center}
\caption{\label{figCircuitBlockDiagram1} Block diagram modelling electrical circuit depicted in Figure \ref{figCircuit1}.}
\end{figure}

Block-diagrams in causal languages correspond to systems of ordinary
differential equations (ODEs) in explicit form; i.e., the causality is
explicit, the input and state variables are used to define the state
derivatives and the output variables. The construction of a block-diagram is
closely related to the process of causalisation (see Section
\ref{secModelling}).

Derivation of a simulation code from a block diagram is straight forward and
is done much in the same way as in Section \ref{secModelling}, but using more
sophisticated numerical methods. This is done automatically by the Simulink
system.

Structurally, the block diagram in Figure \ref{figCircuitBlockDiagram1}
is quite far removed from the circuit it models. Because of this, construction
of block diagrams is generally regarded as a difficult and somewhat involved
task \citep{Nilsson2007}. Moreover, a slight change in a system might require
drastic changes in the corresponding block diagram. This is because causal
models limit reuse \citep{Cellier1996}. For example, a resistor behaviour is
usually modelled using the Ohm's law which can be written as $i = \frac{u}{R}$
or $u = R \cdot i$. Unfortunately, no single causal block can capture the
resistor behaviour. If we need to compute the current from the voltage, we
should use the block that corresponds to the first equation. If we need to
compute the voltage from the current, we should use the block that corresponds
to the second equation.

We demonstrate the aforementioned problem, by causally modelling a modified
circuit where one more resistor is added to the simple electrical circuit as
show in Figure \ref{figCircuit2}. The causal model is given in
Figure \ref{figCircuitBlockDiagram2}.

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
\caption{\label{figCircuitBlockDiagram2} Block diagram modelling electrical circuit depicted in Figure \ref{figCircuit2}.}
\end{figure}


Simulink can be used to model some hybrid systems, special blocks are used two
\emph{switch} between block diagrams as a response to discrete events. This
makes Simulink very useful indeed for hybrid simulation of structurally
dynamic systems. However, the number of configurations or modes must be finite
and all modes must be predetermined before the simulation. Thus Simulink does
not enable modelling and simulation of highly structurally dynamic systems. In
addition, Simulink block diagrams are first-order thus Simulink does not
support higher-order causal modelling.


\section{Non-causal Modelling in Modelica}
\label{secModelica}

%{
%include ../Format/modelica.lhs

Modelica is a declarative language for non-causal modelling and simulation of
physical systems \citep{Modelica2007}. Modelica models are given using
non-causal DAEs. A class system known from object-oriented programming
paradigm is used to structure the equations and support reuse of models.

To illustrate basic features of Modelica we model the circuit that is given in
Figure \ref{figCircuit1} again, this time in Modelica.

Firstly, we define a \emph{connector} record representing an electrical
connector.

\begin{code}
connector Pin
  flow Real i;
  Real v;
end Pin;
\end{code}

It has two variables, |i| and |v|, representing the current and the voltage
respectively. A connector record does not introduce any equations for
variables. The meaning of flow and non-flow (also called potential) variables
are explained later when \emph{connect equations} are introduced.

Secondly, we define a model which captures common properties of electrical
components with two connectors.

\begin{code}
model TwoPin
  Pin  p, n;
  Real u, i;
equation
  u = p.v - n.v;
  0 = p.i + n.i;
  i = p.i;
end TwoPin;
\end{code}

The |TwoPin| model not only defines the variables, but also the non-causal
equations they satisfy. The variables |p| and |n| represent the positive and
negative pins of an electrical component. The variable |u| represents the
voltage drop across the component. The variable |i| represents the current
flowing into the positive pin.

By extending the |TwoPin| model and adding component-specific equations we
define the model representing resistor, capacitor, inductor and voltage source
models. Note the use of \emph{inheritance} to reuse the equations from the
|TwoPin| model.

\begin{code}
model Resistor
  extends TwoPin;
  parameter Real R = 1;
equation
  R * i = u;
end Resistor;

model Capacitor
  extends TwoPin;
  parameter Real C = 1;
equation
  C * der(u) = i;
end Capacitor;

model Inductor
  extends TwoPin;
  parameter Real L = 1;
equation
  u = L * der(i);
end Inductor;

model VSourceAC
  extends TwoPin;
  parameter Real VA      =  1;
  parameter Real FreqHz  =  1;
  constant  Real PI      =  3.14159;
equation
  u = VA * sin(2 * PI * FreqHz * time);
end VSourceAC;
\end{code}

Variables qualified as parameter or as constant remain unchanged during the
simulation. All other variables represent dynamic, time-varying entities. The
difference between constant and parameter is that the value of a constant is
defined once and for all in the source code, while a parameter can be set when
an object of the class is instantiated. In this example all parameters are
provided with default values.

In addition, we define a model representing a ground pin.

\begin{code}
model Ground
  Pin p;
equation
  p.v = 0;
end Ground;
\end{code}

We can now use models of the circuit components to define a model representing
the circuit in Figure \ref{figCircuit1} by ``connecting'' appropriate pins
according to the figure. Note that the parameters of resistor, capacitor,
inductor and voltage source are not specified. This means that the default
values are used.

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

Connect statements are analysed and appropriate \emph{connection equations}
are generated by the Modelica compiler as follows. Connected flow variables
generate sum to zero equations. In the case of an electrical circuit it
corresponds to Kirchhoff's current law. For the |SimpleCircuit| model a
Modelica compiler generates the following equations:

\begin{code}
AC.n.i + C.n.i + L.n.i + G.p.i = 0;
R.n.i + C.p.i = 0;
AC.p.i + R.p.i + L.p.i = 0;
\end{code}

Connected potential variables generate equality constraints stating that all
connected potential variables are equal at any point in time. For
|SimpleCircuit| model a Modelica compiler generates the following equations:

\begin{code}
AC.n.v  =  C.n.v;
C.n.v   =  L.n.v;
L.n.v   =  G.p.v;

R.n.v   =  C.p.v;
AC.p.v  =  R.p.v;
R.p.v   =  L.p.v;
\end{code}

Connect-equations can be used in any physical domain where flow and potential
variables can be identified. The Modelica standard
library\footnote{\texttt{http://www.modelica.org/libraries}} includes examples
of their usage in electrical, hydraulic, and mechanical domains, for example.
Modelica compilers generate executable simulation code from hierarchical
systems of equations structured using object-oriented programming constructs
by utilising state of the art symbolic and numerical integration methods.

As we have seen, non-causal languages, and in this particular case Modelica,
allow us to model physical systems at a high level of abstraction. The
structure of the models resemble the modelled systems. Consequently, it is
easy to reuse or modify existing models. For example, it is now trivial to add
one more resistor to the Modelica model as shown in the following modified
model.

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

\section{Non-causal Modelling of Structurally-dynamic Systems}
\label{secHybridModelling}

A physical system can be hybrid. That is, it may exhibit both continuous-time
and discrete-time behaviour. A hybrid system is usually modelled using a
combination of continuous equations and switching statements that specify
discontinuous changes in the system.

The simulation of continuous systems is relatively well understood
\citep{Cellier2006}. However, hybrid systems introduce a number of unique
challenges \citep{Barton2002a,Mosterman1999a} (e.g., handling a large or
possibly unbounded number of continuous modes, accurate event detection, and
consistent initialisation of state variables during mode switches). The
integration of hybrid modelling with non-causal modelling raises further
problems (e.g., dynamic causalisation and simulation code generation during
switches).

Current non-causal modelling languages and related tools are very limited in
their ability to model and simulate hybrid systems. Many of the limitations
are related to the symbolic and numerical methods that must be used in the
non-causal approach. But more fundamental reason is that most such systems
perform all symbolic manipulations and the simulation code generation before
simulation begins \citep{Mosterman1999a}.

In this section, we discuss hybrid modelling in non-causal languages. The
current limitations are illustrated using the Modelica model of a hybrid
system. In particular, we highlight lack of expressiveness of the Modelica
language when it comes to dynamic addition and removal of time varying
variables and continuous equations to the model, and lack of dynamic
recausalisation and code generation facilities in Modelica implementations.

\subsection{Modelling Structurally-dynamic Systems in Modelica}

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

Here is an attempt to model this system in Modelica that on the surface
appears to solve the problem:

\begin{code}
model BreakingPendulum
  parameter Real l = 1, phi_0 = pi / 4, t = 10;
  Real x, y, v_x, v_y;
  protected Real phi(start = pi / 2);
equation 
   v_x  =  der x
   v_y  =  der y
  if (time > t) then
    x   =     l  *  sin phi
    y   =  -  l  *  cos phi
    der (der phi) + (g / l) * sin phi = 0
  else
    der v_x  =  0
    der v_y  =  -g
  end if;
end BreakingPendulum;
\end{code}

However the model fails to compile. The latest version of the Modelica
standard \citep{Modelica2007} asserts that number of equations in both branches
of an if statement must be equal when the conditional expression contains a
time-varying variable. If considered separately, the equations in both
branches do solve the publicly available variables successfully. In an attempt
to fix the model, the modeller might try to add a dummy equation for the
variable not needed in the second mode (i.e., the variable |phi|, which
represents the angle of deviation of the pendulum before it is broken). This
version compiles, but the generated code fails to simulate the system. This
example was tried using OpenModelica \citep{OpenModelica2006} and Dymola
\citep{Dymola2008} compilers.

One of the difficulties of this example is that causality changes during the
switch between the two modes. In the first mode position is calculated from
state variable |phi|, which is not the case after the switch. This makes the
job of the simulation code generator a lot harder and as it turns out Modelica
tools are not able to handle it. This and related issues are covered in
greater detail in \citep{ModelicaTutorial2000}. The suggested solution is more
involved and requires reformulation of the model by making it causal. The need
of manual reformulation to conform to certain causality eliminates the
advantages of working in non-causal a modelling language.

Currently, the Modelica language lacks expressiveness to describe structural
changes. The breaking pendulum example demonstrated the problems that arise
when there is a need for change of the number of variables in the system.
Secondly, the state-of-the-art Modelica compilers carry out the symbolic
processing and generate the simulation code all at once, prior to simulation.
In the presence of structurally dynamic systems this is not impossible in
general, as the number of modes may be very large, unbounded, or impossible to
determined in advance.

%}
