\chapter{Background}
\label{chapBackground}

\section{Functional Programming in Haskell}

In this section, in order to make the thesis self-contained, we give an
introduction to functional programming in Haskell. The reader familiar with
Haskell may skip this section.

\subsection{Pattern Matching and Recursion}
\subsection{Higher-Order Functions}
\subsection{Algebraic Data Types}
\subsection{Polymorphism and Overloading}


%% \section{Modelling and Simulation Languages}

%% \subsection{Modelling and Simulation of Physical Systems}

%% \subsection{Causal Modelling Languages}
%% \subsection{Simulink}
%% \subsection{Functional Reactive Programming}

%% \subsection{Non-Causal Modelling}
%% \subsection{Modelica}
%% \subsection{Functional Hybrid Modelling}


\section{Modelling and Simulation of Physical Systems}
\label{secModelling}

In this section we overview the field of modelling and simulation of physical
systems by using a simple but illustrative example. We model and simulate the
physical system and through this process introduce basic concepts of modelling
and simulation. When necessary, we abstract from the concrete example and
define the concepts generally.

\subsection{Mathematical Modelling}

Let us introduce a simple electrical circuit, depicted in Figure
\ref{figSimpleCircuit}.

\begin{figure}[h]
\begin{center}
\includegraphics[width = 0.5\textwidth]{Graphics/simpleCircuit}
\end{center}
\caption{A simple electrical circuit}
\label{figSimpleCircuit}
\end{figure}

The following system of equations is a mathematical model of the circuit
\cite{Cellier2006}.

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
  algebraic equations (DAEs)} \cite{Cellier2006}. More generally, system of
implicit DAEs can be written in the following way:

\begin{equation}
f(\frac{d\vec{x}}{dt},\vec{x},\vec{y},t) = 0
\end{equation}

where $\vec{x}$ is a vector of \emph{differential variables}, also known as
\emph{state variables}, $\vec{y}$ is a vector of algebraic variables and $t$
is an independent scalar variable, in physical modelling it represents the
\emph{time}.

In general it is not possible to find an exact solution of a DAE using
analytical methods. Approximate solutions are derived by \emph{numerical
  integration}. Their are number of methods for numerical integration of an
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
rest of the variables are unknowns including state derivatives
($\frac{du_c}{dt}$ and $\frac{di_2}{dt}$). Equations that contain only one
unknown are solved for it. After that, solved variables are assumed to be
known and rest of the variables are solved. In this case these techniques
suffice and we get the following explicit DAE:

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
last two equations. This effectively eliminates algebraic equations from the
system.

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
where $\vec{x}$ is a vector of state variables and $t$ is the
\emph{time}.

\subsection{Numerical Integration}

In the following we explain the simplest numerical integration method
for ODEs, i.e. the \emph{forward Euler} method. The key idea is to replace the
derivatives with the following approximation:
\begin{equation}
\frac{d\vec{x}}{dt} \approx \frac{\vec{x}(t + h) - \vec{x}(t)}{h}
\end{equation}
where $h$ is a \emph{sufficiently small} positive scalar and is called a
\emph{step size} of the numerical integration \cite{Cellier2006}.

We substitute the derivative from Equation \ref{eqExplODE}.

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
\cite{Cellier2006}.

\subsection{Simulation}

Once an initial condition, a value of state vector at time zero, is given it
is possible to numerically integrate the ODE. In the following we give a
Haskell program which performs numerical integration of the ODE, given in
Equation \ref{eqSimpleCircuitODE}, using the forward Euler method.

\begin{code}
integrateSimpleCircuit :: (Floating a) =>
                          a -> a -> a -> a -> [(a, a, a)]
integrateSimpleCircuit dt r c l = go 0 0 0
  where  go t uc i2 = (t, i2, uc) : go (t + dt) (uc + duc) (i2 + di2) 
           where  di2  =  (sin(2 * pi * t) / l) * dt
                  duc  =  ((sin(2 * pi * t) - uc) / (r * c)) * dt
\end{code}

Given a time step for the integration and the circuit parameters, the function
computes the solution by numerical integration and delivers the values of
state vector in the discrete points of time (see Equation
\ref{eqStateVectorDiscreteSeq}) as a list that can be plotted latter.

Algebraic variables can also be solved by adding so called \emph{output
  equations} in the function which performs numerical integration. Output
equations are explicit algebraic equations where algebraic variables are
defined using only state variables. We refine the integration function by
adding output equation which solves algebraic variable $i_1$.

\begin{code}
integrateSimpleCircuit :: (Floating a) =>
                          a -> a -> a -> a -> [(a, a, a, a)]
integrateSimpleCircuit dt r c l = go 0 0 0
  where  go t uc i2 = (t, i2, uc, i1) : go (t + dt) (uc + duc) (i2 + di2) 
           where  di2  =  (sin(2 * pi * t) / l) * dt
                  duc  =  ((sin(2 * pi * t) - uc) / (r * c)) * dt
                  i1   =  (sin (2 * pi * t) - uc) / r
\end{code}

Figure \ref{figSimpleCircuitPlot} shows part of the simulation result obtained
by executing $integrateSimpleCircuit$ function.

\begin{figure}[h]
\includegraphics[width=\textwidth]{Graphics/simpleCircuitPlot}
\caption{The plot shows how variables $i_1$ and $i_2$ change over time}
\label{figSimpleCircuitPlot}
\end{figure}

\subsection{Modelling and Simulation Process Summarized}

This example highlights and identifies the three main steps involved in the
process of modelling and simulation of physical systems.

\begin{itemize}
\item Model the behaviour of the system mathematically
\item Translate the mathematical representation into a computer program
\item Simulate the system by compiling and executing the program
\end{itemize}

As we have already seen, for some systems, it is feasible to conduct this
process manually. Indeed translation of systems of equations into code in a
general purpose programming languages like C, Fortran, Java or Haskell is
still common practise. However, manual translation becomes very tedious and
error prone with growing complexity. Imagine conducting the process presented
in this section for physical systems described with hundreds of
equations. Modelling languages and related simulation tools can help with all
three phases mentioned above. In the following sections we introduce state of
the art representatives of causal and non-causal modelling languages.

\section{Simulink}
\label{secSimulink}

Simulink \cite{Simulink2008} is a graphical block diagramming tool for causal modelling and simulation. The block diagram in Figure \ref{figSimpleCircuitBlockDiagram} is a model of the circuit from Figure \ref{figSimpleCircuit}.

\begin{figure}[h]
\begin{center}
\includegraphics[width = 0.75\textwidth]{Graphics/simpleCircuitBlockDiagram}
\end{center}
\caption{Block diagram modelling electrical circuit in Figure \ref{figSimpleCircuit}}
\label{figSimpleCircuitBlockDiagram}
\end{figure}

Block-diagrams in causal languages correspond to systems of ordinary
differential equations (ODEs) in explicit form; i.e., causality is explicit,
input and state variables are used to define state derivatives and output
variables. The construction of a block-diagram is closely related to the
process of causalisation (see Section \ref{secModelling}).

Derivation of a simulation code from a block diagram is straight forward and
is done much in the same way as in Section \ref{secModelling}, but using more
sophisticated numerical methods. This is done automatically by the Simulink
system.

% TODO Actually one could demonstrate that slight change in the circuit will lead to drastic changes in the simulink model.

Structurally, the block diagram in Figure \ref{figSimpleCircuitBlockDiagram}
is quite far removed from the circuit it models. Because of this, construction
of block diagrams is generally regarded as a difficult task
\cite{Nilsson2007}. Moreover, a slight change in a system might require
drastic changes in the corresponding block diagram. This is because causal
models limit reuse \cite{Cellier1996}. For example, let us consider a resistor
model. The behaviour of a resistor is usually modelled using Ohm's law, $i =
\frac{u}{R}$ or $u = R \cdot i$. Unfortunately, no \emph{single} block can
capture the behaviour of a resistor in a causal setting. If we need to compute
the current from the voltage, we should use a block corresponding to the first
equation.  If we need to compute the voltage from the current, we should use a
block corresponding to the second equation. As an exercise, the interested
reader might want to try to modify the block diagram in order to model a
similar circuit where the voltage source is replaced by a current source.

Simulink can be used to model hybrid systems, special blocks are used two \emph{switch} between block diagrams as a response to discrete events. This makes Simulink very useful indeed for hybrid simulation of structurally dynamic systems. However, the number of configurations or modes must be finite, and all modes are predetermined before the simulation. Thus Simulink does not enable us to model highly structurally dynamic systems. In addition, Simulink block diagrams are first-order thus Simulink does not support higher-order causal modelling.


\section{Modelica}
\label{secModelica}

%{
%include ../Format/modelica.lhs

Modelica \cite{Modelica2007} is a declarative object-oriented language for
non-causal modelling and simulation of physical systems.  Modelica models are
given using non-causal DAEs. A class system known from object-oriented
programming paradigm is used to structure the equations and support reuse of
defined models.

\subsection{Object-oriented Modelling}

To illustrate basic features of Modelica we model the circuit presented in
Figure \ref{figSimpleCircuit} again, this time in Modelica.

Firstly, we define a \emph{connector} record representing an electrical
connector.
\begin{code}
connector Pin
  flow Real i;
  Real v;
end Pin;
\end{code}
It has two variables, |i| and |v|, representing the current and the voltage
respectively. Connector records do not introduce any equations for
variables. The meaning of flow and non-flow (also called potential) variables
are explained later when \emph{connect-equations} are introduced.

Secondly, we define a class which captures common properties of electrical
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
The |TwoPin| class not only defines the variables, but also the non-causal
equations they satisfy. The variables |p| and |n| represent the positive and
negative pins of an electrical component. The variable |u| represents the
voltage drop across the component. The variable |i| represents the current
flowing into the positive pin.

By extending the |TwoPin| base class and adding component-specific equations
we define the classes representing resistor, capacitor, inductor and voltage
source models. Note the use of \emph{class inheritance} to reuse the equations
from the |TwoPin| model.
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
simulation. Their time derivatives are thus known to be zero, which is a
property that can be exploited during the simulation.  The difference between
constant and parameter is that the value of a constant is defined once and for
all in the source code, while a parameter can be set when an object of the
class is instantiated.

In addition, we define the class representing a ground pin.
\begin{code}
model Ground
  Pin p;
equation
  p.v = 0;
end Ground;
\end{code}

We can now use models of the circuit components we have defined to define a
class representing the circuit in Figure \ref{figSimpleCircuit} by
``connecting'' appropriate pins according to the figure. Note that the
parameters of resistor, capacitor, inductor and voltage source are not
specified. This means that the default values will be used.
\begin{code}
model SimpleCircuit
  Resistor   R;
  Capacitor  C;
  Inductor   L;
  VSourceAC AC;
  Ground     G;
equation
  connect(AC.p, R.p);
  connect(AC.p, L.p);
  connect(R.n,  C.p);
  connect(AC.n, C.n);
  connect(AC.n, L.n);
  connect(AC.n, G.p);
end SimpleCircuit;
\end{code}


\subsection{Non-causal Connections}

Connect statements are analysed and appropriate \emph{connection equations}
are generated by the Modelica compiler \cite{Modelica2007} as
follows. Connected flow variables generate sum to zero equations. In the case
of an electrical circuit it corresponds to Kirchhoff's current law. For the
|SimpleCircuit| model a Modelica compiler generates the following equations:
\begin{code}
AC.n.i + C.n.i + L.n.i + G.p.i = 0;
R.n.i + C.p.i = 0;
AC.p.i + R.p.i + L.p.i = 0;
\end{code}
Connected potential variables generate equality constraints stating that all
connected potential variables are equal at any point in time.  For
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

After type-checking and symbolic manipulation of equations in |SimpleCircuit|
model, a Modelica compiler generates executable code by utilising state of the
art symbolic and numerical integration methods.

As we have seen, non-causal languages, and in this particular case Modelica,
allow us to model physical systems at a high level of abstraction. The
structure of the models resemble the modelled systems. Consequently, it is
easy to reuse or modify existing models. For example, it is now trivial to
replace the voltage source with a current source in the Modelica model for the
circuit in Figure \ref{figSimpleCircuit}. We leave this as an exercise for the
reader.

% TODO here I should illustrate shortcomings of Modelica whent it comes to higher-order and structurally dynamic modelling. I can introduce breaking pendulum example as an structurally dynamic example. I also need an example that about higher-order modelling (e.g., transmission line model, maybe)

% \section{Non-causal Hybrid Modelling}
% \label{secHybridModelling}
% 
% Often physical systems are hybrid. For example, electrical and mechanical
% switches, auto-mobiles which have several continuous modes of operation,
% etc. Hybrid systems are usually modelled using the combination of continuous
% equations and the switching statements which specify discontinuous changes in
% the system.
% 
% The simulation of pure continuous systems is relatively well understood (see
% Section \ref{secModelling}). However, hybrid systems introduce a number of
% unique challenges \cite{Barton2002a,Mosterman1999a}, e.g. handling a large
% number of continuous modes, accurate event detection, and consistent
% initialization of state variables during mode switches. The integration of
% hybrid modeling with non-causal modeling raises further problems, e.g. dynamic
% causalisation during switches and simulation code generation.
% 
% Current non-causal modelling languages and related tools are very limited in
% their ability to model and simulate hybrid systems. Many of the limitations
% are related to the symbolic and numerical methods that must be used in the
% non-causal approach. But another important reason is that most such systems
% perform all symbolic manipulations and the simulation code generation before
% simulation begins \cite{Mosterman1999a}.
% 
% In this section we discuss hybrid modelling in non-causal languages. The
% current limitations are illustrated using the Modelica model of an example
% hybrid system.
% 
% \subsection{Modelling Hybrid Systems in Modelica}
% 
% Let us consider the catapult system depicted in Figure \ref{figCatapult}. The
% system is hybrid; it has two modes where the behaviour in each case is
% determined by a different set of continuous equations. In the first mode,
% before the stone with mass $m$ on the catapult gets fired, i.e. $\theta >
% \theta_0$, the stone performs rotational motion and its position is determined
% by the angle $\theta$. At the moment when $\theta = \theta_0$, the stone is
% released with the gained initial velocity from the catapult beam and continues
% its movement under the influence of gravity only. Here is an attempt to model
% this system in Modelica. The function |length| calculates absolute length of
% two-dimensional vectors. The comma-separated expressions enclosed in curly
% braces are vectors.
% 
% \begin{figure}[h]
% \includegraphics[width = \textwidth]{Graphics/catapult}
% \caption{Catapult model}
% \label{figCatapult}
% \end{figure}
% 
% \begin{samepage}
% \begin{code}
% model Catapult 
%   parameter Real k = 100, m = 1, theta0 = pi / 8;
%   parameter Real[2] l0 = {1,0}, h =  {0,1}, g = {0,-9.81};
%   Real[2] pos, vel;
%   protected Real[2] force, r, l;
%   protected Real theta(start = pi / 4), omega;
%   constant Real pi = 3.14159;
% equation 
%   vel = der(pos);
%   if (theta > theta0) then
%     pos - r = l0;
%     pos - l = h;
%     
%     omega = der(theta);
%     r = length(h) * {sin(theta), cos(theta)};
%     
%     force[1] * r[2] - force[2] * r[1]  = (m * r * r) * der(omega);
%     force = k * (length(l0) - length(l)) * (frac (l) (length(l))) + m * g;
%   else
%     m * der(vel) = m * g;
%   end if;
% end Catapult;
% \end{code}
% \end{samepage}
% 
% However this code fails to compile. The latest version of the Modelica
% standard \cite{Modelica2007} asserts that number of equations in both branches
% of an if statement must be equal when the conditional expression contains a
% non-parameter variable. This is a bit unfortunate. If considered separately,
% the equations in both branches do solve the publicly available variables,
% |pos| and |vel|, successfully. To fix the situation, the modeller might try to
% add dummy equations for variables not needed in the second mode. This version
% will compile, but the generated code will fail to simulate the system. This
% example was tried using OpenModelica \cite{OpenModelica2006} and Dymola
% \cite{Dymola2008} compilers. One of the problem with this example is that
% causality changes during the switch between two modes. In the first mode
% position is calculated from state variable $\theta$, which is not the case
% after the switch. This makes the job of the simulation code generator a lot
% harder and as it turns out Modelica tools are not able to handle it. This and
% related issues are covered in greater detail in
% \cite{ModelicaTutorial2000}. The suggested solution is more complicated and
% verbose which requires reformulation of a model by making it causal. The need
% of manual reformulation to conform to certain causality eliminates the
% advantages of working in non-causal modelling language. Thus, Modelica as only
% a limited support for non-causal modelling and simulation of structurally
% dynamic systems.
% 
% For a number of reasons, Modelica does not support modelling and simulation of
% highly structurally dynamic systems. Firstly, the Modelica language lacks
% expressiveness to describe structural changes in the physical systems. The
% catapult example demonstrated the problems which arise when there is a need
% for change of a number of variables in the system. Secondly, the state of the
% art Modelica compilers carry out the symbolic processing and generate the
% simulation code at once. In the presence of highly dynamic systems this is
% very hard or sometimes impossible to do due to very large or unbounded number
% of modes which needs to be compiled in advance before the simulation.
% 
% Efforts are underway in the non-causal modelling community in general, and in
% Modelica community in particular, to improve the support for M{\&}S of
% structurally dynamic systems and to enable M{\&}S of highly structurally
% dynamic systems.

%% %% \subsection{Other Approaches to Non-causal Hybrid Modelling}

%% %% \subsubsection{MOSILA}

%% %% MOSILA is an extension of the Modelica language that supports the description of structural changes using object-oriented statecharts \cite{Nytsch-Geusen2005a}. This enables modelling of structurally dynamic systems. However, the statechart approach implies that all structural modes are specified in advance. This means that MOSILA does not support highly structurally dynamic systems.

%% %% \subsubsection{Sol}

%% %% Sol is a Modelica-like language \cite{Zimmer2008a}. It introduces language constructs which enable the description of systems where objects are dynamically created and deleted, with the aim of supporting modelling of highly structurally dynamic systems. However, the work is in its very early stages and the design and implementation of the language has not been completed yet.


%}
