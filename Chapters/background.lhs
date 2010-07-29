\chapter{Background}
\label{chapBackground}

\section{Functional Programming in Haskell}

In this section, in order to make the thesis self-contained, we give an
introduction to functional programming in Haskell. The readers familiar with
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

In the following we explain the simplest numerical solver integration method
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
\item Translate the mathematical representation into a program
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

Simulink \cite{Simulink2008} is a graphical block diagramming tool for causal
modelling and simulation. The block diagram in Figure
\ref{figSimpleCircuitBlockDiagram} is a model of the circuit from
\ref{figSimpleCircuit}.

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

Simulink can be used to model hybrid systems, special blocks are used two
\emph{switch} between block diagrams as a response to discrete events. This
makes Simulink very useful indeed for hybrid simulation of structurally
dynamic systems. However, the number of configurations or \emph{modes} is
finite, and all modes are predetermined before a simulation. Thus Simulink
does not enable us to model highly structurally dynamic systems. In addition,
Simulink block diagrams are first-order thus Simulink does not support
higher-order causal modelling.


%% \section{Yampa}
%% \label{secYampa}

%% From a modelling perspective Yampa \cite{Nilsson2002a} can be seen as a domain
%% specific language embedded in Haskell for causal modelling and
%% simulation. While being a relatively new language, still under active
%% development and research, Yampa has already been used in number of
%% applications from different domains, e.g. robotics \cite{Peterson1999a,
%%   Peterson1999b, Pembeci2002}, computer vision \cite{Peterson2001}, video
%% games \cite{Courtney2003b, Cheong2005} and musical synthesis
%% \cite{Giorgidze2008a, Giorgidze2008b}.

%% As the concepts of first-class non-causal models and run-time generation of
%% new models is inspired by Yampa, we describe its concepts and features in the
%% following. The presentation in this section draws heavily from the Yampa
%% summaries in earlier Yampa-related publications \cite{Courtney2003b,
%%   Giorgidze2008a, Giorgidze2008b}.

%% \subsection{Fundamental Concepts}
%% \label{subSecYampaConcepts}
%% %% %{
%% %% %include ../Format/fhm.lhs

%% Yampa is based on two central concepts: \emph{signals} and \emph{signal
%%   functions}. A signal is a function from time to values of some type |a|:
%% \begin{center}
%% |Signal alpha ~= Time -> alpha|
%% \end{center}
%% |Time| is continuous, and is represented as a non-negative real
%% number. The type parameter |alpha| specifies the type of values
%% carried by the signal. A \emph{signal function} is a function from |Signal| to |Signal|:
%% \begin{center}
%% |SF alpha beta ~= Signal alpha -> Signal beta|
%% \end{center}

%% When a value of type |SF alpha beta| is applied to an input signal of type
%% |Signal alpha|, it produces an output signal of type |Signal beta|. Signal
%% functions are \emph{first-class values} in Yampa. Signals, however, are not:
%% they only exist indirectly through the notion of signal function.

%% %% %}

%% %% %{
%% %% %include ../Format/arrows.lhs

%% %% In order to ensure that signal functions are executable, we require them to be
%% %% causal, i.e. the output of a signal function at time $t$ is uniquely
%% %% determined by the input signal on the interval $[0,t]$.

%% If a signal function is such that the output at time $t$ only depends on the
%% input at the very same time instant $t$, it is called
%% \emph{stateless}. Otherwise it is \emph{stateful}.

%% Signals and signal functions are, for illustration purposes, often represented
%% as block-diagrams resembling Simulink diagrams. Figure~\ref{fig:sf}
%% illustrates one such diagram. The box in Figure~\ref{fig:sf} represents a
%% signal function with one signal flowing into the box's input port and another
%% signal flowing out of the box's output port. Line segments (or ``wires'')
%% represent signals. Arrow heads are used to indicate the direction of flow. So
%% |f :: SF a b| is a signal function, and |x :: Signal a| and |y :: Signal b|
%% are signals.


%% \begin{figure}[h]
%% \begin{center}
%% \includegraphics[width=0.33\textwidth]{Graphics/sf}
%% \end{center}
%% \caption{A signal function}
%% \label{fig:sf}
%% \end{figure}

%% % The following is out of flow.
%% %In general it is very hard to visualise structurally dynamic signal function networks, because it is not obvious how to represent function which takes argument and returns a diagram whose structure is not known in advance. 

%% \subsection{Composing Signal Functions}

%% Modelling in Yampa consists of defining signal functions compositionally using
%% Yampa's library of primitive signal functions and a set of
%% combinators. Yampa's signal functions are an instance of the arrow framework
%% proposed by Hughes \cite{Hughes2000}. Some central arrow combinators are |arr|
%% (that lifts an ordinary function to the stateless signal function), |>>>|,
%% |&&&|, and |loop|. In Yampa, they have the following type signatures:
%% \begin{code}
%% arr    ::  (a -> b) -> SF a b
%% (>>>)  ::  SF a b -> SF b c -> SF a c
%% (&&&)  ::  SF a b -> SF a c -> SF a (b,c)
%% loop   ::  SF (a,c) (b,c) -> SF a b
%% \end{code}
%% Figure~\ref{fig:basic-combinators} illustrates aforementioned
%% combinators using diagrams. Through the use of these and related
%% combinators, arbitrary signal function networks can be expressed.

%% \begin{figure}[h]
%% \centering
%% \subfigure[|arr f|]{
%%     \begin{minipage}[b][1.5cm][c]{2cm}
%%         \centering
%%         \includegraphics[scale=1.00]{Graphics/combinator-arr-narrow}
%%     \end{minipage}
%%     \label{fig:combinator-arr}
%% }
%% \subfigure[|f >>> g|]{
%%     \begin{minipage}[b][1.5cm][c]{3cm}
%%         \centering
%%         \includegraphics[scale=1.00]{Graphics/combinator-compose-narrow}
%%     \end{minipage}
%%     \label{fig:combinator-compose}
%% }
%% \subfigure[|f &&& g|]{
%%     \begin{minipage}[b][1.5cm][c]{2.5cm}
%%         \centering
%%         \includegraphics[scale=1.00]{Graphics/combinator-parfanout-narrow}
%%     \end{minipage}
%%     \label{fig:combinator-parfanout}
%% }
%% \subfigure[|loop f|]{
%%     \begin{minipage}[b][1.5cm][c]{2.5cm}
%%         \centering
%%         \includegraphics[scale=.90]{Graphics/combinator-loop-narrow}
%%     \end{minipage}
%%     \label{fig:combinator-loop}
%% }
%% \caption{Basic signal function combinators.}
%% \label{fig:basic-combinators}
%% \end{figure}

%% %\subsection{Arrow Syntax}

%% %{
%% %format pat1
%% %format pat2
%% %format patn = "\Varid{pat}_n"
%% %format pati = "\Varid{pat}_i"
%% %format sfexp1
%% %format sfexp2
%% %format sfexpn = "\Varid{sfexp}_n"
%% %format sfexpi = "\Varid{sfexp}_i"
%% %format exp1
%% %format exp2
%% %format expn = "\Varid{exp}_n"
%% %format expi = "\Varid{exp}_i"
%% %format ... = "\ldots"

%% %Paterson's arrow notation \cite{Paterson2001} simplifies writing of Yampa programs as it allows signal function networks to be described directly. In particular, the notation effectively allows signals to be named, despite signals not being first class values. In this syntax, an expression denoting a signal function has the form:
%% %\begin{code}
%% %proc pat -> do
%% %    pat1  <-  sfexp1  -<  exp1
%% %    pat2  <-  sfexp2  -<  exp2
%% %    ...
%% %    patn  <-  sfexpn  -<  expn
%% %    returnA -< exp
%% %\end{code}
%% %Note that this is just \emph{syntactic sugar}. the notation is %translated into plain Haskell using the arrow combinators.

%% %The keyword |proc| is analogous to the $\lambda$ in $\lambda$-expressions, |pat| and |pati| are patterns binding signal variables pointwise by matching on instantaneous signal values, |exp| and |expi| are expressions defining instantaneous signal values, and |sfexpi| are expressions denoting signal functions. The idea is that the signal, being defined pointwise by each |expi|, is fed into the corresponding signal function |sfexpi|, whose output is bound pointwise in |pati|. The overall input to the signal function denoted by the |proc|-expression is bound pointwise by |pat|, and its output signal is defined pointwise by the expression |exp|. The signal variables bound in the patterns may occur in the signal value expressions, but \emph{not} in the signal function expressions |sfexpi|. 

%% %An optional keyword |rec|, applied to a group of definitions, permits signal variables to occur in expressions that textually precede the definition of the variable, allowing recursive definitions (feedback loops). Finally,
%% %\begin{code}
%% %let pat = exp
%% %\end{code}
%% %is shorthand for
%% %\begin{code}
%% %pat <- arr id -< exp
%% %\end{code}
%% %allowing binding of instantaneous values in a straightforward way.

%% %The syntactic sugar is implemented by a preprocessor which expands out the definitions using only the basic arrow combinators |arr|, |>>>|, |first|, and, if |rec| is used, |loop|.

%% %Here we use |first :: SF a b -> SF (a,c) (b,c)| as primitive combinator (see Figure~\ref{fig:combinator-first}) instead of |&&&|. They can be defined in terms of each other, so one or another can be used as primitive without loosing expressiveness.

%% %\begin{figure}[h]
%% %\begin{center}
%% %\includegraphics[width=0.3\textwidth]{Graphics/combinator-first}
%% %\end{center}
%% %\caption{Signal Function combinator |first|}
%% %\label{fig:combinator-first}
%% %\end{figure}
  
%% %For a concrete example, consider the following:
%% %\begin{code}
%% %sf =  proc (a,b) -> do
%% %        (c1, c2)  <-  sf1 &&& sf2  -<  a
%% %        d         <-  sf3 <<< sf4  -<  (c1,b)
%% %        rec
%% %          e <- sf5 -< (c2,d,e)
%% %        returnA -< (d,e)
%% %\end{code}
%% %Note the use of the tuple pattern for splitting |sf|'s input into two ``named signals'', |a| and |b|. Also note the use of tuple expressions and patterns for pairing and splitting signals in the body of the definition; for example, for splitting the output from |sf1 &&& sf2|. Also note how the arrow notation may be freely mixed with the use of basic arrow combinators.

%% %For illustration purposes we translate the code above to plain arrow combinators.

%% %\begin{code}
%% %sf
%% %  = ((first (sf1 &&& sf2) >>> arr (\ ((c1, c2), b) -> ((b, c1), c2)))
%% %       >>>
%% %       (first (arr (\ (b, c1) -> (c1, b)) >>> (sf3 <<< sf4)) >>>
%% %          loop
%% %            (arr (\ ((d, c2), e) -> ((c2, d, e), d)) >>>
%% %               (first sf5 >>> arr (\ (e, d) -> ((d, e), e))))))
%% %\end{code}

%% %Even for moderately complex networks, the combinator notation becomes very hard to read. In contrast the arrow syntactic sugar provides a clearer and more intuitive way to describe signal function networks.

%% %% %}

%% \subsection{Events and Event Sources}

%% While some aspects of a system, like the current flow in an electrical
%% circuit, are naturally modelled as continuous signals, other aspects like
%% electrical switches are more naturally modelled as \emph{discrete events}.

%% To model discrete events, Yampa introduces the |Event| type, which is
%% isomorphic to Haskell's |Maybe| type:

%% \begin{code}
%% data Event a = NoEvent | Event a
%% \end{code}

%% A signal function whose output signal is of type |Event T| for some type |T|
%% is called an \emph{event source}. The value carried by an event occurrence may
%% be used to convey information about the occurrence.

%% \subsection{Switching}
%% \label{sec:yampaSwitch}

%% The structure of a Yampa system may evolve over time. These structural changes
%% are known as \emph{mode switches}. This is accomplished through a family of
%% \emph{switching} primitives that use events to trigger changes in the
%% connectivity of a system. The simplest such primitive is |switch|:

%% \begin{code}
%% switch :: SF a (b,Event c) -> (c -> SF a b) -> SF a b
%% \end{code}

%% The |switch| combinator switches from one subordinate signal function into
%% another when a switching event occurs. Its first argument is the signal
%% function that initially is active. It outputs a pair of signals. The first
%% defines the overall output while the initial signal function is active. The
%% second signal carries the event that will cause the switch to take place. Once
%% the switching event occurs, |switch| applies its second argument to the value
%% tagged to the event and switches into the resulting signal function.

%% Informally, |switch sf sfk| behaves as follows: At time $t=0$, the initial
%% signal function, |sf|, is applied to the input signal of the |switch| to
%% obtain a pair of signals, |bs| (type: |Signal bs|) and |es| (type: |Signal
%% (Event c)|). The output signal of the |switch| is |bs| until the event stream
%% |es| has an occurrence at some time $t_e$, at which point the event value is
%% passed to |sfk| to obtain a signal function |sf'|. The overall output signal
%% switches from |bs| to |sf'| applied to a suffix of the input signal starting
%% at $t_e$.

%% Yampa also includes \emph{parallel} switching constructs that maintain
%% \emph{dynamic collections} of signal functions connected in parallel
%% \cite{Nilsson2002a}.  Signal functions can be added to or removed from such a
%% collection at runtime in response to events, while \emph{preserving} any
%% internal state of all other signal functions in the collection. See
%% Figure~\ref{fig:varyingStructure}.

%% \begin{figure}[h]
%% \begin{center}
%% \includegraphics[width=0.5\textwidth]{Graphics/varying-structure}
%% \end{center}
%% \caption{System of interconnected signal functions with varying structure
%% \label{fig:varyingStructure}}
%% \end{figure}

%% The first-class status of signal functions, in combination with switching over
%% dynamic collections of signal functions, makes Yampa an unusually flexible
%% language for describing higher-order and hybrid systems with highly dynamic
%% structure. However, currently Yampa only features an interpreted
%% implementation, thus sacrificing the efficiency. The run-time symbolic
%% processing and JIT compilation techniques presented in this thesis can be used
%% applied to Yampa to derive more efficient compiled implementation.

%% %% \subsection{Simulating Yampa Models}
%% %% \label{subSecReactimate}

%% %% The Yampa library provides functions to simulate Yampa models. These functions
%% %% approximate the continuous-time model presented here by performing discrete
%% %% sampling of the signal function, feeding input to and processing output from
%% %% the signal function at each time step.

%% %% At present, Yampa uses the forward Euler numerical integration method and has
%% %% no support for more sophisticated numerical methods. This has impact on
%% %% precision and performance aspects of a simulation. Yampa programs also suffer
%% %% from scalability and efficiency issues discussed in detail in
%% %% \cite{Sculthorpe2008a}. This limits Yampa's applicability for large scale and
%% %% high performance scenarios.

%% %}


\section{Modelica}
\label{secModelica}

%% %{
%% %include ../Format/modelica.lhs

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

%% \section{Non-causal Hybrid Modelling}
%% \label{secHybridModelling}

%% Often physical systems are hybrid. For example, electrical and mechanical
%% switches, auto-mobiles which have several continuous modes of operation,
%% etc. Hybrid systems are usually modelled using the combination of continuous
%% equations and the switching statements which specify discontinuous changes in
%% the system.

%% The simulation of pure continuous systems is relatively well understood (see
%% Section \ref{secModelling}). However, hybrid systems introduce a number of
%% unique challenges \cite{Barton2002a,Mosterman1999a}, e.g. handling a large
%% number of continuous modes, accurate event detection, and consistent
%% initialization of state variables during mode switches. The integration of
%% hybrid modeling with non-causal modeling raises further problems, e.g. dynamic
%% causalisation during switches and simulation code generation.

%% Current non-causal modelling languages and related tools are very limited in
%% their ability to model and simulate hybrid systems. Many of the limitations
%% are related to the symbolic and numerical methods that must be used in the
%% non-causal approach. But another important reason is that most such systems
%% perform all symbolic manipulations and the simulation code generation before
%% simulation begins \cite{Mosterman1999a}.

%% In this section we discuss hybrid modelling in non-causal languages. The
%% current limitations are illustrated using the Modelica model of an example
%% hybrid system.

%% \subsection{Modelling Hybrid Systems in Modelica}

%% Let us consider the catapult system depicted in Figure \ref{figCatapult}. The
%% system is hybrid; it has two modes where the behaviour in each case is
%% determined by a different set of continuous equations. In the first mode,
%% before the stone with mass $m$ on the catapult gets fired, i.e. $\theta >
%% \theta_0$, the stone performs rotational motion and its position is determined
%% by the angle $\theta$. At the moment when $\theta = \theta_0$, the stone is
%% released with the gained initial velocity from the catapult beam and continues
%% its movement under the influence of gravity only. Here is an attempt to model
%% this system in Modelica. The function |length| calculates absolute length of
%% two-dimensional vectors. The comma-separated expressions enclosed in curly
%% braces are vectors.

%% \begin{figure}[h]
%% \includegraphics[width = \textwidth]{Graphics/catapult}
%% \caption{Catapult model}
%% \label{figCatapult}
%% \end{figure}

%% \begin{samepage}
%% \begin{code}
%% model Catapult 
%%   parameter Real k = 100, m = 1, theta0 = pi / 8;
%%   parameter Real[2] l0 = {1,0}, h =  {0,1}, g = {0,-9.81};
%%   Real[2] pos, vel;
%%   protected Real[2] force, r, l;
%%   protected Real theta(start = pi / 4), omega;
%%   constant Real pi = 3.14159;
%% equation 
%%   vel = der(pos);
%%   if (theta > theta0) then
%%     pos - r = l0;
%%     pos - l = h;
    
%%     omega = der(theta);
%%     r = length(h) * {sin(theta), cos(theta)};
    
%%     force[1] * r[2] - force[2] * r[1]  = (m * r * r) * der(omega);
%%     force = k * (length(l0) - length(l)) * (l / length(l)) + m * g;
%%   else
%%     m * der(vel) = m * g;
%%   end if;
%% end Catapult;
%% \end{code}
%% \end{samepage}

%% However this code fails to compile. The latest version of the Modelica
%% standard \cite{Modelica2007} asserts that number of equations in both branches
%% of an if statement must be equal when the conditional expression contains a
%% non-parameter variable. This is a bit unfortunate. If considered separately,
%% the equations in both branches do solve the publicly available variables,
%% |pos| and |vel|, successfully. To fix the situation, the modeller might try to
%% add dummy equations for variables not needed in the second mode. This version
%% will compile, but the generated code will fail to simulate the system. This
%% example was tried using OpenModelica \cite{OpenModelica2006} and Dymola
%% \cite{Dymola2008} compilers. One of the problem with this example is that
%% causality changes during the switch between two modes. In the first mode
%% position is calculated from state variable $\theta$, which is not the case
%% after the switch. This makes the job of the simulation code generator a lot
%% harder and as it turns out Modelica tools are not able to handle it. This and
%% related issues are covered in greater detail in
%% \cite{ModelicaTutorial2000}. The suggested solution is more complicated and
%% verbose which requires reformulation of a model by making it causal. The need
%% of manual reformulation to conform to certain causality eliminates the
%% advantages of working in non-causal modelling language. Thus, Modelica as only
%% a limited support for non-causal modelling and simulation of structurally
%% dynamic systems.

%% For a number of reasons, Modelica does not support modelling and simulation of
%% highly structurally dynamic systems. Firstly, the Modelica language lacks
%% expressiveness to describe structural changes in the physical systems. The
%% catapult example demonstrated the problems which arise when there is a need
%% for change of a number of variables in the system. Secondly, the state of the
%% art Modelica compilers carry out the symbolic processing and generate the
%% simulation code at once. In the presence of highly dynamic systems this is
%% very hard or sometimes impossible to do due to very large or unbounded number
%% of modes which needs to be compiled in advance before the simulation.

%% Efforts are underway in the non-causal modelling community in general, and in
%% Modelica community in particular, to improve the support for M{\&}S of
%% structurally dynamic systems and to enable M{\&}S of highly structurally
%% dynamic systems.

%% %% \subsection{Other Approaches to Non-causal Hybrid Modelling}

%% %% \subsubsection{MOSILA}

%% %% MOSILA is an extension of the Modelica language that supports the description of structural changes using object-oriented statecharts \cite{Nytsch-Geusen2005a}. This enables modelling of structurally dynamic systems. However, the statechart approach implies that all structural modes are specified in advance. This means that MOSILA does not support highly structurally dynamic systems.

%% %% \subsubsection{Sol}

%% %% Sol is a Modelica-like language \cite{Zimmer2008a}. It introduces language constructs which enable the description of systems where objects are dynamically created and deleted, with the aim of supporting modelling of highly structurally dynamic systems. However, the work is in its very early stages and the design and implementation of the language has not been completed yet.


%% %% %}
