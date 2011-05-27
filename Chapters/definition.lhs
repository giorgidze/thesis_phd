\chapter{Definition of Hydra}
\label{chapDefinition}

This is a highly-technical chapter giving a formal definition of the Hydra
language. The definition is given in four steps. Firstly, we define Hydra's
lexical structure and concrete syntax by using the regular expression notation
and the BNF notation, respectively. Secondly, we give Hydra's untyped abstract
syntax as a Haskell algebraic data type definition. Thirdly, we define Hydra's
typed abstract syntax as a Haskell generalised algebraic data type definition
and give a translation from the untyped abstract syntax to the typed abstract
syntax. The typed representation fully embodies Hydra's type system and can be
seen as a definition of Hydra's type system in terms of the Haskell type
system. In other words, Hydra's type system is embedded into Haskell's type
system. Finally, we give ideal denotational semantics of Hydra by giving
meaning to the typed abstract syntax in terms of second-order logic.

\section{Concrete Syntax}

\newcommand{\emptyP}{\mbox{$\epsilon$}}
\newcommand{\terminal}[1]{\mbox{{\texttt {#1}}}}
\newcommand{\nonterminal}[1]{\mbox{$\langle \mbox{{\sl #1 }} \! \rangle$}}
\newcommand{\arrow}{\mbox{::=}}
\newcommand{\delimit}{\mbox{$||$}}
\newcommand{\reserved}[1]{\mbox{{\texttt {#1}}}}
\newcommand{\literal}[1]{\mbox{{\texttt {#1}}}}
\newcommand{\symb}[1]{\mbox{{\texttt {#1}}}}

The syntactic structure of Hydra is given in Figure \ref{figSyntax}, which
uses the BNF notation. Specifically, non-terminals are enclosed between
$\langle$ and $\rangle$. The symbols {\arrow} (production), {\delimit} (union)
and {\emptyP} (empty rule) belong to the BNF notation. All other symbols are
terminals.

\begin{figure}

\begin{tabular}{lll}
{\nonterminal{SigRel}} & {\arrow}  &{\nonterminal{Pattern}} {\terminal{{$-$}{$>$}}} {\terminal{\{}} {\nonterminal{ListEquation}} {\terminal{\}}}  \\
\end{tabular}

\begin{tabular}{lll}
{\nonterminal{SigFun}} & {\arrow}  &{\nonterminal{Pattern}} {\terminal{{$-$}{$>$}}} {\terminal{\{}} {\nonterminal{Expr}} {\terminal{\}}}  \\
\end{tabular}

\begin{tabular}{lll}
{\nonterminal{Pattern}} & {\arrow}  &{\terminal{\_}}  \\
 & {\delimit}  &{\nonterminal{Ident}}  \\
 & {\delimit}  &{\terminal{()}}  \\
 & {\delimit}  &{\terminal{(}} {\nonterminal{Pattern}} {\terminal{,}} {\nonterminal{Pattern}} {\terminal{)}}  \\
\end{tabular}

\begin{tabular}{lll}
{\nonterminal{Equation}} & {\arrow}  &{\nonterminal{Expr}} {\terminal{{$=$}}} {\nonterminal{Expr}}  \\
 & {\delimit}  &{\terminal{init}} {\nonterminal{Expr}} {\terminal{{$=$}}} {\nonterminal{Expr}}  \\
 & {\delimit}  &{\terminal{local}} {\nonterminal{Ident}}  \\
 & {\delimit}  &{\nonterminal{HsExpr}} {\terminal{{$<$}{$>$}}} {\nonterminal{Expr}}  \\
\end{tabular}

\begin{tabular}{lll}
{\nonterminal{Expr1}} & {\arrow}  &{\nonterminal{Expr1}} {\terminal{{$||$}{$||$}}} {\nonterminal{Expr2}}  \\
 & {\delimit}  &{\nonterminal{Expr2}}  \\
\end{tabular}

\begin{tabular}{lll}
{\nonterminal{Expr2}} & {\arrow}  &{\nonterminal{Expr2}} {\terminal{\&\&}} {\nonterminal{Expr3}}  \\
 & {\delimit}  &{\nonterminal{Expr3}}  \\
\end{tabular}

\begin{tabular}{lll}
{\nonterminal{Expr3}} & {\arrow}  &{\nonterminal{Expr4}} {\terminal{{$<$}}} {\nonterminal{Expr4}}  \\
 & {\delimit}  &{\nonterminal{Expr4}} {\terminal{{$<$}{$=$}}} {\nonterminal{Expr4}}  \\
 & {\delimit}  &{\nonterminal{Expr4}} {\terminal{{$>$}}} {\nonterminal{Expr4}}  \\
 & {\delimit}  &{\nonterminal{Expr4}} {\terminal{{$>$}{$=$}}} {\nonterminal{Expr4}}  \\
 & {\delimit}  &{\nonterminal{Expr4}}  \\
\end{tabular}

\begin{tabular}{lll}
{\nonterminal{Expr4}} & {\arrow}  &{\nonterminal{Expr4}} {\terminal{{$+$}}} {\nonterminal{Expr5}}  \\
 & {\delimit}  &{\nonterminal{Expr4}} {\terminal{{$-$}}} {\nonterminal{Expr5}}  \\
 & {\delimit}  &{\nonterminal{Expr5}}  \\
\end{tabular}

\begin{tabular}{lll}
{\nonterminal{Expr5}} & {\arrow}  &{\nonterminal{Expr5}} {\terminal{/}} {\nonterminal{Expr6}}  \\
 & {\delimit}  &{\nonterminal{Expr5}} {\terminal{*}} {\nonterminal{Expr6}}  \\
 & {\delimit}  &{\nonterminal{Expr6}}  \\
\end{tabular}

\begin{tabular}{lll}
{\nonterminal{Expr6}} & {\arrow}  &{\nonterminal{Expr6}} {\terminal{\^}} {\nonterminal{Expr7}}  \\
 & {\delimit}  &{\terminal{{$-$}}} {\nonterminal{Expr7}}  \\
 & {\delimit}  &{\nonterminal{Expr7}}  \\
\end{tabular}

\begin{tabular}{lll}
{\nonterminal{Expr7}} & {\arrow}  &{\nonterminal{Expr7}} {\nonterminal{Expr8}}  \\
 & {\delimit}  &{\nonterminal{Expr8}}  \\
\end{tabular}

\begin{tabular}{lll}
{\nonterminal{Expr8}} & {\arrow}  &{\nonterminal{Ident}}  \\
 & {\delimit}  &{\nonterminal{HsExpr}}  \\
 & {\delimit}  &{\nonterminal{Integer}}  \\
 & {\delimit}  &{\nonterminal{Double}}  \\
 & {\delimit}  &{\terminal{()}}  \\
 & {\delimit}  &{\terminal{(}} {\nonterminal{Expr}} {\terminal{,}} {\nonterminal{Expr}} {\terminal{)}}  \\
 & {\delimit}  &{\terminal{(}} {\nonterminal{Expr}} {\terminal{)}}  \\
\end{tabular}

\begin{tabular}{lll}
{\nonterminal{Expr}} & {\arrow}  &{\nonterminal{Expr1}}  \\
\end{tabular}

\begin{tabular}{lll}
{\nonterminal{ListEquation}} & {\arrow}  &{\emptyP} \\
 & {\delimit}  &{\nonterminal{Equation}}  \\
 & {\delimit}  &{\nonterminal{Equation}} {\terminal{;}} {\nonterminal{ListEquation}}  \\
\end{tabular}

\caption{\label{figSyntax} Syntactic structure of Hydra.}
\end{figure}


Identifiers \nonterminal{Ident} are unquoted strings beginning with a letter,
followed by any combination of letters, digits, and the characters {\tt \_}
and {\tt '}, reserved words excluded. The reserved words used in Hydra are
|init| and |local|.

Integer literals \nonterminal{Int}\ are nonempty sequences of digits.
Double-precision float literals \nonterminal{Double}\ have the structure
indicated by the regular expression $\nonterminal{digit}+ \mbox{{\it `.'}}
\nonterminal{digit}+ (\mbox{{\it `e'}} \mbox{{\it `-'}}?
\nonterminal{digit}+)?$; that is, two sequences of digits separated by a
decimal point, optionally followed by an unsigned or negative exponent.

HsExpr literals represent antiquoted Haskell expressions and are recognised by
the regular expression \(\mbox{`\$'} ({\nonterminal{anychar}} - \mbox{`\$'})*
\mbox{`\$'}\)

The symbols used in Hydra are given in Figure \ref{figSymbols}. In Hydra,
single-line comments begin with {\symb{{$-$}{$-$}}} and multiple-line comments
are enclosed with {\symb{\{{$-$}}} and {\symb{{$-$}\}}}.

\begin{figure}[H]
\centering
\begin{tabular}{lll}
{\symb{{$-$}{$>$}}} &{\symb{\{}} &{\symb{\}}} \\
{\symb{\_}} &{\symb{()}} &{\symb{(}} \\
{\symb{,}} &{\symb{)}} &{\symb{{$=$}}} \\
{\symb{{$<$}{$>$}}} &{\symb{{$||$}{$||$}}} &{\symb{\&\&}} \\
{\symb{{$<$}}} &{\symb{{$<$}{$=$}}} &{\symb{{$>$}}} \\
{\symb{{$>$}{$=$}}} &{\symb{{$+$}}} &{\symb{{$-$}}} \\
{\symb{/}} &{\symb{*}} &{\symb{\^}} \\
{\symb{;}} & & \\
\end{tabular}
\caption{\label{figSymbols} Symbols used in Hydra.}
\end{figure}

\section{Abstract Syntax}

Hydra's abstract syntax is given in Figure \ref{figAbstractSyntax}. The
Haskell algebraic data type definition is derived from the concrete syntax
defined in previous section. The representation is untyped; that is, it allows
for terms that are syntactically correct but not necessarily type correct.

\begin{figure}
\begin{code}
data Ident   =  Ident   String
data HsExpr  =  HsExpr  String
\end{code}

\begin{code}
data SigRel  =  SigRel  Pattern  [Equation]
data SigFun  =  SigFun  Pattern  Expr
\end{code}

\begin{code}
data Pattern  =   PatWild
              |   PatName  Ident
              |   PatUnit
              |   PatPair  Pattern  Pattern
\end{code}

\begin{code}
data Equation   =  EquEqual      Expr    Expr
                |  EquInit       Expr    Expr
                |  EquLocal      Ident
                |  EquSigRelApp  HsExpr  Expr
\end{code}

\begin{code}
data Expr  =  ExprOr        Expr    Expr
           |  ExprAnd       Expr    Expr
           |  ExprLt        Expr    Expr
           |  ExprLte       Expr    Expr
           |  ExprGt        Expr    Expr
           |  ExprGte       Expr    Expr
           |  ExprAdd       Expr    Expr
           |  ExprSub       Expr    Expr
           |  ExprDiv       Expr    Expr
           |  ExprMul       Expr    Expr
           |  ExprPow       Expr    Expr
           |  ExprNeg       Expr
           |  ExprApp       Expr    Expr
           |  ExprVar       Ident
           |  ExprAnti      HsExpr
           |  ExprInteger   Integer
           |  ExprDouble    Double
           |  ExprUnit
           |  ExprPair      Expr    Expr
\end{code}
\caption{\label{figAbstractSyntax} Abstract syntax of Hydra.}
\end{figure}


The data type |Ident| is used to represent identifiers, specifically, signal
variable and built-in signal function identifiers. The data type |HsExpr| is
used to represent antiquoted Haskell expressions.

The data type |SigRel| is used to represent signal relations. The data type
has a single constructor. Given a pattern and a list of equations the
constructor constructs the corresponding signal relation.

The data type |SigFun| is used to represent signal functions. The data type
has a single constructor. Given a pattern and a signal expression the
constructor constructs the corresponding signal function.

The data type |Pattern| is used to represent patterns that bind signal
variables. There are four ways to construct a pattern. The constructor
|PatWild| constructs the wild card pattern. Given an identifier the
constructor |PatName| constructs a pattern that binds the corresponding single
signal variable. The constructor |PatUnit| constructs the pattern that only
matches unit signals. The constructor |PatPair| constructs a pattern that
matches a pair of patterns.

The data type |Equation| is used to represent noncausal equations and local
signal variable declarations. The constructor |EquEqual| constructs an
equation that asserts equality of two signal expressions. The constructor
|EquInit| constructs an initialisation equation that asserts equality of two
signal expressions. The constructor |EquLocal| constructs a local variable
declaration. The constructor |EquSigRelApp| constructs a signal relation
application that applies the signal relation referred in the antiquoted
Haskell expression to the given signal expression.

The data type |Expr| is used to represent signal expressions. Common
mathematical operations, identifiers, antiquoted Haskell expressions, integer
and real constants, unit signals, and pairs of signals can be used to
construct signal expressions (see Figure \ref{figAbstractSyntax} for details).

\section{Desugaring}

Before we turn our attention to the translation of the untyped abstract syntax
into the typed abstract syntax, we describe a translation that desugars all
equations that assert equality of signal pairs into equations asserting
equality of scalar signals. This translation allows for a simpler typed
intermediated representation as we show in the following section. The
translation is given in Figure \ref{figDesugaring} as a Haskell function
working with the untyped abstract syntax of Hydra.

\begin{figure}[H]

\begin{code}
desugarPairSigRel                   ::  SigRel -> SigRel
desugarPairSigRel (SigRel pat eqs)  =   SigRel pat (concat [ desugarPairEquation eq | eq <- eqs ])
\end{code}

\begin{code}
desugarPairEquation                                         ::  Equation -> [Equation]
desugarPairEquation (EquEqual  (Pair e1 e2) (Pair e3 e4))   =   desugarPairEquation (EquEqual  e1 e3)  ++ desugarPairEquation (EquEqual  e2 e4)
desugarPairEquation (EquInit   (Pair e1 e2) (Pair e3 e4))   =   desugarPairEquation (EquInit   e1 e3)  ++ desugarPairEquation (EquInit   e2 e4)
desugarPairEquation (eq)                                    =   [eq]
\end{code}

\caption{\label{figDesugaring} Desugaring translation of Hydra.}

\end{figure}


\section{Typed Abstract Syntax}

The typed abstract syntax that embodies the type system of Hydra is given in
Figure \ref{figTypedRepresentation} as a Haskell generalised algebraic data
type definition. Note that the types |Signal alpha| and |PrimSF alpha beta|
are genuine generalised algebraic data types, while the data types |SR alpha|,
|SF alpha beta| and |Equation| are algebraic data types that use the
generalised algebraic data type notation for consistency.

\begin{figure}[H]
\begin{code}
data SR alpha where
  SR      ::  (Signal alpha -> [Equation]) -> SR alpha
  Switch  ::  SR alpha -> SF alpha Bool -> (alpha -> SR alpha) -> SR alpha

data SF alpha beta where
  SF :: (Signal alpha -> Signal beta) -> SF alpha beta

data Equation where
  Local  ::  (Signal Real -> [Equation]) -> Equation
  Equal  ::  Signal Real -> Signal Real -> Equation
  Init   ::  Signal Real -> Signal Real -> Equation
  App    ::  SR alpha -> Signal alpha -> Equation

data Signal alpha where
  Unit     ::  Signal ()
  Time     ::  Signal Real
  Const    ::  alpha -> Signal alpha
  Pair     ::  Signal alpha -> Signal beta -> Signal (alpha,beta)
  PrimApp  ::  PrimSF alpha beta -> Signal alpha -> Signal beta

data PrimSF alpha beta where
  Der    ::  PrimSF Real Real
  Or     ::  PrimSF (Bool,Bool) Bool
  And    ::  PrimSF (Bool,Bool) Bool
  Not    ::  PrimSF Bool Bool
  Lt     ::  PrimSF Real Bool
  Lte    ::  PrimSF Real Bool
  Gt     ::  PrimSF Real Bool
  Gte    ::  PrimSF Real Bool
  Exp    ::  PrimSF Real Real
  Sqrt   ::  PrimSF Real Real
  Log    ::  PrimSF Real Real
  Sin    ::  PrimSF Real Real
  Tan    ::  PrimSF Real Real
  Cos    ::  PrimSF Real Real
  Asin   ::  PrimSF Real Real
  Atan   ::  PrimSF Real Real
  Acos   ::  PrimSF Real Real
  Sinh   ::  PrimSF Real Real
  Tanh   ::  PrimSF Real Real
  Cosh   ::  PrimSF Real Real
  Asinh  ::  PrimSF Real Real
  Atanh  ::  PrimSF Real Real
  Acosh  ::  PrimSF Real Real
  Abs    ::  PrimSF Real Real
  Sgn    ::  PrimSF Real Real
  Add    ::  PrimSF (Real,Real) Real
  Mul    ::  PrimSF (Real,Real) Real
  Div    ::  PrimSF (Real,Real) Real
  Pow    ::  PrimSF (Real,Real) Real
\end{code}
\caption{\label{figTypedRepresentation} Typed intermediate representation of Hydra.}
\end{figure}

\section{From Untyped to Typed Abstract Syntax}

The translation rules that transform a model in the untyped representation
into the corresponding model in the typed representation are given in Figure
\ref{figSigRelSigFunTrans} and Figure \ref{figSigTrans}. These rules translate
an untyped term into Haskell code that builds the corresponding typed term.
The pattern matching semantics in the left-hand side of the translation rules
are that of Haskell.

You may have noticed that there are no translation rules that generate the
|Switch| constructor. The functional-level combinator |switch|, which was
introduced in Chapter \ref{chapHydra}, generates the |Switch| constructor of
the typed abstract syntax. Specifically, the |switch| combinator is defined as
follows.

\begin{code}
switch :: SR a -> SF a Bool -> (a -> SR a) -> SR a
switch = Switch
\end{code}

The typed abstract syntax embodies Hydra's type system features that were only
informally introduced in earlier sections of the thesis. Let us outline
several key features. A type of a signal relation is determined by its
pattern. A type of a structurally-dynamic signal relation remains unchanged
despite the structural changes. Signal relation and signal function
applications must be well typed. This includes the application of the built-in
equality signal relation.

Note, however, that the type system says nothing about solvability of signal
relations. It is possible to define a type correct signal relation that does
not have a solution or has more than one solution. It is the modeller's
responsibility to define a signal relation that has an unique solution. Recent
work in the context of the FHM framework makes progress in the direction of
more expressive type systems incorporating the solvability aspect of noncausal
models \citep{Nilsson2008a,Capper2010a}. Incorporation of the aforementioned
work in Hydra is a subject of future research.

\begin{figure}
\begin{code}
translateSR (SigRel  pattern  equations)   =  SR  (\ translatePat (pattern)  ->  translateEqs (equations))
translateSF (SigFun  pattern  expression)  =  SF  (\ translatePat (pattern)  ->  translateExp (expression))
\end{code}

\begin{code}
translatePat (PatWild)                =  _
translatePat (PatName (Ident s))      =  translateHs (s)
translatePat (PatUnit)                =  Unit
translatePat (PatPair pat1 pat2)      =  Pair (translatePat pat1) (translatePat pat2)
\end{code}

\begin{code}
translateEqs ([])                                   =  []
translateEqs ((EquSigRelApp (HsExpr s) e) : eqs)    =  (App    (translateHs s)    (translateExp e))   :  translateEqs (eqs)
translateEqs ((EquEqual  e1 e2) : eqs)              =  (Equal  (translateExp e1)  (translateExp e2))  :  translateEqs (eqs)
translateEqs ((EquInit   e1 e2) : eqs)              =  (Init   (translateExp e1)  (translateExp e2))  :  translateEqs (eqs)
translateEqs ((EquLocal (Ident s)) : eqs)           =  [Local  (\ (translateHs s) -> (translateEqs eqs))]
\end{code}

\caption{\label{figSigRelSigFunTrans} Translation of untyped signal functions
and signal relations into typed signal functions and signal relations. The
translation rule |translateHs| takes a string in the concrete syntax of
Haskell and generates the corresponding Haskell code. The translation rules
|translateExp| and |translateIdent| are given in Figure \ref{figSigTrans}.}

\end{figure}

\begin{figure}
\begin{code}
translateExp (ExprAnti (HsExpr s1))             =  Const (translateHs (s1))
translateExp (ExprVar (Ident s))                =  translateIdent (s1)
translateExp (ExprAdd e1 e2)                    =  PrimApp Add (Pair (translateExp e1)  (translateExp e1))
translateExp (ExprSub e1 e2)                    =  PrimApp Sub (Pair (translateExp e1)  (translateExp e1))
translateExp (ExprDiv e1 e2)                    =  PrimApp Div (Pair (translateExp e1)  (translateExp e1))
translateExp (ExprMul e1 e2)                    =  PrimApp Mul (Pair (translateExp e1)  (translateExp e1))
translateExp (ExprPow e1 e2)                    =  PrimApp Pow (Pair (translateExp e1)  (translateExp e1))
translateExp (ExprNeg e1)                       =  PrimApp Neg (translateExp e1)
translateExp (ExprApp (ExprAnti (HsExpr s)) e)  =  (case translateHs (s) of SF f -> f) (translateExp e)
translateExp (ExprApp e1 e2)                    =  (translateExp e1) (translateExp e2)
translateExp (ExprInteger i1)                   =  Const (fromIntegral i1)
translateExp (ExprDouble d1)                    =  Const d1
translateExp (ExprUnit)                         =  Unit
translateExp (ExprPair e1 e2)                   =  Pair  (translateExp e1) (translateExp e2)
translateExp (ExprOr  e1 e2)                    =  PrimApp Or   (translateExp e1, translateExp e2)
translateExp (ExprAnd e1 e2)                    =  PrimApp And  (translateExp e1, translateExp e2)
translateExp (ExprLt  e1  e2)                   =  PrimApp Lt   ((translateExp e1)  -  (translateExp e2))
translateExp (ExprLte e1  e2)                   =  PrimApp Lte  ((translateExp e1)  -  (translateExp e2))
translateExp (ExprGt  e1  e2)                   =  PrimApp Gt   ((translateExp e1)  -  (translateExp e2))
translateExp (ExprGte e1  e2)                   =  PrimApp Gte  ((translateExp e1)  -  (translateExp e2))
\end{code}

\begin{code}
translateIdent (Ident "time")    =  Time
translateIdent (Ident "true")    =  Const True
translateIdent (Ident "false")   =  Const False
translateIdent (Ident "not")     =  PrimApp Not
translateIdent (Ident "der")     =  PrimApp Der
translateIdent (Ident "exp")     =  PrimApp Exp
translateIdent (Ident "sqrt")    =  PrimApp Sqrt
translateIdent (Ident "log")     =  PrimApp Log
translateIdent (Ident "sin")     =  PrimApp Sin
translateIdent (Ident "tan")     =  PrimApp Tan
translateIdent (Ident "cos")     =  PrimApp Cos
translateIdent (Ident "asin")    =  PrimApp Asin
translateIdent (Ident "atan")    =  PrimApp Atan
translateIdent (Ident "acos")    =  PrimApp Acos
translateIdent (Ident "sinh")    =  PrimApp Sinh
translateIdent (Ident "tanh")    =  PrimApp Tanh
translateIdent (Ident "cosh")    =  PrimApp Cosh
translateIdent (Ident "asinh")   =  PrimApp Asinh
translateIdent (Ident "atanh")   =  PrimApp Atanh
translateIdent (Ident "acosh")   =  PrimApp Acosh
translateIdent (Ident "abs")     =  PrimApp Abs
translateIdent (Ident "signum")  =  PrimApp Sgn
translateIdent (Ident s)         =  translateHs (s)
\end{code}
\caption{\label{figSigTrans} Translation of untyped signal expressions into typed signal expressions.}
\end{figure}

\section{Ideal Denotational Semantics}

A formal language definition has a number of advantages over an informal
presentation. A formal semantics does not leave room for ambiguity and allows
different implementers to implement the same language. In addition, a formally
defined semantics paves the way for proving useful statements about the
language.

There are a number different approaches to specification of formal semantics.
Two most widely used approaches are operational semantics \citep{Plotkin2004a}
and denotation semantics \citep{Scott1982a}. An operational semantics formally
defines an abstract machine and how the language terms are executed on the
machine. A denotational semantics formally defines translation of the language
terms into terms in a formalism that is well understood (often a filed of
mathematics).

One characteristic of noncausal, modelling languages setting them apart from
traditional programming languages is that concrete implementations of
noncausal languages only aim to approximate the model defined at the source
level. For example, consider the system of equations modelling the simple
electrical circuit given in Chapter \ref{chapBackground}. In the process of
deriving the simulation code we introduced a number of approximations. The
continuous real numbers were approximated using the double-precision machine
floating point numbers and the system of equations was approximated using the
Haskell code implementing the forward Euler method.

Implementations of noncausal, modelling languages allow modellers to choose
floating point representations (e.g., single or double precision), symbolic
processing methods and numerical simulation methods that needs to be used
during the simulation. This amounts to allowing modellers to choose the
combination model approximations prior to simulation.

The fact that the implementations are only expected to approximate noncausal
models needs to be taken into account when defining a formal semantics for a
noncausal language. In particular, definition of operational semantics is
problematic as it is hard to account for myriad of approximation combinations
that were outlined earlier. One option is to parameterise the operational
semantics on approximations. This is feasible, but leaves bulk of operational
details unspecified defeating the purpose of an operational semantics.

For the reasons outlined above, and because the concept of first-class models,
which allows for higher-order and structurally-dynamic modelling, is not
predicated on particular approximations used during simulation, we opted to
use \emph{ideal} denotational semantics for formally defining the Hydra
language. By referring to the semantics as ideal, we emphasise that concrete
implementations are only expected to approximate the denotational semantics.

The primary goal of the denotational semantics that is given in this section
is to precisely and concisely communicate Hydra's definition to modelling
language designers and implementers, in order to facilitate incorporation
Hydra's key features in other noncausal modelling languages.

Although not considered in this thesis, the ideal denotational semantics of
Hydra can also be used to verify concrete implementations of Hydra with
certain approximations. In addition, the denotational semantics can be used to
check whether concrete simulation results correspond to the source-level
noncausal model, again under certain approximation; for example, by using the
absolute error tolerance of the numerical simulation. These two applications
of the ideal denotational semantics are subject of future work.

The ideal denotational semantics of Hydra are given in Figure
\ref{figSigRelSigFunSem} and in Figure \ref{figSigSem}. Note that the domains
of the denotational semantics are the same as the conceptual definitions of
signals, signal functions, and signal relations given in Chapter
\ref{chapHydra}. Specifically, signal relations are mapped to functions from
starting time and signal to second-order logic proposition, signal functions
are mapped to functions from signal to signal, and signals are mapped to
function from time to value. Time is represented as a real number.

A signal relation denotation may involve existentially quantified function
symbols (i.e., signals). This is what makes the denotations second-order logic
propositions (i.e., not expressible in first-order logic). In other words,
solving of a signal relation can be understood as proving of \emph{existence}
of signals that satisfy the given constrains (see Figure
\ref{figSigRelSigFunSem} for details).

\begin{figure}
\begin{code}
semSR (SR f)            =   \t0 s -> semEqs ((0,t0,f s))
semSR (Switch sr sf f)  =   \t0 s -> {-" \forall \, t \in \mathbb{R} . \, "-} t >= t0 =>
    ((semSR sr) t0 s) && ((semSF sf) s t) == ((semSF sf) s t0)
    ||
    ({-" \exists \, t_{e} \in \mathbb{R} . \, "-}   (t < t_e   =>  ((semSR sr) t0 s)            &&  ((semSF sf) s t)    ==  ((semSF sf) s t0))
                                                    &&
                                                    (t >= t_e  =>  ((semSR (f (s t_e))) t_e s)  &&  ((semSF sf) s t_e)  /=  ((semSF sf) s t0)))

\end{code}

\begin{code}
semSF (SF sf)   =   sf
\end{code}

\begin{code}
semEqs  (_  ,  _   ,  []                     )  =   {-" \top "-}
semEqs  (i  ,  t0  ,  (Local f)      :  eqs  )  =   ({-" \exists \, s_{i} \in \mathbb{R} \rightarrow \mathbb{R} . \, "-} (semEqs (i + 1,t0,f s_i ++ eqs)))
semEqs  (i  ,  t0  ,  (Equal s1 s2)  :  eqs  )  =   ({-" \forall \, t \in \mathbb{R} . \, "-}  t >= t0  =>  (semSig s1) t  ==  (semSig s2) t)  &&  semEqs  (i,t0,eqs)
semEqs  (i  ,  t0  ,  (Init  s1 s2)  :  eqs  )  =   ({-" \forall \, t \in \mathbb{R} . \, "-}  t == t0  =>  (semSig s1) t  ==  (semSig s2) t)  &&  semEqs  (i,t0,eqs)
semEqs  (i  ,  t0  ,  (App   sr s)   :  eqs  )  =   ((semSR sr) t0 s)                         &&  semEqs  (i,t0,eqs)
\end{code}
\caption{\label{figSigRelSigFunSem} Denotations for signal relations, signal functions and equations.}
\end{figure}

\begin{figure}
\begin{code}
semSig (Unit)                       =   \_  ->  ()
semSig (Time)                       =   \t  ->  t
semSig (Const d)                    =   \_  ->  d
semSig (Pair s1 s2)                 =   \t  ->  ((semSig s1) t,(semSig s2) t)
semSig (PrimApp Der s)              =   \t  ->  {-" \displaystyle\lim_{\Delta t \to 0} \frac{ "-} (semSig s) {-" (t + \Delta t) - "-} (semSig s) {-" (t)}{\Delta t} "-}
semSig (PrimApp Exp s)              =   \t  ->  exp      ((semSig s)  t)
semSig (PrimApp Sqrt s)             =   \t  ->  sqrt     ((semSig s)  t)
semSig (PrimApp Log s)              =   \t  ->  log      ((semSig s)  t)
semSig (PrimApp Sin s)              =   \t  ->  sin      ((semSig s)  t)
semSig (PrimApp Tan s)              =   \t  ->  tan      ((semSig s)  t)
semSig (PrimApp Cos s)              =   \t  ->  cos      ((semSig s)  t)
semSig (PrimApp Asin s)             =   \t  ->  asin     ((semSig s)  t)
semSig (PrimApp Atan s)             =   \t  ->  atan     ((semSig s)  t)
semSig (PrimApp Acos s)             =   \t  ->  acos     ((semSig s)  t)
semSig (PrimApp Sinh s)             =   \t  ->  sinh     ((semSig s)  t)
semSig (PrimApp Tanh s)             =   \t  ->  tanh     ((semSig s)  t)
semSig (PrimApp Cosh s)             =   \t  ->  cosh     ((semSig s)  t)
semSig (PrimApp Asinh s)            =   \t  ->  asinh    ((semSig s)  t)
semSig (PrimApp Atanh s)            =   \t  ->  atanh    ((semSig s)  t)
semSig (PrimApp Acosh s)            =   \t  ->  acosh    ((semSig s)  t)
semSig (PrimApp Abs s)              =   \t  ->  abs      ((semSig s)  t)
semSig (PrimApp Sgn s)              =   \t  ->  signum   ((semSig s)  t)
semSig (PrimApp Add  (Pair s1 s2))  =   \t  ->  ((semSig s1)  t)  +             ((semSig s2) t)
semSig (PrimApp Mul  (Pair s1 s2))  =   \t  ->  ((semSig s1)  t)  *             ((semSig s2) t)
semSig (PrimApp Div  (Pair s1 s2))  =   \t  ->  ((semSig s1)  t)  /             ((semSig s2) t)
semSig (PrimApp Pow  (Pair s1 s2))  =   \t  ->  ((semSig s1)  t)  ^             ((semSig s2) t)
semSig (PrimApp Or   (Pair s1 s2))  =   \t  ->  ((semSig s1)  t)  ||            ((semSig s2) t)
semSig (PrimApp And  (Pair s1 s2))  =   \t  ->  ((semSig s1)  t)  &&            (semSig s2) t
semSig (PrimApp Not s1)             =   \t  ->  not ((semSig s1)  t)
semSig (PrimApp Lt s)               =   \t  ->  ((semSig s)   t)  <   0
semSig (PrimApp Lte s)              =   \t  ->  ((semSig s)   t)  <=  0
semSig (PrimApp Gt s)               =   \t  ->  ((semSig s)   t)  >   0
semSig (PrimApp Gte s)              =   \t  ->  ((semSig s)   t)  >=  0
\end{code}
\caption{\label{figSigSem} Denotations for signals}
\end{figure}