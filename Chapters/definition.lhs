\chapter{Definition of Hydra}
\label{chapDefinition}

This is highly technical chapter giving a formal definition of the Hydra language. Firstly, we define Hydra's concrete syntax using the regular expression and BNF notations. Secondly, we give Hydra's abstract syntax derived from the concrete syntax as a Haskell algebraic data. Thirdly, we define a typed intermediate representation for the abstract syntax and give translation from the untyped abstract syntax to the typed intermediate representation. The typed intermediated representation also fully embodies Hydra's type system. Finally, we give ideal denotational semantics by giving meaning to the typed intermediated representation using the first-order logic.


\section{Concrete Syntax}

\newcommand{\emptyP}{\mbox{$\epsilon$}}
\newcommand{\terminal}[1]{\mbox{{\texttt {#1}}}}
\newcommand{\nonterminal}[1]{\mbox{$\langle \mbox{{\sl #1 }} \! \rangle$}}
\newcommand{\arrow}{\mbox{::=}}
\newcommand{\delimit}{\mbox{$||$}}
\newcommand{\reserved}[1]{\mbox{{\texttt {#1}}}}
\newcommand{\literal}[1]{\mbox{{\texttt {#1}}}}
\newcommand{\symb}[1]{\mbox{{\texttt {#1}}}}

The reserved words used in Hydra are the following: \\

\begin{tabular}{llll}
{\reserved{connect}} &{\reserved{flow}} &{\reserved{init}} & {\reserved{local}}\\
\end{tabular}\\

The symbols used in Hydra are the following: \\

\begin{tabular}{lll}
{\symb{{$-$}{$>$}}} &{\symb{\{}} &{\symb{\}}} \\
{\symb{\_}} &{\symb{(}} &{\symb{)}} \\
{\symb{,}} &{\symb{{$=$}}} &{\symb{{$<$}{$>$}}} \\
{\symb{;}} &{\symb{{$||$}{$||$}}} &{\symb{\&\&}} \\
{\symb{{$<$}}} &{\symb{{$<$}{$=$}}} &{\symb{{$>$}}} \\
{\symb{{$>$}{$=$}}} &{\symb{{$+$}}} &{\symb{{$-$}}} \\
{\symb{/}} &{\symb{*}} &{\symb{\^}} \\
\end{tabular}\\

Single-line comments begin with {\symb{{$-$}{$-$}}}.

Multiple-line comments are  enclosed with {\symb{\{{$-$}}} and {\symb{{$-$}\}}}.

Integer literals \nonterminal{Int}\ are nonempty sequences of digits.

Double-precision float literals \nonterminal{Double}\ have the structure indicated by the regular expression $\nonterminal{digit}+ \mbox{{\it `.'}} \nonterminal{digit}+ (\mbox{{\it `e'}} \mbox{{\it `-'}}? \nonterminal{digit}+)?$, that is, two sequences of digits separated by a decimal point, optionally followed by an unsigned or negative exponent.


LIdent literals are recognized by the regular expression
\(({\nonterminal{lower}} \mid \mbox{`\_'}) ({\nonterminal{letter}} \mid {\nonterminal{digit}} \mid \mbox{`\_'})*\)

HsExpr literals are recognized by the regular expression
\(\mbox{`\$'} ({\nonterminal{anychar}} - \mbox{`\$'})* \mbox{`\$'}\)


Non-terminals are enclosed between $\langle$ and $\rangle$. 
The symbols  {\arrow}  (production),  {\delimit}  (union) 
and {\emptyP} (empty rule) belong to the BNF notation. 
All other symbols are terminals.\\

\begin{tabular}{lll}
{\nonterminal{SigRel}} & {\arrow}  &{\nonterminal{Pattern}} {\terminal{{$-$}{$>$}}} {\terminal{\{}} {\nonterminal{ListEquation}} {\terminal{\}}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{SigFun}} & {\arrow}  &{\nonterminal{Pattern}} {\terminal{{$-$}{$>$}}} {\terminal{\{}} {\nonterminal{Expr}} {\terminal{\}}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Pattern}} & {\arrow}  &{\terminal{\_}}  \\
 & {\delimit}  &{\nonterminal{PatternNameQual}} {\nonterminal{LIdent}}  \\
 & {\delimit}  &{\terminal{(}} {\nonterminal{ListPattern}} {\terminal{)}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{ListPattern}} & {\arrow}  &{\emptyP} \\
 & {\delimit}  &{\nonterminal{Pattern}}  \\
 & {\delimit}  &{\nonterminal{Pattern}} {\terminal{,}} {\nonterminal{ListPattern}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{PatternNameQual}} & {\arrow}  &{\emptyP} \\
 & {\delimit}  &{\terminal{flow}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Equation}} & {\arrow}  &{\nonterminal{Expr}} {\terminal{{$=$}}} {\nonterminal{Expr}}  \\
 & {\delimit}  &{\terminal{init}} {\nonterminal{Expr}} {\terminal{{$=$}}} {\nonterminal{Expr}}  \\
 & {\delimit}  &{\terminal{local}} {\nonterminal{LIdent}} {\nonterminal{ListLIdent}}  \\
 & {\delimit}  &{\terminal{connect}} {\nonterminal{LIdent}} {\nonterminal{LIdent}} {\nonterminal{ListLIdent}}  \\
 & {\delimit}  &{\terminal{connect}} {\terminal{flow}} {\nonterminal{LIdent}} {\nonterminal{LIdent}} {\nonterminal{ListLIdent}}  \\
 & {\delimit}  &{\nonterminal{HsExpr}} {\terminal{{$<$}{$>$}}} {\nonterminal{Expr}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{ListEquation}} & {\arrow}  &{\emptyP} \\
 & {\delimit}  &{\nonterminal{Equation}}  \\
 & {\delimit}  &{\nonterminal{Equation}} {\terminal{;}} {\nonterminal{ListEquation}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{ListLIdent}} & {\arrow}  &{\emptyP} \\
 & {\delimit}  &{\nonterminal{LIdent}} {\nonterminal{ListLIdent}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Expr1}} & {\arrow}  &{\nonterminal{Expr1}} {\terminal{{$|$}{$|$}}} {\nonterminal{Expr2}}  \\
 & {\delimit}  &{\nonterminal{Expr2}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Expr2}} & {\arrow}  &{\nonterminal{Expr2}} {\terminal{\&\&}} {\nonterminal{Expr3}}  \\
 & {\delimit}  &{\nonterminal{Expr3}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Expr3}} & {\arrow}  &{\nonterminal{Expr4}} {\terminal{{$<$}}} {\nonterminal{Expr4}}  \\
 & {\delimit}  &{\nonterminal{Expr4}} {\terminal{{$<$}{$=$}}} {\nonterminal{Expr4}}  \\
 & {\delimit}  &{\nonterminal{Expr4}} {\terminal{{$>$}}} {\nonterminal{Expr4}}  \\
 & {\delimit}  &{\nonterminal{Expr4}} {\terminal{{$>$}{$=$}}} {\nonterminal{Expr4}}  \\
 & {\delimit}  &{\nonterminal{Expr4}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Expr4}} & {\arrow}  &{\nonterminal{Expr4}} {\terminal{{$+$}}} {\nonterminal{Expr5}}  \\
 & {\delimit}  &{\nonterminal{Expr4}} {\terminal{{$-$}}} {\nonterminal{Expr5}}  \\
 & {\delimit}  &{\nonterminal{Expr5}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Expr5}} & {\arrow}  &{\nonterminal{Expr5}} {\terminal{/}} {\nonterminal{Expr6}}  \\
 & {\delimit}  &{\nonterminal{Expr5}} {\terminal{*}} {\nonterminal{Expr6}}  \\
 & {\delimit}  &{\nonterminal{Expr6}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Expr6}} & {\arrow}  &{\nonterminal{Expr6}} {\terminal{\^}} {\nonterminal{Expr7}}  \\
 & {\delimit}  &{\terminal{{$-$}}} {\nonterminal{Expr7}}  \\
 & {\delimit}  &{\nonterminal{Expr7}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Expr7}} & {\arrow}  &{\nonterminal{Expr7}} {\nonterminal{Expr8}}  \\
 & {\delimit}  &{\nonterminal{Expr8}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Expr8}} & {\arrow}  &{\nonterminal{LIdent}}  \\
 & {\delimit}  &{\nonterminal{HsExpr}}  \\
 & {\delimit}  &{\nonterminal{Integer}}  \\
 & {\delimit}  &{\nonterminal{Double}}  \\
 & {\delimit}  &{\terminal{(}} {\nonterminal{ListExpr}} {\terminal{)}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Expr}} & {\arrow}  &{\nonterminal{Expr1}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{ListExpr}} & {\arrow}  &{\emptyP} \\
 & {\delimit}  &{\nonterminal{Expr}}  \\
 & {\delimit}  &{\nonterminal{Expr}} {\terminal{,}} {\nonterminal{ListExpr}}  \\
\end{tabular}\\

\section{Abstract Syntax}

\begin{code}
data LIdent = LIdent String
data HsExpr = HsExpr String

data SigRel = SigRel Pattern [Equation]

data SigFun = SigFun Pattern Expr

data Pattern =
   PatternWild
 | PatternName PatternNameQual LIdent
 | PatternTuple [Pattern]

data PatternNameQual =
   PatternNameQualEmpty
 | PatternNameQualFlow

data Equation =
   EquationEqual Expr Expr
 | EquationInit Expr Expr
 | EquationLocal LIdent [LIdent]
 | EquationConnect LIdent LIdent [LIdent]
 | EquationConnectFlow LIdent LIdent [LIdent]
 | EquationSigRelApp HsExpr Expr

data Expr =
   ExprOr Expr Expr
 | ExprAnd Expr Expr
 | ExprLt Expr Expr
 | ExprLte Expr Expr
 | ExprGt Expr Expr
 | ExprGte Expr Expr
 | ExprAdd Expr Expr
 | ExprSub Expr Expr
 | ExprDiv Expr Expr
 | ExprMul Expr Expr
 | ExprPow Expr Expr
 | ExprNeg Expr
 | ExprApp Expr Expr
 | ExprVar LIdent
 | ExprAnti HsExpr
 | ExprInt Integer
 | ExprReal Double
 | ExprTuple [Expr]
\end{code}

\section{Typed Intermediate Representation}

\begin{code}
data SR a =
    SigRel (Signal a -> [Equation])
  | Switch (SR a) (SF a Bool) (a -> SR a)

data SF a b = SigFun !(Signal a -> Signal b)

data Equation =
    Local   (Signal Double -> [Equation])
  | Equal   (Signal Double) (Signal Double)
  | Init    (Signal Double) (Signal Double)
  | Reinit  (Signal Double) (Signal Double)
  | forall a. (SignalType a) => App (SR a) (Signal a)

class SignalType a where
  data Signal a

instance SignalType () where
  data Signal () = Unit

instance SignalType Double where
  data Signal Double = Time
                     | Const Double
                     | Var   Int
                     | Der   (Signal Double)
                     | Cur   (Signal Double)
                     | App1  Func1 (Signal Double)
                     | App2  Func2 (Signal Double) (Signal Double)


instance SignalType Bool where
  data Signal Bool = Or    (Signal Bool) (Signal Bool)
                   | And   (Signal Bool) (Signal Bool)
                   | Xor   (Signal Bool) (Signal Bool)
                   | Comp  CompFun (Signal Double)


data Func1 =
    Exp
  | Sqrt
  | Log
  | Sin
  | Tan
  | Cos
  | Asin
  | Atan
  | Acos
  | Sinh
  | Tanh
  | Cosh
  | Asinh
  | Atanh
  | Acosh
  | Abs
  | Sgn

data Func2 =
    Add
  | Mul
  | Div
  | Pow

data CompFun = Lt | Lte | Gt | Gte  
\end{code}

\section{Ideal Semantics of Hydra}
