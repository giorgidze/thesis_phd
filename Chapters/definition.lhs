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

\section{Desugaring}

Before we turn our attention to the typed intermediate representation of Hydra models, we describe the desugaring rules of Hydra. The rules are given as Haskell functions that work with the abstract syntax of Hydra (i.e., the |SigRel| data type). 

We break down the rules intro four simple desugaring stages:
\begin{code}
desugar :: SigRel -> SigRel
desugar =
     desugarFlowSigRel
  .  desugarConnectSigRel
  .  desugarTupleSigRel
  .  desugarLocalSigRel
\end{code}

In the first stage, we desugar all local signal variable definitions that bind multiple variables into the definitions that only bind a single variable.

\begin{code}
desugarLocalSigRel :: SigRel -> SigRel
desugarLocalSigRel sr = case sr of
  SigRel pat1 eqs1 -> SigRel pat1 (concatMap desugarLocalEquation eqs1)

desugarLocalEquation :: Equation -> [Equation]
desugarLocalEquation eq = case eq of
  EquationLocal li1 (li2 : lis) ->
    (EquationLocal li1 []) : desugarLocalEquation (EquationLocal li2 lis)
  _ -> [eq]
\end{code}

In the second stage, desugar all equations that assert equality of tuple of signals into a number of equations asserting equality of scalar signals that are carried by the tuple signals.

\begin{code}
desugarTupleSigRel :: SigRel -> SigRel
desugarTupleSigRel sr = case sr of
  SigRel pat1 eqs1 -> SigRel pat1 (concatMap desugarTupleEquation eqs1)

desugarTupleEquation :: Equation -> [Equation]
desugarTupleEquation eq = case eq of
  EquationEqual (ExprTuple es1) (ExprTuple es2) ->
    if length es1 == length es2
      then  concatMap desugarTupleEquation (zipWith EquationEqual es1 es2)
      else  undefined
  EquationInit (ExprTuple es1) (ExprTuple es2) ->
    if length es1 == length es2
      then  concatMap desugarTupleEquation (zipWith EquationInit es1 es2)
      else  undefined
  _ -> [eq]
\end{code}

In the third stage, we desugar |connect| and |connect flow| equations into the equality constrains and sum to zero equations respectively.

\begin{code}
desugarConnectSigRel :: SigRel -> SigRel
desugarConnectSigRel sr = case sr of
  SigRel pat1 eqs1 -> SigRel pat1 (concatMap desugarConnectEquation eqs1)

desugarConnectEquation :: Equation -> [Equation]
desugarConnectEquation eq = case eq of
  EquationConnect li1 li2 lis ->
    let vs = map ExprVar (li1 : li2 : lis)
    in  zipWith EquationEqual vs (tail vs)
  EquationConnectFlow li1 li2 lis ->
    let vs = map ExprVar (li1 : li2 : lis)
    in  [EquationEqual  (foldr1 ExprAdd vs) (ExprReal 0.0)]
  _ -> [eq]
\end{code}

In the fourth stage, we desugar |flow| signal variable declarations by negating every occurrence of such variables.

\begin{code}
desugarFlowSigRel :: SigRel -> SigRel
desugarFlowSigRel (SigRel pat1 eqs1) =
  let flowVars = desugarFlowFindPattern pat1
      pat2 = desugarFlowForgetPattern pat1
      eqs2 = foldr (\s eqs -> desugarFlowEquations s eqs) eqs1 flowVars
  in  SigRel pat2 eqs2

desugarFlowFindPattern :: Pattern -> [String]
desugarFlowFindPattern pat = case pat of
  PatternWild -> []
  PatternName PatternNameQualEmpty _ -> []
  PatternName PatternNameQualFlow  (LIdent s1) -> [s1]
  PatternTuple pats1 -> concatMap desugarFlowFindPattern pats1

desugarFlowForgetPattern :: Pattern -> Pattern
desugarFlowForgetPattern pat = case pat of
  PatternWild -> pat
  PatternName PatternNameQualEmpty _ -> pat
  PatternName PatternNameQualFlow  li1 -> PatternName PatternNameQualEmpty li1
  PatternTuple pats1 -> PatternTuple (map desugarFlowForgetPattern pats1)

desugarFlowEquations :: String -> [Equation] -> [Equation]
desugarFlowEquations _ [] = []
desugarFlowEquations s (eq : eqs) =
  let go :: Expr -> Expr
      go = desugarFlowExpr s
  in  case eq of
        EquationSigRelApp hsExpr1 e1 ->
          (EquationSigRelApp hsExpr1 (go e1)) : desugarFlowEquations s eqs
        EquationEqual e1 e2          ->
          (EquationEqual (go e1) (go e2))     : desugarFlowEquations s eqs
        EquationInit e1 e2           ->
          (EquationInit (go e1) (go e2))      : desugarFlowEquations s eqs
        EquationReinit e1 e2         ->
          (EquationReinit (go e1) (go e2))    : desugarFlowEquations s eqs
        EquationLocal (LIdent s1) [] ->
          if s1 == s
             then (eq : eqs)
             else  eq : desugarFlowEquations s eqs

desugarFlowExpr :: String -> Expr -> Expr
desugarFlowExpr s expr = go expr
  where
  go :: Expr -> Expr
  go e = case e of
    ExprVar (LIdent s1) -> if s1 == s then ExprNeg e else e

    ExprAdd e1 e2 -> ExprAdd (go e1) (go e2)
    ExprSub e1 e2 -> ExprSub (go e1) (go e2)
    ExprDiv e1 e2 -> ExprDiv (go e1) (go e2)
    ExprMul e1 e2 -> ExprMul (go e1) (go e2)
    ExprPow e1 e2 -> ExprPow (go e1) (go e2)
    ExprOr  e1 e2 -> ExprOr  (go e1) (go e2)
    ExprAnd e1 e2 -> ExprAnd (go e1) (go e2)
    ExprLt  e1 e2 -> ExprLt  (go e1) (go e2)
    ExprLte e1 e2 -> ExprLte (go e1) (go e2)
    ExprGt  e1 e2 -> ExprGt  (go e1) (go e2)
    ExprGte e1 e2 -> ExprGte (go e1) (go e2)
    ExprApp v1 e1 -> ExprApp v1 (go e1)
    ExprNeg e1    -> ExprNeg (go e1)

    ExprTuple es1 -> ExprTuple (map go es1)

    _ -> e
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
