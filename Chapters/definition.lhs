\chapter{Definition of Hydra}
\label{chapDefinition}

This is a highly technical chapter giving a formal definition of the Hydra
language. Firstly, we define Hydra's lexical structure and concrete syntax
using the regular expression and BNF notations respectively. Secondly, we give
Hydra's abstract syntax as a Haskell algebraic data type. Thirdly, we define a
typed intermediate representation and give translation from the untyped
abstract syntax to the typed intermediate representation. The typed
intermediate representation fully embodies Hydra's type system. Finally, we
give ideal denotational semantics of Hydra by giving meaning to the typed
intermediate representation in terms of the first-order logic.

\section{Concrete Syntax}

\newcommand{\emptyP}{\mbox{$\epsilon$}}
\newcommand{\terminal}[1]{\mbox{{\texttt {#1}}}}
\newcommand{\nonterminal}[1]{\mbox{$\langle \mbox{{\sl #1 }} \! \rangle$}}
\newcommand{\arrow}{\mbox{::=}}
\newcommand{\delimit}{\mbox{$||$}}
\newcommand{\reserved}[1]{\mbox{{\texttt {#1}}}}
\newcommand{\literal}[1]{\mbox{{\texttt {#1}}}}
\newcommand{\symb}[1]{\mbox{{\texttt {#1}}}}

Identifiers \nonterminal{Ident} are unquoted strings beginning with a letter,
followed by any combination of letters, digits, and the characters {\tt \_ '},
reserved words excluded.

Integer literals \nonterminal{Integer}\ are nonempty sequences of digits.

Double-precision float literals \nonterminal{Double}\ have the structure
indicated by the regular expression $\nonterminal{digit}+ \mbox{{\it `.'}}
\nonterminal{digit}+ (\mbox{{\it `e'}} \mbox{{\it `-'}}?
\nonterminal{digit}+)?$, that is, two sequences of digits separated by a decimal
point, optionally followed by an unsigned or negative exponent.

HsExpr literals are recognized by the regular expression \(\mbox{`\$'}
({\nonterminal{anychar}} - \mbox{`\$'})* \mbox{`\$'}\)

The reserved words used in Hydra are the following:

\begin{tabular}{llll}
{\reserved{connect}} & {\reserved{flow}} & {\reserved{init}} & {\reserved{local}}
\end{tabular}

The symbols used in Hydra are the following:

\begin{tabular}{lll}
{\symb{{$-$}{$>$}}} &{\symb{\{}} &{\symb{\}}} \\
{\symb{\_}} &{\symb{()}} &{\symb{(}} \\
{\symb{,}} &{\symb{)}} &{\symb{{$=$}}} \\
{\symb{{$<$}{$>$}}} &{\symb{{$||$}{$||$}}} &{\symb{\&\&}} \\
{\symb{{$<$}}} &{\symb{{$<$}{$=$}}} &{\symb{{$>$}}} \\
{\symb{{$>$}{$=$}}} &{\symb{{$+$}}} &{\symb{{$-$}}} \\
{\symb{/}} &{\symb{*}} &{\symb{\^}} \\
{\symb{;}} & & \\
\end{tabular}\\

Single-line comments begin with {\symb{{$-$}{$-$}}}. Multiple-line comments
are enclosed with {\symb{\{{$-$}}} and {\symb{{$-$}\}}}.

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
 & {\delimit}  &{\nonterminal{Qualifier}} {\nonterminal{Ident}}  \\
 & {\delimit}  &{\terminal{()}}  \\
 & {\delimit}  &{\terminal{(}} {\nonterminal{Pattern}} {\terminal{,}} {\nonterminal{Pattern}} {\terminal{)}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Qualifier}} & {\arrow}  &{\emptyP} \\
 & {\delimit}  &{\terminal{flow}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Equation}} & {\arrow}  &{\nonterminal{Expr}} {\terminal{{$=$}}} {\nonterminal{Expr}}  \\
 & {\delimit}  &{\terminal{init}} {\nonterminal{Expr}} {\terminal{{$=$}}} {\nonterminal{Expr}}  \\
 & {\delimit}  &{\terminal{local}} {\nonterminal{Ident}} {\nonterminal{ListIdent}}  \\
 & {\delimit}  &{\terminal{connect}} {\nonterminal{Ident}} {\nonterminal{Ident}} {\nonterminal{ListIdent}}  \\
 & {\delimit}  &{\terminal{connect}} {\terminal{flow}} {\nonterminal{Ident}} {\nonterminal{Ident}} {\nonterminal{ListIdent}}  \\
 & {\delimit}  &{\nonterminal{HsExpr}} {\terminal{{$<$}{$>$}}} {\nonterminal{Expr}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Expr1}} & {\arrow}  &{\nonterminal{Expr1}} {\terminal{{$||$}{$||$}}} {\nonterminal{Expr2}}  \\
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
{\nonterminal{Expr8}} & {\arrow}  &{\nonterminal{Ident}}  \\
 & {\delimit}  &{\nonterminal{HsExpr}}  \\
 & {\delimit}  &{\nonterminal{Integer}}  \\
 & {\delimit}  &{\nonterminal{Double}}  \\
 & {\delimit}  &{\terminal{()}}  \\
 & {\delimit}  &{\terminal{(}} {\nonterminal{Expr}} {\terminal{,}} {\nonterminal{Expr}} {\terminal{)}}  \\
 & {\delimit}  &{\terminal{(}} {\nonterminal{Expr}} {\terminal{)}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{Expr}} & {\arrow}  &{\nonterminal{Expr1}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{ListEquation}} & {\arrow}  &{\emptyP} \\
 & {\delimit}  &{\nonterminal{Equation}}  \\
 & {\delimit}  &{\nonterminal{Equation}} {\terminal{;}} {\nonterminal{ListEquation}}  \\
\end{tabular}\\

\begin{tabular}{lll}
{\nonterminal{ListIdent}} & {\arrow}  &{\emptyP} \\
 & {\delimit}  &{\nonterminal{Ident}} {\nonterminal{ListIdent}}  \\
\end{tabular}\\

\section{Abstract Syntax}

\begin{code}
data Ident = Ident String

data HsExpr = HsExpr String

data SigRel = SigRel Pattern [Equation]

data SigFun = SigFun Pattern Expr

data Pattern =
   PatWild
 | PatName Qualifier Ident
 | PatUnit
 | PatPair Pattern Pattern

data Qualifier =
   QualEmpty
 | QualFlow

data Equation =
   EquEqual Expr Expr
 | EquInit Expr Expr
 | EquLocal Ident [Ident]
 | EquConnect Ident Ident [Ident]
 | EquConnectFlow Ident Ident [Ident]
 | EquSigRelApp HsExpr Expr

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
 | ExprVar Ident
 | ExprAnti HsExpr
 | ExprInteger Integer
 | ExprDouble Double
 | ExprUnit
 | ExprPair Expr Expr
\end{code}

\section{Desugaring}

Before we turn our attention to the translation of the untyped abstract syntax
into the typed intermediate representation, we describe a desugaring
translation of Hydra. The goal of this translation is to represent a Hydra
model with fewer constructors. This in turn allows for a simpler typed
intermediated representation as we show in the following section. The
translation is given as a Haskell function working with the abstract syntax of
Hydra.

We break down the translation into four simple desugaring stages:
\begin{code}
desugar  ::  SigRel -> SigRel
desugar  =   desugarFlowSigRel . desugarConnectSigRel . desugarPairSigRel . desugarLocalSigRel
\end{code}

In the first stage, we desugar all local signal variable definitions that bind
multiple variables into the definitions that only bind a single variable.

\begin{code}
desugarLocalSigRel                   ::  SigRel -> SigRel
desugarLocalSigRel (SigRel pat eqs)  =   SigRel pat [ desugarLocalEquation eq | eq <- eqs ]
\end{code}

\begin{code}
desugarLocalEquation                             ::  Equation -> [Equation]
desugarLocalEquation (EquLocal li1 (li2 : lis))  =   (EquLocal li1 []) : desugarLocalEquation (EquLocal li2 lis)
desugarLocalEquation (eq)                        =   [eq]
\end{code}

In the second stage, we desugar all equations that assert equality of signal
pairs into equations asserting equality of scalar signals.

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

In the third stage, we desugar |connect| and |connect flow| equations into the
equality constrains and sum to zero equations respectively.

\begin{code}
desugarConnectSigRel                     ::  SigRel -> SigRel
desugarConnectSigRel (SigRel pat1 eqs1)  =   SigRel pat1 (concat [ desugarConnectEquation eq | eq <- eqs1 ])
\end{code}

\begin{code}
desugarConnectEquation                               ::  Equation -> [Equation]
desugarConnectEquation (EquConnect li1 li2 lis)      =   let   vs    =  [ ExprVar li | li <- (li1 : li2 : lis) ]
                                                         in    zipWith EquEqual vs (tail vs)
desugarConnectEquation (EquConnectFlow li1 li2 lis)  =   let   vs    =  [ ExprVar li | li <- (li1 : li2 : lis) ]
                                                         in    [EquEqual  (foldr1 ExprAdd vs) (ExprDouble 0)]
desugarConnectEquation (eq)                          =   [eq]
\end{code}

In the fourth stage, we desugar |flow| signal variable declarations by
negating every occurrence of such variables.

\begin{code}
desugarFlowSigRel                     ::  SigRel -> SigRel
desugarFlowSigRel (SigRel pat1 eqs1)  =   let  flowVars = desugarFlowFindPattern pat1
                                               pat2 = desugarFlowForgetPattern pat1
                                               eqs2 = foldr (\s eqs -> desugarFlowEquations (s,eqs)) eqs1 flowVars
                                          in   SigRel pat2 eqs2
\end{code}

\begin{code}
desugarFlowFindPattern                                 ::  Pattern -> [String]
desugarFlowFindPattern (PatUnit)                       =   []
desugarFlowFindPattern (PatWild)                       =   []
desugarFlowFindPattern (PatName QualEmpty _)           =   []
desugarFlowFindPattern (PatName QualFlow (LIdent s1))  =   [s1]
desugarFlowFindPattern (PatPair p1 p2)                 =   desugarFlowFindPattern p1 ++ desugarFlowFindPattern p2
\end{code}

\begin{code}
desugarFlowForgetPattern                                ::  Pattern -> Pattern
desugarFlowForgetPattern (pat@(PatUnit))                =   pat
desugarFlowForgetPattern (pat@(PatWild))                =   pat
desugarFlowForgetPattern (pat@(PatName QualEmpty li1))  =   pat
desugarFlowForgetPattern (pat@(PatName QualFlow  li1))  =   pat
desugarFlowForgetPattern (pat@(PatTuple pats))          =   PatTuple [ desugarFlowForgetPattern p | p <- pats ]
\end{code}

\begin{code}
desugarFlowEquations                                             ::  (String,[Equation]) -> [Equation]
desugarFlowEquations (_  ,  [])                                  =   []
desugarFlowEquations (s  ,  (EquSigRelApp hs1 e1) : eqs)         =   (EquSigRelApp hs1 (desugarFlowExpr (s,e1)))                   :  desugarFlowEquations (s,eqs)
desugarFlowEquations (s  ,  (EquEqual e1 e2) : eqs)              =   (EquEqual (desugarFlowExpr (s,e1)) (desugarFlowExpr (s,e2)))  :  desugarFlowEquations (s,eqs)
desugarFlowEquations (s  ,  (EquInit e1 e2) : eqs)               =   (EquInit (desugarFlowExpr (s,e1)) (desugarFlowExpr (s,e2)))   :  desugarFlowEquations (s,eqs)
desugarFlowEquations (s  ,  eq@(EquLocal (LIdent s1) []) : eqs)  =   if s1 == s then(eq : eqs) else eq : desugarFlowEquations (s,eqs)
\end{code}

\begin{code}
desugarFlowExpr                                ::  (String,Expr) -> Expr
desugarFlowExpr (s,e@(ExprVar (LIdent s1)))    =   if s1 == s then ExprNeg e else e
desugarFlowExpr (s,ExprAdd  e1 e2)             =   ExprAdd     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExprSub  e1 e2)             =   ExprSub     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExprDiv  e1 e2)             =   ExprDiv     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExprMul  e1 e2)             =   ExprMul     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExprPow  e1 e2)             =   ExprPow     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExprOr   e1 e2)             =   ExprOr      (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExprAnd  e1 e2)             =   ExprAnd     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExprLt   e1 e2)             =   ExprLt      (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExprLte  e1 e2)             =   ExprLte     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExprGt   e1 e2)             =   ExprGt      (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExprGte  e1 e2)             =   ExprGte     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExprApp  e1 e2)             =   ExprApp     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExprPair e1 e2)             =   ExprPair    (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExprNeg  e1)                =   ExprNeg     (desugarFlowExpr (s,e1))
desugarFlowExpr (_,e)                         =   e
\end{code}

\section{Typed Intermediate Representation}

The following typed representation of Hydra models embodies the type system of
Hydra.

\begin{code}
data SR a where
  SR      ::  (Signal a -> [Equation]) -> SR a
  Switch  ::  SR a -> SF a Bool -> (a -> SR a) -> SR a

data SF a b where
  SF :: (Signal a -> Signal b) -> SF a b

data Equation where
  Local :: (Signal Double -> [Equation]) -> Equation
  Equal :: Signal Double -> Signal Double -> Equation
  Init  :: Signal Double -> Signal Double -> Equation
  App   :: SR a -> Signal a -> Equation

data Signal a where
  Unit     ::  Signal ()
  Time     ::  Signal Double
  Const    ::  a -> Signal a
  Pair     ::  Signal a -> Signal b -> Signal (a,b)
  PrimApp  ::  PrimSF a b -> Signal a -> Signal b

data PrimSF a b where
  Der    ::  PrimSF Double Double
  Or     ::  PrimSF (Bool,Bool) Bool
  And    ::  PrimSF (Bool,Bool) Bool
  Not    ::  PrimSF Bool Bool
  Lt     ::  PrimSF Double Bool
  Lte    ::  PrimSF Double Bool
  Gt     ::  PrimSF Double Bool
  Gte    ::  PrimSF Double Bool
  Exp    ::  PrimSF Double Double
  Sqrt   ::  PrimSF Double Double
  Log    ::  PrimSF Double Double
  Sin    ::  PrimSF Double Double
  Tan    ::  PrimSF Double Double
  Cos    ::  PrimSF Double Double
  Asin   ::  PrimSF Double Double
  Atan   ::  PrimSF Double Double
  Acos   ::  PrimSF Double Double
  Sinh   ::  PrimSF Double Double
  Tanh   ::  PrimSF Double Double
  Cosh   ::  PrimSF Double Double
  Asinh  ::  PrimSF Double Double
  Atanh  ::  PrimSF Double Double
  Acosh  ::  PrimSF Double Double
  Abs    ::  PrimSF Double Double
  Sgn    ::  PrimSF Double Double
  Add    ::  PrimSF (Double,Double) Double
  Mul    ::  PrimSF (Double,Double) Double
  Div    ::  PrimSF (Double,Double) Double
  Pow    ::  PrimSF (Double,Double) Double
\end{code}

\begin{code}
instance Num (Signal Double) where
  (+) e1 e2     = PrimApp Add (Pair e1 e2)
  (*) e1 e2     = PrimApp Mul (Pair e1 e2)
  (-) e1 e2     = PrimApp Add (Pair e1 ((Const (-1)) * e2))
  negate e1     = (Const (-1)) * e1
  abs e1        = PrimApp Abs e1
  signum e1     = PrimApp Sgn e1
  fromInteger i = Const (fromIntegral i)

instance Fractional (Signal Double) where
  (/) e1 e2 = PrimApp Div (Pair e1 e2)
  recip e1 = 1 / e1
  fromRational r = Const (fromRational r)

instance Floating (Signal Double) where
  pi          = Const pi
  exp   e1    = PrimApp Exp   e1
  log   e1    = PrimApp Log   e1
  sqrt  e1    = PrimApp Sqrt  e1
  sin   e1    = PrimApp Sin   e1
  cos   e1    = PrimApp Cos   e1
  tan   e1    = PrimApp Tan   e1
  asin  e1    = PrimApp Asin  e1
  acos  e1    = PrimApp Acos  e1
  atan  e1    = PrimApp Atan  e1
  sinh  e1    = PrimApp Sinh  e1
  cosh  e1    = PrimApp Cosh  e1
  tanh  e1    = PrimApp Tanh  e1
  asinh e1    = PrimApp Asinh e1
  acosh e1    = PrimApp Acosh e1
  atanh e1    = PrimApp Atanh e1
  (**) e1 e2  = PrimApp Pow   (Pair e1 e2)
\end{code}

\section{From Untyped to Typed Representation}

In this section we present translation rules transforming a model in the
untyped representation into the corresponding model in the typed
representation.

\begin{code}
translateSR (SigRel  pattern  equations)   =  SR  (\ translatePat (pattern)  ->  translateEqs (equations))
translateSF (SigFun  pattern  expression)  =  SF  (\ translatePat (pattern)  ->  translateExp (expression))
\end{code}

\begin{code}
translatePat (PatWild)                =  _
translatePat (PatName _ (LIdent s1))  =  translateHs (s1)
translatePat (PatUnit)                =  Unit
translatePat (PatPair pat1 pat2)      =  Pair (translatePat pat1) (translatePat pat2)
\end{code}

\begin{code}
translateEqs ([])                                   =  []
translateEqs ((EquSigRelApp (HsExpr s1) e1) : eqs)  =  (App    (translateHs s1)   (translateExp e1))  :  translateEqs (eqs)
translateEqs ((EquEqual  e1 e2) : eqs)              =  (Equal  (translateExp e1)  (translateExp e2))  :  translateEqs (eqs)
translateEqs ((EquInit   e1 e2) : eqs)              =  (Init   (translateExp e1)  (translateExp e2))  :  translateEqs (eqs)
translateEqs ((EquLocal (LIdent s1) _) : eqs)       =  [Local  (\ (translateHs s1) -> (translateEqs eqs))]
\end{code}

\begin{code}
translateExp (ExprAnti (HsExpr s1))      =  translateHs (s1)
translateExp (ExprVar (LIdent "time"))   =  Time
translateExp (ExprVar (LIdent "true"))   =  Const True
translateExp (ExprVar (LIdent "false"))  =  Const False
translateExp (ExprVar (LIdent "not"))    =  PrimApp Not
translateExp (ExprVar (LIdent s1))       =  translateHs (s1)
translateExp (ExprAdd e1 e2)             =  (translateExp e1)  +   (translateExp e2)
translateExp (ExprSub e1 e2)             =  (translateExp e1)  -   (translateExp e2)
translateExp (ExprDiv e1 e2)             =  (translateExp e1)  /   (translateExp e2)
translateExp (ExprMul e1 e2)             =  (translateExp e1)  *   (translateExp e2)
translateExp (ExprPow e1 e2)             =  (translateExp e1)  **  (translateExp e2)
translateExp (ExprNeg e1)                =  negate (translateExp e1)
translateExp (ExprApp e1 e2)             =  (translateExp e1) (translateExp e2)
translateExp (ExprInteger i1)            =  Const (fromIntegral i1)
translateExp (ExprDouble d1)             =  Const d1
translateExp (ExprUnit [])               =  Unit
translateExp (ExprPair e1 e2)            =  Pair  (translateExp e1) (translateExp e2)
translateExp (ExprOr  e1 e2)             =  PrimApp Or   (translateExp e1, translateExp e2)
translateExp (ExprAnd e1 e2)             =  PrimApp And  (translateExp e1, translateExp e2)
translateExp (ExprLt  e1  e2)            =  PrimApp Lt   ((translateExp e1)  -  (translateExp e2))
translateExp (ExprLte e1  e2)            =  PrimApp Lte  ((translateExp e1)  -  (translateExp e2))
translateExp (ExprGt  e1  e2)            =  PrimApp Gt   ((translateExp e1)  -  (translateExp e2))
translateExp (ExprGte e1  e2)            =  PrimApp Gte  ((translateExp e1)  -  (translateExp e2))
\end{code}

\section{Ideal Semantics of Hydra}

Note that the domains of the following denotational semantics of Hydra are the
same as the conceptual definitions of signals, signal functions, and signal
relations given in Chapter \ref{chapHydra}.

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