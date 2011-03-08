%{
%include ../Format/hydra.lhs

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
{\nonterminal{Expr1}} & {\arrow}  &{\nonterminal{Expr1}} {\terminal{$||||$}} {\nonterminal{Expr2}}  \\
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
   PatWild
 | PatName PatternNameQual LIdent
 | PatTuple [Pattern]

data PatternNameQual =
   PatNameQualEmpty
 | PatNameQualFlow

data Equation =
   EquEqual Expr Expr
 | EquInit Expr Expr
 | EquLocal LIdent [LIdent]
 | EquConnect LIdent LIdent [LIdent]
 | EquConnectFlow LIdent LIdent [LIdent]
 | EquSigRelApp HsExpr Expr

data Expr =
   ExpOr Expr Expr
 | ExpAnd Expr Expr
 | ExpLt Expr Expr
 | ExpLte Expr Expr
 | ExpGt Expr Expr
 | ExpGte Expr Expr
 | ExpAdd Expr Expr
 | ExpSub Expr Expr
 | ExpDiv Expr Expr
 | ExpMul Expr Expr
 | ExpPow Expr Expr
 | ExpNeg Expr
 | ExpApp Expr Expr
 | ExpVar LIdent
 | ExpAnti HsExpr
 | ExpInt Integer
 | ExpReal Double
 | ExpTuple [Expr]
\end{code}

\section{Desugaring}

Before we turn our attention to the typed intermediate representation of Hydra models, we describe the desugaring rules of Hydra. The rules are given as Haskell functions that work with the abstract syntax of Hydra (i.e., the |SigRel| data type).

We break down the rules intro four simple desugaring stages:
\begin{code}
desugar  ::  SigRel -> SigRel
desugar  =   desugarFlowSigRel . desugarConnectSigRel . desugarTupleSigRel . desugarLocalSigRel
\end{code}

In the first stage, we desugar all local signal variable definitions that bind multiple variables into the definitions that only bind a single variable.

\begin{code}
desugarLocalSigRel                   ::  SigRel -> SigRel
desugarLocalSigRel (SigRel pat eqs)  =   SigRel pat [ desugarLocalEquation eq | eq <- eqs ]
\end{code}

\begin{code}
desugarLocalEquation                             ::  Equation -> [Equation]
desugarLocalEquation (EquLocal li1 (li2 : lis))  =   (EquLocal li1 []) : desugarLocalEquation (EquLocal li2 lis)
desugarLocalEquation (eq)                        =   [eq]
\end{code}

In the second stage, desugar all equations that assert equality of tuple of signals into a number of equations asserting equality of scalar signals that are carried by the tuple signals.

\begin{code}
desugarTupleSigRel                   ::  SigRel -> SigRel
desugarTupleSigRel (SigRel pat eqs)  =   SigRel pat (concat [ desugarTupleEquation eq | eq <- eqs ])
\end{code}

\begin{code}
desugarTupleEquation                                           ::  Equation -> [Equation]
desugarTupleEquation (EquEqual (ExpTuple es1) (ExpTuple es2))  =
  if  length es1 == length es2
      then  concat [ desugarTupleEquation (EquEqual e1 e2) | (e1,e2) <- zip es1 es2 ]
      else  undefined
desugarTupleEquation (EquInit (ExpTuple es1) (ExpTuple es2))   =
  if  length es1 == length es2
      then  concat [ desugarTupleEquation (EquInit e1 e2) | (e1,e2) <- zip es1 es2 ]
      else  undefined
desugarTupleEquation (eq)                                      =  [eq]
\end{code}

In the third stage, we desugar |connect| and |connect flow| equations into the equality constrains and sum to zero equations respectively.

\begin{code}
desugarConnectSigRel                     ::  SigRel -> SigRel
desugarConnectSigRel (SigRel pat1 eqs1)  =   SigRel pat1 (concat [ desugarConnectEquation eq | eq <- eqs1 ])
\end{code}

\begin{code}
desugarConnectEquation                               ::  Equation -> [Equation]
desugarConnectEquation (EquConnect li1 li2 lis)      =   let   vs    =  [ ExpVar li | li <- (li1 : li2 : lis) ]
                                                         in    zipWith EquEqual vs (tail vs)
desugarConnectEquation (EquConnectFlow li1 li2 lis)  =   let   vs    =  [ ExpVar li | li <- (li1 : li2 : lis) ]
                                                         in    [EquEqual  (foldr1 ExpAdd vs) (ExpReal 0)]
desugarConnectEquation (eq)                          =   [eq]
\end{code}

In the fourth stage, we desugar |flow| signal variable declarations by negating every occurrence of such variables.

\begin{code}
desugarFlowSigRel                     ::  SigRel -> SigRel
desugarFlowSigRel (SigRel pat1 eqs1)  =   let  flowVars = desugarFlowFindPattern pat1
                                               pat2 = desugarFlowForgetPattern pat1
                                               eqs2 = foldr (\s eqs -> desugarFlowEquations (s,eqs)) eqs1 flowVars
                                          in   SigRel pat2 eqs2
\end{code}

\begin{code}
desugarFlowFindPattern                                        ::  Pattern -> [String]
desugarFlowFindPattern (PatWild)                              =   []
desugarFlowFindPattern (PatName PatNameQualEmpty _)           =   []
desugarFlowFindPattern (PatName PatNameQualFlow (LIdent s1))  =   [s1]
desugarFlowFindPattern (PatTuple pats)                        =   concat [ desugarFlowFindPattern p | p <- pats ]
\end{code}

\begin{code}
desugarFlowForgetPattern                                       ::  Pattern -> Pattern
desugarFlowForgetPattern (pat@(PatWild))                       =   pat
desugarFlowForgetPattern (pat@(PatName PatNameQualEmpty li1))  =   pat
desugarFlowForgetPattern (pat@(PatName PatNameQualFlow  li1))  =   pat
desugarFlowForgetPattern (pat@(PatTuple pats))                 =   PatTuple [ desugarFlowForgetPattern p | p <- pats ]
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
desugarFlowExpr                              ::  (String,Expr) -> Expr
desugarFlowExpr (s,e@(ExpVar (LIdent s1)))   =   if s1 == s then ExpNeg e else e
desugarFlowExpr (s,ExpAdd e1 e2)             =   ExpAdd     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExpSub e1 e2)             =   ExpSub     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExpDiv e1 e2)             =   ExpDiv     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExpMul e1 e2)             =   ExpMul     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExpPow e1 e2)             =   ExpPow     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExpOr  e1 e2)             =   ExpOr      (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExpAnd e1 e2)             =   ExpAnd     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExpLt  e1 e2)             =   ExpLt      (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExpLte e1 e2)             =   ExpLte     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExpGt  e1 e2)             =   ExpGt      (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExpGte e1 e2)             =   ExpGte     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExpApp e1 e2)             =   ExpApp     (desugarFlowExpr (s,e1))  (desugarFlowExpr (s,e2))
desugarFlowExpr (s,ExpNeg e1)                =   ExpNeg     (desugarFlowExpr (s,e1))
desugarFlowExpr (s,ExpTuple es1)             =   ExpTuple   [ desugarFlowExpr (s,e) | e <- es1 ]
desugarFlowExpr (_,e)                        =   e
\end{code}

\section{Typed Intermediate Representation}

The following typed representation of Hydra models embodies the type system of Hydra.

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
  Unit  :: Signal ()
  Time  :: Signal Double
  Const :: Double -> Signal Double
  Var   :: Int -> Signal Double
  Der   :: Signal Double -> Signal Double
  App1  :: Func1 -> Signal Double -> Signal Double
  App2  :: Func2 -> Signal Double -> Signal Double -> Signal Double
  Or    :: Signal Bool -> Signal Bool -> Signal Bool
  And   :: Signal Bool -> Signal Bool -> Signal Bool
  Xor   :: Signal Bool -> Signal Bool -> Signal Bool
  Comp  :: CompFun -> Signal Double -> Signal Bool
  Pair  :: Signal a -> Signal b -> Signal (a,b)

data Func1 =
  Exp   |  Sqrt  |  Log  |  Sin   |  Tan    |  Cos    |  Asin   |  Atan  |
  Acos  |  Sinh  |  Tanh |  Cosh  |  Asinh  |  Atanh  |  Acosh  |  Abs   |  Sgn

data Func2 = Add | Mul | Div | Pow

data CompFun = Lt | Lte | Gt | Gte
\end{code}

\begin{code}
instance Num (Signal Double) where
  (+) e1 e2     = App2 Add e1 e2
  (*) e1 e2     = App2 Mul e1 e2
  (-) e1 e2     = App2 Add e1 ((Const (-1)) * e2)
  negate e1     = (Const (-1)) * e1
  abs e1        = App1 Abs e1
  signum e1     = App1 Sgn e1
  fromInteger i = Const (fromIntegral i)

instance Fractional (Signal Double) where
  (/) e1 e2 = App2 Div e1 e2
  recip e1 = 1 / e1
  fromRational r = Const (fromRational r)

instance Floating (Signal Double) where
  pi          = Const pi
  exp   e1    = App1 Exp   e1
  log   e1    = App1 Log   e1
  sqrt  e1    = App1 Sqrt  e1
  sin   e1    = App1 Sin   e1
  cos   e1    = App1 Cos   e1
  tan   e1    = App1 Tan   e1
  asin  e1    = App1 Asin  e1
  acos  e1    = App1 Acos  e1
  atan  e1    = App1 Atan  e1
  sinh  e1    = App1 Sinh  e1
  cosh  e1    = App1 Cosh  e1
  tanh  e1    = App1 Tanh  e1
  asinh e1    = App1 Asinh e1
  acosh e1    = App1 Acosh e1
  atanh e1    = App1 Atanh e1
  (**) e1 e2  = App2 Pow e1 e2
\end{code}

\section{From Untyped to Type Representation}

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
translatePat (PatTuple [])            =  Unit
translatePat (PatTuple [pat1])        =  translatePat (pat1)
translatePat (PatTuple [pat1,pat2])   =  Pair (translatePat pat1) (translatePat pat2)
\end{code}

\begin{code}
translateEqs ([])                                   =  []
translateEqs ((EquSigRelApp (HsExpr s1) e1) : eqs)  =  (App    (translateHs s1)   (translateExp e1))  :  translateEqs (eqs)
translateEqs ((EquEqual  e1 e2) : eqs)              =  (Equal  (translateExp e1)  (translateExp e2))  :  translateEqs (eqs)
translateEqs ((EquInit   e1 e2) : eqs)              =  (Init   (translateExp e1)  (translateExp e2))  :  translateEqs (eqs)
translateEqs ((EquLocal (LIdent s1) _) : eqs)       =  [Local  (\ (translateHs s1) -> (translateEqs eqs))]
translateEqs ((EquConnect _ _ _) : _)               =  undefined
translateEqs ((EquConnectFlow _ _ _) : _)           =  undefined
\end{code}

\begin{code}
translateExp (ExpAnti (HsExpr s1))      =  translateHs (s1)
translateExp (ExpVar (LIdent "time"))   =  Time
translateExp (ExpVar (LIdent "true"))   =  Comp Gt (Const 1)
translateExp (ExpVar (LIdent "false"))  =  Comp Lt (Const 1)
translateExp (ExpVar (LIdent s1))       =  translateHs (s1)
translateExp (ExpAdd e1 e2)             =  (translateExp e1)  +   (translateExp e2)
translateExp (ExpSub e1 e2)             =  (translateExp e1)  -   (translateExp e2)
translateExp (ExpDiv e1 e2)             =  (translateExp e1)  /   (translateExp e2)
translateExp (ExpMul e1 e2)             =  (translateExp e1)  *   (translateExp e2)
translateExp (ExpPow e1 e2)             =  (translateExp e1)  **  (translateExp e2)
translateExp (ExpNeg e1)                =  negate (translateExp e1)
translateExp (ExpApp e1 e2)             =  (translateExp e1) (translateExp e2)
translateExp (ExpInt i1)                =  Const (fromIntegral i1)
translateExp (ExpReal d1)               =  Const d1
translateExp (ExpTuple [])              =  Unit
translateExp (ExpTuple [e1])            =  translateExp e1
translateExp (ExpTuple (e1 : e2 : _))   =  Pair  (translateExp e1) (translateExp e2)
translateExp (ExpOr  e1 e2)             =  Or    (translateExp e1) (translateExp e2)
translateExp (ExpAnd e1 e2)             =  And   (translateExp e1) (translateExp e2)
translateExp (ExpLt  e1  e2)            =  Comp  Lt   ((translateExp e1)  -  (translateExp e2))
translateExp (ExpLte e1  e2)            =  Comp  Lte  ((translateExp e1)  -  (translateExp e2))
translateExp (ExpGt  e1  e2)            =  Comp  Gt   ((translateExp e1)  -  (translateExp e2))
translateExp (ExpGte e1  e2)            =  Comp  Gte  ((translateExp e1)  -  (translateExp e2))
\end{code}

\section{Ideal Semantics of Hydra}

Note that the domains of the following denotational semantics of Hydra are the
same as the conceptual definitions of signals, signal functions, and signal
relations given in Chapter {chapHydra}.

\begin{code}
semSR                   ::  SR a -> (Time -> Signal a -> Prop)
semSR (SR f)            =   \t0 s -> semEqs ((t0,f s))
semSR (Switch sr sf f)  =   \t0 s -> ...

\end{code}

\begin{code}
semSF           ::  SF a -> (Signal a -> Signal b)
semSF (SF sf)   =   sf
\end{code}

\begin{code}
semEqs :: (Time,[Equation]) -> Prop
\end{code}


\begin{code}
semSig :: Signal a -> (Time -> a)
\end{code}



%}