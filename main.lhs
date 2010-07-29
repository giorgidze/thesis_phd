\documentclass[10pt,a4paper]{report}

%include polycode.fmt

\usepackage{graphicx}
\usepackage{subfigure}
\usepackage{amsmath}
\usepackage[left=1.5in, right=1in, top=1in, bottom=1in, includefoot, headheight=13.6pt]{geometry}

%% \parindent 0pt
%% \parskip 1ex
\renewcommand{\baselinestretch}{1.33}

\begin{document}

\title{
	\Huge{\textbf{First-class Models}} \\
	\huge{On a Non-causal Language for Higher-order and Structurally-dynamic Modelling and Simulation} \\[2cm]
	\Large{\textbf{George Giorgidze, BSc, MSc}} \\[2cm]
	\Large{Thesis submitted to The University of Nottingham \\
	for the degree of Doctor of Philosophy} \\ \vspace{1cm}
	\Large{May 2010}
}
\author{} \date{}
\maketitle

%include Chapters/abstract.lhs
%include Chapters/declaration.lhs
%include Chapters/acknowledgements.lhs
\tableofcontents
%include Chapters/introduction.lhs
%include Chapters/background.lhs
%include Chapters/relatedWork.lhs


% August
\chapter{Modelling and Simulation in Hydra}

%Week 1
\section{Concepts}
\subsection{Signals}
\subsection{Events}
\subsection{Hierarchical Systems of Non-causal Equations}
\subsection{Switches}

%Week 2
\section{Models with Static Structure}
\section{Higher-Order Modelling}

%Week 3
\section{Structural Dynamism}
\subsection{Breaking Pendulum}

%Week 4
\subsection{Ideal Diodes,Full/Half-way rectifiers}


% \chapter{Higher-Order Reusable Modelling Libraries in Hydra}
% \section{Non-Causal Connection Combinators}
% \section{Analogue Electronics}
% \section{Translational Mechanics}
% \section{Rotational Mechanics}

% September
\chapter{Implementation of Hydra}

%Week 1
\section{Mixed-level Embedding}
\section{Iterative Staging}

%Week 2
\section{Symbolic Processing}
\section{Code Generation}

%Week 3
\section{Interpretation}
\section{Just-in-time Compilation}

%Week 4
\section{Numerical Simulation}
\section{Event Handling}

% October
\chapter{Evaluation}

%Week 1 and 2
\section{Performance}

%Week 3 and 4
\section{Application of proposed approach in other settings}


\chapter{Conclusions}

\bibliographystyle{apalike}
\bibliography{main}

\end{document}
