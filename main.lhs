\documentclass[10pt,a4paper]{report}

%include polycode.fmt

\usepackage{graphicx}
\usepackage{subfigure}
\usepackage{amsmath}
\usepackage[left=1.5in, right=1in, top=1in, bottom=1in, includefoot, headheight=13.6pt]{geometry}

%% \parindent 0pt
%% \parskip 1ex
% \renewcommand{\baselinestretch}{1.33}

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

\chapter*{}
\vspace*{8cm}

\begin{flushleft} 
\large \emph{``Unprovided with original learning, unformed in the habits of thinking, unskilled in the arts of composition, I resolved to write a book.''}
\end{flushleft}
\begin{flushright} 
\large
Edward Gibbon
\end{flushright}

%include Chapters/declaration.lhs
%include Chapters/acknowledgements.lhs
\tableofcontents
%include Chapters/introduction.lhs
%include Chapters/background.lhs
%include Chapters/hydra.lhs
%include Chapters/definition.lhs
%include Chapters/implementation.lhs

% October
\chapter{Evaluation}
\label{chapEvaluation}

%include Chapters/relatedWork.lhs

\chapter{Conclusions}
\label{chapConclusions}

\bibliographystyle{plain}
\bibliography{main}

\end{document}