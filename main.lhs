\documentclass[10pt,a4paper]{report}

%include polycode.fmt
%include Format/haskell.lhs

\usepackage{graphicx}
\usepackage{subfigure}
\usepackage{amsmath}
\usepackage{setspace}
\usepackage{natbib}
\usepackage[left=2in, right=1in, top=1in, bottom=1in, includefoot]{geometry}

\usepackage{hyperref}
\hypersetup{colorlinks, citecolor=black, filecolor=black, linkcolor=black, urlcolor=black}

\doublespacing

\begin{document}

\title{
  \Huge{\textbf{First-class Models}} \\
  \LARGE{On a Non-causal Language for Higher-order and Structurally-dynamic Modelling and Simulation} \\[2cm]
  \Large{\textbf{George Giorgidze, BSc, MSc}} \\[2cm]
  \Large{Thesis submitted to The University of Nottingham \\
  for the degree of Doctor of Philosophy} \\ \vspace{1cm}
  \Large{April 2011}
}
\author{} \date{}
\maketitle

%include Chapters/abstract.lhs

\chapter*{}
\vspace*{8cm}

\begin{flushleft}
\large
\emph{``Unprovided with original learning, unformed in the habits of thinking,
unskilled in the arts of composition, I resolved to write a book.''}
\end{flushleft}
\begin{flushright} 
\large
Edward Gibbon
\end{flushright}

%include Chapters/acknowledgements.lhs
\tableofcontents
\listoffigures
\listoftables
%include Chapters/introduction.lhs
%include Chapters/background.lhs
%include Chapters/hydra.lhs
%include Chapters/definition.lhs
%include Chapters/implementation.lhs
%include Chapters/relatedWork.lhs

\chapter{Conclusions}
\label{chapConclusions}

\bibliographystyle{plainnat}
\bibliography{main}

\end{document}