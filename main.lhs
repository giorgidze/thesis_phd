\documentclass[10pt,a4paper]{report}

%include polycode.fmt
%include Format/haskell.lhs

\usepackage{graphicx}
\usepackage{subfigure}
\usepackage{amsmath}
\usepackage{setspace}
\usepackage[left=3.5cm, right=2.5cm, top=2.5cm, bottom=2.5cm, includefoot, headheight=13.6pt]{geometry}

\doublespacing

\begin{document}

\title{
  \Huge{\textbf{First-class Models}} \\
  \huge{On a Non-causal Language for Higher-order and Structurally-dynamic Modelling and Simulation} \\[2cm]
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

%include Chapters/declaration.lhs
%include Chapters/acknowledgements.lhs
\tableofcontents
%include Chapters/introduction.lhs
%include Chapters/background.lhs
%include Chapters/hydra.lhs
%include Chapters/definition.lhs
%include Chapters/implementation.lhs
%include Chapters/relatedWork.lhs

\chapter{Conclusions}
\label{chapConclusions}

\bibliographystyle{plain}
\bibliography{main}

\end{document}