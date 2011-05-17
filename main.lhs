\documentclass[10pt,a4paper]{report}

%include polycode.fmt
%include Format/haskell.lhs

\usepackage[left=2in, right=1in, top=1in, bottom=1in, includefoot]{geometry}

\usepackage{graphicx}
\usepackage{float}
\usepackage{amsmath}
\usepackage{setspace}
\usepackage{natbib}

\usepackage[firstpage]{draftwatermark}
\SetWatermarkScale{5.0}

\usepackage{hyperref}
\hypersetup{colorlinks, citecolor=black, filecolor=black, linkcolor=black, urlcolor=black}

\doublespacing

\begin{document}

\title{
  \Huge{\textbf{First-class Models}} \\
  \LARGE{On a Noncausal Language for Higher-order and Structurally-dynamic Modelling and Simulation} \\[2cm]
  \Large{\textbf{George Giorgidze, BSc, MSc}} \\[2cm]
  \Large{Thesis submitted to The University of Nottingham \\
  for the degree of Doctor of Philosophy}
}
\author{}
\maketitle

%include Chapters/abstract.lhs
%include Chapters/foreword.lhs
%include Chapters/acknowledgements.lhs
\tableofcontents
\listoffigures
\listoftables
%include Chapters/introduction.lhs
%include Chapters/background.lhs
%include Chapters/concepts.lhs
%include Chapters/hydra.lhs
%include Chapters/definition.lhs
%include Chapters/implementation.lhs
%include Chapters/relatedWork.lhs

\chapter{Conclusions}
\label{chapConclusions}

\bibliographystyle{plainnat}
\bibliography{main}

\end{document}