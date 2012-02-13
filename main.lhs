\documentclass[12pt,a4paper]{report}

%include polycode.fmt
%include Format/haskell.lhs

\usepackage[left=3.8cm, right=2.0cm, top=3.8cm, bottom=3.8cm]{geometry}

\usepackage{graphicx}
\usepackage{amsmath}
\usepackage{setspace}
\usepackage{natbib}

\usepackage{hyperref}
\hypersetup{colorlinks, citecolor=black, filecolor=black, linkcolor=black, urlcolor=black}

\onehalfspacing

\widowpenalty=10000
\clubpenalty=10000

\begin{document}

\title{
  \huge{\textbf{FIRST-CLASS MODELS}} \\
  \LARGE{On a Noncausal Language for Higher-order and Structurally Dynamic Modelling and Simulation} \\[2cm]
  \Large{\textbf{GEORGE GIORGIDZE, BSc, MSc}} \\[2cm]
  \Large{Thesis submitted to the University of Nottingham} \\
  \Large{for the degree of Doctor of Philosophy} \\[2cm]
  \Large{February 2012}
}
\author{}
\date{}
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
%include Chapters/conclusions.lhs

\bibliographystyle{plainnat}
\bibliography{main}

\end{document}