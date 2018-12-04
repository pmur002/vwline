\name{NEWS}
\title{NEWS file for the vwline package}
\encoding{UTF-8}

\section{Changes in version 0.2-1}{
  \itemize{
    \item Add \code{outline} function to return complete outline
    of variable-width line, possibly unsimplified.
  }
}
\section{Changes in version 0.2-0}{
  \itemize{
    \item Add \code{offsetBezier()} function (which requires dependency on
    \pkg{gridBezier} pacakge).

    \item Change to using \code{gridBezier::BezierGrob} and
    \code{gridBezier::BezierPoints} internally, which produces
    slightly different results on rounded ends and joins.

    \item New \code{BezierWidth()} function (as alternative to
    \code{widthSpline()} function).
  }
}
