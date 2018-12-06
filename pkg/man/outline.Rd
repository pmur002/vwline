\name{outline}
\alias{outline}
\alias{outline.vwlineGrob}
\alias{outline.offsetXsplineGrob}
\alias{outline.offsetBezierGrob}
\title{
  Calculate outline
}
\description{
  Calculate outline of a variable-width line, possibly without simplication.
}
\usage{
outline(x, simplify=TRUE, ...)
\method{outline}{vwlineGrob}(x, simplify=TRUE, ...) 
\method{outline}{offsetXsplineGrob}(x, simplify=TRUE, ...) 
\method{outline}{offsetBezierGrob}(x, simplify=TRUE, ...) 
}
\arguments{
  \item{x}{
    A variable-width line grob.
  }
  \item{simplify}{
    Whether to simplify the outline (using \code{polyclip::polysimplify}
    to collapse self-intersections).
  }
  \item{...}{
    Possible additional arguments for methods.
  }
}
\value{
  A list with components \code{x} and \code{y}
  describing the complete outline of the variable-width line
  (possibly unsimplified).
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{grid.vwline}},
  \code{\link{grid.offsetXspline}}
  \code{\link{grid.offsetBezier}}
}
\examples{
grid.newpage()
x <- c(.2, .8, .8, .2)
y <- c(.2, .8, .2, .8)
w <- c(0, .1, .1, 0)
vwlg <- vwlineGrob(x, y, w, gp=gpar(col="grey", lwd=10, fill=NA))
grid.draw(vwlg)
o <- outline(vwlg, simplify=FALSE)
grid.polygon(o$x, o$y, default.units="in")

grid.newpage()
oxsg <- offsetXsplineGrob(x, y, w, gp=gpar(col="grey", lwd=10, fill=NA))
grid.draw(oxsg)
o <- outline(oxsg, simplify=FALSE)
grid.polygon(o$x, o$y, default.units="in")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }% use one of  RShowDoc("KEYWORDS")
