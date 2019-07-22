\name{vwPath}
\alias{vwPath}
\alias{vwPolygon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Functions to render variable-width line
}
\description{
  Functions that can be used to render a variable-width line.
  All variable-width-line functions generate a polygon or path
  to represent the variable-width line and they have a
  \code{render} argument that specifies the rendering function.
  These two functions provide the default rendering behaviour.
}
\usage{
vwPath(rule = "winding")
vwPolygon(x, y, id.lengths, gp, name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rule}{
    The fill rule for rendering a variable-width line as a path.
  }
  \item{x,y,id.lengths,gp,name}{Arguments provided by the calling
    function when rendering occurs.}
}
\details{
  These functions are not called directly;  they are provided as
  arguments to variable-width-line functions (which then call them
  internally to render a variable-width line).
}
\value{
  These functions are used for their side-effect, which is to draw
  on the current graphics device.
}
\author{
  Paul Murrel
}
\seealso{
  \code{\link{grid.curve}},
  \code{\link{grid.vwline}},
  \code{\link{grid.vwcurve}},
  \code{\link{grid.brushXspline}},
  \code{\link{grid.vwXspline}}  
}
\examples{
grid.newpage()
x <- c(.2, .4, .2, .4)
y <- c(.2, .4, .4, .2)
grid.vwXspline(x, y, c(0, .1, .1, 0))
x <- c(.2, .4, .2, .4) + .4
y <- c(.2, .4, .4, .2)
grid.vwXspline(x, y, c(0, .1, .1, 0), render=vwPath("evenodd"))
x <- c(.2, .4, .2, .4) 
y <- c(.2, .4, .4, .2) + .4
grid.brushXspline(verticalBrush, x, y, c(0, .02, .02, 0))
x <- c(.2, .4, .2, .4) + .4
y <- c(.2, .4, .4, .2) + .4
grid.brushXspline(verticalBrush, x, y, c(0, .02, .02, 0),
                  render=vwPolygon)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }% use one of  RShowDoc("KEYWORDS")
