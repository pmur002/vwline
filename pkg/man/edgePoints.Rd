\name{edgePoints}
\alias{edgePoints}
\alias{edgePoints.vwcurveGrob}
\alias{edgePoints.vwlineGrob}
\alias{edgePoints.vwXsplineGrob}
\alias{edgePoints.brushXsplineGrob}
\alias{edgePoints.offsetXsplineGrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate edge points
}
\description{
  Calculate points on the edge or boundary of a variable-width line.
}
\usage{
edgePoints(x, d, ...)
\method{edgePoints}{vwcurveGrob}(x, d, which = c("left", "right"), direction = "forward", debug = FALSE, ...) 
\method{edgePoints}{vwlineGrob}(x, d, x0, y0, which = 1, direction = "forward", debug = FALSE, ...) 
\method{edgePoints}{vwXsplineGrob}(x, d, which = c("left", "right"), direction = "forward", debug = FALSE, ...)
\method{edgePoints}{brushXsplineGrob}(x, d, x0, y0, which = 1, direction = "forward", debug = FALSE, ...) 
\method{edgePoints}{offsetXsplineGrob}(x, d, x0, y0, which = 1, direction = "forward", debug = FALSE, ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A variable-width line grob.
  }
  \item{d}{
    A numeric vector or unit specifying locations along the boundary of
    the variable-width line.
  }
  \item{which}{
    For some methods, this is either \code{"left"} or \code{"right"} (or both)
    indicating which edge to find locations on.  For other methods, this
    is a numeric, selecting which boundary to find locations on.  See Details.
  }
  \item{direction}{
    Either \code{"forwards"} or \code{"backwards"} to indicate the
    direction of traversal for the edge.
  }
  \item{x0, y0}{
    A location used to determine the start point for traversing an edge.
  }
  \item{debug}{
    A logical indicating whether to draw graphical debugging
    information.
  }
  \item{\dots}{
    Additional arguments for methods.
  }
}
\details{
  If the distance is numeric, it is assumed to be a proportion of the
  length of an edge.

  What constitutes an edge varies between different methods:
  some methods produce distinct left and right edges (ignoring line
  endings), in which case locations can be found on either edge;
  other methods generally produce a single boundary (including line
  endings), but self-intersecting lines can produce additional
  boundaries.

  For some methods, it is possible for a boundary to form loops,
  so an edge location is not guaranteed to be on the external boundary
  of the variable-width line.
  
  For lines with distinct left and right edges, the forwards direction
  is the direction of the main curve.  For lines with a single boundary,
  the forwards direction is anticlockwise.

  For lines with a single boundary, the start point on the boundary is
  defined as the nearest point on the boundary to the location specified
  by \code{x0} and \code{y0}.
}
\value{
  For methods with a single boundary,
  a list with components \code{x} and \code{y}, giving locations on the
  edge of the variable-width line, and \code{tangent}, giving the
  tangent to the edge at each location.

  For methods with distinct left and right edges, a list with components
  \code{left} and \code{right}, each a list as above.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{grid.vwcurve}},
  \code{\link{grid.vwline}},
  \code{\link{grid.vwXspline}},
  \code{\link{grid.brushXspline}},
  \code{\link{grid.offsetXspline}}
}
\examples{
grid.newpage()
x <- seq(.2, .8, length.out=100)
y <- .05*sin(seq(0, 2*pi, length.out=100))
w <- unit(seq(2, 10, length.out=100), "mm")
vwcg <- vwcurveGrob(x, y + 2/3, w, lineend="round",
                    gp=gpar(col="black"))
grid.draw(vwcg)
epts <- edgePoints(vwcg, 0:9/9)
grid.circle(epts$left$x, epts$left$y, r=unit(1, "mm"), 
            gp=gpar(fill=hcl(0, 80, seq(10, 100, 10))))
x <- c(.2, .4, .6, .8)
y <- c(-.05, .05, -.05, .05)
w <- unit(c(2, 4, 6, 8), "mm")
vwbg <- brushXsplineGrob(circleBrush(), x, y + 1/3, w,
                         gp=gpar(col="black"))
grid.draw(vwbg)
epts <- edgePoints(vwbg, unit(0:9, "cm"), x0=0, y0=1/3)
grid.circle(epts$x, epts$y, r=unit(1, "mm"), 
            gp=gpar(fill=hcl(0, 80, seq(10, 100, 10))))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }% use one of  RShowDoc("KEYWORDS")
