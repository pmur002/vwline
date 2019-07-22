\name{widthSpec}
\alias{widthSpec}
\alias{widthSpline}
\alias{BezierWidth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Specify the width of a variable-width line
}
\description{
  Specify the width of a variable-width line.  The
  \code{widthSpline} and \code{BezierWidth} functions are for use with
  \code{\link{grid.brushXspline}} or \code{\link{grid.offsetXspline}} or
  \code{\link{grid.offsetBezier}}.
  The \code{widthSpec} function is for use with
  \code{\link{grid.vwline}}.
}
\usage{
widthSpec(x, default.units = "npc")
widthSpline(w=unit(1, "cm"), default.units="in", d=NULL, shape=-1, rep=FALSE)
BezierWidth(w=unit(1, "cm"), default.units="in", d=NULL, rep=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A numeric vector or a unit specifying the width,
    or a list of such with components
    \code{left} and \code{right} specifying the width to either side.
  }
  \item{default.units}{
    The units to use if \code{x} or \code{w} is specified as a
    numeric vector.
  }
  \item{w}{A numeric vector or a unit specifying the width.}
  \item{d}{A numeric vector or a unit specifying the distance along
    the line for each width value.}
  \item{shape}{The shape parameter for the width spline.}
  \item{rep}{A logical indicating whether to repeat the widths
    along the full length of the line.}
}
\details{
  All variable-width functions accept width as just a numeric vector
  or a unit; the former is automatically converted to the latter.

  For \code{\link{grid.vwcurve}} and \code{\link{grid.vwXspline}}, each width
  is associated with an x/y location on the main curve.
  The \code{\link{grid.vwline}} function is similar except that,
  via \code{widthSpec}, it
  also allows the width to be different on either side of the
  main curve.

  For \code{\link{grid.brushXspline}} and \code{\link{grid.offsetXspline}},
  the width is independent of the x/y locations that specify the main
  curve.  The width is itself a spline, with each width associated
  with a distance along the length of the main curve.  By default,
  the specified widths are spaced evenly along the main curve, but
  the \code{widthSpec} function allows fine control over the
  spacing of the widths.

  For \code{BezierWidth}, \code{w} must contain 4 (or 7 or 10)
  values to provide an appropriate number of control points, although
  a single value is automatically replicated four times.
  If \code{d} is specified, the same rules apply.
}
\value{
  \code{widthSpec} creates a \code{"widthSpec"} object.

  \code{widthSpline} creates a \code{"widthSpline"} object.

  \code{BezierWidth} creates a \code{"BezierWidth"} object.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{grid.xspline}},
  \code{\link{grid.curve}},
  \code{\link{grid.vwline}},
  \code{\link{grid.vwcurve}},
  \code{\link{grid.brushXspline}},
  \code{\link{grid.vwXspline}},
  \code{\link{grid.offsetXspline}},
  \code{\link{grid.offsetBezier}}
}
\examples{
grid.newpage()
x <- c(.2, .4, .6, .8)
y <- c(-.05, .05, -.05, .05)
grid.vwcurve(x, y + .8, w=c(0, .1, 0, .1))
grid.lines(x, y + .8, gp=gpar(col="white"))
grid.vwline(x, y + .6, w=widthSpec(list(left=c(0, .1, 0, .1),
                                        right=c(.1, 0, .1, 0))))
grid.lines(x, y + .6, gp=gpar(col="white"))
grid.brushXspline(verticalBrush, x, y + .4,
                  w=widthSpline(c(0, .1, 0, .1), "npc", shape=1))
grid.xspline(x, y + .4, shape=1, gp=gpar(col="white"))
grid.offsetXspline(x, y + .2, 
                  w=widthSpline(c(0, .1, 0, .1, 0), "npc",
                                d=0:4/8, shape=1, rep=TRUE))
grid.xspline(x, y + .2, shape=1, gp=gpar(col="white"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }% use one of  RShowDoc("KEYWORDS")
