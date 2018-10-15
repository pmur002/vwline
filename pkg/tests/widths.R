
library(vwline)

grid.offsetBezier(c(.2, .4, .6, .8), c(.1, .3, .3, .1),
                  w=c(0, .1, .1, 0))
grid.offsetBezier(c(.2, .4, .6, .8), c(.1, .3, .3, .1) + .3,
                  w=widthSpline(c(0, .1, .1, 0)))
grid.offsetBezier(c(.2, .4, .6, .8), c(.1, .3, .3, .1) + .6,
                  w=BezierWidth(c(0, .1, .1, 0)))
