
## Draw a variable-width curve based on control points and
## explicit widths at the control points

grid.vwXspline <- function(...) {
    grid.draw(vwXsplineGrob(...))
}

## IF open=FALSE, endShape and endWidth are IGNORED
vwXsplineGrob <- function(x, y, w, default.units="npc",
                          shape=0, open=TRUE, perp=FALSE,
                          render=vwPath(),
                          gp=gpar(fill="black"), name=NULL, debug=FALSE) {
    checkvwXspline(x, y, w)
    if (!is.unit(x)) {
        x <- unit(x, default.units)
    }
    if (!is.unit(y)) {
        y <- unit(y, default.units)
    }
    if (!is.unit(w)) {
        w <- unit(w, default.units)
    }
    gTree(x=x, y=y, w=w, shape=shape, open=open, perp=perp,
          debug=debug, gp=gp, name=name, cl="vwXsplineGrob")
}

checkvwXspline <- function(x, y, w) {
    if (max(length(x), length(y), length(w)) < 3)
        stop("A vwline must have at least three control points")
    nx <- length(x)
    ny <- length(y)
    nw <- length(w)
    if (nx != ny || nx != nw) {
        stop("x, y, and w must all have same length")
    }
}

## Generate a set of control points from which we can produce
## one or more XSplines for the outline of the vwline
## NOTE that we want to be able to produce separate upper and right (and mid)
## XSplines (for edge points) as well as a single overall boundary XSpline
## (for drawing)
vwXsplineControlPoints <- function(grob) {
    N <- length(grob$x)
    x <- convertX(grob$x, "in", valueOnly=TRUE)
    y <- convertY(grob$y, "in", valueOnly=TRUE)
    w <- pmin(convertWidth(grob$w, "in", valueOnly=TRUE),
              convertHeight(grob$w, "in", valueOnly=TRUE))
    ## Perp is simple
    if (grob$perp) {
        list(left=list(x=x, y=y + w/2),
             right=list(x=x, y=y - w/2),
             mid=list(x=x, y=y))
    } else {
        leftx <- numeric(N)
        lefty <- numeric(N)
        rightx <- numeric(N)
        righty <- numeric(N)
        ## First control point
        if (grob$open) {
            perps <- perpStart(x[1:2], y[1:2], w[1]/2)
            leftx[1] <- perps[1, 1]
            lefty[1] <- perps[1, 2]
            rightx[1] <- perps[2, 1]
            righty[1] <- perps[2, 2]
        } else {
            seq <- c(N, 1:2)
            perps <- perpMid(as.numeric(x[seq]), as.numeric(y[seq]), w[1]/2)
            leftx[1] <- perps[1, 1]
            lefty[1] <- perps[1, 2]
            rightx[1] <- perps[2, 1]
            righty[1] <- perps[2, 2]
        }
        ## All but first and last control points
        for (i in 2:(N - 1)) {
            seq <- (i - 1):(i + 1)
            perps <- perpMid(as.numeric(x[seq]), as.numeric(y[seq]), w[i]/2)
            leftx[i] <- perps[1, 1]
            lefty[i] <- perps[1, 2]
            rightx[i] <- perps[2, 1]
            righty[i] <- perps[2, 2]
        }
        ## Last control point
        if (grob$open) {
            perps <- perpEnd(x[(N-1):N], y[(N-1):N], w[N]/2)
            leftx[N] <- perps[1, 1]
            lefty[N] <- perps[1, 2]
            rightx[N] <- perps[2, 1]
            righty[N] <- perps[2, 2]
        } else {
            seq <- c(N - 1, N, 1)
            perps <- perpMid(as.numeric(x[seq]), as.numeric(y[seq]), w[N]/2)
            leftx[N] <- perps[1, 1]
            lefty[N] <- perps[1, 2]
            rightx[N] <- perps[2, 1]
            righty[N] <- perps[2, 2]
        }
        list(left=list(x=leftx, y=lefty),
             right=list(x=rightx, y=righty),
             mid=list(x=x, y=y))
    }
}

## A single path for makeContent() method (and for xDetails() method)
vwXsplineOutline <- function(grob) {
    N <- length(grob$x)
    cp <- vwXsplineControlPoints(grob)
    ## Debugging
    if (grob$debug) {
        ## grid.lines(cp$right$x, cp$right$y, default.units="in", gp=gpar(col="red", lty="dashed"))
        grid.segments(cp$mid$x, cp$mid$y, cp$right$x, cp$right$y,
                      default.units="in",
                      gp=gpar(col="red"))
        grid.points(cp$right$x, cp$right$y, pch=16, 
                    default.units="in",
                    gp=gpar(col="red"))
        ## grid.lines(cp$left$x, cp$left$y, default.units="in", gp=gpar(col="blue", lty="dashed"))
        grid.segments(cp$left$x, cp$left$y, cp$mid$x, cp$mid$y,
                      default.units="in",
                      gp=gpar(col="blue"))
        grid.points(cp$left$x, cp$left$y, pch=16, 
                    default.units="in",
                    gp=gpar(col="blue"))
    }
    if (grob$open) {
        ## Allow for endShape to be length 2
        startShape <- grob$endShape[1]
        if (length(grob$endShape) > 1) {
            endShape <- grob$endShape[2]
        } else {
            endShape <- grob$endShape
        }
        vwShape <- c(rep(startShape, 2), rep(grob$shape, N - 2),
                     rep(endShape, 3),
                     rep(grob$shape, N - 2), rep(startShape, 2))
        xs <- xsplineGrob(c(cp$mid$x[1], cp$left$x, cp$mid$x[N], rev(cp$right$x)),
                          c(cp$mid$y[1], cp$left$y, cp$mid$y[N], rev(cp$right$y)),
                          default.units="in",
                          open=FALSE, shape=vwShape)
        pts <- xsplinePoints(xs)
        pathGrob(pts$x, pts$y, id.lengths=length(pts$x),
                 rule="winding")
    } else {
        outerPts <- xsplinePoints(xsplineGrob(cp$left$x, cp$left$y,
                                              default.units="in",
                                              open=FALSE, shape=grob$shape))
        innerPts <- xsplinePoints(xsplineGrob(rev(cp$right$x), rev(cp$right$y),
                                              default.units="in",
                                              open=FALSE, shape=grob$shape))
        pathGrob(c(outerPts$x, innerPts$x),
                 c(outerPts$y, innerPts$y),
                 id.lengths=c(length(outerPts$x), length(innerPts$x)),
                 default.units="in",
                 rule="winding")        
    }
}

makeContent.vwXsplineGrob <- function(x, ...) {
    addGrob(x, vwXsplineOutline(x))            
}

## Calculate sets of points along left, right, and mid of vwline
vwXsplinePoints <- function(grob) {
    N <- length(grob$x)
    cp <- vwXsplineControlPoints(grob)
    if (grob$open) {
        ## Allow for endShape to be length 2
        startShape <- grob$endShape[1]
        if (length(grob$endShape) > 1) {
            endShape <- grob$endShape[2]
        } else {
            endShape <- grob$endShape
        }
        leftShape <- c(rep(startShape, 3), rep(grob$shape, N - 2),
                        rep(endShape, 3))
        leftXSpline <- xsplineGrob(c(cp$right$x[1], cp$mid$x[1], cp$left$x,
                                      cp$mid$x[N], cp$right$x[N]),
                                    c(cp$right$y[1], cp$mid$y[1], cp$left$y,
                                      cp$mid$y[N], cp$right$y[N]),
                                    default.units="in",
                                    shape=leftShape, repEnds=FALSE)
        rightShape <- c(rep(startShape, 3), rep(grob$shape, N - 2),
                        rep(endShape, 3))
        rightXSpline <- xsplineGrob(c(cp$left$x[1], cp$mid$x[1], cp$right$x,
                                      cp$mid$x[N], cp$left$x[N]),
                                    c(cp$left$y[1], cp$mid$y[1], cp$right$y,
                                      cp$mid$y[N], cp$left$y[N]),
                                    default.units="in",
                                    shape=rightShape, repEnds=FALSE)
        midShape <- c(startShape, rep(grob$shape, N - 2), endShape)
        midXSpline <- xsplineGrob(cp$mid$x, cp$mid$y, 
                                  default.units="in",
                                  shape=midShape, repEnds=FALSE)
    } else {
        leftXSpline <- xsplineGrob(cp$left$x, cp$left$y,
                                    default.units="in",
                                    shape=grob$shape, open=FALSE)
        rightXSpline <- xsplineGrob(cp$right$x, cp$right$y,
                                    default.units="in",
                                    shape=grob$shape, open=FALSE)
        midXSpline <- xsplineGrob(cp$mid$x, cp$mid$y,
                                    default.units="in",
                                    shape=grob$shape, open=FALSE)
    }
    list(left=xsplinePoints(leftXSpline),
         right=xsplinePoints(rightXSpline),
         mid=xsplinePoints(midXSpline))
}
