
## Draw a variable-width curve based on control points and
## explicit widths at the control points

grid.vwXspline <- function(...) {
    grid.draw(vwXsplineGrob(...))
}

vwXsplineGrob <- function(x, y, w, default.units="npc",
                          shape=0, open=TRUE, angle="perp",
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
    gTree(x=x, y=y, w=w, shape=shape, open=open, angle=angle, render=render,
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
    ## "vert" is simple
    if (grepl("vert", grob$angle)) {
        list(left=list(x=x, y=y + w/2),
             right=list(x=x, y=y - w/2),
             mid=list(x=x, y=y))
    } else if (grepl("horiz", grob$angle)) {
        list(left=list(x=x - w/2, y=y),
             right=list(x=x + w/2, y=y),
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

## Calculate sets of points along left, right, and mid of vwline
vwXsplinePoints <- function(grob) {
    N <- length(grob$x)
    cp <- vwXsplineControlPoints(grob)
    leftXSpline <- xsplineGrob(cp$left$x, cp$left$y,
                               default.units="in",
                               shape=grob$shape, open=grob$open)
    rightXSpline <- xsplineGrob(cp$right$x, cp$right$y,
                                default.units="in",
                                shape=grob$shape, open=grob$open)
    midXSpline <- xsplineGrob(cp$mid$x, cp$mid$y,
                              default.units="in",
                              shape=grob$shape, open=grob$open)
    list(left=xsplinePoints(leftXSpline),
         right=xsplinePoints(rightXSpline),
         mid=xsplinePoints(midXSpline))
}

## A single path for makeContent() method (and for xDetails() method)
vwXsplineOutline <- function(grob) {
    N <- length(grob$x)
    pts <- vwXsplinePoints(grob)
    ## Debugging
    if (grob$debug) {
        cp <- vwXsplineControlPoints(grob)
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
    grob$render(c(pts$left$x, rev(pts$right$x)),
                c(pts$left$y, rev(pts$right$y)),
                length(pts$left$x) + length(pts$right$x),
                grob$gp, grob$name)
}

makeContent.vwXsplineGrob <- function(x, ...) {
    addGrob(x, vwXsplineOutline(x))            
}

