
## Draw a variable-width curve based on control points and
## explicit widths at the control points

grid.vwXspline <- function(...) {
    grid.draw(vwXsplineGrob(...))
}

vwXsplineGrob <- function(x, y, w, default.units="npc",
                          shape=1, open=TRUE, repEnds=TRUE, angle="perp",
                          lineend="butt", mitrelimit=4,
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
    gTree(x=x, y=y, w=w, shape=shape, open=open, repEnds=repEnds,
          lineend=lineend, mitrelimit=mitrelimit, angle=angle, render=render,
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
    ## fixed angle is simple
    if (is.numeric(grob$angle)) {
        c(offset(x, y, w/2, grob$angle),
          list(mid=list(x=x, y=y)))
    } else { # should be "perp" but anything will do
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
    leftpts <- xspline(cp$left$x, cp$left$y,
                       grob$shape, grob$open, grob$repEnds)
    rightpts <- xspline(cp$right$x, cp$right$y,
                        grob$shape, grob$open, grob$repEnds)
    midpts <- xspline(cp$mid$x, cp$mid$y,
                      grob$shape, grob$open, grob$repEnds)
    list(left=leftpts, right=rightpts, mid=midpts)
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
    if (grob$open) {
        x <- convertX(grob$x[1], "in", valueOnly=TRUE)
        y <- convertY(grob$y[1], "in", valueOnly=TRUE)
        seg <- generateSegment(x, y,
                               pts$left$x[1:2], pts$left$y[1:2], 
                               pts$right$x[1:2], pts$right$y[1:2],
                               grob$debug)
        sinfo <- segInfo(seg$x, seg$y, seg$w, TRUE, FALSE, grob$debug)
        einfo <- endInfo(seg$x, seg$y, seg$w, sinfo, FALSE, grob$debug)
        earcinfo <- endArcInfo(sinfo, einfo, grob$debug)
        start <- buildEnds(seg$w, einfo, earcinfo, FALSE,
                           grob$lineend, grob$mitrelimit)
        N <- length(grob$x)
        x <- convertX(grob$x[N], "in", valueOnly=TRUE)
        y <- convertY(grob$y[N], "in", valueOnly=TRUE)
        NL <- length(pts$left$x)
        NR <- length(pts$right$x)
        seg <- generateSegment(x, y,
                               pts$right$x[NR:(NR-1)], pts$right$y[NR:(NR-1)], 
                               pts$left$x[NL:(NL-1)], pts$left$y[NL:(NL-1)],
                               grob$debug)
        sinfo <- segInfo(seg$x, seg$y, seg$w, TRUE, FALSE, grob$debug)
        einfo <- endInfo(seg$x, seg$y, seg$w, sinfo, FALSE, grob$debug)
        earcinfo <- endArcInfo(sinfo, einfo, grob$debug)
        end <- buildEnds(seg$w, einfo, earcinfo, FALSE,
                         grob$lineend, grob$mitrelimit)
        list(x=c(start$startx, pts$left$x, end$startx, rev(pts$right$x)),
             y=c(start$starty, pts$left$y, end$starty, rev(pts$right$y)),
             id.lengths=sum(length(start$startx), length(pts$left$x),
                            length(end$startx), length(pts$right$x)))
    } else {
        list(x=c(pts$left$x, rev(pts$right$x)),
             y=c(pts$left$y, rev(pts$right$y)),
             id.lengths=c(length(pts$left$x), length(pts$right$x)))
    }
}

makeContent.vwXsplineGrob <- function(x, ...) {
    outline <- vwXsplineOutline(x)
    addGrob(x,
            x$render(outline$x, outline$y, outline$id.lengths, x$gp, "outline"))
}

edgePoints.vwXsplineGrob <- function(x, d,
                                     which=c("left", "right"),
                                     direction="forward",
                                     debug=FALSE,
                                     ...) {
    pts <- vwXsplinePoints(x)
    result <- list(left=NULL, right=NULL)
    if ("left" %in% which) {
        result$left=vwEdgePoints(pts$left, d, direction == "forward",
                                 x$open, debug)
    }
    if ("right" %in% which) {
        result$right=vwEdgePoints(pts$right, d, direction == "forward",
                                  x$open, debug)
    }
    result
}


