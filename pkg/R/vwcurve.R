
## Draw a line with variable width by just generating
## points either side of x/y and then joining them up
grid.vwcurve <- function(...) {
    grid.draw(vwcurveGrob(...))
}

## IF open=FALSE, endShape and endWidth are IGNORED
vwcurveGrob <- function(x, y, w, default.units="npc", open=TRUE, angle="perp",
                        lineend="butt", mitrelimit=4,
                        render=if (open) vwPolygon else vwPath(),
                        gp=gpar(fill="black"), name=NULL, debug=FALSE) {
    ## Ok to recycle x or y or w
    maxlen <- max(length(x), length(y), length(w))
    if (length(x) < maxlen) 
        x <- rep(x, length.out=maxlen)
    if (length(y) < maxlen) 
        y <- rep(y, length.out=maxlen)
    if (length(w) < maxlen) 
        w <- rep(w, length.out=maxlen)
    checkvwcurve(x, y, w)
    if (!is.unit(x)) {
        x <- unit(x, default.units)
    }
    if (!is.unit(y)) {
        y <- unit(y, default.units)
    }
    if (!is.unit(w)) {
        w <- unit(w, default.units)
    }
    gTree(x=x, y=y, w=w, open=open, render=render, angle=angle,
          lineend=lineend, mitrelimit=mitrelimit,
          gp=gp, name=name, cl="vwcurveGrob",
          debug=debug)
}

checkvwcurve <- function(x, y, w) {
    if (max(length(x), length(y), length(w)) < 2)
        stop("A vwcurve must have at least two points")
    nx <- length(x)
    ny <- length(y)
    nw <- length(w)
    if (nx != ny || nx != nw) {
        stop("x, y, and w must all have same length")
    }
}

## Calculate points to the left and to the right
vwcurvePoints <- function(grob) {
    N <- length(grob$x)
    x <- convertX(grob$x, "in", valueOnly=TRUE)
    y <- convertY(grob$y, "in", valueOnly=TRUE)
    w <- pmin(convertWidth(grob$w, "in", valueOnly=TRUE),
              convertHeight(grob$w, "in", valueOnly=TRUE))
    ## fixed angle is simple
    if (is.numeric(grob$angle)) {
        offset(x, y, w/2, grob$angle)
    } else { # should be "perp" but anything will do
        leftx <- numeric(N)
        lefty <- numeric(N)
        rightx <- numeric(N)
        righty <- numeric(N)
        midx <- numeric(N)
        midy <- numeric(N)
        ## First point
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
        if (N > 2) {
            ## All but first and last points
            for (i in 2:(N - 1)) {
                seq <- (i - 1):(i + 1)
                perps <- perpMid(as.numeric(x[seq]), as.numeric(y[seq]), w[i]/2)
                leftx[i] <- perps[1, 1]
                lefty[i] <- perps[1, 2]
                rightx[i] <- perps[2, 1]
                righty[i] <- perps[2, 2]
            }
        }
        ## Last point
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
             right=list(x=rightx, y=righty))
    }
}

vwcurveOutline <- function(grob) {
    pts <- vwcurvePoints(grob)
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
        seg <- generateSegment(x, y,
                               pts$right$x[N:(N-1)], pts$right$y[N:(N-1)], 
                               pts$left$x[N:(N-1)], pts$left$y[N:(N-1)],
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

makeContent.vwcurveGrob <- function(x, ...) {
    outline <- vwcurveOutline(x)
    addGrob(x,
            x$render(outline$x, outline$y, outline$id.lengths, x$gp,
                     "outline"))
}

edgePoints.vwcurveGrob <- function(x, d,
                                  which=c("left", "right"),
                                  direction="forward",
                                  debug=FALSE,
                                  ...) {
    pts <- vwcurvePoints(x)
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

