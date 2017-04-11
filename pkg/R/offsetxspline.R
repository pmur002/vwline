
## Draw a line with variable width by calculating an offset curve
## for an X-spline
grid.offsetXspline <- function(...) {
    grid.draw(offsetXsplineGrob(...))
}

## IF open=FALSE, endShape and endWidth are IGNORED
offsetXsplineGrob <- function(x, y, w, default.units="npc", shape=1,
                              open=TRUE, repEnds=TRUE, render=vwPolygon,
                              gp=gpar(fill="black"), name=NULL, debug=FALSE) {
    checkoffsetXspline(x, y, w)
    if (!is.unit(x)) {
        x <- unit(x, default.units)
    }
    if (!is.unit(y)) {
        y <- unit(y, default.units)
    }
    if (!inherits(w, "widthSpline")) {
        w <- widthSpline(w, default.units)
    }
    gTree(x=x, y=y, w=w, open=open, repEnds=repEnds, render=render, shape=shape,
          gp=gp, name=name, cl="offsetXsplineGrob",
          debug=debug)
}

checkoffsetXspline <- function(x, y, w) {
    if (max(length(x), length(y)) < 2)
        stop("An offsetXspline must have at least two points")
    nx <- length(x)
    ny <- length(y)
    if (nx != ny) {
        stop("x and y must have same length")
    }
}

## Calculate points to the left and to the right
offsetXsplinePoints <- function(grob) {
    x <- convertX(grob$x, "in", valueOnly=TRUE)
    y <- convertY(grob$y, "in", valueOnly=TRUE)
    ## Flatten curve
    pts <- xspline(x, y, grob$shape, grob$open,
                   grob$repEnds, xsplineFun=xsplinePts)

    ## Pts are in inches
    xx <- as.numeric(pts$x)
    yy <- as.numeric(pts$y)
    N <- length(xx)
    if (grob$debug) {
        ## Show flattened path vertices
        grid.points(xx, yy, default.units="in", pch=16, size=unit(1, "mm"))
    }

    ## Calculate distances between flattened vertices
    lengths <- c(0, sqrt(diff(xx)^2 + diff(yy)^2))
    if (!grob$open) {
        lengths <- c(lengths, sqrt((xx[N] - xx[1])^2 + (yy[N] - yy[1])^2))
    }
    cumLength <- cumsum(lengths)
    totalLength <- sum(lengths)

    widths <- resolveWidth(grob$w, totalLength)
    ## Interpolate width at each vertex
    ## (divide by 2 because width is added to both left and right)
    ww <- approx(widths$x, widths$y, cumLength, rule=2)$y/2

    opts <- xspline(x, y, grob$shape, grob$open, grob$repEnds,
                    xsplineFun=xsplineOffsets)
    
    list(left=list(x=pts$x + ww*opts$x,
                   y=pts$y + ww*opts$y),
         right=list(x=pts$x - ww*opts$x,
                   y=pts$y - ww*opts$y))
}

offsetXsplineOutline <- function(grob) {
    pts <- offsetXsplinePoints(grob)
    outline <- list(x=c(pts$left$x, rev(pts$right$x)),
                    y=c(pts$left$y, rev(pts$right$y)))
    subset <- is.finite(outline$x) & is.finite(outline$y)
    polysimplify(lapply(outline, "[", subset), filltype="nonzero")
}

makeContent.offsetXsplineGrob <- function(x, ...) {
    outline <- offsetXsplineOutline(x)
    ## outline is list of outlines
    addGrob(x,
            x$render(unlist(lapply(outline, "[[", "x")),
                     unlist(lapply(outline, "[[", "y")),
                     sapply(outline, function(o) length(o$x)),
                     x$gp, "outline"))
}

edgePoints.offsetXsplineGrob <- function(x, d,
                                  x0, y0,
                                  which=1,
                                  direction="forward",
                                  debug=FALSE,
                                  ...) {
    ## Silently force which to length 1
    which <- which[1]
    outline <- offsetXsplineOutline(x)
    ## outline is list of outlines
    if (which > length(outline)) {
        stop("Invalid which value")
    }
    edge <- outline[[which]]
    if (!is.unit(x0)) x0 <- unit(x0, "npc")
    if (!is.unit(y0)) y0 <- unit(y0, "npc")
    pts <- reorderEdge(edge, x0, y0)
    vwEdgePoints(pts, d, direction == "forward", x$open, debug)
}

