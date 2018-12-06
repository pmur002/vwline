
## Draw a line with variable width by calculating an offset curve
## for an X-spline
grid.offsetXspline <- function(...) {
    grid.draw(offsetXsplineGrob(...))
}

## IF open=FALSE, endShape and endWidth are IGNORED
offsetXsplineGrob <- function(x, y, w, default.units="npc", shape=1,
                              open=TRUE, repEnds=TRUE,
                              lineend="butt", mitrelimit=4,
                              render=if (open) vwPolygon else vwPath(),
                              gp=gpar(fill="black"), name=NULL, debug=FALSE) {
    checkoffsetXspline(x, y, w)
    if (!is.unit(x)) {
        x <- unit(x, default.units)
    }
    if (!is.unit(y)) {
        y <- unit(y, default.units)
    }
    if (!inherits(w, "widthSpline") && !inherits(w, "BezierWidth")) {
        w <- widthSpline(w, default.units)
    }
    gTree(x=x, y=y, w=w, open=open, repEnds=repEnds, render=render, shape=shape,
          lineend=lineend, mitrelimit=mitrelimit,
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
    if (grob$open && is.logical(grob$repEnds) && grob$repEnds) {
        ## Drop first/last points (because first/last offsets will be NaN)
        npts <- length(pts$x)
        pts <- list(x=pts$x[-c(1, npts)], y=pts$y[-c(1, npts)])
    }
    
    ## Pts are in inches
    xx <- pts$x
    yy <- pts$y
    N <- length(xx)
    if (grob$debug) {
        ## Show flattened path vertices
        grid.points(xx, yy, default.units="in", pch=16, size=unit(1, "mm"))
    }

    ## Calculate distances between flattened vertices
    lengths <- c(0, sqrt(diff(xx)^2 + diff(yy)^2))
    cumLength <- cumsum(lengths)
    totalLength <- sum(lengths)

    widths <- resolveWidth(grob$w, totalLength)
    ## Interpolate width at each vertex
    ## (divide by 2 because width is added to both left and right)
    ww <- approx(widths$x, widths$y, cumLength, rule=2)$y/2

    opts <- xspline(x, y, grob$shape, grob$open, grob$repEnds,
                    xsplineFun=xsplineOffsets)
    if (grob$open && is.logical(grob$repEnds) && grob$repEnds) {
        ## Drop first/last points (because first/last offsets will be NaN)
        opts <- list(x=opts$x[-c(1, npts)], y=opts$y[-c(1, npts)])
    }
    
    list(mid=pts,
         left=list(x=pts$x - ww*opts$x,
                   y=pts$y - ww*opts$y),
         right=list(x=pts$x + ww*opts$x,
                   y=pts$y + ww*opts$y))
}

offsetXsplineOutline <- function(grob, simplify=TRUE) {
    pts <- offsetXsplinePoints(grob)
    pts <- lapply(pts, function(x) lapply(x, function(y) y[is.finite(y)]))
    if (grob$open) {
        seg <- generateSegment(pts$mid$x[1], pts$mid$y[1],
                               pts$left$x[1:2], pts$left$y[1:2], 
                               pts$right$x[1:2], pts$right$y[1:2],
                               grob$debug)
        sinfo <- segInfo(seg$x, seg$y, seg$w, TRUE, FALSE, grob$debug)
        einfo <- endInfo(seg$x, seg$y, seg$w, sinfo, FALSE, grob$debug)
        earcinfo <- endArcInfo(sinfo, einfo, grob$debug)
        start <- buildEnds(seg$w, einfo, earcinfo, FALSE,
                           grob$lineend, grob$mitrelimit)
        N <- length(pts$left$x)
        seg <- generateSegment(pts$mid$x[N], pts$mid$y[N],
                               pts$right$x[N:(N-1)], pts$right$y[N:(N-1)], 
                               pts$left$x[N:(N-1)], pts$left$y[N:(N-1)],
                               grob$debug)
        sinfo <- segInfo(seg$x, seg$y, seg$w, TRUE, FALSE, grob$debug)
        einfo <- endInfo(seg$x, seg$y, seg$w, sinfo, FALSE, grob$debug)
        earcinfo <- endArcInfo(sinfo, einfo, grob$debug)
        end <- buildEnds(seg$w, einfo, earcinfo, FALSE,
                         grob$lineend, grob$mitrelimit)
        outline <- list(x=c(start$startx, pts$left$x,
                            end$startx, rev(pts$right$x)),
                        y=c(start$starty, pts$left$y,
                            end$starty, rev(pts$right$y)))
    } else {
        outline <- list(pts$left, lapply(pts$right, rev))
    }
    if (simplify) 
        polysimplify(outline, filltype="nonzero")
    else
        outline
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

outline.offsetXsplineGrob <- function(x, simplify=TRUE, ...) {
    offsetXsplineOutline(x, simplify=simplify)
}
