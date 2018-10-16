
## grid.offsetBezier() based on gridBezier::BezierGrob() (not grid::bezierGrob)


## Calculate points to the left and right (and middle)
offsetBezierPoints <- function(grob) {
    pts <- BezierPoints(grob)
    xx <- pts$x
    yy <- pts$y
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

    opts <- BezierNormal(grob)

    list(mid=list(x=xx, y=yy),
         left=list(x=xx - ww*opts$x,
                   y=yy - ww*opts$y),
         right=list(x=xx + ww*opts$x,
                    y=yy + ww*opts$y))
}

## Build complete outline by adding ends (and joins if necessary)
offsetBezierOutline <- function(grob) {
    pts <- offsetBezierPoints(grob)

    ## FIXME: needs joins!
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
        polysimplify(outline, filltype="nonzero")
    } else {
        outline <- list(pts$left, lapply(pts$right, rev))
        polysimplify(outline, filltype="nonzero")
    }
}

edgePoints.offsetBezierGrob <- function(x, d,
                                        x0, y0,
                                        which=1,
                                        direction="forward",
                                        debug=FALSE,
                                        ...) {
    ## Silently force which to length 1
    which <- which[1]
    outline <- offsetBezierOutline(x)
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

makeContent.offsetBezierGrob <- function(x, ...) {
    outline <- offsetBezierOutline(x)
    addGrob(x,
            x$render(unlist(lapply(outline, "[[", "x")),
                     unlist(lapply(outline, "[[", "y")),
                     sapply(outline, function(o) length(o$x)),
                     x$gp, "outline"))
}

checkoffsetBezier <- function(x, y, w) {
    if (max(length(x), length(y)) < 4)
        stop("An offsetBezier must have at least four points")
    nx <- length(x)
    ny <- length(y)
    if (nx != ny) {
        stop("x and y must have same length")
    }
}

offsetBezierGrob <- function(x, y, w, default.units="npc",
                             stepFn=nSteps(100), open=TRUE,
                             lineend="butt", linejoin="round", mitrelimit=4,
                             render=if (open) vwPolygon else vwPath(),
                             gp=gpar(fill="black"), name=NULL, debug=FALSE) {
    checkoffsetBezier(x, y, w)
    if (!is.unit(x)) {
        x <- unit(x, default.units)
    }
    if (!is.unit(y)) {
        y <- unit(y, default.units)
    }
    if (!inherits(w, "widthSpline") && !inherits(w, "BezierWidth")) {
        w <- widthSpline(w, default.units)
    }
    gTree(x=x, y=y, w=w, stepFn=stepFn, open=open, render=render,
          lineend=lineend, linejoin=linejoin, mitrelimit=mitrelimit,
          gp=gp, name=name, cl="offsetBezierGrob",
          debug=debug)
}

grid.offsetBezier <- function(...) {
    grid.draw(offsetBezierGrob(...))
}

edgePoints.offsetBezierGrob <- function(x, d,
                                        x0, y0,
                                        which=1,
                                        direction="forward",
                                        debug=FALSE,
                                        ...) {
    ## Silently force which to length 1
    which <- which[1]
    outline <- offsetBezierOutline(x)
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

