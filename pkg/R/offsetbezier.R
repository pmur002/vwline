
## grid.offsetBezier() based on gridBezier::BezierGrob() (not grid::bezierGrob)

offsetBezierEdge <- function(pts1, pts2, w1, w2, opts1, lengths1, lengths2,
                             i1, i2, grob, widths) {
    if (grob$linejoin == "extend" &&
        inherits(grob$w, "BezierWidth")) {
        ## Create a "vwlineGrob" consisting of just end segment of
        ## one curve and start segment of another
        ## WITH "bevel" line join 
        ## (which gives us the inside corner and the fall back outside corner)
        ## and use vwlinePoints() to generate the corner
        n1 <- length(pts1$x)
        n2 <- length(pts2$x)
        ## last point of curve 1 is first point of curve 2
        vwline <- vwlineGrob(c(pts1$x[n1 - 1], pts2$x[1:2]),
                             c(pts1$y[n1 - 1], pts2$y[1:2]),
                             c(w1[n1 - 1], w2[1:2])*2,
                             default.units="in",
                             linejoin="bevel",
                             lineend=grob$lineend,
                             mitrelimit=grob$mitrelimit,
                             debug=grob$debug)
        pts <- vwlinePoints(vwline)
        nl <- length(pts$left$x)
        nr <- length(pts$right$x)
        if (nl > 4) {
            keepl <- -c(1:2, nl-1, nl)
        } else {
            keepl <- -c(1, nl)
        }
        if (nr > 4) {
            keepr <- -c(1:2, nr-1, nr)
        } else {
            keepr <- -c(1, nr)
        }
        ## Calculate extended outside corner
        extend <- extendJoins(pts1, pts2, w1, w2, opts1, lengths1, lengths2,
                              i1, i2, grob, widths)
        ## Add corner to curve 1
        ## Attempt to avoid duplicating first and last points from corner
        ## Result from vwlinePoints() has right end going other direction
        if (extend$extend) {
            if (extend$leftInside) {
                list(left=list(x=c(pts1$x - w1*opts1$x, pts$left$x[keepl]),
                               y=c(pts1$y - w1*opts1$y, pts$left$y[keepl])),
                     right=list(x=c(pts1$x + w1*opts1$x, extend$x),
                                y=c(pts1$y + w1*opts1$y, extend$y)))
            } else {
                list(left=list(x=c(pts1$x - w1*opts1$x, extend$x),
                               y=c(pts1$y - w1*opts1$y, extend$y)),
                     right=list(x=c(pts1$x + w1*opts1$x,
                                    rev(pts$right$x[keepr])),
                                y=c(pts1$y + w1*opts1$y,
                                    rev(pts$right$y[keepr]))))
            }
        } else {
            ## Just use the vwlineGrob result
            list(left=list(x=c(pts1$x - w1*opts1$x, pts$left$x[keepl]),
                           y=c(pts1$y - w1*opts1$y, pts$left$y[keepl])),
                 right=list(x=c(pts1$x + w1*opts1$x, rev(pts$right$x[keepr])),
                            y=c(pts1$y + w1*opts1$y, rev(pts$right$y[keepr]))))
        }
    } else {
        ## Create a "vwlineGrob" consisting of just end segment of
        ## one curve and start segment of another
        ## and use vwlinePoints() to generate the corner
        n1 <- length(pts1$x)
        n2 <- length(pts2$x)
        ## last point of curve 1 is first point of curve 2
        vwline <- vwlineGrob(c(pts1$x[n1 - 1], pts2$x[1:2]),
                             c(pts1$y[n1 - 1], pts2$y[1:2]),
                             c(w1[n1 - 1], w2[1:2])*2,
                             default.units="in",
                             linejoin=grob$linejoin,
                             lineend=grob$lineend,
                             mitrelimit=grob$mitrelimit,
                             debug=grob$debug)
        pts <- vwlinePoints(vwline)
        nl <- length(pts$left$x)
        nr <- length(pts$right$x)
        ## Add corner to curve 1
        ## Attempt to avoid duplicating first and last points from corner
        ## (but NOT by reducing corner to nothing)
        if (nl > 4) {
            keepl <- -c(1:2, nl-1, nl)
        } else {
            keepl <- -c(1, nl)
        }
        if (nr > 4) {
            keepr <- -c(1:2, nr-1, nr)
        } else {
            keepr <- -c(1, nr)
        }
        ## Result from vwlinePoints() has right end going other direction
        list(left=list(x=c(pts1$x - w1*opts1$x, pts$left$x[keepl]),
                       y=c(pts1$y - w1*opts1$y, pts$left$y[keepl])),
             right=list(x=c(pts1$x + w1*opts1$x, rev(pts$right$x[keepr])),
                        y=c(pts1$y + w1*opts1$y, rev(pts$right$y[keepr]))))
    }
}

## Calculate points to the left and right (and middle)
offsetBezierPoints <- function(grob) {
    x <- convertX(grob$x, "in", valueOnly=TRUE)
    y <- convertY(grob$y, "in", valueOnly=TRUE)
    if (!grob$open) {
        x <- c(x, x[1])
        y <- c(y, y[1])
    }
    ## Break bezier spline into bezier curves
    ncurves <- (length(x) - 1) %/% 3
    if (ncurves*3 + 1 != length(x)) 
        stop("Invalid number of control points")
    index <- lapply(1:ncurves, function(i) ((i-1)*3 + 1):(i*3 + 1))
    ## Calculate points for each curve
    pts <- lapply(index,
                  function(i) {
                      BezierPoints(BezierGrob(x[i], y[i],
                                              default.units="in",
                                              stepFn=grob$stepFn))
                  })
    opts <- lapply(index,
                   function(i) {
                       BezierNormal(BezierGrob(x[i], y[i], default.units="in",
                                               stepFn=grob$stepFn))
                   })
    ptsIndex <- rep(1:ncurves, sapply(pts, function(p) length(p$x)))
    ## Run all points together to calculate widths at each point
    xx <- unlist(lapply(pts, "[[", "x"))
    yy <- unlist(lapply(pts, "[[", "y"))
    ox <- unlist(lapply(opts, "[[", "x"))
    oy <- unlist(lapply(opts, "[[", "y"))
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

    ## If more than one curve in the spline, we need corners where
    ## curves meet
    if (ncurves > 1) {
        ## split widths into separate curves
        curveWidths <- split(ww, ptsIndex)
        curveLengths <- tapply(lengths, ptsIndex, sum, simplify=FALSE)
        ## If closed, need an extra corner
        if (!grob$open) {
            pts <- c(pts, pts[1])
            opts <- c(opts, opts[1])
            curveWidths <- c(curveWidths, curveWidths[1])
            curveLengths <- c(curveLengths, curveLengths[1])
            index <- c(index, index[1])
            ncurves <- ncurves + 1
        }
        edges <- mapply(offsetBezierEdge,
                        pts[-ncurves], pts[-1],
                        curveWidths[-ncurves], curveWidths[-1],
                        opts[-ncurves],
                        curveLengths[-ncurves], curveLengths[-1],
                        index[-ncurves], index[-1],
                        MoreArgs=list(grob, widths),
                        SIMPLIFY=FALSE)
        ## run everything together (including final curve if open)
        leftx <- unlist(lapply(edges, function(e) e$left$x))
        lefty <- unlist(lapply(edges, function(e) e$left$y))
        rightx <- unlist(lapply(edges, function(e) e$right$x))
        righty <- unlist(lapply(edges, function(e) e$right$y))
        if (grob$open) {
            leftx <- c(leftx,
                       pts[[ncurves]]$x -
                       curveWidths[[ncurves]]*opts[[ncurves]]$x)
            lefty <- c(lefty,
                       pts[[ncurves]]$y -
                       curveWidths[[ncurves]]*opts[[ncurves]]$y)
            rightx <- c(rightx,
                        pts[[ncurves]]$x +
                        curveWidths[[ncurves]]*opts[[ncurves]]$x)
            righty <- c(righty,
                        pts[[ncurves]]$y +
                        curveWidths[[ncurves]]*opts[[ncurves]]$y)
        } 
        list(mid=list(x=xx, y=yy),
             left=list(x=leftx, y=lefty),
             right=list(x=rightx, y=righty),
             info=list(lengths=lengths, cumLength=cumLength,
                       totalLength=totalLength, ww=ww))
    } else {
        list(mid=list(x=xx, y=yy),
             left=list(x=xx - ww*ox,
                       y=yy - ww*oy),
             right=list(x=xx + ww*ox,
                        y=yy + ww*oy),
             info=list(lengths=lengths, cumLength=cumLength,
                       totalLength=totalLength, ww=ww))
    }
}

## Build complete outline by adding ends (and joins if necessary)
offsetBezierOutline <- function(grob, simplify=TRUE) {
    pts <- offsetBezierPoints(grob)
    if (grob$open) {
        if (grob$lineend == "extend" &&
            inherits(grob$w, "BezierWidth")) {
            ends <- extendEnds(grob, pts)
            outline <- list(x=c(ends$startx, pts$left$x,
                                ends$endx, rev(pts$right$x)),
                            y=c(ends$starty, pts$left$y,
                                ends$endy, rev(pts$right$y)))
        } else {
            seg <- generateSegment(pts$mid$x[1], pts$mid$y[1],
                                   pts$left$x[1:2], pts$left$y[1:2], 
                                   pts$right$x[1:2], pts$right$y[1:2],
                                   grob$debug)
            sinfo <- segInfo(seg$x, seg$y, seg$w, TRUE, FALSE, grob$debug)
            einfo <- endInfo(seg$x, seg$y, seg$w, sinfo, FALSE, grob$debug)
            earcinfo <- endArcInfo(sinfo, einfo, grob$debug)
            start <- buildEnds(seg$w, einfo, earcinfo, FALSE,
                               grob$lineend, grob$mitrelimit)
            NM <- length(pts$mid$x)
            NL <- length(pts$left$x)
            NR <- length(pts$right$x)
            seg <- generateSegment(pts$mid$x[NM], pts$mid$y[NM],
                                   pts$right$x[NR:(NR-1)],
                                   pts$right$y[NR:(NR-1)], 
                                   pts$left$x[NL:(NL-1)],
                                   pts$left$y[NL:(NL-1)],
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
        }
    } else {
        outline <- list(pts$left, lapply(pts$right, rev))
    }
    if (simplify)
        polysimplify(outline, filltype="nonzero")
    else
        outline
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

outline.offsetBezierGrob <- function(x, simplify=TRUE, ...) {
    offsetBezierOutline(x, simplify=simplify)
}
