
################################################################################
## brushXspline()

grid.brushXspline <- function(...) {
    grid.draw(brushXsplineGrob(...))
}

## Lower 'tol'erance means more detail in the width curve
## (for when you want fine detail from the width curve)
## Higher 'tol'erance means less detail in the width curve
## (for when you want a bit less detail from the width curve)
brushXsplineGrob <- function(brush, x, y, w=unit(1, "cm"), default.units="npc",
                             shape=1, angle="perp", open=TRUE, spacing=NULL,
                             render=vwPath(),
                             gp=gpar(fill="black"),
                             name=NULL, debug=FALSE) {
    checkbrushXspline(x, y)
    if (!is.unit(x)) {
        x <- unit(x, default.units)
    }
    if (!is.unit(y)) {
        y <- unit(y, default.units)
    }
    if (!inherits(w, "widthSpline") && !inherits(w, "BezierWidth")) {
        w <- widthSpline(w, default.units)
    }
    gTree(brush=brush, x=x, y=y, w=w, shape=shape, angle=angle, open=open,
          spacing=spacing, render=render, tol=.01,
          gp=gp, name=name, cl="brushXsplineGrob",
          debug=debug)
}

checkbrushXspline <- function(x, y, w) {
    if (max(length(x), length(y)) < 2)
        stop("A brushXspline must have at least two points")
    nx <- length(x)
    ny <- length(y)
    if (nx != ny) {
        stop("x and y must have same length")
    }
}

brushXsplineOutline <- function(grob) {
    ## Flatten curve
    pts <- xsplinePoints(xsplineGrob(grob$x, grob$y, shape=grob$shape,
                                     open=grob$open))

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
    ## Drop points that are too close to each other
    ## (can lead to numerical instability when calculating angles)
    smallLengths <- c(FALSE, lengths[-1] < grob$tol)
    if (any(smallLengths)) {
        xx <- xx[!smallLengths]
        yy <- yy[!smallLengths]
        N <- length(xx)
        lengths <- c(0, sqrt(diff(xx)^2 + diff(yy)^2))
    }
    if (!grob$open) {
        lengths <- c(lengths, sqrt((xx[N] - xx[1])^2 + (yy[N] - yy[1])^2))
    }
    cumLength <- cumsum(lengths)
    totalLength <- sum(lengths)

    widths <- resolveWidth(grob$w, totalLength)
    ## Add vertex corresponding to each explicit width
    ## (handles case of more widths than vertices for straight edges)
    ## No need to worry about 0 and 1 (they correspond exactly to vertices)
    if (length(widths$x) > 2) {
        newPath <- fortifyPath(xx, yy, widths$x, lengths, grob$open, grob$tol)
        ## New vertices
        xx <- newPath$x
        yy <- newPath$y
        N <- length(xx)
        ## Recalculate distances between vertices
        lengths <- c(0, sqrt(diff(xx)^2 + diff(yy)^2))
        if (!grob$open) {
            lengths <- c(lengths, sqrt((xx[N] - xx[1])^2 + (yy[N] - yy[1])^2))
        }
        cumLength <- cumsum(lengths)
        totalLength <- sum(lengths)
    }

    if (is.null(grob$spacing)) {
        ## Interpolate width at each vertex
        ww <- approx(widths$x, widths$y, cumLength, rule=2)$y
        ## fixed angle is simple
        if (is.numeric(grob$angle)) {
            a <- rep(grob$angle, length(xx))
        } else { # should be "perp" but anything will do
            a <- numeric(length(xx))
            if (grob$open) {
                a[1] <- brushEndAngle(xx[1:2], yy[1:2])
            } else {
                a[1] <- brushAngle(xx[c(N, 1:2)], yy[c(N, 1:2)])
            }
            if (N > 2) {
                for (i in 2:(N - 1)) {
                    a[i] <- brushAngle(xx[(i-1):(i+1)], yy[(i-1):(i+1)])
                }
            }
            if (grob$open) {
                a[N] <- brushEndAngle(xx[(N-1):N], yy[(N-1):N])
            } else {
                a[N] <- brushAngle(xx[c((N-1):N, 1)], yy[c((N-1):N, 1)])
            }
        } 
        brushes <- vector("list", N)
        brushes[[1]] <- placeBrush(grob$brush, xx[1], yy[1], ww[1], a[1])
        if (N > 2) {
            for (i in 2:(N - 1)) {
                brushes[[i]] <- placeBrush(grob$brush, xx[i], yy[i], ww[i], a[i])
            }
        }
        brushes[[N]] <- placeBrush(grob$brush, xx[N], yy[N], ww[N], a[N])
        ## Make segment for each pair of vertices
        ## (based on convex hull of brush at vertices)
        segments <- vector("list", N - 1)
        for (i in 1:(N - 1)) {
            segments[[i]] <- makeSegment(brushes[[i]], brushes[[i+1]],
                                         debug=grob$debug)
        }
        if (!grob$open) {
            ## Connect last brush to first brush
            segments[[N]] <- makeSegment(brushes[[N]], brushes[[1]],
                                         debug=grob$debug)            
        }
        ## Form union of all segments (may be more than one polygon)
        curve <- Reduce(combineShapes, segments)

        if (grob$debug) {
            ## Show the brushes at each vertex
            for (i in 1:N) {
                b <- brushes[[i]]
                grid.polygon(b$x, b$y, default.units="in", gp=gpar(col="grey"))
            }
        }
        
    } else {
        ## Determine locations of brushes
        s <- resolveDistance(grob$spacing, totalLength)
        brushLocs <- interpPath(xx, yy, s, lengths, grob$open)
        bx <- brushLocs$x
        by <- brushLocs$y
        a <- brushLocs$angle
        N <- length(bx)
        ## Interpolate width at each location
        ww <- approx(widths$x, widths$y, s, rule=2)$y
        ## fixed angle is simple
        if (is.numeric(grob$angle)) {
            a <- rep(grob$angle, length(xx))
        } # else should be "perp" but anything will do
        brushes <- vector("list", N)
        brushes[[1]] <- placeBrush(grob$brush, bx[1], by[1], ww[1], a[1])
        if (N > 2) {
            for (i in 2:(N - 1)) {
                brushes[[i]] <- placeBrush(grob$brush, bx[i], by[i], ww[i], a[i])
            }
        }
        if (N > 1) {
            brushes[[N]] <- placeBrush(grob$brush, bx[N], by[N], ww[N], a[N])
        }
        if (N > 1) {
            curve <- Reduce(combineShapes, brushes)
        } else {
            curve <- brushes
        }
    }
    curve
}

makeContent.brushXsplineGrob <- function(x, ...) {
    outline <- brushXsplineOutline(x)
    if (is.null(outline$x)) {
        ## outline is list of outlines
        addGrob(x,
                x$render(unlist(lapply(outline, "[[", "x")),
                         unlist(lapply(outline, "[[", "y")),
                         sapply(outline, function(o) length(o$x)),
                         x$gp, "outline"))
    } else {
        addGrob(x,
                x$render(outline$x,
                         outline$y,
                         length(outline$x),
                         x$gp, "outline"))
    }
}

edgePoints.brushXsplineGrob <- function(x, d,
                                        x0, y0,
                                        which=1,
                                        direction="forward",
                                        debug=FALSE,
                                        ...) {
    ## Silently force which to length 1
    which <- which[1]
    outline <- brushXsplineOutline(x)
    if (is.null(outline$x)) {
        ## outline is list of outlines
        if (which > length(outline)) {
            stop("Invalid which value")
        }
        edge <- outline[[which]]
    } else {
        if (length(which) != 1 || which != 1) {
            stop("Invalid which value")
        }
        edge <- outline
    }
    if (!is.unit(x0)) x0 <- unit(x0, "npc")
    if (!is.unit(y0)) y0 <- unit(y0, "npc")
    pts <- reorderEdge(edge, x0, y0)
    vwEdgePoints(pts, d, direction == "forward", x$open, debug)
}

