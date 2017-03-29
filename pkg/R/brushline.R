
################################################################################
## brushXspline()

grid.brushline <- function(...) {
    grid.draw(brushlineGrob(...))
}

brushlineGrob <- function(brush, x, y, w, default.units="npc",
                          shape=1, angle="perp", open=TRUE, 
                          render=vwPath(), 
                          gp=gpar(fill="black"), name=NULL, debug=FALSE) {
    ## Ok to recycle x or y or w
    maxlen <- max(length(x), length(y), length(w))
    if (length(x) < maxlen) 
        x <- rep(x, length.out=maxlen)
    if (length(y) < maxlen) 
        y <- rep(y, length.out=maxlen)
    if (length(w) < maxlen) 
        w <- rep(w, length.out=maxlen)
    checkbrushline(x, y)
    if (!is.unit(x)) {
        x <- unit(x, default.units)
    }
    if (!is.unit(y)) {
        y <- unit(y, default.units)
    }
    if (!is.unit(w)) {
        w <- unit(w, default.units)
    }
    gTree(brush=brush, x=x, y=y, w=w, shape=shape, angle=angle, open=open,
          render=render,
          gp=gp, name=name, cl="brushlineGrob",
          debug=debug)
}

checkbrushline <- function(x, y, w) {
    if (max(length(x), length(y)) < 2)
        stop("A brushline must have at least two points")
    nx <- length(x)
    ny <- length(y)
    nw <- length(w)
    if (nx != ny || nx != nw) {
        stop("x, y, and w must all have same length")
    }
}

brushlineOutline <- function(grob) {
    N <- length(grob$x)
    x <- convertX(grob$x, "in", valueOnly=TRUE)
    y <- convertY(grob$y, "in", valueOnly=TRUE)
    w <- pmin(convertWidth(grob$w, "in", valueOnly=TRUE),
              convertHeight(grob$w, "in", valueOnly=TRUE))

    ## Calculate distances between flattened vertices
    lengths <- c(0, sqrt(diff(x)^2 + diff(y)^2))
    cumLength <- cumsum(lengths)
    totalLength <- sum(lengths)

    if (grepl("vert", grob$angle)) {
        a <- rep(0, length(x))
    } else if (grepl("horiz", grob$angle)) {
        a <- rep(pi/2, length(x))
    } else {
        a <- numeric(length(x))
        if (grob$open) {
            a[1] <- brushEndAngle(x[1:2], y[1:2])
        } else {
            a[1] <- brushAngle(x[c(N, 1:2)], y[c(N, 1:2)])
        }                
        if (N > 2) {
            for (i in 2:(N - 1)) {
                a[i] <- brushAngle(x[(i-1):(i+1)], y[(i-1):(i+1)])
            }
        }
        if (grob$open) {
            a[N] <- brushEndAngle(x[(N-1):N], y[(N-1):N])
        } else {
            a[N] <- brushAngle(x[c((N-1):N, 1)], y[c((N-1):N, 1)])
        }
    } 
    brushes <- vector("list", N)
    brushes[[1]] <- placeBrush(grob$brush, x[1], y[1], w[1], a[1])
    if (N > 2) {
        for (i in 2:(N - 1)) {
            brushes[[i]] <- placeBrush(grob$brush, x[i], y[i], w[i], a[i])
        }
    }
    brushes[[N]] <- placeBrush(grob$brush, x[N], y[N], w[N], a[N])
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
        
    curve
}

makeContent.brushlineGrob <- function(x, ...) {
    outline <- brushlineOutline(x)
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
