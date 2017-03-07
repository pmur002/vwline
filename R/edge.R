
## Generic interface for calculating points on the boundary of a
## variable-width curve

## 'x' is the curve object
## 'd' specifies distance along the edge
edgePoints <- function(x, d, ...) {
    UseMethod("edgePoints")
}

## The difference types of curves generate different boundaries, but
## given a boundary, they all do the same thing
vwEdgePoints <- function(pts, d, forward, open, debug=FALSE) {
    ## each boundary is either numeric (imlpicit inches) or a unit in "in"
    x <- as.numeric(pts$x)
    y <- as.numeric(pts$y)
    if (!forward) {
        x <- rev(x)
        y <- rev(y)
    }
    if (!open) {
        x <- c(x, x[1])
        y <- c(y, y[1])
    }
    ## Calculate total length of boundary
    lengths <- c(0, sqrt(diff(x)^2 + diff(y)^2))
    cumLength <- cumsum(lengths)
    length <- sum(lengths)
    ## Determine point selection
    locs <- resolveDistance(d, length, fill=FALSE)
    index <- apply(outer(locs, cumLength, "<="), 1,
                   function(x) min(which(x)))
    ## Add tangent info
    n <- length(locs)
    tangent <- numeric(n)
    xx <- numeric(n)
    yy <- numeric(n)
    for (i in 1:n) {
        ## What is the distance to the next boundary point ?
        prev <- index[i] - 1
        if (prev < 1) {
            dist <- locs[i]
            prev <- length(x)
        } else {
            dist <- locs[i] - cumLength[prev]
        }
        if (dist == 0) {
            ## Bang on a boundary point
            xx[i] <- x[index[i]]
            yy[i] <- y[index[i]]
            below <- index[i] - 1
            if (below < 1) below <- length(x)
            above <- index[i] + 1
            if (above > length(x)) above <- 1
            tangent[i] <- angle(x[c(above, below)], y[c(above, below)])
        } else {
            tangent[i] <- angle(x[c(prev, index[i])], y[c(prev, index[i])])
            xx[i] <- x[prev] + dist*cos(tangent[i])
            yy[i] <- y[prev] + dist*sin(tangent[i])
        }
    }
    x <- unit(xx, "in")
    y <- unit(yy, "in")
    if (debug) {
        grid.points(x, y, size=unit(2, "mm"), 
                    pch=16, gp=gpar(col="red"))
        grid.segments(x, y,
                      x + unit(5*cos(tangent - pi/2), "mm"),
                      y + unit(5*sin(tangent - pi/2), "mm"),
                      gp=gpar(col="red"))
        grid.text(1:n, x, y)
    }
    list(x=x, y=y, tangent=tangent)
}

reorderEdge <- function(pts, x0, y0) {
    x0 <- convertX(x0, "in", valueOnly=TRUE)
    y0 <- convertY(y0, "in", valueOnly=TRUE)
    N <- length(pts$x)
    dist <- (pts$x - x0)^2 + (pts$y - y0)^2
    closest <- which.min(dist)[1]
    order <- (1:N + closest - 2) %% N
    list(x=pts$x[order], y=pts$y[order])
}

