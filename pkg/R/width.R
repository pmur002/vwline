
################################################################################
## Handling of width specification

## Convert single width to two-sided width
widthSpec <- function(x, default.units="npc") {
    if (is.list(x)) {
        if (!all(c("left", "right") %in% names(x)) ||
            length(x$left) != length(x$right)) {
            stop("Invalid width specification")
        }
    } else {
        x <- list(left=.5*x, right=.5*x)
    }
    if (!is.unit(x$left)) {
        x$left <- unit(x$left, default.units)
    }
    if (!is.unit(x$right)) {
        x$right <- unit(x$right, default.units)
    }
    class(x) <- "widthSpec"
    x
}

length.widthSpec <- function(x) {
    length(x$left)
}

## Width is specified as an X-spline where ...
## ... x values are distances along the path
## ... y values are widths
widthSpline <- function(w=unit(1, "cm"), default.units="in",
                        d=NULL, shape=-1, rep=FALSE) {
    if (length(w) == 1) {
        w <- rep(w, 2)
    }
    if (!is.unit(w)) {
        w <- unit(w, default.units)
    }
    if (is.null(d)) {
        d <- seq(0, 1, length.out=length(w))
    } 
    sw <- list(w=w, d=d, shape=shape, rep=rep)
    class(sw) <- "widthSpline"
    sw
}

## Resolve the width transitions for a path 
resolveWidth <- function(w, length) {
    ## y-locations are widths
    ## x-locations are along path
    ## order() by x-locations
    y <- pmin(convertWidth(w$w, "in", valueOnly=TRUE),
              convertHeight(w$w, "in", valueOnly=TRUE))
    if (is.unit(w$d)) {
        x <- convertX(w$d, "in", valueOnly=TRUE)
    } else {
        ## Assume x-location is a proportion
        x <- pmin(w$d, 1)*length
    }
    o <- order(x)
    y <- y[o]
    x <- x[o]
    widthSpline <- xsplineGrob(x, y, default.units="in", shape=w$shape)
    widthPts <- xsplinePoints(widthSpline)
    xx <- as.numeric(widthPts$x)
    yy <- as.numeric(widthPts$y)
    minx <- min(xx)
    maxx <- max(xx)
    ## repeat or extend if necessary 
    if (minx > 0 || maxx < length) {
        lefty <- NULL
        leftx <- NULL
        righty <- NULL
        rightx <- NULL
        if (minx > 0) {
            if (w$rep) {
                ## Repeat the widths
                multiple <- minx %/% (maxx - minx) + 1
                lefty <- rep(yy, multiple)
                leftx <- rep(xx, multiple) -
                    rep(multiple:1*(maxx - minx), each=length(xx)) 
            } else {
                ## Add initial width at start of path
                lefty <- yy[1]
                leftx <- 0
            }
        }
        if (maxx < length) {
            if (w$rep) {
                ## Repeat the widths
                multiple <- (length - maxx) %/% (maxx - minx) + 1
                righty <- rep(yy, multiple)
                rightx <- rep(1:multiple*(maxx - minx), each=length(xx)) +
                    rep(xx, multiple)
            } else {
                ## Add final width at end of path
                righty <- yy[length(yy)]
                rightx <- length
                
            }
        }
        yy <- c(lefty, yy, righty)
        xx <- c(leftx, xx, rightx)
    }
    ## No need to trim because these widths are just used to interpolate
    ## locations along path and widths at existing locations along path
    ## What is important is that these widths EXCEED the range 0 to 'length'
    ## (otherwise the interpolation will produce constant values)
    ## Trim to path 
    list(x=xx, y=yy)
}

