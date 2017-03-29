
################################################################################
## Angles of lines

## x and y are vectors of length 2
angle <- function(x, y) {
    atan2(y[2] - y[1], x[2] - x[1])
}

## x and y are vectors of length 3
avgangle <- function(x, y) {
    a1 <- angle(x[1:2], y[1:2])
    a2 <- angle(x[2:3], y[2:3])
    atan2(sin(a1) + sin(a2), cos(a1) + cos(a2))
}

## extend direction from pt 1 to pt 2
extend <- function(x, y, angle, d) {
    list(x=x + d*cos(angle),
         y=y + d*sin(angle))
}

## x and y are vectors; ends defines subset of length 2
perp <- function(x, y, len, a, mid) {
    dx <- len*cos(a + pi/2)
    dy <- len*sin(a + pi/2)
    upper <- c(x[mid] + dx, y[mid] + dy)
    lower <- c(x[mid] - dx, y[mid] - dy)
    rbind(upper, lower)    
}

## x and y are vectors of length 2
perpStart <- function(x, y, len) {
    perp(x, y, len, angle(x, y), 1)
}

perpEnd <- function(x, y, len) {
    perp(x, y, len, angle(x, y), 2)
}

## x and y are vectors of length 3
## We want the "average" angle at the middle point
perpMid <- function(x, y, len) {
    ## Now determine angle at midpoint
    perp(x, y, len, avgangle(x, y), 2)
}

## x and y and len are vectors of any length
offset <- function(x, y, len, a) {
    dx <- len*cos(a)
    dy <- len*sin(a)
    upper <- c(x + dx, y + dy)
    lower <- c(x - dx, y - dy)
    list(left=list(x=x + dx, y=y + dy),
         right=list(x=x - dx, y=y - dy))
}

dist <- function(dx, dy) {
    sqrt(dx^2 + dy^2)
}

angleInRange <- function(x) {
    while (any(x < -pi)) {
        toolow <- x < -pi 
        x[toolow] <- x[toolow] + 2*pi
    }
    while (any(x > pi)) {
        toohigh <- x > pi
        x[toohigh] <- x[toohigh] - 2*pi
    }
    x
}

angleDiff <- function(a1, a2, clockwise, debug=FALSE) {
    if (clockwise) {
        result <- ifelse(a1 > a2, -(a1 - a2), -((a1 + pi) + (pi - a2)))
    } else {
        result <- ifelse(a1 < a2, a2 - a1, (pi - a1) + (a2 + pi))
    }

    if (debug) {
        grid.newpage()
        pushViewport(viewport(width=.9, height=.9,
                              xscale=c(-1, 1), yscale=c(-1, 1)))
        grid.segments(0, .5, 1, .5)
        grid.segments(.5, 0, .5, 1)
        grid.segments(0, 0, cos(a1), sin(a1), default.units="native",
                      gp=gpar(lwd=3), arrow=arrow())
        grid.text("a1", 1.05*cos(a1), 1.05*sin(a1), default.units="native")
        grid.segments(0, 0, cos(a2), sin(a2), default.units="native",
                      gp=gpar(lwd=3), arrow=arrow())
        grid.text("a2", 1.05*cos(a2), 1.05*sin(a2), default.units="native")
        t <- seq(a1, a1 + result, length.out=100)
        grid.polygon(c(0, .5*cos(t)), c(0, .5*sin(t)), default.units="native",
                     gp=gpar(fill="grey"))
        grid.text(if (clockwise) "clockwise" else "anticlockwise",
                  .25*cos(a1 + .5*result), .25*sin(a1 + .5*result),
                  default.units="native")
        popViewport()
    }

    result
}

angleSeq <- function(a1, a2, len, clockwise, debug=FALSE) {
    adiff <- angleDiff(a1, a2, clockwise)
    result <- seq(a1, a1 + adiff, length.out=len)

    if (debug) {
        grid.newpage()
        pushViewport(viewport(width=.9, height=.9,
                              xscale=c(-1, 1), yscale=c(-1, 1)))
        grid.segments(0, .5, 1, .5)
        grid.segments(.5, 0, .5, 1)
        grid.segments(0, 0, cos(a1), sin(a1), default.units="native",
                      gp=gpar(lwd=3), arrow=arrow())
        grid.text("a1", 1.05*cos(a1), 1.05*sin(a1), default.units="native")
        grid.segments(0, 0, cos(a2), sin(a2), default.units="native",
                      gp=gpar(lwd=3), arrow=arrow())
        grid.text("a2", 1.05*cos(a2), 1.05*sin(a2), default.units="native")
        grid.lines(.5*cos(result), .5*sin(result), default.units="native",
                   arrow=arrow())
        grid.text(if (clockwise) "clockwise" else "anticlockwise",
                  .25*cos(a1 + .5*adiff), .25*sin(a1 + .5*adiff),
                  default.units="native")
        popViewport()
    }

    result
}

################################################################################
## Intersections of line segments

## Following
## http://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/
## Given three colinear points start, pt, end the function checks if
## pt lies on line segment start->end
onSegment <- function(startx, starty, ptx, pty, endx, endy) {
    rptx <- round(ptx, 10)
    rpty <- round(pty, 10)
    rsx <- round(startx, 10)
    rsy <- round(starty, 10)
    rex <- round(endx, 10)
    rey <- round(endy, 10)
    rptx <= pmax(rsx, rex) & rptx >= pmin(rsx, rex) &
        rpty <= pmax(rsy, rey) & rpty >= pmin(rsy, rey)
}

## To find orientation of ordered triplet (start, end, pt).
## The function returns following values
## 0 --> start, end and pt are colinear
## 1 --> Clockwise
## 2 --> Counterclockwise
orientation <- function(startx, starty, endx, endy, ptx, pty) {
    val = (endy - starty) * (ptx - endx) - (endx - startx) * (pty - endy);
    ifelse(val == 0, 0, ifelse(val > 0, 1, 2))
}

## The main function that returns true if line segment start1->end1
## and start2->end2 intersect.
doIntersect <- function(start1x, start1y, end1x, end1y,
                        start2x, start2y, end2x, end2y,
                        debug=FALSE) {
    ## Find the four orientations needed for general and special cases
    o1 <- orientation(start1x, start1y, end1x, end1y, start2x, start2y);
    o2 <- orientation(start1x, start1y, end1x, end1y, end2x, end2y);
    o3 <- orientation(start2x, start2y, end2x, end2y, start1x, start1y);
    o4 <- orientation(start2x, start2y, end2x, end2y, end1x, end1y);

    if (debug) {
        pts(c(start1x, end1x, start2x, end2x),
            c(start1y, end1y, start2y, end2y))
        segs(c(start1x, start2x), c(start1y, start2y),
             c(end1x, end2x), c(end1y, end2y))
    }
    
    (o1 != o2 & o3 != o4) |
        (o1 == 0 &
         onSegment(start1x, start1y, start2x, start2y, end1x, end1y)) |
        (o2 == 0 &
         onSegment(start1x, start1y, end2x, end2y, end1x, end1y)) |
        (o3 == 0 &
         onSegment(start2x, start2y, start1x, start1y, end2x, end2y)) |
        (o4 == 0 & onSegment(start2x, start2y, end1x, end1y, end2x, end2y))
}

intersection <- function(start1x, start1y, end1x, end1y,
                         start2x, start2y, end2x, end2y,
                         debug=FALSE) {
    dx1 <- end1x - start1x
    dy1 <- end1y - start1y
    dx2 <- end2x - start2x
    dy2 <- end2y - start2y
    slope1 <- dy1/dx1
    slope2 <- dy2/dx2
    intercept1 <- start1y - slope1*start1x
    intercept2 <- start2y - slope2*start2x
    finite1 <- is.finite(slope1)
    finite2 <- is.finite(slope2)
    eps <- 1e-10
    sameslope <- abs(slope1 - slope2) < eps |
        (!finite1 & !finite2)
    sameintercept <- abs(intercept1 - intercept2) < eps |
        (!finite1 & !finite2 & abs(start1x - start2x) < eps)
    intx <-
        ifelse(finite1 & finite2 & !sameslope,
               ## Normal
               (intercept2 - intercept1)/(slope1 - slope2),
               ifelse(!finite1 & finite2,
                      ## Line 1 vertical
                      start1x,
                      ifelse(!finite2 & finite1,
                             ## Line 2 vertical
                             start2x,
                             ifelse(sameslope & !sameintercept,
                                    ## parallel
                                    Inf,
                                    ## collinear (sameslope & sameintercept)
                                    (start1x + end1x + start2x + end2x)/4))))
    inty <-
        ifelse(finite1 & finite2 & !sameslope,
               ## Normal
               (slope1*intercept2 - slope2*intercept1)/(slope1 - slope2),
               ifelse(!finite1 & finite2,
                      ## Line 1 vertical
                      intercept2 + slope2*start1x,
                      ifelse(!finite2 & finite1,
                             ## Line 2 vertical
                             intercept1 + slope1*start2x,
                             ifelse(sameslope & !sameintercept,
                                    ## parallel
                                    Inf,
                                    ## collinear (sameslope & sameintercept)
                                    (start1y + end1y + start2y + end2y)/4))))
    
    
    if (debug) {
        pts(c(start1x, end1x, start2x, end2x),
            c(start1y, end1y, start2y, end2y))
        segs(c(start1x, start2x), c(start1y, start2y),
             c(end1x, end2x), c(end1y, end2y))
        pts(intx, inty, "red")
    }
    
    list(x=intx, y=inty)
}

################################################################################
## 2D transformations
translation <- function(tx, ty) {
    m <- diag(1, 3, 3)
    m[1, 3] <- tx
    m[2, 3] <- ty
    m
}

scaling <- function(sx, sy) {
    m <- diag(1, 3, 3)
    m[1, 1] <- sx
    m[2, 2] <- sy
    m
}

rotation <- function(angle) {
    sa <- sin(angle)
    ca <- cos(angle)
    m <- diag(1, 3, 3)
    m[1, 1] <- ca
    m[1, 2] <- -sa
    m[2, 1] <- sa
    m[2, 2] <- ca
    m
}

transformation <- function(transformations) {
    Reduce("%*%", transformations)
}

transform <- function(obj, transformation) {
    result <- transformation %*% rbind(obj$x, obj$y, 1) 
    list(x=result[1,], y=result[2,])
}

################################################################################
## Interpolate points on a (flattened) path (x, y),
## given distance (d) along the path
## and lengths of each path segment
## (ALL in inches)
## Return a new path, with new points included
## The algorithm below could SURELY be improved!
fortifyPath <- function(x, y, d, lengths, open, tol=.01) {
    cumLength <- cumsum(lengths)
    N <- length(x)
    if (open) {
        xx <- as.list(x)[-N]
        yy <- as.list(y)[-N]
        loopEnd <- N - 1
    } else {
        xx <- as.list(x)
        yy <- as.list(y)
        x <- c(x, x[1])
        y <- c(y, y[1])
        loopEnd <- N
    }
    distIndex <- 1
    lastDist <- 0
    ## For each segment START point
    for (i in 1:loopEnd) {
        while (d[distIndex] <= cumLength[i + 1] &&
               distIndex < length(d) + 1) {
                   thisDist <- d[distIndex]
                   dist1 <- thisDist - cumLength[i]
                   dist2 <- cumLength[i+1] - thisDist
                   if (dist1 > tol && dist2 > tol &&
                       (thisDist - lastDist) > tol) {
                       newx <- x[i] + dist1/lengths[i + 1]*
                           (x[i + 1] - x[i])
                       xx[[i]] <- c(xx[[i]], newx)
                       newy <- y[i] + dist1/lengths[i + 1]*
                           (y[i + 1] - y[i])
                       yy[[i]] <- c(yy[[i]], newy)
                       lastDist <- thisDist
                   }
                   distIndex <- distIndex + 1
               }
    }
    if (open) {
        list(x=c(unlist(xx), x[N]), y=c(unlist(yy), y[N]))
    } else {
        list(x=unlist(xx), y=unlist(yy))
    }
}

## Calculate points on a (flattened) path (x, y),
## given distance (d) along the path
## and lengths of each path segment
## (ALL in inches)
interpPath <- function(x, y, d, lengths, open) {
    cumLength <- cumsum(lengths)
    N <- length(x)
    if (open) {
        loopEnd <- N - 1
    } else {
        x <- c(x, x[1])
        y <- c(y, y[1])
        loopEnd <- N
    }
    xx <- numeric(length(d))
    yy <- numeric(length(d))
    a <- numeric(length(d))
    di <- 1
    ## For each segment START point
    for (i in 1:loopEnd) {
        while (d[di] <= cumLength[i + 1] &&
               di < length(d) + 1) {
                   dist <- d[di] - cumLength[i]
                   xx[di] <- x[i] + dist/lengths[i + 1]*
                       (x[i + 1] - x[i])
                   yy[di] <- y[i] + dist/lengths[i + 1]*
                       (y[i + 1] - y[i])
                   a[di] <- angle(x[i:(i+1)], y[i:(i+1)])
                   di <- di + 1
               }
    }
    list(x=xx, y=yy, angle=a)
}

