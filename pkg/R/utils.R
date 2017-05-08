
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

################################################################################
## Intersections of line segments (take 2)

## Following J. O'Rourke. Computational Geometry in C. Cambridge University Press, New York, 1994.

## ALL input coordinates are INTEGER

Area2 <- function(ax, ay, bx, by, cx, cy) {
    (bx - ax)*(cy - ay) - (cx - ax)*(by - ay)
}

Collinear <- function(ax, ay, bx, by, cx, cy) {
    Area2(ax, ay, bx, by, cx, cy) == 0
}

Between <- function(ax, ay, bx, by, cx, cy) {
    ifelse(ax != bx,
           (ax <= cx & cx <= bx) | (ax >= cx & cx >= bx),
           (ay <= cy & cy <= by) | (ay >= cy & cy >= by))
}

ParallelInt <- function(ax, ay, bx, by, cx, cy, dx, dy) {
    Cabc <- Collinear(ax, ay, bx, by, cx, cy)
    Babc <- Between(ax, ay, bx, by, cx, cy)
    Babd <- Between(ax, ay, bx, by, dx, dy)
    Bcda <- Between(cx, cy, dx, dy, ax, ay)
    Bcdb <- Between(cx, cy, dx, dy, bx, by)
    ## Extension (for non-overlap)
    Badb <- Between(ax, ay, dx, dy, bx, by)
    Badc <- Between(ax, ay, dx, dy, cx, cy)
    Bbda <- Between(bx, by, dx, dy, ax, ay)
    Bbdc <- Between(bx, by, dx, dy, cx, cy)
    Bacb <- Between(ax, ay, cx, cy, bx, by)
    Bacd <- Between(ax, ay, cx, cy, dx, dy)
    Bbca <- Between(bx, by, cx, cy, ax, ay)
    Bbcd <- Between(bx, by, cx, cy, dx, dy)
    ## x <- ifelse(!Cabc, NA, ifelse(Babc, cx, ifelse(Babd, dx, ifelse(Bcda, ax, ifelse(Bcdb, bx, NA)))))
    ## y <- ifelse(!Cabc, NA, ifelse(Babc, cy, ifelse(Babd, dy, ifelse(Bcda, ay, ifelse(Bcdb, by, NA)))))
    x <- ifelse(!Cabc, NA,
         ifelse(Babc & Babd, (cx + dx)/2,
         ifelse(Bcda & Bcdb, (ax + bx)/2,
         ifelse(Babc & Bcdb, (bx + cx)/2,
         ifelse(Babc & Bcda, (ax + cx)/2,
         ifelse(Babd & Bcdb, (bx + dx)/2,
         ifelse(Babd & Bcda, (ax + dx)/2,
         ## Extension (for non-overlap)
         ifelse(Badb & Babc, (bx + cx)/2,
         ifelse(Bbda & Bbdc, (ax + cx)/2,
         ifelse(Bacb & Bacd, (bx + dx)/2,
         ifelse(Bbca & Bbcd, (ax + dx)/2, NA)))))))))))
    y <- ifelse(!Cabc, NA,
         ifelse(Babc & Babd, (cy + dy)/2,
         ifelse(Bcda & Bcdb, (ay + by)/2,
         ifelse(Babc & Bcdb, (by + cy)/2,
         ifelse(Babc & Bcda, (ay + cy)/2,
         ifelse(Babd & Bcdb, (by + dy)/2,
         ifelse(Babd & Bcda, (ay + dy)/2,
         ## Extension (for non-overlap)
         ifelse(Badb & Babc, (by + cy)/2,
         ifelse(Bbda & Bbdc, (ay + cy)/2,
         ifelse(Bacb & Bacd, (by + dy)/2,
         ifelse(Bbca & Bbcd, (ay + dy)/2, NA)))))))))))
    list(x=x, y=y)
} 

SegSegInt <- function(ax, ay, bx, by, cx, cy, dx, dy) {
    denom <- ax*(dy - cy) + bx*(cy - dy) + dx*(by - ay) + cx*(ay - by)
    num1 <- ax*(dy - cy) + cx*(ay - dy) + dx*(cy - ay)
    s <- num1/denom
    px <- ax + s*(bx - ax)
    py <- ay + s*(by - ay)
    paraInt <- ParallelInt(ax, ay, bx, by, cx, cy, dx, dy)
    x <- ifelse(denom == 0, paraInt$x, px)
    y <- ifelse(denom == 0, paraInt$y, py)
    list(x=x, y=y)
}

## We are dealing in INCHES, so 1e-6 means a millionth of an inch
intersection <- function(start1x, start1y, end1x, end1y,
                         start2x, start2y, end2x, end2y,
                         eps=1e-6, debug=FALSE) {
    int <- SegSegInt(round(start1x/eps), round(start1y/eps),
                     round(end1x/eps), round(end1y/eps),
                     round(start2x/eps), round(start2y/eps),
                     round(end2x/eps), round(end2y/eps))
    if (debug) {
        pts(c(start1x, end1x, start2x, end2x),
            c(start1y, end1y, start2y, end2y))
        segs(c(start1x, start2x), c(start1y, start2y),
             c(end1x, end2x), c(end1y, end2y))
        pts(int$x*eps, int$y*eps, "red")
    }
    list(x=int$x*eps, y=int$y*eps)
}

notrun <- function() {
    testIntersect <- function(ax, ay, bx, by, cx, cy, dx, dy, x, y) {
        pushViewport(viewport(x=unit(x - .5, "in"),
                              y=unit(y - .5, "in"),
                              just=c("left", "bottom"),
                              width=unit(2, "in"), height=unit(2, "in")))
        int <- intersection(ax, ay, bx, by, cx, cy, dx, dy, debug=TRUE)
        popViewport()
        int
    }
    grid.newpage()
    testIntersect(1, 1, 2, 2, 1, 2, 2, 1, 0, 0)
    testIntersect(1, 1, 2, 2, 1.5, 1.5, 2.5, 2.5, 2, 0)
    testIntersect(1, 1, 2, 2, 1.5, 1, 2.5, 2, 4, 0)    
    testIntersect(1.5, 1, 1.5, 2, 1, 1.5, 2, 1.5, 0, 2)    
    testIntersect(1.5, 1, 1.5, 2, 1.5, 1.5, 1.5, 2.5, 2, 2)    
    testIntersect(1, 1.5, 2, 1.5, 1.5, 1.5, 2.5, 1.5, 4, 2)    
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

