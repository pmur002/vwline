
################################################################################
## Angles of lines

## x and y are vectors of length 2
angle <- function(x, y) {
    atan2(y[2] - y[1], x[2] - x[1])
}

## x and y are vectors of length 3
avgangle <- function(x, y) {
    atan2(sum(diff(y))/2, sum(diff(x))/2)
}

## extend direction from pt 1 to pt 2
extend <- function(x, y) {
    a <- angle(x[2:1], y[2:1])
    len <- sqrt(diff(x)^2 + diff(y)^2)
    dx <- len*cos(a)
    dy <- len*sin(a)
    c(x[1] + dx, y[1] + dy)
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
## Interpolat points on a (flattened) path (x, y),
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
    ## For each segment START point
    for (i in 1:loopEnd) {
        while (d[distIndex] <= cumLength[i + 1] &&
               distIndex < length(d) + 1) {
                   dist1 <- d[distIndex] - cumLength[i]
                   dist2 <- cumLength[i+1] - d[distIndex]
                   if (dist1 > tol && dist2 > tol) {
                       newx <- x[i] + dist1/lengths[i + 1]*
                           (x[i + 1] - x[i])
                       xx[[i]] <- c(xx[[i]], newx)
                       newy <- y[i] + dist1/lengths[i + 1]*
                           (y[i + 1] - y[i])
                       yy[[i]] <- c(yy[[i]], newy)
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

